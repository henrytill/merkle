{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Data.Hounds.MerklePatricia where

import qualified Crypto.Hash.BLAKE2.BLAKE2b    as BLAKE2b
import           Data.Array                    (Array, listArray, (//))
import qualified Data.ByteString               as B
import           Data.Serialize
import           Data.Word                     (Word8, Word64)


newtype Hash = MkHash { unHash :: B.ByteString }
  deriving (Eq, Show)

mkHash :: B.ByteString -> Hash
mkHash = MkHash . BLAKE2b.hash 32 mempty

putHash :: Putter Hash
putHash = putByteString . unHash

getHash :: Get Hash
getHash = MkHash <$> getByteString 32

newtype PointerBlock = MkPointerBlock { unPointerBlock :: Array Word8 (Maybe Hash) }
  deriving (Eq, Show)

mkPointerBlock :: PointerBlock
mkPointerBlock = MkPointerBlock (listArray (0, 255) (replicate 256 Nothing))

update :: PointerBlock -> (Word8, Maybe Hash) -> PointerBlock
update (MkPointerBlock arr) indexedHash = MkPointerBlock (arr // [indexedHash])

putPointerBlock :: Putter PointerBlock
putPointerBlock = putIArrayOf putWord8 (putMaybeOf putHash) . unPointerBlock

getPointerBlock :: Get PointerBlock
getPointerBlock = MkPointerBlock <$> getIArrayOf getWord8 (getMaybeOf getHash)

serializePointerBlock :: PointerBlock -> B.ByteString
serializePointerBlock = runPut . putPointerBlock

deserializePointerBlock :: B.ByteString -> Either String PointerBlock
deserializePointerBlock = runGet getPointerBlock

hashPointerBlock :: PointerBlock -> Hash
hashPointerBlock = mkHash . serializePointerBlock

newtype HashSuffix = MkHashSuffix { unHashSuffix :: B.ByteString }
  deriving (Eq, Show)

putHashSuffix :: Putter HashSuffix
putHashSuffix (MkHashSuffix bs) = do
  (putWord8 . fromIntegral . B.length) bs
  putByteString bs

getHashSuffix :: Get HashSuffix
getHashSuffix = do
  len <- getWord8
  MkHashSuffix <$> getByteString (fromIntegral len)

data Tree
  = Node PointerBlock
  | Leaf HashSuffix Hash
  deriving (Eq, Show)

instance Serialize Tree where
  put (Node pb)    = do { putWord8 0
                        ; putPointerBlock pb
                        }
  put (Leaf rp hs) = do { putWord8 1
                        ; putHashSuffix rp
                        ; putHash hs
                        }
  get              = do { tag <- getWord8
                        ; case tag of
                            0 -> Node <$> getPointerBlock
                            1 -> Leaf <$> getHashSuffix <*> getHash
                            _ -> fail "no such tag"
                        }

mkTree :: Tree
mkTree = Node mkPointerBlock

hashTree :: Tree -> Hash
hashTree (Node pb)   = hashPointerBlock pb
hashTree (Leaf _ hs) = hs

data LogKey = MkLogKey
  { logKeyLeafHash :: Hash
  , logKeyCount :: Word64
  } deriving (Eq, Show)

putLogKey :: Putter LogKey
putLogKey lk = do
  putHash     (logKeyLeafHash lk)
  putWord64le (logKeyCount    lk)

getLogKey :: Get LogKey
getLogKey = MkLogKey <$> getHash <*> getWord64le

data Operation = Insert | Delete
  deriving (Eq, Show)

putOperation :: Putter Operation
putOperation Insert = putWord8 0
putOperation Delete = putWord8 1

getOperation :: Get Operation
getOperation = do
  tag <- getWord8
  case tag of
    0 -> return Insert
    1 -> return Delete
    _ -> fail "no such tag"

data LogValue = MkLogValue
  { logValueOperation :: Operation
  , logValueLeafHash  :: Hash
  } deriving (Eq, Show)

putLogValue :: Putter LogValue
putLogValue le = do
  putOperation (logValueOperation le)
  putHash      (logValueLeafHash  le)

getLogValue :: Get LogValue
getLogValue = MkLogValue <$> getOperation <*> getHash

data LeafValue = MkLeafValue
  { leafValueDeleted :: Bool
  , leafValueBytes   :: B.ByteString
  } deriving (Eq, Show)

putLeafValue :: Putter LeafValue
putLeafValue (MkLeafValue True  bs)
  = do putWord8 1
       putWord64le (fromIntegral (B.length bs))
       putByteString bs
putLeafValue (MkLeafValue False bs)
  = do putWord8 0
       putWord64le (fromIntegral (B.length bs))
       putByteString bs

getLeafValue :: Get LeafValue
getLeafValue = do
  tag <- getWord8
  len <- getWord64le
  case tag of
    0 -> MkLeafValue False <$> getByteString (fromIntegral len)
    1 -> MkLeafValue True  <$> getByteString (fromIntegral len)
    _ -> fail "no such tag"
