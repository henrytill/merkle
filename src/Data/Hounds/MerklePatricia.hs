module Data.Hounds.MerklePatricia where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import           Data.Array                 (Array, listArray, (//))
import qualified Data.ByteString            as B
import           Data.Int                   (Int32)
import           Data.Serialize


newtype Hash = MkHash { unHash :: B.ByteString }
  deriving (Eq, Show)

mkHash :: B.ByteString -> Hash
mkHash = MkHash . BLAKE2b.hash 32 mempty

putHash :: Putter Hash
putHash = putByteString . unHash

getHash :: Get Hash
getHash = MkHash <$> getByteString 32

newtype PointerBlock = MkPointerBlock { unPointerBlock :: Array Int32 (Maybe Hash) }
  deriving (Eq, Show)

mkPointerBlock :: PointerBlock
mkPointerBlock = MkPointerBlock (listArray (0, 255) (replicate 256 Nothing))

update :: PointerBlock -> (Int32, Maybe Hash) -> PointerBlock
update (MkPointerBlock arr) indexedHash = MkPointerBlock (arr // [indexedHash])

putPointerBlock :: Putter PointerBlock
putPointerBlock = putIArrayOf putInt32le (putMaybeOf putHash) . unPointerBlock

getPointerBlock :: Get PointerBlock
getPointerBlock = MkPointerBlock <$> getIArrayOf getInt32le (getMaybeOf getHash)

serializePointerBlock :: PointerBlock -> B.ByteString
serializePointerBlock = runPut . putPointerBlock

deserializePointerBlock :: B.ByteString -> Either String PointerBlock
deserializePointerBlock = runGet getPointerBlock

hashPointerBlock :: PointerBlock -> Hash
hashPointerBlock = mkHash . serializePointerBlock

data Tree
  = Node PointerBlock
  | Leaf Hash
  deriving (Eq, Show)

instance Serialize Tree where
  put (Node pb) = do { putWord8 0
                     ; putPointerBlock pb
                     }
  put (Leaf hs) = do { putWord8 1
                     ; putHash hs
                     }
  get           = do { tag <- getWord8
                     ; case tag of
                         0 -> Node <$> getPointerBlock
                         1 -> Leaf <$> getHash
                         _ -> fail "no such tag"
                     }
