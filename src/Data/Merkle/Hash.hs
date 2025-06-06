{-# LANGUAGE TupleSections #-}

module Data.Merkle.Hash
  ( Hash (..)
  , mkHash
  )
where

import Crypto.Hash.BLAKE2.BLAKE2b qualified as BLAKE2b
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C
import Data.Serialize

digestLength :: Int
digestLength = 32

newtype Hash = MkHash {unHash :: B.ByteString}
  deriving (Eq, Ord)

instance Show Hash where
  show (MkHash bs) = C.unpack (Base16.encode bs)

instance Read Hash where
  readsPrec _ input =
    [(MkHash bs, C.unpack r) | r == C.empty, B.length bs == digestLength]
    where
      -- TODO: This was hacked together to overcome API changes in base16-bytestring,
      -- but it could be rethought.
      (bs, r) = either (const (B.empty, C.empty)) (,C.empty) (Base16.decode (C.pack input))

mkHash :: B.ByteString -> Hash
mkHash = MkHash . BLAKE2b.hash digestLength mempty

putHash :: Putter Hash
putHash = putByteString . unHash

getHash :: Get Hash
getHash = MkHash <$> getByteString digestLength

instance Serialize Hash where
  put = putHash
  get = getHash
