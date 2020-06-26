module Data.Hounds.Hash
  ( Hash(..)
  , mkHash
  ) where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Char8      as C
import           Data.Serialize


digestLength :: Int
digestLength = 32

newtype Hash = MkHash { unHash :: B.ByteString }
  deriving (Eq, Ord)

instance Show Hash where
  show (MkHash bs) = C.unpack (Base16.encode bs)

instance Read Hash where
  readsPrec _ = \ input ->
    let
      (bs, r) = Base16.decode (C.pack input)
    in
      [(MkHash bs, C.unpack r) | r == C.empty, B.length bs == digestLength]

mkHash :: B.ByteString -> Hash
mkHash = MkHash . BLAKE2b.hash digestLength mempty

putHash :: Putter Hash
putHash = putByteString . unHash

getHash :: Get Hash
getHash = MkHash <$> getByteString digestLength

instance Serialize Hash where
  put = putHash
  get = getHash
