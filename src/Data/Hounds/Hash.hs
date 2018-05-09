module Data.Hounds.Hash
  ( Hash(..)
  , mkHash
  ) where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Char8      as C
import           Data.Serialize


newtype Hash = MkHash { unHash :: B.ByteString }
  deriving (Eq, Ord)

instance Show Hash where
  show (MkHash bs) = "<" ++ C.unpack (Base16.encode bs) ++ ">"

mkHash :: B.ByteString -> Hash
mkHash = MkHash . BLAKE2b.hash 32 mempty

putHash :: Putter Hash
putHash = putByteString . unHash

getHash :: Get Hash
getHash = MkHash <$> getByteString 32

instance Serialize Hash where
  put = putHash
  get = getHash
