module Data.Hounds.Hash
  ( Hash
  , unHash
  , mkHash
  , putHash
  , getHash
  ) where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import qualified Data.ByteString            as B
import           Data.Serialize


newtype Hash = MkHash { unHash :: B.ByteString }
  deriving (Eq, Show)

mkHash :: B.ByteString -> Hash
mkHash = MkHash . BLAKE2b.hash 32 mempty

putHash :: Putter Hash
putHash = putByteString . unHash

getHash :: Get Hash
getHash = MkHash <$> getByteString 32
