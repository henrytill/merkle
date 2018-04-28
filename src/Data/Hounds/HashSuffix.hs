module Data.Hounds.HashSuffix
  ( HashSuffix(..)
  , mkHashSuffix
  , putHashSuffix
  , getHashSuffix
  ) where

import qualified Data.ByteString as B
import           Data.Serialize

import           Data.Hounds.Hash


newtype HashSuffix = MkHashSuffix { unHashSuffix :: B.ByteString }
  deriving (Eq, Show)

mkHashSuffix :: Int -> Hash -> HashSuffix
mkHashSuffix level = MkHashSuffix . B.drop level . unHash

putHashSuffix :: Putter HashSuffix
putHashSuffix (MkHashSuffix bs) = do
  (putWord8 . fromIntegral . B.length) bs
  putByteString bs

getHashSuffix :: Get HashSuffix
getHashSuffix = do
  len <- getWord8
  MkHashSuffix <$> getByteString (fromIntegral len)

instance Serialize HashSuffix where
  put = putHashSuffix
  get = getHashSuffix
