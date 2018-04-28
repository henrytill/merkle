module Data.Hounds.HashSuffix
  ( HashSuffix(..)
  , putHashSuffix
  , getHashSuffix
  ) where

import qualified Data.ByteString as B
import           Data.Serialize


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

instance Serialize HashSuffix where
  put = putHashSuffix
  get = getHashSuffix
