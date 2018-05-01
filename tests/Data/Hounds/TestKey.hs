module Data.Hounds.TestKey where

import           Control.Exception    (Exception, throw)
import qualified Data.ByteString as B
import           Data.Serialize
import           Data.Word            (Word8)


data TestKeyException
  = InvalidSizeException
  deriving Show

instance Exception TestKeyException

newtype TestKey = MkTestKey { unTestKey :: B.ByteString }
  deriving (Eq, Show)

putTestKey :: Putter TestKey
putTestKey (MkTestKey bs) = putByteString bs

getTestKey :: Get TestKey
getTestKey = MkTestKey <$> getByteString 4

instance Serialize TestKey where
  put = putTestKey
  get = getTestKey

mkTestKey :: [Word8] -> TestKey
mkTestKey ws | length ws == 4 = MkTestKey (B.pack ws)
             | otherwise      = throw InvalidSizeException
