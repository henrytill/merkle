module Data.Hounds.LeafValue where

import qualified Data.ByteString as B
import           Data.Serialize


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
