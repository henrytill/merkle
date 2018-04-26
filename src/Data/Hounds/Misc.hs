module Data.Hounds.Misc where

import           Data.Char             (chr, ord)
import           Data.Word
import           Foreign.Marshal.Array
import           Foreign.Ptr

stringToPtr :: String -> IO (Ptr Word8)
stringToPtr = newArray . fmap (fromIntegral . ord)

ptrToString :: Int -> Ptr Word8 -> IO String
ptrToString int ptr= fmap (chr . fromIntegral) <$> peekArray int ptr
