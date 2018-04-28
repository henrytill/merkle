module Data.Hounds.PointerBlock
  ( PointerBlock(..)
  , mkPointerBlock
  , update
  , putPointerBlock
  , getPointerBlock
  , hashPointerBlock
  ) where

import           Data.Array       (Array, listArray, (//))
import           Data.Serialize
import           Data.Word        (Word8)

import           Data.Hounds.Hash


newtype PointerBlock = MkPointerBlock { unPointerBlock :: Array Word8 (Maybe Hash) }
  deriving (Eq, Show)

mkPointerBlock :: PointerBlock
mkPointerBlock = MkPointerBlock (listArray (0, 255) (replicate 256 Nothing))

putPointerBlock :: Putter PointerBlock
putPointerBlock = putIArrayOf putWord8 (putMaybeOf putHash) . unPointerBlock

getPointerBlock :: Get PointerBlock
getPointerBlock = MkPointerBlock <$> getIArrayOf getWord8 (getMaybeOf getHash)

instance Serialize PointerBlock where
  put = putPointerBlock
  get = getPointerBlock

update :: PointerBlock -> (Word8, Maybe Hash) -> PointerBlock
update (MkPointerBlock arr) indexedHash = MkPointerBlock (arr // [indexedHash])

hashPointerBlock :: PointerBlock -> Hash
hashPointerBlock = mkHash . encode
