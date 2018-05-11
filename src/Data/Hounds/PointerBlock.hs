module Data.Hounds.PointerBlock
  ( PointerBlock(..)
  , mkPointerBlock
  , update
  , getChildren
  ) where

import           Control.Monad    (guard)
import           Data.Array       (Array, Ix, bounds, listArray, range, (!),
                                   (//))
import           Data.Maybe       (fromJust, isJust)
import           Data.Serialize
import           Data.Word        (Word8)

import           Data.Hounds.Hash


--
-- Notes:
-- =====
-- * Use sparse array/matrix?
-- * Tag & skip bytes?
--
--   data Tag
--     = Node B.ByteString Hash
--     | Child Hash
--     | None
--

newtype PointerBlock = MkPointerBlock { unPointerBlock :: Array Word8 (Maybe Hash) }
  deriving (Eq, Show)

mkPointerBlock :: PointerBlock
mkPointerBlock = MkPointerBlock (listArray (0, 255) (replicate 256 Nothing))

putPointerBlock :: Putter PointerBlock
putPointerBlock = putIArrayOf putWord8 (putMaybeOf put) . unPointerBlock

getPointerBlock :: Get PointerBlock
getPointerBlock = MkPointerBlock <$> getIArrayOf getWord8 (getMaybeOf get)

instance Serialize PointerBlock where
  put = putPointerBlock
  get = getPointerBlock

update :: PointerBlock -> [(Word8, Maybe Hash)] -> PointerBlock
update (MkPointerBlock arr) as = MkPointerBlock (arr // as)

-- compare to [(i, fromJust x) | i <- range (bounds arr), let x = arr ! i, isJust x]
asshocs :: Ix i => Array i (Maybe a) -> [(i, a)]
asshocs arr = do
  i <- range (bounds arr)
  let x = arr ! i
  guard (isJust x)
  return (i, fromJust x)

getChildren :: PointerBlock -> [(Word8, Hash)]
getChildren = asshocs . unPointerBlock
