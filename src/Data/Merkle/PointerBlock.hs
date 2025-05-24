module Data.Merkle.PointerBlock
  ( bounds
  , PointerBlock (..)
  , mkPointerBlock
  , fillPointerBlock
  , index
  , update
  , getChildren
  )
where

import Data.Array (Array)
import Data.Array.IArray ((!), (//))
import Data.Array.IArray qualified as IArray
import Data.Ix (Ix)
import Data.Ix qualified as Ix
import Data.Merkle.Hash
import Data.Serialize
import Data.Word (Word8)

--
-- Notes:
-- =====

-- * Use skip blocks?

-- * Possible Alternate encoding:

--
--   data PointerBlockHash
--     = LeafHash Hash
--     | NodeHash Hash
--     | Nothing
--
--   newtype PointerBlock = MkPointerBlock { unPointerBlock :: IntMap PointerBlockHash }
--
-- (Using `PointerBlockHash` saves us a trip to the database to fetch the children)
--

bounds :: (Word8, Word8)
bounds = (0, 255)

newtype PointerBlock = MkPointerBlock {unPointerBlock :: Array Word8 (Maybe Hash)}
  deriving (Eq, Show)

mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray f bnds = IArray.array bnds [(i, f i) | i <- Ix.range bnds]

mkPointerBlock :: PointerBlock
mkPointerBlock = MkPointerBlock (mkArray (const Nothing) bounds)

fillPointerBlock :: Maybe Hash -> PointerBlock
fillPointerBlock maybeHash = MkPointerBlock (mkArray (const maybeHash) bounds)

putPointerBlock :: Putter PointerBlock
putPointerBlock = mapM_ (putMaybeOf put) . IArray.elems . unPointerBlock

getArrayOf :: (Ix i) => (i, i) -> Get a -> Get (Array i a)
getArrayOf bnds m = IArray.listArray bnds <$> go [] (Ix.rangeSize bnds)
  where
    go as 0 = return (reverse as)
    go as i = do
      x <- m
      x `seq` go (x : as) (i - 1)

getPointerBlock :: Get PointerBlock
getPointerBlock = MkPointerBlock <$> getArrayOf bounds (getMaybeOf get)

instance Serialize PointerBlock where
  put = putPointerBlock
  get = getPointerBlock

index :: PointerBlock -> Word8 -> Maybe Hash
index (MkPointerBlock arr) i = arr ! i

update :: PointerBlock -> [(Word8, Maybe Hash)] -> PointerBlock
update (MkPointerBlock curr) as = MkPointerBlock (curr // as)

getChildren :: PointerBlock -> [(Word8, Hash)]
getChildren = foldr f [] . IArray.assocs . unPointerBlock
  where
    f (idx, Just p) acc = (idx, p) : acc
    f (_, Nothing) acc = acc
