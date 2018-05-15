module Data.Hounds.PointerBlock
  ( PointerBlock(..)
  , mkPointerBlock
  , update
  , getChildren
  ) where

import           Control.Monad         (guard)
import           Data.Maybe            (fromJust, isJust)
import qualified Data.Sequence         as Seq
import           Data.Serialize
import           Data.Serialize.Extras
import           Data.Word             (Word8)

import           Data.Hounds.Hash

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

newtype PointerBlock = MkPointerBlock { unPointerBlock :: Seq.Seq (Maybe Hash) }
  deriving (Eq, Show)

mkPointerBlock :: PointerBlock
mkPointerBlock = MkPointerBlock (Seq.replicate 256 Nothing)

putPointerBlock :: Putter PointerBlock
putPointerBlock = putFixedLengthSeqOf (putMaybeOf put) . unPointerBlock

getPointerBlock :: Get PointerBlock
getPointerBlock = MkPointerBlock <$> getFixedLengthSeqOf 256 (getMaybeOf get)

instance Serialize PointerBlock where
  put = putPointerBlock
  get = getPointerBlock

update :: PointerBlock -> [(Word8, Maybe Hash)] -> PointerBlock
update (MkPointerBlock curr) as = MkPointerBlock new
  where
    new = foldl (\ acc (idx, a) -> Seq.update (fromIntegral idx) a acc) curr as

asshocs :: Seq.Seq (Maybe a) -> [(Word8, a)]
asshocs curr = do
  i <- [0..length curr - 1]
  let x = Seq.index curr i
  guard (isJust x)
  return (fromIntegral i, fromJust x)

getChildren :: PointerBlock -> [(Word8, Hash)]
getChildren = asshocs . unPointerBlock
