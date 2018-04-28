module Data.Hounds.Tree where

import           Data.Serialize

import           Data.Hounds.Hash
import           Data.Hounds.HashSuffix
import           Data.Hounds.PointerBlock


data Tree
  = Node PointerBlock
  | Leaf HashSuffix Hash
  deriving (Eq, Show)

putTree :: Putter Tree
putTree (Node pb)    = putWord8 0 >> putPointerBlock pb
putTree (Leaf rp hs) = putWord8 1 >> putHashSuffix rp >> putHash hs

getTree :: Get Tree
getTree = do
  tag <- getWord8
  case tag of
    0 -> Node <$> getPointerBlock
    1 -> Leaf <$> getHashSuffix <*> getHash
    _ -> fail "no such tag"

instance Serialize Tree where
  put = putTree
  get = getTree

mkTree :: Tree
mkTree = Node mkPointerBlock

hashTree :: Tree -> Hash
hashTree (Node pb)   = hashPointerBlock pb
hashTree (Leaf _ hs) = hs
