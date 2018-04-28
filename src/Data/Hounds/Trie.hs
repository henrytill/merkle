module Data.Hounds.Trie where

import           Data.Serialize

import           Data.Hounds.Hash
import           Data.Hounds.HashSuffix
import           Data.Hounds.PointerBlock


data Trie
  = Node PointerBlock
  | Leaf HashSuffix Hash
  deriving (Eq, Show)

putTrie :: Putter Trie
putTrie (Node pb)    = putWord8 0 >> putPointerBlock pb
putTrie (Leaf rp hs) = putWord8 1 >> putHashSuffix rp >> putHash hs

getTrie :: Get Trie
getTrie = do
  tag <- getWord8
  case tag of
    0 -> Node <$> getPointerBlock
    1 -> Leaf <$> getHashSuffix <*> getHash
    _ -> fail "no such tag"

instance Serialize Trie where
  put = putTrie
  get = getTrie

mkTrie :: Trie
mkTrie = Node mkPointerBlock

hashTrie :: Trie -> Hash
hashTrie (Node pb)   = hashPointerBlock pb
hashTrie (Leaf _ hs) = hs
