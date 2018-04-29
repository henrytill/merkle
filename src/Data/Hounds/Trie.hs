module Data.Hounds.Trie where

import           Data.Serialize
import           Data.Word                (Word8)

import           Data.Hounds.PointerBlock


type Offset = Word8

data Trie k v
  = Node PointerBlock
  | Leaf Offset k v
  deriving (Eq, Show)

putTrie :: (Serialize k, Serialize v) => Putter (Trie k v)
putTrie (Node pb)      = putWord8 0 >> putPointerBlock pb
putTrie (Leaf off k v) = putWord8 1 >> putWord8 off >> put k >> put v

getTrie :: (Serialize k, Serialize v) => Get (Trie k v)
getTrie = do
  tag <- getWord8
  case tag of
    0 -> Node <$> getPointerBlock
    1 -> Leaf <$> getWord8 <*> get <*> get
    _ -> fail "no such tag"

instance (Serialize k, Serialize v) => Serialize (Trie k v) where
  put = putTrie
  get = getTrie

mkTrie :: Trie k v
mkTrie = Node mkPointerBlock
