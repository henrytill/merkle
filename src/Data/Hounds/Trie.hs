module Data.Hounds.Trie where

import           Control.Exception        (finally, onException)
import qualified Data.ByteString          as B
import           Data.Serialize
import           Data.Word                (Word8)
import           Database.LMDB.Raw

import qualified Data.Hounds.Db           as Db
import qualified Data.Hounds.Env          as Env
import           Data.Hounds.Hash
import           Data.Hounds.PointerBlock


type Offset = Word8

data Trie k v
  = Node PointerBlock
  | Leaf k v
  deriving (Eq, Show)

putTrie :: (Serialize k, Serialize v) => Putter (Trie k v)
putTrie (Node pb)  = putWord8 0 >> put pb
putTrie (Leaf k v) = putWord8 1 >> put k >> put v

getTrie :: (Serialize k, Serialize v) => Get (Trie k v)
getTrie = do
  tag <- getWord8
  case tag of
    0 -> Node <$> get
    1 -> Leaf <$> get <*> get
    _ -> fail "no such tag"

instance (Serialize k, Serialize v) => Serialize (Trie k v) where
  put = putTrie
  get = getTrie

mkTrie :: (Serialize k, Serialize v) => Trie k v
mkTrie = Node mkPointerBlock

hashTrie :: (Serialize k, Serialize v) => Trie k v -> Hash
hashTrie (Node pb)  = mkHash (encode pb)
hashTrie (Leaf k v) = mkHash (B.append (encode k) (encode v))

store :: (Serialize k, Serialize v) => Env.Env k v -> Hash -> Trie k v -> IO Bool
store env hash trie = do
  let db = Env.envDb env
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  onException (do succPut <- Db.put txn (Db.dbDbiTrie db) hash trie
                  mdb_txn_commit txn
                  return succPut)
              (mdb_txn_abort txn)

fetch :: (Serialize k, Serialize v) => Env.Env k v -> Hash -> IO (Maybe (Trie k v))
fetch env hash = do
  let db = Env.envDb env
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  finally (Db.get txn (Db.dbDbiTrie db) hash)
          (mdb_txn_abort txn)

insert :: (Serialize k, Serialize v) => Env.Env k v -> k -> v -> IO ()
insert = undefined
