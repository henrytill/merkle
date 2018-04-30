module Data.Hounds.Trie where

import           Control.Concurrent.MVar  (takeMVar, putMVar)
import           Control.Exception        (Exception, finally, onException,
                                           throwIO)
import           Control.Monad            (unless)
import           Data.Array
import qualified Data.ByteString          as B
import           Data.Serialize
import           Data.Word                (Word8)
import           Database.LMDB.Raw

import qualified Data.Hounds.Context      as Context
import qualified Data.Hounds.Db           as Db
import           Data.Hounds.Hash
import           Data.Hounds.PointerBlock


data TrieException
  = InsertException
  | InvariantViolationException String
  deriving Show

instance Exception TrieException

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

mkTrie :: (Serialize k, Serialize v) => PointerBlock -> Trie k v
mkTrie = Node

hashTrie :: (Serialize k, Serialize v) => Trie k v -> Hash
hashTrie = mkHash . encode

store :: (Serialize k, Serialize v) => Context.Context k v -> Hash -> Trie k v -> IO Bool
store context hash trie = do
  let db = Context.contextDb context
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  onException (do succPut <- Db.put txn (Db.dbDbiTrie db) hash trie
                  mdb_txn_commit txn
                  return succPut)
              (mdb_txn_abort txn)

fetch :: (Serialize k, Serialize v) => Context.Context k v -> Hash -> IO (Maybe (Trie k v))
fetch context hash = do
  let db = Context.contextDb context
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  finally (Db.get txn (Db.dbDbiTrie db) hash)
          (mdb_txn_abort txn)

rehash :: (Serialize k, Serialize v) => Context.Context k v -> Hash -> [Hash] -> IO Hash
rehash = undefined

inserter :: (Serialize k, Serialize v)
         => Context.Context k v
         -> k
         -> v
         -> B.ByteString
         -> Int
         -> Trie k v
         -> [Hash]
         -> IO (Hash, [Hash])
inserter context k v sk depth (Node pb) dirtyParents
  = do let pos       = B.index sk depth
           maybeHash = unPointerBlock pb ! pos
       case maybeHash of
         Just hash -> do (Just next) <- fetch context hash
                         inserter context k v sk (depth + 1) next (hash:dirtyParents)
         Nothing   -> do let newLeaf     = Leaf k v
                             newLeafHash = hashTrie newLeaf
                             newNode     = mkTrie (update mkPointerBlock [(pos, Just newLeafHash)])
                             newNodeHash = hashTrie newNode
                         succPutNewLeaf <- store context newLeafHash newLeaf
                         succPutNewNode <- store context newNodeHash newNode
                         unless succPutNewLeaf (throwIO InsertException)
                         unless succPutNewNode (throwIO InsertException)
                         unless succPutNewLeaf (throwIO InsertException)
                         return (newNodeHash, dirtyParents)
inserter context k v sk depth leaf@(Leaf lk _) dirtyParents
  = do let leafPos     = B.index (encode lk) (depth + 1)
           leafHash    = hashTrie leaf
           newLeaf     = Leaf k v
           newLeafPos  = B.index sk (depth + 1)
           newLeafHash = hashTrie newLeaf
           newNode     = mkTrie (update mkPointerBlock [(leafPos, Just leafHash), (newLeafPos, Just newLeafHash)])
           newNodeHash = hashTrie newNode
       succPutNewLeaf <- store context newLeafHash newLeaf
       succPutNewNode <- store context newNodeHash newNode
       unless succPutNewLeaf (throwIO InsertException)
       unless succPutNewNode (throwIO InsertException)
       return (newNodeHash, dirtyParents)

insert :: (Serialize k, Serialize v) => Context.Context k v -> k -> v -> IO ()
insert context k v = do
  let currentRootVar = Context.contextWorkingRoot context
  hash            <- takeMVar currentRootVar
  (Just currRoot) <- fetch context hash
  case currRoot of
    Leaf _ _ -> throwIO (InvariantViolationException "root must be a Node")
    node     -> onException (do (newNode, path) <- inserter context k v (encode k) 0 node []
                                newRoot         <- rehash context newNode path
                                putMVar currentRootVar newRoot
                                return ())
                            (putMVar currentRootVar hash)
