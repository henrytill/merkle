{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hounds.Trie where

import           Control.Concurrent.MVar  (putMVar, takeMVar, readMVar)
import           Control.Exception        (Exception, finally, onException, throwIO)
import           Control.Monad            (foldM, unless)
import           Data.Array
import qualified Data.ByteString          as B
import           Data.Foldable            (fold)
import           Data.Maybe               (isNothing)
import           Data.Monoid              (First (..))
import           Data.Proxy               (Proxy (..))
import           Data.Serialize
import           Data.Word                (Word8)
import           Database.LMDB.Raw

import qualified Data.Hounds.Context      as Context
import qualified Data.Hounds.Db           as Db
import           Data.Hounds.Hash
import           Data.Hounds.PointerBlock


data TrieException
  = InsertException String
  | LookupException String
  | DeleteException String
  | RehashException
  deriving Show

instance Exception TrieException

data Trie k v
  = Node PointerBlock
  | Leaf k v
  deriving (Eq, Show)

isNode :: Trie k v -> Bool
isNode (Node _) = True
isNode _        = False

isLeaf :: Trie k v -> Bool
isLeaf (Leaf _ _) = True
isLeaf _          = False

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

lookup :: forall k v. (Eq k, Serialize k, Serialize v) => Context.Context k v -> k -> IO (Maybe v)
lookup context k = do
  let db             = Context.contextDb context
      currentRootVar = Context.contextWorkingRoot context
      dbiTrie        = Db.dbDbiTrie db
  rootHash <- readMVar currentRootVar
  txn      <- mdb_txn_begin (Db.dbEnv db) Nothing True
  finally (do currRoot <- Db.get txn dbiTrie rootHash
              case currRoot of
                Just root -> go txn dbiTrie 0 root
                Nothing   -> return Nothing)
          (mdb_txn_abort txn)
    where
      path :: B.ByteString
      path = encode k

      go :: MDB_txn -> MDB_dbi -> Int -> Trie k v -> IO (Maybe v)
      go txn dbi depth (Node pb)
        = case unPointerBlock pb ! B.index path depth of
            Nothing   -> return Nothing
            Just hash -> do maybeNode <- Db.get txn dbi hash
                            case maybeNode of
                              Nothing   -> throwIO (LookupException "no node at hash")
                              Just next -> go txn dbi (depth + 1) next
      go _ _ _ (Leaf lk lv)
        = if k == lk
            then return (Just lv)
            else return Nothing

rehash :: forall k v. (Serialize k, Serialize v)
       => Proxy (k, v)
       -> MDB_txn
       -> MDB_dbi
       -> Hash
       -> [(Word8, Maybe Hash)]
       -> IO Hash
rehash _ _ _ newHash []
  = return newHash
rehash fant txn dbi newHash ((byte, Just oldHash):parents)
  = do (Just (Node pb)) <- Db.get txn dbi oldHash :: IO (Maybe (Trie k v))
       let updatedNode :: Trie k v = Node (update pb [(byte, Just newHash)])
           updatedNodeHash         = hashTrie updatedNode
       Db.putOrThrow txn dbi updatedNodeHash updatedNode RehashException
       rehash fant txn dbi updatedNodeHash parents
rehash fant txn dbi newHash ((byte, Nothing):parents)
  = do let updatedNode :: Trie k v = Node (update mkPointerBlock [(byte, Just newHash)])
           updatedNodeHash         = hashTrie updatedNode
       Db.putOrThrow txn dbi updatedNodeHash updatedNode RehashException
       rehash fant txn dbi updatedNodeHash parents

insert :: forall k v. (Serialize k, Serialize v) => Context.Context k v -> k -> v -> IO ()
insert context k v = do
  let db             = Context.contextDb context
      currentRootVar = Context.contextWorkingRoot context
      dbiTrie        = Db.dbDbiTrie db
  rootHash <- takeMVar currentRootVar
  txn      <- mdb_txn_begin (Db.dbEnv db) Nothing False
  onException (do currRoot <- Db.get txn dbiTrie rootHash :: IO (Maybe (Trie k v))
                  case currRoot of
                    Nothing         -> throwIO (InsertException "root must exist")
                    Just (Leaf _ _) -> throwIO (InsertException "root must be a Node")
                    Just node       -> do Db.putOrThrow txn dbiTrie newLeafHash newLeaf (InsertException "persisting newLeaf failed")
                                          -- putStrLn ""
                                          -- putStrLn ("key:          " ++ show (B.unpack (encode k)))
                                          (newNode, dirtyParents) <- inserter txn dbiTrie 0 node (B.head path, Just rootHash) []
                                          -- putStrLn ("newNode:      " ++ show newNode)
                                          -- putStrLn ("dirtyParents: " ++ show dirtyParents)
                                          newRoot <- rehash (Proxy :: Proxy (k, v)) txn dbiTrie newNode dirtyParents
                                          -- putStrLn ("newRoot:      " ++ show newRoot)
                                          mdb_txn_commit txn
                                          putMVar currentRootVar newRoot
                                          return ())
              (do mdb_txn_abort txn
                  putMVar currentRootVar rootHash)
    where
      path :: B.ByteString
      path = encode k

      newLeaf :: Trie k v
      newLeaf = Leaf k v

      newLeafHash :: Hash
      newLeafHash = hashTrie newLeaf

      inserter :: MDB_txn
               -> MDB_dbi
               -> Int
               -> Trie k v
               -> (Word8, Maybe Hash)
               -> [(Word8, Maybe Hash)]
               -> IO (Hash, [(Word8, Maybe Hash)])
      inserter txn dbi offset (Node pb) parent@(byte, _) dirtyParents
        = case unPointerBlock pb ! byte of
            Just nextHash -> do maybeNext <- Db.get txn dbi nextHash
                                case maybeNext of
                                  Nothing   -> throwIO (InsertException "(inserter) value at nextHash must exist")
                                  Just next -> let
                                                 nextOffset = offset + 1
                                                 nextByte   = B.index path nextOffset
                                               in
                                                 inserter txn dbi nextOffset next (nextByte, Just nextHash) (parent:dirtyParents)
            Nothing       -> do let newNode     = Node (update pb [(byte, Just newLeafHash)]) :: Trie k v
                                    newNodeHash = hashTrie newNode
                                Db.putOrThrow txn dbi newNodeHash newNode (InsertException "(inserter) persisting newNode failed")
                                return (newNodeHash, dirtyParents)
      inserter txn dbi offset leaf@(Leaf lk _) parent@(_, maybeCurrLeafHash) dirtyParents
        = let
            newLeafByte      = B.index path offset
            currLeafNextByte = B.index (encode lk) offset
          in
            if currLeafNextByte == newLeafByte
              then inserter txn dbi (offset + 1) leaf parent ((currLeafNextByte, Nothing):dirtyParents)
              else do let newNode     = Node (update mkPointerBlock [(currLeafNextByte, maybeCurrLeafHash), (newLeafByte, Just newLeafHash)]) :: Trie k v
                          newNodeHash = hashTrie newNode
                      Db.putOrThrow txn dbi newNodeHash newNode (InsertException "(inserter) persisting newNode failed")
                      return (newNodeHash, dirtyParents)

getParents :: forall k v. (Serialize k, Serialize v)
           => Proxy (k, v)
           -> MDB_txn
           -> MDB_dbi
           -> Hash                       -- ^ root hash
           -> k                          -- ^ key
           -> IO (Trie k v, [(Word8, Hash, Trie k v)])
getParents _ txn dbi rootHash k = do
  Just root <- Db.get txn dbi rootHash
  loop 0 root rootHash []
    where
      path :: B.ByteString
      path = encode k

      loop :: Int
           -> Trie k v
           -> Hash
           -> [(Word8, Hash, Trie k v)]
           -> IO (Trie k v, [(Word8, Hash, Trie k v)])
      loop offset n@(Node pb) currHash acc
        = let
            byte = B.index path offset
          in
            case unPointerBlock pb ! byte of
              Just nextHash -> do next <- Db.getOrThrow txn dbi nextHash (LookupException "(getParents) value at nextHash must exist")
                                  loop (offset + 1) next nextHash ((byte, currHash, n):acc)
              Nothing       -> return (n, acc)
      loop _ leaf _ acc
        = return (leaf, acc)

hasOnlyChild :: Word8 -> Trie k v -> Bool
hasOnlyChild _   (Leaf _ _) = False
hasOnlyChild idx (Node pb)  = (isNothing . getFirst . fold . fmap First) updatedPointerBlockArray
  where
    updatedPointerBlockArray :: Array Word8 (Maybe Hash)
    updatedPointerBlockArray = unPointerBlock pb // [(idx, Nothing)]

delete :: forall k v. (Serialize k, Serialize v, Eq k, Eq v) => Context.Context k v -> k -> v -> IO ()
delete context k v = do
  let db             = Context.contextDb context
      currentRootVar = Context.contextWorkingRoot context
      dbiTrie        = Db.dbDbiTrie db
  rootHash <- takeMVar currentRootVar
  txn      <- mdb_txn_begin (Db.dbEnv db) Nothing False
  onException (do (trie, dirtyParents) <- getParents (Proxy :: Proxy (k, v)) txn dbiTrie rootHash k
                  unless (trie == expectedLeaf) (throwIO (DeleteException "(delete) couldn't find expectedLeaf"))
                  (_, newRootHash) <- foldM (deleter txn dbiTrie) (True, hashTrie expectedLeaf) dirtyParents
                  mdb_txn_commit txn
                  putMVar currentRootVar newRootHash)
              (do mdb_txn_abort txn
                  putMVar currentRootVar rootHash)
    where
      expectedLeaf :: Trie k v
      expectedLeaf = Leaf k v

      deleter :: MDB_txn -> MDB_dbi -> (Bool, Hash) -> (Word8, Hash, Trie k v) -> IO (Bool, Hash)
      deleter _ _ _ (_, _, Leaf _ _)
        = throwIO (DeleteException "(delete) tried to delete a child in a Leaf")
      deleter txn dbi (latch, hash) (idx, _, node@(Node pb))
        = if latch && hasOnlyChild idx node
            then return (True, hash)
            else do let newNode     = Node (update pb [(idx, Nothing)]) :: Trie k v
                        newNodeHash = hashTrie newNode
                    succPut <- Db.put txn dbi newNodeHash newNode
                    unless succPut (throwIO (DeleteException "(delete) persisting newNode failed"))
                    return (False, newNodeHash)
