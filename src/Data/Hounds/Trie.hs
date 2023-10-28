{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Hounds.Trie where

import Control.Concurrent.MVar (putMVar, readMVar, takeMVar)
import Control.Exception (Exception, finally, onException, throw, throwIO)
import Control.Monad (foldM)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Hounds.Context qualified as Context
import Data.Hounds.Db qualified as Db
import Data.Hounds.Hash
import Data.Hounds.PointerBlock
import Data.Serialize
import Data.Word (Word8)
import Database.LMDB.Raw

data TrieException
  = InsertException String
  | LookupException String
  | DeleteException String
  | RehashException
  deriving (Show)

instance Exception TrieException

data Trie k v
  = Leaf k v
  | Node PointerBlock
  deriving (Eq)

instance (Show k, Show v) => Show (Trie k v) where
  show (Leaf k v) = "Leaf " ++ show k ++ " " ++ show v
  show (Node pb) = "Node " ++ show (getChildren pb)

isNode :: Trie k v -> Bool
isNode (Node _) = True
isNode _ = False

isLeaf :: Trie k v -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

putTrie :: (Serialize k, Serialize v) => Putter (Trie k v)
putTrie (Leaf k v) = putWord8 0 >> put k >> put v
putTrie (Node pb) = putWord8 1 >> put pb

getTrie :: (Serialize k, Serialize v) => Get (Trie k v)
getTrie = do
  tag <- getWord8
  case tag of
    0 -> Leaf <$> get <*> get
    1 -> Node <$> get
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
  onException
    ( do
        succPut <- Db.put txn (Db.dbDbiTrie db) hash trie
        mdb_txn_commit txn
        return succPut
    )
    (mdb_txn_abort txn)

lookup :: forall k v. (Eq k, Serialize k, Serialize v) => Context.Context k v -> k -> IO (Maybe v)
lookup context k = do
  let db = Context.contextDb context
      currentRootVar = Context.contextWorkingRoot context
      dbiTrie = Db.dbDbiTrie db
  rootHash <- readMVar currentRootVar
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing True
  finally
    ( do
        currRoot <- Db.get txn dbiTrie rootHash
        case currRoot of
          Just root -> go txn dbiTrie 0 root
          Nothing -> return Nothing
    )
    (mdb_txn_abort txn)
  where
    path :: B.ByteString
    path = encode k

    go :: MDB_txn -> MDB_dbi -> Int -> Trie k v -> IO (Maybe v)
    go txn dbi depth (Node pb) =
      case index pb (fromIntegral (B.index path depth)) of
        Nothing -> return Nothing
        Just hash -> do
          maybeNode <- Db.get txn dbi hash
          case maybeNode of
            Nothing -> throwIO (LookupException "no node at hash")
            Just next -> go txn dbi (succ depth) next
    go _ _ _ (Leaf lk lv) =
      if k == lk
        then return (Just lv)
        else return Nothing

getParents ::
  forall k v.
  (Serialize k, Serialize v) =>
  MDB_txn ->
  MDB_dbi ->
  B.ByteString ->
  Int ->
  Trie k v ->
  [(Word8, Trie k v)] ->
  IO (Trie k v, [(Word8, Trie k v)])
getParents txn dbi path offset curr@(Node pb) acc =
  case index pb (fromIntegral byte) of
    Just nextHash ->
      do
        next <- Db.getOrThrow txn dbi nextHash (LookupException "(getParents) value at nextHash must exist")
        getParents txn dbi path (succ offset) next ((byte, curr) : acc)
    Nothing ->
      return (curr, acc)
  where
    byte = B.index path offset
getParents _ _ _ _ leaf acc =
  return (leaf, acc)

commonPrefix :: B.ByteString -> B.ByteString -> [Word8]
commonPrefix a b = map fst . takeWhile (uncurry (==)) $ B.zip a b

rehash ::
  forall k v.
  (Serialize k, Serialize v) =>
  Trie k v ->
  [(Word8, Trie k v)] ->
  [(Hash, Trie k v)]
rehash trie = scanl f (hashTrie trie, trie)
  where
    f (lastHash, _) (offset, Node pb) =
      let node :: Trie k v = Node $ update pb [(offset, Just lastHash)]
       in (hashTrie node, node)
    f _ _ = throw RehashException

insertTrie :: forall k v. (Serialize k, Serialize v) => MDB_txn -> MDB_dbi -> [(Hash, Trie k v)] -> IO Hash
insertTrie txn dbi = foldM inserter (mkHash (C.pack "initial"))
  where
    inserter :: Hash -> (Hash, Trie k v) -> IO Hash
    inserter _ (hash, trie) = Db.putOrThrow txn dbi hash trie exception >> return hash
    exception = InsertException "(insertTrie) could not insert"

insert :: forall k v. (Serialize k, Serialize v, Eq k, Eq v) => Context.Context k v -> k -> v -> IO ()
insert context k v = do
  let db = Context.contextDb context
      currentRootVar = Context.contextWorkingRoot context
      dbiTrie = Db.dbDbiTrie db
  rootHash <- takeMVar currentRootVar
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  onException
    ( do
        (Just currRoot) <- Db.get txn dbiTrie rootHash :: IO (Maybe (Trie k v))
        -- First, we create the new leaf, hash it, and persist it
        let newLeaf = Leaf k v
            newLeafHash = hashTrie newLeaf
            encodedKeyNew = encode k
        Db.putOrThrow txn dbiTrie newLeafHash newLeaf (InsertException "persisting newLeaf failed")
        -- Now, we collect a list of our new leaf's existing parents
        (tip, parents) <- getParents txn dbiTrie encodedKeyNew 0 currRoot []
        case tip of
          existingLeaf@(Leaf _ _) | existingLeaf == newLeaf ->
            do
              mdb_txn_abort txn
              putMVar currentRootVar rootHash
          existingLeaf@(Leaf ek _) ->
            do
              let encodedKeyExisting = encode ek
                  sharedPrefix = commonPrefix encodedKeyNew encodedKeyExisting
                  sharedPrefixLength = length sharedPrefix
                  sharedPath = reverse (drop (length parents) sharedPrefix)
                  newLeafIndex = B.index encodedKeyNew sharedPrefixLength
                  existingLeafIndex = B.index encodedKeyExisting sharedPrefixLength
                  newPointerBlock = [(newLeafIndex, Just newLeafHash), (existingLeafIndex, Just (hashTrie existingLeaf))]
                  hd = Node $ update mkPointerBlock newPointerBlock
                  empty = Node mkPointerBlock
                  emptys = fmap (,empty) sharedPath
                  nodes = emptys ++ parents
                  rehashedNodes = rehash hd nodes
              newRootHash <- insertTrie txn dbiTrie rehashedNodes
              mdb_txn_commit txn
              putMVar currentRootVar newRootHash
          Node pb ->
            do
              let pathLength = length parents
                  newLeafIndex = B.index encodedKeyNew pathLength
                  hd = Node $ update pb [(newLeafIndex, Just newLeafHash)]
                  nodes = parents
                  rehashedNodes = rehash hd nodes
              newRootHash <- insertTrie txn dbiTrie rehashedNodes
              mdb_txn_commit txn
              putMVar currentRootVar newRootHash
    )
    (mdb_txn_abort txn >> putMVar currentRootVar rootHash)

deleteLeaf ::
  forall k v.
  (Serialize k, Serialize v) =>
  MDB_txn ->
  MDB_dbi ->
  [(Word8, Trie k v)] ->
  IO (Trie k v, [(Word8, Trie k v)])
deleteLeaf _ _ [(byte, Node pb)] =
  return (Node $ update pb [(byte, Nothing)], [])
deleteLeaf txn dbi ((byte, Node pb) : tl) =
  case getChildren pb of
    [] -> throwIO (DeleteException "(deleteLeaf) no children")
    [_] -> deleteLeaf txn dbi tl
    c@[_, _] -> do
      otherHash <- getOtherHash byte c
      otherNode :: Trie k v <- Db.getOrThrow txn dbi otherHash (DeleteException "(deleteLeaf) could not get otherHash")
      case otherNode of
        Node _ -> return updated
        Leaf _ _ -> propagateLeafUpward txn dbi otherHash tl
    _ -> return updated
  where
    updated = (Node $ update pb [(byte, Nothing)], tl)
    getOtherHash b c = case [child | (cByte, child) <- c, cByte /= b] of
      [x] -> return x
      _ -> throwIO (DeleteException "(deleteLeaf) could not produce otherHash")
deleteLeaf _ _ _ =
  throwIO (DeleteException "(deleteLeaf) shit happened")

propagateLeafUpward ::
  forall k v.
  (Serialize k, Serialize v) =>
  MDB_txn ->
  MDB_dbi ->
  Hash ->
  [(Word8, Trie k v)] ->
  IO (Trie k v, [(Word8, Trie k v)])
propagateLeafUpward _ _ hash [(byte, Node pb)] =
  return (Node $ update pb [(byte, Just hash)], [])
propagateLeafUpward txn dbi hash ((byte, Node pb) : tl) =
  case getChildren pb of
    [] -> throwIO (DeleteException "(propagateLeafUpward) no Children")
    [_] -> propagateLeafUpward txn dbi hash tl
    _ -> return (Node $ update pb [(byte, Just hash)], tl)
propagateLeafUpward _ _ _ _ =
  throwIO (DeleteException "(propagateLeafUpward) shit happened")

delete :: forall k v. (Serialize k, Serialize v, Eq k, Eq v) => Context.Context k v -> k -> v -> IO Bool
delete context k v = do
  let db = Context.contextDb context
      currentRootVar = Context.contextWorkingRoot context
      dbiTrie = Db.dbDbiTrie db
      expectedLeaf = Leaf k v
  rootHash <- takeMVar currentRootVar
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  onException
    ( do
        (Just node) <- Db.get txn dbiTrie rootHash :: IO (Maybe (Trie k v))
        (trie, dirtyParents) <- getParents txn dbiTrie (encode k) 0 node []
        case trie of
          Node pb | null (getChildren pb) ->
            do
              mdb_txn_abort txn
              putMVar currentRootVar rootHash
              return False
          _ | trie == expectedLeaf ->
            do
              (hd, nodesToRehash) <- deleteLeaf txn dbiTrie dirtyParents
              let rehashedNodes = rehash hd nodesToRehash
              newRootHash <- insertTrie txn dbiTrie rehashedNodes
              mdb_txn_commit txn
              putMVar currentRootVar newRootHash
              return True
          _ ->
            do
              mdb_txn_abort txn
              putMVar currentRootVar rootHash
              return False
    )
    (mdb_txn_abort txn >> putMVar currentRootVar rootHash)
