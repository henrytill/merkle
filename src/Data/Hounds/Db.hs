{-# LANGUAGE NamedFieldPuns #-}

module Data.Hounds.Db where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception       (onException)
import qualified Data.ByteString         as B
import qualified Data.Serialize          as S
import           Data.Word               (Word64)
import           Database.LMDB.Raw
import           Foreign.Marshal.Array   (mallocArray, peekArray, pokeArray)

import qualified Data.Hounds.Hash        as Hash
import qualified Data.Hounds.LeafValue   as LeafValue
import qualified Data.Hounds.Log         as Log


data Db = MkDb
  { dbEnv       :: MDB_env
  , dbDbiLeaves :: MDB_dbi
  , dbDbiTrie   :: MDB_dbi
  , dbDbiLog    :: MDB_dbi
  }

mkDb :: FilePath -> Int -> IO Db
mkDb dbDir mapSize = do
  env <- mdb_env_create
  mdb_env_set_mapsize env mapSize
  mdb_env_set_maxreaders env 4
  mdb_env_set_maxdbs env 4
  mdb_env_open env dbDir []
  txn <- mdb_txn_begin env Nothing False
  onException (do dbiLeaves <- mdb_dbi_open txn (Just "leaves") [MDB_CREATE]
                  dbiTrie   <- mdb_dbi_open txn (Just "trie")   [MDB_CREATE]
                  dbiLog    <- mdb_dbi_open txn (Just "log")    [MDB_CREATE]
                  mdb_txn_commit txn
                  return MkDb { dbEnv         = env
                              , dbDbiLeaves   = dbiLeaves
                              , dbDbiTrie     = dbiTrie
                              , dbDbiLog      = dbiLog
                              })
              (mdb_txn_abort txn)

data Env = MkEnv
  { envDb       :: Db
  , envTxnCount :: MVar Word64
  }

mkEnv :: Db -> IO Env
mkEnv db = MkEnv db <$> newMVar 0

emptyWriteFlags :: MDB_WriteFlags
emptyWriteFlags = compileWriteFlags []

mdbValToByteString :: MDB_val -> IO B.ByteString
mdbValToByteString MDB_val{mv_size, mv_data}
  = B.pack <$> peekArray (fromIntegral mv_size) mv_data

byteStringToMdbVal :: B.ByteString -> IO MDB_val
byteStringToMdbVal bs = do
  let len = B.length bs
  ptr <- mallocArray len
  pokeArray ptr (B.unpack bs)
  return MDB_val { mv_size = fromIntegral len
                 , mv_data = ptr
                 }

get :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> IO (Maybe B.ByteString)
get dbi txn bs = do
  key <- byteStringToMdbVal bs
  ret <- mdb_get txn dbi key
  mapM mdbValToByteString ret

del :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> IO Bool
del dbi txn bs = do
  key <- byteStringToMdbVal bs
  mdb_del txn dbi key Nothing

put :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> B.ByteString
    -> IO Bool
put dbi txn kbs vbs = do
  key <- byteStringToMdbVal kbs
  val <- byteStringToMdbVal vbs
  mdb_put emptyWriteFlags txn dbi key val

close :: Db -> IO ()
close db = do
  let env = dbEnv db
  mdb_dbi_close env (dbDbiLeaves db)
  mdb_dbi_close env (dbDbiTrie   db)
  mdb_dbi_close env (dbDbiLog    db)
  mdb_env_close env

withTxn :: Num i
        => MVar i
        -> MDB_txn
        -> (i -> MDB_txn -> IO a)
        -> IO a
withTxn txnCountVar txn f =
  onException (do count <- takeMVar txnCountVar
                  ret   <- f count txn
                  mdb_txn_commit txn
                  putMVar txnCountVar (count + 1)
                  return ret)
              (mdb_txn_abort txn)

loggedPut :: Env -> B.ByteString -> IO Bool
loggedPut env bs = do
  let db = envDb env
  t <- mdb_txn_begin (dbEnv db) Nothing False
  withTxn (envTxnCount env) t $ \ count txn -> do
    let hash      = Hash.mkHash bs
        logKey    = Log.MkLogKey hash (fromIntegral count)
        leafValue = LeafValue.MkLeafValue False bs
    b1 <- put (dbDbiLeaves db) txn (S.encode hash)   (S.encode leafValue)
    b2 <- put (dbDbiLog    db) txn (S.encode logKey) (S.encode Log.Insert)
    return (b1 && b2)

loggedDel :: Env -> Hash.Hash -> IO Bool
loggedDel env hash = do
  let db = envDb env
  t <- mdb_txn_begin (dbEnv db) Nothing False
  withTxn (envTxnCount env) t $ \ count txn -> do
    let logKey = Log.MkLogKey hash (fromIntegral count)
    b1 <- del (dbDbiLeaves db) txn (S.encode hash)
    b2 <- put (dbDbiLog    db) txn (S.encode logKey) (S.encode Log.Delete)
    return (b1 && b2)