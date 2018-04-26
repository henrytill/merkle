{-# LANGUAGE NamedFieldPuns #-}

module Data.Hounds.MerklePatricia.Db where

import           Control.Concurrent    (runInBoundThread)
import qualified Data.ByteString       as B
import           Database.LMDB.Raw
import           Foreign.Marshal.Array (mallocArray, peekArray, pokeArray)


data Db = MkDb
  { dbEnv          :: MDB_env
  , dbDbiLeaves    :: MDB_dbi
  , dbDbiMaster    :: MDB_dbi
  , dbDbiReversals :: MDB_dbi
  , dbDbiForwards  :: MDB_dbi
  }

mkDb :: FilePath -> IO Db
mkDb dbDir = runInBoundThread $ do
  env          <- mdb_env_create
  let mapSize = 1024 * 1024 * 100
  _            <- mdb_env_set_mapsize env mapSize
  _            <- mdb_env_set_maxreaders env 4
  _            <- mdb_env_set_maxdbs env 4
  _            <- mdb_env_open env dbDir []
  txn          <- mdb_txn_begin env Nothing False
  dbiLeaves    <- mdb_dbi_open txn (Just "leaves")    [MDB_CREATE]
  dbiMaster    <- mdb_dbi_open txn (Just "master")    [MDB_CREATE]
  dbiReversals <- mdb_dbi_open txn (Just "reversals") [MDB_CREATE]
  dbiForwards  <- mdb_dbi_open txn (Just "forwards")  [MDB_CREATE]
  _            <- mdb_txn_commit txn
  return MkDb { dbEnv          = env
              , dbDbiLeaves    = dbiLeaves
              , dbDbiMaster    = dbiMaster
              , dbDbiReversals = dbiReversals
              , dbDbiForwards  = dbiForwards
              }

mdbValToByteString :: MDB_val -> IO B.ByteString
mdbValToByteString MDB_val{mv_size, mv_data}
  = B.pack <$> peekArray (fromIntegral mv_size) mv_data

byteStringToMdbVal :: B.ByteString -> IO MDB_val
byteStringToMdbVal bs = do
  let len = B.length bs
  ptr <- mallocArray len
  _   <- pokeArray ptr (B.unpack bs)
  return MDB_val { mv_size = fromIntegral len
                 , mv_data = ptr
                 }

get :: Db -> (Db -> MDB_dbi) -> B.ByteString -> IO (Maybe B.ByteString)
get db f bs = runInBoundThread $ do
  txn <- mdb_txn_begin (dbEnv db) Nothing True
  k   <- byteStringToMdbVal bs
  ret <- mdb_get txn (f db) k
  _   <- mdb_txn_commit txn
  mapM mdbValToByteString ret

put :: Db -> (Db -> MDB_dbi) -> B.ByteString -> B.ByteString -> IO Bool
put db f key val = runInBoundThread $ do
  txn <- mdb_txn_begin (dbEnv db) Nothing False
  k   <- byteStringToMdbVal key
  v   <- byteStringToMdbVal val
  ret <- mdb_put (compileWriteFlags []) txn (f db) k v
  _   <- mdb_txn_commit txn
  return ret

close :: Db -> IO ()
close db = runInBoundThread $ do
  let env = dbEnv db
  _ <- mdb_dbi_close env (dbDbiLeaves    db)
  _ <- mdb_dbi_close env (dbDbiMaster    db)
  _ <- mdb_dbi_close env (dbDbiReversals db)
  _ <- mdb_dbi_close env (dbDbiForwards  db)
  mdb_env_close env
