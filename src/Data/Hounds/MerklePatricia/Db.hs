module Data.Hounds.MerklePatricia.Db where

import           Database.LMDB.Raw


data Db = MkDb { dbEnv :: MDB_env, dbDbi :: MDB_dbi }

mkDb :: FilePath -> IO Db
mkDb dbDir = do
  env <- mdb_env_create
  let mapSize = 1024 * 1024 * 100
  _   <- mdb_env_set_mapsize env mapSize
  _   <- mdb_env_set_maxreaders env 4
  _   <- mdb_env_set_maxdbs env 1
  _   <- mdb_env_open env dbDir []
  txn <- mdb_txn_begin env Nothing False
  dbi <- mdb_dbi_open txn (Just "db") []
  _   <- mdb_txn_commit txn
  return MkDb { dbEnv = env, dbDbi = dbi }
