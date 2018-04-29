module Data.Hounds.Store where

import           Control.Concurrent.MVar (putMVar, takeMVar)
import           Control.Exception       (finally, onException)
import qualified Data.Serialize          as S
import           Database.LMDB.Raw

import qualified Data.Hounds.Db          as Db
import qualified Data.Hounds.Env         as Env
import qualified Data.Hounds.Log         as Log


put :: (S.Serialize k, S.Serialize v)
    => Env.Env k v
    -> k
    -> v
    -> IO Bool
put env k v = do
  let db            = Env.envDb env
      countVar      = Env.envCount env
      currentLogVar = Env.envCurrentLog env
  txn     <- mdb_txn_begin (Db.dbEnv db) Nothing False
  count   <- takeMVar countVar
  currLog <- takeMVar currentLogVar
  onException (do succPut <- Db.put txn (Db.dbDbiStore db) k v
                  if succPut
                    then do let logEntry = Log.MkLogEntry k count Log.Insert v
                            putMVar countVar      (count + 1)
                            putMVar currentLogVar (logEntry : currLog)
                            mdb_txn_commit txn
                            return succPut
                    else do putMVar countVar      count
                            putMVar currentLogVar currLog
                            mdb_txn_commit txn
                            return succPut)
              (do mdb_txn_abort txn
                  putMVar countVar      count
                  putMVar currentLogVar currLog)

del :: (S.Serialize k, S.Serialize v)
    => Env.Env k v
    -> k
    -> IO Bool
del env k = do
  let db            = Env.envDb env
      countVar      = Env.envCount env
      currentLogVar = Env.envCurrentLog env
  txn     <- mdb_txn_begin (Db.dbEnv db) Nothing False
  count   <- takeMVar countVar
  currLog <- takeMVar currentLogVar
  onException (do maybeVal <- Db.get txn (Db.dbDbiStore db) k
                  case maybeVal of
                    Nothing -> do putMVar countVar      count
                                  putMVar currentLogVar currLog
                                  mdb_txn_commit txn
                                  return False
                    Just v  -> do succDel <- Db.del txn (Db.dbDbiStore db) k
                                  if succDel
                                    then do let logEntry = Log.MkLogEntry k count Log.Delete v
                                            putMVar countVar      (count + 1)
                                            putMVar currentLogVar (logEntry : currLog)
                                            mdb_txn_commit txn
                                            return succDel
                                    else do putMVar countVar      count
                                            putMVar currentLogVar currLog
                                            mdb_txn_commit txn
                                            return succDel)
              (do mdb_txn_abort txn
                  putMVar countVar      count
                  putMVar currentLogVar currLog)

get :: (S.Serialize k, S.Serialize v)
    => Env.Env k v
    -> k
    -> IO (Maybe v)
get env k = do
  let db = Env.envDb env
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  finally (Db.get txn (Db.dbDbiStore db) k) (mdb_txn_abort txn)
