{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hounds.Store where

import           Control.Concurrent.MVar (putMVar, readMVar, takeMVar)
import           Control.Exception       (finally, onException)
import qualified Data.Serialize          as S
import           Database.LMDB.Raw

import qualified Data.Hounds.Context     as Context
import qualified Data.Hounds.Db          as Db
import qualified Data.Hounds.Hash        as Hash
import qualified Data.Hounds.Log         as Log
import qualified Data.Hounds.Trie        as Trie


put :: (S.Serialize k, S.Serialize v)
    => Context.Context k v
    -> k
    -> v
    -> IO Bool
put context k v = do
  let db            = Context.contextDb    context
      countVar      = Context.contextCount context
      currentLogVar = Context.contextLog   context
  txn     <- mdb_txn_begin (Db.dbEnv db) Nothing False
  count   <- takeMVar countVar
  currLog <- takeMVar currentLogVar
  onException (do succPut <- Db.put txn (Db.dbDbiStore db) k v
                  if succPut
                    then do let logEntry = Log.MkLogEntry count Log.Insert k v
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
    => Context.Context k v
    -> k
    -> IO Bool
del context k = do
  let db            = Context.contextDb    context
      countVar      = Context.contextCount context
      currentLogVar = Context.contextLog   context
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
                                    then do let logEntry = Log.MkLogEntry count Log.Delete k v
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

get :: forall k v. (S.Serialize k, S.Serialize v)
    => Context.Context k v
    -> k
    -> IO (Maybe v)
get context k = do
  let db = Context.contextDb context
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  finally (Db.get txn (Db.dbDbiStore db) k) (mdb_txn_abort txn)

checkpoint :: forall k v. (S.Serialize k, S.Serialize v, Eq k, Eq v, Show k, Show v)
           => Context.Context k v
           -> IO Hash.Hash
checkpoint context = do
  let logVar         = Context.contextLog context
      workingRootVar = Context.contextWorkingRoot context
  logEntries <- takeMVar logVar
  mapM_ operate logEntries
  putMVar logVar []
  readMVar workingRootVar
    where
      operate :: Log.LogEntry k v -> IO ()
      operate (Log.MkLogEntry _ Log.Insert key value) = Trie.insert context key value
      operate (Log.MkLogEntry _ Log.Delete key value) = Trie.delete context key value
