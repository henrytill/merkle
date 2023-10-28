{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hounds.Store where

import Control.Concurrent.MVar (newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (onException)
import Control.Monad (void)
import Data.Hounds.Context qualified as Context
import Data.Hounds.Hash qualified as Hash
import Data.Hounds.Log qualified as Log
import Data.Hounds.Trie qualified as Trie
import Data.Map qualified as Map
import Data.Serialize qualified as S

put :: (Ord k) => Context.Context k v -> k -> v -> IO Bool
put context k v = do
  let storeVar = Context.contextStore context
      countVar = Context.contextCount context
      currentLogVar = Context.contextLog context
      f _ new _ = new
  store <- takeMVar storeVar
  count <- takeMVar countVar
  currLog <- takeMVar currentLogVar
  onException
    ( do
        let (old, newStore) = Map.insertLookupWithKey f k v store
        putMVar storeVar newStore
        case old of
          Nothing ->
            do
              let logEntry = Log.MkLogEntry count Log.Insert k v
              putMVar countVar (succ count)
              putMVar currentLogVar (logEntry : currLog)
              return True
          (Just _) ->
            do
              putMVar countVar count
              putMVar currentLogVar currLog
              return False
    )
    ( do
        putMVar storeVar store
        putMVar countVar count
        putMVar currentLogVar currLog
    )

del :: (Eq k, Eq v, Ord k) => Context.Context k v -> k -> IO Bool
del context k = do
  let storeVar = Context.contextStore context
      countVar = Context.contextCount context
      currentLogVar = Context.contextLog context
      f _ _ = Nothing
  store <- takeMVar storeVar
  count <- takeMVar countVar
  currLog <- takeMVar currentLogVar
  onException
    ( do
        let (maybeOldValue, newStore) = Map.updateLookupWithKey f k store
        putMVar storeVar newStore
        case maybeOldValue of
          Nothing ->
            do
              putMVar countVar count
              putMVar currentLogVar currLog
              return False
          Just oldValue ->
            do
              let logEntry = Log.MkLogEntry count Log.Delete k oldValue
              putMVar countVar (succ count)
              putMVar currentLogVar (logEntry : currLog)
              return True
    )
    ( do
        putMVar storeVar store
        putMVar countVar count
        putMVar currentLogVar currLog
    )

get :: (Ord k) => Context.Context k v -> k -> IO (Maybe v)
get context k = Map.lookup k <$> readMVar (Context.contextStore context)

checkpoint ::
  forall k v.
  (Ord k, Eq k, Eq v, S.Serialize k, S.Serialize v) =>
  Context.Context k v ->
  IO Hash.Hash
checkpoint context = do
  let logVar = Context.contextLog context
      workingRootVar = Context.contextWorkingRoot context
  logEntries <- takeMVar logVar
  mapM_ operate logEntries
  putMVar logVar []
  readMVar workingRootVar
  where
    operate :: Log.LogEntry k v -> IO ()
    operate (Log.MkLogEntry _ Log.Insert key value) = Trie.insert context key value
    operate (Log.MkLogEntry _ Log.Delete key value) = void (Trie.delete context key value)

reset :: Context.Context k v -> Hash.Hash -> IO (Context.Context k v)
reset context hash = do
  workingRootVar <- newMVar hash
  return context {Context.contextWorkingRoot = workingRootVar}
