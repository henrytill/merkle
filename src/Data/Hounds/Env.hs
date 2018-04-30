module Data.Hounds.Env where

import           Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import           Data.Serialize
import           Data.Word               (Word64)

import qualified Data.Hounds.Db          as Db
import qualified Data.Hounds.Log         as Log


data Env k v = MkEnv
  { envDb         :: Db.Db
  , envCount      :: MVar Word64
  , envCurrentLog :: MVar [Log.LogEntry k v]
  }

mkEnv :: Db.Db -> IO (Env k v)
mkEnv db = MkEnv db <$> newMVar 0 <*> newMVar []

fetchCount :: Env k v -> IO (Maybe Word64)
fetchCount = tryTakeMVar . envCount

fetchLog :: (Serialize k, Serialize v) => Env k v -> IO (Maybe [Log.LogEntry k v])
fetchLog = tryTakeMVar . envCurrentLog
