module Data.Hounds.Context where

import           Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import           Data.Serialize
import           Data.Word               (Word64)

import qualified Data.Hounds.Db          as Db
import qualified Data.Hounds.Hash        as Hash
import qualified Data.Hounds.Log         as Log


data Context k v = MkContext
  { contextDb          :: Db.Db
  , contextCount       :: MVar Word64
  , contextLog         :: MVar [Log.LogEntry k v]
  , contextWorkingRoot :: MVar Hash.Hash
  }

mkContext :: Db.Db -> Hash.Hash -> IO (Context k v)
mkContext db hash
  = MkContext db <$> newMVar 0
                 <*> newMVar []
                 <*> newMVar hash

fetchCount :: Context k v -> IO (Maybe Word64)
fetchCount = tryTakeMVar . contextCount

fetchLog :: (Serialize k, Serialize v) => Context k v -> IO (Maybe [Log.LogEntry k v])
fetchLog = tryTakeMVar . contextLog
