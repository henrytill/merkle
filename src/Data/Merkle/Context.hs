module Data.Merkle.Context where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Merkle.Db qualified as Db
import Data.Merkle.Hash qualified as Hash

data Context k v = MkContext
  { contextDb :: Db.Db,
    contextWorkingRoot :: MVar Hash.Hash
  }

mkContext :: Db.Db -> Hash.Hash -> IO (Context k v)
mkContext db hash = MkContext db <$> newMVar hash

fetchWorkingRoot :: Context k v -> IO Hash.Hash
fetchWorkingRoot = readMVar . contextWorkingRoot

setWorkingRoot :: Context k v -> Hash.Hash -> IO ()
setWorkingRoot context = modifyMVar_ (contextWorkingRoot context) . const . pure

close :: Context k v -> IO ()
close = Db.close . contextDb
