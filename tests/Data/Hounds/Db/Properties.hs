module Data.Hounds.Db.Properties (dbProperties) where

import           Control.Concurrent      (runInBoundThread)
import           Control.Exception       (onException)
import qualified Data.ByteString         as B
import           Database.LMDB.Raw
import           Test.QuickCheck         (Property, arbitrary)
import qualified Test.QuickCheck.Monadic as M
import           Test.Tasty
import           Test.Tasty.QuickCheck   (testProperty)

import qualified Data.Hounds.Db          as Db
import qualified Data.Hounds.Hash        as Hash
import           Data.Hounds.Orphans     ()
import           Data.Hounds.Test


prop_roundTripDb :: IO Db.Db -> Property
prop_roundTripDb iodb = M.monadicIO $ do
  db  <- M.run iodb
  bs  <- M.pick arbitrary
  mbs <- M.run (go db (Hash.mkHash bs) bs)
  M.assert (mbs == Just bs)
  where
    go :: Db.Db -> Hash.Hash -> B.ByteString -> IO (Maybe B.ByteString)
    go db hash bs = runInBoundThread $ do
      txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
      onException (do _   <- Db.put txn (Db.dbDbiTrie db) hash bs
                      mbs <- Db.get txn (Db.dbDbiTrie db) hash
                      _   <- mdb_txn_commit txn
                      return mbs)
                  (mdb_txn_abort txn)

dbProperties :: [TestTree]
dbProperties =
  [ withResource (runInBoundThread initTempDb)
                 (runInBoundThread . Db.close)
                 (testProperty "Round trip leaves to db" . prop_roundTripDb)
  ]
