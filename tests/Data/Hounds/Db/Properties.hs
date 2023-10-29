module Data.Hounds.Db.Properties (dbProperties) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (onException)
import Data.ByteString qualified as B
import Data.Hounds.Db qualified as Db
import Data.Hounds.Hash qualified as Hash
import Data.Hounds.Orphans ()
import Data.Hounds.Test
import Database.LMDB.Raw
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic qualified as M
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

roundTripDb :: B.ByteString -> IO (Maybe B.ByteString)
roundTripDb bs = runInBoundThread $ do
  db <- initTempDb
  txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
  onException
    ( do
        let hash = Hash.mkHash bs
        _ <- Db.put txn (Db.dbDbi db) hash bs
        r <- Db.get txn (Db.dbDbi db) hash
        _ <- mdb_txn_commit txn
        _ <- Db.close db
        return r
    )
    ( do
        mdb_txn_abort txn
        Db.close db
    )

prop_roundTripDb :: B.ByteString -> Property
prop_roundTripDb bs = M.monadicIO $ do
  (Just ret) <- M.run $ roundTripDb bs
  M.assert (bs == ret)

dbProperties :: TestTree
dbProperties =
  testGroup
    "Db property tests"
    [ testProperty "Round trip leaves to db" prop_roundTripDb
    ]
