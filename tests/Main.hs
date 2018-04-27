module Main (main) where

import           Control.Concurrent                   (runInBoundThread)
import           Control.Exception                    (onException)
import qualified Data.ByteString                      as B
import           Data.Serialize                       (decode, encode)
import           Database.LMDB.Raw
import           System.IO.Temp                       (createTempDirectory, getCanonicalTemporaryDirectory)
import           Test.QuickCheck                      (Property, arbitrary)
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Monadic              (assert, monadicIO, pick,
                                                       run)
import           Test.Tasty                           (TestTree, defaultMain,
                                                       testGroup, withResource)
import           Test.Tasty.QuickCheck                (testProperty)

import qualified Data.Hounds.Db                       as Db
import qualified Data.Hounds.Hash                     as Hash
import           Data.Hounds.Orphans                  ()


prop_roundTrip :: Bool -> Bool
prop_roundTrip t = decode (encode t) == Right t

prop_roundTripDb :: IO Db.Db -> Property
prop_roundTripDb iodb = monadicIO $ do
  db  <- run iodb
  bs  <- pick arbitrary
  mbs <- run (go db (Hash.mkHash bs) bs)
  assert (mbs == Just bs)
  where
    go :: Db.Db -> Hash.Hash -> B.ByteString -> IO (Maybe B.ByteString)
    go db hash bs = runInBoundThread $ do
      txn <- mdb_txn_begin (Db.dbEnv db) Nothing False
      onException (do _   <- Db.put (Db.dbDbiLeaves db) txn (Hash.unHash hash) bs
                      mbs <- Db.get (Db.dbDbiLeaves db) txn (Hash.unHash hash)
                      _   <- mdb_txn_commit txn
                      return mbs)
                  (mdb_txn_abort txn)

initTempDb :: IO Db.Db
initTempDb = do
  dir  <- getCanonicalTemporaryDirectory
  path <- createTempDirectory dir "hounds-"
  Db.mkDb path (1024 * 1024 * 128)

props :: [TestTree]
props =
  [ testProperty "Round trip Tree serialization" prop_roundTrip
  , withResource (runInBoundThread initTempDb)
                 (runInBoundThread . Db.close)
                 (testProperty "Round trip leaves to db" . prop_roundTripDb)
  ]

tests :: TestTree
tests = testGroup "props" props

main :: IO ()
main = defaultMain tests
