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

import           Data.Hounds
import           Data.Hounds.Orphans                  ()


prop_roundTrip :: Bool -> Bool
prop_roundTrip t = decode (encode t) == Right t

go :: Db -> Hash -> B.ByteString -> IO (Maybe B.ByteString)
go db hash bs = runInBoundThread $ do
  txn <- mdb_txn_begin (dbEnv db) Nothing False
  onException (do _    <- put (dbDbiLeaves db) txn (unHash hash) bs
                  mbs  <- get (dbDbiLeaves db) txn (unHash hash)
                  _    <- mdb_txn_commit txn
                  return mbs)
              (mdb_txn_abort txn)

prop_roundTripDb :: IO Db -> Property
prop_roundTripDb iodb = monadicIO $ do
  db   <- run iodb
  bs   <- pick arbitrary
  let hash = mkHash bs
  mbs  <- run (go db hash bs)
  assert (mbs == Just bs)

initDb :: IO Db
initDb = do
  dir  <- getCanonicalTemporaryDirectory
  path <- createTempDirectory dir "hounds-"
  mkDb path (1024 * 1024 * 128)

props :: [TestTree]
props =
  [ testProperty "Round trip Tree serialization" prop_roundTrip
  , withResource (runInBoundThread initDb)
                 (runInBoundThread . close)
                 (testProperty "Round trip leaves to db" . prop_roundTripDb)
  ]

tests :: TestTree
tests = testGroup "props" props

main :: IO ()
main = defaultMain tests
