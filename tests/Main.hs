module Main (main) where

import           Control.Concurrent                   (runInBoundThread)
import           Control.Concurrent.MVar              (takeMVar)
import           Control.Exception                    (onException)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as C
import           Data.Serialize                       (decode, encode)
import           Database.LMDB.Raw
import           System.IO.Temp                       (createTempDirectory, getCanonicalTemporaryDirectory)
import           Test.QuickCheck                      (Property, arbitrary)
import           Test.QuickCheck.Instances.ByteString ()
import qualified Test.QuickCheck.Monadic              as M
import           Test.Tasty                           (TestTree, defaultMain,
                                                       testGroup, withResource)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck                (testProperty)

import qualified Data.Hounds.Db                       as Db
import qualified Data.Hounds.Hash                     as Hash
import           Data.Hounds.Orphans                  ()


-- * Property Tests

prop_roundTrip :: Bool -> Bool
prop_roundTrip t = decode (encode t) == Right t

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
      onException (do _   <- Db.put (Db.dbDbiLeaves db) txn (Hash.unHash hash) bs
                      mbs <- Db.get (Db.dbDbiLeaves db) txn (Hash.unHash hash)
                      _   <- mdb_txn_commit txn
                      return mbs)
                  (mdb_txn_abort txn)

-- * Unit Tests

twoPuts :: IO Db.Env -> Assertion
twoPuts ioenv = runInBoundThread $ do
  env   <- ioenv
  _     <- Db.loggedPut env (C.pack "one")
  _     <- Db.loggedPut env (C.pack "two")
  count <- takeMVar (Db.envTxnCount env)
  assertEqual "the transaction count did not equal 2" 2 count

-- * Setup

initTempDb :: IO Db.Db
initTempDb = do
  dir  <- getCanonicalTemporaryDirectory
  path <- createTempDirectory dir "hounds-"
  Db.mkDb path (1024 * 1024 * 128)

initTempEnv :: IO Db.Env
initTempEnv = Db.mkEnv =<< initTempDb

props :: [TestTree]
props =
  [ testProperty "Round trip Tree serialization" prop_roundTrip
  , withResource (runInBoundThread initTempDb)
                 (runInBoundThread . Db.close)
                 (testProperty "Round trip leaves to db" . prop_roundTripDb)
  ]

units :: [TestTree]
units =
  [ withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Db.envDb)
                 (testCase "two puts" . twoPuts)
  ]

tests :: TestTree
tests = testGroup "tests" [testGroup "props" props, testGroup "units" units]

main :: IO ()
main = defaultMain tests
