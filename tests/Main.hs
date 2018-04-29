module Main (main) where

import           Control.Concurrent                   (runInBoundThread)
import           Control.Concurrent.MVar              (takeMVar)
import           Control.Exception                    (onException)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as C
import           Data.Serialize                       (Serialize, decode,
                                                       encode)
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
import qualified Data.Hounds.Env                      as Env
import qualified Data.Hounds.Hash                     as Hash
import qualified Data.Hounds.Log                      as Log
import           Data.Hounds.Orphans                  ()
import qualified Data.Hounds.Store                    as Store


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
      onException (do _   <- Db.put txn (Db.dbDbiStore db) hash bs
                      mbs <- Db.get txn (Db.dbDbiStore db) hash
                      _   <- mdb_txn_commit txn
                      return mbs)
                  (mdb_txn_abort txn)

-- * Unit Tests

putGet :: IO (Env.Env B.ByteString B.ByteString) -> Assertion
putGet ioenv = runInBoundThread $ do
  let key = C.pack "one"
      val = C.pack "val"
  env        <- ioenv
  _          <- Store.put env key val
  (Just ret) <- Store.get env key
  assertEqual "round" val ret

twoPuts :: IO (Env.Env B.ByteString B.ByteString) -> Assertion
twoPuts ioenv = runInBoundThread $ do
  let key1 = C.pack "one"
      val1 = C.pack "val"
      key2 = C.pack "two"
      val2 = C.pack "val"
  env   <- ioenv
  _     <- Store.put env key1 val1
  _     <- Store.put env key2 val2
  count <- takeMVar (Env.envCount env)
  assertEqual "the transaction count did not equal 2" 2 count

duplicatePuts :: IO (Env.Env B.ByteString B.ByteString) -> Assertion
duplicatePuts ioenv = runInBoundThread $ do
  let key = C.pack "one"
      val = C.pack "val"
  env   <- ioenv
  _     <- Store.put env key val
  _     <- Store.put env key val
  count <- takeMVar (Env.envCount env)
  assertEqual "the transaction count did not equal 1" 1 count

twoPutsOneDelete :: IO (Env.Env B.ByteString B.ByteString) -> Assertion
twoPutsOneDelete ioenv = runInBoundThread $ do
  let key1 = C.pack "one"
      val1 = C.pack "val"
      key2 = C.pack "two"
      val2 = C.pack "val"
  env   <- ioenv
  _     <- Store.put env key1 val1
  _     <- Store.put env key2 val2
  _     <- Store.del env key1
  count <- takeMVar (Env.envCount env)
  assertEqual "the transaction count did not equal 3" 3 count

onePutTwoDeletes :: IO (Env.Env B.ByteString B.ByteString) -> Assertion
onePutTwoDeletes ioenv = runInBoundThread $ do
  let key = C.pack "one"
      val = C.pack "val"
  env   <- ioenv
  _     <- Store.put env key val
  _     <- Store.del env key
  _     <- Store.del env key
  count <- takeMVar (Env.envCount env)
  assertEqual "the transaction count did not equal 2" 2 count

twoPutsFetchLog :: IO (Env.Env B.ByteString B.ByteString) -> Assertion
twoPutsFetchLog ioenv = runInBoundThread $ do
  let key1     = C.pack "one"
      val1     = C.pack "val"
      key2     = C.pack "two"
      val2     = C.pack "val"
      expected = [ Log.MkLogEntry key1 0 Log.Insert val1
                 , Log.MkLogEntry key2 1 Log.Insert val2
                 ]
  env       <- ioenv
  _         <- Store.put env key1 val1
  _         <- Store.put env key2 val2
  (Just lg) <- Env.fetchLog env
  assertEqual "the log did not contain the expected contents" expected (reverse lg)

threePutsFetchLog :: IO (Env.Env B.ByteString B.ByteString) -> Assertion
threePutsFetchLog ioenv = runInBoundThread $ do
  let key1     = C.pack "one"
      val1     = C.pack "val"
      key2     = C.pack "two"
      val2     = C.pack "val"
      key3     = C.pack "three"
      val3     = C.pack "val"
      expected = [ Log.MkLogEntry key1 0 Log.Insert val1
                 , Log.MkLogEntry key2 1 Log.Insert val2
                 , Log.MkLogEntry key3 2 Log.Insert val3
                 ]
  env       <- ioenv
  _         <- Store.put env key1 val1
  _         <- Store.put env key2 val2
  _         <- Store.put env key3 val3
  (Just lg) <- Env.fetchLog env
  assertEqual "the log did not contain the expected contents" expected (reverse lg)

-- * Setup

initTempDb :: IO Db.Db
initTempDb = do
  dir  <- getCanonicalTemporaryDirectory
  path <- createTempDirectory dir "hounds-"
  Db.mkDb path (1024 * 1024 * 128)

initTempEnv :: (Serialize k, Serialize v) => IO (Env.Env k v)
initTempEnv = Env.mkEnv =<< initTempDb

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
                 (runInBoundThread . Db.close . Env.envDb)
                 (testCase "put get" . putGet)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Env.envDb)
                 (testCase "two puts" . twoPuts)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Env.envDb)
                 (testCase "duplicate puts" . duplicatePuts)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Env.envDb)
                 (testCase "two puts, one delete" . twoPutsOneDelete)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Env.envDb)
                 (testCase "one put, two deletes" . onePutTwoDeletes)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Env.envDb)
                 (testCase "two puts, fetchLog" . twoPutsFetchLog)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Env.envDb)
                 (testCase "three puts, fetchLog" . threePutsFetchLog)
  ]

tests :: TestTree
tests = testGroup "tests" [testGroup "props" props, testGroup "units" units]

main :: IO ()
main = defaultMain tests
