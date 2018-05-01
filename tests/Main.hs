module Main (main) where

import           Control.Concurrent                   (runInBoundThread)
import           Control.Concurrent.MVar              (takeMVar)
import           Control.Exception                    (onException, throwIO)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as C
import           Data.Serialize
import           Database.LMDB.Raw
import           System.IO.Temp                       (createTempDirectory, getCanonicalTemporaryDirectory)
import           Test.QuickCheck                      (Property, arbitrary)
import           Test.QuickCheck.Instances.ByteString ()
import qualified Test.QuickCheck.Monadic              as M
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck                (testProperty)

import qualified Data.Hounds.Context                  as Context
import qualified Data.Hounds.Db                       as Db
import qualified Data.Hounds.Hash                     as Hash
import qualified Data.Hounds.Log                      as Log
import           Data.Hounds.Orphans                  ()
import qualified Data.Hounds.PointerBlock             as PointerBlock
import qualified Data.Hounds.Store                    as Store
import qualified Data.Hounds.TestKey                  as TestKey
import qualified Data.Hounds.Trie                     as Trie


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

key1, key2, key3, key4, key5 :: TestKey.TestKey
key1 = TestKey.mkTestKey [1, 0, 0, 0]
key2 = TestKey.mkTestKey [1, 0, 0, 1]
key3 = TestKey.mkTestKey [1, 0, 1, 0]
key4 = TestKey.mkTestKey [1, 0, 1, 1]
key5 = TestKey.mkTestKey [1, 0, 2, 1]

val1, val2, val3, val4, val5 :: B.ByteString
val1 = C.pack "value1"
val2 = C.pack "value2"
val3 = C.pack "value3"
val4 = C.pack "value4"
val5 = C.pack "value5"

type TestContext = Context.Context TestKey.TestKey B.ByteString

putGet :: IO TestContext -> Assertion
putGet ioContext = runInBoundThread $ do
  context    <- ioContext
  _          <- Store.put context key1 val1
  (Just ret) <- Store.get context key1
  assertEqual "round" val1 ret

twoPuts :: IO TestContext -> Assertion
twoPuts ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Store.put context key1 val1
  _       <- Store.put context key2 val2
  count   <- takeMVar (Context.contextCount context)
  assertEqual "the transaction count did not equal 2" 2 count

duplicatePuts :: IO TestContext -> Assertion
duplicatePuts ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Store.put context key1 val1
  _       <- Store.put context key1 val1
  count   <- takeMVar (Context.contextCount context)
  assertEqual "the transaction count did not equal 1" 1 count

twoPutsOneDelete :: IO TestContext -> Assertion
twoPutsOneDelete ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Store.put context key1 val1
  _       <- Store.put context key2 val2
  _       <- Store.del context key1
  count   <- takeMVar (Context.contextCount context)
  assertEqual "the transaction count did not equal 3" 3 count

onePutTwoDeletes :: IO TestContext -> Assertion
onePutTwoDeletes ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Store.put context key1 val1
  _       <- Store.del context key1
  _       <- Store.del context key1
  count   <- takeMVar (Context.contextCount context)
  assertEqual "the transaction count did not equal 2" 2 count

twoPutsFetchLog :: IO TestContext -> Assertion
twoPutsFetchLog ioContext = runInBoundThread $ do
  let expected = [ Log.MkLogEntry 0 Log.Insert key1 val1
                 , Log.MkLogEntry 1 Log.Insert key2 val2
                 ]
  context   <- ioContext
  _         <- Store.put context key1 val1
  _         <- Store.put context key2 val2
  (Just lg) <- Context.fetchLog context
  assertEqual "the log did not contain the expected contents" expected (reverse lg)

fourPutsFetchLog :: IO TestContext -> Assertion
fourPutsFetchLog ioContext = runInBoundThread $ do
  let expected = [ Log.MkLogEntry 0 Log.Insert key1 val1
                 , Log.MkLogEntry 1 Log.Insert key2 val2
                 , Log.MkLogEntry 2 Log.Insert key3 val3
                 , Log.MkLogEntry 3 Log.Insert key4 val4
                 ]
  context   <- ioContext
  _         <- Store.put context key1 val1
  _         <- Store.put context key2 val2
  _         <- Store.put context key3 val3
  _         <- Store.put context key4 val4
  (Just lg) <- Context.fetchLog context
  assertEqual "the log did not contain the expected contents" expected (reverse lg)

insertLookupTest :: IO TestContext -> Assertion
insertLookupTest ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Trie.insert context key1 val1
  ret1    <- Trie.lookup context key1
  assertEqual "returned value did not equal inserted value" (Just val1) ret1

twoInsertsTwoLookupsTest :: IO TestContext -> Assertion
twoInsertsTwoLookupsTest ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Trie.insert context key1 val1
  _       <- Trie.insert context key2 val2
  ret1    <- Trie.lookup context key1
  ret2    <- Trie.lookup context key2
  assertEqual "first returned value did not equal inserted value"  (Just val1) ret1
  assertEqual "second returned value did not equal inserted value" (Just val2) ret2

threeInsertsThreeLookupsTest :: IO TestContext -> Assertion
threeInsertsThreeLookupsTest ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Trie.insert context key1 val1
  _       <- Trie.insert context key2 val2
  _       <- Trie.insert context key3 val3
  ret1    <- Trie.lookup context key1
  ret2    <- Trie.lookup context key2
  ret3    <- Trie.lookup context key3
  assertEqual "first returned value did not equal inserted value"  (Just val1) ret1
  assertEqual "second returned value did not equal inserted value" (Just val2) ret2
  assertEqual "third returned value did not equal inserted value"  (Just val3) ret3

fourInsertsFourLookupsTest :: IO TestContext -> Assertion
fourInsertsFourLookupsTest ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Trie.insert context key1 val1
  _       <- Trie.insert context key2 val2
  _       <- Trie.insert context key3 val3
  _       <- Trie.insert context key4 val4
  ret1    <- Trie.lookup context key1
  ret2    <- Trie.lookup context key2
  ret3    <- Trie.lookup context key3
  ret4    <- Trie.lookup context key4
  assertEqual "first returned value did not equal inserted value"  (Just val1) ret1
  assertEqual "second returned value did not equal inserted value" (Just val2) ret2
  assertEqual "third returned value did not equal inserted value"  (Just val3) ret3
  assertEqual "fourth returned value did not equal inserted value" (Just val4) ret4

fiveInsertsFiveLookupsTest :: IO TestContext -> Assertion
fiveInsertsFiveLookupsTest ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Trie.insert context key1 val1
  _       <- Trie.insert context key2 val2
  _       <- Trie.insert context key3 val3
  _       <- Trie.insert context key4 val4
  _       <- Trie.insert context key5 val5
  ret1    <- Trie.lookup context key1
  ret2    <- Trie.lookup context key2
  ret3    <- Trie.lookup context key3
  ret4    <- Trie.lookup context key4
  ret5    <- Trie.lookup context key5
  assertEqual "first returned value did not equal inserted value"  (Just val1) ret1
  assertEqual "second returned value did not equal inserted value" (Just val2) ret2
  assertEqual "third returned value did not equal inserted value"  (Just val3) ret3
  assertEqual "fourth returned value did not equal inserted value" (Just val4) ret4
  assertEqual "fifth returned value did not equal inserted value"  (Just val5) ret5

-- * Setup

initTempDb :: IO Db.Db
initTempDb = do
  dir  <- getCanonicalTemporaryDirectory
  path <- createTempDirectory dir "hounds-"
  Db.mkDb path (1024 * 1024 * 128)

initTempEnv :: (Serialize k, Serialize v) => IO (Context.Context k v)
initTempEnv = do
  let trie     = Trie.mkTrie PointerBlock.mkPointerBlock
      rootHash = Trie.hashTrie trie
  tempDb  <- initTempDb
  context <- Context.mkContext tempDb rootHash
  succPut <- Trie.store context rootHash trie
  if succPut
    then return context
    else throwIO Db.PutException

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
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "put get" . putGet)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "two puts" . twoPuts)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "duplicate puts" . duplicatePuts)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "two puts, one delete" . twoPutsOneDelete)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "one put, two deletes" . onePutTwoDeletes)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "two puts, fetchLog" . twoPutsFetchLog)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "four puts, fetchLog" . fourPutsFetchLog)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "insert, lookup" . insertLookupTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "two inserts, two lookups" . twoInsertsTwoLookupsTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "three inserts, three lookups" . threeInsertsThreeLookupsTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "four inserts, four lookups" . fourInsertsFourLookupsTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "five inserts, five lookups" . fiveInsertsFiveLookupsTest)
  ]

tests :: TestTree
tests = testGroup "tests" [testGroup "props" props, testGroup "units" units]

main :: IO ()
main = defaultMain tests
