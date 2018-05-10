module Data.Hounds.Trie.Tests (trieTests) where

import           Control.Concurrent  (runInBoundThread)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Hounds.Context as Context
import qualified Data.Hounds.Db      as Db
import           Data.Hounds.Test
import qualified Data.Hounds.Trie    as Trie


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

insertLookupDeleteTest :: IO TestContext -> Assertion
insertLookupDeleteTest ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Trie.insert context key1 val1
  ret1    <- Trie.lookup context key1
  _       <- Trie.delete context key1 val1
  ret2    <- Trie.lookup context key1
  assertEqual "returned value did not equal inserted value" (Just val1) ret1
  assertEqual "returned value was not Nothing"              Nothing     ret2

trieTests :: [TestTree]
trieTests =
  [ withResource (runInBoundThread initTempEnv)
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
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "insert, lookup, delete" . insertLookupDeleteTest)
  ]
