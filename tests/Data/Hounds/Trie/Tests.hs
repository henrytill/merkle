{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches   #-}

module Data.Hounds.Trie.Tests (trieTests) where

import           Control.Concurrent  (runInBoundThread)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Hounds.Context as Context
import qualified Data.Hounds.Db      as Db
import           Data.Hounds.Test
import qualified Data.Hounds.Trie    as Trie


assertNotEqual :: (HasCallStack, Eq a) => String -> a -> a -> Assertion
assertNotEqual msg a b = assertBool msg (a /= b)

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

insertLookupDeleteLookupTest :: IO TestContext -> Assertion
insertLookupDeleteLookupTest ioContext = runInBoundThread $ do
  context     <- ioContext
  initialRoot <- Context.fetchWorkingRoot context
  _           <- Trie.insert context key1 val1
  ret1        <- Trie.lookup context key1
  _           <- Trie.delete context key1 val1
  finalRoot   <- Context.fetchWorkingRoot context
  ret2        <- Trie.lookup context key1
  assertEqual "returned value did not equal inserted value"     (Just val1) ret1
  assertEqual "final root hash did not equal initial root hash" initialRoot finalRoot
  assertEqual "returned value was not Nothing"                  Nothing     ret2

insertInsertDeleteDeleteTest :: IO TestContext -> Assertion
insertInsertDeleteDeleteTest ioContext = runInBoundThread $ do
  context <- ioContext
  root0   <- Context.fetchWorkingRoot context
  _       <- Trie.insert context key1 val1
  root1   <- Context.fetchWorkingRoot context
  assertNotEqual "root 1 was equal to root 0" root0 root1
  _       <- Trie.insert context key2 val2
  root2   <- Context.fetchWorkingRoot context
  assertNotEqual "root 2 was equal to root 1" root1 root2
  ret1    <- Trie.lookup context key1
  ret2    <- Trie.lookup context key2
  assertEqual "returned value 1 did not equal inserted value" (Just val1) ret1
  assertEqual "returned value 2 did not equal inserted value" (Just val2) ret2
  _       <- Trie.delete context key2 val2
  root3   <- Context.fetchWorkingRoot context
  ret3    <- Trie.lookup context key1
  ret4    <- Trie.lookup context key2
  assertEqual "root 3 did not equal root 1" root1 root3
  assertEqual "returned value 3 did not equal inserted value" (Just val1) ret3
  assertEqual "returned value 4 was not Nothing"              Nothing     ret4
  _       <- Trie.delete context key1 val1
  root4   <- Context.fetchWorkingRoot context
  ret5    <- Trie.lookup context key1
  ret6    <- Trie.lookup context key2
  assertEqual "root 4 did not equal root 0"      root0   root4
  assertEqual "returned value 5 was not Nothing" Nothing ret5
  assertEqual "returned value 6 was not Nothing" Nothing ret4

duplicateInsertsLookupTest :: IO TestContext -> Assertion
duplicateInsertsLookupTest ioContext = runInBoundThread $ do
  context <- ioContext
  _       <- Trie.insert context key1 val1
  root1   <- Context.fetchWorkingRoot context
  _       <- Trie.insert context key1 val1
  root2   <- Context.fetchWorkingRoot context
  ret1    <- Trie.lookup context key1
  assertEqual "root 2 did not equal root 1"                 root1       root2
  assertEqual "returned value did not equal inserted value"  (Just val1) ret1

insertLookupDeleteLookupDeleteAgainLookupTest :: IO TestContext -> Assertion
insertLookupDeleteLookupDeleteAgainLookupTest ioContext = runInBoundThread $ do
  context <- ioContext
  root0   <- Context.fetchWorkingRoot context
  _       <- Trie.insert context key1 val1
  root1   <- Context.fetchWorkingRoot context
  ret1    <- Trie.lookup context key1
  assertNotEqual "root 1 was equal to root 0"                    root0       root1
  assertEqual    "returned value 1 did not equal inserted value" (Just val1) ret1
  _       <- Trie.delete context key1 val1
  root2   <- Context.fetchWorkingRoot context
  ret2    <- Trie.lookup context key1
  assertEqual "root 2 was not equal to root 0"   root0 root2
  assertEqual "returned value 2 was not Nothing" Nothing ret2
  _       <- Trie.delete context key1 val1
  root3   <- Context.fetchWorkingRoot context
  ret3    <- Trie.lookup context key1
  assertEqual "root 3 was not equal to root 0"   root0 root3
  assertEqual "returned value 3 was not Nothing" Nothing ret3


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
                 (testCase "insert, lookup, delete" . insertLookupDeleteLookupTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "insert, insert, lookup, lookup, delete, lookup, lookup, delete, lookup, lookup" . insertInsertDeleteDeleteTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "duplicate inserts, lookup" . duplicateInsertsLookupTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "insert, lookup, delete, lookup, delete again, lookup" . insertLookupDeleteLookupDeleteAgainLookupTest)
  ]
