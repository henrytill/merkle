{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches   #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Hounds.Trie.Tests (trieTests) where

import           Control.Concurrent       (runInBoundThread)
import qualified Data.ByteString.Base16   as Base16
import qualified Data.ByteString.Char8    as C
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Hounds.Context      as Context
import qualified Data.Hounds.Db           as Db
import qualified Data.Hounds.Hash         as Hash
import qualified Data.Hounds.PointerBlock as PointerBlock
import           Data.Hounds.Test
import qualified Data.Hounds.Trie         as Trie

emptyRootHashTest :: Assertion
emptyRootHashTest = assertEqual "The empty root hash was not the expected value" expected actual
  where
    (expected, _) = Base16.decode (C.pack "c575260cf13e36f179a50b0882bd64fc0466ecd25bdd7bc88766c2cc2e4c0dfe")
    actual        = Hash.unHash (Trie.hashTrie (Trie.mkTrie PointerBlock.mkPointerBlock :: Trie.Trie TestKey String))

oneLeafHashTest :: Assertion
oneLeafHashTest = assertEqual "The hash of a simple leaf was not the expected value" expected actual
  where
    (expected, _) = Base16.decode (C.pack "dbad1a97cd55325a85072099ab0ad79ce4c2d1e2d0548a140bd2ec5741a33587")
    hello         = C.pack "hello"
    helloHash     = Hash.mkHash hello
    leaf          = Trie.Leaf helloHash hello
    actual        = (Hash.unHash . Trie.hashTrie) leaf

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

rollbackRepeatTest :: IO TestContext -> Assertion
rollbackRepeatTest ioContext = runInBoundThread $ do
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
  -- === Rollback ===
  _       <- Context.setWorkingRoot context root2
  ret7    <- Trie.lookup context key1
  ret8    <- Trie.lookup context key2
  assertEqual "returned value 7 did not equal inserted value" (Just val1) ret7
  assertEqual "returned value 8 did not equal inserted value" (Just val2) ret8
  _       <- Trie.delete context key2 val2
  root5   <- Context.fetchWorkingRoot context
  ret9    <- Trie.lookup context key1
  ret10   <- Trie.lookup context key2
  assertEqual "root 5 did not equal root 1"                   root1       root5
  assertEqual "returned value 9 did not equal inserted value" (Just val1) ret9
  assertEqual "returned value 10 was not Nothing"             Nothing     ret10
  _       <- Trie.delete context key1 val1
  root6   <- Context.fetchWorkingRoot context
  ret11   <- Trie.lookup context key1
  ret12   <- Trie.lookup context key2
  assertEqual "root 6 did not equal root 0"       root0   root6
  assertEqual "returned value 11 was not Nothing" Nothing ret11
  assertEqual "returned value 12 was not Nothing" Nothing ret4

rollbackForkTest :: IO TestContext -> Assertion
rollbackForkTest ioContext = runInBoundThread $ do
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
  -- === Rollback ===
  _       <- Context.setWorkingRoot context root2
  ret7    <- Trie.lookup context key1
  ret8    <- Trie.lookup context key2
  assertEqual "returned value 7 did not equal inserted value" (Just val1) ret7
  assertEqual "returned value 8 did not equal inserted value" (Just val2) ret8
  _       <- Trie.insert context key3 val3
  root5   <- Context.fetchWorkingRoot context
  ret9    <- Trie.lookup context key1
  ret10   <- Trie.lookup context key2
  ret11   <- Trie.lookup context key3
  assertNotEqual "root 5 did not equal root 4"                     root4       root5
  assertEqual    "returned value 9 did not equal inserted value"   (Just val1) ret9
  assertEqual    "returned value 10 did not equal inserted value"  (Just val2) ret10
  assertEqual    "returned value 11 did not equal inserted value"  (Just val3) ret11
  _       <- Trie.insert context key4 val4
  root6   <- Context.fetchWorkingRoot context
  ret12   <- Trie.lookup context key1
  ret13   <- Trie.lookup context key2
  ret14   <- Trie.lookup context key3
  ret15   <- Trie.lookup context key4
  assertNotEqual "root 6 did not equal root 5"                     root5       root6
  assertEqual    "returned value 12 did not equal inserted value"  (Just val1) ret12
  assertEqual    "returned value 13 did not equal inserted value"  (Just val2) ret13
  assertEqual    "returned value 14 did not equal inserted value"  (Just val3) ret14
  assertEqual    "returned value 15 did not equal inserted value"  (Just val4) ret15

trieTests :: TestTree
trieTests = testGroup "Trie unit tests"
  [ testCase "empty root hash" emptyRootHashTest
  , testCase "single leaf hash" oneLeafHashTest
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "insert" . insertLookupTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "two inserts" . twoInsertsTwoLookupsTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "three inserts" . threeInsertsThreeLookupsTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "four inserts" . fourInsertsFourLookupsTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "five inserts" . fiveInsertsFiveLookupsTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "insert, delete" . insertLookupDeleteLookupTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "insert, insert, delete, delete" . insertInsertDeleteDeleteTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "duplicate inserts" . duplicateInsertsLookupTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "insert, delete, delete again" . insertLookupDeleteLookupDeleteAgainLookupTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "rollback repeat test" . rollbackRepeatTest)
  , withResource (runInBoundThread initTempEnv)
                 (runInBoundThread . Db.close . Context.contextDb)
                 (testCase "rollback fork test" . rollbackForkTest)
  ]
