module Data.Hounds.Store.Tests (storeTests) where

import           Control.Concurrent      (runInBoundThread)
import           Control.Concurrent.MVar (takeMVar)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Hounds.Context     as Context
import qualified Data.Hounds.Db          as Db
import qualified Data.Hounds.Log         as Log
import qualified Data.Hounds.Store       as Store
import           Data.Hounds.Test


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
  assertEqual "the transaction count did not equal 2" 2 count

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

storeTests :: [TestTree]
storeTests =
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
  ]
