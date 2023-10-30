{-# LANGUAGE ScopedTypeVariables #-}

module Data.Hounds.Trie.Properties (trieProperties) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (finally)
import Data.ByteString qualified as B
import Data.Hounds.Context qualified as Context
import Data.Hounds.Hash qualified as Hash
import Data.Hounds.Orphans ()
import Data.Hounds.Test
import Data.Hounds.Trie qualified as Trie
import Data.Map qualified as Map
import Data.Serialize
import Test.QuickCheck (Arbitrary, Property)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic qualified as M
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

prop_roundTripSerialization :: (Arbitrary a, Eq a, Serialize a) => a -> Bool
prop_roundTripSerialization t = decode (encode t) == Right t

insertAll ::
  (Eq k, Eq v, Serialize k, Serialize v, Foldable t) =>
  Context.Context k v ->
  t (k, v) ->
  IO ()
insertAll context = mapM_ (uncurry $ Trie.insert context)

deleteAll ::
  (Eq k, Eq v, Serialize k, Serialize v, Foldable t, Show k, Show v) =>
  Context.Context k v ->
  t (k, v) ->
  IO ()
deleteAll context = mapM_ (uncurry $ Trie.delete context)

fetchAll ::
  (Eq k, Serialize k, Serialize v, Eq k, Traversable t) =>
  Context.Context k v ->
  t (k, v) ->
  IO (t (Maybe v))
fetchAll context = mapM (Trie.lookup context . fst)

data RResult k v = MkRResult Hash.Hash [[Maybe v]]
  deriving (Eq, Show)

roundTrip ::
  forall k v.
  (Eq k, Eq v, Serialize k, Serialize v) =>
  [(k, v)] ->
  IO (Maybe [v])
roundTrip pairs = runInBoundThread $ do
  context <- initTempEnv
  finally
    ( do
        insertAll context pairs
        sequence <$> fetchAll context pairs
    )
    (Context.close context)

prop_roundTrip ::
  forall k v.
  ( Arbitrary k,
    Arbitrary v,
    Eq k,
    Eq v,
    Serialize k,
    Serialize v,
    Show k,
    Show v,
    Ord k
  ) =>
  Map.Map k v ->
  Property
prop_roundTrip kvs = M.monadicIO $ do
  let pairs = Map.toList kvs
      values = snd <$> pairs
  (Just rets) <- M.run $ roundTrip pairs
  M.assert (values == rets)

roundTripRollback ::
  forall k v.
  (Eq k, Eq v, Serialize k, Serialize v, Show k, Show v) =>
  [(k, v)] ->
  [(k, v)] ->
  IO [RResult k v]
roundTripRollback first second = runInBoundThread $ do
  context <- initTempEnv
  finally
    ( do
        root0 <- Context.fetchWorkingRoot context
        insertAll context first
        root1 <- Context.fetchWorkingRoot context
        ret1 <- fetchAll context first
        insertAll context second
        root2 <- Context.fetchWorkingRoot context
        ret2f <- fetchAll context first
        ret2s <- fetchAll context second
        deleteAll context second
        root3 <- Context.fetchWorkingRoot context
        ret3f <- fetchAll context first
        ret3s <- fetchAll context second
        deleteAll context second
        root4 <- Context.fetchWorkingRoot context
        ret4f <- fetchAll context first
        ret4s <- fetchAll context second
        let res0 = MkRResult root0 []
            res1 = MkRResult root1 [ret1]
            res2 = MkRResult root2 [ret2f, ret2s]
            res3 = MkRResult root3 [ret3f, ret3s]
            res4 = MkRResult root4 [ret4f, ret4s]
        return [res0, res1, res2, res3, res4]
    )
    (Context.close context)

prop_roundTripRollback ::
  forall k v.
  ( Arbitrary k,
    Arbitrary v,
    Eq k,
    Eq v,
    Serialize k,
    Serialize v,
    Show k,
    Show v,
    Ord k
  ) =>
  Map.Map k v ->
  Property
prop_roundTripRollback kvs = M.monadicIO $ do
  let pairs = Map.toList kvs
      (first, second) = splitAt ((length pairs + 1) `div` 2) pairs
  _ <- M.run $ roundTripRollback first second
  M.assert True

trieProperties :: TestTree
trieProperties =
  testGroup
    "Trie property tests"
    [ testProperty
        "Round trip Trie serialization"
        (prop_roundTripSerialization :: Trie.Trie TestKey B.ByteString -> Bool),
      testProperty
        "Round trip"
        (prop_roundTrip :: Map.Map TestKey B.ByteString -> Property),
      testProperty
        "Round trip rollback"
        (prop_roundTripRollback :: Map.Map TestKey B.ByteString -> Property)
    ]
