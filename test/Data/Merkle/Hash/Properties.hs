module Data.Merkle.Hash.Properties (hashProperties) where

import Data.Merkle.Hash
import Data.Merkle.Orphans ()
import Test.QuickCheck (Arbitrary)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

prop_roundTripShowRead :: (Eq a, Show a, Read a, Arbitrary a) => a -> Bool
prop_roundTripShowRead t = read (show t) == t

hashProperties :: TestTree
hashProperties =
  testGroup
    "Hash property tests"
    [testProperty "Round trip Show and Read instances for Hash" (prop_roundTripShowRead :: Hash -> Bool)]
