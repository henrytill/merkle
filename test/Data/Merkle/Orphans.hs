{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Merkle.Orphans where

import Data.Array qualified as Array
import Data.Ix qualified as Ix
import Data.Merkle.Hash
import Data.Merkle.PointerBlock
import Data.Merkle.Test
import Data.Merkle.Trie
import Test.QuickCheck
import Test.QuickCheck.Instances.Array ()
import Test.QuickCheck.Instances.ByteString ()

instance Arbitrary Hash where
  arbitrary = mkHash <$> arbitrary

instance Arbitrary PointerBlock where
  arbitrary = MkPointerBlock <$> arb
    where
      size = Ix.rangeSize bounds
      arb = Array.listArray bounds <$> vectorOf size arbitrary

instance (Arbitrary k, Arbitrary v) => Arbitrary (Trie k v) where
  arbitrary = oneof [Node <$> arbitrary, Leaf <$> arbitrary <*> arbitrary]

instance Arbitrary TestKey where
  arbitrary = mkTestKey <$> vectorOf 4 arbitrary
