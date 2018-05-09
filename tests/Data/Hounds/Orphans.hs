{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hounds.Orphans where

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Array      ()
import           Test.QuickCheck.Instances.ByteString ()

import           Data.Hounds.Hash
import           Data.Hounds.PointerBlock
import           Data.Hounds.Test
import           Data.Hounds.Trie


instance Arbitrary Hash where
  arbitrary = mkHash <$> arbitrary

instance Arbitrary PointerBlock where
  arbitrary = MkPointerBlock <$> resize 256 arbitrary

instance (Arbitrary k, Arbitrary v) => Arbitrary (Trie k v) where
  arbitrary = oneof [Node <$> arbitrary, Leaf <$> arbitrary <*> arbitrary]

instance Arbitrary TestKey where
  arbitrary = mkTestKey <$> vectorOf 4 arbitrary
