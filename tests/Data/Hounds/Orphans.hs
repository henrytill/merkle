{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hounds.Orphans where

import qualified Data.ByteString                      as B
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Array      ()
import           Test.QuickCheck.Instances.ByteString ()

import           Data.Hounds.Hash
import           Data.Hounds.HashSuffix
import           Data.Hounds.PointerBlock
import           Data.Hounds.Trie


instance Arbitrary Hash where
  arbitrary = mkHash <$> arbitrary

instance Arbitrary PointerBlock where
  arbitrary = MkPointerBlock <$> resize 256 arbitrary

instance Arbitrary HashSuffix where
  arbitrary = MkHashSuffix <$> arbitrary `suchThat` (\ bs -> B.length bs <= 32)

instance Arbitrary Trie where
  arbitrary = oneof [Node <$> arbitrary, Leaf <$> arbitrary <*> arbitrary]
