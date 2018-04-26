{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hounds.MerklePatricia.Orphans where

import qualified Data.ByteString                      as B
import           Data.Hounds.MerklePatricia
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Array      ()
import           Test.QuickCheck.Instances.ByteString ()


instance Arbitrary Hash where
  arbitrary = mkHash <$> arbitrary

instance Arbitrary PointerBlock where
  arbitrary = MkPointerBlock <$> resize 256 arbitrary

instance Arbitrary HashSuffix where
  arbitrary = MkHashSuffix <$> arbitrary `suchThat` (\ bs -> B.length bs <= 32)

instance Arbitrary Tree where
  arbitrary = oneof [Node <$> arbitrary, Leaf <$> arbitrary <*> arbitrary]
