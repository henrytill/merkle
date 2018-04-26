{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hounds.MerklePatricia.Instances where

import           Data.Hounds.MerklePatricia
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Array      ()
import           Test.QuickCheck.Instances.ByteString ()


instance Arbitrary Hash where
  arbitrary = mkHash <$> arbitrary

instance Arbitrary PointerBlock where
  arbitrary                   = MkPointerBlock <$> arbitrary
  shrink (MkPointerBlock arr) = MkPointerBlock <$> shrink arr

instance Arbitrary Tree where
  arbitrary = oneof [Node <$> arbitrary, Leaf <$> arbitrary]
