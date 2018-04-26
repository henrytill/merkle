{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hounds.MerklePatricia.Instances where

import qualified Data.ByteString                      as B
import           Data.Hounds.MerklePatricia
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Array      ()
import           Test.QuickCheck.Instances.ByteString ()


instance Arbitrary Hash where
  arbitrary          = MkHash <$> arbitrary `suchThat` (\ bs -> B.length bs == 32)
  shrink (MkHash bs) = MkHash <$> shrink bs

instance Arbitrary PointerBlock where
  arbitrary                   = MkPointerBlock <$> arbitrary
  shrink (MkPointerBlock arr) = MkPointerBlock <$> shrink arr

instance Arbitrary Tree where
  arbitrary = oneof [Node <$> arbitrary, Leaf <$> arbitrary]
