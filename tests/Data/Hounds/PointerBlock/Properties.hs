module Data.Hounds.PointerBlock.Properties (pointerBlockProperties) where

import Data.Map.Strict (Map, toList)
import Data.Serialize
import Data.Word (Word8)
import Test.QuickCheck (Arbitrary)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Data.Hounds.Hash
import Data.Hounds.Orphans ()
import Data.Hounds.PointerBlock

prop_roundTripSerialization :: (Arbitrary a, Eq a, Serialize a) => a -> Bool
prop_roundTripSerialization t = decode (encode t) == Right t

prop_getChildren :: Map Word8 Hash -> Bool
prop_getChildren children = getChildren pb == childrenList
  where
    childrenList = toList children
    f (i, h) = (i, Just h)
    pb = update mkPointerBlock (fmap f childrenList)

pointerBlockProperties :: TestTree
pointerBlockProperties = testGroup "PointerBlock property tests"
  [ testProperty "Round trip PointerBlock serialization"
                 (prop_roundTripSerialization :: PointerBlock -> Bool)
  , testProperty "getChildren" prop_getChildren
  ]
