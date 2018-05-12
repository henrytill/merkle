module Data.Hounds.PointerBlock.Properties (pointerBlockProperties) where

import           Data.Map.Strict          (Map, toList)
import           Data.Word                (Word8)
import           Test.Tasty
import           Test.Tasty.QuickCheck    (testProperty)

import           Data.Hounds.Hash
import           Data.Hounds.Orphans      ()
import           Data.Hounds.PointerBlock


prop_getChildren :: Map Word8 Hash -> Bool
prop_getChildren children = getChildren pb == childrenList
  where
    childrenList = toList children
    f (i, h)     = (i, Just h)
    pb           = update mkPointerBlock (fmap f childrenList)

pointerBlockProperties :: TestTree
pointerBlockProperties = testGroup "PointerBlock property tests"
  [ testProperty "getChildren" prop_getChildren
  ]
