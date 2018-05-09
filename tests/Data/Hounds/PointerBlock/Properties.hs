module Data.Hounds.PointerBlock.Properties where


import           Data.Map.Strict          (Map, toList)
import           Data.Word                (Word8)

import           Data.Hounds.Hash
import           Data.Hounds.PointerBlock


prop_getChildren :: Map Word8 Hash -> Bool
prop_getChildren children = getChildren pb == childrenList
  where
    childrenList = toList children
    f (i, h)     = (i, Just h)
    pb           = update mkPointerBlock (fmap f childrenList)
