module Data.Hounds.Trie.Properties (trieProperties) where

import qualified Data.ByteString                      as B
import           Data.Serialize
import           Test.QuickCheck                      (Arbitrary)
import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty
import           Test.Tasty.QuickCheck                (testProperty)

import           Data.Hounds.Orphans                  ()
import           Data.Hounds.Test
import qualified Data.Hounds.Trie                     as Trie


prop_roundTrip :: (Arbitrary a, Eq a, Serialize a) => a -> Bool
prop_roundTrip t = decode (encode t) == Right t

trieProperties :: [TestTree]
trieProperties =
  [ testProperty "Round trip Tree serialization"
                 (prop_roundTrip :: Trie.Trie TestKey B.ByteString -> Bool)
  ]
