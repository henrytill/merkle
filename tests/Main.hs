module Main (main) where

import           Data.Hounds.MerklePatricia         (Tree)
import           Data.Hounds.MerklePatricia.Orphans ()
import           Data.Serialize                     (decode, encode)
import           Test.Tasty                         (TestTree, defaultMain,
                                                     testGroup)
import           Test.Tasty.QuickCheck              (testProperty)


prop_roundTrip :: Tree -> Bool
prop_roundTrip t = decode (encode t) == Right t

props :: [TestTree]
props = [testProperty "Round trip Tree serialization" prop_roundTrip]

tests :: TestTree
tests = testGroup "props" props

main :: IO ()
main = defaultMain tests
