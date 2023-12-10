module Main (main) where

import Data.Merkle.Db.Properties (dbProperties)
import Data.Merkle.Hash.Properties (hashProperties)
import Data.Merkle.Hash.Tests (hashTests)
import Data.Merkle.PointerBlock.Properties (pointerBlockProperties)
import Data.Merkle.PointerBlock.Tests (pointerBlockTests)
import Data.Merkle.Trie.Properties (trieProperties)
import Data.Merkle.Trie.Tests (trieTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

props :: [TestTree]
props = [dbProperties, hashProperties, pointerBlockProperties, trieProperties]

units :: [TestTree]
units = [hashTests, pointerBlockTests, trieTests]

tests :: TestTree
tests = testGroup "Tests" [testGroup "Property tests" props, testGroup "Unit tests" units]

main :: IO ()
main = defaultMain tests
