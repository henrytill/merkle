module Main (main) where

import           Test.Tasty (TestTree, defaultMain, testGroup)


tests :: TestTree
tests = testGroup "tests" []

main :: IO ()
main = defaultMain tests
