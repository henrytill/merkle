module Data.Hounds.Hash.Tests (hashTests) where

import Data.ByteString.Char8 qualified as C
import Data.Hounds.Hash qualified as Hash
import Test.Tasty
import Test.Tasty.HUnit

helloHashTest :: Assertion
helloHashTest = assertEqual "The hash of hello was not the expected value" expected actual
  where
    expected = read "324dcf027dd4a30a932c441f365a25e86b173defa4b8e58948253471b81b72cf"
    actual = (Hash.mkHash . C.pack) "hello"

hashTests :: TestTree
hashTests =
  testGroup
    "Hash unit tests"
    [ testCase "hello hash" helloHashTest
    ]
