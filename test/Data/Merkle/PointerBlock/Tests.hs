module Data.Merkle.PointerBlock.Tests where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Merkle.Hash qualified as Hash
import Data.Merkle.PointerBlock qualified as PointerBlock
import Data.Serialize
import Test.Tasty
import Test.Tasty.HUnit

emptyPb :: PointerBlock.PointerBlock
emptyPb = PointerBlock.mkPointerBlock

emptyPbHash :: Hash.Hash
emptyPbHash = Hash.mkHash (encode emptyPb)

helloHash :: Hash.Hash
helloHash = Hash.mkHash (C.pack "hello")

fullPb :: PointerBlock.PointerBlock
fullPb = PointerBlock.fillPointerBlock (Just helloHash)

emptyHashTest :: Assertion
emptyHashTest = assertEqual "The hash of an empty PointerBlock was not the expected value" expected actual
  where
    expected = read "2b69702a889248a4d6620475a105dccd5e0d4230aca8a492aaf6510e55d55b02"
    actual = emptyPbHash

emptyLengthTest :: Assertion
emptyLengthTest = assertEqual "A serialized empty PointerBlock did not have the expected length" expected actual
  where
    expected = 256
    actual = B.length (encode emptyPb)

fullLengthTest :: Assertion
fullLengthTest = assertEqual "A serialized full PointerBlock did not have the expected length" expected actual
  where
    expected = 256 * (32 + 1)
    actual = B.length (encode fullPb)

index1Test :: Assertion
index1Test = assertEqual "The hash of a PointerBlock with a known item an index 1 was not the expected value" expected actual
  where
    expected = read "b11665e990d461db27f850481ad538662cc5321d67f9688c3a6ae77fa4b63f03"
    actual = (Hash.mkHash . encode . PointerBlock.update emptyPb) [(1, Just helloHash)]

index42Test :: Assertion
index42Test = assertEqual "The hash of a PointerBlock with a known item an index 42 was not the expected value" expected actual
  where
    expected = read "bc50d1148e6c4197bc978a80e424d4a0b3f065496102d376ba6c138a2ed2c3a7"
    actual = (Hash.mkHash . encode . PointerBlock.update emptyPb) [(42, Just helloHash)]

fullHashTest :: Assertion
fullHashTest = assertEqual "The hash of a full PointerBlock was not the expected value" expected actual
  where
    expected = read "2b5e43a142c6b5e1b2ff614185d76d1e215b0efe627e124b4244006f4da4ed64"
    actual = (Hash.mkHash . encode) fullPb

pointerBlockTests :: TestTree
pointerBlockTests =
  testGroup
    "PointerBlock unit tests"
    [ testCase "Empty PointerBlock has the expected hash" emptyHashTest,
      testCase "A serialized empty PointerBlock has the expected length" emptyLengthTest,
      testCase "A serialized full PointerBlock has the expected length" fullLengthTest,
      testCase "A PointerBlock with a known item at index 1 has the expected hash" index1Test,
      testCase "A PointerBlock with a known item at index 42 has the expected hash" index42Test,
      testCase "A full PointerBlock has the expected hash" fullHashTest
    ]
