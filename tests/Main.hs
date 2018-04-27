module Main (main) where

import           Data.Hounds
import           Data.Hounds.Orphans                  ()
import           Data.Serialize                       (decode, encode)
import           System.IO.Temp                       (createTempDirectory, getCanonicalTemporaryDirectory)
import           Test.QuickCheck                      (Property, arbitrary)
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Monadic              (assert, monadicIO, pick,
                                                       run)
import           Test.Tasty                           (TestTree, defaultMain,
                                                       testGroup, withResource)
import           Test.Tasty.QuickCheck                (testProperty)


prop_roundTrip :: Bool -> Bool
prop_roundTrip t = decode (encode t) == Right t

prop_roundTripDb :: IO Db -> Property
prop_roundTripDb iodb = monadicIO $ do
  db   <- run iodb
  bs   <- pick arbitrary
  let hash = mkHash bs
  _    <- run (put db dbDbiLeaves (unHash hash) bs)
  mbs  <- run (get db dbDbiLeaves (unHash hash))
  assert (mbs == Just bs)

initDb :: IO Db
initDb = do
  dir  <- getCanonicalTemporaryDirectory
  path <- createTempDirectory dir "hounds-"
  mkDb path (1024 * 1024 * 128)

props :: [TestTree]
props =
  [ testProperty "Round trip Tree serialization" prop_roundTrip
  , withResource initDb close (testProperty "Round trip leaves to db" . prop_roundTripDb)
  ]

tests :: TestTree
tests = testGroup "props" props

main :: IO ()
main = defaultMain tests
