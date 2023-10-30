module Main (main) where

import Control.Monad (unless)
import Language.Haskell.HLint (hlint)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  hints <- hlint (["src", "test"] ++ args)
  unless (null hints) exitFailure
