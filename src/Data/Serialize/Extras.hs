module Data.Serialize.Extras
  ( getFixedLengthSeqOf
  , putFixedLengthSeqOf
  ) where

import qualified Data.Foldable  as F
import qualified Data.Sequence  as Seq
import           Data.Serialize


getFixedLengthSeqOf :: Int -> Get a -> Get (Seq.Seq a)
getFixedLengthSeqOf len m = go Seq.empty len
  where
    go xs 0 = return $! xs
    go xs n = xs `seq` n `seq` do
      x <- m
      go (xs Seq.|> x) (n - 1)

putFixedLengthSeqOf :: Putter a -> Putter (Seq.Seq a)
putFixedLengthSeqOf = F.mapM_
