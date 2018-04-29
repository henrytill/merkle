module Data.Hounds.Log where

import           Data.Serialize
import           Data.Word        (Word64)

import           Data.Hounds.Hash


data LogKey = MkLogKey
  { logKeyCount    :: Word64
  , logKeyLeafHash :: Hash
  } deriving (Eq, Show)

putLogKey :: Putter LogKey
putLogKey lk = do
  putWord64le (logKeyCount    lk)
  putHash     (logKeyLeafHash lk)

getLogKey :: Get LogKey
getLogKey = MkLogKey <$> getWord64le <*> getHash

instance Serialize LogKey where
  put = putLogKey
  get = getLogKey

data Operation = Insert | Delete
  deriving (Eq, Show)

putOperation :: Putter Operation
putOperation Insert = putWord8 0
putOperation Delete = putWord8 1

getOperation :: Get Operation
getOperation = do
  tag <- getWord8
  case tag of
    0 -> return Insert
    1 -> return Delete
    _ -> fail "no such tag"

instance Serialize Operation where
  put = putOperation
  get = getOperation

data Range = MkRange Word64 Word64
  deriving (Eq, Show)
