module Data.Hounds.Log where

import           Data.Serialize
import           Data.Word        (Word64)

import           Data.Hounds.Hash


data LogKey = MkLogKey
  { logKeyLeafHash :: Hash
  , logKeyCount    :: Word64
  } deriving (Eq, Show)

putLogKey :: Putter LogKey
putLogKey lk = do
  putHash     (logKeyLeafHash lk)
  putWord64le (logKeyCount    lk)

getLogKey :: Get LogKey
getLogKey = MkLogKey <$> getHash <*> getWord64le

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

data Range = MkRange
  { logKeyCountLo :: Word64
  , logKeyCountHi :: Word64
  } deriving (Eq, Show)
