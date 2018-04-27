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

data LogValue = MkLogValue
  { logValueOperation :: Operation
  , logValueLeafHash  :: Hash
  } deriving (Eq, Show)

putLogValue :: Putter LogValue
putLogValue le = do
  putOperation (logValueOperation le)
  putHash      (logValueLeafHash  le)

getLogValue :: Get LogValue
getLogValue = MkLogValue <$> getOperation <*> getHash
