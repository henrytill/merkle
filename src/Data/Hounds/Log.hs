{-# LANGUAGE NamedFieldPuns #-}

module Data.Hounds.Log where

import           Data.Serialize
import           Data.Word      (Word64)


data LogEntry k v = MkLogEntry
  { logEntryKey       :: k
  , logEntryCount     :: Word64
  , logEntryOperation :: Operation
  , logEntryValue     :: v
  } deriving (Eq, Show)

putLogEntry :: (Serialize k, Serialize v) => Putter (LogEntry k v)
putLogEntry MkLogEntry{logEntryKey, logEntryCount, logEntryOperation, logEntryValue}
  = do put          logEntryKey
       putWord64le  logEntryCount
       putOperation logEntryOperation
       put          logEntryValue

getLogEntry :: (Serialize k, Serialize v) => Get (LogEntry k v)
getLogEntry
  = MkLogEntry <$> get
               <*> getWord64le
               <*> getOperation
               <*> get

instance (Serialize k, Serialize v) => Serialize (LogEntry k v) where
  put = putLogEntry
  get = getLogEntry

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
