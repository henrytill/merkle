{-# LANGUAGE NamedFieldPuns #-}

module Data.Hounds.Db where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception       (Exception, onException, throwIO)
import           Control.Monad           (unless)
import qualified Data.ByteString         as B
import qualified Data.Serialize          as S
import           Data.Word               (Word64)
import           Database.LMDB.Raw
import           Foreign.Marshal.Alloc   (alloca)
import           Foreign.Marshal.Array   (peekArray, withArray)
import           Foreign.Ptr             (Ptr)
import           Foreign.Storable        (peek, poke)

import qualified Data.Hounds.Hash        as Hash
import qualified Data.Hounds.LeafValue   as LeafValue
import qualified Data.Hounds.Log         as Log


data DbException
  = GetException
  | LogException
  | SerializationException String
  deriving Show

instance Exception DbException

-- * Low-level API

emptyWriteFlags :: MDB_WriteFlags
emptyWriteFlags = compileWriteFlags []

writeFlags :: MDB_WriteFlags
writeFlags = compileWriteFlags [MDB_NOOVERWRITE]

mdbValToByteString :: MDB_val -> IO B.ByteString
mdbValToByteString MDB_val{mv_size, mv_data}
  = B.pack <$> peekArray (fromIntegral mv_size) mv_data

get :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> IO (Maybe B.ByteString)
get dbi txn bs
  = withArray (B.unpack bs) $ \ ptr ->
    do ret <- mdb_get txn dbi (MDB_val (fromIntegral (B.length bs)) ptr)
       mapM mdbValToByteString ret

del :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> IO Bool
del dbi txn bs
  = withArray (B.unpack bs) $ \ ptr ->
      mdb_del txn dbi (MDB_val (fromIntegral (B.length bs)) ptr) Nothing

put :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> B.ByteString
    -> IO Bool
put dbi txn kbs vbs
  = withArray (B.unpack kbs) $ \ kptr ->
    withArray (B.unpack vbs) $ \ vptr ->
    let
      key = MDB_val (fromIntegral (B.length kbs)) kptr
      val = MDB_val (fromIntegral (B.length vbs)) vptr
    in
      mdb_put writeFlags txn dbi key val

-- * High-level API

data Db = MkDb
  { dbEnv       :: MDB_env
  , dbDbiLeaves :: MDB_dbi
  , dbDbiTrie   :: MDB_dbi
  , dbDbiLog    :: MDB_dbi
  }

mkDb :: FilePath -> Int -> IO Db
mkDb dbDir mapSize = do
  env <- mdb_env_create
  mdb_env_set_mapsize env mapSize
  mdb_env_set_maxreaders env 4
  mdb_env_set_maxdbs env 4
  mdb_env_open env dbDir []
  txn <- mdb_txn_begin env Nothing False
  onException (do dbiLeaves <- mdb_dbi_open txn (Just "leaves") [MDB_CREATE]
                  dbiTrie   <- mdb_dbi_open txn (Just "trie")   [MDB_CREATE]
                  dbiLog    <- mdb_dbi_open txn (Just "log")    [MDB_CREATE]
                  mdb_txn_commit txn
                  return MkDb { dbEnv         = env
                              , dbDbiLeaves   = dbiLeaves
                              , dbDbiTrie     = dbiTrie
                              , dbDbiLog      = dbiLog
                              })
              (mdb_txn_abort txn)

data Env = MkEnv
  { envDb       :: Db
  , envTxnCount :: MVar Word64
  }

mkEnv :: Db -> IO Env
mkEnv db = MkEnv db <$> newMVar 0

loggedPut :: Env -> B.ByteString -> IO Hash.Hash
loggedPut env bs = do
  let db          = envDb env
      txnCountVar = envTxnCount env
  txn   <- mdb_txn_begin (dbEnv db) Nothing False
  count <- takeMVar txnCountVar
  onException (do let hash      = Hash.mkHash bs
                      logKey    = Log.MkLogKey count hash
                      leafValue = LeafValue.MkLeafValue False bs
                  succPut <- put (dbDbiLeaves db) txn (S.encode hash) (S.encode leafValue)
                  if succPut
                    then do succLog <- put (dbDbiLog db) txn (S.encode logKey) (S.encode Log.Insert)
                            unless succLog (throwIO LogException)
                            mdb_txn_commit txn
                            putMVar txnCountVar (count + 1)
                            return hash
                    else do mdb_txn_commit txn
                            putMVar txnCountVar count
                            return hash)
              (do mdb_txn_abort txn
                  putMVar txnCountVar count)

loggedDel :: Env -> Hash.Hash -> IO Bool
loggedDel env hash = do
  let db          = envDb env
      txnCountVar = envTxnCount env
  txn   <- mdb_txn_begin (dbEnv db) Nothing False
  count <- takeMVar txnCountVar
  onException (do let logKey = Log.MkLogKey count hash
                  succPut <- del (dbDbiLeaves db) txn (S.encode hash)
                  if succPut
                    then do succLog <- put (dbDbiLog db) txn (S.encode logKey) (S.encode Log.Delete)
                            unless succLog (throwIO LogException)
                            mdb_txn_commit txn
                            putMVar txnCountVar (count + 1)
                            return succLog
                    else do mdb_txn_commit txn
                            putMVar txnCountVar count
                            return succPut)
              (do mdb_txn_abort txn
                  putMVar txnCountVar count)

deserializeLogEntry :: Ptr MDB_val -> Ptr MDB_val -> IO (Either String (Log.LogKey, Log.Operation))
deserializeLogEntry keyPtr valPtr = do
  kbs <- peek keyPtr >>= mdbValToByteString
  vbs <- peek valPtr >>= mdbValToByteString
  let key = S.decode kbs :: Either String Log.LogKey
      val = S.decode vbs :: Either String Log.Operation
  case (key, val) of
    (Left s,        _) -> return (Left s)
    (Right _,  Left s) -> return (Left s)
    (Right k, Right v) -> return (Right (k, v))

positionLo :: MDB_cursor -> Log.Range -> IO Bool
positionLo cursor (Log.MkRange lo _)
  = let
      zeroHs = Hash.MkHash (B.pack [])
      logKey = Log.MkLogKey lo zeroHs
      bs     = S.encode logKey
    in
      withArray (B.unpack bs) $ \ ptr    ->
      alloca                  $ \ keyPtr ->
      alloca                  $ \ valPtr ->
      do poke keyPtr (MDB_val (fromIntegral (B.length bs)) ptr)
         mdb_cursor_get MDB_SET_RANGE cursor keyPtr valPtr

fetchLog :: Env -> Log.Range -> IO [(Log.LogKey, Log.Operation)]
fetchLog env range = do
  let db = envDb env
  txn    <- mdb_txn_begin (dbEnv db) Nothing False
  cursor <- mdb_cursor_open txn (dbDbiLog db)
  onException (do succPos <- positionLo cursor range
                  if succPos
                    then do ret <- go cursor range []
                            mdb_cursor_close cursor
                            mdb_txn_commit txn
                            return ret
                    else return [])
              (do mdb_cursor_close cursor
                  mdb_txn_abort txn)
    where
      go :: MDB_cursor
         -> Log.Range
         -> [(Log.LogKey, Log.Operation)]
         -> IO [(Log.LogKey, Log.Operation)]
      go cursor r@(Log.MkRange _ hi) acc
        = alloca $ \ keyPtr ->
          alloca $ \ valPtr ->
          mdb_cursor_get MDB_GET_CURRENT cursor keyPtr valPtr >>= \ succGet ->
          if succGet
            then do row        <- deserializeLogEntry keyPtr valPtr
                    t@(key, _) <- either (throwIO . SerializationException) pure row
                    let updated = t:acc
                    if Log.logKeyCount key >= hi
                      then return (reverse updated)
                      else mdb_cursor_get MDB_NEXT cursor keyPtr valPtr >> go cursor r updated
            else return (reverse acc)

close :: Db -> IO ()
close db = do
  let env = dbEnv db
  mdb_dbi_close env (dbDbiLeaves db)
  mdb_dbi_close env (dbDbiTrie   db)
  mdb_dbi_close env (dbDbiLog    db)
  mdb_env_close env
