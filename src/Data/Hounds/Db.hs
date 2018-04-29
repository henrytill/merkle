{-# LANGUAGE NamedFieldPuns #-}

module Data.Hounds.Db
  ( put
  , get
  , Db(..)
  , mkDb
  , Env(..)
  , mkEnv
  , putLeaf
  , delLeaf
  , getLeaf
  , getLog
  , close
  ) where

import           Control.Concurrent.MVar  (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception        (Exception, finally, onException,
                                           throwIO)
import           Control.Monad            (unless)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Serialize           as S
import           Data.Word                (Word64)
import           Database.LMDB.Raw
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Marshal.Alloc    (alloca)
import           Foreign.Marshal.Array    (withArray)
import           Foreign.Marshal.Utils    (copyBytes)
import           Foreign.Ptr              (Ptr, plusPtr)
import           Foreign.Storable         (peek, poke)

import qualified Data.Hounds.Hash         as Hash
import qualified Data.Hounds.LeafValue    as LeafValue
import qualified Data.Hounds.Log          as Log


data DbException
  = GetException
  | PutException
  | LogException
  | SerializationException String
  deriving Show

instance Exception DbException

-- * Low-level API

writeFlags :: MDB_WriteFlags
writeFlags = compileWriteFlags [MDB_NOOVERWRITE]

mdbValToByteString :: MDB_val -> IO B.ByteString
mdbValToByteString MDB_val{mv_size, mv_data}
  = let
      size = fromIntegral mv_size
    in
      BI.create size $ \ ptr -> copyBytes ptr mv_data size

get :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> IO (Maybe B.ByteString)
get dbi txn bs
  = let
      (fptr, off, len) = BI.toForeignPtr bs
    in
      withForeignPtr fptr $ \ ptr ->
      do ret <- mdb_get txn dbi (MDB_val (fromIntegral len) (ptr `plusPtr` off))
         mapM mdbValToByteString ret

del :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> IO Bool
del dbi txn bs
  = let
      (fptr, off, len) = BI.toForeignPtr bs
    in
      withForeignPtr fptr $ \ ptr ->
      mdb_del txn dbi (MDB_val (fromIntegral len) (ptr `plusPtr` off)) Nothing

put :: MDB_dbi
    -> MDB_txn
    -> B.ByteString
    -> B.ByteString
    -> IO Bool
put dbi txn kbs vbs
  = let
      (kfptr, koff, klen) = BI.toForeignPtr kbs
      (vfptr, voff, vlen) = BI.toForeignPtr vbs
    in
      withForeignPtr kfptr $ \ kptr ->
      withForeignPtr vfptr $ \ vptr ->
      mdb_put writeFlags txn dbi (MDB_val (fromIntegral klen) (kptr `plusPtr` koff))
                                 (MDB_val (fromIntegral vlen) (vptr `plusPtr` voff))

-- * High-level API

data Db = MkDb
  { dbEnv            :: MDB_env
  , dbDbiLeaves      :: MDB_dbi
  , dbDbiTrie        :: MDB_dbi
  , dbDbiLog         :: MDB_dbi
  , dbDbiCheckpoints :: MDB_dbi
  }

mkDb :: FilePath -> Int -> IO Db
mkDb dbDir mapSize = do
  env <- mdb_env_create
  mdb_env_set_mapsize env mapSize
  mdb_env_set_maxreaders env 4
  mdb_env_set_maxdbs env 4
  mdb_env_open env dbDir []
  txn <- mdb_txn_begin env Nothing False
  onException (do dbiLeaves      <- mdb_dbi_open txn (Just "leaves")      [MDB_CREATE]
                  dbiTrie        <- mdb_dbi_open txn (Just "trie")        [MDB_CREATE]
                  dbiLog         <- mdb_dbi_open txn (Just "log")         [MDB_CREATE]
                  dbiCheckpoints <- mdb_dbi_open txn (Just "checkpoints") [MDB_CREATE]
                  mdb_txn_commit txn
                  return MkDb { dbEnv            = env
                              , dbDbiLeaves      = dbiLeaves
                              , dbDbiTrie        = dbiTrie
                              , dbDbiLog         = dbiLog
                              , dbDbiCheckpoints = dbiCheckpoints
                              })
              (mdb_txn_abort txn)

data Env = MkEnv
  { envDb       :: Db
  , envTxnCount :: MVar Word64
  }

mkEnv :: Db -> IO Env
mkEnv db = MkEnv db <$> newMVar 0

putLeaf :: Env -> B.ByteString -> IO Hash.Hash
putLeaf env bs = do
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

delLeaf :: Env -> Hash.Hash -> IO Bool
delLeaf env hash = do
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

getLeaf :: Env -> Hash.Hash -> IO (Maybe B.ByteString)
getLeaf env hash = do
  let db = envDb env
  txn <- mdb_txn_begin (dbEnv db) Nothing True
  finally (do mbs <- get (dbDbiLeaves db) txn (S.encode hash)
              case mbs of
                Nothing -> return Nothing
                Just bs -> do let val = S.decode bs :: Either String LeafValue.LeafValue
                              leafValue <- either (throwIO . SerializationException) pure val
                              return $ if LeafValue.leafValueDeleted leafValue
                                         then Nothing
                                         else Just (LeafValue.leafValueBytes leafValue))
          (mdb_txn_abort txn)

deserializeLogEntry :: Ptr MDB_val
                    -> Ptr MDB_val
                    -> IO (Either String (Log.LogKey, Log.Operation))
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

getLog :: Env -> Log.Range -> IO [(Log.LogKey, Log.Operation)]
getLog env range = do
  let db = envDb env
  txn    <- mdb_txn_begin (dbEnv db) Nothing False
  cursor <- mdb_cursor_open txn (dbDbiLog db)
  finally (do succPos <- positionLo cursor range
              if succPos
                then go cursor range []
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
