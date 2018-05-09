{-# LANGUAGE NamedFieldPuns #-}

module Data.Hounds.Db
  ( DbException(..)
  , Db(..)
  , mkDb
  , close
  , put
  , get
  , del
  , putOrThrow
  , getOrThrow
  ) where

import           Control.Exception        (Exception, onException, throwIO)
import           Control.Monad            (unless)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI
import           Data.Serialize           (Serialize, decode, encode)
import           Database.LMDB.Raw
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Marshal.Utils    (copyBytes)
import           Foreign.Ptr              (plusPtr)


data DbException
  = PutException
  | GetException
  | DelException
  | SerializationException String
  deriving Show

instance Exception DbException

data Db = MkDb
  { dbEnv      :: MDB_env
  , dbDbiStore :: MDB_dbi
  , dbDbiTrie  :: MDB_dbi
  }

mkDb :: FilePath -> Int -> IO Db
mkDb dbDir mapSize = do
  env <- mdb_env_create
  mdb_env_set_mapsize env mapSize
  mdb_env_set_maxreaders env 126
  mdb_env_set_maxdbs env 2
  mdb_env_open env dbDir []
  txn <- mdb_txn_begin env Nothing False
  onException (do dbiStore <- mdb_dbi_open txn (Just "store") [MDB_CREATE]
                  dbiTrie  <- mdb_dbi_open txn (Just "trie")  [MDB_CREATE]
                  mdb_txn_commit txn
                  return MkDb { dbEnv      = env
                              , dbDbiStore = dbiStore
                              , dbDbiTrie  = dbiTrie
                              })
              (mdb_txn_abort txn)

close :: Db -> IO ()
close db = do
  let env = dbEnv db
  mdb_dbi_close env (dbDbiStore db)
  mdb_dbi_close env (dbDbiTrie  db)
  mdb_env_close env

writeFlags :: MDB_WriteFlags
writeFlags = compileWriteFlags [MDB_NOOVERWRITE]

mdbValToByteString :: MDB_val -> IO B.ByteString
mdbValToByteString MDB_val{mv_size, mv_data}
  = let
      size = fromIntegral mv_size
    in
      BI.create size $ \ ptr -> copyBytes ptr mv_data size

put :: (Serialize k, Serialize v)
    => MDB_txn
    -> MDB_dbi
    -> k
    -> v
    -> IO Bool
put txn dbi k v
  = let
      (kfptr, koff, klen) = BI.toForeignPtr (encode k)
      (vfptr, voff, vlen) = BI.toForeignPtr (encode v)
    in
      withForeignPtr kfptr $ \ kptr ->
      withForeignPtr vfptr $ \ vptr ->
      mdb_put writeFlags txn dbi (MDB_val (fromIntegral klen) (kptr `plusPtr` koff))
                                 (MDB_val (fromIntegral vlen) (vptr `plusPtr` voff))

get :: (Serialize k, Serialize v)
    => MDB_txn
    -> MDB_dbi
    -> k
    -> IO (Maybe v)
get txn dbi k
  = let
      (fptr, off, len) = BI.toForeignPtr (encode k)
      de               = either (throwIO . SerializationException) pure . decode
    in
      withForeignPtr fptr $ \ ptr ->
      do ret <- mdb_get txn dbi (MDB_val (fromIntegral len) (ptr `plusPtr` off))
         bs  <- mapM mdbValToByteString ret
         mapM de bs

del :: Serialize k
    => MDB_txn
    -> MDB_dbi
    -> k
    -> IO Bool
del txn dbi k
  = let
      (fptr, off, len) = BI.toForeignPtr (encode k)
    in
      withForeignPtr fptr $ \ ptr ->
      mdb_del txn dbi (MDB_val (fromIntegral len) (ptr `plusPtr` off)) Nothing

getOrThrow :: (Serialize k, Serialize v, Exception e)
           => MDB_txn
           -> MDB_dbi
           -> k
           -> e
           -> IO v
getOrThrow txn dbi k e = get txn dbi k >>= maybe (throwIO e) return

putOrThrow :: (Serialize k, Serialize v, Exception e)
           => MDB_txn
           -> MDB_dbi
           -> k
           -> v
           -> e
           -> IO ()
putOrThrow txn dbi k v e = put txn dbi k v >>= flip unless (throwIO e)

