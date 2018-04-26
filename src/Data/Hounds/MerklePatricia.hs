module Data.Hounds.MerklePatricia where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2b
import           Data.Array
import qualified Data.ByteString            as B
import           Data.Int                   (Int32)
import           Data.Serialize
import           Database.LMDB.Raw


data Db = MkDb { dbEnv :: MDB_env, dbDbi :: MDB_dbi }

mkDb :: FilePath -> IO Db
mkDb dbDir = do
  env <- mdb_env_create
  let mapSize = 1024 * 1024 * 100
  _   <- mdb_env_set_mapsize env mapSize
  _   <- mdb_env_set_maxreaders env 4
  _   <- mdb_env_set_maxdbs env 1
  _   <- mdb_env_open env dbDir []
  txn <- mdb_txn_begin env Nothing False
  dbi <- mdb_dbi_open txn (Just "db") []
  _   <- mdb_txn_commit txn
  return MkDb { dbEnv = env, dbDbi = dbi }

type Hash = B.ByteString

blake2b :: B.ByteString -> Hash
blake2b = BLAKE2b.hash 32 mempty

type PointerBlock = Array Int32 (Maybe Hash)

mkPointerBlock :: PointerBlock
mkPointerBlock = listArray (0, 255) (replicate 256 Nothing)

update :: PointerBlock -> (Int32, Maybe B.ByteString) -> PointerBlock
update pointerBlock indexedHash = pointerBlock // [indexedHash]

putterPointerBlock :: Putter PointerBlock
putterPointerBlock = putIArrayOf putInt32le (putMaybeOf putByteString)

getPointerBlock :: Get PointerBlock
getPointerBlock = getIArrayOf getInt32le (getMaybeOf (getByteString 32))

serializePointerBlock :: PointerBlock -> B.ByteString
serializePointerBlock = runPut . putterPointerBlock

deserializePointerBlock :: B.ByteString -> Either String PointerBlock
deserializePointerBlock = runGet getPointerBlock

data Tree
  = Node PointerBlock
  | Leaf Hash
  deriving (Eq, Show)

instance Serialize Tree where
  put (Node pb) = do { putWord8 0
                     ; putterPointerBlock pb
                     }
  put (Leaf bs) = do { putWord8 1
                     ; putByteString bs
                     }
  get           = do { tag <- getWord8
                     ; case tag of
                         0 -> Node <$> getPointerBlock
                         1 -> Leaf <$> getByteString 32
                         _ -> fail "no such tag"
                     }
