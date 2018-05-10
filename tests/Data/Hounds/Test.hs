module Data.Hounds.Test where

import           Control.Exception        (Exception, throw, throwIO)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import           Data.Serialize
import           Data.Word                (Word8)
import           System.IO.Temp           (createTempDirectory,
                                           getCanonicalTemporaryDirectory)

import qualified Data.Hounds.Context      as Context
import qualified Data.Hounds.Db           as Db
import qualified Data.Hounds.PointerBlock as PointerBlock
import qualified Data.Hounds.Trie         as Trie


initTempDb :: IO Db.Db
initTempDb = do
  dir  <- getCanonicalTemporaryDirectory
  path <- createTempDirectory dir "hounds-"
  Db.mkDb path (1024 * 1024 * 128)

initTempEnv :: (Serialize k, Serialize v) => IO (Context.Context k v)
initTempEnv = do
  let trie     = Trie.mkTrie PointerBlock.mkPointerBlock
      rootHash = Trie.hashTrie trie
  tempDb  <- initTempDb
  context <- Context.mkContext tempDb rootHash
  succPut <- Trie.store context rootHash trie
  if succPut
    then return context
    else throwIO Db.PutException

data TestKeyException
  = InvalidSizeException
  deriving Show

instance Exception TestKeyException

newtype TestKey = MkTestKey { unTestKey :: B.ByteString }
  deriving Eq

instance Show TestKey where
  show (MkTestKey bs) = "TestKey " ++ show (B.unpack bs)

putTestKey :: Putter TestKey
putTestKey (MkTestKey bs) = putByteString bs

getTestKey :: Get TestKey
getTestKey = MkTestKey <$> getByteString 4

instance Serialize TestKey where
  put = putTestKey
  get = getTestKey

mkTestKey :: [Word8] -> TestKey
mkTestKey ws | length ws == 4 = MkTestKey (B.pack ws)
             | otherwise      = throw InvalidSizeException

type TestContext = Context.Context TestKey B.ByteString

key1, key2, key3, key4, key5 :: TestKey
key1 = mkTestKey [1, 0, 0, 0]
key2 = mkTestKey [1, 0, 0, 1]
key3 = mkTestKey [1, 0, 1, 0]
key4 = mkTestKey [1, 0, 1, 1]
key5 = mkTestKey [1, 0, 2, 1]

val1, val2, val3, val4, val5 :: B.ByteString
val1 = C.pack "value1"
val2 = C.pack "value2"
val3 = C.pack "value3"
val4 = C.pack "value4"
val5 = C.pack "value5"
