{-# LANGUAGE FlexibleInstances #-}

module Extism.PDK (module Extism.PDK, module Extism.Manifest, ToBytes (..), FromBytes (..)) where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Int
import Data.Word
import Extism.JSON (JSON, JSValue)
import Extism.Manifest (toString)
import Extism.PDK.Bindings
import Extism.PDK.Memory
import qualified Extism.PDK.MsgPack (MsgPack, decode, encode)
import Extism.PDK.Util
import Text.JSON (JSON, decode, encode, resultToEither)

-- | A wrapper type for JSON encoded values
newtype JSONValue a = JSONValue a

-- | A wrapper type for MsgPack encoded values
newtype MsgPackValue a = MsgPackValue a

instance FromBytes ByteString where
  fromBytes = Right

instance ToBytes ByteString where
  toBytes bs = bs

instance FromBytes String where
  fromBytes = Right . fromByteString

instance ToBytes String where
  toBytes = toByteString

instance (JSON a) => FromBytes (JSONValue a) where
  fromBytes x =
    case resultToEither $ decode (fromByteString x) of
      Left e -> Left e
      Right y -> Right (JSONValue y)

instance (JSON a) => ToBytes (JSONValue a) where
  toBytes (JSONValue x) = toByteString (encode x)

instance (Extism.PDK.MsgPack.MsgPack a) => FromBytes (MsgPackValue a) where
  fromBytes x =
    case Extism.PDK.MsgPack.decode x of
      Left e -> Left e
      Right y -> Right (MsgPackValue y)

instance (Extism.PDK.MsgPack.MsgPack a) => ToBytes (MsgPackValue a) where
  toBytes (MsgPackValue x) = Extism.PDK.MsgPack.encode x

-- | Get plugin input
input :: (FromBytes a) => IO (Either String a)
input = do
  len <- extismInputLength
  fromBytes <$> readInputBytes len

-- | Get plugin input as a String
inputString :: IO String
inputString = do
  len <- extismInputLength
  fromByteString <$> readInputBytes len

-- | Get plugin input as a ByteString
inputByteString :: IO ByteString
inputByteString = do
  len <- extismInputLength
  readInputBytes len

-- | Get input as 'JSON', this is similar to calling `input (JsonValue ...)`
inputJSON :: (JSON a) => IO (Either String a)
inputJSON = do
  s <- input :: IO (Either String String)
  case s of
    Left e -> return (Left e)
    Right x ->
      case resultToEither $ decode x of
        Left e -> return (Left e)
        Right y -> return (Right y)

-- | Set plugin output
output :: (ToBytes a) => a -> IO ()
output x =
  let bs = toBytes x
   in let len = fromIntegral $ B.length bs
       in do
            offs <- extismAlloc len
            b <- store (Memory offs len) bs
            extismSetOutput offs len

-- | Set plugin output to a JSON encoded version of the provided value
outputJSON :: (JSON a) => a -> IO ()
outputJSON x =
  output (toString x)

-- | Load string from 'Memory' block
loadString :: Memory -> IO String
loadString mem = do
  bs <- load mem
  case bs of
    Left e -> error e
    Right x -> return $ fromByteString x

-- | Store string in 'Memory' block
storeString :: Memory -> String -> IO ()
storeString mem s =
  let bs = toByteString s
   in store mem bs

-- | Allocate a new 'Memory' block
alloc :: Int -> IO Memory
alloc n =
  let len = fromIntegral n
   in do
        offs <- extismAlloc len
        return $ Memory offs len

-- | Free a 'Memory' block
free :: Memory -> IO ()
free (Memory 0 _) = return ()
free (Memory _ 0) = return ()
free (Memory offs _) =
  extismFree offs

-- | Allocate a new 'Memory' block and copy the encoded value
allocBytes :: (ToBytes a) => a -> IO Memory
allocBytes x = do
  let bs = toBytes x
  mem <- alloc (B.length bs)
  store mem bs
  return mem

-- | Allocate a new 'Memory' block and copy the contents of the provided 'String'
allocString :: String -> IO Memory
allocString = allocBytes

-- | Get the offset of a 'Memory' block
memoryOffset :: Memory -> Word64
memoryOffset (Memory offs _) = offs

-- | Get the length of a 'Memory' block
memoryLength :: Memory -> Word64
memoryLength (Memory _ len) = len

-- | Find 'Memory' block by offset
findMemory :: Word64 -> IO Memory
findMemory offs = do
  len <- extismLength offs
  return $ Memory offs len

-- | Get a variable from the Extism runtime
getVar :: (FromBytes a) => String -> IO (Maybe a)
getVar key = do
  k <- allocString key
  v <- extismGetVar (memoryOffset k)
  free k
  if v == 0
    then return Nothing
    else do
      mem <- findMemory v
      bs <- load mem
      free k
      case bs of
        Left _ -> return Nothing
        Right x -> return (Just x)

-- | Set a variable
setVar :: (ToBytes a) => String -> Maybe a -> IO ()
setVar key Nothing = do
  k <- allocString key
  extismSetVar (memoryOffset k) 0
  free k
setVar key (Just v) = do
  k <- allocString key
  x <- allocBytes v
  extismSetVar (memoryOffset k) (memoryOffset x)
  free k
  free x

-- | Get a configuration value
getConfig :: String -> IO (Maybe String)
getConfig key = do
  k <- allocString key
  v <- extismGetConfig (memoryOffset k)
  free k
  if v == 0
    then return Nothing
    else do
      mem <- findMemory v
      s <- loadString mem
      free mem
      return $ Just s

-- | Set the current error message
setError :: String -> IO ()
setError msg = do
  s <- allocString msg
  extismSetError $ memoryOffset s
  free s

-- | Log level
data LogLevel = Info | Debug | Warn | Error

-- | Log to configured log file
log :: LogLevel -> String -> IO ()
log Info msg = do
  s <- allocString msg
  extismLogInfo (memoryOffset s)
  free s
log Debug msg = do
  s <- allocString msg
  extismLogDebug (memoryOffset s)
  free s
log Warn msg = do
  s <- allocString msg
  extismLogWarn (memoryOffset s)
  free s
log Error msg = do
  s <- allocString msg
  extismLogError (memoryOffset s)
  free s

instance ToBytes Int32 where
  toBytes i = B.toStrict (runPut (putInt32le i))

instance FromBytes Int32 where
  fromBytes bs =
    case runGetOrFail getInt32le (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Int64 where
  toBytes i = B.toStrict (runPut (putInt64le i))

instance FromBytes Int64 where
  fromBytes bs =
    case runGetOrFail getInt64le (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Word32 where
  toBytes i = B.toStrict (runPut (putWord32le i))

instance FromBytes Word32 where
  fromBytes bs =
    case runGetOrFail getWord32le (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Word64 where
  toBytes i = B.toStrict (runPut (putWord64le i))

instance FromBytes Word64 where
  fromBytes bs =
    case runGetOrFail getWord64le (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Float where
  toBytes i = B.toStrict (runPut (putFloatle i))

instance FromBytes Float where
  fromBytes bs =
    case runGetOrFail getFloatle (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Double where
  toBytes i = B.toStrict (runPut (putDoublele i))

instance FromBytes Double where
  fromBytes bs =
    case runGetOrFail getDoublele (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x
