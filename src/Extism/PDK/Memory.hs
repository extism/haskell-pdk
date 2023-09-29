{-# LANGUAGE FlexibleInstances #-}

module Extism.PDK.Memory
  ( Memory (..),
    MemoryOffset,
    MemoryLength,
    FromBytes (..),
    ToBytes (..),
    JSONValue (..),
    MsgPackValue (..),
    load,
    loadString,
    loadByteString,
    outputMemory,
    memAlloc,
    free,
    alloc,
    allocString,
    allocByteString,
    memoryOffset,
    memoryLength,
    findMemory,
  )
where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.Int
import Data.Word
import Extism.PDK.Bindings
import qualified Extism.PDK.MsgPack (MsgPack, decode, encode)
import Extism.PDK.Util
import Text.JSON (JSON, decode, encode, resultToEither)

-- | Represents a block of memory by offset and length
data Memory = Memory MemoryOffset MemoryLength

-- | Load data from 'Memory' block
load :: (FromBytes a) => Memory -> IO (Either String a)
load (Memory offs len) = do
  x <- readBytes offs len
  return $ fromBytes x

-- | Store data into a 'Memory' block
-- store :: (ToBytes a) => Memory -> a -> IO ()
-- store (Memory offs len) a =
--   let bs = toBytes a
--    in writeBytes offs len bs

-- | Set plugin output to the provided 'Memory' block
outputMemory :: Memory -> IO ()
outputMemory (Memory offs len) =
  extismSetOutput offs len

-- | Load ByteString from 'Memory' block
loadByteString :: Memory -> IO B.ByteString
loadByteString (Memory offs len) = do
  readBytes offs len

-- | Load string from 'Memory' block
loadString :: Memory -> IO String
loadString (Memory offs len) =
  fromByteString <$> readBytes offs len

-- | Store string in 'Memory' block
-- storeString :: Memory -> String -> IO ()
-- storeString mem s =
--   let bs = toByteString s
--    in store mem bs

-- | Encode a value and copy it into Extism memory, returning the Memory block
alloc :: (ToBytes a) => a -> IO Memory
alloc x =
  let bs = toBytes x
   in do
        Memory offs len <- memAlloc (B.length bs)
        writeBytes offs len bs
        return $ Memory offs len

-- | Allocate a new 'Memory' block
memAlloc :: Int -> IO Memory
memAlloc n =
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
allocByteString :: B.ByteString -> IO Memory
allocByteString bs = do
  Memory offs len <- memAlloc (B.length bs)
  writeBytes offs len bs
  return (Memory offs len)

-- | Allocate a new 'Memory' block and copy the contents of the provided 'String'
allocString :: String -> IO Memory
allocString = allocByteString . toByteString

-- | Get the offset of a 'Memory' block
memoryOffset :: Memory -> MemoryOffset
memoryOffset (Memory offs _) = offs

-- | Get the length of a 'Memory' block
memoryLength :: Memory -> MemoryLength
memoryLength (Memory _ len) = len

-- | Find 'Memory' block by offset
findMemory :: MemoryOffset -> IO Memory
findMemory offs = do
  len <- extismLength offs
  return $ Memory offs len

-- | A class used to convert values from bytes read from linear memory
class FromBytes a where
  fromBytes :: B.ByteString -> Either String a

-- | A class used to convert values to bytes to be written into linear memory
class ToBytes a where
  toBytes :: a -> B.ByteString

-- | A wrapper type for JSON encoded values
newtype JSONValue a = JSONValue a

-- | A wrapper type for MsgPack encoded values
newtype MsgPackValue a = MsgPackValue a

instance FromBytes B.ByteString where
  fromBytes = Right

instance ToBytes B.ByteString where
  toBytes = id

instance FromBytes String where
  fromBytes mem =
    let s = fromBytes mem
     in case s of
          Left e -> Left e
          Right x -> Right $ fromByteString x

instance ToBytes String where
  toBytes = toByteString

instance (JSON a) => FromBytes (JSONValue a) where
  fromBytes mem =
    let a = fromBytes mem
     in case a of
          Left e -> Left e
          Right x ->
            case resultToEither $ decode x of
              Left e -> Left e
              Right y -> Right (JSONValue y)

instance (JSON a) => ToBytes (JSONValue a) where
  toBytes (JSONValue x) = toBytes (encode x)

instance (Extism.PDK.MsgPack.MsgPack a) => FromBytes (MsgPackValue a) where
  fromBytes mem =
    let a = fromBytes mem
     in case a of
          Left e -> Left e
          Right x ->
            case Extism.PDK.MsgPack.decode x of
              Left e -> Left e
              Right y -> Right (MsgPackValue y)

instance (Extism.PDK.MsgPack.MsgPack a) => ToBytes (MsgPackValue a) where
  toBytes (MsgPackValue x) = toBytes $ Extism.PDK.MsgPack.encode x

instance ToBytes Int32 where
  toBytes i = toBytes $ B.toStrict (runPut (putInt32le i))

instance FromBytes Int32 where
  fromBytes mem =
    let bs = fromBytes mem
     in case bs of
          Left e -> Left e
          Right x ->
            case runGetOrFail getInt32le (B.fromStrict x) of
              Left (_, _, e) -> Left e
              Right (_, _, x) -> Right x

instance ToBytes Int64 where
  toBytes i = toBytes $ B.toStrict (runPut (putInt64le i))

instance FromBytes Int64 where
  fromBytes mem =
    let bs = fromBytes mem
     in case bs of
          Left e -> Left e
          Right x ->
            case runGetOrFail getInt64le (B.fromStrict x) of
              Left (_, _, e) -> Left e
              Right (_, _, x) -> Right x

instance ToBytes Word32 where
  toBytes i = toBytes $ B.toStrict (runPut (putWord32le i))

instance FromBytes Word32 where
  fromBytes mem =
    let bs = fromBytes mem
     in case bs of
          Left e -> Left e
          Right x ->
            case runGetOrFail getWord32le (B.fromStrict x) of
              Left (_, _, e) -> Left e
              Right (_, _, x) -> Right x

instance ToBytes Word64 where
  toBytes i = toBytes $ B.toStrict (runPut (putWord64le i))

instance FromBytes Word64 where
  fromBytes mem =
    let bs = fromBytes mem
     in case bs of
          Left e -> Left e
          Right x ->
            case runGetOrFail getWord64le (B.fromStrict x) of
              Left (_, _, e) -> Left e
              Right (_, _, x) -> Right x

instance ToBytes Float where
  toBytes i = toBytes $ B.toStrict (runPut (putFloatle i))

instance FromBytes Float where
  fromBytes mem =
    let bs = fromBytes mem
     in case bs of
          Left e -> Left e
          Right x ->
            case runGetOrFail getFloatle (B.fromStrict x) of
              Left (_, _, e) -> Left e
              Right (_, _, x) -> Right x

instance ToBytes Double where
  toBytes i = toBytes $ B.toStrict (runPut (putDoublele i))

instance FromBytes Double where
  fromBytes mem =
    let bs = fromBytes mem
     in case bs of
          Left e -> Left e
          Right x ->
            case runGetOrFail getDoublele (B.fromStrict x) of
              Left (_, _, e) -> Left e
              Right (_, _, x) -> Right x
