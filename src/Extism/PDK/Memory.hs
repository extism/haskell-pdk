module Extism.PDK.Memory
  ( Memory (..),
    MemoryOffset,
    MemoryLength,
    inputMemory,
    load,
    loadString,
    loadByteString,
    store,
    outputMemory,
    alloc,
    free,
    allocString,
    allocBytes,
    memoryOffset,
    memoryLength,
    findMemory,
  )
where

import qualified Data.ByteString as B
import Extism.PDK.Bindings
import Extism.PDK.Util

-- | Represents a block of memory by offset and length
data Memory = Memory MemoryOffset MemoryLength

-- | Get plugin input as 'Memory' block
inputMemory :: IO Memory
inputMemory = do
  len <- extismInputLength
  offs <- extismAlloc len
  Prelude.mapM_
    ( \x ->
        extismStoreU8 (offs + x) <$> extismInputLoadU8 x
    )
    [0, 1 .. len]
  return $ Memory offs len

-- | Load data from 'Memory' block
load :: (FromBytes a) => Memory -> IO (Either String a)
load (Memory offs len) =
  fromBytes <$> readBytes offs len

-- | Store data into a 'Memory' block
store :: (ToBytes a) => Memory -> a -> IO ()
store (Memory offs len) a =
  let bs = toBytes a
   in writeBytes offs len bs

-- | Set plugin output to the provided 'Memory' block
outputMemory :: Memory -> IO ()
outputMemory (Memory offs len) =
  extismSetOutput offs len

-- | Load ByteString from 'Memory' block
loadByteString :: Memory -> IO B.ByteString
loadByteString mem = do
  bs <- load mem
  case bs of
    Left e -> error e
    Right x -> return x

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
