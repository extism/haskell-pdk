module Extism.PDK.Memory (Memory (..), MemoryOffset, MemoryLength, inputMemory, load, store, outputMemory) where

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
