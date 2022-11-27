module Extism.PDK where

import Extism.PDK.Bindings
import Data.Word
import Data.Int
import Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Unsafe (unsafeUseAsCString)


data Memory = Memory MemoryOffset MemoryLength

-- Helper function to convert a string to a bytestring
toByteString :: String -> ByteString
toByteString x = B.pack (Prelude.map c2w x)

-- Helper function to convert a bytestring to a string
fromByteString :: ByteString -> String
fromByteString bs = Prelude.map w2c $ B.unpack bs

readInputByte i =
  extismInputLoadU8 i
  
readInputBytes len =
  let b = [1, 2 .. len] in
  do
    bytes <- Prelude.mapM (extismInputLoadU8) b
    return $ B.pack bytes
    
input :: () -> IO ByteString
input () = do
  len <- extismInputLength
  readInputBytes len
  
inputString :: () -> IO String
inputString () = do
  x <- input ()
  return $ fromByteString x

load :: Memory -> IO ByteString
load (Memory offs len) =
  let b = [1, 2 .. len] in
  do
    bytes <- Prelude.mapM (\x -> extismLoadU8 (offs + x)) b
    return $ B.pack bytes
    
store :: Memory -> ByteString -> IO ()
store (Memory offs len) bs =
  let bytes = Prelude.zip [0..] (B.unpack bs) in
  Prelude.mapM_ (\(index, x) -> extismStoreU8 (offs + index) x) bytes

output :: ByteString -> IO ()
output bs =
  let len = fromIntegral $ B.length bs in
  do
    offs <- extismAlloc len
    b <- store (Memory offs len) bs
    extismSetOutput offs len

outputString :: String -> IO ()
outputString s =
  let bs = toByteString s in
  output bs