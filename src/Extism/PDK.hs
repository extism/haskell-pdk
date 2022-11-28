module Extism.PDK where

import Extism.PDK.Bindings
import Extism.PDK.HTTP
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
  let b = [0, 1 .. len] in
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
  let b = [0, 1 .. len] in
  do
    -- TODO: use extismLoadU64 to reduce total number of loads
    bytes <- Prelude.mapM (\x -> extismLoadU8 (offs + x)) b
    return $ B.pack bytes

store :: Memory -> ByteString -> IO ()
store (Memory offs len) bs =
  let bytes = Prelude.zip [0..] (B.unpack bs) in
  -- TODO: use extismStoreU64 to reduce total number of stores
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

loadString :: Memory -> IO String
loadString mem = do
  bs <- load mem
  return $ fromByteString bs

storeString :: Memory -> String -> IO ()
storeString mem s =
  let bs = toByteString s in
  store mem bs

alloc :: Int -> IO Memory
alloc n =
  let len = fromIntegral n in
  do
    offs <- extismAlloc len
    return $ Memory offs len

free :: Memory -> IO ()
free (Memory offs _) =
  extismFree offs

withMemory :: (Memory -> IO a) -> Memory -> IO a
withMemory f m = do
  x <- f m
  free m
  return x

allocByteString :: ByteString -> IO Memory
allocByteString bs = do
  mem <- alloc (B.length bs)
  store mem bs
  return mem

allocString :: String -> IO Memory
allocString s =
  let bs = toByteString s in
  allocByteString bs

memoryOffset (Memory offs _) = offs
memoryLength (Memory _ len) = len
findMemory offs = do
  len <- extismLength offs
  return $ Memory offs len

getVar :: String -> IO (Maybe ByteString)
getVar key = do
  k <- allocString key
  v <- extismGetVar (memoryOffset k)
  free k
  if v == 0 then
    return Nothing
  else do
    mem <- findMemory v
    bs <- load mem
    free mem
    return (Just bs)

setVar :: String -> Maybe ByteString -> IO ()
setVar key Nothing = do
  k <- allocString key
  extismSetVar (memoryOffset k) 0
  free k
setVar key (Just v) = do
  k <- allocString key
  x <- allocByteString v
  extismSetVar (memoryOffset k) (memoryOffset x)
  free k
  free x

getConfig :: String -> IO (Maybe String)
getConfig key = do
  k <- allocString key
  v <- extismGetConfig (memoryOffset k)
  free k
  if v == 0 then
    return Nothing
  else do
    mem <- findMemory v
    s <- loadString mem
    free mem
    return (Just s)

error :: String -> IO ()
error msg = do
  s <- allocString msg
  extismSetError (memoryOffset s)
  free s
  
data LogLevel = Info | Debug | Warn | Error

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


tryFree (Memory 0 0) = return ()
tryFree x = free x

httpRequest :: Request -> Maybe ByteString -> IO Response
httpRequest req b = 
  let json = toString req in
  let bodyMem = case b of
               Nothing -> return $ Memory 0 0
               Just b -> allocByteString b
  in
  do
    body <- bodyMem
    j <- allocString json
    res <- extismHTTPRequest (memoryOffset j) (memoryOffset body)
    free j
    tryFree body
    code <- extismHTTPStatusCode
    if res == 0 then 
      return (Response (fromIntegral code) Nothing)
    else do
      mem <- findMemory res
      bytes <- load mem
      free mem
      return (Response (fromIntegral code) (Just bytes))