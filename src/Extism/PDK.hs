module Extism.PDK (module Extism.PDK, module Extism.Manifest) where

import Extism.PDK.Bindings
import Extism.Manifest(JSONValue, toJSONValue, toString)
import Data.Word
import Data.Int
import Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Text.JSON(JSON, decode, resultToEither)

-- | Represents a block of memory
data Memory = Memory MemoryOffset MemoryLength

-- | Helper function to convert a string to a bytestring
toByteString :: String -> ByteString
toByteString x = B.pack (Prelude.map c2w x)

-- | Helper function to convert a bytestring to a string
fromByteString :: ByteString -> String
fromByteString bs = Prelude.map w2c $ B.unpack bs

-- | Get plugin input as 'ByteString'
input :: IO ByteString
input = do
  len <- extismInputLength
  readInputBytes len

-- | Get plugin input as 'String'
inputString :: IO String
inputString = fromByteString <$> input

-- | Get plugin input as 'Memory' block
inputMemory :: IO Memory
inputMemory = do
  len <- extismInputLength
  offs <- extismAlloc len
  Prelude.mapM_ (\x ->
    extismStoreU8 (offs + x) <$> extismInputLoadU8 x) [0, 1 .. len]
  return $ Memory offs len

-- | Get input as 'JSON'
inputJSON :: JSON a => IO (Maybe a)
inputJSON = do
  s <- inputString
  case resultToEither $ decode s of
    Left _ -> return Nothing
    Right x -> return (Just x)

-- | Load data from 'Memory' block into a 'ByteString'
load :: Memory -> IO ByteString
load (Memory offs len) =
  readBytes offs len

-- | Store data from a 'ByteString' into a 'Memory' block
store :: Memory -> ByteString -> IO ()
store (Memory offs len) bs =
  writeBytes offs len bs

-- | Set plugin output to the provided 'Memory' block
outputMemory :: Memory -> IO ()
outputMemory (Memory offs len) =
  extismSetOutput offs len

-- | Set plugin output to the provided 'ByteString'
output :: ByteString -> IO ()
output bs =
  let len = fromIntegral $ B.length bs in
  do
    offs <- extismAlloc len
    b <- store (Memory offs len) bs
    extismSetOutput offs len

-- | Set plugin output to the provided 'String'
outputString :: String -> IO ()
outputString s =
  let bs = toByteString s in
  output bs

-- | Set plugin output to a JSON encoded version of the provided value
outputJSON :: JSONValue a => a -> IO ()
outputJSON x =
  outputString (toString $ toJSONValue x)

-- | Load string from 'Memory' block
loadString :: Memory -> IO String
loadString mem = do
  bs <- load mem
  return $ fromByteString bs

-- | Store string in 'Memory' block
storeString :: Memory -> String -> IO ()
storeString mem s =
  let bs = toByteString s in
  store mem bs

-- | Allocate a new 'Memory' block
alloc :: Int -> IO Memory
alloc n =
  let len = fromIntegral n in
  do
    offs <- extismAlloc len
    return $ Memory offs len

-- | Free a 'Memory' block
free :: Memory -> IO ()
free (Memory 0 _) = return ()
free (Memory _ 0) = return ()
free (Memory offs _) =
  extismFree offs

-- | Allocate a new 'Memory' block and copy the contents of the provided 'ByteString'
allocByteString :: ByteString -> IO Memory
allocByteString bs = do
  mem <- alloc (B.length bs)
  store mem bs
  return mem

-- | Allocate a new 'Memory' block and copy the contents of the provided 'String'
allocString :: String -> IO Memory
allocString s =
  let bs = toByteString s in
  allocByteString bs

-- | Get the offset of a 'Memory' block
memoryOffset (Memory offs _) = offs

-- | Get the length of a 'Memory' block
memoryLength (Memory _ len) = len

-- | Find 'Memory' block by offset
findMemory offs = do
  len <- extismLength offs
  return $ Memory offs len

-- | Get a variable from the Extism runtime
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

-- | Set a variable
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

-- | Get a configuration value
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
    return $ Just s

-- | Set the current error message
error :: String -> IO ()
error msg = do
  s <- allocString msg
  extismSetError $ memoryOffset s
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
