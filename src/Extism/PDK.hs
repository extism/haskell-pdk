module Extism.PDK
  ( module Extism.PDK,
    module Extism.Manifest,
    ToBytes (..),
    FromBytes (..),
    JSONValue (..),
    MsgPackValue (..),
  )
where

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

-- | Get plugin input
input :: (FromBytes a) => IO (Either String a)
input = fromBytes <$> inputByteString

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
output x = do
  Memory offs len <- alloc x
  extismSetOutput offs len

-- | Set plugin output to a JSON encoded version of the provided value
outputJSON :: (JSON a) => a -> IO ()
outputJSON x =
  output (toString x)

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
  x <- alloc v
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
