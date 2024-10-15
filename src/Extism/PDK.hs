{-# LANGUAGE Rank2Types #-}

-- |
-- Extism plugin development kit, used with the [wasm32-wasi-ghc](https://gitlab.haskell.org/ghc/ghc-wasm-meta) backend to make Extism plugins
module Extism.PDK
  ( module Extism.PDK,
    ToBytes (..),
    FromBytes (..),
    JSON (..),
    MsgPack (..),
  )
where

import Data.ByteString as B
import Extism.PDK.Bindings
import Extism.PDK.Memory
import qualified Extism.PDK.MsgPack (MsgPack, decode, encode)
import Extism.PDK.Util
import qualified Text.JSON (decode, encode, resultToEither)
import qualified Text.JSON.Generic

-- | Get plugin input, returning an error message if the encoding is invalid
tryInput :: (FromBytes a) => IO (Either String a)
tryInput = fromBytes <$> inputByteString

-- | Get plugin input
input :: forall a. (FromBytes a) => IO a
input = do
  i <- inputByteString
  case fromBytes i of
    Left e -> error e
    Right y -> return y

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
inputJSON :: (Text.JSON.Generic.Data a) => IO a
inputJSON = do
  Text.JSON.Generic.decodeJSON <$> input

-- | Set plugin output
output :: (ToBytes a) => a -> IO ()
output x = do
  Memory offs len <- alloc x
  extismSetOutput offs len

-- | Set plugin output to a JSON encoded version of the provided value
outputJSON :: (Text.JSON.Generic.Data a) => a -> IO ()
outputJSON x =
  output (Text.JSON.Generic.encodeJSON x)

-- | Get a variable from the Extism runtime
getVar :: (FromBytes a) => String -> IO (Maybe a)
getVar key = do
  k <- allocString key
  v <- extismGetVar (memoryOffset k)
  if v == 0
    then return Nothing
    else do
      mem <- findMemory v
      bs <- load mem
      case bs of
        Left _ -> return Nothing
        Right x -> return (Just x)

-- | Set a variable
setVar :: (ToBytes a) => String -> Maybe a -> IO ()
setVar key Nothing = do
  k <- allocString key
  extismSetVar (memoryOffset k) 0
setVar key (Just v) = do
  k <- allocString key
  x <- alloc v
  extismSetVar (memoryOffset k) (memoryOffset x)

-- | Get a configuration value
getConfig :: String -> IO (Maybe String)
getConfig key = do
  k <- allocString key
  v <- extismGetConfig (memoryOffset k)
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
data LogLevel = LogInfo | LogDebug | LogWarn | LogError

-- | Log to configured log file
log :: LogLevel -> String -> IO ()
log LogInfo msg = do
  s <- allocString msg
  extismLogInfo (memoryOffset s)
log LogDebug msg = do
  s <- allocString msg
  extismLogDebug (memoryOffset s)
log LogWarn msg = do
  s <- allocString msg
  extismLogWarn (memoryOffset s)
log LogError msg = do
  s <- allocString msg
  extismLogError (memoryOffset s)

-- Log with "error" level
logError :: String -> IO ()
logError = Extism.PDK.log LogError

-- Log with "info" level
logInfo :: String -> IO ()
logInfo = Extism.PDK.log LogInfo

-- Log with "debug" level
logDebug :: String -> IO ()
logDebug = Extism.PDK.log LogDebug

-- Log with "warn" level
logWarn :: String -> IO ()
logWarn = Extism.PDK.log LogWarn
