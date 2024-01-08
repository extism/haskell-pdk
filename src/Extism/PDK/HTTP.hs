{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Contains bindings to the Extism PDK HTTP interface
module Extism.PDK.HTTP where

import Data.ByteString as B
import Data.Word
import Extism.JSON (Nullable (..))
import qualified Extism.Manifest (HTTPRequest (..))
import Extism.PDK
import Extism.PDK.Bindings
import Extism.PDK.Memory
import Text.JSON (Result(..), decode, encode, makeObj)
import qualified Text.JSON.Generic

-- | HTTP Request
data Request = Request
  { url :: String,
    headers :: [(String, String)],
    method :: String
  }
  deriving (Text.JSON.Generic.Typeable, Text.JSON.Generic.Data)

-- | HTTP Response
data Response = Response
  { statusCode :: Int,
    memory :: Memory
  }

-- | Creates a new 'Request'
newRequest :: String -> Request
newRequest url =
  Request url [] "GET"

-- | Update a 'Request' with the provided HTTP request method (GET, POST, PUT, DELETE, ...)
withMethod :: String -> Request -> Request
withMethod meth req =
  req {method = meth}

-- | Update a 'Request' with the provided HTTP request headers
withHeaders :: [(String, String)] -> Request -> Request
withHeaders h req =
  req {headers = h}

-- | Access the Memory block associated with a 'Response'
responseMemory :: Response -> Memory
responseMemory (Response _ mem) = mem

-- | Get the 'Response' body as a 'ByteString'
responseByteString :: Response -> IO ByteString
responseByteString (Response _ mem) = do
  a <- load mem
  case a of
    Left e -> error e
    Right x -> return x

-- | Get the 'Response' body as a 'String'
responseString :: Response -> IO String
responseString (Response _ mem) = loadString mem

-- | Get the 'Response' body as JSON
responseJSON :: (Text.JSON.Generic.Data a) => Response -> IO (Either String a)
responseJSON (Response _ mem) = do
  json <- decode <$> loadString mem
  case json of
    Ok json ->
      case Text.JSON.Generic.fromJSON json of
        Ok x -> return $ Right x
        Error msg -> return (Left msg)
    Error msg -> return (Left msg)

-- | Get the 'Response' body and decode it
response :: (FromBytes a) => Response -> IO (Either String a)
response (Response _ mem) = load mem

-- | Send HTTP request with an optional request body
sendRequestWithBody :: (ToBytes a) => Request -> a -> IO Response
sendRequestWithBody req b = do
  body <- alloc b
  let json =
        encode
          Extism.Manifest.HTTPRequest
            { Extism.Manifest.url = url req,
              Extism.Manifest.headers = NotNull $ headers req,
              Extism.Manifest.method = NotNull $ method req
            }
  j <- allocString json
  res <- extismHTTPRequest (memoryOffset j) (memoryOffset body)
  free j
  free body
  code <- extismHTTPStatusCode
  if res == 0
    then return (Response (fromIntegral code) (Memory 0 0))
    else do
      mem <- findMemory res
      return (Response (fromIntegral code) mem)

-- | Send HTTP request with an optional request body
sendRequest :: (ToBytes a) => Request -> Maybe a -> IO Response
sendRequest req b =
  let json =
        encode
          Extism.Manifest.HTTPRequest
            { Extism.Manifest.url = url req,
              Extism.Manifest.headers = NotNull $ headers req,
              Extism.Manifest.method = NotNull $ method req
            }
   in let bodyMem = case b of
            Nothing -> return $ Memory 0 0
            Just b -> alloc b
       in do
            body <- bodyMem
            j <- allocString json
            res <- extismHTTPRequest (memoryOffset j) (memoryOffset body)
            free j
            free body
            code <- extismHTTPStatusCode
            if res == 0
              then return (Response (fromIntegral code) (Memory 0 0))
              else do
                len <- extismLengthUnsafe res
                return (Response (fromIntegral code) (Memory res len))
