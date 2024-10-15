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
import Extism.PDK.Util (fromByteString)
import Text.JSON (Result (..), decode, encode, makeObj)
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
    responseData :: ByteString,
    responseHeaders :: [(String, String)]
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

-- | Get the 'Response' body as a 'ByteString'
responseByteString :: Response -> ByteString
responseByteString (Response _ mem _) = mem

-- | Get the 'Response' body as a 'String'
responseString :: Response -> String
responseString (Response _ mem _) = fromByteString mem

-- | Get the 'Response' body as JSON
responseJSON :: (Text.JSON.Generic.Data a) => Response -> IO (Either String a)
responseJSON res = do
  case json of
    Ok json ->
      case Text.JSON.Generic.fromJSON json of
        Ok x -> return $ Right x
        Error msg -> return (Left msg)
    Error msg -> return (Left msg)
  where
    s = responseString res
    json = decode s

-- | Get the 'Response' body and decode it
response :: (FromBytes a) => Response -> Either String a
response (Response _ mem _) = fromBytes mem

getHeaders = do
  offs <- extismHTTPHeaders
  if offs == 0
    then
      return []
    else do
      mem <- Extism.PDK.Memory.findMemory offs
      h <- Extism.PDK.Memory.load mem
      () <- Extism.PDK.Memory.free mem
      case h of
        Left _ -> return []
        Right (JSON x) -> return x

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
  code <- extismHTTPStatusCode
  h <- getHeaders
  if res == 0
    then return (Response (fromIntegral code) empty h)
    else do
      mem <- findMemory res
      bs <- loadByteString mem
      free mem
      return (Response (fromIntegral code) bs h)

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
            code <- extismHTTPStatusCode
            h <- getHeaders
            if res == 0
              then return (Response (fromIntegral code) empty h)
              else do
                len <- extismLengthUnsafe res
                let mem = Memory res len
                bs <- loadByteString mem
                free mem
                return (Response (fromIntegral code) bs h)
