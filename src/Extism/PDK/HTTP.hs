-- |
-- Contains bindings to the Extism PDK HTTP interface
module Extism.PDK.HTTP where

import Data.ByteString as B
import Data.Word
import Extism.JSON (JSON, Nullable (..), Result (..), decode)
import Extism.Manifest (HTTPRequest (..), headers, method, toString, url)
import Extism.PDK
import Extism.PDK.Bindings
import Extism.PDK.Memory

-- | HTTP Request
type Request = HTTPRequest

-- | HTTP Response
data Response = Response
  { statusCode :: Int,
    memory :: Memory
  }

-- | Creates a new 'Request'
newRequest :: String -> Request
newRequest url =
  HTTPRequest
    { url = url,
      headers = Null,
      method = Null
    }

-- | Update a 'Request' with the provided HTTP request method (GET, POST, PUT, DELETE, ...)
withMethod :: String -> Request -> Request
withMethod meth req =
  req {method = NotNull meth}

-- | Update a 'Request' with the provided HTTP request headers
withHeaders :: [(String, String)] -> Request -> Request
withHeaders h req =
  req {headers = NotNull h}

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
responseJSON :: (JSON a) => Response -> IO (Either String a)
responseJSON (Response _ mem) = do
  json <- decode <$> loadString mem
  case json of
    Ok json -> return $ Right json
    Extism.JSON.Error msg -> return (Left msg)

-- | Get the 'Response' body and decode it
response :: (FromBytes a) => Response -> IO (Either String a)
response (Response _ mem) = load mem

-- | Send HTTP request with an optional request body
sendRequestWithBody :: (ToBytes a) => Request -> a -> IO Response
sendRequestWithBody req b = do
  body <- alloc b
  let json = Extism.Manifest.toString req
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
  let json = Extism.Manifest.toString req
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
                mem <- findMemory res
                return (Response (fromIntegral code) mem)
