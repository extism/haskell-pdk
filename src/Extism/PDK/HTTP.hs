module Extism.PDK.HTTP where

import Extism.Manifest(toString, HTTPRequest(..), method, headers, url, Nullable(..))
import Extism.PDK.Bindings
import Extism.PDK
import Data.Word
import Data.ByteString as B

-- | HTTP Request
type Request = HTTPRequest

-- | HTTP Response
data Response = Response
  {
    statusCode :: Int
  , memory :: Memory
  }

-- | Creates a new 'Request'
newRequest :: String -> Request
newRequest url =
  HTTPRequest {
    url = url
  , headers = Null
  , method = Null
  }

-- | Update a 'Request' with the provided HTTP request method (GET, POST, PUT, DELETE, ...)
withMethod :: String -> Request -> Request
withMethod meth req =
  req { method = NotNull meth }

-- | Update a 'Request' with the provided HTTP request headers
withHeaders :: [(String, String)] -> Request -> Request
withHeaders h req =
  req { headers = NotNull h }

-- | Access the Memory block associated with a 'Response'
responseMemory :: Response -> Memory
responseMemory (Response _ mem) = mem

-- | Get the 'Response' body as a 'ByteString'
responseByteString :: Response -> IO ByteString
responseByteString (Response _ mem) = load mem

-- | Get the 'Response' body as a 'String'
responseString :: Response -> IO String
responseString (Response _ mem) = loadString mem

-- | Send HTTP request with an optional request body
sendRequest :: Request -> Maybe ByteString -> IO Response
sendRequest req b =
  let json = Extism.Manifest.toString req in
  let bodyMem = case b of
               Nothing -> return $ Memory 0 0
               Just b -> allocByteString b
  in
  do
    body <- bodyMem
    j <- allocString json
    res <- extismHTTPRequest (memoryOffset j) (memoryOffset body)
    free j
    free body
    code <- extismHTTPStatusCode
    if res == 0 then
      return (Response (fromIntegral code) (Memory 0 0))
    else do
      mem <- findMemory res
      return (Response (fromIntegral code) mem)
