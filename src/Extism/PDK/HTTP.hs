module Extism.PDK.HTTP where

import Extism.Manifest(toString, HTTPRequest(..), method, header, url)
import Extism.PDK.Bindings
import Extism.PDK
import Data.Word
import Data.ByteString as B

type Request = HTTPRequest

data Response = Response
  {
    statusCode :: Int
  , memory :: Memory
  }

newRequest :: String -> Request
newRequest url =
  HTTPRequest {
    url = url
  , header = Nothing
  , method = Nothing
  }

withMethod :: String -> Request -> Request
withMethod meth req =
  req { method = Just meth }

withHeaders :: [(String, String)] -> Request -> Request
withHeaders h req =
  req { header = Just h }

toString :: Request -> String
toString req =
    Extism.Manifest.toString req

responseMemory :: Response -> Memory
responseMemory (Response _ mem) = mem

responseByteString :: Response -> IO ByteString
responseByteString (Response _ mem) = load mem

responseString :: Response -> IO String
responseString (Response _ mem) = loadString mem

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
