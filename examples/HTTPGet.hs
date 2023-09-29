module HTTPGet where

import Data.Int
import Extism.PDK
import Extism.PDK.HTTP
import Extism.PDK.Memory

getInput = do
  req <- tryInput
  case req of
    Right (JSONValue x) -> return x
    Left e -> do
      putStrLn e
      url <- inputString
      return $ newRequest url

httpGet = do
  -- Get URL or JSON encoded request from host
  req <- getInput
  -- Send the request, get a 'Response'
  res <- sendRequest req (Nothing :: Maybe String)
  -- Save response body to memory
  outputMemory (memory res)
  -- Return code
  return 0

foreign export ccall "http_get" httpGet :: IO Int32
