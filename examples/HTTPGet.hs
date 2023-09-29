module HTTPGet where

import Extism.PDK
import Extism.PDK.HTTP
import Extism.PDK.Memory

getInput = do
  req <- input
  case req of
    Right (JSONValue x) -> return x
    Left _ -> do
      url <- inputString
      return $ newRequest url

httpGet = do
  -- Get URL or JSON encoded request from host
  req <- getInput
  -- Send the request, get a 'Response'
  res <- sendRequest req Nothing
  -- Save response body to memory
  outputMemory (memory res)

foreign export ccall "http_get" httpGet :: IO ()
