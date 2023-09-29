module HTTPGet where

import Extism.PDK
import Extism.PDK.HTTP
import Extism.PDK.Memory

httpGet = do
  -- Get URL from the host
  url <- inputString
  -- Create a new 'Request'
  let req = newRequest url
  -- Send the request, get a 'Response'
  res <- sendRequest req Nothing
  -- Save response body to memory
  outputMemory (memory res)

foreign export ccall "http_get" httpGet :: IO ()
