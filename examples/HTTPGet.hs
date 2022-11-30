module Main where

import Extism.PDK
import Extism.PDK.HTTP

main = do
  -- Get URL from the host
  url <- inputString
  -- Create a new 'Request'
  let req = newRequest url
  -- Send the request, get a 'Response'
  res <- sendRequest req Nothing
  -- Save response body to memory
  outputMemory (memory res)
