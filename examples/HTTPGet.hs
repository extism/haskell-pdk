module Main where

import Extism.PDK
import Extism.PDK.HTTP

main = do
  url <- inputString ()
  let req = newRequest url
  res <- sendRequest req Nothing
  outputMemory (memory res)
