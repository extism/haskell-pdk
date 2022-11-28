module Main where

import Extism.PDK

greet g n =
  outputString $ g ++ ", " ++ n

main = do
  name <- inputString ()
  greeting <- getConfig "greeting"
  case greeting of
    Just greeting -> greet greeting name
    Nothing -> greet "Hello" name
