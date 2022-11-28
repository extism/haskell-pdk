module Main where

import Extism.PDK

main = do
  name <- inputString ()
  x <- getConfig "greeting"
  case x of
    Just x ->
      outputString $ x ++ ", " ++ name
    Nothing ->
      outputString $ "Hello, " ++ name
