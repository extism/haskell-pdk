module Main where

import Extism.PDK
import Data.Int

test = do
  name <- inputString ()
  x <- getConfig "greeting"
  case x of
    Just x ->
      outputString $ x ++ ", " ++ name
    Nothing ->
      outputString $ "Hello, " ++ name
  return 0
foreign export ccall "test" test :: IO Int32


main = do
  name <- inputString ()
  x <- getConfig "greeting"
  case x of
    Just x ->
      outputString $ x ++ ", " ++ name
    Nothing ->
      outputString $ "Hello, " ++ name
