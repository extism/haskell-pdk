module Main where

import Extism.PDK
import Data.Maybe

greet g n =
  outputString $ g ++ ", " ++ n

main = do
  name <- inputString ()
  greeting <- getConfig "greeting"
  greet (fromMaybe "Hello" greeting) name
