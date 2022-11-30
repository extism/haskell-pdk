module Main where

import Extism.PDK
import Data.Maybe

greet g n =
  outputString $ g ++ ", " ++ n

main = do
  -- Get a name from the Extism runtime
  name <- inputString
  -- Get  configured greeting
  greeting <- getConfig "greeting"
  -- Greet the user, if no greeting is configured then "Hello" is used
  greet (fromMaybe "Hello" greeting) name
