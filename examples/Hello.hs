module Hello where

import Data.Maybe
import Extism.PDK
import Foreign.C.Types

defaultGreeting = "Hello"

greet g n =
  output $ g ++ ", " ++ n

testing = do
  -- Get a name from the Extism runtime
  name <- inputString
  -- Get  configured greeting
  greeting <- getConfig "greeting"
  -- Greet the user, if no greeting is configured then "Hello" is used
  greet (fromMaybe defaultGreeting greeting) name

foreign export ccall "testing" testing :: IO ()
