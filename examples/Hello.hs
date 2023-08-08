module Hello where

import Extism.PDK
import Data.Maybe
import Foreign.C.Types

defaultGreeting = "Hello"

greet g n =
  outputString $ g ++ ", " ++ n
  
testing = do
  -- Get a name from the Extism runtime
  name <- inputString
  -- Get  configured greeting
  greeting <- getConfig "greeting"
  -- Greet the user, if no greeting is configured then "Hello" is used
  greet (fromMaybe defaultGreeting greeting) name

foreign export ccall "testing" testing ::  IO ()
