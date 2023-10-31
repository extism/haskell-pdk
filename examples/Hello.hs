module Hello where

import Data.Maybe
import Extism.PDK
import Extism.PDK.JSON

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

data Add = Add
  { a :: Int,
    b :: Int
  }

data Sum = Sum
  { sum :: Int
  }

instance JSON Add where
  showJSON (Add a' b') =
    object ["a" .= a', "b" .= b']

  readJSON obj =
    let a = fromNotNull $ obj .? "a"
     in let b = fromNotNull $ obj .? "b"
         in Ok (Add a b)

instance JSON Sum where
  showJSON (Sum x) =
    object ["sum" .= x]

  readJSON obj =
    let x = fromNotNull $ obj .? "sum"
     in Ok (Sum x)

add = do
  -- Get float value
  JSONValue value <- (input :: IO (JSONValue Add))
  outputJSON $ Sum (a value + b value)
  return 0
