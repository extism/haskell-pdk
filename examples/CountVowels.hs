module CountVowels where

import Extism.PDK
import Extism.PDK.JSON

data Output = Output
  { count :: Int
  }

instance JSON Output where
  showJSON (Output c) =
    object ["count" .= c]

  readJSON obj =
    case obj .? "count" of
      Null -> Extism.PDK.JSON.Error "count field not found"
      NotNull x -> Ok x

isVowel c =
  c == 'a'
    || c == 'A'
    || c == 'e'
    || c == 'E'
    || c == 'i'
    || c == 'I'
    || c == 'o'
    || c == 'O'
    || c == 'u'
    || c == 'U'

countVowels = do
  -- Get input string from Extism host
  s <- input
  -- Calculate the number of vowels
  let count = length (filter isVowel s)
  -- Return a JSON object {"count": count} back to the host
  output $ JSONValue $ Output count

foreign export ccall "count_vowels" countVowels :: IO ()
