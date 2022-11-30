module Main where

import Extism.PDK
import Extism.PDK.JSON

isVowel c = 
  c == 'a' || c == 'A' ||
  c == 'e' || c == 'E' ||
  c == 'i' || c == 'I' ||
  c == 'o' || c == 'O' ||
  c == 'u' || c == 'U'

main = do
  -- Get input string from Extism host
  s <- inputString
  -- Calculate the number of vowels
  let count = length (filter isVowel s)
  -- Return a JSON object {"count": count} back to the host
  outputJSON $ object ["count" .= count]
