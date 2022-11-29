module Main where

import Extism.PDK

isVowel c = 
  c == 'a' || c == 'A' ||
  c == 'e' || c == 'E' ||
  c == 'i' || c == 'I' ||
  c == 'o' || c == 'O' ||
  c == 'u' || c == 'U'

main = do
  s <- inputString ()
  let count = length (filter isVowel s)
  outputString ("{\"count\": " ++ show count ++ "}")