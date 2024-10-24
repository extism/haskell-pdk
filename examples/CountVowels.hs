{-# LANGUAGE DeriveDataTypeable #-}

module CountVowels where

import Extism.PDK
import Extism.PDK.JSON

data Output = Output {count :: Int} deriving (Data)

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

  -- Log input
  () <- Extism.PDK.log LogInfo ("Got input: " ++ s)

  -- Calculate the number of vowels
  let count = length (filter isVowel s)
  -- Return a JSON object {"count": count} back to the host
  output $ JSON $ Output count

foreign export ccall "count_vowels" countVowels :: IO ()
