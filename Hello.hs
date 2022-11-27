module Main where

import Extism.PDK.Bindings
import Data.Word
import Data.Char(ord)

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

main = do
  offs <- extismAlloc 4
  extismStoreU8 offs (charToWord8 't')
  extismStoreU8 (offs + 1) (charToWord8 'e')
  extismStoreU8 (offs + 2) (charToWord8 's')
  extismStoreU8 (offs + 3) (charToWord8 't')
  extismSetOutput offs 4
