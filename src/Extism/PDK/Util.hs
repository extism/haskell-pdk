module Extism.PDK.Util where

import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)

-- | Helper function to convert a string to a bytestring
toByteString :: String -> B.ByteString
toByteString x = B.pack (Prelude.map c2w x)

-- | Helper function to convert a bytestring to a string
fromByteString :: B.ByteString -> String
fromByteString bs = Prelude.map w2c $ B.unpack bs
