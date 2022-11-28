module Extism.PDK.HTTP where

import Extism.Manifest
import Data.Word

type Request = HttpRequest
  
data Response = Response
  {
    statusCode :: Int
  , memory :: (Word64, Word64)
  }
  
toString :: Request -> String
toString req =
    Extism.Manifest.toString req