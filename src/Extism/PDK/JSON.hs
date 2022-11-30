module Extism.PDK.JSON (
  module Extism.PDK.JSON, 
  module Text.JSON,
  module Text.JSON.Generic,
  object, (.=), JSONValue(..), makeArray, toString,
) where

import Extism.Manifest
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types

( .? ) obj k = get_field obj k
( +++ ) obj (k, v) = set_field obj k (toJSONValue v)
  
lookup obj k = obj .? k