module Extism.PDK.HTTP where

import Data.ByteString as B
import Text.JSON
  (
    JSValue(JSNull, JSString, JSArray),
    toJSString, showJSON, makeObj, encode
  )

data Request = Request
  {
    url :: String
  , header :: [(String, String)]
  , method :: Maybe String
  }
  
data Response = Response
  {
    statusCode :: Int
  , bytes :: Maybe ByteString
  }
  
valueOrNull f Nothing = JSNull
valueOrNull f (Just x) = f x
makeString s = JSString (toJSString s)
stringOrNull = valueOrNull makeString
filterNulls obj = [(a, b) | (a, b) <- obj, not (isNull b)]
mapObj f x = makeObj (filterNulls [(a, f b) | (a, b) <- x])
isNull JSNull = True
isNull _ = False

requestObj x =
  let meth = stringOrNull $ method x in
  let h = mapObj makeString $ header x in
  filterNulls [
    ("url", makeString $ url x),
    ("header", h),
    ("method", meth)
  ]

toString :: Request -> String
toString req =
    encode $ makeObj $ requestObj req
