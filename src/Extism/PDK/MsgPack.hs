{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Extism.PDK.MsgPack (
  module Extism.PDK.MsgPack, 
  module Data.MessagePack,
  module Map,
) where

import GHC.Generics
import Data.MessagePack
import Data.Int
import Data.Word
import qualified Data.Map.Strict as Map

import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Serialize as S

-- ( .? ) obj k = get_field obj k
-- ( +++ ) obj (k, v) = set_field obj k (toJSONValue v)
  
-- lookup obj k = obj .? k

class MsgPack a where
  toMsgPack :: a -> Object
  fromMsgPack :: Object -> Maybe a


toByteString x = B.pack (Prelude.map c2w x)
fromByteString bs = Prelude.map w2c $ B.unpack bs


instance MsgPack Bool where
  toMsgPack b = ObjectBool b
  fromMsgPack (ObjectBool b) = Just b
  fromMsgPack _ = Nothing
  

instance MsgPack String where
  toMsgPack s = ObjectString (toByteString s)
  fromMsgPack (ObjectString s) = Just (fromByteString s)
  fromMsgPack _ = Nothing
  
  
instance MsgPack B.ByteString where
  toMsgPack s = ObjectBinary s
  fromMsgPack (ObjectString s) = Just s
  fromMsgPack (ObjectBinary s) = Just s
  fromMsgPack _ = Nothing
  
instance MsgPack Int where
  toMsgPack i = ObjectInt (fromIntegral i)
  fromMsgPack (ObjectInt i) = Just (fromIntegral i)
  fromMsgPack _ = Nothing
    
instance MsgPack Int64 where
  toMsgPack i = ObjectInt i
  fromMsgPack (ObjectInt i) = Just i
  fromMsgPack _ = Nothing
  
instance MsgPack Word where
  toMsgPack w = ObjectUInt (fromIntegral w)
  fromMsgPack (ObjectUInt x) = Just (fromIntegral x)
  fromMsgPack _ = Nothing
  
instance MsgPack Word64 where
  toMsgPack w = ObjectUInt w
  fromMsgPack (ObjectUInt x) = Just x
  fromMsgPack _ = Nothing
  
instance MsgPack a => MsgPack (Maybe a) where
  toMsgPack Nothing = ObjectNil
  toMsgPack (Just a) = toMsgPack a
  fromMsgPack bs = fromMsgPack bs

instance MsgPack () where
  toMsgPack () = ObjectNil
  fromMsgPack ObjectNil = Just ()
  fromMsgPack _ = Nothing

instance MsgPack Float where
  toMsgPack f = ObjectFloat f
  fromMsgPack (ObjectFloat f) = Just f
  fromMsgPack _ = Nothing
  
instance MsgPack Double where
  toMsgPack d = ObjectDouble d
  fromMsgPack (ObjectDouble d) = Just d
  fromMsgPack _ = Nothing
  
instance MsgPack (U1 a) where
  toMsgPack d = ObjectNil
  fromMsgPack ObjectNil = Just U1
  fromMsgPack _ = Nothing

  
instance MsgPack Object where
  toMsgPack x = x
  fromMsgPack x = Just x
  

( .= ) :: MsgPack a => MsgPack b => a -> b -> (Object, Object)
( .= ) k v = (toMsgPack k, toMsgPack v)

lookup :: MsgPack a => MsgPack b => a -> Object -> Maybe b
lookup k (ObjectMap map) = 
  let x = Map.lookup (toMsgPack k) map in
  case x of
    Nothing -> Nothing
    Just x -> fromMsgPack x
lookup _ _ = Nothing

set k v (ObjectMap map) =
  ObjectMap $ Map.insert (toMsgPack k) (toMsgPack v) map

( .? ) :: MsgPack a => MsgPack b => Object -> a -> Maybe b
( .? ) a b = Extism.PDK.MsgPack.lookup b a

object :: MsgPack a => MsgPack b => [(a, b)] -> Object
object l = ObjectMap (Map.fromList $ map (\(k, v) -> (toMsgPack k, toMsgPack v)) l)

array :: MsgPack a => [a] -> Object
array l = ObjectArray (map toMsgPack l)

encode :: MsgPack a => a -> B.ByteString
encode x =
  let y = toMsgPack x in
  S.encode y

decode :: MsgPack a => B.ByteString -> Either String a
decode bs =
  case S.decode bs of
    Right a -> case fromMsgPack a of
                 Nothing -> Left "Invalid type conversion"
                 Just x -> Right x
    Left s -> Left s
    
