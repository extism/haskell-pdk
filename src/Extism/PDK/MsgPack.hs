{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Provides the ability to use MessagePack for plugin input/output
module Extism.PDK.MsgPack
  ( module Extism.PDK.MsgPack,
    module Data.MessagePack,
    module Map,
  )
where

import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.MessagePack
import qualified Data.Serialize as S
import Data.Word
import GHC.Generics

class MsgPack a where
  toMsgPack :: a -> Object
  fromMsgPack :: Object -> Maybe a

class GMsgPack f where
  toGMsgPack :: f a -> Object
  fromGMsgPack :: Object -> Maybe (f a)
  fromGMsgPack _ = Nothing

instance GMsgPack U1 where
  toGMsgPack U1 = ObjectNil
  fromGMsgPack ObjectNil = Just U1

instance (GMsgPack a, GMsgPack b) => GMsgPack (a :*: b) where
  toGMsgPack (x :*: y) = array [toGMsgPack x, toGMsgPack y]

-- fromGMsgPack (ObjectArray [a, b]) = Just (a :*: b)

instance (GMsgPack a, GMsgPack b) => GMsgPack (a :+: b) where
  toGMsgPack (L1 x) = toGMsgPack x
  toGMsgPack (R1 x) = toGMsgPack x

instance (GMsgPack a) => GMsgPack (M1 i c a) where
  toGMsgPack (M1 x) = toGMsgPack x

instance (MsgPack a) => GMsgPack (K1 i a) where
  toGMsgPack (K1 x) = toMsgPack x

toByteString x = B.pack (Prelude.map c2w x)

fromByteString bs = Prelude.map w2c $ B.unpack bs

instance MsgPack Bool where
  toMsgPack = ObjectBool
  fromMsgPack (ObjectBool b) = Just b
  fromMsgPack _ = Nothing

instance MsgPack String where
  toMsgPack s = ObjectString (toByteString s)
  fromMsgPack (ObjectString s) = Just (fromByteString s)
  fromMsgPack _ = Nothing

instance MsgPack B.ByteString where
  toMsgPack = ObjectBinary
  fromMsgPack (ObjectString s) = Just s
  fromMsgPack (ObjectBinary s) = Just s
  fromMsgPack _ = Nothing

instance MsgPack Int where
  toMsgPack i = ObjectInt (fromIntegral i)
  fromMsgPack (ObjectInt i) = Just (fromIntegral i)
  fromMsgPack _ = Nothing

instance MsgPack Int64 where
  toMsgPack = ObjectInt
  fromMsgPack (ObjectInt i) = Just i
  fromMsgPack _ = Nothing

instance MsgPack Word where
  toMsgPack w = ObjectUInt (fromIntegral w)
  fromMsgPack (ObjectUInt x) = Just (fromIntegral x)
  fromMsgPack _ = Nothing

instance MsgPack Word64 where
  toMsgPack = ObjectUInt
  fromMsgPack (ObjectUInt x) = Just x
  fromMsgPack _ = Nothing

instance (MsgPack a) => MsgPack (Maybe a) where
  toMsgPack Nothing = ObjectNil
  toMsgPack (Just a) = toMsgPack a
  fromMsgPack = fromMsgPack

instance MsgPack () where
  toMsgPack () = ObjectNil
  fromMsgPack ObjectNil = Just ()
  fromMsgPack _ = Nothing

instance MsgPack Float where
  toMsgPack = ObjectFloat
  fromMsgPack (ObjectFloat f) = Just f
  fromMsgPack _ = Nothing

instance MsgPack Double where
  toMsgPack = ObjectDouble
  fromMsgPack (ObjectDouble d) = Just d
  fromMsgPack _ = Nothing

instance MsgPack Object where
  toMsgPack x = x
  fromMsgPack = Just

(.=) :: (MsgPack a) => (MsgPack b) => a -> b -> (Object, Object)
(.=) k v = (toMsgPack k, toMsgPack v)

lookup :: (MsgPack a) => (MsgPack b) => a -> Object -> Maybe b
lookup k (ObjectMap map) =
  let x = Map.lookup (toMsgPack k) map
   in fromMsgPack =<< x
lookup _ _ = Nothing

set k v (ObjectMap map) =
  ObjectMap $ Map.insert (toMsgPack k) (toMsgPack v) map

(.?) :: (MsgPack a) => (MsgPack b) => Object -> a -> Maybe b
(.?) a b = Extism.PDK.MsgPack.lookup b a

object :: (MsgPack a) => (MsgPack b) => [(a, b)] -> Object
object l = ObjectMap (Map.fromList $ map (bimap toMsgPack toMsgPack) l)

array :: (MsgPack a) => [a] -> Object
array l = ObjectArray (map toMsgPack l)

encode :: (MsgPack a) => a -> B.ByteString
encode x =
  let y = toMsgPack x
   in S.encode y

decode :: (MsgPack a) => B.ByteString -> Either String a
decode bs =
  case S.decode bs of
    Right a -> case fromMsgPack a of
      Nothing -> Left "Invalid type conversion"
      Just x -> Right x
    Left s -> Left s
