{-# LANGUAGE FlexibleInstances #-}

module Extism.PDK.Util where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.Int
import Data.Word
import qualified Extism.PDK.MsgPack (MsgPack, decode, encode)
import Text.JSON (JSON, decode, encode, resultToEither)

-- | Helper function to convert a string to a bytestring
toByteString :: String -> B.ByteString
toByteString x = B.pack (Prelude.map c2w x)

-- | Helper function to convert a bytestring to a string
fromByteString :: B.ByteString -> String
fromByteString bs = Prelude.map w2c $ B.unpack bs

-- | A class used to convert values from bytes read from linear memory
class FromBytes a where
  fromBytes :: B.ByteString -> Either String a

-- | A class used to convert values to bytes to be written into linear memory
class ToBytes a where
  toBytes :: a -> B.ByteString

-- | A wrapper type for JSON encoded values
newtype JSONValue a = JSONValue a

-- | A wrapper type for MsgPack encoded values
newtype MsgPackValue a = MsgPackValue a

instance FromBytes B.ByteString where
  fromBytes = Right

instance ToBytes B.ByteString where
  toBytes bs = bs

instance FromBytes String where
  fromBytes = Right . fromByteString

instance ToBytes String where
  toBytes = toByteString

instance (JSON a) => FromBytes (JSONValue a) where
  fromBytes x =
    case resultToEither $ decode (fromByteString x) of
      Left e -> Left e
      Right y -> Right (JSONValue y)

instance (JSON a) => ToBytes (JSONValue a) where
  toBytes (JSONValue x) = toByteString (encode x)

instance (Extism.PDK.MsgPack.MsgPack a) => FromBytes (MsgPackValue a) where
  fromBytes x =
    case Extism.PDK.MsgPack.decode x of
      Left e -> Left e
      Right y -> Right (MsgPackValue y)

instance (Extism.PDK.MsgPack.MsgPack a) => ToBytes (MsgPackValue a) where
  toBytes (MsgPackValue x) = Extism.PDK.MsgPack.encode x

instance ToBytes Int32 where
  toBytes i = B.toStrict (runPut (putInt32le i))

instance FromBytes Int32 where
  fromBytes bs =
    case runGetOrFail getInt32le (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Int64 where
  toBytes i = B.toStrict (runPut (putInt64le i))

instance FromBytes Int64 where
  fromBytes bs =
    case runGetOrFail getInt64le (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Word32 where
  toBytes i = B.toStrict (runPut (putWord32le i))

instance FromBytes Word32 where
  fromBytes bs =
    case runGetOrFail getWord32le (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Word64 where
  toBytes i = B.toStrict (runPut (putWord64le i))

instance FromBytes Word64 where
  fromBytes bs =
    case runGetOrFail getWord64le (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Float where
  toBytes i = B.toStrict (runPut (putFloatle i))

instance FromBytes Float where
  fromBytes bs =
    case runGetOrFail getFloatle (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x

instance ToBytes Double where
  toBytes i = B.toStrict (runPut (putDoublele i))

instance FromBytes Double where
  fromBytes bs =
    case runGetOrFail getDoublele (B.fromStrict bs) of
      Left (_, _, e) -> Left e
      Right (_, _, x) -> Right x
