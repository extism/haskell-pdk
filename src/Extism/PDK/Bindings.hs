{-# LANGUAGE TypeApplications #-}

module Extism.PDK.Bindings where

import Control.Monad
import Data.ByteString as B
import Data.ByteString.Internal
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.Exit

-- | Offset in Extism memory
type MemoryOffset = Word64

-- | Offset of input from 0 to 'InputLength'
type InputOffset = Word64

-- | Length of allocated block of memory
type MemoryLength = Word64

-- | Total length of the input
type InputLength = Word64

foreign import ccall "extism_output_set" extismSetOutput :: MemoryOffset -> MemoryLength -> IO ()

foreign import ccall "extism_error_set" extismSetError :: MemoryOffset -> IO ()

foreign import ccall "extism_log_info" extismLogInfo :: MemoryOffset -> IO ()

foreign import ccall "extism_log_warn" extismLogWarn :: MemoryOffset -> IO ()

foreign import ccall "extism_log_debug" extismLogDebug :: MemoryOffset -> IO ()

foreign import ccall "extism_log_error" extismLogError :: MemoryOffset -> IO ()

foreign import ccall "extism_store_u8" extismStoreU8 :: MemoryOffset -> Word8 -> IO ()

foreign import ccall "extism_store_u64" extismStoreU64 :: MemoryOffset -> Word64 -> IO ()

foreign import ccall "extism_load_u8" extismLoadU8 :: MemoryOffset -> IO Word8

foreign import ccall "extism_load_u64" extismLoadU64 :: MemoryOffset -> IO Word64

foreign import ccall "extism_alloc" extismAlloc :: MemoryLength -> IO MemoryOffset

foreign import ccall "extism_length" extismLength :: MemoryOffset -> IO MemoryLength

foreign import ccall "extism_length_unsafe" extismLengthUnsafe :: MemoryOffset -> IO MemoryLength

foreign import ccall "extism_free" extismFree :: MemoryOffset -> IO ()

foreign import ccall "extism_input_length" extismInputLength :: IO InputLength

foreign import ccall "extism_input_load_u8" extismInputLoadU8 :: InputOffset -> IO Word8

foreign import ccall "extism_input_load_u64" extismInputLoadU64 :: InputOffset -> IO Word64

foreign import ccall "extism_config_get" extismGetConfig :: MemoryOffset -> IO MemoryOffset

foreign import ccall "extism_var_get" extismGetVar :: MemoryOffset -> IO MemoryOffset

foreign import ccall "extism_var_set" extismSetVar :: MemoryOffset -> MemoryOffset -> IO ()

foreign import ccall "extism_http_request" extismHTTPRequest :: MemoryOffset -> MemoryOffset -> IO MemoryOffset

foreign import ccall "extism_http_status_code" extismHTTPStatusCode :: IO Int32

foreign import ccall "extism_http_headers" extismHTTPHeaders :: IO MemoryOffset

foreign import ccall "__wasm_call_ctors" wasmConstructor :: IO ()

foreign import ccall "__wasm_call_dtors" wasmDestructor :: IO ()

bsToWord64 :: ByteString -> IO Word64
bsToWord64 (BS fp len) =
  if len /= 8
    then error "invalid bytestring"
    else
      withForeignPtr fp $ peek . castPtr @Word8 @Word64

word64ToBS :: Word64 -> ByteString
word64ToBS word =
  unsafeCreate 8 $ \p ->
    poke (castPtr @Word8 @Word64 p) word

readLoop :: (Word64 -> IO Word8) -> (Word64 -> IO Word64) -> Word64 -> Word64 -> [ByteString] -> IO ByteString
readLoop f1 f8 total index acc =
  if index >= total
    then return $ B.concat . Prelude.reverse $ acc
    else do
      let diff = total - index
      (n, x) <-
        if diff >= 8
          then do
            u <- f8 index
            return (8, word64ToBS u)
          else do
            b <- f1 index
            return (1, B.singleton b)
      readLoop f1 f8 total (index + n) (x : acc)

readInputBytes :: InputLength -> IO ByteString
readInputBytes len =
  readLoop extismInputLoadU8 extismInputLoadU64 len 0 []

readBytes :: MemoryOffset -> MemoryLength -> IO ByteString
readBytes offs len =
  readLoop extismLoadU8 extismLoadU64 (offs + len) offs []

writeBytesLoop :: MemoryOffset -> MemoryOffset -> ByteString -> IO ()
writeBytesLoop index total src =
  if index >= total
    then pure ()
    else do
      let diff = total - index
      (n, sub) <-
        if diff >= 8
          then do
            let (curr, next) = B.splitAt 8 src
            u <- bsToWord64 curr
            extismStoreU64 index u
            return (8, next)
          else do
            let u = B.head src
            extismStoreU8 index u
            return (1, B.tail src)
      writeBytesLoop (index + n) total sub

writeBytes :: MemoryOffset -> MemoryLength -> ByteString -> IO ()
writeBytes offs len =
  writeBytesLoop offs (offs + len)
