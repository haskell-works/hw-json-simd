{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module HaskellWorks.Data.Json.Simd.Internal.Foreign where

import Foreign
import Foreign.C.Types (CChar(..), CSize(..))

#include "../cbits/simd.h"

type UInt8  = {#type uint8_t#}
type UInt64 = {#type uint64_t#}
type Size = {#type size_t#}

foreign import ccall "run" run
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

processChunk :: ()
  => Ptr UInt8
  -> Size
  -> Ptr UInt8
  -> Ptr UInt8
  -> Ptr UInt8
  -> Ptr UInt8
  -> Ptr UInt8
  -> Ptr UInt8
  -> Ptr Size
  -> Ptr Size
  -> Ptr Size
  -> Ptr UInt64
  -> Ptr UInt8
  -> Ptr UInt8
  -> Ptr UInt8
  -> IO UInt64
processChunk = do
  {#call unsafe process_chunk as c_process_chunk#}
{-# INLINE processChunk #-}
