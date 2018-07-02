{-# LANGUAGE ForeignFunctionInterface #-}

module HaskellWorks.Data.Json.Simd.Foreign where

import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CSize(..))

#include "../cbits/simd.h"

{#fun moo as ^{} -> `()'#}

foreign import ccall "run" run
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()
