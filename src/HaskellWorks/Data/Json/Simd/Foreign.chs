{-# LANGUAGE ForeignFunctionInterface #-}

module HaskellWorks.Data.Json.Simd.Foreign where

#include "../cbits/simd.h"

{#fun moo as ^{} -> `()'#}