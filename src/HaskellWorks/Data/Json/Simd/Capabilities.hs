module HaskellWorks.Data.Json.Simd.Capabilities where

import qualified HaskellWorks.Data.Json.Simd.Internal.Foreign as F
import qualified System.IO.Unsafe                             as U

avx_2 :: Bool
avx_2 = U.unsafePerformIO F.enabled_avx_2 /= 0
{-# NOINLINE avx_2 #-}

sse_4_2 :: Bool
sse_4_2 = U.unsafePerformIO F.enabled_sse_4_2 /= 0
{-# NOINLINE sse_4_2 #-}

bmi_2 :: Bool
bmi_2 = U.unsafePerformIO F.enabled_bmi_2 /= 0
{-# NOINLINE bmi_2 #-}
