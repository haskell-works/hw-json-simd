module HaskellWorks.Data.Json.Simd.Index.Simple
  ( makeIndex
  ) where

import HaskellWorks.Data.Json.Simd.Internal.Index.Simple

import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Internal                     as BSI
import qualified Data.ByteString.Lazy                         as LBS
import qualified Foreign.ForeignPtr                           as F
import qualified Foreign.ForeignPtr.Unsafe                    as F
import qualified Foreign.Marshal.Unsafe                       as F
import qualified Foreign.Ptr                                  as F
import qualified HaskellWorks.Data.Json.Simd.Internal.Foreign as F
import qualified System.IO.Unsafe                             as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

makeIndex :: LBS.ByteString -> [(BS.ByteString, BS.ByteString, BS.ByteString)]
makeIndex lbs = F.unsafeLocalState $ do
  wb <- allocWorkBuffers (32 * 1024 * 1204)
  ws <- allocWorkState
  IO.unsafeInterleaveIO $ go wb ws (LBS.toChunks lbs)
  where go :: WorkBuffers -> WorkState -> [BS.ByteString] -> IO [(BS.ByteString, BS.ByteString, BS.ByteString)]
        go _  _  []       = return []
        go wb ws (bs:bss) = do
          let resLen = BS.length bs `div` 8
          resIbFptr  <- F.mallocForeignPtrBytes resLen
          resAFptr   <- F.mallocForeignPtrBytes resLen
          resBFptr   <- F.mallocForeignPtrBytes resLen
          let resIbPtr  = F.castPtr (F.unsafeForeignPtrToPtr resIbFptr)
          let resAPtr   = F.castPtr (F.unsafeForeignPtrToPtr resAFptr )
          let resBPtr   = F.castPtr (F.unsafeForeignPtrToPtr resBFptr )
          let (bsFptr, bsOff, bsLen) = BSI.toForeignPtr bs
          let bsPtr = F.castPtr (F.unsafeForeignPtrToPtr bsFptr)
          _ <- F.processChunk
            (F.plusPtr bsPtr bsOff) --   => Ptr UInt8   --  in_buffer
            (fromIntegral bsLen)    --   -> Size        --  in_length
            (workBuffersD wb)       --   -> Ptr UInt8   --  work_bits_of_d
            (workBuffersA wb)       --   -> Ptr UInt8   --  work_bits_of_a
            (workBuffersZ wb)       --   -> Ptr UInt8   --  work_bits_of_z
            (workBuffersQ wb)       --   -> Ptr UInt8   --  work_bits_of_q
            (workBuffersB wb)       --   -> Ptr UInt8   --  work_bits_of_b
            (workBuffersE wb)       --   -> Ptr UInt8   --  work_bits_of_e
            (workStateZ ws)         --   -> Ptr Size    --  last_trailing_ones
            (workStateO ws)         --   -> Ptr Size    --  quote_odds_carry
            (workStateE ws)         --   -> Ptr Size    --  quote_evens_carry
            (workStateM ws)         --   -> Ptr UInt64  --  quote_mask_carry
            resIbPtr                --   -> Ptr UInt8   --  result_ibs
            resAPtr                 --   -> Ptr UInt8   --  result_a
            resBPtr                 --   -> Ptr UInt8   --  result_z
          let r =
                ( BSI.fromForeignPtr resIbFptr 0 resLen
                , BSI.fromForeignPtr resAFptr  0 resLen
                , BSI.fromForeignPtr resBFptr  0 resLen
                )
          rs <- IO.unsafeInterleaveIO $ go wb ws bss
          return (r:rs)


