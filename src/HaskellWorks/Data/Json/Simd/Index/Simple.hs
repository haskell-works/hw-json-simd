module HaskellWorks.Data.Json.Simd.Index.Simple
  ( makeIbs
  , ibsToIndexBuilders
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Word
import HaskellWorks.Data.Json.Simd.Internal.Index.Simple

import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Builder                      as B
import qualified Data.ByteString.Internal                     as BSI
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.Vector.Storable                         as DVS
import qualified Data.Vector.Storable.Mutable                 as DVSM
import qualified Foreign.ForeignPtr                           as F
import qualified Foreign.ForeignPtr.Unsafe                    as F
import qualified Foreign.Marshal.Unsafe                       as F
import qualified Foreign.Ptr                                  as F
import qualified HaskellWorks.Data.Json.Simd.Internal.Foreign as F
import qualified System.IO.Unsafe                             as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

makeIbs :: LBS.ByteString -> [(BS.ByteString, BS.ByteString, BS.ByteString)]
makeIbs lbs = F.unsafeLocalState $ do
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
            (F.plusPtr bsPtr bsOff) -- in_buffer:           Ptr UInt8
            (fromIntegral bsLen)    -- in_length:           Size
            (workBuffersD wb)       -- work_bits_of_d:      Ptr UInt8
            (workBuffersA wb)       -- work_bits_of_a:      Ptr UInt8
            (workBuffersZ wb)       -- work_bits_of_z:      Ptr UInt8
            (workBuffersQ wb)       -- work_bits_of_q:      Ptr UInt8
            (workBuffersB wb)       -- work_bits_of_b:      Ptr UInt8
            (workBuffersE wb)       -- work_bits_of_e:      Ptr UInt8
            (workStateZ ws)         -- last_trailing_ones:  Ptr Size
            (workStateO ws)         -- quote_odds_carry:    Ptr Size
            (workStateE ws)         -- quote_evens_carry:   Ptr Size
            (workStateM ws)         -- quote_mask_carry:    Ptr UInt64
            resIbPtr                -- result_ibs:          Ptr UInt8
            resAPtr                 -- result_a:            Ptr UInt8
            resBPtr                 -- result_z:            Ptr UInt8
          let r =
                ( BSI.fromForeignPtr resIbFptr 0 resLen
                , BSI.fromForeignPtr resAFptr  0 resLen
                , BSI.fromForeignPtr resBFptr  0 resLen
                )
          rs <- IO.unsafeInterleaveIO $ go wb ws bss
          return (r:rs)

ibsToIndexBuilders :: ()
  => [(BS.ByteString, BS.ByteString, BS.ByteString)]
  -> [(B.Builder, B.Builder)]
ibsToIndexBuilders = go emptyBpState
  where go :: ()
          => BpState
          -> [(BS.ByteString, BS.ByteString, BS.ByteString)]
          -> [(B.Builder, B.Builder)]
        go s ((is, as, zs):ibs) =
          let (s', ib, bp) = mkIndex s' is as zs
              rs =  go s' ibs
          in (ib, bp):rs
        go s []                 = []
        mkIndex :: BpState -> BS.ByteString -> BS.ByteString -> BS.ByteString -> (BpState, B.Builder, B.Builder)
        mkIndex s is as zs = undefined

