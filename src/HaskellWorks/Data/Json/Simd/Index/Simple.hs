{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module HaskellWorks.Data.Json.Simd.Index.Simple
  ( makeIbs
  , ibsToIndexByteStrings
  ) where

import Control.Monad.ST
import Data.Bits.Pdep
import Data.Bits.Pext
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Json.Simd.Internal.Index.Simple

import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Internal                     as BSI
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.Vector.Storable                         as DVS
import qualified Data.Vector.Storable.Mutable                 as DVSM
import qualified Foreign.ForeignPtr                           as F
import qualified Foreign.ForeignPtr.Unsafe                    as F
import qualified Foreign.Marshal.Unsafe                       as F
import qualified Foreign.Ptr                                  as F
import qualified HaskellWorks.Data.Json.Simd.Internal.Foreign as F
import qualified HaskellWorks.Data.Vector.AsVector64          as DVS
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

ibsToIndexByteStrings :: ()
  => [(BS.ByteString, BS.ByteString, BS.ByteString)]
  -> [BS.ByteString]
ibsToIndexByteStrings bits = go emptyBpState (fmap (\(a, b, c) -> mkIndexStep a b c) bits)
  where go :: ()
          => BpState
          -> [Step]
          -> [BS.ByteString]
        go s (step:steps) = let (s', bp) = stepToByteString s step in bp:go s' steps
        go _ []           = []

mkIndexStep :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Step
mkIndexStep is as zs | isLen == asLen && asLen == zsLen = Step (go 0 0) isLen
  where isLen = BS.length is
        asLen = BS.length as
        zsLen = BS.length zs
        isv   = DVS.asVector64 is
        asv   = DVS.asVector64 as
        zsv   = DVS.asVector64 zs
        len   = DVS.length isv
        go  :: Int
            -> Int
            -> BpState
            -> DVSM.MVector s Word64
            -> ST s (BpState, Int)
        go i bpsReady bpState bpvm | i < len = do
          let w64_ib = DVS.unsafeIndex isv i
          let w64_a  = DVS.unsafeIndex asv i
          let w64_z  = DVS.unsafeIndex zsv i

          let pc_ib = popCount1 w64_ib

          let ext_d = pext (comp (w64_a .|. w64_z)) w64_ib :: Word64
          let ext_a = pext w64_a                    w64_ib :: Word64
          let ext_z = pext w64_z                    w64_ib :: Word64

          let remainder_len = bpStateLen bpState

          let remainder_bits_d = bpStateD bpState .|. (ext_d .<. bpStateLen bpState)
          let remainder_bits_a = bpStateA bpState .|. (ext_a .<. bpStateLen bpState)
          -- let remainder_bits_z = bpStateZ bpState .|. (ext_z .<. bpStateLen bpState)

          if bpStateLen bpState + pc_ib >= 64
            then do
              -- Write full word
              DVSM.write bpvm bpsReady $
                pdep remainder_bits_a 0x5555555555555555 .|.
                pdep remainder_bits_a 0xaaaaaaaaaaaaaaaa .|.
                pdep remainder_bits_d 0xaaaaaaaaaaaaaaaa

              DVSM.write bpvm (bpsReady + 1) $
                pdep (remainder_bits_a .>. 32) 0x5555555555555555 .|.
                pdep (remainder_bits_a .>. 32) 0xaaaaaaaaaaaaaaaa .|.
                pdep (remainder_bits_d .>. 32) 0xaaaaaaaaaaaaaaaa

              -- Set up for next iteration
              let nextBpState = bpState
                    { bpStateD    = ext_d .>. (64 - remainder_len)
                    , bpStateA    = ext_a .>. (64 - remainder_len)
                    , bpStateZ    = ext_z .>. (64 - remainder_len)
                    , bpStateLen  = remainder_len + pc_ib - 64
                    }
              go (i + 1) (bpsReady + 2) nextBpState bpvm
            else do
              let nextBpState = bpState
                    { bpStateLen  = remainder_len + pc_ib
                    }
              go (i + 1) bpsReady nextBpState bpvm
        go i _ bpState _ = return (bpState, i)
mkIndexStep _ _ _ = error "Mismatched input size"

stepToByteString :: BpState -> Step -> (BpState, BS.ByteString)
stepToByteString state (Step step size) = F.unsafeLocalState $ do
  let bsSize = size * 8
  bpFptr <- BSI.mallocByteString bsSize
  let bpVm = DVSM.unsafeFromForeignPtr (F.castForeignPtr bpFptr) 0 size
  (bpState2, _) <- stToIO $ step state bpVm
  return (bpState2, BSI.PS bpFptr 0 size)
