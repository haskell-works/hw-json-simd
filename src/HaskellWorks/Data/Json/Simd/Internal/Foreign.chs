{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module HaskellWorks.Data.Json.Simd.Internal.Foreign where

import Foreign
import System.IO.Unsafe

#include "../cbits/simd.h"

type UInt8  = {#type uint8_t #}
type UInt32 = {#type uint32_t#}
type UInt64 = {#type uint64_t#}
type Size   = {#type size_t  #}

enabled_avx_2 :: IO Int
enabled_avx_2 = fromIntegral <$> do
  {#call unsafe hw_json_simd_avx2_enabled as c_hw_json_simd_avx2_enabled#}
{-# NOINLINE enabled_avx_2 #-}

enabled_sse_4_2 :: IO Int
enabled_sse_4_2 = fromIntegral <$> do
  {#call unsafe hw_json_simd_sse4_2_enabled as c_hw_json_simd_sse4_2_enabled#}
{-# NOINLINE enabled_sse_4_2 #-}

enabled_bmi_2 :: IO Int
enabled_bmi_2 = fromIntegral <$> do
  {#call unsafe hw_json_simd_bmi2_enabled as c_hw_json_simd_bmi2_enabled#}
{-# NOINLINE enabled_bmi_2 #-}

processChunk :: ()
  => Ptr UInt8    -- in_buffer
  -> Size         -- in_length
  -> Ptr UInt8    -- work_bits_of_d
  -> Ptr UInt8    -- work_bits_of_a
  -> Ptr UInt8    -- work_bits_of_z
  -> Ptr UInt8    -- work_bits_of_q
  -> Ptr UInt8    -- work_bits_of_b
  -> Ptr UInt8    -- work_bits_of_e
  -> Ptr Size     -- last_trailing_ones
  -> Ptr Size     -- quote_odds_carry
  -> Ptr Size     -- quote_evens_carry
  -> Ptr UInt64   -- quote_mask_carry
  -> Ptr UInt8    -- result_ibs
  -> Ptr UInt8    -- result_a
  -> Ptr UInt8    -- result_z
  -> IO UInt64
processChunk = do
  {#call unsafe hw_json_simd_process_chunk as c_hw_json_simd_process_chunk#}
{-# INLINE processChunk #-}

initBpState :: ()
  => Ptr ()
  -> IO ()
initBpState = {#call unsafe hw_json_simd_init_bp_state as c_hw_json_simd_init_bp_state#}
{-# INLINE initBpState #-}

writeBpChunk :: ()
  => Ptr UInt8  -- result_ib
  -> Ptr UInt8  -- result_a
  -> Ptr UInt8  -- result_z
  -> Size       -- ib_bytes
  -> Ptr ()     -- bp_state
  -> Ptr UInt8  -- out_buffer
  -> IO Size
writeBpChunk = {#call unsafe hw_json_simd_write_bp_chunk as c_hw_json_simd_write_bp_chunk#}
{-# INLINE writeBpChunk #-}

writeBpChunkFinal :: ()
  => Ptr ()     -- bp_state
  -> Ptr UInt8  -- out_buffer
  -> IO Size
writeBpChunkFinal = {#call unsafe hw_json_simd_write_bp_chunk_final as c_hw_json_simd_write_bp_chunk_final#}
{-# INLINE writeBpChunkFinal #-}

smProcessChunk :: ()
  => Ptr UInt8    -- in_buffer
  -> Size         -- in_length
  -> Ptr UInt32   -- inout_state
  -> Ptr UInt32   -- out_phi_buffer
  -> IO ()
smProcessChunk = {#call unsafe hw_json_simd_sm_process_chunk as c_hw_json_simd_sm_process_chunk#}

smMakeIbOpClChunks :: ()
  => UInt8        -- state
  -> Ptr UInt32   -- in_phis
  -> Size         -- phi_length
  -> Ptr UInt8    -- out_ibs
  -> Ptr UInt8    -- out_ops
  -> Ptr UInt8    -- out_cls
  -> IO ()
smMakeIbOpClChunks = {#call unsafe hw_json_simd_sm_make_ib_op_cl_chunks as c_hw_json_simd_sm_make_ib_op_cl_chunks#}

smWriteBpChunk :: ()
  => Ptr UInt8    -- result_op
  -> Ptr UInt8    -- result_cl
  -> Size         -- ib_bytes
  -> Ptr UInt64   -- remaining_bp_bits
  -> Ptr Size     -- remaning_bp_bits_len
  -> Ptr UInt64   -- out_buffer
  -> IO Size
smWriteBpChunk = {#call unsafe hw_json_simd_sm_write_bp_chunk as c_hw_json_simd_sm_write_bp_chunk#}

smWriteBpChunkFinal :: ()
  => UInt64       -- remaining_bits
  -> Size         -- remaining_bits_len
  -> Ptr UInt64   -- out_buffer
  -> IO Size
smWriteBpChunkFinal = {#call unsafe hw_json_simd_sm_write_bp_chunk_final as c_hw_json_simd_sm_write_bp_chunk_final#}
