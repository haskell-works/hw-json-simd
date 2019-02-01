#include "intrinsics.h"

#include <stdint.h>
#include <stdio.h>

#define W8_BUFFER_SIZE    (1024 * 32)
#define W32_BUFFER_SIZE   (W8_BUFFER_SIZE / 4)
#define W64_BUFFER_SIZE   (W8_BUFFER_SIZE / 8)

typedef struct hw_json_simd_bp_state hw_json_simd_bp_state_t;

int hw_json_simd_avx2_enabled();

int hw_json_simd_bmi2_enabled();

int hw_json_simd_sse4_2_enabled();

extern uint32_t hw_json_simd_transition_table_32[256];
extern uint32_t hw_json_simd_phi_table_32       [256];

int hw_json_simd_main_spliced(
    int argc,
    char **argv);

uint64_t hw_json_simd_process_chunk(
    uint8_t *in_buffer,
    size_t in_length,
    uint8_t *work_bits_of_d,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_a,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_z,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_q,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_b,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_e,       // Working buffer of minimum length ((in_length + 63) / 64)
    size_t *last_trailing_ones,
    size_t *quote_odds_carry,
    size_t *quote_evens_carry,
    uint64_t *quote_mask_carry,
    uint8_t *result_ibs,
    uint8_t *result_a,
    uint8_t *result_z);

void hw_json_simd_init_bp_state(
    hw_json_simd_bp_state_t *bp_state);

size_t hw_json_simd_write_bp_chunk(
    uint8_t *result_ib,
    uint8_t *result_a,
    uint8_t *result_z,
    size_t ib_bytes,
    hw_json_simd_bp_state_t *bp_state,
    uint8_t *out_buffer);

size_t hw_json_simd_write_bp_chunk_final(
    hw_json_simd_bp_state_t *bp_state,
    uint8_t *out_buffer);

// ---

size_t
hw_json_simd_sm_write_bp_chunk(
    uint8_t *result_op,
    uint8_t *result_cl,
    size_t ib_bytes,
    uint64_t *remaining_bp_bits,
    size_t *remaning_bp_bits_len,
    uint64_t *out_buffer);

size_t
hw_json_simd_sm_write_bp_chunk_final(
    uint64_t remaining_bits,
    size_t remaining_bits_len,
    uint64_t *out_buffer);

void
hw_json_simd_sm_process_chunk(
    uint8_t *in_buffer,
    size_t in_length,
    uint32_t *inout_state,
    uint32_t *out_phi_buffer);

void
hw_json_simd_sm_make_ib_op_cl_chunks(
    uint8_t state,
    uint32_t *in_phis,
    size_t phi_length,
    uint8_t *out_ibs,
    uint8_t *out_ops,
    uint8_t *out_cls);
