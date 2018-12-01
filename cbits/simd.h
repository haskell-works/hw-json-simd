#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>

void print256_num(__m256i var);

void fprint256_num(FILE *file, __m256i var);

void print128_num(__m128i var);

void fprint128_num(FILE *file, __m128i var);

void print_bits_64(uint64_t v);

typedef struct bp_state bp_state_t;

int main_spliced(
    int argc,
    char **argv);

uint64_t process_chunk(
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

void init_bp_state(
    bp_state_t *bp_state);

size_t write_bp_chunk(
    uint8_t *result_ib,
    uint8_t *result_a,
    uint8_t *result_z,
    size_t ib_bytes,
    bp_state_t *bp_state,
    uint8_t *out_buffer);

size_t write_bp_chunk_final(
    bp_state_t *bp_state,
    uint8_t *out_buffer);

// ---

int sm_main(
    int argc,
    char **argv);

void sm_process_chunk(
    uint8_t *in_buffer,
    size_t in_length,
    uint32_t *inout_state,
    uint32_t *out_phi_buffer);

void make_ib_bp_chunks(
    uint8_t state,
    uint32_t *in_phis,
    size_t phi_length,
    uint8_t *out_ibs,
    uint8_t *out_ops,
    uint8_t *out_cls);

void summarise_2(
    uint8_t *buffer,
    uint16_t *out_mask_d,
    uint16_t *out_mask_a,
    uint16_t *out_mask_z,
    uint16_t *out_mask_q,
    uint16_t *out_mask_b);
