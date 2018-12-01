#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <immintrin.h>

#include "simd.h"

#define W8_BUFFER_SIZE    (1024 * 32)
#define W32_BUFFER_SIZE   (W8_BUFFER_SIZE / 4)
#define W64_BUFFER_SIZE   (W8_BUFFER_SIZE / 8)

extern __m256i transition_phi_table_wide[256];
extern __m128i transition_phi_table[256];

typedef struct vec32_8i {
  uint8_t w00;
  uint8_t w01;
  uint8_t w02;
  uint8_t w03;
  uint8_t w04;
  uint8_t w05;
  uint8_t w06;
  uint8_t w07;
  uint8_t w08;
  uint8_t w09;
  uint8_t w10;
  uint8_t w11;
  uint8_t w12;
  uint8_t w13;
  uint8_t w14;
  uint8_t w15;
  uint8_t w16;
  uint8_t w17;
  uint8_t w18;
  uint8_t w19;
  uint8_t w20;
  uint8_t w21;
  uint8_t w22;
  uint8_t w23;
  uint8_t w24;
  uint8_t w25;
  uint8_t w26;
  uint8_t w27;
  uint8_t w28;
  uint8_t w29;
  uint8_t w30;
  uint8_t w31;
} vec32_8i_t;

typedef struct vec16_16i {
  uint16_t w00;
  uint16_t w01;
  uint16_t w02;
  uint16_t w03;
  uint16_t w04;
  uint16_t w05;
  uint16_t w06;
  uint16_t w07;
  uint16_t w08;
  uint16_t w09;
  uint16_t w10;
  uint16_t w11;
  uint16_t w12;
  uint16_t w13;
  uint16_t w14;
  uint16_t w15;
} vec16_16i_t;

typedef struct vec8_32i {
  uint32_t w0;
  uint32_t w1;
  uint32_t w2;
  uint32_t w3;
  uint32_t w4;
  uint32_t w5;
  uint32_t w6;
  uint32_t w7;
} vec8_32i_t;

typedef struct vec4_64i {
  uint64_t w0;
  uint64_t w1;
  uint64_t w2;
  uint64_t w3;
} vec4_64i_t;

typedef union vm256i {
  vec8_32i_t w8s;
  vec8_32i_t w16s;
  vec8_32i_t w32s;
  vec4_64i_t w64s;
  __m256i m;
} vm256i_t;





typedef struct vec16_8i {
  uint8_t w00;
  uint8_t w01;
  uint8_t w02;
  uint8_t w03;
  uint8_t w04;
  uint8_t w05;
  uint8_t w06;
  uint8_t w07;
  uint8_t w08;
  uint8_t w09;
  uint8_t w10;
  uint8_t w11;
  uint8_t w12;
  uint8_t w13;
  uint8_t w14;
  uint8_t w15;
} vec16_8i_t;

typedef struct vec8_16i {
  uint16_t w00;
  uint16_t w01;
  uint16_t w02;
  uint16_t w03;
  uint16_t w04;
  uint16_t w05;
  uint16_t w06;
  uint16_t w07;
} vec8_16i_t;

typedef struct vec4_32i {
  uint32_t w0;
  uint32_t w1;
  uint32_t w2;
  uint32_t w3;
} vec4_32i_t;

typedef struct vec2_64i {
  uint64_t w0;
  uint64_t w1;
} vec2_64i_t;

typedef union vm128i {
  vec8_32i_t w8s;
  vec8_32i_t w16s;
  vec8_32i_t w32s;
  vec4_64i_t w64s;
  __m128i m;
} vm128i_t;



int sm_main(
    int argc,
    char **argv) {
  if (argc != 6) {
    fprintf(stderr, "./a.out <input-file> <output-ib-file> <output-bp-file> <output-op-file> <output-cl-file>\n");
    exit(1);
  }

  char *in_filename     = argv[1];
  char *ib_out_filename = argv[2];
  char *bp_out_filename = argv[3];
  char *op_out_filename = argv[4];
  char *cl_out_filename = argv[5];

  FILE *in = fopen(in_filename, "r");

  if (!in) {
    fprintf(stderr, "Failed to open input file %s\n", in_filename);
    exit(1);
  }

  FILE *ib_out = fopen(ib_out_filename, "w");
  
  if (!ib_out) {
    fprintf(stderr, "Failed to open ib output file %s\n", ib_out_filename);
    exit(1);
  }

  FILE *bp_out = fopen(bp_out_filename, "w");
  
  if (!bp_out) {
    fprintf(stderr, "Failed to open bp output file %s\n", bp_out_filename);
    exit(1);
  }

  FILE *op_out = fopen(op_out_filename, "w");

  if (!op_out) {
    fprintf(stderr, "Failed to open op output file %s\n", op_out_filename);
    exit(1);
  }

  FILE *cl_out = fopen(cl_out_filename, "w");

  if (!cl_out) {
    fprintf(stderr, "Failed to open cl output file %s\n", cl_out_filename);
    exit(1);
  }

  uint8_t buffer[W8_BUFFER_SIZE];
  uint32_t phi_buffer[W8_BUFFER_SIZE];

  uint8_t ibs_buffer[W8_BUFFER_SIZE];
  uint8_t ops_buffer[W8_BUFFER_SIZE];
  uint8_t cls_buffer[W8_BUFFER_SIZE];

  uint32_t result_ib[W8_BUFFER_SIZE];
  uint32_t result_a [W8_BUFFER_SIZE];
  uint32_t result_z [W8_BUFFER_SIZE];
  uint64_t accum = 0;

  uint8_t out_bp_buffer[W32_BUFFER_SIZE * 2];

  size_t total_bytes_read = 0;
  uint32_t state = 0x03020100;

  while (1) {
    size_t bytes_read = fread(buffer, 1, W8_BUFFER_SIZE, in);

    total_bytes_read += bytes_read;

    if (bytes_read < W8_BUFFER_SIZE) {
      if (ferror(in)) {
        fprintf(stderr, "Error reading file\n");
        exit(1);
      }

      if (bytes_read == 0) {
        if (feof(in)) {
          break;
        }
      }

      size_t next_alignment = ((bytes_read + 63) / 64) * 64;

      memset(buffer + bytes_read, 0, next_alignment - bytes_read);

      bytes_read = next_alignment;
    }

    sm_process_chunk(buffer, bytes_read,
      &state,
      phi_buffer);

    make_ib_bp_chunks(state, phi_buffer, bytes_read,
      ibs_buffer,
      ops_buffer,
      cls_buffer);

    size_t idx_bytes = (bytes_read + 7) / 8;

    fwrite(result_ib, 1, idx_bytes, ib_out);

    // size_t out_bp_bytes = write_bp_chunk(
    //   result_ib,
    //   result_a,
    //   result_z,
    //   out_bp_buffer);

    // fwrite(out_bp_buffer, out_bp_bytes, sizeof(uint64_t), bp_out);

    fflush(ib_out);
    fflush(bp_out);
  }

  // write_bp_chunk_final(&bp_state, out_bp_buffer);

  fprintf(stderr, "Final state %u\n", state);

  fwrite(out_bp_buffer, 2, sizeof(uint64_t), bp_out);

  fclose(in);
  fclose(ib_out);

  return 0;
}

void make_ib_bp_chunks(
    uint8_t state,
    uint32_t *in_phis,
    size_t phi_length,
    uint8_t *out_ibs,
    uint8_t *out_ops,
    uint8_t *out_cls) {
  __m128i ib_offset = _mm_set_epi64x(0, 5 + state * 8);
  __m128i op_offset = _mm_set_epi64x(0, 6 + state * 8);
  __m128i cl_offset = _mm_set_epi64x(0, 7 + state * 8);

  for (size_t i = 0; i < phi_length; i += 8) {
    __m256i v_8 = *(__m256i *)&in_phis[i];
    __m256i v_ib_8 = _mm256_sll_epi64(v_8, ib_offset);
    __m256i v_op_8 = _mm256_sll_epi64(v_8, op_offset);
    __m256i v_cl_8 = _mm256_sll_epi64(v_8, cl_offset);
    uint8_t all_ibs = (uint8_t)_pext_u32(_mm256_movemask_epi8(v_ib_8), 0x11111111);
    uint8_t all_ops = (uint8_t)_pext_u32(_mm256_movemask_epi8(v_op_8), 0x11111111);
    uint8_t all_cls = (uint8_t)_pext_u32(_mm256_movemask_epi8(v_cl_8), 0x11111111);

    size_t j = i / 8;
    out_ibs[j] = all_ibs;
    out_ops[j] = all_ops;
    out_cls[j] = all_cls;
  }

}
// __m256i _mm256_mask_i32gather_epi32 (
//     __m256i src,
//     int const* base_addr,
//     __m256i vindex,
//     __m256i mask,
//     const int scale)



extern uint32_t transition_table_simd[];

extern uint32_t phi_table_simd[];

void sm_process_chunk(
    uint8_t *in_buffer,
    size_t in_length,
    uint32_t *inout_state,
    uint32_t *out_phi_buffer) {  
  __m256i s = _mm256_set_epi64x(0, *inout_state, 0, *inout_state);

  size_t in_length_div_8 = in_length / 8;

  uint8_t *buf0 = in_buffer;
  uint8_t *buf1 = in_buffer + in_length_div_8 * 1;
  uint8_t *buf2 = in_buffer + in_length_div_8 * 2;
  uint8_t *buf3 = in_buffer + in_length_div_8 * 3;
  uint8_t *buf4 = in_buffer + in_length_div_8 * 4;
  uint8_t *buf5 = in_buffer + in_length_div_8 * 5;
  uint8_t *buf6 = in_buffer + in_length_div_8 * 6;
  uint8_t *buf7 = in_buffer + in_length_div_8 * 7;

  uint32_t *phi0 = out_phi_buffer;
  uint32_t *phi1 = out_phi_buffer + in_length_div_8 * 1;
  uint32_t *phi2 = out_phi_buffer + in_length_div_8 * 2;
  uint32_t *phi3 = out_phi_buffer + in_length_div_8 * 3;

  __m256i s0 = _mm256_set_epi64x(0, 0, 0, 0x03020100);
  __m256i s1 = _mm256_set_epi64x(0, 0, 0, 0x03020100);
  __m256i s2 = _mm256_set_epi64x(0, 0, 0, 0x03020100);
  __m256i s3 = _mm256_set_epi64x(0, 0, 0, 0x03020100);

  for (size_t i = 0; i < in_length_div_8; i += 1) {
    s0 = _mm256_shuffle_epi8(_mm256_set_epi64x(0, transition_table_simd[buf4[i]], 0, transition_table_simd[buf0[i]]), s0);
    s1 = _mm256_shuffle_epi8(_mm256_set_epi64x(0, transition_table_simd[buf5[i]], 0, transition_table_simd[buf1[i]]), s1);
    s2 = _mm256_shuffle_epi8(_mm256_set_epi64x(0, transition_table_simd[buf6[i]], 0, transition_table_simd[buf2[i]]), s2);
    s3 = _mm256_shuffle_epi8(_mm256_set_epi64x(0, transition_table_simd[buf7[i]], 0, transition_table_simd[buf3[i]]), s3);
  }

  s = _mm256_shuffle_epi8(s0, s);
  s = _mm256_shuffle_epi8(s1, s);
  s = _mm256_shuffle_epi8(s2, s);
  s = _mm256_shuffle_epi8(s3, s);

  *inout_state = _mm256_extract_epi64(s, 0);
}
