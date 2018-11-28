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
  if (argc != 4) {
    fprintf(stderr, "./a.out <input-file> <output-ib-file> <output-bp-file>\n");
    exit(1);
  }

  char *in_filename     = argv[1];
  char *ib_out_filename = argv[2];
  char *bp_out_filename = argv[3];

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

  uint8_t buffer[W8_BUFFER_SIZE];

  uint32_t result_ib[W8_BUFFER_SIZE];
  uint32_t result_a [W8_BUFFER_SIZE];
  uint32_t result_z [W8_BUFFER_SIZE];
  uint64_t accum = 0;

  uint8_t out_bp_buffer[W32_BUFFER_SIZE * 2];

  size_t total_bytes_read = 0;
  vm256i_t state;
  memset(&state, 0, sizeof(state));
  state.w32s.w0 = 0x03020100;

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

    accum += sm_process_chunk(buffer, bytes_read,
      &state.w32s.w0);

    size_t ib_bytes = (bytes_read + 7) / 8;

    fwrite(result_ib, 1, ib_bytes, ib_out);

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

  fwrite(out_bp_buffer, 2, sizeof(uint64_t), bp_out);

  fclose(in);
  fclose(ib_out);

  return 0;
}

// __m256i _mm256_mask_i32gather_epi32 (
//     __m256i src,
//     int const* base_addr,
//     __m256i vindex,
//     __m256i mask,
//     const int scale)

uint64_t sm_process_chunk(
    uint8_t *in_buffer,
    size_t in_length,
    uint32_t *inout_state) {
  vm128i_t states;

  memset(&states, 0, sizeof(states));
  
  states.w32s.w0 = *inout_state;
  states.w32s.w2 = *inout_state;

  vm128i_t new_states;

  // printf("%zu\n", in_length);

  for (size_t i = 0; i < in_length; i += 4) {
    // printf("====\n");
    new_states.m = _mm_shuffle_epi8(transition_phi_table[in_buffer[i    ]], states.m);
    states.w32s.w0  = new_states.w32s.w0;
    states.w32s.w4  = new_states.w32s.w0;

    new_states.m = _mm_shuffle_epi8(transition_phi_table[in_buffer[i + 1]], states.m);
    states.w32s.w0  = new_states.w32s.w0;
    states.w32s.w4  = new_states.w32s.w0;

    new_states.m = _mm_shuffle_epi8(transition_phi_table[in_buffer[i + 2]], states.m);
    states.w32s.w0  = new_states.w32s.w0;
    states.w32s.w4  = new_states.w32s.w0;

    new_states.m = _mm_shuffle_epi8(transition_phi_table[in_buffer[i + 3]], states.m);
    states.w32s.w0  = new_states.w32s.w0;
    states.w32s.w4  = new_states.w32s.w0;

    // printf("states.m    : "); print256_num(states.m     ); printf("\n");
    // printf("step        : "); print256_num(step         ); printf("\n");
    // printf("new_states.m: "); print256_num(new_states.m ); printf("\n");

    // printf("%c: %d\n", in_buffer[i], new_states.w8s.w0);

  }

  *inout_state = states.w32s.w0;

  return 0;
}
