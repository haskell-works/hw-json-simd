#include <stdint.h>
#include <stdio.h>

#include "intrinsics.h"

#if defined __AVX2__
inline void hw_simd_json_fprint256_num(FILE *file, __m256i var) {
  uint8_t *val = (uint8_t*)&var;
  fprintf(file,
    "%02x %02x %02x %02x %02x %02x %02x %02x  %02x %02x %02x %02x %02x %02x %02x %02x  "
    "%02x %02x %02x %02x %02x %02x %02x %02x  %02x %02x %02x %02x %02x %02x %02x %02x"
    , val[ 0], val[ 1], val[ 2], val[ 3], val[ 4], val[ 5], val[ 6], val[ 7]
    , val[ 8], val[ 9], val[10], val[11], val[12], val[13], val[14], val[15]
    , val[16], val[17], val[18], val[19], val[20], val[21], val[22], val[23]
    , val[24], val[25], val[26], val[27], val[28], val[29], val[30], val[31]);
}
#endif//__AVX2__

#if defined __AVX2__
inline void hw_simd_json_fprint128_num(FILE *file, __m128i var) {
  uint8_t *val = (uint8_t*)&var;
  fprintf(file,
    "%02x %02x %02x %02x %02x %02x %02x %02x  %02x %02x %02x %02x %02x %02x %02x %02x"
    , val[ 0], val[ 1], val[ 2], val[ 3], val[ 4], val[ 5], val[ 6], val[ 7]
    , val[ 8], val[ 9], val[10], val[11], val[12], val[13], val[14], val[15]);
}
#endif//__AVX2__

#if defined __AVX2__
inline void hw_simd_json_print256_num(__m256i var) {
  hw_simd_json_fprint256_num(stdout, var);
}
#endif//__AVX2__

#if defined __AVX2__
inline void hw_simd_json_print128_num(__m128i var) {
  hw_simd_json_fprint128_num(stdout, var);
}
#endif//__AVX2__

inline void hw_simd_json_print_bits_8(uint8_t v) {
  char *digits = "01";
  int i = 0;

  for (i = 0; i < 8; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}

inline void hw_simd_json_print_bits_16(uint16_t v) {
  char *digits = "01";
  int i = 0;

  for (i = 0; i < 16; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}

inline void hw_simd_json_print_bits_32(uint32_t v) {
  char *digits = "01";
  int i = 0;

  for (i = 0; i < 32; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}

inline void hw_simd_json_print_bits_64(uint64_t v) {
  char *digits = "01";
  int i = 0;

  for (i = 0; i < 64; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}

#if defined __AVX2__
inline void hw_simd_json_print_bits_128(__m128i v) {
  hw_simd_json_print_bits_64(_mm_extract_epi64(v, 0));
  printf("-");
  hw_simd_json_print_bits_64(_mm_extract_epi64(v, 1));
}
#endif//__AVX2__

#if defined __AVX2__
inline void hw_simd_json_print_bits_256(__m256i v) {
  int i = 0;

  hw_simd_json_print_bits_64(_mm256_extract_epi64(v, 0));
  printf("-");
  hw_simd_json_print_bits_64(_mm256_extract_epi64(v, 1));
  printf("-");
  hw_simd_json_print_bits_64(_mm256_extract_epi64(v, 2));
  printf("-");
  hw_simd_json_print_bits_64(_mm256_extract_epi64(v, 3));
}
#endif//__AVX2__
