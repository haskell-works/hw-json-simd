#include "intrinsics.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "simd.h"

#define W8_BUFFER_SIZE    (1024 * 32)
#define W32_BUFFER_SIZE   (W8_BUFFER_SIZE / 4)
#define W64_BUFFER_SIZE   (W8_BUFFER_SIZE / 8)

typedef struct hw_json_simd_bp_state {
  uint64_t  remainder_bits_d;
  uint64_t  remainder_bits_a;
  uint64_t  remainder_bits_z;
  size_t    remainder_len;
} hw_json_simd_bp_state_t;

int hw_json_simd_main_spliced(
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

  // align stack buffer on 32 bytes 
  uint8_t unaligned_buffer[W8_BUFFER_SIZE + 32];
  uint8_t * buffer = unaligned_buffer + ((uintptr_t)unaligned_buffer & (uintptr_t)0x10);
  fprintf(stderr, "Buffer is at address %p of size %zu\n", buffer, (size_t) W8_BUFFER_SIZE);

  uint8_t *bits_of_d = malloc(W32_BUFFER_SIZE); memset(bits_of_d, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_a = malloc(W32_BUFFER_SIZE); memset(bits_of_a, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_z = malloc(W32_BUFFER_SIZE); memset(bits_of_z, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_b = malloc(W32_BUFFER_SIZE); memset(bits_of_b, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_e = malloc(W32_BUFFER_SIZE); memset(bits_of_e, 0, W32_BUFFER_SIZE);
  uint8_t *bits_of_q = malloc(W32_BUFFER_SIZE); memset(bits_of_q, 0, W32_BUFFER_SIZE);

  uint8_t result_ib[W8_BUFFER_SIZE / 8];
  uint8_t result_a [W8_BUFFER_SIZE / 8];
  uint8_t result_z [W8_BUFFER_SIZE / 8];
  hw_json_simd_bp_state_t bp_state;

  uint64_t accum = 0;

  size_t last_trailing_ones = 0;
  size_t total_bytes_read   = 0;
  uint64_t quote_mask_carry = 0;
  size_t quote_odds_carry   = 0;
  size_t quote_evens_carry  = 1;

  uint8_t out_bp_buffer[W32_BUFFER_SIZE * 2];

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

    accum += hw_json_simd_process_chunk(buffer, bytes_read,
      bits_of_d,
      bits_of_a,
      bits_of_z,
      bits_of_q,
      bits_of_b,
      bits_of_e,
      &last_trailing_ones,
      &quote_odds_carry,
      &quote_evens_carry,
      &quote_mask_carry,
      result_ib,
      result_a,
      result_z);

    size_t ib_bytes = (bytes_read + 7) / 8;

    fwrite(result_ib, 1, ib_bytes, ib_out);

    size_t out_bp_bytes = hw_json_simd_write_bp_chunk(
      result_ib,
      result_a,
      result_z,
      ib_bytes,
      &bp_state,
      out_bp_buffer);

    fwrite(out_bp_buffer, out_bp_bytes, sizeof(uint64_t), bp_out);

    fflush(ib_out);
    fflush(bp_out);
  }

  hw_json_simd_write_bp_chunk_final(&bp_state, out_bp_buffer);

  fwrite(out_bp_buffer, 2, sizeof(uint64_t), bp_out);

  fclose(in);
  fclose(ib_out);

  return 0;
}

void hw_json_simd_init_bp_state(
    hw_json_simd_bp_state_t *bp_state) {
  memset(bp_state, 0, sizeof(*bp_state));
}

size_t hw_json_simd_write_bp_chunk(
    uint8_t *result_ib,
    uint8_t *result_a,
    uint8_t *result_z,
    size_t ib_bytes,
    hw_json_simd_bp_state_t *bp_state,
    uint8_t *out_buffer) {
  uint64_t *w64_result_ib = (uint64_t *)result_ib;
  uint64_t *w64_result_a  = (uint64_t *)result_a;
  uint64_t *w64_result_z  = (uint64_t *)result_z;
  uint64_t *w64_work_bp   = (uint64_t *)out_buffer;

  uint64_t  w64_len           = ib_bytes / 8;
  uint64_t  remainder_bits_d  = (*bp_state).remainder_bits_d;
  uint64_t  remainder_bits_a  = (*bp_state).remainder_bits_a;
  uint64_t  remainder_bits_z  = (*bp_state).remainder_bits_z;
  size_t    remainder_len     = (*bp_state).remainder_len;
  size_t    w64s_ready        = 0;

  for (size_t i = 0; i < w64_len; ++i) {
    uint64_t w64_ib = w64_result_ib[i];
    uint64_t w64_a  = w64_result_a[i];
    uint64_t w64_z  = w64_result_z[i];

    size_t pc_ib = __builtin_popcountll(w64_ib);

    // Ignoring non interesting bits, get a bitmask of all delimiters and
    // opens and closes.
    uint64_t ext_d = _pext_u64(~(w64_a | w64_z) , w64_ib);
    uint64_t ext_a = _pext_u64(w64_a            , w64_ib);
    uint64_t ext_z = _pext_u64(w64_z            , w64_ib);

    // Merge with the remainder bits.  Extract bits need to be shifted
    // to avoid cloberring the remainder bits.
    remainder_bits_d |= (ext_d << remainder_len);
    remainder_bits_a |= (ext_a << remainder_len);
    remainder_bits_z |= (ext_z << remainder_len);

    if (remainder_len + pc_ib >= 64) {
      // Write full word because we have enough bits
      w64_work_bp[w64s_ready] =
        _pdep_u64(remainder_bits_a, 0x5555555555555555) |
        _pdep_u64(remainder_bits_a, 0xaaaaaaaaaaaaaaaa) |
        _pdep_u64(remainder_bits_d, 0xaaaaaaaaaaaaaaaa);

      w64s_ready += 1;

      remainder_bits_a = remainder_bits_a >> 32;
      remainder_bits_z = remainder_bits_z >> 32;
      remainder_bits_d = remainder_bits_d >> 32;

      w64_work_bp[w64s_ready] =
        _pdep_u64(remainder_bits_a, 0x5555555555555555) |
        _pdep_u64(remainder_bits_a, 0xaaaaaaaaaaaaaaaa) |
        _pdep_u64(remainder_bits_d, 0xaaaaaaaaaaaaaaaa);

      w64s_ready += 1;

      // Set up for next iteration the bits that didn't fit
      remainder_bits_d = ext_d >> (64 - remainder_len);
      remainder_bits_a = ext_a >> (64 - remainder_len);
      remainder_bits_z = ext_z >> (64 - remainder_len);

      remainder_len = remainder_len + pc_ib - 64;
    } else {
      remainder_len += pc_ib;
    }
  }

  (*bp_state).remainder_bits_d  = remainder_bits_d;
  (*bp_state).remainder_bits_a  = remainder_bits_a;
  (*bp_state).remainder_bits_z  = remainder_bits_z;
  (*bp_state).remainder_len     = remainder_len;

  return w64s_ready;
}

size_t hw_json_simd_write_bp_chunk_final(
    hw_json_simd_bp_state_t *bp_state,
    uint8_t *out_buffer) {
  uint64_t *w64_work_bp   = (uint64_t *)out_buffer;

  uint64_t  remainder_bits_d  = (*bp_state).remainder_bits_d;
  uint64_t  remainder_bits_a  = (*bp_state).remainder_bits_a;
  uint64_t  remainder_bits_z  = (*bp_state).remainder_bits_z;
  size_t    w64s_ready        = 0;

  // Write full word
  w64_work_bp[w64s_ready] =
    _pdep_u64(remainder_bits_a, 0x5555555555555555) |
    _pdep_u64(remainder_bits_a, 0xaaaaaaaaaaaaaaaa) |
    _pdep_u64(remainder_bits_d, 0xaaaaaaaaaaaaaaaa);

  w64s_ready += 1;

  remainder_bits_a = remainder_bits_a >> 32;
  remainder_bits_z = remainder_bits_z >> 32;
  remainder_bits_d = remainder_bits_d >> 32;

  w64_work_bp[w64s_ready] =
    _pdep_u64(remainder_bits_a, 0x5555555555555555) |
    _pdep_u64(remainder_bits_a, 0xaaaaaaaaaaaaaaaa) |
    _pdep_u64(remainder_bits_d, 0xaaaaaaaaaaaaaaaa);

  w64s_ready += 1;

  return w64s_ready;
}

uint8_t hw_json_simd_escape_mask[2][256] =
  { { 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0xdf, 0xdd, 0xdb, 0xdf, 0xd7, 0xd5, 0xdf, 0xd7, 0xff, 0xfd, 0xfb, 0xff, 0xdf, 0xdd, 0xff, 0xdf
    , 0xbf, 0xbd, 0xbb, 0xbf, 0xb7, 0xb5, 0xbf, 0xb7, 0xaf, 0xad, 0xab, 0xaf, 0xbf, 0xbd, 0xaf, 0xbf
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xbf, 0xbd, 0xbb, 0xbf, 0xff, 0xfd, 0xbf, 0xff
    , 0x7f, 0x7d, 0x7b, 0x7f, 0x77, 0x75, 0x7f, 0x77, 0x6f, 0x6d, 0x6b, 0x6f, 0x7f, 0x7d, 0x6f, 0x7f
    , 0x5f, 0x5d, 0x5b, 0x5f, 0x57, 0x55, 0x5f, 0x57, 0x7f, 0x7d, 0x7b, 0x7f, 0x5f, 0x5d, 0x7f, 0x5f
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0x7f, 0x7d, 0x7b, 0x7f, 0x77, 0x75, 0x7f, 0x77, 0xff, 0xfd, 0xfb, 0xff, 0x7f, 0x7d, 0xff, 0x7f
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0xdf, 0xdd, 0xdb, 0xdf, 0xd7, 0xd5, 0xdf, 0xd7, 0xff, 0xfd, 0xfb, 0xff, 0xdf, 0xdd, 0xff, 0xdf
    , 0xbf, 0xbd, 0xbb, 0xbf, 0xb7, 0xb5, 0xbf, 0xb7, 0xaf, 0xad, 0xab, 0xaf, 0xbf, 0xbd, 0xaf, 0xbf
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xbf, 0xbd, 0xbb, 0xbf, 0xff, 0xfd, 0xbf, 0xff
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0xdf, 0xdd, 0xdb, 0xdf, 0xd7, 0xd5, 0xdf, 0xd7, 0xff, 0xfd, 0xfb, 0xff, 0xdf, 0xdd, 0xff, 0xdf
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xef, 0xed, 0xeb, 0xef, 0xff, 0xfd, 0xef, 0xff
    , 0xff, 0xfd, 0xfb, 0xff, 0xf7, 0xf5, 0xff, 0xf7, 0xff, 0xfd, 0xfb, 0xff, 0xff, 0xfd, 0xff, 0xff
    }
  , { 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0xde, 0xdf, 0xda, 0xdb, 0xd6, 0xd7, 0xde, 0xdf, 0xfe, 0xff, 0xfa, 0xfb, 0xde, 0xdf, 0xfe, 0xff
    , 0xbe, 0xbf, 0xba, 0xbb, 0xb6, 0xb7, 0xbe, 0xbf, 0xae, 0xaf, 0xaa, 0xab, 0xbe, 0xbf, 0xae, 0xaf
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xbe, 0xbf, 0xba, 0xbb, 0xfe, 0xff, 0xbe, 0xbf
    , 0x7e, 0x7f, 0x7a, 0x7b, 0x76, 0x77, 0x7e, 0x7f, 0x6e, 0x6f, 0x6a, 0x6b, 0x7e, 0x7f, 0x6e, 0x6f
    , 0x5e, 0x5f, 0x5a, 0x5b, 0x56, 0x57, 0x5e, 0x5f, 0x7e, 0x7f, 0x7a, 0x7b, 0x5e, 0x5f, 0x7e, 0x7f
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0x7e, 0x7f, 0x7a, 0x7b, 0x76, 0x77, 0x7e, 0x7f, 0xfe, 0xff, 0xfa, 0xfb, 0x7e, 0x7f, 0xfe, 0xff
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0xde, 0xdf, 0xda, 0xdb, 0xd6, 0xd7, 0xde, 0xdf, 0xfe, 0xff, 0xfa, 0xfb, 0xde, 0xdf, 0xfe, 0xff
    , 0xbe, 0xbf, 0xba, 0xbb, 0xb6, 0xb7, 0xbe, 0xbf, 0xae, 0xaf, 0xaa, 0xab, 0xbe, 0xbf, 0xae, 0xaf
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xbe, 0xbf, 0xba, 0xbb, 0xfe, 0xff, 0xbe, 0xbf
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0xde, 0xdf, 0xda, 0xdb, 0xd6, 0xd7, 0xde, 0xdf, 0xfe, 0xff, 0xfa, 0xfb, 0xde, 0xdf, 0xfe, 0xff
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xee, 0xef, 0xea, 0xeb, 0xfe, 0xff, 0xee, 0xef
    , 0xfe, 0xff, 0xfa, 0xfb, 0xf6, 0xf7, 0xfe, 0xff, 0xfe, 0xff, 0xfa, 0xfb, 0xfe, 0xff, 0xfe, 0xff
    }
  };

// Summarise the input buffer into masks that indicate characters that have bearing on how the
// JSON fragment is structured.  Of these only ":,{}[]" are candidates to be interesting-bits.
// However, not every such occurence is an interesting-bit because they may be inside a string
// literal.  What makes this more complicated is that the string literal may itself contain
// escaped characters.
// This function generates the masks for the characters of interest so that a later stage may
// using them to determine the interesting-bits.
void hw_json_simd_summarise(
    uint8_t *buffer,          // Input buffer of 32 bytes containing a JSON fragment
    uint32_t *out_mask_d,     // Output buffer for receiving the mask for delimiter characters: ':,'
    uint32_t *out_mask_a,     // Output buffer for receiving the mask of the opening character: '{['
    uint32_t *out_mask_z,     // Output buffer for receiving the mask of the closing character: ']}'
    uint32_t *out_mask_q,     // Output buffer for receiving the mask of the quote character: '"'
    uint32_t *out_mask_b) {   // Output buffer for receiving the mask of the backslash character: '\'
#ifdef __AVX2__
  __m256i v_in_data = *(__m256i *)buffer;
  __m256i v_bytes_of_comma      = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8(','));
  __m256i v_bytes_of_colon      = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8(':'));
  __m256i v_bytes_of_brace_a    = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('{'));
  __m256i v_bytes_of_brace_z    = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('}'));
  __m256i v_bytes_of_bracket_a  = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('['));
  __m256i v_bytes_of_bracket_z  = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8(']'));
  __m256i v_bytes_of_quote      = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('"'));
  __m256i v_bytes_of_backslash  = _mm256_cmpeq_epi8(v_in_data, _mm256_set1_epi8('\\'));

  uint32_t mask_comma     = (uint32_t)_mm256_movemask_epi8(v_bytes_of_comma     );
  uint32_t mask_colon     = (uint32_t)_mm256_movemask_epi8(v_bytes_of_colon     );
  uint32_t mask_brace_a   = (uint32_t)_mm256_movemask_epi8(v_bytes_of_brace_a   );
  uint32_t mask_brace_z   = (uint32_t)_mm256_movemask_epi8(v_bytes_of_brace_z   );
  uint32_t mask_bracket_a = (uint32_t)_mm256_movemask_epi8(v_bytes_of_bracket_a );
  uint32_t mask_bracket_z = (uint32_t)_mm256_movemask_epi8(v_bytes_of_bracket_z );

  *out_mask_d = mask_comma    | mask_colon;
  *out_mask_a = mask_brace_a  | mask_bracket_a;
  *out_mask_z = mask_brace_z  | mask_bracket_z;
  *out_mask_q = (uint32_t)_mm256_movemask_epi8(v_bytes_of_quote    );
  *out_mask_b = (uint32_t)_mm256_movemask_epi8(v_bytes_of_backslash);
#elif defined __SSE4_2__
  __m128i v_in_data_0 = *((__m128i *)buffer    );
  __m128i v_in_data_1 = *((__m128i *)buffer + 1);
  uint16_t *out_w32_mask_d = (uint16_t *)out_mask_d;
  uint16_t *out_w32_mask_a = (uint16_t *)out_mask_a;
  uint16_t *out_w32_mask_z = (uint16_t *)out_mask_z;
  uint16_t *out_w32_mask_q = (uint16_t *)out_mask_q;
  uint16_t *out_w32_mask_b = (uint16_t *)out_mask_b;
  out_w32_mask_d[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)":,", 2, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_d[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)":,", 2, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_a[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"{[", 2, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_a[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"{[", 2, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_z[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"]}", 2, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_z[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"]}", 2, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_q[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"\"", 1, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_q[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"\"", 1, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_b[0] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"\\", 1, v_in_data_0, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
  out_w32_mask_b[1] = _mm_extract_epi16(_mm_cmpestrm(*(__m128i*)"\\", 1, v_in_data_1, 16, _SIDD_CMP_EQUAL_ANY | _SIDD_BIT_MASK), 0);
#else
#error "Require -mavx2 or -msse42 flags to be defined"
#endif
}

// Add two words 'a' and 'b' and a carry bit 'c' together.
// Detect this three-way addition overflow and set the carry bit accordingly in 'c'.
// A utility function that can be used to add two arbitrary bit strings of the same length together.
// The function itself doesn't peform the entire addition, but only word-wise in a way that propagates
// the carry.
uint64_t hw_json_simd_bitwise_add(uint64_t a, uint64_t b, uint64_t *c) {
  uint64_t d = a + b + *c;

  *c = (d <= a) & 1;

  return d;
}

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
    uint8_t *result_ib,
    uint8_t *result_a,
    uint8_t *result_z) {
  size_t m256_in_len = in_length / 32;
  size_t w64_out_len = in_length / 64;
  size_t w8_out_len  = in_length / 8;

  uint8_t  *w8_bits_of_b  = (uint8_t  *)work_bits_of_b;

  uint32_t *w32_bits_of_d = (uint32_t *)work_bits_of_d;
  uint32_t *w32_bits_of_a = (uint32_t *)work_bits_of_a;
  uint32_t *w32_bits_of_z = (uint32_t *)work_bits_of_z;
  uint32_t *w32_bits_of_q = (uint32_t *)work_bits_of_q;
  uint32_t *w32_bits_of_b = (uint32_t *)work_bits_of_b;

  uint64_t *w64_bits_of_d = (uint64_t *)work_bits_of_d;
  uint64_t *w64_bits_of_a = (uint64_t *)work_bits_of_a;
  uint64_t *w64_bits_of_z = (uint64_t *)work_bits_of_z;
  uint64_t *w64_bits_of_q = (uint64_t *)work_bits_of_q;
  uint64_t *w64_bits_of_e = (uint64_t *)work_bits_of_e;

  uint64_t *w64_result_ib = (uint64_t *)result_ib;
  uint64_t *w64_result_a  = (uint64_t *)result_a;
  uint64_t *w64_result_z  = (uint64_t *)result_z;

  uint64_t accum = 0;

  for (size_t i = 0; i < m256_in_len; ++i) {
    hw_json_simd_summarise(in_buffer + (i * 32),
      w32_bits_of_d + i,  // Mask of delimiter characters ':,'
      w32_bits_of_a + i,  // Mask of opening characters '{['
      w32_bits_of_z + i,  // Mask of closing characters ']}'
      w32_bits_of_q + i,  // Mask of quote characters '"'
      w32_bits_of_b + i); // Mask of backslash characters '\'
  }

  // Generate an escape mask that can tell us which quote characters are escaped
  // and which are not.
  for (size_t i = 0; i < w8_out_len; ++i) {
    char w8 = w8_bits_of_b[i];
    size_t j = (*last_trailing_ones) % 2;
    size_t k = (size_t)(uint8_t)w8;
    char w8e = hw_json_simd_escape_mask[j][k];
    work_bits_of_e[i] = w8e;
    *last_trailing_ones = _lzcnt_u64(~(int64_t)w8);
  }

  for (size_t i = 0; i < w64_out_len; ++i) {
    // Use the escape mask to remove the escaped quote characters from the quote mask
    w64_bits_of_q[i] = w64_bits_of_e[i] & w64_bits_of_q[i];

    uint64_t w64_bits_of_q_word = w64_bits_of_q[i];

    // In the following, the bitmask 0x5555555555555555 has all the odd bits selected.  For example:
    // 0101010101010101010101010101010101010101010101010101010101010101.
    // Figure out which quote characters are open (qas) and which are closing quotes (qaz).
    // Whether or not the quote is open or closed depends on if there has been unpaired
    // quote carrying over from the previous fragment.
    uint64_t qas = _pdep_u64(0x5555555555555555 << ((*quote_odds_carry ) & 1), w64_bits_of_q_word); // All the open quotes
    uint64_t qzs = _pdep_u64(0x5555555555555555 << ((*quote_evens_carry) & 1), w64_bits_of_q_word); // All the closed quotes

    // The quote mask tells us which characters are eligible to be interesting-bits (ie the ones not
    // inside a string literal).

    // Example:
    //    input:              *"__"*""**"__""_ <- every character that is '*' is unquoted and _ is quoted
    //    quote_mask_carry:   0
    //    w64_bits_of_q_word: 0100101100100110
    //    qas:                0100001000100010
    //    qaz:                0000100100000100
    //    ~qaz:               1111011011111011
    //    quote_mask:         1000110111000100
    //    input:              *"__"*""**"__""_ <- every character that is '*' is unquoted and _ is quoted
    // In the above quote_mask and input, every * (unquoted) matches a 1 and every _ (quoted) matches a 0.
    // We don't care about the rest of the bits because they correspond to quotes which cannot be
    // interesting-bits.
    uint64_t quote_mask = hw_json_simd_bitwise_add(qas, ~qzs, quote_mask_carry);

    // Only characters outside of string literals a eligible to be interesting-bits so the
    // others are masked out.
    uint64_t w64_d = quote_mask & w64_bits_of_d[i];
    uint64_t w64_a = quote_mask & w64_bits_of_a[i];
    uint64_t w64_z = quote_mask & w64_bits_of_z[i];

    w64_result_ib[i]  = w64_d | w64_a | w64_z;  // Interesting bits
    w64_result_a[i]   = w64_a;                  // Opening characters
    w64_result_z[i]   = w64_z;                  // Closing characters

    size_t pc = __builtin_popcountll(w64_bits_of_q[i]);
    *quote_odds_carry  += pc;
    *quote_evens_carry += pc;
  }

  return accum;
}
