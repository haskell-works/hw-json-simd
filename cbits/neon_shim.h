#ifndef hw_json_simd_neon_shim_h___
#define hw_json_simd_neon_shim_h___

// -----------------------------------------------------------------------------
// NEON / scalar implementations of the x86 SIMD intrinsics (AVX2 / SSSE3 /
// BMI2) that the hw-json-simd C kernels call, for aarch64 targets (AWS
// Graviton, Apple Silicon).  Included from intrinsics.h on aarch64.
//
//   * Vector ops map onto NEON (Advanced SIMD), which is baseline on every
//     ARMv8-A core, so no runtime feature detection is required.
//   * The BMI2 bit-gather ops (_pext / _pdep) and _lzcnt have no NEON
//     counterpart and are implemented as portable scalar code.
//
// Covers the subset of intrinsics referenced by the compiled sources.
// -----------------------------------------------------------------------------

#include <arm_neon.h>
#include <stdint.h>

// Selects the NEON vector branch of hw_json_simd_summarise in simd-spliced.c.
#define HW_JSON_SIMD_NEON 1

// ----- 128-bit vector type ---------------------------------------------------

typedef uint8x16_t __m128i;

static inline __m128i _mm_set_epi64x(int64_t e1, int64_t e0) {
  int64_t tmp[2] = { e0, e1 };
  return vreinterpretq_u8_s64(vld1q_s64(tmp));
}

static inline __m128i _mm_set1_epi32(int32_t a) {
  return vreinterpretq_u8_s32(vdupq_n_s32(a));
}

// PSHUFB: for each byte, if the control byte's high bit is set the result is 0,
// otherwise the low 4 bits select a source byte.  Masking the index with 0x8F
// reproduces this exactly on vqtbl1q_u8 (which yields 0 for indices >= 16).
static inline __m128i _mm_shuffle_epi8(__m128i a, __m128i b) {
  uint8x16_t idx = vandq_u8(b, vdupq_n_u8(0x8F));
  return vqtbl1q_u8(a, idx);
}

#define _mm_extract_epi32(a, imm) \
  ((int32_t)vgetq_lane_u32(vreinterpretq_u32_u8(a), (imm)))

// ----- 256-bit vector type (emulated as two 128-bit halves) ------------------

typedef struct { uint8x16_t lo; uint8x16_t hi; } __m256i;

static inline __m256i _mm256_set1_epi8(int8_t a) {
  __m256i r;
  r.lo = vdupq_n_u8((uint8_t)a);
  r.hi = r.lo;
  return r;
}

static inline __m256i _mm256_cmpeq_epi8(__m256i a, __m256i b) {
  __m256i r;
  r.lo = vceqq_u8(a.lo, b.lo);
  r.hi = vceqq_u8(a.hi, b.hi);
  return r;
}

// movemask: gather the most-significant bit of each byte into an integer.
// Works for arbitrary byte values (not only 0x00/0xFF compare results): shift
// each byte down to its MSB, weight by lane position, then sum per 8-lane half.
static inline int hw_json_simd_neon_movemask_u8x16(uint8x16_t v) {
  const uint8x16_t weights = { 1, 2, 4, 8, 16, 32, 64, 128,
                               1, 2, 4, 8, 16, 32, 64, 128 };
  uint8x16_t msb = vshrq_n_u8(v, 7);          // each lane -> 0 or 1 (its MSB)
  uint8x16_t w   = vmulq_u8(msb, weights);    // lane i -> msb_i << (i % 8)
  int lo = vaddv_u8(vget_low_u8(w));          // -> bits 0..7
  int hi = vaddv_u8(vget_high_u8(w));         // -> bits 8..15
  return lo | (hi << 8);
}

static inline int _mm256_movemask_epi8(__m256i a) {
  return hw_json_simd_neon_movemask_u8x16(a.lo)
       | (hw_json_simd_neon_movemask_u8x16(a.hi) << 16);
}

// Logical 64-bit-lane shifts.  The shift count is a runtime value here, so use
// the variable-shift vshlq_u64 (a negative count performs a logical right
// shift on the unsigned reinterpretation).
static inline __m256i _mm256_slli_epi64(__m256i a, int count) {
  int64x2_t c = vdupq_n_s64((int64_t)count);
  __m256i r;
  r.lo = vreinterpretq_u8_u64(vshlq_u64(vreinterpretq_u64_u8(a.lo), c));
  r.hi = vreinterpretq_u8_u64(vshlq_u64(vreinterpretq_u64_u8(a.hi), c));
  return r;
}

static inline __m256i _mm256_srli_epi64(__m256i a, int count) {
  int64x2_t c = vdupq_n_s64(-(int64_t)count);
  __m256i r;
  r.lo = vreinterpretq_u8_u64(vshlq_u64(vreinterpretq_u64_u8(a.lo), c));
  r.hi = vreinterpretq_u8_u64(vshlq_u64(vreinterpretq_u64_u8(a.hi), c));
  return r;
}

// ----- BMI2 scalar equivalents (no NEON counterpart) -------------------------

// Parallel bit extract: gather the bits of `val` selected by `mask` into the
// low-order bits of the result, in mask order.
static inline uint64_t _pext_u64(uint64_t val, uint64_t mask) {
  uint64_t res = 0;
  uint64_t bb  = 1;
  while (mask) {
    uint64_t lsb = mask & (uint64_t)(-(int64_t)mask);   // lowest set bit
    if (val & lsb) res |= bb;
    mask &= mask - 1;
    bb <<= 1;
  }
  return res;
}

static inline uint32_t _pext_u32(uint32_t val, uint32_t mask) {
  uint32_t res = 0;
  uint32_t bb  = 1;
  while (mask) {
    uint32_t lsb = mask & (uint32_t)(-(int32_t)mask);
    if (val & lsb) res |= bb;
    mask &= mask - 1;
    bb <<= 1;
  }
  return res;
}

// Parallel bit deposit: scatter the low-order bits of `val` into the positions
// selected by `mask`.
static inline uint64_t _pdep_u64(uint64_t val, uint64_t mask) {
  uint64_t res = 0;
  uint64_t bb  = 1;
  while (mask) {
    uint64_t lsb = mask & (uint64_t)(-(int64_t)mask);
    if (val & bb) res |= lsb;
    mask &= mask - 1;
    bb <<= 1;
  }
  return res;
}

// Count leading zeros of a 64-bit value; matches _lzcnt_u64(0) == 64.
static inline uint64_t _lzcnt_u64(uint64_t x) {
  return x ? (uint64_t)__builtin_clzll(x) : 64;
}

#endif // hw_json_simd_neon_shim_h___
