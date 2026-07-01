#include "simd.h"

// These report whether the SIMD indexing kernels are available for the build
// target. On aarch64 the kernels run on the NEON / scalar path (cbits/neon_shim.h),
// so all three report available.

int hw_json_simd_avx2_enabled() {
#if defined(__AVX2__) || defined(__aarch64__)
  return 1;
#else
  return 0;
#endif
}

int hw_json_simd_bmi2_enabled() {
#if defined(__BMI2__) || defined(__aarch64__)
  return 1;
#else
  return 0;
#endif
}

int hw_json_simd_sse4_2_enabled() {
#if defined(__BMI2__) || defined(__aarch64__)
  return 1;
#else
  return 0;
#endif
}
