#include "simd.h"

int hw_json_simd_avx2_enabled() {
#ifdef __AVX2__
  return 1;
#else
  return 0;
#endif
}

int hw_json_simd_bmi2_enabled() {
#ifdef __BMI2__
  return 1;
#else
  return 0;
#endif
}

int hw_json_simd_sse4_2_enabled() {
#ifdef __BMI2__
  return 1;
#else
  return 0;
#endif
}
