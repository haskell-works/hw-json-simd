#ifndef intrinsics_h___
#define intrinsics_h___

#if defined(__x86_64__) || defined(__i386__)

#include <immintrin.h>
#include <mmintrin.h>

#elif defined(__aarch64__)

// On aarch64 (AWS Graviton, Apple Silicon) the x86 intrinsic headers do not
// exist; provide bit-exact NEON / scalar equivalents instead.
#include "neon_shim.h"

#else

#error "hw-json-simd: unsupported target architecture (need x86 or aarch64)"

#endif

#endif//intrinsics_h___
