# Changelog for hw-json-simd

## 0.1.2.0

- Add support for aarch64 (AWS Graviton, Apple Silicon). A self-contained NEON /
  scalar compatibility shim (`cbits/neon_shim.h`) implements the x86 SIMD
  intrinsics (AVX2 / SSSE3 / BMI2) used by the C kernels, so the library builds
  and runs on aarch64. NEON is baseline on ARMv8-A, so no runtime feature
  detection is required.

## Unreleased changes
