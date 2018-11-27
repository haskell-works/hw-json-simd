#include <stdio.h>
#include <immintrin.h>
#include <mmintrin.h>

void print256_num(__m256i var);

void print128_num(__m128i var);

void fprint256_num(FILE *file, __m256i var);

void fprint128_num(FILE *file, __m128i var);

void print_bits_8(uint8_t v);

void print_bits_16(uint16_t v);

void print_bits_32(uint32_t v);

void print_bits_64(uint64_t v);

void print_bits_128(__m128i v);

void print_bits_256(__m256i v);
