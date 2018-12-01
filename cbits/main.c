#include "simd.h"

#include <stdio.h>
#include <string.h>
#include <immintrin.h>

int tt_main(
    int argc,
    char **argv);

int main(
    int argc,
    char **argv) {
  if (argc <= 1) {
    fprintf(stderr, "Require command\n");
    exit(1);
  }
  
  if (strcmp(argv[1], "sp") == 0) {
    main_spliced(argc - 1, argv + 1);
  } else if (strcmp(argv[1], "sm") == 0) {
    sm_main(argc - 1, argv + 1);
  } else if (strcmp(argv[1], "tt") == 0) {
    tt_main(argc - 1, argv + 1);
  } else {
    fprintf(stderr, "Unrecognised command: %s\n", argv[1]);
    exit(1);
  }

  return 0;
}

int tt_main(
    int argc,
    char **argv) {
  char buffer[1024] = "bbbb bb  bb  bb";
  char operand[1024] = "bb";

  __m128i a = *(__m128i *)operand;
  __m128i b = *(__m128i *)buffer;
  __m128i x = _mm_cmpestrm(a, 2, b, 13, _SIDD_CMP_EQUAL_ORDERED | _SIDD_BIT_MASK);

  print128_num(x); printf("\n");
  printf("done\n");

  return 0;
}
