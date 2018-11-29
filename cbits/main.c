#include "simd.h"

#include <stdio.h>
#include <string.h>

int main(
    int argc,
    char **argv) {
  if (argc <= 2) {
    fprintf(stderr, "Require command\n");
    exit(1);
  }
  
  if (strcmp(argv[1], "sp") == 0) {
    main_spliced(argc - 1, argv + 1);
  } else if (strcmp(argv[1], "sm") == 0) {
    sm_main(argc - 1, argv + 1);
  } else {
    fprintf(stderr, "Unrecognised command: %s\n", argv[1]);
    exit(1);
  }

  return 0;
}
