#include "xxhash.h"
#include <stdio.h>

int main () {
  uint32_t vec[] = {1, 2, 3, 5};
  printf("%u\n", xxh32(vec, 4, 0));
  return 0;
}
