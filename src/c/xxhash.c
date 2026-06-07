#include "xxhash.h"
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

/*
 * 64 bit version
 */
#define GOLDEN_RATIO 0x9e3779b97f4a7c13L
#define mix(a,b,c)                              \
  {                                             \
    a=a-b;  a=a-c;  a=a^(c>>43);                \
    b=b-c;  b=b-a;  b=b^(a<<9);                 \
    c=c-a;  c=c-b;  c=c^(b>>8);                 \
    a=a-b;  a=a-c;  a=a^(c>>38);                \
    b=b-c;  b=b-a;  b=b^(a<<23);                \
    c=c-a;  c=c-b;  c=c^(b>>5);                 \
    a=a-b;  a=a-c;  a=a^(c>>35);                \
    b=b-c;  b=b-a;  b=b^(a<<49);                \
    c=c-a;  c=c-b;  c=c^(b>>11);                \
    a=a-b;  a=a-c;  a=a^(c>>12);                \
    b=b-c;  b=b-a;  b=b^(a<<18);                \
    c=c-a;  c=c-b;  c=c^(b>>22);                \
  }


static uint64_t
old64(uint64_t *keys, uint64_t size, uint64_t seed)
{
  uint64_t c = size, n = size, a = GOLDEN_RATIO, b = GOLDEN_RATIO;
  for (; n >= 3; ) {
    c += keys[--n];
    b += keys[--n];
    a += keys[--n];
    mix(a, b, c);
  }
  switch (n) {
  case 2: b += keys[--n];
  case 1: a += keys[--n];
    mix(a,b,c);
  }
  return c;
}

#define TEST_LENGTH 8

int main () {
  uint32_t vec32[TEST_LENGTH];
  uint64_t vec64[TEST_LENGTH];
  volatile uint64_t result = 0;

  for(int i =0 ;i<TEST_LENGTH; i++) {
    vec32[i] = random();
    vec64[i] = random();
  }

  printf("%" PRIu32 ", %" PRIu64 ", %" PRIu64 "\n",
         xxh32(vec32, TEST_LENGTH, 0),
         xxh64(vec64, TEST_LENGTH, 0),
         old64(vec64, TEST_LENGTH, 0)
         );

  for(uint64_t i=0; i<100000000ul; i++) {
    result += old64(vec64, TEST_LENGTH, 0);
  }
  printf("result is %" PRIu64 "\n", result);
  return 0;
}
