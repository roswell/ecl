/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/* This is a header implementation of xxhash family of algorithms:
   https://www.ietf.org/archive/id/draft-josefsson-xxhash-00.html

   It is provided to improve performance of table-based dispatch of generic:

   1. faster hash computation: it performs less computations than newhash and
      may be parallelized using sse.

   2. computing a perfect hash function - to avoid collissions we will compute a
      perfect hash function on cache miss (slow path). The algorithm requires
      from us to be able to parametrize the hash function.

   Our implementation, unlike the one linked in the article, does not operate on
   bytes -- we always operate on lane-sized integers (i.e 32bit or 64bit).

    -- jd 2026-06-05 */

/* TODO test this and jenkins with https://github.com/aappleby/smhasher */
/* TODO see whether we could use this new hash function for hash tables */

#include <stdint.h>

/* XXH32 */

static const uint32_t PRIME32_1 = 0x9E3779B1U;
static const uint32_t PRIME32_2 = 0x85EBCA77U;
static const uint32_t PRIME32_3 = 0xC2B2AE3DU;
static const uint32_t PRIME32_4 = 0x27D4EB2FU;
static const uint32_t PRIME32_5 = 0x165667B1U;

#define ROL32(x,s) ((x << s) | (x >> (32 - s)))

static uint32_t
xxh32(uint32_t *vector, uint32_t length, uint32_t seed)
{
  uint32_t acc, rem, idx, lane;
  /* Step 1. Initialize internal accumulators */
  uint32_t acc1 = seed + PRIME32_1 + PRIME32_2;
  uint32_t acc2 = seed + PRIME32_2;
  uint32_t acc3 = seed + 0;
  uint32_t acc4 = seed + PRIME32_1;
  /* Step 2. Process stripes */
  for(rem=length, idx=0; rem>=4; rem-=4, idx+=4) {
    acc1 = acc1 + vector[idx+0] * PRIME32_2;
    acc1 = ROL32(acc1, 13);
    acc1 = acc1 * PRIME32_1;

    acc2 = acc2 + vector[idx+1] * PRIME32_2;
    acc2 = ROL32(acc2,13);
    acc2 = acc2 * PRIME32_1;

    acc3 = acc3 + vector[idx+2] * PRIME32_2;
    acc3 = ROL32(acc3,13);
    acc3 = acc3 * PRIME32_1;

    acc4 = acc4 + vector[idx+3] * PRIME32_2;
    acc4 = ROL32(acc4,13);
    acc4 = acc4 * PRIME32_1;
  }
  /* Step 3. Accumulator convergence */
  acc = ROL32(acc1,1) + ROL32(acc2,7) + ROL32(acc3,12) + ROL32(acc4,18);
  /* Step 4. Add input length */
  acc = acc + length;
  /* Step 5. Consume remaining input */
  /* There may be 0, 4, 8 or 12 remaining bytes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc + lane * PRIME32_5;
    acc = ROL32(acc, 11) * PRIME32_1;
  }
  /* Step 6. Final mix (avalanche) */
  acc = acc ^ (acc >> 15);
  acc = acc * PRIME32_2;
  acc = acc ^ (acc >> 13);
  acc = acc * PRIME32_3;
  acc = acc ^ (acc >> 16);

  /* Step 7. Output */
  return acc;
}
