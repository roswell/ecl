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

#define xxh32_rol(x,s) ((x << s) | (x >> (32 - s)))

#define xxh32_cvg(acc1,acc2,acc3,acc4) \
  (xxh32_rol(acc1,1) + xxh32_rol(acc2,7) + xxh32_rol(acc3,12) + xxh32_rol(acc4,18))


static uint32_t
xxh32_round(uint32_t accN, uint32_t laneN)
{
  accN = accN + (laneN * PRIME32_2);
  accN = xxh32_rol(accN, 13);
  return accN * PRIME32_1;
}

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
    acc1 = xxh32_round(acc1, vector[idx+0]);
    acc2 = xxh32_round(acc2, vector[idx+1]);
    acc3 = xxh32_round(acc3, vector[idx+2]);
    acc4 = xxh32_round(acc4, vector[idx+3]);
  }
  /* Step 3. Accumulator convergence */
  acc = xxh32_cvg(acc1, acc2, acc3, acc4);
  /* Step 4. Add input length */
  acc = acc + length;
  /* Step 5. Consume remaining input */
  /* There may be 0, 4, 8 or 12 remaining bytes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc + lane * PRIME32_5;
    acc = xxh32_rol(acc, 11) * PRIME32_1;
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


/* XXH64 */

static const uint64_t PRIME64_1 = 0x9E3779B185EBCA87ULL;
static const uint64_t PRIME64_2 = 0xC2B2AE3D27D4EB4FULL;
static const uint64_t PRIME64_3 = 0x165667B19E3779F9ULL;
static const uint64_t PRIME64_4 = 0x85EBCA77C2B2AE63ULL;
static const uint64_t PRIME64_5 = 0x27D4EB2F165667C5ULL;

#define xxh64_rol(x,s) ((x << s) | (x >> (64 - s)))

#define xxh64_cvg(acc1,acc2,acc3,acc4) \
  (xxh64_rol(acc1,1) + xxh64_rol(acc2,7) + xxh64_rol(acc3,12) + xxh64_rol(acc4,18))

static uint64_t
xxh64_round(uint64_t accN, uint64_t laneN)
{
  accN = accN + (laneN * PRIME64_2);
  accN = xxh64_rol(accN, 31);
  return accN * PRIME64_1;
}

static uint64_t
xxh64_merge(uint64_t acc, uint64_t accN)
{
  acc = acc ^ xxh64_round(0, accN);
  acc = acc * PRIME64_1;
  return acc + PRIME64_4;
}

static uint64_t
xxh64(uint64_t *vector, uint64_t length, uint64_t seed)
{
  uint64_t acc, rem, idx, lane;
  /* Step 1. Initialize internal accumulators */
  uint64_t acc1 = seed + PRIME64_1 + PRIME64_2;
  uint64_t acc2 = seed + PRIME64_2;
  uint64_t acc3 = seed + 0;
  uint64_t acc4 = seed + PRIME64_1;
  /* Step 2. Process stripes */
  for(rem=length, idx=0; rem>=4; rem-=4, idx+=4) {
    acc1 = xxh64_round(acc1, vector[idx+0]);
    acc2 = xxh64_round(acc2, vector[idx+1]);
    acc3 = xxh64_round(acc3, vector[idx+2]);
    acc4 = xxh64_round(acc4, vector[idx+3]);
  }
  /* Step 3. Accumulator convergence */
  acc = xxh64_cvg(acc1,acc2,acc3,acc4);
  acc = xxh64_merge(acc, acc1);
  acc = xxh64_merge(acc, acc2);
  acc = xxh64_merge(acc, acc3);
  acc = xxh64_merge(acc, acc4);
  /* Step 4. Add input length */
  acc = acc + length;
  /* Step 5. Consume remaining input */
  /* There may be 0, 4, 8 or 12 remaining bytes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc + xxh64_round(0, lane);
    acc = xxh64_rol(acc, 27) * PRIME64_1;
  }
  /* Step 6. Final mix (avalanche) */
  acc = acc ^ (acc >> 33);
  acc = acc * PRIME64_2;
  acc = acc ^ (acc >> 29);
  acc = acc * PRIME64_3;
  acc = acc ^ (acc >> 32);

  /* Step 7. Output */
  return acc;
}
