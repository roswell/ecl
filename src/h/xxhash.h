/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/* This is a header implementation of xxhash family of algorithms:
   https://www.ietf.org/archive/id/draft-josefsson-xxhash-00.html

   It is provided to improve the performance of generic function dispatch and
   hash tables. The hash computation is faster than newhash with possibility to
   further optimize it using SSE.

   XXH32 and XXH64, unlike implementations in the linked article, operate on
   lane-sized integers instead of bytes. This is a deliberate choice to allow
   passing the vector of specializers to the hashing function (gf dispatch).

   XXH3 implementation on the other hand is faithful to the original algorithm
   in that it operates on octets. This is useful for hash tables where hashes
   are computed from objects of varying sizes. Moreover we implement streaming
   mode because it is necessary for hashing using for EQUAL and similar tests.

   Tradeoffs:

   Lane-based XXH32/XXH64 will not produce the same hashes for equal byte arrays
   in upstream algorithm when the host is big endian. They are competetive with
   upstream and we don't need to pay cost of converting to bytes.

   XXH3 is scalar (so it could be improved for the large variant). Our version
   is competetive with upstream when we disable SSE and ~2x slower for large
   inputs (> 240B). Our source code is around 10x smaller.

   We hardcode a default secret and seed, but the code to derive a custom one is
   implemented for customization.

   I've also cobbled together a small benchmark to see how we fare compared to
   FNV1A (SBCL currently uses this for strings) - for short strings <4B we are
   slower, 4B is a crossover and then we quickly pull ahead. The difference for
   1B is very small (around 2ns), so I wouldn't stress it, but generally short
   strings are not our forte. Also FNV1A implementation is trivial (but at a
   cost of worse hash quality and throughput).

   Compared to newhash (our old algorithm) XXH3 one-shot mode is always faster
   (1.7x at 1B and growing with input size) while the streaming version is
   slower at 1B, but quickly pulls ahead (crossover is also at 4B).

   Another tradeoff for XXH3 streaming is that our state is ~1.2KB to keep the
   code of update/digest straightforward, while upstream's state is half of it
   (while FNV1A and newhash are practically stateless). We allocate the state on
   the stack and that's not much of a problem (we could move it to cl_env too).

    -- jd 2026-06-05 */

#ifndef ECL_XXHASH_H
#define ECL_XXHASH_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

/* XXH32 */

#define PRIME32_1 (0x9E3779B1U)
#define PRIME32_2 (0x85EBCA77U)
#define PRIME32_3 (0xC2B2AE3DU)
#define PRIME32_4 (0x27D4EB2FU)
#define PRIME32_5 (0x165667B1U)

/* INV never called with s=0 */
#define xxh32_rol(x,s) ((x << s) | (x >> (32 - s)))

#define xxh32_cvg(acc1,acc2,acc3,acc4) \
  (xxh32_rol(acc1,1) + xxh32_rol(acc2,7) + xxh32_rol(acc3,12) + xxh32_rol(acc4,18))

static inline uint32_t
xxh32_avalanche(uint32_t x)
{
  x = x ^ (x >> 15);
  x = x * PRIME32_2;
  x = x ^ (x >> 13);
  x = x * PRIME32_3;
  x = x ^ (x >> 16);
  return x;
}

static inline uint32_t
xxh32_round(uint32_t accN, uint32_t laneN)
{
  accN = accN + (laneN * PRIME32_2);
  accN = xxh32_rol(accN, 13);
  return accN * PRIME32_1;
}

static inline uint32_t
xxh32_small(const uint32_t *vector, uint32_t length, uint32_t seed)
{
  uint32_t acc, rem, idx, lane;
  rem = length, idx = 0;
  /* Step 1. Initialize internal accumulator */
  acc = seed + PRIME32_5;
  /* Step 4. Add input length */
  acc = acc + length*4;
  /* Step 5. Consume remaining input */
  /* There may be 0, 1, 2 or 3 remaining lanes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc + lane * PRIME32_3;
    acc = xxh32_rol(acc, 17) * PRIME32_4;
  }
  /* Step 6. Final mix (avalanche) */
  acc = xxh32_avalanche(acc);
  /* Step 7. Output */
  return acc;
}

static inline uint32_t
xxh32(const uint32_t *vector, uint32_t length, uint32_t seed)
{
  uint32_t acc, rem, idx, lane;
  if (length<4)
    return xxh32_small(vector, length, seed);
  rem = length, idx = 0;
  /* Step 1. Initialize internal accumulators */
  uint32_t acc1 = seed + PRIME32_1 + PRIME32_2;
  uint32_t acc2 = seed + PRIME32_2;
  uint32_t acc3 = seed + 0;
  uint32_t acc4 = seed - PRIME32_1;
  /* Step 2. Process stripes */
  for(; rem>=4; rem-=4, idx+=4) {
    acc1 = xxh32_round(acc1, vector[idx+0]);
    acc2 = xxh32_round(acc2, vector[idx+1]);
    acc3 = xxh32_round(acc3, vector[idx+2]);
    acc4 = xxh32_round(acc4, vector[idx+3]);
  }
  /* Step 3. Accumulator convergence */
  acc = xxh32_cvg(acc1, acc2, acc3, acc4);
  /* Step 4. Add input length */
  acc = acc + length*4;
  /* Step 5. Consume remaining input */
  /* There may be 0, 1, 2 or 3 remaining lanes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc + lane * PRIME32_3;
    acc = xxh32_rol(acc, 17) * PRIME32_4;
  }
  /* Step 6. Final mix (avalanche) */
  acc = xxh32_avalanche(acc);
  /* Step 7. Output */
  return acc;
}


/* XXH64 */

#define PRIME64_1 (0x9E3779B185EBCA87ULL)
#define PRIME64_2 (0xC2B2AE3D27D4EB4FULL)
#define PRIME64_3 (0x165667B19E3779F9ULL)
#define PRIME64_4 (0x85EBCA77C2B2AE63ULL)
#define PRIME64_5 (0x27D4EB2F165667C5ULL)

/* INV never called with s=0 */
#define xxh64_rol(x,s) ((x << s) | (x >> (64 - s)))

#define xxh64_cvg(acc1,acc2,acc3,acc4) \
  (xxh64_rol(acc1,1) + xxh64_rol(acc2,7) + xxh64_rol(acc3,12) + xxh64_rol(acc4,18))

static inline uint64_t
xxh64_avalanche(uint64_t x)
{
  x = x ^ (x >> 33);
  x = x * PRIME64_2;
  x = x ^ (x >> 29);
  x = x * PRIME64_3;
  x = x ^ (x >> 32);
  return x;
}

static inline uint64_t
xxh64_round(uint64_t accN, uint64_t laneN)
{
  accN = accN + (laneN * PRIME64_2);
  accN = xxh64_rol(accN, 31);
  return accN * PRIME64_1;
}

static inline uint64_t
xxh64_merge(uint64_t acc, uint64_t accN)
{
  acc = acc ^ xxh64_round(0, accN);
  acc = acc * PRIME64_1;
  return acc + PRIME64_4;
}

static inline uint64_t
xxh64_small(const uint64_t *vector, uint64_t length, uint64_t seed)
{
  uint64_t acc, rem, idx, lane;
  rem = length, idx = 0;
  /* Step 1. Initialize internal accumulator */
  acc = seed + PRIME64_5;
  /* Step 4. Add input length */
  acc = acc + length*8;
  /* Step 5. Consume remaining input */
  /* There may be 0, 1, 2 or 3 remaining lanes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc ^ xxh64_round(0, lane);
    acc = xxh64_rol(acc, 27) * PRIME64_1;
    acc = acc + PRIME64_4;
  }
  /* Step 6. Final mix (avalanche) */
  acc = xxh64_avalanche(acc);
  /* Step 7. Output */
  return acc;
}

static inline uint64_t
xxh64(const uint64_t *vector, uint64_t length, uint64_t seed)
{
  uint64_t acc, rem, idx, lane;
  if (length<4)
    return xxh64_small(vector, length, seed);
  rem = length, idx = 0;
  /* Step 1. Initialize internal accumulators */
  uint64_t acc1 = seed + PRIME64_1 + PRIME64_2;
  uint64_t acc2 = seed + PRIME64_2;
  uint64_t acc3 = seed + 0;
  uint64_t acc4 = seed - PRIME64_1;
  /* Step 2. Process stripes */
  for(; rem>=4; rem-=4, idx+=4) {
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
  acc = acc + length*8;
  /* Step 5. Consume remaining input */
  /* There may be 0, 1, 2 or 3 remaining lanes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc ^ xxh64_round(0, lane);
    acc = xxh64_rol(acc, 27) * PRIME64_1;
    acc = acc + PRIME64_4;
  }
  /* Step 6. Final mix (avalanche) */
  acc = xxh64_avalanche(acc);
  /* Step 7. Output */
  return acc;
}


/* XXH3-64 (we don't currently need XXH3-128) */

#define PRIME_MX1 (0x165667919E3779F9ULL)
#define PRIME_MX2 (0x9FB21C651E98DF25ULL)

static const uint8_t xxh3_default_secret[192] = {
  0xb8, 0xfe, 0x6c, 0x39, 0x23, 0xa4, 0x4b, 0xbe,
  0x7c, 0x01, 0x81, 0x2c, 0xf7, 0x21, 0xad, 0x1c,
  0xde, 0xd4, 0x6d, 0xe9, 0x83, 0x90, 0x97, 0xdb,
  0x72, 0x40, 0xa4, 0xa4, 0xb7, 0xb3, 0x67, 0x1f,
  0xcb, 0x79, 0xe6, 0x4e, 0xcc, 0xc0, 0xe5, 0x78,
  0x82, 0x5a, 0xd0, 0x7d, 0xcc, 0xff, 0x72, 0x21,
  0xb8, 0x08, 0x46, 0x74, 0xf7, 0x43, 0x24, 0x8e,
  0xe0, 0x35, 0x90, 0xe6, 0x81, 0x3a, 0x26, 0x4c,
  0x3c, 0x28, 0x52, 0xbb, 0x91, 0xc3, 0x00, 0xcb,
  0x88, 0xd0, 0x65, 0x8b, 0x1b, 0x53, 0x2e, 0xa3,
  0x71, 0x64, 0x48, 0x97, 0xa2, 0x0d, 0xf9, 0x4e,
  0x38, 0x19, 0xef, 0x46, 0xa9, 0xde, 0xac, 0xd8,
  0xa8, 0xfa, 0x76, 0x3f, 0xe3, 0x9c, 0x34, 0x3f,
  0xf9, 0xdc, 0xbb, 0xc7, 0xc7, 0x0b, 0x4f, 0x1d,
  0x8a, 0x51, 0xe0, 0x4b, 0xcd, 0xb4, 0x59, 0x31,
  0xc8, 0x9f, 0x7e, 0xc9, 0xd9, 0x78, 0x73, 0x64,
  0xea, 0xc5, 0xac, 0x83, 0x34, 0xd3, 0xeb, 0xc3,
  0xc5, 0x81, 0xa0, 0xff, 0xfa, 0x13, 0x63, 0xeb,
  0x17, 0x0d, 0xdd, 0x51, 0xb7, 0xf0, 0xda, 0x49,
  0xd3, 0x16, 0x55, 0x26, 0x29, 0xd4, 0x68, 0x9e,
  0x2b, 0x16, 0xbe, 0x58, 0x7d, 0x47, 0xa1, 0xfc,
  0x8f, 0xf8, 0xb8, 0xd1, 0x7a, 0xd0, 0x31, 0xce,
  0x45, 0xcb, 0x3a, 0x8f, 0x95, 0x16, 0x04, 0x28,
  0xaf, 0xd7, 0xfb, 0xca, 0xbb, 0x4b, 0x40, 0x7e,
};

static inline uint64_t
xxh3_mul128_fold64(uint64_t a, uint64_t b) {
  /* xxh3 has a few passages where we do
       u128 x = (u128)a * (u128)b;
       result = ... + (u64)lower(x) ^ (u64)higher(x);
     this function returns (u64)lower(x) ^ (u64)higher(x) */
#if defined(__SIZEOF_INT128__)
  /* Notably this should work on GCC and Clang.  */
  unsigned __int128 result = (unsigned __int128)a * b;
  return (uint64_t)result ^ (uint64_t)(result >> 64);
#else
  uint64_t a_lo = (uint32_t)a;
  uint64_t a_hi = a >> 32;
  uint64_t b_lo = (uint32_t)b;
  uint64_t b_hi = b >> 32;

  uint64_t x_lo = a_lo * b_lo;
  uint64_t x_hi = a_hi * b_hi;

  uint64_t tmp0;
  uint64_t tmp1 = a_lo * b_hi;
  uint64_t tmp2 = a_hi * b_lo;

  tmp0 = tmp1 << 32;
  x_lo += tmp0;
  x_hi += tmp1 >> 32; 
  if(tmp0 > x_lo)               /* overflow */
    x_hi++;

  tmp0 = tmp2 << 32;
  x_lo += tmp0;
  x_hi += tmp2 >> 32; 
  if(tmp0 > x_lo)               /* overflow */
    x_hi++;

  return x_lo ^ x_hi;
#endif
}

#if defined(__GNUC__) || defined(__clang__)
# define xxh3_bswap64(x) __builtin_bswap64(x)
# define xxh3_bswap32(x) __builtin_bswap32(x)
#else
static inline uint64_t
xxh3_bswap64(uint64_t x)
{
  return ((x & 0x00000000000000ff) << 56)
    |    ((x & 0x000000000000ff00) << 40)
    |    ((x & 0x0000000000ff0000) << 24)
    |    ((x & 0x00000000ff000000) <<  8)
    |    ((x & 0x000000ff00000000) >>  8)
    |    ((x & 0x0000ff0000000000) >> 24)
    |    ((x & 0x00ff000000000000) >> 40)
    |    ((x & 0xff00000000000000) >> 56);
}

static inline uint32_t
xxh3_bswap32(uint32_t x)
{
  return ((x & 0x000000ff) << 24)
    |    ((x & 0x0000ff00) <<  8)
    |    ((x & 0x00ff0000) >>  8)
    |    ((x & 0xff000000) >> 24);
}
#endif

static inline uint32_t
xxh3_read32(const uint8_t *src)
{
#ifndef WORDS_BIGENDIAN
  uint32_t val;
  memcpy(&val, src, 4);
  return val;
#else
  return ((uint32_t)src[0]      )
       | ((uint32_t)src[1] <<  8)
       | ((uint32_t)src[2] << 16)
       | ((uint32_t)src[3] << 24);
#endif
}

static inline uint64_t
xxh3_read64(const uint8_t *src)
{
#ifndef WORDS_BIGENDIAN
  uint64_t val;
  memcpy(&val, src, 8);
  return val;
#else
  return ((uint64_t)src[0])
       | ((uint64_t)src[1] <<  8)
       | ((uint64_t)src[2] << 16)
       | ((uint64_t)src[3] << 24)
       | ((uint64_t)src[4] << 32)
       | ((uint64_t)src[5] << 40)
       | ((uint64_t)src[6] << 48)
       | ((uint64_t)src[7] << 56);
#endif
}

static inline void
xxh3_write32(uint8_t *dst, uint32_t x)
{
#ifndef WORDS_BIGENDIAN
  memcpy(dst, &x, 4);
#else
  dst[0] = (uint8_t)(x);
  dst[1] = (uint8_t)(x >> 8);
  dst[2] = (uint8_t)(x >> 16);
  dst[3] = (uint8_t)(x >> 24);
#endif
}

static inline void
xxh3_write64(uint8_t *dst, uint64_t x)
{
#ifndef WORDS_BIGENDIAN
  memcpy(dst, &x, 8);
#else
  dst[0] = (uint8_t)(x);
  dst[1] = (uint8_t)(x >> 8);
  dst[2] = (uint8_t)(x >> 16);
  dst[3] = (uint8_t)(x >> 24);
  dst[4] = (uint8_t)(x >> 32);
  dst[5] = (uint8_t)(x >> 40);
  dst[6] = (uint8_t)(x >> 48);
  dst[7] = (uint8_t)(x >> 56);
#endif
}

static inline uint64_t
xxh3_avalanche(uint64_t x)
{
  x = x ^ (x >> 37);
  x = x * PRIME_MX1;
  x = x ^ (x >> 32);
  return x;
}

/* If the hashing security matters, then derive a different secret based on
   random seed and replace xxh3_seed and xxh3_secret. -- jd 2026-09-17 */
static inline void
xxh3_derive_secret(uint8_t buffer[192], const uint8_t secret[192], uint64_t seed)
{
  int i;
  for (i=0; i<12; i++) {
    uint64_t a = xxh3_read64(secret+i*16);
    uint64_t b = xxh3_read64(secret+i*16+8);
    xxh3_write64(buffer+i*16, a+seed);
    xxh3_write64(buffer+i*16+8, b-seed);
  }
}

#define xxh3_seed 0
#define xxh3_secret xxh3_default_secret

/* Small input */

static inline uint64_t
xxh3_64_empty(const uint8_t *input, const size_t length)
{
  uint64_t secret0, secret1;
  secret0 = xxh3_read64(xxh3_secret+56);
  secret1 = xxh3_read64(xxh3_secret+56+8);
  return xxh64_avalanche(xxh3_seed ^ secret0 ^ secret1);
}

static inline uint64_t
xxh3_64_1to3(const uint8_t *input, const size_t length)
{
  uint32_t secret0, secret1, combined;
  uint64_t value;
  secret0 = xxh3_read32(xxh3_secret);
  secret1 = xxh3_read32(xxh3_secret+4);
  combined =
    ((uint32_t)input[length-1]) | ((uint32_t)length << 8) |
    ((uint32_t)input[0] << 16)  | ((uint32_t)input[length>>1] << 24);
  value = ((uint64_t)(secret0 ^ secret1) + xxh3_seed) ^ (uint64_t)combined;
  return xxh64_avalanche(value);
}

static inline uint64_t
xxh3_64_4to8(const uint8_t *input, const size_t length)
{
  uint32_t input_first, input_last;
  uint64_t modified_seed, secret0, secret1, combined, value;
  input_first = xxh3_read32(input);
  input_last = xxh3_read32(input+length-4);
  modified_seed = xxh3_seed ^
                  ((uint64_t)xxh3_bswap32((uint32_t)xxh3_seed) << 32);
  secret0 = xxh3_read64(xxh3_secret+8);
  secret1 = xxh3_read64(xxh3_secret+8+8);
  combined = (uint64_t)input_last | ((uint64_t)input_first << 32);
  value = ((secret0 ^ secret1) - modified_seed) ^ combined;
  value = value ^ xxh64_rol(value, 49) ^ xxh64_rol(value, 24);
  value = value * PRIME_MX2;
  value = value ^ ((value >> 35) + length);
  value = value * PRIME_MX2;
  value = value ^ (value >> 28);
  return value;
}

static inline uint64_t
xxh3_64_9to16(const uint8_t *input, const size_t length)
{
  uint64_t input_first, input_last,
    secret0, secret1, secret2, secret3,
    low, high, fold, value;
  input_first = xxh3_read64(input);
  input_last = xxh3_read64(input+length-8);
  secret0 = xxh3_read64(xxh3_secret+24);
  secret1 = xxh3_read64(xxh3_secret+24+8);
  secret2 = xxh3_read64(xxh3_secret+24+16);
  secret3 = xxh3_read64(xxh3_secret+24+24);
  low = ((secret0 ^ secret1) + xxh3_seed) ^ input_first;
  high= ((secret2 ^ secret3) - xxh3_seed) ^ input_last;
  fold= xxh3_mul128_fold64(low, high);
  value = length + xxh3_bswap64(low) + high + fold;
  return xxh3_avalanche(value);
}

/* Medium input */

static inline uint64_t
xxh3_64_medium_mix(const uint8_t *input, const uint8_t *secret)
{
  uint64_t input0, input1, secret0, secret1;
  input0 = xxh3_read64(input);
  input1 = xxh3_read64(input+8);
  secret0 = xxh3_read64(secret);
  secret1 = xxh3_read64(secret+8);
  return xxh3_mul128_fold64(input0 ^ (secret0 + xxh3_seed),
                            input1 ^ (secret1 - xxh3_seed));
}

#define xxh3_64_medium_mix2(i) do {                                     \
  acc += xxh3_64_medium_mix(input+i*16,           xxh3_secret+i*32);    \
  acc += xxh3_64_medium_mix(input+length-i*16-16, xxh3_secret+i*32+16); \
  } while(0)

static inline uint64_t
xxh3_64_17to128(const uint8_t *input, const size_t length)
{
  uint64_t acc;
  acc = length * PRIME64_1;
  if (length <= 32) {
    xxh3_64_medium_mix2(0);
  } else if (length <= 64) {
    xxh3_64_medium_mix2(1);
    xxh3_64_medium_mix2(0);
  } else if (length <= 96) {
    xxh3_64_medium_mix2(2);
    xxh3_64_medium_mix2(1);
    xxh3_64_medium_mix2(0);
  } else /*if (length <= 128)*/ {
    xxh3_64_medium_mix2(3);
    xxh3_64_medium_mix2(2);
    xxh3_64_medium_mix2(1);
    xxh3_64_medium_mix2(0);
  }
  return xxh3_avalanche(acc);
}
#undef xxh3_64_medium_mix2

/* Deliberately not declared inline to allow compiler decide. */
static uint64_t
xxh3_64_129to240(const uint8_t *input, const size_t length)
{
  uint64_t acc, num_chunks, i;
  acc = length * PRIME64_1;
  num_chunks = length >> 4;
  for (i=0; i<8; i++) {
    acc += xxh3_64_medium_mix(input+i*16, xxh3_secret+i*16);
  }
  acc = xxh3_avalanche(acc);
  for (i=8; i<num_chunks; i++) {
    /* Unrolling this into a switch makes xxh3 slower (!) */
    acc += xxh3_64_medium_mix(input+i*16, xxh3_secret+(i-8)*16+3);
  }
  acc += xxh3_64_medium_mix(input+length-16, xxh3_secret+119);
  return xxh3_avalanche(acc);
}

/* Large input */
static inline void
xxh3_64_large_acc(const uint8_t *block, const uint8_t *secret, uint64_t acc[8])
{
  uint64_t stripe, secret_var, value;
  int i;
  for (i=0; i<8; i++) {
    stripe = xxh3_read64(block+i*8);
    secret_var = xxh3_read64(secret+i*8);
    value = stripe ^ secret_var;
    acc[i^1] = acc[i^1]+stripe;
    acc[i] += (value & 0xffffffff) * (value>>32);
  }
}

static inline void
xxh3_64_large_mix(uint64_t acc[8])
{
  uint64_t secret_var,i ;
  for (i=0; i<8; i++) {
    secret_var = xxh3_read64(xxh3_secret+192-64+i*8);
    acc[i] = acc[i] ^ (acc[i] >> 47);
    acc[i] = acc[i] ^ secret_var;
    acc[i] = acc[i] * PRIME32_1;
  }
}

/* Deliberately not declared inline to allow compiler decide. */
static uint64_t
xxh3_64_large(const uint8_t *input, const size_t length)
{
  /* The upsteram algorithm is paramerize, we assume:
     secret_length=192, stripes_per_block=16, block_size=1024 */
  uint64_t i, n, num, value, len;
  const uint8_t *block;
  /* Step 1. Initialize internal acumulators */
  uint64_t acc[8] = {
    PRIME32_3, PRIME64_1, PRIME64_2, PRIME64_3,
    PRIME64_4, PRIME32_2, PRIME64_5, PRIME32_1 };
  /* Step 2. Process blocks */
  len = length;
  block = input;
  while(len > 1024) {
    /* Step 2.1 Process stripes in the block */
    for (n=0; n<16; n++) {
      xxh3_64_large_acc(block+n*64, xxh3_secret+n*8, acc);
    }
    /* Step 2.2 Scramble accumulators */
    xxh3_64_large_mix(acc);
    /* Advance! */
    block += 1024;
    len -= 1024;
  }
  /* Step 3. Process the last block and the last 64 bytes */
  num = (len-1)/64;
  for (n=0; n<num; n++) {
    xxh3_64_large_acc(block+n*64, xxh3_secret+n*8, acc);
  }
  /* The last stripe is taken from the end and may overlap. */
  block = input + length - 64;
  xxh3_64_large_acc(block, xxh3_secret+192-71, acc);
  /* Step 4. Finalization */
  value = length*PRIME64_1;
  for (i=0; i<4; i++) {
    uint64_t a = acc[i*2]   ^ xxh3_read64(xxh3_secret+11+i*16);
    uint64_t b = acc[i*2+1] ^ xxh3_read64(xxh3_secret+11+i*16+8);
    value += xxh3_mul128_fold64(a, b);
  }
  return xxh3_avalanche(value);
}

static inline uint64_t
xxh3_64(const uint8_t *input, const size_t length)
{
  /* specialized variants */
  if (length == 0)   return xxh3_64_empty    (input, length);
  if (length <= 3)   return xxh3_64_1to3     (input, length);
  if (length <= 8)   return xxh3_64_4to8     (input, length);
  if (length <= 16)  return xxh3_64_9to16    (input, length);
  if (length <= 128) return xxh3_64_17to128  (input, length);
  if (length <= 240) return xxh3_64_129to240 (input, length);
  return xxh3_64_large(input, length);
}

/* Streaming variant */

/* The upstream algorithm is paramerize, we assume:
   secret_length=192, stripes_per_block=16, block_size=1024

   We could store the seend and the secret in this structure as well at a cost
   of runtime dereferences (right now they are available at compile time). */
typedef struct xxh3_state {
  size_t buf_size;              /* buffer fill pointer */
  uint64_t total_len;           /* total length */
  bool ready_acc;
  uint64_t acc[8];              /* accumulators */
  uint8_t rem[64];              /* last 64 bytes for digest */
  uint8_t buf[1024];            /* remaining data */
} xxh3_state;

static inline void
xxh3_reset(xxh3_state *state)
{
  state->buf_size=0;
  state->total_len=0;
  state->ready_acc=0;
}

static inline void
xxh3_ensure_acc(xxh3_state *state, uint64_t acc[8])
{
  if (state->ready_acc) {
    memcpy(acc, state->acc, 8*sizeof(uint64_t));
  } else {
    acc[0]=PRIME32_3;
    acc[1]=PRIME64_1;
    acc[2]=PRIME64_2;
    acc[3]=PRIME64_3;
    acc[4]=PRIME64_4;
    acc[5]=PRIME32_2;
    acc[6]=PRIME64_5;
    acc[7]=PRIME32_1;
  }
}

/* Keep the slow path separate to keep the fast path small for inlining. */
static void
xxh3_update_slow(xxh3_state *state, const uint8_t *input, const size_t length)
{
  /* INV length>0; bsize+length>1024 */
  uint8_t *buffer = state->buf;
  const uint8_t *block = input;
  size_t bsize = state->buf_size;
  size_t len = length;
  size_t delta;
  uint64_t acc[8];
  uint64_t idx;
  bool copy_rem = 1;
  bool copy_acc = 0;
  /* Fill the current block and use it for update. */
  if (bsize) {
    delta = 1024-bsize;
    memcpy(buffer+bsize, input, delta);
    /* Ensure accumulators */
    xxh3_ensure_acc(state, acc);
    copy_acc = 1;
    /* Step 2.1 Process stripes in the block */
    for (idx=0; idx<16; idx++) {
      xxh3_64_large_acc(buffer+idx*64, xxh3_secret+idx*8, acc);
    }
    /* Step 2.2 Scramble accumulators */
    xxh3_64_large_mix(acc);
    /* Advance! */
    block = input + delta;
    len -= delta;
    bsize = 0;
    if(len <= 1024) {
      memcpy(state->rem, buffer+1024-64, 64);
      copy_rem = 0;
    }
  }
  /* Continue with input without copying. */
  while(len > 1024) {
    if (!copy_acc) {
      xxh3_ensure_acc(state, acc);
      copy_acc = 1;
    }
    /* Step 2.1 Process stripes in the block */
    for (idx=0; idx<16; idx++) {
      xxh3_64_large_acc(block+idx*64, xxh3_secret+idx*8, acc);
    }
    /* Step 2.2 Scramble accumulators */
    xxh3_64_large_mix(acc);
    /* Advance! */
    block += 1024;
    len -= 1024;
  }
  if (copy_rem) {
    memcpy(state->rem, block-64, 64);
  }
  /* store accumulators */
  if (copy_acc) {
    state->ready_acc = 1;
    memcpy(state->acc, acc, sizeof(acc));
  }
  /* Put the reminder in the buffer. */
  memcpy(buffer + bsize, block, len);
  state->buf_size = bsize + len;
  state->total_len += length;
}

static inline void
xxh3_update(xxh3_state *state, const uint8_t *input, const size_t length)
{
  size_t bsize = state->buf_size;
  if (length==0)
    return;
  /* fast path for incomplete block. */
  if (length <= 1024-bsize) {
    memcpy(state->buf+bsize, input, length);
    state->buf_size = bsize + length;
    state->total_len += length;
    return;
  }
  xxh3_update_slow(state, input, length);
}

/* ECL hashing functions sometimes directly pass words. We maintain the little
   endian order per XXH3 requirements. Both 32 and 64 variants are available. */
static inline void
xxh3_update32(xxh3_state *state, const uint32_t word)
{
  size_t bsize = state->buf_size;
  if (bsize <= 1024-4) {
    xxh3_write32(state->buf+bsize, word);
    state->buf_size += 4;
    state->total_len += 4;
  } else {
    uint8_t input[4];
    xxh3_write32(input, word);
    xxh3_update(state, input, 4);
  }
}

static inline void
xxh3_update64(xxh3_state *state, const uint64_t word)
{
  size_t bsize = state->buf_size;
  if (bsize <= 1024-8) {
    xxh3_write64(state->buf+bsize, word);
    state->buf_size += 8;
    state->total_len += 8;
  } else {
    uint8_t input[8];
    xxh3_write64(input, word);
    xxh3_update(state, input, 8);
  }
}

/* Keep the slow path separate to keep the fast path small for inlining. */
static uint64_t
xxh3_digest_slow(xxh3_state *state)
{
  /* INV total > 240 */
  uint64_t total = state->total_len;
  uint64_t len = state->buf_size;
  uint64_t acc[8];
  uint64_t n, i, num, value;
  const uint8_t *block = state->buf;
  /* Don't modify the state. */
  xxh3_ensure_acc(state, acc);
  /* Step 3. Process the last block and the last 64 bytes */
  num = (len-1)/64;
  for (n=0; n<num; n++) {
    xxh3_64_large_acc(block+n*64, xxh3_secret+n*8, acc);
  }
  /* The last stripe is taken from the end and may overlap. */
  if (len>=64) {
    xxh3_64_large_acc(block+len-64, xxh3_secret+192-71, acc);
  } else {
    uint8_t rem[64];
    memcpy(rem, state->rem+len, 64-len);
    memcpy(rem+64-len, block, len);
    xxh3_64_large_acc(rem, xxh3_secret+192-71, acc);
  }
  /* Step 4. Finalization */
  value = total*PRIME64_1;
  for (i=0; i<4; i++) {
    uint64_t a = acc[i*2]   ^ xxh3_read64(xxh3_secret+11+i*16);
    uint64_t b = acc[i*2+1] ^ xxh3_read64(xxh3_secret+11+i*16+8);
    value += xxh3_mul128_fold64(a, b);
  }
  return xxh3_avalanche(value);
}

/* While slow path is separated, this is still potentially big because xxh3_64
   itself may get inlined here. That's deliberate. */
static inline uint64_t
xxh3_digest(xxh3_state *state)
{
  uint64_t total = state->total_len;
  const uint8_t *block = state->buf;
  if (total <= 240)
    return xxh3_64(block, total);
  return xxh3_digest_slow(state);
}

#endif
