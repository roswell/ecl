/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/* This is a header implementation of xxhash family of algorithms:
   https://www.ietf.org/archive/id/draft-josefsson-xxhash-00.html

   SIMD instrinsics:
   - SSE/AVX: https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html
   - WASM: https://emscripten.org/docs/porting/simd.html
   - NEON/SVE: https://developer.arm.com/architectures/instruction-sets/intrinsics/
   - RVV: https://docs.riscv.org/reference/vector-c-intrinsics/

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

   We hardcode a default secret and seed, but the code to derive a custom one is
   implemented for customization.

   Compared to newhash (our old algorithm) XXH3 one-shot mode is always faster
   (1.7x at 1B and growing with input size) while the streaming version is
   slower at 1B, but quickly pulls ahead (crossover is also at 4B).

   Another tradeoff for XXH3 streaming is that our state is ~1.2KB to keep the
   code of update/digest straightforward, while upstream's state is half of it
   (while newhash is practically stateless). We allocate the state on the stack
   and that's not much of a problem (we could move it to cl_env too).

   -- jd 2026-06-05 */

#ifndef ECL_XXHASH_H
#define ECL_XXHASH_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

/* XXH32 */

#define ECL_XXH_PRIME32_1 (0x9E3779B1U)
#define ECL_XXH_PRIME32_2 (0x85EBCA77U)
#define ECL_XXH_PRIME32_3 (0xC2B2AE3DU)
#define ECL_XXH_PRIME32_4 (0x27D4EB2FU)
#define ECL_XXH_PRIME32_5 (0x165667B1U)

/* INV never called with s=0 */
#define ecl_xxh32_rol(x,s) ((x << s) | (x >> (32 - s)))

#define ecl_xxh32_cvg(acc1,acc2,acc3,acc4)                              \
  (ecl_xxh32_rol(acc1,1) + ecl_xxh32_rol(acc2,7) + ecl_xxh32_rol(acc3,12) + ecl_xxh32_rol(acc4,18))

static inline uint32_t
ecl_xxh32_avalanche(uint32_t x)
{
  x = x ^ (x >> 15);
  x = x * ECL_XXH_PRIME32_2;
  x = x ^ (x >> 13);
  x = x * ECL_XXH_PRIME32_3;
  x = x ^ (x >> 16);
  return x;
}

static inline uint32_t
ecl_xxh32_round(uint32_t accN, uint32_t laneN)
{
  accN = accN + (laneN * ECL_XXH_PRIME32_2);
  accN = ecl_xxh32_rol(accN, 13);
  return accN * ECL_XXH_PRIME32_1;
}

static inline uint32_t
ecl_xxh32_lanes_small(const uint32_t *vector, uint32_t length, uint32_t seed)
{
  uint32_t acc, rem, idx, lane;
  rem = length, idx = 0;
  /* Step 1. Initialize internal accumulator */
  acc = seed + ECL_XXH_PRIME32_5;
  /* Step 4. Add input length */
  acc = acc + length*4;
  /* Step 5. Consume remaining input */
  /* There may be 0, 1, 2 or 3 remaining lanes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc + lane * ECL_XXH_PRIME32_3;
    acc = ecl_xxh32_rol(acc, 17) * ECL_XXH_PRIME32_4;
  }
  /* Step 6. Final mix (avalanche) */
  acc = ecl_xxh32_avalanche(acc);
  /* Step 7. Output */
  return acc;
}

static inline uint32_t
ecl_xxh32_lanes(const uint32_t *vector, uint32_t length, uint32_t seed)
{
  uint32_t acc, rem, idx, lane;
  if (length<4)
    return ecl_xxh32_lanes_small(vector, length, seed);
  rem = length, idx = 0;
  /* Step 1. Initialize internal accumulators */
  uint32_t acc1 = seed + ECL_XXH_PRIME32_1 + ECL_XXH_PRIME32_2;
  uint32_t acc2 = seed + ECL_XXH_PRIME32_2;
  uint32_t acc3 = seed + 0;
  uint32_t acc4 = seed - ECL_XXH_PRIME32_1;
  /* Step 2. Process stripes */
  for(; rem>=4; rem-=4, idx+=4) {
    acc1 = ecl_xxh32_round(acc1, vector[idx+0]);
    acc2 = ecl_xxh32_round(acc2, vector[idx+1]);
    acc3 = ecl_xxh32_round(acc3, vector[idx+2]);
    acc4 = ecl_xxh32_round(acc4, vector[idx+3]);
  }
  /* Step 3. Accumulator convergence */
  acc = ecl_xxh32_cvg(acc1, acc2, acc3, acc4);
  /* Step 4. Add input length */
  acc = acc + length*4;
  /* Step 5. Consume remaining input */
  /* There may be 0, 1, 2 or 3 remaining lanes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc + lane * ECL_XXH_PRIME32_3;
    acc = ecl_xxh32_rol(acc, 17) * ECL_XXH_PRIME32_4;
  }
  /* Step 6. Final mix (avalanche) */
  acc = ecl_xxh32_avalanche(acc);
  /* Step 7. Output */
  return acc;
}


/* XXH64 */

#define ECL_XXH_PRIME64_1 (0x9E3779B185EBCA87ULL)
#define ECL_XXH_PRIME64_2 (0xC2B2AE3D27D4EB4FULL)
#define ECL_XXH_PRIME64_3 (0x165667B19E3779F9ULL)
#define ECL_XXH_PRIME64_4 (0x85EBCA77C2B2AE63ULL)
#define ECL_XXH_PRIME64_5 (0x27D4EB2F165667C5ULL)

/* INV never called with s=0 */
#define ecl_xxh64_rol(x,s) ((x << s) | (x >> (64 - s)))

#define ecl_xxh64_cvg(acc1,acc2,acc3,acc4)                              \
  (ecl_xxh64_rol(acc1,1) + ecl_xxh64_rol(acc2,7) + ecl_xxh64_rol(acc3,12) + ecl_xxh64_rol(acc4,18))

static inline uint64_t
ecl_xxh64_avalanche(uint64_t x)
{
  x = x ^ (x >> 33);
  x = x * ECL_XXH_PRIME64_2;
  x = x ^ (x >> 29);
  x = x * ECL_XXH_PRIME64_3;
  x = x ^ (x >> 32);
  return x;
}

static inline uint64_t
ecl_xxh64_round(uint64_t accN, uint64_t laneN)
{
  accN = accN + (laneN * ECL_XXH_PRIME64_2);
  accN = ecl_xxh64_rol(accN, 31);
  return accN * ECL_XXH_PRIME64_1;
}

static inline uint64_t
ecl_xxh64_merge(uint64_t acc, uint64_t accN)
{
  acc = acc ^ ecl_xxh64_round(0, accN);
  acc = acc * ECL_XXH_PRIME64_1;
  return acc + ECL_XXH_PRIME64_4;
}

static inline uint64_t
ecl_xxh64_lanes_small(const uint64_t *vector, uint64_t length, uint64_t seed)
{
  uint64_t acc, rem, idx, lane;
  rem = length, idx = 0;
  /* Step 1. Initialize internal accumulator */
  acc = seed + ECL_XXH_PRIME64_5;
  /* Step 4. Add input length */
  acc = acc + length*8;
  /* Step 5. Consume remaining input */
  /* There may be 0, 1, 2 or 3 remaining lanes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc ^ ecl_xxh64_round(0, lane);
    acc = ecl_xxh64_rol(acc, 27) * ECL_XXH_PRIME64_1;
    acc = acc + ECL_XXH_PRIME64_4;
  }
  /* Step 6. Final mix (avalanche) */
  acc = ecl_xxh64_avalanche(acc);
  /* Step 7. Output */
  return acc;
}

static inline uint64_t
ecl_xxh64_lanes(const uint64_t *vector, uint64_t length, uint64_t seed)
{
  uint64_t acc, rem, idx, lane;
  if (length<4)
    return ecl_xxh64_lanes_small(vector, length, seed);
  rem = length, idx = 0;
  /* Step 1. Initialize internal accumulators */
  uint64_t acc1 = seed + ECL_XXH_PRIME64_1 + ECL_XXH_PRIME64_2;
  uint64_t acc2 = seed + ECL_XXH_PRIME64_2;
  uint64_t acc3 = seed + 0;
  uint64_t acc4 = seed - ECL_XXH_PRIME64_1;
  /* Step 2. Process stripes */
  for(; rem>=4; rem-=4, idx+=4) {
    acc1 = ecl_xxh64_round(acc1, vector[idx+0]);
    acc2 = ecl_xxh64_round(acc2, vector[idx+1]);
    acc3 = ecl_xxh64_round(acc3, vector[idx+2]);
    acc4 = ecl_xxh64_round(acc4, vector[idx+3]);
  }
  /* Step 3. Accumulator convergence */
  acc = ecl_xxh64_cvg(acc1,acc2,acc3,acc4);
  acc = ecl_xxh64_merge(acc, acc1);
  acc = ecl_xxh64_merge(acc, acc2);
  acc = ecl_xxh64_merge(acc, acc3);
  acc = ecl_xxh64_merge(acc, acc4);
  /* Step 4. Add input length */
  acc = acc + length*8;
  /* Step 5. Consume remaining input */
  /* There may be 0, 1, 2 or 3 remaining lanes. */
  for(; rem>=1; rem--, idx++) {
    lane = vector[idx];
    acc = acc ^ ecl_xxh64_round(0, lane);
    acc = ecl_xxh64_rol(acc, 27) * ECL_XXH_PRIME64_1;
    acc = acc + ECL_XXH_PRIME64_4;
  }
  /* Step 6. Final mix (avalanche) */
  acc = ecl_xxh64_avalanche(acc);
  /* Step 7. Output */
  return acc;
}


/* XXH3-64 (we don't currently need XXH3-128) */

#define ECL_XXH_PRIME_MX1 (0x165667919E3779F9ULL)
#define ECL_XXH_PRIME_MX2 (0x9FB21C651E98DF25ULL)

static const uint8_t ecl_xxh3_default_secret[192] = {
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
ecl_xxh3_mul128_fold64(uint64_t a, uint64_t b) {
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
# define ecl_xxh3_bswap64(x) __builtin_bswap64(x)
# define ecl_xxh3_bswap32(x) __builtin_bswap32(x)
#else
static inline uint64_t
ecl_xxh3_bswap64(uint64_t x)
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
ecl_xxh3_bswap32(uint32_t x)
{
  return ((x & 0x000000ff) << 24)
    |    ((x & 0x0000ff00) <<  8)
    |    ((x & 0x00ff0000) >>  8)
    |    ((x & 0xff000000) >> 24);
}
#endif

static inline uint32_t
ecl_xxh3_read32(const uint8_t *src)
{
  uint32_t val;
  memcpy(&val, src, 4);
#ifdef WORDS_BIGENDIAN
  val = ecl_xxh3_bswap32(val);
#endif
  return val;
}

static inline uint64_t
ecl_xxh3_read64(const uint8_t *src)
{
  uint64_t val;
  memcpy(&val, src, 8);
#ifdef WORDS_BIGENDIAN
  val = ecl_xxh3_bswap64(val);
#endif
  return val;
}

static inline void
ecl_xxh3_write32(uint8_t *dst, uint32_t x)
{
#ifdef WORDS_BIGENDIAN
  x = ecl_xxh3_bswap32(x);
#endif
  memcpy(dst, &x, 4);
}

static inline void
ecl_xxh3_write64(uint8_t *dst, uint64_t x)
{
#ifdef WORDS_BIGENDIAN
  x = ecl_xxh3_bswap64(x);
#endif
  memcpy(dst, &x, 8);
}

static inline uint64_t
ecl_xxh3_avalanche(uint64_t x)
{
  x = x ^ (x >> 37);
  x = x * ECL_XXH_PRIME_MX1;
  x = x ^ (x >> 32);
  return x;
}

/* If the hashing security matters, then derive a different secret based on
   random seed and replace ecl_xxh3_seed and ecl_xxh3_secret. -- jd 2026-09-17 */
static inline void
ecl_xxh3_derive_secret(uint8_t buffer[192], const uint8_t secret[192], uint64_t seed)
{
  int i;
  for (i=0; i<12; i++) {
    uint64_t a = ecl_xxh3_read64(secret+i*16);
    uint64_t b = ecl_xxh3_read64(secret+i*16+8);
    ecl_xxh3_write64(buffer+i*16, a+seed);
    ecl_xxh3_write64(buffer+i*16+8, b-seed);
  }
}

#define ecl_xxh3_seed 0
#define ecl_xxh3_secret ecl_xxh3_default_secret

/* Small input */

static inline uint64_t
ecl_xxh3_64_empty(const uint8_t *input, const size_t length)
{
  uint64_t secret0, secret1;
  secret0 = ecl_xxh3_read64(ecl_xxh3_secret+56);
  secret1 = ecl_xxh3_read64(ecl_xxh3_secret+56+8);
  return ecl_xxh64_avalanche(ecl_xxh3_seed ^ secret0 ^ secret1);
}

static inline uint64_t
ecl_xxh3_64_1to3(const uint8_t *input, const size_t length)
{
  uint32_t secret0, secret1, combined;
  uint64_t value;
  secret0 = ecl_xxh3_read32(ecl_xxh3_secret);
  secret1 = ecl_xxh3_read32(ecl_xxh3_secret+4);
  combined =
    ((uint32_t)input[length-1]) | ((uint32_t)length << 8) |
    ((uint32_t)input[0] << 16)  | ((uint32_t)input[length>>1] << 24);
  value = ((uint64_t)(secret0 ^ secret1) + ecl_xxh3_seed) ^ (uint64_t)combined;
  return ecl_xxh64_avalanche(value);
}

static inline uint64_t
ecl_xxh3_64_4to8(const uint8_t *input, const size_t length)
{
  uint32_t input_first, input_last;
  uint64_t modified_seed, secret0, secret1, combined, value;
  input_first = ecl_xxh3_read32(input);
  input_last = ecl_xxh3_read32(input+length-4);
  modified_seed = ecl_xxh3_seed ^
    ((uint64_t)ecl_xxh3_bswap32((uint32_t)ecl_xxh3_seed) << 32);
  secret0 = ecl_xxh3_read64(ecl_xxh3_secret+8);
  secret1 = ecl_xxh3_read64(ecl_xxh3_secret+8+8);
  combined = (uint64_t)input_last | ((uint64_t)input_first << 32);
  value = ((secret0 ^ secret1) - modified_seed) ^ combined;
  value = value ^ ecl_xxh64_rol(value, 49) ^ ecl_xxh64_rol(value, 24);
  value = value * ECL_XXH_PRIME_MX2;
  value = value ^ ((value >> 35) + length);
  value = value * ECL_XXH_PRIME_MX2;
  value = value ^ (value >> 28);
  return value;
}

static inline uint64_t
ecl_xxh3_64_9to16(const uint8_t *input, const size_t length)
{
  uint64_t input_first, input_last,
    secret0, secret1, secret2, secret3,
    low, high, fold, value;
  input_first = ecl_xxh3_read64(input);
  input_last = ecl_xxh3_read64(input+length-8);
  secret0 = ecl_xxh3_read64(ecl_xxh3_secret+24);
  secret1 = ecl_xxh3_read64(ecl_xxh3_secret+24+8);
  secret2 = ecl_xxh3_read64(ecl_xxh3_secret+24+16);
  secret3 = ecl_xxh3_read64(ecl_xxh3_secret+24+24);
  low = ((secret0 ^ secret1) + ecl_xxh3_seed) ^ input_first;
  high= ((secret2 ^ secret3) - ecl_xxh3_seed) ^ input_last;
  fold= ecl_xxh3_mul128_fold64(low, high);
  value = length + ecl_xxh3_bswap64(low) + high + fold;
  return ecl_xxh3_avalanche(value);
}

/* Medium input */

static inline uint64_t
ecl_xxh3_64_medium_mix(const uint8_t *input, const uint8_t *secret)
{
  uint64_t input0, input1, secret0, secret1;
  input0 = ecl_xxh3_read64(input);
  input1 = ecl_xxh3_read64(input+8);
  secret0 = ecl_xxh3_read64(secret);
  secret1 = ecl_xxh3_read64(secret+8);
  return ecl_xxh3_mul128_fold64(input0 ^ (secret0 + ecl_xxh3_seed),
                                input1 ^ (secret1 - ecl_xxh3_seed));
}

#define ecl_xxh3_64_medium_mix2(i) do {                                 \
    acc += ecl_xxh3_64_medium_mix(input+i*16,           ecl_xxh3_secret+i*32); \
    acc += ecl_xxh3_64_medium_mix(input+length-i*16-16, ecl_xxh3_secret+i*32+16); \
  } while(0)

static inline uint64_t
ecl_xxh3_64_17to128(const uint8_t *input, const size_t length)
{
  uint64_t acc;
  acc = length * ECL_XXH_PRIME64_1;
  if (length <= 32) {
    ecl_xxh3_64_medium_mix2(0);
  } else if (length <= 64) {
    ecl_xxh3_64_medium_mix2(1);
    ecl_xxh3_64_medium_mix2(0);
  } else if (length <= 96) {
    ecl_xxh3_64_medium_mix2(2);
    ecl_xxh3_64_medium_mix2(1);
    ecl_xxh3_64_medium_mix2(0);
  } else /*if (length <= 128)*/ {
    ecl_xxh3_64_medium_mix2(3);
    ecl_xxh3_64_medium_mix2(2);
    ecl_xxh3_64_medium_mix2(1);
    ecl_xxh3_64_medium_mix2(0);
  }
  return ecl_xxh3_avalanche(acc);
}
#undef ecl_xxh3_64_medium_mix2

/* Deliberately not declared inline to allow compiler decide. */
static uint64_t
ecl_xxh3_64_129to240(const uint8_t *input, const size_t length)
{
  uint64_t acc, num_chunks, i;
  acc = length * ECL_XXH_PRIME64_1;
  num_chunks = length >> 4;
  for (i=0; i<8; i++) {
    acc += ecl_xxh3_64_medium_mix(input+i*16, ecl_xxh3_secret+i*16);
  }
  acc = ecl_xxh3_avalanche(acc);
  for (i=8; i<num_chunks; i++) {
    /* Unrolling this into a switch makes ecl_xxh3 slower (!) */
    acc += ecl_xxh3_64_medium_mix(input+i*16, ecl_xxh3_secret+(i-8)*16+3);
  }
  acc += ecl_xxh3_64_medium_mix(input+length-16, ecl_xxh3_secret+119);
  return ecl_xxh3_avalanche(acc);
}

/* FIXME Preprocessor macros are predefined here. We need to use ECL_SSE2 flag
   and add support for others as well in autotools. Alternatively (optimally?)
   we should have our own portable instructions to avoid duplication. For now
   let's roll with raw SIMD intrinsics.

   AVX512 implies AVX2 implies SSE2, so preprocessor test order matters.
   GCC flags to enable extensions:

   - SSE2:    (baseline) - enabled by default when host supports it
   - AVX2:    -mavx2
   - AVX512F: -mavx512f */

#define ECL_XXH3_SSE2 1    /* __SSE2__            128-bit    <emmintrin.h>    */
#define ECL_XXH3_AVX2 0    /* __AVX2__            256-bit    <immintrin.h>    */
#define ECL_XXH3_AVXF 0    /* __AVX512F__         512-bit    <immintrin.h>    */
#define ECL_XXH3_WASM 0    /* __wasm_simd128__    128-bit    <wasm_simd128.h> */
#define ECL_XXH3_NEON 0    /* __ARM_NEON          128-bit    <arm_neon.h>     */
#define ECL_XXH3_SCALAR 0

/* Profiler shows that ecl_xxh3_64_large_acc is dominating the hash computation.
   ecl_xxh3_64_large_mix is transformed to hoist xacc. -- jd 2026-10-01 */
#if ECL_XXH3_SCALAR

typedef uint64_t ecl_xacc[8];

static inline void
init_xacc(ecl_xacc acc)
{
  acc[0] = ECL_XXH_PRIME32_3; acc[1] = ECL_XXH_PRIME64_1;
  acc[2] = ECL_XXH_PRIME64_2; acc[3] = ECL_XXH_PRIME64_3;
  acc[4] = ECL_XXH_PRIME64_4; acc[5] = ECL_XXH_PRIME32_2;
  acc[6] = ECL_XXH_PRIME64_5; acc[7] = ECL_XXH_PRIME32_1;
}

static inline void
load_xacc(ecl_xacc xacc, const uint64_t acc[8])
{
  memcpy(xacc, acc, 8*sizeof(uint64_t));
}

static inline void
save_xacc(uint64_t acc[8], const ecl_xacc xacc)
{
  memcpy(acc, xacc, 8*sizeof(uint64_t));
}

static inline void
ecl_xxh3_64_large_acc(const uint8_t *block, const uint8_t *secret, ecl_xacc acc)
{
  uint64_t stripe, key, value;
  int i;
  for (i=0; i<8; i++) {
    stripe = ecl_xxh3_read64(block+i*8);
    key = ecl_xxh3_read64(secret+i*8);
    value = stripe ^ key;
    acc[i^1] = acc[i^1]+stripe;
    acc[i] += (value & 0xffffffff) * (value>>32);
  }
}

static inline void
ecl_xxh3_64_large_mix(ecl_xacc acc)
{
  uint64_t secret_var,i ;
  for (i=0; i<8; i++) {
    secret_var = ecl_xxh3_read64(ecl_xxh3_secret+192-64+i*8);
    acc[i] = acc[i] ^ (acc[i] >> 47);
    acc[i] = acc[i] ^ secret_var;
    acc[i] = acc[i] * ECL_XXH_PRIME32_1;
  }
}
#elif ECL_XXH3_SSE2
#include <emmintrin.h>

/* _MM_SHUFFLE argument order is deranged, XX_SHUFFLE: dst[k] = src[ik]. */
#define XX_SHUFFLE(i0, i1, i2, i3) _MM_SHUFFLE(i3, i2, i1, i0)
/* _mm_set_epi64x has the same issue as with _MM_SHUFFLE -_-.  */
#define xx_set_epi64x(a, b) _mm_set_epi64x(b, a)

typedef __m128i ecl_xacc[4];

static inline void
init_xacc(ecl_xacc acc)
{
  acc[0] = xx_set_epi64x(ECL_XXH_PRIME32_3, ECL_XXH_PRIME64_1);
  acc[1] = xx_set_epi64x(ECL_XXH_PRIME64_2, ECL_XXH_PRIME64_3);
  acc[2] = xx_set_epi64x(ECL_XXH_PRIME64_4, ECL_XXH_PRIME32_2);
  acc[3] = xx_set_epi64x(ECL_XXH_PRIME64_5, ECL_XXH_PRIME32_1);
}

static inline void
load_xacc(ecl_xacc xacc, const uint64_t acc[8])
{
  xacc[0] = _mm_loadu_si128((__m128i const *)(acc+0));
  xacc[1] = _mm_loadu_si128((__m128i const *)(acc+2));
  xacc[2] = _mm_loadu_si128((__m128i const *)(acc+4));
  xacc[3] = _mm_loadu_si128((__m128i const *)(acc+6));
}

static inline void
save_xacc(uint64_t acc[8], const ecl_xacc xacc)
{
  _mm_storeu_si128((__m128i *)(acc+0), xacc[0]);
  _mm_storeu_si128((__m128i *)(acc+2), xacc[1]);
  _mm_storeu_si128((__m128i *)(acc+4), xacc[2]);
  _mm_storeu_si128((__m128i *)(acc+6), xacc[3]);
}

static inline void
ecl_xxh3_64_large_acc(const uint8_t *block, const uint8_t *secret, ecl_xacc acc)
{
  __m128i stripe, key, value, extra;
  int i;
  for (i=0; i<4; i++) {
    /* INV x86 (implied by SSE2) is little endian so no need for byte swaps. */
    stripe = _mm_loadu_si128 ((__m128i const *)(block+i*16));
    key    = _mm_loadu_si128 ((__m128i const *)(secret+i*16));
    value = _mm_xor_si128(stripe, key);
    /* acc[i^1] += stripe; every two iterations translates to:
       xacc_lower += stripe_upper,
       xacc_upper += stripe_lower; */
    stripe = _mm_shuffle_epi32(stripe, XX_SHUFFLE(2, 3, 0, 1));
    acc[i] = _mm_add_epi64(acc[i], stripe);
    /* acc[i] += value_lower * value_upper
         xacc_lower += value[0]*value[1],
         xacc_upper += value[2]*value[3];
       but _mm_mul_epu32 is a product:
         xacc_lower += arg1[0]*arg2[0],
         xacc_upper += arg1[2]*arg2[2];
    */
    extra = _mm_shuffle_epi32(value, XX_SHUFFLE(1, 0, 3, 0));
    value = _mm_mul_epu32(value, extra);
    acc[i] = _mm_add_epi64(acc[i], value);
  }
}

static inline void
ecl_xxh3_64_large_mix(ecl_xacc acc)
{
  __m128i key, tmp, prime, lo, hi;
  int i;
  prime = _mm_set_epi64x(ECL_XXH_PRIME32_1, ECL_XXH_PRIME32_1);
  for (i=0; i<4; i++) {
    key = _mm_loadu_si128 ((__m128i const *)(ecl_xxh3_secret+192-64+i*16));
    tmp = acc[i];
    tmp = _mm_xor_si128(tmp, _mm_srli_epi64(tmp, 47));
    tmp = _mm_xor_si128(tmp, key);
    /* acc[i] = acc[i] * ECL_XXH_PRIME32_1; */
    /* xacc_lower = xacc_lower * ECL_XXH_PRIME32_1 */
    /* xacc_upper = xacc_upper * ECL_XXH_PRIME32_1 */
    lo = _mm_mul_epu32(tmp, prime);
    hi = _mm_mul_epu32(_mm_srli_epi64(tmp, 32), prime);
    acc[i] = _mm_add_epi64( lo, _mm_slli_epi64(hi, 32));
  }
}
#elif ECL_XXH3_AVX2
#include <immintrin.h>

/* _MM_SHUFFLE argument order is deranged, XX_SHUFFLE: dst[k] = src[ik]. */
#define XX_SHUFFLE(i0, i1, i2, i3) _MM_SHUFFLE(i3, i2, i1, i0)

/* _mm_set_epi64x has the same issue as with _MM_SHUFFLE -_-.  */
#define xx256_set_epi64x(a, b, c, d) _mm256_set_epi64x(d, c, b, a)

typedef __m256i ecl_xacc[2];

static inline void
init_xacc(ecl_xacc acc)
{
  acc[0] = xx256_set_epi64x(ECL_XXH_PRIME32_3, ECL_XXH_PRIME64_1,
                            ECL_XXH_PRIME64_2, ECL_XXH_PRIME64_3);
  acc[1] = xx256_set_epi64x(ECL_XXH_PRIME64_4, ECL_XXH_PRIME32_2,
                            ECL_XXH_PRIME64_5, ECL_XXH_PRIME32_1);
}

static inline void
load_xacc(ecl_xacc xacc, const uint64_t acc[8])
{
  xacc[0] = _mm256_loadu_si256((__m256i const *)(acc+0));
  xacc[1] = _mm256_loadu_si256((__m256i const *)(acc+4));
}

static inline void
save_xacc(uint64_t acc[8], const ecl_xacc xacc)
{
  _mm256_storeu_si256((__m256i *)(acc+0), xacc[0]);
  _mm256_storeu_si256((__m256i *)(acc+4), xacc[1]);
}

static inline void
ecl_xxh3_64_large_acc(const uint8_t *block, const uint8_t *secret, ecl_xacc acc)
{
  __m256i stripe, key, value, extra;
  int i;
  for (i=0; i<2; i++) {
    /* INV x86 (implied by AVX2) is little endian so no need for byte swaps. */
    stripe = _mm256_loadu_si256 ((__m256i const *)(block+i*32));
    key    = _mm256_loadu_si256 ((__m256i const *)(secret+i*32));
    value = _mm256_xor_si256(stripe, key);
    /* acc[i^1] += stripe; every two iterations translates to:
       xacc_q1 += stripe_q2,
       xacc_q2 += stripe_q1;
       xacc_q3 += stripe_q4,
       xacc_q4 += stripe_q3; */
    stripe = _mm256_shuffle_epi32(stripe, XX_SHUFFLE(2, 3, 0, 1));
    acc[i] = _mm256_add_epi64(acc[i], stripe);
    /* acc[i] += value_lower * value_upper */
    extra = _mm256_shuffle_epi32(value, XX_SHUFFLE(1, 0, 3, 0));
    value = _mm256_mul_epu32(value, extra);
    acc[i] = _mm256_add_epi64(acc[i], value);
  }
}

static inline void
ecl_xxh3_64_large_mix(ecl_xacc acc)
{
  __m256i key, tmp, prime, lo, hi;
  int i;
  prime = _mm256_set_epi64x(ECL_XXH_PRIME32_1, ECL_XXH_PRIME32_1,
                            ECL_XXH_PRIME32_1, ECL_XXH_PRIME32_1);
  for (i=0; i<2; i++) {
    key = _mm256_loadu_si256 ((__m256i const *)(ecl_xxh3_secret+192-64+i*32));
    tmp = acc[i];
    tmp = _mm256_xor_si256(tmp, _mm256_srli_epi64(tmp, 47));
    tmp = _mm256_xor_si256(tmp, key);

    lo = _mm256_mul_epu32(tmp, prime);
    hi = _mm256_mul_epu32(_mm256_srli_epi64(tmp, 32), prime);
    acc[i] = _mm256_add_epi64( lo, _mm256_slli_epi64(hi, 32));
  }
}

#elif ECL_XXH3_AVXF
#include <immintrin.h>

/* _MM_SHUFFLE argument order is deranged, XX_SHUFFLE: dst[k] = src[ik]. */
#define XX_SHUFFLE(i0, i1, i2, i3) _MM_SHUFFLE(i3, i2, i1, i0)

/* _mm_set_epi64x has the same issue as with _MM_SHUFFLE -_-.  */
#define xx512_set_epi64(a,b,c,d,e,f,g,h) _mm512_set_epi64(h,g,f,e,d,c,b,a)

typedef __m512i ecl_xacc[1];

static inline void
init_xacc(ecl_xacc acc)
{
  acc[0] = xx512_set_epi64(ECL_XXH_PRIME32_3, ECL_XXH_PRIME64_1,
                           ECL_XXH_PRIME64_2, ECL_XXH_PRIME64_3,
                           ECL_XXH_PRIME64_4, ECL_XXH_PRIME32_2,
                           ECL_XXH_PRIME64_5, ECL_XXH_PRIME32_1);
}

static inline void
load_xacc(ecl_xacc xacc, const uint64_t acc[8])
{
  xacc[0] = _mm512_loadu_si512((__m512i const *)(acc+0));
}

static inline void
save_xacc(uint64_t acc[8], const ecl_xacc xacc)
{
  _mm512_storeu_si512((__m512i *)(acc+0), xacc[0]);
}

static inline void
ecl_xxh3_64_large_acc(const uint8_t *block, const uint8_t *secret, ecl_xacc acc)
{
  __m512i stripe, key, value, extra;
  {
    /* INV x86 (implied by AVX2) is little endian so no need for byte swaps. */
    stripe = _mm512_loadu_si512 ((__m512i const *)(block));
    key    = _mm512_loadu_si512 ((__m512i const *)(secret));
    value = _mm512_xor_si512(stripe, key);
    /* acc[i^1] += stripe; */
    stripe = _mm512_shuffle_epi32(stripe, XX_SHUFFLE(2, 3, 0, 1));
    acc[0] = _mm512_add_epi64(acc[0], stripe);
    /* acc[i] += value_lower * value_upper */
    extra = _mm512_shuffle_epi32(value, XX_SHUFFLE(1, 0, 3, 0));
    value = _mm512_mul_epu32(value, extra);
    acc[0] = _mm512_add_epi64(acc[0], value);
  }
}

static inline void
ecl_xxh3_64_large_mix(ecl_xacc acc)
{
  __m512i key, tmp, prime, lo, hi;
  prime = _mm512_set_epi64(ECL_XXH_PRIME32_1, ECL_XXH_PRIME32_1,
                           ECL_XXH_PRIME32_1, ECL_XXH_PRIME32_1,
                           ECL_XXH_PRIME32_1, ECL_XXH_PRIME32_1,
                           ECL_XXH_PRIME32_1, ECL_XXH_PRIME32_1);
  {
    key = _mm512_loadu_si512((__m512i const *)(ecl_xxh3_secret+192-64));
    tmp = acc[0];
    tmp = _mm512_xor_si512(tmp, _mm512_srli_epi64(tmp, 47));
    tmp = _mm512_xor_si512(tmp, key);

    lo = _mm512_mul_epu32(tmp, prime);
    hi = _mm512_mul_epu32(_mm512_srli_epi64(tmp, 32), prime);
    acc[0] = _mm512_add_epi64( lo, _mm512_slli_epi64(hi, 32));
  }
}
#endif  /* There's more! SVE2 (128-2048b), RVV (128-2048b) */

/* Deliberately not declared inline to allow compiler decide. */
static uint64_t
ecl_xxh3_64_large(const uint8_t *input, const size_t length)
{
  /* The upsteram algorithm is paramerize, we assume:
     secret_length=192, stripes_per_block=16, block_size=1024 */
  uint64_t i, n, num, value, len;
  const uint8_t *block;
  /* Step 1. Initialize internal accumulators */
  uint64_t acc[8];
  ecl_xacc xacc;
  init_xacc(xacc);
  /* Step 2. Process blocks */
  len = length;
  block = input;
  while(len > 1024) {
    /* Step 2.1 Process stripes in the block */
    for (n=0; n<16; n++) {
      ecl_xxh3_64_large_acc(block+n*64, ecl_xxh3_secret+n*8, xacc);
    }
    /* Step 2.2 Scramble accumulators */
    ecl_xxh3_64_large_mix(xacc);
    /* Advance! */
    block += 1024;
    len -= 1024;
  }
  /* Step 3. Process the last block and the last 64 bytes */
  num = (len-1)/64;
  for (n=0; n<num; n++) {
    ecl_xxh3_64_large_acc(block+n*64, ecl_xxh3_secret+n*8, xacc);
  }
  /* The last stripe is taken from the end and may overlap. */
  block = input + length - 64;
  ecl_xxh3_64_large_acc(block, ecl_xxh3_secret+192-71, xacc);
  /* Step 4. Finalization */
  value = length*ECL_XXH_PRIME64_1;
  save_xacc(acc, xacc);
  for (i=0; i<4; i++) {
    uint64_t a = acc[i*2]   ^ ecl_xxh3_read64(ecl_xxh3_secret+11+i*16);
    uint64_t b = acc[i*2+1] ^ ecl_xxh3_read64(ecl_xxh3_secret+11+i*16+8);
    value += ecl_xxh3_mul128_fold64(a, b);
  }
  return ecl_xxh3_avalanche(value);
}

static inline uint64_t
ecl_xxh3_64(const uint8_t *input, const size_t length)
{
  /* specialized variants */
  if (length == 0)   return ecl_xxh3_64_empty    (input, length);
  if (length <= 3)   return ecl_xxh3_64_1to3     (input, length);
  if (length <= 8)   return ecl_xxh3_64_4to8     (input, length);
  if (length <= 16)  return ecl_xxh3_64_9to16    (input, length);
  if (length <= 128) return ecl_xxh3_64_17to128  (input, length);
  if (length <= 240) return ecl_xxh3_64_129to240 (input, length);
  return ecl_xxh3_64_large(input, length);
}

/* Streaming variant */

/* The upstream algorithm is paramerize, we assume:
   secret_length=192, stripes_per_block=16, block_size=1024

   We could store the seend and the secret in this structure as well at a cost
   of runtime dereferences (right now they are available at compile time). */
typedef struct ecl_xxh3_state {
  size_t buf_size;              /* buffer fill pointer */
  uint64_t total_len;           /* total length */
  bool ready_acc;
  uint64_t acc[8];              /* accumulators */
  uint8_t rem[64];              /* last 64 bytes for digest */
  uint8_t buf[1024];            /* remaining data */
} ecl_xxh3_state;

static inline void
ecl_xxh3_reset(ecl_xxh3_state *state)
{
  state->buf_size=0;
  state->total_len=0;
  state->ready_acc=0;
}

static inline void
ecl_xxh3_ensure_acc(ecl_xxh3_state *state, ecl_xacc xacc)
{
  if (state->ready_acc) {
    load_xacc(xacc, state->acc);
  } else {
    init_xacc(xacc);
  }
}

/* Keep the slow path separate to keep the fast path small for inlining. */
static void
ecl_xxh3_update_slow(ecl_xxh3_state *state, const uint8_t *input, const size_t length)
{
  /* INV length>0; bsize+length>1024 */
  uint8_t *buffer = state->buf;
  const uint8_t *block = input;
  size_t bsize = state->buf_size;
  size_t len = length;
  size_t delta;
  ecl_xacc xacc;
  uint64_t idx;
  bool copy_rem = 1;
  bool copy_acc = 0;
  /* Fill the current block and use it for update. */
  if (bsize) {
    delta = 1024-bsize;
    memcpy(buffer+bsize, input, delta);
    /* Ensure accumulators */
    ecl_xxh3_ensure_acc(state, xacc);
    copy_acc = 1;
    /* Step 2.1 Process stripes in the block */
    for (idx=0; idx<16; idx++) {
      ecl_xxh3_64_large_acc(buffer+idx*64, ecl_xxh3_secret+idx*8, xacc);
    }
    /* Step 2.2 Scramble accumulators */
    ecl_xxh3_64_large_mix(xacc);
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
      ecl_xxh3_ensure_acc(state, xacc);
      copy_acc = 1;
    }
    /* Step 2.1 Process stripes in the block */
    for (idx=0; idx<16; idx++) {
      ecl_xxh3_64_large_acc(block+idx*64, ecl_xxh3_secret+idx*8, xacc);
    }
    /* Step 2.2 Scramble accumulators */
    ecl_xxh3_64_large_mix(xacc);
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
    save_xacc(state->acc, xacc);
  }
  /* Put the reminder in the buffer. */
  memcpy(buffer + bsize, block, len);
  state->buf_size = bsize + len;
  state->total_len += length;
}

static inline void
ecl_xxh3_update(ecl_xxh3_state *state, const uint8_t *input, const size_t length)
{
  size_t bsize = state->buf_size;
  if (length==0)
    return;
  /* fast path for incomplete block. */
  /* bsize is always <= 1024, but GCC barfs that it can't prove it. */
  if (bsize <= 1024 && length <= 1024-bsize) {
    memcpy(state->buf+bsize, input, length);
    state->buf_size = bsize + length;
    state->total_len += length;
    return;
  }
  ecl_xxh3_update_slow(state, input, length);
}

/* ECL hashing functions sometimes directly pass words. We maintain the little
   endian order per ECL_XXH3 requirements. Both 32 and 64 variants are available. */
static inline void
ecl_xxh3_update32(ecl_xxh3_state *state, const uint32_t word)
{
  size_t bsize = state->buf_size;
  if (bsize <= 1024-4) {
    ecl_xxh3_write32(state->buf+bsize, word);
    state->buf_size += 4;
    state->total_len += 4;
  } else {
    uint8_t input[4];
    ecl_xxh3_write32(input, word);
    ecl_xxh3_update(state, input, 4);
  }
}

static inline void
ecl_xxh3_update64(ecl_xxh3_state *state, const uint64_t word)
{
  size_t bsize = state->buf_size;
  if (bsize <= 1024-8) {
    ecl_xxh3_write64(state->buf+bsize, word);
    state->buf_size += 8;
    state->total_len += 8;
  } else {
    uint8_t input[8];
    ecl_xxh3_write64(input, word);
    ecl_xxh3_update(state, input, 8);
  }
}

/* Keep the slow path separate to keep the fast path small for inlining. */
static uint64_t
ecl_xxh3_digest_slow(ecl_xxh3_state *state)
{
  /* INV total > 240 */
  uint64_t total = state->total_len;
  uint64_t len = state->buf_size;
  ecl_xacc xacc;
  uint64_t n, i, num, value, acc[8];
  const uint8_t *block = state->buf;
  /* Don't modify the state. */
  ecl_xxh3_ensure_acc(state, xacc);
  /* Step 3. Process the last block and the last 64 bytes */
  num = (len-1)/64;
  for (n=0; n<num; n++) {
    ecl_xxh3_64_large_acc(block+n*64, ecl_xxh3_secret+n*8, xacc);
  }
  /* The last stripe is taken from the end and may overlap. */
  if (len>=64) {
    ecl_xxh3_64_large_acc(block+len-64, ecl_xxh3_secret+192-71, xacc);
  } else {
    uint8_t rem[64];
    memcpy(rem, state->rem+len, 64-len);
    memcpy(rem+64-len, block, len);
    ecl_xxh3_64_large_acc(rem, ecl_xxh3_secret+192-71, xacc);
  }
  /* Step 4. Finalization */
  value = total*ECL_XXH_PRIME64_1;
  save_xacc(acc, xacc);
  for (i=0; i<4; i++) {
    uint64_t a = acc[i*2]   ^ ecl_xxh3_read64(ecl_xxh3_secret+11+i*16);
    uint64_t b = acc[i*2+1] ^ ecl_xxh3_read64(ecl_xxh3_secret+11+i*16+8);
    value += ecl_xxh3_mul128_fold64(a, b);
  }
  return ecl_xxh3_avalanche(value);
}

/* While slow path is separated, this is still potentially big because ecl_xxh3_64
   itself may get inlined here. That's deliberate. */
static inline uint64_t
ecl_xxh3_digest(ecl_xxh3_state *state)
{
  uint64_t total = state->total_len;
  const uint8_t *block = state->buf;
  if (total <= 240)
    return ecl_xxh3_64(block, total);
  return ecl_xxh3_digest_slow(state);
}

#endif
