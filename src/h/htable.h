/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/* This is a header implementation of a hash table that is inspired by swiss
   hash tables that was initially implemented to improve generic function
   dispatch cache - the implementation is meant to be reused in normal hash
   tables and in specialized caches.

   The code assumes 64bit hashes (c.f. xxhash.h) -- that solution is portable
   but may perform worse on 32bit systems. Lookup is currently scalar and it can
   be further improved with SIMD later.

   Structures of this hash table are designed to be cache friendly. The header
   fits in a single cache line. The array with entry metadata is deliberately
   SIMD friendly.

   FIXME currently we rely on allocator to correctly align structures, but later
   introduce ECL_ALIGNAS and double-check with the garbage collector.

   FIXME concurrent access is currently not addressed and synchronous tables
   take an rw-lock over whole hash table. After finishing general table design I
   want to implement lock-less thread-safe variants. They are not needed for
   generic function dispatch cache, because these caches are thread-local.

   FIXME currently the implementation focuses 64bit platforms (it will work on
   32bit, but structures may be not cache-line aligned). Unless stated otherwise
   all comments and diagrams are written with 64bit in mind.

   -- jd 2026-09-24 */

#ifndef ECL_HTABLE_H
#define ECL_HTABLE_H

#include <stdint.h>

/* Currently the entry contains a key and a value. GF dispatch considerations:

   - accessors (writers included):   entry = [class, index]
   - generic functions with arity=1: entry = [spec, method]
   - generic functions with arity>1: entry = [keys, method]

   Note that optimized test for generic functions with arity 1 already requires
   insert/lookup specialization.

   A bucket is an implicit logical unit of the dispatch table. Each bucket
   contains 16 entries.

   [entry][entry][entry][entry]
   [entry][entry][entry][entry]
   [entry][entry][entry][entry]
   [entry][entry][entry][entry]

   Entries are densely packed to improve data locality. The important invariant
   to preserve is that entries cleanly fit in a cache line. On 64bit the bucket
   occupies four cache lines, and on 32bit the bucket occupies two cache lines.

*/

typedef struct {
  cl_object key;
  cl_object val;
} ecl_hentry;

/* The header occupies exactly 64B on 64bit system and 32B on 32bit system:

   [head] table header packing test, weakness, size and rehash properties
   [slot] general-purpose entries that are at a disposal of the client. For hash
          tables we may store metadata like a generic test/hash, while for cache
          we may store hot entries for quick lookup (gf dispatch).

   [fill] is the number of records in the table
   [left] is the number of free spots left before growing
   [ctrl] is an array containing record metadata
   [data] is an array containing actual records

   [head][slot][slot][slot] [fill][left][ctrl][data]

   When the hash table is allocated it initially contains only the header.
   Generic functions that are never called or have monomorphic profile won't
   extend their cache beyond the header.

   It is a responsibility of the client to resize the table. The size must be
   always a power of two to allow quick index compute as index=hash&(size-1).

   ctrl == NULL -> header
   ctrl != NULL -> header -> *ctrl, *data

   The table is expected to have a wrapper to automatically swap the pointer
   when we rehash the table without confusing clients with inconsistent state.
*/

/* Compare with _ECL_HDR. Here we ensure that the header is aligned to a single
   word boundary. We fit size and rehash values in uint8_t to make room slots in
   the header.

   test, weak -- ecl_httest and ecl_htweak enum values
   sizepw -- represents the hash table capacity (2^sizepw)
   rehash -- ecl_htsize_rehash | ecl_htsize_threshold

   There is also an important invariant about sizepw. To make probing the table
   with entries easier, when data is allocated then always sizepw>=4. Invariant
   is important to scan entries in groups of 16. General purpose hash tables
   will always have sizepw>=4, while gf-caches, when empty, can assign here a
   lower value. */

#if ECL_FIXNUM_BITS <= 32
# define _ECL_HTT_HDR \
  uint8_t test, weak, sizepw, rehash
#else
# define _ECL_HTT_HDR \
  uint8_t test, weak, sizepw, rehash, padding[4]
#endif

typedef struct {
  _ECL_HTT_HDR;
  cl_object slot[3];            /* client-dependent slots */
  cl_index fill;                /* #records in the table (no tombstones) */
  cl_index left;                /* #slots available before resize */
  uint8_t *ctrl;                /* per-entry control bytes */
  ecl_hentry *data;             /* array containing entries */
} ecl_htable;

/* Accessors for general-purpose hash tables. */
#define ecl_htable_generic_hash(ht) ((ht)->slot[0])
#define ecl_htable_generic_test(ht) ((ht)->slot[1])

/* rehash represents rehash-size and rehash-threshold tightly packed in uint8_t:

 - two lowest bits represent the rehash-size (x2, x4, x8, x16)
 - six higher bits represent the threshold (1-64 scaled to (0,1]) */
#define ecl_htable_rehash_size(ht)      (2 << (((ht)->rehash) & 3))
#define ecl_htable_rehash_threshold(ht) ((((ht)->rehash>>2)+1)/64.0)

/* In practice we will exhaust the memory much earlier. This maximum is defined
   to avoid UB when we shift bits. */
#define ECL_HTABLE_MAX_SIZEPW (ECL_FIXNUM_BITS-6)

#define ECL_CONTROLBIT 0x80         /* denotes special meaning */
#define ECL_FINGERMASK 0x7F         /* a mask for the fingerprint  */

#define ECL_EMPTYSLOT 0x80          /* empty entry */
#define ECL_TOMBSTONE 0xfe          /* freed entry */

#if 0
# define ECL_RESERVED  0xfd          /* not used yet  */
# define ECL_SENTINEL  0xff          /* not used yet */
#endif

/* [ctrl] stores hash fingerprints of elements. Each fingerprint is 8 bits,
   where 1b is reserved to signify a special state.

   ------------------------------------------
   #b0xxx xxxx :: occupied (7bit fingerprint)
   #b1xxx xxxx :: special states        [vvv]
   ------------------------------------------
   #b1000 0000 :: empty
   #b1111 1110 :: tombstone
   #b1111 1101 :: reserved     (not used yet)
   #b1111 1111 :: sentinel     (not used yet)
   ------------------------------------------

   When computing the fingerprint, we want to take highest bits to avoid
   collisions with indexes. We hash everything xxh3 to ensure sufficient mixing.
*/

static inline uint8_t
ecl_hash_fingerprint(uint64_t hash)
{
  return (hash >> 57) & ECL_FINGERMASK;
}

static inline cl_index
ecl_hash_group_index(uint64_t hash, cl_index size)
{
  return (cl_index)((hash & (size-1)) & ~(cl_index)15);
}

#endif /* ECL_HTABLE_H */
