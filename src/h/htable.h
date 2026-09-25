/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/* This is a header implementation of a hash table that is inspired by swiss
   hash tables that was initially implemented to improve generic function
   dispatch cache - the implementation is meant to be reused in normal hash
   tables and in specialized caches.

   The code assumes 64bit hashes (c.f. xxhash.h) -- that solution is portable
   but may perform worse on 32bit systems. Searching element in a bucket is
   currently scalar and it can be further improved with SIMD later.

   Structures of this hash table are designed to be cache friendly. The header
   fits in a single cache line as well as bucket headers. The array with bucket
   metadata is deliberately SIMD friendly.

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
#include <ecl/ecl.h>

/* Currently the entry contains a key and a value. GF dispatch considerations:

   - accessors (writers included):   entry = [class, index]
   - generic functions with arity=1: entry = [spec, method]
   - generic functions with arity>1: entry = [keys, method]

   Note that optimized test for generic functions with arity 1 already requires
   insert/lookup specialization. */

typedef struct {
  cl_object key;
  cl_object val;
} ecl_hentry;

/*
  The cache bucket is an organizational unit of the dispatch table. Each bucket
  contains 16 entries.

  [entry][entry][entry][entry]
  [entry][entry][entry][entry]
  [entry][entry][entry][entry]
  [entry][entry][entry][entry]

  Entries are densely packed to improve data locality. The important invariant
  to preserve is that entries are aligned to cache lines. On 64bit the bucket
  occupies four cache lines, and on 32bit the bucket occupies two cache lines.
*/

typedef struct {
  ecl_hentry record[16];
} ecl_bucket;

/* The header occupies exactly 64B on 64bit system and 32B on 32bit system:

   [meta] is ECL object header (with padding)

   [slot] are general-purpose entries that are at a disposal of the client. For
   hash tables we may store metadata like a generic test/hash functions while
   for generic function dispatch we may store here hot entries for quick lookup.

   [size] is the number of buckets in the table
   [fill] is the number of records in the table
   [ctrl] is an array containing bucket metadata
   [data] is an array containing actual buckets

   [meta][slot][slot][slot] [size][fill][ctrl][data]

   When the hash table is allocated it initially contains only the header.
   Generic functions that are never called or have monomorphic/bimorphic profile
   won't extend their cache beyond the header.

   It is a responsibility of the client to resize the table. The size must be
   always a power of two to allow quick index compute as index=hash&(size-1).

   size == 0 -> header
   size >= 1 -> header + buckets
*/

/* c.f _ECL_HDR */
#if ECL_FIXNUM_BITS <= 32
# define _ECL_HDR32(field1,field2) int8_t t, m, field1, field2, p1, p2, p3, p4
#else
# define _ECL_HDR32(field1,field2) int8_t t, m, field1, field2
#endif

typedef struct {
  _ECL_HDR32(test,flags);       /* padded lisp object header */
  cl_object slot[3];            /* client-dependent slots */
  cl_index size;                /* number of buckets in the table */
  cl_index fill;                /* number of records in the table */
  uint8_t *ctrl;                /* per-entry control bytes */
  ecl_bucket *buckets;          /* array containing entries */
} ecl_htable;

#define ECL_CONTROLBIT 0x80         /* denotes special meaning */
#define ECL_FINGERMASK 0x7F         /* a mask for the fingerprint  */

#define ECL_EMPTYSLOT 0x80          /* empty entry */
#define ECL_TOMBSTONE 0xfe          /* freed entry */

#if 0
# define ECL_RESERVED  0xfd          /* not used yet  */
# define ECL_SENTINEL  0xff          /* not used yet */
#endif

/* [head] stores hash fingerprints of elements. Each fingerprint is 8 bits,
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

#endif /* ECL_HTABLE_H */
