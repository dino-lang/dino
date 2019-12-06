/* This file is a part of DINO project.

  Copyright (C) 2018, 2019 Vladimir Makarov <vmakarov.gcc@gmail.com>.
*/

/* Simple high-quality multiplicative hash passing demerphq-smhsher,
   faster than spooky, city, or xxhash for strings less 100 bytes.
   Hash for the same key can be different on different architectures.
   To get machine-independent hash, use dino_hash_strict which is about
   1.5 times slower than dino_hash.  */
#ifndef __DINO_HASH__
#define __DINO_HASH__

#include <stddef.h>
#include <stdint.h>

#if defined(__x86_64__) || defined(__i386__) || defined(__PPC64__) || defined(__s390__) \
  || defined(__m32c__) || defined(cris) || defined(__CR16__) || defined(__vax__)        \
  || defined(__m68k__) || defined(__aarch64__) || defined(_M_AMD64) || defined(_M_IX86)
#define DINO_HASH_UNALIGNED_ACCESS 1
#else
#define DINO_HASH_UNALIGNED_ACCESS 0
#endif

static inline uint64_t dino_get_key_part (const uint8_t *v, size_t len, int relax_p) {
  size_t i, start = 0;
  uint64_t tail = 0;

  if (relax_p || __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__) {
#if DINO_HASH_UNALIGNED_ACCESS
    if (len == sizeof (uint64_t)) return *(uint64_t *) v;
    if (len >= sizeof (uint32_t)) {
      tail = (uint64_t) * (uint32_t *) v << 32;
      start = 4;
    }
#endif
  }
  for (i = start; i < len; i++) tail = (tail >> 8) | ((uint64_t) v[i] << 56);
  return tail;
}

static const uint64_t p1 = 0X65862b62bdf5ef4d, p2 = 0X288eea216831e6a7;
static inline uint64_t dino_mum (uint64_t v, uint64_t c, int relax_p) {
  if (relax_p) {
#if defined(__SIZEOF_INT128__)
    __uint128_t r = (__uint128_t) v * (__uint128_t) c;
    return (uint64_t) (r >> 64) + (uint64_t) r;
#endif
  }
  uint64_t v1 = v >> 32, v2 = (uint32_t) v, c1 = c >> 32, c2 = (uint32_t) c, rm = v2 * c1 + v1 * c2;
  return v1 * c1 + (rm >> 32) + v2 * c2 + (rm << 32);
}

static inline uint64_t dino_round (uint64_t state, uint64_t v, int relax_p) {
  state ^= dino_mum (v, p1, relax_p);
  return state ^ dino_mum (state, p2, relax_p);
}

static inline uint64_t dino_hash_1 (const void *key, size_t len, uint64_t seed, int relax_p) {
  const uint8_t *v = (const uint8_t *) key;
  uint64_t r = seed + len;

  for (; len >= 16; len -= 16, v += 16) {
    r ^= dino_mum (dino_get_key_part (v, 8, relax_p), p1, relax_p);
    r ^= dino_mum (dino_get_key_part (v + 8, 8, relax_p), p2, relax_p);
    r ^= dino_mum (r, p1, relax_p);
  }
  if (len >= 8) {
    r ^= dino_mum (dino_get_key_part (v, 8, relax_p), p1, relax_p);
    len -= 8, v += 8;
  }
  if (len != 0) r ^= dino_mum (dino_get_key_part (v, len, relax_p), p2, relax_p);
  return dino_round (r, r, relax_p);
}

static inline uint64_t dino_hash (const void *key, size_t len, uint64_t seed) {
  return dino_hash_1 (key, len, seed, 1);
}

static inline uint64_t dino_hash_strict (const void *key, size_t len, uint64_t seed) {
  return dino_hash_1 (key, len, seed, 0);
}

static inline uint64_t dino_hash_init (uint64_t seed) { return seed; }
static inline uint64_t dino_hash_step (uint64_t h, uint64_t key) { return dino_round (h, key, 1); }
static inline uint64_t dino_hash_finish (uint64_t h) { return dino_round (h, h, 1); }

static inline uint64_t dino_hash64 (uint64_t key, uint64_t seed) {
  return dino_hash_finish (dino_hash_step (dino_hash_init (seed), key));
}

#endif
