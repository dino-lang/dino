/* This file is a part of Dino project.
   Copyright (C) 2018, 2019 Vladimir Makarov <vmakarov.gcc@gmail.com>.
*/

#ifndef MIR_MP_H
#define MIR_MP_H

#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <assert.h>

#if !defined(MP_ENABLE_CHECKING) && !defined(NDEBUG)
#define MP_ENABLE_CHECKING
#endif

#ifndef MP_ENABLE_CHECKING
#define MP_ASSERT(EXPR, OP, T) ((void) (EXPR))

#else
static inline void mir_mp_assert_fail (const char *op, const char *var) {
  fprintf (stderr, "wrong %s for %s", op, var);
  assert (0);
}

#define MP_ASSERT(EXPR, OP, T) (void) ((EXPR) ? 0 : (mir_mp_assert_fail (OP, #T), 0))

#endif

#ifdef __GNUC__
#define MIR_MP_NO_RETURN __attribute__ ((noreturn))
#else
#define MIR_MP_NO_RETURN
#endif

static inline void MIR_MP_NO_RETURN mir_mp_error (const char *message) {
#ifdef MIR_MP_ERROR
  MIR_MP_ERROR (message);
  assert (0);
#else
  fprintf (stderr, "%s\n", message);
#endif
  exit (1);
}

/*--------------- Memory pool with stack style alloc/free ------------------------*/

#include "d_dlist.h"

#define MP_USE_STD_MALLOC

#ifndef MP_DEFAULT_CHUNK_SIZE
#define MP_DEFAULT_CHUNK_SIZE 16000
#endif

#define MP_MAX_ALIGNMENT 8

typedef struct mp_chunk *mp_chunk_t;

DEF_DLIST_LINK (mp_chunk_t);

struct mp_chunk {
  size_t size;
  DLIST_LINK (mp_chunk_t) chunk_link;
#ifndef MP_USE_STD_MALLOC
  void *free;
#endif
  _Alignas(MP_MAX_ALIGNMENT) char start[1];
};

DEF_DLIST (mp_chunk_t, chunk_link);

typedef struct mp_s *mp_t;

struct mp_s { /* memory pool */
  void *(*mp_malloc) (mp_t, size_t);
  void *(*mp_realloc) (mp_t, void *, size_t);
  void (*mp_free) (mp_t, void *);
  DLIST (mp_chunk_t) chunks;
#ifndef MP_USE_STD_MALLOC
  mp_chunk_t curr_chunk;
  size_t default_chunk_size;
#endif
  size_t alloc_size, max_alloc_size;
};

#define MP_MALLOC(MP, S) ((MP)->mp_malloc (MP, S))
#define MP_REALLOC(MP, OBJ, S) ((MP)->mp_realloc (MP, OBJ, S))
#define MP_FREE(MP, OBJ) ((MP)->mp_free (MP, OBJ))

#ifdef MP_USE_STD_MALLOC
static inline mp_chunk_t get_chunk (void *obj) {
  return (void *) ((char *) obj - offsetof (struct mp_chunk, start));
}
#endif

static inline size_t mp_get_size (void *obj) {
#ifdef MP_USE_STD_MALLOC
  return get_chunk (obj)->size;
#else
  return *(size_t *) ((char *) obj - MP_MAX_ALIGNMENT);
#endif
}

#ifdef MP_USE_STD_MALLOC
static inline void *mp_malloc (mp_t mp, size_t size) {
  size_t new_size = sizeof (struct mp_chunk) - 1 + size;
  mp_chunk_t chunk = malloc (new_size);

  chunk->size = new_size;
  DLIST_APPEND (mp_chunk_t, mp->chunks, chunk);
  mp->alloc_size += new_size;
  if (mp->max_alloc_size < mp->alloc_size) mp->max_alloc_size = mp->alloc_size;
  return chunk->start;
}
#else
static inline void *mp_malloc (mp_t mp, size_t size) {
  void *obj;
  size_t new_size = ((size + MP_MAX_ALIGNMENT - 1) / MP_MAX_ALIGNMENT + 1) * MP_MAX_ALIGNMENT;
  mp_chunk_t chunk = mp->curr_chunk;

  assert (MP_DEFAULT_CHUNK_SIZE >= 0 && MP_MAX_ALIGNMENT >= sizeof (size_t));
  if (chunk == NULL || (char *) chunk->free + new_size > chunk->start + chunk->size) {
    size_t s, chunk_size = new_size > MP_DEFAULT_CHUNK_SIZE ? new_size : MP_DEFAULT_CHUNK_SIZE;

    s = sizeof (struct mp_chunk) - 1 + chunk_size;
    chunk = chunk = malloc (s);
    assert ((size_t) chunk->start % MP_MAX_ALIGNMENT == 0);
    chunk->size = chunk_size;
    chunk->free = chunk->start;
    if (mp->curr_chunk == NULL)
      DLIST_APPEND (mp_chunk_t, mp->chunks, chunk);
    else
      DLIST_INSERT_AFTER (mp_chunk_t, mp->chunks, mp->curr_chunk, chunk);
    mp->curr_chunk = chunk;
    mp->alloc_size += s;
    if (mp->max_alloc_size < mp->alloc_size) mp->max_alloc_size = mp->alloc_size;
  }
  *(size_t *) chunk->free = size;
  obj = (char *) chunk->free + MP_MAX_ALIGNMENT;
  chunk->free = (char *) chunk->free + new_size;
  return obj;
}
#endif

static inline void mp_free (mp_t mp, void *obj) { /* nothing */
}

#ifdef MP_USE_STD_MALLOC
static inline void *mp_realloc (mp_t mp, void *obj, size_t new_size) {
  mp_chunk_t before, chunk = get_chunk (obj);
  size_t size = mp_get_size (obj);

  before = DLIST_PREV (mp_chunk_t, chunk);
  DLIST_REMOVE (mp_chunk_t, mp->chunks, chunk);
  mp->alloc_size -= size;
  mp->alloc_size += new_size;
  if (mp->max_alloc_size < mp->alloc_size) mp->max_alloc_size = mp->alloc_size;
  new_size += sizeof (struct mp_chunk) - 1;
  chunk = realloc (chunk, new_size);
  if (before == NULL)
    DLIST_PREPEND (mp_chunk_t, mp->chunks, chunk);
  else
    DLIST_INSERT_AFTER (mp_chunk_t, mp->chunks, before, chunk);
  return chunk->start;
}
#else
static inline void *mp_realloc (mp_t mp, void *obj, size_t new_size) {
  size_t size = mp_get_size (obj);
  void *new_obj;

  if (size >= new_size) return obj;
  new_obj = MP_MALLOC (mp, new_size);
  memcpy (new_obj, obj, size);
  MP_FREE (mp, obj);
  return new_obj;
}
#endif

static inline mp_t mp_create (size_t default_size) {
  mp_t mp = malloc (sizeof (struct mp_s));

  mp->mp_malloc = mp_malloc;
  mp->mp_realloc = mp_realloc;
  mp->mp_free = mp_free;
  DLIST_INIT (mp_chunk_t, mp->chunks);
#ifndef MP_USE_STD_MALLOC
  mp->curr_chunk = NULL;
  mp->default_chunk_size = default_size != 0 ? default_size : MP_DEFAULT_CHUNK_SIZE;
#endif
  mp->alloc_size = mp->max_alloc_size = sizeof (struct mp_s);
  return mp;
}

static inline void *mp_mark (mp_t mp) {
#ifdef MP_USE_STD_MALLOC
  return DLIST_TAIL (mp_chunk_t, mp->chunks);
#else
  return mp->curr_chunk == NULL ? NULL : mp->curr_chunk->free;
#endif
}

#ifdef MP_USE_STD_MALLOC
static inline void mp_pop (mp_t mp, void *mark) {
  mp_chunk_t chunk, prev_chunk;

  for (chunk = DLIST_TAIL (mp_chunk_t, mp->chunks); chunk != mark; chunk = prev_chunk) {
    prev_chunk = DLIST_PREV (mp_chunk_t, chunk);
    DLIST_REMOVE (mp_chunk_t, mp->chunks, chunk);
    mp->alloc_size -= sizeof (struct mp_chunk) - 1 + chunk->size;
    free (chunk);
  }
  if (chunk == NULL && mark != NULL) mir_mp_error ("wrong mp_mark/mp_pop order");
}
#else
static inline void mp_pop (mp_t mp, void *mark) {
  if (mp->curr_chunk == NULL) {
    assert (mark == NULL);
    return;
  } else if (mark == NULL) {
    mp->curr_chunk = DLIST_HEAD (mp_chunk_t, mp->chunks);
    mp->curr_chunk->free = mp->curr_chunk->start;
    return;
  }
  for (mp_chunk_t chunk = mp->curr_chunk; chunk != NULL; chunk = DLIST_PREV (mp_chunk_t, chunk))
    if (chunk->start <= (char *) mark && (char *) mark <= (char *) chunk->free) {
      mp->curr_chunk = chunk;
      chunk->free = mark;
      return;
    }
  mir_mp_error ("wrong mp_mark/mp_pop order");
}
#endif

static inline void mp_tailor (mp_t mp) {
#ifndef MP_USE_STD_MALLOC
  mp_chunk_t chunk, next_chunk;

  if ((chunk = mp->curr_chunk) == NULL) return;
  for (chunk = DLIST_NEXT (mp_chunk_t, chunk); chunk != NULL; chunk = next_chunk) {
    next_chunk = DLIST_NEXT (mp_chunk_t, chunk);
    DLIST_REMOVE (mp_chunk_t, mp->chunks, chunk);
    mp->alloc_size -= chunk->size + sizeof (struct mp_chunk) - 1;
    free (chunk);
  }
#endif
}

static inline size_t mp_curr_alloc_size (mp_t mp) { return mp->alloc_size; }
static inline size_t mp_max_alloc_size (mp_t mp) { return mp->max_alloc_size; }

static inline void mp_destroy (mp_t mp) {
  mp_pop (mp, NULL);
  mp_tailor (mp);
#ifndef MP_USE_STD_MALLOC
  if (mp->curr_chunk != NULL) {
    assert (DLIST_PREV (mp_chunk_t, mp->curr_chunk) == NULL
            && DLIST_NEXT (mp_chunk_t, mp->curr_chunk) == NULL);
    free (mp->curr_chunk);
  }
#endif
  free (mp);
}

#endif /* #ifndef MIR_MP_H */
