/* FILE NAME:   cspset.h

   Copyright (C) 2014 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This is part of package allocate; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License
   as published by the Free Software Foundation; either version 2, or
   (at your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Compact expandable sparse set

   DESCRIPTION: This is header file contains macros and the ANSI C
       prototype definitions for the compact expandable sparse set
       package and class for the compact sparse set package.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C/C++ compiler
       command line) before the package macros usage disables to
       fix some internal errors and errors of usage of the package.

*/


#ifndef __CSPSET__
#define __CSPSET__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

#include "allocate.h"
#include "spset.h"

/* Type of elements of the set.  */
typedef spset_elem_t cspset_elem_t;

#ifndef __cplusplus

/* Values used to mark empty and deleted elements in the set.  */
static const cspset_elem_t cspset_empty_elem = SPSET_MAX_ELEM - 1;
static const cspset_elem_t cspset_deleted_elem = SPSET_MAX_ELEM;
/* Elements are unsigned int in the range [0, cspset_max_elem].  */
static const cspset_elem_t cspset_max_elem = SPSET_MAX_ELEM - 2;

extern size_t cspset_primes[];
extern int cspset_get_prime_num (size_t, int);
extern cspset_elem_t *cspset_get_memory (int);
extern void cspset_free_memory (cspset_elem_t *, int);

#ifndef NDEBUG
/* Overall number of collisions and searches for all sets.  */
extern int cspset_all_collisions, cspset_all_searches;
#endif

/* The set representation.  */
typedef struct
{
  /* Number of prime in the package table used for the length of array
     ELEMS.  */
  int prime_num;
  /* Correspondingly number of non-empty elems in the array (it
     includes deleted elems too), number of deleted elems in the
     array.  */
  size_t all_num, del_num;
#ifndef NDEBUG
  /* The following is used for debuging: processed collisions,
     searches, and tunes so far.  */
  size_t collisions, searches, tunes;
#endif
  cspset_elem_t *elems;
} cspset_t;

/* Return set S size.  */
static inline size_t
cspset_size (cspset_t *s)
{
  return cspset_primes[s->prime_num];
}

/* Return set S elements number (non-empty, non-deleted elements).  */
static inline size_t
cspset_cardinality (cspset_t *s)
{
  return s->all_num - s->del_num;
}

/* Initialize set S with approximate SIZE.  Remember you can work only with
   initialized and non-finished sets.  */
static inline void
cspset_init (cspset_t *s, size_t size)
{
  size_t i, length;

  s->prime_num = cspset_get_prime_num (size, 0);
  s->all_num = s->del_num = s->del_num = 0;
#ifndef NDEBUG
  s->collisions = s->searches = s->tunes = 0;
#endif
  length = cspset_primes[s->prime_num];
  s->elems = cspset_get_memory (s->prime_num);
  for (i = 0; i < length; i++)
    s->elems[i] = cspset_empty_elem;
}

/* Finish set S.  */
static inline void
cspset_finish (cspset_t *s)
{
  cspset_free_memory (s->elems, s->prime_num);
}

/* Swap sets S1 and S2.  */
static inline void
cspset_swap (cspset_t *s1, cspset_t *s2)
{
  cspset_t t = *s1;

  *s1 = *s2;
  *s2 = t;
}

/* Copy set FROM to set TO.  */
static inline void
cspset_copy (cspset_t *to, cspset_t *from)
{
  size_t length;

  if (to == from)
    return;
  cspset_finish (to);
  *to = *from;
  length = cspset_primes[to->prime_num];
  to->elems = cspset_get_memory (to->prime_num);
  memcpy (to->elems, from->elems, sizeof (cspset_elem_t) * length);
}

extern void cspset_release_unused_memory (void);
extern void cspset_shrink (cspset_t *);
extern int cspset_in_p (cspset_t *, cspset_elem_t);
extern int cspset_insert (cspset_t *, cspset_elem_t);
extern int cspset_remove (cspset_t *, cspset_elem_t);
extern inline int cspset_clear (cspset_t *);
extern int cspset_equal_p (cspset_t *, cspset_t *);
extern int cspset_intersect (cspset_t *, cspset_t *);
extern int cspset_unity (cspset_t *, cspset_t *);
extern int cspset_diff (cspset_t *, cspset_t *);
extern void cspset_to_spset (cspset_t *, spset_t *);
extern void cspset_from_spset (cspset_t *, spset_t *);
extern void cspset_print (FILE *f, cspset_t *);
extern void cspset_debug (cspset_t *);

/* The set iterator.  It is used in EXECUTE_FOR_EACH_CSPSET_ELEM.  START
   is where to start next element search.  */
typedef struct { size_t start; } cspset_iterator_t;

/* Initialize the iterator SI to work with set S.  */
static inline void
cspset_iter_init (cspset_t *s, cspset_iterator_t *si)
{
  si->start = 0;
}

/* Return the next element for the iterator SI of set S.  Return
   CSPSET_EMPTY_ELEM if there is no more elements to process.  */
static inline cspset_elem_t
cspset_iter_elm (cspset_t *s, cspset_iterator_t *si)
{
  size_t i, length;
  cspset_elem_t el;

  length = cspset_primes[s->prime_num];
  for (i = si->start; i < length; i++)
    if ((el = s->elems[i]) != cspset_empty_elem && el != cspset_deleted_elem)
      {
	si->start = i + 1;
	return el;
      }
  return cspset_empty_elem;
}

/* Loop over all elements of SET using iterator SI.  Set the element
   on each iter to EL.  The order of processing elements is
   undefined.  */
#define EXECUTE_FOR_EACH_CSPSET_ELEM(SET, EL, SI)				\
  for (cspset_iter_init ((SET), &(SI));						\
       ((EL) = cspset_iter_elm ((SET), &(SI))) != cspset_empty_elem;)

#else

/* The set representation.  */
class cspset
{
  friend class cspset_iterator;
  /* Number of prime in the package table used for the length of array
     ELEMS.  */
  int prime_num;
  /* Correspondingly number of non-empty elems in
     the array (it includes deleted elems too), number of deleted
     elems in the array.  */
  size_t all_num, del_num;
#ifndef NDEBUG
  /* The following is used for debuging: processed collisions,
     searches, and tunes so far.  */
  size_t collisions, searches, tunes;
#endif
  cspset_elem_t *elems;

  /* Values used to mark empty and deleted elements in the set.  */
  static const cspset_elem_t empty_elem = SPSET_MAX_ELEM - 1;
  static const cspset_elem_t deleted_elem = SPSET_MAX_ELEM;

  static size_t primes[];
  static cspset_elem_t *mem_pool[];
  static int get_prime_num (size_t, int);
  static cspset_elem_t *get_memory (int);
  static void free_memory (cspset_elem_t *, int);

  int optimal_size (void);
  void tune_size (size_t);

 public:

#ifndef NDEBUG
  /* Overall number of collisions and searches for all sets.  */
  static int all_collisions, all_searches;
#endif

  /* Elements are unsigned int in the range [0, cspset_max_elem].  */
  static const cspset_elem_t max_elem = SPSET_MAX_ELEM - 2;

  /* Constructor: set with approximate SIZE.  */
  cspset (size_t size = 0)
    {
      size_t i, length;
      
      prime_num = get_prime_num (size, 0);
      length = primes[prime_num];
      all_num = del_num = del_num = 0;
#ifndef NDEBUG
      collisions = searches = tunes = 0;
#endif
      elems = get_memory (prime_num);
      for (i = 0; i < length; i++)
	elems[i] = empty_elem;
    }

  /* Destructor.  */
  ~cspset (void) { free_memory (elems, prime_num); }

  /* The following functions allocate/free memory for set
     descriptor. */
  void *operator new (size_t size) { return allocate::malloc (size); }
  void operator delete (void *mem) { allocate::free (mem); }
  void *operator new[] (size_t size) { return allocate::malloc (size); }
  void operator delete[] (void *mem) { allocate::free (mem); }

  /* Return set size.  */
  inline size_t size (void) { return primes[prime_num]; }
  /* Return set elements number (non-empty, non-deleted elements).  */
  inline size_t cardinality (void) { return all_num - del_num; }

  /* Swap with set S.  */
  inline void swap (cspset *s)
  {
    int ti;
    size_t t;
    cspset_elem_t *tel;
    
    ti = s->prime_num; s->prime_num = prime_num; prime_num = ti;
    t = s->all_num; s->all_num = all_num; all_num = t;
    t = s->del_num; s->del_num = del_num; del_num = t;
#ifndef NDEBUG
    t = s->collisions; s->collisions = collisions; collisions = t;
    t = s->searches; s->searches = searches; searches = t;
    t = s->tunes; s->tunes = tunes; tunes = t;
#endif
    tel = s->elems; s->elems = elems; elems = tel;
  }

  /* Copy set FROM to the set.  */
  inline void copy (cspset *from)
  {
    size_t length;

    if (this == from)
      return;
    free_memory (elems, prime_num);
    prime_num = from->prime_num; all_num = from->all_num; del_num = from->del_num;
#ifndef NDEBUG
    collisions = from->collisions; searches = from->searches;
    tunes = from->tunes;
#endif
    length = primes[prime_num];
    elems = get_memory (prime_num);
    memcpy (elems, from->elems, sizeof (cspset_elem_t) * length);
  }

  static void release_unused_memory (void);
  void shrink (void);
  int in_p (cspset_elem_t);
  int insert (cspset_elem_t);
  int remove (cspset_elem_t);
  int clear (void);
  int equal_p (cspset *);
  int intersect (cspset *);
  int unity (cspset *);
  int diff (cspset *);
  void to_spset (spset *);
  void from_spset (spset *);
  void print (FILE *f);
  void debug (void);

 private:
  cspset_elem_t *find (cspset_elem_t, int);
  void tune_size (void);
  /* Prevent copy constructor and assignment */
  cspset (const cspset&) {}
  cspset& operator=(const cspset&) {}
};


/* The set iterator.  It is used in EXECUTE_FOR_EACH_CSPSET_ELEM.  START
   is where to start next element search.  */
class cspset_iterator
{
  friend class cspset;
  cspset *s;
  size_t start;

 public:
  /* Initialize the iterator to work with set S.  */
  inline void iter_init (cspset *ws) { s = ws; start = 0; }

  inline cspset_elem_t iter_stop_elem (void) { return cspset::empty_elem; }

  /* Return the next element for the iterator.  Return the stop
     element if there is no more elements to process.  */
  inline cspset_elem_t iter_elem (void)
  {
    size_t i, length;
    cspset_elem_t el;
    
    length = cspset::primes[s->prime_num];
    for (i = start; i < length; i++)
      if ((el = s->elems[i]) != cspset::empty_elem && el != cspset::deleted_elem)
	{
	  start = i + 1;
	  return el;
	}
    return iter_stop_elem ();
  }
};

/* Loop over all elements of SET using iterator SI.  Set the element
   on each iter to EL.  The order of processing elements is
   undefined.  */
#define EXECUTE_FOR_EACH_CSPSET_ELEM(SET, EL, SI)		\
  for ((SI).iter_init ((SET)); ((EL) = (SI).iter_elem ()) != (SI).iter_stop_elem ();)

#endif

#endif /* #ifndef __CSPSET__ */
