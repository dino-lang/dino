/* FILE NAME:   spset.h

   Copyright (C) 2014 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

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

   TITLE:      Expandable sparse set

   DESCRIPTION: This is header file contains macros and the ANSI C
       prototype definitions for the expandable sparse set package and
       class for the sparse set package.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C/C++ compiler
       command line) before the package macros usage disables to
       fix some internal errors and errors of usage of the package.

*/


#ifndef __SPSET__
#define __SPSET__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <assert.h>
#include <limits.h>

#include "allocate.h"

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/* Type of elements of the set.  */
typedef unsigned int spset_elem_t;
/* Maximum value of spset_elem_t type.  */
#define SPSET_MAX_ELEM UINT_MAX

#ifndef __cplusplus

extern spset_elem_t spset_sizes[];
extern int spset_get_size_num (spset_elem_t, int);
extern spset_elem_t *spset_get_memory (int);
extern void spset_free_memory (spset_elem_t *, int);

typedef struct
{
  /* Number of the size in the package table used for the length of array
     ELEMS.  */
  int size_num;
  /* Maximum number and current number of set elements.  */
  spset_elem_t els_num;
  /* sparse els contain indexes in dense, dense els are actual set
     elements.  */
  spset_elem_t *dense, *sparse;
} spset_t;

/* Return set S size.  */
static inline spset_elem_t
spset_size (spset_t *s)
{
  return spset_sizes[s->size_num];
}

/* Return set S elements number.  */
static inline spset_elem_t
spset_cardinality (spset_t *s)
{
  return s->els_num;
}

/* Initialize set S with size > SIZE.  Remember you can work only
   with initialized and non-finished sets.  */
static inline void
spset_init (spset_t *s, spset_elem_t size)
{
  s->size_num = spset_get_size_num (size, 0);
  s->els_num = 0;
  s->dense = spset_get_memory (s->size_num);
  s->sparse = spset_get_memory (s->size_num);
}

/* Finish set S.  */
static inline void
spset_finish (spset_t *s)
{
  spset_free_memory (s->dense, s->size_num);
  spset_free_memory (s->sparse,s->size_num);
}

/* Swap sets S1 and S2.  */
static inline void
spset_swap (spset_t *s1, spset_t *s2)
{
  spset_t t = *s1;

  *s1 = *s2;
  *s2 = t;
}

/* Copy set FROM to set TO.  */
static inline void
spset_copy (spset_t *to, spset_t *from)
{
  spset_elem_t size;
  if (to == from)
    return;
  spset_finish (to);
  *to = *from;
  to->dense = spset_get_memory (to->size_num);
  to->sparse = spset_get_memory (to->size_num);
  size = spset_sizes [to->size_num];
  memcpy (to->dense, from->dense, sizeof (spset_elem_t) * size);
  memcpy (to->sparse, from->sparse, sizeof (spset_elem_t) * size);
}

/* Return TRUE if EL in S.  */
static inline int
spset_in_p (spset_t *s, spset_elem_t el)
{
  spset_elem_t n;
  spset_elem_t size = spset_sizes[s->size_num];

  assert (el >= 0);
  if (size <= el)
    return FALSE;
  n = s->sparse[el];
  return n < s->els_num && s->dense[n] == el;
}

/* Remove EL from S.  Return TRUE if it was there, in order words,
   if the set has been changed.  */
static int inline
spset_remove (spset_t *s, spset_elem_t el)
{
  spset_elem_t n;
  spset_elem_t last_ind;
  spset_elem_t last;
  spset_elem_t size = spset_sizes[s->size_num];

  if (! spset_in_p (s, el) || el >= size)
    return FALSE;
  n = s->sparse[el];
  last_ind = s->els_num - 1;
  last = s->dense[last_ind];
  s->sparse[last] = n;
  s->dense[n] = last;
  s->els_num = last_ind;
  return TRUE;
}

/* Make S empty.  Return TRUE if it was not empty, in order words, if
   the set has been changed.  */
static inline int
spset_clear (spset_t *s)
{
  int res = s->els_num != 0;
  s->els_num = 0;
  return res;
}

extern void spset_release_unused_memory (void);
extern void spset_shrink (spset_t *);
extern int spset_insert (spset_t *, spset_elem_t);
extern int spset_equal_p (spset_t *, spset_t *);
extern int spset_intersect (spset_t *, spset_t *);
extern int spset_unity (spset_t *, spset_t *);
extern int spset_diff (spset_t *, spset_t *);
extern void spset_print (FILE *f, spset_t *);
extern void spset_debug (spset_t *);

/* The set iterator.  It is used in EXECUTE_FOR_EACH_SPSET_ELEM.
   START is where to start next element search.  */
typedef struct { spset_t *s; spset_elem_t start; } spset_iterator_t;

/* Initialize the iterator SI to work with set S.  */
static inline void
spset_iter_init (spset_iterator_t *si, spset_t *s)
{
  si->start = 0;
  si->s = s;
}

/* Return true if we are not finished yet with work with iterator
   SI.  */
static inline int
spset_iter_p (spset_iterator_t *si)
{
  return si->start < si->s->els_num;
}

/* Return the next element for the iterator SI.  */
static inline spset_elem_t
spset_iter_elm (spset_iterator_t *si)
{
  return si->s->dense[si->start++];
}

/* Loop over all elements of SET using iterator SI.  Set the element
   on each iter to EL.  The order of processing elements is
   undefined.  */
#define EXECUTE_FOR_EACH_SPSET_ELEM(SET, EL, SI)			\
  for (spset_iter_init (&(SI), (SET));					\
       spset_iter_p (&(SI))						\
       && (((EL) = spset_iter_elm (&(SI))) || 1);)

#else

class spset
{
  friend class spset_iterator;

  /* Number of the size in the package table used for the length of array
     ELEMS.  */
  int size_num;
  /* Maximum number and current number of set elements.  */
  spset_elem_t els_num;
  /* sparse els contain indexes in dense, dense els are actual set
     elements.  */
  spset_elem_t *dense, *sparse;

  static spset_elem_t sizes[];
  static spset_elem_t *mem_pool[];
  static int get_size_num (spset_elem_t, int);
  static spset_elem_t *get_memory (int);
  static void free_memory (spset_elem_t *, int);

  void tune_size (int);

public:

  /* Constructor of set of SIZE.  */
  spset (spset_elem_t size = 0)
  {
    spset_elem_t actual_size;

    size_num = get_size_num (size, 0);
    els_num = 0;
    dense = get_memory (size_num);
    sparse = get_memory (size_num);
  }

  /* Destructor.  */
  ~spset (void)
  {
    free_memory (dense, size_num);
    free_memory (sparse, size_num);
  }

  /* The following two functions allocate/free memory for set
     descriptor. */
  void *operator new (size_t size) { return allocate::malloc (size); }
  void operator delete (void *mem) { allocate::free (mem); }
  void *operator new[] (size_t size) { return allocate::malloc (size); }
  void operator delete[] (void *mem) { allocate::free (mem); }

  /* Return the set size.  */
  inline spset_elem_t size (void)
  {
    return sizes[size_num];
  }

  /* Return set elements number.  */
  inline spset_elem_t cardinality (void)
  {
    return els_num;
  }

  /* Swap with set S.  */
  inline void swap (spset *s)
  {
    int ti;
    spset_elem_t tel;
    spset_elem_t *ptel;
    
    ti = s->size_num; s->size_num = size_num; size_num = ti;
    tel = s->els_num; s->els_num = els_num; els_num = tel;
    ptel = s->dense; s->dense = dense; dense = ptel;
    ptel = s->sparse; s->sparse = sparse; sparse = ptel;
  }

  /* Copy from set FROM.  */
  inline void copy (spset *from)
  {
    spset_elem_t size;

    if (this == from)
      return;
    free_memory (dense, size_num);
    free_memory (sparse, size_num);
    size_num = from->size_num;
    els_num = from->els_num;
    dense = get_memory (size_num);
    sparse = get_memory (size_num);
    size = sizes [size_num];
    memcpy (dense, from->dense, sizeof (spset_elem_t) * size);
    memcpy (sparse, from->sparse, sizeof (spset_elem_t) * size);
  }

  /* Return TRUE if EL in the set.  */
  inline int in_p (spset_elem_t el)
  {
    spset_elem_t n;
    spset_elem_t size = sizes [size_num];

    assert (el >= 0);
    if (size <= el)
      return FALSE;
    n = sparse[el];
    return n < els_num && dense[n] == el;
  }

  /* Remove EL from the set.  Return TRUE if it was there, in order
     words, if the set has been changed.  */
  int inline remove (spset_elem_t el)
  {
    spset_elem_t n;
    spset_elem_t last_ind;
    spset_elem_t last;
    spset_elem_t size = sizes [size_num];

    if (! in_p (el) || el >= size)
      return FALSE;
    n = sparse[el];
    last_ind = els_num - 1;
    last = dense[last_ind];
    sparse[last] = n;
    dense[n] = last;
    els_num = last_ind;
    return TRUE;
  }

  /* Make the set empty.  Return TRUE if it was not empty, in order
     words, if the set has been changed.  */
  inline int clear (void)
  {
    int res = els_num != 0;
    els_num = 0;
    return res;
  }

  static void release_unused_memory (void);
  void shrink (void);
  int insert (spset_elem_t);
  int equal_p (spset *);
  int intersect (spset *);
  int unity (spset *);
  int diff (spset *);
  void print (FILE *f);
  void debug (void);

 private:
  /* Prevent copy constructor and assignment */
  spset (const spset&);
  spset& operator=(const spset&);
};

/* The set iterator.  It is used in EXECUTE_FOR_EACH_SPSET_ELEM.
   START is where to start next element search.  */
class spset_iterator
{
  friend class spset;
  spset *s;
  spset_elem_t start;
 public:
  /* Initialize the iterator to work with set S.  */
  inline void iter_init (spset *set) { start = 0; s = set; }
  /* Return true if we are not finished yet with work with the
     iterator.  */
  inline int iter_p (void) { return start < s->els_num; }
  /* Return the next element for the iterator.  */
  inline spset_elem_t iter_elm (void) { return s->dense[start++];}
};

/* Loop over all elements of SET using iterator SI.  Set the element
   on each iter to EL.  The order of processing elements is
   undefined.  */
#define EXECUTE_FOR_EACH_SPSET_ELEM(SET, EL, SI)			\
  for ((SI).iter_init ((SET)); (SI).iter_p () && (((EL) = (SI).iter_elm ()) || 1);)

#endif

#endif /* #ifndef __SPSET__ */
