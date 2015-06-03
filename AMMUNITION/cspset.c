/* FILE NAME:   cspset.c

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

   TITLE:       Compact expandable sparse set package

   DESCRIPTION: This file implements the compact expandable sparse set
       package functions.

   IMPLEMENTATION: We use special case hash tables of appropriate
       sizes to keep the tables compact but still fast for access.
       The major differenecs from the sparsesets are:
         o the tables are more compact when sets are sparse.
         o the tables are slower than sparsets.
       So compact sets are suitable when you need to keep many such
       sets, e.g. for program data flow problems.
*/


#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */

#include "cspset.h"

/* Prime numbers used for set sizes.  Each one is about 2 times of the
   previous one.  */
size_t cspset_primes [] = {7, 17, 37, 79, 163, 331, 673, 1361, 2729,
			   5471, 10949, 21911, 43853, 87719, 175447,
			   350899, 701819, 1403641, 2807303, 5614657,
			   11229331, 22458671, 44917381, 89834777,
			   179669557, 359339171, 718678369, 1437356741};

/* Memory pool for sparse and dense arrays.  */
static spset_elem_t *mem_pool[sizeof (cspset_primes) / sizeof (size_t)];

/* Allocate and return an array of PRIME_NUM.  */
cspset_elem_t *
cspset_get_memory (int prime_num)
{
  cspset_elem_t *arr;

  assert (sizeof (cspset_elem_t) * cspset_primes[0] >= sizeof (cspset_elem_t *));
  assert (0 <= prime_num && prime_num < sizeof (cspset_primes) / sizeof (size_t));
  if ((arr = mem_pool[prime_num]) != NULL)
    {
      mem_pool[prime_num] = *(cspset_elem_t **) arr;
      return arr;
    }
  MALLOC (arr, sizeof (cspset_elem_t) * cspset_primes[prime_num]);
  return arr;
}

/* Free array ARR of PRIME_NUM.  */
void
cspset_free_memory (cspset_elem_t *arr, int prime_num)
{
  assert (sizeof (cspset_elem_t) * cspset_primes[0] >= sizeof (cspset_elem_t *));
  assert (0 <= prime_num && prime_num < sizeof (cspset_primes) / sizeof (size_t));
  *(cspset_elem_t **) arr = mem_pool [prime_num];
  mem_pool [prime_num] = arr;
}

/* Release all unused memory.  */
void
cspset_release_unused_memory (void)
{
  cspset_elem_t *arr, *next;
  int i, nel = sizeof (cspset_primes) / sizeof (size_t);

  for (i = 0; i < nel; i++)
    {
      for (arr = mem_pool[i]; arr != NULL; arr = next)
	{
	  next = *(cspset_elem_t **) arr;
	  FREE (arr);
	}
      mem_pool[i] = NULL;
    }
}

/* Two function to decide when to expand or shrink the table.  */
static inline int
full_p (size_t els_num, size_t length) { return length < els_num * 10 / 6; }

static inline int
sparse_p (size_t els_num, size_t length) { return length > els_num * 10 / 4;}

/* Return the optimal size of set S.  */
static inline int
optimal_size (cspset_t *s) {return cspset_cardinality (s) * 10 / 6;}

/* Tune size of set S.  New size should be enough to hold all set
   elements.  */
static void
tune_size (cspset_t *s, size_t size)
{
  cspset_elem_t el;
  cspset_t t;
  cspset_iterator_t si;

  cspset_init (&t, size);
  assert (! full_p (cspset_cardinality (s), cspset_primes[t.prime_num]));
#ifndef NDEBUG
  t.collisions = s->collisions;
  t.searches = s->searches;
  t.tunes = s->tunes + 1;
#endif
  EXECUTE_FOR_EACH_CSPSET_ELEM (s, el, si)
    cspset_insert (&t, el);
  cspset_finish (s);
  *s = t;
}

/* This internally used function returns number in the above table of
   the first prime number which is greater or equal to N.  START is a
   hint from where to start the search.  */
int
cspset_get_prime_num (size_t n, int start)
{
  int i, pn, nel;

  nel = sizeof (cspset_primes) / sizeof (size_t);
  if (start >= nel)
    start = nel - 1;
  else if (start < 0)
    start = 0;
  if (cspset_primes[start] >= n)
    {
      for (i = start - 1; i >= 0 && cspset_primes[i] >= n; i--)
	;
      pn = i + 1;
    }
  else
    {
      for (i = start + 1; i < nel && cspset_primes[i] < n; i++)
	;
      if (i == nel)
	abort ();
      pn = i;
    }
  return pn;
}

#ifndef NDEBUG
/* Overall number of collisions and searches for all sets.  */
int cspset_all_collisions = 0, cspset_all_searches = 0;
#endif

/* Find slot for element ELEM in set S.  If the elemnt is in the set,
   the slot contains it.  Otherwise, the slot is empty or constains a
   deleted element.  if RESERVE_P, the slot will be used to store the
   element if it is not there yet.  */
static cspset_elem_t *
cspset_find (cspset_t *s, cspset_elem_t elem, int reserve_p)
{
  size_t length;
  cspset_elem_t *el_ptr;
  cspset_elem_t *first_deleted_el_ptr;
  unsigned hash_value, secondary_hash_value;

  length = cspset_primes[s->prime_num];
  hash_value = elem;
  secondary_hash_value = 1 + hash_value % (length - 2);
  hash_value %= length;
#ifndef NDEBUG
  s->searches++;
  cspset_all_searches++;
#endif
  first_deleted_el_ptr = NULL;
  for (;;)
    {
      el_ptr = s->elems + hash_value;
      if (*el_ptr == cspset_empty_elem)
        {
          if (reserve_p && first_deleted_el_ptr != NULL)
	    {
	      el_ptr = first_deleted_el_ptr;
	      *el_ptr = cspset_empty_elem;
	    }
          break;
        }
      else if (*el_ptr == cspset_deleted_elem && first_deleted_el_ptr == NULL)
	first_deleted_el_ptr = el_ptr;
      else if (*el_ptr == elem)
	break;
      hash_value += secondary_hash_value;
      if (hash_value >= length)
        hash_value -= length;
#ifndef NDEBUG
      s->collisions++;
      cspset_all_collisions++;
#endif
    }
  return el_ptr;
}

void
cspset_shrink (cspset_t *s)
{
  if (! sparse_p (cspset_cardinality (s), cspset_primes[s->prime_num]))
    return;
  tune_size (s, optimal_size (s));
}

/* Return TRUE if element EL is in set S.  */
int
cspset_in_p (cspset_t *s, cspset_elem_t el)
{
  cspset_elem_t *el_ptr = cspset_find (s, el, FALSE);

  assert (*el_ptr == el
	  || *el_ptr == cspset_empty_elem || *el_ptr == cspset_deleted_elem);
  return *el_ptr == el;
}

/* Insert element EL into set S.  Return TRUE if the element was not
   in set S, in other words, if the set has been changed.  */
int
cspset_insert (cspset_t *s, cspset_elem_t el)
{
  cspset_elem_t *el_ptr;
  size_t length = cspset_primes[s->prime_num];

  assert (s->all_num < length);
  el_ptr = cspset_find (s, el, TRUE);
  assert (*el_ptr == el
	  || *el_ptr == cspset_empty_elem || *el_ptr == cspset_deleted_elem);
  if (*el_ptr == el)
    return FALSE;
  if (*el_ptr == cspset_deleted_elem)
    s->del_num--;
  else
    s->all_num++;
  *el_ptr = el;
  if (full_p (s->all_num, length))
    tune_size (s, optimal_size (s));
  return TRUE;
}

/* Delete element EL into set S.  Return TRUE if the element was in
   set S, in other words, if the set has been changed.  */
int
cspset_remove (cspset_t *s, cspset_elem_t el)
{
  cspset_elem_t *el_ptr;

  el_ptr = cspset_find (s, el, FALSE);
  assert (*el_ptr == el
	  || *el_ptr == cspset_empty_elem || *el_ptr == cspset_deleted_elem);
  if (*el_ptr != el)
    return FALSE;
  *el_ptr = cspset_deleted_elem;
  s->del_num++;
  return TRUE;
}

/* Make set S empty.  Return TRUE if it has been changed.  */
int
cspset_clear (cspset_t *s)
{
  size_t i;
  size_t length = cspset_primes[s->prime_num];

  if (s->all_num - s->del_num == 0)
    return FALSE;
  for (i = 0; i < length; i++)
    s->elems[i] = cspset_empty_elem;
  s->all_num = s->del_num  = 0;
  return TRUE;
}

/* Return TRUE if S1 is equal to S2.  */
int
cspset_equal_p (cspset_t *s1, cspset_t *s2)
{
  cspset_elem_t el;
  cspset_iterator_t si;

  if (s1 == s2)
    return TRUE;

  if (cspset_cardinality (s1) != cspset_cardinality (s2))
    return FALSE;
  EXECUTE_FOR_EACH_CSPSET_ELEM (s1, el, si)
    if (! cspset_in_p (s2, el))
      return FALSE;
  return TRUE;
}

/* S1 = intersection (S1, S2).  Return TRUE if S1 has been changed.  */
int
cspset_intersect (cspset_t *s1, cspset_t *s2)
{
  cspset_elem_t el;
  cspset_iterator_t si;
  int change_p = FALSE;

  if (s1 == s2)
    return FALSE;
  EXECUTE_FOR_EACH_CSPSET_ELEM (s1, el, si)
    if (! cspset_in_p (s2, el))
      {
	change_p = TRUE;
	cspset_remove (s1, el);
      }
  return change_p;
}

/* S1 = union (S1, S2).  Return TRUE if S1 has been changed.  */
int
cspset_unity (cspset_t *s1, cspset_t *s2)
{
  cspset_elem_t el;
  cspset_iterator_t si;
  int change_p = FALSE;

  if (s1 == s2)
    return FALSE;
  EXECUTE_FOR_EACH_CSPSET_ELEM (s2, el, si)
    if (cspset_insert (s1, el))
      change_p = TRUE;
  return change_p;
}

/* S1 = difference (S1, S2).  Return TRUE if S1 has been changed.  */
int
cspset_diff (cspset_t *s1, cspset_t *s2)
{
  cspset_elem_t el;
  cspset_iterator_t si;
  int change_p = FALSE;

  if (s1 == s2)
    {
      change_p = (s1->all_num - s1->del_num) != 0;
      cspset_clear (s1);
      return change_p;
    }
  if (cspset_cardinality (s1) < cspset_cardinality (s2))
    {
      EXECUTE_FOR_EACH_CSPSET_ELEM (s1, el, si)
	if (cspset_in_p (s2, el))
	  {
	    cspset_remove (s1, el);
	    change_p = TRUE;
	  }
    }
  else
    {
      EXECUTE_FOR_EACH_CSPSET_ELEM (s2, el, si)
	if (cspset_remove (s1, el))
	  change_p = TRUE;
    }
  return change_p;
}

/* Transform compact set S into set R.  Be aware of the result set
   size as it can be too big if S contains big elements.  */
void
cspset_to_spset (cspset_t *s, spset_t *r)
{
  cspset_elem_t el;
  cspset_iterator_t si;
  spset_t t;
  cspset_elem_t max = 0;
  
  EXECUTE_FOR_EACH_CSPSET_ELEM (s, el, si)
    if (max < el)
      max = el;
  spset_init (&t, max + 1);
  EXECUTE_FOR_EACH_CSPSET_ELEM (s, el, si)
    spset_insert (&t, el);
  spset_swap (&t, r);
  spset_finish (&t);
}

/* Transform set S into compact set R.  */
void
cspset_from_spset (cspset_t *r, spset_t *s)
{
  spset_elem_t el;
  spset_iterator_t si;

  cspset_clear (r);
  EXECUTE_FOR_EACH_SPSET_ELEM (s, el, si)
    cspset_insert (r, el);
}

/* Print set S to file F.  */
void
cspset_print (FILE *f, cspset_t *s)
{
  cspset_elem_t el;
  cspset_iterator_t si;
  size_t length = cspset_primes[s->prime_num];
  int n = 0;

  fprintf (f, "compact sparse set (size=%lu, all_num=%lu, del_num=%lu",
	   (unsigned long) length,
	   (unsigned long) s->all_num, (unsigned long) s->del_num);
#ifndef NDEBUG
  fprintf (f, ", collisions=%lu, searches=%lu, tunes=%lu",
	   (unsigned long) s->collisions, (unsigned long) s->searches,
	   (unsigned long) s->tunes);
#endif
  fprintf (f, "):");
  EXECUTE_FOR_EACH_CSPSET_ELEM (s, el, si)
    {
      if (n == 0)
	fprintf (f, "\n");
      fprintf (f, "%6lu ", (unsigned long) el);
      n++;
      if (n >= 10)
	n = 0;
    }
  fprintf (f, "\n", el);
}

/* Print set S to stderr.  */
void
cspset_debug (cspset_t *s)
{
  cspset_print (stderr, s);
}
