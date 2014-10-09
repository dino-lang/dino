/* FILE NAME:   cspset.cpp

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
size_t cspset::primes [] = {7, 17, 37, 79, 163, 331, 673, 1361, 2729,
			    5471, 10949, 21911, 43853, 87719, 175447,
			    350899, 701819, 1403641, 2807303, 5614657,
			    11229331, 22458671, 44917381, 89834777,
			    179669557, 359339171, 718678369, 1437356741};


/* Memory pool for elems .  */
spset_elem_t *cspset::mem_pool[sizeof (primes) / sizeof (size_t)];

/* Allocate and return an array of PRIME_NUM.  */
cspset_elem_t *
cspset::get_memory (int prime_num)
{
  cspset_elem_t *arr;

  assert (sizeof (cspset_elem_t) * primes[0] >= sizeof (cspset_elem_t *));
  assert (0 <= prime_num && prime_num < sizeof (primes) / sizeof (size_t));
  if ((arr = mem_pool[prime_num]) != NULL)
    {
      mem_pool[prime_num] = *(cspset_elem_t **) arr;
      return arr;
    }
  arr = (cspset_elem_t *) allocate::malloc (sizeof (cspset_elem_t)
					    * primes[prime_num]);
  return arr;
}

/* Free array ARR of PRIME_NUM.  */
void
cspset::free_memory (cspset_elem_t *arr, int prime_num)
{
  assert (sizeof (cspset_elem_t) * primes[0] >= sizeof (cspset_elem_t *));
  assert (0 <= prime_num && prime_num < sizeof (primes) / sizeof (size_t));
  *(cspset_elem_t **) arr = mem_pool [prime_num];
  mem_pool [prime_num] = arr;
}

/* Release all unused memory.  */
void
cspset::release_unused_memory (void)
{
  cspset_elem_t *arr, *next;
  int i, nel = sizeof (primes) / sizeof (size_t);

  for (i = 0; i < nel; i++)
    {
      for (arr = mem_pool[i]; arr != NULL; arr = next)
	{
	  next = *(cspset_elem_t **) arr;
	  allocate::free (arr);
	}
      mem_pool[i] = NULL;
    }
}

/* Two function to decide when to expand or shrink the table.  */
static inline int
full_p (size_t els_num, size_t length) { return length < els_num * 10 / 6; }

static inline int
sparse_p (size_t els_num, size_t length) { return length > els_num * 10 / 4;}

/* Return the optimal size of the set.  */
inline int
cspset::optimal_size (void) {return cardinality () * 10 / 6;}

/* Tune size of the set.  New size should be enough to hold all set
   elements.  */
void
cspset::tune_size (size_t size)
{
  cspset_elem_t el;
  cspset t (size);
  cspset_iterator si;

  assert (! full_p (cardinality (), primes[t.prime_num]));
#ifndef NDEBUG
  t.collisions = collisions;
  t.searches = searches;
  t.tunes = tunes + 1;
#endif
  EXECUTE_FOR_EACH_CSPSET_ELEM (this, el, si)
    t.insert (el);
  copy (&t);
}

/* This internally used function returns number in the above table of
   the first prime number which is greater or equal to N.  START is a
   hint from where to start the search.  */
int
cspset::get_prime_num (size_t n, int start)
{
  int i, nel, pn;

  nel = sizeof (primes) / sizeof (size_t);
  if (start >= nel)
    start = nel - 1;
  else if (start < 0)
    start = 0;
  if (primes[start] >= n)
    {
      for (i = start - 1; i >= 0 && primes[i] >= n; i--)
	;
      pn = i + 1;
    }
  else
    {
      for (i = start + 1; i < nel && primes[i] < n; i++)
	;
      if (i == nel)
	abort ();
      pn = i;
    }
  return pn;
}

#ifndef NDEBUG
/* Overall number of collisions and searches for all sets.  */
int cspset::all_collisions = 0, cspset::all_searches = 0;
#endif

/* Find slot for element ELEM in the set.  If the elemnt is in the set,
   the slot contains it.  Otherwise, the slot is empty or constains a
   deleted element.  if RESERVE_P, the slot will be used to store the
   element if it is not there yet.  */
cspset_elem_t *
cspset::find (cspset_elem_t elem, int reserve_p)
{
  size_t length;
  cspset_elem_t *el_ptr;
  cspset_elem_t *first_deleted_el_ptr;
  unsigned hash_value, secondary_hash_value;

  hash_value = elem;
  length = primes[prime_num];
  secondary_hash_value = 1 + hash_value % (length - 2);
  hash_value %= length;
#ifndef NDEBUG
  searches++;
  all_searches++;
#endif
  first_deleted_el_ptr = NULL;
  for (;;)
    {
      el_ptr = elems + hash_value;
      if (*el_ptr == empty_elem)
        {
          if (reserve_p && first_deleted_el_ptr != NULL)
	    {
	      el_ptr = first_deleted_el_ptr;
	      *el_ptr = empty_elem;
	    }
          break;
        }
      else if (*el_ptr == deleted_elem && first_deleted_el_ptr == NULL)
	first_deleted_el_ptr = el_ptr;
      else if (*el_ptr == elem)
	break;
      hash_value += secondary_hash_value;
      if (hash_value >= length)
        hash_value -= length;
#ifndef NDEBUG
      collisions++;
      all_collisions++;
#endif
    }
  return el_ptr;
}

void
cspset::shrink (void)
{
  if (! sparse_p (cardinality (), primes[prime_num]))
    return;
  tune_size (optimal_size ());
}

/* Return TRUE if element EL is in the set.  */
int
cspset::in_p (cspset_elem_t el)
{
  cspset_elem_t *el_ptr = find (el, FALSE);

  assert (*el_ptr == el || *el_ptr == empty_elem || *el_ptr == deleted_elem);
  return *el_ptr == el;
}

/* Insert element EL into set S.  Return TRUE if the element was not
   in the set, in other words, if the set has been changed.  */
int
cspset::insert (cspset_elem_t el)
{
  cspset_elem_t *el_ptr;
  size_t length = primes[prime_num];

  assert (all_num < length);
  el_ptr = find (el, TRUE);
  assert (*el_ptr == el || *el_ptr == empty_elem || *el_ptr == deleted_elem);
  if (*el_ptr == el)
    return FALSE;
  if (*el_ptr == deleted_elem)
    del_num--;
  else
    all_num++;
  *el_ptr = el;
  if (full_p (all_num, length))
    tune_size (optimal_size ());
  return TRUE;
}

/* Delete element EL in the set.  Return TRUE if the element was in
   the set, in other words, if the set has been changed.  */
int
cspset::remove (cspset_elem_t el)
{
  cspset_elem_t *el_ptr;

  el_ptr = find (el, FALSE);
  assert (*el_ptr == el || *el_ptr == empty_elem || *el_ptr == deleted_elem);
  if (*el_ptr != el)
    return FALSE;
  *el_ptr = deleted_elem;
  del_num++;
  return TRUE;
}

/* Make the set empty.  Return TRUE if it has been changed.  */
int
cspset::clear (void)
{
  size_t i;
  size_t length = primes[prime_num];

  if (all_num - del_num == 0)
    return FALSE;
  for (i = 0; i < length; i++)
    elems[i] = empty_elem;
  all_num = del_num  = 0;
  return TRUE;
}

/* Return TRUE if this is equal to S.  */
int
cspset::equal_p (cspset *s)
{
  cspset_elem_t el;
  cspset_iterator si;

  if (this == s)
    return TRUE;

  if (cardinality () != s->cardinality ())
    return FALSE;
  EXECUTE_FOR_EACH_CSPSET_ELEM (this, el, si)
    if (! s->in_p (el))
      return FALSE;
  return TRUE;
}

/* this = intersection (this, S).  Return TRUE if this has been changed.  */
int
cspset::intersect (cspset *s)
{
  cspset_elem_t el;
  cspset_iterator si;
  int change_p = FALSE;

  if (this == s)
    return FALSE;
  EXECUTE_FOR_EACH_CSPSET_ELEM (this, el, si)
    if (! s->in_p (el))
      {
	change_p = TRUE;
	remove (el);
      }
  return change_p;
}

/* this = union (this, S).  Return TRUE if this has been changed.  */
int
cspset::unity (cspset *s)
{
  cspset_elem_t el;
  cspset_iterator si;
  int change_p = FALSE;

  if (this == s)
    return FALSE;
  EXECUTE_FOR_EACH_CSPSET_ELEM (s, el, si)
    if (insert (el))
      change_p = TRUE;
  return change_p;
}

/* this = difference (this, S).  Return TRUE if this has been changed.  */
int
cspset::diff (cspset *s)
{
  cspset_elem_t el;
  cspset_iterator si;
  int change_p = FALSE;

  if (this == s)
    {
      change_p = (all_num - del_num) != 0;
      clear ();
      return change_p;
    }
  if (cardinality () < s->cardinality ())
    {
      EXECUTE_FOR_EACH_CSPSET_ELEM (this, el, si)
	if (s->in_p (el))
	  {
	    remove (el);
	    change_p = TRUE;
	  }
    }
  else
    {
      EXECUTE_FOR_EACH_CSPSET_ELEM (s, el, si)
	if (remove (el))
	  change_p = TRUE;
    }
  return change_p;
}

/* Transform the compact set into set R.  Be aware of the result set
   size as it can be too big if the compact set contains big
   elements.  */
void
cspset::to_spset (spset *r)
{
  cspset_elem_t el;
  cspset_iterator si;
  cspset_elem_t max = 0;

  EXECUTE_FOR_EACH_CSPSET_ELEM (this, el, si)
    if (max < el)
      max = el;

  spset t (max + 1);
  EXECUTE_FOR_EACH_CSPSET_ELEM (this, el, si)
    t.insert (el);
  r->swap (&t);
}

/* Transform set S into the compact.  */
void
cspset::from_spset (spset *s)
{
  spset_elem_t el;
  spset_iterator si;

  clear ();
  EXECUTE_FOR_EACH_SPSET_ELEM (s, el, si)
    insert (el);
}

/* Print the set to file F.  */
void
cspset::print (FILE *f)
{
  cspset_elem_t el;
  cspset_iterator si;
  size_t length = primes[prime_num];
  int n = 0;

  fprintf (f, "compact sparse set (size=%lu, all_num=%lu, del_num=%lu",
	   (unsigned long) length,
	   (unsigned long) all_num, (unsigned long) del_num);
#ifndef NDEBUG
  fprintf (f, ", collisions=%lu, searches=%lu, tunes=%lu",
	   (unsigned long) collisions, (unsigned long) searches,
	   (unsigned long) tunes);
#endif
  fprintf (f, "):");
  EXECUTE_FOR_EACH_CSPSET_ELEM (this, el, si)
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

/* Print the set to stderr.  */
void
cspset::debug (void)
{
  print (stderr);
}
