/* FILE NAME:   spset.c

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

   TITLE:       Expandable sparse set package

   DESCRIPTION: This file implements the expandable sparse set package
       functions.

   IMPLEMENTATION: See "An Efficient Representation for Sparse Sets"
       by Preston Briggs and Linda Torczon.
*/


#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */

#include "spset.h"

/* Numbers used for set sizes.  Each one is 2 times of the previous
   one.  */
spset_elem_t spset_sizes [] = {2, 4, 8, 16, 32, 64, 128, 256, 512, 1024,
			       1<<11, 1<<12, 1<<13, 1<<14, 1<<15,
			       1<<16, 1<<17, 1<<18, 1<<19, 1<<20,
			       1<<21, 1<<22, 1<<23, 1<<24, 1<<25,
			       1<<26, 1<<27, 1<<28, 1<<29, 1<<30};

/* Memory pool for sparse and dense arrays.  */
static spset_elem_t *mem_pool[sizeof (spset_sizes) / sizeof (spset_elem_t)];

/* Allocate and return an array of SIZE_NUM.  */
spset_elem_t *
spset_get_memory (int size_num)
{
  spset_elem_t *arr;

  assert (sizeof (spset_elem_t) * spset_sizes[0] >= sizeof (spset_elem_t *));
  assert (0 <= size_num && size_num < sizeof (spset_sizes) / sizeof (spset_elem_t));
  if ((arr = mem_pool[size_num]) != NULL)
    {
      mem_pool[size_num] = *(spset_elem_t **) arr;
      return arr;
    }
  MALLOC (arr, sizeof (spset_elem_t) * spset_sizes[size_num]);
  return arr;
}

/* Free array ARR of SIZE_NUM.  */
void
spset_free_memory (spset_elem_t *arr, int size_num)
{
  assert (sizeof (spset_elem_t) * spset_sizes[0] >= sizeof (spset_elem_t *));
  assert (0 <= size_num && size_num < sizeof (spset_sizes) / sizeof (spset_elem_t));
  *(spset_elem_t **) arr = mem_pool [size_num];
  mem_pool [size_num] = arr;
}

/* Release all unused memory.  */
void
spset_release_unused_memory (void)
{
  spset_elem_t *arr, *next;
  int i, nel = sizeof (spset_sizes) / sizeof (spset_elem_t);

  for (i = 0; i < nel; i++)
    {
      for (arr = mem_pool[i]; arr != NULL; arr = next)
	{
	  next = *(spset_elem_t **) arr;
	  FREE (arr);
	}
      mem_pool[i] = NULL;
    }
}

/* Make S having NEW_SIZE_NUM.  */
static void
tune_size (spset_t *s, int new_size_num)
{
  spset_elem_t size, *new_dense, *new_sparse;

  if (new_size_num == s->size_num)
    return;
  new_dense = spset_get_memory (new_size_num);
  new_sparse = spset_get_memory (new_size_num);
  size = spset_sizes[new_size_num > s->size_num ? s->size_num : new_size_num];
  assert (s->els_num <= size);
  memcpy (new_dense, s->dense, sizeof (spset_elem_t) * s->els_num);
  memcpy (new_sparse, s->sparse, sizeof (spset_elem_t) * size);
  spset_free_memory (s->dense, s->size_num);
  spset_free_memory (s->sparse, s->size_num);
  s->dense = new_dense;
  s->sparse = new_sparse;
  s->size_num = new_size_num;
}

/* This internally used function returns number in the above table of
   the first number which is greater or equal to N.  START is a hint
   from where to start the search.  */
int
spset_get_size_num (spset_elem_t n, int start)
{
  int i, pn, nel;

  nel = sizeof (spset_sizes) / sizeof (spset_elem_t);
  if (start >= nel)
    start = nel - 1;
  else if (start < 0)
    start = 0;
  if (spset_sizes[start] >= n)
    {
      for (i = start - 1; i >= 0 && spset_sizes[i] >= n; i--)
	;
      pn = i + 1;
    }
  else
    {
      for (i = start + 1; i < nel && spset_sizes[i] < n; i++)
	;
      if (i == nel)
	abort ();
      pn = i;
    }
  return pn;
}

/* Shrink the set.  */
void
spset_shrink (spset_t *s)
{
  spset_elem_t i, el, max, new_size_num;

  max = 0;
  for (i = 0; i < s->els_num; i++)
    if ((el = s->dense[i]) > max)
      max = el;
  new_size_num = spset_get_size_num (max + 1, s->size_num);
  tune_size (s, new_size_num);
}

/* Insert EL into S.  Return TRUE if it was not there, in order words,
   if the set has been changed.  */
int
spset_insert (spset_t *s, spset_elem_t el)
{
  int new_size_num;
  size_t size;

  if (spset_in_p (s, el))
    return FALSE;
  size = spset_sizes[s->size_num];
  if (el >= size)
    {
      new_size_num = spset_get_size_num (el + 1, s->size_num);
      tune_size (s, new_size_num);
    }
  s->sparse[el] = s->els_num;
  s->dense[s->els_num++] = el;
  return TRUE;
}

/* Return TRUE if S1 is equal to S2.  */
int
spset_equal_p (spset_t *s1, spset_t *s2)
{
  spset_elem_t el;
  spset_iterator_t si;

  if (s1 == s2)
    return TRUE;

  if (spset_cardinality (s1) != spset_cardinality (s2))
    return FALSE;

  EXECUTE_FOR_EACH_SPSET_ELEM (s1, el, si)
    if (! spset_in_p (s2, el))
      return FALSE;

  return TRUE;
}

/* S1 = intersection (S1, S2).  Return TRUE if S1 has been changed.  */
int
spset_intersect (spset_t *s1, spset_t *s2)
{
  spset_elem_t el;
  spset_iterator_t si;
  int change_p = FALSE;

  if (s1 == s2)
    return FALSE;
  EXECUTE_FOR_EACH_SPSET_ELEM (s1, el, si)
    if (! spset_in_p (s2, el))
      {
	change_p = TRUE;
	spset_remove (s1, el);
	/* This is a trick to process all elements as it depends on
	   implementation of the set iterator and set element
	   deletion.  If anyone is changed, change the code here
	   too.  */
	assert (si.start != 0);
	si.start--;
      }
  return change_p;
}

/* S1 = union (S1, S2).  Return TRUE if S1 has been changed.  */
int
spset_unity (spset_t *s1, spset_t *s2)
{
  spset_elem_t el;
  spset_iterator_t si;
  int change_p = FALSE;

  if (s1 == s2)
    return FALSE;
  EXECUTE_FOR_EACH_SPSET_ELEM (s2, el, si)
    if (spset_insert (s1, el))
      change_p = TRUE;
  return change_p;
}

/* S1 = difference (S1, S2).  Return TRUE if S1 has been changed.  */
int
spset_diff (spset_t *s1, spset_t *s2)
{
  spset_elem_t el;
  spset_iterator_t si;
  int change_p = FALSE;

  if (s1 == s2)
    {
      change_p = s1->els_num != 0;
      spset_clear (s1);
      return change_p;
    }
  EXECUTE_FOR_EACH_SPSET_ELEM (s2, el, si)
    if (spset_remove (s1, el))
      change_p = TRUE;
  return change_p;
}

/* Print set S to file F.  */
void
spset_print (FILE *f, spset_t *s)
{
  spset_elem_t el;
  spset_iterator_t si;
  int n = 0;
  spset_elem_t size = spset_sizes[s->size_num];
  
  fprintf (f, "sparse set (size=%lu, els_num=%lu);",
	   (unsigned long) size, (unsigned long) s->els_num);
  EXECUTE_FOR_EACH_SPSET_ELEM (s, el, si)
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
spset_debug (spset_t *s)
{
  spset_print (stderr, s);
}
