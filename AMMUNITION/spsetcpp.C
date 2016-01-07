/* FILE NAME:   spsetcpp.C

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
#include "config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */

#include "spset.h"

/* Numbers used for set sizes.  Each one is 2 times of the previous
   one.  */
spset_elem_t spset::sizes [] = {2, 4, 8, 16, 32, 64, 128, 256, 512, 1024,
				1<<11, 1<<12, 1<<13, 1<<14, 1<<15,
				1<<16, 1<<17, 1<<18, 1<<19, 1<<20,
				1<<21, 1<<22, 1<<23, 1<<24, 1<<25,
				1<<26, 1<<27, 1<<28, 1<<29, 1<<30};

/* Memory pool for sparse and dense arrays.  */
spset_elem_t *spset::mem_pool[sizeof (sizes) / sizeof (spset_elem_t)];

/* Allocate and return an array of SIZE_NUM.  */
spset_elem_t *
spset::get_memory (int size_num)
{
  spset_elem_t *arr;

  assert (sizeof (spset_elem_t) * sizes[0] >= sizeof (spset_elem_t *));
  assert (0 <= size_num && size_num < sizeof (sizes) / sizeof (spset_elem_t));
  if ((arr = mem_pool[size_num]) != NULL)
    {
      mem_pool[size_num] = *(spset_elem_t **) arr;
      return arr;
    }
   return (spset_elem_t *) allocate::malloc (sizeof (spset_elem_t)
					     * sizes[size_num]);
}

/* Free array ARR of SIZE_NUM.  */
void
spset::free_memory (spset_elem_t *arr, int size_num)
{
  assert (sizeof (spset_elem_t) * sizes[0] >= sizeof (spset_elem_t *));
  assert (0 <= size_num && size_num < sizeof (sizes) / sizeof (spset_elem_t));
  *(spset_elem_t **) arr = mem_pool [size_num];
  mem_pool [size_num] = arr;
}

/* Release all unused memory.  */
void
spset::release_unused_memory (void)
{
  spset_elem_t *arr, *next;
  int i, nel = sizeof (sizes) / sizeof (spset_elem_t);

  for (i = 0; i < nel; i++)
    {
      for (arr = mem_pool[i]; arr != NULL; arr = next)
	{
	  next = *(spset_elem_t **) arr;
	  allocate::free (arr);
	}
      mem_pool[i] = NULL;
    }
}

/* Make the set having NEW_SIZE_NUM.  */
void
spset::tune_size (int new_size_num)
{
  spset_elem_t size, *new_dense, *new_sparse;

  if (new_size_num == size_num)
    return;
  new_dense = get_memory (new_size_num);
  new_sparse = get_memory (new_size_num);
  size = sizes[new_size_num > size_num ? size_num : new_size_num];
  assert (els_num <= size);
  memcpy (new_dense, dense, sizeof (spset_elem_t) * els_num);
  memcpy (new_sparse, sparse, sizeof (spset_elem_t) * size);
  free_memory (dense, size_num);
  free_memory (sparse, size_num);
  dense = new_dense;
  sparse = new_sparse;
  size_num = new_size_num;
}

/* This internally used function returns number in the above table of
   the first number which is greater or equal to N.  START is a hint
   from where to start the search.  */
int
spset::get_size_num (spset_elem_t n, int start)
{
  int i, pn, nel;

  nel = sizeof (sizes) / sizeof (spset_elem_t);
  if (start >= nel)
    start = nel - 1;
  else if (start < 0)
    start = 0;
  if (sizes[start] >= n)
    {
      for (i = start - 1; i >= 0 && sizes[i] >= n; i--)
	;
      pn = i + 1;
    }
  else
    {
      for (i = start + 1; i < nel && sizes[i] < n; i++)
	;
      if (i == nel)
	abort ();
      pn = i;
    }
  return pn;
}

/* Shrink the set.  */
void
spset::shrink (void)
{
  spset_elem_t i, el, max, new_size_num;

  max = 0;
  for (i = 0; i < els_num; i++)
    if ((el = dense[i]) > max)
      max = el;
  new_size_num = get_size_num (max + 1, size_num);
  tune_size (new_size_num);
}

/* Insert EL into the set.  Return TRUE if it was not there, in order words,
   if the set has been changed.  */
int
spset::insert (spset_elem_t el)
{
  int new_size_num;
  spset_elem_t size;

  if (in_p (el))
    return FALSE;
  size = sizes[size_num];
  if (el >= size)
    {
      new_size_num = get_size_num (el + 1, size_num);
      tune_size (new_size_num);
    }
  sparse[el] = els_num;
  dense[els_num++] = el;
  return TRUE;
}

/* Return TRUE if this is equal to S.  */
int
spset::equal_p (spset *s)
{
  spset_elem_t el;
  spset_iterator si;

  if (this == s)
    return TRUE;

  if (cardinality () != s->cardinality ())
    return FALSE;

  EXECUTE_FOR_EACH_SPSET_ELEM (this, el, si)
    if (! s->in_p (el))
      return FALSE;

  return TRUE;
}

/* this = intersection (this, S).  Return TRUE if this has been changed.  */
int
spset::intersect (spset *s)
{
  spset_elem_t el;
  spset_iterator si;
  int change_p = FALSE;

  if (this == s)
    return FALSE;
  EXECUTE_FOR_EACH_SPSET_ELEM (this, el, si)
    if (! s->in_p (el))
      {
	change_p = TRUE;
	remove (el);
	/* This is a trick to process all elements as it depends on
	   implementation of the set iterator and set element
	   deletion.  If anyone is changed, change the code here
	   too.  */
	si.start--;
      }
  return change_p;
}

/* this = union (this, S).  Return TRUE if this has been changed.  */
int
spset::unity (spset *s)
{
  spset_elem_t el;
  spset_iterator si;
  int change_p = FALSE;

  if (this == s)
    return FALSE;
  EXECUTE_FOR_EACH_SPSET_ELEM (s, el, si)
    if (insert (el))
      change_p = TRUE;
  return change_p;
}

/* this = difference (this, S).  Return TRUE if this has been
   changed.  */
int
spset::diff (spset *s)
{
  spset_elem_t el;
  spset_iterator si;
  int change_p = FALSE;

  if (this == s)
    {
      change_p = this->els_num != 0;
      clear ();
      return change_p;
    }
  EXECUTE_FOR_EACH_SPSET_ELEM (s, el, si)
    if (remove (el))
      change_p = TRUE;
  return change_p;
}

/* Print set S to file F.  */
void
spset::print (FILE *f)
{
  spset_elem_t el;
  spset_iterator si;
  int n = 0;
  spset_elem_t size = sizes[size_num];
  
  fprintf (f, "sparse set (size=%lu, els_num=%lu);",
	   (unsigned long) size, (unsigned long) this->els_num);
  EXECUTE_FOR_EACH_SPSET_ELEM (this, el, si)
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
spset::debug (void)
{
  print (stderr);
}
