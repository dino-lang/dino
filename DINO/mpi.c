/*
   Copyright (C) 1997-2002 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of interpreter of DINO.

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

*/

/* We made all check inside DINO code therefore we trust in correct
   operand types. */

#include "d_extern.h"
#include "arithm.h"
#include <assert.h>

#ifdef WIN32

#include <windows.h>

#define WIN_EXPORT  __declspec(dllexport)

BOOL APIENTRY
DllMain (HANDLE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
{
  return TRUE;
}

#else
#define WIN_EXPORT
#endif

static void
mpi_binary_op (int npars, val_t *vals, int_t *size,
	       void **hidevalue1, void **hidevalue2,
	       ER_node_t *res, void **res_hidevalue)
{
  ER_node_t var1;
  ER_node_t var2;
  ER_node_t size_var;
  ER_node_t hideblock;

  assert (npars == 3
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 2)) == ER_NM_instance);
  var1 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  var2 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		      1);
  *res = ER_instance ((ER_node_t) (vals + 2));
  size_var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)),
			  0);
  *size = ER_i (size_var);
  hideblock = create_hideblock (*size);
  *res_hidevalue = ER_hideblock_start (hideblock);
  /* Size is already set up. */
  ER_SET_MODE (INDEXED_VAL (ER_instance_vars (*res), 1), ER_NM_hideblock);
  ER_set_hideblock (INDEXED_VAL (ER_instance_vars (*res), 1), hideblock);
  *hidevalue1 = ER_hideblock_start (ER_hideblock (var1));
  *hidevalue2 = ER_hideblock_start (ER_hideblock (var2));
}

static val_t
return_mpi (ER_node_t mpi)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  ER_SET_MODE (res, ER_NM_instance);
  ER_set_instance (res, mpi);
  return val;
}

static val_t
return_int (int_t i)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i);
  return val;
}

WIN_EXPORT val_t mpi_overflow;

static void
check_overflow (void)
{
  ER_SET_MODE ((ER_node_t) &mpi_overflow, ER_NM_int);
  ER_set_i ((ER_node_t) &mpi_overflow, overflow_bit != 0);
}

static val_t
arithm_op (int npars, val_t *vals,
	   void (*func) (int, const void *, const void *, void *))
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  (*func) (size, hidevalue1, hidevalue2, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_add (int npars, val_t *vals)
{
  return arithm_op (npars, vals, add_integer);
}

WIN_EXPORT val_t
mpi_unsigned_add (int npars, val_t *vals)
{
  return arithm_op (npars, vals, add_unsigned_integer);
}

WIN_EXPORT val_t
mpi_subtract (int npars, val_t *vals)
{
  return arithm_op (npars, vals, subtract_integer);
}

WIN_EXPORT val_t
mpi_unsigned_subtract (int npars, val_t *vals)
{
  return arithm_op (npars, vals, subtract_unsigned_integer);
}

WIN_EXPORT val_t
mpi_multiply (int npars, val_t *vals)
{
  return arithm_op (npars, vals, multiply_integer);
}

WIN_EXPORT val_t
mpi_unsigned_multiply (int npars, val_t *vals)
{
  return arithm_op (npars, vals, multiply_unsigned_integer);
}

WIN_EXPORT val_t
mpi_divide (int npars, val_t *vals)
{
  return arithm_op (npars, vals, divide_integer);
}

WIN_EXPORT val_t
mpi_unsigned_divide (int npars, val_t *vals)
{
  return arithm_op (npars, vals, divide_unsigned_integer);
}

WIN_EXPORT val_t
mpi_remainder (int npars, val_t *vals)
{
  return arithm_op (npars, vals, integer_remainder);
}

WIN_EXPORT val_t
mpi_unsigned_remainder (int npars, val_t *vals)
{
  return arithm_op (npars, vals, unsigned_integer_remainder);
}

static void
mpi_par_op (int npars, val_t *vals, int_t *size, int_t *par, int size_is_par,
	    void **hidevalue, ER_node_t *res, void **res_hidevalue)
{
  ER_node_t var;
  ER_node_t size_var;
  ER_node_t hideblock;

  assert (npars == 3
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int
	  && ER_NODE_MODE ((ER_node_t) (vals + 2)) == ER_NM_instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  *par = ER_i ((ER_node_t) (vals + 1));
  *res = ER_instance ((ER_node_t) (vals + 2));
  size_var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)),
			  0);
  *size = ER_i (size_var);
  hideblock = create_hideblock (size_is_par ? *par : *size);
  *res_hidevalue = ER_hideblock_start (hideblock);
  /* Size is already set up. */
  ER_SET_MODE (INDEXED_VAL (ER_instance_vars (*res), 1), ER_NM_hideblock);
  ER_set_hideblock (INDEXED_VAL (ER_instance_vars (*res), 1), hideblock);
  ER_set_i (INDEXED_VAL (ER_instance_vars (*res), 0),
	    (size_is_par ? *par : *size));
  *hidevalue = ER_hideblock_start (ER_hideblock (var));
}

static val_t
shift (int npars, val_t *vals, void (*func) (int, const void *, int, void *))
{
  int_t size, par;
  void *hidevalue, *res_hidevalue;
  ER_node_t mpi;

  mpi_par_op (npars, vals, &size, &par, 0 /*FALSE*/,
	      &hidevalue, &mpi, &res_hidevalue);
  (*func) (size, hidevalue, par, res_hidevalue);
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_shift_right (int npars, val_t *vals)
{
  return shift (npars, vals, integer_shift_right);
}

WIN_EXPORT val_t
mpi_unsigned_shift_right (int npars, val_t *vals)
{
  return shift (npars, vals, unsigned_integer_shift_right);
}

WIN_EXPORT val_t
mpi_shift_left (int npars, val_t *vals)
{
  return shift (npars, vals, integer_shift_left);
}

WIN_EXPORT val_t
mpi_unsigned_shift_left (int npars, val_t *vals)
{
  return shift (npars, vals, unsigned_integer_shift_left);
}

static val_t
logical (int npars, val_t *vals,
	 void (*func) (int, const void *, const void *, void *))
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  (*func) (size, hidevalue1, hidevalue2, res_hidevalue);
  return return_mpi (mpi);
}


WIN_EXPORT val_t
mpi_or (int npars, val_t *vals)
{
  return logical (npars, vals, integer_or);
}

WIN_EXPORT val_t
mpi_unsigned_or (int npars, val_t *vals)
{
  return logical (npars, vals, unsigned_integer_or);
}

WIN_EXPORT val_t
mpi_and (int npars, val_t *vals)
{
  return logical (npars, vals, integer_and);
}

WIN_EXPORT val_t
mpi_unsigned_and (int npars, val_t *vals)
{
  return logical (npars, vals, unsigned_integer_and);
}

static void
mpi_unary_op (int npars, val_t *vals, int_t *size,
	      void **hidevalue, ER_node_t *res, void **res_hidevalue)
{
  ER_node_t var;
  ER_node_t size_var;
  ER_node_t hideblock;

  assert (npars == 2
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)),
		     1);
  size_var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)),
			  0);
  *size = ER_i (size_var);
  hideblock = create_hideblock (*size);
  *res_hidevalue = ER_hideblock_start (hideblock);
  *res = ER_instance ((ER_node_t) (vals + 1));
  /* Size is already set up. */
  ER_SET_MODE (INDEXED_VAL (ER_instance_vars (*res), 1), ER_NM_hideblock);
  ER_set_hideblock (INDEXED_VAL (ER_instance_vars (*res), 1), hideblock);
  *hidevalue = ER_hideblock_start (ER_hideblock (var));
}

static val_t
not (int npars, val_t *vals, void (*func) (int, const void *, void *))
{
  int_t size;
  void *hidevalue, *res_hidevalue;
  ER_node_t mpi;

  mpi_unary_op (npars, vals, &size, &hidevalue, &mpi, &res_hidevalue);
  (*func) (size, hidevalue, res_hidevalue);
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_not (int npars, val_t *vals)
{
  return not (npars, vals, integer_not);
}

WIN_EXPORT val_t
mpi_unsigned_not (int npars, val_t *vals)
{
  return not (npars, vals, unsigned_integer_not);
}

static void
mpi_cmp_op (int npars, val_t *vals, int_t *size,
	    void **hidevalue1, void **hidevalue2)
{
  ER_node_t var1;
  ER_node_t var2;
  ER_node_t size_var;

  assert (npars == 2
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance);
  var1 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  var2 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		      1);
  size_var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)),
			  0);
  *size = ER_i (size_var);
  *hidevalue1 = ER_hideblock_start (ER_hideblock (var1));
  *hidevalue2 = ER_hideblock_start (ER_hideblock (var2));
}

static val_t
cmp (int npars, val_t *vals, int (*func) (int, const void *, const void *))
{
  int_t size;
  void *hidevalue1, *hidevalue2;

  mpi_cmp_op (npars, vals, &size, &hidevalue1, &hidevalue2);
  return return_int ((*func) (size, hidevalue1, hidevalue2));
}

WIN_EXPORT val_t
mpi_eq (int npars, val_t *vals)
{
  return cmp (npars, vals, eq_integer);
}

WIN_EXPORT val_t
mpi_unsigned_eq (int npars, val_t *vals)
{
  return cmp (npars, vals, eq_unsigned_integer);
}

WIN_EXPORT val_t
mpi_ne (int npars, val_t *vals)
{
  return cmp (npars, vals, ne_integer);
}

WIN_EXPORT val_t
mpi_unsigned_ne (int npars, val_t *vals)
{
  return cmp (npars, vals, ne_unsigned_integer);
}

WIN_EXPORT val_t
mpi_gt (int npars, val_t *vals)
{
  return cmp (npars, vals, gt_integer);
}

WIN_EXPORT val_t
mpi_unsigned_gt (int npars, val_t *vals)
{
  return cmp (npars, vals, gt_unsigned_integer);
}

WIN_EXPORT val_t
mpi_lt (int npars, val_t *vals)
{
  return cmp (npars, vals, lt_integer);
}

WIN_EXPORT val_t
mpi_unsigned_lt (int npars, val_t *vals)
{
  return cmp (npars, vals, lt_unsigned_integer);
}

WIN_EXPORT val_t
mpi_ge (int npars, val_t *vals)
{
  return cmp (npars, vals, ge_integer);
}

WIN_EXPORT val_t
mpi_unsigned_ge (int npars, val_t *vals)
{
  return cmp (npars, vals, ge_unsigned_integer);
}

WIN_EXPORT val_t
mpi_le (int npars, val_t *vals)
{
  return cmp (npars, vals, le_integer);
}

WIN_EXPORT val_t
mpi_unsigned_le (int npars, val_t *vals)
{
  return cmp (npars, vals, le_unsigned_integer);
}

static val_t
change_size (int npars, val_t *vals,
	     void (*func) (int, const void *, int, void *))
{
  int_t size, par;
  void *hidevalue, *res_hidevalue;
  ER_node_t mpi;

  mpi_par_op (npars, vals, &size, &par, 1 /*TRUE*/,
	      &hidevalue, &mpi, &res_hidevalue);
  (*func) (size, hidevalue, par, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_change_size (int npars, val_t *vals)
{
  return change_size (npars, vals, change_integer_size);
}

WIN_EXPORT val_t
mpi_unsigned_change_size (int npars, val_t *vals)
{
  return change_size (npars, vals, change_unsigned_integer_size);
}

#define MAX_INTEGER_SIZE 128

static val_t
to_based_string (int npars, val_t *vals,
		 char *(*func) (int, const void *, int, char *))
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t var;
  ER_node_t size_var;
  int_t size;
  void *hidevalue;
  ER_node_t vect;
  char str [3 * MAX_INTEGER_SIZE];

  assert (npars == 2
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  size_var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)),
			  0);
  size = ER_i (size_var);
  hidevalue = ER_hideblock_start (ER_hideblock (var));
  (*func) (size, hidevalue, ER_i ((ER_node_t) (vals + 1)), str);
  vect = create_string (str);
  ER_SET_MODE (res, ER_NM_vect);
  ER_set_vect (res, vect);
  return val;
}

WIN_EXPORT val_t
mpi_to_based_string (int npars, val_t *vals)
{
  return to_based_string (npars, vals, integer_to_based_string);
}

WIN_EXPORT val_t
mpi_unsigned_to_based_string (int npars, val_t *vals)
{
  return to_based_string (npars, vals, unsigned_integer_to_based_string);
}

static val_t
from_based_string (int npars, val_t *vals,
		   char *(*func) (int, const char *, int, void *))
{
  int_t size;
  void *hideblock;
  ER_node_t var, size_var;
  ER_node_t mpi;

  assert (npars == 3
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 2)) == ER_NM_int);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		     1);
  size_var = INDEXED_VAL (ER_instance_vars
			  (ER_instance ((ER_node_t) (vals + 1))), 0);
  size = ER_i (size_var);
  hideblock = create_hideblock (size);
  (*func) (size, ER_pack_els (ER_vect ((ER_node_t) vals)),
	   ER_i ((ER_node_t) (vals + 2)),
	   ER_hideblock_start ((ER_node_t) hideblock));
  check_overflow ();
  mpi = ER_instance ((ER_node_t) (vals + 1));
  /* Size is already set up. */
  ER_SET_MODE (INDEXED_VAL (ER_instance_vars (mpi), 1), ER_NM_hideblock);
  ER_set_hideblock (INDEXED_VAL (ER_instance_vars (mpi), 1), hideblock);
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_from_based_string (int npars, val_t *vals)
{
  return from_based_string (npars, vals, integer_from_based_string);
}

WIN_EXPORT val_t
mpi_unsigned_from_based_string (int npars, val_t *vals)
{
  return from_based_string (npars, vals, unsigned_integer_from_based_string);
}


#ifndef WIN32
#if !defined(HAVE_DLOPEN) || defined(NO_EXTERN_SHLIB)

/* Function for implementing externals with static libraries.  See all
   externals name in mpi.d. */
void *
mpi_address (const char *name)
{
  if (strcmp (name, "mpi_overflow") == 0)
    return &mpi_overflow;
  else if (strcmp (name, "mpi_add") == 0)
    return mpi_add;
  else if (strcmp (name, "mpi_unsigned_add") == 0)
    return mpi_unsigned_add;
  else if (strcmp (name, "mpi_subtract") == 0)
    return mpi_subtract;
  else if (strcmp (name, "mpi_unsigned_subtract") == 0)
    return mpi_unsigned_subtract;
  else if (strcmp (name, "mpi_multiply") == 0)
    return mpi_multiply;
  else if (strcmp (name, "mpi_unsigned_multiply") == 0)
    return mpi_unsigned_multiply;
  else if (strcmp (name, "mpi_divide") == 0)
    return mpi_divide;
  else if (strcmp (name, "mpi_unsigned_divide") == 0)
    return mpi_unsigned_divide;
  else if (strcmp (name, "mpi_remainder") == 0)
    return mpi_remainder;
  else if (strcmp (name, "mpi_unsigned_remainder") == 0)
    return mpi_unsigned_remainder;
  else if (strcmp (name, "mpi_shift_right") == 0)
    return mpi_shift_right;
  else if (strcmp (name, "mpi_unsigned_shift_right") == 0)
    return mpi_unsigned_shift_right;
  else if (strcmp (name, "mpi_shift_left") == 0)
    return mpi_shift_left;
  else if (strcmp (name, "mpi_unsigned_shift_left") == 0)
    return mpi_unsigned_shift_left;
  else if (strcmp (name, "mpi_or") == 0)
    return mpi_or;
  else if (strcmp (name, "mpi_unsigned_or") == 0)
    return mpi_unsigned_or;
  else if (strcmp (name, "mpi_and") == 0)
    return mpi_and;
  else if (strcmp (name, "mpi_unsigned_and") == 0)
    return mpi_unsigned_and;
  else if (strcmp (name, "mpi_not") == 0)
    return mpi_not;
  else if (strcmp (name, "mpi_unsigned_not") == 0)
    return mpi_unsigned_not;
  else if (strcmp (name, "mpi_eq") == 0)
    return mpi_eq;
  else if (strcmp (name, "mpi_unsigned_eq") == 0)
    return mpi_unsigned_eq;
  else if (strcmp (name, "mpi_ne") == 0)
    return mpi_ne;
  else if (strcmp (name, "mpi_unsigned_ne") == 0)
    return mpi_unsigned_ne;
  else if (strcmp (name, "mpi_gt") == 0)
    return mpi_gt;
  else if (strcmp (name, "mpi_unsigned_gt") == 0)
    return mpi_unsigned_gt;
  else if (strcmp (name, "mpi_lt") == 0)
    return mpi_lt;
  else if (strcmp (name, "mpi_unsigned_lt") == 0)
    return mpi_unsigned_lt;
  else if (strcmp (name, "mpi_ge") == 0)
    return mpi_ge;
  else if (strcmp (name, "mpi_unsigned_ge") == 0)
    return mpi_unsigned_ge;
  else if (strcmp (name, "mpi_le") == 0)
    return mpi_le;
  else if (strcmp (name, "mpi_unsigned_le") == 0)
    return mpi_unsigned_le;
  else if (strcmp (name, "mpi_change_size") == 0)
    return mpi_change_size;
  else if (strcmp (name, "mpi_unsigned_change_size") == 0)
    return mpi_unsigned_change_size;
  else if (strcmp (name, "mpi_to_based_string") == 0)
    return mpi_to_based_string;
  else if (strcmp (name, "mpi_unsigned_to_based_string") == 0)
    return mpi_unsigned_to_based_string;
  else if (strcmp (name, "mpi_from_based_string") == 0)
    return mpi_from_based_string;
  else if (strcmp (name, "mpi_unsigned_from_based_string") == 0)
    return mpi_unsigned_from_based_string;
  else
    return NULL;
}
#endif
#endif
