/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

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
   operand types */

#include "d_extern.h"
#include "arithm.h"
#include "IEEE.h"
#include <string.h>
#include <stdio.h>

/* Reset, bits, round etc. initi single, float, double. */


static val_t
return_hideblock (void *hideblock)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  ER_SET_MODE (res, ER_NM_hideblock);
  ER_set_hideblock (res, hideblock);
  return val;
}

static val_t
return_int (rint_t i)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i);
  return val;
}

static val_t
return_nil (void)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  ER_SET_MODE (res, ER_NM_nil);
  return val;
}

val_t
ieee_set_trap_mask (int npars, val_t *vals)
{
  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  IEEE_set_trap_mask (ER_i ((ER_node_t) vals));
  return return_nil ();
}

val_t
ieee_get_trap_mask (int npars, val_t *vals)
{
  d_assert (npars == 0);
  return return_int (IEEE_get_trap_mask ());
}

val_t
ieee_set_sticky_status_bits (int npars, val_t *vals)
{
  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  IEEE_set_sticky_status_bits (ER_i ((ER_node_t) vals));
  return return_nil ();
}

val_t
ieee_get_sticky_status_bits (int npars, val_t *vals)
{
  d_assert (npars == 0);
  return return_int (IEEE_get_sticky_status_bits ());
}

val_t
ieee_get_status_bits (int npars, val_t *vals)
{
  d_assert (npars == 0);
  return return_int (IEEE_get_status_bits ());
}

val_t
ieee_set_round (int npars, val_t *vals)
{
  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  IEEE_set_round (ER_i ((ER_node_t) vals));
  return return_nil ();
}

val_t
ieee_get_round (int npars, val_t *vals)
{
  d_assert (npars == 0);
  return return_int (IEEE_get_round ());
}

val_t
ieee_single_positive_zero (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_positive_zero ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_positive_zero (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_positive_zero ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_positive_zero (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_positive_zero ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_negative_zero (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_negative_zero ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_negative_zero (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_negative_zero ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_negative_zero (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_negative_zero ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_nan (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_NaN ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_nan (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_NaN ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_nan (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_NaN ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_trapping_nan (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_trapping_NaN ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_trapping_nan (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_trapping_NaN ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_trapping_nan (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_trapping_NaN ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_positive_infinity (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_positive_infinity ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_positive_infinity (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_positive_infinity ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_positive_infinity (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_positive_infinity ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_negative_infinity (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_negative_infinity ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_negative_infinity (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_negative_infinity ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_negative_infinity (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_negative_infinity ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_positive_maximum (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_positive_maximum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_positive_maximum (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_positive_maximum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_positive_maximum (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_positive_maximum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_negative_maximum (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_negative_maximum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_negative_maximum (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_negative_maximum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_negative_maximum (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_negative_maximum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_positive_minimum (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_positive_minimum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_positive_minimum (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_positive_minimum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_positive_minimum (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_positive_minimum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_negative_minimum (int npars, val_t *vals)
{
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_negative_minimum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_negative_minimum (int npars, val_t *vals)
{
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_double_negative_minimum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_negative_minimum (int npars, val_t *vals)
{
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 0);
  res = IEEE_quad_negative_minimum ();
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

static void
ieee_un_op (int npars, val_t *vals, void *value, int size)
{
  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (value, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)), size);
}

val_t
ieee_is_single_positive_zero (int npars, val_t *vals)
{
  IEEE_float_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_positive_zero (temp));
}

val_t
ieee_is_double_positive_zero (int npars, val_t *vals)
{
  IEEE_double_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_double_positive_zero (temp));
}

val_t
ieee_is_quad_positive_zero (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_quad_positive_zero (temp));
}

val_t
ieee_is_single_negative_zero (int npars, val_t *vals)
{
  IEEE_float_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_negative_zero (temp));
}

val_t
ieee_is_double_negative_zero (int npars, val_t *vals)
{
  IEEE_double_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_double_negative_zero (temp));
}

val_t
ieee_is_quad_negative_zero (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_quad_negative_zero (temp));
}

val_t
ieee_is_single_nan (int npars, val_t *vals)
{
  IEEE_float_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_NaN (temp));
}

val_t
ieee_is_double_nan (int npars, val_t *vals)
{
  IEEE_double_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_double_NaN (temp));
}

val_t
ieee_is_quad_nan (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_quad_NaN (temp));
}

val_t
ieee_is_single_trapping_nan (int npars, val_t *vals)
{
  IEEE_float_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_trapping_NaN (temp));
}

val_t
ieee_is_double_trapping_nan (int npars, val_t *vals)
{
  IEEE_double_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_double_trapping_NaN (temp));
}

val_t
ieee_is_quad_trapping_nan (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_quad_trapping_NaN (temp));
}

val_t
ieee_is_single_positive_infinity (int npars, val_t *vals)
{
  IEEE_float_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_positive_infinity (temp));
}

val_t
ieee_is_double_positive_infinity (int npars, val_t *vals)
{
  IEEE_double_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_double_positive_infinity (temp));
}

val_t
ieee_is_quad_positive_infinity (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_quad_positive_infinity (temp));
}

val_t
ieee_is_single_negative_infinity (int npars, val_t *vals)
{
  IEEE_float_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_negative_infinity (temp));
}

val_t
ieee_is_double_negative_infinity (int npars, val_t *vals)
{
  IEEE_double_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_double_negative_infinity (temp));
}

val_t
ieee_is_quad_negative_infinity (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_quad_negative_infinity (temp));
}

val_t
ieee_is_single_normalized (int npars, val_t *vals)
{
  IEEE_float_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_normalized (temp));
}

val_t
ieee_is_double_normalized (int npars, val_t *vals)
{
  IEEE_double_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_double_normalized (temp));
}

val_t
ieee_is_quad_normalized (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_quad_normalized (temp));
}

val_t
ieee_is_single_denormalized (int npars, val_t *vals)
{
  IEEE_float_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_denormalized (temp));
}

val_t
ieee_is_double_denormalized (int npars, val_t *vals)
{
  IEEE_double_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_double_denormalized (temp));
}

val_t
ieee_is_quad_denormalized (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  
  ieee_un_op (npars, vals, &temp, sizeof (temp));
  return return_int (IEEE_is_quad_denormalized (temp));
}

static void
ieee_bin_op (int npars, val_t *vals, void *value1, void *value2, int size)
{
  d_assert (npars == 2
	    && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock
	    && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_hideblock);
  memcpy (value1, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)), size);
  memcpy (value2, ER_hideblock_start (ER_hideblock ((ER_node_t) (vals + 1))),
	  size);
}
 
val_t
ieee_add_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_add_single (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_add_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_add_double (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_add_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_add_quad (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_subtract_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_subtract_single (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_subtract_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_subtract_double (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_subtract_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_subtract_quad (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_multiply_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_multiply_single (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_multiply_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_multiply_double (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_multiply_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_multiply_quad (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_divide_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_divide_single (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_divide_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_divide_double (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_divide_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2, res;
  void *hideblock;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  res = IEEE_divide_quad (temp1, temp2);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_eq_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_eq_single (temp1, temp2));
}

val_t
ieee_eq_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_eq_double (temp1, temp2));
}

val_t
ieee_eq_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_eq_quad (temp1, temp2));
}

val_t
ieee_ne_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_ne_single (temp1, temp2));
}

val_t
ieee_ne_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_ne_double (temp1, temp2));
}

val_t
ieee_ne_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_ne_quad (temp1, temp2));
}

val_t
ieee_lt_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_lt_single (temp1, temp2));
}

val_t
ieee_lt_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_lt_double (temp1, temp2));
}

val_t
ieee_lt_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_lt_quad (temp1, temp2));
}

val_t
ieee_gt_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_gt_single (temp1, temp2));
}

val_t
ieee_gt_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_gt_double (temp1, temp2));
}

val_t
ieee_gt_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_gt_quad (temp1, temp2));
}

val_t
ieee_le_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_le_single (temp1, temp2));
}

val_t
ieee_le_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_le_double (temp1, temp2));
}

val_t
ieee_le_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_le_quad (temp1, temp2));
}

val_t
ieee_ge_single (int npars, val_t *vals)
{
  IEEE_float_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_ge_single (temp1, temp2));
}

val_t
ieee_ge_double (int npars, val_t *vals)
{
  IEEE_double_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_ge_double (temp1, temp2));
}

val_t
ieee_ge_quad (int npars, val_t *vals)
{
  IEEE_quad_t temp1, temp2;

  ieee_bin_op (npars, vals, &temp1, &temp2, sizeof (temp1));
  return return_int (IEEE_ge_quad (temp1, temp2));
}

val_t
ieee_single_to_double (int npars, val_t *vals)
{
  IEEE_float_t temp;
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  res = IEEE_single_to_double (temp);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_single_to_quad (int npars, val_t *vals)
{
  IEEE_float_t temp;
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  res = IEEE_single_to_quad (temp);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_to_single (int npars, val_t *vals)
{
  IEEE_double_t temp;
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  res = IEEE_double_to_single (temp);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_double_to_quad (int npars, val_t *vals)
{
  IEEE_double_t temp;
  IEEE_quad_t res;
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  res = IEEE_double_to_quad (temp);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_to_single (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  IEEE_float_t res;
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  res = IEEE_quad_to_single (temp);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_to_double (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  IEEE_double_t res;
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  res = IEEE_quad_to_double (temp);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

static void *
extract_mpi (int npars, val_t *vals, rint_t *size)
{
  ER_node_t var;
  ER_node_t size_var;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_stack);
  var = IVAL (ER_stack_vars (ER_stack ((ER_node_t) vals)), 1);
  size_var = IVAL (ER_stack_vars (ER_stack ((ER_node_t) vals)), 0);
  d_assert (ER_NODE_MODE (var) == ER_NM_hideblock
	    && ER_NODE_MODE (size_var) == ER_NM_int);
  *size = ER_i (size_var);
  return ER_hideblock_start (ER_hideblock (var));
}

val_t
ieee_single_from_integer (int npars, val_t *vals)
{
  rint_t size;
  IEEE_float_t res;
  void *hideblock;

  hideblock = extract_mpi (npars, vals, &size);
  res = IEEE_single_from_integer (size, hideblock);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_integer_from_single (int npars, val_t *vals)
{
  rint_t size;
  IEEE_float_t temp;
  void *hideblock;

  d_assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  hideblock = extract_mpi (npars - 1, vals + 1, &size);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  IEEE_single_to_integer (size, temp, hideblock);
  return return_nil ();
}

val_t
ieee_double_from_integer (int npars, val_t *vals)
{
  rint_t size;
  IEEE_double_t res;
  void *hideblock;

  hideblock = extract_mpi (npars, vals, &size);
  res = IEEE_double_from_integer (size, hideblock);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_integer_from_double (int npars, val_t *vals)
{
  rint_t size;
  IEEE_double_t temp;
  void *hideblock;

  d_assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  hideblock = extract_mpi (npars - 1, vals + 1, &size);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  IEEE_double_to_integer (size, temp, hideblock);
  return return_nil ();
}

val_t
ieee_quad_from_integer (int npars, val_t *vals)
{
  rint_t size;
  IEEE_quad_t res;
  void *hideblock;

  hideblock = extract_mpi (npars, vals, &size);
  res = IEEE_quad_from_integer (size, hideblock);
  hideblock = create_hideblock (sizeof (res));
  memcpy (ER_hideblock_start ((ER_node_t) hideblock), &res, sizeof (res));
  return return_hideblock (hideblock);
}

val_t
ieee_integer_from_quad (int npars, val_t *vals)
{
  rint_t size;
  IEEE_quad_t temp;
  void *hideblock;

  d_assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  hideblock = extract_mpi (npars - 1, vals + 1, &size);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  sizeof (temp));
  IEEE_quad_to_integer (size, temp, hideblock);
  return return_nil ();
}

val_t
ieee_single_to_binary_string (int npars, val_t *vals)
{
  IEEE_float_t temp;
  int base;
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t vect;
  char str [80];

  d_assert (npars == 2
	    && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock
	    && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  IEEE_FLOAT_SIZE);
  base = ER_i ((ER_node_t) (vals + 1));
  IEEE_single_to_binary_string (temp, base, str);
  vect = create_string (str);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
  return val;
}

val_t
ieee_single_to_string (int npars, val_t *vals)
{
  IEEE_float_t temp;
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t vect;
  char str [80];

  d_assert (npars == 1
	    && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  IEEE_FLOAT_SIZE);
  IEEE_single_to_string (temp, str);
  vect = create_string (str);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
  return val;
}

val_t
ieee_double_to_binary_string (int npars, val_t *vals)
{
  IEEE_double_t temp;
  int base;
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t vect;
  char str [80];

  d_assert (npars == 2
	    && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  IEEE_DOUBLE_SIZE);
  base = ER_i ((ER_node_t) (vals + 1));
  IEEE_double_to_binary_string (temp, base, str);
  vect = create_string (str);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
  return val;
}

val_t
ieee_double_to_string (int npars, val_t *vals)
{
  IEEE_double_t temp;
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t vect;
  char str [80];

  d_assert (npars == 1
	    && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  IEEE_DOUBLE_SIZE);
  IEEE_double_to_string (temp, str);
  vect = create_string (str);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
  return val;
}

val_t
ieee_quad_to_binary_string (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  int base;
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t vect;
  char str [80];

  d_assert (npars == 2
	    && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  IEEE_QUAD_SIZE);
  base = ER_i ((ER_node_t) (vals + 1));
  IEEE_quad_to_binary_string (temp, base, str);
  vect = create_string (str);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
  return val;
}

val_t
ieee_quad_to_string (int npars, val_t *vals)
{
  IEEE_quad_t temp;
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t vect;
  char str [80];

  d_assert (npars == 1
	    && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  memcpy (&temp, ER_hideblock_start (ER_hideblock ((ER_node_t) vals)),
	  IEEE_QUAD_SIZE);
  IEEE_quad_to_string (temp, str);
  vect = create_string (str);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
  return val;
}

val_t
ieee_single_from_float (int npars, val_t *vals)
{
  void *hideblock;
  char str [40];

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_float);
  sprintf (str, "%.20e", ER_f ((ER_node_t) vals));
  hideblock = create_hideblock (IEEE_FLOAT_SIZE);
  IEEE_single_from_string
    (str, (IEEE_float_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_double_from_float (int npars, val_t *vals)
{
  void *hideblock;
  char str [40];

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_float);
  sprintf (str, "%.20e", ER_f ((ER_node_t) vals));
  hideblock = create_hideblock (IEEE_DOUBLE_SIZE);
  IEEE_double_from_string
    (str, (IEEE_double_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_from_float (int npars, val_t *vals)
{
  void *hideblock;
  char str [40];

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_float);
  sprintf (str, "%.20e", ER_f ((ER_node_t) vals));
  hideblock = create_hideblock (IEEE_QUAD_SIZE);
  IEEE_quad_from_string
    (str, (IEEE_quad_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_single_from_binary_string (int npars, val_t *vals)
{
  void *hideblock;
  int base;

  d_assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	    && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  hideblock = create_hideblock (IEEE_FLOAT_SIZE);
  base = ER_i ((ER_node_t) (vals + 1));
  IEEE_single_from_binary_string
    (ER_pack_els (ER_vect ((ER_node_t) vals)), base,
     (IEEE_float_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_single_from_string (int npars, val_t *vals)
{
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect);
  hideblock = create_hideblock (IEEE_FLOAT_SIZE);
  IEEE_single_from_string
    (ER_pack_els (ER_vect ((ER_node_t) vals)),
     (IEEE_float_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_double_from_binary_string (int npars, val_t *vals)
{
  void *hideblock;
  int base;

  d_assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	    && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  hideblock = create_hideblock (IEEE_DOUBLE_SIZE);
  base = ER_i ((ER_node_t) (vals + 1));
  IEEE_double_from_binary_string
    (ER_pack_els (ER_vect ((ER_node_t) vals)), base,
     (IEEE_double_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_double_from_string (int npars, val_t *vals)
{
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect);
  hideblock = create_hideblock (IEEE_DOUBLE_SIZE);
  IEEE_double_from_string
    (ER_pack_els (ER_vect ((ER_node_t) vals)),
     (IEEE_double_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_from_binary_string (int npars, val_t *vals)
{
  void *hideblock;
  int base;

  d_assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	    && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  hideblock = create_hideblock (IEEE_QUAD_SIZE);
  base = ER_i ((ER_node_t) (vals + 1));
  IEEE_quad_from_binary_string
    (ER_pack_els (ER_vect ((ER_node_t) vals)), base,
     (IEEE_quad_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_quad_from_string (int npars, val_t *vals)
{
  void *hideblock;

  d_assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect);
  hideblock = create_hideblock (IEEE_QUAD_SIZE);
  IEEE_quad_from_string
    (ER_pack_els (ER_vect ((ER_node_t) vals)),
     (IEEE_quad_t *) ER_hideblock_start ((ER_node_t) hideblock));
  return return_hideblock (hideblock);
}

val_t
ieee_reset (int npars, val_t *vals)
{
  d_assert (npars == 0);
  IEEE_reset ();
  return return_nil ();
}

#if !defined(HAVE_DLOPEN) || defined(NO_EXTERN_SHLIB)

/* Function for implementing externals with static libraries.  See all
   externals name in ieee.d. */
void *
ieee_address (const char *name)
{
  if (strcmp (name, "ieee_set_trap_mask") == 0)
    return ieee_set_trap_mask;
  else if (strcmp (name, "ieee_get_trap_mask") == 0)
    return ieee_get_trap_mask;
  else if (strcmp (name, "ieee_set_sticky_status_bits") == 0)
    return ieee_set_sticky_status_bits;
  else if (strcmp (name, "ieee_get_sticky_status_bits") == 0)
    return ieee_get_sticky_status_bits;
  else if (strcmp (name, "ieee_get_status_bits") == 0)
    return ieee_get_status_bits;
  else if (strcmp (name, "ieee_set_round") == 0)
    return ieee_set_round;
  else if (strcmp (name, "ieee_get_round") == 0)
    return ieee_get_round;
  else if (strcmp (name, "ieee_single_positive_zero") == 0)
    return ieee_single_positive_zero;
  else if (strcmp (name, "ieee_double_positive_zero") == 0)
    return ieee_double_positive_zero;
  else if (strcmp (name, "ieee_quad_positive_zero") == 0)
    return ieee_quad_positive_zero;
  else if (strcmp (name, "ieee_single_negative_zero") == 0)
    return ieee_single_negative_zero;
  else if (strcmp (name, "ieee_double_negative_zero") == 0)
    return ieee_double_negative_zero;
  else if (strcmp (name, "ieee_quad_negative_zero") == 0)
    return ieee_quad_negative_zero;
  else if (strcmp (name, "ieee_single_nan") == 0)
    return ieee_single_nan;
  else if (strcmp (name, "ieee_double_nan") == 0)
    return ieee_double_nan;
  else if (strcmp (name, "ieee_quad_nan") == 0)
    return ieee_quad_nan;
  else if (strcmp (name, "ieee_single_trapping_nan") == 0)
    return ieee_single_trapping_nan;
  else if (strcmp (name, "ieee_double_trapping_nan") == 0)
    return ieee_double_trapping_nan;
  else if (strcmp (name, "ieee_quad_trapping_nan") == 0)
    return ieee_quad_trapping_nan;
  else if (strcmp (name, "ieee_single_positive_infinity") == 0)
    return ieee_single_positive_infinity;
  else if (strcmp (name, "ieee_double_positive_infinity") == 0)
    return ieee_double_positive_infinity;
  else if (strcmp (name, "ieee_quad_positive_infinity") == 0)
    return ieee_quad_positive_infinity;
  else if (strcmp (name, "ieee_single_negative_infinity") == 0)
    return ieee_single_negative_infinity;
  else if (strcmp (name, "ieee_double_negative_infinity") == 0)
    return ieee_double_negative_infinity;
  else if (strcmp (name, "ieee_quad_negative_infinity") == 0)
    return ieee_quad_negative_infinity;
  else if (strcmp (name, "ieee_single_positive_maximum") == 0)
    return ieee_single_positive_maximum;
  else if (strcmp (name, "ieee_double_positive_maximum") == 0)
    return ieee_double_positive_maximum;
  else if (strcmp (name, "ieee_quad_positive_maximum") == 0)
    return ieee_quad_positive_maximum;
  else if (strcmp (name, "ieee_single_negative_maximum") == 0)
    return ieee_single_negative_maximum;
  else if (strcmp (name, "ieee_double_negative_maximum") == 0)
    return ieee_double_negative_maximum;
  else if (strcmp (name, "ieee_quad_negative_maximum") == 0)
    return ieee_quad_negative_maximum;
  else if (strcmp (name, "ieee_single_positive_minimum") == 0)
    return ieee_single_positive_minimum;
  else if (strcmp (name, "ieee_double_positive_minimum") == 0)
    return ieee_double_positive_minimum;
  else if (strcmp (name, "ieee_quad_positive_minimum") == 0)
    return ieee_quad_positive_minimum;
  else if (strcmp (name, "ieee_single_negative_minimum") == 0)
    return ieee_single_negative_minimum;
  else if (strcmp (name, "ieee_double_negative_minimum") == 0)
    return ieee_double_negative_minimum;
  else if (strcmp (name, "ieee_quad_negative_minimum") == 0)
    return ieee_quad_negative_minimum;
  else if (strcmp (name, "ieee_is_single_positive_zero") == 0)
    return ieee_is_single_positive_zero;
  else if (strcmp (name, "ieee_is_double_positive_zero") == 0)
    return ieee_is_double_positive_zero;
  else if (strcmp (name, "ieee_is_quad_positive_zero") == 0)
    return ieee_is_quad_positive_zero;
  else if (strcmp (name, "ieee_is_single_negative_zero") == 0)
    return ieee_is_single_negative_zero;
  else if (strcmp (name, "ieee_is_double_negative_zero") == 0)
    return ieee_is_double_negative_zero;
  else if (strcmp (name, "ieee_is_quad_negative_zero") == 0)
    return ieee_is_quad_negative_zero;
  else if (strcmp (name, "ieee_is_single_nan") == 0)
    return ieee_is_single_nan;
  else if (strcmp (name, "ieee_is_double_nan") == 0)
    return ieee_is_double_nan;
  else if (strcmp (name, "ieee_is_quad_nan") == 0)
    return ieee_is_quad_nan;
  else if (strcmp (name, "ieee_is_single_trapping_nan") == 0)
    return ieee_is_single_trapping_nan;
  else if (strcmp (name, "ieee_is_double_trapping_nan") == 0)
    return ieee_is_double_trapping_nan;
  else if (strcmp (name, "ieee_is_quad_trapping_nan") == 0)
    return ieee_is_quad_trapping_nan;
  else if (strcmp (name, "ieee_is_single_positive_infinity") == 0)
    return ieee_is_single_positive_infinity;
  else if (strcmp (name, "ieee_is_double_positive_infinity") == 0)
    return ieee_is_double_positive_infinity;
  else if (strcmp (name, "ieee_is_quad_positive_infinity") == 0)
    return ieee_is_quad_positive_infinity;
  else if (strcmp (name, "ieee_is_single_negative_infinity") == 0)
    return ieee_is_single_negative_infinity;
  else if (strcmp (name, "ieee_is_double_negative_infinity") == 0)
    return ieee_is_double_negative_infinity;
  else if (strcmp (name, "ieee_is_quad_negative_infinity") == 0)
    return ieee_is_quad_negative_infinity;
  else if (strcmp (name, "ieee_is_single_normalized") == 0)
    return ieee_is_single_normalized;
  else if (strcmp (name, "ieee_is_double_normalized") == 0)
    return ieee_is_double_normalized;
  else if (strcmp (name, "ieee_is_quad_normalized") == 0)
    return ieee_is_quad_normalized;
  else if (strcmp (name, "ieee_is_single_denormalized") == 0)
    return ieee_is_single_denormalized;
  else if (strcmp (name, "ieee_is_double_denormalized") == 0)
    return ieee_is_double_denormalized;
  else if (strcmp (name, "ieee_is_quad_denormalized") == 0)
    return ieee_is_quad_denormalized;
  else if (strcmp (name, "ieee_add_single") == 0)
    return ieee_add_single;
  else if (strcmp (name, "ieee_add_double") == 0)
    return ieee_add_double;
  else if (strcmp (name, "ieee_add_quad") == 0)
    return ieee_add_quad;
  else if (strcmp (name, "ieee_subtract_single") == 0)
    return ieee_subtract_single;
  else if (strcmp (name, "ieee_subtract_double") == 0)
    return ieee_subtract_double;
  else if (strcmp (name, "ieee_subtract_quad") == 0)
    return ieee_subtract_quad;
  else if (strcmp (name, "ieee_multiply_single") == 0)
    return ieee_multiply_single;
  else if (strcmp (name, "ieee_multiply_double") == 0)
    return ieee_multiply_double;
  else if (strcmp (name, "ieee_multiply_quad") == 0)
    return ieee_multiply_quad;
  else if (strcmp (name, "ieee_divide_single") == 0)
    return ieee_divide_single;
  else if (strcmp (name, "ieee_divide_double") == 0)
    return ieee_divide_double;
  else if (strcmp (name, "ieee_divide_quad") == 0)
    return ieee_divide_quad;
  else if (strcmp (name, "ieee_eq_single") == 0)
    return ieee_eq_single;
  else if (strcmp (name, "ieee_eq_double") == 0)
    return ieee_eq_double;
  else if (strcmp (name, "ieee_eq_quad") == 0)
    return ieee_eq_quad;
  else if (strcmp (name, "ieee_ne_single") == 0)
    return ieee_ne_single;
  else if (strcmp (name, "ieee_ne_double") == 0)
    return ieee_ne_double;
  else if (strcmp (name, "ieee_ne_quad") == 0)
    return ieee_ne_quad;
  else if (strcmp (name, "ieee_lt_single") == 0)
    return ieee_lt_single;
  else if (strcmp (name, "ieee_lt_double") == 0)
    return ieee_lt_double;
  else if (strcmp (name, "ieee_lt_quad") == 0)
    return ieee_lt_quad;
  else if (strcmp (name, "ieee_gt_single") == 0)
    return ieee_gt_single;
  else if (strcmp (name, "ieee_gt_double") == 0)
    return ieee_gt_double;
  else if (strcmp (name, "ieee_gt_quad") == 0)
    return ieee_gt_quad;
  else if (strcmp (name, "ieee_le_single") == 0)
    return ieee_le_single;
  else if (strcmp (name, "ieee_le_double") == 0)
    return ieee_le_double;
  else if (strcmp (name, "ieee_le_quad") == 0)
    return ieee_le_quad;
  else if (strcmp (name, "ieee_ge_single") == 0)
    return ieee_ge_single;
  else if (strcmp (name, "ieee_ge_double") == 0)
    return ieee_ge_double;
  else if (strcmp (name, "ieee_ge_quad") == 0)
    return ieee_ge_quad;
  else if (strcmp (name, "ieee_single_to_double") == 0)
    return ieee_single_to_double;
  else if (strcmp (name, "ieee_single_to_quad") == 0)
    return ieee_single_to_quad;
  else if (strcmp (name, "ieee_double_to_single") == 0)
    return ieee_double_to_single;
  else if (strcmp (name, "ieee_double_to_quad") == 0)
    return ieee_double_to_quad;
  else if (strcmp (name, "ieee_quad_to_single") == 0)
    return ieee_quad_to_single;
  else if (strcmp (name, "ieee_quad_to_double") == 0)
    return ieee_quad_to_double;
  else if (strcmp (name, "ieee_single_from_integer") == 0)
    return ieee_single_from_integer;
  else if (strcmp (name, "ieee_integer_from_single") == 0)
    return ieee_integer_from_single;
  else if (strcmp (name, "ieee_double_from_integer") == 0)
    return ieee_double_from_integer;
  else if (strcmp (name, "ieee_integer_from_double") == 0)
    return ieee_integer_from_double;
  else if (strcmp (name, "ieee_quad_from_integer") == 0)
    return ieee_quad_from_integer;
  else if (strcmp (name, "ieee_integer_from_quad") == 0)
    return ieee_integer_from_quad;
  else if (strcmp (name, "ieee_single_to_binary_string") == 0)
    return ieee_single_to_binary_string;
  else if (strcmp (name, "ieee_single_to_string") == 0)
    return ieee_single_to_string;
  else if (strcmp (name, "ieee_double_to_binary_string") == 0)
    return ieee_double_to_binary_string;
  else if (strcmp (name, "ieee_double_to_string") == 0)
    return ieee_double_to_string;
  else if (strcmp (name, "ieee_quad_to_binary_string") == 0)
    return ieee_quad_to_binary_string;
  else if (strcmp (name, "ieee_quad_to_string") == 0)
    return ieee_quad_to_string;
  else if (strcmp (name, "ieee_single_from_float") == 0)
    return ieee_single_from_float;
  else if (strcmp (name, "ieee_double_from_float") == 0)
    return ieee_double_from_float;
  else if (strcmp (name, "ieee_quad_from_float") == 0)
    return ieee_quad_from_float;
  else if (strcmp (name, "ieee_single_from_binary_string") == 0)
    return ieee_single_from_binary_string;
  else if (strcmp (name, "ieee_single_from_string") == 0)
    return ieee_single_from_string;
  else if (strcmp (name, "ieee_double_from_binary_string") == 0)
    return ieee_double_from_binary_string;
  else if (strcmp (name, "ieee_double_from_string") == 0)
    return ieee_double_from_string;
  else if (strcmp (name, "ieee_quad_from_binary_string") == 0)
    return ieee_quad_from_binary_string;
  else if (strcmp (name, "ieee_quad_from_string") == 0)
    return ieee_quad_from_string;
  else if (strcmp (name, "ieee_reset") == 0)
    return ieee_reset;
  else
    return NULL;
}
#endif
