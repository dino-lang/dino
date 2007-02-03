/*
   Copyright (C) 1997-2007 Vladimir Makarov.

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
#include <stdlib.h>

#ifdef HAVE_GMP_H
#include "gmp.h"
#endif

#include <assert.h>

#ifdef HAVE_GMP_H
static val_t
return_hideblock (ER_node_t hideblock)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  ER_SET_MODE (res, ER_NM_hideblock);
  ER_set_hideblock (res, hideblock);
  return val;
}

val_t
_z_create (int npars, val_t *vals)
{
  void *mpz;
  ER_node_t hideblock;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  hideblock = create_hideblock (sizeof (mpz_t));
  mpz = ER_hideblock_start (hideblock);
  mpz_init (mpz);
  mpz_set_si (mpz, ER_i ((ER_node_t) vals));
  return return_hideblock (hideblock);
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
_z_clear (int npars, val_t *vals)
{
  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_hideblock);
  mpz_clear (* (mpz_t *) ER_hideblock_start ((ER_node_t) vals));
  return return_nil ();
}

static void
mpz_three_op (int npars, val_t *vals, void **hidevalue1, void **hidevalue2,
	       void **first_hidevalue)
{
  ER_node_t var1;
  ER_node_t var2;
  ER_node_t first_var;

  assert (npars == 3
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 2)) == ER_NM_instance);
  var1 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		      1);
  var2 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 2))),
		      1);
  first_var
    = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  *hidevalue1 = ER_hideblock_start (ER_hideblock (var1));
  *hidevalue2 = ER_hideblock_start (ER_hideblock (var2));
  *first_hidevalue = ER_hideblock_start (ER_hideblock (first_var));
}

static void
mpz_two_op_ui (int npars, val_t *vals, void **hidevalue, int_t *ui,
	       void **first_hidevalue)
{
  ER_node_t var1;
  ER_node_t first_var;

  assert (npars == 3
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 2)) == ER_NM_int);
  var1 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		      1);
  first_var
    = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  *hidevalue = ER_hideblock_start (ER_hideblock (var1));
  *ui = ER_i ((ER_node_t) (vals + 2));
  *first_hidevalue = ER_hideblock_start (ER_hideblock (first_var));
}

static void
mpz_two_op (int npars, val_t *vals, void **hidevalue, void **first_hidevalue)
{
  ER_node_t var;
  ER_node_t first_var;

  assert (npars == 2
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		     1);
  first_var
    = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  *hidevalue = ER_hideblock_start (ER_hideblock (var));
  *first_hidevalue = ER_hideblock_start (ER_hideblock (first_var));
}

static void
mpz_one_op_ui (int npars, val_t *vals, int_t *ui, void **first_hidevalue)
{
  ER_node_t first_var;

  assert (npars == 2
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  first_var
    = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  *ui = ER_i ((ER_node_t) (vals + 1));
  *first_hidevalue = ER_hideblock_start (ER_hideblock (first_var));
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

static val_t
return_float (floating_t f)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f);
  return val;
}

static val_t
arithm_op (int npars, val_t *vals,
	   void (*func) (mpz_t, const mpz_t, const mpz_t))
{
  void *hidevalue1, *hidevalue2, *res_hidevalue;

  mpz_three_op (npars, vals, &hidevalue1, &hidevalue2, &res_hidevalue);
  (*func) (*(mpz_t *) res_hidevalue, *(mpz_t *) hidevalue1,
	   *(mpz_t *) hidevalue2);
  return vals [0];
}

static val_t
arithm_unary_op (int npars, val_t *vals, void (*func) (mpz_t, const mpz_t))
{
  void *hidevalue, *res_hidevalue;

  mpz_two_op (npars, vals, &hidevalue, &res_hidevalue);
  (*func) (*(mpz_t *) res_hidevalue, *(mpz_t *) hidevalue);
  return vals [0];
}

static val_t
op_ui (int npars, val_t *vals,
       void (*func) (mpz_t, const mpz_t, unsigned long ui))
{
  void *hidevalue, *res_hidevalue;
  int_t ui;

  mpz_two_op_ui (npars, vals, &hidevalue, &ui, &res_hidevalue);
  (*func) (*(mpz_t *) res_hidevalue, *(mpz_t *) hidevalue, (unsigned long) ui);
  return vals [0];
}

static val_t
int_op_ui (int npars, val_t *vals,
	   int (*func) (mpz_t, const mpz_t, unsigned long ui))
{
  void *hidevalue, *res_hidevalue;
  int_t ui;

  mpz_two_op_ui (npars, vals, &hidevalue, &ui, &res_hidevalue);
  (*func) (*(mpz_t *) res_hidevalue, *(mpz_t *) hidevalue, (unsigned long) ui);
  return vals [0];
}

val_t
_z_add (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_add);
}

val_t
_z_sub (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_sub);
}

val_t
_z_neg (int npars, val_t *vals)
{
  return arithm_unary_op (npars, vals, mpz_neg);
}

val_t
_z_abs (int npars, val_t *vals)
{
  return arithm_unary_op (npars, vals, mpz_abs);
}

val_t
_z_mul (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_mul);
}

val_t
_z_tdiv_q (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_tdiv_q);
}

val_t
_z_tdiv_r (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_tdiv_r);
}

val_t
_z_pow_ui (int npars, val_t *vals)
{
  return op_ui (npars, vals, mpz_pow_ui);
}

val_t
_z_root (int npars, val_t *vals)
{
  return int_op_ui (npars, vals, mpz_root);
}

static val_t
int_res_op (int npars, val_t *vals, int (*func) (const mpz_t, const mpz_t))
{
  void *hidevalue1, *hidevalue2;
  int res;

  mpz_two_op (npars, vals, &hidevalue2, &hidevalue1);
  res = (*func) (*(mpz_t *) hidevalue1, *(mpz_t *) hidevalue2);
  return return_int (res);
}

val_t
_z_cmp (int npars, val_t *vals)
{
  return int_res_op (npars, vals, mpz_cmp);
}

val_t
_z_ior (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_ior);
}

val_t
_z_xor (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_xor);
}

val_t
_z_and (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_and);
}

val_t
_z_com (int npars, val_t *vals)
{
  return arithm_unary_op (npars, vals, mpz_com);
}

val_t
_z_set (int npars, val_t *vals)
{
  void *hidevalue, *res_hidevalue;

  mpz_two_op (npars, vals, &hidevalue, &res_hidevalue);
  mpz_set (*(mpz_t *) res_hidevalue, *(mpz_t *) hidevalue);
  return return_nil ();
}

val_t
_z_set_str (int npars, val_t *vals)
{
  ER_node_t res_var, var;

  assert (npars == 2
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_vect);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		     1);
  res_var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  mpz_set_str (*(mpz_t *) ER_hideblock_start (ER_hideblock (res_var)),
	       ER_pack_els (ER_vect ((ER_node_t) (vals + 1))), 0);
  return return_nil ();
}

val_t
_z_get_si (int npars, val_t *vals)
{
  ER_node_t var;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  return return_int (mpz_get_si (*(mpz_t *)
				 ER_hideblock_start (ER_hideblock (var))));
}

val_t
_z_get_d (int npars, val_t *vals)
{
  ER_node_t var;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  return return_float (mpz_get_d (*(mpz_t *)
				  ER_hideblock_start (ER_hideblock (var))));
}

val_t
_z_get_str (int npars, val_t *vals)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t vect;
  void *hidevalue;
  char *str, *start;
  int_t ui;
  int len;

  mpz_one_op_ui (npars, vals, &ui, &hidevalue);
  len = mpz_sizeinbase (*(mpz_t *) hidevalue, ui) + 4;
  start = str = malloc (len);
  if (ui == 16)
    {
      *str++= '0';
      *str++= 'X';
    }
  else if (ui == 2)
    {
      *str++= '0';
      *str++= 'B';
    }
  else if (ui == 8)
    *str++= '0';
  mpz_get_str (str, ui, *(mpz_t *) hidevalue);
  vect = create_string (start);
  free (start);
  ER_SET_MODE (res, ER_NM_vect);
  ER_set_vect (res, vect);
  return val;
}

val_t
_z_set_bit (int npars, val_t *vals)
{
  void *res_hidevalue;
  int_t ui;

  mpz_one_op_ui (npars, vals, &ui, &res_hidevalue);
  mpz_setbit (*(mpz_t *) res_hidevalue, ui);
  return return_nil ();
}

val_t
_z_clr_bit (int npars, val_t *vals)
{
  void *res_hidevalue;
  int_t ui;

  mpz_one_op_ui (npars, vals, &ui, &res_hidevalue);
  mpz_clrbit (*(mpz_t *) res_hidevalue, ui);
  return return_nil ();
}

val_t
_z_tst_bit (int npars, val_t *vals)
{
  void *res_hidevalue;
  int_t ui;

  mpz_one_op_ui (npars, vals, &ui, &res_hidevalue);
  return return_int (mpz_tstbit (*(mpz_t *) res_hidevalue, ui));
}

val_t
_z_urandomm (int npars, val_t *vals)
{
  void *hidevalue, *res_hidevalue;
  int_t ui;
  static gmp_randstate_t state;

  mpz_two_op_ui (npars, vals, &hidevalue, &ui, &res_hidevalue);
  if (ui != 0)
    gmp_randinit_default (state);
  mpz_urandomm (*(mpz_t *) res_hidevalue, state, *(mpz_t *) hidevalue);
  return vals [0];
}

#endif /* #ifdef HAVE_GMP_H */

#if !defined(HAVE_DLOPEN) || defined(NO_EXTERN_SHLIB)

/* Function for implementing externals with static libraries.  See all
   externals name in mpz.d. */
void *
mpz_address (const char *name)
{
#ifdef HAVE_GMP_H
  if (strcmp (name, "_z_create") == 0)
    return _z_create;
  else if (strcmp (name, "_z_clear") == 0)
    return _z_clear;
  else if (strcmp (name, "_z_set") == 0)
    return _z_set;
  else if (strcmp (name, "_z_set_str") == 0)
    return _z_set_str;
  else if (strcmp (name, "_z_get_si") == 0)
    return _z_get_si;
  else if (strcmp (name, "_z_get_d") == 0)
    return _z_get_d;
  else if (strcmp (name, "_z_get_str") == 0)
    return _z_get_str;
  else if (strcmp (name, "_z_set_bit") == 0)
    return _z_set_bit;
  else if (strcmp (name, "_z_clr_bit") == 0)
    return _z_clr_bit;
  else if (strcmp (name, "_z_tst_bit") == 0)
    return _z_tst_bit;
  else if (strcmp (name, "_z_add") == 0)
    return _z_add;
  else if (strcmp (name, "_z_sub") == 0)
    return _z_sub;
  else if (strcmp (name, "_z_neg") == 0)
    return _z_neg;
  else if (strcmp (name, "_z_abs") == 0)
    return _z_abs;
  else if (strcmp (name, "_z_mul") == 0)
    return _z_mul;
  else if (strcmp (name, "_z_tdiv_q") == 0)
    return _z_tdiv_q;
  else if (strcmp (name, "_z_tdiv_r") == 0)
    return _z_tdiv_r;
  else if (strcmp (name, "_z_pow_ui") == 0)
    return _z_pow_ui;
  else if (strcmp (name, "_z_root") == 0)
    return _z_root;
  else if (strcmp (name, "_z_cmp") == 0)
    return _z_cmp;
  else if (strcmp (name, "_z_ior") == 0)
    return _z_ior;
  else if (strcmp (name, "_z_xor") == 0)
    return _z_xor;
  else if (strcmp (name, "_z_and") == 0)
    return _z_and;
  else if (strcmp (name, "_z_com") == 0)
    return _z_com;
  else if (strcmp (name, "_z_urandomm") == 0)
    return _z_com;
  else
#endif /* #ifdef HAVE_GMP_H */
    return NULL;
}
#endif
