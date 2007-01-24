/*
   Copyright (C) 1997-2005 Vladimir Makarov.

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
mpz_binary_op (int npars, val_t *vals, void **hidevalue1, void **hidevalue2,
	       void **res_hidevalue)
{
  ER_node_t var1;
  ER_node_t var2;
  ER_node_t res_var;
  ER_node_t init_var;
  ER_node_t hideblock;

  assert (npars == 3
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance
	  && ER_NODE_MODE ((ER_node_t) (vals + 2)) == ER_NM_instance);
  var1 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		      1);
  var2 = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 2))),
		      1);
  res_var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  *hidevalue1 = ER_hideblock_start (ER_hideblock (var1));
  *hidevalue2 = ER_hideblock_start (ER_hideblock (var2));
  *res_hidevalue = ER_hideblock_start (ER_hideblock (res_var));
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
arithm_op (int npars, val_t *vals, void (*func) (mpz_t, const mpz_t, const mpz_t))
{
  void *hidevalue1, *hidevalue2, *res_hidevalue;

  mpz_binary_op (npars, vals, &hidevalue1, &hidevalue2, &res_hidevalue);
  (*func) (*(mpz_t *) res_hidevalue, *(mpz_t *) hidevalue1,
	   *(mpz_t *) hidevalue2);
  return vals [0];
}

val_t
_z_add (int npars, val_t *vals)
{
  return arithm_op (npars, vals, mpz_add);
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
_z_get_si (int npars, val_t *vals)
{
  ER_node_t var;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  return return_int (mpz_get_si (*(mpz_t *)
				 ER_hideblock_start (ER_hideblock (var))));
}

#endif /* #ifdef HAVE_GMP_H */

#if !defined(HAVE_DLOPEN) || defined(NO_EXTERN_SHLIB)

/* Function for implementing externals with static libraries.  See all
   externals name in mpz.d. */
void *
mpz_address (const char *name)
{
  if (strcmp (name, "_z_create") == 0)
    return _z_create;
  else if (strcmp (name, "_z_clear") == 0)
    return _z_clear;
  else if (strcmp (name, "_z_get_si") == 0)
    return _z_get_si;
  else if (strcmp (name, "_z_add") == 0)
    return _z_add;
  else if (strcmp (name, "_z_mul") == 0)
    return _z_mul;
  else if (strcmp (name, "_z_tdiv_q") == 0)
    return _z_tdiv_q;
  else
    return NULL;
}
#endif
