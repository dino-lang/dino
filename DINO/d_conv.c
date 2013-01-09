/*
   Copyright (C) 1997-2013 Vladimir Makarov.

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

#include "d_conv.h"
#include "d_common.h"
#include "d_run.h"

/* The file contains functions for run-time conversion values. */

ER_node_t do_inline
to_vect_string_conversion (ER_node_t var, const char *format, int tvar_num)
{
  ER_node_mode_t mode;
  const char *representation;
  char str[1000];
  ER_node_t vect, tvar;
      
  mode = ER_NODE_MODE (var);
  if (mode == ER_NM_float || mode == ER_NM_int || mode == ER_NM_char)
    {
      if (format != NULL)
	{
	  if (mode == ER_NM_float)
	    sprintf (str, format, ER_f (var));
	  else if (mode == ER_NM_int)
	    sprintf (str, format, ER_i (var));
	  else
	    sprintf (str, format, ER_ch (var));
	  representation = str;
	}
      else if (mode == ER_NM_float)
	representation = f2a (ER_f (var));
      else if (mode == ER_NM_int)
	representation = i2a (ER_i (var));
      else
	{
	  *str = ER_ch (var);
	  str [1] = '\0';
	  representation = str;
	}
      /* Remeber `var' may be changed in GC. */
      vect = create_string (representation);
      tvar = tvar_num < 0 ? var : IVAL (cvars, tvar_num);
      ER_SET_MODE (tvar, ER_NM_vect);
      set_vect_dim (tvar, vect, 0);
      return tvar;
  }
  else if (mode == ER_NM_vect)
    {
      vect = ER_vect (var);
      GO_THROUGH_REDIR (vect);
      ER_set_vect (var, vect);
      if (ER_NODE_MODE (ER_vect (var)) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (ER_vect (var));
      if (format != NULL)
	{
	  d_assert (ER_NODE_MODE (ER_vect (var)) == ER_NM_heap_vect
		    && ER_pack_vect_el_type (ER_vect (var)) == ER_NM_char);
	  sprintf (str, format, ER_pack_els (ER_vect (var)));
	  /* Remeber `var' may be changed in GC. */
	  vect = create_string (str);
	  tvar = tvar_num < 0 ? var : IVAL (cvars, tvar_num);
	  ER_SET_MODE (tvar, ER_NM_vect);
	  set_vect_dim (tvar, vect, 0);
	  return tvar;
	}
    }
  return var;
}

ER_node_t do_inline
implicit_arithmetic_conversion (ER_node_t var, int tvar_num)
{
  int_t i;
  floating_t f;
  ER_node_t tvar = tvar_num < 0 ? var : IVAL (cvars, tvar_num);

  if (ER_NODE_MODE (var) == ER_NM_char)
    {
      i = ER_ch (var);
      ER_SET_MODE (tvar, ER_NM_int);
      ER_set_i (tvar, i);
      return tvar;
    }
  else if (ER_NODE_MODE (var) == ER_NM_vect)
    {
      tvar = to_vect_string_conversion (var, NULL, tvar_num);
      if (ER_NODE_MODE (ER_vect (tvar)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (ER_vect (tvar)) == ER_NM_char)
	{
	  ER_node_t pack_vect = ER_vect (tvar);

	  if (it_is_int_string (ER_pack_els (pack_vect)))
	    {
	      i = a2i (ER_pack_els (pack_vect));
	      if (errno)
		process_system_errors ("string-to-int conversion");
	      ER_SET_MODE (tvar, ER_NM_int);
	      ER_set_i (tvar, i);
	    }
	  else
	    {
	      f = a2f (ER_pack_els (pack_vect));
	      if (errno)
		process_system_errors ("string-to-float conversion");
	      ER_SET_MODE (tvar, ER_NM_float);
	      ER_set_f (tvar, f);
	    }
	  return tvar;
	}
    }
  return var;
}

void
implicit_conversion_for_binary_arithmetic_op (ER_node_t op1, ER_node_t op2,
					      ER_node_t *l, ER_node_t *r)
{
  int float1_p, float2_p;
  floating_t f;
  ER_node_t tvar1 = IVAL (cvars, tvar_num1);
  ER_node_t tvar2 = IVAL (cvars, tvar_num2);

  *l = op1;
  *r = op2;
  if (! (float2_p = ER_NODE_MODE (op2) == ER_NM_float)
      && ER_NODE_MODE (op2) != ER_NM_int)
    {
      *r = op2 = implicit_arithmetic_conversion (op2, tvar_num2);
      float2_p = ER_NODE_MODE (op2) == ER_NM_float;
    }
  if (! (float1_p = ER_NODE_MODE (op1) == ER_NM_float)
      && ER_NODE_MODE (op1) != ER_NM_int)
    {
      *l = op1 = implicit_arithmetic_conversion (op1, tvar_num1);
      float1_p = ER_NODE_MODE (op1) == ER_NM_float;
    }
  if (float1_p && ER_NODE_MODE (op2) == ER_NM_int)
    {
      f = ER_i (op2);
      ER_SET_MODE (tvar2, ER_NM_float);
      ER_set_f (tvar2, f);
      *r = tvar2;
    }
  else if (float2_p && ER_NODE_MODE (op1) == ER_NM_int)
    {
      f = ER_i (op1);
      ER_SET_MODE (tvar1, ER_NM_float);
      ER_set_f (tvar1, f);
      *l = tvar1;
    }
}

ER_node_t do_inline
implicit_int_conversion (ER_node_t op, int tvar_num)
{
  int_t i;

  op = implicit_arithmetic_conversion (op, tvar_num);
  if (ER_NODE_MODE (op) == ER_NM_float)
    {
      i = (int_t) ER_f (op);
      ER_SET_MODE (op, ER_NM_int);
      ER_set_i (op, i);
    }
  return op;
}

void
implicit_conversion_for_binary_int_op (ER_node_t op1, ER_node_t op2,
				       ER_node_t *l, ER_node_t *r)
{
  *l = implicit_int_conversion (op1, tvar_num1);
  *r = implicit_int_conversion (op2, tvar_num2);
}

static ER_node_t do_inline
implicit_eq_conversion (ER_node_t op, int tvar_num)
{
  int_t i;

  if (ER_NODE_MODE (op) == ER_NM_char)
    {
      i = ER_ch (op);
      op = IVAL (cvars, tvar_num);
      ER_SET_MODE (op, ER_NM_int);
      ER_set_i (op, i);
    }
  return op;
}

void
implicit_conversion_for_eq_op (ER_node_t op1, ER_node_t op2,
			       ER_node_t *l, ER_node_t *r)
{
  int float_flag;
  int string_flag;
  ER_node_t vect;
  floating_t f;

  if (ER_NODE_MODE (op2) == ER_NM_vect)
    {
      vect = ER_vect (op2);
      GO_THROUGH_REDIR (vect);
      ER_set_vect (op2, vect);
    }
  if (ER_NODE_MODE (op1) == ER_NM_vect)
    {
      vect = ER_vect (op1);
      GO_THROUGH_REDIR (vect);
      ER_set_vect (op1, vect);
    }
  string_flag = ((ER_NODE_MODE (op2) == ER_NM_vect
		  && ER_NODE_MODE (ER_vect (op2)) == ER_NM_heap_pack_vect
		  && ER_pack_vect_el_type (ER_vect (op2)) == ER_NM_char)
		  || (ER_NODE_MODE (op1) == ER_NM_vect
		      && ER_NODE_MODE (ER_vect (op1)) == ER_NM_heap_pack_vect
		      && ER_pack_vect_el_type (ER_vect (op1)) == ER_NM_char));
  if (string_flag)
    {
      *r = op2 = to_vect_string_conversion (op2, NULL, tvar_num2);
      *l = op1 = to_vect_string_conversion (op1, NULL, tvar_num1);
    }
  else if (ER_NODE_MODE (op2) == ER_NM_vect
	   && ER_NODE_MODE (op1) == ER_NM_vect
	   && (ER_NODE_MODE (ER_vect (op2))
	       != ER_NODE_MODE (ER_vect (op1))))
    {
      if (ER_NODE_MODE (ER_vect (op2)) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (ER_vect (op2));
      else
	pack_vector_if_possible (ER_vect (op1));
    }
  else
    {
      *r = op2 = implicit_eq_conversion (op2, tvar_num2);
      *l = op1 = implicit_eq_conversion (op1, tvar_num1);
      float_flag = (ER_NODE_MODE (op2) == ER_NM_float
		    || ER_NODE_MODE (op1) == ER_NM_float);
      if (float_flag && ER_NODE_MODE (op2) == ER_NM_int)
	{
	  f = ER_i (op2);
	  ER_SET_MODE (op2, ER_NM_float);
	  ER_set_f (op2, f);
	}
      else if (float_flag && ER_NODE_MODE (op1) == ER_NM_int)
	{
	  f = ER_i (op1);
	  ER_SET_MODE (op1, ER_NM_float);
	  ER_set_f (op1, f);
	}
    }
}
