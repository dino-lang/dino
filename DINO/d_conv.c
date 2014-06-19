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

/* Form string in TEMP_VLOBJ from N_PARS PARS according to format FMT.
   Throw exception with NAME if something is wrong.  */
void
form_format_string (const char *fmt, ER_node_t pars, int n_pars,
		    const char *name)
{
  ER_node_t val;
  const char *ptr, *str;
  char *curr_fmt, res_fmt [100];
  int curr_par_num, width, precision, add, out;
  int alternate_flag, zero_flag, left_adjust_flag;
  int blank_flag, plus_flag, width_flag, precision_flag;
  char next;

  errno = 0;
  VLO_NULLIFY (temp_vlobj);
  curr_par_num = 0;
  for (ptr = fmt; *ptr != '\0'; ptr++)
    if (*ptr != '%')
      VLO_ADD_BYTE (temp_vlobj, *ptr);
    else
      {
	alternate_flag = zero_flag = left_adjust_flag = FALSE;
	blank_flag = plus_flag = FALSE;
	for (;;)
	  {
	    next = *++ptr;
	    if (next == '#')
	      alternate_flag = TRUE;
	    else if (next == '0')
	      zero_flag = TRUE;
	    else if (next == '-')
	      left_adjust_flag = TRUE;
	    else if (next == ' ')
	      blank_flag = TRUE;
	    else if (next == '+')
	      plus_flag = TRUE;
	    else
	      {
		ptr--;
		break;
	      }
	  }
	next = *++ptr;
	width = 0;
	width_flag = FALSE;
	if (next >= '1' && next <= '9')
	  {
	    width_flag = TRUE;
	    do
	      {
		if (width <= MAX_INT / 10)
		  {
		    width *= 10;
		    if (width <= MAX_INT - (next - '0'))
		      width += next - '0';
		    else
		      width = -1;
		  }
		else
		  width = -1;
		if (width < 0)
		  eval_error (invfmt_bc_decl, get_cpos (),
			      DERR_invalid_format, name);
		next = *++ptr;
	      }
	    while (next >= '0' && next <= '9');
	    ptr--;
	  }
	else if (next == '*')
	  {
	    width_flag = TRUE;
	    if (curr_par_num >= n_pars)
	      eval_error (parnumber_bc_decl, get_cpos (),
			  DERR_parameters_number, name);
	    val = IVAL (pars, curr_par_num);
	    if (ER_NODE_MODE (val) != ER_NM_int)
	      eval_error (partype_bc_decl, get_cpos (),
			  DERR_parameter_type, name);
	    width = ER_i (val);
	    if (width < 0)
	      {
		left_adjust_flag = TRUE;
		width = -width;
	      }
	    curr_par_num++;
	  }
	else
	  ptr--;
	next = *++ptr;
	precision = 0;
	precision_flag = FALSE;
	if (next == '.')
	  {
	    precision_flag = TRUE;
	    next = *++ptr;
	    if (next == '*')
	      {
		if (curr_par_num >= n_pars)
		  eval_error (parnumber_bc_decl, get_cpos (),
			      DERR_parameters_number, name);
		val = IVAL (pars, curr_par_num);
		if (ER_NODE_MODE (val) != ER_NM_int)
		  eval_error (partype_bc_decl, get_cpos (),
			      DERR_parameter_type, name);
		precision = ER_i (val);
		if (precision < 0)
		  precision = 0;
		curr_par_num++;
	      }
	    else if (next >= '0' && next <= '9')
	      {
		do
		  {
		    if (precision <= MAX_INT / 10)
		      {
			precision *= 10;
			if (precision <= MAX_INT - (next - '0'))
			  precision += next - '0';
			else
			  precision = -1;
		      }
		    else
		      precision = -1;
		    if (precision < 0)
		      eval_error (invfmt_bc_decl, get_cpos (),
				  DERR_invalid_format, name);
		    next = *++ptr;
		  }
		while (next >= '0' && next <= '9');
		ptr--;
	      }
	    else
	      ptr--;
	  }
	else
	  ptr--;
	next = *++ptr;
	/* '-' switches off '0', '+' switches off ' '.  */
	if (left_adjust_flag && zero_flag)
	  zero_flag = FALSE;
	if (plus_flag && blank_flag)
	  blank_flag = FALSE;
	/* Zero flag is ignored when precision is given for an integer
	   format.  */
	if (zero_flag && precision_flag
	    && (next == 'd' || next == 'o' || next == 'x' || next == 'X'))
	  zero_flag = FALSE;
	curr_fmt = res_fmt;
	*curr_fmt++ = '%';
	if (alternate_flag)
	  *curr_fmt++ = '#';
	if (zero_flag)
	  *curr_fmt++ = '0';
	if (left_adjust_flag)
	  *curr_fmt++ = '-';
	if (blank_flag)
	  *curr_fmt++ = ' ';
	if (plus_flag)
	  *curr_fmt++ = '+';
	if (width_flag && width != 0)
	  curr_fmt += sprintf (curr_fmt, "%d", width);
	if (precision_flag)
	  curr_fmt += sprintf (curr_fmt, ".%d", precision);
	if (next == '%')
	  {
	    if (alternate_flag || zero_flag || left_adjust_flag
		|| blank_flag || plus_flag || width_flag || precision_flag)
	      eval_error (invfmt_bc_decl, get_cpos (),
			  DERR_invalid_format, name);
	    VLO_ADD_BYTE (temp_vlobj, '%');
	    continue;
	  }
	if (curr_par_num >= n_pars)
	  eval_error (parnumber_bc_decl, get_cpos (),
		      DERR_parameters_number, name);
	val = IVAL (pars, curr_par_num);
	add = width + 5;
	if (next == 'd' || next == 'o' || next == 'x' || next == 'X'
	    || next == 'e' || next == 'E' || next == 'f' || next == 'g'
	    || next == 'G')
	  {
	    if ((next == 'd' && alternate_flag)
		|| ((next == 'o' || next == 'x' || next == 'X')
		    && (blank_flag || plus_flag)))
	      eval_error (invfmt_bc_decl, get_cpos (),
			  DERR_invalid_format, name);
	    curr_fmt += sprintf (curr_fmt, "%c", next);
	    add += 100;
	    VLO_EXPAND (temp_vlobj, add);
	    if (next == 'd' || next == 'o' || next == 'x' || next == 'X')
	      {
		if (ER_NODE_MODE (val) == ER_NM_int)
		  out = sprintf ((char *) VLO_BOUND (temp_vlobj) - add, res_fmt,
				 ER_i (val));
		else if (ER_NODE_MODE (val) != ER_NM_long)
		  eval_error (partype_bc_decl, get_cpos (),
			      DERR_parameter_type, name);
		else
		  {
		    ER_node_t gmp = ER_l (val);
		    int base, sign = mpz_sgn (*ER_mpz_ptr (gmp));
		    mpz_t absv;
		    size_t len, len2, prefix_len;
		    const char *str;
		    char *ch_ptr;

		    mpz_init (absv);
		    mpz_abs (absv, *ER_mpz_ptr (gmp));
		    base = next == 'd' ? 10 : next == 'o' ? 8 : 16;
		    str = mpz2a (absv, base, next == 'X');
		    mpz_clear (absv);
		    if (precision_flag && precision == 0 && sign == 0)
		      str = "";
		    len = strlen (str);
		    VLO_NULLIFY (temp_vlobj2);
		    prefix_len = 1;
		    if (sign < 0)
		      VLO_ADD_STRING (temp_vlobj2, "-");
		    else if (plus_flag)
		      VLO_ADD_STRING (temp_vlobj2, "+");
		    else if (blank_flag)
		      VLO_ADD_STRING (temp_vlobj2, " ");
		    else
		      prefix_len = 0;
		    if (next == 'o')
		      {
			if (alternate_flag && sign != 0)
			  {
			    prefix_len++;
			    VLO_ADD_STRING (temp_vlobj2, "0");
			  }
		      }
		    else if (next != 'd')
		      {
			if (alternate_flag && sign != 0)
			  {
			    prefix_len += 2;
			    VLO_ADD_STRING (temp_vlobj2,
					    next == 'x' ? "0x" : "0X");
			  }
		      }
		    if (precision_flag)
		      for (; len < precision; len++)
			VLO_ADD_STRING (temp_vlobj2, "0");
		    else if (zero_flag)
		      for (len += prefix_len; len < width; len++)
			VLO_ADD_STRING (temp_vlobj2, "0");
		    VLO_ADD_STRING (temp_vlobj2, str);
		    len = len2 = strlen (VLO_BEGIN (temp_vlobj2));
		    add += len;
		    VLO_EXPAND (temp_vlobj, len);
		    ch_ptr = (char *) VLO_BOUND (temp_vlobj) - add;
		    out = 0;
		    if (! left_adjust_flag)
		      for (; len < width; len++, out++)
			ch_ptr [out] = ' ';
		    strcpy (ch_ptr + out, VLO_BEGIN (temp_vlobj2));
		    out += len2;
		    if (left_adjust_flag)
		      for (; len < width; len++, out++)
			ch_ptr [out] = ' ';
		  }
	      }
	    else
	      {
		if (ER_NODE_MODE (val) != ER_NM_float)
		  eval_error (partype_bc_decl, get_cpos (),
			      DERR_parameter_type, name);
		out = sprintf ((char *) VLO_BOUND (temp_vlobj) - add, res_fmt,
			       ER_f (val));
	      }
	  }
	else if (next == 'c')
	  {
	    if (alternate_flag || zero_flag || blank_flag || plus_flag
		|| precision_flag)
	      eval_error (invfmt_bc_decl, get_cpos (),
			  DERR_invalid_format, name);
	    *curr_fmt++ = 'c';
	    *curr_fmt++ = '\0';
	    add += 10;
	    VLO_EXPAND (temp_vlobj, add);
	    if (ER_NODE_MODE (val) != ER_NM_char)
	      eval_error (partype_bc_decl, get_cpos (),
			  DERR_parameter_type, name);
	    out = sprintf ((char *) VLO_BOUND (temp_vlobj) - add, res_fmt,
			   ER_ch (val));
	  }
	else if (next == 's')
	  {
	    if (alternate_flag || zero_flag || blank_flag || plus_flag)
	      eval_error (invfmt_bc_decl, get_cpos (),
			  DERR_invalid_format, name);
	    *curr_fmt++ = 's';
	    *curr_fmt++ = '\0';
	    to_vect_string_conversion (val, NULL, NULL);
	    if (ER_NODE_MODE (val) != ER_NM_vect
		|| ER_NODE_MODE (ER_vect (val)) != ER_NM_heap_pack_vect
		|| ER_pack_vect_el_mode (ER_vect (val)) != ER_NM_char)
	      eval_error (partype_bc_decl, get_cpos (),
			  DERR_parameter_type, name);
	    str = ER_pack_els (ER_vect (val));
	    add += strlen (str) + 10;
	    VLO_EXPAND (temp_vlobj, add);
	    out = sprintf ((char *) VLO_BOUND (temp_vlobj) - add, res_fmt,
			   str);
	  }
	else
	  eval_error (invfmt_bc_decl, get_cpos (), DERR_invalid_format, name);
	curr_par_num++;
	d_assert (out <= add);
	VLO_SHORTEN (temp_vlobj, add - out);
      }
  if (curr_par_num != n_pars)
    eval_error (parnumber_bc_decl, get_cpos (), DERR_parameters_number, name);
  VLO_ADD_BYTE (temp_vlobj, '\0');
  if (errno != 0)
    process_system_errors (name);
}

ER_node_t
to_vect_string_conversion (ER_node_t var, const char *format, ER_node_t tvar)
{
  ER_node_mode_t mode;
  const char *representation;
  char str[1000];
  ER_node_t vect;
      
  mode = ER_NODE_MODE (var);
  if (mode == ER_NM_float || mode == ER_NM_int || mode == ER_NM_long || mode == ER_NM_char)
    {
      if (format != NULL)
	{
	  form_format_string (format, var, 1, "vect (...)");
	  representation = VLO_BEGIN (temp_vlobj);
	}
      else if (mode == ER_NM_float)
	representation = f2a (ER_f (var));
      else if (mode == ER_NM_int)
	representation = i2a (ER_i (var));
      else if (mode == ER_NM_long)
	representation = mpz2a (*ER_mpz_ptr (ER_l (var)), 10, FALSE);
      else
	{
	  *str = ER_ch (var);
	  str [1] = '\0';
	  representation = str;
	}
      /* Remeber `var' may be changed in GC. */
      vect = create_string (representation);
      if (tvar == NULL)
	tvar = var;
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
		    && ER_pack_vect_el_mode (ER_vect (var)) == ER_NM_char);
	  form_format_string (format, var, 1, "vect (...)");
	  representation = VLO_BEGIN (temp_vlobj);
	  /* Remeber `var' may be changed in GC. */
	  vect = create_string (representation);
	  if (tvar == NULL)
	    tvar = var;
	  ER_SET_MODE (tvar, ER_NM_vect);
	  set_vect_dim (tvar, vect, 0);
	  return tvar;
	}
    }
  return var;
}

/* Used by read_number.  */
static size_t vindex;
static const char *vnumber;
static int vgetc (void) { return vnumber[vindex++]; }
static void vungetc (int c) { vindex--; d_assert (vnumber[vindex] == c); }

ER_node_t do_inline
implicit_arithmetic_conversion (ER_node_t var, ER_node_t tvar)
{
  int_t i;
  floating_t f;

  if (tvar == NULL)
    tvar = var;
  if (ER_NODE_MODE (var) == ER_NM_char)
    {
      i = ER_ch (var);
      ER_SET_MODE (tvar, ER_NM_int);
      ER_set_i (tvar, i);
      return tvar;
    }
  else if (ER_NODE_MODE (var) == ER_NM_vect)
    {
      tvar = to_vect_string_conversion (var, NULL, tvar);
      if (ER_NODE_MODE (ER_vect (tvar)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_mode (ER_vect (tvar)) == ER_NM_char)
	{
	  ER_node_t pack_vect = ER_vect (tvar);
	  enum read_number_code err_code;
	  int read_ch_num, float_p, long_p, base;
	  const char *repr;
	  
	  vindex = 0;
	  vnumber = ER_pack_els (pack_vect);
	  while (vnumber[vindex] == ' ' || vnumber[vindex] == '\t')
	    vindex++;
	  if (isdigit (vnumber[vindex])
	      || ((vnumber[vindex] == '-' || vnumber[vindex] == '+')
		  && isdigit (vnumber[vindex + 1])))
	    {
	      vindex++;
	      err_code = read_number (vnumber[vindex - 1], vgetc, vungetc,
				      &read_ch_num, &repr, &base,
				      &float_p, &long_p);
	      if (err_code == NUMBER_OK)
		{
		  vindex += read_ch_num;
		  while (vnumber[vindex] == ' ' || vnumber[vindex] == '\t')
		    vindex++;
		  if (vnumber[vindex] == 0)
		    {
		      if (long_p)
			{
			  ER_node_t gmp = create_gmp ();
			  
			  mpz_set_str (*ER_mpz_ptr (gmp), repr, base);
			  d_assert (errno == 0);
			  ER_SET_MODE (tvar, ER_NM_long);
			  ER_set_l (tvar, gmp);
			}
		      else if (float_p)
			{
			  f = a2f (ER_pack_els (pack_vect));
			  if (errno)
			    process_system_errors ("string-to-float conversion");
			  ER_SET_MODE (tvar, ER_NM_float);
			  ER_set_f (tvar, f);
			}
		      else
			{
			  i = a2i (repr, base);
			  if (errno)
			    process_system_errors ("string-to-int conversion");
			  ER_SET_MODE (tvar, ER_NM_int);
			  ER_set_i (tvar, i);
			}
		    }
		}
	    }
	}
    }
  return var;
}

static ER_node_t do_inline
implicit_only_int_conversion (ER_node_t op, ER_node_t tvar)
{
  int_t i;
  
  if (ER_NODE_MODE (op) == ER_NM_float)
    {
      i = (int_t) ER_f (op);
      ER_SET_MODE (tvar, ER_NM_int);
      ER_set_i (tvar, i);
      return tvar;
    }
  else if (ER_NODE_MODE (op) == ER_NM_long)
    {
      mpz_t *mpz_ptr = ER_mpz_ptr (ER_l (op));

      if (! mpz_ok_for_int_p (*mpz_ptr))
	eval_error (opvalue_bc_decl, get_cpos (),
		    DERR_long_is_too_big_for_conversion_to_int);
      ER_SET_MODE (tvar, ER_NM_int);
      ER_set_i (tvar, mpz2i (*mpz_ptr));
      return tvar;
    }
  return op;
}

static ER_node_t do_inline
implicit_only_long_conversion (ER_node_t op, ER_node_t tvar)
{
  ER_node_t gmp;

  if (ER_NODE_MODE (op) == ER_NM_int)
    {
      gmp = create_gmp ();
      i2mpz (*ER_mpz_ptr (gmp), ER_i (op));
      ER_SET_MODE (tvar, ER_NM_long);
      ER_set_l (tvar, gmp);
      return tvar;
    }
  else if (ER_NODE_MODE (op) == ER_NM_float)
    {
      gmp = create_gmp ();
      f2mpz (*ER_mpz_ptr (gmp), ER_f (op));
      ER_SET_MODE (tvar, ER_NM_long);
      ER_set_l (tvar, gmp);
      return tvar;
    }
  return op;
}

static ER_node_t do_inline
implicit_only_float_conversion (ER_node_t op, ER_node_t tvar)
{
  if (ER_NODE_MODE (op) == ER_NM_int)
    {
      floating_t f = ER_i (op);

      ER_SET_MODE (tvar, ER_NM_float);
      ER_set_f (tvar, f);
      return tvar;
    }
  else if (ER_NODE_MODE (op) == ER_NM_long)
    {
      ER_node_t gmp = ER_l (op);
      double d = mpz_get_d (*ER_mpz_ptr (gmp));
      
      ER_SET_MODE (tvar, ER_NM_float);
      ER_set_f (tvar, (floating_t) d);
      return tvar;
    }
  return op;
}

void
implicit_conversion_for_binary_arithmetic_op (ER_node_t op1, ER_node_t op2,
					      ER_node_t *l, ER_node_t *r)
{
  static val_t tvar1, tvar2;

  *l = op1 = implicit_arithmetic_conversion (op1, (ER_node_t) &tvar1);
  *r = op2 = implicit_arithmetic_conversion (op2, (ER_node_t) &tvar2);
  if (ER_NODE_MODE (op1) == ER_NM_float)
    *r = implicit_only_float_conversion (op2, (ER_node_t) &tvar2);
  else if (ER_NODE_MODE (op2) == ER_NM_float)
    *l = implicit_only_float_conversion (op1, (ER_node_t) &tvar2);
  else if (ER_NODE_MODE (op1) == ER_NM_long)
    *r = implicit_only_long_conversion (op2, (ER_node_t) &tvar2);
  else if (ER_NODE_MODE (op2) == ER_NM_long)
    *l = implicit_only_long_conversion (op1, (ER_node_t) &tvar1);
}

ER_node_t do_inline
implicit_int_conversion (ER_node_t op, ER_node_t tvar)
{
  if (tvar == NULL)
    tvar = op;
  op = implicit_arithmetic_conversion (op, tvar);
  return implicit_only_int_conversion (op, tvar);
}

ER_node_t do_inline
implicit_float_conversion (ER_node_t op, ER_node_t tvar)
{
  if (tvar == NULL)
    tvar = op;
  op = implicit_arithmetic_conversion (op, tvar);
  return implicit_only_float_conversion (op, tvar);
}

ER_node_t do_inline
implicit_long_conversion (ER_node_t op, ER_node_t tvar)
{
  if (tvar == NULL)
    tvar = op;
  op = implicit_arithmetic_conversion (op, tvar);
  return implicit_only_long_conversion (op, tvar);
}

void
implicit_conversion_for_binary_int_op (ER_node_t op1, ER_node_t op2,
				       ER_node_t *l, ER_node_t *r)
{
  static val_t tvar1, tvar2;

  op1 = implicit_arithmetic_conversion (op1, (ER_node_t) &tvar1);
  op2 = implicit_arithmetic_conversion (op2, (ER_node_t) &tvar2);
  if (ER_NODE_MODE (op1) == ER_NM_long)
    op2 = implicit_only_long_conversion (op2, (ER_node_t) &tvar2);
  else if (ER_NODE_MODE (op2) == ER_NM_long)
    op1 = implicit_only_long_conversion (op1, (ER_node_t) &tvar1);
  else
    {
      op1 = implicit_only_int_conversion (op1, (ER_node_t) &tvar1);
      op2 = implicit_only_int_conversion (op2, (ER_node_t) &tvar2);
    }
  *l = op1;
  *r = op2;
}

static ER_node_t do_inline
implicit_eq_conversion (ER_node_t op, ER_node_t tvar)
{
  int_t i;

  if (ER_NODE_MODE (op) == ER_NM_char)
    {
      i = ER_ch (op);
      op = tvar;
      ER_SET_MODE (op, ER_NM_int);
      ER_set_i (op, i);
    }
  return op;
}

void
implicit_conversion_for_eq_op (ER_node_t op1, ER_node_t op2,
			       ER_node_t *l, ER_node_t *r)
{
  int string_flag;
  ER_node_t vect;
  static val_t tvar1, tvar2;

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
		  && ER_pack_vect_el_mode (ER_vect (op2)) == ER_NM_char)
		  || (ER_NODE_MODE (op1) == ER_NM_vect
		      && ER_NODE_MODE (ER_vect (op1)) == ER_NM_heap_pack_vect
		      && ER_pack_vect_el_mode (ER_vect (op1)) == ER_NM_char));
  if (string_flag)
    {
      *r = op2 = to_vect_string_conversion (op2, NULL, (ER_node_t) &tvar2);
      *l = op1 = to_vect_string_conversion (op1, NULL, (ER_node_t) &tvar1);
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
      *r = op2 = implicit_eq_conversion (op2, (ER_node_t) &tvar2);
      *l = op1 = implicit_eq_conversion (op1, (ER_node_t) &tvar1);
      if (ER_NODE_MODE (op1) == ER_NM_float)
	*r = implicit_only_float_conversion (op2, (ER_node_t) &tvar2);
      else if (ER_NODE_MODE (op2) == ER_NM_float)
	*l = implicit_only_float_conversion (op1, (ER_node_t) &tvar1);
      else if (ER_NODE_MODE (op1) == ER_NM_long)
	*r = implicit_only_long_conversion (op2, (ER_node_t) &tvar2);
      else if (ER_NODE_MODE (op2) == ER_NM_long)
	*l = implicit_only_long_conversion (op1, (ER_node_t) &tvar1);
    }
}
