/*
   Copyright (C) 1997-2016 Vladimir Makarov.

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

#include "d_conv.h"
#include "d_common.h"
#include "d_run.h"

/* The file contains functions for run-time conversion values. */

static position_t get_fmt_op_pos (int vec_p) { return vec_p ? get_cpos () : call_pos (); }

/* Return (const char *) ptr + size.  */
static inline const void *cptr_add (const void *ptr, int size) {
  return (const void *) ((const char *) ptr + size);
}

/* Return (char *) ptr + size.  */
static inline void *ptr_add (void *ptr, int size) { return (void *) ((char *) ptr + size); }

static inline int ptr_ref (const void *ptr, int byte_p) {
  return byte_p ? *(const char *) ptr : *(const ucode_t *) ptr;
}

/* Put byte (if BYTE_P) or ucode C to string PTR as IND element.  */
static inline void char_assign (void *ptr, int ind, int c, int byte_p) {
  if (byte_p)
    ((byte_t *) ptr)[ind] = c;
  else
    ((ucode_t *) ptr)[ind] = c;
}

/* Add byte (if BYTE_P) or ucode WHAT to varr TO.  */
static inline void add_char (VARR (char) * to, int what, int byte_p) {
  if (byte_p)
    VARR_PUSH (char, to, what);
  else {
    ucode_t uc = what;
    VARR_PUSH_ARR (char, to, (char *) &uc, sizeof (ucode_t));
  }
}

/* Form string of bytes or unicodes in TEMP_VARR from N_PARS PARS
   according to format FMT.  Throw exception with NAME if something is
   wrong.  Return true if TEMP_VARR contains bytes, false if it
   contains unicodes.  */
int form_format_string (ER_node_t fmt, ER_node_t pars, int n_pars, const char *name, int vec_p) {
  ER_node_t val;
  const void *ptr;
  char *curr_fmt, res_fmt[100];
  int curr_par_num, width, precision, addition, out;
  int alternate_flag, zero_flag, left_adjust_flag;
  int blank_flag, plus_flag, width_flag, precision_flag;
  int byte_p, res_byte_p, char_size, res_char_size, next;
  size_t len, len2, bound;
  const char *str;
  char *ch_ptr;

  VARR_TRUNC (char, temp_varr, 0);
  curr_par_num = 0;
  d_assert (
    ER_NODE_MODE (fmt) == ER_NM_heap_pack_vect
    && (ER_pack_vect_el_mode (fmt) == ER_NM_char || ER_pack_vect_el_mode (fmt) == ER_NM_byte));
  res_byte_p = byte_p = ER_pack_vect_el_mode (fmt) == ER_NM_byte;
  res_char_size = char_size = (byte_p ? sizeof (char) : sizeof (ucode_t));
  for (ptr = ER_pack_els (fmt); ptr_ref (ptr, byte_p) != '\0'; ptr = cptr_add (ptr, char_size))
    if (ptr_ref (ptr, byte_p) != '%')
      add_char (temp_varr, ptr_ref (ptr, byte_p), res_byte_p);
    else {
      alternate_flag = zero_flag = left_adjust_flag = FALSE;
      blank_flag = plus_flag = FALSE;
      for (;;) {
        ptr = cptr_add (ptr, char_size);
        next = ptr_ref (ptr, byte_p);
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
        else {
          ptr = cptr_add (ptr, -char_size);
          break;
        }
      }
      ptr = cptr_add (ptr, char_size);
      next = ptr_ref (ptr, byte_p);
      width = 0;
      width_flag = FALSE;
      if (next >= '1' && next <= '9') {
        width_flag = TRUE;
        do {
          if (width <= INT_MAX / 10) {
            width *= 10;
            if (width <= INT_MAX - (next - '0'))
              width += next - '0';
            else
              width = -1;
          } else
            width = -1;
          if (width < 0)
            eval_error (invfmt_bc_decl, get_fmt_op_pos (vec_p), DERR_invalid_format, name);
          ptr = cptr_add (ptr, char_size);
          next = ptr_ref (ptr, byte_p);
        } while (next >= '0' && next <= '9');
        ptr = cptr_add (ptr, -char_size);
      } else if (next == '*') {
        width_flag = TRUE;
        if (curr_par_num >= n_pars)
          eval_error (parnumber_bc_decl, get_fmt_op_pos (vec_p), DERR_parameters_number, name);
        val = IVAL (pars, curr_par_num);
        if (ER_NODE_MODE (val) != ER_NM_int)
          eval_error (partype_bc_decl, get_fmt_op_pos (vec_p), DERR_parameter_type, name);
        width = ER_i (val);
        if (width < 0) {
          left_adjust_flag = TRUE;
          width = -width;
        }
        curr_par_num++;
      } else
        ptr = cptr_add (ptr, -char_size);
      ptr = cptr_add (ptr, char_size);
      next = ptr_ref (ptr, byte_p);
      precision = 0;
      precision_flag = FALSE;
      if (next == '.') {
        precision_flag = TRUE;
        ptr = cptr_add (ptr, char_size);
        next = ptr_ref (ptr, byte_p);
        if (next == '*') {
          if (curr_par_num >= n_pars)
            eval_error (parnumber_bc_decl, get_fmt_op_pos (vec_p), DERR_parameters_number, name);
          val = IVAL (pars, curr_par_num);
          if (ER_NODE_MODE (val) != ER_NM_int)
            eval_error (partype_bc_decl, get_fmt_op_pos (vec_p), DERR_parameter_type, name);
          precision = ER_i (val);
          if (precision < 0) precision = 0;
          curr_par_num++;
        } else if (next >= '0' && next <= '9') {
          do {
            if (precision <= INT_MAX / 10) {
              precision *= 10;
              if (precision <= INT_MAX - (next - '0'))
                precision += next - '0';
              else
                precision = -1;
            } else
              precision = -1;
            if (precision < 0)
              eval_error (invfmt_bc_decl, get_fmt_op_pos (vec_p), DERR_invalid_format, name);
            ptr = cptr_add (ptr, char_size);
            next = ptr_ref (ptr, byte_p);
          } while (next >= '0' && next <= '9');
          ptr = cptr_add (ptr, -char_size);
        } else
          ptr = cptr_add (ptr, -char_size);
      } else
        ptr = cptr_add (ptr, -char_size);
      ptr = cptr_add (ptr, char_size);
      next = ptr_ref (ptr, byte_p);
      /* '-' switches off '0', '+' switches off ' '.  */
      if (left_adjust_flag && zero_flag) zero_flag = FALSE;
      if (plus_flag && blank_flag) blank_flag = FALSE;
      /* Zero flag is ignored when precision is given for an integer
         format.  */
      if (zero_flag && precision_flag && (next == 'd' || next == 'o' || next == 'x' || next == 'X'))
        zero_flag = FALSE;
      curr_fmt = res_fmt;
      *curr_fmt++ = '%';
      if (alternate_flag) *curr_fmt++ = '#';
      if (zero_flag) *curr_fmt++ = '0';
      if (left_adjust_flag) *curr_fmt++ = '-';
      if (blank_flag) *curr_fmt++ = ' ';
      if (plus_flag) *curr_fmt++ = '+';
      if (width_flag && width != 0) curr_fmt += sprintf (curr_fmt, "%d", width);
      if (precision_flag) curr_fmt += sprintf (curr_fmt, ".%d", precision);
      if (next == '%') {
        if (alternate_flag || zero_flag || left_adjust_flag || blank_flag || plus_flag || width_flag
            || precision_flag)
          eval_error (invfmt_bc_decl, get_fmt_op_pos (vec_p), DERR_invalid_format, name);
        add_char (temp_varr, '%', res_byte_p);
        continue;
      }
      if (curr_par_num >= n_pars)
        eval_error (parnumber_bc_decl, get_fmt_op_pos (vec_p), DERR_parameters_number, name);
      val = IVAL (pars, curr_par_num);
      addition = width + 5;
      if (next == 'd' || next == 'o' || next == 'x' || next == 'X' || next == 'e' || next == 'E'
          || next == 'f' || next == 'g' || next == 'G') {
        if ((next == 'd' && alternate_flag)
            || ((next == 'o' || next == 'x' || next == 'X') && (blank_flag || plus_flag)))
          eval_error (invfmt_bc_decl, get_fmt_op_pos (vec_p), DERR_invalid_format, name);
        if (next == 'd' || next == 'o' || next == 'x' || next == 'X') {
          if (sizeof (rint_t) == sizeof (int))
            ;
          else if (sizeof (rint_t) == sizeof (long))
            curr_fmt += sprintf (curr_fmt, "l");
          else {
            assert (sizeof (rint_t) == sizeof (long long));
            curr_fmt += sprintf (curr_fmt, "ll");
          }
        }
        curr_fmt += sprintf (curr_fmt, "%c", next);
        addition += 100;
        VARR_GROW (char, temp_varr, addition);
        if (next == 'd' || next == 'o' || next == 'x' || next == 'X') {
          if (ER_NODE_MODE (val) == ER_NM_int) {
            bound = VARR_LENGTH (char, temp_varr) - addition;
            out = sprintf (&VARR_ADDR (char, temp_varr)[bound], res_fmt, ER_i (val));
          } else if (ER_NODE_MODE (val) != ER_NM_long) {
            eval_error (partype_bc_decl, get_fmt_op_pos (vec_p), DERR_parameter_type, name);
          } else {
            ER_node_t gmp = ER_l (val);
            int base, sign = mpz_sgn (*ER_mpz_ptr (gmp));
            mpz_t absv;
            size_t prefix_len;

            mpz_init (absv);
            mpz_abs (absv, *ER_mpz_ptr (gmp));
            base = next == 'd' ? 10 : next == 'o' ? 8 : 16;
            str = mpz2a (absv, base, next == 'X');
            mpz_clear (absv);
            if (precision_flag && precision == 0 && sign == 0) str = "";
            len = strlen (str);
            VARR_TRUNC (char, temp_varr2, 0);
            prefix_len = 1;
            if (sign < 0)
              push_str (temp_varr2, "-");
            else if (plus_flag)
              push_str (temp_varr2, "+");
            else if (blank_flag)
              push_str (temp_varr2, " ");
            else
              prefix_len = 0;
            if (next == 'o') {
              if (alternate_flag && sign != 0) {
                prefix_len++;
                push_str (temp_varr2, "0");
              }
            } else if (next != 'd') {
              if (alternate_flag && sign != 0) {
                prefix_len += 2;
                push_str (temp_varr2, next == 'x' ? "0x" : "0X");
              }
            }
            if (precision_flag)
              for (; len < precision; len++) push_str (temp_varr2, "0");
            else if (zero_flag)
              for (len += prefix_len; len < width; len++) push_str (temp_varr2, "0");
            push_str (temp_varr2, str);
            len = len2 = strlen (VARR_ADDR (char, temp_varr2));
            addition += len;
            VARR_GROW (char, temp_varr, len);
            bound = VARR_LENGTH (char, temp_varr) - addition;
            ch_ptr = &VARR_ADDR (char, temp_varr)[bound];
            out = 0;
            if (!left_adjust_flag)
              for (; len < width; len++, out++) char_assign (ch_ptr, out, ' ', res_byte_p);
            strcpy (ch_ptr + out, VARR_ADDR (char, temp_varr2));
            out += len2;
            if (left_adjust_flag)
              for (; len < width; len++, out++) char_assign (ch_ptr, out, ' ', res_byte_p);
          }
        } else {
          if (ER_NODE_MODE (val) != ER_NM_float)
            eval_error (partype_bc_decl, get_fmt_op_pos (vec_p), DERR_parameter_type, name);
          bound = VARR_LENGTH (char, temp_varr) - addition;
          out = sprintf (&VARR_ADDR (char, temp_varr)[bound], res_fmt, ER_f (val));
        }
        if (!res_byte_p) {
          str_to_ucode_varr (temp_varr2, &VARR_ADDR (char, temp_varr)[bound], out);
          VARR_GROW (char, temp_varr, addition *(res_char_size - 1));
          bound = VARR_LENGTH (char, temp_varr) - addition * res_char_size;
          memcpy (&VARR_ADDR (char, temp_varr)[bound], VARR_ADDR (char, temp_varr2),
                  out * sizeof (ucode_t));
        }
      } else if (next == 'c') {
        int ch;

        if (alternate_flag || zero_flag || blank_flag || plus_flag || precision_flag)
          eval_error (invfmt_bc_decl, get_fmt_op_pos (vec_p), DERR_invalid_format, name);
        addition += 10;
        VARR_GROW (char, temp_varr, addition *res_char_size);
        if (ER_NODE_MODE (val) != ER_NM_char)
          eval_error (partype_bc_decl, get_fmt_op_pos (vec_p), DERR_parameter_type, name);
        ch = ER_ch (val);
        if (res_byte_p && !in_byte_range_p (ch)) {
          /* Transform the result into ucode.  */
          copy_varr (temp_varr2, temp_varr);
          str_to_ucode_varr (temp_varr, VARR_ADDR (char, temp_varr2),
                             VARR_LENGTH (char, temp_varr2));
          res_byte_p = FALSE;
          res_char_size = sizeof (ucode_t);
        }
        bound = VARR_LENGTH (char, temp_varr) - addition * res_char_size;
        ch_ptr = &VARR_ADDR (char, temp_varr)[bound];
        out = 0;
        len = 1;
        if (!left_adjust_flag)
          for (; len < width; len++, out++) char_assign (ch_ptr, out, ' ', res_byte_p);
        char_assign (ch_ptr, out, ch, res_byte_p);
        out++;
        if (left_adjust_flag)
          for (; len < width; len++, out++) char_assign (ch_ptr, out, ' ', res_byte_p);
      } else if (next == 's') {
        int str_byte_p;
        size_t i, str_char_size;

        if (alternate_flag || zero_flag || blank_flag || plus_flag)
          eval_error (invfmt_bc_decl, get_fmt_op_pos (vec_p), DERR_invalid_format, name);
        to_vect_string_conversion (val, NULL, NULL);
        if (ER_NODE_MODE (val) != ER_NM_vect || ER_NODE_MODE (ER_vect (val)) != ER_NM_heap_pack_vect
            || (ER_pack_vect_el_mode (ER_vect (val)) != ER_NM_char
                && ER_pack_vect_el_mode (ER_vect (val)) != ER_NM_byte))
          eval_error (partype_bc_decl, get_fmt_op_pos (vec_p), DERR_parameter_type, name);
        str_byte_p = ER_pack_vect_el_mode (ER_vect (val)) == ER_NM_byte;
        str_char_size = (str_byte_p ? sizeof (char) : sizeof (ucode_t));
        if (res_byte_p && !str_byte_p) {
          /* Transform the result into ucode.  */
          copy_varr (temp_varr2, temp_varr);
          str_to_ucode_varr (temp_varr, VARR_ADDR (char, temp_varr2),
                             VARR_LENGTH (char, temp_varr2));
          res_byte_p = FALSE;
          res_char_size = sizeof (ucode_t);
        }
        str = ER_pack_els (ER_vect (val));
        len2 = str_byte_p ? strlen (str) : ucodestrlen ((ucode_t *) str);
        addition += len2 + 10;
        if (precision_flag && len2 > precision) len2 = precision;
        VARR_GROW (char, temp_varr, addition *res_char_size);
        bound = VARR_LENGTH (char, temp_varr) - addition * res_char_size;
        ch_ptr = &VARR_ADDR (char, temp_varr)[bound];
        out = 0;
        len = len2;
        if (!left_adjust_flag)
          for (; len < width; len++, out++) char_assign (ch_ptr, out, ' ', res_byte_p);
        for (i = 0; i < len2; i++) {
          char_assign (ch_ptr, out, ptr_ref (str, str_byte_p), res_byte_p);
          str = cptr_add (str, str_char_size);
          out++;
        }
        if (left_adjust_flag)
          for (; len < width; len++, out++) char_assign (ch_ptr, out, ' ', res_byte_p);
      } else
        eval_error (invfmt_bc_decl, get_fmt_op_pos (vec_p), DERR_invalid_format, name);
      curr_par_num++;
      d_assert (out <= addition);
      VARR_TRUNC (char, temp_varr,
                  VARR_LENGTH (char, temp_varr) - (addition - out) * res_char_size);
    }
  if (curr_par_num != n_pars)
    eval_error (parnumber_bc_decl, get_fmt_op_pos (vec_p), DERR_parameters_number, name);
  add_char (temp_varr, '\0', res_byte_p);
  /* Different alloc function might change errno even in case of
     success.  */
  errno = 0;
  return res_byte_p;
}

ER_node_t to_vect_string_conversion (ER_node_t var, ER_node_t format, ER_node_t tvar) {
  ER_node_mode_t mode;
  const char *representation;
  char str[1000];
  ER_node_t vect;
  int byte_p = TRUE;

  mode = ER_NODE_MODE (var);
  if (mode == ER_NM_float || mode == ER_NM_int || mode == ER_NM_long || mode == ER_NM_char) {
    if (format != NULL) {
      byte_p = form_format_string (format, var, 1, "vect (...)", TRUE);
      representation = VARR_ADDR (char, temp_varr);
    } else if (mode == ER_NM_float)
      representation = f2a (ER_f (var));
    else if (mode == ER_NM_int)
      representation = i2a (ER_i (var));
    else if (mode == ER_NM_long)
      representation = mpz2a (*ER_mpz_ptr (ER_l (var)), 10, FALSE);
    else {
      *str = ER_ch (var);
      str[1] = '\0';
      representation = str;
    }
    if (byte_p)
      vect = create_string (representation);
    else
      vect = create_ucodestr ((const ucode_t *) representation);
    if (tvar == NULL) tvar = var;
    ER_SET_MODE (tvar, ER_NM_vect);
    set_vect_dim (tvar, vect, 0);
    return tvar;
  } else if (mode == ER_NM_vect) {
    vect = ER_vect (var);
    GO_THROUGH_REDIR (vect);
    ER_set_vect (var, vect);
    try_full_pack (ER_vect (var));
    if (format != NULL) {
      d_assert (ER_NODE_MODE (ER_vect (var)) == ER_NM_heap_vect
                && (ER_pack_vect_el_mode (ER_vect (var)) == ER_NM_char
                    || ER_pack_vect_el_mode (ER_vect (var)) == ER_NM_byte));
      byte_p = form_format_string (format, var, 1, "vec (...)", TRUE);
      representation = VARR_ADDR (char, temp_varr);
      if (byte_p)
        vect = create_string (representation);
      else
        vect = create_ucodestr ((const ucode_t *) representation);
      if (tvar == NULL) tvar = var;
      ER_SET_MODE (tvar, ER_NM_vect);
      set_vect_dim (tvar, vect, 0);
      return tvar;
    }
  }
  return var;
}

/* Used by read_dino_number.  */
static size_t vindex;
static const char *vnumber;
static int vgetc (void) { return vnumber[vindex++]; }
static void vungetc (int c) {
  vindex--;
  d_assert (vnumber[vindex] == c);
}

ER_node_t do_inline implicit_arithmetic_conversion (ER_node_t var, ER_node_t tvar) {
  rint_t i;
  rfloat_t f;

  if (tvar == NULL) tvar = var;
  if (ER_NODE_MODE (var) == ER_NM_char) {
    i = ER_ch (var);
    ER_SET_MODE (tvar, ER_NM_int);
    ER_set_i (tvar, i);
    return tvar;
  } else if (ER_NODE_MODE (var) == ER_NM_vect) {
    tvar = to_vect_string_conversion (var, NULL, tvar);
    if (ER_NODE_MODE (ER_vect (tvar)) == ER_NM_heap_pack_vect
        /* Unicode string after try_full_pack in
           to_vect_string_conversion can not contain number.  */
        && ER_pack_vect_el_mode (ER_vect (tvar)) == ER_NM_byte) {
      ER_node_t pack_vect = ER_vect (tvar);
      enum read_number_code err_code;
      int read_ch_num, float_p, long_p, base;
      const char *repr;

      vindex = 0;
      vnumber = ER_pack_els (pack_vect);
      while (vnumber[vindex] == ' ' || vnumber[vindex] == '\t') vindex++;
      if (isdigit_ascii (vnumber[vindex])
          || ((vnumber[vindex] == '-' || vnumber[vindex] == '+')
              && isdigit_ascii (vnumber[vindex + 1]))) {
        vindex++;
        err_code = read_dino_number (vnumber[vindex - 1], vgetc, vungetc, &read_ch_num, &repr,
                                     &base, &float_p, &long_p);
        if (err_code == NUMBER_OK) {
          while (vnumber[vindex] == ' ' || vnumber[vindex] == '\t') vindex++;
          if (vnumber[vindex] == 0) {
            if (long_p) {
              ER_node_t gmp = create_gmp ();

              mpz_set_str (*ER_mpz_ptr (gmp), repr, base);
              /* errno migh be changed even in success
                 case as the mpz function might use
                 malloc/realloc.  */
              errno = 0;
              ER_SET_MODE (tvar, ER_NM_long);
              ER_set_l (tvar, gmp);
            } else if (float_p) {
              f = a2f (ER_pack_els (pack_vect));
              if (errno) {
                ifun_call_pc = cpc;
                process_system_errors ("string-to-float conversion");
              }
              ER_SET_MODE (tvar, ER_NM_float);
              ER_set_f (tvar, f);
            } else {
              i = a2i (repr, base);
              if (errno) {
                ifun_call_pc = cpc;
                process_system_errors ("string-to-int conversion");
              }
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

static ER_node_t do_inline implicit_only_int_conversion (ER_node_t op, ER_node_t tvar) {
  rint_t i;

  if (ER_NODE_MODE (op) == ER_NM_float) {
    i = (rint_t) ER_f (op);
    ER_SET_MODE (tvar, ER_NM_int);
    ER_set_i (tvar, i);
    return tvar;
  } else if (ER_NODE_MODE (op) == ER_NM_long) {
    mpz_t *mpz_ptr = ER_mpz_ptr (ER_l (op));

    if (!mpz_ok_for_rint_p (*mpz_ptr))
      eval_error (opvalue_bc_decl, get_cpos (), DERR_long_is_too_big_for_conversion_to_int);
    ER_SET_MODE (tvar, ER_NM_int);
    ER_set_i (tvar, mpz2i (*mpz_ptr));
    return tvar;
  }
  return op;
}

static ER_node_t do_inline implicit_only_long_conversion (ER_node_t op, ER_node_t tvar) {
  ER_node_t gmp;

  if (ER_NODE_MODE (op) == ER_NM_int) {
    gmp = create_gmp ();
    i2mpz (*ER_mpz_ptr (gmp), ER_i (op));
    ER_SET_MODE (tvar, ER_NM_long);
    ER_set_l (tvar, gmp);
    return tvar;
  } else if (ER_NODE_MODE (op) == ER_NM_float) {
    gmp = create_gmp ();
    f2mpz (*ER_mpz_ptr (gmp), ER_f (op));
    ER_SET_MODE (tvar, ER_NM_long);
    ER_set_l (tvar, gmp);
    return tvar;
  }
  return op;
}

static ER_node_t do_inline implicit_only_float_conversion (ER_node_t op, ER_node_t tvar) {
  if (ER_NODE_MODE (op) == ER_NM_int) {
    rfloat_t f = ER_i (op);

    ER_SET_MODE (tvar, ER_NM_float);
    ER_set_f (tvar, f);
    return tvar;
  } else if (ER_NODE_MODE (op) == ER_NM_long) {
    ER_node_t gmp = ER_l (op);
    double d = mpz_get_d (*ER_mpz_ptr (gmp));

    ER_SET_MODE (tvar, ER_NM_float);
    ER_set_f (tvar, (rfloat_t) d);
    return tvar;
  }
  return op;
}

void implicit_conversion_for_binary_arithmetic_op (ER_node_t op1, ER_node_t op2, ER_node_t *l,
                                                   ER_node_t *r) {
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

ER_node_t do_inline implicit_int_conversion (ER_node_t op, ER_node_t tvar) {
  if (tvar == NULL) tvar = op;
  op = implicit_arithmetic_conversion (op, tvar);
  return implicit_only_int_conversion (op, tvar);
}

ER_node_t do_inline implicit_float_conversion (ER_node_t op, ER_node_t tvar) {
  if (tvar == NULL) tvar = op;
  op = implicit_arithmetic_conversion (op, tvar);
  return implicit_only_float_conversion (op, tvar);
}

ER_node_t do_inline implicit_long_conversion (ER_node_t op, ER_node_t tvar) {
  if (tvar == NULL) tvar = op;
  op = implicit_arithmetic_conversion (op, tvar);
  return implicit_only_long_conversion (op, tvar);
}

void implicit_conversion_for_binary_int_op (ER_node_t op1, ER_node_t op2, ER_node_t *l,
                                            ER_node_t *r) {
  static val_t tvar1, tvar2;

  op1 = implicit_arithmetic_conversion (op1, (ER_node_t) &tvar1);
  op2 = implicit_arithmetic_conversion (op2, (ER_node_t) &tvar2);
  if (ER_NODE_MODE (op1) == ER_NM_long)
    op2 = implicit_only_long_conversion (op2, (ER_node_t) &tvar2);
  else if (ER_NODE_MODE (op2) == ER_NM_long)
    op1 = implicit_only_long_conversion (op1, (ER_node_t) &tvar1);
  else {
    op1 = implicit_only_int_conversion (op1, (ER_node_t) &tvar1);
    op2 = implicit_only_int_conversion (op2, (ER_node_t) &tvar2);
  }
  *l = op1;
  *r = op2;
}

static ER_node_t do_inline implicit_eq_conversion (ER_node_t op, ER_node_t tvar) {
  rint_t i;

  if (ER_NODE_MODE (op) == ER_NM_char) {
    i = ER_ch (op);
    op = tvar;
    ER_SET_MODE (op, ER_NM_int);
    ER_set_i (op, i);
  }
  return op;
}

void implicit_conversion_for_eq_op (ER_node_t op1, ER_node_t op2, ER_node_t *l, ER_node_t *r) {
  int string_flag;
  ER_node_t vect;
  static val_t tvar1, tvar2;

  if (ER_NODE_MODE (op2) == ER_NM_vect) {
    vect = ER_vect (op2);
    GO_THROUGH_REDIR (vect);
    ER_set_vect (op2, vect);
  }
  if (ER_NODE_MODE (op1) == ER_NM_vect) {
    vect = ER_vect (op1);
    GO_THROUGH_REDIR (vect);
    ER_set_vect (op1, vect);
  }
  string_flag
    = ((ER_NODE_MODE (op2) == ER_NM_vect && ER_NODE_MODE (ER_vect (op2)) == ER_NM_heap_pack_vect
        && (ER_pack_vect_el_mode (ER_vect (op2)) == ER_NM_char
            || ER_pack_vect_el_mode (ER_vect (op2)) == ER_NM_byte))
       || (ER_NODE_MODE (op1) == ER_NM_vect && ER_NODE_MODE (ER_vect (op1)) == ER_NM_heap_pack_vect
           && (ER_pack_vect_el_mode (ER_vect (op1)) == ER_NM_char
               || ER_pack_vect_el_mode (ER_vect (op1)) == ER_NM_byte)));
  if (string_flag) {
    *r = op2 = to_vect_string_conversion (op2, NULL, (ER_node_t) &tvar2);
    *l = op1 = to_vect_string_conversion (op1, NULL, (ER_node_t) &tvar1);
  } else if (ER_NODE_MODE (op2) == ER_NM_vect && ER_NODE_MODE (op1) == ER_NM_vect
             && (ER_NODE_MODE (ER_vect (op2)) != ER_NODE_MODE (ER_vect (op1)))) {
    try_full_pack (ER_vect (op2));
    try_full_pack (ER_vect (op1));
    *l = op1;
    *r = op2;
  } else {
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
