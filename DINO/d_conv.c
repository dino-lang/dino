#include "d_conv.h"
#include "d_common.h"
#include "d_run.h"

/* The file contains functions for run-time conversion values. */

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
void
to_vect_string_conversion (ER_node_t var, const char *format)
{
  ER_node_mode_t mode;
  const char *representation;
  char str[1000];
  ER_node_t vect;
  int saved_no_gc_flag;
      
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
      saved_no_gc_flag = no_gc_flag;
      no_gc_flag = TRUE;
      vect = create_string (representation);
      no_gc_flag = saved_no_gc_flag;
      ER_SET_MODE (var, ER_NM_vect);
      ER_set_vect (var, vect);
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
	  assert (ER_NODE_MODE (ER_vect (var)) == ER_NM_heap_vect
		  && ER_pack_vect_el_type (ER_vect (var)) == ER_NM_char);
	  sprintf (str, format, ER_pack_els (ER_vect (var)));
	  /* Remeber `var' may be changed in GC. */
	  saved_no_gc_flag = no_gc_flag;
	  no_gc_flag = TRUE;
	  vect = create_string (str);
	  no_gc_flag = saved_no_gc_flag;
	  ER_SET_MODE (var, ER_NM_vect);
	  ER_set_vect (var, vect);
	}
    }
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
implicit_var_arithmetic_conversion (ER_node_t var)
{
  int_t i;
  floating_t f;

  if (ER_NODE_MODE (var) == ER_NM_char)
    {
      i = ER_ch (var);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, i);
    }
  else if (ER_NODE_MODE (var) == ER_NM_vect)
    {
      to_vect_string_conversion (var, NULL);
      if (ER_NODE_MODE (ER_vect (var)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (ER_vect (var)) == ER_NM_char)
	{
	  ER_node_t pack_vect = ER_vect (var);

	  if (it_is_int_string (ER_pack_els (pack_vect)))
	    {
	      i = a2i (ER_pack_els (pack_vect));
	      if (errno)
		process_system_errors ("string-to-int conversion");
	      ER_SET_MODE (var, ER_NM_int);
	      ER_set_i (var, i);
	    }
	  else
	    {
	      f = a2f (ER_pack_els (pack_vect));
	      if (errno)
		process_system_errors ("string-to-float conversion");
	      ER_SET_MODE (var, ER_NM_float);
	      ER_set_f (var, f);
	    }
	}
    }
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
void
implicit_arithmetic_conversion (int depth)
{
  implicit_var_arithmetic_conversion
    (INDEXED_VAL (ER_free (cstack), -depth));
}

void
implicit_conversion_for_binary_arithmetic_op (void)
{
  int float_flag;
  floating_t f;

  implicit_arithmetic_conversion (1);
  implicit_arithmetic_conversion (2);
  float_flag = (ER_NODE_MODE (ctop) == ER_NM_float
		|| ER_NODE_MODE (below_ctop) == ER_NM_float);
  if (float_flag && ER_NODE_MODE (ctop) == ER_NM_int)
    {
      f = ER_i (ctop);
      ER_SET_MODE (ctop, ER_NM_float);
      ER_set_f (ctop, f);
    }
  else if (float_flag && ER_NODE_MODE (below_ctop) == ER_NM_int)
    {
      f = ER_i (below_ctop);
      ER_SET_MODE (below_ctop, ER_NM_float);
      ER_set_f (below_ctop, f);
    }
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
void
implicit_int_conversion (int depth)
{
  ER_node_t var;
  int_t i;

  implicit_arithmetic_conversion (depth);
  var = INDEXED_VAL (ER_free (cstack), -depth);
  if (ER_NODE_MODE (var) == ER_NM_float)
    {
      i = (int_t) ER_f (var);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, i);
    }
}

void
implicit_conversion_for_binary_int_op (void)
{
  implicit_int_conversion (1);
  implicit_int_conversion (2);
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
implicit_eq_conversion (int depth)
{
  int_t i;
  ER_node_t var;

  var = INDEXED_VAL (ER_free (cstack), -depth);
  if (ER_NODE_MODE (var) == ER_NM_char)
    {
      i = ER_ch (var);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, i);
    }
}

void
implicit_conversion_for_eq_op (void)
{
  int float_flag;
  int string_flag;
  ER_node_t vect;
  floating_t f;

  if (ER_NODE_MODE (ctop) == ER_NM_vect)
    {
      vect = ER_vect (ctop);
      GO_THROUGH_REDIR (vect);
      ER_set_vect (ctop, vect);
    }
  if (ER_NODE_MODE (below_ctop) == ER_NM_vect)
    {
      vect = ER_vect (below_ctop);
      GO_THROUGH_REDIR (vect);
      ER_set_vect (below_ctop, vect);
    }
  string_flag = (ER_NODE_MODE (ctop) == ER_NM_vect
		 && ER_NODE_MODE (ER_vect (ctop)) == ER_NM_heap_pack_vect
		 && ER_pack_vect_el_type (ER_vect (ctop)) == ER_NM_char
		 || ER_NODE_MODE (below_ctop) == ER_NM_vect
		 && ER_NODE_MODE (ER_vect (below_ctop)) == ER_NM_heap_pack_vect
		 && ER_pack_vect_el_type (ER_vect (below_ctop)) == ER_NM_char);
  if (string_flag)
    {
      to_vect_string_conversion (ctop, NULL);
      to_vect_string_conversion (below_ctop, NULL);
    }
  else if (ER_NODE_MODE (ctop) == ER_NM_vect
	   && ER_NODE_MODE (below_ctop) == ER_NM_vect
	   && (ER_NODE_MODE (ER_vect (ctop))
	       != ER_NODE_MODE (ER_vect (below_ctop))))
    {
      if (ER_NODE_MODE (ER_vect (ctop)) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (ER_vect (ctop));
      else
	pack_vector_if_possible (ER_vect (below_ctop));
    }
  else
    {
      implicit_eq_conversion (1);
      implicit_eq_conversion (2);
      float_flag = (ER_NODE_MODE (ctop) == ER_NM_float
		    || ER_NODE_MODE (below_ctop) == ER_NM_float);
      if (float_flag && ER_NODE_MODE (ctop) == ER_NM_int)
	{
	  f = ER_i (ctop);
	  ER_SET_MODE (ctop, ER_NM_float);
	  ER_set_f (ctop, f);
	}
      else if (float_flag && ER_NODE_MODE (below_ctop) == ER_NM_int)
	{
	  f = ER_i (below_ctop);
	  ER_SET_MODE (below_ctop, ER_NM_float);
	  ER_set_f (below_ctop, f);
	}
    }
}
