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

WIN_EXPORT val_t
mpi_add (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  add_integer (size, hidevalue1, hidevalue2, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_subtract (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  subtract_integer (size, hidevalue1, hidevalue2, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_multiply (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  multiply_integer (size, hidevalue1, hidevalue2, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_divide (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  divide_integer (size, hidevalue1, hidevalue2, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_remainder (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  integer_remainder (size, hidevalue1, hidevalue2, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
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
  *hidevalue = ER_hideblock_start (ER_hideblock (var));
}

WIN_EXPORT val_t
mpi_shift_right (int npars, val_t *vals)
{
  int_t size, par;
  void *hidevalue, *res_hidevalue;
  ER_node_t mpi;

  mpi_par_op (npars, vals, &size, &par, 0 /*FALSE*/,
	      &hidevalue, &mpi, &res_hidevalue);
  integer_shift_right (size, hidevalue, par, res_hidevalue);
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_shift_left (int npars, val_t *vals)
{
  int_t size, par;
  void *hidevalue, *res_hidevalue;
  ER_node_t mpi;

  mpi_par_op (npars, vals, &size, &par, 0 /*FALSE*/,
	      &hidevalue, &mpi, &res_hidevalue);
  integer_shift_left (size, hidevalue, par, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_or (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  integer_or (size, hidevalue1, hidevalue2, res_hidevalue);
  return return_mpi (mpi);
}

WIN_EXPORT val_t
mpi_and (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2, *res_hidevalue;
  ER_node_t mpi;

  mpi_binary_op (npars, vals, &size, &hidevalue1, &hidevalue2,
		 &mpi, &res_hidevalue);
  integer_and (size, hidevalue1, hidevalue2, res_hidevalue);
  return return_mpi (mpi);
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
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((IR_hidden_node_t) vals)),
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

WIN_EXPORT val_t
mpi_not (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue, *res_hidevalue;
  ER_node_t mpi;

  mpi_unary_op (npars, vals, &size, &hidevalue, &mpi, &res_hidevalue);
  integer_not (size, hidevalue, res_hidevalue);
  return return_mpi (mpi);
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

WIN_EXPORT val_t
mpi_eq (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2;

  mpi_cmp_op (npars, vals, &size, &hidevalue1, &hidevalue2);
  return return_int (eq_integer (size, hidevalue1, hidevalue2));
}

WIN_EXPORT val_t
mpi_ne (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2;

  mpi_cmp_op (npars, vals, &size, &hidevalue1, &hidevalue2);
  return return_int (ne_integer (size, hidevalue1, hidevalue2));
}

WIN_EXPORT val_t
mpi_gt (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2;

  mpi_cmp_op (npars, vals, &size, &hidevalue1, &hidevalue2);
  return return_int (gt_integer (size, hidevalue1, hidevalue2));
}

WIN_EXPORT val_t
mpi_lt (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2;

  mpi_cmp_op (npars, vals, &size, &hidevalue1, &hidevalue2);
  return return_int (lt_integer (size, hidevalue1, hidevalue2));
}

WIN_EXPORT val_t
mpi_ge (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2;

  mpi_cmp_op (npars, vals, &size, &hidevalue1, &hidevalue2);
  return return_int (ge_integer (size, hidevalue1, hidevalue2));
}

WIN_EXPORT val_t
mpi_le (int npars, val_t *vals)
{
  int_t size;
  void *hidevalue1, *hidevalue2;

  mpi_cmp_op (npars, vals, &size, &hidevalue1, &hidevalue2);
  return return_int (le_integer (size, hidevalue1, hidevalue2));
}

WIN_EXPORT val_t
mpi_change_size (int npars, val_t *vals)
{
  int_t size, par;
  void *hidevalue, *res_hidevalue;
  ER_node_t mpi;

  mpi_par_op (npars, vals, &size, &par, 1 /*TRUE*/,
	      &hidevalue, &mpi, &res_hidevalue);
  change_integer_size (size, hidevalue, par, res_hidevalue);
  check_overflow ();
  return return_mpi (mpi);
}

#define MAX_INTEGER_SIZE 128

WIN_EXPORT val_t
mpi_to_string (int npars, val_t *vals)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  ER_node_t var;
  ER_node_t size_var;
  int_t size;
  void *hidevalue;
  ER_node_t vect;
  char str [3 * MAX_INTEGER_SIZE];

  assert (npars == 1
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)), 1);
  size_var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) vals)),
			  0);
  size = ER_i (size_var);
  hidevalue = ER_hideblock_start (ER_hideblock (var));
  integer_to_string (size, hidevalue, str);
  vect = create_string (str);
  ER_SET_MODE (res, ER_NM_vect);
  ER_set_vect (res, vect);
  return val;
}

WIN_EXPORT val_t
mpi_from_string (int npars, val_t *vals)
{
  int_t size;
  void *hideblock;
  ER_node_t var, size_var;
  ER_node_t mpi;

  assert (npars == 2
	  && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance ((ER_node_t) (vals + 1))),
		     1);
  size_var = INDEXED_VAL (ER_instance_vars
			  (ER_instance ((ER_node_t) (vals + 1))), 0);
  size = ER_i (size_var);
  hideblock = create_hideblock (size);
  integer_from_string (size, ER_pack_els (ER_vect ((ER_node_t) vals)),
		       ER_hideblock_start ((ER_node_t) hideblock));
  check_overflow ();
  mpi = ER_instance ((ER_node_t) (vals + 1));
  /* Size is already set up. */
  ER_SET_MODE (INDEXED_VAL (ER_instance_vars (mpi), 1), ER_NM_hideblock);
  ER_set_hideblock (INDEXED_VAL (ER_instance_vars (mpi), 1), hideblock);
  return return_mpi (mpi);
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
  else if (strcmp (name, "mpi_subtract") == 0)
    return mpi_subtract;
  else if (strcmp (name, "mpi_multiply") == 0)
    return mpi_multiply;
  else if (strcmp (name, "mpi_divide") == 0)
    return mpi_divide;
  else if (strcmp (name, "mpi_remainder") == 0)
    return mpi_remainder;
  else if (strcmp (name, "mpi_shift_right") == 0)
    return mpi_shift_right;
  else if (strcmp (name, "mpi_shift_left") == 0)
    return mpi_shift_left;
  else if (strcmp (name, "mpi_or") == 0)
    return mpi_or;
  else if (strcmp (name, "mpi_and") == 0)
    return mpi_and;
  else if (strcmp (name, "mpi_not") == 0)
    return mpi_not;
  else if (strcmp (name, "mpi_eq") == 0)
    return mpi_eq;
  else if (strcmp (name, "mpi_ne") == 0)
    return mpi_ne;
  else if (strcmp (name, "mpi_gt") == 0)
    return mpi_gt;
  else if (strcmp (name, "mpi_lt") == 0)
    return mpi_lt;
  else if (strcmp (name, "mpi_ge") == 0)
    return mpi_ge;
  else if (strcmp (name, "mpi_le") == 0)
    return mpi_le;
  else if (strcmp (name, "mpi_change_size") == 0)
    return mpi_change_size;
  else if (strcmp (name, "mpi_to_string") == 0)
    return mpi_to_string;
  else if (strcmp (name, "mpi_from_string") == 0)
    return mpi_from_string;
  else
    return NULL;
}
#endif
#endif
