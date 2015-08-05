#include "d_common.h"
#include "d_conv.h"
#include "d_func.h"
#include "d_run.h"
#include "d_eval.h"

/* !!! All above the function find_context_by_scope is minimized for
   building the header for the generated code (see script
   d_minimize.d).  So the place of this function is important. */
extern ER_node_t find_context_by_scope (BC_node_t scope);
extern void process_var_ref_and_val (ER_node_t res, BC_node_t vdecl,
				     int val_too_p);
extern void process_var_val (ER_node_t res, BC_node_t vdecl);
extern void process_external_var (ER_node_t res, BC_node_t evdecl,
				  int lvalue_p, int val_too_p);
extern position_t get_designator_pos (void);

extern BC_node_t find_field (int block_decl_ident_number, ER_node_t *container);
extern void execute_general_period_operation (int block_decl_ident_number,
					      ER_node_t res, ER_node_t op,
					      int lvalue_p, int lvalue_val_p);
static rint_t do_always_inline
check_vector_index_value (ER_node_t vect, rint_t index_value)
{
  /* Negative will be too big after the cast.  Therefore we don't need
     to check on less than zero.  */
  if ((unsigned_rint_t) index_value >= ER_els_number (vect))
    {
      if (index_value < 0)
	eval_error (indexvalue_bc_decl,
		    get_designator_pos (), DERR_index_is_negative_number);
      else
	eval_error (indexvalue_bc_decl, get_designator_pos (),
		    DERR_index_is_greater_than_array_bound);
    }
  return index_value;
}

static rint_t do_always_inline
check_vector_index (ER_node_t vect, ER_node_t index)
{
  rint_t index_value;
  val_t tvar;

  if (doubt (ER_NODE_MODE (index) != ER_NM_int))
    {
      index = implicit_int_conversion (index, (ER_node_t) &tvar);
      if (doubt (ER_NODE_MODE (index) != ER_NM_int))
	eval_error (indextype_bc_decl, get_cpos (), DERR_index_is_not_int);
    }
  return check_vector_index_value (vect, ER_i (index));
}

extern void load_vector_element_by_index (ER_node_t to, ER_node_t vect,
					  ER_node_t index);
extern void load_table_element_by_key (ER_node_t to, ER_node_t tab,
				       ER_node_t key);
extern void store_designator_value (ER_node_t container, ER_node_t index,
				    ER_node_t val);
extern void store_vect_tab_designator_value (ER_node_t vec_tab, ER_node_t index,
					     ER_node_t val);
extern void store_stack_designator_value (ER_node_t stack, ER_node_t index,
					  ER_node_t val);
extern void slice_extract (ER_node_t res, ER_node_t container, int dim);
extern void slice_assign (ER_node_t container, int dim, ER_node_t val);

/* The following three functions return TRUE if OP1 and OP2 are
   correspondingly int, float, or slice type.  */
static int do_always_inline
int_bin_op (ER_node_t op1, ER_node_t op2)
{
  return expect (ER_NODE_MODE (op1) == ER_NM_int
		 && ER_NODE_MODE (op2) == ER_NM_int);
}

static int do_always_inline
float_bin_op (ER_node_t op1, ER_node_t op2)
{
  return expect (ER_NODE_MODE (op1) == ER_NM_float
		 && ER_NODE_MODE (op2) == ER_NM_float);
}

static int do_always_inline
vect_bin_op (ER_node_t op1, ER_node_t op2)
{
  return ((ER_NODE_MODE (op1) == ER_NM_vect && ER_dim (op1) != 0)
	  || (ER_NODE_MODE (op2) == ER_NM_vect && ER_dim (op2) != 0));
}

extern void binary_vect_op (ER_node_t, ER_node_t, ER_node_t);

/* Define inline functions used to do actual different type
   binary operations.  */
#define ifunc(name) static do_always_inline rint_t name (rint_t a, rint_t b)
#define ffunc(name) static do_always_inline rfloat_t name (rfloat_t a, rfloat_t b)
#define icmpf(name) static do_always_inline int name (rint_t a, rint_t b)
#define fcmpf(name) static do_always_inline int name (rfloat_t a, rfloat_t b)
ifunc (i_plus) { return a + b;}
ifunc (i_minus) { return a - b;}
ifunc (i_mult) { return a * b;}
ifunc (i_div) { return a / b;}
ifunc (i_mod) { return a % b;}
ifunc (i_and) { return a & b;}
ifunc (i_xor) { return a ^ b;}
ifunc (i_or) { return a | b;}
ifunc (i_lshift) { return a << b;}
ifunc (i_rshift) { return (unsigned_rint_t) a >> b;}
ifunc (i_ashift) { return a >> b;}
icmpf (i_eq) { return a == b;}
icmpf (i_ne) { return a != b;}
icmpf (i_lt) { return a < b;}
icmpf (i_le) { return a <= b;}
icmpf (i_gt) { return a > b;}
icmpf (i_ge) { return a >= b;}
ffunc (f_plus) { return a + b;}
ffunc (f_minus) { return a - b;}
ffunc (f_mult) { return a * b;}
ffunc (f_div) { return a / b;}
ffunc (f_mod) { return fmod (a, b);}
fcmpf (f_eq) { return a == b;}
fcmpf (f_ne) { return a != b;}
fcmpf (f_lt) { return a < b;}
fcmpf (f_le) { return a <= b;}
fcmpf (f_gt) { return a > b;}
fcmpf (f_ge) { return a >= b;}

/* Return true if OP1 is a slice.  */
static int do_always_inline
vect_unary_op (ER_node_t op1)
{
  return ER_NODE_MODE (op1) == ER_NM_vect && ER_dim (op1) != 0;
}

extern void unary_vect_op (ER_node_t, ER_node_t);

/* Define inline functions used to do actual different type
   unary operations.  */
#define ifunc1(name) static do_always_inline rint_t name (rint_t a)
#define ffunc1(name) static do_always_inline rfloat_t name (rfloat_t a)
#define iffunc1(name) static do_always_inline rint_t name (rfloat_t a)
#define lfunc1(name) static do_always_inline ER_node_t name (ER_node_t a)
#define ilfunc1(name) static do_always_inline rint_t name (ER_node_t a)
ifunc1 (iunary_plus) { return a;}
ifunc1 (iunary_minus) { return -a;}
ffunc1 (funary_plus) { return a;}
ffunc1 (funary_minus) { return -a;}
ifunc1 (i_not) { return a == 0;}
iffunc1 (fnot) { return a == 0.0;}
ifunc1 (ibitwise_not) { return ~a;}

lfunc1 (lunary_minus)
{
  ER_node_t res = create_gmp ();

  mpz_neg (*ER_mpz_ptr (res), *ER_mpz_ptr (a));
  return res;
}

lfunc1 (lunary_plus) {return copy_gmp (a);}
ilfunc1 (lnot) { return mpz_sgn (*ER_mpz_ptr (a)) == 0; }

extern void execute_plus_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			     int vect_p);
extern void execute_minus_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			      int vect_p);
extern void execute_mult_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			     int vect_p);
extern void execute_div_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			    int vect_p);
extern void execute_mod_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			    int vect_p);

extern void execute_lshift_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			       int vect_p);
extern void execute_rshift_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			       int vect_p);
extern void execute_ashift_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			       int vect_p);
extern void execute_and_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			    int vect_p);
extern void execute_xor_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			    int vect_p);
extern void execute_or_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			   int vect_p);
extern void execute_concat_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			       int vect_p);
extern void execute_in_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			   int vect_p);
extern int common_eq_ne_op (BC_node_mode_t cmp_op, ER_node_t op1, ER_node_t op2);
extern void execute_common_eq_ne_op (BC_node_mode_t cmp_op, ER_node_t res,
				     ER_node_t op1, ER_node_t op2, int vect_p);

/* Implement comparison OP of OP1 and OP2 using icmp, fcmp, or gencmp.
   Put result into RES.  Check and do vector operations if VECT_P.  */
static void do_always_inline
comp_op (BC_node_mode_t op, ER_node_t res, ER_node_t op1, ER_node_t op2,
	 int vect_p, int icmp (rint_t, rint_t), int fcmp (rfloat_t, rfloat_t),
	 void gencmp (BC_node_mode_t, ER_node_t, ER_node_t, ER_node_t, int))
{
  int cmp;

  if (int_bin_op (op1, op2))
    {
      cmp = icmp (ER_i (op1), ER_i (op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, cmp);
      return;
    }
  if (float_bin_op (op1, op2))
    {
      cmp = fcmp (ER_f (op1), ER_f (op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, cmp);
      return;
    }
  gencmp (op, res, op1, op2, vect_p);
}

extern void execute_eq_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			   int vect_p);
extern void execute_ne_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			   int vect_p);
extern void execute_common_cmp_op (BC_node_mode_t oper, ER_node_t res,
				   ER_node_t op1, ER_node_t op2, int vect_p);
extern void execute_lt_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			   int vect_p);
extern void execute_ge_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			   int vect_p);
extern void execute_gt_op (ER_node_t res, ER_node_t op1, ER_node_t op2,
			   int vect_p);
extern void execute_le_op (ER_node_t res, ER_node_t op1, ER_node_t op2
			   , int vect_p);
extern void execute_identity_op (int identity_p, ER_node_t res,
				 ER_node_t op1, ER_node_t op2, int vect_p);
/* Do unary arithmetic operation using iop or fop on OP1 and OP2.  Put
   result into RES.  Use MSG in case of error.  Check and do vector
   operations if VECT_P.  */
static void do_always_inline
execute_unary_ar_op (ER_node_t res, ER_node_t op1, int vect_p,
		     const char *err_message, rint_t iop (rint_t),
		     rfloat_t fop (rfloat_t),
		     ER_node_t lop (ER_node_t))
{
  rint_t i;
  rfloat_t f;
  val_t tvar1;

  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    {
      i = iop (ER_i (op1));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
      return;
    }
  else if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_arithmetic_conversion (op1, (ER_node_t) &tvar1);
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    {
      i = iop (ER_i (op1));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
    }
  else if (expect (ER_NODE_MODE (op1) == ER_NM_float))
    {
      f = fop (ER_f (op1));
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f);
    }
  else if (ER_NODE_MODE (op1) == ER_NM_long)
    {
      ER_node_t l = lop (ER_l (op1));
      ER_SET_MODE (res, ER_NM_long);
      ER_set_l (res, l);
    }
  else
    eval_error (optype_bc_decl, get_cpos (), err_message);
}

/* The following different functions to implement unary
   operations:  */
static void do_always_inline
execute_not_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  rint_t i;
  val_t tvar1;

  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    {
      i = i_not (ER_i (op1));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
      return;
    }
  else if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_arithmetic_conversion (op1, (ER_node_t) &tvar1);
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    i = i_not (ER_i (op1));
  else if (expect (ER_NODE_MODE (op1) == ER_NM_float))
    i = fnot (ER_f (op1));
  else if (ER_NODE_MODE (op1) == ER_NM_long)
    i = lnot (ER_l (op1));
  else
    eval_error (optype_bc_decl, get_cpos (), DERR_not_operand_type);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i);
}

static void do_always_inline
execute_bitwise_not_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  rint_t i;
  val_t tvar1;

  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    {
      i = ibitwise_not (ER_i (op1));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
    }
  else
    {
      if (vect_p && vect_unary_op (op1))
	{
	  unary_vect_op (res, op1);
	  return;
	}
      op1 = implicit_int_conversion (op1, (ER_node_t) &tvar1);
      if (expect (ER_NODE_MODE (op1) == ER_NM_int))
	{
	  i = ibitwise_not (ER_i (op1));
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, i);
	}
      else
	eval_error (optype_bc_decl, get_cpos (), DERR_bitwise_not_operand_type);
    }
}

static void do_always_inline
execute_length_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  rint_t i;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = to_vect_string_conversion (op1, NULL, (ER_node_t) &tvar1);
  if (ER_NODE_MODE (op1) != ER_NM_vect && ER_NODE_MODE (op1) != ER_NM_tab)
    eval_error (optype_bc_decl,	get_cpos (), DERR_length_operand_type);
  if (ER_NODE_MODE (op1) == ER_NM_vect)
    {
      ER_node_t vect = ER_vect (op1);
      
      GO_THROUGH_REDIR (vect);
      i = ER_els_number (vect);
    }
  else
    {
      ER_node_t tab = ER_tab (op1);
      
      GO_THROUGH_REDIR (tab);
      i = ER_els_number (tab);
    }
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i);
}

static void do_always_inline
execute_new_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  if (ER_NODE_MODE (op1) == ER_NM_vect)
    {
      ER_node_t vect = ER_vect (op1);
      
      GO_THROUGH_REDIR (vect);
      vect = copy_vector (vect);
      ER_SET_MODE (res, ER_NM_vect);
      set_vect_dim (res, vect, ER_dim (op1));
    }
  else if (ER_NODE_MODE (op1) == ER_NM_tab)
    {
      ER_node_t tab = ER_tab (op1);
      
      GO_THROUGH_REDIR (tab);
      tab = copy_tab (tab);
      ER_SET_MODE (res, ER_NM_tab);
      ER_set_tab (res, tab);
    }
  else if (ER_NODE_MODE (op1) == ER_NM_stack) /* !!! */
    {
      size_t size, un;
      ER_node_t stack;
      
      size = stack_size (ER_stack (op1));
      stack = heap_allocate (size, FALSE);
      ER_SET_MODE (stack, ER_NM_heap_stack);
      un = ER_unique_number (stack);
      memcpy (stack, ER_stack (op1), size);
      ER_set_immutable (stack, FALSE);
      ER_set_unique_number (stack, un);
      ER_SET_MODE (res, ER_NM_stack);
      ER_set_stack (res, stack);
    }
  else if (res != op1)
    *(val_t *) res = *(val_t *) op1;
}

static void do_always_inline
execute_const_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  make_immutable (op1);
  *(val_t *) res = *(val_t *) op1;
}

static void do_always_inline
execute_typeof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  type_val_t type;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  type = mode_to_type (ER_NODE_MODE (op1));
  if (type == type_fun)
    type = code_type (ID_TO_CODE (ER_code_id (op1)));
  ER_SET_MODE (res, ER_NM_type);
  ER_set_type (res, type);
}

static void do_always_inline
execute_charof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  rint_t i;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_int_conversion (op1, (ER_node_t) &tvar1);
  if (doubt (ER_NODE_MODE (op1) != ER_NM_int))
    eval_error (optype_bc_decl, get_cpos (),
		DERR_conversion_to_char_operand_type);
  if (ER_i (op1) > MAX_CHAR || ER_i (op1) < 0)
    {
#ifdef ERANGE
      errno = ERANGE;
      ifun_call_pc = cpc;
      process_system_errors ("int-to-char conversion");
#endif
    }
  i = ER_i (op1);
  ER_SET_MODE (res, ER_NM_char);
  ER_set_ch (res, i);
}

static void do_always_inline
execute_intof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  rint_t i;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_int_conversion (op1, (ER_node_t) &tvar1);
  if (doubt (ER_NODE_MODE (op1) != ER_NM_int))
    eval_error (optype_bc_decl, get_cpos (), DERR_conversion_to_int_operand_type);
  i = ER_i (op1);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i);
}

static void do_always_inline
execute_longof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  ER_node_t l;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_long_conversion (op1, (ER_node_t) &tvar1);
  if (ER_NODE_MODE (op1) != ER_NM_long)
    eval_error (optype_bc_decl, get_cpos (),
		DERR_conversion_to_long_operand_type);
  l = ER_l (op1);
  ER_SET_MODE (res, ER_NM_long);
  ER_set_l (res, copy_gmp (l));
}

static void do_always_inline
execute_floatof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  rfloat_t f;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_float_conversion (op1, (ER_node_t) &tvar1);
  if (doubt (ER_NODE_MODE (op1) != ER_NM_float))
    eval_error (optype_bc_decl, get_cpos (),
		DERR_conversion_to_float_operand_type);
  f = ER_f (op1);
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f);
}

/* Implement vectorof (if OP2==NULL) or format_vectorof.  */
static void do_always_inline
execute_vectorof_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  ER_node_t vect;
  val_t tvar1, tvar2;

  if (vect_p)
    {
      if (op2 == NULL && vect_unary_op (op1))
	{
	  unary_vect_op (res, op1);
	  return;
	}
      else if (op2 != NULL && vect_bin_op (op1, op2))
	{
	  binary_vect_op (res, op1, op2);
	  return;
	}
    }
  if (op2 != NULL && ER_NODE_MODE (op2) != ER_NM_nil) // ???
    {
      if (ER_NODE_MODE (op1) != ER_NM_char
	  && ER_NODE_MODE (op1) != ER_NM_int
	  && ER_NODE_MODE (op1) != ER_NM_long
	  && ER_NODE_MODE (op1) != ER_NM_float
	  && (ER_NODE_MODE (ER_vect (op1)) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_mode (ER_vect (op1)) != ER_NM_char))
	eval_error (optype_bc_decl, get_cpos (),
		    DERR_format_conversion_to_vector_operand_type);
      op2 = to_vect_string_conversion (op2, NULL, (ER_node_t) &tvar2);
      if (ER_NODE_MODE (op2) != ER_NM_vect
	  || ER_NODE_MODE (ER_vect (op2)) != ER_NM_heap_pack_vect
	  || ER_pack_vect_el_mode (ER_vect (op2)) != ER_NM_char)
	eval_error (optype_bc_decl, get_cpos (),
		    DERR_vector_conversion_format_type);
      op1 = to_vect_string_conversion (op1, ER_pack_els (ER_vect (op2)),
				       (ER_node_t) &tvar1);
      vect = ER_vect (op1);
      ER_SET_MODE (res, ER_NM_vect);
      set_vect_dim (res, vect, 0);
      return;
    }
  op1 = to_vect_string_conversion (op1, NULL, (ER_node_t) &tvar1);
  if (ER_NODE_MODE (op1) == ER_NM_vect)
    vect = ER_vect (op1);
  else if (ER_NODE_MODE (op1) == ER_NM_tab) 
    vect = table_to_vector_conversion (ER_tab (op1));
  else
    eval_error (optype_bc_decl, get_cpos (),
		DERR_conversion_to_vector_operand_type);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
}

static void do_always_inline
execute_tableof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  ER_node_t tab;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  if (ER_NODE_MODE (op1) == ER_NM_tab)
    tab = ER_tab (op1);
  else
    {
      op1 = to_vect_string_conversion (op1, NULL, (ER_node_t) &tvar1);
      if (ER_NODE_MODE (op1) == ER_NM_vect)
	tab = vector_to_table_conversion (ER_vect (op1));
      else
	eval_error (optype_bc_decl, get_cpos (),
		    DERR_conversion_to_table_operand_type);
    }
  ER_SET_MODE (res, ER_NM_tab);
  ER_set_tab (res, tab);
}

extern void fold_vect_op (ER_node_t res, ER_node_t op);

/* Return I-th var of VARS.  */
static ER_node_t do_always_inline
get_var (ER_node_t vars, int i)
{
  return IVAL (vars, i);
}

/* Return I-th var.  If i is non-negative, it is local variable.
   Negative numbers encode global vars.  */
static ER_node_t do_always_inline
get_op (int i)
{
  return i >= 0 ? get_var (cvars, i) : get_var (tvars, -i - 1);
}

static int do_always_inline
non_zero_p (ER_node_t op, const char *msg)
{
  val_t tvar;

  op = implicit_arithmetic_conversion (op, (ER_node_t) &tvar);
  if (expect (ER_NODE_MODE (op) == ER_NM_int))
    return ER_i (op) != 0;
  else if (expect (ER_NODE_MODE (op) == ER_NM_float))
    return ER_f (op) != 0.0;
  else if (ER_NODE_MODE (op) == ER_NM_long)
    return mpz_sgn (*ER_mpz_ptr (ER_l (op))) != 0;
  else
    eval_error (optype_bc_decl, get_cpos (), msg);
}

static int do_always_inline
common_bt (ER_node_t op1)
{
#ifndef SMALL_CODE
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    return ER_i (op1) != 0;
  else
#endif
    return non_zero_p (op1, DERR_invalid_for_guard_expr_type);
}

static int do_always_inline
execute_btcmp (int int_p, ER_node_t op1, ER_node_t op2, int bcmp_resn,
	       BC_node_mode_t cmp_nm, int icmp (rint_t, rint_t),
	       int fcmp (rfloat_t, rfloat_t),
	       void gencmp (BC_node_mode_t, ER_node_t, ER_node_t, ER_node_t, int))
{
  ER_node_t res;

  if (int_p)
    {
      d_assert (ER_NODE_MODE (op1) == ER_NM_int
		&& ER_NODE_MODE (op2) == ER_NM_int);
      return icmp (ER_i (op1), ER_i (op2));
    }
  if (expect (ER_NODE_MODE (op1) == ER_NM_int
	      && ER_NODE_MODE (op2) == ER_NM_int))
    return icmp (ER_i (op1), ER_i (op2));
  res = get_op (bcmp_resn);
  comp_op (cmp_nm, res, op1, op2, FALSE, icmp, fcmp, gencmp);
  return common_bt (res);
}

static int do_always_inline
execute_btcmpi (int int_p, ER_node_t op1, int i, int bcmp_resn,
		BC_node_mode_t cmp_nm, int icmp (rint_t, rint_t),
		int fcmp (rfloat_t, rfloat_t),
		void gencmp (BC_node_mode_t, ER_node_t, ER_node_t, ER_node_t, int))
{
  ER_node_t res, op2;
  static val_t v;

  if (int_p)
    {
      d_assert (ER_NODE_MODE (op1) == ER_NM_int);
      return icmp (ER_i (op1), i);
    }
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    return icmp (ER_i (op1), i);
  res = get_op (bcmp_resn);
  op2 = (ER_node_t) &v;
  ER_SET_MODE (op2, ER_NM_int);
  ER_set_i (op2, i);
  comp_op (cmp_nm, res, op1, op2, FALSE, icmp, fcmp, gencmp);
  return common_bt (res);
}

static int do_always_inline
execute_btcmpinc (int int_p,
		  ER_node_t op1, ER_node_t op2, int bcmp_resn, int i,
		  int icmp (rint_t, rint_t),
		  int fcmp (rfloat_t, rfloat_t),
		  void gencmp (BC_node_mode_t, ER_node_t, ER_node_t, ER_node_t, int))
{
  val_t v;
  rint_t res;

  if (int_p)
    {
      d_assert (ER_NODE_MODE (op1) == ER_NM_int
		&& ER_NODE_MODE (op2) == ER_NM_int);
      res = i + ER_i (op1);
      ER_set_i (op1, res);
      return icmp (res, ER_i (op2));
    }
  if (expect (ER_NODE_MODE (op1) == ER_NM_int
	      && ER_NODE_MODE (op2) == ER_NM_int))
    {
      res = i + ER_i (op1);
      ER_set_i (op1, res);
      return icmp (res, ER_i (op2));
    }
  ER_SET_MODE ((ER_node_t) &v, ER_NM_int);
  ER_set_i ((ER_node_t) &v, i);
  execute_plus_op (op1, op1, (ER_node_t) &v, FALSE);
  return execute_btcmp (FALSE, op1, op2, bcmp_resn, BC_NM_eq, icmp, fcmp, gencmp);
}

static int do_always_inline
execute_cmpi (ER_node_t res, ER_node_t op1, int op3n,
	      ER_node_t *op2, int icmp (rint_t, rint_t))
{
  rint_t i;
  static val_t v;

  i = op3n;
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, icmp (ER_i (op1), i));
      return FALSE;
    }
  *op2 = (ER_node_t) &v;
  ER_SET_MODE (*op2, ER_NM_int);
  ER_set_i (*op2, i);
  return TRUE;
}

static void do_always_inline
common_stvt (ER_node_t vec_tab, ER_node_t index, ER_node_t val)
{
  if (expect (ER_NODE_MODE (vec_tab) == ER_NM_vect))
    {
      ER_node_t v = ER_vect (vec_tab);

      if (expect (ER_NODE_MODE (index) == ER_NM_int))
	{
	  if (expect (ER_NODE_MODE (v) == ER_NM_heap_pack_vect
		      && ! ER_immutable (v)))
	    {
	      rint_t index_val;
	      ER_node_mode_t el_type = ER_pack_vect_el_mode (v);
	      ER_node_mode_t val_type = ER_NODE_MODE (val);

	      if (el_type == ER_NM_int && val_type == ER_NM_int)
		{
		  index_val = check_vector_index_value (v, ER_i (index));
		  ((rint_t *) ER_pack_els (v)) [index_val] = ER_i (val);
		  return;
		}
	      if (el_type == ER_NM_float && val_type == ER_NM_float)
		{
		  index_val = check_vector_index_value (v, ER_i (index));
		  ((rfloat_t *) ER_pack_els (v)) [index_val] = ER_f (val);
		  return;
		}
	    }
	}
    }
  store_vect_tab_designator_value (vec_tab, index, val);
}

static void do_always_inline
stvt (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  common_stvt (res, op1, op2);
}

static void do_always_inline
stvtu (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (ER_NODE_MODE (op2) == ER_NM_undef)
    eval_error (accessop_bc_decl, get_cpos (),
		DERR_undefined_value_assign);
  common_stvt (res, op1, op2);
}

static void do_always_inline
sts (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  store_stack_designator_value (res, op1, op2);
}

static void do_always_inline
stsu (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (ER_NODE_MODE (op2) == ER_NM_undef)
    eval_error (accessop_bc_decl, get_cpos (),
		DERR_undefined_value_assign);
  store_stack_designator_value (res, op1, op2);
}

static void do_always_inline
ste (ER_node_t res, ER_node_t op2)
{
  *(val_t *) ER_external_var_ref (res) = *(val_t *) op2;
}

static void do_always_inline
steu (ER_node_t res, ER_node_t op2)
{
  if (ER_NODE_MODE (op2) == ER_NM_undef)
    eval_error (accessop_bc_decl, get_cpos (),
		DERR_undefined_value_assign);
  *(val_t *) ER_external_var_ref (res) = *(val_t *) op2;
}

static void do_always_inline
ldnil (ER_node_t res)
{
  ER_SET_MODE (res, ER_NM_nil);
}

static void do_always_inline
ldthis (ER_node_t res)
{
  ER_node_t stack;
  BC_node_t block_node;
  
  ER_SET_MODE (res, ER_NM_stack);
  for (stack = cstack;; stack = ER_context (stack))
    {
      d_assert (stack != NULL);
      block_node = ER_block_node (stack);
      
      if (BC_NODE_MODE (block_node) == BC_NM_fblock)
	{
	  ER_set_stack (res, stack);
	  break;
	}
    }
}

static void do_always_inline
ldch (ER_node_t res, int op2n)
{
  ER_SET_MODE (res, ER_NM_char);
  ER_set_ch (res, op2n);
}

static void do_always_inline
ldi (ER_node_t res, int op2n)
{
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, op2n);
}

static void do_always_inline
ldbi (ER_node_t res)
{
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, BC_bi (cpc));
}

static void do_always_inline
ldl (ER_node_t res)
{
  ER_node_t heap_gmp;
  
  ER_SET_MODE (res, ER_NM_long);
  heap_gmp = create_gmp ();
  ER_set_l (res, heap_gmp);
  mpz_set (*ER_mpz_ptr (heap_gmp), *BC_mpz_ptr (cpc));
}

static void do_always_inline
ldf (ER_node_t res)
{
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, BC_f (cpc));
}

static void do_always_inline
lds (ER_node_t res)
{
  ER_node_t vect;
	    
  vect = create_string (BC_str (cpc));
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
}

static void do_always_inline
ldtp (ER_node_t res, int op2n)
{
  ER_SET_MODE (res, ER_NM_type);
  ER_set_type (res, op2n);
}

static void do_always_inline
flat (ER_node_t op1)
{
  ER_set_dim (op1, 0);
}

static void do_inline
execute_a_period_operation (int block_decl_ident_number, ER_node_t res,
			    ER_node_t op, int lvalue_p, int lvalue_val_p)
{
  BC_node_t block, fblock, decl;
  ER_node_t stack, ref, val;

  if (ER_NODE_MODE (op) == ER_NM_stack)
    {
      stack = ER_stack (op);
      block = ER_block_node (stack);
      if ((decl = BC_hint (cpc)) != NULL && BC_decl_scope (decl) == block)
	{
	  if (BC_NODE_MODE (decl) == BC_NM_vdecl)
	    {
	      if (! lvalue_p)
		val = res;
	      else
		{
		  ER_SET_MODE (res, ER_NM_stack);
		  ER_set_stack (res, stack);
		  ER_SET_MODE (IVAL (res, 1), ER_NM_int);
		  ER_set_i (IVAL (res, 1), BC_var_num (decl));
		  if (! lvalue_val_p)
		    return;
		  val = IVAL (res, 2);
		}
	      ref = IVAL (ER_stack_vars (stack), BC_var_num (decl));
	      if (ER_NODE_MODE (ref) != ER_NM_undef)
		{
		  *(val_t *) val = *(val_t *) ref;
		  return;
		}
	    }
	  else
	    {
	      BC_node_t fblock = BC_fblock (decl);

	      assert (BC_NODE_MODE (decl) == BC_NM_fdecl && ! lvalue_p);
	      if (BC_forward_p (fblock))
		eval_error (accessvalue_bc_decl, get_cpos (),
			    DERR_undefined_class_or_fun, BC_ident (decl));
	      ER_SET_MODE (res, ER_NM_code);
	      ER_set_code_context (res, stack);
	      ER_set_code_id (res, CODE_ID (fblock));
	      return;
	    }
	}
    }
  execute_general_period_operation (block_decl_ident_number, res,
				    op, lvalue_p, lvalue_val_p);
}

static void do_always_inline
fld (ER_node_t res, ER_node_t op1, int op3n)
{
  execute_a_period_operation (op3n, res, op1, FALSE, FALSE);
}

static void do_always_inline
lfld (ER_node_t res, ER_node_t op1, int op3n)
{
  execute_a_period_operation (op3n, res, op1, TRUE, FALSE);
}

static void do_always_inline
lfldv (ER_node_t res, ER_node_t op1, int op3n)
{
  execute_a_period_operation (op3n, res, op1, TRUE, TRUE);
}

static int do_always_inline
brts (ER_node_t op1, ER_node_t res)
{
  int true_p;

#ifndef SMALL_CODE
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    true_p = ER_i (op1) != 0;
  else
#endif
    true_p = non_zero_p (op1, DERR_logical_or_operands_types);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, true_p);
  return true_p;
}

static int do_always_inline
brfs (ER_node_t op1, ER_node_t res)
{
  int true_p;

#ifndef SMALL_CODE
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    true_p = ER_i (op1) != 0;
  else
#endif
    true_p = non_zero_p (op1, DERR_logical_and_operands_types);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, true_p);
  return ! true_p;
}

static void do_always_inline
lconv (ER_node_t res, ER_node_t op1)
{
  int true_p;
  
#ifndef SMALL_CODE
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    true_p = ER_i (op1) != 0;
  else
#endif
    true_p = non_zero_p (op1, DERR_logical_operands_types);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, true_p);
}

static void do_always_inline
in (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_in_op (res, op1, op2, TRUE);
}

static void do_always_inline
not (ER_node_t res, ER_node_t op1)
{
  execute_not_op (res, op1, TRUE);
}

static void do_always_inline
inot (ER_node_t res, ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_not (ER_i (op1)));
}

static void do_always_inline
bnot (ER_node_t res, ER_node_t op1)
{
  execute_bitwise_not_op (res, op1, TRUE);
}

static void do_always_inline
ibnot (ER_node_t res, ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, ibitwise_not (ER_i (op1)));
}

static void do_always_inline
foldop (ER_node_t res, ER_node_t op1)
{
  fold_vect_op (res, op1);
}

static void do_always_inline
common_eq (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_eq_op (res, op1, op2, TRUE);
}

static void do_always_inline
eq (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  common_eq (res, op1, op2);
}

static void do_always_inline
ieq (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (int_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_eq (ER_i (op1), ER_i (op2)));
}

static void do_always_inline
eqi (ER_node_t res, ER_node_t op1, int op3n)
{
  ER_node_t op2;
  
  if (execute_cmpi (res, op1, op3n, &op2, i_eq))
    common_eq (res, op1, op2);
}

static void do_always_inline
ieqi (ER_node_t res, ER_node_t op1, int op3n)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_eq (ER_i (op1), op3n));
}

static void do_always_inline
common_ne (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_ne_op (res, op1, op2, TRUE);
}

static void do_always_inline
ne (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  common_ne (res, op1, op2);
}

static void do_always_inline
ine (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (int_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_ne (ER_i (op1), ER_i (op2)));
}

static void do_always_inline
nei (ER_node_t res, ER_node_t op1, int op3n)
{
  ER_node_t op2;
  
  if (execute_cmpi (res, op1, op3n, &op2, i_ne))
    common_ne (res, op1, op2);
}

static void do_always_inline
inei (ER_node_t res, ER_node_t op1, int op3n)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_ne (ER_i (op1), op3n));
}

static void do_always_inline
id (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_identity_op (TRUE, res, op1, op2, TRUE);
}

static void do_always_inline
unid (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_identity_op (FALSE, res, op1, op2, TRUE);
}

static void do_always_inline
common_lt (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_lt_op (res, op1, op2, TRUE);
}

static void do_always_inline
lt (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  common_lt (res, op1, op2);
}

static void do_always_inline
ilt (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (int_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_lt (ER_i (op1), ER_i (op2)));
}

static void do_always_inline
lti (ER_node_t res, ER_node_t op1, int op3n)
{
  ER_node_t op2;
  
  if (execute_cmpi (res, op1, op3n, &op2, i_lt))
    common_lt (res, op1, op2);
}

static void do_always_inline
ilti (ER_node_t res, ER_node_t op1, int op3n)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_lt (ER_i (op1), op3n));
}

static void do_always_inline
common_ge (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_ge_op (res, op1, op2, TRUE);
}

static void do_always_inline
ge (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  common_ge (res, op1, op2);
}

static void do_always_inline
ige (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (int_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_ge (ER_i (op1), ER_i (op2)));
}

static void do_always_inline
gei (ER_node_t res, ER_node_t op1, int op3n)
{
  ER_node_t op2;
  
  if (execute_cmpi (res, op1, op3n, &op2, i_ge))
    common_ge (res, op1, op2);
}

static void do_always_inline
igei (ER_node_t res, ER_node_t op1, int op3n)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_ge (ER_i (op1), op3n));
}

static void do_always_inline
common_gt (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_gt_op (res, op1, op2, TRUE);
}

static void do_always_inline
gt (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  common_gt (res, op1, op2);
}

static void do_always_inline
igt (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (int_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_gt (ER_i (op1), ER_i (op2)));
}

static void do_always_inline
gti (ER_node_t res, ER_node_t op1, int op3n)
{
  ER_node_t op2;
  
  if (execute_cmpi (res, op1, op3n, &op2, i_gt))
    common_gt (res, op1, op2);
}

static void do_always_inline
igti (ER_node_t res, ER_node_t op1, int op3n)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_gt (ER_i (op1), op3n));
}

static void do_always_inline
common_le (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_le_op (res, op1, op2, TRUE);
}

static void do_always_inline
le (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  common_le (res, op1, op2);
}

static void do_always_inline
ile (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (int_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_le (ER_i (op1), ER_i (op2)));
}

static void do_always_inline
lei (ER_node_t res, ER_node_t op1, int op3n)
{
  ER_node_t op2;
  
  if (execute_cmpi (res, op1, op3n, &op2, i_le))
    common_le (res, op1, op2);
}

static void do_always_inline
ilei (ER_node_t res, ER_node_t op1, int op3n)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, i_le (ER_i (op1), op3n));
}

static void do_always_inline
plus (ER_node_t res, ER_node_t op1)
{
  execute_unary_ar_op (res, op1, TRUE, DERR_unary_plus_operand_type,
		       iunary_plus, funary_plus, lunary_plus);
}

static void do_always_inline
iplus (ER_node_t res, ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, iunary_plus (ER_i (op1)));
}

static void do_always_inline
fplus (ER_node_t res, ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_float);
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, funary_plus (ER_f (op1)));
}

static void do_always_inline
minus (ER_node_t res, ER_node_t op1)
{
  execute_unary_ar_op (res, op1, TRUE, DERR_unary_minus_operand_type,
		       iunary_minus, funary_minus, lunary_minus);
}

static void do_always_inline
iminus (ER_node_t res, ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, iunary_minus (ER_i (op1)));
}

static void do_always_inline
fminus (ER_node_t res, ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_float);
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, funary_minus (ER_f (op1)));
}

static void do_always_inline
length (ER_node_t res, ER_node_t op1)
{
  execute_length_op (res, op1, TRUE);
}

static void do_always_inline
constop (ER_node_t res, ER_node_t op1)
{
  execute_const_op (res, op1, TRUE);
}

static void do_always_inline
new (ER_node_t res, ER_node_t op1)
{
  execute_new_op (res, op1, TRUE);
}

static void do_always_inline
tpof (ER_node_t res, ER_node_t op1)
{
  execute_typeof_op (res, op1, TRUE);
}

static void do_always_inline
chof (ER_node_t res, ER_node_t op1)
{
  execute_charof_op (res, op1, TRUE);
}

static void do_always_inline
iof (ER_node_t res, ER_node_t op1)
{
  execute_intof_op (res, op1, TRUE);
}

static void do_always_inline
lof (ER_node_t res, ER_node_t op1)
{
  execute_longof_op (res, op1, TRUE);
}

static void do_always_inline
fof (ER_node_t res, ER_node_t op1)
{
  execute_floatof_op (res, op1, TRUE);
}

static void do_always_inline
fmtvecof (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_vectorof_op (res, op1, op2, TRUE);
}

static void do_always_inline
vecof (ER_node_t res, ER_node_t op1)
{
  execute_vectorof_op (res, op1, NULL, TRUE);
}

static void do_always_inline
tabof (ER_node_t res, ER_node_t op1)
{
  execute_tableof_op (res, op1, TRUE);
}

extern void vec (ER_node_t res, ER_node_t op1, rint_t vect_parts_number);
extern void tab (ER_node_t res, ER_node_t op1, rint_t tab_els_number);

static void do_always_inline
ind (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (expect (ER_NODE_MODE (op1) == ER_NM_vect))
    {
      ER_node_t v = ER_vect (op1);

      if (expect (ER_NODE_MODE (op2) == ER_NM_int))
	{
	  if (expect (ER_NODE_MODE (v) == ER_NM_heap_pack_vect))
	    {
	      rint_t index_val;
	      ER_node_mode_t el_type = ER_pack_vect_el_mode (v);
	      
	      if (el_type == ER_NM_int)
		{
		  index_val = check_vector_index_value (v, ER_i (op2));
		  ER_SET_MODE (res, ER_NM_int);
		  ER_set_i (res, ((rint_t *) ER_pack_els (v)) [index_val]);
		  return;
		}
	      if (el_type == ER_NM_float)
		{
		  index_val = check_vector_index_value (v, ER_i (op2));
		  ER_SET_MODE (res, ER_NM_float);
		  ER_set_f (res, ((rfloat_t *) ER_pack_els (v)) [index_val]);
		  return;
		}
	    }
	}
      load_vector_element_by_index (res, v, op2);
    }
  else if (ER_NODE_MODE (op1) == ER_NM_tab)
    load_table_element_by_key (res, ER_tab (op1), op2);
  else
    eval_error (indexop_bc_decl, get_cpos (),
		DERR_index_operation_for_non_vec_tab);
}

static void do_always_inline
ind2 (ER_node_t res, ER_node_t op1, ER_node_t op2, ER_node_t op3)
{
  if (expect (ER_NODE_MODE (op1) == ER_NM_vect))
    {
      ER_node_t v = ER_vect (op1);

      if (expect (ER_NODE_MODE (op2) == ER_NM_int))
	{
	  if (expect (ER_NODE_MODE (v) == ER_NM_heap_pack_vect))
	    {
	      rint_t index_val;
	      ER_node_mode_t el_type = ER_pack_vect_el_mode (v);
	      
	      if (el_type == ER_NM_vect)
		{
		  index_val = check_vector_index_value (v, ER_i (op2));
		  ER_SET_MODE (res, ER_NM_vect);
		  ER_set_vect (res, ((ER_node_t *) ER_pack_els (v)) [index_val]);
		  ind (res, res, op3);
		  return;
		}
	    }
	}
      load_vector_element_by_index (res, v, op2);
      ind (res, res, op3);
    }
  else if (ER_NODE_MODE (op1) == ER_NM_tab)
    {
      load_table_element_by_key (res, ER_tab (op1), op2);
      ind (res, res, op3);
    }
  else
    eval_error (indexop_bc_decl, get_cpos (),
		DERR_index_operation_for_non_vec_tab);
}

static void do_always_inline
sl (ER_node_t res, ER_node_t op1, int op3n)
{
  slice_extract (res, op1, op3n);
}

static void do_always_inline
lslv (ER_node_t res, ER_node_t op1, int op3n)
{
  slice_extract (res, op1, op3n);
}

static void do_always_inline
call (ER_node_t op1, int op2n, int from_c_code_p)
{
  process_call (op1, op2n, FALSE, from_c_code_p);
}
  
static void do_always_inline
tcall (ER_node_t op1, int op2n, int from_c_code_p)
{
  process_call (op1, op2n, TRUE, from_c_code_p);
}

static void do_always_inline
mcall (ER_node_t op1, int op2n, ER_node_t op3, int op4n, int from_c_code_p)
{
  BC_node_t block, fblock, decl;
  ER_node_t stack, ref, val;

  if (ER_NODE_MODE (op3) != ER_NM_stack)
    eval_error (accessop_bc_decl, get_cpos (),
		DERR_value_is_not_class_instance_or_stack);
  stack = ER_stack (op3);
  block = ER_block_node (stack);
  if ((decl = BC_mhint (cpc)) == NULL || BC_decl_scope (decl) != block)
    {
      decl = find_field (op4n, &stack);
      if (BC_NODE_MODE (decl) != BC_NM_fdecl
	  && BC_NODE_MODE (decl) != BC_NM_efdecl)
	eval_error (callop_bc_decl, get_cpos (),
		    DERR_none_class_or_fun_before_left_bracket);
      if (BC_NODE_MODE (decl) == BC_NM_fdecl
	  && BC_forward_p (BC_fblock (decl)))
	eval_error (accessvalue_bc_decl, get_cpos (),
		    DERR_undefined_class_or_fun, BC_ident (decl));
      if (BC_decl_scope (decl) == block)
	BC_set_mhint (cpc, decl);
    }
  process_fun_class_call (decl, stack, op1, op1, op2n, FALSE, from_c_code_p);
}
  
/* Try to reuse the result of pure function.  Return true if we had an
   success.  Otherwise, setup new stack and return FALSE.  */
static int do_always_inline
heap_push_or_set_res (BC_node_t block_node, ER_node_t context,
		      val_t *call_start, int vars_num)
{
  ER_node_t stack;

  d_assert (BC_NODE_MODE (block_node) == BC_NM_fblock
	    && BC_vars_num (block_node) == vars_num);
#ifndef NO_PROFILE
  if (profile_flag)
    update_profile (block_node);
#endif
  if ((stack = BC_free_stacks (block_node)) != NULL
      && BC_pure_fun_p (block_node)
      && eq_val ((val_t *) ER_stack_vars (stack), call_start,
		 BC_pars_num (block_node)))
    {
      /* Restoring the calculated value.  */
      *(val_t *) call_start = *(val_t *) IVAL (ER_stack_vars (stack),
					       BC_pars_num (block_node));
      return TRUE;
    }
  heap_push_without_profile_update (block_node, context, vars_num, -1);
  return FALSE;
}

/* The same as previous but also process implementation functions.  */
static int do_always_inline
process_imm_fun_call (val_t *call_start, BC_node_t code, ER_node_t context,
		      int actuals_num, int vars_number,
		      int tail_flag, int from_c_code_p)
{
  int i, res_p;
  
  d_assert (BC_NODE_MODE (code) == BC_NM_fblock
	    && BC_fmode (code) != BC_builtin
	    && ! BC_thread_p (code) && ! BC_args_p (code)
	    && actuals_num == BC_pars_num (code));
  real_fun_call_pc = cpc;
  res_p = FALSE;
  if (tail_flag && ! BC_ext_life_p (ER_block_node (cstack))
      && context != cstack && cstack != uppest_stack
      /* We should not worry about extending stack.  Finally in the
	 chain of calls of different functions we have a function
	 block big enough to contain all subsequent tail calls.  */
      && (ER_all_block_vars_num (cstack) >= vars_number + BC_tvars_num (code)))
    {
      res_p = TRUE;
      ER_set_context (cstack, context);
      ER_set_block_node (cstack, code);
      ctop = IVAL (cvars, vars_number - 1);
    }
  else if (heap_push_or_set_res (code, context, call_start, vars_number))
    {
      INCREMENT_PC ();
      return FALSE; /* It might be pure function call w/o result use.  */
    }
  /* Transfer actuals.  */
  for (i = 0; i < actuals_num; i++)
    *(val_t *) IVAL (cvars, i) = *call_start++;
  /* Reset rest of variables.  */
  reset_vars ((ER_node_t) ((val_t *) cvars + actuals_num),
	      (ER_node_t) ((val_t *) cvars + vars_number));
  do_call (code, from_c_code_p);
  return res_p;
}

static int do_always_inline
common_icall (ER_node_t op1, int op2n, ER_node_t op3,
	      int vars_number, int tail_flag, int from_c_code_p)
{
  ctop = IVAL (op1, -1);
  return process_imm_fun_call ((val_t *) op1, BC_cfblock (cpc), op3,
			       op2n, vars_number, tail_flag, from_c_code_p);
}

static void do_always_inline
icall (ER_node_t op1, int op2n, int vars_number, int from_c_code_p)
{
  common_icall (op1, op2n,
		find_context_by_scope (BC_scope (BC_cfblock (cpc))),
		vars_number, FALSE, from_c_code_p);
}

static int do_always_inline
itcall (ER_node_t op1, int op2n, int vars_number, int from_c_code_p)
{
  return common_icall (op1, op2n,
		       find_context_by_scope (BC_scope (BC_cfblock (cpc))),
		       vars_number, TRUE, from_c_code_p);
}

static void do_always_inline
cicall (ER_node_t op1, int op2n, int vars_number, int from_c_code_p)
{
  common_icall (op1, op2n, cstack, vars_number, FALSE, from_c_code_p);
}

static int do_always_inline
citcall (ER_node_t op1, int op2n, int vars_number, int from_c_code_p)
{
  return common_icall (op1, op2n, cstack, vars_number, TRUE, from_c_code_p);
}

static void do_always_inline
ticall (ER_node_t op1, int op2n, int vars_number, int from_c_code_p)
{
  common_icall (op1, op2n, uppest_stack, vars_number, FALSE, from_c_code_p);
}

static int do_always_inline
titcall (ER_node_t op1, int op2n, int vars_number, int from_c_code_p)
{
  return common_icall (op1, op2n, uppest_stack, vars_number, TRUE, from_c_code_p);
}

static void do_always_inline
ibcall (ER_node_t op1, int op2n, int from_c_code_p)
{
  ctop = IVAL (op1, -1);
  process_imm_ifun_call (BC_cfblock (cpc), op2n, from_c_code_p);
}

static void do_always_inline
common_plus (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_plus_op (res, op1, op2, TRUE);
}

static void do_always_inline
cadd (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_plus (ER_i (op1), ER_i (op2)));
      return;
    }
  if (float_bin_op (op1, op2))
    {
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f_plus (ER_f (op1), ER_f (op2)));
      return;
    }
  common_plus (res, op1, op2);
}

static void do_always_inline
add (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cadd (FALSE, res, op1, op2);
}

static void do_always_inline
iadd (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cadd (TRUE, res, op1, op2);
}

static void do_always_inline
fadd (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (float_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f_plus (ER_f (op1), ER_f (op2)));
}

static void do_always_inline
ifadd (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  rfloat_t f;

  d_assert (ER_NODE_MODE (op1) == ER_NM_int
	    && ER_NODE_MODE (op2) == ER_NM_float);
  f = f_plus ((rfloat_t) ER_i (op1), ER_f (op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f);
}

static void do_always_inline
caddi (int int_p, ER_node_t res, ER_node_t op1, int op3n)
{
  ER_node_t op2;
  val_t v;
  rint_t i;

  i = op3n;
  if (int_p || expect (ER_NODE_MODE (op1) == ER_NM_int))
    {
      d_assert (! int_p || ER_NODE_MODE (op1) == ER_NM_int);
      i = i_plus (ER_i (op1), i);
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
    }
  else
    {
      op2 = (ER_node_t) &v;
      ER_SET_MODE (op2, ER_NM_int);
      ER_set_i (op2, i);
      common_plus (res, op1, op2);
    }
}

static void do_always_inline
addi (ER_node_t op1, ER_node_t op2, int op3n)
{
  caddi (FALSE, op1, op2, op3n);
}

static void do_always_inline
iaddi (ER_node_t op1, ER_node_t op2, int op3n)
{
  caddi (TRUE, op1, op2, op3n);
}

static void do_always_inline
faddi (ER_node_t op1, ER_node_t op2, int op3n)
{
  rfloat_t f;

  d_assert (ER_NODE_MODE (op2) == ER_NM_float);
  f = f_plus (ER_f (op2), (rfloat_t) op3n);
  ER_SET_MODE (op1, ER_NM_float);
  ER_set_f (op1, f);
}

static void do_always_inline
csub (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_minus (ER_i (op1), ER_i (op2)));
      return;
    }
  if (float_bin_op (op1, op2))
    {
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f_minus (ER_f (op1), ER_f (op2)));
      return;
    }
  execute_minus_op (res, op1, op2, TRUE);
}

static void do_always_inline
sub (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  csub (FALSE, res, op1, op2);
}

static void do_always_inline
isub (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  csub (TRUE, res, op1, op2);
}

static void do_always_inline
fsub (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (float_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f_minus (ER_f (op1), ER_f (op2)));
}

static void do_always_inline
ifsub (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  rfloat_t f;

  d_assert (ER_NODE_MODE (op1) == ER_NM_int
	    && ER_NODE_MODE (op2) == ER_NM_float);
  f = f_minus ((rfloat_t) ER_i (op1), ER_f (op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f);
}

static void do_always_inline
fisub (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  rfloat_t f;

  d_assert (ER_NODE_MODE (op1) == ER_NM_float
	    && ER_NODE_MODE (op2) == ER_NM_int);
  f = f_minus (ER_f (op1), (rfloat_t) ER_i (op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f);
}

static void do_always_inline
common_mult (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_mult_op (res, op1, op2, TRUE);
}

static void do_always_inline
cmult (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_mult (ER_i (op1), ER_i (op2)));
      return;
    }
  if (float_bin_op (op1, op2))
    {
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f_mult (ER_f (op1), ER_f (op2)));
      return;
    }
  common_mult (res, op1, op2);
}

static void do_always_inline
mult (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cmult (FALSE, res, op1, op2);
}

static void do_always_inline
imult (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cmult (TRUE, res, op1, op2);
}

static void do_always_inline
fmult (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (float_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f_mult (ER_f (op1), ER_f (op2)));
}

static void do_always_inline
ifmult (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  rfloat_t f;

  d_assert (ER_NODE_MODE (op1) == ER_NM_int
	    && ER_NODE_MODE (op2) == ER_NM_float);
  f = f_mult ((rfloat_t) ER_i (op1), ER_f (op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f);
}

static void do_always_inline
cmulti (int int_p, ER_node_t res, ER_node_t op1, int op3n)
{
  ER_node_t op2;
  val_t v;
  rint_t i;

  i = op3n;
  if (int_p || expect (ER_NODE_MODE (op1) == ER_NM_int))
    {
      d_assert (! int_p || ER_NODE_MODE (op1) == ER_NM_int);
      i = i_mult (ER_i (op1), i);
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
    }
  else
    {
      op2 = (ER_node_t) &v;
      ER_SET_MODE (op2, ER_NM_int);
      ER_set_i (op2, i);
      common_mult (res, op1, op2);
    }
}

static void do_always_inline
multi (ER_node_t op1, ER_node_t op2, int op3n)
{
  cmulti (FALSE, op1, op2, op3n);
}

static void do_always_inline
imulti (ER_node_t op1, ER_node_t op2, int op3n)
{
  cmulti (TRUE, op1, op2, op3n);
}

static void do_always_inline
fmulti (ER_node_t op1, ER_node_t op2, int op3n)
{
  rfloat_t f;

  d_assert (ER_NODE_MODE (op2) == ER_NM_float);
  f = f_mult (ER_f (op2), (rfloat_t) op3n);
  ER_SET_MODE (op1, ER_NM_float);
  ER_set_f (op1, f);
}

static void do_always_inline
cdiv (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_div (ER_i (op1), ER_i (op2)));
      return;
    }
  if (float_bin_op (op1, op2))
    {
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f_div (ER_f (op1), ER_f (op2)));
      return;
    }
  execute_div_op (res, op1, op2, TRUE);
}

static void do_always_inline
divop (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cdiv (FALSE, res, op1, op2);
}

static void do_always_inline
idiv (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cdiv (TRUE, res, op1, op2);
}

static void do_always_inline
fdiv (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (float_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f_div (ER_f (op1), ER_f (op2)));
}

static void do_always_inline
ifdiv (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  rfloat_t f;

  d_assert (ER_NODE_MODE (op1) == ER_NM_int
	    && ER_NODE_MODE (op2) == ER_NM_float);
  f = f_div ((rfloat_t) ER_i (op1), ER_f (op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f);
}

static void do_always_inline
fidiv (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  rfloat_t f;

  d_assert (ER_NODE_MODE (op1) == ER_NM_float
	    && ER_NODE_MODE (op2) == ER_NM_int);
  f = f_div (ER_f (op1), (rfloat_t) ER_i (op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f);
}

static void do_always_inline
cmod (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_mod (ER_i (op1), ER_i (op2)));
      return;
    }
  if (float_bin_op (op1, op2))
    {
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f_mod (ER_f (op1), ER_f (op2)));
      return;
    }
  execute_mod_op (res, op1, op2, TRUE);
}

static void do_always_inline
mod (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cmod (FALSE, res, op1, op2);
}

static void do_always_inline
imod (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cmod (TRUE, res, op1, op2);
}

static void do_always_inline
fmodop (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  d_assert (float_bin_op (op1, op2));
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, f_mod (ER_f (op1), ER_f (op2)));
}

static int madd_mult_p;

static void do_always_inline
madd (ER_node_t res, ER_node_t op1, ER_node_t op2, ER_node_t op3)
{
  val_t interm;
  rint_t i;
  rfloat_t f;

  if (expect (ER_NODE_MODE (op1) == ER_NM_int)
      && expect (ER_NODE_MODE (op2) == ER_NM_int)
      && expect (ER_NODE_MODE (op3) == ER_NM_int))
    {
      i = ER_i (op1) * ER_i (op2) + ER_i (op3);
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
      return;
    }
  if (expect (ER_NODE_MODE (op1) == ER_NM_float)
      && expect (ER_NODE_MODE (op2) == ER_NM_float)
      && expect (ER_NODE_MODE (op3) == ER_NM_float))
    {
      f = ER_f (op1) * ER_f (op2) + ER_f (op3);
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f);
      return;
    }
  madd_mult_p = TRUE;
  cmult (FALSE, (ER_node_t) &interm, op1, op2);
  madd_mult_p = FALSE;
  cadd (FALSE, res, (ER_node_t) &interm, op3);
}

static void do_always_inline
concat (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  execute_concat_op (res, op1, op2, TRUE);
}

static void do_always_inline
clsh (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_lshift (ER_i (op1), ER_i (op2)));
      return;
    }
  execute_lshift_op (res, op1, op2, TRUE);
}

static void do_always_inline
lsh (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  clsh (FALSE, res, op1, op2);
}

static void do_always_inline
ilsh (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  clsh (TRUE, res, op1, op2);
}

static void do_always_inline
crsh (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_rshift (ER_i (op1), ER_i (op2)));
      return;
    }
  execute_rshift_op (res, op1, op2, TRUE);
}

static void do_always_inline
rsh (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  crsh (FALSE, res, op1, op2);
}

static void do_always_inline
irsh (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  crsh (TRUE, res, op1, op2);
}

static void do_always_inline
cash (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_ashift (ER_i (op1), ER_i (op2)));
      return;
    }
  execute_ashift_op (res, op1, op2, TRUE);
}

static void do_always_inline
ash (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cash (FALSE, res, op1, op2);
}

static void do_always_inline
iash (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cash (TRUE, res, op1, op2);
}

static void do_always_inline
cand (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_and (ER_i (op1), ER_i (op2)));
      return;
    }
  execute_and_op (res, op1, op2, TRUE);
}

static void do_always_inline
and (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cand (FALSE, res, op1, op2);
}

static void do_always_inline
iand (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cand (TRUE, res, op1, op2);
}

static void do_always_inline
cxor (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_xor (ER_i (op1), ER_i (op2)));
      return;
    }
  execute_xor_op (res, op1, op2, TRUE);
}

static void do_always_inline
xor (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cxor (FALSE, res, op1, op2);
}

static void do_always_inline
ixor (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cxor (TRUE, res, op1, op2);
}

static void do_always_inline
cor (int int_p, ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  if (int_p || int_bin_op (op1, op2))
    {
      d_assert (! int_p || int_bin_op (op1, op2));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i_or (ER_i (op1), ER_i (op2)));
      return;
    }
  execute_or_op (res, op1, op2, TRUE);
}

static void do_always_inline
or (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cor (FALSE, res, op1, op2);
}

static void do_always_inline
ior (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  cor (TRUE, res, op1, op2);
}

static void do_always_inline
common_op_st (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  store_designator_value (res, op1, op2);
}

static void do_always_inline
add_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_plus_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
sub_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_minus_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
mult_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_mult_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
div_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_div_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
mod_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_mod_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
concat_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_concat_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
lsh_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_lshift_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
rsh_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_rshift_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
ash_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_ashift_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
and_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_and_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
xor_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_xor_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
or_st (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4)
{
  execute_or_op (op4, op4, op3, TRUE);
  common_op_st (op1, op2, op4);
}

static void do_always_inline
common_slst (ER_node_t res, int op1, ER_node_t op2)
{
  slice_assign (res, op1, op2);
}

static void do_always_inline
op_slst (ER_node_t op1, int op2n, ER_node_t op3, ER_node_t op4)
{
  binary_vect_op (op4, op4, op3);
  common_slst (op1, op2n, op4);
}

static void do_always_inline
slst (ER_node_t op1, int op2n, ER_node_t op3)
{
  common_slst (op1, op2n, op3);
}

static void do_always_inline
b (void)
{
}

static int do_always_inline
btdef (ER_node_t op1)
{
  return ER_NODE_MODE (op1) != ER_NM_undef;
}

static int do_always_inline
bf (ER_node_t op1)
{
#ifndef SMALL_CODE
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    return ER_i (op1) == 0;
  else
#endif
    return ! non_zero_p (op1, DERR_invalid_if_expr_type);
}

static int do_always_inline
ibf (ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  return ER_i (op1) == 0;
}

static int do_always_inline
bfni (ER_node_t op1)
{
#ifndef SMALL_CODE
  if (expect (ER_NODE_MODE (op1) == ER_NM_int))
    return ER_i (op1) == 0;
  else
#endif
    return ! non_zero_p (op1, DERR_cond_operand_type);
}

static int do_always_inline
ibfni (ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  return ER_i (op1) == 0;
}

static int do_always_inline
cbteqinc (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return execute_btcmpinc (int_p, op1, bcmp_op2, bcmp_resn, inc, i_eq, f_eq,
			   execute_common_eq_ne_op);
}

static int do_always_inline
bteqinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbteqinc (FALSE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
ibteqinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbteqinc (TRUE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
cbtneinc (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return execute_btcmpinc (int_p, op1, bcmp_op2, bcmp_resn, inc, i_ne, f_ne,
			   execute_common_eq_ne_op);
}

static int do_always_inline
btneinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtneinc (FALSE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
ibtneinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtneinc (TRUE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
cbtgeinc (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return execute_btcmpinc (int_p, op1, bcmp_op2, bcmp_resn, inc, i_ge, f_ge,
			   execute_common_cmp_op);
}

static int do_always_inline
btgeinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtgeinc (FALSE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
ibtgeinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtgeinc (TRUE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
cbtltinc (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return execute_btcmpinc (int_p, op1, bcmp_op2, bcmp_resn, inc, i_lt, f_lt,
			   execute_common_cmp_op);
}

static int do_always_inline
btltinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtltinc (FALSE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
ibtltinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtltinc (TRUE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
cbtleinc (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return execute_btcmpinc (int_p, op1, bcmp_op2, bcmp_resn, inc, i_le, f_le,
			   execute_common_cmp_op);
}

static int do_always_inline
btleinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtleinc (FALSE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
ibtleinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtleinc (TRUE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
cbtgtinc (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return execute_btcmpinc (int_p, op1, bcmp_op2, bcmp_resn, inc, i_gt, f_gt,
			   execute_common_cmp_op);
}

static int do_always_inline
btgtinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtgtinc (FALSE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
ibtgtinc (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn, int inc)
{
  return cbtgtinc (TRUE, op1, bcmp_op2, bcmp_resn, inc);
}

static int do_always_inline
cbteq (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return execute_btcmp (int_p, op1, bcmp_op2, bcmp_resn, BC_NM_eq, i_eq, f_eq,
			execute_common_eq_ne_op);
}

static int do_always_inline
bteq (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbteq (FALSE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
ibteq (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbteq (TRUE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
fbteq (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  d_assert (float_bin_op (op1, bcmp_op2));
  return f_eq (ER_f (op1), ER_f (bcmp_op2));
}

static int do_always_inline
cbtne (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return execute_btcmp (int_p, op1, bcmp_op2, bcmp_resn, BC_NM_gt, i_ne, f_ne,
			execute_common_eq_ne_op);
}

static int do_always_inline
btne (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtne (FALSE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
ibtne (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtne (TRUE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
fbtne (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  d_assert (float_bin_op (op1, bcmp_op2));
  return f_ne (ER_f (op1), ER_f (bcmp_op2));
}

static int do_always_inline
cbtge (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return execute_btcmp (int_p, op1, bcmp_op2, bcmp_resn, BC_NM_ge, i_ge, f_ge,
			execute_common_cmp_op);
}

static int do_always_inline
btge (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtge (FALSE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
ibtge (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtge (TRUE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
fbtge (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  d_assert (float_bin_op (op1, bcmp_op2));
  return f_ge (ER_f (op1), ER_f (bcmp_op2));
}

static int do_always_inline
cbtlt (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return execute_btcmp (int_p, op1, bcmp_op2, bcmp_resn, BC_NM_lt, i_lt, f_lt,
			execute_common_cmp_op);
}

static int do_always_inline
btlt (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtlt (FALSE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
ibtlt (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtlt (TRUE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
fbtlt (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  d_assert (float_bin_op (op1, bcmp_op2));
  return f_lt (ER_f (op1), ER_f (bcmp_op2));
}

static int do_always_inline
cbtle (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return execute_btcmp (int_p, op1, bcmp_op2, bcmp_resn, BC_NM_le, i_le, f_le,
			execute_common_cmp_op);
}

static int do_always_inline
btle (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtle (FALSE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
ibtle (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtle (TRUE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
fbtle (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  d_assert (float_bin_op (op1, bcmp_op2));
  return f_le (ER_f (op1), ER_f (bcmp_op2));
}

static int do_always_inline
cbtgt (int int_p, ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return execute_btcmp (int_p, op1, bcmp_op2, bcmp_resn, BC_NM_gt, i_gt, f_gt,
			execute_common_cmp_op);
}

static int do_always_inline
btgt (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtgt (FALSE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
ibtgt (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  return cbtgt (TRUE, op1, bcmp_op2, bcmp_resn);
}

static int do_always_inline
fbtgt (ER_node_t op1, ER_node_t bcmp_op2, int bcmp_resn)
{
  d_assert (float_bin_op (op1, bcmp_op2));
  return f_gt (ER_f (op1), ER_f (bcmp_op2));
}

static int do_always_inline
cbteqi (int int_p, ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return (execute_btcmpi (int_p, op1, bcmp_op2n, bcmp_resn, BC_NM_eq, i_eq, f_eq,
			  execute_common_eq_ne_op));
}

static int do_always_inline
bteqi (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbteqi (FALSE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
ibteqi (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbteqi (TRUE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
cbtnei (int int_p, ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return (execute_btcmpi (int_p, op1, bcmp_op2n, bcmp_resn, BC_NM_ne, i_ne, f_ne,
			  execute_common_eq_ne_op));
}

static int do_always_inline
btnei (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtnei (FALSE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
ibtnei (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtnei (TRUE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
cbtlti (int int_p, ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return (execute_btcmpi (int_p, op1, bcmp_op2n, bcmp_resn, BC_NM_lt, i_lt, f_lt,
			  execute_common_cmp_op));
}

static int do_always_inline
btlti (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtlti (FALSE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
ibtlti (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtlti (TRUE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
cbtlei (int int_p, ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return (execute_btcmpi (int_p, op1, bcmp_op2n, bcmp_resn, BC_NM_le, i_le, f_le,
			  execute_common_cmp_op));
}

static int do_always_inline
btlei (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtlei (FALSE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
ibtlei (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtlei (TRUE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
cbtgti (int int_p, ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return (execute_btcmpi (int_p, op1, bcmp_op2n, bcmp_resn, BC_NM_gt, i_gt, f_gt,
			  execute_common_cmp_op));
}

static int do_always_inline
btgti (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtgti (FALSE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
ibtgti (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtgti (TRUE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
cbtgei (int int_p, ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return (execute_btcmpi (int_p, op1, bcmp_op2n, bcmp_resn, BC_NM_ge, i_ge, f_ge,
			  execute_common_cmp_op));
}

static int do_always_inline
btgei (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtgei (FALSE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
ibtgei (ER_node_t op1, int bcmp_op2n, int bcmp_resn)
{
  return cbtgei (TRUE, op1, bcmp_op2n, bcmp_resn);
}

static int do_always_inline
bt (ER_node_t op1)
{
  return common_bt (op1);
}

static int do_always_inline
ibt (ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  return ER_i (op1) != 0;
}

static void out (void);

static int do_always_inline
bend (void)
{
  BC_node_t bc_node = ER_block_node (cstack);

  d_assert (BC_block (cpc) == bc_node);
  sync_flag = BC_saved_sync_p (bc_node);
  if (cstack == uppest_stack)
    return TRUE;
  d_assert (BC_vars_num (bc_node) >= 0);
  heap_pop ();
  return FALSE;
}

/* The return value is used only from the evaluator not C-code.  So it
   has sense only when we are in the evaluator.  Return true when we
   need to leave the evaluator.  */
static int do_always_inline
evaluate_exit_p (ER_node_t from_stack)
{
  if (cpc == NULL)
    return TRUE;
  if (BC_NODE_MODE (ER_block_node (cstack)) == BC_NM_fblock)
    {
      /* It is a return to C-code.  */
      if (ER_c_stack_p (cstack)
	  && BC_implementation_fun (ER_block_node (cstack)) != NULL)
	return TRUE;
    }
  else
    {
      ER_node_t stack;
      
      for (stack = cstack;;)
	{
	  stack = ER_prev_stack (stack);
	  if (stack == NULL
	      || BC_NODE_MODE (ER_block_node (stack)) == BC_NM_fblock)
	    break;
	}
      /* It is a return to C-code.  */
      if (stack != NULL && ER_c_stack_p (stack)
	  && BC_implementation_fun (ER_block_node (stack)) != NULL)
	return TRUE;
    }
  /* It might be a return from chain of tail calls originated from
     C-code.  */
  return  (ER_c_stack_p (from_stack)
	   && BC_implementation_fun (ER_block_node (from_stack)) == NULL);
}

static int do_always_inline
fblock_end (BC_node_t bc_node)
{
  ER_node_t stack;

  if (BC_fun_p (bc_node))
    ER_SET_MODE (IVAL (ER_ctop (ER_prev_stack (cstack)), 1),
		 ER_NM_undef);
  else if (BC_thread_p (bc_node))
    {
      delete_cprocess ();
      return FALSE;
    }
  else if (BC_class_p (bc_node))
    {
      ER_node_t res = IVAL (ER_ctop (ER_prev_stack (cstack)), 1);
	      
      ER_SET_MODE (res, ER_NM_stack);
      ER_set_stack (res, cstack);
    }
  else
    d_unreachable ();
  stack = cstack;
  heap_pop ();
  return evaluate_exit_p (stack);
  /* Do not put INTERRUPT here as the result is not on the top and
     possible GC called directly (or indirectly through thread
     switching) from INTERRUPT can make the result wrong.  Another
     solution could be adding a node to pop result as INTERRUPT should
     be the last statement executed for the node.  */
}

static int do_always_inline
leave (void)
{
  BC_node_t bc_node;

  for (;;)
    {
      bc_node = ER_block_node (cstack);
      sync_flag = BC_saved_sync_p (bc_node);
      if (BC_NODE_MODE (bc_node) == BC_NM_fblock)
	return fblock_end (bc_node);
      d_assert (BC_NODE_MODE (bc_node) == BC_NM_block
		&& cstack != uppest_stack);
      d_assert (BC_vars_num (bc_node) >= 0);
      heap_pop ();
    }
}

static int do_always_inline
fbend (void)
{
  /* We should never reach the node.  */
  assert (FALSE);
}

static int do_always_inline
ret (ER_node_t res)
{
  BC_node_t bc_node;
  ER_node_t stack;

  for (;;)
    {
      bc_node = ER_block_node (cstack);
      sync_flag = BC_saved_sync_p (bc_node);
      if (BC_NODE_MODE (bc_node) == BC_NM_fblock)
	{
	  d_assert (BC_fun_p (bc_node));
	  *(val_t *) IVAL (ER_ctop (ER_prev_stack (cstack)), 1)
	    = *(val_t *) res;
	  stack = cstack;
	  heap_pop ();
	  return evaluate_exit_p (stack);
	}
      heap_pop ();
      d_assert (cstack != NULL);
    }
  /* See comment for fbend.  */
}

static void do_always_inline
wait (ER_node_t op1)
{
  int true_p;
  
  if (sync_flag)
    eval_error (syncwait_bc_decl, get_cpos (), DERR_wait_in_sync_stmt);
  true_p = non_zero_p (op1, DERR_invalid_wait_guard_expr_type);
  if (! true_p)
    block_cprocess (BC_pc (cpc), TRUE);
  else
    {
      INCREMENT_PC ();
      sync_flag = TRUE;
    }
}

static void do_always_inline
waitend (void)
{
  sync_flag = FALSE;
}

static void do_always_inline
stinc (ER_node_t op1, ER_node_t op2, int op3i)
{
  reset_vars (op2, IVAL (op1, op3i));
  ctop = IVAL (op1, op3i - 1);
}

static void do_always_inline
stpop (int op1i, int op2i)
{
  ctop = IVAL (ctop, -op1i);
}

static void do_always_inline
stdecu (ER_node_t op1, int op2i)
{
  ER_SET_MODE (op1, ER_NM_undef);
  /* ??? To conservative until next call: some temporaries are
     saved.  */
  ctop = IVAL (op1, -1);
}

static void do_always_inline
stdecm (ER_node_t op1, ER_node_t op2)
{
  *(val_t *) op1 = *(val_t *) op2;
  /* ??? Too conservative until next call: some temporaries are
     saved.  */
  ctop = IVAL (op1, -1);
}

static void do_always_inline
block (void)
{
  d_assert (BC_vars_num (cpc) >= 0);
  heap_push (cpc, cstack, 0);
  BC_set_saved_sync_p (cpc, sync_flag);
}

extern int throw (ER_node_t op1);

static int do_always_inline
foreach (ER_node_t tv, ER_node_t start, ER_node_t key)
{
  ER_node_t tab;
  val_t *k;

  if (ER_NODE_MODE (tv) != ER_NM_tab)
    eval_error (keyop_bc_decl, get_cpos (),
		DERR_in_table_operand_type);
  tab = ER_tab (tv);
  GO_THROUGH_REDIR (tab);
  k = (val_t *) find_next_key (tab, ER_i (start));
  if (k != NULL)
    {
      ER_set_i (start, ER_i (start) + 1);
      *(val_t *) key = *(val_t *) k;
      return TRUE;
    }
  return FALSE;
}

static int do_always_inline
foreach2 (ER_node_t tv, ER_node_t start, ER_node_t container, ER_node_t element)
{
  ER_node_t tab;
  val_t *k;

  if (ER_NODE_MODE (tv) != ER_NM_tab)
    eval_error (keyop_bc_decl, get_cpos (),
		DERR_in_table_operand_type);
  tab = ER_tab (tv);
  GO_THROUGH_REDIR (tab);
  k = (val_t *) find_next_key (tab, ER_i (start));
  if (k != NULL)
    {
      ER_set_i (start, ER_i (start) + 1);
      store_designator_value (container, element, (ER_node_t) k);
      return TRUE;
    }
  return FALSE;
}

static int except (void);

static int do_always_inline
chvec (ER_node_t op1, pc_t target)
{
  if (expect (ER_NODE_MODE (op1) == ER_NM_vect))
    return FALSE;
  if (target == NULL)
    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_vector_pattern_match);
  return TRUE;
}

static int do_always_inline
chvend (ER_node_t op1, ER_node_t op2, pc_t target)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_vect && ER_NODE_MODE (op2) == ER_NM_int);
  if (ER_i (op2) == ER_els_number (ER_vect (op1)))
    return FALSE;
  if (target == NULL)
    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_vector_pattern_match);
  return TRUE;
}

static int do_always_inline
chvlen (ER_node_t op1, ER_node_t op2, ER_node_t op3, pc_t target)
{
  rint_t i;
  val_t tvar;
  
  d_assert (ER_NODE_MODE (op1) == ER_NM_vect && ER_NODE_MODE (op2) == ER_NM_int);
  if (ER_NODE_MODE (op3) != ER_NM_int)
    {
      op3 = implicit_int_conversion (op3, (ER_node_t) &tvar);
      if (doubt (ER_NODE_MODE (op3) != ER_NM_int))
	eval_error (optype_bc_decl, get_cpos (),
		    DERR_elist_repetition_type);
    }
  i = ER_i (op3);
  if (i <= 0)
    return FALSE;
  i += ER_i (op2);
  if (i <= ER_els_number (ER_vect (op1)))
    {
      ER_set_i (op2, i);
      return FALSE;
    }
  if (target == NULL)
    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_vector_pattern_match);
  return TRUE;
}

extern void load_packed_vector_element (ER_node_t, ER_node_t, rint_t);

static int do_always_inline
chvel (ER_node_t op1, ER_node_t op2, ER_node_t op3, ER_node_t op4,
       int op5n, pc_t target)
{
  rint_t i, rep, start, bound;
  val_t tvar;
  ER_node_t vect;
  int pack_flag;
  
  d_assert (ER_NODE_MODE (op1) == ER_NM_vect && ER_NODE_MODE (op2) == ER_NM_int);
  if (ER_NODE_MODE (op3) != ER_NM_int)
    {
      op3 = implicit_int_conversion (op3, (ER_node_t) &tvar);
      if (doubt (ER_NODE_MODE (op3) != ER_NM_int))
	eval_error (optype_bc_decl, get_cpos (), DERR_elist_repetition_type);
    }
  rep = ER_i (op3);
  if (rep <= 0)
    return FALSE;
  start = ER_i (op2);
  bound = rep + start;
  vect = ER_vect (op1);
  GO_THROUGH_REDIR (vect);
  pack_flag = ER_NODE_MODE (vect) == ER_NM_heap_pack_vect;
  d_assert (pack_flag || ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect);
  if (bound <= ER_els_number (vect))
    {
      if (op5n)
	{
	  if (pack_flag)
	    load_packed_vector_element (op4, vect, start);
	  else
	    *(val_t *) op4 = *(val_t *) IVAL (ER_unpack_els (vect), start);
	}
      for (i = start + (op5n ? 1 : 0); i < bound; i++)
	{
	  if (pack_flag)
	    load_packed_vector_element ((ER_node_t) &tvar, vect, i);
	  else
	    tvar = *(val_t *) IVAL (ER_unpack_els (vect), i);
	  if (! common_eq_ne_op (BC_NM_eq, op4, (ER_node_t) &tvar))
	    break;
	}
      if (i == bound)
	{
	  ER_set_i (op2, bound);
	  return FALSE;
	}
    }
  if (target == NULL)
    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_vector_pattern_match);
  return TRUE;
}

static int do_always_inline
chtab (ER_node_t op1, pc_t target)
{
  if (expect (ER_NODE_MODE (op1) == ER_NM_tab))
    return FALSE;
  if (target == NULL)
    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_table_pattern_match);
  return TRUE;
}

static int do_always_inline
chtend (ER_node_t op1, int op2i, pc_t target)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_tab);
  if (op2i == ER_els_number (ER_tab (op1)))
    return FALSE;
  if (target == NULL)
    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_table_pattern_match);
  return TRUE;
}

static int do_always_inline
chtel (ER_node_t op1, ER_node_t op2, ER_node_t op3, int op4n, pc_t target)
{
  val_t tvar;
  ER_node_t tab;
  ER_node_t entry;

  d_assert (ER_NODE_MODE (op1) == ER_NM_tab);
  tab = ER_tab (op1);
  GO_THROUGH_REDIR (tab);
  entry = find_tab_el (tab, op2, FALSE);
  if (entry == NULL || ER_NODE_MODE (entry) == ER_NM_empty_el)
    {
      if (target == NULL)
	eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_table_pattern_match);
      return TRUE;
    }
  else if (op4n == 2)
    return FALSE;
  if (op4n)
    *(val_t *) op3 = *(val_t *) INDEXED_EL_VAL (entry, 0);
  else
    {
      tvar = *(val_t *) INDEXED_EL_VAL (entry, 0);
      if (! common_eq_ne_op (BC_NM_eq, op3, (ER_node_t) &tvar))
	{
	  if (target == NULL)
	    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_table_pattern_match);
	  return TRUE;
	}
    }
  return FALSE;
}

static int do_always_inline
chst (ER_node_t op1, ER_node_t op2, pc_t target)
{
  BC_node_t code, code_2;

  if (expect (ER_NODE_MODE (op1) == ER_NM_stack) && ER_NODE_MODE (op2) == ER_NM_code)
    {
      code = ER_block_node (ER_stack (op1));
      code_2 = ID_TO_CODE (ER_code_id (op2));
      if (code_use_p (code, code_2))
	return FALSE;
    }
  if (target == NULL)
    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_stack_pattern_match);
  return TRUE;
}

static int do_always_inline
chstend (ER_node_t op1, int op2n, pc_t target)
{
  BC_node_t code;
  
  d_assert (ER_NODE_MODE (op1) == ER_NM_code);
  code = ID_TO_CODE (ER_code_id (op1));
  if (BC_NODE_MODE (code) == BC_NM_fblock)
    {
      if (op2n == BC_pars_num (code))
	return FALSE;
    }
  else if (op2n == 0)
    return FALSE;
  if (target == NULL)
    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_stack_pattern_match);
  return TRUE;
}

static int do_always_inline
chstel (ER_node_t op1, ER_node_t op2, ER_node_t op3, int op4n, int op5n,
	pc_t target)
{
  val_t tvar;
  ER_node_t stack;
  BC_node_t code;

  d_assert (ER_NODE_MODE (op1) == ER_NM_stack
	    && ER_NODE_MODE (op2) == ER_NM_code);
  stack = ER_stack (op1);
  code = ID_TO_CODE (ER_code_id (op2));
  if (BC_NODE_MODE (code) != BC_NM_fblock || op4n >= BC_pars_num (code))
    {
      if (target == NULL)
	eval_error (patternmatch_bc_decl, get_cpos (),
		    DERR_wrong_stack_pattern_match);
      return TRUE;
    }
  else if (op5n == 2)
    return FALSE;
  if (op5n)
    *(val_t *) op3 = *(val_t *) get_var (ER_stack_vars (stack), op4n);
  else
    {
      tvar = *(val_t *) get_var (ER_stack_vars (stack), op4n);
      if (! common_eq_ne_op (BC_NM_eq, op3, (ER_node_t) &tvar))
	{
	  if (target == NULL)
	    eval_error (patternmatch_bc_decl, get_cpos (), DERR_wrong_stack_pattern_match);
	  return TRUE;
	}
    }
  return FALSE;
}

static void do_always_inline
move (ER_node_t res, ER_node_t op1)
{
  if (ER_NODE_MODE (op1) == ER_NM_undef)
    {
      if (op1 < cvars || op1 > ctop)
	eval_error (accessop_bc_decl, get_cpos (),
		    DERR_undefined_value_assign);
      else
	eval_error (accessop_bc_decl, get_cpos (),
		    DERR_undefined_value_access,
		    BC_ident (BC_move_decl (cpc)));
    }
  *(val_t *) res = *(val_t *) op1;
}

static void do_always_inline
imove (ER_node_t res, ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_int);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, ER_i (op1));
}

static void do_always_inline
fmove (ER_node_t res, ER_node_t op1)
{
  d_assert (ER_NODE_MODE (op1) == ER_NM_float);
  ER_SET_MODE (res, ER_NM_float);
  ER_set_f (res, ER_f (op1));
}

static void do_always_inline
var (ER_node_t res)
{
  process_var_val (res, BC_decl (cpc));
}

static void do_always_inline
lvar (ER_node_t res)
{
  process_var_ref_and_val (res, BC_decl (cpc), FALSE);
}

static void do_always_inline
lvarv (ER_node_t res)
{
  process_var_ref_and_val (res, BC_decl (cpc), TRUE);
}

static void do_always_inline
evar (ER_node_t res)
{
  process_external_var (res, BC_decl (cpc), FALSE, FALSE);
}

static void do_always_inline
levar (ER_node_t res)
{
  process_external_var (res, BC_decl (cpc), TRUE, FALSE);
}

static void do_always_inline
levarv (ER_node_t res)
{
  process_external_var (res, BC_decl (cpc), TRUE, TRUE);
}

static void do_always_inline
efun (ER_node_t res)
{
  BC_node_t decl = BC_decl (cpc);
	    
  ER_SET_MODE (res, ER_NM_efun);
  ER_set_efdecl (res, decl);
}

static void do_always_inline
funclass (ER_node_t res)
{
  BC_node_t fblock, decl = BC_decl (cpc);
	    
  d_assert (BC_NODE_MODE (decl) == BC_NM_fdecl);
  ER_SET_MODE (res, ER_NM_code);
  fblock = BC_fblock (decl);
  if (BC_forward_p (fblock))
    eval_error (accessvalue_bc_decl, get_cpos (),
		DERR_undefined_class_or_fun, BC_ident (decl));
  ER_set_code_id (res, CODE_ID (fblock));
  ER_set_code_context
    (res, find_context_by_scope (BC_decl_scope (decl)));
}

static void do_always_inline
rpr (ER_node_t res)
{
  repl_print (res, FALSE);
}

static void do_always_inline
rpr_def (ER_node_t res)
{
  repl_print (res, TRUE);
}

static void do_always_inline
nop (void)
{
}

static void do_always_inline
set_c_code_p (void)
{
  ER_set_c_stack_p (cstack, TRUE);
}
