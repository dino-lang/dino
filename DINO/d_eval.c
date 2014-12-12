/*
   Copyright (C) 1997-2014 Vladimir Makarov.

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

#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include "d_common.h"
#include "d_conv.h"
#include "d_func.h"
#include "d_run.h"
#include "d_eval.h"
#include "d_gen.c"

/* The following variable value is TRUE if we've started evaluation
   and set up longjump buffer `eval_longjump_buff'. */
int eval_long_jump_set_flag;
/* The following variable is used for throwing errors. */
static jmp_buf eval_longjump_buff;

static void do_always_inline
extract_op1 (ER_node_t *op1)
{
  *op1 = get_op (BC_op1 (cpc));
}

static void do_always_inline
extract_op2 (ER_node_t *op1, ER_node_t *op2)
{
  extract_op1 (op1);
  *op2 = get_op (BC_op2 (cpc));
}

static void do_always_inline
extract_op3 (ER_node_t *op1, ER_node_t *op2, ER_node_t *op3)
{
  extract_op2 (op1, op2);
  *op3 = get_op (BC_op3 (cpc));
}

ER_node_t do_inline
find_context_by_scope (BC_node_t scope)
{
  ER_node_t container;
  BC_node_t curr_scope;

#ifndef NO_CONTAINER_CACHE
  if (BC_cached_container_tick (scope) == current_cached_container_tick)
    {
      container = (ER_node_t) BC_cached_container_address (scope);
      return container;
    }
#endif
  for (container = cstack, curr_scope = ER_block_node (container);
       scope != curr_scope;
       container = ER_context (container),
	 curr_scope = ER_block_node (container))
    ;
#ifndef NO_CONTAINER_CACHE
  BC_set_cached_container_address (scope, (string_t) container);
  BC_set_cached_container_tick (scope, current_cached_container_tick);
#endif
  return container;
}

void do_inline
process_var_ref_and_val (ER_node_t res, BC_node_t vdecl, int val_too_p)
{
  BC_node_t scope = BC_decl_scope (vdecl);
  int var_number_in_block = BC_var_num (vdecl);
  ER_node_t container;

  container = find_context_by_scope (scope);
  d_assert (ER_NODE_MODE (container) == ER_NM_heap_stack);
  ER_SET_MODE (res, ER_NM_stack);
  ER_set_stack (res, container);
  ER_SET_MODE (IVAL (res, 1), ER_NM_int);
  ER_set_i (IVAL (res, 1), var_number_in_block);
  if (val_too_p)
    *(val_t *) IVAL (res, 2) = *(val_t *) IVAL (ER_stack_vars (container),
						var_number_in_block);
}

static ER_node_t do_inline
get_var_val_ref (BC_node_t vdecl)
{
  int var_number_in_block = BC_var_num (vdecl);

  BC_node_t scope = BC_decl_scope (vdecl);
  ER_node_t container;

  container = find_context_by_scope (scope);
  return IVAL (ER_stack_vars (container), var_number_in_block);
}

void do_inline
process_var_val (ER_node_t res, BC_node_t vdecl)
{
  ER_node_t ref;

  ref = get_var_val_ref (vdecl);
  if (ER_NODE_MODE (ref) == ER_NM_undef)
    eval_error (accessop_bc_decl, get_cpos (), DERR_undefined_value_access,
		BC_ident (vdecl));
  *(val_t *) res = *(val_t *) ref;
}

void do_inline
process_external_var (ER_node_t res, BC_node_t evdecl,
		      int lvalue_p, int val_too_p)
{
  void *addr;
  
  addr = external_address (evdecl);
  if (lvalue_p)
    {
      ER_SET_MODE (res, ER_NM_external_var_ref);
      ER_set_external_var_ref (res, addr);
      ER_SET_MODE (IVAL (res, 1), ER_NM_nil);
      if (val_too_p)
	{
	  if (ER_NODE_MODE ((ER_node_t) addr) == ER_NM_undef)
	    eval_error (accessop_bc_decl, get_cpos (),
			DERR_undefined_value_access, BC_ident (evdecl));
	  *(val_t *) IVAL (res, 2) = *(val_t *) addr;
	}
    }
  else if (ER_NODE_MODE ((ER_node_t) addr) == ER_NM_undef)
    eval_error (accessop_bc_decl, get_cpos (), DERR_undefined_value_access,
		BC_ident (evdecl));
  else
    *(val_t *) res = *(val_t *) addr;
}

position_t
get_designator_pos (void)
{
  BC_node_t info = BC_info (cpc);

  if (BC_IS_OF_TYPE (info, BC_NM_source2))
    return BC_pos2 (info);
  else
    return BC_pos (info);
}

static void do_inline
check_member_access (BC_node_t decl, BC_node_t fblock)
{
  if (!BC_public_p (decl))
    {
      BC_node_t curr_block, friend;

      /* ??? Is it worth to make it more effective (e.g. bit tables). */
      for (curr_block = ER_block_node (cstack);
	   curr_block != NULL;
	   curr_block = BC_scope (curr_block))
	if (curr_block == fblock)
	  break;
	else
	  {
	    for (friend = BC_friends (fblock);
		 friend != NULL;
		 friend = BC_next_friend (friend))
	      if (BC_friend (friend) == curr_block)
		break;
	    if (friend != NULL)
	      break;
	  }
      if (curr_block == NULL)
	eval_error (accessop_bc_decl, get_designator_pos (),
		    DERR_private_decl_access_from_outside_block,
		    BC_ident (decl));
    }
}

/* Find and return decl with BLOCK_DECL_IDENT_NUMBER starting with
   *CONTAINER.  Pass the container of the found decl through
   CONTAINER.  */
BC_node_t do_inline
find_field (int block_decl_ident_number, ER_node_t *container)
{
  ER_node_t curr_container;
  BC_node_t block, decl = NULL;

  for (curr_container = *container;;)
    {
      d_assert (curr_container != NULL);
      block = ER_block_node (curr_container);
      if (block_decl_ident_number >= 0)
	decl = LV_BLOCK_IDENT_DECL (block, block_decl_ident_number);
      if (decl != NULL
	  || BC_NODE_MODE (block) != BC_NM_fblock || ! BC_class_p (block))
	break;
      curr_container = ER_context (curr_container);
      if (BC_NODE_MODE (ER_block_node (curr_container)) != BC_NM_fblock
	  || ! BC_class_p (ER_block_node (curr_container)))
	break;
    }
  if (decl == NULL)
    eval_error (accessop_bc_decl, get_cpos (),
		DERR_decl_is_absent_in_given_class_or_block);
  else
    check_member_access (decl, block);
  *container = curr_container;
  return decl;
}

void do_inline
execute_general_period_operation (int block_decl_ident_number, ER_node_t res,
				  ER_node_t op, int lvalue_p, int lvalue_val_p)
{
  BC_node_t decl;
  ER_node_t container, val;

  if (ER_NODE_MODE (op) != ER_NM_stack)
    eval_error (accessop_bc_decl, get_cpos (),
		DERR_value_is_not_class_instance_or_stack);
  container = ER_stack (op);
  decl = find_field (block_decl_ident_number, &container);
  switch ((unsigned char) BC_NODE_MODE (decl))
    {
#ifdef __GNUC__
	  /* This is a trick to make switch faster by removing range
	     comparison.  */
    case 0:
    case UCHAR_MAX:
      abort ();
#endif
    case BC_NM_vdecl:
      val = NULL;
      if (! lvalue_p)
	val = res;
      else
	{
	  ER_SET_MODE (res, ER_NM_stack);
	  ER_set_stack (res, container);
	  ER_SET_MODE (IVAL (res, 1), ER_NM_int);
	  ER_set_i (IVAL (res, 1), BC_var_num (decl));
	  if (lvalue_val_p)
	    val = IVAL (res, 2);
	}
      if (val != NULL)
	{
	  ER_node_t ref = IVAL (ER_stack_vars (container), BC_var_num (decl));
	  
	  if (ER_NODE_MODE (ref) == ER_NM_undef)
	    eval_error (accessop_bc_decl, get_cpos (),
			DERR_undefined_value_access, BC_ident (decl));
	  *(val_t *) val = *(val_t *) ref;
	}
      BC_set_hint (cpc, decl);
      break;
    case BC_NM_evdecl:
      process_external_var (res, decl, lvalue_p, lvalue_val_p);
      break;
    case BC_NM_fdecl:
      {
	BC_node_t fblock = BC_fblock (decl);

	if (BC_forward_p (fblock))
	  eval_error (accessvalue_bc_decl, get_cpos (),
		      DERR_undefined_class_or_fun, BC_ident (decl));
	if (lvalue_p)
	  eval_error
	    (accessop_bc_decl, get_cpos (),
	     BC_NODE_MODE (fblock) == BC_NM_fblock && BC_class_p (fblock)
	     ? DERR_class_as_variable : DERR_fun_as_variable);
	ER_SET_MODE (res, ER_NM_code);
	ER_set_code_context (res, container);
	ER_set_code_id (res, CODE_ID (fblock));
        BC_set_hint (cpc, decl);
      }
      break;
    case BC_NM_efdecl:
      if (lvalue_p)
	eval_error (accessop_bc_decl, get_cpos (), DERR_fun_as_variable);
      ER_SET_MODE (res, ER_NM_efun);
      ER_set_efdecl (res, decl);
      break;
    default:
      d_unreachable ();
    }
}

/* Load element INDEX_VAL of packed VECT into TO.  */
static void do_inline
load_packed_vector_element (ER_node_t to, ER_node_t vect, rint_t index_val)
{
  ER_node_t t;
  ER_node_mode_t el_type;
  size_t el_type_size;

  d_assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect);
  el_type = ER_pack_vect_el_mode (vect);
  ER_SET_MODE (to, el_type);
  switch ((unsigned char) el_type)
    {
#ifdef __GNUC__
	  /* This is a trick to make switch faster by removing range
	     comparison.  */
    case 0:
    case 255:
      abort ();
#endif
    case ER_NM_nil:
      break;
    case ER_NM_hide:
      ER_set_hide (to, ((hide_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_char:
      ER_set_ch (to, ((char_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_int:
      ER_set_i (to, ((rint_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_float:
      ER_set_f (to, ((rfloat_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_long:
      ER_set_l (to, ((ER_node_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_type:
      ER_set_type (to, ((ER_node_mode_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_vect:
      t = ((ER_node_t *) ER_pack_els (vect)) [index_val];
      GO_THROUGH_REDIR (t);
      set_vect_dim (to, t, 0);
      break;
    case ER_NM_tab:
      t = ((ER_node_t *) ER_pack_els (vect)) [index_val];
      GO_THROUGH_REDIR (t);
      ER_set_tab (to, t);
      break;
    case ER_NM_process:
      ER_set_process
	(to, ((ER_node_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_stack:
      ER_set_stack (to, ((ER_node_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_code:
      el_type_size = type_size_table [el_type];
      memcpy ((char *) to + val_displ_table [ER_NODE_MODE (to)],
	      ER_pack_els (vect) + index_val * el_type_size,
	      el_type_size);
      break;
    default:
      d_unreachable ();
    }
}

void do_inline
load_vector_element_by_index (ER_node_t to, ER_node_t vect, ER_node_t index)
{
  rint_t index_val;
  int pack_flag;

  GO_THROUGH_REDIR (vect);
  pack_flag = ER_NODE_MODE (vect) == ER_NM_heap_pack_vect;
  d_assert (pack_flag || ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect);
  index_val = check_vector_index (vect, index);
  if (pack_flag)
    load_packed_vector_element (to, vect, index_val);
  else
    *(val_t *) to = *(val_t *) IVAL (ER_unpack_els (vect), index_val);
}

/* Store VAL into element INDEX_VAL of packed VECT.  */
static void do_inline
store_packed_vector_element (ER_node_t vect, rint_t index_val, ER_node_t val)
{
  ER_node_mode_t el_type;
  size_t el_type_size;
  
  d_assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect);
  el_type = ER_pack_vect_el_mode (vect);
  d_assert (ER_NODE_MODE (val) == el_type);
  switch ((unsigned char) el_type)
    {
#ifdef __GNUC__
      /* This is a trick to make switch faster by removing range
	 comparison.  */
    case 0:
    case 255:
      abort ();
#endif
    case ER_NM_nil:
      break;
    case ER_NM_hide:
      ((hide_t *) ER_pack_els (vect)) [index_val] = ER_hide (val);
      break;
    case ER_NM_char:
      ((char_t *) ER_pack_els (vect)) [index_val] = ER_ch (val);
      break;
    case ER_NM_int:
      ((rint_t *) ER_pack_els (vect)) [index_val] = ER_i (val);
      break;
    case ER_NM_float:
      ((rfloat_t *) ER_pack_els (vect)) [index_val] = ER_f (val);
      break;
    case ER_NM_long:
      ((ER_node_t *) ER_pack_els (vect)) [index_val] = ER_l (val);
      break;
    case ER_NM_type:
      ((ER_node_mode_t *) ER_pack_els (vect)) [index_val] = ER_type (val);
      break;
    case ER_NM_vect:
      set_packed_vect_el (vect, index_val, ER_vect (val));
      break;
    case ER_NM_tab:
      ((ER_node_t *) ER_pack_els (vect)) [index_val] = ER_tab (val);
      break;
    case ER_NM_process:
      ((ER_node_t *) ER_pack_els (vect)) [index_val] = ER_process (val);
      break;
    case ER_NM_stack:
      ((ER_node_t *) ER_pack_els (vect)) [index_val] = ER_stack (val);
      break;
    case ER_NM_code:
      el_type_size = type_size_table [el_type];
      memcpy (ER_pack_els (vect) + index_val * el_type_size,
	      (char *) val + val_displ_table [ER_NODE_MODE (val)],
	      el_type_size);
      break;
    default:
      d_unreachable ();
    }
}

static void do_inline
store_vector_element (ER_node_t vect, ER_node_t index, ER_node_t val)
{
  rint_t index_val;
  int pack_flag;

  GO_THROUGH_REDIR (vect);
  pack_flag = ER_NODE_MODE (vect) == ER_NM_heap_pack_vect;
  if (ER_immutable (vect))
    eval_error (immutable_bc_decl, get_designator_pos (),
		DERR_immutable_vector_modification);
  index_val = check_vector_index (vect, index);
  if (pack_flag && ER_pack_vect_el_mode (vect) != ER_NODE_MODE (val))
    {
      vect = unpack_vector (vect);
      pack_flag = FALSE;
    }
  if (pack_flag)
    store_packed_vector_element (vect, index_val, val);
  else
    *(val_t *) IVAL (ER_unpack_els (vect), index_val) = *(val_t *)val;
}

void do_inline
load_table_element_by_key (ER_node_t to, ER_node_t tab, ER_node_t key)
{
  ER_node_t entry;

  GO_THROUGH_REDIR (tab);
  entry = find_tab_el (tab, key, FALSE);
  if (entry == NULL || ER_NODE_MODE (entry) == ER_NM_empty_el)
    eval_error (keyvalue_bc_decl, get_cpos (), DERR_no_such_key);
  *(val_t *) to = *(val_t *) INDEXED_EL_VAL (entry, 0);
  d_assert (ER_IS_OF_TYPE (INDEXED_EL_VAL (entry, 0), ER_NM_val));
}

static void do_inline
store_table_element (ER_node_t tab, ER_node_t index, ER_node_t val)
{
  ER_node_t entry;

  GO_THROUGH_REDIR (tab);
  if (ER_immutable (tab))
    eval_error (immutable_bc_decl, get_designator_pos (),
		DERR_immutable_table_modification);
  entry = find_tab_el (tab, index, TRUE);
  d_assert (entry != NULL);
  *(val_t *) entry = *(val_t *) index;
  make_immutable (entry);
  *(val_t *) INDEXED_EL_VAL (entry, 0) = *(val_t *) val;
  d_assert (ER_IS_OF_TYPE (INDEXED_EL_VAL (entry, 0), ER_NM_val));
}

static void do_inline
store_stack_element (ER_node_t stack, ER_node_t index, ER_node_t val)
{
  if (ER_immutable (stack))
    eval_error (immutable_bc_decl,
		get_designator_pos (), DERR_immutable_instance_modification);
  *(val_t *) IVAL (ER_stack_vars (stack), ER_i (index)) = *(val_t *) val;
}

/* Used only for op store when check is done for lvalue value.  */
void do_inline
store_designator_value (ER_node_t container, ER_node_t index, ER_node_t val)
{
  if (ER_NODE_MODE (container) == ER_NM_stack)
    store_stack_element (ER_stack (container), index, val);
  else if (ER_NODE_MODE (container) == ER_NM_vect)
    store_vector_element (ER_vect (container), index, val);
  else if (ER_NODE_MODE (container) == ER_NM_tab)
    store_table_element (ER_tab (container), index, val);
  else if (ER_NODE_MODE (container) == ER_NM_external_var_ref)
    *(val_t *) ER_external_var_ref (container) = *(val_t *) val;
  else /* The check is done for lvalue value.  */
    d_unreachable ();
}

void do_inline
store_vect_tab_designator_value (ER_node_t vec_tab, ER_node_t index, ER_node_t val)
{
  if (ER_NODE_MODE (vec_tab) == ER_NM_vect)
    store_vector_element (ER_vect (vec_tab), index, val);
  else if (ER_NODE_MODE (vec_tab) == ER_NM_tab)
    store_table_element (ER_tab (vec_tab), index, val);
  else
    eval_error (indexop_bc_decl, get_designator_pos (),
		DERR_index_operation_for_non_vec_tab);
}

void do_inline
store_stack_designator_value (ER_node_t stack, ER_node_t index, ER_node_t val)
{
  if (ER_NODE_MODE (stack) != ER_NM_stack)
    eval_error (accessop_bc_decl, get_designator_pos (),
		DERR_value_is_not_class_instance_or_stack);
  store_stack_element (ER_stack (stack), index, val);
}

/* Return and check slice characteristics (start, bound, step and
   result len) of a slice on DEPTH whose start starts with
   START_VAL.  */
static void do_inline
check_and_get_slice_info (ER_node_t start_val, unsigned_rint_t vec_len,
			  int depth, rint_t *start, rint_t *bound,
			  rint_t *step, size_t *niter)
{
  rint_t start0, bound0, step0, niter0, abs_step;

  if (ER_NODE_MODE (IVAL (start_val, 0)) != ER_NM_int)
    eval_error (slicetype_bc_decl, get_designator_pos (),
		DERR_slice_start_is_not_int, depth);
  if (ER_NODE_MODE (IVAL (start_val, 1)) != ER_NM_int)
    eval_error (slicetype_bc_decl, get_designator_pos (),
		DERR_slice_bound_is_not_int, depth);
  if (ER_NODE_MODE (IVAL (start_val, 2)) != ER_NM_int)
    eval_error (slicetype_bc_decl, get_designator_pos (),
		DERR_slice_step_is_not_int, depth);
  start0 = ER_i (IVAL (start_val, 0));
  bound0 = ER_i (IVAL (start_val, 1));
  step0 = ER_i (IVAL (start_val, 2));
  if (start0 < 0)
    ;
  if (step0 == 0)
    ;
  if (bound0 < 0)
    {
      bound0 = vec_len + bound0 + 1;
      if (bound0 < 0)
	bound0 = 0;
    }
  else if (bound0 > vec_len)
    bound0 = vec_len;
  abs_step = (step0 > 0 ? step0 : -step0);
  niter0 = (bound0 - start0 + abs_step - 1) / abs_step;
  if (step0 > 0)
    bound0 = start0 + niter0 * abs_step;
  else
    {
      bound0 = start0 + step0;
      start0 = bound0 - niter0 * step0;
    }
  *start = start0; *bound = bound0; *step = step0; *niter = niter0;
}

/* Process slice extract to CONTAINER1 of dimension DIM1 (> 0) with
   slice start, bound, step placed on START_VAL1.  */
static ER_node_t
process_slice_extract (ER_node_t container1, ER_node_t start_val1, int dim1,
		       int depth)
{
  ER_node_t unpack_els, unpack_els1 = NULL;
  char *pack_els, *pack_els1 = NULL;
  rint_t len1, start1, bound1, step1;
  size_t niter1;
  rint_t i, i1;
  ER_node_t vect, vect1, v1;
  int pack_flag1 = FALSE;
  ER_node_mode_t el_type1;

  d_assert (dim1 > 0);
  vect1 = container1;
  GO_THROUGH_REDIR (vect1);
  if (ER_immutable (vect1))
    eval_error (immutable_bc_decl, get_designator_pos (),
		DERR_immutable_vector_modification);
  pack_flag1 = ER_NODE_MODE (vect1) == ER_NM_heap_pack_vect;
  len1 = ER_els_number (vect1);
  check_and_get_slice_info (start_val1, len1, depth,
			    &start1, &bound1, &step1, &niter1);
  if (dim1 == 1)
    {
      if (pack_flag1)
	{
	  pack_els1 = ER_pack_els (vect1);
	  el_type1 = ER_pack_vect_el_mode (vect1);
	  vect = create_pack_vector (niter1, el_type1);
	  pack_els = ER_pack_els (vect);
	  switch (el_type1)
	    {
	    case ER_NM_nil:
	      break;
	    case ER_NM_hide:
	      for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
		((hide_t *) pack_els) [i] = ((hide_t *) pack_els1) [i1];
	      break;
	    case ER_NM_char:
	      for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
		((char_t *) pack_els) [i] = ((char_t *) pack_els1) [i1];
	      break;
	    case ER_NM_int:
	      for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
		((rint_t *) pack_els) [i] = ((rint_t *) pack_els1) [i1];
	      break;
	    case ER_NM_float:
	      for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
		((rfloat_t *) pack_els) [i] = ((rfloat_t *) pack_els1) [i1];
	      break;
	    case ER_NM_type:
	      for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
		((ER_node_mode_t *) pack_els) [i]
		  = ((ER_node_mode_t *) pack_els1) [i1];
	      break;
	    case ER_NM_long:
	    case ER_NM_vect:
	    case ER_NM_tab:
	    case ER_NM_process:
	    case ER_NM_stack:
	      for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
		((ER_node_t *) pack_els) [i] = ((ER_node_t *) pack_els1) [i1];
	      break;
	    case ER_NM_code:
	      {
		size_t el_type_size1 = type_size_table [el_type1];

		for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
		  memcpy (pack_els + i * el_type_size1,
			  pack_els1 + i1 * el_type_size1, el_type_size1);
		break;
	      }
	    default:
	      d_unreachable ();
	    }
	}
      else
	{
	  vect = create_unpack_vector (niter1);
	  unpack_els = ER_unpack_els (vect);
	  unpack_els1 = ER_unpack_els (vect1);
	  for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
	    *(val_t *)IVAL (unpack_els, i) = *(val_t *)IVAL (unpack_els1, i1);
	}
    }
  else
    {
      ER_node_t p;

      d_assert (dim1 > 1);
      if (! pack_flag1)
	unpack_els1 = ER_unpack_els (vect1);
      else if (dim1 > 1 && ER_pack_vect_el_mode (vect1) != ER_NM_vect)
	eval_error (sliceform_bc_decl, get_designator_pos (),
		    DERR_slice_operand_form, depth);
      else
	pack_els1 = ER_pack_els (vect1);
      dim1--;
      vect = create_pack_vector (niter1, ER_NM_vect);
      ER_set_els_number (vect, 0);
      if (pack_flag1)
	{
	  for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
	    {
	      p = process_slice_extract (((ER_node_t *) pack_els1) [i1],
					 IVAL (start_val1, 3), dim1, depth + 1);
	      set_packed_vect_el (vect, i, p);
	    }
	}
      else
	{
	  for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
	    {
	      v1 = IVAL (unpack_els1, i1);
	      if (ER_NODE_MODE (v1) != ER_NM_vect)
		eval_error (sliceform_bc_decl, get_designator_pos (),
			    DERR_slice_operand_form, depth);
	      v1 = ER_vect (v1);
	      p = process_slice_extract (v1, IVAL (start_val1, 3), dim1,
					 depth + 1);
	      set_packed_vect_el (vect, i, p);
	    }
	}
      ER_set_els_number (vect, niter1);
    }
  return vect;
}

/* Extract slice starting with CONTAINER and of dimension DIM.  Put
   the result into RES.  */
void do_inline
slice_extract (ER_node_t res, ER_node_t container, int dim)
{
  ER_node_t vect;

  if (ER_NODE_MODE (container) != ER_NM_vect)
    eval_error (sliceform_bc_decl, get_designator_pos (),
		DERR_slice_operand_form, 1);
  vect = process_slice_extract (ER_vect (container),
				IVAL (container, 1), dim, 1);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, dim);
}

/* Process slice assign to CONTAINER1 of dimension DIM1 (> 0) with
   slice start, bound, step placed as START_VAL1.  CONTAINER2 is a
   whole vector of DIM2 (when DIM2 > 0) or the assigned value
   otherwise.  */
static void
process_slice_assign (ER_node_t container1, ER_node_t start_val1, int dim1,
		      ER_node_t container2, int dim2, int depth)
{
  ER_node_t unpack_els1 = NULL, unpack_els2 = NULL;
  char *pack_els1 = NULL, *pack_els2 = NULL;
  rint_t len1, start1, bound1, step1;
  size_t niter1;
  rint_t len2 = 0;
  size_t niter2 = 0;
  rint_t i1, i2;
  ER_node_t vect1, vect2 = NULL;
  int pack_flag1, pack_flag2 = FALSE;
  ER_node_t v1, v2;
  ER_node_mode_t el_type1;

  d_assert (dim1 > 0);
  vect1 = container1;
  GO_THROUGH_REDIR (vect1);
  if (ER_immutable (vect1))
    eval_error (immutable_bc_decl, get_designator_pos (),
		DERR_immutable_vector_modification);
  pack_flag1 = ER_NODE_MODE (vect1) == ER_NM_heap_pack_vect;
  len1 = ER_els_number (vect1);
  check_and_get_slice_info (start_val1, len1, depth,
			    &start1, &bound1, &step1, &niter1);
  if (dim2 != 0)
    {
      vect2 = container2;
      GO_THROUGH_REDIR (vect2);
      pack_flag2 = ER_NODE_MODE (vect2) == ER_NM_heap_pack_vect;
      niter2 = len2 = ER_els_number (vect2);
      
    }
  if (dim2 != 0 && niter1 != niter2)
    eval_error (optype_bc_decl, get_designator_pos (),
		DERR_different_slice_operand_lengths,
		(long) niter1, (long) niter2, depth);
  if (dim1 == 1 && dim2 == 0)
    {
      if (pack_flag1 && ER_pack_vect_el_mode (vect1) == ER_NODE_MODE (container2))
	{
	  pack_els1 = ER_pack_els (vect1);
	  el_type1 = ER_pack_vect_el_mode (vect1);
	  switch (el_type1)
	    {
	    case ER_NM_nil:
	      break;
	    case ER_NM_hide:
	      for (i1 = start1; i1 != bound1; i1 += step1)
		((hide_t *) pack_els1) [i1] = ER_hide (container2);
	      break;
	    case ER_NM_char:
	      for (i1 = start1; i1 != bound1; i1 += step1)
		((char_t *) pack_els1) [i1] = ER_ch (container2);
	      break;
	    case ER_NM_int:
	      for (i1 = start1; i1 != bound1; i1 += step1)
		((rint_t *) pack_els1) [i1] = ER_i (container2);
	      break;
	    case ER_NM_float:
	      for (i1 = start1; i1 != bound1; i1 += step1)
		((rfloat_t *) pack_els1) [i1] = ER_f (container2);
	      break;
	    case ER_NM_long:
	      for (i1 = start1; i1 != bound1; i1 += step1)
		((ER_node_t *) pack_els1) [i1] = ER_l (container2);
	      break;
	    case ER_NM_type:
	      for (i1 = start1; i1 != bound1; i1 += step1)
		((ER_node_mode_t *) pack_els1) [i1] = ER_type (container2);
	      break;
	    case ER_NM_vect:
	      v2 = ER_vect (container2);
	      goto node_common1;
	    case ER_NM_tab:
	      v2 = ER_tab (container2);
	      goto node_common1;
	    case ER_NM_process:
	      v2 = ER_process (container2);
	      goto node_common1;
	    case ER_NM_stack:
	      v2 = ER_process (container2);
	    node_common1:
	      for (i1 = start1; i1 != bound1; i1 += step1)
		((ER_node_t *) pack_els1) [i1] = v2;
	      break;
	    case ER_NM_code:
	      {
		size_t el_type_size1 = type_size_table [el_type1];
		char *v = ((char *) container2
			   + val_displ_table [ER_NODE_MODE (container2)]);
		
		for (i1 = start1; i1 != bound1; i1 += step1)
		  memcpy (pack_els1 + i1 * el_type_size1, v, el_type_size1);
		break;
	      }
	    default:
	      d_unreachable ();
	    }
	}
      else
	{
	  if (pack_flag1)
	    vect1 = unpack_vector (vect1);
	  unpack_els1 = ER_unpack_els (vect1);
	  for (i1 = start1; i1 != bound1; i1 += step1)
	    *(val_t *)IVAL (unpack_els1, i1) = *(val_t *)container2;
	}
    }
  else if (dim1 == 1 && dim2 >= 1)
    {
      if (pack_flag1 && ! pack_flag2)
	{
	  pack_flag1 = FALSE;
	  vect1 = unpack_vector (vect1);
	}
      else if (! pack_flag1 && pack_flag2)
	{
	  pack_flag2 = FALSE;
	  vect2 = unpack_vector (vect2);
	}
      if (pack_flag1
	  && ER_pack_vect_el_mode (vect1) != ER_pack_vect_el_mode (vect2))
	{
	  d_assert (pack_flag2);
	  pack_flag1 = pack_flag2 = FALSE;
	  vect1 = unpack_vector (vect1);
	  vect2 = unpack_vector (vect2);
	}
      if (pack_flag1)
	{
	  pack_els1 = ER_pack_els (vect1);
	  el_type1 = ER_pack_vect_el_mode (vect1);
	  pack_els2 = ER_pack_els (vect2);
	  d_assert (el_type1 == ER_pack_vect_el_mode (vect2));
	  switch (el_type1)
	    {
	    case ER_NM_nil:
	      break;
	    case ER_NM_hide:
	      for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		((hide_t *) pack_els1) [i1] = ((hide_t *) pack_els2) [i2];
	      break;
	    case ER_NM_char:
	      for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		((char_t *) pack_els1) [i1] = ((char_t *) pack_els2) [i2];
	      break;
	    case ER_NM_int:
	      for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		((rint_t *) pack_els1) [i1] = ((rint_t *) pack_els2) [i2];
	      break;
	    case ER_NM_float:
	      for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		((rfloat_t *) pack_els1) [i1]
		  = ((rfloat_t *) pack_els2) [i2];
	      break;
	    case ER_NM_long:
	      for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		((ER_node_t *) pack_els1) [i1] = ((ER_node_t *) pack_els2) [i2];
	      break;
	    case ER_NM_type:
	      for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		((ER_node_mode_t *) pack_els1) [i1]
		  = ((ER_node_mode_t *) pack_els2) [i2];
	      break;
	    case ER_NM_vect:
	    case ER_NM_tab:
	    case ER_NM_process:
	    case ER_NM_stack:
	      for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		((ER_node_t *) pack_els1) [i1] = ((ER_node_t *) pack_els2) [i2];
	      break;
	    case ER_NM_code:
	      {
		size_t el_type_size1 = type_size_table [el_type1];
		for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		  memcpy (pack_els1 + i1 * el_type_size1,
			  pack_els2 + i2 * el_type_size1, el_type_size1);
		break;
	      }
	    default:
	      d_unreachable ();
	    }
	}
      else
	{
	  d_assert (! pack_flag1 && ! pack_flag2);
	  unpack_els1 = ER_unpack_els (vect1);
	  unpack_els2 = ER_unpack_els (vect2);
	  for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
	    *(val_t *)IVAL (unpack_els1, i1) = *(val_t *)IVAL (unpack_els2, i2);
	  pack_vector_if_possible (vect1);
	}
    }
  else if (dim2 == 0)
    {
      d_assert (dim1 > 1);
      if (! pack_flag1)
	unpack_els1 = ER_unpack_els (vect1);
      else if (dim1 > 1 && ER_pack_vect_el_mode (vect1) != ER_NM_vect)
	eval_error (sliceform_bc_decl, get_designator_pos (),
		    DERR_slice_operand_form, depth);
      else
	pack_els1 = ER_pack_els (vect1);
      dim1--;
      if (pack_flag1)
	{
	  for (i1 = start1; i1 != bound1; i1 += step1)
	    process_slice_assign (((ER_node_t *) pack_els1) [i1],
				  IVAL (start_val1, 3), dim1, container2, 0,
				  depth + 1);
	}
      else
	{
	  for (i1 = start1; i1 != bound1; i1 += step1)
	    {
	      if (pack_flag1)
		v1 = ((ER_node_t *) pack_els1) [i1];
	      else
		{
		  v1 = IVAL (unpack_els1, i1);
		  if (ER_NODE_MODE (v1) != ER_NM_vect)
		    eval_error (sliceform_bc_decl, get_designator_pos (),
				DERR_slice_operand_form, depth);
		  v1 = ER_vect (v1);
		}
	      process_slice_assign (v1, IVAL (start_val1, 3), dim1,
				    container2, 0, depth + 1);
	    }
	}
    }
  else
    {
      d_assert (dim1 > 1 && dim2 > 0);
      if (! pack_flag1)
	unpack_els1 = ER_unpack_els (vect1);
      else if (dim1 > 1 && ER_pack_vect_el_mode (vect1) != ER_NM_vect)
	eval_error (sliceform_bc_decl, get_designator_pos (),
		    DERR_slice_operand_form, depth);
      else
	pack_els1 = ER_pack_els (vect1);
      if (! pack_flag2)
	unpack_els2 = ER_unpack_els (vect2);
      else if (dim2 > 1 && ER_pack_vect_el_mode (vect2) != ER_NM_vect)
	eval_error (sliceform_bc_decl, get_designator_pos (),
		    DERR_slice_operand_form, depth);
      else
	pack_els2 = ER_pack_els (vect2);
      dim1--;
      dim2--;
      if (pack_flag1 && pack_flag2)
	{
	  for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
	    process_slice_assign (((ER_node_t *) pack_els1) [i1],
				  IVAL (start_val1, 3), dim1,
				  ((ER_node_t *) pack_els2) [i2], dim2,
				  depth + 1);
	}
      else
	{
	  for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
	    {
	      if (pack_flag1)
		v1 = ((ER_node_t *) pack_els1) [i1];
	      else
		{
		  v1 = IVAL (unpack_els1, i1);
		  if (ER_NODE_MODE (v1) != ER_NM_vect)
		    eval_error (sliceform_bc_decl, get_designator_pos (),
				DERR_slice_operand_form, depth);
		  v1 = ER_vect (v1);
		}
	      if (pack_flag2)
		v2 = ((ER_node_t *) pack_els2) [i2];
	      else
		{
		  v2 = IVAL (unpack_els2, i2);
		  if (ER_NODE_MODE (v2) != ER_NM_vect)
		    eval_error (sliceform_bc_decl, get_designator_pos (),
				DERR_slice_operand_form, depth);
		  v2 = ER_vect (v2);
		}
	      process_slice_assign (v1, IVAL (start_val1, 3), dim1, v2, dim2,
				    depth + 1);
	    }
	}
    }
}

/* Assign VAL to slice starting with CONTAINER and of dimension DIM.  */
void do_inline
slice_assign (ER_node_t container, int dim, ER_node_t val)
{
  int val_dim = 0;

  if (ER_NODE_MODE (container) != ER_NM_vect)
    eval_error (sliceform_bc_decl, get_designator_pos (),
		DERR_slice_operand_form, 1);
  if (ER_NODE_MODE (val) == ER_NM_vect)
    {
      val_dim = ER_dim (val);
      val = ER_vect (val);
    }
  process_slice_assign (ER_vect (container), IVAL (container, 1), dim,
			val, val_dim, 1);
}

/* The following variable value is source of the exception (error)
   occurrence. */
static position_t exception_position;

/* Find PC to process EXCEPT and set it as CPC.  If it is not found,
   finish program or set up CPC as NULL in case of REPL.  Jump if we
   need to pop C stack or found PC (it becomes CPC) is in C stack.  */
static void
find_catch_pc (ER_node_t except)
{
  int jump_p;
  BC_node_t block;
  ER_node_t message;
  struct trace_stack_elem elem;
  val_t v1, v2;

  if (trace_flag)
    VLO_NULLIFY (trace_stack);
  jump_p = ER_c_stack_p (cstack);
  for (; cstack != uppest_stack;)
    {
      block = ER_block_node (cstack);
      sync_flag = BC_saved_sync_p (block);
      if (trace_flag && BC_NODE_MODE (block) != BC_NM_block)
	{
	  elem.block = block;
	  elem.pc = ER_call_pc (cstack);
	  VLO_ADD_MEMORY (trace_stack, &elem, sizeof (elem));
	}
      if (BC_NODE_MODE (block) == BC_NM_fblock && BC_thread_p (block))
	/* It can change cstack.  */
	delete_cprocess_during_exception ();
      if (cstack != uppest_stack)
	heap_pop ();
      if (ER_c_stack_p (cstack))
	jump_p = TRUE;
      /* Set up ctop as it should be for any statement begin.  */
      if (cstack != NULL)
	ctop = (ER_node_t) ((char *) cvars
			    + BC_vars_num (ER_block_node (cstack))
			    * sizeof (val_t) - sizeof (val_t));
      cpc = BC_excepts (block);
      if (cpc != NULL)
	{
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_stack);
	  ER_set_stack (ctop, except);
	  /* For catch deadlock.  The first catch statement is always
             block. */
	  ER_set_process_status (cprocess, PS_READY);
	  if (trace_flag)
	    VLO_NULLIFY (trace_stack);
	  if (jump_p)
	    switch_to_bcode ();
	  return;
	}
      else if (cstack == NULL)
	break;
    }
  ER_SET_MODE ((ER_node_t) &v1, ER_NM_stack);
  ER_set_stack ((ER_node_t) &v1, except);
  ER_SET_MODE ((ER_node_t) &v2, ER_NM_code);
  ER_set_code_id ((ER_node_t) &v2, CODE_ID (error_bc_decl));
  if (internal_isa_call (NULL, (ER_node_t) &v2, (ER_node_t) &v1))
    {
      BC_node_t decl = get_another_block_decl (ER_block_node (except),
					       msg_bc_decl);

      d_assert (BC_NODE_MODE (decl) == BC_NM_vdecl);
      message = IVAL (ER_stack_vars (except), BC_var_num (decl));
      if (ER_NODE_MODE (message) == ER_NM_vect)
	{
	  ER_node_t vect;
	  
	  vect = ER_vect (message);
	  GO_THROUGH_REDIR (vect);
	  if (ER_NODE_MODE (vect) != ER_NM_heap_pack_vect)
	    pack_vector_if_possible (vect);
	  if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
	      && ER_pack_vect_el_mode (vect) == ER_NM_char)
	    /* No return after error unless REPL. */
	    d_error (! repl_flag, exception_position, ER_pack_els (vect));
	}
    }
  if (! repl_flag)
    {
      if (ER_block_node (except) != sigint_bc_decl
	  && ER_block_node (except) != sigterm_bc_decl)
	d_error (TRUE, exception_position, DERR_unprocessed_exception,
		 BC_ident (BC_fdecl (ER_block_node (except))));
      dino_finish (1);
    }
  cpc = NULL;
  if (jump_p)
    switch_to_bcode ();
}

static int do_always_inline
long_bin_op (ER_node_t op1, ER_node_t op2)
{
  return ER_NODE_MODE (op1) == ER_NM_long && ER_NODE_MODE (op2) == ER_NM_long;
}

static do_always_inline ER_node_t
lbop (ER_node_t a, ER_node_t b, void mpzf (mpz_t, const mpz_t, const mpz_t)) {
  ER_node_t res = create_gmp ();

  mpzf (*ER_mpz_ptr (res), *ER_mpz_ptr (a), *ER_mpz_ptr (b));
  return res;
}

#define lfunc(name) static do_always_inline ER_node_t name (ER_node_t a, ER_node_t b)
lfunc (lplus) { return lbop (a, b, mpz_add);}
lfunc (lminus) { return lbop (a, b, mpz_sub);}
lfunc (lmult) { return lbop (a, b, mpz_mul);}
lfunc (lidiv) { return lbop (a, b, mpz_tdiv_q);}
lfunc (limod) { return lbop (a, b, mpz_tdiv_r);}

#define lcmpf(name) static do_always_inline int name (ER_node_t a, ER_node_t b)
lcmpf (leq) { return mpz_cmp (*ER_mpz_ptr (a), *ER_mpz_ptr (b)) == 0;}
lcmpf (lne) { return mpz_cmp (*ER_mpz_ptr (a), *ER_mpz_ptr (b)) != 0;}
lcmpf (llt) { return mpz_cmp (*ER_mpz_ptr (a), *ER_mpz_ptr (b)) < 0;}
lcmpf (lle) { return mpz_cmp (*ER_mpz_ptr (a), *ER_mpz_ptr (b)) <= 0;}
lcmpf (lgt) { return mpz_cmp (*ER_mpz_ptr (a), *ER_mpz_ptr (b)) > 0;}
lcmpf (lge) { return mpz_cmp (*ER_mpz_ptr (a), *ER_mpz_ptr (b)) >= 0;}

/* Do arithmetic operations using iop or fop on OP1 and OP2.  Put
   result into RES.  Use MSG in case of error.  Check and do vector
   operations if VECT_P.  */
static void do_always_inline
execute_ar_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p,
	       const char *err_message, rint_t iop (rint_t, rint_t),
	       rfloat_t fop (rfloat_t, rfloat_t),
	       ER_node_t lop (ER_node_t, ER_node_t))
{
  if (int_bin_op (op1, op2))
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, iop (ER_i (op1), ER_i (op2)));
    }
  else if (float_bin_op (op1, op2))
    {
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, fop (ER_f (op1),  ER_f (op2)));
    }
  else if (vect_p && vect_bin_op (op1, op2))
    binary_vect_op (res, op1, op2);
  else
    {
      ER_node_t l, r;

      implicit_conversion_for_binary_arithmetic_op (op1, op2, &l, &r);
      if (int_bin_op (l, r))
	{
	  rint_t op_i1 = ER_i (l), op_i2 = ER_i (r);

	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, iop (op_i1, op_i2));
	}
      else if (float_bin_op (l, r))
	{
	  rfloat_t op_f1 = ER_f (l), op_f2 = ER_f (r);

	  ER_SET_MODE (res, ER_NM_float);
	  ER_set_f (res, fop (op_f1, op_f2));
	}
      else if (long_bin_op (l, r))
	{
	  ER_node_t op1 = ER_l (l), op2 = ER_l (r);

	  ER_SET_MODE (res, ER_NM_long);
	  ER_set_l (res, lop (op1, op2));
	}
      else
	eval_error (optype_bc_decl, get_cpos (), err_message);
    }
}

/* The following different functions to implement binary
   operations:  */

void do_inline
execute_plus_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_plus_operands_types,
		 i_plus, f_plus, lplus);
}

void do_inline
execute_minus_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_minus_operands_types,
		 i_minus, f_minus, lminus);
}

void do_inline
execute_mult_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_mult_operands_types,
		 i_mult, f_mult, lmult);
}

void do_inline
execute_div_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_div_operands_types,
		 i_div, f_div, lidiv);
}

void do_inline
execute_mod_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_mod_operands_types,
		 i_mod, f_mod, limod);
}

/* Do int operations using iop on OP1 and OP2.  Put result into
   RES.  Use MSG in case of error.  Check and do vector operations if
   VECT_P.  */
static void do_always_inline
execute_int_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p,
	       const char *err_message, rint_t iop (rint_t, rint_t))
{
  if (int_bin_op (op1, op2))
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, iop (ER_i (op1), ER_i (op2)));
    }
  else if (vect_p && vect_bin_op (op1, op2))
    binary_vect_op (res, op1, op2);
  else 
    {
      ER_node_t l, r;

      implicit_conversion_for_binary_int_op (op1, op2, &l, &r);
      if (int_bin_op (l, r))
	{
	  rint_t op_i1 = ER_i (l), op_i2 = ER_i (r);

	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, iop (op_i1, op_i2));
	}
      else
	eval_error (optype_bc_decl, get_cpos (), err_message);
    }
}

void do_inline
execute_lshift_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_lshift_operands_types, i_lshift);
}

void do_inline
execute_rshift_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_rshift_operands_types, i_rshift);
}

void do_inline
execute_ashift_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_ashift_operands_types, i_ashift);
}

void do_inline
execute_and_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_and_operands_types, i_and);
}

void do_inline
execute_xor_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_xor_operands_types, i_xor);
}

void do_inline
execute_or_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_or_operands_types, i_or);
}

/* The following function implements array concatenation.  */
void do_inline
execute_concat_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  size_t els_number;
  ER_node_t vect1, vect2;
  val_t tvar1, tvar2;

  if (vect_p && vect_bin_op (op1, op2))
    {
      binary_vect_op (res, op1, op2);
      return;
    }
  op2 = to_vect_string_conversion (op2, NULL, (ER_node_t) &tvar2);
  op1 = to_vect_string_conversion (op1, NULL, (ER_node_t) &tvar1);
  if (ER_NODE_MODE (op2) != ER_NM_vect
      || ER_NODE_MODE (op1) != ER_NM_vect)
    eval_error (optype_bc_decl,	get_cpos (), DERR_concat_operands_types);
  vect1 = ER_vect (op1);
  vect2 = ER_vect (op2);
  if (ER_NODE_MODE (vect2) != ER_NODE_MODE (vect1)
      || (ER_NODE_MODE (vect2) == ER_NM_heap_pack_vect
	  && (ER_pack_vect_el_mode (vect2)
	      != ER_pack_vect_el_mode (vect1))))
    {
      if (ER_NODE_MODE (vect2) == ER_NM_heap_pack_vect)
	vect2 = unpack_vector (vect2);
      if (ER_NODE_MODE (vect1) == ER_NM_heap_pack_vect)
	vect1 = unpack_vector (vect1);
    }
  if (ER_NODE_MODE (vect2) == ER_NM_heap_pack_vect)
    {
      ER_node_mode_t result_el_type;
      size_t el_size;
      ER_node_t result;
      
      if (ER_pack_vect_el_mode (vect2) == ER_NM_nil)
	result_el_type = ER_pack_vect_el_mode (vect1);
      else if (ER_pack_vect_el_mode (vect1) == ER_NM_nil)
	result_el_type = ER_pack_vect_el_mode (vect2);
      else if (ER_pack_vect_el_mode (vect2)
	       == ER_pack_vect_el_mode (vect1))
	result_el_type = ER_pack_vect_el_mode (vect2);
      else
	d_unreachable ();
      el_size = type_size_table [result_el_type];
      els_number = ER_els_number (vect2) + ER_els_number (vect1);
      result = create_pack_vector (els_number, result_el_type);
      if (ER_els_number (vect1) != 0)
	memcpy (ER_pack_els (result), ER_pack_els (vect1),
		ER_els_number (vect1) * el_size);
      if (ER_els_number (vect2) != 0)
	memcpy (ER_pack_els (result) + ER_els_number (vect1) * el_size,
		ER_pack_els (vect2), ER_els_number (vect2) * el_size);
      if (result_el_type == ER_NM_char)
	ER_pack_els (result) [ER_els_number (vect1) + ER_els_number (vect2)]
	  = '\0';
      ER_SET_MODE (res, ER_NM_vect);
      set_vect_dim (res, result, 0);
    }
  else
    {
      ER_node_t result;
      
      els_number = ER_els_number (vect2) + ER_els_number (vect1);
      result = create_unpack_vector (els_number);
      if (ER_els_number (vect1) != 0)
	memcpy (ER_unpack_els (result), ER_unpack_els (vect1),
		ER_els_number (vect1) * sizeof (val_t));
      if (ER_els_number (vect2) != 0)
	memcpy ((char *) ER_unpack_els (result)
		+ ER_els_number (vect1) * sizeof (val_t),
		ER_unpack_els (vect2), ER_els_number (vect2) * sizeof (val_t));
      ER_SET_MODE (res, ER_NM_vect);
      set_vect_dim (res, result, 0);
    }
}

void do_inline
execute_in_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  ER_node_t tab;
  ER_node_t entry;
  int cmp;

  if (vect_p && vect_bin_op (op1, op2))
    {
      binary_vect_op (res, op1, op2);
      return;
    }
  if (ER_NODE_MODE (op2) != ER_NM_tab)
    eval_error (keyop_bc_decl, get_cpos (), DERR_in_table_operand_type);
  tab = ER_tab (op2);
  GO_THROUGH_REDIR (tab);
  entry = find_tab_el (tab, op1, FALSE);
  cmp = (entry != NULL && ER_NODE_MODE (entry) != ER_NM_empty_el);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, cmp);
}

/* Implement common case of equality/inequality comparison CMP_OP of
   OP1 and OP2.  Put result into RES.  Check and do vector operations
   if VECT_P.  */
void do_inline
execute_common_eq_ne_op (BC_node_mode_t cmp_op, ER_node_t res,
			 ER_node_t op1, ER_node_t op2, int vect_p)
{
  int cmp;
  ER_node_t l, r;

  if (vect_p && vect_bin_op (op1, op2))
    {
      binary_vect_op (res, op1, op2);
      return;
    }
  implicit_conversion_for_eq_op (op1, op2, &l, &r);
  if (ER_NODE_MODE (r) != ER_NODE_MODE (l))
    {
      if (ER_NODE_MODE (l) == ER_NM_undef || ER_NODE_MODE (r) == ER_NM_undef)
	eval_error (optype_bc_decl, get_cpos (), DERR_eq_operands_types);
      cmp = 0;
    }
  else
    switch ((unsigned char) ER_NODE_MODE (r))
      {
#ifdef __GNUC__
	/* This is a trick to make switch faster by removing range
	   comparison.  */
      case 0:
      case 255:
	abort ();
#endif
      case ER_NM_undef:
	eval_error (optype_bc_decl, get_cpos (), DERR_eq_operands_types);
	break;
      case ER_NM_nil:
	cmp = 1;
	break;
      case ER_NM_hide:
	cmp = ER_hide (r) == ER_hide (l);
	break;
      case ER_NM_hideblock:
	if (ER_hideblock_length (r) != ER_hideblock_length (l))
	  cmp = 0;
	else
	  cmp = memcmp (ER_hideblock (r), ER_hideblock (l),
			ER_hideblock_length (r)) == 0;
	break;
      case ER_NM_char:
	d_unreachable (); /* ALWAYS transformed to string/int/float */
      case ER_NM_int:
	cmp = ER_i (r) == ER_i (l);
	break;
      case ER_NM_float:
	cmp = ER_f (r) == ER_f (l);
	break;
      case ER_NM_long:
        cmp = leq (ER_l (r), ER_l (l));
	break;
      case ER_NM_type:
	cmp = ER_type (r) == ER_type (l);
	break;
      case ER_NM_vect:
	cmp = eq_vector (ER_vect (r), ER_vect (l));
	break;
      case ER_NM_tab:
	cmp = eq_table (ER_tab (r), ER_tab (l));
	break;
      case ER_NM_code:
	cmp = (ER_code_id (r) == ER_code_id (l)
	       && (ER_code_context (r) == ER_code_context (l)));
	break;
      case ER_NM_efun:
	/* Different definitions of one external function are not
	   equal.  */
	cmp = ER_efdecl (r) == ER_efdecl (l);
	break;
      case ER_NM_process:
	cmp = ER_process (r) == ER_process (l);
	break;
      case ER_NM_stack:
	cmp = eq_stack (ER_stack (r), ER_stack (l));
	break;
      default:
	d_unreachable ();
      }
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, cmp_op == BC_NM_eq ? cmp : !cmp);
}

void do_inline
execute_eq_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_eq, res, op1, op2, vect_p, i_eq, f_eq, execute_common_eq_ne_op);
}

void do_inline
execute_ne_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_ne, res, op1, op2, vect_p, i_ne, f_ne, execute_common_eq_ne_op);
}

/* Implement common case of comparison OPER of OP1 and OP2.  Put
   result into RES.  Check and do vector operations if VECT_P.  */
void do_inline
execute_common_cmp_op (BC_node_mode_t oper, ER_node_t res,
		       ER_node_t op1, ER_node_t op2, int vect_p)
{
  int cmp;
  ER_node_t l, r;

  if (vect_p && vect_bin_op (op1, op2))
    {
      binary_vect_op (res, op1, op2);
      return;
    }
  implicit_conversion_for_binary_arithmetic_op (op1, op2, &l, &r);
  if (int_bin_op (l, r))
    cmp = (oper == BC_NM_lt ? ER_i (l) < ER_i (r)
	   : (oper == BC_NM_gt ? ER_i (l) > ER_i (r)
	      : (oper == BC_NM_le ? ER_i (l) <= ER_i (r)
		 : ER_i (l) >= ER_i (r))));
  else if (float_bin_op (l, r))
    cmp = (oper == BC_NM_lt ? ER_f (l) < ER_f (r)
	   : (oper == BC_NM_gt ? ER_f (l) > ER_f (r)
	      : (oper == BC_NM_le ? ER_f (l) <= ER_f (r)
		 : ER_f (l) >= ER_f (r))));
  else if (long_bin_op (l, r))
    cmp = (oper == BC_NM_lt ? llt (ER_l (l), ER_l (r))
	   : (oper == BC_NM_gt ? lgt (ER_l (l), ER_l (r))
	      : (oper == BC_NM_le ? lle (ER_l (l), ER_l (r))
		 : lge (ER_l (l), ER_l (r)))));
  else
    eval_error (optype_bc_decl, get_cpos (),
		(oper == BC_NM_lt ? DERR_lt_operands_types
		 : (oper == BC_NM_gt ? DERR_gt_operands_types
		    : (oper == BC_NM_le ? DERR_le_operands_types
		       : DERR_ge_operands_types))));
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, cmp);
}

void do_inline
execute_lt_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_lt, res, op1, op2, vect_p, i_lt, f_lt, execute_common_cmp_op);
}

void do_inline
execute_ge_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_ge, res, op1, op2, vect_p, i_ge, f_ge, execute_common_cmp_op);
}

void do_inline
execute_gt_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_gt, res, op1, op2, vect_p, i_gt, f_gt, execute_common_cmp_op);
}

void do_inline
execute_le_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_le, res, op1, op2, vect_p, i_le, f_le, execute_common_cmp_op);
}

/* Implement identity (if INDENTITY_P) or unidentity comparison of OP1
   and OP2.  Put result into RES.  Check and do vector operations if
   VECT_P.  */
void do_inline
execute_identity_op (int identity_p,
		     ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  int cmp;

  if (vect_p && vect_bin_op (op1, op2))
    {
      binary_vect_op (res, op1, op2);
      return;
    }
  if (ER_NODE_MODE (op1) != ER_NODE_MODE (op2))
    {
      if (ER_NODE_MODE (op1) == ER_NM_undef
	  || ER_NODE_MODE (op2) == ER_NM_undef)
	eval_error (optype_bc_decl, get_cpos (), DERR_identity_operands_types);
      cmp = FALSE;
    }
  else
    switch (ER_NODE_MODE (op1))
      {
      case ER_NM_undef:
	eval_error (optype_bc_decl, get_cpos (), DERR_identity_operands_types);
	break;
      case ER_NM_nil:
	cmp = 1;
	break;
      case ER_NM_hide:
	cmp = ER_hide (op1) == ER_hide (op2);
	break;
      case ER_NM_hideblock:
	cmp = ER_hideblock (op1) == ER_hideblock (op2);
	break;
      case ER_NM_char:
	cmp = ER_ch (op1) == ER_ch (op2);
	break;
      case ER_NM_int:
	cmp = ER_i (op1) == ER_i (op2);
	break;
      case ER_NM_float:
	cmp = ER_f (op1) == ER_f (op2);
	break;
      case ER_NM_long:
	cmp = mpz_cmp (*ER_mpz_ptr (ER_l (op1)), *ER_mpz_ptr (ER_l (op2))) == 0;
	break;
      case ER_NM_type:
	cmp = ER_type (op1) == ER_type (op2);
	break;
      case ER_NM_vect:
	{
	  ER_node_t vect1 = ER_vect (op2);
	  ER_node_t vect2 = ER_vect (op1);
		  
	  GO_THROUGH_REDIR (vect2);
	  GO_THROUGH_REDIR (vect1);
	  cmp = vect1 == vect2;
	}
	break;
      case ER_NM_tab:
	{
	  ER_node_t tab1 = ER_tab (op2);
	  ER_node_t tab2 = ER_tab (op1);
		  
	  GO_THROUGH_REDIR (tab2);
	  GO_THROUGH_REDIR (tab1);
	  cmp = tab1 == tab2;
	}
	break;
      case ER_NM_code:
	/* We don't check the context here. */
	cmp = (ER_code_id (op1) == ER_code_id (op2));
	break;
      case ER_NM_efun:
	/* Different definitions of one external function are not
	   identical.  */
	cmp = (ER_efdecl (op1) == ER_efdecl (op2));
	break;
      case ER_NM_process:
	cmp = ER_process (op1) == ER_process (op2);
	break;
      case ER_NM_stack:
	cmp = ER_stack (op1) == ER_stack (op2);
	break;
      default:
	d_unreachable ();
      }
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, identity_p ? cmp : !cmp);
}

static position_t
get_left_operand_pos (void)
{
  BC_node_t info = BC_info (cpc);

  d_assert (BC_IS_OF_TYPE (info, BC_NM_source2));
  return BC_pos2 (info);
}

static position_t
get_right_operand_pos (void)
{
  BC_node_t info = BC_info (cpc);

  d_assert (BC_IS_OF_TYPE (info, BC_NM_source3));
  return BC_pos3 (info);
}

/* Execute LEN int operations IOP of packed array elements
   PACK_RES_ELS1 and PACK_ELS2 (if it is not NULL), otherwise
   implement operation PACK_ELS1 element IOP OP2 or OP2 IOP PACK_ELS1
   element (if REV_P).  Put results in PACK_RES_ELS.  */
static do_always_inline void
int_pack_vect_op (int rev_p, size_t len, char *pack_res_els, char *pack_els1,
		  char *pack_els2, ER_node_t op2, rint_t iop (rint_t, rint_t))
{
  size_t i;
  rint_t op_i2;

  if (pack_els2 != NULL)
    {
      d_assert (! rev_p);
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i]
	  = iop (((rint_t *) pack_els1) [i], ((rint_t *) pack_els2) [i]);
    }
  else if (rev_p)
    {
      op_i2 = ER_i (op2);
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i] = iop (op_i2, ((rint_t *) pack_els1) [i]);
    }
  else
    {
      op_i2 = ER_i (op2);
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i] = iop (((rint_t *) pack_els1) [i], op_i2);
    }
}

/* Analogous the function above but for rfloat point operations.  */
static do_always_inline void
float_pack_vect_op (int rev_p, size_t len, char *pack_res_els, char *pack_els1,
		    char *pack_els2, ER_node_t op2,
		    rfloat_t fop (rfloat_t, rfloat_t))
{
  size_t i;
  rfloat_t op_f2;

  if (pack_els2 != NULL)
    {
      d_assert (! rev_p);
      for (i = 0; i < len; i++)
	((rfloat_t *) pack_res_els) [i]
	  = fop (((rfloat_t *) pack_els1) [i],
		 ((rfloat_t *) pack_els2) [i]);
    }
  else if (rev_p)
    {
      op_f2 = ER_f (op2);
      for (i = 0; i < len; i++)
	((rfloat_t *) pack_res_els) [i]
	  = fop (op_f2, ((rfloat_t *) pack_els1) [i]);
    }
  else
    {
      op_f2 = ER_f (op2);
      for (i = 0; i < len; i++)
	((rfloat_t *) pack_res_els) [i]
	  = fop (((rfloat_t *) pack_els1) [i], op_f2);
    }
}

/* Analogous the function above but used for char comparison
   operations.  */
static do_always_inline void
char_pack_vect_cmp_op (size_t len, char *pack_res_els, char *pack_els1,
		       char *pack_els2, ER_node_t op2, int icmp (rint_t, rint_t))
{
  size_t i;
  rint_t op_i2;

  if (pack_els2 != NULL)
    {
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i]
	  = icmp (((char_t *) pack_els1) [i], ((char_t *) pack_els2) [i]);
    }
  else
    {
      op_i2 = ER_i (op2);
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i]
	  = icmp (((char_t *) pack_els1) [i], op_i2);
    }
}

/* Analogous the function above but used for int comparison
   operations.  */
static do_always_inline void
int_pack_vect_cmp_op (size_t len, char *pack_res_els, char *pack_els1,
		      char *pack_els2, ER_node_t op2, int icmp (rint_t, rint_t))
{
  size_t i;
  rint_t op_i2;

  if (pack_els2 != NULL)
    {
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i]
	  = icmp (((rint_t *) pack_els1) [i], ((rint_t *) pack_els2) [i]);
    }
  else
    {
      op_i2 = ER_i (op2);
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i]
	  = icmp (((rint_t *) pack_els1) [i], op_i2);
    }
}

/* Analogous the function above but used for float comparison
   operations.  */
static do_always_inline void
float_pack_vect_cmp_op (size_t len, char *pack_res_els, char *pack_els1,
			char *pack_els2, ER_node_t op2,
			int fcmp (rfloat_t, rfloat_t))
{
  size_t i;
  rfloat_t op_f2;

  if (pack_els2 != NULL)
    {
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i]
	  = fcmp (((rfloat_t *) pack_els1) [i],
		 ((rfloat_t *) pack_els2) [i]);
    }
  else
    {
      op_f2 = ER_f (op2);
      for (i = 0; i < len; i++)
	((rint_t *) pack_res_els) [i]
	  = fcmp (((rfloat_t *) pack_els1) [i], op_f2);
    }
}

/* Recursive function implementing binary vector operations defined by
   CPC.  Operands are OP1 with dimension DIM1 and OP2 with dimension
   DIM2 (DIM1 >= DIM2).  If REV_P, operands are taken for operations
   in different order.  The current recursion depth is DEPTH.  */
static ER_node_t
process_binary_vect_op (int rev_p, ER_node_t op1, int dim1,
			ER_node_t op2, int dim2, int depth)
{
  int pack_flag1, pack_flag2, scalar_p2, neg_p;
  size_t i, len1, len2;
  BC_node_mode_t oper;
  ER_node_mode_t el_type1, el_type2;
  val_t l, r, rval;
  char *pack_els1 = NULL, *pack_els2 = NULL, *pack_res_els;
  ER_node_t res, v1, v2;
  ER_node_t el_res, unpack_els1 = NULL, unpack_els2 = NULL, unpack_res_els;

  d_assert (dim1 >= dim2 && dim2 >= 0);
  GO_THROUGH_REDIR (op1);
  len1 = ER_els_number (op1);
  if (ER_NODE_MODE (op1) == ER_NM_heap_pack_vect)
    {
      pack_flag1 = TRUE;
      pack_els1 = ER_pack_els (op1);
      el_type1 = ER_pack_vect_el_mode (op1);
    }
  else
    {
      pack_flag1 = FALSE;
      unpack_els1 = ER_unpack_els (op1);
      el_type1 = ER_NM__error;
    }
  if (dim2 == 0)
    {
      scalar_p2 = pack_flag2 = TRUE;
      el_type2 = ER_NODE_MODE (op2);
    }
  else
    {
      scalar_p2 = FALSE;
      GO_THROUGH_REDIR (op2);
      len2 = ER_els_number (op2);
      if (ER_NODE_MODE (op2) == ER_NM_heap_pack_vect)
	{
	  pack_flag2 = TRUE;
	  pack_els2 = ER_pack_els (op2);
	  el_type2 = ER_pack_vect_el_mode (op2);
	}
      else
	{
	  pack_flag2 = FALSE;
	  unpack_els2 = ER_unpack_els (op2);
	  el_type2 = ER_NM__error;
	}
      if (len1 != len2)
	eval_error (veclen_bc_decl, get_cpos (),
		    DERR_different_vec_operand_lengths,
		    (long) len1, (long) len2, depth);
    }
  if (dim1 > 1)
    {
      ER_node_t p;

      /* Process recursively  */
      dim1--;
      if (pack_flag1 && el_type1 != ER_NM_vect)
	eval_error (vecform_bc_decl, get_left_operand_pos (),
		    DERR_vector_form_type, depth);
      if (! scalar_p2)
	{
	  if (pack_flag2 && el_type2 != ER_NM_vect)
	    eval_error (vecform_bc_decl, get_right_operand_pos (),
			DERR_vector_form_type, depth);
	  dim2--;
	}
      res = create_pack_vector (len1, ER_NM_vect);
      ER_set_els_number (res, 0);
      for (i = 0; i < len1; i++)
	{
	  if (pack_flag1)
	    v1 = ((ER_node_t *) pack_els1) [i];
	  else
	    {
	      v1 = IVAL (unpack_els1, i);
	      if (ER_NODE_MODE (v1) != ER_NM_vect)
		eval_error (vecform_bc_decl, get_left_operand_pos (),
			    DERR_vector_form_type, depth);
	      v1 = ER_vect (v1);
	    }
	  if (scalar_p2)
	    v2 = op2;
	  else if (pack_flag2)
	    v2 = ((ER_node_t *) pack_els2) [i];
	  else
	    {
	      v2 = IVAL (unpack_els2, i);
	      if (ER_NODE_MODE (v2) != ER_NM_vect)
		eval_error (vecform_bc_decl, get_right_operand_pos (),
			    DERR_vector_form_type, depth);
	      v2 = ER_vect (v2);
	    }
	  p = process_binary_vect_op (rev_p, v1, dim1, v2, dim2, depth + 1);
	  set_packed_vect_el (res, i, p);
	}
      ER_set_els_number (res, len1);
      return res;
    }
  oper = BC_NODE_MODE (cpc);
  if (oper == BC_NM_madd)
    oper = madd_mult_p ? BC_NM_mult : BC_NM_add;
  switch (oper)
    {
    case BC_NM_add:
    case BC_NM_addi:
    case BC_NM_add_st:
    case BC_NM_add_slst:
    case BC_NM_sub:
    case BC_NM_sub_st:
    case BC_NM_sub_slst:
    case BC_NM_mult:
    case BC_NM_multi:
    case BC_NM_mult_st:
    case BC_NM_mult_slst:
    case BC_NM_div:
    case BC_NM_div_st:
    case BC_NM_div_slst:
    case BC_NM_mod:
    case BC_NM_mod_st:
    case BC_NM_mod_slst:
      if (pack_flag1 && pack_flag2 && el_type1 == el_type2)
	{
	  /* Both packed and the same type.  */
	  if (el_type1 == ER_NM_int)
	    {
	      res = create_pack_vector (len1, ER_NM_int);
	      ER_set_els_number (res, 0);
	      pack_res_els = ER_pack_els (res);
	      switch (oper)
		{
		case BC_NM_add:
		case BC_NM_addi:
		case BC_NM_add_st:
		case BC_NM_add_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_plus);
		  break;
		case BC_NM_sub:
		case BC_NM_sub_st:
		case BC_NM_sub_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_minus);
		  break;
		case BC_NM_mult:
		case BC_NM_multi:
		case BC_NM_mult_st:
		case BC_NM_mult_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_mult);
		  break;
		case BC_NM_div:
		case BC_NM_div_st:
		case BC_NM_div_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_div);
		  break;
		case BC_NM_mod:
		case BC_NM_mod_st:
		case BC_NM_mod_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_mod);
		  break;
		default:
		  d_unreachable ();
		}
	      ER_set_els_number (res, len1);
	      return res;
	    }
	  else if (el_type1 == ER_NM_float)
	    {
	      res = create_pack_vector (len1, ER_NM_float);
	      ER_set_els_number (res, 0);
	      pack_res_els = ER_pack_els (res);
	      switch (oper)
		{
		case BC_NM_add:
		case BC_NM_addi:
		case BC_NM_add_st:
		case BC_NM_add_slst:
		  float_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				      pack_els2, op2, f_plus);
		  break;
		case BC_NM_sub:
		case BC_NM_sub_st:
		case BC_NM_sub_slst:
		  float_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				      pack_els2, op2, f_minus);
		  break;
		case BC_NM_mult:
		case BC_NM_multi:
		case BC_NM_mult_st:
		case BC_NM_mult_slst:
		  float_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				      pack_els2, op2, f_mult);
		  break;
		case BC_NM_div:
		case BC_NM_div_st:
		case BC_NM_div_slst:
		  float_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				      pack_els2, op2, f_div);
		  break;
		case BC_NM_mod:
		case BC_NM_mod_st:
		case BC_NM_mod_slst:
		  float_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				      pack_els2, op2, f_mod);
		  break;
		default:
		  d_unreachable ();
		}
	      ER_set_els_number (res, len1);
	      return res;
	    }
	}
      if (pack_flag1)
	op1 = unpack_vector (op1);
      unpack_els1 = ER_unpack_els (op1);
      if (! scalar_p2)
	{
	  if (pack_flag2)
	    op2 = unpack_vector (op2);
	  unpack_els2 = ER_unpack_els (op2);
	}
      res = create_unpack_vector (len1);
      ER_set_els_number (res, 0);
      unpack_res_els = ER_unpack_els (res);
      for (i = 0; i < len1; i++)
	{
	  l = *(val_t *) IVAL (unpack_els1, i);
	  if (! scalar_p2)
	    r = *(val_t *) IVAL (unpack_els2, i);
	  else if (! rev_p)
	    r = *(val_t *) op2;
	  else
	    {
	      r = l;
	      l = *(val_t *) op2;
	    }
	  el_res = IVAL (unpack_res_els, i);
	  switch (oper)
	    {
	    case BC_NM_add:
	    case BC_NM_addi:
	    case BC_NM_add_st:
	    case BC_NM_add_slst:
	      execute_plus_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_sub:
	    case BC_NM_sub_st:
	    case BC_NM_sub_slst:
	      execute_minus_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_mult:
	    case BC_NM_multi:
	    case BC_NM_mult_st:
	    case BC_NM_mult_slst:
	      execute_mult_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_div:
	    case BC_NM_div_st:
	    case BC_NM_div_slst:
	      execute_div_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_mod:
	    case BC_NM_mod_st:
	    case BC_NM_mod_slst:
	      execute_mod_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    default:
	      d_unreachable ();
	    }
	}
      ER_set_els_number (res, len1);
      pack_vector_if_possible (res);
      return res;
    case BC_NM_lsh:
    case BC_NM_lsh_st:
    case BC_NM_lsh_slst:
    case BC_NM_rsh:
    case BC_NM_rsh_st:
    case BC_NM_rsh_slst:
    case BC_NM_ash:
    case BC_NM_ash_st:
    case BC_NM_ash_slst:
    case BC_NM_and:
    case BC_NM_and_st:
    case BC_NM_and_slst:
    case BC_NM_xor:
    case BC_NM_xor_st:
    case BC_NM_xor_slst:
    case BC_NM_or:
    case BC_NM_or_st:
    case BC_NM_or_slst:
      if (pack_flag1 && pack_flag2 && el_type1 == el_type2)
	{
	  /* Both packed and the same type.  */
	  if (el_type1 == ER_NM_int)
	    {
	      res = create_pack_vector (len1, ER_NM_int);
	      pack_res_els = ER_pack_els (res);
	      switch (oper)
		{
		case BC_NM_lsh:
		case BC_NM_lsh_st:
		case BC_NM_lsh_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_lshift);
		  break;
		case BC_NM_rsh:
		case BC_NM_rsh_st:
		case BC_NM_rsh_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_rshift);
		  break;
		case BC_NM_ash:
		case BC_NM_ash_st:
		case BC_NM_ash_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_ashift);
		  break;
		case BC_NM_and:
		case BC_NM_and_st:
		case BC_NM_and_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_and);
		  break;
		case BC_NM_xor:
		case BC_NM_xor_st:
		case BC_NM_xor_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_xor);
		  break;
		case BC_NM_or:
		case BC_NM_or_st:
		case BC_NM_or_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, i_or);
		  break;
		default:
		  d_unreachable ();
		}
	      return res;
	    }
	}
      if (pack_flag1)
	op1 = unpack_vector (op1);
      unpack_els1 = ER_unpack_els (op1);
      if (! scalar_p2)
	{
	  if (pack_flag2)
	    op2 = unpack_vector (op2);
	  unpack_els2 = ER_unpack_els (op2);
	}
      res = create_unpack_vector (len1);
      ER_set_els_number (res, 0);
      unpack_res_els = ER_unpack_els (res);
      for (i = 0; i < len1; i++)
	{
	  l = *(val_t *) IVAL (unpack_els1, i);
	  if (! scalar_p2)
	    r = *(val_t *) IVAL (unpack_els2, i);
	  else if (! rev_p)
	    r = *(val_t *) op2;
	  else
	    {
	      r = l;
	      l = *(val_t *) op2;
	    }
	  el_res = IVAL (unpack_res_els, i);
	  switch (oper)
	    {
	    case BC_NM_lsh:
	    case BC_NM_lsh_st:
	    case BC_NM_lsh_slst:
	      execute_lshift_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_rsh:
	    case BC_NM_rsh_st:
	    case BC_NM_rsh_slst:
	      execute_rshift_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_ash:
	    case BC_NM_ash_st:
	    case BC_NM_ash_slst:
	      execute_ashift_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_and:
	    case BC_NM_and_st:
	    case BC_NM_and_slst:
	      execute_and_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_xor:
	    case BC_NM_xor_st:
	    case BC_NM_xor_slst:
	      execute_xor_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_or:
	    case BC_NM_or_st:
	    case BC_NM_or_slst:
	      execute_or_op (el_res, (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    default:
	      d_unreachable ();
	    }
	}
      ER_set_els_number (res, len1);
      pack_vector_if_possible (res);
      return res;
    case BC_NM_concat:
    case BC_NM_concat_st:
    case BC_NM_concat_slst:
      res = create_unpack_vector (len1);
      ER_set_els_number (res, 0);
      unpack_res_els = ER_unpack_els (res);
      for (i = 0; i < len1; i++)
	{
	  if (pack_flag1)
	    load_packed_vector_element ((ER_node_t) &l, op1, i);
	  else
	    l = *(val_t *) IVAL (ER_unpack_els (op1), i);
	  if (! scalar_p2)
	    {
	      if (pack_flag2)
		load_packed_vector_element ((ER_node_t) &r, op2, i);
	      else
		r = *(val_t *) IVAL (ER_unpack_els (op2), i);
	    }
	  else if (! rev_p)
	    r = *(val_t *) op2;
	  else
	    {
	      r = l;
	      l = *(val_t *) op2;
	    }
	  execute_concat_op (IVAL (unpack_res_els, i),
			     (ER_node_t) &l, (ER_node_t) &r, FALSE);
	}
      ER_set_els_number (res, len1);
      pack_vector_if_possible (res);
      return res;
    case BC_NM_eq:
    case BC_NM_ne:
    case BC_NM_id:
    case BC_NM_unid:
      res = create_pack_vector (len1, ER_NM_int);
      ER_set_els_number (res, 0);
      pack_res_els = ER_pack_els (res);
      neg_p = oper == BC_NM_ne || oper == BC_NM_unid;
      if (pack_flag1 && pack_flag2 && el_type1 == el_type2)
	{
	  switch (el_type1)
	    {
	    case ER_NM_char:
	      char_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
				     pack_els2, op2, neg_p ? i_ne : i_eq);
	      break;
	    case ER_NM_int:
	      int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
				    pack_els2, op2, neg_p ? i_ne : i_eq);
	      break;
	    case ER_NM_float:
	      float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
				      pack_els2, op2, neg_p ? f_ne : f_eq);
	      break;
	    default:
	      goto eq_common;
	    }
	  ER_set_els_number (res, len1);
	  return res;
	}
    eq_common:
      if (pack_flag1)
	op1 = unpack_vector (op1);
      unpack_els1 = ER_unpack_els (op1);
      if (! scalar_p2)
	{
	  if (pack_flag2)
	    op2 = unpack_vector (op2);
	  unpack_els2 = ER_unpack_els (op2);
	}
      for (i = 0; i < len1; i++)
	{
	  l = *(val_t *) IVAL (unpack_els1, i);
	  if (! scalar_p2)
	    r = *(val_t *) IVAL (unpack_els2, i);
	  else
	    r = *(val_t *) op2;
	  if (oper == BC_NM_eq)
	    execute_eq_op ((ER_node_t) &rval,
			   (ER_node_t) &l, (ER_node_t) &r, FALSE);
	  else if (oper == BC_NM_ne)
	    execute_ne_op ((ER_node_t) &rval,
			   (ER_node_t) &l, (ER_node_t) &r, FALSE);
	  else
	    execute_identity_op (! neg_p, (ER_node_t) &rval,
				 (ER_node_t) &l, (ER_node_t) &r, FALSE);
	  ((rint_t *) pack_res_els) [i] = ER_i ((ER_node_t) &rval);
	}
      ER_set_els_number (res, len1);
      return res;
    case BC_NM_le:
    case BC_NM_ge:
    case BC_NM_lt:
    case BC_NM_gt:
      res = create_pack_vector (len1, ER_NM_int);
      ER_set_els_number (res, 0);
      pack_res_els = ER_pack_els (res);
      if (pack_flag1 && pack_flag2 && el_type1 == el_type2)
	{
	  switch (el_type1)
	    {
	    case ER_NM_int:
	      switch (oper)
		{
		case BC_NM_lt:
		  int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					pack_els2, op2, rev_p ? i_ge : i_lt);
		  break;
		case BC_NM_ge:
		  int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					pack_els2, op2, rev_p ? i_lt : i_ge);
		  break;
		case BC_NM_le:
		  int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					pack_els2, op2, rev_p ? i_gt : i_le);
		  break;
		case BC_NM_gt:
		  int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					pack_els2, op2, rev_p ? i_le : i_gt);
		  break;
		default:
		  d_unreachable ();
		}
	      break;
	    case ER_NM_float:
	      switch (oper)
		{
		case BC_NM_lt:
		  float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					  pack_els2, op2, rev_p ? f_ge : f_lt);
		  break;
		case BC_NM_ge:
		  float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					  pack_els2, op2, rev_p ? f_lt : f_ge);
		  break;
		case BC_NM_le:
		  float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					  pack_els2, op2, rev_p ? f_gt : f_le);
		  break;
		case BC_NM_gt:
		  float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					  pack_els2, op2, rev_p ? f_le : f_gt);
		  break;
		default:
		  d_unreachable ();
		}
	      break;
	    default:
	      goto cmp_common;
	    }
	  ER_set_els_number (res, len1);
	  return res;
	}
    cmp_common:
      if (pack_flag1)
	op1 = unpack_vector (op1);
      unpack_els1 = ER_unpack_els (op1);
      if (! scalar_p2)
	{
	  if (pack_flag2)
	    op2 = unpack_vector (op2);
	  unpack_els2 = ER_unpack_els (op2);
	}
      for (i = 0; i < len1; i++)
	{
	  l = *(val_t *) IVAL (unpack_els1, i);
	  if (! scalar_p2)
	    r = *(val_t *) IVAL (unpack_els2, i);
	  else if (! rev_p)
	    r = *(val_t *) op2;
	  else
	    {
	      r = l;
	      l = *(val_t *) op2;
	    }
	  switch (oper)
	    {
	    case BC_NM_lt:
	      execute_lt_op ((ER_node_t) &rval,
			     (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_ge:
	      execute_ge_op ((ER_node_t) &rval,
			     (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_le:
	      execute_le_op ((ER_node_t) &rval,
			     (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    case BC_NM_gt:
	      execute_gt_op ((ER_node_t) &rval,
			     (ER_node_t) &l, (ER_node_t) &r, FALSE);
	      break;
	    default:
	      d_unreachable ();
	    }
	  ((rint_t *) pack_res_els) [i] = ER_i ((ER_node_t) &rval);
	}
      ER_set_els_number (res, len1);
      return res;
    case BC_NM_in:
      res = create_pack_vector (len1, ER_NM_int);
      ER_set_els_number (res, 0);
      pack_res_els = ER_pack_els (res);
      for (i = 0; i < len1; i++)
	{
	  if (pack_flag1)
	    load_packed_vector_element ((ER_node_t) &l, op1, i);
	  else
	    l = *(val_t *) IVAL (ER_unpack_els (op1), i);
	  if (! scalar_p2)
	    {
	      if (pack_flag2)
		load_packed_vector_element ((ER_node_t) &r, op2, i);
	      else
		r = *(val_t *) IVAL (ER_unpack_els (op2), i);
	    }
	  else if (! rev_p)
	    r = *(val_t *) op2;
	  else
	    {
	      r = l;
	      l = *(val_t *) op2;
	    }
	  execute_in_op ((ER_node_t) &rval,
			 (ER_node_t) &l, (ER_node_t) &r, FALSE);
	  ((rint_t *) pack_res_els) [i] = ER_i ((ER_node_t) &rval);
	}
      ER_set_els_number (res, len1);
      return res;
    case BC_NM_fmtvecof:
      res = create_unpack_vector (len1);
      ER_set_els_number (res, 0);
      unpack_res_els = ER_unpack_els (res);
      for (i = 0; i < len1; i++)
	{
	  if (pack_flag1)
	    load_packed_vector_element ((ER_node_t) &l, op1, i);
	  else
	    l = *(val_t *) IVAL (ER_unpack_els (op1), i);
	  if (! scalar_p2)
	    {
	      if (pack_flag2)
		load_packed_vector_element ((ER_node_t) &r, op2, i);
	      else
		r = *(val_t *) IVAL (ER_unpack_els (op2), i);
	    }
	  else if (! rev_p)
	    r = *(val_t *) op2;
	  else
	    {
	      r = l;
	      l = *(val_t *) op2;
	    }
	  execute_vectorof_op (IVAL (unpack_res_els, i),
			       (ER_node_t) &l, (ER_node_t) &r, FALSE);
	}
      ER_set_els_number (res, len1);
      pack_vector_if_possible (res);
      return res;
    default:
      d_unreachable ();
    }
}

static int do_always_inline
get_dim (ER_node_t op)
{
  return ER_NODE_MODE (op) == ER_NM_vect ? ER_dim (op) : 0;
}

/* Implement binary vector operation defined by CPC.  */
void
binary_vect_op (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  int rev_p = FALSE;
  int dim1, dim2, t;
  ER_node_t top, vect;

  dim1 = get_dim (op1);
  dim2 = get_dim (op2);
  if (dim1 < dim2)
    {
      t = dim1; dim1 = dim2; dim2 = t;
      top = op1; op1 = op2; op2 = top; rev_p = TRUE;
    }
  d_assert (dim1 > 0);
  if (ER_NODE_MODE (op1) == ER_NM_vect)
    op1 = ER_vect (op1);
  else
    eval_error (vecform_bc_decl, get_left_operand_pos (),
		DERR_vector_form_type, 1);
  if (dim2 != 0)
    {
      if (ER_NODE_MODE (op2) == ER_NM_vect)
	op2 = ER_vect (op2);
      else
	eval_error (vecform_bc_decl, get_right_operand_pos (),
		    DERR_vector_form_type, 1);
    }
  vect = process_binary_vect_op (rev_p, op1, dim1, op2, dim2, 1);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, dim1);
}


/* Execute LEN int operations IOP of packed array elements
   PACK_ELS.  Put results in PACK_RES_ELS.  */
static do_always_inline void
int_pack_vect_unary_op (size_t len, char *pack_res_els, char *pack_els,
			rint_t iop (rint_t))
{
  size_t i;

  for (i = 0; i < len; i++)
    ((rint_t *) pack_res_els) [i] = iop (((rint_t *) pack_els) [i]);
}

/* Analogous to function above but for rfloat point operations.  */
static do_always_inline void
float_pack_vect_unary_op (size_t len, char *pack_res_els, char *pack_els,
			  rfloat_t fop (rfloat_t))
{
  size_t i;

  for (i = 0; i < len; i++)
    ((rfloat_t *) pack_res_els) [i] = fop (((rfloat_t *) pack_els) [i]);
}

/* Recursive function implementing unary vector operations defined by
   CPC.  Operand is OP with dimension DIM.  The current recursion
   depth is DEPTH.  */
static ER_node_t
process_unary_vect_op (ER_node_t op, int dim, int depth)
{
  int pack_flag;
  size_t i, len;
  BC_node_mode_t oper;
  ER_node_mode_t el_type;
  char *pack_els = NULL, *pack_res_els;
  ER_node_t res, v;
  ER_node_t el_res, el, unpack_els = NULL, unpack_res_els;

  d_assert (dim > 0);
  GO_THROUGH_REDIR (op);
  len = ER_els_number (op);
  if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
    {
      pack_flag = TRUE;
      pack_els = ER_pack_els (op);
      el_type = ER_pack_vect_el_mode (op);
    }
  else
    {
      pack_flag = FALSE;
      unpack_els = ER_unpack_els (op);
      el_type = ER_NM__error;
    }
  if (dim > 1)
    {
      ER_node_t p;

      /* Process recursively  */
      dim--;
      if (pack_flag && el_type != ER_NM_vect)
	eval_error (vecform_bc_decl, get_left_operand_pos (),
		    DERR_vector_form_type, depth);
      res = create_pack_vector (len, ER_NM_vect);
      ER_set_els_number (res, 0);
      for (i = 0; i < len; i++)
	{
	  if (pack_flag)
	    v = ((ER_node_t *) pack_els) [i];
	  else
	    {
	      v = IVAL (unpack_els, i);
	      if (ER_NODE_MODE (v) != ER_NM_vect)
		eval_error (vecform_bc_decl, get_left_operand_pos (),
			    DERR_vector_form_type, depth);
	      v = ER_vect (v);
	    }
	  p = process_unary_vect_op (v, dim, depth + 1);
	  set_packed_vect_el (res, i, p);
	}
      ER_set_els_number (res, len);
      return res;
    }
  oper = BC_NODE_MODE (cpc);
  if (pack_flag)
    {
      if (el_type == ER_NM_float)
	{
	  if (oper != BC_NM_plus && oper != BC_NM_minus)
	    goto general_unary;
	  res = create_pack_vector (len, ER_NM_float);
	  ER_set_els_number (res, 0);
	  pack_res_els = ER_pack_els (res);
	  if (oper == BC_NM_plus)
	    float_pack_vect_unary_op (len, pack_res_els, pack_els, funary_plus);
	  else
	    float_pack_vect_unary_op (len, pack_res_els, pack_els,
				      funary_minus);
	}
      else if (el_type == ER_NM_int)
	{
	  if (oper != BC_NM_plus && oper != BC_NM_minus
	      && oper != BC_NM_not && oper != BC_NM_bnot)
	    goto general_unary;
	  res = create_pack_vector (len, ER_NM_int);
	  ER_set_els_number (res, 0);
	  pack_res_els = ER_pack_els (res);
	  switch (oper)
	    {
	    case BC_NM_not:
	      int_pack_vect_unary_op (len, pack_res_els, pack_els, i_not);
	      break;
	    case BC_NM_bnot:
	      int_pack_vect_unary_op (len, pack_res_els, pack_els, ibitwise_not);
	      break;
	    case BC_NM_plus:
	      int_pack_vect_unary_op (len, pack_res_els, pack_els, iunary_plus);
	      break;
	    case BC_NM_minus:
	      int_pack_vect_unary_op (len, pack_res_els, pack_els, iunary_minus);
	      break;
	    default:
	      d_unreachable ();
	    }
	}
      else
	goto general_unary;
      ER_set_els_number (res, len);
      return res;
    }
 general_unary:
  if (pack_flag)
    {
      op = unpack_vector (op);
      unpack_els = ER_unpack_els (op);
    }
  /* ???We can create pack vector for some.  */
  res = create_unpack_vector (len);
  ER_set_els_number (res, 0);
  unpack_res_els = ER_unpack_els (res);
  for (i = 0; i < len; i++)
    {
      el = IVAL (unpack_els, i);
      el_res = IVAL (unpack_res_els, i);
      switch (oper)
	{
	case BC_NM_not:
	  execute_not_op (el_res, el, FALSE);
	  break;
	case BC_NM_bnot:
	  execute_bitwise_not_op (el_res, el, FALSE);
	  break;
	case BC_NM_plus:
	  execute_unary_ar_op (el_res, el, FALSE, DERR_unary_plus_operand_type,
			       iunary_plus, funary_plus, lunary_plus);
	  break;
	case BC_NM_minus:
	  execute_unary_ar_op (el_res, el, FALSE, DERR_unary_minus_operand_type,
			       iunary_minus, funary_minus, lunary_minus);
	  break;
	case BC_NM_length:
	  execute_length_op (el_res, el, FALSE);
	  break;
	case BC_NM_const:
	  execute_const_op (el_res, el, FALSE);
	  break;
	case BC_NM_new:
	  execute_new_op (el_res, el, FALSE);
	  break;
	case BC_NM_tpof:
	  execute_typeof_op (el_res, el, FALSE);
	  break;
	case BC_NM_chof:
	  execute_charof_op (el_res, el, FALSE);
	  break;
	case BC_NM_iof:
	  execute_intof_op (el_res, el, FALSE);
	  break;
	case BC_NM_lof:
	  execute_longof_op (el_res, el, FALSE);
	  break;
	case BC_NM_fof:
	  execute_floatof_op (el_res, el, FALSE);
	  break;
	case BC_NM_vecof:
	  execute_vectorof_op (el_res, el, NULL, FALSE);
	  break;
	case BC_NM_tabof:
	  execute_tableof_op (el_res, el, FALSE);
	  break;
	default:
	  d_unreachable ();
	}
    }
  ER_set_els_number (res, len);
  pack_vector_if_possible (res);
  return res;
}

/* Implement unary vector operation defined by CPC.  */
void
unary_vect_op (ER_node_t res, ER_node_t op)
{
  int dim;
  ER_node_t vect;

  dim = get_dim (op);
  d_assert (dim > 0);
  if (ER_NODE_MODE (op) == ER_NM_vect)
    op = ER_vect (op);
  else
    eval_error (vecform_bc_decl, get_left_operand_pos (),
		DERR_vector_form_type, 1);
  vect = process_unary_vect_op (op, dim, 1);
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, dim);
}

/* Implement folding operation using IOP, FOP, LOP, or GENOP on packed
   vector PACK_VECT of length LEN with elements type EL_TYPE.  Use
   INITVAL as initial value for folding.  Put result into RES.  */
static void do_always_inline
execute_pack_fold_op (ER_node_t res, rint_t initval, ER_node_t pack_vect,
		      ER_node_mode_t el_type, size_t len, char *pack_els,
		      void genop (ER_node_t, ER_node_t, ER_node_t, int),
		      rint_t iop (rint_t, rint_t),
		      rfloat_t fop (rfloat_t, rfloat_t))
{
  size_t i;
  val_t l, rval;
  ER_node_t r = (ER_node_t) &rval;
  rint_t ires;
  rfloat_t fres;

  if (el_type == ER_NM_int)
    {
      ires = initval;
      for (i = 0; i < len; i++)
	ires = iop (ires, ((rint_t *) pack_els) [i]);
      ER_SET_MODE (r, ER_NM_int);
      ER_set_i (r, ires);
    }
  else if (fop != NULL && el_type == ER_NM_float)
    {
      fres = initval;
      for (i = 0; i < len; i++)
	fres = fop (fres, ((rfloat_t *) pack_els) [i]);
      ER_SET_MODE (r, ER_NM_float);
      ER_set_f (r, fres);
    }
  else
    {
      ER_SET_MODE (r, ER_NM_int);
      ER_set_i (r, initval);
      for (i = 0; i < len; i++)
	{
	  load_packed_vector_element ((ER_node_t) &l, pack_vect, i);
	  genop (r, r, (ER_node_t) &l, FALSE);
	}
    }
  genop (res, res, r, FALSE);
}


/* Recursive function implementing unary vector operations defined by
   CPC.  Operand is vector OP with dimension DIM.  The current
   recursion depth is DEPTH.  */
static void
process_fold_vect_op (ER_node_t res, ER_node_t op, int dim, int depth)
{
  int pack_flag;
  size_t i, len;
  BC_node_mode_t oper;
  ER_node_mode_t el_type;
  val_t l;
  char *pack_els = NULL;
  ER_node_t v;
  ER_node_t unpack_els = NULL;

  d_assert (dim > 0);
  GO_THROUGH_REDIR (op);
  len = ER_els_number (op);
  if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
    {
      pack_flag = TRUE;
      pack_els = ER_pack_els (op);
      el_type = ER_pack_vect_el_mode (op);
    }
  else
    {
      pack_flag = FALSE;
      unpack_els = ER_unpack_els (op);
      el_type = ER_NM__error;
    }
  if (dim > 1)
    {
      /* Process recursively  */
      dim--;
      if (pack_flag && el_type != ER_NM_vect)
	eval_error (vecform_bc_decl, get_left_operand_pos (),
		    DERR_vector_form_type, depth);
      for (i = 0; i < len; i++)
	{
	  if (pack_flag)
	    v = ((ER_node_t *) pack_els) [i];
	  else
	    {
	      v = IVAL (unpack_els, i);
	      if (ER_NODE_MODE (v) != ER_NM_vect)
		eval_error (vecform_bc_decl, get_left_operand_pos (),
			    DERR_vector_form_type, depth);
	      v = ER_vect (v);
	    }
	  process_fold_vect_op (res, v, dim, depth + 1);
	}
    }
  oper = BC_NODE_MODE (cpc);
  if (pack_flag)
    {
      switch (oper)
	{
	case BC_NM_fold_add:
	  execute_pack_fold_op (res, 0, op, el_type, len, pack_els,
				execute_plus_op, i_plus, f_plus);
	  break;
	case BC_NM_fold_mult:
	  execute_pack_fold_op (res, 1, op, el_type, len, pack_els,
				execute_mult_op, i_mult, f_mult);
	  break;
	case BC_NM_fold_and:
	  execute_pack_fold_op (res, ~ (rint_t) 0, op, el_type, len, pack_els,
				execute_and_op, i_and, NULL);
	  break;
	case BC_NM_fold_xor:
	  execute_pack_fold_op (res, 0, op, el_type, len, pack_els,
				execute_xor_op, i_xor, NULL);
	  break;
	case BC_NM_fold_or:
	  execute_pack_fold_op (res, 0, op, el_type, len, pack_els,
				execute_or_op, i_or, NULL);
	  break;
	default:
	  d_unreachable ();
      }
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  l = *(val_t *) IVAL (unpack_els, i);
	  switch (oper)
	    {
	    case BC_NM_fold_add:
	      execute_plus_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    case BC_NM_fold_mult:
	      execute_mult_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    case BC_NM_fold_and:
	      execute_and_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    case BC_NM_fold_xor:
	      execute_xor_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    case BC_NM_fold_or:
	      execute_or_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    default:
	      d_unreachable ();
	    }
	}
    }
}

/* Implement folding vector operation defined by CPC.  */
void
fold_vect_op (ER_node_t res, ER_node_t op)
{
  int dim;

  dim = get_dim (op);
  if (ER_NODE_MODE (op) == ER_NM_vect && dim > 0)
    op = ER_vect (op);
  else
    eval_error (vecform_bc_decl,
		BC_pos2 (BC_info (cpc)), DERR_vector_form_type, 1);
  ER_SET_MODE (res, ER_NM_int);
  switch (BC_NODE_MODE (cpc))
    {
    case BC_NM_fold_add:
    case BC_NM_fold_xor:
    case BC_NM_fold_or:
      ER_set_i (res, 0);
      break;
    case BC_NM_fold_mult:
      ER_set_i (res, 1);
      break;
    case BC_NM_fold_and:
      ER_set_i (res, ~ (rint_t) 0);
      break;
    default:
      d_unreachable ();
    }
  process_fold_vect_op (res, op, dim, 1);
}

/* Macro for possible switch the process or to do GC.  Remeber cpc
   must be correct for resuming the current process lately.  For
   correct work of JIT, this macro should be the last for byte code
   execution as the long jump might be from it.  */
#define INTERRUPT_CHECK\
  do {\
    if (++executed_stmts_count >= 0) {			\
      interrupt (cpc); 		       			\
    }				       			\
  } while (0)

void
vec (ER_node_t res, ER_node_t op1, rint_t vect_parts_number)
{
  /* If you make a change here, please look at DINO read
     functions. */
  ER_node_t vect, saved_ctop;
  rint_t curr_vect_part_number;
  rint_t curr_vect_element_number;
  size_t el_type_size;
  size_t els_number;
  rint_t repetition;
  int pack_flag;
	    
  if (vect_parts_number == 0)
    vect = create_empty_vector ();
  else
    {
      saved_ctop = ctop;
      ctop = op1;
      DECR_CTOP (-2 * vect_parts_number);
      els_number = 0;
      for (curr_vect_part_number = 0;
	   curr_vect_part_number < 2 * vect_parts_number;
	   curr_vect_part_number += 2)
	{
	  implicit_int_conversion (IVAL (op1, curr_vect_part_number),
				   NULL);
	  if (doubt (ER_NODE_MODE (IVAL (op1, curr_vect_part_number))
		     != ER_NM_int))
	    eval_error (optype_bc_decl, get_cpos (),
			DERR_elist_repetition_type);
	  else if (ER_i (IVAL (op1, curr_vect_part_number)) > 0)
	    els_number += ER_i (IVAL (op1, curr_vect_part_number));
	}
      pack_flag = TRUE;
      for (curr_vect_part_number = 3;
	   curr_vect_part_number < 2 * vect_parts_number;
	   curr_vect_part_number += 2)
	if (ER_NODE_MODE (IVAL (op1, 1))
	    != ER_NODE_MODE (IVAL (op1, curr_vect_part_number)))
	  {
	    pack_flag = FALSE;
	    break;
	  }
      if (pack_flag)
	{
	  el_type_size
	    = type_size_table [ER_NODE_MODE (IVAL (op1, 1))];
	  vect = create_pack_vector (els_number,
				     ER_NODE_MODE (IVAL (op1, 1)));
	}
      else
	vect = create_unpack_vector (els_number);
      ctop = saved_ctop;
      for (curr_vect_element_number = 0,
	     curr_vect_part_number = 0;
	   curr_vect_part_number < 2 * vect_parts_number;
	   curr_vect_part_number += 2)
	if (ER_i (IVAL (op1, curr_vect_part_number)) > 0)
	  {
	    repetition = ER_i (IVAL (op1, curr_vect_part_number));
	    while (repetition > 0)
	      {
		if (pack_flag)
		  memcpy
		    (ER_pack_els (vect)
		     + el_type_size * curr_vect_element_number,
		     (char *) IVAL (op1, curr_vect_part_number + 1)
		     + val_displ_table
		     [ER_NODE_MODE
		      (IVAL (op1, curr_vect_part_number + 1))],
		     el_type_size);
		else
		  memcpy (IVAL (ER_unpack_els (vect),
				curr_vect_element_number),
			  IVAL (op1, curr_vect_part_number + 1),
			  sizeof (val_t));
		repetition--;
		curr_vect_element_number++;
	      }
	  }
      d_assert
	((unsigned_rint_t) curr_vect_element_number == els_number);
    }
  ER_SET_MODE (res, ER_NM_vect);
  set_vect_dim (res, vect, 0);
}

void
tab (ER_node_t res, ER_node_t op1, rint_t tab_els_number)
{
  ER_node_t tab, saved_ctop;
  rint_t curr_tab_el_number;
  ER_node_t entry;
  
  saved_ctop = ctop;
  ctop = op1;
  DECR_CTOP (-2 * tab_els_number);
  tab = create_tab (tab_els_number);
  for (curr_tab_el_number = 0;
       curr_tab_el_number < 2 * tab_els_number;
       curr_tab_el_number += 2)
    {
      entry = find_tab_el (tab, IVAL (op1, curr_tab_el_number), TRUE);
      d_assert (entry != NULL);
      if (ER_NODE_MODE (entry) != ER_NM_empty_el)
	eval_error (keyvalue_bc_decl, get_cpos (), DERR_repeated_key,
		    (long long) curr_tab_el_number);
      *(val_t *) entry = *(val_t *) IVAL (op1, curr_tab_el_number);
      make_immutable (entry);
      *((val_t *) entry + 1)
	= *(val_t *) IVAL (op1, curr_tab_el_number + 1);
      d_assert (ER_IS_OF_TYPE (INDEXED_EL_VAL (entry, 0), ER_NM_val));
    }
  ctop = saved_ctop;
  ER_SET_MODE (res, ER_NM_tab);
  ER_set_tab (res, tab);
}

static void
out (void)
{
  int i;
  
  for (i = 0; i < BC_op1 (cpc); i++)
    heap_pop ();
}

int
throw (ER_node_t op1)
{
  const char *message;
  ER_node_t op2;
  val_t tvar1;

  op2 = (ER_node_t) &tvar1;
  ER_SET_MODE (op2, ER_NM_code);
  ER_set_code_id (op2, CODE_ID (except_bc_decl));
  ER_set_code_context (op2, uppest_stack);
  if (! ER_IS_OF_TYPE (op1, ER_NM_stack)
      || ! internal_isa_call (&message, op2, op1))
    eval_error (optype_bc_decl, get_cpos (),
		DERR_no_exception_after_throw);
  exception_position = get_cpos ();
  find_catch_pc (ER_stack (op1));
  return cpc == NULL;
}

static int
except (void)
{
  ER_node_t op1, op2;
  ER_node_t exception;
  const char *message;
	    
  extract_op2 (&op1, &op2);
  d_assert (ER_IS_OF_TYPE (op1, ER_NM_stack));
  exception = ER_stack (op1);
  if (ER_IS_OF_TYPE (op2, ER_NM_code)
      && internal_isa_call (&message, op2, op1))
    {
      INCREMENT_PC ();
      d_assert (cpc != NULL
		&& BC_IS_OF_TYPE (cpc, BC_NM_block));
      TOP_DOWN; /* exception */
      heap_push (cpc, cstack, 1);
      /* Zeroth val of catch block is always corresponding the
	 exception. */
      ER_SET_MODE (IVAL (ER_stack_vars (cstack), 0), ER_NM_stack);
      ER_set_stack (IVAL (ER_stack_vars (cstack), 0), exception);
      INCREMENT_PC ();
    }
  else
    {
      /* Given catch does not correspond to the exception -- try
	 the next catch. */
      cpc = BC_next_except (cpc);
      if (cpc == NULL)
	{
	  /* No more catches - go to covered try-blocks. */
	  find_catch_pc (ER_stack (op1));
	  return cpc == NULL;
	}
    }
  return FALSE;
}

void
evaluate_code (void)
{
  BC_node_mode_t node_mode;

  /* Check that all real executed byte code can be stored in unsigned
     char.  */
  d_assert ((int) BC_NM_nop < 256);
  for (;;)
    {
      node_mode = BC_NODE_MODE (cpc);
      switch ((unsigned char) node_mode)
	{
#ifdef __GNUC__
	  /* This is a trick to make switch faster by removing range
	     comparison.  */
	case 0:
	case 255:
	  abort ();
#endif
	case BC_NM_stvt:
	  stvt (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_stvtu:
	  stvtu (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_sts:
	  sts (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_stsu:
	  stsu (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ste:
	  ste (get_op (BC_op1 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_steu:
	  steu (get_op (BC_op1 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ldnil:
	  ldnil (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldthis:
	  ldthis (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldch:
	  ldch (get_op (BC_op1 (cpc)), BC_op2 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldi:
	  ldi (get_op (BC_op1 (cpc)), BC_op2 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldbi:
	  ldbi (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldl:
	  ldl (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldf:
	  ldf (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lds:
	  lds (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldtp:
	  ldtp (get_op (BC_op1 (cpc)), BC_op2 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_flat:
	  flat (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fld:
	  fld (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lfld:
	  lfld (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lfldv:
	  lfldv (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_brts:
	  if (brts (get_op (BC_op1 (cpc)), get_op (BC_res (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  break;
	case BC_NM_brfs:
	  if (brfs (get_op (BC_op1 (cpc)), get_op (BC_res (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  break;
	case BC_NM_lconv:
	  lconv (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_in:
	  in (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_not:
	  not (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_inot:
	  inot (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_bnot:
	  bnot (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ibnot:
	  ibnot (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fold_add:
	case BC_NM_fold_mult:
	case BC_NM_fold_and:
	case BC_NM_fold_xor:
	case BC_NM_fold_or:
	  foldop (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_eq:
	  eq (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ieq:
	  ieq (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_eqi:
	  eqi (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ieqi:
	  ieqi (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ne:
	  ne (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ine:
	  ine (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_nei:
	  nei (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_inei:
	  inei (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_id:
	  id (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_unid:
	  unid (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lt:
	  lt (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ilt:
	  ilt (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lti:
	  lti (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ilti:
	  ilti (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ge:
	  ge (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ige:
	  ige (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_gei:
	  gei (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_igei:
	  igei (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_gt:
	  gt (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_igt:
	  igt (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_gti:
	  gti (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_igti:
	  igti (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_le:
	  le (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ile:
	  ile (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lei:
	  lei (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ilei:
	  ilei (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_plus:
	  plus (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_iplus:
	  iplus (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fplus:
	  fplus (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_minus:
	  minus (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_iminus:
	  iminus (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fminus:
	  fminus (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_length:
	  length (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_const:
	  constop (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_new:
	  new (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_tpof:
	  tpof (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_chof:
	  chof (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_iof:
	  iof (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lof:
	  lof (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fof:
	  fof (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fmtvecof:
	  fmtvecof (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_vecof:
	  vecof (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_tabof:
	  tabof (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_vec:
	  vec (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_tab:
	  tab (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ind:
	  ind (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ind2:
	  ind2 (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)),
		get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_sl:
	  sl (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lslv:
	  lslv (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_call:
	  call (get_op (BC_op1 (cpc)), BC_op2 (cpc), FALSE);
	  break;
	case BC_NM_tcall:
	  tcall (get_op (BC_op1 (cpc)), BC_op2 (cpc), FALSE);
	  break;
	case BC_NM_icall:
	  icall (get_op (BC_op1 (cpc)), BC_op2 (cpc),
		 BC_vars_num (BC_cfblock (cpc)), FALSE);
	  break;
	case BC_NM_itcall:
	  itcall (get_op (BC_op1 (cpc)), BC_op2 (cpc),
		  BC_vars_num (BC_cfblock (cpc)), FALSE);
	  break;
	case BC_NM_cicall:
	  cicall (get_op (BC_op1 (cpc)), BC_op2 (cpc),
		  BC_vars_num (BC_cfblock (cpc)), FALSE);
	  break;
	case BC_NM_citcall:
	  citcall (get_op (BC_op1 (cpc)), BC_op2 (cpc),
		   BC_vars_num (BC_cfblock (cpc)), FALSE);
	  break;
	case BC_NM_ticall:
	  ticall (get_op (BC_op1 (cpc)), BC_op2 (cpc),
		  BC_vars_num (BC_cfblock (cpc)), FALSE);
	  break;
	case BC_NM_titcall:
	  titcall (get_op (BC_op1 (cpc)), BC_op2 (cpc),
		   BC_vars_num (BC_cfblock (cpc)), FALSE);
	  break;
	case BC_NM_ibcall:
	  ibcall (get_op (BC_op1 (cpc)), BC_op2 (cpc), FALSE);
	  break;
	case BC_NM_mcall:
	  mcall (get_op (BC_op1 (cpc)), BC_op2 (cpc),
		 get_op (BC_op3 (cpc)), BC_op4 (cpc), FALSE);
	  break;
	case BC_NM_add:
	  add (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_addi:
	  addi (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_sub:
	  sub (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_mult:
	  mult (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_multi:
	  multi (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_div:
	  divop (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_mod:
	  mod (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_madd:
	  madd (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)),
		get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_concat:
	  concat (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lsh:
	  lsh (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_rsh:
	  rsh (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ash:
	  ash (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_and:
	  and (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_xor:
	  xor (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_or:
	  or (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_iadd:
	  iadd (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fadd:
	  fadd (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ifadd:
	  ifadd (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_iaddi:
	  iaddi (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_faddi:
	  faddi (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_isub:
	  isub (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fsub:
	  fsub (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ifsub:
	  ifsub (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fisub:
	  fisub (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_imult:
	  imult (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fmult:
	  fmult (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ifmult:
	  ifmult (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_imulti:
	  imulti (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fmulti:
	  fmulti (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_idiv:
	  idiv (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fdiv:
	  fdiv (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ifdiv:
	  ifdiv (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fidiv:
	  fidiv (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_imod:
	  imod (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fmodop:
	  fmodop (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ilsh:
	  ilsh (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_irsh:
	  irsh (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_iash:
	  iash (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_iand:
	  iand (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ixor:
	  ixor (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ior:
	  ior (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_add_st:
	  add_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_sub_st:
	  sub_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_mult_st:
	  mult_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_div_st:
	  div_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_mod_st:
	  mod_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_concat_st:
	  concat_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_lsh_st:
	  lsh_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_rsh_st:
	  rsh_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ash_st:
	  ash_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_and_st:
	  and_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_xor_st:
	  xor_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_or_st:
	  or_st (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_mult_slst:
	case BC_NM_div_slst:
	case BC_NM_mod_slst:
	case BC_NM_add_slst:
	case BC_NM_sub_slst:
	case BC_NM_concat_slst:
	case BC_NM_lsh_slst:
	case BC_NM_rsh_slst:
	case BC_NM_ash_slst:
	case BC_NM_and_slst:
	case BC_NM_xor_slst:
	case BC_NM_or_slst:
	  op_slst (get_op (BC_op1 (cpc)), BC_op2 (cpc), get_op (BC_op3 (cpc)), get_op (BC_op4 (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_slst:
	  slst (get_op (BC_op1 (cpc)), BC_op2 (cpc), get_op (BC_op3 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_b:
	  b ();
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btdef:
	  /* Branch if defined  */
	  if (btdef (get_op (BC_op1 (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_bf:
	  if (bf (get_op (BC_op1 (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibf:
	  if (ibf (get_op (BC_op1 (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_bfni:
	  if (bfni (get_op (BC_op1 (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  break;
	case BC_NM_ibfni:
	  if (ibfni (get_op (BC_op1 (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  break;
	case BC_NM_bteqinc:
	  if (bteqinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
		       BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btneinc:
	  if (btneinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
		       BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btgeinc:
	  if (btgeinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
		       BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btltinc:
	  if (btltinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
		       BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btleinc:
	  if (btleinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
		       BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btgtinc:
	  if (btgtinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
		       BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibteqinc:
	  if (ibteqinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
			BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtneinc:
	  if (ibtneinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
			BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtgeinc:
	  if (ibtgeinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
			BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtltinc:
	  if (ibtltinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
			BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtleinc:
	  if (ibtleinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
			BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtgtinc:
	  if (ibtgtinc (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc),
			BC_binc_inc (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_bteq:
	  if (bteq (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btne:
	  if (btne (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btge:
	  if (btge (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btlt:
	  if (btlt (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btle:
	  if (btle (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btgt:
	  if (btgt (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibteq:
	  if (ibteq (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtne:
	  if (ibtne (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtge:
	  if (ibtge (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtlt:
	  if (ibtlt (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtle:
	  if (ibtle (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtgt:
	  if (ibtgt (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_fbteq:
	  if (fbteq (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_fbtne:
	  if (fbtne (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_fbtge:
	  if (fbtge (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_fbtlt:
	  if (fbtlt (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_fbtle:
	  if (fbtle (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_fbtgt:
	  if (fbtgt (get_op (BC_op1 (cpc)), get_op (BC_bcmp_op2 (cpc)), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_bteqi:
	  if (bteqi (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btnei:
	  if (btnei (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btlti:
	  if (btlti (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btlei:
	  if (btlei (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btgti:
	  if (btgti (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btgei:
	  if (btgei (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibteqi:
	  if (ibteqi (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtnei:
	  if (ibtnei (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtlti:
	  if (ibtlti (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtlei:
	  if (ibtlei (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtgti:
	  if (ibtgti (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibtgei:
	  if (ibtgei (get_op (BC_op1 (cpc)), BC_bcmp_op2 (cpc), BC_bcmp_res (cpc)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_bt:
	  if (bt (get_op (BC_op1 (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ibt:
	  if (ibt (get_op (BC_op1 (cpc))))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_foreach:
	  if (foreach (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc))))
	    cpc = BC_body_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_foreach2:
	  if (foreach2 (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), get_op (BC_op3 (cpc)),
			get_op (BC_element (cpc))))
	    cpc = BC_body_pc (cpc);
	  else
	    INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_out:
	  out ();
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_bend:
          if (bend ())
	    return;
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_stpop:
	  stpop (BC_op1 (cpc), BC_op2 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_leave:
	  if (leave ())
	    return;
	  break;
	case BC_NM_fbend:
	  if (fbend ())
	    return;
	  break;
	case BC_NM_ret:
	  if (ret (get_op (BC_op1 (cpc))))
	    return;
	  break;
	case BC_NM_wait:
	  wait (get_op (BC_op1 (cpc)));
	  break;
	case BC_NM_waitend:
	  waitend ();
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_stinc:
	  stinc (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)), BC_op3 (cpc));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_stdecm:
	  stdecm (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_stdecu:
	  stdecu (get_op (BC_op1 (cpc)), BC_op2 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_block:
	  block ();
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_throw:
	  if (throw (get_op (BC_op1 (cpc))))
	    return;
	  break;
	case BC_NM_except:
	  if (except ())
	    return;
	  break;
	case BC_NM_move:
	  move (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_imove:
	  imove (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fmove:
	  fmove (get_op (BC_op1 (cpc)), get_op (BC_op2 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_var:
	  var (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lvar:
	  lvar (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lvarv:
	  lvarv (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_evar:
	  evar (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_levar:
	  levar (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_levarv:
	  levarv (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_efun:
	  efun (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_fun:
	case BC_NM_class:
	  funclass (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_rpr:
	  rpr (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_rpr_def:
	  rpr_def (get_op (BC_op1 (cpc)));
	  INCREMENT_PC ();
	  break;
	case BC_NM_nop:
	  /* for DINO developing purposes only.  It should be absent
	     in unoptimized code.  */
	  nop ();
	  INCREMENT_PC ();
	  break;
	default:
	  d_unreachable ();
	}
    }
}

static void
initiate_vars (void)
{
  ER_node_t var;
  ER_node_t vect, tab, string, string2;
  ER_node_t entry;
  val_t key;
  int i, j;
  
  /* Set argv. */
  if (program_arguments_number == 0)
    vect = create_empty_vector ();
  else
    {
      vect = create_unpack_vector (program_arguments_number);
      for (i = 0; i < program_arguments_number; i++)
	{
	  string = create_string (program_arguments [i]);
	  var = IVAL (ER_unpack_els (vect), i);
	  ER_SET_MODE (var, ER_NM_vect);
	  set_vect_dim (var, string, 0);
	}
    }
  d_assert (BC_decl_scope (argv_bc_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack), BC_var_num (argv_bc_decl));
  ER_SET_MODE (var, ER_NM_vect);
  set_vect_dim (var, vect, 0);
  ER_set_immutable (vect, TRUE);
  /* Set env. */
  for (i = 0; program_environment [i] != NULL; i++)
    ;
  tab = create_tab (i);
  for (i = 0; program_environment [i] != NULL; i++)
    {
      for (j = 0; program_environment [i][j] != '\0'; j++)
	if (program_environment [i][j] == '=')
	  break;
      if (program_environment [i][j] == '\0')
	eval_error (invenv_bc_decl, no_position, DERR_environment_corrupted);
      program_environment [i][j] = '\0';
      string = create_string (program_environment [i]);
      program_environment [i][j] = '=';
      string2 = create_string (program_environment [i] + j + 1);
      ER_SET_MODE ((ER_node_t) &key, ER_NM_vect);
      set_vect_dim ((ER_node_t) &key, string, 0);
      entry = find_tab_el (tab, (ER_node_t) &key, TRUE);
      d_assert (ER_NODE_MODE (tab) != ER_NM_heap_redir);
      if (ER_NODE_MODE (entry) != ER_NM_empty_el)
	eval_error (invenv_bc_decl, no_position, DERR_environment_corrupted);
      ER_SET_MODE (entry, ER_NM_vect);
      set_vect_dim (entry, string, 0);
      make_immutable (entry);
      var = (ER_node_t) ((char *) entry + sizeof (val_t));
      ER_SET_MODE (var, ER_NM_vect);
      set_vect_dim (var, string2, 0);
   }
  d_assert (BC_decl_scope (env_bc_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack), BC_var_num (env_bc_decl));
  ER_SET_MODE (var, ER_NM_tab);
  ER_set_tab (var, tab);
  ER_set_immutable (tab, TRUE);
  /* Set version */
  d_assert (BC_decl_scope (version_bc_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack), BC_var_num (version_bc_decl));
  ER_SET_MODE (var, ER_NM_float);
  ER_set_f (var, DINO_VERSION);
  /* Set main_thread, curr_thread */
  d_assert (BC_decl_scope (main_thread_bc_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack),
	      BC_var_num (main_thread_bc_decl));
  ER_SET_MODE (var, ER_NM_process);
  ER_set_process (var, cprocess);
  d_assert (BC_decl_scope (curr_thread_bc_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack),
	      BC_var_num (curr_thread_bc_decl));
  ER_SET_MODE (var, ER_NM_process);
  ER_set_process (var, cprocess);
}

#ifdef __GNUC__
static void restart_eval (void) __attribute__ ((noreturn));
#endif

/* Restart top level evaluator.  */
static void
restart_eval (void)
{
  /* We need to remove all c_stack_p flags as we jump through
     generated C functions.  */
  clear_c_stack_flags ();
  longjmp (eval_longjump_buff, 1);
}

#define MAX_EVAL_ERROR_MESSAGE_LENGTH 300

void
eval_error (BC_node_t except_class_block,
	    position_t position, const char *format, ...)
{
  char message[MAX_EVAL_ERROR_MESSAGE_LENGTH + 1];
  va_list arguments;
  ER_node_t error_instance, string;

  d_assert (eval_long_jump_set_flag);
  va_start (arguments, format);
  vsprintf (message, format, arguments);
  va_end (arguments);
  d_assert (strlen (message) <= MAX_EVAL_ERROR_MESSAGE_LENGTH);
  exception_position = position;
  string = create_string (message);
  heap_push (except_class_block, uppest_stack, -1);
  error_instance = cstack;
  heap_pop ();
  /* Zeroth variable is message in class `error' */
  ER_SET_MODE (IVAL (ER_stack_vars (error_instance), 0), ER_NM_vect);
  set_vect_dim (IVAL (ER_stack_vars (error_instance), 0), string, 0);
  find_catch_pc (error_instance);
  restart_eval ();
}

void
call_fun_class (BC_node_t code, ER_node_t context, int pars_number,
		int from_c_code_p)
{
  pc_t saved_cpc;
  pc_t saved_next_pc;
  int saved_process_number;

  saved_cpc = cpc;
  saved_next_pc = BC_next (cpc);
  BC_set_next (cpc, NULL);
  saved_process_number = ER_process_number (cprocess);
  DECR_CTOP (pars_number);
  if (BC_implementation_fun (code) != NULL)
    process_imm_ifun_call (code, pars_number, from_c_code_p);
  else
    process_imm_fun_call ((val_t *) IVAL (ctop, 1), code, context,
			  pars_number, BC_vars_num (code),
			  FALSE, from_c_code_p);
  for (;;)
    {
      if (cpc != NULL)
	evaluate_code ();
      if (saved_process_number != ER_process_number (cprocess))
	delete_cprocess ();
      else
	break;
    }
  cpc = saved_cpc;
  BC_set_next (cpc, saved_next_pc);
}

static ticker_t all_time_ticker;


/* Switch to byte code execution: CPC should be at the right place
   (first executed stmt will be CPC).  */
void
switch_to_bcode (void)
{
  restart_eval ();
}

/* Evaluate top level block START_PC.  Initiate data if INIT_P.
   Otherwise, reuse the already created data.  */
void
evaluate_program (pc_t start_pc, int init_p, int last_p)
{
  /* The first statement is always block. */
  d_assert (start_pc != NULL && BC_NODE_MODE (start_pc) == BC_NM_block);
  cpc = start_pc;
  d_assert (! BC_ext_life_p (cpc));
  if (! init_p)
    expand_uppest_stack ();
  else
    {
      initiate_heap ();
      initiate_int_tables ();
      initiate_tables ();
      initiate_funcs ();
      sync_flag = FALSE;
      create_uppest_stack (cpc);
      initiate_processes (cpc);
      /* Initialized standard variables.  It should be after
	 initiate_process to set up main thread var.  */
      initiate_vars ();
#ifndef NO_PROFILE
      if (profile_flag)
	{
	  struct itimerval itimer;
      
	  all_time_ticker = create_ticker ();
#if HAVE_SETITIMER
	  itimer.it_value.tv_sec = itimer.it_interval.tv_sec = 0;
	  itimer.it_value.tv_usec = itimer.it_interval.tv_usec = 1;
	  if (setitimer (ITIMER_VIRTUAL, &itimer, NULL))
	    abort ();
#endif
	}
#endif
    }
  INCREMENT_PC ();
  while (setjmp (eval_longjump_buff))
    ;
  if (cpc == NULL)
    {
      /* Uncatched exception in REPL or through C stack. */
      d_assert (repl_flag);
      return;
    }
  eval_long_jump_set_flag = TRUE;
  for (;;)
    {
      evaluate_code ();
      if (repl_flag && ! last_p && cstack == uppest_stack)
	{
	  eval_long_jump_set_flag = FALSE;
	  return;
	}
      delete_cprocess ();
    }
  d_unreachable ();
}

#ifndef NO_PROFILE

/* Array of pointers to functions which will be reported in profile. */
static vlo_t profile_funcs;

/* The following recursive function collects functions which will be
   reported in profile. */
static void
collect_profile (BC_node_t block)
{
  BC_node_t decl, fblock;

  for (decl = BC_decls (block); decl != NULL; decl = BC_next_decl (decl))
    if (BC_IS_OF_TYPE (decl, BC_NM_fdecl)
	&& strcmp (BC_pos (decl).file_name,
		   ENVIRONMENT_PSEUDO_FILE_NAME) != 0)
      {
	fblock = BC_fblock (decl);
	VLO_ADD_MEMORY (profile_funcs, &fblock, sizeof (decl));
	collect_profile (fblock);
      }
}

/* The following comparison function is used to sort collected
   functions being reported in profile. */
static int
profile_compare_function (const void *el1, const void *el2)
{
  BC_node_t func1 = *(BC_node_t *) el1;
  BC_node_t func2 = *(BC_node_t *) el2;
  double time1 ATTRIBUTE_UNUSED, time2 ATTRIBUTE_UNUSED;

#if HAVE_SETITIMER
  return BC_interrupts_number (func2) - BC_interrupts_number (func1);
#else
  time1 = active_time (BC_exec_time (func1));
  time2 = active_time (BC_exec_time (func2));
  
  if (time1 < time2)
    return 1;
  else if (time1 > time2)
    return -1;
  else
    return 0;
#endif
}

/* The following function prints execition profile. */
void
print_profile (BC_node_t block)
{
  BC_node_t *fun_ptr;
  double all_time = active_time (all_time_ticker);
  double fun_time;
#if !HAVE_SETITIMER
  double gc_time = active_time (gc_ticker);
#endif
	
  ticker_off (&all_time_ticker);
  VLO_CREATE (profile_funcs, 0);
  collect_profile (block);
  qsort (VLO_BEGIN (profile_funcs),
	 VLO_LENGTH (profile_funcs) / sizeof (BC_node_t), sizeof (BC_node_t),
	 profile_compare_function);
  fprintf
    (stderr,
#if !HAVE_SETITIMER
     "\n** Calls ** In Time ** Name **************************************\n"
#else
     "\n** Calls *** Time **** Name **************************************\n"
#endif
     );
  for (fun_ptr = (BC_node_t *) VLO_BEGIN (profile_funcs);
       (char *) fun_ptr <= (char *) VLO_END (profile_funcs);
       fun_ptr++)
    {
      BC_node_t fdecl = BC_fdecl (*fun_ptr);

#if !HAVE_SETITIMER
      fun_time = active_time (BC_exec_time (*fun_ptr));
      if (gc_number != 0 && gc_time != 0.0 && gc_time > fun_time)
	{
	  fprintf (stderr, "%8u %8.2f  --  * garbage collection *\n",
		   gc_number, gc_time);
	  gc_time = 0.0;
	}
#else
      fun_time = (all_interrupts_number == 0
		  ? 0.0
		  : (all_time * BC_interrupts_number (*fun_ptr)
		     / all_interrupts_number));
      if (gc_interrupts_number != 0
	  && gc_interrupts_number > BC_interrupts_number (*fun_ptr))
	{
	  fprintf (stderr, "%8u %8.2f  --  * garbage collection *\n",
		   gc_number,
		   all_time * gc_interrupts_number / all_interrupts_number);
	  gc_interrupts_number = 0;
	}
	
#endif
      fprintf (stderr, "%8d %8.2f  --  %s: \"%s\": %d\n",
	       BC_calls_number (*fun_ptr), fun_time, BC_ident (fdecl),
	       BC_pos (fdecl).file_name, BC_pos (fdecl).line_number);
    }
#if !HAVE_SETITIMER
  if (gc_number != 0 && gc_time != 0.0)
    fprintf (stderr, "%8u %8.2f  --  * garbage collection *\n",
	     gc_number, gc_time);
#else
  if (gc_interrupts_number != 0)
    fprintf (stderr, "%8u %8.2f  --  * garbage collection *\n",
	     gc_number,
	     all_time * gc_interrupts_number / all_interrupts_number);
#endif
  fprintf (stderr, "         %8.2f  --  All Program\n", all_time);
  VLO_DELETE (profile_funcs);
}
#endif
