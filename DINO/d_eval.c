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

static ER_node_t do_inline
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

static void do_inline
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

static void do_inline
process_var_val (ER_node_t res, BC_node_t vdecl)
{
  ER_node_t ref;

  ref = get_var_val_ref (vdecl);
  if (ER_NODE_MODE (ref) == ER_NM_undef)
    eval_error (accessop_bc_decl, get_cpos (), DERR_undefined_value_access,
		BC_ident (vdecl));
  *(val_t *) res = *(val_t *) ref;
}

static void do_inline
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

static position_t
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

static void do_inline
execute_a_period_operation (int block_decl_ident_number, ER_node_t res,
			    ER_node_t op, int lvalue_p, int lvalue_val_p)
{
  BC_node_t decl;
  BC_node_t block;
  ER_node_t container, val;

  if (ER_NODE_MODE (op) == ER_NM_stack)
    container = ER_stack (op);
  else
    eval_error (accessop_bc_decl, get_cpos (),
		DERR_value_is_not_class_instance_or_stack);
  decl = NULL;
  for (;;)
    {
      d_assert (container != NULL);
      block = ER_block_node (container);
      if (block_decl_ident_number >= 0)
	decl = LV_BLOCK_IDENT_DECL (BC_block_number (block),
				    block_decl_ident_number);
      if (decl != NULL
	  || BC_NODE_MODE (block) != BC_NM_fblock || ! BC_class_p (block))
	break;
      container = ER_context (container);
      if (BC_NODE_MODE (ER_block_node (container)) != BC_NM_fblock
	  || ! BC_class_p (ER_block_node (container)))
	break;
    }
  if (decl == NULL)
    eval_error (accessop_bc_decl, get_cpos (),
		DERR_decl_is_absent_in_given_class_or_block);
  else
    check_member_access (decl, block);

  switch ((unsigned char) BC_NODE_MODE (decl))
    {
#ifdef __GNUC__
	  /* This is a trick to make switch faster by removing range
	     comparison.  */
    case 0:
    case 255:
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
      break;
    case BC_NM_evdecl:
      process_external_var (res, decl, lvalue_p, lvalue_val_p);
      break;
    case BC_NM_fdecl:
      {
	BC_node_t fblock = BC_fblock (decl);

	if (fblock == NULL)
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

static int_t do_always_inline
check_vector_index (ER_node_t vect, ER_node_t index)
{
  int_t index_value;
  val_t tvar;

  if (ER_NODE_MODE (index) != ER_NM_int)
    {
      index = implicit_int_conversion (index, (ER_node_t) &tvar);
      if (ER_NODE_MODE (index) != ER_NM_int)
	eval_error (indextype_bc_decl, get_cpos (), DERR_index_is_not_int);
    }
  index_value = ER_i (index);
  if (index_value < 0
      || (unsigned_int_t) index_value >= ER_els_number (vect))
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

/* Load element INDEX_VAL of packed VECT into TO.  */
static void do_inline
load_packed_vector_element (ER_node_t to, ER_node_t vect, int_t index_val)
{
  ER_node_mode_t el_type;
  size_t el_type_size;

  d_assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect);
  el_type = ER_pack_vect_el_type (vect);
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
      ER_set_i (to, ((int_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_float:
      ER_set_f (to, ((floating_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_long:
      ER_set_l (to, ((ER_node_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_type:
      ER_set_type (to, ((ER_node_mode_t *) ER_pack_els (vect)) [index_val]);
      break;
    case ER_NM_vect:
      set_vect_dim (to, ((ER_node_t *) ER_pack_els (vect)) [index_val], 0);
      break;
    case ER_NM_tab:
      ER_set_tab (to, ((ER_node_t *) ER_pack_els (vect)) [index_val]);
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

static void do_inline
load_vector_element_by_index (ER_node_t to, ER_node_t vect, ER_node_t index)
{
  int_t index_val;
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
store_packed_vector_element (ER_node_t vect, int_t index_val, ER_node_t val)
{
  ER_node_mode_t el_type;
  size_t el_type_size;
  
  d_assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect);
  el_type = ER_pack_vect_el_type (vect);
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
      ((int_t *) ER_pack_els (vect)) [index_val] = ER_i (val);
      break;
    case ER_NM_float:
      ((floating_t *) ER_pack_els (vect)) [index_val] = ER_f (val);
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
  int_t index_val;
  int pack_flag;

  GO_THROUGH_REDIR (vect);
  pack_flag = ER_NODE_MODE (vect) == ER_NM_heap_pack_vect;
  if (ER_immutable (vect))
    eval_error (immutable_bc_decl, get_designator_pos (),
		DERR_immutable_vector_modification);
  index_val = check_vector_index (vect, index);
  if (pack_flag && ER_pack_vect_el_type (vect) != ER_NODE_MODE (val))
    {
      vect = unpack_vector (vect);
      pack_flag = FALSE;
    }
  if (pack_flag)
    store_packed_vector_element (vect, index_val, val);
  else
    *(val_t *) IVAL (ER_unpack_els (vect), index_val) = *(val_t *)val;
}

static void do_inline
load_table_element_by_key (ER_node_t to, ER_node_t tab, ER_node_t key)
{
  ER_node_t entry;

  GO_THROUGH_REDIR (tab);
  entry = find_tab_entry (tab, key, FALSE);
  if (ER_NODE_MODE (entry) == ER_NM_empty_entry
      || ER_NODE_MODE (entry) == ER_NM_deleted_entry)
    eval_error (keyvalue_bc_decl, get_cpos (), DERR_no_such_key);
  *(val_t *) to = *(val_t *) INDEXED_ENTRY_VAL (entry, 0);
  d_assert (ER_IS_OF_TYPE (INDEXED_ENTRY_VAL (entry, 0), ER_NM_val));
}

static void do_inline
store_table_element (ER_node_t tab, ER_node_t index, ER_node_t val)
{
  ER_node_t entry;

  GO_THROUGH_REDIR (tab);
  if (ER_immutable (tab))
    eval_error (immutable_bc_decl, get_designator_pos (),
		DERR_immutable_table_modification);
  entry = find_tab_entry (tab, index, TRUE);
  *(val_t *) entry = *(val_t *) index;
  make_immutable (entry);
  *(val_t *) INDEXED_ENTRY_VAL (entry, 0) = *(val_t *) val;
  d_assert (ER_IS_OF_TYPE (INDEXED_ENTRY_VAL (entry, 0), ER_NM_val));
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
static void do_inline
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

static void do_inline
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

static void do_inline
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
check_and_get_slice_info (ER_node_t start_val, unsigned_int_t vec_len,
			  int_t depth, int_t *start, int_t *bound,
			  int_t *step, int_t *niter)
{
  int_t start0, bound0, step0, niter0, abs_step;

  if (ER_NODE_MODE (IVAL (start_val, 0)) != ER_NM_int)
    eval_error (slicetype_bc_decl, get_designator_pos (),
		DERR_slice_start_is_not_int, depth);
  if (ER_NODE_MODE (IVAL (start_val, 1)) != ER_NM_int)
    eval_error (slicetype_bc_decl,	get_designator_pos (),
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
process_slice_extract (ER_node_t container1, ER_node_t start_val1, int_t dim1,
		       int_t depth)
{
  ER_node_t unpack_els, unpack_els1 = NULL;
  char *pack_els, *pack_els1 = NULL;
  int_t len1, start1, bound1, step1, niter1;
  int_t i, i1;
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
	  el_type1 = ER_pack_vect_el_type (vect1);
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
		((int_t *) pack_els) [i] = ((int_t *) pack_els1) [i1];
	      break;
	    case ER_NM_float:
	      for (i = 0, i1 = start1; i1 != bound1; i++, i1 += step1)
		((floating_t *) pack_els) [i] = ((floating_t *) pack_els1) [i1];
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
	    *(val_t *)IVAL (unpack_els, i1) = *(val_t *)IVAL (unpack_els1, i1);
	}
    }
  else
    {
      ER_node_t p;

      d_assert (dim1 > 1);
      if (! pack_flag1)
	unpack_els1 = ER_unpack_els (vect1);
      else if (dim1 > 1 && ER_pack_vect_el_type (vect1) != ER_NM_vect)
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
static void do_inline
slice_extract (ER_node_t res, ER_node_t container, int_t dim)
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
process_slice_assign (ER_node_t container1, ER_node_t start_val1, int_t dim1,
		      ER_node_t container2, int_t dim2, int depth)
{
  ER_node_t unpack_els1 = NULL, unpack_els2 = NULL;
  char *pack_els1 = NULL, *pack_els2 = NULL;
  int_t len1, start1, bound1, step1, niter1;
  int_t len2 = 0, niter2 = 0;
  int_t i1, i2;
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
		DERR_different_slice_operand_lengths, niter1, niter2, depth);
  if (dim1 == 1 && dim2 == 0)
    {
      if (pack_flag1 && ER_pack_vect_el_type (vect1) == ER_NODE_MODE (container2))
	{
	  pack_els1 = ER_pack_els (vect1);
	  el_type1 = ER_pack_vect_el_type (vect1);
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
		((int_t *) pack_els1) [i1] = ER_i (container2);
	      break;
	    case ER_NM_float:
	      for (i1 = start1; i1 != bound1; i1 += step1)
		((floating_t *) pack_els1) [i1] = ER_f (container2);
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
	  && ER_pack_vect_el_type (vect1) != ER_pack_vect_el_type (vect2))
	{
	  d_assert (pack_flag2);
	  pack_flag1 = pack_flag2 = FALSE;
	  vect1 = unpack_vector (vect1);
	  vect2 = unpack_vector (vect2);
	}
      if (pack_flag1)
	{
	  pack_els1 = ER_pack_els (vect1);
	  el_type1 = ER_pack_vect_el_type (vect1);
	  pack_els2 = ER_pack_els (vect2);
	  d_assert (el_type1 == ER_pack_vect_el_type (vect2));
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
		((int_t *) pack_els1) [i1] = ((int_t *) pack_els2) [i2];
	      break;
	    case ER_NM_float:
	      for (i1 = start1, i2 = 0; i1 != bound1; i1 += step1, i2++)
		((floating_t *) pack_els1) [i1]
		  = ((floating_t *) pack_els2) [i2];
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
      else if (dim1 > 1 && ER_pack_vect_el_type (vect1) != ER_NM_vect)
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
      else if (dim1 > 1 && ER_pack_vect_el_type (vect1) != ER_NM_vect)
	eval_error (sliceform_bc_decl, get_designator_pos (),
		    DERR_slice_operand_form, depth);
      else
	pack_els1 = ER_pack_els (vect1);
      if (! pack_flag2)
	unpack_els2 = ER_unpack_els (vect2);
      else if (dim2 > 1 && ER_pack_vect_el_type (vect2) != ER_NM_vect)
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
static void do_inline
slice_assign (ER_node_t container, int_t dim, ER_node_t val)
{
  int_t val_dim = 0;

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

/* Find PC to process EXCEPT.  If it is not found, finish program or
   return NULL in case of REPL.  */
static pc_t
find_catch_pc (ER_node_t except)
{
  BC_node_t block;
  ER_node_t message;
  struct trace_stack_elem elem;
  val_t v1, v2;

  if (trace_flag)
    VLO_NULLIFY (trace_stack);
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
      /* Set up ctop as it should be for any statement begin.  */
      if (cstack != NULL)
	ctop = (ER_node_t) ((char *) cvars
			    + real_block_vars_number (ER_block_node (cstack))
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
	  return cpc;
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
	      && ER_pack_vect_el_type (vect) == ER_NM_char)
	    /* No return after error. */
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
  return NULL;
}

/* The following three functions return TRUE if OP1 and OP2 are
   correspondingly int, float, long, or slice type.  */
static int do_always_inline
int_bin_op (ER_node_t op1, ER_node_t op2)
{
  return ER_NODE_MODE (op1) == ER_NM_int && ER_NODE_MODE (op2) == ER_NM_int;
}

static int do_always_inline
float_bin_op (ER_node_t op1, ER_node_t op2)
{
  return ER_NODE_MODE (op1) == ER_NM_float && ER_NODE_MODE (op2) == ER_NM_float;
}

static int do_always_inline
long_bin_op (ER_node_t op1, ER_node_t op2)
{
  return ER_NODE_MODE (op1) == ER_NM_long && ER_NODE_MODE (op2) == ER_NM_long;
}

static int do_always_inline
vect_bin_op (ER_node_t op1, ER_node_t op2)
{
  return ((ER_NODE_MODE (op1) == ER_NM_vect && ER_dim (op1) != 0)
	  || (ER_NODE_MODE (op2) == ER_NM_vect && ER_dim (op2) != 0));
}

static void binary_vect_op (ER_node_t, ER_node_t, ER_node_t);

/* Define inline functions used to do actual different type
   binary operations.  */
#define ifunc(name) static do_always_inline int_t name (int_t a, int_t b)
#define ffunc(name) static do_always_inline floating_t name (floating_t a, floating_t b)
#define icmpf(name) static do_always_inline int name (int_t a, int_t b)
#define fcmpf(name) static do_always_inline int name (floating_t a, floating_t b)
ifunc (iplus) { return a + b;}
ifunc (iminus) { return a - b;}
ifunc (imult) { return a * b;}
ifunc (idiv) { return a / b;}
ifunc (imod) { return a % b;}
ifunc (iand) { return a & b;}
ifunc (ixor) { return a ^ b;}
ifunc (ior) { return a | b;}
ifunc (ilshift) { return a << b;}
ifunc (irshift) { return (unsigned_int_t) a >> b;}
ifunc (iashift) { return a >> b;}
icmpf (ieq) { return a == b;}
icmpf (ine) { return a != b;}
icmpf (ilt) { return a < b;}
icmpf (ile) { return a <= b;}
icmpf (igt) { return a > b;}
icmpf (ige) { return a >= b;}
ffunc (fplus) { return a + b;}
ffunc (fminus) { return a - b;}
ffunc (fmult) { return a * b;}
ffunc (fdiv) { return a / b;}
ffunc (frem) { return fmod (a, b);}
fcmpf (feq) { return a == b;}
fcmpf (fne) { return a != b;}
fcmpf (flt) { return a < b;}
fcmpf (fle) { return a <= b;}
fcmpf (fgt) { return a > b;}
fcmpf (fge) { return a >= b;}

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

/* Return true if OP1 is a slice.  */
static int do_always_inline
vect_unary_op (ER_node_t op1)
{
  return ER_NODE_MODE (op1) == ER_NM_vect && ER_dim (op1) != 0;
}

static void unary_vect_op (ER_node_t, ER_node_t);

/* Define inline functions used to do actual different type
   unary operations.  */
#define ifunc1(name) static do_always_inline int_t name (int_t a)
#define ffunc1(name) static do_always_inline floating_t name (floating_t a)
#define iffunc1(name) static do_always_inline int_t name (floating_t a)
#define lfunc1(name) static do_always_inline ER_node_t name (ER_node_t a)
#define ilfunc1(name) static do_always_inline int_t name (ER_node_t a)
ifunc1 (iunary_plus) { return a;}
ifunc1 (iunary_minus) { return -a;}
ffunc1 (funary_plus) { return a;}
ffunc1 (funary_minus) { return -a;}
ifunc1 (inot) { return a == 0;}
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

/* Do arithmetic operations using iop or fop on OP1 and OP2.  Put
   result into RES.  Use MSG in case of error.  Check and do vector
   operations if VECT_P.  */
static void do_always_inline
execute_ar_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p,
	       const char *err_message, int_t iop (int_t, int_t),
	       floating_t fop (floating_t, floating_t),
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
	  int_t op_i1 = ER_i (l), op_i2 = ER_i (r);

	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, iop (op_i1, op_i2));
	}
      else if (float_bin_op (l, r))
	{
	  floating_t op_f1 = ER_f (l), op_f2 = ER_f (r);

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

static void do_inline
execute_plus_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_plus_operands_types,
		 iplus, fplus, lplus);
}

static void do_inline
execute_minus_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_minus_operands_types,
		 iminus, fminus, lminus);
}

static void do_inline
execute_mult_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_mult_operands_types,
		 imult, fmult, lmult);
}

static void do_inline
execute_div_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_div_operands_types,
		 idiv, fdiv, lidiv);
}

static void do_inline
execute_mod_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_ar_op (res, op1, op2, vect_p, DERR_mod_operands_types,
		 imod, frem, limod);
}

/* Do integer operations using iop on OP1 and OP2.  Put result into
   RES.  Use MSG in case of error.  Check and do vector operations if
   VECT_P.  */
static void do_always_inline
execute_int_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p,
	       const char *err_message, int_t iop (int_t, int_t))
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
	  int_t op_i1 = ER_i (l), op_i2 = ER_i (r);

	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, iop (op_i1, op_i2));
	}
      else
	eval_error (optype_bc_decl, get_cpos (), err_message);
    }
}

static void do_inline
execute_lshift_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_lshift_operands_types, ilshift);
}

static void do_inline
execute_rshift_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_rshift_operands_types, irshift);
}

static void do_inline
execute_ashift_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_ashift_operands_types, iashift);
}

static void do_inline
execute_and_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_and_operands_types, iand);
}

static void do_inline
execute_xor_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_xor_operands_types, ixor);
}

static void do_inline
execute_or_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  execute_int_op (res, op1, op2, vect_p, DERR_or_operands_types, ior);
}

/* The following function implements array concatenation.  */
static void do_inline
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
	  && (ER_pack_vect_el_type (vect2)
	      != ER_pack_vect_el_type (vect1))))
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
      
      if (ER_pack_vect_el_type (vect2) == ER_NM_nil)
	result_el_type = ER_pack_vect_el_type (vect1);
      else if (ER_pack_vect_el_type (vect1) == ER_NM_nil)
	result_el_type = ER_pack_vect_el_type (vect2);
      else if (ER_pack_vect_el_type (vect2)
	       == ER_pack_vect_el_type (vect1))
	result_el_type = ER_pack_vect_el_type (vect2);
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

static void do_inline
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
  entry = find_tab_entry (tab, op1, FALSE);
  cmp = (ER_NODE_MODE (entry) != ER_NM_empty_entry
	 && ER_NODE_MODE (entry) != ER_NM_deleted_entry);
  ER_SET_MODE (res, ER_NM_int);
  ER_set_i (res, cmp);
}

/* Implement common case of equality/inequality comparison CMP_OP of
   OP1 and OP2.  Put result into RES.  Check and do vector operations
   if VECT_P.  */
static void do_inline
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

/* Implement comparison OP of OP1 and OP2 using icmp, fcmp, or gencmp.
   Put result into RES.  Check and do vector operations if VECT_P.  */
static void do_always_inline
comp_op (BC_node_mode_t op, ER_node_t res, ER_node_t op1, ER_node_t op2,
	 int vect_p, int icmp (int_t, int_t), int fcmp (floating_t, floating_t),
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

static void do_inline
execute_eq_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_eq, res, op1, op2, vect_p, ieq, feq, execute_common_eq_ne_op);
}

static void do_inline
execute_ne_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_ne, res, op1, op2, vect_p, ine, fne, execute_common_eq_ne_op);
}

/* Implement common case of comparison OPER of OP1 and OP2.  Put
   result into RES.  Check and do vector operations if VECT_P.  */
static void do_inline
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

static void do_inline
execute_lt_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_lt, res, op1, op2, vect_p, ilt, flt, execute_common_cmp_op);
}

static void do_inline
execute_ge_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_ge, res, op1, op2, vect_p, ige, fge, execute_common_cmp_op);
}

static void do_inline
execute_gt_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_gt, res, op1, op2, vect_p, igt, fgt, execute_common_cmp_op);
}

static void do_inline
execute_le_op (ER_node_t res, ER_node_t op1, ER_node_t op2, int vect_p)
{
  comp_op (BC_NM_le, res, op1, op2, vect_p, ile, fle, execute_common_cmp_op);
}

/* Implement identity (if INDENTITY_P) or unidentity comparison of OP1
   and OP2.  Put result into RES.  Check and do vector operations if
   VECT_P.  */
static void do_inline
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

/* Do unary arithmetic operation using iop or fop on OP1 and OP2.  Put
   result into RES.  Use MSG in case of error.  Check and do vector
   operations if VECT_P.  */
static void do_always_inline
execute_unary_ar_op (ER_node_t res, ER_node_t op1, int vect_p,
		     const char *err_message, int_t iop (int_t),
		     floating_t fop (floating_t),
		     ER_node_t lop (ER_node_t))
{
  int_t i;
  floating_t f;
  val_t tvar1;

  if (ER_NODE_MODE (op1) == ER_NM_int)
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
  if (ER_NODE_MODE (op1) == ER_NM_int)
    {
      i = iop (ER_i (op1));
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
    }
  else if (ER_NODE_MODE (op1) == ER_NM_float)
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
  int_t i;
  val_t tvar1;

  if (ER_NODE_MODE (op1) == ER_NM_int)
    {
      i = inot (ER_i (op1));
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
  if (ER_NODE_MODE (op1) == ER_NM_int)
    i = inot (ER_i (op1));
  else if (ER_NODE_MODE (op1) == ER_NM_float)
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
  int_t i;
  val_t tvar1;

  if (ER_NODE_MODE (op1) == ER_NM_int)
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
      if (ER_NODE_MODE (op1) == ER_NM_int)
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
  int_t i;
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
  BC_node_mode_t type;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  type = ER_NODE_MODE (op1);
  ER_SET_MODE (res, ER_NM_type);
  ER_set_type (res, type);
}

static void do_always_inline
execute_charof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  int_t i;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_int_conversion (op1, (ER_node_t) &tvar1);
  if (ER_NODE_MODE (op1) != ER_NM_int)
    eval_error (optype_bc_decl, get_cpos (),
		DERR_conversion_to_char_operand_type);
  if (ER_i (op1) > MAX_CHAR || ER_i (op1) < 0)
    {
#ifdef ERANGE
      errno = ERANGE;
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
  int_t i;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_int_conversion (op1, (ER_node_t) &tvar1);
  if (ER_NODE_MODE (op1) != ER_NM_int)
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
  floating_t f;
  val_t tvar1;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  op1 = implicit_float_conversion (op1, (ER_node_t) &tvar1);
  if (ER_NODE_MODE (op1) != ER_NM_float)
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
	      || ER_pack_vect_el_type (ER_vect (op1)) != ER_NM_char))
	eval_error (optype_bc_decl, get_cpos (),
		    DERR_format_conversion_to_vector_operand_type);
      op2 = to_vect_string_conversion (op2, NULL, (ER_node_t) &tvar2);
      if (ER_NODE_MODE (op2) != ER_NM_vect
	  || ER_NODE_MODE (ER_vect (op2)) != ER_NM_heap_pack_vect
	  || ER_pack_vect_el_type (ER_vect (op2)) != ER_NM_char)
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

static void do_always_inline
execute_funof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  BC_node_t block;

  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  if (ER_NODE_MODE (op1) == ER_NM_stack
      && (block = ER_block_node (ER_stack (op1))) != NULL
      && BC_fun_p (block))
    {
      ER_node_t stack;
      
      stack = ER_stack (op1);
      ER_SET_MODE (res, ER_NM_code);
      ER_set_code_id (res, CODE_ID (block));
      ER_set_code_context (res, ER_context (stack));
    }
  else
    ER_SET_MODE (res, ER_NM_nil);
}

static void do_always_inline
execute_threadof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  if (ER_NODE_MODE (op1) == ER_NM_process
      && ER_process_block (ER_process (op1)) != NULL)
    {
      ER_node_t process;
      
      process = ER_process (op1);
      ER_SET_MODE (res, ER_NM_code);
      ER_set_code_id (res, CODE_ID (ER_process_block (process)));
      ER_set_code_context (res, ER_context (process));
    }
  else
    ER_SET_MODE (res, ER_NM_nil);
}

static void do_always_inline
execute_classof_op (ER_node_t res, ER_node_t op1, int vect_p)
{
  if (vect_p && vect_unary_op (op1))
    {
      unary_vect_op (res, op1);
      return;
    }
  if (ER_NODE_MODE (op1) == ER_NM_stack)
    {
      ER_node_t instance;
      BC_node_t block;
      
      instance = ER_stack (op1);
      block = ER_block_node (instance);
      if (block == NULL || ! BC_class_p (block))
	ER_SET_MODE (res, ER_NM_nil);
      else
	{
	  ER_SET_MODE (res, ER_NM_code);
	  ER_set_code_id (res, CODE_ID (block));
	  ER_set_code_context (res, ER_context (instance));
	}
    }
  else
    ER_SET_MODE (res, ER_NM_nil);
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

/* Execute LEN integer operations IOP of packed array elements
   PACK_RES_ELS1 and PACK_ELS2 (if it is not NULL), otherwise
   implement operation PACK_ELS1 element IOP OP2 or OP2 IOP PACK_ELS1
   element (if REV_P).  Put results in PACK_RES_ELS.  */
static do_always_inline void
int_pack_vect_op (int rev_p, size_t len, char *pack_res_els, char *pack_els1,
		  char *pack_els2, ER_node_t op2, int_t iop (int_t, int_t))
{
  size_t i;
  int_t op_i2;

  if (pack_els2 != NULL)
    {
      d_assert (! rev_p);
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i]
	  = iop (((int_t *) pack_els1) [i], ((int_t *) pack_els2) [i]);
    }
  else if (rev_p)
    {
      op_i2 = ER_i (op2);
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i] = iop (op_i2, ((int_t *) pack_els1) [i]);
    }
  else
    {
      op_i2 = ER_i (op2);
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i] = iop (((int_t *) pack_els1) [i], op_i2);
    }
}

/* Analogous the function above but for floating point operations.  */
static do_always_inline void
float_pack_vect_op (int rev_p, size_t len, char *pack_res_els, char *pack_els1,
		    char *pack_els2, ER_node_t op2,
		    floating_t fop (floating_t, floating_t))
{
  size_t i;
  floating_t op_f2;

  if (pack_els2 != NULL)
    {
      d_assert (! rev_p);
      for (i = 0; i < len; i++)
	((floating_t *) pack_res_els) [i]
	  = fop (((floating_t *) pack_els1) [i],
		 ((floating_t *) pack_els2) [i]);
    }
  else if (rev_p)
    {
      op_f2 = ER_f (op2);
      for (i = 0; i < len; i++)
	((floating_t *) pack_res_els) [i]
	  = fop (op_f2, ((floating_t *) pack_els1) [i]);
    }
  else
    {
      op_f2 = ER_f (op2);
      for (i = 0; i < len; i++)
	((floating_t *) pack_res_els) [i]
	  = fop (((floating_t *) pack_els1) [i], op_f2);
    }
}

/* Analogous the function above but used for char comparison
   operations.  */
static do_always_inline void
char_pack_vect_cmp_op (size_t len, char *pack_res_els, char *pack_els1,
		       char *pack_els2, ER_node_t op2, int icmp (int_t, int_t))
{
  size_t i;
  int_t op_i2;

  if (pack_els2 != NULL)
    {
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i]
	  = icmp (((char_t *) pack_els1) [i], ((char_t *) pack_els2) [i]);
    }
  else
    {
      op_i2 = ER_i (op2);
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i]
	  = icmp (((char_t *) pack_els1) [i], op_i2);
    }
}

/* Analogous the function above but used for integer comparison
   operations.  */
static do_always_inline void
int_pack_vect_cmp_op (size_t len, char *pack_res_els, char *pack_els1,
		      char *pack_els2, ER_node_t op2, int icmp (int_t, int_t))
{
  size_t i;
  int_t op_i2;

  if (pack_els2 != NULL)
    {
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i]
	  = icmp (((int_t *) pack_els1) [i], ((int_t *) pack_els2) [i]);
    }
  else
    {
      op_i2 = ER_i (op2);
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i]
	  = icmp (((int_t *) pack_els1) [i], op_i2);
    }
}

/* Analogous the function above but used for float comparison
   operations.  */
static do_always_inline void
float_pack_vect_cmp_op (size_t len, char *pack_res_els, char *pack_els1,
			char *pack_els2, ER_node_t op2,
			int fcmp (floating_t, floating_t))
{
  size_t i;
  floating_t op_f2;

  if (pack_els2 != NULL)
    {
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i]
	  = fcmp (((floating_t *) pack_els1) [i],
		 ((floating_t *) pack_els2) [i]);
    }
  else
    {
      op_f2 = ER_f (op2);
      for (i = 0; i < len; i++)
	((int_t *) pack_res_els) [i]
	  = fcmp (((floating_t *) pack_els1) [i], op_f2);
    }
}

/* Recursive function implementing binary vector operations defined by
   CPC.  Operands are OP1 with dimension DIM1 and OP2 with dimension
   DIM2 (DIM1 >= DIM2).  If REV_P, operands are taken for operations
   in different order.  The current recursion depth is DEPTH.  */
static ER_node_t
process_binary_vect_op (int rev_p, ER_node_t op1, int_t dim1,
			ER_node_t op2, int_t dim2, int_t depth)
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
      el_type1 = ER_pack_vect_el_type (op1);
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
	  el_type2 = ER_pack_vect_el_type (op2);
	}
      else
	{
	  pack_flag2 = FALSE;
	  unpack_els2 = ER_unpack_els (op2);
	  el_type2 = ER_NM__error;
	}
      if (len1 != len2)
	eval_error (veclen_bc_decl, get_cpos (),
		    DERR_different_vec_operand_lengths, len1, len2, depth);
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
  switch (oper = BC_NODE_MODE (cpc))
    {
    case BC_NM_add:
    case BC_NM_addi:
    case BC_NM_add_st:
    case BC_NM_add_slst:
    case BC_NM_sub:
    case BC_NM_sub_st:
    case BC_NM_sub_slst:
    case BC_NM_mult:
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
				    pack_els2, op2, iplus);
		  break;
		case BC_NM_sub:
		case BC_NM_sub_st:
		case BC_NM_sub_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, iminus);
		  break;
		case BC_NM_mult:
		case BC_NM_mult_st:
		case BC_NM_mult_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, imult);
		  break;
		case BC_NM_div:
		case BC_NM_div_st:
		case BC_NM_div_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, idiv);
		  break;
		case BC_NM_mod:
		case BC_NM_mod_st:
		case BC_NM_mod_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, imod);
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
				      pack_els2, op2, fplus);
		  break;
		case BC_NM_sub:
		case BC_NM_sub_st:
		case BC_NM_sub_slst:
		  float_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				      pack_els2, op2, fminus);
		  break;
		case BC_NM_mult:
		case BC_NM_mult_st:
		case BC_NM_mult_slst:
		  float_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				      pack_els2, op2, fmult);
		  break;
		case BC_NM_div:
		case BC_NM_div_st:
		case BC_NM_div_slst:
		  float_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				      pack_els2, op2, fdiv);
		  break;
		case BC_NM_mod:
		case BC_NM_mod_st:
		case BC_NM_mod_slst:
		  float_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				      pack_els2, op2, frem);
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
				    pack_els2, op2, ilshift);
		  break;
		case BC_NM_rsh:
		case BC_NM_rsh_st:
		case BC_NM_rsh_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, irshift);
		  break;
		case BC_NM_ash:
		case BC_NM_ash_st:
		case BC_NM_ash_slst:
		  int_pack_vect_op (rev_p, len1, pack_res_els, pack_els1,
				    pack_els2, op2, iashift);
		  break;
		case BC_NM_and:
		case BC_NM_and_st:
		case BC_NM_and_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, iand);
		  break;
		case BC_NM_xor:
		case BC_NM_xor_st:
		case BC_NM_xor_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, ixor);
		  break;
		case BC_NM_or:
		case BC_NM_or_st:
		case BC_NM_or_slst:
		  int_pack_vect_op (FALSE, len1, pack_res_els, pack_els1,
				    pack_els2, op2, ior);
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
				     pack_els2, op2, neg_p ? ine : ieq);
	      break;
	    case ER_NM_int:
	      int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
				    pack_els2, op2, neg_p ? ine : ieq);
	      break;
	    case ER_NM_float:
	      float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
				      pack_els2, op2, neg_p ? fne : feq);
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
	  ((int_t *) pack_res_els) [i] = ER_i ((ER_node_t) &rval);
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
					pack_els2, op2, rev_p ? ige : ilt);
		  break;
		case BC_NM_ge:
		  int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					pack_els2, op2, rev_p ? ilt : ige);
		  break;
		case BC_NM_le:
		  int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					pack_els2, op2, rev_p ? igt : ile);
		  break;
		case BC_NM_gt:
		  int_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					pack_els2, op2, rev_p ? ile : igt);
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
					  pack_els2, op2, rev_p ? fge : flt);
		  break;
		case BC_NM_ge:
		  float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					  pack_els2, op2, rev_p ? flt : fge);
		  break;
		case BC_NM_le:
		  float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					  pack_els2, op2, rev_p ? fgt : fle);
		  break;
		case BC_NM_gt:
		  float_pack_vect_cmp_op (len1, pack_res_els, pack_els1,
					  pack_els2, op2, rev_p ? fle : fgt);
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
	  ((int_t *) pack_res_els) [i] = ER_i ((ER_node_t) &rval);
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
	  ((int_t *) pack_res_els) [i] = ER_i ((ER_node_t) &rval);
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

static int_t do_always_inline
get_dim (ER_node_t op)
{
  return ER_NODE_MODE (op) == ER_NM_vect ? ER_dim (op) : 0;
}

/* Implement binary vector operation defined by CPC.  */
static void
binary_vect_op (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  int rev_p = FALSE;
  int_t dim1, dim2, t;
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


/* Execute LEN integer operations IOP of packed array elements
   PACK_ELS.  Put results in PACK_RES_ELS.  */
static do_always_inline void
int_pack_vect_unary_op (size_t len, char *pack_res_els, char *pack_els,
			int_t iop (int_t))
{
  size_t i;

  for (i = 0; i < len; i++)
    ((int_t *) pack_res_els) [i] = iop (((int_t *) pack_els) [i]);
}

/* Analogous to function above but for floating point operations.  */
static do_always_inline void
float_pack_vect_unary_op (size_t len, char *pack_res_els, char *pack_els,
			  floating_t fop (floating_t))
{
  size_t i;

  for (i = 0; i < len; i++)
    ((floating_t *) pack_res_els) [i] = fop (((floating_t *) pack_els) [i]);
}

/* Recursive function implementing unary vector operations defined by
   CPC.  Operand is OP with dimension DIM.  The current recursion
   depth is DEPTH.  */
static ER_node_t
process_unary_vect_op (ER_node_t op, int_t dim, int_t depth)
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
      el_type = ER_pack_vect_el_type (op);
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
	      int_pack_vect_unary_op (len, pack_res_els, pack_els, inot);
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
	case BC_NM_funof:
	  execute_funof_op (el_res, el, FALSE);
	  break;
	case BC_NM_threadof:
	  execute_threadof_op (el_res, el, FALSE);
	  break;
	case BC_NM_classof:
	  execute_classof_op (el_res, el, FALSE);
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
static void
unary_vect_op (ER_node_t res, ER_node_t op)
{
  int_t dim;
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
execute_pack_fold_op (ER_node_t res, int_t initval, ER_node_t pack_vect,
		      ER_node_mode_t el_type, size_t len, char *pack_els,
		      void genop (ER_node_t, ER_node_t, ER_node_t, int),
		      int_t iop (int_t, int_t),
		      floating_t fop (floating_t, floating_t))
{
  size_t i;
  val_t l, rval;
  ER_node_t r = (ER_node_t) &rval;
  int_t ires;
  floating_t fres;

  if (el_type == ER_NM_int)
    {
      ires = initval;
      for (i = 0; i < len; i++)
	ires = iop (ires, ((int_t *) pack_els) [i]);
      ER_SET_MODE (r, ER_NM_int);
      ER_set_i (r, ires);
    }
  else if (fop != NULL && el_type == ER_NM_float)
    {
      fres = initval;
      for (i = 0; i < len; i++)
	fres = fop (fres, ((floating_t *) pack_els) [i]);
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
process_fold_vect_op (ER_node_t res, ER_node_t op, int_t dim, int_t depth)
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
      el_type = ER_pack_vect_el_type (op);
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
	case BC_NM_fadd:
	  execute_pack_fold_op (res, 0, op, el_type, len, pack_els,
				execute_plus_op, iplus, fplus);
	  break;
	case BC_NM_fmult:
	  execute_pack_fold_op (res, 1, op, el_type, len, pack_els,
				execute_mult_op, imult, fmult);
	  break;
	case BC_NM_fand:
	  execute_pack_fold_op (res, ~ (int_t) 0, op, el_type, len, pack_els,
				execute_and_op, iand, NULL);
	  break;
	case BC_NM_fxor:
	  execute_pack_fold_op (res, 0, op, el_type, len, pack_els,
				execute_xor_op, ixor, NULL);
	  break;
	case BC_NM_for:
	  execute_pack_fold_op (res, 0, op, el_type, len, pack_els,
				execute_or_op, ior, NULL);
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
	    case BC_NM_fadd:
	      execute_plus_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    case BC_NM_fmult:
	      execute_mult_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    case BC_NM_fand:
	      execute_and_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    case BC_NM_fxor:
	      execute_xor_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    case BC_NM_for:
	      execute_or_op (res, res, (ER_node_t) &l, FALSE);
	      break;
	    default:
	      d_unreachable ();
	    }
	}
    }
}

/* Implement folding vector operation defined by CPC.  */
static void
fold_vect_op (ER_node_t res, ER_node_t op)
{
  int_t dim;

  dim = get_dim (op);
  if (ER_NODE_MODE (op) == ER_NM_vect && dim > 0)
    op = ER_vect (op);
  else
    eval_error (vecform_bc_decl,
		BC_pos2 (BC_info (cpc)), DERR_vector_form_type, 1);
  ER_SET_MODE (res, ER_NM_int);
  switch (BC_NODE_MODE (cpc))
    {
    case BC_NM_fadd:
    case BC_NM_fxor:
    case BC_NM_for:
      ER_set_i (res, 0);
      break;
    case BC_NM_fmult:
      ER_set_i (res, 1);
      break;
    case BC_NM_fand:
      ER_set_i (res, ~ (int_t) 0);
      break;
    defuaylt:
      d_unreachable ();
    }
  process_fold_vect_op (res, op, dim, 1);
}

/* Macro for possible switch the process or to do GC.  Remeber cpc
   must be correct for resuming the current process lately. */
#define INTERRUPT_CHECK\
  do {\
    if (++executed_stmts_count >= 0) {			\
      interrupt (cpc); 		       			\
    }				       			\
  } while (0)

/* Return I-th vars.  If i is non-negative, it is local variable.
   Negative numbers encode global vars.  */
static ER_node_t do_always_inline
get_op (int_t i)
{
  return i >= 0 ? IVAL (cvars, i) : IVAL (tvars, -i - 1);
}

static int do_always_inline
extract_op1 (ER_node_t *op1)
{
  *op1 = get_op (BC_op1 (cpc));
}

static int do_always_inline
extract_op2 (ER_node_t *op1, ER_node_t *op2)
{
  extract_op1 (op1);
  *op2 = get_op (BC_op2 (cpc));
}

static int do_always_inline
extract_op3 (ER_node_t *op1, ER_node_t *op2, ER_node_t *op3)
{
  int_t i = BC_op3 (cpc);

  extract_op2 (op1, op2);
  *op3 = get_op (BC_op3 (cpc));
}

static ER_node_t do_always_inline
execute_btcmpinc (ER_node_t op1, ER_node_t op2, int icmp (int_t, int_t))
{
  ER_node_t res;
  int_t i;
  val_t v;

  i = BC_binc_inc (cpc);
  if (ER_NODE_MODE (op1) == ER_NM_int)
    {
      i += ER_i (op1);
      ER_set_i (op1, i);
      if (ER_NODE_MODE (op2) == ER_NM_int)
	{
	  if (icmp (i, ER_i (op2)))
	    cpc = BC_pc (cpc);
	  else
	    INCREMENT_PC ();
	  return NULL;
	}
    }
  else
    {
      ER_SET_MODE ((ER_node_t) &v, ER_NM_int);
      ER_set_i ((ER_node_t) &v, i);
      execute_plus_op (op1, op1, (ER_node_t) &v, FALSE);
    }
  return op1;
}

static ER_node_t do_always_inline
execute_btcmp (ER_node_t op1, ER_node_t op2,
	       BC_node_mode_t cmp_nm, int icmp (int_t, int_t),
	       int fcmp (floating_t, floating_t),
	       void gencmp (BC_node_mode_t, ER_node_t, ER_node_t, ER_node_t, int))
{
  ER_node_t res;

  if (ER_NODE_MODE (op1) == ER_NM_int && ER_NODE_MODE (op2) == ER_NM_int)
    {
      if (icmp (ER_i (op1), ER_i (op2)))
	cpc = BC_pc (cpc);
      else
	INCREMENT_PC ();
      INTERRUPT_CHECK;
      return NULL;
    }
  res = get_op (BC_bcmp_res (cpc));
  comp_op (cmp_nm, res, op1, op2, FALSE, icmp, fcmp, gencmp);
  return res;
}

static ER_node_t do_always_inline
execute_btcmpi (ER_node_t op1, int_t i,
	       BC_node_mode_t cmp_nm, int icmp (int_t, int_t),
	       int fcmp (floating_t, floating_t),
	       void gencmp (BC_node_mode_t, ER_node_t, ER_node_t, ER_node_t, int))
{
  ER_node_t res, op2;
  static val_t v;

  if (ER_NODE_MODE (op1) == ER_NM_int)
    {
      if (icmp (ER_i (op1), i))
	cpc = BC_pc (cpc);
      else
	INCREMENT_PC ();
      INTERRUPT_CHECK;
      return NULL;
    }
  res = get_op (BC_bcmp_res (cpc));
  op2 = (ER_node_t) &v;
  ER_SET_MODE (op2, ER_NM_int);
  ER_set_i (op2, i);
  comp_op (cmp_nm, res, op1, op2, FALSE, icmp, fcmp, gencmp);
  return res;
}

static int do_always_inline
execute_cmpi (ER_node_t *res, ER_node_t *op1, ER_node_t *op2, int icmp (int_t, int_t))
{
  int_t i;
  static val_t v;

  extract_op2 (res, op1);
  i = BC_op3 (cpc);
  if (ER_NODE_MODE (*op1) == ER_NM_int)
    {
      ER_SET_MODE (*res, ER_NM_int);
      ER_set_i (*res, icmp (ER_i (*op1), i));
      INCREMENT_PC ();
      return FALSE;
    }
  *op2 = (ER_node_t) &v;
  ER_SET_MODE (*op2, ER_NM_int);
  ER_set_i (*op2, i);
  return TRUE;
}

static int do_always_inline
non_zero_p (ER_node_t op, const char *msg)
{
  val_t tvar;

  op = implicit_arithmetic_conversion (op, (ER_node_t) &tvar);
  if (ER_NODE_MODE (op) == ER_NM_int)
    return ER_i (op) != 0;
  else if (ER_NODE_MODE (op) == ER_NM_float)
    return ER_f (op) != 0.0;
  else if (ER_NODE_MODE (op) == ER_NM_long)
    return mpz_sgn (*ER_mpz_ptr (ER_l (op))) != 0;
  else
    eval_error (optype_bc_decl, get_cpos (), msg);
}

static void
evaluate_code (void)
{
  int tail_flag;
  int_t i;
  ER_node_t res, op1, op2, op3;
  BC_node_mode_t node_mode;
  BC_node_t bc_node;
  val_t v;

  /* Check that all BC_node_mode_t can be stored in unsigned char.  */
  d_assert ((int) BC_NM__error < 256);
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
	  op2 = get_op (BC_op3 (cpc));
	  extract_op2 (&res, &op1);
	  store_vect_tab_designator_value (res, op1, op2);
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_sts:
	  op2 = get_op (BC_op3 (cpc));
	  extract_op2 (&res, &op1);
	  store_stack_designator_value (res, op1, op2);
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ste:
	  op2 = get_op (BC_op3 (cpc));
	  extract_op1 (&res);
	  *(val_t *) ER_external_var_ref (res) = *(val_t *) op2;
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_ldnil:
	  extract_op1 (&res);
	  ER_SET_MODE (res, ER_NM_nil);
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldthis:
	  {
	    ER_node_t stack;
	    BC_node_t block_node;

	    extract_op1 (&res);
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
	    INCREMENT_PC ();
	  }
	  break;
	case BC_NM_ldch:
	  extract_op1 (&res);
	  ER_SET_MODE (res, ER_NM_char);
	  ER_set_ch (res, BC_op2 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldi:
	  extract_op1 (&res);
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, BC_op2 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_ldl:
	  {
	    ER_node_t heap_gmp;

	    extract_op1 (&res);
	    ER_SET_MODE (res, ER_NM_long);
	    heap_gmp = create_gmp ();
	    ER_set_l (res, heap_gmp);
	    mpz_set (*ER_mpz_ptr (heap_gmp), *BC_mpz_ptr (cpc));
	    INCREMENT_PC ();
	    break;
	  }
	case BC_NM_ldf:
	  extract_op1 (&res);
	  ER_SET_MODE (res, ER_NM_float);
	  ER_set_f (res, BC_f (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lds:
	  {
	    ER_node_t vect;
	    
	    vect = create_string (BC_str (cpc));
	    extract_op1 (&res);
	    ER_SET_MODE (res, ER_NM_vect);
	    set_vect_dim (res, vect, 0);
	    INCREMENT_PC ();
	  }
	  break;
	case BC_NM_ldtp:
	  extract_op1 (&res);
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, BC_op2 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_flat:
	  extract_op1 (&op1);
	  ER_set_dim (op1, 0);
	  INCREMENT_PC ();
	  break;
	case BC_NM_fld:
	case BC_NM_lfld:
	case BC_NM_lfldv:
	  extract_op2 (&res, &op1);
	  execute_a_period_operation (BC_op3 (cpc), res, op1,
				      node_mode != BC_NM_fld,
				      node_mode == BC_NM_lfldv);
	  INCREMENT_PC ();
	  break;
	case BC_NM_brts:
	case BC_NM_brfs:
	  {
	    int true_p;

	    extract_op1 (&op1);
	    res = get_op (BC_res (cpc));
#ifndef SMALL_CODE
	    if (ER_NODE_MODE (op1) == ER_NM_int)
	      true_p = ER_i (op1) != 0;
	    else
#endif
	      true_p = non_zero_p (op1, node_mode == BC_NM_brts
				   ? DERR_logical_or_operands_types
				   : DERR_logical_and_operands_types);
	    ER_SET_MODE (res, ER_NM_int);
	    ER_set_i (res, true_p);
	    if (true_p == (node_mode == BC_NM_brts))
	      cpc = BC_pc (cpc);
	    else
	      INCREMENT_PC ();
	    break;
	  }
	case BC_NM_lconv:
	  {
	    int true_p;

	    extract_op2 (&res, &op1);
#ifndef SMALL_CODE
	    if (ER_NODE_MODE (op1) == ER_NM_int)
	      true_p = ER_i (op1) != 0;
	    else
#endif
	      true_p = non_zero_p (op1, DERR_logical_operands_types);
	    ER_SET_MODE (res, ER_NM_int);
	    ER_set_i (res, true_p);
	    INCREMENT_PC ();
	    break;
	  }
	case BC_NM_in:
	  extract_op3 (&res, &op1, &op2);
	  execute_in_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_not:
	  extract_op2 (&res, &op1);
	  execute_not_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_bnot:
	  extract_op2 (&res, &op1);
	  execute_bitwise_not_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_fadd:
	case BC_NM_fmult:
	case BC_NM_fand:
	case BC_NM_fxor:
	case BC_NM_for:
	  extract_op2 (&res, &op1);
	  fold_vect_op (res, op1);
	  INCREMENT_PC ();
	  break;
	case BC_NM_eq:
	  extract_op3 (&res, &op1, &op2);
	common_eq:
	  execute_eq_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_eqi:
	  if (execute_cmpi (&res, &op1, &op2, ieq))
	    goto common_eq;
	  break;
	case BC_NM_ne:
	  extract_op3 (&res, &op1, &op2);
	common_ne:
	  execute_ne_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_nei:
	  if (execute_cmpi (&res, &op1, &op2, ine))
	    goto common_ne;
	  break;
	case BC_NM_id:
	case BC_NM_unid:
	  extract_op3 (&res, &op1, &op2);
	  execute_identity_op (node_mode == BC_NM_id, res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_lt:
	  extract_op3 (&res, &op1, &op2);
	common_lt:
	  execute_lt_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_lti:
	  if (execute_cmpi (&res, &op1, &op2, ilt))
	    goto common_lt;
	  break;
	case BC_NM_ge:
	  extract_op3 (&res, &op1, &op2);
	common_ge:
	  execute_ge_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_gei:
	  if (execute_cmpi (&res, &op1, &op2, ige))
	    goto common_ge;
	  break;
	case BC_NM_gt:
	  extract_op3 (&res, &op1, &op2);
	common_gt:
	  execute_gt_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_gti:
	  if (execute_cmpi (&res, &op1, &op2, igt))
	    goto common_gt;
	  break;
	case BC_NM_le:
	  extract_op3 (&res, &op1, &op2);
	common_le:
	  execute_le_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_lei:
	  if (execute_cmpi (&res, &op1, &op2, ile))
	    goto common_le;
	  break;
	case BC_NM_plus:
	  extract_op2 (&res, &op1);
	  execute_unary_ar_op (res, op1, TRUE, DERR_unary_plus_operand_type,
			       iunary_plus, funary_plus, lunary_plus);
	  INCREMENT_PC ();
	  break;
	case BC_NM_minus:
	  extract_op2 (&res, &op1);
	  execute_unary_ar_op (res, op1, TRUE, DERR_unary_minus_operand_type,
			       iunary_minus, funary_minus, lunary_minus);
	  INCREMENT_PC ();
	  break;
	case BC_NM_length:
	  extract_op2 (&res, &op1);
	  execute_length_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_const:
	  extract_op2 (&res, &op1);
	  execute_const_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_new:
	  extract_op2 (&res, &op1);
	  execute_new_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_tpof:
	  extract_op2 (&res, &op1);
	  execute_typeof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_chof:
	  extract_op2 (&res, &op1);
	  execute_charof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_iof:
	  extract_op2 (&res, &op1);
	  execute_intof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_lof:
	  extract_op2 (&res, &op1);
	  execute_longof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_fof:
	  extract_op2 (&res, &op1);
	  execute_floatof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_fmtvecof:
	  extract_op3 (&res, &op1, &op2);
	  execute_vectorof_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_vecof:
	  extract_op2 (&res, &op1);
	  execute_vectorof_op (res, op1, NULL, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_tabof:
	  extract_op2 (&res, &op1);
	  execute_tableof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_funof:
	  extract_op2 (&res, &op1);
	  execute_funof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_threadof:
	  extract_op2 (&res, &op1);
	  execute_threadof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_classof:
	  extract_op2 (&res, &op1);
	  execute_classof_op (res, op1, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_vec:
	  {
	    /* If you make a change here, please look at DINO read
	       functions. */
	    ER_node_t vect, saved_ctop;
	    int_t vect_parts_number, curr_vect_part_number;
	    int_t curr_vect_element_number;
	    size_t el_type_size;
	    size_t els_number;
	    int_t repetition;
	    int pack_flag;
	    
	    extract_op2 (&res, &op1);
	    vect_parts_number = BC_op3 (cpc);
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
		    if (ER_NODE_MODE (IVAL (op1, curr_vect_part_number))
			!= ER_NM_int)
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
		  ((unsigned_int_t) curr_vect_element_number == els_number);
	      }
	    ER_SET_MODE (res, ER_NM_vect);
	    set_vect_dim (res, vect, 0);
	    INCREMENT_PC ();
	    break;
	  }
	case BC_NM_tab:
	  {
	    ER_node_t tab, saved_ctop;
	    int_t tab_els_number, curr_tab_el_number;
	    ER_node_t entry;
	    
	    extract_op2 (&res, &op1);
	    tab_els_number = BC_op3 (cpc);
	    saved_ctop = ctop;
	    ctop = op1;
	    DECR_CTOP (-2 * tab_els_number);
	    tab = create_tab (tab_els_number);
	    for (curr_tab_el_number = 0;
		 curr_tab_el_number < 2 * tab_els_number;
		 curr_tab_el_number += 2)
	      {
		entry = find_tab_entry (tab, IVAL (op1, curr_tab_el_number),
					TRUE);
		if (ER_NODE_MODE (entry) != ER_NM_empty_entry
		    && ER_NODE_MODE (entry) != ER_NM_deleted_entry)
		  eval_error (keyvalue_bc_decl, get_cpos (), DERR_repeated_key,
			      curr_tab_el_number);
		*(val_t *) entry = *(val_t *) IVAL (op1, curr_tab_el_number);
		make_immutable (entry);
		*((val_t *) entry + 1)
		  = *(val_t *) IVAL (op1, curr_tab_el_number + 1);
		d_assert (ER_IS_OF_TYPE (INDEXED_ENTRY_VAL (entry, 0),
					 ER_NM_val));
	      }
	    ctop = saved_ctop;
	    ER_SET_MODE (res, ER_NM_tab);
	    ER_set_tab (res, tab);
	    INCREMENT_PC ();
	    break;
	  }
	case BC_NM_ind:
	case BC_NM_lindv:
	  {
	    val_t tvar1;

	    extract_op3 (&res, &op1, &op2);
	    if (ER_NODE_MODE (op1) == ER_NM_vect)
	      {
		op2 = implicit_int_conversion (op2, (ER_node_t) &tvar1);
		load_vector_element_by_index (res, ER_vect (op1), op2);
	      }
	    else if (ER_NODE_MODE (op1) == ER_NM_tab)
	      load_table_element_by_key (res, ER_tab (op1), op2);
	    else
	      eval_error (indexop_bc_decl, get_cpos (),
			  DERR_index_operation_for_non_vec_tab);
	    INCREMENT_PC ();
	    break;
	  }
	case BC_NM_sl:
	  extract_op2 (&res, &op1);
	  slice_extract (res, op1, BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lslv:
	  /* ??? Do we need lvalue_slice_and_val for future or slice
	     is enough. */
	  extract_op2 (&res, &op1);
	  slice_extract (res, op1, BC_op3 (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_call:
	case BC_NM_tcall:
	  extract_op1 (&op1);
	  process_fun_class_call (op1, BC_op2 (cpc), node_mode == BC_NM_tcall);
	  break;
	case BC_NM_icall:
	case BC_NM_itcall:
	  op3 = find_context_by_scope (BC_scope (BC_cfblock (cpc)));
	  tail_flag = node_mode == BC_NM_itcall;
	  goto common_icall;
	case BC_NM_cicall:
	case BC_NM_citcall:
	  op3 = cstack;
	  tail_flag = node_mode == BC_NM_citcall;
	  goto common_icall;
	case BC_NM_ticall:
	case BC_NM_titcall:
	  op3 = uppest_stack;
	  tail_flag = node_mode == BC_NM_titcall;
	common_icall:
	  extract_op1 (&op1);
	  ctop = IVAL (op1, -1);
	  process_imm_fun_call ((val_t *) op1, BC_cfblock (cpc), op3,
				BC_op2 (cpc), tail_flag);
	  break;
	case BC_NM_ibcall:
	  extract_op1 (&op1);
	  ctop = IVAL (op1, -1);
	  process_imm_ifun_call (BC_cfblock (cpc), BC_op2 (cpc));
	  break;
	case BC_NM_add:
	  extract_op3 (&res, &op1, &op2);
	common_plus:
	  execute_plus_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_addi:
	  extract_op2 (&res, &op1);
	  i = BC_op3 (cpc);
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      i = iplus (ER_i (op1), i);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, i);
	      INCREMENT_PC ();
	    }
	  else
	    {
	      op2 = (ER_node_t) &v;
	      ER_SET_MODE (op2, ER_NM_int);
	      ER_set_i (op2, i);
	      goto common_plus;
	    }
	  break;
	case BC_NM_sub:
	  extract_op3 (&res, &op1, &op2);
	  execute_minus_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_mult:
	  extract_op3 (&res, &op1, &op2);
	  execute_mult_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_div:
	  extract_op3 (&res, &op1, &op2);
	  execute_div_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_mod:
	  extract_op3 (&res, &op1, &op2);
	  execute_mod_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_concat:
	  extract_op3 (&res, &op1, &op2);
	  execute_concat_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_lsh:
	  extract_op3 (&res, &op1, &op2);
	  execute_lshift_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_rsh:
	  extract_op3 (&res, &op1, &op2);
	  execute_rshift_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_ash:
	  extract_op3 (&res, &op1, &op2);
	  execute_ashift_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_and:
	  extract_op3 (&res, &op1, &op2);
	  execute_and_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_xor:
	  extract_op3 (&res, &op1, &op2);
	  execute_xor_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_or:
	  extract_op3 (&res, &op1, &op2);
	  execute_or_op (res, op1, op2, TRUE);
	  INCREMENT_PC ();
	  break;
	case BC_NM_add_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_plus_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_sub_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_minus_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_mult_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_mult_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_div_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_div_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_mod_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_mod_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_concat_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_concat_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_lsh_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_lshift_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_rsh_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_rshift_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_ash_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_ashift_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_and_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_and_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_xor_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_xor_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	  goto common_op_st;
	case BC_NM_or_st:
	  op2 = get_op (BC_op4 (cpc));
	  execute_or_op (op2, op2, get_op (BC_op3 (cpc)), TRUE);
	common_op_st:
	  extract_op2 (&res, &op1);
	  store_designator_value (res, op1, op2);
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
	  op2 = get_op (BC_op4 (cpc));
	  binary_vect_op (op2, op2, get_op (BC_op3 (cpc)));
	  goto common_slst;
	case BC_NM_slst:
	  op2 = get_op (BC_op3 (cpc));
	common_slst:
	  extract_op1 (&res);
	  slice_assign (res, BC_op2 (cpc), op2);
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_b:	
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_btdef: /* Branch if defined  */
	  extract_op1 (&op1);
	  if (ER_NODE_MODE (op1) == ER_NM_undef)
	    {
	      INCREMENT_PC ();
	      break;
	    }
	  cpc = BC_pc (cpc);
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_bf:
	case BC_NM_bfni:
	  extract_op1 (&op1);
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      if (ER_i (op1) == 0)
		cpc = BC_pc (cpc);
	      else
		INCREMENT_PC ();
	    }
	  else
#endif
	    {
	      int true_p = non_zero_p (op1, node_mode == BC_NM_bf
				       ? DERR_invalid_if_expr_type
				       : DERR_cond_operand_type);
	      if (! true_p)
		cpc = BC_pc (cpc);
	      else
		INCREMENT_PC ();
	    }
	  if (node_mode == BC_NM_bf)
	    INTERRUPT_CHECK;
	  break;
	case BC_NM_bteqinc:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	  if (execute_btcmpinc (op2, op3, ieq) != NULL)
	    goto common_bteq;
	  break;
	case BC_NM_btneinc:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	  if (execute_btcmpinc (op2, op3, ine) != NULL)
	    goto common_btne;
	  break;
	case BC_NM_btgeinc:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	  if (execute_btcmpinc (op2, op3, ige) != NULL)
	    goto common_btge;
	  break;
	case BC_NM_btltinc:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	  if (execute_btcmpinc (op2, op3, ilt) != NULL)
	    goto common_btlt;
	  break;
	case BC_NM_btleinc:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	  if (execute_btcmpinc (op2, op3, ile) != NULL)
	    goto common_btle;
	  break;
	case BC_NM_btgtinc:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	  if (execute_btcmpinc (op2, op3, igt) != NULL)
	    goto common_btgt;
	  break;
	case BC_NM_bteq:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	common_bteq:
	  if ((op1 = execute_btcmp (op2, op3, BC_NM_eq, ieq, feq,
				    execute_common_eq_ne_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btne:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	common_btne:
	  if ((op1 = execute_btcmp (op2, op3, BC_NM_ne, ine, fne,
				    execute_common_eq_ne_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btge:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	common_btge:
	  if ((op1 = execute_btcmp (op2, op3, BC_NM_ge, ige, fge,
				    execute_common_cmp_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btlt:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	common_btlt:
	  if ((op1 = execute_btcmp (op2, op3, BC_NM_lt, ilt, flt,
				    execute_common_cmp_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btle:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	common_btle:
	  if ((op1 = execute_btcmp (op2, op3, BC_NM_le, ile, fle,
				    execute_common_cmp_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btgt:
	  extract_op1 (&op2);
	  op3 = get_op (BC_bcmp_op2 (cpc));
	common_btgt:
	  if ((op1 = execute_btcmp (op2, op3, BC_NM_gt, igt, fgt,
				    execute_common_cmp_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_bteqi:
	  extract_op1 (&op2);
	  i = BC_bcmp_op2 (cpc);
	  if ((op1 = execute_btcmpi (op2, i, BC_NM_eq, ieq, feq,
				     execute_common_eq_ne_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btnei:
	  extract_op1 (&op2);
	  i = BC_bcmp_op2 (cpc);
	  if ((op1 = execute_btcmpi (op2, i, BC_NM_ne, ine, fne,
				     execute_common_eq_ne_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btlti:
	  extract_op1 (&op2);
	  i = BC_bcmp_op2 (cpc);
	  if ((op1 = execute_btcmpi (op2, i, BC_NM_lt, ilt, flt,
				     execute_common_cmp_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btlei:
	  extract_op1 (&op2);
	  i = BC_bcmp_op2 (cpc);
	  if ((op1 = execute_btcmpi (op2, i, BC_NM_le, ile, fle,
				     execute_common_cmp_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btgti:
	  extract_op1 (&op2);
	  i = BC_bcmp_op2 (cpc);
	  if ((op1 = execute_btcmpi (op2, i, BC_NM_gt, igt, fgt,
				     execute_common_cmp_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_btgei:
	  extract_op1 (&op2);
	  i = BC_bcmp_op2 (cpc);
	  if ((op1 = execute_btcmpi (op2, i, BC_NM_ge, ige, fge,
				     execute_common_cmp_op)) != NULL)
	    goto common_bt;
	  break;
	case BC_NM_bt:
	  extract_op1 (&op1);
	common_bt:
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      if (ER_i (op1) != 0)
		cpc = BC_pc (cpc);
	      else
		INCREMENT_PC ();
	    }
	  else
#endif
	    {
	      int true_p = non_zero_p (op1, DERR_invalid_for_guard_expr_type);

	      if (true_p)
		cpc = BC_pc (cpc);
	      else
		INCREMENT_PC ();
	    }
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_foreach_val:
	case BC_NM_foreach:
	  {
	    ER_node_t tab, tv;
	    val_t *k;

	    extract_op3 (&tv, &op1, &op2);
	    if (ER_NODE_MODE (tv) != ER_NM_tab)
	      eval_error (keyop_bc_decl, get_cpos (),
			  DERR_in_table_operand_type);
	    tab = ER_tab (tv);
	    GO_THROUGH_REDIR (tab);
	    res = get_op (BC_op4 (cpc));
	    k = (val_t *) find_next_key (tab, ER_i (res));
	    if (ER_NODE_MODE ((ER_node_t) k) != ER_NM_empty_entry
		&& ER_NODE_MODE ((ER_node_t) k) != ER_NM_deleted_entry)
	      {
		ER_set_i (res, (k - (val_t *) ER_tab_els (tab)) / 2 + 1);
		if (node_mode == BC_NM_foreach_val)
		  {
		    ER_node_t container = get_op (BC_vcontainer (cpc));
		    ER_node_t index = get_op (BC_vindex (cpc));

		    store_designator_value (container, index,
					    (ER_node_t) (k + 1));
		  }
		store_designator_value (op1, op2, (ER_node_t) k);
		cpc = BC_body_pc (cpc);
	      }
	    else
	      INCREMENT_PC ();
	    INTERRUPT_CHECK;
	    break;
	  }
	case BC_NM_out:
	  {
	    int i;

	    for (i = 0; i < BC_op1 (cpc); i++)
	      heap_pop ();
	    INCREMENT_PC ();
	    INTERRUPT_CHECK;
	    break;
	  }
	case BC_NM_bend:
	  bc_node = ER_block_node (cstack);
	  d_assert (BC_block (cpc) == bc_node);
	  sync_flag = BC_saved_sync_p (bc_node);
	  if (cstack == uppest_stack)
	    return;
	  d_assert (BC_vars_num (bc_node) >= 0);
	  heap_pop ();
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_leave:
	  for (;;)
	    {
	      bc_node = ER_block_node (cstack);
	      sync_flag = BC_saved_sync_p (bc_node);
	      if (BC_NODE_MODE (bc_node) == BC_NM_fblock)
		goto fblock_end;
	      d_assert (BC_NODE_MODE (bc_node) == BC_NM_block
			&& cstack != uppest_stack);
	      d_assert (BC_vars_num (bc_node) >= 0);
	      heap_pop ();
	    }
	  break;
	case BC_NM_fbend:
	  bc_node = ER_block_node (cstack);
	  
	  d_assert (BC_block (cpc) == bc_node);
	  sync_flag = BC_saved_sync_p (bc_node);
	  d_assert (BC_NODE_MODE (bc_node) == BC_NM_fblock);
	  
	fblock_end:
	  if (BC_fun_p (bc_node))
	    ER_SET_MODE (IVAL (ER_ctop (ER_prev_stack (cstack)), 1),
			 ER_NM_undef);
	  else if (BC_thread_p (bc_node))
	    {
	      delete_cprocess ();
	      break;
	    }
	  else if (BC_class_p (bc_node))
	    {
	      ER_node_t res = IVAL (ER_ctop (ER_prev_stack (cstack)), 1);
	      
	      ER_SET_MODE (res, ER_NM_stack);
	      ER_set_stack (res, cstack);
	    }
	  else
	    d_unreachable ();
	  heap_pop ();
	  if (cpc == NULL)
	    return;
	  /* Do not put INTERRUPT here as the result is not on the top
	     and possible GC called directly (or indirectly through
	     thread switching) from INTERRUPT can make the result
	     wrong.  Another solution could be adding a node to pop
	     result as INTERRUPT should be the last statement executed
	     for the node.  */
	  break;
	case BC_NM_ret:
	  extract_op1 (&res);
	  for (;;)
	    {
	      bc_node = ER_block_node (cstack);
	      sync_flag = BC_saved_sync_p (bc_node);
	      if (BC_NODE_MODE (bc_node) == BC_NM_fblock)
		{
		  d_assert (BC_fun_p (bc_node));
		  /* There is no GC since the return execution
		     start. */
		  if (ER_NODE_MODE (res) == ER_NM_undef)
		    {
		      d_assert (BC_ret_decl (cpc) != NULL);
		      eval_error (accessop_bc_decl, get_cpos (),
				  DERR_undefined_value_access,
				  BC_ident (BC_ret_decl (cpc)));
		    }
		  *(val_t *) IVAL (ER_ctop (ER_prev_stack (cstack)), 1)
		    = *(val_t *) res;
		  heap_pop ();
		  if (cpc == NULL)
		    return;
		  break;
		}
	      heap_pop ();
	      d_assert (cstack != NULL);
	    }
	  /* See comment for fbend.  */
	  break;
	case BC_NM_wait:
	  {
	    int true_p;

	    if (sync_flag)
	      eval_error (syncwait_bc_decl, get_cpos (), DERR_wait_in_sync_stmt);
	    extract_op1 (&op1);
	    true_p = non_zero_p (op1, DERR_invalid_wait_guard_expr_type);
	    if (! true_p)
	      block_cprocess (BC_pc (cpc), TRUE);
	    else
	      {
		INCREMENT_PC ();
		sync_flag = TRUE;
	      }
	    break;
	  }
	case BC_NM_waitend:
          sync_flag = FALSE;
          INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_block:
	  d_assert (BC_vars_num (cpc) >= 0);
	  heap_push (cpc, cstack, 0);
	  BC_set_saved_sync_p (cpc, sync_flag);
	  cpc = BC_next (cpc);
	  INTERRUPT_CHECK;
	  break;
	case BC_NM_throw:
	  {
	    const char *message;
	    val_t tvar1;

	    op2 = (ER_node_t) &tvar1;
	    ER_SET_MODE (op2, ER_NM_code);
	    ER_set_code_id (op2, CODE_ID (except_bc_decl));
	    ER_set_code_context (op2, uppest_stack);
	    op1 = get_op (BC_op1 (cpc));
	    if (! ER_IS_OF_TYPE (op1, ER_NM_stack)
		|| ! internal_isa_call (&message, op2, op1))
	      eval_error (optype_bc_decl, get_cpos (),
			  DERR_no_exception_after_throw);
	    exception_position = get_cpos ();
	    cpc = find_catch_pc (ER_stack (op1));
	    if (cpc == NULL)
	      return;
	  }
	  break;
	case BC_NM_except:
	  {
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
		    cpc = find_catch_pc (ER_stack (op1));
		    if (cpc == NULL)
		      return;
		  }
	      }
	  }
	  break;
	case BC_NM_move:
	  extract_op2 (&res, &op1);
	  if (ER_NODE_MODE (op1) == ER_NM_undef)
	    eval_error (accessop_bc_decl, get_cpos (),
			DERR_undefined_value_access,
			BC_ident (BC_rhs_decl (cpc)));
	  *(val_t *) res = *(val_t *) op1;
	  INCREMENT_PC ();
	  break;
	case BC_NM_var:
	  extract_op1 (&res);
	  process_var_val (res, BC_decl (cpc));
	  INCREMENT_PC ();
	  break;
	case BC_NM_lvar:
	case BC_NM_lvarv:
	  extract_op1 (&res);
	  process_var_ref_and_val (res, BC_decl (cpc), node_mode != BC_NM_lvar);
	  INCREMENT_PC ();
	  break;
	case BC_NM_evar:
	case BC_NM_levar:
	case BC_NM_levarv:
	  extract_op1 (&res);
	  process_external_var (res, BC_decl (cpc),
				node_mode != BC_NM_evar,
				node_mode == BC_NM_levarv);
	  INCREMENT_PC ();
	  break;
	case BC_NM_efun:
	  {
	    BC_node_t decl = BC_decl (cpc);
	    
	    extract_op1 (&res);
	    ER_SET_MODE (res, ER_NM_efun);
	    ER_set_efdecl (res, decl);
	    INCREMENT_PC ();
	    break;
	  }
	case BC_NM_fun:
	case BC_NM_class:
	  {
	    BC_node_t fblock, decl = BC_decl (cpc);
	    
	    d_assert (BC_NODE_MODE (decl) == BC_NM_fdecl);
	    extract_op1 (&res);
	    ER_SET_MODE (res, ER_NM_code);
	    fblock = BC_fblock (decl);
	    if (fblock == NULL)
	      eval_error (accessvalue_bc_decl, get_cpos (),
			  DERR_undefined_class_or_fun, BC_ident (decl));
	    ER_set_code_id (res, CODE_ID (fblock));
	    ER_set_code_context
	      (res, find_context_by_scope (BC_decl_scope (decl)));
	    INCREMENT_PC ();
	    break;
	  }
	case BC_NM_rpr:
	case BC_NM_rpr_def:
	  extract_op1 (&res);
	  repl_print (res, node_mode == BC_NM_rpr_def);
	  INCREMENT_PC ();
	  break;
	case BC_NM_nop: /* for DINO developing purposes only.  */
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
      entry = find_tab_entry (tab, (ER_node_t) &key, TRUE);
      if (ER_NODE_MODE (entry) != ER_NM_empty_entry)
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

/* The following variable value is TRUE if we've started evaluation
   and set up longjump buffer `eval_longjump_buff'. */
int eval_long_jump_set_flag;
/* The following variable is used for throwing errors. */
static jmp_buf eval_longjump_buff;

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
  cpc = find_catch_pc (error_instance);
  longjmp (eval_longjump_buff, 1);
}

void
call_fun_class (BC_node_t code, ER_node_t context, int_t pars_number)
{
  pc_t saved_cpc;
  pc_t saved_next_pc;
  int_t saved_process_number;

  saved_cpc = cpc;
  saved_next_pc = BC_next (cpc);
  BC_set_next (cpc, NULL);
  saved_process_number = ER_process_number (cprocess);
  DECR_CTOP (pars_number);
  if (BC_implementation_fun (code) != NULL)
    process_imm_ifun_call (code, pars_number);
  else
    process_imm_fun_call ((val_t *) IVAL (ctop, 1), code, context,
			  pars_number, FALSE);
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
      /* Uncatched exception in REPL. */
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
