/*
   Copyright (C) 1997-2012 Vladimir Makarov.

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
#include "d_runtab.h"
#include "d_eval.h"



/* The following is common code for find_context_by_scope. */
#define FIND_CONTEXT_BY_SCOPE						\
  IR_node_t curr_scope;							\
  IR_node_t func_class_ext;						\
									\
  for (container = cstack, curr_scope = ER_block_node (container);	\
       _scope != curr_scope;						\
       container = ER_context (container),				\
	     curr_scope = ER_block_node (container))			\
    ;									\
  func_class_ext = IR_func_class_ext (_scope);				\
  if (func_class_ext != NULL						\
      && IR_IS_OF_TYPE (func_class_ext, IR_NM_class)			\
      && (ER_block_node (container)					\
	      == ER_block_node (ER_context (container))))		\
    /* We are searching for from the class constructor and the		\
	   current stack corresponds to the constructor stack (not to	\
	   the class instance). */					\
    container = ER_context (container);

#if defined (NO_CONTAINER_CACHE) || !defined (__GNUC__)

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static ER_node_t
find_context_by_scope (IR_node_t _scope)
{
  ER_node_t container;

#ifndef NO_CONTAINER_CACHE
  if (IR_cached_container_tick (_scope) == current_cached_container_tick)
    {
      container = (ER_node_t) IR_cached_container_address (_scope);
      return container;
    }
#endif
  {
    FIND_CONTEXT_BY_SCOPE
  }
#ifndef NO_CONTAINER_CACHE
  IR_set_cached_container_address (_scope, (string_t) container);
  IR_set_cached_container_tick (_scope, current_cached_container_tick);
#endif
  return container;
}

#else

/* The following code is very critical and gcc inliner is not good
   now.  Therefore we use gcc extension for better code generation. */
#define find_context_by_scope(scope)					\
({									\
  IR_node_t _scope = scope;						\
  ER_node_t container;							\
									\
  if (IR_cached_container_tick (_scope) == current_cached_container_tick)\
      container = (ER_node_t) IR_cached_container_address (_scope);	\
  else									\
    {									\
      FIND_CONTEXT_BY_SCOPE						\
      IR_set_cached_container_address (_scope, (string_t) container);	\
      IR_set_cached_container_tick (_scope, current_cached_container_tick);\
    }									\
  container;								\
})

#endif

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
process_var_ref (ER_node_t res, IR_node_t decl)
{
  IR_node_t scope = IR_scope (decl);
  int var_number_in_block = IR_var_number_in_block (decl);
  ER_node_t container;

  container = find_context_by_scope (scope);
  if (ER_NODE_MODE (container) == ER_NM_heap_stack)
    {
      ER_SET_MODE (res, ER_NM_stack);
      ER_set_stack (res, container);
    }
  else
    {
      ER_SET_MODE (res, ER_NM_instance);
      ER_set_instance (res, container);
    }
  ER_SET_MODE (IVAL (res, 1), ER_NM_int);
  ER_set_i (IVAL (res, 1), var_number_in_block);
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
process_var_ref_and_val (ER_node_t res, IR_node_t decl, int val_too_p)
{
  IR_node_t scope = IR_scope (decl);
  int var_number_in_block = IR_var_number_in_block (decl);
  ER_node_t container;

  container = find_context_by_scope (scope);
  if (ER_NODE_MODE (container) == ER_NM_heap_stack)
    {
      ER_SET_MODE (res, ER_NM_stack);
      ER_set_stack (res, container);
      ER_SET_MODE (IVAL (res, 1), ER_NM_int);
      ER_set_i (IVAL (res, 1), var_number_in_block);
      if (val_too_p)
	*(val_t *) IVAL (res, 2) = *(val_t *) IVAL (ER_stack_vars (container),
						    var_number_in_block);
    }
  else
    {
      ER_SET_MODE (res, ER_NM_instance);
      ER_set_instance (res, container);
      ER_SET_MODE (IVAL (res, 1), ER_NM_int);
      ER_set_i (IVAL (res, 1), var_number_in_block);
      if (val_too_p)
	*(val_t *) IVAL (res, 2)
	  = *(val_t *) IVAL (ER_instance_vars (container),
			     var_number_in_block);
    }
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
process_var_val (ER_node_t res, IR_node_t decl)
{
  int var_number_in_block = IR_var_number_in_block (decl);
  IR_node_t scope = IR_scope (decl);
  ER_node_t container;

  container = find_context_by_scope (scope);
  if (ER_NODE_MODE (container) == ER_NM_heap_instance)
    *(val_t *) res = *(val_t *) IVAL (ER_instance_vars (container),
				      var_number_in_block);
  else
    *(val_t *) res = *(val_t *) IVAL (ER_stack_vars (container),
				      var_number_in_block);
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
process_external_var (ER_node_t res, IR_node_t decl,
		      int lvalue_p, int val_too_p)
{
  void *addr;
  
  addr = external_address (decl);
  if (lvalue_p)
    {
      ER_SET_MODE (res, ER_NM_external_var_ref);
      ER_set_external_var_ref (res, addr);
      ER_SET_MODE (IVAL (res, 1), ER_NM_nil);
      if (val_too_p)
	*(val_t *) IVAL (res, 2) = *(val_t *) addr;
    }
  else
    *(val_t *) res = *(val_t *) addr;
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static IR_node_t
get_designator (IR_node_t stmt)
{
  if (IR_IS_OF_TYPE (stmt, IR_NM_foreach_stmt))
    return IR_foreach_designator (stmt);
  else if (IR_IS_OF_TYPE (stmt, IR_NM_assign_stmt))
    return IR_assignment_var (stmt);
  else
    return stmt;
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
check_member_access (IR_node_t decl, IR_node_t block)
{
  if (!IR_public_flag (decl))
    {
      IR_node_t curr_block, friend;

      /* ??? Is it worth to make it more effective (e.g. bit tables). */
      for (curr_block = ER_block_node (cstack);
	   curr_block != NULL;
	   curr_block = IR_block_scope (curr_block))
	if (curr_block == block)
	  break;
	else
	  {
	    for (friend = IR_friends (block);
		 friend != NULL;
		 friend = IR_next_friend (friend))
	      if (IR_friend_decl (friend) == IR_func_class_ext (curr_block))
		break;
	    if (friend != NULL)
	      break;
	  }
      if (curr_block == NULL)
	eval_error (accessop_decl, invaccesses_decl,
		    IR_pos (get_designator (IR_PTR (cpc))),
		    DERR_private_decl_access_from_outside_block,
		    IR_ident_string (IR_unique_ident (IR_ident (decl))));
    }
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
execute_a_period_operation (int block_decl_ident_number, ER_node_t res,
			    ER_node_t op, int lvalue_p, int lvalue_val_p)
{
  IR_node_t decl;
  IR_node_t block;
  ER_node_t container;
  int instance_p;

  if (instance_p = ER_NODE_MODE (op) == ER_NM_instance)
    container = ER_instance (op);
  else if (ER_NODE_MODE (op) == ER_NM_stack)
    container = ER_stack (op);
  else
    eval_error (accessop_decl, invaccesses_decl, IR_pos (cpc),
		DERR_value_is_not_class_instance_or_stack);
  decl = NULL;
  for (;;)
    {
      assert (container != NULL);
      block = ER_block_node (container);
      if (block_decl_ident_number >= 0)
	decl = LV_BLOCK_IDENT_DECL (IR_block_number (block),
				    block_decl_ident_number);
      if (decl != NULL || ER_NODE_MODE (container) != ER_NM_heap_instance)
	break;
      container = ER_context (container);
      if (ER_NODE_MODE (container) != ER_NM_heap_instance)
	break;
    }
  if (decl == NULL)
    eval_error (accessop_decl, invaccesses_decl, IR_pos (cpc),
		DERR_decl_is_absent_in_given_class_or_block);
  else
    check_member_access (decl, block);

  switch (IR_NODE_MODE (decl))
    {
    case IR_NM_var:
      if (! lvalue_p)
	*(val_t *) res
	  = *(val_t *) IVAL ((instance_p
			      ? ER_instance_vars (container)
			      : ER_stack_vars (container)),
			     IR_var_number_in_block (decl));
      else
	{
	  if (! instance_p)
	    {
	      ER_SET_MODE (res, ER_NM_stack);
	      ER_set_stack (res, container);
	      ER_SET_MODE (IVAL (res, 1), ER_NM_int);
	      ER_set_i (IVAL (res, 1), IR_var_number_in_block (decl));
	      if (lvalue_val_p)
		*(val_t *) IVAL (res, 2)
		  = *(val_t *) IVAL (ER_stack_vars (container),
				     IR_var_number_in_block (decl));
	    }
	  else
	    {
	      ER_SET_MODE (res, ER_NM_instance);
	      ER_set_instance (res, container);
	      ER_SET_MODE (IVAL (res, 1), ER_NM_int);
	      ER_set_i (IVAL (res, 1), IR_var_number_in_block (decl));
	      if (lvalue_val_p)
		*(val_t *) IVAL (res, 2)
		  = *(val_t *) IVAL (ER_instance_vars (container),
				     IR_var_number_in_block (decl));
	    }
	}
      break;
    case IR_NM_external_var:
      process_external_var (res, decl, lvalue_p, lvalue_val_p);
      break;
    case IR_NM_external_func:
    case IR_NM_func:
      if (lvalue_p)
	eval_error (accessop_decl, invaccesses_decl,
		    IR_pos (cpc), DERR_func_as_variable);
      if (IR_thread_flag (decl))
	{
	  ER_SET_MODE (res, ER_NM_thread);
	  ER_set_thread_context (res, container);
	  ER_set_thread_no (res, FUNC_CLASS_NO (decl));
	}
      else
	{
	  ER_SET_MODE (res, ER_NM_func);
	  ER_set_func_context (res, container);
	  ER_set_func_no (res, FUNC_CLASS_NO (decl));
	}
      break;
    case IR_NM_class:
      if (lvalue_p)
	eval_error (accessop_decl, invaccesses_decl,
		    IR_pos (cpc), DERR_class_as_variable);
      ER_SET_MODE (res, ER_NM_class);
      ER_set_class_context (res, container);
      ER_set_class_no (res, FUNC_CLASS_NO (decl));
      break;
    default:
      assert (FALSE);
    }
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static int_t
check_vector_index (ER_node_t _vect, ER_node_t _index)
{
  int_t index_value;

  if (ER_NODE_MODE (_index) != ER_NM_int)
    eval_error (indextype_decl, invindexes_decl,
		IR_pos (cpc), DERR_index_is_not_int);
  index_value = ER_i (_index);
  if (index_value < 0
      || (unsigned_int_t) index_value >= ER_els_number (_vect))
    {
      if (index_value < 0)
	eval_error (indexvalue_decl, invindexes_decl,
		    IR_pos (get_designator (IR_PTR (cpc))),
		    DERR_index_is_negative_number);
      else
	eval_error (indexvalue_decl, invindexes_decl,
		    IR_pos (get_designator (IR_PTR (cpc))),
		    DERR_index_is_greater_than_array_bound);
    }
  return index_value;
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
load_vector_element_by_index (ER_node_t to, ER_node_t vect, ER_node_t index)
{
  int_t index_value;
  size_t el_size_type;
  int pack_flag;

  GO_THROUGH_REDIR (vect);
  pack_flag = ER_NODE_MODE (vect) == ER_NM_heap_pack_vect;
  assert (pack_flag || ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect);
  index_value = check_vector_index (vect, index);
  if (pack_flag)
    {
      ER_node_mode_t el_type = ER_pack_vect_el_type (vect);

      ER_SET_MODE (to, el_type);
      switch (el_type)
	{
	case ER_NM_nil:
	  break;
	case ER_NM_hide:
	  ER_set_hide (to, ((hide_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_char:
	  ER_set_ch (to, ((char_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_int:
	  ER_set_i (to, ((int_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_float:
	  ER_set_f (to, ((floating_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_type:
	  ER_set_type (to,
		       ((IR_node_mode_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_vect:
	  ER_set_vect (to, ((ER_node_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_tab:
	  ER_set_tab (to, ((ER_node_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_instance:
	  ER_set_instance (to,
			   ((ER_node_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_process:
	  ER_set_process
	    (to, ((ER_node_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_stack:
	  ER_set_stack (to, ((ER_node_t *) ER_pack_els (vect)) [index_value]);
	  break;
	case ER_NM_func:
	case ER_NM_thread:
	case ER_NM_class:
	  el_size_type = type_size_table [el_type];
	  memcpy ((char *) to + val_displ_table [ER_NODE_MODE (to)],
		  ER_pack_els (vect) + index_value * el_size_type,
		  el_size_type);
	  break;
	default:
	  assert (FALSE);
	}
    }
  else
    memcpy (to, (char *) ER_unpack_els (vect) + index_value * sizeof (val_t),
            sizeof (val_t));
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
store_vector_element (ER_node_t vect, ER_node_t index, ER_node_t val)
{
  size_t el_size_type;
  int_t index_value;
  int pack_flag;

  GO_THROUGH_REDIR (vect);
  pack_flag = ER_NODE_MODE (vect) == ER_NM_heap_pack_vect;
  if (ER_immutable (vect))
    eval_error (immutable_decl, invaccesses_decl,
		IR_pos (get_designator (IR_PTR (cpc))),
		DERR_immutable_vector_modification);
  index_value = check_vector_index (vect, index);
  if (pack_flag && ER_pack_vect_el_type (vect) != ER_NODE_MODE (val))
    {
      vect = unpack_vector (vect);
      pack_flag = FALSE;
    }
  if (pack_flag)
    {
      ER_node_mode_t el_type = ER_pack_vect_el_type (vect);

      switch (el_type)
	{
	case ER_NM_nil:
	  break;
	case ER_NM_hide:
	  ((hide_t *) ER_pack_els (vect)) [index_value] = ER_hide (val);
	  break;
	case ER_NM_char:
	  ((char_t *) ER_pack_els (vect)) [index_value] = ER_ch (val);
	  break;
	case ER_NM_int:
	  ((int_t *) ER_pack_els (vect)) [index_value] = ER_i (val);
	  break;
	case ER_NM_float:
	  ((floating_t *) ER_pack_els (vect)) [index_value] = ER_f (val);
	  break;
	case ER_NM_type:
	  ((IR_node_mode_t *) ER_pack_els (vect)) [index_value]
	    = ER_type (val);
	  break;
	case ER_NM_vect:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value] = ER_vect (val);
	  break;
	case ER_NM_tab:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value] = ER_tab (val);
	  break;
	case ER_NM_instance:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value]
	    = ER_instance (val);
	  break;
	case ER_NM_process:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value] = ER_process (val);
	  break;
	case ER_NM_stack:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value] = ER_stack (val);
	  break;
	case ER_NM_func:
	case ER_NM_thread:
	case ER_NM_class:
	  el_size_type = type_size_table [el_type];
	  memcpy (ER_pack_els (vect) + index_value * el_size_type,
		  (char *) val + val_displ_table [ER_NODE_MODE (val)],
		  el_size_type);
	  break;
	default:
	  assert (FALSE);
	}
    }
  else
      *(val_t *)IVAL (ER_unpack_els (vect), index_value) = *(val_t *)val;
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
load_table_element_by_key (ER_node_t to, ER_node_t tab, ER_node_t key)
{
  ER_node_t entry;

  GO_THROUGH_REDIR (tab);
  entry = find_tab_entry (tab, key, FALSE);
  if (ER_NODE_MODE (entry) == ER_NM_empty_entry
      || ER_NODE_MODE (entry) == ER_NM_deleted_entry)
    eval_error (keyvalue_decl, invkeys_decl, IR_pos (cpc), DERR_no_such_key);
  *(val_t *) to = *(val_t *) INDEXED_ENTRY_VAL (entry, 0);
  assert (ER_IS_OF_TYPE (INDEXED_ENTRY_VAL (entry, 0), ER_NM_val));
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
store_table_element (ER_node_t tab, ER_node_t index, ER_node_t val)
{
  IR_node_t node;
  ER_node_t entry;

  GO_THROUGH_REDIR (tab);
  if (ER_immutable (tab))
    eval_error (immutable_decl, invaccesses_decl,
		IR_pos (get_designator (IR_PTR (cpc))),
		DERR_immutable_table_modification);
  entry = find_tab_entry (tab, index, TRUE);
  *(val_t *) entry = *(val_t *) index;
  make_immutable (entry);
  *(val_t *) INDEXED_ENTRY_VAL (entry, 0) = *(val_t *) val;
  assert (ER_IS_OF_TYPE (INDEXED_ENTRY_VAL (entry, 0), ER_NM_val));
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
load_given_designator_value (ER_node_t to, ER_node_t designator)
{
  if (ER_NODE_MODE (designator) == ER_NM_stack)
    {
      *(val_t *) to
	= *(val_t *) IVAL (ER_stack_vars (ER_stack (designator)),
			   ER_i (IVAL (designator, 1)));
    }
  else if (ER_NODE_MODE (designator) == ER_NM_vect)
    load_vector_element_by_index (to, ER_vect (designator),
				  IVAL (designator, 1));
  else if (ER_NODE_MODE (designator) == ER_NM_instance)
    *(val_t *) to
      = *(val_t *) IVAL (ER_instance_vars (ER_instance (designator)),
			 ER_i (IVAL (designator, 1)));
  else if (ER_NODE_MODE (designator) == ER_NM_tab)
    load_table_element_by_key (to, ER_tab (designator),
			       IVAL (designator, 1));
  else if (ER_NODE_MODE (designator) == ER_NM_external_var_ref)
    *(val_t *) to = *(val_t *) ER_external_var_ref (designator);
  else
    assert (FALSE);
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
store_designator_value (ER_node_t container, ER_node_t index, ER_node_t val,
			IR_node_t des)
{
  if (ER_NODE_MODE (container) == ER_NM_stack)
    *(val_t *) IVAL (ER_stack_vars (ER_stack (container)), ER_i (index))
      = *(val_t *) val;
  else if (ER_NODE_MODE (container) == ER_NM_vect)
    store_vector_element (ER_vect (container), index, val);
  else if (ER_NODE_MODE (container) == ER_NM_instance)
    {
      if (ER_immutable (ER_instance (container)))
	eval_error (immutable_decl, invaccesses_decl,
		    IR_pos (get_designator (IR_PTR (cpc))),
		    DERR_immutable_instance_modification);
      *(val_t *) IVAL (ER_instance_vars (ER_instance (container)),
		       ER_i (index))
	= *(val_t *) val;
    }
  else if (ER_NODE_MODE (container) == ER_NM_tab)
    store_table_element (ER_tab (container), index, val);
  else if (ER_NODE_MODE (container) == ER_NM_external_var_ref)
    *(val_t *) ER_external_var_ref (container) = *(val_t *) val;
  else
    {
      if (IR_IS_OF_TYPE (des, IR_NM_vec_index_designator))
	eval_error (indexop_decl, invindexes_decl, IR_pos (des),
		    DERR_index_operation_for_non_array);
      else if (IR_IS_OF_TYPE (des, IR_NM_key_index_designator))
	eval_error (keyop_decl, invkeys_decl, IR_pos (des),
		    DERR_key_index_operation_for_non_table);
      else if (IR_IS_OF_TYPE (des, IR_NM_period_designator))
	eval_error (accessop_decl, invaccesses_decl, IR_pos (des),
		    DERR_value_is_not_class_instance_or_stack);
      assert (FALSE);
    }
}

/* The following variable value is source of the exception (error)
   occurrence. */
static position_t exception_position;

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static pc_t
find_catch_pc (ER_node_t instance)
{
  IR_node_t block;
  IR_node_t func_class;
  ER_node_t except;
  IR_node_t curr_scope;
  ER_node_t message;
  struct trace_stack_elem elem;

  assert (ER_NODE_MODE (instance) == ER_NM_instance);
  except = ER_instance (instance);
  if (trace_flag)
    VLO_NULLIFY (trace_stack);
  for (; cstack != uppest_stack;)
    {
      block = ER_block_node (cstack);
      sync_flag = IR_block_saved_sync_flag (block);
      func_class = IR_func_class_ext (block);
      if (trace_flag && func_class != NULL)
	{
	  elem.func_class = func_class;
	  elem.pc = ER_call_pc (cstack);
	  VLO_ADD_MEMORY (trace_stack, &elem, sizeof (elem));
	}
      if (func_class != NULL && IR_NODE_MODE (func_class) == IR_NM_func
	  && IR_thread_flag (func_class)
	  && !delete_cprocess_during_exception ())
	break;
      heap_pop ();
      /* Set up ctop as it should be for any statement begin.  */
      if (cstack != NULL)
	ctop = (ER_node_t) ((char *) cvars
			    + real_block_vars_number (ER_block_node (cstack))
			    * sizeof (val_t) - sizeof (val_t));
      cpc = IR_catch_list_pc (block);
      if (cpc != NULL)
	{
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_instance);
	  ER_set_instance (ctop, except);
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
  for (curr_scope = IR_next_stmt (ER_instance_class (except));
       curr_scope != NULL;
       curr_scope = IR_block_scope (curr_scope))
    if (IR_func_class_ext (curr_scope) == error_decl)
      break;
  message = IVAL (ER_instance_vars (except), 0);
  if (curr_scope != NULL && ER_NODE_MODE (message) == ER_NM_vect)
    {
      ER_node_t vect;

      vect = ER_vect (message);
      GO_THROUGH_REDIR (vect);
      if (ER_NODE_MODE (vect) != ER_NM_heap_pack_vect)
	pack_vector_if_possible (vect);
      if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (vect) == ER_NM_char)
	/* No return after error. */
	error (TRUE, exception_position, ER_pack_els (vect));
    }
  if (ER_instance_class (except) != sigint_decl
      && ER_instance_class (except) != sigterm_decl)
    error (TRUE, exception_position, DERR_unprocessed_exception,
	   IR_ident_string (IR_unique_ident
			    (IR_ident (ER_instance_class (except)))));
  else
    dino_finish (1);
  return NULL; /* to prevent compiler diagnostic. */
}

/* The following macro is code for execution arithmethic operation OP
   and reporting MSG if there are errors. */
#define EXECUTE_AR_OP(OP, result, left, right, MSG)			  \
  do                                                                      \
    {                                                                     \
      ER_node_t _l, _r, _res = result, _op1 = left, _op2 = right;	  \
      int_t _i;								  \
      floating_t _f;							  \
      									  \
      if (ER_NODE_MODE (_op2) == ER_NM_int                                \
	  && ER_NODE_MODE (_op1) == ER_NM_int)			          \
        {							          \
          _i = ER_i (_op1) OP ER_i (_op2);			          \
          ER_SET_MODE (_res, ER_NM_int);				  \
	  ER_set_i (_res, _i);		       			          \
        }							          \
      else if (ER_NODE_MODE (_op2) == ER_NM_float                         \
	       && ER_NODE_MODE (_op1) == ER_NM_float)                     \
	{							          \
	  _f = ER_f (_op1) OP ER_f (_op2);				  \
          ER_SET_MODE (_res, ER_NM_float);			          \
	  ER_set_f (_res, _f);				       	          \
	}							          \
      else                                                                \
	{                                                              	  \
	  implicit_conversion_for_binary_arithmetic_op			  \
            (_op1, _op2, &_l, &_r);					  \
	  if (ER_NODE_MODE (_r) != ER_NM_int                              \
	      && ER_NODE_MODE (_r) != ER_NM_float                         \
	      || ER_NODE_MODE (_l) != ER_NM_int                           \
	      && ER_NODE_MODE (_l) != ER_NM_float)                        \
	    eval_error (optype_decl, invops_decl, IR_pos (cpc), MSG);     \
	  if (ER_NODE_MODE (_l) == ER_NM_int)				  \
            {								  \
	      _i = ER_i (_l) OP ER_i (_r);				  \
	      ER_SET_MODE (_res, ER_NM_int);				  \
	      ER_set_i (_res, _i);					  \
	    }								  \
	  else								  \
	    {								  \
	      _f = ER_f (_l) OP ER_f (_r);				  \
	      ER_SET_MODE (_res, ER_NM_float);				  \
	      ER_set_f (_res, _f);					  \
	    }								  \
	}                                                                 \
    }                                                                     \
  while (0)

/* The following macro is code for execution integer operation OP with
   CAST and reporting MSG if there are errors. */
#define EXECUTE_INT_OP(OP, CAST, result, left, right, MSG)              \
  do                                                                    \
    {                                                                   \
      int_t _i;								\
      ER_node_t _l, _r, _res = result, _op1 = left, _op2 = right;	\
									\
      if (ER_NODE_MODE (_op1) == ER_NM_int		       		\
          && ER_NODE_MODE (_op2) == ER_NM_int)				\
	{                                                               \
	  _i = (CAST) ER_i (_op1) OP ER_i (_op2);	       		\
	  ER_SET_MODE (_res, ER_NM_int);				\
	  ER_set_i (_res, _i); 	       				        \
	}								\
      else								\
        {								\
	  implicit_conversion_for_binary_int_op (_op1, _op2, &_l, &_r);	\
	  if (ER_NODE_MODE (_r) != ER_NM_int                            \
	      || ER_NODE_MODE (_l) != ER_NM_int)                        \
	    eval_error (optype_decl, invops_decl,                       \
			IR_pos (cpc), MSG);                     	\
	  _i = (CAST) ER_i (_l) OP ER_i (_r);		       		\
	  ER_SET_MODE (_res, ER_NM_int);				\
	  ER_set_i (_res, _i);   	       			        \
	}                                                               \
    }                                                                   \
  while (0)

/* The following function implements division. */
#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
execute_div_op (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  int_t i;
  floating_t f;
  ER_node_t l, r;

  if (ER_NODE_MODE (op2) == ER_NM_int
      && ER_NODE_MODE (op1) == ER_NM_int)
    {
      i = ER_i (op1) / ER_i (op2);
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
    }
  else if (ER_NODE_MODE (op2) == ER_NM_float
	   && ER_NODE_MODE (op1) == ER_NM_float)
    {
      f = ER_f (op1) / ER_f (op2);
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f);
    }
  else
    {
      implicit_conversion_for_binary_arithmetic_op (op1, op2, &l, &r);
      if (ER_NODE_MODE (r) != ER_NM_int
	  && ER_NODE_MODE (r) != ER_NM_float
	  || ER_NODE_MODE (l) != ER_NM_int
	  && ER_NODE_MODE (l) != ER_NM_float)
	eval_error (optype_decl, invops_decl,
		    IR_pos (cpc), DERR_div_operands_types);
      if (ER_NODE_MODE (l) == ER_NM_int)
	{
	  i = ER_i (l) / ER_i (r);
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, i);
	}
      else
	{
	  f = ER_f (l) / ER_f (r);
	  ER_SET_MODE (res, ER_NM_float);
	  ER_set_f (res, f);
	}
    }
}

/* The following function implements modulo op. */
#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
execute_mod_op (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  int_t i;
  floating_t f;
  ER_node_t l, r;

  if (ER_NODE_MODE (op2) == ER_NM_int
      && ER_NODE_MODE (op1) == ER_NM_int)
    {
      i = ER_i (op1) % ER_i (op2);
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, i);
    }
  else if (ER_NODE_MODE (op2) == ER_NM_float
	   && ER_NODE_MODE (op1) == ER_NM_float)
    {
      f = fmod (ER_f (op1), ER_f (op2));
      ER_SET_MODE (res, ER_NM_float);
      ER_set_f (res, f);
    }
  else
    {
      implicit_conversion_for_binary_arithmetic_op (op1, op2, &l, &r);
      if (ER_NODE_MODE (r) != ER_NM_int
	  && ER_NODE_MODE (r) != ER_NM_float
	  || ER_NODE_MODE (l) != ER_NM_int
	  && ER_NODE_MODE (l) != ER_NM_float)
	eval_error (optype_decl, invops_decl,
		    IR_pos (cpc), DERR_mod_operands_types);
      if (ER_NODE_MODE (l) == ER_NM_int)
	{
	  i = ER_i (l) % ER_i (r);
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, i);
	}
      else
	{
	  f = fmod (ER_f (l), ER_f (r));
	  ER_SET_MODE (res, ER_NM_float);
	  ER_set_f (res, f);
	}
    }
}

/* The following function implements array concatenation. */
#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
execute_concat_op (ER_node_t res, ER_node_t op1, ER_node_t op2)
{
  size_t els_number;
  ER_node_t vect, vect1, vect2;

  op2 = to_vect_string_conversion (op2, NULL, tvar_num2);
  op1 = to_vect_string_conversion (op1, NULL, tvar_num1);
  if (ER_NODE_MODE (op2) != ER_NM_vect
      || ER_NODE_MODE (op1) != ER_NM_vect)
    eval_error (optype_decl, invops_decl,
		IR_pos (cpc), DERR_concat_operands_types);
  vect1 = ER_vect (op1);
  vect2 = ER_vect (op2);
  if (ER_NODE_MODE (vect2)
      != ER_NODE_MODE (vect1)
      || ER_NODE_MODE (vect2) == ER_NM_heap_pack_vect
      && (ER_pack_vect_el_type (vect2)
	  != ER_pack_vect_el_type (vect1)))
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
	assert (FALSE);
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
      ER_set_vect (res, result);
    }
  else
    {
      ER_node_t result;
      
      fprintf (stderr, "concat...");
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
      ER_set_vect (res, result);
    }
}


static void
evaluate_code (void)
{
  int cmp;
  int_t i;
  floating_t f;
  ER_node_t res, op1, op2, l, r;
  IR_node_mode_t node_mode;

  for (;;)
    {
      switch (node_mode = IR_NODE_MODE (IR_PTR (cpc)))
	{
	case IR_NM_nil:
	  res = IVAL (cvars, IR_nil_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_nil);
	  INCREMENT_PC ();
	  break;
	case IR_NM_char:
	  res = IVAL (cvars, IR_char_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_char);
	  ER_set_ch (res, IR_ch_val (IR_PTR (cpc)));
	  INCREMENT_PC ();
	  break;
	case IR_NM_int:
	  res = IVAL (cvars, IR_int_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, IR_i_val (IR_PTR (cpc)));
	  INCREMENT_PC ();
	  break;
	case IR_NM_float:
	  res = IVAL (cvars, IR_float_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_float);
	  ER_set_f (res, IR_f_val (IR_PTR (cpc)));
	  INCREMENT_PC ();
	  break;
	case IR_NM_string:
	  {
	    ER_node_t vect;

	    vect = create_string (IR_str_val (IR_PTR (cpc)));
	    res = IVAL (cvars, IR_string_result_num (IR_PTR (cpc)));
	    ER_SET_MODE (res, ER_NM_vect);
	    ER_set_vect (res, vect);
	    INCREMENT_PC ();
	  }
	  break;
	case IR_NM_hide_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_hide);
	  INCREMENT_PC ();
	  break;
	case IR_NM_hideblock_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_hideblock);
	  INCREMENT_PC ();
	  break;
	case IR_NM_char_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_char);
	  INCREMENT_PC ();
	  break;
	case IR_NM_int_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_int);
	  INCREMENT_PC ();
	  break;
	case IR_NM_float_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_float);
	  INCREMENT_PC ();
	  break;
	case IR_NM_vector_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_vect);
	  INCREMENT_PC ();
	  break;
	case IR_NM_table_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_tab);
	  INCREMENT_PC ();
	  break;
	case IR_NM_func_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_func);
	  INCREMENT_PC ();
	  break;
	case IR_NM_thread_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_thread);
	  INCREMENT_PC ();
	  break;
	case IR_NM_class_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_class);
	  INCREMENT_PC ();
	  break;
	case IR_NM_stack_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_stack);
	  INCREMENT_PC ();
	  break;
	case IR_NM_process_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_process);
	  INCREMENT_PC ();
	  break;
	case IR_NM_instance_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_instance);
	  INCREMENT_PC ();
	  break;
	case IR_NM_type_type:
	  res = IVAL (cvars, IR_type_result_num (IR_PTR (cpc)));
	  ER_SET_MODE (res, ER_NM_type);
	  ER_set_type (res, ER_NM_type);
	  INCREMENT_PC ();
	  break;
	case IR_NM_period:
	case IR_NM_lvalue_period:
	case IR_NM_lvalue_period_and_val:
	  execute_a_period_operation
	    (IR_right_block_decl_ident_number (IR_PTR (cpc)),
	     IVAL (cvars, IR_designator_result_num (IR_PTR (cpc))),
	     IVAL (cvars, IR_designator_op_num (IR_PTR (cpc))),
	     node_mode != IR_NM_period,
	     node_mode == IR_NM_lvalue_period_and_val);
	  INCREMENT_PC ();
	  break;
	case IR_NM_logical_or:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    cmp = ER_i (op1) != 0;
	  else
#endif
	    {
	      op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	      if (ER_NODE_MODE (op1) != ER_NM_int
		  && ER_NODE_MODE (op1) != ER_NM_float)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_logical_or_operands_types);
	      if (ER_NODE_MODE (op1) == ER_NM_int)
		cmp = ER_i (op1) != 0;
	      else
		cmp = ER_f (op1) != 0.0;
	    }
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, cmp);
	  if (cmp)
	    cpc = IR_short_path_pc (IR_PTR (cpc));
	  else
	    INCREMENT_PC ();
	  break;
	case IR_NM_logical_and:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    cmp = ER_i (op1) != 0;
	  else
#endif
	    {
	      op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	      if (ER_NODE_MODE (op1) != ER_NM_int
		  && ER_NODE_MODE (op1) != ER_NM_float)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_logical_and_operands_types);
	      if (ER_NODE_MODE (op1) == ER_NM_int)
		cmp = ER_i (op1) != 0;
	      else
		cmp = ER_f (op1) != 0.0;
	    }
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, cmp);
	  if (! cmp)
	    cpc = IR_short_path_pc (IR_PTR (cpc));
	  else
	    INCREMENT_PC ();
	  break;
	case IR_NM_logical_or_end:
	case IR_NM_logical_and_end:
	  res = IVAL (cvars, IR_cont_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_cont_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    cmp = ER_i (op1) != 0;
	  else
#endif
	    {
	      op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	      if (ER_NODE_MODE (op1) != ER_NM_int
		  && ER_NODE_MODE (op1) != ER_NM_float)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    (node_mode == IR_NM_logical_or_end
			     ? DERR_logical_or_operands_types
			     : DERR_logical_and_operands_types));
	      if (ER_NODE_MODE (op1) == ER_NM_int)
		cmp = ER_i (op1) != 0;
	      else
		cmp = ER_f (op1) != 0.0;
	    }
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, cmp);
	  INCREMENT_PC ();
	  break;
	case IR_NM_in:
	  {
	    ER_node_t tab;
	    ER_node_t entry;
	    
	    op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
	    if (ER_NODE_MODE (op2) != ER_NM_tab)
	      eval_error (keyop_decl, invkeys_decl,
			  IR_pos (cpc), DERR_in_table_operand_type);
	    tab = ER_tab (op2);
	    GO_THROUGH_REDIR (tab);
	    entry = find_tab_entry (tab,
				    IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
				    FALSE);
	    res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	    cmp = (ER_NODE_MODE (entry) != ER_NM_empty_entry
		   && ER_NODE_MODE (entry) != ER_NM_deleted_entry);
	    ER_SET_MODE (res, ER_NM_int);
	    ER_set_i (res, cmp);
	    INCREMENT_PC ();
	    break;
	  }
	case IR_NM_not:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    cmp = ER_i (op1) == 0;
	  else
#endif
	    {
	      op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	      if (ER_NODE_MODE (op1) != ER_NM_int
		  && ER_NODE_MODE (op1) != ER_NM_float)
		eval_error (optype_decl, invops_decl,
			    IR_pos (cpc), DERR_not_operand_type);
	      if (ER_NODE_MODE (op1) == ER_NM_int)
		cmp = ER_i (op1) == 0;
	      else
		cmp = ER_f (op1) == 0.0;
	    }
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, cmp);
	  INCREMENT_PC ();
	  break;
	case IR_NM_bitwise_not:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    i = ~ER_i (op1);
	  else
#endif
	    {
	      op1 = implicit_int_conversion (op1, tvar_num1);
	      if (ER_NODE_MODE (op1) != ER_NM_int)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_bitwise_not_operand_type);
	      i = ~ER_i (op1);
	    }
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, i);
	  INCREMENT_PC ();
	  break;
	case IR_NM_eq:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_left_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int
	      && ER_NODE_MODE (op2) == ER_NM_int)
	    {
	      cmp = ER_i (op1) == ER_i (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  goto common_eq_ne;
#endif
	case IR_NM_ne:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_left_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int
	      && ER_NODE_MODE (op2) == ER_NM_int)
	    {
	      cmp = ER_i (op1) != ER_i (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  /* Fall through. */
#endif
	common_eq_ne:
	  implicit_conversion_for_eq_op (op1, op2, &l, &r);
	  if (ER_NODE_MODE (r) != ER_NODE_MODE (l))
	    cmp = 0;
	  else
	    switch (ER_NODE_MODE (r))
	      {
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
		assert (FALSE); /* ALWAYS transformed to string/int/float */
		break;
	      case ER_NM_int:
		cmp = ER_i (r) == ER_i (l);
		break;
	      case ER_NM_float:
		cmp = ER_f (r) == ER_f (l);
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
	      case ER_NM_instance:
		cmp = eq_instance (ER_instance (r), ER_instance (l));
		break;
	      case ER_NM_func:
		cmp = (ER_func_no (r) == ER_func_no (l)
		       && (ER_func_context (r) == ER_func_context (l)));
		break;
	      case ER_NM_thread:
		cmp = (ER_thread_no (r) == ER_thread_no (l)
		       && (ER_thread_context (r) == ER_thread_context (l)));
		break;
	      case ER_NM_process:
		cmp = ER_process (r) == ER_process (l);
		break;
	      case ER_NM_class:
		cmp = (ER_class_no (r) == ER_class_no (l)
		       && (ER_class_context (r) == ER_class_context (l)));
		break;
	      case ER_NM_stack:
		cmp = ER_stack (r) == ER_stack (l);
		break;
	      default:
		assert (FALSE);
	      }
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, IR_NODE_MODE (IR_PTR (cpc)) == IR_NM_eq ? cmp : !cmp);
	  INCREMENT_PC ();
	  break;
	case IR_NM_identity:
	case IR_NM_unidentity:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_left_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) != ER_NODE_MODE (op2))
	    cmp = FALSE;
	  else
	    switch (ER_NODE_MODE (op1))
	      {
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
	      case ER_NM_instance:
		cmp = ER_instance (op1) == ER_instance (op2);
		break;
	      case ER_NM_func:
		/* We don't check the context here. */
		cmp = (ER_func_no (op1) == ER_func_no (op2));
		break;
	      case ER_NM_thread:
		/* We don't check the context here. */
		cmp = (ER_thread_no (op1) == ER_thread_no (op2));
		break;
	      case ER_NM_process:
		cmp = ER_process (op1) == ER_process (op2);
		break;
	      case ER_NM_class:
		/* We don't check the context here. */
		cmp = ER_class_no (op1) == ER_class_no (op2);
		break;
	      case ER_NM_stack:
		cmp = ER_stack (op1) == ER_stack (op2);
		break;
	      default:
		assert (FALSE);
	      }
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i
	    (res, IR_NODE_MODE (IR_PTR (cpc)) == IR_NM_identity ? cmp : !cmp);
	  INCREMENT_PC ();
	  break;
	case IR_NM_lt:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_left_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op2) == ER_NM_int
	      && ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      cmp = ER_i (op1) < ER_i (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  else if (ER_NODE_MODE (op2) == ER_NM_float
		   && ER_NODE_MODE (op1) == ER_NM_float)
	    {
	      cmp = ER_f (op1) < ER_f (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  goto common_cmp;
#endif
	case IR_NM_ge:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_left_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op2) == ER_NM_int
	      && ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      cmp = ER_i (op1) >= ER_i (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  else if (ER_NODE_MODE (op2) == ER_NM_float
		   && ER_NODE_MODE (op1) == ER_NM_float)
	    {
	      cmp = ER_f (op1) >= ER_f (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  goto common_cmp;
#endif
	case IR_NM_gt:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_left_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op2) == ER_NM_int
	      && ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      cmp = ER_i (op1) > ER_i (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  else if (ER_NODE_MODE (op2) == ER_NM_float
		   && ER_NODE_MODE (op1) == ER_NM_float)
	    {
	      cmp = ER_f (op1) > ER_f (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  goto common_cmp;
#endif
	case IR_NM_le:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_left_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op2) == ER_NM_int
	      && ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      cmp = ER_i (op1) <= ER_i (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  else if (ER_NODE_MODE (op2) == ER_NM_float
		   && ER_NODE_MODE (op1) == ER_NM_float)
	    {
	      cmp = ER_f (op1) <= ER_f (op2);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, cmp);
	      INCREMENT_PC ();
	      break;
	    }
	  /* Fall through */
#endif
	common_cmp:
	  implicit_conversion_for_binary_arithmetic_op (op1, op2, &l, &r);
	  if (ER_NODE_MODE (r) != ER_NM_int
	      && ER_NODE_MODE (r) != ER_NM_float
	      || ER_NODE_MODE (l) != ER_NM_int
	      && ER_NODE_MODE (l) != ER_NM_float)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			(node_mode == IR_NM_lt
			 ? DERR_lt_operands_types
			 : (node_mode == IR_NM_gt
			    ? DERR_gt_operands_types
			    : (node_mode == IR_NM_le
			       ? DERR_le_operands_types
			       : DERR_ge_operands_types))));
	  if (ER_NODE_MODE (l) == ER_NM_int)
	    cmp = (node_mode == IR_NM_lt ? ER_i (l) < ER_i (r)
		   : (node_mode == IR_NM_gt ? ER_i (l) > ER_i (r)
		      : (node_mode == IR_NM_le ? ER_i (l) <= ER_i (r)
			 : ER_i (l) >= ER_i (r))));
	  else
	    cmp = (node_mode == IR_NM_lt ? ER_f (l) < ER_f (r)
		   : (node_mode == IR_NM_gt ? ER_f (l) > ER_f (r)
		      : (node_mode == IR_NM_le ? ER_f (l) <= ER_f (r)
			 : ER_f (l) >= ER_f (r))));
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, cmp);
	  INCREMENT_PC ();
	  break;
	case IR_NM_unary_plus:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      i = ER_i (op1);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, i);
	      INCREMENT_PC ();
	      break;
	    }
	  goto common_unary_plus_minus;
#endif
	case IR_NM_unary_minus:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      i = -ER_i (op1);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, i);
	      INCREMENT_PC ();
	      break;
	    }
	  /* Fall through */
#endif
	common_unary_plus_minus:
	  op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	  if (ER_NODE_MODE (op1) != ER_NM_int
	      && ER_NODE_MODE (op1) != ER_NM_float)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			(node_mode == IR_NM_unary_plus
			 ? DERR_unary_plus_operand_type
			 : DERR_unary_minus_operand_type));
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      i = node_mode == IR_NM_unary_plus ? ER_i (op1) : -ER_i (op1);
	      ER_SET_MODE (res, ER_NM_int);
	      ER_set_i (res, i);
	    }
	  else
	    {
	      f = node_mode == IR_NM_unary_plus ? ER_f (op1) : -ER_f (op1);
	      ER_SET_MODE (res, ER_NM_float);
	      ER_set_f (res, f);
	    }
	  INCREMENT_PC ();
	  break;
	case IR_NM_length:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  op1 = to_vect_string_conversion (op1, NULL, tvar_num1);
	  if (ER_NODE_MODE (op1) != ER_NM_vect
	      && ER_NODE_MODE (op1) != ER_NM_tab)
	    eval_error (optype_decl, invops_decl,
			IR_pos (cpc), DERR_length_operand_type);
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
	  INCREMENT_PC ();
	  break;
	case IR_NM_const:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  make_immutable (op1);
	  *(val_t *) res = *(val_t *) op1;
	  INCREMENT_PC ();
	  break;
	case IR_NM_new:
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) == ER_NM_vect)
	    {
	      ER_node_t vect = ER_vect (op1);
	      
	      GO_THROUGH_REDIR (vect);
	      vect = copy_vector (vect);
	      res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	      ER_SET_MODE (res, ER_NM_vect);
	      ER_set_vect (res, vect);
	    }
	  else if (ER_NODE_MODE (op1) == ER_NM_tab)
	    {
	      ER_node_t tab = ER_tab (op1);
	      
	      GO_THROUGH_REDIR (tab);
	      tab = copy_tab (tab);
	      res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	      ER_SET_MODE (res, ER_NM_tab);
	      ER_set_tab (res, tab);
	    }
	  else if (ER_NODE_MODE (op1) == ER_NM_instance)
	    {
	      size_t size, un;
	      ER_node_t instance;
	      
	      size = instance_size (ER_instance_class (ER_instance (op1)));
	      instance = heap_allocate (size, FALSE);
	      ER_SET_MODE (instance, ER_NM_heap_instance);
	      un = ER_unique_number (instance);
	      memcpy (instance, ER_instance (op1), size);
	      ER_set_immutable (instance, FALSE);
	      ER_set_unique_number (instance, un);
	      res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	      ER_SET_MODE (res, ER_NM_instance);
	      ER_set_instance (res, instance);
	    }
	  INCREMENT_PC ();
	  break;
	case IR_NM_typeof:
	  {
	    IR_node_mode_t type;
	    
	    res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	    op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	    type = ER_NODE_MODE (op1);
	    ER_SET_MODE (res, ER_NM_type);
	    ER_set_type (res, type);
	    INCREMENT_PC ();
	    break;
	  }
	case IR_NM_charof:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  op1 = implicit_int_conversion (op1, tvar_num1);
	  if (ER_NODE_MODE (op1) != ER_NM_int)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
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
	  INCREMENT_PC ();
	  break;
	case IR_NM_intof:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  op1 = implicit_int_conversion (op1, tvar_num1);
	  if (ER_NODE_MODE (op1) != ER_NM_int)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			DERR_conversion_to_int_operand_type);
	  i = ER_i (op1);
	  ER_SET_MODE (res, ER_NM_int);
	  ER_set_i (res, i);
	  INCREMENT_PC ();
	  break;
	case IR_NM_floatof:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    f = ER_i (op1);
	  else if (ER_NODE_MODE (op1) == ER_NM_float)
	    f = ER_f (op1);
	  else
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			DERR_conversion_to_float_operand_type);
	  ER_SET_MODE (res, ER_NM_float);
	  ER_set_f (res, f);
	  INCREMENT_PC ();
	  break;
	case IR_NM_format_vectorof:
	  {
	    ER_node_t vect;
	    
	    res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	    op1 = IVAL (cvars, IR_left_op_num (IR_PTR (cpc)));
	    op2 = IVAL (cvars, IR_right_op_num (IR_PTR (cpc)));
	    if (ER_NODE_MODE (op2) != ER_NM_nil)
	      {
		if (ER_NODE_MODE (op1) != ER_NM_char
		    && ER_NODE_MODE (op1) != ER_NM_int
		    && ER_NODE_MODE (op1) != ER_NM_float
		    && (ER_NODE_MODE (ER_vect (op1))
			!= ER_NM_heap_pack_vect
			|| (ER_pack_vect_el_type (ER_vect (op1))
			    != ER_NM_char)))
		  eval_error (optype_decl, invops_decl, IR_pos (cpc),
			      DERR_format_conversion_to_vector_operand_type);
		op2 = to_vect_string_conversion (op2, NULL, tvar_num2);
		if (ER_NODE_MODE (op2) != ER_NM_vect
		    || ER_NODE_MODE (ER_vect (op2)) != ER_NM_heap_pack_vect
		    || ER_pack_vect_el_type (ER_vect (op2)) != ER_NM_char)
		  eval_error (optype_decl, invops_decl, IR_pos (cpc),
			      DERR_vector_conversion_format_type);
		op1 = to_vect_string_conversion (op1,
						 ER_pack_els (ER_vect (op2)),
						 tvar_num1);
		vect = ER_vect (op1);
		ER_SET_MODE (res, ER_NM_vect);
		ER_set_vect (res, vect);
		INCREMENT_PC ();
		break;
	      }
	  }
	  goto common_vectorof;
	case IR_NM_vectorof:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	common_vectorof:
	  {
	    ER_node_t vect;
	    
	    op1 = to_vect_string_conversion (op1, NULL, tvar_num1);
	    if (ER_NODE_MODE (op1) == ER_NM_vect)
	      vect = ER_vect (op1);
	    else if (ER_NODE_MODE (op1) == ER_NM_tab) 
	      vect = table_to_vector_conversion (ER_tab (op1));
	    else
	      eval_error (optype_decl, invops_decl, IR_pos (cpc),
			  DERR_conversion_to_vector_operand_type);
	    ER_SET_MODE (res, ER_NM_vect);
	    ER_set_vect (res, vect);
	    INCREMENT_PC ();
	    break;
	  }
	case IR_NM_tableof:
	  {
	    ER_node_t tab;
	    
	    res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	    op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	    if (ER_NODE_MODE (op1) == ER_NM_tab)
	      tab = ER_tab (op1);
	    else
	      {
		op1 = to_vect_string_conversion (op1, NULL, tvar_num1);
		if (ER_NODE_MODE (op1) == ER_NM_vect)
		  tab = vector_to_table_conversion (ER_vect (op1));
		else
		  eval_error (optype_decl, invops_decl, IR_pos (cpc),
			      DERR_conversion_to_table_operand_type);
	      }
	    ER_SET_MODE (res, ER_NM_tab);
	    ER_set_tab (res, tab);
	    INCREMENT_PC ();
	    break;
	  }
	case IR_NM_funcof:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) == ER_NM_stack
	      && IR_func_class_ext (ER_block_node (ER_stack (op1))) != NULL
	      && !IR_thread_flag (IR_func_class_ext (ER_block_node
						     (ER_stack (op1)))))
	    {
	      ER_node_t stack;
	      
	      stack = ER_stack (op1);
	      ER_SET_MODE (res, ER_NM_func);
	      ER_set_func_no (res,
			      FUNC_CLASS_NO (IR_func_class_ext
					     (ER_block_node (stack))));
	      ER_set_func_context (res, ER_context (stack));
	    }
	  else
	    ER_SET_MODE (res, ER_NM_nil);
	  INCREMENT_PC ();
	  break;
	case IR_NM_threadof:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) == ER_NM_process
	      && ER_thread_func (ER_process (op1)) != NULL)
	    {
	      ER_node_t process;
	      
	      process = ER_process (op1);
	      ER_SET_MODE (res, ER_NM_thread);
	      ER_set_thread_no (res, FUNC_CLASS_NO (ER_thread_func (process)));
	      ER_set_thread_context (res, ER_context (process));
	    }
	  else
	    ER_SET_MODE (res, ER_NM_nil);
	  INCREMENT_PC ();
	  break;
	case IR_NM_classof:
	  res = IVAL (cvars, IR_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) == ER_NM_instance)
	    {
	      ER_node_t instance;
	      
	      instance = ER_instance (op1);
	      ER_SET_MODE (res, ER_NM_class);
	      ER_set_class_no (res,
			       FUNC_CLASS_NO (ER_instance_class (instance)));
	      ER_set_class_context (res, ER_context (instance));
	    }
	  else
	    ER_SET_MODE (res, ER_NM_nil);
	  INCREMENT_PC ();
	  break;
	case IR_NM_cond:
	  op1 = IVAL (cvars, IR_cond_op_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      if (ER_i (op1) != 0)
		INCREMENT_PC ();
	      else
		cpc = IR_false_path_pc (IR_PTR (cpc));
	    }
	  else
#endif
	    {
	      op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	      if (ER_NODE_MODE (op1) != ER_NM_int
		  && ER_NODE_MODE (op1) != ER_NM_float)
		eval_error (optype_decl, invops_decl,
			    IR_pos (cpc), DERR_cond_operand_type);
	      if (ER_NODE_MODE (op1) == ER_NM_int && ER_i (op1) != 0
		  || ER_NODE_MODE (op1) == ER_NM_float && ER_f (op1) != 0.0)
		INCREMENT_PC ();
	      else
		cpc = IR_false_path_pc (IR_PTR (cpc));
	    }
	  break;
	case IR_NM_vector:
	  {
	    /* If you make a change here, please look at DINO read
	       functions. */
	    ER_node_t vect, saved_ctop;
	    int_t vect_parts_number, curr_vect_part_number;
	    int_t curr_vect_element_number;
	    size_t el_size_type;
	    size_t els_number;
	    int_t repetition;
	    int pack_flag;
	    
	    vect_parts_number = IR_parts_number (IR_PTR (cpc));
	    if (vect_parts_number == 0)
	      vect = create_empty_vector ();
	    else
	      {
		saved_ctop = ctop;
		ctop = op1 = IVAL (cvars, IR_vec_tab_el_num (IR_PTR (cpc)));
		DECR_CTOP (-2 * vect_parts_number);
		els_number = 0;
		for (curr_vect_part_number = 0;
		     curr_vect_part_number < 2 * vect_parts_number;
		     curr_vect_part_number += 2)
		  {
		    implicit_int_conversion (IVAL (op1, curr_vect_part_number),
					     -1);
		    if (ER_NODE_MODE (IVAL (op1, curr_vect_part_number))
			!= ER_NM_int)
		      eval_error (optype_decl, invops_decl, IR_pos (cpc),
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
		    el_size_type
		      = type_size_table [ER_NODE_MODE (IVAL (op1, 1))];
		    vect = create_pack_vector (els_number,
					       ER_NODE_MODE (IVAL (op1, 1)));
		  }
		else
		  vect = create_unpack_vector (els_number);
		op1 = IVAL (cvars, IR_vec_tab_el_num (IR_PTR (cpc)));
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
			       + el_size_type * curr_vect_element_number,
			       (char *) IVAL (op1, curr_vect_part_number + 1)
			       + val_displ_table
			         [ER_NODE_MODE
				  (IVAL (op1, curr_vect_part_number + 1))],
			       el_size_type);
			  else
			    memcpy (IVAL (ER_unpack_els (vect),
					  curr_vect_element_number),
				    IVAL (op1, curr_vect_part_number + 1),
				    sizeof (val_t));
			  repetition--;
			  curr_vect_element_number++;
			}
		    }
		assert
		  ((unsigned_int_t) curr_vect_element_number == els_number);
	      }
	    res = IVAL (cvars, IR_vec_tab_result_num (IR_PTR (cpc)));
	    ER_SET_MODE (res, ER_NM_vect);
	    ER_set_vect (res, vect);
	    INCREMENT_PC ();
	    break;
	  }
	case IR_NM_table:
	  {
	    ER_node_t tab, saved_ctop;
	    int_t tab_els_number, curr_tab_el_number;
	    ER_node_t entry;
	    IR_node_t elist;
	    
	    tab_els_number = IR_parts_number (IR_PTR (cpc));
	    saved_ctop = ctop;
	    ctop = IVAL (cvars, IR_vec_tab_el_num (IR_PTR (cpc)));
	    DECR_CTOP (-2 * tab_els_number);
	    tab = create_tab (tab_els_number);
	    op1 = IVAL (cvars, IR_vec_tab_el_num (IR_PTR (cpc)));
	    for (elist = IR_elist (IR_PTR (cpc)), curr_tab_el_number = 0;
		 curr_tab_el_number < 2 * tab_els_number;
		 curr_tab_el_number += 2)
	      {
		entry = find_tab_entry (tab, IVAL (op1, curr_tab_el_number),
					TRUE);
		op1 = IVAL (cvars, IR_vec_tab_el_num (IR_PTR (cpc)));
		if (ER_NODE_MODE (entry) != ER_NM_empty_entry
		    && ER_NODE_MODE (entry) != ER_NM_deleted_entry)
		  eval_error (keyvalue_decl, invkeys_decl, IR_pos (elist),
			      DERR_repeated_key);
		*(val_t *) entry = *(val_t *) IVAL (op1, curr_tab_el_number);
		make_immutable (entry);
		*((val_t *) entry + 1)
		  = *(val_t *) IVAL (op1, curr_tab_el_number + 1);
		assert (ER_IS_OF_TYPE (INDEXED_ENTRY_VAL (entry, 0),
				       ER_NM_val));
		elist = IR_next_elist (elist);
	      }
	    ctop = saved_ctop;
	    res = IVAL (cvars, IR_vec_tab_result_num (IR_PTR (cpc)));
	    ER_SET_MODE (res, ER_NM_tab);
	    ER_set_tab (res, tab);
	    INCREMENT_PC ();
	    break;
	  }
	case IR_NM_index:
	  res = IVAL (cvars, IR_designator_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_designator_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_component_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) != ER_NM_vect)
	    eval_error (indexop_decl, invindexes_decl, IR_pos (cpc),
			DERR_index_operation_for_non_array);
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op2) != ER_NM_int)
#endif
	    op2 = implicit_int_conversion (op2, tvar_num2);
	  load_vector_element_by_index (res, ER_vect (op1), op2);
	  INCREMENT_PC ();
	  break;
	case IR_NM_key_index:
	  res = IVAL (cvars, IR_designator_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_designator_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_component_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) != ER_NM_tab)
	    eval_error (keyop_decl, invkeys_decl, IR_pos (cpc),
			DERR_key_index_operation_for_non_table);
	  load_table_element_by_key (res, ER_tab (op1), op2);
	  INCREMENT_PC ();
	  break;
	case IR_NM_lvalue_index_and_val:
	  res = IVAL (cvars, IR_designator_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_designator_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_component_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) != ER_NM_vect)
	    eval_error (indexop_decl, invindexes_decl, IR_pos (cpc),
			DERR_index_operation_for_non_array);
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op2) != ER_NM_int)
#endif
	    op2 = implicit_int_conversion (op2, tvar_num2);
	  load_vector_element_by_index (res, ER_vect (op1), op2);
	  INCREMENT_PC ();
	  break;
	case IR_NM_lvalue_key_index_and_val:
	  res = IVAL (cvars, IR_designator_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_designator_op_num (IR_PTR (cpc)));
	  op2 = IVAL (cvars, IR_component_op_num (IR_PTR (cpc)));
	  if (ER_NODE_MODE (op1) != ER_NM_tab)
	    eval_error (keyop_decl, invkeys_decl, IR_pos (cpc),
			DERR_key_index_operation_for_non_table);
	  load_table_element_by_key (res, ER_tab (op1), op2);
	  INCREMENT_PC ();
	  break;
	case IR_NM_class_func_thread_call:
	  {
	    IR_node_t call = IR_PTR (cpc);

	    ctop = IVAL (cvars, IR_func_call_start_num (call));
	    DECR_CTOP (-IR_class_func_thread_call_parameters_number (call));
	    process_func_class_call
	      (IR_class_func_thread_call_parameters_number (call));
	    break;
	  }
	case IR_NM_mult:
	  EXECUTE_AR_OP (*, IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			 IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			 IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			 DERR_mult_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_div:
	  execute_div_op (IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_right_op_num (IR_PTR (cpc))));
	  INCREMENT_PC ();
	  break;
	case IR_NM_mod:
	  execute_mod_op (IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_right_op_num (IR_PTR (cpc))));
	  INCREMENT_PC ();
	  break;
	case IR_NM_plus:
	  EXECUTE_AR_OP (+, IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			 IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			 IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			 DERR_plus_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_minus:
	  EXECUTE_AR_OP (-, IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			 IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			 IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			 DERR_minus_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_concat:
	  execute_concat_op (IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			     IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			     IVAL (cvars, IR_right_op_num (IR_PTR (cpc))));
	  INCREMENT_PC ();
	  break;
	case IR_NM_lshift:
	  EXECUTE_INT_OP (<<, int_t, IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			  DERR_lshift_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_rshift:
	  EXECUTE_INT_OP (<<, unsigned_int_t,
			  IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			  DERR_rshift_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_ashift:
	  EXECUTE_INT_OP (>>, int_t, IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			  DERR_ashift_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_and:
	  EXECUTE_INT_OP (&, int_t, IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			  DERR_and_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_xor:
	  EXECUTE_INT_OP (^, int_t, IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			  DERR_xor_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_or:
	  EXECUTE_INT_OP (|, int_t, IVAL (cvars, IR_result_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_left_op_num (IR_PTR (cpc))),
			  IVAL (cvars, IR_right_op_num (IR_PTR (cpc))),
			  DERR_or_operands_types);
	  INCREMENT_PC ();
	  break;
	case IR_NM_mult_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_AR_OP (*, op1, op1, IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_mult_operands_types);
	  goto common_assign;
	case IR_NM_div_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  execute_div_op (op1, op1, IVAL (cvars, IR_expr_num (IR_PTR (cpc))));
	  goto common_assign;
	case IR_NM_rem_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  execute_mod_op (op1, op1, IVAL (cvars, IR_expr_num (IR_PTR (cpc))));
	  goto common_assign;
	case IR_NM_minus_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_AR_OP (-, op1, op1, IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_minus_operands_types);
	  goto common_assign;
	case IR_NM_concat_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  execute_concat_op (op1, op1,
			     IVAL (cvars, IR_expr_num (IR_PTR (cpc))));
	  goto common_assign;
	case IR_NM_lshift_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_INT_OP (<<, int_t, op1, op1,
			 IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_lshift_operands_types);
	  goto common_assign;
	case IR_NM_rshift_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_INT_OP (<<, unsigned_int_t, op1, op1,
			 IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_rshift_operands_types);
	  goto common_assign;
	case IR_NM_ashift_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_INT_OP (>>, int_t, op1, op1,
			 IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_ashift_operands_types);
	  goto common_assign;
	case IR_NM_and_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_INT_OP (&, int_t, op1, op1,
			 IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_and_operands_types);
	  goto common_assign;
	case IR_NM_xor_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_INT_OP (^, int_t, op1, op1,
			 IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_xor_operands_types);
	  goto common_assign;
	case IR_NM_or_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_INT_OP (|, int_t, op1, op1,
			 IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_or_operands_types);
	  goto common_assign;
	case IR_NM_plus_assign:
	  op1 = IVAL (cvars, IR_lvalue_val_num (IR_PTR (cpc)));
	  EXECUTE_AR_OP (+, op1, op1, IVAL (cvars, IR_expr_num (IR_PTR (cpc))),
			 DERR_plus_operands_types);
	  goto common_assign;
	case IR_NM_assign:
	case IR_NM_var_assign:
	case IR_NM_par_assign:
	  op1 = IVAL (cvars, IR_expr_num (IR_PTR (cpc)));
	common_assign:
	  store_designator_value (IVAL (cvars, IR_container_num (IR_PTR (cpc))),
				  IVAL (cvars, IR_index_num (IR_PTR (cpc))),
				  op1, IR_assignment_var (IR_PTR (cpc)));
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case IR_NM_par_assign_test:
	  op1 = IVAL (cvars, IR_par_num (IR_PTR (cpc)));
	  if (ER_IS_OF_TYPE (op1, ER_NM_nil))
	    {
	      INCREMENT_PC ();
	      break;
	    }
	    cpc = IR_skip_par_assign_path_pc (IR_PTR (cpc));
	    INTERRUPT_CHECK;
	  break;
	case IR_NM_proc_call:
	  ctop = IVAL (cvars, IR_proc_call_start_num (IR_PTR (cpc)));
	  DECR_CTOP (-IR_proc_call_pars_number (IR_PTR (cpc)));
	  process_func_class_call (IR_proc_call_pars_number (IR_PTR (cpc)));
	  INTERRUPT_CHECK;
	  break;
	case IR_NM_if_stmt:
	  op1 = IVAL (cvars, IR_if_cond_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      if (ER_i (op1) == 0)
		cpc = IR_else_part_pc (IR_PTR (cpc));
	      else
		INCREMENT_PC ();
	    }
	  else
#endif
	    {
	      op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	      if (ER_NODE_MODE (op1) != ER_NM_int
		  && ER_NODE_MODE (op1) != ER_NM_float)
		eval_error (optype_decl, invops_decl,
			    IR_pos (cpc), DERR_invalid_if_expr_type);
	      if (ER_NODE_MODE (op1) == ER_NM_int && ER_i (op1) == 0
		  || ER_NODE_MODE (op1) == ER_NM_float && ER_f (op1) == 0.0)
		cpc = IR_else_part_pc (IR_PTR (cpc));
	      else
		INCREMENT_PC ();
	    }
	  INTERRUPT_CHECK;
	  break;
	case IR_NM_for_stmt:
	  op1 = IVAL (cvars, IR_for_guard_num (IR_PTR (cpc)));
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (op1) == ER_NM_int)
	    {
	      if (ER_i (op1) != 0)
		cpc = IR_for_body_pc (IR_PTR (cpc));
	      else
		INCREMENT_PC ();
	    }
	  else
#endif
	    {
	      op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	      if (ER_NODE_MODE (op1) != ER_NM_int
		  && ER_NODE_MODE (op1) != ER_NM_float)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_invalid_for_guard_expr_type);
	      if (ER_NODE_MODE (op1) == ER_NM_int && ER_i (op1) != 0
		  || ER_NODE_MODE (op1) == ER_NM_float && ER_f (op1) != 0.0)
		cpc = IR_for_body_pc (IR_PTR (cpc));
	      else
		INCREMENT_PC ();
	    }
	  INTERRUPT_CHECK;
	  break;
	case IR_NM_foreach_start:
	  /* foreach_stmt always follows after this node (through the
	     lvalue and table).  */
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_int);
	  ER_set_i (ctop, 0);
	  INCREMENT_PC ();
	  break;
	case IR_NM_foreach_next_iteration:
	  /* foreach_stmt always follows after this node (through the
	     lvalue and table).  */
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_int);
	  ER_set_i (ctop, 1);
	  INCREMENT_PC ();
	  break;
	case IR_NM_foreach_stmt:
	  {
	    int next_iteration_flag;
	    ER_node_t tab;
	    
	    next_iteration_flag = ER_i (ctop);
	    TOP_DOWN;
	    op1 = IVAL (cvars, IR_foreach_tab_num (IR_PTR (cpc)));
	    if (ER_NODE_MODE (op1) != ER_NM_tab)
	      eval_error (keyop_decl, invkeys_decl,
			  IR_pos (cpc), DERR_in_table_operand_type);
	    tab = ER_tab (op1);
	    GO_THROUGH_REDIR (tab);
	    res = IVAL (cvars, IR_foreach_lvalue_val_num (IR_PTR (cpc)));
	    if (next_iteration_flag)
	      *(val_t *) res = *(val_t *) find_next_key (tab, res);
	    else
	      *(val_t *) res = *(val_t *) find_next_key (tab, NULL);
	    if (ER_NODE_MODE (res) != ER_NM_empty_entry
		&& ER_NODE_MODE (res) != ER_NM_deleted_entry)
	      {
		store_designator_value
		  (IVAL (cvars, IR_foreach_container_num (IR_PTR (cpc))),
		   IVAL (cvars, IR_foreach_index_num (IR_PTR (cpc))),
		   IVAL (cvars, IR_foreach_lvalue_val_num (IR_PTR (cpc))),
		   IR_foreach_designator (IR_PTR (cpc)));
		cpc = IR_foreach_body_pc (IR_PTR (cpc));
	      }
	    else
	      INCREMENT_PC ();
	    INTERRUPT_CHECK;
	    break;
	  }
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	  {
	    int i;

	    for (i = 0;
		 i < IR_number_of_surrounding_blocks (IR_PTR (cpc));
		 i++)
	      heap_pop ();
	    INCREMENT_PC ();
	    INTERRUPT_CHECK;
	    break;
	  }
	case IR_NM_block_finish:
	  if (IR_simple_block_finish_flag (IR_PTR (cpc)))
	    {
	      INCREMENT_PC ();
	      INTERRUPT_CHECK;
	      break;
	    }
	  /* Flow through */
	case IR_NM_return_without_result:
	  {
	    IR_node_t block_node, func_class;
	    
	    for (;;)
	      {
		block_node = ER_block_node (cstack);
		sync_flag = IR_block_saved_sync_flag (block_node);
		func_class = IR_func_class_ext (block_node);
		if (func_class != NULL)
		  {
		    if (IR_NODE_MODE (func_class) == IR_NM_func)
		      {
			if (IR_thread_flag (func_class))
			  {
			    delete_cprocess ();
			    break;
			  }
			ER_SET_MODE (IVAL (ER_ctop (ER_prev_stack (cstack)), 1),
				     ER_NM_nil);
		      }
		    else if (IR_NODE_MODE (func_class) == IR_NM_class)
		      {
			/* Self value - 0-th var of block. */
			*(val_t *) IVAL (ER_ctop (ER_prev_stack (cstack)), 1)
			  = *(val_t *) ER_stack_vars (cstack);
		      }
		    else
		      assert (FALSE);
		    heap_pop ();
		    if (cpc == NULL)
		      return;
		    break;
		  }
		else if (cstack == uppest_stack)
		  return;
		heap_pop ();
		if (node_mode == IR_NM_block_finish)
		  {
		    INCREMENT_PC ();
		    break;
		  }
		else
		  assert (cstack != NULL);
	      }
	    /* Do not put INTERRUPT here as the result is not on the
	       top and possible GC called directly (or indirectly
	       through thread switching) from INTERRUPT can make the
	       result wrong.  Another solution could be adding a node
	       to pop result as INTERRUPT should be the last statement
	       executed for the node.  */
	    break;
	  }
	case IR_NM_return_with_result:
	  {
	    IR_node_t block_node, func;
	    
	    res = IVAL (cvars, IR_return_expr_num (IR_PTR (cpc)));
	    for (;;)
	      {
		block_node = ER_block_node (cstack);
		sync_flag = IR_block_saved_sync_flag (block_node);
		func = IR_func_class_ext (block_node);
		if (func != NULL)
		  {
		    if (IR_NODE_MODE (func) == IR_NM_func)
		      {
			/* There is no GC since the return execution
			   start. */
			assert (!IR_thread_flag (func));
			*(val_t *) IVAL (ER_ctop (ER_prev_stack (cstack)), 1)
			  = *(val_t *) res;
		      }
		    else
		      assert (FALSE);
		    heap_pop ();
		    if (cpc == NULL)
		      return;
		    break;
		  }
		heap_pop ();
		assert (cstack != NULL);
	      }
	    /* See comment for return_without_result.  */
	    break;
	  }
	case IR_NM_wait_stmt:
	  if (sync_flag)
	    eval_error (syncwait_decl, errors_decl, IR_pos (cpc),
			DERR_wait_in_sync_stmt);
	  op1 = IVAL (cvars, IR_wait_guard_expr_num (IR_PTR (cpc)));
	  op1 = implicit_arithmetic_conversion (op1, tvar_num1);
	  if (ER_NODE_MODE (op1) != ER_NM_int
	      && ER_NODE_MODE (op1) != ER_NM_float)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			DERR_invalid_wait_guard_expr_type);
	  if (ER_NODE_MODE (op1) == ER_NM_int && ER_i (op1) == 0
	      || ER_NODE_MODE (op1) == ER_NM_float && ER_f (op1) == 0.0)
	    block_cprocess (IR_start_wait_guard_expr_pc (IR_PTR (cpc)), TRUE);
	  else
	    {
	      INCREMENT_PC ();
	      sync_flag = TRUE;
	    }
	  break;
	case IR_NM_wait_finish:
          sync_flag = FALSE;
          INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case IR_NM_block:
	  if (!IR_simple_block_flag (IR_PTR (cpc)))
	    {
	      heap_push (IR_PTR (cpc), cstack, 0);
	      IR_set_block_saved_sync_flag (IR_PTR (cpc), sync_flag);
	    }
	  else
            assert (FALSE);
	  INCREMENT_PC ();
	  INTERRUPT_CHECK;
	  break;
	case IR_NM_throw:
	  {
	    const char *message;
	    
	    op2 = IVAL (cvars, tvar_num1);
	    ER_SET_MODE (op2, ER_NM_class);
	    ER_set_class_no (op2, FUNC_CLASS_NO (except_decl));
	    ER_set_class_context (op2, uppest_stack);
	    op1 = IVAL (cvars, IR_throw_expr_num (IR_PTR (cpc)));
	    if (!ER_IS_OF_TYPE (op1, ER_NM_instance)
		|| !internal_inside_call (&message, op2, op1, FALSE))
	      eval_error (optype_decl, invops_decl, IR_pos (cpc),
			  DERR_no_exception_after_throw);
	    exception_position = IR_pos (cpc);
	    cpc = find_catch_pc (op1);
	  }
	  break;
	case IR_NM_exception:
	  {
	    ER_node_t exception;
	    const char *message;
	    
	    op1 = IVAL (cvars, IR_except_instance_num (IR_PTR (cpc)));
	    op2 = IVAL (cvars, IR_except_class_num (IR_PTR (cpc)));
	    assert (ER_IS_OF_TYPE (op1, ER_NM_instance));
	    exception = ER_instance (op1);
	    if (ER_IS_OF_TYPE (op2, ER_NM_class)
		&& internal_inside_call (&message, op2, op1, FALSE))
	      {
		INCREMENT_PC ();
		assert (IR_PTR (cpc) != NULL
			&& IR_IS_OF_TYPE (IR_PTR (cpc), IR_NM_block));
		TOP_DOWN; /* exception */
		heap_push (IR_PTR (cpc), cstack, 1);
		/* Zeroth val of catch block is always corresponding the
		   exception. */
		ER_SET_MODE (IVAL (ER_stack_vars (cstack), 0), ER_NM_instance);
		ER_set_instance (IVAL (ER_stack_vars (cstack), 0), exception);
		cpc = IR_next_pc (IR_PTR (cpc));
	      }
	    else
	      {
		/* Given catch does not correspond to the exception -- try
		   the next catch. */
		cpc = IR_next_catch_list_pc (cpc);
		if (cpc == NULL)
		  /* No more catches - go to covered try-blocks. */
		  cpc = find_catch_pc (op1);
	      }
	  }
	  break;
	case IR_NM_local_var_occurrence:
	  res = IVAL (cvars, IR_occurrence_result_num (IR_PTR (cpc)));
	  op1 = IVAL (cvars, IR_local_var_num (IR_PTR (cpc)));
	  *(val_t *) res = *(val_t *) op1;
	  INCREMENT_PC ();
	  break;
	case IR_NM_var_occurrence:
	  res = IVAL (cvars, IR_occurrence_result_num (IR_PTR (cpc)));
	  process_var_val (res, IR_decl (IR_PTR (cpc)));
	  INCREMENT_PC ();
	  break;
	case IR_NM_lvalue_var_occurrence:
	case IR_NM_lvalue_var_occurrence_and_val:
	  res = IVAL (cvars, IR_occurrence_result_num (IR_PTR (cpc)));
	  process_var_ref_and_val (res, IR_decl (IR_PTR (cpc)),
				   node_mode != IR_NM_lvalue_var_occurrence);
	  INCREMENT_PC ();
	  break;
	case IR_NM_external_var_occurrence:
	case IR_NM_lvalue_external_var_occurrence:
	case IR_NM_lvalue_external_var_occurrence_and_val:
	  res = IVAL (cvars, IR_occurrence_result_num (IR_PTR (cpc)));
	  process_external_var
	    (res, IR_decl (IR_PTR (cpc)),
	     node_mode != IR_NM_external_var_occurrence,
	     node_mode == IR_NM_lvalue_external_var_occurrence_and_val);
	  INCREMENT_PC ();
	  break;
	case IR_NM_external_func_occurrence:
	case IR_NM_func_occurrence:
	  {
	    IR_node_t decl = IR_decl (IR_PTR (cpc));
	    
	    res = IVAL (cvars, IR_occurrence_result_num (IR_PTR (cpc)));
	    if (node_mode == IR_NM_func_occurrence && IR_thread_flag (decl))
	      {
		ER_SET_MODE (res, ER_NM_thread);
		ER_set_thread_no (res, FUNC_CLASS_NO (decl));
		ER_set_thread_context (res, find_context_by_scope (IR_scope (decl)));
	      }
	    else
	      {
		ER_SET_MODE (res, ER_NM_func);
		ER_set_func_no (res, FUNC_CLASS_NO (decl));
		ER_set_func_context (res, find_context_by_scope (IR_scope (decl)));
	      }
	    INCREMENT_PC ();
	    break;
	  }
	case IR_NM_class_occurrence:
	  {
	    IR_node_t decl = IR_decl (IR_PTR (cpc));
	    
	    res = IVAL (cvars, IR_occurrence_result_num (IR_PTR (cpc)));
	    ER_SET_MODE (res, ER_NM_class);
	    ER_set_class_no (res, FUNC_CLASS_NO (decl));
	    ER_set_class_context (res, find_context_by_scope (IR_scope (decl)));
	    INCREMENT_PC ();
	    break;
	  }
#ifndef __GNUC__
	default:
	  assert (FALSE);
#endif
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
	  ER_set_vect (var, string);
	}
    }
  assert (IR_scope (argv_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack), IR_var_number_in_block (argv_decl));
  ER_SET_MODE (var, ER_NM_vect);
  ER_set_vect (var, vect);
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
	eval_error (invenv_decl, errors_decl,
		    no_position, DERR_environment_corrupted);
      program_environment [i][j] = '\0';
      string = create_string (program_environment [i]);
      program_environment [i][j] = '=';
      string2 = create_string (program_environment [i] + j + 1);
      ER_SET_MODE ((ER_node_t) &key, ER_NM_vect);
      ER_set_vect ((ER_node_t) &key, string);
      entry = find_tab_entry (tab, (ER_node_t) &key, TRUE);
      if (ER_NODE_MODE (entry) != ER_NM_empty_entry)
	eval_error (invenv_decl, errors_decl,
		    no_position, DERR_environment_corrupted);
      ER_SET_MODE (entry, ER_NM_vect);
      ER_set_vect (entry, string);
      make_immutable (entry);
      var = (ER_node_t) ((char *) entry + sizeof (val_t));
      ER_SET_MODE (var, ER_NM_vect);
      ER_set_vect (var, string2);
   }
  assert (IR_scope (env_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack), IR_var_number_in_block (env_decl));
  ER_SET_MODE (var, ER_NM_tab);
  ER_set_tab (var, tab);
  ER_set_immutable (tab, TRUE);
  /* Set version */
  assert (IR_scope (version_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack), IR_var_number_in_block (version_decl));
  ER_SET_MODE (var, ER_NM_float);
  ER_set_f (var, DINO_VERSION);
  /* Set main_thread, curr_thread */
  assert (IR_scope (main_thread_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack),
	      IR_var_number_in_block (main_thread_decl));
  ER_SET_MODE (var, ER_NM_process);
  ER_set_process (var, cprocess);
  assert (IR_scope (curr_thread_decl) == ER_block_node (cstack));
  var = IVAL (ER_stack_vars (cstack),
	      IR_var_number_in_block (curr_thread_decl));
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
eval_error (IR_node_t except_class, IR_node_t context_var,
	    position_t position, const char *format, ...)
{
  char message[MAX_EVAL_ERROR_MESSAGE_LENGTH + 1];
  va_list arguments;
  ER_node_t error_instance, string;

  assert (eval_long_jump_set_flag);
  va_start (arguments, format);
  vsprintf (message, format, arguments);
  va_end (arguments);
  assert (strlen (message) <= MAX_EVAL_ERROR_MESSAGE_LENGTH);
  exception_position = position;
  string = create_string (message);
  error_instance = (ER_node_t) heap_allocate (instance_size (except_class),
					      FALSE);
  ER_SET_MODE (error_instance, ER_NM_heap_instance);
  ER_set_instance_class (error_instance, except_class);
  ER_set_block_node (error_instance, IR_next_stmt (except_class));
  ER_set_immutable (error_instance, FALSE);
  ER_set_context
    (error_instance,
     ER_instance (IVAL (ER_stack_vars (uppest_stack),
			IR_var_number_in_block (context_var))));
  ER_set_context_number (error_instance, context_number);
  context_number++;
  /* Zeroth variable is message in class `error' */
  ER_SET_MODE (IVAL (ER_instance_vars (error_instance), 0), ER_NM_vect);
  ER_set_vect (IVAL (ER_instance_vars (error_instance), 0), string);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_instance);
  ER_set_instance (ctop, error_instance);
  cpc = find_catch_pc (ctop);
  longjmp (eval_longjump_buff, 1);
}

void
call_func_class (int_t pars_number)
{
  pc_t saved_cpc;
  pc_t saved_next_pc;
  int_t saved_process_number;

  saved_cpc = cpc;
  saved_next_pc = IR_next_pc (cpc);
  IR_set_next_pc (cpc, NULL);
  saved_process_number = ER_process_number (cprocess);
  process_func_class_call (pars_number);
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
  IR_set_next_pc (cpc, saved_next_pc);
}

static ticker_t all_time_ticker;

void
evaluate_program (pc_t start_pc)
{
  assert (start_pc != NULL
	  && IR_NODE_MODE (IR_PTR (start_pc)) == IR_NM_block);
  initiate_int_tables ();
  initiate_tables ();
  initiate_funcs ();
  sync_flag = FALSE;
  cpc = start_pc;
  /* The first statement is always block. */
  assert (IR_NODE_MODE (IR_PTR (cpc)) == IR_NM_block);
  heap_push (IR_PTR (cpc), NULL, 0);
  /* Initialized standard variables. */
  uppest_stack = cstack;
  initiate_processes (cpc);
  INCREMENT_PC ();
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
  while (setjmp (eval_longjump_buff))
    ;
  eval_long_jump_set_flag = TRUE;
  for (;;)
    {
      evaluate_code ();
      delete_cprocess ();
    }
  assert (FALSE);
}

#ifndef NO_PROFILE

/* Array of pointers to functions which will be reported in profile. */
static vlo_t profile_funcs;

/* The following recursive function collects functions which will be
   reported in profile. */
static void
collect_profile (IR_node_t first_level_stmt)
{
  IR_node_t stmt;

  for (stmt = first_level_stmt;
       stmt != NULL;
       stmt = IR_next_stmt (stmt))
    {
      switch (IR_NODE_MODE (stmt))
	{
	case IR_NM_if_stmt:
	  collect_profile (IR_if_part (stmt));
	  collect_profile (IR_else_part (stmt));
	  break;
	case IR_NM_for_stmt:
	  collect_profile (IR_for_initial_stmt (stmt));
	  collect_profile (IR_for_iterate_stmt (stmt));
	  collect_profile (IR_for_stmts (stmt));
	  break;
	case IR_NM_foreach_stmt:
	  collect_profile (IR_foreach_stmts (stmt));
	  break;
	case IR_NM_block:
	  {
	    IR_node_t curr_except;

	    collect_profile (IR_block_stmts (stmt));
	    for (curr_except = IR_exceptions (stmt);
		 curr_except != NULL;
		 curr_except = IR_next_exception (curr_except))
	      if (IR_catch_block (curr_except) != NULL)
		collect_profile (IR_catch_block (curr_except));
	    break;
	  }
	case IR_NM_func:
	case IR_NM_class:
	  if (IR_calls_number (stmt) != 0
	      && strcmp (IR_pos (stmt).file_name,
			 ENVIRONMENT_PSEUDO_FILE_NAME) != 0)
	    VLO_ADD_MEMORY (profile_funcs, &stmt, sizeof (stmt));
	  break;
	}
    }
}

/* The following comparison function is used to sort collected
   functions being reported in profile. */
static int
profile_compare_function (const void *el1, const void *el2)
{
  IR_node_t func1 = *(IR_node_t *) el1;
  IR_node_t func2 = *(IR_node_t *) el2;
  double time1, time2;

#if HAVE_SETITIMER
  return IR_interrupts_number (func2) - IR_interrupts_number (func1);
#else
  time1 = active_time (IR_exec_time (func1));
  time2 = active_time (IR_exec_time (func2));
  
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
print_profile (IR_node_t first_level_stmt)
{
  IR_node_t *func_ptr;
  double all_time = active_time (all_time_ticker);
  double func_time;
#if !HAVE_SETITIMER
  double gc_time = active_time (gc_ticker);
#endif
	
  ticker_off (&all_time_ticker);
  VLO_CREATE (profile_funcs, 0);
  collect_profile (first_level_stmt);
  qsort (VLO_BEGIN (profile_funcs),
	 VLO_LENGTH (profile_funcs) / sizeof (IR_node_t), sizeof (IR_node_t),
	 profile_compare_function);
  fprintf
    (stderr,
#if !HAVE_SETITIMER
     "\n** Calls ** In Time ** Name **************************************\n"
#else
     "\n** Calls *** Time **** Name **************************************\n"
#endif
     );
  for (func_ptr = (IR_node_t *) VLO_BEGIN (profile_funcs);
       (char *) func_ptr <= (char *) VLO_END (profile_funcs);
       func_ptr++)
    {
#if !HAVE_SETITIMER
      func_time = active_time (IR_exec_time (*func_ptr));
      if (gc_number != 0 && gc_time != 0.0 && gc_time > func_time)
	{
	  fprintf (stderr, "%8u %8.2f  --  * garbage collection *\n",
		   gc_number, gc_time);
	  gc_time = 0.0;
	}
#else
      func_time = (all_interrupts_number == 0
		   ? 0.0
		   : (all_time * IR_interrupts_number (*func_ptr)
		      / all_interrupts_number));
      if (gc_interrupts_number != 0
	  && gc_interrupts_number > IR_interrupts_number (*func_ptr))
	{
	  fprintf (stderr, "%8u %8.2f  --  * garbage collection *\n",
		   gc_number,
		   all_time * gc_interrupts_number / all_interrupts_number);
	  gc_interrupts_number = 0;
	}
	
#endif
      fprintf (stderr, "%8d %8.2f  --  %s: \"%s\": %d\n",
	       IR_calls_number (*func_ptr), func_time,
	       IR_ident_string (IR_unique_ident (IR_ident (*func_ptr))),
	       IR_pos (*func_ptr).file_name, IR_pos (*func_ptr).line_number);
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
