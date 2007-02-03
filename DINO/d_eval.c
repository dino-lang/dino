/*
   Copyright (C) 1997-2007 Vladimir Makarov.

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
#define USE_SWITCH


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
push_var_ref (IR_node_t _decl)
{
  IR_node_t scope = IR_scope (_decl);
  int var_number_in_block = IR_var_number_in_block (_decl);
  ER_node_t container;

  container = find_context_by_scope (scope);
  DECR_CTOP (-2);
  SET_TOP;
  if (ER_NODE_MODE (container) == ER_NM_heap_stack)
    {
      ER_SET_MODE (below_ctop, ER_NM_stack);
      ER_set_stack (below_ctop, container);
    }
  else
    {
      ER_SET_MODE (below_ctop, ER_NM_instance);
      ER_set_instance (below_ctop, container);
    }
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, var_number_in_block);
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
push_var_ref_and_val (IR_node_t _decl)
{
  IR_node_t scope = IR_scope (_decl);
  int var_number_in_block = IR_var_number_in_block (_decl);
  ER_node_t container;

  container = find_context_by_scope (scope);
  DECR_CTOP (-3);
  SET_TOP;
  if (ER_NODE_MODE (container) == ER_NM_heap_stack)
    {
      ER_SET_MODE (INDEXED_VAL (ER_CTOP (), -2), ER_NM_stack);
      ER_set_stack (INDEXED_VAL (ER_CTOP (), -2), container);
      ER_SET_MODE (below_ctop, ER_NM_int);
      ER_set_i (below_ctop, var_number_in_block);
      *(val_t *) ctop = *(val_t *) INDEXED_VAL (ER_stack_vars (container),
						var_number_in_block);
    }
  else
    {
      ER_SET_MODE (INDEXED_VAL (ER_CTOP (), -2), ER_NM_instance);
      ER_set_instance (INDEXED_VAL (ER_CTOP (), -2), container);
      ER_SET_MODE (below_ctop, ER_NM_int);
      ER_set_i (below_ctop, var_number_in_block);
      *(val_t *) ctop = *(val_t *) INDEXED_VAL (ER_instance_vars (container),
						var_number_in_block);
    }
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
push_var_val (IR_node_t _decl)
{
  int var_number_in_block = IR_var_number_in_block (_decl);
  IR_node_t scope = IR_scope (_decl);
  ER_node_t container;

  TOP_UP;
  container = find_context_by_scope (scope);
  if (ER_NODE_MODE (container) == ER_NM_heap_instance)
    *(val_t *) ctop = *(val_t *) INDEXED_VAL (ER_instance_vars (container),
					      var_number_in_block);
  else
    *(val_t *) ctop = *(val_t *) INDEXED_VAL (ER_stack_vars (container),
					      var_number_in_block);
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
push_external_var (IR_node_t decl, int var_ref_flag, int val_too_p)
{
  void *addr;
  
  addr = external_address (decl);
  TOP_UP;
  if (var_ref_flag)
    {
      ER_SET_MODE (ctop, ER_NM_external_var_ref);
      ER_set_external_var_ref (ctop, addr);
      TOP_UP;
      ER_SET_MODE (ctop, ER_NM_nil);
      if (val_too_p)
	{
	  TOP_UP;
	  *(val_t *) ctop = *(val_t *) addr;
	}
    }
  else
    *(val_t *) ctop = *(val_t *) addr;
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
execute_a_period_operation (int block_decl_ident_number, int lvalue_p,
			    int val_too_p)
{
  IR_node_t decl;
  IR_node_t block;
  ER_node_t container;
  int instance_p;

  if (instance_p = ER_NODE_MODE (ctop) == ER_NM_instance)
    container = ER_instance (ctop);
  else if (ER_NODE_MODE (ctop) == ER_NM_stack)
    container = ER_stack (ctop);
  else
    eval_error (accessop_decl, invaccesses_decl, IR_pos (cpc),
		DERR_value_is_not_class_instance_or_stack);
  decl = NULL;
  for (;;)
    {
      assert (container != NULL);
      block = ER_block_node (container);
      if (block_decl_ident_number >= 0)
	decl = LV_BLOCK_DECL (IR_block_number (block),
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
  else if (!IR_public_flag (decl))
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
	eval_error (accessop_decl, invaccesses_decl, IR_pos (cpc),
		    DERR_private_decl_access_from_outside_block,
		    IR_ident_string (IR_unique_ident (IR_ident (decl))));
    }
  switch (IR_NODE_MODE (decl))
    {
    case IR_NM_var:
      if (! lvalue_p)
	*(val_t *) ctop
	  = *(val_t *) INDEXED_VAL (instance_p
				    ? ER_instance_vars (container)
				    : ER_stack_vars (container),
				    IR_var_number_in_block (decl));
      else
	{
	  if (! instance_p)
	    {
	      ER_SET_MODE (ctop, ER_NM_stack);
	      ER_set_stack (ctop, container);
	      TOP_UP;
	      ER_SET_MODE (ctop, ER_NM_int);
	      ER_set_i (ctop, IR_var_number_in_block (decl));
	      if (val_too_p)
		{
		  TOP_UP;
		  *(val_t *) ctop
		    = *(val_t *) INDEXED_VAL (ER_stack_vars (container),
					      IR_var_number_in_block (decl));
		}
	    }
	  else
	    {
	      ER_SET_MODE (ctop, ER_NM_instance);
	      ER_set_instance (ctop, container);
	      TOP_UP;
	      ER_SET_MODE (ctop, ER_NM_int);
	      ER_set_i (ctop, IR_var_number_in_block (decl));
	      if (val_too_p)
		{
		  TOP_UP;
		  *(val_t *) ctop
		    = *(val_t *) INDEXED_VAL (ER_instance_vars (container),
					      IR_var_number_in_block (decl));
		}
	    }
	}
      break;
    case IR_NM_external_var:
      TOP_DOWN;
      push_external_var (decl, lvalue_p, val_too_p);
      break;
    case IR_NM_external_func:
    case IR_NM_func:
      if (lvalue_p)
	eval_error (accessop_decl, invaccesses_decl,
		    IR_pos (cpc), DERR_func_as_variable);
      ER_SET_MODE (ctop, ER_NM_func);
      ER_set_func_context (ctop, container);
      ER_set_func_no (ctop, FUNC_CLASS_NO (decl));
      break;
    case IR_NM_class:
      if (lvalue_p)
	eval_error (accessop_decl, invaccesses_decl,
		    IR_pos (cpc), DERR_class_as_variable);
      ER_SET_MODE (ctop, ER_NM_class);
      ER_set_class_context (ctop, container);
      ER_set_class_no (ctop, FUNC_CLASS_NO (decl));
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
  IR_node_t node;

  if (ER_NODE_MODE (_index) != ER_NM_int)
    eval_error (indextype_decl, invindexes_decl,
		IR_pos (cpc), DERR_index_is_not_int);
  index_value = ER_i (_index);
  if (index_value < 0
      || (unsigned_int_t) index_value >= ER_els_number (_vect))
    {
      if (IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_foreach_stmt))
	node = IR_foreach_designator (IR_POINTER (cpc));
      else if (IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_assign_stmt))
	node = IR_assignment_var (IR_POINTER (cpc));
      else
	node = IR_POINTER (cpc);
      if (index_value < 0)
	eval_error (indexvalue_decl, invindexes_decl,
		    IR_pos (node), DERR_index_is_negative_number);
      else
	eval_error (indexvalue_decl, invindexes_decl,
		    IR_pos (node), DERR_index_is_greater_than_array_bound);
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
  int _pack_flag;

  GO_THROUGH_REDIR (vect);
  _pack_flag = ER_NODE_MODE (vect) == ER_NM_heap_pack_vect;
  index_value = check_vector_index (vect, index);
  if (_pack_flag)
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
store_vector_element (void)
{
  size_t el_size_type;
  IR_node_t node;
  ER_node_t vect;
  int_t index_value;
  int pack_flag;

  vect = ER_vect (INDEXED_VAL (ER_CTOP (), -2));
  GO_THROUGH_REDIR (vect);
  pack_flag = ER_NODE_MODE (vect) == ER_NM_heap_pack_vect;
  if (ER_immutable (vect))
    {
      if (IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_foreach_stmt))
	node = IR_foreach_designator (IR_POINTER (cpc));
      else if (IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_assign_stmt))
	node = IR_assignment_var (IR_POINTER (cpc));
      else
	node = IR_POINTER (cpc);
      eval_error (immutable_decl, invaccesses_decl, IR_pos (node),
		  DERR_immutable_vector_modification);
    }
  index_value = check_vector_index (vect, below_ctop);
  if (pack_flag && ER_pack_vect_el_type (vect) != ER_NODE_MODE (ctop))
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
	  ((hide_t *) ER_pack_els (vect)) [index_value] = ER_hide (ctop);
	  break;
	case ER_NM_char:
	  ((char_t *) ER_pack_els (vect)) [index_value] = ER_ch (ctop);
	  break;
	case ER_NM_int:
	  ((int_t *) ER_pack_els (vect)) [index_value] = ER_i (ctop);
	  break;
	case ER_NM_float:
	  ((floating_t *) ER_pack_els (vect)) [index_value] = ER_f (ctop);
	  break;
	case ER_NM_type:
	  ((IR_node_mode_t *) ER_pack_els (vect)) [index_value]
	    = ER_type (ctop);
	  break;
	case ER_NM_vect:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value] = ER_vect (ctop);
	  break;
	case ER_NM_tab:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value] = ER_tab (ctop);
	  break;
	case ER_NM_instance:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value]
	    = ER_instance (ctop);
	  break;
	case ER_NM_process:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value] = ER_process (ctop);
	  break;
	case ER_NM_stack:
	  ((ER_node_t *) ER_pack_els (vect)) [index_value] = ER_stack (ctop);
	  break;
	case ER_NM_func:
	case ER_NM_class:
	  el_size_type = type_size_table [el_type];
	  memcpy (ER_pack_els (vect) + index_value * el_size_type,
		  (char *) ctop + val_displ_table [ER_NODE_MODE (ctop)],
		  el_size_type);
	  break;
	default:
	  assert (FALSE);
	}
    }
  else
      *(val_t *)INDEXED_VAL (ER_unpack_els (vect), index_value)
        = *(val_t *)ctop;
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
store_table_element (void)
{
  IR_node_t node;
  ER_node_t tab;
  ER_node_t entry;

  tab = ER_tab (INDEXED_VAL (ER_CTOP (), -2));
  GO_THROUGH_REDIR (tab);
  if (ER_immutable (tab))
    {
      if (IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_foreach_stmt))
	node = IR_foreach_designator (IR_POINTER (cpc));
      else if (IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_assign_stmt))
	node = IR_assignment_var (IR_POINTER (cpc));
      else
	node = IR_POINTER (cpc);
      eval_error (immutable_decl, invaccesses_decl, IR_pos (node),
		  DERR_immutable_table_modification);
    }
  entry = find_tab_entry (tab, below_ctop, TRUE);
  *(val_t *) entry = *(val_t *) below_ctop;
  make_immutable (entry);
  *(val_t *) INDEXED_ENTRY_VAL (entry, 0) = *(val_t *) ctop;
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
	= *(val_t *) INDEXED_VAL (ER_stack_vars (ER_stack (designator)),
				  ER_i (INDEXED_VAL (designator, 1)));
    }
  else if (ER_NODE_MODE (designator) == ER_NM_vect)
    load_vector_element_by_index (to, ER_vect (designator),
				  INDEXED_VAL (designator, 1));
  else if (ER_NODE_MODE (designator) == ER_NM_instance)
    *(val_t *) to
      = *(val_t *) INDEXED_VAL (ER_instance_vars (ER_instance (designator)),
				ER_i (INDEXED_VAL (designator, 1)));
  else if (ER_NODE_MODE (designator) == ER_NM_tab)
    load_table_element_by_key (to, ER_tab (designator),
			       INDEXED_VAL (designator, 1));
  else if (ER_NODE_MODE (designator) == ER_NM_external_var_ref)
    *(val_t *) to = *(val_t *) ER_external_var_ref (designator);
  else
    assert (FALSE);
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
load_designator_value (void)
{
  if (ER_NODE_MODE (below_ctop) == ER_NM_stack)
    *(val_t *) below_ctop
      = *(val_t *) INDEXED_VAL (ER_stack_vars (ER_stack (below_ctop)),
				  ER_i (ctop));
  else if (ER_NODE_MODE (below_ctop) == ER_NM_vect)
    load_vector_element_by_index (below_ctop, ER_vect (below_ctop), ctop);
  else if (ER_NODE_MODE (below_ctop) == ER_NM_instance)
    *(val_t *) below_ctop
      = *(val_t *) INDEXED_VAL (ER_instance_vars (ER_instance (below_ctop)),
				ER_i (ctop));
  else if (ER_NODE_MODE (below_ctop) == ER_NM_tab)
    load_table_element_by_key (below_ctop, ER_tab (below_ctop), ctop);
  else if (ER_NODE_MODE (below_ctop) == ER_NM_external_var_ref)
    *(val_t *) below_ctop = *(val_t *) ER_external_var_ref (below_ctop);
  else
    assert (FALSE);
  TOP_DOWN;
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
store_designator_value (void)
{
  IR_node_t node;
  ER_node_t container;

  container = INDEXED_VAL (ER_CTOP (), -2);
  if (ER_NODE_MODE (container) == ER_NM_stack)
    {
	*(val_t *) INDEXED_VAL (ER_stack_vars (ER_stack (container)),
				ER_i (below_ctop))
          = *(val_t *) ctop;
    }
  else if (ER_NODE_MODE (container) == ER_NM_vect)
    store_vector_element ();
  else if (ER_NODE_MODE (container) == ER_NM_instance)
    {
      if (ER_immutable (ER_instance (container)))
	{
	  if (IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_foreach_stmt))
	    node = IR_foreach_designator (IR_POINTER (cpc));
	  else if (IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_assign_stmt))
		node = IR_assignment_var (IR_POINTER (cpc));
	  else
	    node = IR_POINTER (cpc);
	  eval_error (immutable_decl, invaccesses_decl, IR_pos (node),
		      DERR_immutable_instance_modification);
	}
      *(val_t *) INDEXED_VAL (ER_instance_vars (ER_instance (container)),
			      ER_i (below_ctop))
	= *(val_t *) ctop;
    }
  else if (ER_NODE_MODE (container) == ER_NM_tab)
    store_table_element ();
  else if (ER_NODE_MODE (container) == ER_NM_external_var_ref)
    *(val_t *) ER_external_var_ref (container) = *(val_t *) ctop;
  else
    assert (FALSE);
}

/* The following variable value is source of the exception (error)
   occurrence. */
static position_t exception_position;

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static pc_t
find_catch_pc (void)
{
  IR_node_t block;
  IR_node_t func_class;
  ER_node_t except;
  IR_node_t curr_scope;
  ER_node_t message;

  assert (ER_NODE_MODE (ctop) == ER_NM_instance);
  no_gc_flag = FALSE;
  except = ER_instance (ctop);
  temp_ref = NULL;
  EMPTY_TEMP_REF ();
  for (; cstack != uppest_stack;)
    {
      block = ER_block_node (cstack);
      sync_flag = IR_block_saved_sync_flag (block);
      func_class = IR_func_class_ext (block);
      if (func_class != NULL && IR_NODE_MODE (func_class) == IR_NM_func
	  && IR_thread_flag (func_class)
	  && !delete_cprocess_during_exception ())
	break;
      heap_pop ();
      cpc = IR_catch_list_pc (block);
      if (cpc != NULL)
	{
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_instance);
	  ER_set_instance (ctop, except);
	  /* For catch deadlock.  The first catch statement is always
             block. */
	  ER_set_process_status (cprocess, PS_READY);
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
  message = INDEXED_VAL (ER_instance_vars (except), 0);
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
#define EXECUTE_AR_OP(OP, MSG)                                         \
  do                                                                   \
    {                                                                  \
      if (ER_NODE_MODE (ctop) == ER_NM_int                             \
	  && ER_NODE_MODE (below_ctop) == ER_NM_int)                   \
	ER_set_i (below_ctop, ER_i (below_ctop) OP ER_i (ctop));       \
      else if (ER_NODE_MODE (ctop) == ER_NM_float                      \
	       && ER_NODE_MODE (below_ctop) == ER_NM_float)            \
	ER_set_f (below_ctop, ER_f (below_ctop) OP ER_f (ctop));       \
      else                                                             \
	{                                                              \
	  implicit_conversion_for_binary_arithmetic_op ();             \
	  if (ER_NODE_MODE (ctop) != ER_NM_int                         \
	      && ER_NODE_MODE (ctop) != ER_NM_float                    \
	      || ER_NODE_MODE (below_ctop) != ER_NM_int                \
	      && ER_NODE_MODE (below_ctop) != ER_NM_float)             \
	    eval_error (optype_decl, invops_decl,                      \
			IR_pos (cpc), MSG);		               \
	  if (ER_NODE_MODE (below_ctop) == ER_NM_int)                  \
	    ER_set_i (below_ctop, ER_i (below_ctop) OP ER_i (ctop));   \
	  else                                                         \
	    ER_set_f (below_ctop, ER_f (below_ctop) OP ER_f (ctop));   \
	}                                                              \
      TOP_DOWN;                                                        \
    }                                                                  \
  while (0)

/* The following macro is code for execution integer operation OP with
   CAST and reporting MSG if there are errors. */
#define EXECUTE_INT_OP(OP, CAST, MSG)                                   \
  do                                                                    \
    {                                                                   \
      if (ER_NODE_MODE (ctop) != ER_NM_int)                             \
	{                                                               \
	  implicit_conversion_for_binary_int_op ();                     \
	  if (ER_NODE_MODE (ctop) != ER_NM_int                          \
	      || ER_NODE_MODE (below_ctop) != ER_NM_int)                \
	    eval_error (optype_decl, invops_decl,                       \
			IR_pos (cpc), MSG);                     	\
	}                                                               \
      ER_set_i (below_ctop, (CAST) ER_i (below_ctop) OP ER_i (ctop));   \
      TOP_DOWN;                                                         \
    }                                                                   \
  while (0)

/* The following function implements division. */
#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
execute_div_op (void)
{
  if (ER_NODE_MODE (ctop) == ER_NM_int
      && ER_NODE_MODE (below_ctop) == ER_NM_int)
    ER_set_i (below_ctop, ER_i (below_ctop) / ER_i (ctop));
  else if (ER_NODE_MODE (ctop) == ER_NM_float
	   && ER_NODE_MODE (below_ctop) == ER_NM_float)
    ER_set_f (below_ctop, ER_f (below_ctop) / ER_f (ctop));
  else
    {
      implicit_conversion_for_binary_arithmetic_op ();
      if (ER_NODE_MODE (ctop) != ER_NM_int
	  && ER_NODE_MODE (ctop) != ER_NM_float
	  || ER_NODE_MODE (below_ctop) != ER_NM_int
	  && ER_NODE_MODE (below_ctop) != ER_NM_float)
	eval_error (optype_decl, invops_decl,
		    IR_pos (cpc), DERR_div_operands_types);
      if (ER_NODE_MODE (below_ctop) == ER_NM_int)
	ER_set_i (below_ctop, ER_i (below_ctop) / ER_i (ctop));
      else
	ER_set_f (below_ctop, ER_f (below_ctop) / ER_f (ctop));
    }
  TOP_DOWN;
}

/* The following function implements modulo op. */
#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
execute_mod_op (void)
{
  if (ER_NODE_MODE (ctop) == ER_NM_int
      && ER_NODE_MODE (below_ctop) == ER_NM_int)
    ER_set_i (below_ctop, ER_i (below_ctop) % ER_i (ctop));
  else if (ER_NODE_MODE (ctop) == ER_NM_float
	   && ER_NODE_MODE (below_ctop) == ER_NM_float)
    ER_set_f (below_ctop, fmod (ER_f (below_ctop), ER_f (ctop)));
  else
    {
      implicit_conversion_for_binary_arithmetic_op ();
      if (ER_NODE_MODE (ctop) != ER_NM_int
	  && ER_NODE_MODE (ctop) != ER_NM_float
	  || ER_NODE_MODE (below_ctop) != ER_NM_int
	  && ER_NODE_MODE (below_ctop) != ER_NM_float)
	eval_error (optype_decl, invops_decl,
		    IR_pos (cpc), DERR_mod_operands_types);
      if (ER_NODE_MODE (below_ctop) == ER_NM_int)
	ER_set_i (below_ctop, ER_i (below_ctop) % ER_i (ctop));
      else
	ER_set_f (below_ctop, fmod (ER_f (below_ctop), ER_f (ctop)));
    }
  TOP_DOWN;
}

/* The following function implements array concatenation. */
#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
execute_concat_op (void)
{
  size_t els_number;
  ER_node_t vect;
  
  to_vect_string_conversion (ctop, NULL);
  to_vect_string_conversion (below_ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (below_ctop) != ER_NM_vect)
    eval_error (optype_decl, invops_decl,
		IR_pos (cpc), DERR_concat_operands_types);
  if (ER_NODE_MODE (ER_vect (ctop))
      != ER_NODE_MODE (ER_vect (below_ctop))
      || ER_NODE_MODE (ER_vect (ctop)) == ER_NM_heap_pack_vect
      && (ER_pack_vect_el_type (ER_vect (ctop))
	  != ER_pack_vect_el_type (ER_vect (below_ctop))))
    {
      if (ER_NODE_MODE (ER_vect (ctop)) == ER_NM_heap_pack_vect)
	ER_set_vect (ctop, unpack_vector (ER_vect (ctop)));
      if (ER_NODE_MODE (ER_vect (below_ctop)) == ER_NM_heap_pack_vect)
	ER_set_vect (below_ctop, unpack_vector (ER_vect (below_ctop)));
    }
  if (ER_NODE_MODE (ER_vect (ctop)) == ER_NM_heap_pack_vect)
    {
      ER_node_mode_t result_el_type;
      size_t el_size;
      ER_node_t result;
      ER_node_t pack_vect1 = ER_vect (below_ctop);
      ER_node_t pack_vect2 = ER_vect (ctop);
      
      if (ER_pack_vect_el_type (pack_vect2) == ER_NM_nil)
	result_el_type = ER_pack_vect_el_type (pack_vect1);
      else if (ER_pack_vect_el_type (pack_vect1) == ER_NM_nil)
	result_el_type = ER_pack_vect_el_type (pack_vect2);
      else if (ER_pack_vect_el_type (pack_vect2)
	       == ER_pack_vect_el_type (pack_vect1))
	result_el_type = ER_pack_vect_el_type (pack_vect2);
      else
	assert (FALSE);
      el_size = type_size_table [result_el_type];
      els_number = (ER_els_number (pack_vect2)
		    + ER_els_number (pack_vect1));
      /* Do not change size & packing. */
      PUSH_TEMP_REF (ER_vect (ctop));
      PUSH_TEMP_REF (ER_vect (below_ctop));
      result = create_pack_vector (els_number, result_el_type);
      POP_TEMP_REF (2);
      /* Restore locals after allocation (remember GC). */
      pack_vect1 = ER_vect (below_ctop);
      pack_vect2 = ER_vect (ctop);
      if (ER_els_number (pack_vect1) != 0)
	memcpy (ER_pack_els (result), ER_pack_els (pack_vect1),
		ER_els_number (pack_vect1) * el_size);
      if (ER_els_number (pack_vect2) != 0)
	memcpy (ER_pack_els (result)
		+ ER_els_number (pack_vect1) * el_size,
		ER_pack_els (pack_vect2),
		ER_els_number (pack_vect2) * el_size);
      if (result_el_type == ER_NM_char)
	ER_pack_els (result) [ER_els_number (pack_vect1)
			     + ER_els_number (pack_vect2)] = '\0';
      ER_set_vect (below_ctop, result);
    }
  else
    {
      ER_node_t result;
      ER_node_t unpack_vect1 = ER_vect (below_ctop);
      ER_node_t unpack_vect2 = ER_vect (ctop);
      
      els_number = (ER_els_number (unpack_vect2)
		    + ER_els_number (unpack_vect1));
      /* Do not change size & packing. */
      PUSH_TEMP_REF (ER_vect (ctop));
      PUSH_TEMP_REF (ER_vect (below_ctop));
      result = create_unpack_vector (els_number);
      POP_TEMP_REF (2);
      /* Restore after allocation (remeber about GC). */
      unpack_vect1 = ER_vect (below_ctop);
      unpack_vect2 = ER_vect (ctop);
      if (ER_els_number (unpack_vect1) != 0)
	memcpy (ER_unpack_els (result), ER_unpack_els (unpack_vect1),
		ER_els_number (unpack_vect1) * sizeof (val_t));
      if (ER_els_number (unpack_vect2) != 0)
	memcpy ((char *) ER_unpack_els (result)
		+ ER_els_number (unpack_vect1) * sizeof (val_t),
		ER_unpack_els (unpack_vect2),
		ER_els_number (unpack_vect2) * sizeof (val_t));
      ER_set_vect (below_ctop, result);
    }
  TOP_DOWN;
}


static void
evaluate_code (void)
{
  int res;
  IR_node_mode_t node_mode;

#if !defined (USE_SWITCH) && defined (__GNUC__)
  static void *table [IR_NM__error];
  if (table [IR_NM_char] == NULL)
    {
      table [IR_NM_char] = &&l_IR_NM_char;
      table [IR_NM_char] = &&l_IR_NM_char;
      table [IR_NM_int] = &&l_IR_NM_int;
      table [IR_NM_float] = &&l_IR_NM_float;
      table [IR_NM_hide_type] = &&l_IR_NM_hide_type;
      table [IR_NM_hideblock_type] = &&l_IR_NM_hideblock_type;
      table [IR_NM_char_type] = &&l_IR_NM_char_type;
      table [IR_NM_int_type] = &&l_IR_NM_int_type;
      table [IR_NM_float_type] = &&l_IR_NM_float_type;
      table [IR_NM_vector_type] = &&l_IR_NM_vector_type;
      table [IR_NM_table_type] = &&l_IR_NM_table_type;
      table [IR_NM_func_type] = &&l_IR_NM_func_type;
      table [IR_NM_thread_type] = &&l_IR_NM_thread_type;
      table [IR_NM_class_type] = &&l_IR_NM_class_type;
      table [IR_NM_stack_type] = &&l_IR_NM_stack_type;
      table [IR_NM_process_type] = &&l_IR_NM_process_type;
      table [IR_NM_instance_type] = &&l_IR_NM_instance_type;
      table [IR_NM_type_type] = &&l_IR_NM_type_type;
      table [IR_NM_string] = &&l_IR_NM_string;
      table [IR_NM_nil] = &&l_IR_NM_nil;
      table [IR_NM_period] = &&l_IR_NM_period;
      table [IR_NM_lvalue_period] = &&l_IR_NM_lvalue_period;
      table [IR_NM_lvalue_period_and_val] = &&l_IR_NM_lvalue_period_and_val;
      table [IR_NM_no_testing_period] = &&l_IR_NM_no_testing_period;
      table [IR_NM_lvalue_no_testing_period]
	= &&l_IR_NM_lvalue_no_testing_period;
      table [IR_NM_lvalue_no_testing_period_and_val]
	= &&l_IR_NM_lvalue_no_testing_period_and_val;
      table [IR_NM_logical_or] = &&l_IR_NM_logical_or;
      table [IR_NM_logical_and] = &&l_IR_NM_logical_and;
      table [IR_NM_logical_or_end] = &&l_IR_NM_logical_or_end;
      table [IR_NM_logical_and_end] = &&l_IR_NM_logical_and_end;
      table [IR_NM_in] = &&l_IR_NM_in;
      table [IR_NM_not] = &&l_IR_NM_not;
      table [IR_NM_bitwise_not] = &&l_IR_NM_bitwise_not;
      table [IR_NM_eq] = &&l_IR_NM_eq;
      table [IR_NM_ne] = &&l_IR_NM_ne;
      table [IR_NM_identity] = &&l_IR_NM_identity;
      table [IR_NM_unidentity] = &&l_IR_NM_unidentity;
      table [IR_NM_lt] = &&l_IR_NM_lt;
      table [IR_NM_ge] = &&l_IR_NM_ge;
      table [IR_NM_gt] = &&l_IR_NM_gt;
      table [IR_NM_le] = &&l_IR_NM_le;
      table [IR_NM_unary_plus] = &&l_IR_NM_unary_plus;
      table [IR_NM_unary_minus] = &&l_IR_NM_unary_minus;
      table [IR_NM_length] = &&l_IR_NM_length;
      table [IR_NM_const] = &&l_IR_NM_const;
      table [IR_NM_new] = &&l_IR_NM_new;
      table [IR_NM_typeof] = &&l_IR_NM_typeof;
      table [IR_NM_charof] = &&l_IR_NM_charof;
      table [IR_NM_intof] = &&l_IR_NM_intof;
      table [IR_NM_floatof] = &&l_IR_NM_floatof;
      table [IR_NM_format_vectorof] = &&l_IR_NM_format_vectorof;
      table [IR_NM_vectorof] = &&l_IR_NM_vectorof;
      table [IR_NM_tableof] = &&l_IR_NM_tableof;
      table [IR_NM_funcof] = &&l_IR_NM_funcof;
      table [IR_NM_threadof] = &&l_IR_NM_threadof;
      table [IR_NM_classof] = &&l_IR_NM_classof;
      table [IR_NM_cond] = &&l_IR_NM_cond;
      table [IR_NM_cond_end] = &&l_IR_NM_cond_end;
      table [IR_NM_par_assign_end] = &&l_IR_NM_par_assign_end;
      table [IR_NM_vector] = &&l_IR_NM_vector;
      table [IR_NM_table] = &&l_IR_NM_table;
      table [IR_NM_index] = &&l_IR_NM_index;
      table [IR_NM_lvalue_index] = &&l_IR_NM_lvalue_index;
      table [IR_NM_lvalue_index_and_val] = &&l_IR_NM_lvalue_index_and_val;
      table [IR_NM_key_index] = &&l_IR_NM_key_index;
      table [IR_NM_lvalue_key_index] = &&l_IR_NM_lvalue_key_index;
      table [IR_NM_lvalue_key_index_and_val]
	= &&l_IR_NM_lvalue_key_index_and_val;
      table [IR_NM_class_func_thread_call] = &&l_IR_NM_class_func_thread_call;
      table [IR_NM_mult] = &&l_IR_NM_mult;
      table [IR_NM_div] = &&l_IR_NM_div;
      table [IR_NM_mod] = &&l_IR_NM_mod;
      table [IR_NM_plus] = &&l_IR_NM_plus;
      table [IR_NM_minus] = &&l_IR_NM_minus;
      table [IR_NM_concat] = &&l_IR_NM_concat;
      table [IR_NM_lshift] = &&l_IR_NM_lshift;
      table [IR_NM_rshift] = &&l_IR_NM_rshift;
      table [IR_NM_ashift] = &&l_IR_NM_ashift;
      table [IR_NM_and] = &&l_IR_NM_and;
      table [IR_NM_xor] = &&l_IR_NM_xor;
      table [IR_NM_or] = &&l_IR_NM_or;
      table [IR_NM_mult_assign] = &&l_IR_NM_mult_assign;
      table [IR_NM_div_assign] = &&l_IR_NM_div_assign;
      table [IR_NM_rem_assign] = &&l_IR_NM_rem_assign;
      table [IR_NM_plus_assign] = &&l_IR_NM_plus_assign;
      table [IR_NM_minus_assign] = &&l_IR_NM_minus_assign;
      table [IR_NM_concat_assign] = &&l_IR_NM_concat_assign;
      table [IR_NM_lshift_assign] = &&l_IR_NM_lshift_assign;
      table [IR_NM_rshift_assign] = &&l_IR_NM_rshift_assign;
      table [IR_NM_ashift_assign] = &&l_IR_NM_ashift_assign;
      table [IR_NM_and_assign] = &&l_IR_NM_and_assign;
      table [IR_NM_xor_assign] = &&l_IR_NM_xor_assign;
      table [IR_NM_or_assign] = &&l_IR_NM_or_assign;
      table [IR_NM_assign] = &&l_IR_NM_assign;
      table [IR_NM_swap] = &&l_IR_NM_swap;
      table [IR_NM_var_assign] = &&l_IR_NM_var_assign;
      table [IR_NM_par_assign] = &&l_IR_NM_par_assign;
      table [IR_NM_par_assign_test] = &&l_IR_NM_par_assign_test;
      table [IR_NM_procedure_call] = &&l_IR_NM_procedure_call;
      table [IR_NM_if_stmt] = &&l_IR_NM_if_stmt;
      table [IR_NM_for_stmt] = &&l_IR_NM_for_stmt;
      table [IR_NM_foreach_start] = &&l_IR_NM_foreach_start;
      table [IR_NM_foreach_next_iteration] = &&l_IR_NM_foreach_next_iteration;
      table [IR_NM_foreach_stmt] = &&l_IR_NM_foreach_stmt;
      table [IR_NM_break_stmt] = &&l_IR_NM_break_stmt;
      table [IR_NM_continue_stmt] = &&l_IR_NM_continue_stmt;
      table [IR_NM_block_finish] = &&l_IR_NM_block_finish;
      table [IR_NM_return_without_result] = &&l_IR_NM_return_without_result;
      table [IR_NM_return_with_result] = &&l_IR_NM_return_with_result;
      table [IR_NM_wait_stmt] = &&l_IR_NM_wait_stmt;
      table [IR_NM_block] = &&l_IR_NM_block;
      table [IR_NM_throw] = &&l_IR_NM_throw;
      table [IR_NM_exception] = &&l_IR_NM_exception;
      table [IR_NM_var_occurrence] = &&l_IR_NM_var_occurrence;
      table [IR_NM_lvalue_var_occurrence] = &&l_IR_NM_lvalue_var_occurrence;
      table [IR_NM_lvalue_var_occurrence_and_val]
	= &&l_IR_NM_lvalue_var_occurrence_and_val;
      table [IR_NM_local_lvalue_var_occurrence]
	= &&l_IR_NM_local_lvalue_var_occurrence;
      table [IR_NM_local_lvalue_var_occurrence_and_val]
	= &&l_IR_NM_local_lvalue_var_occurrence_and_val;
      table [IR_NM_external_var_occurrence]
	= &&l_IR_NM_external_var_occurrence;
      table [IR_NM_lvalue_external_var_occurrence]
	= &&l_IR_NM_lvalue_external_var_occurrence;
      table [IR_NM_lvalue_external_var_occurrence_and_val]
	= &&l_IR_NM_lvalue_external_var_occurrence_and_val;
      table [IR_NM_external_func_occurrence]
	= &&l_IR_NM_external_func_occurrence;
      table [IR_NM_func_occurrence] = &&l_IR_NM_func_occurrence;
      table [IR_NM_class_occurrence] = &&l_IR_NM_class_occurrence;
      table [IR_NM_if_finish] = &&l_IR_NM_if_finish;
      table [IR_NM_for_finish] = &&l_IR_NM_for_finish;
      table [IR_NM_catches_finish] = &&l_IR_NM_catches_finish;
      table [IR_NM_pop_func_call] = &&l_IR_NM_pop_func_call;
    }
#define CASE(value) l_ ## value
#define BREAK       continue
#else
#define CASE(value) case value
#define BREAK       break
#endif
  for (;;)
    {
      node_mode = IR_NODE_MODE (IR_POINTER (cpc));
#if !defined (USE_SWITCH) && defined (__GNUC__)
      goto *table [node_mode];
#else
      switch (node_mode = IR_NODE_MODE (IR_POINTER (cpc)))
#endif
	{
	CASE (IR_NM_char):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_char);
	  ER_set_ch (ctop, IR_ch_val (IR_POINTER (cpc)));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_int):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_int);
	  ER_set_i (ctop, IR_i_val (IR_POINTER (cpc)));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_float):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_float);
	  ER_set_f (ctop, IR_f_val (IR_POINTER (cpc)));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_hide_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_hide);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_hideblock_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_hideblock);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_char_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_char);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_int_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_int);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_float_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_float);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_vector_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_vector);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_table_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_table);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_func_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_func);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_thread_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_thread);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_class_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_class);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_stack_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_stack);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_process_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_process);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_instance_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_instance);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_type_type):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_type);
	  ER_set_type (ctop, ER_T_type);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_string):
	  {
	    ER_node_t vect;
	    
	    vect = create_string (IR_str_val (IR_POINTER (cpc)));
	    TOP_UP;
	    ER_SET_MODE (ctop, ER_NM_vect);
	    ER_set_vect (ctop, vect);
	    INCREMENT_PC ();
	  }
	  BREAK;
	CASE (IR_NM_nil):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_nil);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_period):
	CASE (IR_NM_no_testing_period):
	  execute_a_period_operation
  	    (IR_right_block_decl_ident_number (IR_POINTER (cpc)),
	     FALSE, FALSE);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_lvalue_period):
	CASE (IR_NM_lvalue_no_testing_period):
	  execute_a_period_operation
	    (IR_right_block_decl_ident_number (IR_POINTER (cpc)), TRUE, FALSE);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_lvalue_period_and_val):
	CASE (IR_NM_lvalue_no_testing_period_and_val):
	  execute_a_period_operation
	    (IR_right_block_decl_ident_number (IR_POINTER (cpc)), TRUE, TRUE);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_logical_or):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    ER_set_i (ctop, ER_i (ctop) != 0);
	  else
#endif
	    {
	      implicit_arithmetic_conversion (0);
	      if (ER_NODE_MODE (ctop) != ER_NM_int
		  && ER_NODE_MODE (ctop) != ER_NM_float)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_logical_or_operands_types);
	      if (ER_NODE_MODE (ctop) == ER_NM_int)
		ER_set_i (ctop, ER_i (ctop) != 0);
	      else
		{
		  res = ER_f (ctop) != 0.0;
		  ER_SET_MODE (ctop, ER_NM_int);
		  ER_set_i (ctop, res);
		}
	    }
	  if (ER_i (ctop))
	    cpc = IR_short_path_pc (IR_POINTER (cpc));
	  else
	    {
	      TOP_DOWN;
	      INCREMENT_PC ();
	    }
	  BREAK;
	CASE (IR_NM_logical_and):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    ER_set_i (ctop, ER_i (ctop) != 0);
	  else
#endif
	    {
	      implicit_arithmetic_conversion (0);
	      if (ER_NODE_MODE (ctop) != ER_NM_int
		  && ER_NODE_MODE (ctop) != ER_NM_float)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_logical_and_operands_types);
	      if (ER_NODE_MODE (ctop) == ER_NM_int)
		ER_set_i (ctop, ER_i (ctop) != 0);
	      else
		{
		  res = ER_f (ctop) != 0.0;
		  ER_SET_MODE (ctop, ER_NM_int);
		  ER_set_i (ctop, res);
		}
	    }
	  if (! ER_i (ctop))
	    cpc = IR_short_path_pc (IR_POINTER (cpc));
	  else
	    {
	      TOP_DOWN;
	      INCREMENT_PC ();
	    }
	  BREAK;
	CASE (IR_NM_logical_or_end):
	CASE (IR_NM_logical_and_end):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    ER_set_i (ctop, ER_i (ctop) != 0);
	  else
#endif
	    {
	      implicit_arithmetic_conversion (0);
	      if (ER_NODE_MODE (ctop) != ER_NM_int
		  && ER_NODE_MODE (ctop) != ER_NM_float)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    (node_mode == IR_NM_logical_or_end
			     ? DERR_logical_or_operands_types
			     : DERR_logical_and_operands_types));
	      if (ER_NODE_MODE (ctop) == ER_NM_int)
		ER_set_i (ctop, ER_i (ctop) != 0);
	      else
		{
		  res = ER_f (ctop) != 0.0;
		  ER_SET_MODE (ctop, ER_NM_int);
		  ER_set_i (ctop, res);
		}
	    }
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_in):
	  {
	    ER_node_t tab;
	    ER_node_t entry;
	    
	    if (ER_NODE_MODE (ctop) != ER_NM_tab)
	      eval_error (keyop_decl, invkeys_decl,
			  IR_pos (cpc), DERR_in_table_operand_type);
	    tab = ER_tab (ctop);
	    GO_THROUGH_REDIR (tab);
	    entry = find_tab_entry (tab, below_ctop, FALSE);
	    ER_SET_MODE (below_ctop, ER_NM_int);
	    ER_set_i (below_ctop,
		      ER_NODE_MODE (entry) != ER_NM_empty_entry
		      && ER_NODE_MODE (entry) != ER_NM_deleted_entry);
	    TOP_DOWN;
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_not):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    ER_set_i (ctop, ER_i (ctop) == 0);
	  else
#endif
	    {
	      implicit_arithmetic_conversion (0);
	      if (ER_NODE_MODE (ctop) != ER_NM_int
		  && ER_NODE_MODE (ctop) != ER_NM_float)
		eval_error (optype_decl, invops_decl,
			    IR_pos (cpc), DERR_not_operand_type);
	      if (ER_NODE_MODE (ctop) == ER_NM_int)
		ER_set_i (ctop, ER_i (ctop) == 0);
	      else
		{
		  res = ER_f (ctop) == 0.0;
		  ER_SET_MODE (ctop, ER_NM_int);
		  ER_set_i (ctop, res);
		}
	    }
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_bitwise_not):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) != ER_NM_int)
#endif
	    {
	      implicit_int_conversion (0);
	      if (ER_NODE_MODE (ctop) != ER_NM_int)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_bitwise_not_operand_type);
	    }
	  ER_set_i (ctop, ~ER_i (ctop));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_eq):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int
	      && ER_NODE_MODE (below_ctop) == ER_NM_int)
	    {
	      ER_set_i (below_ctop, ER_i (ctop) == ER_i (below_ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  goto common_eq_ne;
#endif
	CASE (IR_NM_ne):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int
	      && ER_NODE_MODE (below_ctop) == ER_NM_int)
	    {
	      ER_set_i (below_ctop, ER_i (ctop) != ER_i (below_ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  /* Fall through. */
#endif
	common_eq_ne:
	  implicit_conversion_for_eq_op ();
	  if (ER_NODE_MODE (ctop) != ER_NODE_MODE (below_ctop))
	    res = 0;
	  else
	    switch (ER_NODE_MODE (ctop))
	      {
	      case ER_NM_nil:
		res = 1;
		break;
	      case ER_NM_hide:
		res = ER_hide (ctop) == ER_hide (below_ctop);
		break;
	      case ER_NM_hideblock:
		if (ER_hideblock_length (ctop)
		    != ER_hideblock_length (below_ctop))
		  res = 0;
		else
		  res = memcmp (ER_hideblock (ctop), ER_hideblock (below_ctop),
				ER_hideblock_length (ctop)) == 0;
		break;
	      case ER_NM_char:
		assert (FALSE); /* ALWAYS transformed to string/int/float */
		break;
	      case ER_NM_int:
		res = ER_i (ctop) == ER_i (below_ctop);
		break;
	      case ER_NM_float:
		res = ER_f (ctop) == ER_f (below_ctop);
		break;
	      case ER_NM_type:
		res = ER_type (ctop) == ER_type (below_ctop);
		break;
	      case ER_NM_vect:
		res = eq_vector (ER_vect (ctop), ER_vect (below_ctop));
		break;
	      case ER_NM_tab:
		res = eq_table (ER_tab (ctop), ER_tab (below_ctop));
		break;
	      case ER_NM_instance:
		res = eq_instance (ER_instance (ctop),
				   ER_instance (below_ctop));
		break;
	      case ER_NM_func:
		res = (ER_func_no (ctop) == ER_func_no (below_ctop)
		       && (ER_func_context (ctop)
			   == ER_func_context (below_ctop)));
		break;
	      case ER_NM_process:
		res = ER_process (ctop) == ER_process (below_ctop);
		break;
	      case ER_NM_class:
		res = (ER_class_no (ctop) == ER_class_no (below_ctop)
		       && (ER_class_context (ctop)
			   == ER_class_context (below_ctop)));
		break;
	      case ER_NM_stack:
		res = ER_stack (ctop) == ER_stack (below_ctop);
		break;
	      default:
		assert (FALSE);
	      }
	  ER_SET_MODE (below_ctop, ER_NM_int);
	  ER_set_i (below_ctop, 
		    (IR_NODE_MODE (IR_POINTER (cpc)) == IR_NM_eq
		     ? res : !res));
	  TOP_DOWN;
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_identity):
	CASE (IR_NM_unidentity):
	  if (ER_NODE_MODE (ctop) != ER_NODE_MODE (below_ctop))
	    res = FALSE;
	  else
	    switch (ER_NODE_MODE (ctop))
	      {
	      case ER_NM_nil:
		res = 1;
		break;
	      case ER_NM_hide:
		res = ER_hide (ctop) == ER_hide (below_ctop);
		break;
	      case ER_NM_hideblock:
		res = ER_hideblock (ctop) == ER_hideblock (below_ctop);
		break;
	      case ER_NM_char:
		res = ER_ch (ctop) == ER_ch (below_ctop);
		break;
	      case ER_NM_int:
		res = ER_i (ctop) == ER_i (below_ctop);
		break;
	      case ER_NM_float:
		res = ER_f (ctop) == ER_f (below_ctop);
		break;
	      case ER_NM_type:
		res = ER_type (ctop) == ER_type (below_ctop);
		break;
	      case ER_NM_vect:
		{
		  ER_node_t vect1 = ER_vect (below_ctop);
		  ER_node_t vect2 = ER_vect (ctop);
		  
		  GO_THROUGH_REDIR (vect2);
		  GO_THROUGH_REDIR (vect1);
		  res = vect1 == vect2;
		}
		break;
	      case ER_NM_tab:
		{
		  ER_node_t tab1 = ER_tab (below_ctop);
		  ER_node_t tab2 = ER_tab (ctop);
		  
		  GO_THROUGH_REDIR (tab2);
		  GO_THROUGH_REDIR (tab1);
		  res = tab1 == tab2;
		}
		break;
	      case ER_NM_instance:
		res = ER_instance (ctop) == ER_instance (below_ctop);
		break;
	      case ER_NM_func:
		/* We don't check the context here. */
		res = (ER_func_no (ctop) == ER_func_no (below_ctop));
		break;
	      case ER_NM_process:
		res = ER_process (ctop) == ER_process (below_ctop);
		break;
	      case ER_NM_class:
		/* We don't check the context here. */
		res = ER_class_no (ctop) == ER_class_no (below_ctop);
		break;
	      case ER_NM_stack:
		res = ER_stack (ctop) == ER_stack (below_ctop);
		break;
	      default:
		assert (FALSE);
	      }
	  ER_SET_MODE (below_ctop, ER_NM_int);
	  ER_set_i (below_ctop,
		    (IR_NODE_MODE (IR_POINTER (cpc)) == IR_NM_identity
		     ? res : !res));
	  TOP_DOWN;
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_lt):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int
	      && ER_NODE_MODE (below_ctop) == ER_NM_int)
	    {
	      ER_set_i (below_ctop, ER_i (below_ctop) < ER_i (ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  else if (ER_NODE_MODE (ctop) == ER_NM_float
		   && ER_NODE_MODE (below_ctop) == ER_NM_float)
	    {
	      ER_set_f (below_ctop, ER_f (below_ctop) < ER_f (ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  goto common_cmp;
#endif
	CASE (IR_NM_ge):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int
	      && ER_NODE_MODE (below_ctop) == ER_NM_int)
	    {
	      ER_set_i (below_ctop, ER_i (below_ctop) >= ER_i (ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  else if (ER_NODE_MODE (ctop) == ER_NM_float
		   && ER_NODE_MODE (below_ctop) == ER_NM_float)
	    {
	      ER_set_f (below_ctop, ER_f (below_ctop) >= ER_f (ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  goto common_cmp;
#endif
	CASE (IR_NM_gt):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int
	      && ER_NODE_MODE (below_ctop) == ER_NM_int)
	    {
	      ER_set_i (below_ctop, ER_i (below_ctop) > ER_i (ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  else if (ER_NODE_MODE (ctop) == ER_NM_float
		   && ER_NODE_MODE (below_ctop) == ER_NM_float)
	    {
	      ER_set_f (below_ctop, ER_f (below_ctop) > ER_f (ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  goto common_cmp;
#endif
	CASE (IR_NM_le):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int
	      && ER_NODE_MODE (below_ctop) == ER_NM_int)
	    {
	      ER_set_i (below_ctop, ER_i (below_ctop) <= ER_i (ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  else if (ER_NODE_MODE (ctop) == ER_NM_float
		   && ER_NODE_MODE (below_ctop) == ER_NM_float)
	    {
	      ER_set_f (below_ctop, ER_f (below_ctop) <= ER_f (ctop));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  /* Fall through */
#endif
	common_cmp:
	  implicit_conversion_for_binary_arithmetic_op ();
	  if (ER_NODE_MODE (ctop) != ER_NM_int
	      && ER_NODE_MODE (ctop) != ER_NM_float
	      || ER_NODE_MODE (below_ctop) != ER_NM_int
	      && ER_NODE_MODE (below_ctop) != ER_NM_float)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			(node_mode == IR_NM_lt
			 ? DERR_lt_operands_types
			 : (node_mode == IR_NM_gt
			    ? DERR_gt_operands_types
			    : (node_mode == IR_NM_le
			       ? DERR_le_operands_types
			       : DERR_ge_operands_types))));
	  if (ER_NODE_MODE (below_ctop) == ER_NM_int)
	    ER_set_i (below_ctop, (node_mode == IR_NM_lt
				   ? ER_i (below_ctop) < ER_i (ctop)
				   : (node_mode == IR_NM_gt
				      ? ER_i (below_ctop) > ER_i (ctop)
				      : (node_mode == IR_NM_le
					 ? ER_i (below_ctop) <= ER_i (ctop)
					 : ER_i (below_ctop) >= ER_i (ctop)))));
	  else
	    {
	      res = (node_mode == IR_NM_lt
		     ? ER_f (below_ctop) < ER_f (ctop)
		     : (node_mode == IR_NM_gt
			? ER_f (below_ctop) > ER_f (ctop)
			: (node_mode == IR_NM_le
			   ? ER_f (below_ctop) <= ER_f (ctop)
			   : ER_f (below_ctop) >= ER_f (ctop))));
	      ER_SET_MODE (below_ctop, ER_NM_int);
	      ER_set_i (below_ctop, res);
	    }
	  TOP_DOWN;
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_unary_plus):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    {
	      INCREMENT_PC ();
	      BREAK;
	    }
	  goto common_unary_plus_minus;
#endif
	CASE (IR_NM_unary_minus):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    {
	      ER_set_i (ctop, -ER_i (ctop));
	      INCREMENT_PC ();
	      BREAK;
	    }
	  /* Fall through */
#endif
	common_unary_plus_minus:
	  implicit_arithmetic_conversion (0);
	  if (ER_NODE_MODE (ctop) != ER_NM_int
	      && ER_NODE_MODE (ctop) != ER_NM_float)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			(node_mode == IR_NM_unary_plus
			 ? DERR_unary_plus_operand_type
			 : DERR_unary_minus_operand_type));
	  if (node_mode == IR_NM_unary_minus)
	    {
	      if (ER_NODE_MODE (ctop) == ER_NM_int)
		ER_set_i (ctop, -ER_i (ctop));
	      else
		ER_set_f (ctop, -ER_f (ctop));
	    }
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_length):
	  to_vect_string_conversion (ctop, NULL);
	  if (ER_NODE_MODE (ctop) != ER_NM_vect
	      && ER_NODE_MODE (ctop) != ER_NM_tab)
	    eval_error (optype_decl, invops_decl,
			IR_pos (cpc), DERR_length_operand_type);
	  if (ER_NODE_MODE (ctop) == ER_NM_vect)
	    {
	      ER_node_t vect = ER_vect (ctop);
	      
	      GO_THROUGH_REDIR (vect);
	      ER_SET_MODE (ctop, ER_NM_int);
	      ER_set_i (ctop, ER_els_number (vect));
	    }
	  else
	    {
	      ER_node_t tab = ER_tab (ctop);
	      
	      GO_THROUGH_REDIR (tab);
	      ER_SET_MODE (ctop, ER_NM_int);
	      ER_set_i (ctop, ER_els_number (tab));
	    }
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_const):
	  make_immutable (ctop);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_new):
	  if (ER_NODE_MODE (ctop) == ER_NM_vect)
	    {
	      ER_node_t vect = ER_vect (ctop);
	      
	      GO_THROUGH_REDIR (vect);
	      vect = copy_vector (vect);
	      ER_set_vect (ctop, vect);
	    }
	  else if (ER_NODE_MODE (ctop) == ER_NM_tab)
	    {
	      ER_node_t tab = ER_tab (ctop);
	      
	      GO_THROUGH_REDIR (tab);
	      tab = copy_tab (tab);
	      ER_set_tab (ctop, tab);
	    }
	  else if (ER_NODE_MODE (ctop) == ER_NM_instance)
	    {
	      size_t size, un;
	      ER_node_t instance;
	      
	      size = instance_size (ER_instance_class (ER_instance (ctop)));
	      instance = heap_allocate (size, FALSE);
	      ER_SET_MODE (instance, ER_NM_heap_instance);
	      un = ER_unique_number (instance);
	      memcpy (instance, ER_instance (ctop), size);
	      ER_set_immutable (instance, FALSE);
	      ER_set_unique_number (instance, un);
	      ER_set_instance (ctop, instance);
	    }
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_typeof):
	  {
	    IR_node_mode_t type;
	    
	    type = node_mode_2_type (ER_NODE_MODE (ctop));
	    ER_SET_MODE (ctop, ER_NM_type);
	    ER_set_type (ctop, type);
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_charof):
	  {
	    int i;
	    
	    implicit_int_conversion (0);
	    if (ER_NODE_MODE (ctop) != ER_NM_int)
	      eval_error (optype_decl, invops_decl, IR_pos (cpc),
			  DERR_conversion_to_char_operand_type);
	    if (ER_i (ctop) > MAX_CHAR || ER_i (ctop) < 0)
	      {
#ifdef ERANGE
		errno = ERANGE;
		process_system_errors ("int-to-char conversion");
#endif
	      }
	    i = ER_i (ctop);
	    ER_SET_MODE (ctop, ER_NM_char);
	    ER_set_ch (ctop, i);
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_intof):
	  implicit_int_conversion (0);
	  if (ER_NODE_MODE (ctop) != ER_NM_int)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			DERR_conversion_to_int_operand_type);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_floatof):
	  {
	    floating_t f;
	    
	    implicit_arithmetic_conversion (0);
	    if (ER_NODE_MODE (ctop) == ER_NM_int)
	      {
		f = ER_i (ctop);
		ER_SET_MODE (ctop, ER_NM_float);
		ER_set_f (ctop, f);
	      }
	    else if (ER_NODE_MODE (ctop) != ER_NM_float)
	      eval_error (optype_decl, invops_decl, IR_pos (cpc),
			  DERR_conversion_to_float_operand_type);
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_format_vectorof):
	  if (ER_NODE_MODE (ctop) != ER_NM_nil)
	    {
	      ER_node_t vect;
	      
	      if (ER_NODE_MODE (below_ctop) != ER_NM_char
		  && ER_NODE_MODE (below_ctop) != ER_NM_int
		  && ER_NODE_MODE (below_ctop) != ER_NM_float
		  && (ER_NODE_MODE (ER_vect (below_ctop))
		      != ER_NM_heap_pack_vect
		      || (ER_pack_vect_el_type (ER_vect (below_ctop))
			  != ER_NM_char)))
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_format_conversion_to_vector_operand_type);
	      to_vect_string_conversion (ctop, NULL);
	      if (ER_NODE_MODE (ctop) != ER_NM_vect
		  || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
		  || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_vector_conversion_format_type);
	      to_vect_string_conversion (below_ctop,
					 ER_pack_els (ER_vect (ctop)));
	      TOP_DOWN;
	      INCREMENT_PC ();
	      BREAK;
	    }
	  TOP_DOWN;
	  /* fall through */
	CASE (IR_NM_vectorof):
	  {
	    ER_node_t vect;
	    
	    to_vect_string_conversion (ctop, NULL);
	    if (ER_NODE_MODE (ctop) != ER_NM_vect)
	      {
		if (ER_NODE_MODE (ctop) != ER_NM_tab)
		  eval_error (optype_decl, invops_decl, IR_pos (cpc),
			      DERR_conversion_to_vector_operand_type);
		vect = table_to_vector_conversion (ER_tab (ctop));
		ER_SET_MODE (ctop, ER_NM_vect);
		ER_set_vect (ctop, vect);
	      }
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_tableof):
	  {
	    ER_node_t tab;
	    
	    if (ER_NODE_MODE (ctop) != ER_NM_tab)
	      {
		to_vect_string_conversion (ctop, NULL);
		if (ER_NODE_MODE (ctop) != ER_NM_vect)
		  eval_error (optype_decl, invops_decl,
			      IR_pos (cpc),
			      DERR_conversion_to_table_operand_type);
		tab = vector_to_table_conversion (ER_vect (ctop));
		ER_SET_MODE (ctop, ER_NM_tab);
		ER_set_tab (ctop, tab);
	      }
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_funcof):
	  if (ER_NODE_MODE (ctop) == ER_NM_stack
	      && IR_func_class_ext (ER_block_node (ER_stack (ctop))) != NULL
	      && !IR_thread_flag (IR_func_class_ext (ER_block_node
						     (ER_stack (ctop)))))
	    {
	      ER_node_t stack;
	      
	      stack = ER_stack (ctop);
	      ER_SET_MODE (ctop, ER_NM_func);
	      ER_set_func_no (ctop,
			      FUNC_CLASS_NO (IR_func_class_ext
					     (ER_block_node (stack))));
	      ER_set_func_context (ctop, ER_context (stack));
	    }
	  else
	    ER_SET_MODE (ctop, ER_NM_nil);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_threadof):
	  if (ER_NODE_MODE (ctop) == ER_NM_process
	      && ER_thread_func (ER_process (ctop)) != NULL)
	    {
	      ER_node_t process;
	      
	      process = ER_process (ctop);
	      ER_SET_MODE (ctop, ER_NM_func);
	      ER_set_func_no (ctop, FUNC_CLASS_NO (ER_thread_func (process)));
	      ER_set_func_context (ctop, ER_context (process));
	    }
	  else
	    ER_SET_MODE (ctop, ER_NM_nil);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_classof):
	  if (ER_NODE_MODE (ctop) == ER_NM_instance)
	    {
	      ER_node_t instance;
	      
	      instance = ER_instance (ctop);
	      ER_SET_MODE (ctop, ER_NM_class);
	      ER_set_class_no (ctop,
			       FUNC_CLASS_NO (ER_instance_class (instance)));
	      ER_set_class_context (ctop, ER_context (instance));
	    }
	  else
	    ER_SET_MODE (ctop, ER_NM_nil);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_cond):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    {
	      if (ER_i (ctop) != 0)
		INCREMENT_PC ();
	      else
		cpc = IR_false_path_pc (IR_POINTER (cpc));
	    }
	  else
#endif
	    {
	      implicit_arithmetic_conversion (0);
	      if (ER_NODE_MODE (ctop) != ER_NM_int
		  && ER_NODE_MODE (ctop) != ER_NM_float)
		eval_error (optype_decl, invops_decl,
			    IR_pos (cpc), DERR_cond_operand_type);
	      if (ER_NODE_MODE (ctop) == ER_NM_int && ER_i (ctop) != 0
		  || ER_NODE_MODE (ctop) == ER_NM_float && ER_f (ctop) != 0.0)
		INCREMENT_PC ();
	      else
		cpc = IR_false_path_pc (IR_POINTER (cpc));
	    }
	  TOP_DOWN;
	  BREAK;
	CASE (IR_NM_cond_end):
	CASE (IR_NM_par_assign_end):
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_vector):
	  {
	    /* If you make a change here, please look at DINO read
	       functions. */
	    ER_node_t vect;
	    int_t vect_parts_number, curr_vect_part_number;
	    int_t curr_vect_element_number;
	    size_t el_size_type;
	    size_t els_number;
	    int_t repetition;
	    int pack_flag;
	    
	    vect_parts_number = IR_parts_number (IR_POINTER (cpc));
	    if (vect_parts_number == 0)
	      vect = create_empty_vector ();
	    else
	      {
		els_number = 0;
		for (curr_vect_part_number = 2 * vect_parts_number - 1;
		     curr_vect_part_number > 0;
		     curr_vect_part_number -= 2)
		  {
		    implicit_int_conversion (curr_vect_part_number);
		    if (ER_NODE_MODE (INDEXED_VAL (ER_CTOP (),
						   -curr_vect_part_number))
			!= ER_NM_int)
		      eval_error (optype_decl, invops_decl, IR_pos (cpc),
				  DERR_elist_repetition_type);
		    else if (ER_i (INDEXED_VAL (ER_CTOP (),
						-curr_vect_part_number)) > 0)
		      els_number += ER_i (INDEXED_VAL
					  (ER_CTOP (),
					   -curr_vect_part_number));
		  }
		pack_flag = TRUE;
		for (curr_vect_part_number = 2 * vect_parts_number - 4;
		     curr_vect_part_number >= 0;
		     curr_vect_part_number -= 2)
		  if (ER_NODE_MODE (INDEXED_VAL (ER_CTOP (),
						 -curr_vect_part_number))
		      != ER_NODE_MODE (INDEXED_VAL (ER_CTOP (),
						    -vect_parts_number * 2
						    + 2)))
		    {
		      pack_flag = FALSE;
		      break;
		    }
		if (pack_flag)
		  {
		    el_size_type = type_size_table [ER_NODE_MODE (ctop)];
		    vect = create_pack_vector (els_number,
					       ER_NODE_MODE (ctop));
		  }
		else
		  vect = create_unpack_vector (els_number);
		for (curr_vect_element_number = 0,
		       curr_vect_part_number = 2 * vect_parts_number - 1;
		     curr_vect_part_number >= 0;
		     curr_vect_part_number -= 2)
		  if (ER_i (INDEXED_VAL (ER_CTOP (), -curr_vect_part_number))
		      > 0)
		    {
		      repetition = ER_i (INDEXED_VAL (ER_CTOP (),
						      -curr_vect_part_number));
		      while (repetition > 0)
			{
			  if (pack_flag)
			    memcpy (ER_pack_els (vect)
				    + el_size_type * curr_vect_element_number,
				    (char *) INDEXED_VAL
                                             (ER_CTOP (),
					      -curr_vect_part_number + 1)
				    + val_displ_table [ER_NODE_MODE
						       (INDEXED_VAL
							(ER_CTOP (),
							 -curr_vect_part_number
							 + 1))],
				    el_size_type);
			  else
			    memcpy (INDEXED_VAL (ER_unpack_els (vect),
						 curr_vect_element_number),
				    INDEXED_VAL (ER_CTOP (),
						 -curr_vect_part_number + 1),
				    sizeof (val_t));
			  repetition--;
			  curr_vect_element_number++;
			}
		    }
		assert ((unsigned_int_t) curr_vect_element_number
			== els_number);
		DECR_CTOP (2 * vect_parts_number);
		SET_TOP;
	      }
	    TOP_UP;
	    ER_SET_MODE (ctop, ER_NM_vect);
	    ER_set_vect (ctop, vect);
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_table):
	  {
	    ER_node_t tab;
	    int_t tab_els_number, curr_tab_el_number;
	    ER_node_t entry;
	    IR_node_t elist;
	    
	    tab_els_number = IR_parts_number (IR_POINTER (cpc));
	    tab = create_tab (tab_els_number);
	    for (elist = IR_elist (IR_POINTER (cpc)),
		   curr_tab_el_number = 2 * tab_els_number - 1;
		 curr_tab_el_number >= 0;
		 curr_tab_el_number -= 2)
	      {
		entry = find_tab_entry (tab,
					INDEXED_VAL (ER_CTOP (),
						     -curr_tab_el_number),
					TRUE);
		if (ER_NODE_MODE (entry) != ER_NM_empty_entry
		    && ER_NODE_MODE (entry) != ER_NM_deleted_entry)
		  eval_error (keyvalue_decl, invkeys_decl, IR_pos (elist),
			      DERR_repeated_key);
		*(val_t *) entry
		  = *(val_t *) INDEXED_VAL (ER_CTOP (), -curr_tab_el_number);
		make_immutable (entry);
		*((val_t *) entry + 1)
		  = *(val_t *) INDEXED_VAL (ER_CTOP (),
					    -curr_tab_el_number + 1);
		assert (ER_IS_OF_TYPE (INDEXED_ENTRY_VAL (entry, 0),
				       ER_NM_val));
		elist = IR_next_elist (elist);
	      }
	    DECR_CTOP (2 * tab_els_number - 1);
	    SET_TOP;
	    ER_SET_MODE (ctop, ER_NM_tab);
	    ER_set_tab (ctop, tab);
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_index):
	  {
	    if (ER_NODE_MODE (below_ctop) != ER_NM_vect)
	      eval_error (indexop_decl, invindexes_decl, IR_pos (cpc),
			  DERR_index_operation_for_non_array);
#ifndef SMALL_CODE
	    if (ER_NODE_MODE (ctop) != ER_NM_int)
#endif
	      implicit_int_conversion (0);
	    load_vector_element_by_index (below_ctop,
					  ER_vect (below_ctop),
					  ctop);
	    TOP_DOWN;
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_lvalue_index):
	  {
	    if (ER_NODE_MODE (below_ctop) != ER_NM_vect)
	      eval_error (indexop_decl, invindexes_decl, IR_pos (cpc),
			  DERR_index_operation_for_non_array);
#ifndef SMALL_CODE
	    if (ER_NODE_MODE (ctop) != ER_NM_int)
#endif
	      implicit_int_conversion (0);
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_lvalue_index_and_val):
	  {
	    if (ER_NODE_MODE (below_ctop) != ER_NM_vect)
	      eval_error (indexop_decl, invindexes_decl, IR_pos (cpc),
			  DERR_index_operation_for_non_array);
#ifndef SMALL_CODE
	    if (ER_NODE_MODE (ctop) != ER_NM_int)
#endif
	      implicit_int_conversion (0);
	    TOP_UP;
	    load_vector_element_by_index
	      (ctop, ER_vect (INDEXED_VAL (ER_CTOP (), -2)), below_ctop);
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_key_index):
	CASE (IR_NM_lvalue_key_index):
	CASE (IR_NM_lvalue_key_index_and_val):
	  {
	    if (ER_NODE_MODE (below_ctop) != ER_NM_tab)
	      eval_error (keyop_decl, invkeys_decl, IR_pos (cpc),
			  DERR_key_index_operation_for_non_table);
	    if (IR_NODE_MODE (IR_POINTER (cpc)) == IR_NM_key_index)
	      {
		load_table_element_by_key (below_ctop, ER_tab (below_ctop),
					   ctop);
		TOP_DOWN;
	      }
	    else if (IR_NODE_MODE (IR_POINTER (cpc))
		     == IR_NM_lvalue_key_index_and_val)
	      {
		TOP_UP;
		load_table_element_by_key
		  (ctop, ER_tab (INDEXED_VAL (ER_CTOP (), -2)), below_ctop);
	      }
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_class_func_thread_call):
	  process_func_class_call
	    (IR_class_func_thread_call_parameters_number (IR_POINTER (cpc)));
	  BREAK;
	CASE (IR_NM_mult):
	  EXECUTE_AR_OP (*, DERR_mult_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_div):
	  execute_div_op ();
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_mod):
	  execute_mod_op ();
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_plus):
	  EXECUTE_AR_OP (+, DERR_plus_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_minus):
	  EXECUTE_AR_OP (-, DERR_minus_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_concat):
	  execute_concat_op ();
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_lshift):
	  EXECUTE_INT_OP (<<, int_t, DERR_lshift_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_rshift):
	  EXECUTE_INT_OP (<<, unsigned_int_t, DERR_rshift_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_ashift):
	  EXECUTE_INT_OP (>>, int_t, DERR_ashift_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_and):
	  EXECUTE_INT_OP (&, int_t, DERR_and_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_xor):
	  EXECUTE_INT_OP (^, int_t, DERR_xor_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_or):
	  EXECUTE_INT_OP (|, int_t, DERR_or_operands_types);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_mult_assign):
	  EXECUTE_AR_OP (*, DERR_mult_operands_types);
	  goto common_assign;
	CASE (IR_NM_div_assign):
	  execute_div_op ();
	  goto common_assign;
	CASE (IR_NM_rem_assign):
	  execute_mod_op ();
	  goto common_assign;
	CASE (IR_NM_minus_assign):
	  EXECUTE_AR_OP (-, DERR_minus_operands_types);
	  goto common_assign;
	CASE (IR_NM_concat_assign):
	  execute_concat_op ();
	  goto common_assign;
	CASE (IR_NM_lshift_assign):
	  EXECUTE_INT_OP (<<, int_t, DERR_lshift_operands_types);
	  goto common_assign;
	CASE (IR_NM_rshift_assign):
	  EXECUTE_INT_OP (<<, unsigned_int_t, DERR_rshift_operands_types);
	  goto common_assign;
	CASE (IR_NM_ashift_assign):
	  EXECUTE_INT_OP (>>, int_t, DERR_ashift_operands_types);
	  goto common_assign;
	CASE (IR_NM_and_assign):
	  EXECUTE_INT_OP (&, int_t, DERR_and_operands_types);
	  goto common_assign;
	CASE (IR_NM_xor_assign):
	  EXECUTE_INT_OP (^, int_t, DERR_xor_operands_types);
	  goto common_assign;
	CASE (IR_NM_or_assign):
	  EXECUTE_INT_OP (|, int_t, DERR_or_operands_types);
	  goto common_assign;
	CASE (IR_NM_plus_assign):
	  EXECUTE_AR_OP (+, DERR_plus_operands_types);
	  /* Fall through: */
	CASE (IR_NM_assign):
	CASE (IR_NM_var_assign):
	CASE (IR_NM_par_assign):
	common_assign:
	  store_designator_value ();
	  DECR_CTOP (3);
	  SET_TOP;
	  INCREMENT_PC ();
	  QUANTUM_SWITCH_PROCESS;
	  BREAK;
	CASE (IR_NM_swap):
	  TOP_UP;
   	  load_given_designator_value (INDEXED_VAL (ER_CTOP (), 1),
				       INDEXED_VAL (ER_CTOP (), -2));
	  load_given_designator_value (ctop, INDEXED_VAL (ER_CTOP (), -4));
	  store_designator_value ();
	  DECR_CTOP (2);
	  SET_TOP;
	  *(val_t *) ctop = *(val_t *) INDEXED_VAL (ER_CTOP (), 3);
	  store_designator_value ();
	  DECR_CTOP (3);
	  SET_TOP;
	  INCREMENT_PC ();
	  QUANTUM_SWITCH_PROCESS;
	  BREAK;
	CASE (IR_NM_par_assign_test):
	  {
	    ER_node_t val;
	    
	    if (ER_NODE_MODE (below_ctop) == ER_NM_stack)
	      val = INDEXED_VAL (ER_stack_vars (ER_stack (below_ctop)),
				 ER_i (ctop));
	    else
	      {
		assert (ER_NODE_MODE (below_ctop) == ER_NM_instance);
		val = INDEXED_VAL (ER_instance_vars (ER_instance (below_ctop)),
				   ER_i (ctop));
	      }
	    if (ER_IS_OF_TYPE (val, ER_NM_nil))
	      {
		INCREMENT_PC ();
		BREAK;
	      }
	    DECR_CTOP (2);
	    SET_TOP;
	    cpc = IR_skip_par_assign_path_pc (IR_POINTER (cpc));
	    QUANTUM_SWITCH_PROCESS;
	  }
	  BREAK;
	CASE (IR_NM_procedure_call):
	  process_func_class_call (IR_procedure_call_pars_number
				   (IR_POINTER (cpc)));
	  /* Switching after popping the result. */
	  BREAK;
	CASE (IR_NM_if_stmt):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    {
	      if (ER_i (ctop) == 0)
		cpc = IR_else_part_pc (IR_POINTER (cpc));
	      else
		INCREMENT_PC ();
	    }
	  else
#endif
	    {
	      implicit_arithmetic_conversion (0);
	      if (ER_NODE_MODE (ctop) != ER_NM_int
		  && ER_NODE_MODE (ctop) != ER_NM_float)
		eval_error (optype_decl, invops_decl,
			    IR_pos (cpc), DERR_invalid_if_expr_type);
	      if (ER_NODE_MODE (ctop) == ER_NM_int && ER_i (ctop) == 0
		  || ER_NODE_MODE (ctop) == ER_NM_float && ER_f (ctop) == 0.0)
		cpc = IR_else_part_pc (IR_POINTER (cpc));
	      else
		INCREMENT_PC ();
	    }
	  TOP_DOWN;
	  QUANTUM_SWITCH_PROCESS;
	  BREAK;
	CASE (IR_NM_for_stmt):
#ifndef SMALL_CODE
	  if (ER_NODE_MODE (ctop) == ER_NM_int)
	    {
	      if (ER_i (ctop) != 0)
		cpc = IR_for_body_pc (IR_POINTER (cpc));
	      else
		INCREMENT_PC ();
	    }
	  else
#endif
	    {
	      implicit_arithmetic_conversion (0);
	      if (ER_NODE_MODE (ctop) != ER_NM_int
		  && ER_NODE_MODE (ctop) != ER_NM_float)
		eval_error (optype_decl, invops_decl, IR_pos (cpc),
			    DERR_invalid_for_guard_expr_type);
	      if (ER_NODE_MODE (ctop) == ER_NM_int && ER_i (ctop) != 0
		  || ER_NODE_MODE (ctop) == ER_NM_float && ER_f (ctop) != 0.0)
		cpc = IR_for_body_pc (IR_POINTER (cpc));
	      else
		INCREMENT_PC ();
	    }
	  TOP_DOWN;
	  QUANTUM_SWITCH_PROCESS;
	  BREAK;
	CASE (IR_NM_foreach_start):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_int);
	  ER_set_i (ctop, 0);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_foreach_next_iteration):
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_int);
	  ER_set_i (ctop, 1);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_foreach_stmt):
	  {
	    int next_iteration_flag;
	    ER_node_t tab;
	    
	    next_iteration_flag = ER_i (INDEXED_VAL (ER_CTOP (), -3));
	    if (ER_NODE_MODE (ctop) != ER_NM_tab)
	      eval_error (keyop_decl, invkeys_decl,
			  IR_pos (cpc), DERR_in_table_operand_type);
	    tab = ER_tab (ctop);
	    GO_THROUGH_REDIR (tab);
	    if (next_iteration_flag)
	      {
		TOP_UP;
		/* Copy designator reference */
		*(val_t *) ctop = *(val_t *) INDEXED_VAL (ER_CTOP (), -2);
		*(val_t *) below_ctop
		  = *(val_t *) INDEXED_VAL (ER_CTOP (), -3);
		load_designator_value ();
		*(val_t *) ctop = *(val_t *) find_next_key (tab, ctop);
	      }
	    else
	      *(val_t *) ctop = *(val_t *) find_next_key (tab, NULL);
	    if (ER_NODE_MODE (ctop) != ER_NM_empty_entry
		&& ER_NODE_MODE (ctop) != ER_NM_deleted_entry)
	      {
		store_designator_value ();
		cpc = IR_foreach_body_pc (IR_POINTER (cpc));
	      }
	    else
	      INCREMENT_PC ();
	    DECR_CTOP (4);
	    SET_TOP;
	    QUANTUM_SWITCH_PROCESS;
	    BREAK;
	  }
	CASE (IR_NM_break_stmt):
	CASE (IR_NM_continue_stmt):
	  {
	    int i;
	    for (i = 0;
		 i < IR_number_of_surrounding_blocks (IR_POINTER (cpc));
		 i++)
	      heap_pop ();
	    INCREMENT_PC ();
	    QUANTUM_SWITCH_PROCESS;
	    BREAK;
	  }
	CASE (IR_NM_block_finish):
	  if (IR_simple_block_finish_flag (IR_POINTER (cpc)))
	    {
	      INCREMENT_PC ();
	      QUANTUM_SWITCH_PROCESS;
	      BREAK;
	    }
	  /* Flow through */
	CASE (IR_NM_return_without_result):
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
			ER_SET_MODE
			  (INDEXED_VAL (ER_ctop (ER_prev_stack (cstack)), 1),
			   ER_NM_nil);
			DECR_TOP (ER_prev_stack (cstack), -1);
		      }
		    else if (IR_NODE_MODE (func_class) == IR_NM_class)
		      {
			/* Self value - 0-th var of block. */
			*(val_t *) INDEXED_VAL (ER_ctop (ER_prev_stack
							 (cstack)), 1)
			  = *(val_t *) ER_stack_vars (cstack);
			DECR_TOP (ER_prev_stack (cstack), -1);
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
	    QUANTUM_SWITCH_PROCESS;
	    BREAK;
	  }
	CASE (IR_NM_return_with_result):
	  {
	    IR_node_t block_node, func;
	    val_t result;
	    
	    result = *(val_t *) ctop;
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
			*(val_t *) INDEXED_VAL (ER_ctop (ER_prev_stack
							 (cstack)), 1)
			  = result;
			TOP_DOWN;
			DECR_TOP (ER_prev_stack (cstack), -1);
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
	    QUANTUM_SWITCH_PROCESS;
	    BREAK;
	  }
	CASE (IR_NM_wait_stmt):
	  if (sync_flag)
	    eval_error (syncwait_decl, errors_decl, IR_pos (cpc),
			DERR_wait_in_sync_stmt);
	  implicit_arithmetic_conversion (0);
	  if (ER_NODE_MODE (ctop) != ER_NM_int
	      && ER_NODE_MODE (ctop) != ER_NM_float)
	    eval_error (optype_decl, invops_decl, IR_pos (cpc),
			DERR_invalid_wait_guard_expr_type);
	  if (ER_NODE_MODE (ctop) == ER_NM_int && ER_i (ctop) == 0
	      || ER_NODE_MODE (ctop) == ER_NM_float && ER_f (ctop) == 0.0)
	    {
	      TOP_DOWN;
	      block_cprocess (IR_start_wait_guard_expr_pc (IR_POINTER (cpc)),
			      TRUE);
	    }
	  else
	    {
	      TOP_DOWN;
	      INCREMENT_PC ();
	      sync_flag = TRUE;
	    }
	  BREAK;
	CASE (IR_NM_wait_finish):
          sync_flag = FALSE;
          INCREMENT_PC ();
	  QUANTUM_SWITCH_PROCESS;
	  BREAK;
	CASE (IR_NM_block):
	  if (!IR_simple_block_flag (IR_POINTER (cpc)))
	    {
	      heap_push (IR_POINTER (cpc), cstack, 0);
	      IR_set_block_saved_sync_flag (IR_POINTER (cpc), sync_flag);
	    }
#if 0
	  else
            abort ();
#endif
	  INCREMENT_PC ();
	  QUANTUM_SWITCH_PROCESS;
	  BREAK;
	CASE (IR_NM_throw):
	  {
	    const char *message;
	    
	    TOP_UP;
	    ER_SET_MODE (ctop, ER_NM_class);
	    ER_set_class_no (ctop, FUNC_CLASS_NO (except_decl));
	    ER_set_class_context (ctop, uppest_stack);
	    if (!ER_IS_OF_TYPE (below_ctop, ER_NM_instance)
		|| !internal_inside_call (&message, 0))
	      eval_error (optype_decl, invops_decl, IR_pos (cpc),
			  DERR_no_exception_after_throw);
	    TOP_DOWN; /* pop class except */
	    exception_position = IR_pos (cpc);
	    cpc = find_catch_pc ();
	  }
	  BREAK;
	CASE (IR_NM_exception):
	  {
	    ER_node_t exception;
	    const char *message;
	    
	    assert (ER_IS_OF_TYPE (below_ctop, ER_NM_instance));
	    exception = ER_instance (below_ctop);
	    if (ER_IS_OF_TYPE (ctop, ER_NM_class)
		&& internal_inside_call (&message, 0))
	      {
		TOP_DOWN; /* class */
		INCREMENT_PC ();
		assert (IR_POINTER (cpc) != NULL
			&& IR_IS_OF_TYPE (IR_POINTER (cpc), IR_NM_block));
		PUSH_TEMP_REF (exception);
		TOP_DOWN; /* exception */
		heap_push (IR_POINTER (cpc), cstack, 1);
		/* Zeroth val of catch block is always corresponding the
		   exception. */
		ER_SET_MODE (INDEXED_VAL (ER_stack_vars (cstack), 0),
			     ER_NM_instance);
		ER_set_instance (INDEXED_VAL (ER_stack_vars (cstack), 0),
				 GET_TEMP_REF (0));
		POP_TEMP_REF (1);
		cpc = IR_next_pc (IR_POINTER (cpc));
	      }
	    else
	      {
		/* Given catch does not correspond to the exception -- try
		   the next catch. */
		TOP_DOWN; /* class */
		cpc = IR_catch_list_pc (cpc);
		if (cpc == NULL)
		  /* No more catches - go to covered try-blocks. */
		  cpc = find_catch_pc ();
	      }
	  }
	  BREAK;
	CASE (IR_NM_local_var_occurrence):
	  TOP_UP;
          *(val_t *) ctop
	    = *(val_t *) INDEXED_VAL (ER_stack_vars (cstack),
				      IR_var_number_in_block
				      (IR_decl (IR_POINTER (cpc))));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_local_lvalue_var_occurrence):
	  DECR_CTOP (-2);
	  SET_TOP;
	  ER_SET_MODE (below_ctop, ER_NM_stack);
          ER_set_stack (below_ctop, cstack);
          ER_SET_MODE (ctop, ER_NM_int);
          ER_set_i (ctop, IR_var_number_in_block (IR_decl (IR_POINTER (cpc))));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_local_lvalue_var_occurrence_and_val):
	  {
	    int var_number_in_block
	      = IR_var_number_in_block (IR_decl (IR_POINTER (cpc)));

	    DECR_CTOP (-3);
	    SET_TOP;
	    ER_SET_MODE (INDEXED_VAL (ER_CTOP (), -2), ER_NM_stack);
	    ER_set_stack (INDEXED_VAL (ER_CTOP (), -2), cstack);
	    ER_SET_MODE (below_ctop, ER_NM_int);
	    ER_set_i (below_ctop, var_number_in_block);
	    *(val_t *) ctop = *(val_t *) INDEXED_VAL (ER_stack_vars (cstack),
						      var_number_in_block);
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_var_occurrence):
	  push_var_val (IR_decl (IR_POINTER (cpc)));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_lvalue_var_occurrence):
	  push_var_ref (IR_decl (IR_POINTER (cpc)));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_lvalue_var_occurrence_and_val):
	  push_var_ref_and_val (IR_decl (IR_POINTER (cpc)));
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_external_var_occurrence):
	CASE (IR_NM_lvalue_external_var_occurrence):
	CASE (IR_NM_lvalue_external_var_occurrence_and_val):
	  push_external_var
	  (IR_decl (IR_POINTER (cpc)),
	   node_mode != IR_NM_external_var_occurrence,
	   node_mode == IR_NM_lvalue_external_var_occurrence_and_val);
	  INCREMENT_PC ();
	  BREAK;
	CASE (IR_NM_external_func_occurrence):
	CASE (IR_NM_func_occurrence):
	  {
	    IR_node_t decl = IR_decl (IR_POINTER (cpc));
	    
	    TOP_UP;
	    ER_SET_MODE (ctop, ER_NM_func);
	    ER_set_func_no (ctop, FUNC_CLASS_NO (decl));
	    ER_set_func_context (ctop,
				 find_context_by_scope (IR_scope (decl)));
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_class_occurrence):
	  {
	    IR_node_t decl = IR_decl (IR_POINTER (cpc));
	    
	    TOP_UP;
	    ER_SET_MODE (ctop, ER_NM_class);
	    ER_set_class_no (ctop, FUNC_CLASS_NO (decl));
	    ER_set_class_context (ctop,
				  find_context_by_scope (IR_scope (decl)));
	    INCREMENT_PC ();
	    BREAK;
	  }
	CASE (IR_NM_if_finish):
	CASE (IR_NM_for_finish):
	CASE (IR_NM_catches_finish):
	  INCREMENT_PC ();
	  QUANTUM_SWITCH_PROCESS;
	  BREAK;
	CASE (IR_NM_pop_func_call):
	  /* Pop possible func result. */
	  TOP_DOWN;
	  INCREMENT_PC ();
	  QUANTUM_SWITCH_PROCESS;
	  BREAK;
#if defined (USE_SWITCH) || !defined (__GNUC__)
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
  ER_node_t string;
  ER_node_t entry;
  val_t key;
  int i, j;
  
  /* Set argv. */
  if (program_arguments_number == 0)
    PUSH_TEMP_REF (create_empty_vector ());
  else
    {
      PUSH_TEMP_REF (create_unpack_vector (program_arguments_number));
      for (i = 0; i < program_arguments_number; i++)
	{
	  string = create_string (program_arguments [i]);
	  var = INDEXED_VAL (ER_unpack_els (GET_TEMP_REF (0)), i);
	  ER_SET_MODE (var, ER_NM_vect);
	  ER_set_vect (var, string);
	}
    }
  assert (IR_scope (argv_decl) == ER_block_node (cstack));
  var = INDEXED_VAL (ER_stack_vars (cstack),
		     IR_var_number_in_block (argv_decl));
  ER_SET_MODE (var, ER_NM_vect);
  ER_set_vect (var, GET_TEMP_REF (0));
  ER_set_immutable (GET_TEMP_REF (0), TRUE);
  POP_TEMP_REF (1);
  /* Set env. */
  for (i = 0; program_environment [i] != NULL; i++)
    ;
  PUSH_TEMP_REF (create_tab (i));
  for (i = 0; program_environment [i] != NULL; i++)
    {
      for (j = 0; program_environment [i][j] != '\0'; j++)
	if (program_environment [i][j] == '=')
	  break;
      if (program_environment [i][j] == '\0')
	eval_error (invenv_decl, errors_decl,
		    no_position, DERR_environment_corrupted);
      program_environment [i][j] = '\0';
      PUSH_TEMP_REF (create_string (program_environment [i]));
      program_environment [i][j] = '=';
      PUSH_TEMP_REF (create_string (program_environment [i] + j + 1));
      ER_SET_MODE ((ER_node_t) &key, ER_NM_vect);
      ER_set_vect ((ER_node_t) &key, GET_TEMP_REF (1));
      entry = find_tab_entry (GET_TEMP_REF (2), (ER_node_t) &key, TRUE);
      if (ER_NODE_MODE (entry) != ER_NM_empty_entry)
	eval_error (invenv_decl, errors_decl,
		    no_position, DERR_environment_corrupted);
      ER_SET_MODE (entry, ER_NM_vect);
      ER_set_vect (entry, GET_TEMP_REF (1));
      make_immutable (entry);
      var = (ER_node_t) ((char *) entry + sizeof (val_t));
      ER_SET_MODE (var, ER_NM_vect);
      ER_set_vect (var, GET_TEMP_REF (0));
      POP_TEMP_REF (2);
   }
  assert (IR_scope (env_decl) == ER_block_node (cstack));
  var = INDEXED_VAL (ER_stack_vars (cstack),
		     IR_var_number_in_block (env_decl));
  ER_SET_MODE (var, ER_NM_tab);
  ER_set_tab (var, GET_TEMP_REF (0));
  ER_set_immutable (GET_TEMP_REF (0), TRUE);
  POP_TEMP_REF (1);
  /* Set version */
  assert (IR_scope (version_decl) == ER_block_node (cstack));
  var = INDEXED_VAL (ER_stack_vars (cstack),
		     IR_var_number_in_block (version_decl));
  ER_SET_MODE (var, ER_NM_float);
  ER_set_f (var, DINO_VERSION);
  /* Set main_thread, curr_thread */
  assert (IR_scope (main_thread_decl) == ER_block_node (cstack));
  var = INDEXED_VAL (ER_stack_vars (cstack),
		     IR_var_number_in_block (main_thread_decl));
  ER_SET_MODE (var, ER_NM_process);
  ER_set_process (var, cprocess);
  assert (IR_scope (curr_thread_decl) == ER_block_node (cstack));
  var = INDEXED_VAL (ER_stack_vars (cstack),
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
  ER_node_t error_instance;

  assert (eval_long_jump_set_flag);
  va_start (arguments, format);
  vsprintf (message, format, arguments);
  va_end (arguments);
  assert (strlen (message) <= MAX_EVAL_ERROR_MESSAGE_LENGTH);
  exception_position = position;
  PUSH_TEMP_REF (create_string (message));
  error_instance = (ER_node_t) heap_allocate (instance_size (except_class),
					      FALSE);
  ER_SET_MODE (error_instance, ER_NM_heap_instance);
  ER_set_instance_class (error_instance, except_class);
  ER_set_block_node (error_instance, IR_next_stmt (except_class));
  ER_set_immutable (error_instance, FALSE);
  ER_set_context
    (error_instance,
     ER_instance (INDEXED_VAL (ER_stack_vars (uppest_stack),
			       IR_var_number_in_block (context_var))));
  ER_set_context_number (error_instance, context_number);
  context_number++;
  /* Zeroth variable is message in class `error' */
  ER_SET_MODE (INDEXED_VAL (ER_instance_vars (error_instance), 0), ER_NM_vect);
  ER_set_vect (INDEXED_VAL (ER_instance_vars (error_instance), 0),
	       GET_TEMP_REF (0));
  POP_TEMP_REF (1);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_instance);
  ER_set_instance (ctop, error_instance);
  cpc = find_catch_pc ();
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
	  && IR_NODE_MODE (IR_POINTER (start_pc)) == IR_NM_block);
  initiate_int_tables ();
  initiate_tables ();
  initiate_funcs ();
  sync_flag = FALSE;
  cpc = start_pc;
  /* The first statement is always block. */
  assert (IR_NODE_MODE (IR_POINTER (cpc)) == IR_NM_block);
  heap_push (IR_POINTER (cpc), NULL, 0);
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
