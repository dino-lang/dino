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

#include "d_common.h"
#include "d_ir.h"
#include "d_runtab.h"
#include "d_context.h"

/* Pointer to block node which opens current scope. */
static IR_node_t curr_scope;

/* Current and previous program counter.  This value is used to
   connect commands for interpreter (see comments for typedef
   pc_t). */
static pc_t curr_pc, prev_pc;

/* Program counter of starting new iteration of current for stmt.
   This value is used to connect continue stmt. */
static pc_t start_next_iteration_pc;

/* Program counter of finishing of current for stmt.
   This value is for_finish node and is used to connect break stmt.
   If this value is equal to NULL then there is not surrounding
   for-stmt. */
static IR_node_t for_finish;

/* Number of blocks inside current for-stmt.  This value is used to
   determine corresponding members in break_stmt node
   or continue_stmt node. */
static int number_of_surrounding_blocks;

/* Report error MSG if DES is a slice.  Use position of POS_NODE for
   this.  */
static void
check_slice (IR_node_t pos_node, IR_node_t des, const char *msg)
{
  if (des == NULL || ! IR_IS_OF_TYPE (des, IR_NM_slice))
    return;
  d_assert (pos_node != NULL);
  error (FALSE, IR_pos (pos_node), msg);
}

/* The following recursive func passes (correctly setting up
   SOURCE_POSITION) EXPR (it may be NULL) and processes idents in
   operations PERIOD.  See also commentaries for abstract data
   block_decl_tables.  The func also sets up members
   parts_number and class_func_thread_call_parameters_number in vector
   node (table node) and class_func_thread_call node. */
static void
first_expr_processing (IR_node_t expr)
{
  if (expr == NULL)
    return;
  switch (IR_NODE_MODE (expr))
    {
    case IR_NM_char:
    case IR_NM_int:
    case IR_NM_float:
    case IR_NM_string:
    case IR_NM_nil:
    case IR_NM_char_type:
    case IR_NM_int_type:
    case IR_NM_float_type:
    case IR_NM_hide_type:
    case IR_NM_hideblock_type:
    case IR_NM_vector_type:
    case IR_NM_table_type:
    case IR_NM_func_type:
    case IR_NM_thread_type:
    case IR_NM_class_type:
    case IR_NM_stack_type:
    case IR_NM_process_type:
    case IR_NM_instance_type:
    case IR_NM_type_type:
      break;
    case IR_NM_ident:
      {
	IR_node_t ident;

	ident = expr;
	expr = find_decl (expr, curr_scope);
	if (expr != NULL && IR_scope (expr) != NULL)
	  process_block_decl_unique_ident (IR_unique_ident (ident));
	break;
      }
    case IR_NM_period:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_designator (expr));
      check_slice (expr, IR_designator (expr),
		   ERR_period_ident_applied_to_slice);
      if (IR_component (expr) != NULL)
	{
	  process_block_decl_unique_ident
	    (IR_unique_ident (IR_component (expr)));
	  IR_set_right_block_decl_ident_number
	    (expr,
	     IR_block_decl_ident_number (IR_unique_ident
					 (IR_component (expr))));
	}
      break;
    case IR_NM_logical_or:
    case IR_NM_logical_and:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_operand (expr));
      first_expr_processing (IR_cont_operand (expr));
      break;
    case IR_NM_in:
    case IR_NM_or:
    case IR_NM_xor:
    case IR_NM_and:
    case IR_NM_eq:
    case IR_NM_ne:
    case IR_NM_identity:
    case IR_NM_unidentity:
    case IR_NM_lt:
    case IR_NM_gt:
    case IR_NM_le:
    case IR_NM_ge:
    case IR_NM_lshift:
    case IR_NM_rshift:
    case IR_NM_ashift:
    case IR_NM_plus:
    case IR_NM_minus:
    case IR_NM_concat:
    case IR_NM_mult:
    case IR_NM_div:
    case IR_NM_mod:
    case IR_NM_format_vectorof:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_left_operand (expr));
      first_expr_processing (IR_right_operand (expr));
      break;
    case IR_NM_unary_plus:
    case IR_NM_unary_minus:
    case IR_NM_length:
    case IR_NM_bitwise_not:
    case IR_NM_not:
    case IR_NM_fold_plus:
    case IR_NM_fold_mult:
    case IR_NM_fold_and:
    case IR_NM_fold_xor:
    case IR_NM_fold_or:
    case IR_NM_const:
    case IR_NM_new:
    case IR_NM_typeof:
    case IR_NM_charof:
    case IR_NM_intof:
    case IR_NM_floatof:
    case IR_NM_vectorof:
    case IR_NM_tableof:
    case IR_NM_funcof:
    case IR_NM_threadof:
    case IR_NM_classof:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_operand (expr));
      break;
    case IR_NM_cond:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_cond_expr (expr));
      first_expr_processing (IR_true_expr (expr));
      first_expr_processing (IR_false_expr (expr));
      break;
    case IR_NM_vector:
    case IR_NM_table:
      SET_SOURCE_POSITION (expr);
      {
	IR_node_t elist;
	int elements_number;
	
	for (elements_number = 0, elist = IR_elist (expr);
	     elist != NULL; elist = IR_next_elist (elist))
	  {
	    elements_number++;
	    SET_SOURCE_POSITION (elist);
	    first_expr_processing (IR_repetition_key (elist));
	    first_expr_processing (IR_expr (elist));
	  }
	IR_set_parts_number (expr, elements_number);
	break;
      }
    case IR_NM_index:
    case IR_NM_key_index:
      SET_SOURCE_POSITION (expr);
      /* Two stack entries are necessary for reference. */
      first_expr_processing (IR_designator (expr));
      check_slice (expr, IR_designator (expr),
		   IR_NODE_MODE (expr) == IR_NM_index
		   ? ERR_vector_element_access_applied_to_slice
		   : ERR_table_element_access_applied_to_slice);
      first_expr_processing (IR_component (expr));
      break;
    case IR_NM_slice:
      SET_SOURCE_POSITION (expr);
      /* Four stack entries are necessary for reference. */
      first_expr_processing (IR_designator (expr));
      first_expr_processing (IR_component (expr));
      first_expr_processing (IR_bound (expr));
      first_expr_processing (IR_step (expr));
    break;
  case IR_NM_class_func_thread_call:
    SET_SOURCE_POSITION (expr);
    {
      IR_node_t elist;
      int parameters_number;
      
      first_expr_processing (IR_func_expr (expr));
      check_slice (expr, IR_func_expr (expr), ERR_call_applied_to_slice);
      for (parameters_number = 0, elist = IR_actuals (expr); elist != NULL;
           elist = IR_next_elist (elist))
	{
	  parameters_number++;
	  SET_SOURCE_POSITION (elist);
	  d_assert (IR_repetition_key (elist) == NULL);
	  first_expr_processing (IR_expr (elist));
	}
      IR_set_class_func_thread_call_parameters_number (expr,
						       parameters_number);
      break;
    }
  default:
    d_unreachable ();
  }
}

/* This func includes DECL into the hash table.  The funcs also fixes
   repeated decl error.  In this case the func returns FALSE.
   Otherwise the func returns TRUE, sets up decl_number_in_block and
   increases decls_number of the block in which the decl is
   immediately placed if the decl has a position. */
static int
include_decl (IR_node_t decl)
{
  IR_node_t *table_entry;

  table_entry = find_table_entry (decl, TRUE);
  if (*table_entry != NULL)
    {
      error (FALSE, IR_pos (IR_ident (decl)), ERR_repeated_decl,
             IR_ident_string (IR_unique_ident (IR_ident (decl))));
      append_message (IR_pos (IR_ident (*table_entry)),
		      ERR_previous_decl_location,
		      IR_ident_string (IR_unique_ident (IR_ident (decl))));
      return FALSE;
    }
  else
    {
      if (!IR_IS_OF_TYPE (decl, IR_NM_ext) && IR_pos (decl).file_name != NULL)
	{
	  IR_set_decl_number_in_block (decl,
				       IR_decls_number (IR_scope (decl)));
	  IR_set_decls_number (IR_scope (decl),
			       IR_decls_number (IR_scope (decl)) + 1);
	}
      *table_entry = decl;
      return TRUE;
    }
}

static IR_node_t first_block_passing (IR_node_t first_level_stmt,
				      int curr_block_level);

/* The function moving EXT_BLOCK stmts into the corresponding
   ORIGIN_BLOCK with possible processing when
   PROCESS_EXT_BLOCK_FLAG. */
static void
process_extension_block (IR_node_t ext_block, IR_node_t origin_block,
			 int process_ext_block_flag)
{
  IR_node_t curr_stmt;
  IR_node_t saved_curr_scope;
  IR_node_t first_stmt;
  IR_node_t last_block_stmt;
  IR_node_t first_access_ident;
  IR_node_t last_access_ident;

  first_stmt = IR_block_stmts (ext_block);
  /* Change scope of decls & blocks in the extension. */
  for (curr_stmt = first_stmt;
       curr_stmt != NULL;
       curr_stmt = IR_next_stmt (curr_stmt))
    if (IR_IS_OF_TYPE (curr_stmt, IR_NM_decl))
      IR_set_scope (curr_stmt, origin_block);
    else if (IR_IS_OF_TYPE (curr_stmt, IR_NM_block))
      IR_set_block_scope (curr_stmt, origin_block);
  if (process_ext_block_flag)
    {
      /* Process the extension block. */
      saved_curr_scope = curr_scope;
      curr_scope = origin_block;
      first_stmt = first_block_passing (first_stmt,
					IR_block_level (origin_block) + 1);
      curr_scope = saved_curr_scope;
    }
  /* Insert the extension block stmts at the end of the
     origin_block. */
  for (last_block_stmt = IR_block_stmts (origin_block);
       last_block_stmt != NULL
	 && IR_next_stmt (last_block_stmt) != NULL;
       last_block_stmt = IR_next_stmt (last_block_stmt))
    ;
  if (last_block_stmt == NULL)
    IR_set_block_stmts (origin_block, first_stmt);
  else
    IR_set_next_stmt (last_block_stmt, first_stmt);
  /* Insert public/private extension block list at the end of the
     origin_block. */
  first_access_ident = IR_access_list (ext_block);
  for (last_access_ident = IR_access_list (origin_block);
       last_access_ident != NULL
	 && IR_next_access_ident (last_access_ident) != NULL;
       last_access_ident = IR_next_access_ident (last_access_ident))
    ;
  if (last_access_ident == NULL)
    IR_set_access_list (origin_block, first_access_ident);
  else
    IR_set_next_access_ident (last_access_ident, first_access_ident);
  /* Check const. */
  if (IR_IS_OF_TYPE (IR_func_class_ext (origin_block), IR_NM_func_or_class)
      && IR_final_flag (IR_func_class_ext (origin_block)))
    error (FALSE, IR_pos (IR_ident (IR_func_class_ext (ext_block))),
	   ERR_extension_of_final,
	   IR_ident_string (IR_unique_ident
			    (IR_ident (IR_func_class_ext (origin_block)))));
}

/* All extensions included into the table during first_block_passing.
   It is used for checking that the extended funcs or classes have
   been found. */
static vlo_t possible_table_extensions;

/* This recursive func passes all stmts and exprs (correctly
   setting up SOURCE_POSITION and curr_scope (before first call of
   the func curr_scope is to be NULL)) on the same
   stmt nesting level, includes decls into the hash table, sets up
   decl block_level of any block node, sets up
   decl_number_in_block, increases decls_number and
   vars_number of the block in which the decl (var) is
   immediately placed, processes idents in operations PERIOD
   create class decls idents table for new class decl
   (see also commentaries for corresponding abstract data) and sets up
   member proc_call_pars_number in proc_call node.
   The funcs also fixes repeated decl error.
   FIRST_LEVEL_STMT (it may be NULL) is first stmt of the processed
   stmt nesting level.  CURR_BLOCK_LEVEL is number of blocks which
   contain given stmt list.
   The function also removes extensions (moving its statements into
   the extended func or class) and their blocks from the statement
   list.  Therefore the function returns new first_level_stmt which is
   different from the original one oly if an extension is the first in
   the list. */
static IR_node_t
first_block_passing (IR_node_t first_level_stmt, int curr_block_level)
{
  IR_node_t stmt;
  IR_node_t prev_stmt;
  IR_node_t next_stmt;

  for (prev_stmt = NULL, stmt = first_level_stmt;
       stmt != NULL;
       prev_stmt = stmt, stmt = next_stmt)
    {
      next_stmt = IR_next_stmt (stmt);
      SET_SOURCE_POSITION (stmt);
      switch (IR_NODE_MODE (stmt))
	{
	case IR_NM_assign:
	case IR_NM_var_assign:
	case IR_NM_par_assign:
	  first_expr_processing (IR_assignment_var (stmt));
	  first_expr_processing (IR_assignment_expr (stmt));
	  break;
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_mod_assign:
	case IR_NM_plus_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	  first_expr_processing (IR_assignment_var (stmt));
	  first_expr_processing (IR_assignment_expr (stmt));
	  break;
	case IR_NM_proc_call:
	  {
	    IR_node_t elist;
	    int parameters_number;
	    
	    first_expr_processing (IR_proc_expr (stmt));
	    check_slice (stmt, IR_proc_expr (stmt), ERR_call_applied_to_slice);
	    for (parameters_number = 0, elist = IR_proc_actuals (stmt);
		 elist != NULL; elist = IR_next_elist (elist))
	      {
		parameters_number++;
		SET_SOURCE_POSITION (elist);
		d_assert (IR_repetition_key (elist) == NULL);
		first_expr_processing (IR_expr (elist));
	      }
	    IR_set_proc_call_pars_number (stmt, parameters_number);
	    break;  
	  }
	case IR_NM_if_stmt:
	  first_expr_processing (IR_if_expr (stmt));
	  IR_set_if_part
	    (stmt,
	     first_block_passing (IR_if_part (stmt), curr_block_level));
	  IR_set_else_part
	    (stmt,
	     first_block_passing (IR_else_part (stmt), curr_block_level));
	  break;
	case IR_NM_for_stmt:
	  IR_set_for_initial_stmt
	    (stmt, first_block_passing (IR_for_initial_stmt (stmt),
					curr_block_level));
	  first_expr_processing (IR_for_guard_expr (stmt));
	  IR_set_for_iterate_stmt
	    (stmt, first_block_passing (IR_for_iterate_stmt (stmt),
					curr_block_level));
	  IR_set_for_stmts
	    (stmt,
	     first_block_passing (IR_for_stmts (stmt), curr_block_level));
	  break;
	case IR_NM_foreach_stmt:
	  first_expr_processing (IR_foreach_designator (stmt));
	  first_expr_processing (IR_foreach_table (stmt));
	  IR_set_foreach_stmts (stmt,
				first_block_passing (IR_foreach_stmts (stmt),
						     curr_block_level));
	  break;
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	case IR_NM_return_without_result:
	  break;
	case IR_NM_return_with_result:
	  first_expr_processing (IR_returned_expr (stmt));
	  break;
	case IR_NM_throw:
	  first_expr_processing (IR_throw_expr (stmt));
	  break;
	case IR_NM_wait_stmt:
	  first_expr_processing (IR_wait_guard_expr (stmt));
	  IR_set_wait_stmt (stmt,
			    first_block_passing (IR_wait_stmt (stmt),
						 curr_block_level));
	  break;
	case IR_NM_block:
	  {
	    IR_node_t saved_curr_scope = curr_scope;
	    IR_node_t curr_except;

	    IR_set_decls_number (stmt, 0);
	    IR_set_vars_number (stmt, 0);
	    curr_scope = stmt;
	    IR_set_block_level (stmt, curr_block_level);
	    if (max_block_level < curr_block_level)
	      max_block_level = curr_block_level;
	    IR_set_block_stmts
	      (stmt, first_block_passing (IR_block_stmts (stmt),
					  curr_block_level + 1));
	    curr_scope = saved_curr_scope;
	    IR_set_block_number (stmt, new_block ());
	    if (IR_func_class_ext (stmt) == NULL
		&& IR_decls_number (stmt) == 0
		&& IR_exceptions (stmt) == NULL)
	      IR_set_simple_block_flag (stmt, TRUE);
	    for (curr_except = IR_exceptions (stmt);
		 curr_except != NULL;
		 curr_except = IR_next_exception (curr_except))
	      {
		first_expr_processing (IR_exception_class_expr (curr_except));
		if (IR_catch_block (curr_except) != NULL)
		  IR_set_catch_block
		    (curr_except,
		     first_block_passing (IR_catch_block (curr_except),
					  curr_block_level));
	      }
	    break;
	  }
	case IR_NM_var:
	  if (include_decl (stmt))
	    {
	      IR_set_var_number_in_block (stmt,
					  IR_vars_number (IR_scope (stmt)));
	      IR_set_vars_number (IR_scope (stmt),
				  IR_vars_number (IR_scope (stmt)) + 1);
	    }
	  break;
	case IR_NM_external_var:
	case IR_NM_external_func:
	  include_decl (stmt);
	  break;
	case IR_NM_func:
	  {
	    IR_node_t *table_entry;

	    table_entry = find_table_entry (stmt, FALSE);
	    if (*table_entry != NULL
		&& IR_IS_OF_TYPE (*table_entry, IR_NM_ext))
	      {
		process_extension_block (IR_next_stmt (*table_entry),
					 IR_next_stmt (stmt), FALSE);
		/* Change extension in the table onto the func */
		*table_entry = stmt;
	      }
	    else
	      include_decl (stmt);
	  }
	  break;
	case IR_NM_class:
	  {
	    IR_node_t *table_entry;

	    table_entry = find_table_entry (stmt, FALSE);
	    if (*table_entry != NULL
		&& IR_IS_OF_TYPE (*table_entry, IR_NM_ext))
	      {
		process_extension_block (IR_next_stmt (*table_entry),
					 IR_next_stmt (stmt), FALSE);
		/* Change extension in the table onto the class */
		*table_entry = stmt;
	      }
	    else
	      include_decl (stmt);
	  }
	  break;
	case IR_NM_ext:
	  {
	    IR_node_t decl;

	    decl = *(IR_node_t *) find_table_entry (stmt, FALSE);
	    if (decl != NULL && IR_IS_OF_TYPE (decl, IR_NM_func_class_ext))
	      process_extension_block (IR_next_stmt (stmt),
				       IR_next_stmt (decl), TRUE);
	    else if (include_decl (stmt))
	      VLO_ADD_MEMORY (possible_table_extensions, &stmt, sizeof (stmt));
	    /* Skip subsequent processing of the extension block
	       becuase we've processed it already. */
	    next_stmt = IR_next_stmt (IR_next_stmt (stmt));
	    stmt = prev_stmt;
	    /* Remove extension and its block from the chain of
	       stmts. See epilog of the loop. */
	    if (prev_stmt == NULL)
	      first_level_stmt = next_stmt;
	    else
	      IR_set_next_stmt (prev_stmt, next_stmt);
	  }
	  break;
	default:
	  d_unreachable ();
	}
    }
  return first_level_stmt;
}

/* This recursive func passes (correctly setting up SOURCE_POSITION,
   curr_scope and curr_real_scope (before first call of the
   func to both are to be NULL)) all stmts on the same stmt nesting
   level and sets up elements values of class decls idents tables (see
   also commentaries for corresponding abstract data).
   FIRST_LEVEL_STMT (it may be NULL) is the first stmt of the
   processed stmt nesting level.
   
   The function also processes access lists, sets up public_flag
   for declarations, and sets up func class table.  */
static void
second_block_passing (IR_node_t first_level_stmt)
{
  IR_node_t stmt;

  for (stmt = first_level_stmt; stmt != NULL; stmt = IR_next_stmt (stmt))
    {
      SET_SOURCE_POSITION (stmt);
      switch (IR_NODE_MODE (stmt))
	{
	case IR_NM_assign:
	case IR_NM_var_assign:
	case IR_NM_par_assign:
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_mod_assign:
	case IR_NM_plus_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	case IR_NM_proc_call:
	  break;
	case IR_NM_if_stmt:
	  second_block_passing (IR_if_part (stmt));
	  second_block_passing (IR_else_part (stmt));
	  break;
	case IR_NM_for_stmt:
	  second_block_passing (IR_for_initial_stmt (stmt));
	  second_block_passing (IR_for_iterate_stmt (stmt));
	  second_block_passing (IR_for_stmts (stmt));
	  break;
	case IR_NM_foreach_stmt:
	  {
	    IR_node_t des = IR_foreach_designator (stmt);

	    if (des != NULL && IR_IS_OF_TYPE (des, IR_NM_slice))
	      {
		IR_set_foreach_designator (stmt, NULL);
		error (FALSE, IR_pos (des), ERR_slice_as_foreach_designator);
	      }
	    second_block_passing (IR_foreach_stmts (stmt));
	    break;
	  }
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	case IR_NM_return_without_result:
	case IR_NM_return_with_result:
	case IR_NM_throw:
	  break;
	case IR_NM_wait_stmt:
	  second_block_passing (IR_wait_stmt (stmt));
	  break;
	case IR_NM_block:
	  {
	    IR_node_t saved_curr_scope = curr_scope;
	    IR_node_t curr_stmt;
	    IR_node_t curr_access_ident;
	    IR_node_t decl;
	    IR_node_t friend;
	    IR_node_t curr_except;
	    int public_flag;

	    curr_scope = stmt;
	    second_block_passing (IR_block_stmts (stmt));
	    public_flag = (IR_func_class_ext (curr_scope) != NULL
			   && IR_IS_OF_TYPE (IR_func_class_ext (curr_scope),
					     IR_NM_class));
	    for (curr_stmt = IR_block_stmts (stmt); curr_stmt != NULL;
		 curr_stmt = IR_next_stmt (curr_stmt))
	      if (IR_IS_OF_TYPE (curr_stmt, IR_NM_decl))
		IR_set_public_flag (curr_stmt, public_flag);
	    for (curr_access_ident = IR_access_list (curr_scope);
		 curr_access_ident != NULL;
		 curr_access_ident = IR_next_access_ident (curr_access_ident))
	      if (IR_friend_flag (curr_access_ident))
		{
		  d_assert (!IR_access_ident_public_flag (curr_access_ident));
		  decl = find_decl (IR_ident_in_clause (curr_access_ident),
				    curr_scope);
		  if (decl == NULL)
		    error (FALSE, IR_pos (IR_ident_in_clause (curr_access_ident)),
			   ERR_udenclared_ident_access_list,
			   IR_ident_string (IR_unique_ident
					    (IR_ident_in_clause
					     (curr_access_ident))));
		  else if (!IR_IS_OF_TYPE (decl, IR_NM_func_or_class))
		    error (FALSE, IR_pos (IR_ident_in_clause (curr_access_ident)),
			   ERR_invalid_friend,
			   IR_ident_string (IR_unique_ident
					    (IR_ident_in_clause
					     (curr_access_ident))));
		  else
		    {
		      friend = create_node (IR_NM_friend);
		      IR_set_friend_decl (friend, decl);
		      IR_set_next_friend (friend, IR_friends (stmt));
		      IR_set_friends (stmt, friend);
		    }
		}
	      else
		{
		  d_assert (!IR_friend_flag (curr_access_ident));
		  decl
		    = find_decl_in_given_scope (IR_ident_in_clause (curr_access_ident),
						curr_scope);
		  if (decl == NULL)
		    error (FALSE, IR_pos (IR_ident_in_clause (curr_access_ident)),
			   ERR_udenclared_ident_access_list,
			   IR_ident_string (IR_unique_ident
					    (IR_ident_in_clause
					     (curr_access_ident))));
		  else if (IR_access_ident (decl) == NULL)
		    {
		      IR_set_public_flag
			(decl,
			 IR_access_ident_public_flag (curr_access_ident));
		      IR_set_access_ident (decl, curr_access_ident);
		    }
		  else if (IR_access_ident_public_flag (curr_access_ident)
			   != IR_access_ident_public_flag (IR_access_ident
							   (decl)))
		    {
		      error (FALSE, IR_pos (IR_ident_in_clause (curr_access_ident)),
			     ERR_contradicted_ident_access_list,
			     IR_ident_string (IR_unique_ident
					      (IR_ident_in_clause (curr_access_ident))));
		      append_message
			(IR_pos (IR_ident_in_clause (IR_access_ident (decl))),
			 ERR_previous_access_location,
			 IR_ident_string (IR_unique_ident
					  (IR_ident_in_clause (curr_access_ident))));
		    }
		}
	    curr_scope = saved_curr_scope;
	    for (curr_except = IR_exceptions (stmt);
		 curr_except != NULL;
		 curr_except = IR_next_exception (curr_except))
	      if (IR_catch_block (curr_except) != NULL)
		second_block_passing (IR_catch_block (curr_except));
	    break;
	  }
	case IR_NM_external_func:
	case IR_NM_func:
	case IR_NM_class:
	  set_func_class_no (stmt);
	  /* FALL THROUGH  */
	case IR_NM_var:
	case IR_NM_external_var:
	  {
	    IR_node_t block;

	    block = IR_scope (stmt);
	    d_assert (block != NULL && IR_NODE_MODE (block) == IR_NM_block);
	    define_block_decl (stmt, block);
	    break;
	  }
	default:
	  d_unreachable ();
	}
    }
}

/* The following macro value is TRUE if EXPR is of type within
   TYPE_MASK or of unknown type (i.e. type must be determined in
   run_time).  EXPR may be equal to NULL. */
#define IT_IS_OF_TYPE(expr, type_mask)\
  ((value_type (expr) & (type_mask)) || value_type (expr) == EVT_UNKNOWN)

/* The following func returns type of EXPR.  The func may return
   unknown type (it means that the type can be determined only in run time).
   EXPR may be equal to NULL (in this case func result is unknown
   type).  The members value_type must be set up already, and idents must
   be changed by corresponding decl occurrence.  */
static do_inline int
value_type (IR_node_t expr)
{
  if (expr == NULL)
    return EVT_UNKNOWN;
  return IR_value_type (expr);
}

/* The func tests that EXPR is of TYPE (or unknown type).  If it is
   false then error MESSAGE is fixed.  EXPR may be NULL, in this case
   the func does nothing. */
static void
type_test (IR_node_t expr, type_mask_t type_mask, char *message)
{
  if (expr == NULL)
    return;
  if (!IT_IS_OF_TYPE (expr, type_mask))
    error (FALSE, source_position, message);
}

/* Pointer to non simple block node containing the current scope. */
static IR_node_t curr_real_scope;
/* Vars number in the current real scope.  */
static int curr_vars_number;

/* Number of additional variables for different purpose. */
#define ADD_TEMP_VARS_NUMBER 2

/* This macro is used to connect commands for interpreter (see comments for
   typedef pc_t) into chain by members next_pc. */
#define SET_PC(node)\
   do {\
     prev_pc = curr_pc;\
     if (curr_pc != NULL)\
       IR_set_next_pc (IR_PTR (curr_pc), PC (node));\
     curr_pc = PC (node);\
   } while (0)

/* See comments for the function third_expr_processing.  This variable
   is used for searching for side effects in wait stmt expression. */
static int there_is_function_call_in_expr;

static int do_inline
get_temp_stack_slot (int *temp_vars_num)
{
  int res = *temp_vars_num + curr_vars_number;

  (*temp_vars_num)++;
  if (*temp_vars_num > IR_temporary_vars_number (curr_real_scope))
    IR_set_temporary_vars_number (curr_real_scope, *temp_vars_num);
  return res;
}

static int do_inline
setup_result_var_number (int *result, int *temp_vars_num)
{
  int new_p = result == NULL || *result < 0;
  int res = (new_p ? get_temp_stack_slot (temp_vars_num) : *result);

  if (result != NULL)
    *result = res;
  return res;
}

static IR_node_t
third_expr_processing (IR_node_t expr, int func_class_assign_p,
		       int *result, int *curr_temp_vars_num, int lvalue_p);

static void do_inline
process_unary_op (IR_node_t op, int *result, int *curr_temp_vars_num)
{
  int op_result = -1;
  int temp_vars_num = *curr_temp_vars_num;

  SET_SOURCE_POSITION (op);
  op_result = -1;
  IR_set_operand
    (op, third_expr_processing (IR_operand (op), FALSE,
				&op_result, &temp_vars_num, FALSE));
  IR_set_op_num (op, op_result);
  IR_set_result_num
    (op, setup_result_var_number (result, curr_temp_vars_num));
  SET_PC (op);
}

static void do_inline
process_binary_op (IR_node_t op, int *result, int *curr_temp_vars_num)
{
  int op_result = -1;
  int temp_vars_num = *curr_temp_vars_num;

  SET_SOURCE_POSITION (op);
  op_result = -1;
  IR_set_left_operand
    (op, third_expr_processing (IR_left_operand (op), FALSE,
				&op_result, &temp_vars_num, FALSE));
  IR_set_left_op_num (op, op_result);
  op_result = -1;
  IR_set_right_operand
    (op, third_expr_processing (IR_right_operand (op), FALSE,
				&op_result, &temp_vars_num, FALSE));
  IR_set_right_op_num (op, op_result);
  IR_set_result_num
    (op, setup_result_var_number (result, curr_temp_vars_num));
  SET_PC (op);
}

/* Return true if at least one operand of L and R is a slice.  */
static do_inline int
bin_slice_p (IR_node_t l, IR_node_t r)
{
  return ((l != NULL && IR_value_type (l) == EVT_SLICE)
	  || (r != NULL && IR_value_type (r) == EVT_SLICE));
}

/* Return true if OP is a slice.  */
static do_inline int
unary_slice_p (IR_node_t op)
{
  return op != NULL && IR_value_type (op) == EVT_SLICE;
}

/* Check EXPR whose value in OP_NUM for a slice and, if it is so, add
   flatten node.  */
static do_inline void
add_flatten_node_if_necessary (IR_node_t expr, int op_num)
{
  IR_node_t flatten;

  if (expr != NULL && IR_value_type (expr) == EVT_SLICE)
    {
      flatten = create_node_with_pos (IR_NM_flatten, source_position);
      IR_set_flatten_vect_num (flatten, op_num);
      SET_PC (flatten);
    }
}

/* Process actual parameters ACTUALS and add flatten nodes if
   necessary.  */
static void
process_actuals (IR_node_t actuals, int *temp_vars_num)
{
  int par_num;
  IR_node_t elist;

  for (elist = actuals; elist != NULL; elist = IR_next_elist (elist))
    {
      SET_SOURCE_POSITION (elist);
      par_num = *temp_vars_num + curr_vars_number;
      IR_set_expr (elist, third_expr_processing (IR_expr (elist), TRUE,
						 NULL, temp_vars_num, FALSE));
      add_flatten_node_if_necessary (IR_expr (elist), par_num);
    }
}

/* The following recursive func passes (correctly setting up
   SOURCE_POSITION) EXPR (it may be NULL) and
   changes idents on corresponding decls occurrences
   according to languge visibility rules.
   Also the func tests that ident in period operations is
   defined in any class.
   Therefore the func returns pointer to (possibly)
   modified expr.  See also commentaries for abstract data
   block_decl_tables. 
   The func also tests operands types in operation.
   The func creates chain by member next_pc to evaluate the processed 
   expr.  The chain forms polish postfix order.

   The function sets up flag THERE_IS_FUNCTION_CALL_IN_EXPR if there
   is a function call in EXPR.  Otherwise, the function does not
   change the variable value.

   The function also sets up flag extended_life_context depending on
   func_class_assign_p (see comment for field
   extended_life_context. */

static IR_node_t
third_expr_processing (IR_node_t expr, int func_class_assign_p,
		       int *result, int *curr_temp_vars_num, int lvalue_p)
{
  IR_node_t l, r;
  int temp_vars_num = *curr_temp_vars_num;

  if (expr == NULL)
    return expr;
  switch (IR_NODE_MODE (expr))
    {
    case IR_NM_char:
      IR_set_ch_val (expr, IR_char_value (IR_unique_char (expr)));
      IR_set_char_result_num
	(expr, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_CHAR);
      SET_PC (expr);
      break;
    case IR_NM_int:
      IR_set_i_val (expr, IR_int_value (IR_unique_int (expr)));
      IR_set_int_result_num
	(expr, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_INT);
      SET_PC (expr);
      break;
    case IR_NM_float:
      IR_set_f_val (expr, IR_float_value (IR_unique_float (expr)));
      IR_set_float_result_num
	(expr, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_FLOAT);
      SET_PC (expr);
      break;
    case IR_NM_string:
      IR_set_str_val (expr, IR_string_value (IR_unique_string (expr)));
      IR_set_string_result_num
	(expr, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_VECTOR);
      SET_PC (expr);
      break;
    case IR_NM_nil:
      IR_set_nil_result_num
	(expr, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_UNKNOWN);
      SET_PC (expr);
      break;
    case IR_NM_char_type:
    case IR_NM_int_type:
    case IR_NM_float_type:
    case IR_NM_hide_type:
    case IR_NM_hideblock_type:
    case IR_NM_vector_type:
    case IR_NM_table_type:
    case IR_NM_func_type:
    case IR_NM_thread_type:
    case IR_NM_class_type:
    case IR_NM_stack_type:
    case IR_NM_process_type:
    case IR_NM_instance_type:
    case IR_NM_type_type:
      IR_set_type_result_num
	(expr, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_TYPE);
      SET_PC (expr);
      break;
    case IR_NM_ident:
      {
	IR_node_t ident;
	IR_node_t decl;
	int local_p;

	ident = expr;
	decl = find_decl (expr, curr_scope);
	SET_SOURCE_POSITION (expr);
	if (decl == NULL)
	  {
	    decl = create_node_with_pos (IR_NM_var, no_position);
	    IR_set_scope (decl, NULL); /* Mark it.  */
	    IR_set_ident (decl, ident);
	    include_decl (decl);
	    error (FALSE, source_position, ERR_undeclared_ident,
		   IR_ident_string (IR_unique_ident (ident)));
	    return NULL;
	  }
	else if (IR_pos (decl).file_name == NULL)
	  return NULL; /* no_position.  */
	else
	  {
	    local_p
	      = (curr_real_scope == IR_scope (decl)
		 && (IR_func_class_ext (curr_real_scope) == NULL
		     || ! IR_IS_OF_TYPE (IR_func_class_ext (curr_real_scope),
					 IR_NM_class)));
	    expr = create_occurrence_node (decl, IR_pos (ident), local_p);
	    if (result != NULL
		&& IR_IS_OF_TYPE (expr, IR_NM_local_var_occurrence))
	      *result = IR_var_number_in_block (decl);
	    else if (! lvalue_p)
	      {
		IR_set_occurrence_result_num
		  (expr,
		   setup_result_var_number (result, curr_temp_vars_num));
		if (IR_IS_OF_TYPE (expr, IR_NM_local_var_occurrence))
		  IR_set_local_var_num (expr, IR_var_number_in_block (decl));
		SET_PC (expr);
	      }
	    else
	      {
		d_assert (result != NULL && *result < 0);
		/* We need 2 stack slots for non local var occurrence
		   lvalue representation.  */
		IR_set_occurrence_result_num
		  (expr, setup_result_var_number (result, curr_temp_vars_num));
		get_temp_stack_slot (curr_temp_vars_num);
		SET_PC (expr);
	      }
	    if (IR_NODE_MODE (expr) == IR_NM_func_occurrence)
	      IR_set_value_type (expr, EVT_FUNC);
	    else if (IR_NODE_MODE (expr) == IR_NM_class_occurrence)
	      IR_set_value_type (expr, EVT_CLASS);
	    else
	      IR_set_value_type (expr, EVT_UNKNOWN);
	    if (func_class_assign_p
		&& (IR_NODE_MODE (expr) == IR_NM_func_occurrence
		    || IR_NODE_MODE (expr) == IR_NM_class_occurrence))
	      IR_set_extended_life_context_flag (curr_real_scope, TRUE);
	  }
	break;
      }
    case IR_NM_period:
      {
	int op_result = -1;

	d_assert (! lvalue_p || result == NULL || *result < 0);
	SET_SOURCE_POSITION (expr);
	IR_set_designator
	  (expr, third_expr_processing (IR_designator (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	IR_set_designator_op_num (expr, op_result);
	if (lvalue_p)
	  {
	    IR_set_designator_result_num (expr, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  IR_set_designator_result_num
	    (expr, setup_result_var_number (result, curr_temp_vars_num));
	SET_PC (expr);
	d_assert (IR_component (expr) != NULL
		  && IR_NODE_MODE (IR_component (expr)) == IR_NM_ident);
	if (IR_designator (expr) != NULL)
	  {
	    if (!IR_it_is_declared_in_block (IR_unique_ident
					     (IR_component (expr))))
	      error (FALSE, source_position, ERR_decl_is_absent_in_a_block);
	  }
	break;
      }
    case IR_NM_logical_or:
    case IR_NM_logical_and:
      {
	IR_node_t logical_end;
	int op_result;

	SET_SOURCE_POSITION (expr);
	op_result = (result == NULL ? -1 : *result);
	IR_set_operand
	  (expr, third_expr_processing (IR_operand (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	IR_set_op_num (expr, op_result);
	IR_set_result_num
	  (expr, setup_result_var_number (result, curr_temp_vars_num));
	SET_PC (expr);
	logical_end = create_node_with_pos ((IR_NODE_MODE (expr)
					     == IR_NM_logical_or
					     ? IR_NM_logical_or_end
					     : IR_NM_logical_and_end),
					    source_position);
        IR_set_short_path_pc (IR_PTR (curr_pc), PC (logical_end));
	op_result = (result == NULL ? -1 : *result);
	temp_vars_num = *curr_temp_vars_num;
	IR_set_cont_operand
	  (expr, third_expr_processing (IR_cont_operand (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	IR_set_cont_op_num (logical_end, op_result);
	IR_set_cont_result_num (logical_end, IR_result_num (expr));
	IR_set_pos (logical_end, source_position);
	type_test (IR_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_logical_operation_operand_type);
	type_test (IR_cont_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_logical_operation_operand_type);
	IR_set_value_type (expr, EVT_INT);
	SET_PC (logical_end);
      }
      break;
    case IR_NM_not:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_logical_operation_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_eq:
    case IR_NM_ne:
    case IR_NM_identity:
    case IR_NM_unidentity:
      process_binary_op (expr, result, curr_temp_vars_num);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_lt:
    case IR_NM_gt:
    case IR_NM_le:
    case IR_NM_ge:
      process_binary_op (expr, result, curr_temp_vars_num);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_order_comparison_operation_operand_type);
      type_test (r, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_order_comparison_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_concat:
      process_binary_op (expr, result, curr_temp_vars_num);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_concat_operation_operand_type);
      type_test (r, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_concat_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_VECTOR);
      break;
    case IR_NM_in:
      process_binary_op (expr, result, curr_temp_vars_num);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (r, EVT_TABLE_SLICE_MASK, ERR_invalid_table_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_or:
    case IR_NM_xor:
    case IR_NM_and:
    case IR_NM_lshift:
    case IR_NM_rshift:
    case IR_NM_ashift:
      process_binary_op (expr, result, curr_temp_vars_num);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      type_test (r, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_plus:
    case IR_NM_minus:
    case IR_NM_mult:
    case IR_NM_div:
    case IR_NM_mod:
      {
	type_mask_t type_mask1;
	type_mask_t type_mask2;
	
	process_binary_op (expr, result, curr_temp_vars_num);
	l = IR_left_operand (expr); r = IR_right_operand (expr);
	type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		   ERR_invalid_arithmetic_operation_operand_type);
	type_test (r, EVT_NUMBER_VECTOR_SLICE_MASK,
		   ERR_invalid_arithmetic_operation_operand_type);
	type_mask1 = value_type (l);
	type_mask2 = value_type (r);
	if ((type_mask1 == EVT_FLOAT || type_mask2 == EVT_FLOAT)
	    && (type_mask1 & EVT_SLICE) == 0
	    && (type_mask2 & EVT_SLICE) == 0)
	  IR_set_value_type (expr, EVT_FLOAT);
	else if ((type_mask1 == EVT_INT || type_mask1 == EVT_CHAR)
		 && (type_mask2 == EVT_INT || type_mask2 == EVT_CHAR))
	  IR_set_value_type (expr, EVT_INT);
	else
	  IR_set_value_type (expr,
			     bin_slice_p (l, r) ? EVT_SLICE : EVT_NUMBER_MASK);
      }
      break;
    case IR_NM_unary_plus:
    case IR_NM_unary_minus:
      {
	type_mask_t type_mask;
	
	process_unary_op (expr, result, curr_temp_vars_num);
	l = IR_operand (expr);
	type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		   ERR_invalid_arithmetic_operation_operand_type);
	type_mask = value_type (l);
	if (type_mask == EVT_FLOAT)
	  IR_set_value_type (expr, EVT_FLOAT);
	else if (type_mask == EVT_INT || type_mask == EVT_CHAR)
	  IR_set_value_type (expr, EVT_INT);
	else
	  IR_set_value_type
	    (expr, unary_slice_p (l) ? EVT_SLICE : EVT_NUMBER_MASK);
      }
      break;
    case IR_NM_bitwise_not:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_length:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_TABLE_SLICE_MASK,
		 ERR_invalid_length_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_fold_plus:
    case IR_NM_fold_mult:
      process_unary_op (expr, result, curr_temp_vars_num);
      type_test (IR_operand (expr), EVT_SLICE,
		 ERR_invalid_fold_arithmetic_operation_operand_type);
      IR_set_value_type (expr, EVT_NUMBER_MASK);
      break;
    case IR_NM_fold_and:
    case IR_NM_fold_xor:
    case IR_NM_fold_or:
      process_unary_op (expr, result, curr_temp_vars_num);
      type_test (IR_operand (expr), EVT_SLICE,
		 ERR_invalid_fold_arithmetic_operation_operand_type);
      IR_set_value_type (expr, EVT_INT);
      break;
    case IR_NM_typeof:
      process_unary_op (expr, result, curr_temp_vars_num);
      IR_set_value_type
	(expr, unary_slice_p (IR_operand (expr)) ? EVT_SLICE : EVT_TYPE);
      break;
    case IR_NM_charof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_conversion_to_char_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_CHAR);
      break;
    case IR_NM_intof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_conversion_to_int_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_floatof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_conversion_to_float_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_FLOAT);
      break;
    case IR_NM_vectorof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_TABLE_SLICE_MASK,
		 ERR_invalid_conversion_to_vector_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_VECTOR);
      break;
    case IR_NM_format_vectorof:
      process_binary_op (expr, result, curr_temp_vars_num);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_TABLE_SLICE_MASK,
		 ERR_invalid_conversion_to_vector_operand_type);
      type_test (r, EVT_VECTOR_SLICE_MASK, ERR_invalid_conversion_format_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_VECTOR);
      break;
    case IR_NM_tableof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_TABLE_SLICE_MASK,
		 ERR_invalid_conversion_to_table_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_TABLE);
      break;
    case IR_NM_funcof:
    case IR_NM_threadof:
    case IR_NM_classof:
      process_unary_op (expr, result, curr_temp_vars_num);
      IR_set_value_type (expr, EVT_UNKNOWN); /* May be nil */
      break;
    case IR_NM_new:
    case IR_NM_const:
      process_unary_op (expr, result, curr_temp_vars_num);
      IR_set_value_type (expr, value_type (IR_operand (expr)));
      break;
    case IR_NM_cond:
      {
	IR_node_t cond_end;
        pc_t true_path_begin_pc, saved_prev_pc;
	int op_result;

        SET_SOURCE_POSITION (expr);
	op_result = -1;
        IR_set_cond_expr
	  (expr, third_expr_processing (IR_cond_expr (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	IR_set_cond_op_num (expr, op_result);
	saved_prev_pc = curr_pc;
	SET_PC (expr);
	cond_end = create_node_with_pos (IR_NM_cond_end, source_position);
	IR_set_cond_common_part_pc (expr, PC (cond_end));
	temp_vars_num = *curr_temp_vars_num;
	/* Put it on stack.  */
        IR_set_true_expr
	  (expr, third_expr_processing (IR_true_expr (expr),
					func_class_assign_p,
					NULL, &temp_vars_num, FALSE));
	d_assert (temp_vars_num = *curr_temp_vars_num + 1);
	SET_PC (cond_end);
	true_path_begin_pc = IR_next_pc (expr);
	prev_pc = saved_prev_pc;
        curr_pc = PC (expr);
	temp_vars_num = *curr_temp_vars_num;
	/* Put it on stack.  */
        IR_set_false_expr
	  (expr, third_expr_processing (IR_false_expr (expr),
					func_class_assign_p,
					NULL, &temp_vars_num, FALSE));
	if (result != NULL)
	  *result = -1;
	setup_result_var_number (result, curr_temp_vars_num);
	/* Overall result should be on stack.  */
	d_assert (result == NULL
		  || (*result + 1 == temp_vars_num + curr_vars_number
		      && *curr_temp_vars_num == temp_vars_num));
	SET_PC (cond_end);
	IR_set_false_path_pc (expr, IR_next_pc (expr));
        IR_set_next_pc (expr, true_path_begin_pc);
        type_test (IR_cond_expr (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_cond_type);
        IR_set_value_type (expr,
                           value_type (IR_true_expr (expr))
                           | value_type (IR_false_expr (expr)));
      }
      break;
    case IR_NM_vector:
    case IR_NM_table:
      {
	IR_node_t elist;
	
	SET_SOURCE_POSITION (expr);
	IR_set_vec_tab_el_num (expr, *curr_temp_vars_num + curr_vars_number);
	IR_set_vec_tab_result_num
	  (expr, setup_result_var_number (result, curr_temp_vars_num));
	for (elist = IR_elist (expr);
	     elist != NULL;
	     elist = IR_next_elist (elist))
	  {
	    SET_SOURCE_POSITION (elist);
	    IR_set_repetition_key
	      (elist,
	       third_expr_processing (IR_repetition_key (elist), FALSE,
				      NULL, &temp_vars_num, FALSE));
            if (IR_NODE_MODE (expr) == IR_NM_vector)
              type_test (IR_repetition_key (elist), EVT_NUMBER_STRING_MASK,
                         ERR_invalid_repetition_type);
	    IR_set_expr
	      (elist, third_expr_processing (IR_expr (elist), TRUE,
					     NULL, &temp_vars_num, FALSE));
	  }
	IR_set_value_type (expr,
			   IR_NODE_MODE (expr) == IR_NM_vector
			   ? EVT_VECTOR : EVT_TABLE);
	SET_PC (expr);
	break; 
      }
    case IR_NM_index:
    case IR_NM_key_index:
      {
	int op_result = -1;

	d_assert (! lvalue_p || result == NULL || *result < 0);
	SET_SOURCE_POSITION (expr);
	IR_set_designator
	  (expr, third_expr_processing (IR_designator (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	IR_set_designator_op_num (expr, op_result);
	if (lvalue_p)
	  *curr_temp_vars_num = temp_vars_num;
	op_result = -1;
	IR_set_component
	  (expr, third_expr_processing (IR_component (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	IR_set_component_op_num (expr, op_result);
	if (lvalue_p)
	  {
	    IR_set_designator_result_num (expr, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  IR_set_designator_result_num
	    (expr, setup_result_var_number (result, curr_temp_vars_num));
	SET_PC (expr);
	if (IR_NODE_MODE (expr) == IR_NM_index)
	  {
	    type_test (IR_designator (expr), EVT_VECTOR_SLICE_MASK,
		       ERR_invalid_vector_type);
	    type_test (IR_component (expr), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_index_type);
	  }
	else
	  type_test (IR_designator (expr), EVT_TABLE_SLICE_MASK,
		     ERR_invalid_table_type);
	break;
      }
    case IR_NM_slice:
      {
	IR_node_t des = IR_designator (expr);

	d_assert (! lvalue_p || result == NULL || *result < 0);
	SET_SOURCE_POSITION (expr);
	if (des != NULL)
	  {
	    IR_set_designator_op_num (expr, temp_vars_num + curr_vars_number);
	    des = third_expr_processing (des, FALSE, NULL, &temp_vars_num,
					 IR_IS_OF_TYPE (des, IR_NM_slice));
	    if (des != NULL && IR_IS_OF_TYPE (des, IR_NM_slice))
	      {
		IR_set_dim (expr, IR_dim (des) + 1);
		IR_set_designator_op_num (expr, IR_designator_op_num (des));
		curr_pc = prev_pc;
	      }
	  }
	IR_set_designator (expr, des);
	IR_set_component
	  (expr, third_expr_processing (IR_component (expr), FALSE,
					NULL, &temp_vars_num, FALSE));
	IR_set_bound
	  (expr, third_expr_processing (IR_bound (expr), FALSE,
					NULL, &temp_vars_num, FALSE));
	IR_set_step
	  (expr, third_expr_processing (IR_step (expr), FALSE,
					NULL, &temp_vars_num, FALSE));
	if (lvalue_p)
	  {
	    IR_set_designator_result_num (expr, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  IR_set_designator_result_num
	    (expr, setup_result_var_number (result, curr_temp_vars_num));
	SET_PC (expr);
	type_test (IR_designator (expr), EVT_VECTOR_SLICE_MASK,
		   ERR_invalid_vector_type);
	type_test (IR_component (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_slice_start_type);
	type_test (IR_bound (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_slice_bound_type);
	type_test (IR_step (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_slice_step_type);
	IR_set_value_type (expr, EVT_SLICE);
	break;
      }
    case IR_NM_class_func_thread_call:
      {
	int par_num;
	IR_node_t elist, flatten;

	there_is_function_call_in_expr = TRUE;
	SET_SOURCE_POSITION (expr);
	IR_set_func_call_start_num (expr, temp_vars_num + curr_vars_number);
	IR_set_func_expr
	  (expr, third_expr_processing (IR_func_expr (expr), FALSE,
					NULL, &temp_vars_num, FALSE));
	process_actuals (IR_actuals (expr), &temp_vars_num);
	if (result != NULL)
	  *result = -1;
	setup_result_var_number (result, curr_temp_vars_num);
	SET_PC (expr);
	if (!IT_IS_OF_TYPE (IR_func_expr (expr), EVT_FUNC)
	    && !IT_IS_OF_TYPE (IR_func_expr (expr), EVT_CLASS))
	  error (FALSE, source_position,
		 ERR_invalid_class_func_thread_designator);
	IR_set_value_type (expr, EVT_UNKNOWN);
	break;
      }
    default:
      d_unreachable ();
    }
  return expr;
}

/* Return new mode of designator node.  Difference between value and
   lvalue is needed for the evaluator.  The following table are used
   for that

   IR_NM_period                  IR_NM_lvalue_period(_and_val)
   IR_NM_index                   IR_NM_lvalue_index(_and_val)
   IR_NM_key_index               IR_NM_lvalue_key_index(_and_val)
   IR_NM_var_occurrence          IR_NM_lvalue_var_occurrence(_and_val)
   IR_NM_external_var_occurrence IR_NM_lvalue_external_var_occurrence(_and_val)

   IR_NM_local_var_occurrence    IR_NM_local_var_occurrence

   There is no change for local_var_occurrence.  If DESIGNATOR is not
   correct, generate ERROR_MESSAGE. */

static IR_node_mode_t
make_designator_lvalue (IR_node_t designator, const char *error_message,
			int val_p)
{
    switch (IR_NODE_MODE (designator))
      {
      case IR_NM_period:
	return (val_p ? IR_NM_lvalue_period_and_val : IR_NM_lvalue_period);
      case IR_NM_index:
	return (val_p ? IR_NM_lvalue_index_and_val : IR_NM_lvalue_index);
      case IR_NM_slice:
	return (val_p ? IR_NM_lvalue_slice_and_val : IR_NM_lvalue_slice);
      case IR_NM_key_index:
	return (val_p
		? IR_NM_lvalue_key_index_and_val : IR_NM_lvalue_key_index);
      case IR_NM_local_var_occurrence:
	return IR_NM_local_var_occurrence;
      case IR_NM_var_occurrence:
	return (val_p
		? IR_NM_lvalue_var_occurrence_and_val
		: IR_NM_lvalue_var_occurrence);
      case IR_NM_external_var_occurrence:
	return (val_p
		? IR_NM_lvalue_external_var_occurrence_and_val
		: IR_NM_lvalue_external_var_occurrence);
      default:
	error (FALSE, IR_pos (designator), error_message);
	return IR_NODE_MODE (designator);
      }
}

/* Return the corresponding binary operation mode to an assignment
   node ASSIGN.  */
static IR_node_mode_t
make_op_mode (IR_node_t assign)
{
    switch (IR_NODE_MODE (assign))
      {
      case IR_NM_mult_assign:
	return IR_NM_mult;
      case IR_NM_div_assign:
	return IR_NM_div;
      case IR_NM_mod_assign:
	return IR_NM_mod;
      case IR_NM_plus_assign:
	return IR_NM_plus;
      case IR_NM_minus_assign:
	return IR_NM_minus;
      case IR_NM_concat_assign:
	return IR_NM_concat;
      case IR_NM_lshift_assign:
	return IR_NM_lshift;
      case IR_NM_rshift_assign:
	return IR_NM_rshift;
      case IR_NM_ashift_assign:
	return IR_NM_ashift;
      case IR_NM_and_assign:
	return IR_NM_and;
      case IR_NM_xor_assign:
	return IR_NM_xor;
      case IR_NM_or_assign:
	return IR_NM_or;
      default:
	d_unreachable ();
      }
}

/* Return the corresponding slice assign mode for assignment of MODE.  */
static IR_node_mode_t
make_slice_assign_mode (IR_node_mode_t mode)
{
    switch (mode)
      {
      case IR_NM_mult_assign:
	return IR_NM_mult_slice_assign;
      case IR_NM_div_assign:
	return IR_NM_div_slice_assign;
      case IR_NM_mod_assign:
	return IR_NM_mod_slice_assign;
      case IR_NM_plus_assign:
	return IR_NM_plus_slice_assign;
      case IR_NM_minus_assign:
	return IR_NM_minus_slice_assign;
      case IR_NM_concat_assign:
	return IR_NM_concat_slice_assign;
      case IR_NM_lshift_assign:
	return IR_NM_lshift_slice_assign;
      case IR_NM_rshift_assign:
	return IR_NM_rshift_slice_assign;
      case IR_NM_ashift_assign:
	return IR_NM_ashift_slice_assign;
      case IR_NM_and_assign:
	return IR_NM_and_slice_assign;
      case IR_NM_xor_assign:
	return IR_NM_xor_slice_assign;
      case IR_NM_or_assign:
	return IR_NM_or_slice_assign;
      default:
	return IR_NM_slice_assign;
      }
}

static IR_node_t
find_covered_func_class_ext (IR_node_t scope)
{
  while (scope != NULL && IR_func_class_ext (scope) == NULL)
    scope = IR_block_scope (scope);
  if (scope != NULL)
    return IR_func_class_ext (scope);
  else
    return NULL;
}


/* Add local var occurrence tp the pc chain.  Define VAR_NUM and
   RESULT_NUM for VAR.  */
static void
add_local_var_occurrence (IR_node_t var, int result_num, int var_num)
{
  d_assert (IR_IS_OF_TYPE (var, IR_NM_local_var_occurrence));
  IR_set_occurrence_result_num (var, result_num);
  IR_set_local_var_num (var, var_num);
  SET_PC (var);
}

/* Return location and lvalue (through CONTAINER_NUM, INDEX_NUM, and
   LVALUE_VAL_NUM) of a lvalue and value DESIGNATOR.  If the
   designator is local var occurrence, create first corresponding
   lvalue_var_occurrence_and_val node and add it to the pc chain.  */
static void
get_lvalue_and_val_location (IR_node_t designator, int *temp_vars_num,
			     int *container_num, int *index_num,
			     int *lvalue_val_num)
{
  IR_node_t op;
  
  if (IR_IS_OF_TYPE (designator, IR_NM_local_var_occurrence))
    {
      op = create_node_with_pos (IR_NM_lvalue_var_occurrence_and_val,
				 IR_pos (designator));
      SET_PC (op);
      IR_set_decl (op, IR_decl (designator));
      IR_set_occurrence_result_num
	(op, get_temp_stack_slot (temp_vars_num));
      *container_num = IR_occurrence_result_num (op);
      *index_num = get_temp_stack_slot (temp_vars_num);
      *lvalue_val_num = get_temp_stack_slot (temp_vars_num);
    }
  else if (IR_IS_OF_TYPE (designator, IR_NM_lvalue_index_and_val)
	   || IR_IS_OF_TYPE (designator, IR_NM_lvalue_key_index_and_val))
    {
      *container_num = IR_designator_op_num (designator);
      *index_num = IR_component_op_num (designator);
      *lvalue_val_num = get_temp_stack_slot (temp_vars_num);
      IR_set_designator_result_num (designator, *lvalue_val_num);
    }
  else if (IR_IS_OF_TYPE (designator, IR_NM_lvalue_period_and_val))
    {
      IR_set_designator_result_num  /* container */
	(designator, get_temp_stack_slot (temp_vars_num));
      *container_num = IR_designator_result_num (designator);
      *index_num = get_temp_stack_slot (temp_vars_num);
      *lvalue_val_num = get_temp_stack_slot (temp_vars_num);
    }
  else
    {
      *container_num = IR_occurrence_result_num (designator);
      *index_num = IR_occurrence_result_num (designator) + 1;
      *lvalue_val_num = get_temp_stack_slot (temp_vars_num);
    }
}

/* Return pointer to the block of first function decl surrounding
   given SCOPE (it may be NULL).  Return NULL if there is not such
   function decl.  */
static IR_node_t
surrounding_func_block (IR_node_t scope)
{
  IR_node_t func_class_ext;

  for (; scope != NULL; scope = IR_block_scope (scope))
    {
      func_class_ext = IR_func_class_ext (scope);
      if (func_class_ext != NULL && IR_NODE_MODE (func_class_ext) == IR_NM_func)
	return scope;
    }
  return NULL;
}

/* This recursive func passes (correctly setting up SOURCE_POSITION
   and curr_scope (before first call of the func curr_scope is to be
   NULL)) all stmts on the same stmt nesting level and sets up
   elements values of class decls idents tables (see also commentaries
   for corresponding abstract data).  FIRST_LEVEL_STMT (it may be
   NULL) is first stmt of the processed stmt nesting level.  The func
   creates chain by members next_pc, (for_,foreach_)body_pc,
   else_part_pc and start_wait_guard_expr_pc to execution of the
   processed stmts.  The func may create new nodes block_finish node,
   if_finish node, and for_finish node and include them into the
   chain.  The func also change mode of node given as ASSIGN NODE
   (stmt)->assignment_var in assignment stmt or foreach stmt (with the
   aid of `make_designator_lvalue').  It is needed for the evaluator.

   The funcs also fixes repeated errors ERR_continue_is_not_in_loop,
   ERR_break_is_not_in_loop. */
static void
third_block_passing (IR_node_t first_level_stmt)
{
  IR_node_t stmt, res_stmt;
  IR_node_t op, temp;
  IR_node_mode_t stmt_mode;
  int result, var_result, temp_vars_num;
  int container_num, index_num, lvalue_val_num;

  for (stmt = first_level_stmt; stmt != NULL; stmt = IR_next_stmt (stmt))
    {
      temp_vars_num = 0;
      SET_SOURCE_POSITION (stmt);
      switch (stmt_mode = IR_NODE_MODE (stmt))
	{
	case IR_NM_assign:
	case IR_NM_var_assign:
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_mod_assign:
	case IR_NM_plus_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	  var_result = -1;
	  temp = third_expr_processing (IR_assignment_var (stmt), FALSE,
					&var_result, &temp_vars_num, TRUE);
	  res_stmt = stmt;
	  if (temp != NULL)
	    {
	      IR_SET_MODE (temp,
			   make_designator_lvalue
			   (temp, ERR_non_variable_in_assignment,
			    stmt_mode != IR_NM_assign
			    && stmt_mode != IR_NM_var_assign));
	      if (IR_IS_OF_TYPE (temp, IR_NM_index_designator))
		{
		  IR_set_container_num (res_stmt, IR_designator_op_num (temp));
		  IR_set_index_num (res_stmt, IR_component_op_num (temp));
		  if (IR_IS_OF_TYPE (temp, IR_NM_slice))
		    {
		      res_stmt = (create_node_with_pos
				  (make_slice_assign_mode (stmt_mode),
				   IR_pos (stmt)));
		      IR_set_assignment_var (res_stmt,
					     IR_assignment_var (stmt));
		      IR_set_assignment_expr (res_stmt,
					      IR_assignment_expr (stmt));
		      IR_set_container_num (res_stmt,
					    IR_designator_op_num (temp));
		      IR_set_index_num (res_stmt, IR_dim (temp));
		    }
		}
	      else if (IR_IS_OF_TYPE (temp, IR_NM_lvalue_var_occurrence)
		       || IR_IS_OF_TYPE (temp,
					 IR_NM_lvalue_external_var_occurrence))
		{
		  IR_set_container_num (res_stmt, IR_occurrence_result_num (temp));
		  IR_set_index_num (res_stmt, IR_occurrence_result_num (temp) + 1);
		}
	      else if (IR_IS_OF_TYPE (temp,
				      IR_NM_lvalue_var_occurrence_and_val)
		       || (IR_IS_OF_TYPE
			   (temp,
			    IR_NM_lvalue_external_var_occurrence_and_val)))
		{
		  IR_set_container_num (res_stmt, IR_occurrence_result_num (temp));
		  IR_set_index_num (res_stmt, IR_occurrence_result_num (temp) + 1);
		}
	      if (IR_IS_OF_TYPE (temp,
				 IR_NM_lvalue_external_var_occurrence_and_val)
		  || IR_IS_OF_TYPE (temp, IR_NM_lvalue_var_occurrence_and_val))
		IR_set_lvalue_val_num
		  (res_stmt, get_temp_stack_slot (&temp_vars_num));
	      else if (IR_IS_OF_TYPE (temp, IR_NM_lvalue_period)
		       || IR_IS_OF_TYPE (temp, IR_NM_lvalue_period_and_val))
		{
		  IR_set_designator_result_num  /* container */
		    (temp, get_temp_stack_slot (&temp_vars_num));
		  IR_set_container_num (res_stmt, IR_designator_result_num (temp));
		  /* decl number: */
		  IR_set_index_num (res_stmt, get_temp_stack_slot (&temp_vars_num));
		  if (IR_IS_OF_TYPE (temp, IR_NM_lvalue_period_and_val))
		    IR_set_lvalue_val_num
		      (res_stmt, get_temp_stack_slot (&temp_vars_num));
		}
	      else if (IR_IS_OF_TYPE (temp, IR_NM_lvalue_index_and_val)
		       || IR_IS_OF_TYPE (temp, IR_NM_lvalue_slice_and_val)
		       || IR_IS_OF_TYPE (temp, IR_NM_lvalue_key_index_and_val))
		{
		  IR_set_lvalue_val_num
		    (res_stmt, get_temp_stack_slot (&temp_vars_num));
		  IR_set_designator_result_num (temp, IR_lvalue_val_num (res_stmt));
		}
	      else
		IR_set_lvalue_val_num (res_stmt, -1);
	      d_assert ((curr_pc == temp
			 || IR_IS_OF_TYPE (temp, IR_NM_local_var_occurrence))
			&& prev_pc != NULL);
	      if (IR_IS_OF_TYPE (temp, IR_NM_lvalue_index)
		  || IR_IS_OF_TYPE (temp, IR_NM_lvalue_slice)
		  || IR_IS_OF_TYPE (temp, IR_NM_lvalue_key_index))
		curr_pc = prev_pc;
	    }
	  if (stmt_mode != IR_NM_var_assign && temp != NULL
	      && (IR_IS_OF_TYPE (temp, IR_NM_local_var_occurrence)
		  || IR_IS_OF_TYPE (temp, IR_NM_lvalue_var_occurrence)
		  || IR_IS_OF_TYPE (temp,
				    IR_NM_lvalue_external_var_occurrence)
		  || IR_IS_OF_TYPE (temp, IR_NM_lvalue_var_occurrence_and_val)
		  || IR_IS_OF_TYPE
 		     (temp, IR_NM_lvalue_external_var_occurrence_and_val))
	      && (IR_IS_OF_TYPE (IR_decl (temp), IR_NM_var)
		  || IR_IS_OF_TYPE (IR_decl (temp), IR_NM_external_var))
	      && IR_const_flag (IR_decl (temp)))
	    error (FALSE, IR_pos (temp), ERR_const_assignment,
		   IR_ident_string (IR_unique_ident
				    (IR_ident (IR_decl (temp)))));
	  if (temp != NULL)
	    IR_set_assignment_var (res_stmt, temp);
	  if (temp != NULL
	      && IR_IS_OF_TYPE (temp, IR_NM_local_var_occurrence)
	      && (stmt_mode == IR_NM_assign || stmt_mode == IR_NM_var_assign))
	    result = var_result;
	  else
	    result = -1;
	  IR_set_assignment_expr
	    (res_stmt,
	     third_expr_processing (IR_assignment_expr (res_stmt), TRUE,
				    &result, &temp_vars_num, FALSE));
	  IR_set_expr_num (res_stmt, result);
	  if (temp == NULL
	      || ! IR_IS_OF_TYPE (temp, IR_NM_local_var_occurrence))
	    {
	      if (temp != NULL && ! IR_IS_OF_TYPE (temp, IR_NM_slice))
		add_flatten_node_if_necessary (IR_assignment_expr (res_stmt),
					       result);
	      SET_PC (res_stmt);
	    }
	  else
	    {
	      if (stmt_mode == IR_NM_assign || stmt_mode == IR_NM_var_assign)
		{
		  if (result != var_result)
		    add_local_var_occurrence (temp, var_result, result);
		}
	      else
		{
		  /* Generate op (var_result, var_result, result).  */
		  op = create_node_with_pos (make_op_mode (res_stmt),
					     IR_pos (res_stmt));
		  IR_set_left_operand (op, temp);
		  IR_set_right_operand (op, IR_assignment_expr (res_stmt));
		  IR_set_result_num (op, var_result);
		  IR_set_left_op_num (op, var_result);
		  IR_set_right_op_num (op, result);
		  SET_PC (op);
		}
	      if (! IR_IS_OF_TYPE (temp, IR_NM_slice))
		add_flatten_node_if_necessary (IR_assignment_expr (res_stmt),
					       var_result);
	    }
	  break;
	case IR_NM_par_assign:
	  {
	    IR_node_t par_assign_test, par_assign_end, lvalue;

	    var_result = -1;
	    temp = third_expr_processing (IR_assignment_var (stmt), FALSE,
					  &var_result, &temp_vars_num, FALSE);
	    d_assert (temp != NULL
		      && ((IR_IS_OF_TYPE (temp, IR_NM_local_var_occurrence)
			   && var_result >= 0 && temp_vars_num == 0)
			  || IR_IS_OF_TYPE (temp, IR_NM_var_occurrence)));
	    IR_set_assignment_var (stmt, temp);
	    par_assign_test = create_node_with_pos (IR_NM_par_assign_test,
						    source_position);
	    par_assign_end = create_node_with_pos (IR_NM_par_assign_end,
						   source_position);
	    IR_set_par_num (par_assign_test, var_result);
	    IR_set_skip_par_assign_path_pc (par_assign_test, par_assign_end);
	    SET_PC (par_assign_test);
	    temp_vars_num = 0;
	    if (temp != NULL)
	      {
		if (IR_IS_OF_TYPE (temp, IR_NM_local_var_occurrence))
		  result = var_result;
		else
		  {
		    result = -1;
		    lvalue = (create_node_with_pos
			      (make_designator_lvalue
			       (temp, ERR_non_variable_in_assignment, FALSE),
			       source_position));
		    IR_set_decl (lvalue, IR_decl (temp));
		    /* We need two stack slots for lvalue var occurrence.  */
		    IR_set_occurrence_result_num
		      (lvalue, get_temp_stack_slot (&temp_vars_num));
		    get_temp_stack_slot (&temp_vars_num);
		    SET_PC (lvalue);
		  }
	      }
	    IR_set_assignment_expr
	      (stmt, third_expr_processing (IR_assignment_expr (stmt), TRUE,
					    &result, &temp_vars_num, FALSE));
	    add_flatten_node_if_necessary (IR_assignment_expr (stmt), result);
	    if (! IR_IS_OF_TYPE (temp, IR_NM_local_var_occurrence))
	      {
		IR_set_container_num (stmt, IR_occurrence_result_num (temp));
		IR_set_index_num (stmt, IR_occurrence_result_num (temp) + 1);
		IR_set_expr_num (stmt, result);
		SET_PC (stmt);
	      }
	    else if (result != var_result)
	      add_local_var_occurrence (temp, var_result, result);
	    SET_PC (par_assign_end);
	  }
	  break;
	case IR_NM_proc_call:
	  IR_set_proc_call_start_num (stmt, temp_vars_num + curr_vars_number);
	  IR_set_proc_expr
	    (stmt, third_expr_processing (IR_proc_expr (stmt), FALSE,
					  NULL, &temp_vars_num, FALSE));
	  process_actuals (IR_proc_actuals (stmt), &temp_vars_num);
	  SET_PC (stmt);
	  break;  
	case IR_NM_if_stmt:
	  {
	    IR_node_t if_finish;
	    pc_t if_part_begin_pc, saved_prev_pc;
	    
	    result = -1;
	    IR_set_if_expr
	      (stmt, third_expr_processing (IR_if_expr (stmt), FALSE,
					    &result, &temp_vars_num, FALSE));
	    IR_set_if_cond_num (stmt, result);
	    type_test (IR_if_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_if_expr_type);
	    saved_prev_pc = curr_pc;
	    SET_PC (stmt);
	    if_finish = create_node_with_pos (IR_NM_if_finish,
					      source_position);
	    IR_set_if_common_part_pc (stmt, PC (if_finish));
	    third_block_passing (IR_if_part (stmt));
	    
	    SET_PC (if_finish);
	    if_part_begin_pc = IR_next_pc (stmt);
	    prev_pc = saved_prev_pc;
	    curr_pc = PC (stmt);
	    third_block_passing (IR_else_part (stmt));
	    IR_set_pos (if_finish, source_position);
	    SET_PC (if_finish);
	    IR_set_else_part_pc (stmt, IR_next_pc (stmt));
	    IR_set_next_pc (stmt, if_part_begin_pc);
	    break;
	  }
	case IR_NM_for_stmt:
	  {
	    pc_t before_guard_expr;
	    pc_t for_iterate_start, for_iterate_finish;
	    pc_t saved_start_next_iteration_pc, saved_for_finish;
	    int saved_number_of_surrounding_blocks;
	    
	    saved_number_of_surrounding_blocks = number_of_surrounding_blocks;
	    number_of_surrounding_blocks = 0;
	    saved_start_next_iteration_pc = start_next_iteration_pc;
	    saved_for_finish = for_finish;
	    third_block_passing (IR_for_initial_stmt (stmt));
	    d_assert (curr_pc != NULL);
	    before_guard_expr = curr_pc;
	    result = -1;
	    IR_set_for_guard_expr
	      (stmt, third_expr_processing (IR_for_guard_expr (stmt), FALSE,
					    &result, &temp_vars_num, FALSE));
	    IR_set_for_guard_num (stmt, result);
	    type_test (IR_for_guard_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_for_guard_expr_type);
	    SET_PC (stmt);
	    third_block_passing (IR_for_iterate_stmt (stmt));
	    for_iterate_start = IR_next_pc (stmt);
	    for_iterate_finish = curr_pc;
	    if (for_iterate_finish != PC (stmt))
	      {
		/* There is break (although it is semantically invalid). */
		if (for_iterate_finish != NULL) 
		  IR_set_next_pc (for_iterate_finish,
				  IR_next_pc (before_guard_expr));
		start_next_iteration_pc = for_iterate_start;
	      }
	    else
	      start_next_iteration_pc = IR_next_pc (before_guard_expr);
	    for_finish = create_node_with_pos (IR_NM_for_finish,
					       source_position);
	    curr_pc = PC (stmt);
	    third_block_passing (IR_for_stmts (stmt));
	    IR_set_pos (for_finish, source_position);
	    /* The following guard is needed because the last stmt
	       may be break or continue. */
	    if (curr_pc != NULL)
	      IR_set_next_pc (curr_pc, start_next_iteration_pc);
	    IR_set_for_body_pc (stmt, IR_next_pc (stmt));
	    curr_pc = PC (stmt);
	    SET_PC (for_finish);
	    number_of_surrounding_blocks = saved_number_of_surrounding_blocks;
	    start_next_iteration_pc = saved_start_next_iteration_pc;
	    for_finish = saved_for_finish;
	    break;
	  }
	case IR_NM_foreach_stmt:
	  {
	    IR_node_t foreach_start;
	    IR_node_t foreach_next_iteration;
	    pc_t before_in_expr;
	    pc_t saved_start_next_iteration_pc, saved_for_finish;
	    int saved_number_of_surrounding_blocks;
	    
	    saved_number_of_surrounding_blocks = number_of_surrounding_blocks;
	    number_of_surrounding_blocks = 0;
	    saved_start_next_iteration_pc = start_next_iteration_pc;
	    saved_for_finish = for_finish;
	    foreach_start = create_node_with_pos (IR_NM_foreach_start,
						  source_position);
	    SET_PC (foreach_start);
	    before_in_expr = curr_pc;
	    var_result = -1;
	    /* We need one stack slot for a flag set up by
	       foreach_start and foreach_next_iteration node
	       execution.  */
	    temp_vars_num++;
	    temp = third_expr_processing (IR_foreach_designator (stmt), FALSE,
					  &var_result, &temp_vars_num, TRUE);
	    if (temp != NULL)
	      IR_SET_MODE (temp,
			   make_designator_lvalue
			   (temp, ERR_non_variable_in_foreach, TRUE));
	    if (temp != NULL)
	      {
		IR_set_foreach_designator (stmt, temp);
		/* In order not to generate different foreach_stmt
		   corresponding to local non-local variables, we use
		   a general solution by representing local variables
		   by nodes lvalue_var_occurrence_and_val.  */
		get_lvalue_and_val_location (temp, &temp_vars_num,
					     &container_num, &index_num,
					     &lvalue_val_num);
		IR_set_foreach_container_num (stmt, container_num);
		IR_set_foreach_index_num (stmt, index_num);
		IR_set_foreach_lvalue_val_num (stmt, lvalue_val_num);
	      }
	    result = -1;
	    IR_set_foreach_table
	      (stmt, third_expr_processing (IR_foreach_table (stmt), FALSE,
					    &result, &temp_vars_num, FALSE));
	    IR_set_foreach_tab_num (stmt, result);
	    type_test (IR_foreach_table (stmt), EVT_TABLE,
		       ERR_invalid_foreach_table_type);
	    SET_PC (stmt);
	    foreach_next_iteration
	      = create_node_with_pos (IR_NM_foreach_next_iteration,
				      source_position);
	    IR_set_next_pc (foreach_next_iteration,
			    IR_next_pc (before_in_expr));
	    start_next_iteration_pc = PC (foreach_next_iteration);
	    for_finish = create_node_with_pos (IR_NM_for_finish,
					       source_position);
	    third_block_passing (IR_foreach_stmts (stmt));
	    IR_set_pos (for_finish, source_position);
	    /* The following guard is needed because the last stmt may
	       be break or continue. */
	    if (curr_pc != NULL)
	      IR_set_next_pc (curr_pc, start_next_iteration_pc);
	    IR_set_foreach_body_pc (stmt, IR_next_pc (stmt));
	    curr_pc = PC (stmt);
	    SET_PC (for_finish);
	    number_of_surrounding_blocks = saved_number_of_surrounding_blocks;
	    start_next_iteration_pc = saved_start_next_iteration_pc;
	    for_finish = saved_for_finish;
	    break;
	  }
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	  if (for_finish == NULL)
	    error (FALSE, source_position,
		   stmt_mode == IR_NM_continue_stmt
		   ? ERR_continue_is_not_in_loop : ERR_break_is_not_in_loop);
	  else
	    {
	      SET_PC (stmt);
	      IR_set_number_of_surrounding_blocks
		(stmt, number_of_surrounding_blocks);
	      if (stmt_mode == IR_NM_continue_stmt)
		SET_PC (IR_PTR (start_next_iteration_pc));
	      else
		SET_PC (for_finish);
	      curr_pc = NULL;
	    }
	  break;
	case IR_NM_return_without_result:
	case IR_NM_return_with_result:
	  {
	    IR_node_t func_class_ext;

	    func_class_ext = find_covered_func_class_ext (curr_scope);
	    if (func_class_ext == NULL)
	      error (FALSE, source_position,
		     ERR_return_outside_func_class_ext);
	    else if (stmt_mode == IR_NM_return_with_result)
	      {
		if (IR_IS_OF_TYPE (func_class_ext, IR_NM_class))
		  error (FALSE, source_position,
			 ERR_return_with_result_in_class);
		else if (IR_IS_OF_TYPE (func_class_ext, IR_NM_func)
			 && IR_thread_flag (func_class_ext))
		  error (FALSE, source_position,
			 ERR_return_with_result_in_thread);
		result = -1;
		IR_set_returned_expr
		  (stmt,
		   third_expr_processing (IR_returned_expr (stmt), TRUE,
					  &result, &temp_vars_num, FALSE));
		add_flatten_node_if_necessary (IR_returned_expr (stmt), result);
		IR_set_return_expr_num (stmt, result);
	      }
	    SET_PC (stmt);
	    break;
	  }
	case IR_NM_wait_stmt:
	  {
	    pc_t before_wait_guard_expr, wait_finish;
	    
	    before_wait_guard_expr = curr_pc;
	    there_is_function_call_in_expr = FALSE;
	    result = -1;
	    IR_set_wait_guard_expr
	      (stmt, third_expr_processing (IR_wait_guard_expr (stmt), FALSE,
					    &result, &temp_vars_num, FALSE));
	    IR_set_wait_guard_expr_num (stmt, result);
	    type_test (IR_wait_guard_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_wait_guard_expr_type);
	    if (there_is_function_call_in_expr)
	      error (FALSE, source_position, ERR_function_call_in_wait_stmt);
	    IR_set_start_wait_guard_expr_pc
	      (stmt, IR_next_pc (IR_PTR (before_wait_guard_expr)));
	    SET_PC (stmt);
	    wait_finish = create_node_with_pos (IR_NM_wait_finish,
						source_position);
	    third_block_passing (IR_wait_stmt (stmt));
	    IR_set_pos (wait_finish, source_position);
	    SET_PC (wait_finish);
	    break;
	  }
	case IR_NM_throw:
	  IR_set_throw_expr_num (stmt, temp_vars_num + curr_vars_number);
	  IR_set_throw_expr
	    (stmt, third_expr_processing (IR_throw_expr (stmt), FALSE,
					  NULL, &temp_vars_num, FALSE));
	  type_test (IR_throw_expr (stmt), EVT_UNKNOWN,
		     ERR_invalid_throw_expr_type);
	  SET_PC (stmt);
	  break;
	case IR_NM_block:
	  {
	    IR_node_t saved_curr_scope = curr_scope;
	    IR_node_t saved_curr_real_scope = curr_real_scope;
	    int saved_curr_vars_number = curr_vars_number;
	    IR_node_t curr_except;
	    IR_node_t last_except_with_block;
	    pc_t saved_curr_pc = curr_pc;
	    pc_t block_finish;
	    pc_t catches_finish;
	    pc_t previous_node_catch_list_pc;
	    IR_node_t func_class;

	    curr_scope = stmt;
	    if ((func_class = IR_func_class_ext (stmt)) != NULL)
	      curr_pc = PC (stmt);
	    else
	      SET_PC (stmt);
	    if (! IR_simple_block_flag (stmt))
	      {
 		curr_real_scope = stmt;
		curr_vars_number = real_block_vars_number (stmt);
		number_of_surrounding_blocks++;
	      }
	    third_block_passing (IR_block_stmts (stmt));
	    curr_scope = saved_curr_scope;
	    if (! IR_simple_block_flag (stmt))
	      {
		number_of_surrounding_blocks--;
		curr_real_scope = saved_curr_real_scope;
		curr_vars_number = saved_curr_vars_number;
	      }
	    block_finish = PC (create_node_with_pos (IR_NM_block_finish,
						     source_position));
	    IR_set_block (IR_PTR (block_finish), stmt);
	    IR_set_simple_block_finish_flag (IR_PTR (block_finish),
					     IR_simple_block_flag (stmt));
	    SET_PC (block_finish);
	    if (func_class != NULL && IR_IS_OF_TYPE (func_class, IR_NM_class)
		&& IR_IS_OF_TYPE (IR_PTR (IR_next_pc (PC (stmt))),
				  IR_NM_block_finish))
	      IR_set_simple_class_flag (func_class, TRUE);
	    IR_set_catch_list_pc (stmt, NULL);
	    if (func_class != NULL)
	      curr_pc = saved_curr_pc;
	    else if (IR_exceptions (stmt) != NULL)
	      {
		catches_finish = PC (create_node_with_pos
				     (IR_NM_catches_finish, source_position));
		previous_node_catch_list_pc = PC (stmt); /* block */
		SET_PC (catches_finish);
		last_except_with_block = NULL;
		for (curr_except = IR_exceptions (stmt);
		     curr_except != NULL;
		     curr_except = IR_next_exception (curr_except))
		  {
		    d_assert (curr_pc == catches_finish);
		    d_assert (IR_IS_OF_TYPE (previous_node_catch_list_pc,
					     IR_NM_block)
			      || IR_IS_OF_TYPE (previous_node_catch_list_pc,
						IR_NM_exception));
		    result = -1;
		    temp_vars_num = 0;
		    /* The exception instance in the first local
		       temporary variable.  */
		    IR_set_except_instance_num
		      (curr_except,
		       setup_result_var_number (NULL, &temp_vars_num));
		    IR_set_exception_class_expr
		      (curr_except,
		       third_expr_processing
		       (IR_exception_class_expr (curr_except),
			FALSE, &result, &temp_vars_num, FALSE));
		    IR_set_except_class_num (curr_except, result);
		    type_test (IR_exception_class_expr (curr_except),
			       EVT_CLASS, ERR_invalid_catch_expr_type);
		    if (IR_IS_OF_TYPE (previous_node_catch_list_pc,
				       IR_NM_block))
		      IR_set_catch_list_pc (previous_node_catch_list_pc,
					    IR_next_pc (catches_finish));
		    else
		      IR_set_next_catch_list_pc (previous_node_catch_list_pc,
						 IR_next_pc (catches_finish));
		    SET_PC (curr_except);
		    previous_node_catch_list_pc = curr_pc;
		    if (IR_catch_block (curr_except) != NULL)
		      {
			last_except_with_block = curr_except;
			third_block_passing (IR_catch_block (curr_except));
			SET_PC (catches_finish);
		      }
		    else
		      {
			d_assert (last_except_with_block != NULL);
			IR_set_next_pc (curr_except,
					IR_next_pc (last_except_with_block));
			curr_pc = catches_finish;
		      }
		  }
		d_assert (curr_pc == catches_finish);
		IR_set_next_pc (catches_finish, NULL);
	      }
	    if (!IR_simple_block_flag (stmt))
	      IR_set_temporary_vars_number
		(stmt,
		 IR_temporary_vars_number (stmt) + ADD_TEMP_VARS_NUMBER);
	    break;
	  }
	case IR_NM_var:
	case IR_NM_external_var:
	case IR_NM_external_func:
	case IR_NM_func:
	case IR_NM_class:
	  {
	    IR_node_t block;
	    
	    block = IR_scope (stmt);
	    d_assert (block != NULL && IR_NODE_MODE (block) == IR_NM_block);
	    define_block_decl (stmt, block);
	    if (stmt_mode == IR_NM_class
		&& (block = surrounding_func_block (block)) != NULL)
	      /* Objects created from the class inside a function will
		 need the function context.  */
	      IR_set_extended_life_context_flag (block, TRUE);
	    break;
	  }
	default:
	  d_unreachable ();
	}
    }
}

/* The following function passes through nodes unnecessary for
   execution. */
static IR_node_t
go_through (IR_node_t pc)
{
  while (pc != NULL)
    switch (IR_NODE_MODE (pc))
      {
      case IR_NM_cond_end:
      case IR_NM_par_assign_end:
      case IR_NM_if_finish:
      case IR_NM_for_finish:
      case IR_NM_catches_finish:
        pc = IR_next_pc (pc);
        break;
      case IR_NM_block:
        if (!IR_simple_block_flag (pc))
          return pc;
        pc = IR_next_pc (pc);
        break;
      case IR_NM_block_finish:
        if (!IR_simple_block_finish_flag (pc))
          return pc;
        pc = IR_next_pc (pc);
        break;
      default:
        return pc;
      }
  return pc;
}

/* The following recursive function removes nodes unnecessary for
   execution. */
static void
fourth_block_passing (IR_node_t first_level_stmt)
{
  IR_node_t stmt;

  for (stmt = first_level_stmt; stmt != NULL; stmt = IR_next_pc (stmt))
    {
      if (IR_traverse_flag (stmt))
	return;
      IR_set_traverse_flag (stmt, TRUE);
      switch (IR_NODE_MODE (stmt))
	{
	case IR_NM_if_stmt:
          IR_set_else_part_pc (stmt, go_through (IR_else_part_pc (stmt)));
	  fourth_block_passing (IR_else_part_pc (stmt));
          IR_set_if_common_part_pc (stmt, go_through (IR_if_common_part_pc (stmt)));
          break;
	case IR_NM_cond:
          IR_set_false_path_pc (stmt, go_through (IR_false_path_pc (stmt)));
	  fourth_block_passing (IR_false_path_pc (stmt));
          IR_set_cond_common_part_pc (stmt, go_through (IR_cond_common_part_pc (stmt)));
          break;
	case IR_NM_par_assign_test:
          IR_set_skip_par_assign_path_pc
	    (stmt, go_through (IR_skip_par_assign_path_pc (stmt)));
          break;
	case IR_NM_for_stmt:
          IR_set_for_body_pc (stmt, go_through (IR_for_body_pc (stmt)));
	  fourth_block_passing (IR_for_body_pc (stmt));
          break;
	case IR_NM_foreach_stmt:
          IR_set_foreach_body_pc (stmt,
				  go_through (IR_foreach_body_pc (stmt)));
	  fourth_block_passing (IR_foreach_body_pc (stmt));
          break;
	case IR_NM_wait_stmt:
          IR_set_start_wait_guard_expr_pc
            (stmt, go_through (IR_start_wait_guard_expr_pc (stmt)));
	  fourth_block_passing (IR_start_wait_guard_expr_pc (stmt));
          break;
	case IR_NM_block:
	  {
	    IR_node_t curr_except;
	    IR_node_t curr_stmt;

            IR_set_catch_list_pc (stmt, go_through (IR_catch_list_pc (stmt)));
	    fourth_block_passing (IR_catch_list_pc (stmt));
            for (curr_except = IR_exceptions (stmt);
                 curr_except != NULL;
                 curr_except = IR_next_exception (curr_except))
              {
                IR_set_next_catch_list_pc
                  (curr_except,
		   go_through (IR_next_catch_list_pc (curr_except)));
		fourth_block_passing (IR_next_catch_list_pc (curr_except));
	      }
	    /* Classes and functions are unreachable by the pc chain.  */
	    for (curr_stmt = IR_block_stmts (stmt);
		 curr_stmt != NULL;
		 curr_stmt = IR_next_stmt (curr_stmt))
	      if (IR_IS_OF_TYPE (curr_stmt, IR_NM_func_or_class))
		fourth_block_passing (IR_next_stmt (curr_stmt));
	    break;
	  }
	default:
	  /* Do nothing */
	  break;
	}
      IR_set_next_pc (stmt, go_through (IR_next_pc (stmt)));
    }
}

/* Print INDENT spaces into stdout.  */
static void
print_indent (int indent)
{
  int i;

  for (i = 0; i < indent; i++)
    printf (" ");
}

/* Info about node used to dump the program code.  */
struct node_info
{
  IR_node_t n; /* node itself */
  bool_t traverse_flag; /* true if it was printed already */
  int_t num; /* node number used for printing */
};


/* Temporary structure. */
static struct node_info node_info;

/* Hash table which implements the node info table. */
static hash_table_t node_info_tab;
/* This object stack contains elements of the table. */
static os_t node_info_os;

/* The last number used to enumerate the nodes.  */
static int curr_node_num;

/* Hash of the node info. */
static unsigned
node_info_hash (hash_table_entry_t n)
{
  struct node_info *ni = ((struct node_info *) n);

  return (size_t) ni->n;
}

/* Equality of nodes. */
static int
node_info_eq (hash_table_entry_t n1, hash_table_entry_t n2)
{
  struct node_info *ni1 = ((struct node_info *) n1);
  struct node_info *ni2 = ((struct node_info *) n2);

  return ni1->n == ni2->n;
}

/* Return the node info of node N.  Create and initialize if it is not
   created yet.  */
static struct node_info *
find_node_info (IR_node_t n)
{
  hash_table_entry_t *entry;
  struct node_info *ni;

  node_info.n = n;
  entry = find_hash_table_entry (node_info_tab, &node_info, TRUE);
  if (*entry != NULL)
    return *(struct node_info **) entry;
  OS_TOP_EXPAND (node_info_os, sizeof (struct node_info));
  ni = OS_TOP_BEGIN (node_info_os);
  OS_TOP_FINISH (node_info_os);
  ni->n = n;
  ni->traverse_flag = FALSE;
  ni->num = ++curr_node_num;
  *entry = ni;
  return ni;
}

/* Create the node info table. */
static void
initiate_node_tab (void)
{
  OS_CREATE (node_info_os, 0);
  node_info_tab = create_hash_table (400, node_info_hash, node_info_eq);
  curr_node_num = 0;
}

/* Delete the node info table. */
static void
finish_node_tab (void)
{
  delete_hash_table (node_info_tab);
  OS_DELETE (node_info_os);
}

/* True if a goto was last printed. */
static bool_t last_goto_flag;
/* The line number of the last printed node. */
static unsigned curr_line_number = 0;

/* Print code from node CN to STOP (not including it) using
   INDENT.  */
static void
dump_code (int indent, IR_node_t cn, IR_node_t stop)
{
  IR_node_mode_t node_mode;
  IR_node_t cl, cf = NULL;
  struct node_info *ni;
  bool_t line_flag;

  for (; cn != stop;)
    {
      ni = find_node_info (cn);
      if (ni->traverse_flag)
	{
	  if (! last_goto_flag)
	    {
	      print_indent (indent);
	      printf ("       goto %d\n", ni->num);
	    }
	  last_goto_flag = TRUE;
	  break;
	}
      ni->traverse_flag = TRUE;
      node_mode = IR_NODE_MODE (cn);
      if ((node_mode != IR_NM_block || indent != 0)
	  && dump_flag == 1
	  && strcmp (IR_pos (cn).file_name, ENVIRONMENT_PSEUDO_FILE_NAME) == 0)
	{
	  cn = IR_next_pc (cn);
	  continue;
	}
      if (node_mode == IR_NM_block_finish)
	indent -= 2;
      print_indent (indent);
      printf ("%6d %s", ni->num, IR_node_name[node_mode]);
      last_goto_flag = FALSE;
      d_assert (indent >= 0);
      if (curr_line_number != IR_pos (cn).line_number)
	{
	  curr_line_number = IR_pos (cn).line_number;
	  printf ("(line=%u)", curr_line_number);
	}
      line_flag = TRUE;
      switch (node_mode)
	{
	case IR_NM_char:
	  printf (": %d <- ", IR_char_result_num (cn));
	  printf ("(%x)", IR_ch_val (cn));
	  if (isgraph (IR_ch_val (cn)))
	    printf ("(%c -- %x)", IR_ch_val (cn), IR_ch_val (cn));
	  else
	    printf ("(%x)", IR_ch_val (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_int:
	  printf (": %d <- ", IR_int_result_num (cn));
	  printf ("(%d)", IR_i_val (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_float:
	  printf (": %d <- ", IR_float_result_num (cn));
	  printf ("(%g)", IR_f_val (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_hide_type:
	case IR_NM_hideblock_type:
	case IR_NM_char_type:
	case IR_NM_int_type:
	case IR_NM_float_type:
	case IR_NM_vector_type:
	case IR_NM_table_type:
	case IR_NM_func_type:
	case IR_NM_thread_type:
	case IR_NM_class_type:
	case IR_NM_stack_type:
	case IR_NM_process_type:
	case IR_NM_instance_type:
	case IR_NM_type_type:
	  printf (": %d <- ", IR_type_result_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_nil:
	  printf (": %d <- ", IR_nil_result_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_string:
	  {
	    const char *s;

	    printf (": %d <- ", IR_string_result_num (cn));
	    printf ("(\"");
	    for (s = IR_str_val (cn); *s != '\0'; s++)
	      printf ("%s", get_ch_repr (*s));
	    printf ("\")");
	    cn = IR_next_pc (cn);
	    break;
	  }
	case IR_NM_period:
	case IR_NM_lvalue_period:
	case IR_NM_lvalue_period_and_val:
	  printf (": %d <- %d (%s, block_decl_ident_num=%d)",
		  IR_designator_result_num (cn),
		  IR_designator_op_num (cn),
		  IR_ident_string (IR_unique_ident (IR_component (cn))),
 		  IR_right_block_decl_ident_number (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_logical_or:
	case IR_NM_logical_and:
	  printf (": %d <- %d (short part pc = %d)",
		  IR_result_num (cn), IR_op_num (cn),
 		  find_node_info (IR_short_path_pc (cn))->num);
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_logical_or_end:
	case IR_NM_logical_and_end:
	  printf (": %d <- %d",
		  IR_cont_result_num (cn), IR_cont_op_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_not:
	case IR_NM_bitwise_not:
	case IR_NM_unary_plus:
	case IR_NM_unary_minus:
	case IR_NM_length:
	case IR_NM_fold_plus:
	case IR_NM_fold_mult:
	case IR_NM_fold_and:
	case IR_NM_fold_xor:
	case IR_NM_fold_or:
	case IR_NM_const:
	case IR_NM_new:
	case IR_NM_typeof:
	case IR_NM_charof:
	case IR_NM_intof:
	case IR_NM_floatof:
	case IR_NM_vectorof:
	case IR_NM_tableof:
	case IR_NM_funcof:
	case IR_NM_threadof:
	case IR_NM_classof:
	  printf (": %d <- %d", IR_result_num (cn), IR_op_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_vector:
	case IR_NM_table:
	  printf (": %d <- (els=%d, parts=%d)",
		  IR_vec_tab_result_num (cn), IR_vec_tab_el_num (cn),
		  IR_parts_number (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_flatten:
	  printf (": %d", IR_flatten_vect_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_class_func_thread_call:
	  printf (": call (start=%d) (npars=%d)", IR_func_call_start_num (cn),
		  IR_class_func_thread_call_parameters_number (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_index:
	case IR_NM_lvalue_index_and_val:
	  /* lvalue_index should be not here.  */
	  printf (": %d <- %d[%d]", IR_designator_result_num (cn),
		  IR_designator_op_num (cn), IR_component_op_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_slice:
	case IR_NM_lvalue_slice_and_val:
	  /* lvalue_slice should be not here.  */
	  printf (": %d <- %d (dim=%d)", IR_designator_result_num (cn),
		  IR_designator_op_num (cn), IR_dim (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_key_index:
	case IR_NM_lvalue_key_index_and_val:
	  /* lvalue_key_index should be not here.  */
	  printf (": %d <- %d{%d}", IR_designator_result_num (cn),
		  IR_designator_op_num (cn), IR_component_op_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_in:
	case IR_NM_eq:
	case IR_NM_ne:
	case IR_NM_identity:
	case IR_NM_unidentity:
	case IR_NM_lt:
	case IR_NM_ge:
	case IR_NM_gt:
	case IR_NM_le:
	case IR_NM_mult:
	case IR_NM_div:
	case IR_NM_mod:
	case IR_NM_plus:
	case IR_NM_minus:
	case IR_NM_concat:
	case IR_NM_lshift:
	case IR_NM_rshift:
	case IR_NM_ashift:
	case IR_NM_and:
	case IR_NM_xor:
	case IR_NM_or:
	case IR_NM_format_vectorof:
	  printf (": %d <- %d %d",
		  IR_result_num (cn), IR_left_op_num (cn), IR_right_op_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_mod_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	case IR_NM_plus_assign:
	  printf (": %d(%d) <- %d op %d", IR_container_num (cn), IR_index_num (cn),
		  IR_lvalue_val_num (cn), IR_expr_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_assign:
	case IR_NM_var_assign:
	case IR_NM_par_assign:
	  printf (": %d(%d) <- %d", IR_container_num (cn), IR_index_num (cn),
		  IR_expr_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_slice_assign:
	  printf (": %d(dim=%d) <- %d", IR_container_num (cn), IR_index_num (cn),
		  IR_expr_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_mult_slice_assign:
	case IR_NM_div_slice_assign:
	case IR_NM_mod_slice_assign:
	case IR_NM_minus_slice_assign:
	case IR_NM_concat_slice_assign:
	case IR_NM_lshift_slice_assign:
	case IR_NM_rshift_slice_assign:
	case IR_NM_ashift_slice_assign:
	case IR_NM_and_slice_assign:
	case IR_NM_xor_slice_assign:
	case IR_NM_or_slice_assign:
	case IR_NM_plus_slice_assign:
	  printf (": %d(dim=%d) <- %d op %d", IR_container_num (cn),
		  IR_index_num (cn), IR_lvalue_val_num (cn), IR_expr_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_par_assign_test:
	  printf (": %d? (skip=%d)", IR_par_num (cn),
		  find_node_info (IR_skip_par_assign_path_pc (cn))->num);
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_proc_call:
	  printf (": call (start=%d) (pars=%d)",
		  IR_proc_call_start_num (cn), IR_proc_call_pars_number (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_if_stmt:
	case IR_NM_cond:
	  {
	    IR_node_t cp;

	    indent += 2;
	    if (node_mode == IR_NM_if_stmt)
	      {
		printf (": %d (else=%d)\n", IR_if_cond_num (cn),
			find_node_info (IR_else_part_pc (cn))->num);
		cp = IR_if_common_part_pc (cn);
	      }
	    else
	      {
		printf (": %d (else=%d)\n", IR_cond_op_num (cn),
			find_node_info (IR_false_path_pc (cn))->num);
		cp = IR_cond_common_part_pc (cn);
	      }
	    find_node_info (cp)->traverse_flag = TRUE;
	    dump_code (indent, IR_next_pc (cn), NULL);
	    find_node_info (cp)->traverse_flag = FALSE;
	    if (node_mode == IR_NM_if_stmt)
	      dump_code (indent, IR_else_part_pc (cn), cp);
	    else
	      dump_code (indent, IR_false_path_pc (cn), cp);
	    line_flag = FALSE;
	    indent -= 2;
	    cn = cp;
	    break;
	  }
	case IR_NM_for_stmt:
	  indent += 2;
	  printf (": %d (end=%d)\n", IR_for_guard_num (cn),
		  find_node_info (IR_next_pc (cn))->num);
	  dump_code (indent, IR_for_body_pc (cn), NULL);
	  line_flag = FALSE;
	  indent -= 2;
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_foreach_start:
	  indent += 2;
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_foreach_next_iteration:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_foreach_stmt:
	  printf (": %d(%d) (val=%d) in %d (end=%d)\n",
		  IR_foreach_container_num (cn), IR_foreach_index_num (cn),
		  IR_foreach_lvalue_val_num (cn), IR_foreach_tab_num (cn),
		  find_node_info (IR_next_pc (cn))->num);
	  dump_code (indent, IR_foreach_body_pc (cn), NULL);
	  line_flag = FALSE;
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_block_finish:
	  d_assert (! IR_simple_block_finish_flag (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_return_with_result:
	  printf (": %d", IR_return_expr_num (cn));
	case IR_NM_return_without_result:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_wait_stmt:
	  printf (": %d (guard_expr=%d)", IR_wait_guard_expr_num (cn),
		  find_node_info (IR_start_wait_guard_expr_pc (cn))->num);
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_wait_finish:
          cn = IR_next_pc (cn);
	  break;
	case IR_NM_block:
	  {
	    IR_node_t stmt;
	    bool_t first_p = TRUE;

	    d_assert (! IR_simple_block_flag (cn));
	    printf (": vars=%d, tvars=%d", real_block_vars_number (cn),
		    IR_temporary_vars_number (cn));
	    indent += 2;
	    for (stmt = IR_block_stmts (cn);
		 stmt != NULL;
		 stmt = IR_next_stmt (stmt))
	      if (IR_IS_OF_TYPE (stmt, IR_NM_func_or_class))
		{
		  if (first_p)
		    {
		      printf ("\n");
		      first_p = FALSE;
		    }
		  dump_code (indent, stmt, NULL);
		}
	    if ((cl = IR_catch_list_pc (cn)) == NULL)
	      cn = IR_next_pc (cn);
	    else
	      {
		IR_node_t fn;
		
		for (fn = IR_next_pc (cn); fn != NULL; fn = IR_next_pc (fn))
		  if (IR_NODE_MODE (fn) == IR_NM_block_finish && IR_block (fn) == cn)
		    break;
		d_assert (fn != NULL);
		printf ("(catch = %d)\n", find_node_info (cl)->num);
		cf = IR_next_pc (fn);
		dump_code (indent, IR_next_pc (cn), cf);
		line_flag = FALSE;
		cn = cl;
	      }
	    break;
	  }
	case IR_NM_throw:
	  printf (": %d", IR_throw_expr_num (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_exception:
	  printf (": %d inside %d ",
		  IR_except_instance_num (cn), IR_except_class_num (cn));
	  if ((cl = IR_next_catch_list_pc (cn)) == NULL)
	    {
	      cn = IR_next_pc (cn);
	      cf = NULL;
	    }
	  else
	    {
	      printf ("(catch = %d)\n", find_node_info (cl)->num);
	      dump_code (indent, IR_next_pc (cn), cf);
	      line_flag = FALSE;
	      cn = cl;
	    }
	  break;
	case IR_NM_var_occurrence:
	case IR_NM_lvalue_var_occurrence:
	case IR_NM_lvalue_var_occurrence_and_val:
	case IR_NM_external_var_occurrence:
	case IR_NM_lvalue_external_var_occurrence:
	case IR_NM_lvalue_external_var_occurrence_and_val:
	case IR_NM_external_func_occurrence:
	case IR_NM_func_occurrence:
	case IR_NM_class_occurrence:
	  printf (": %d <- (%s)", IR_occurrence_result_num (cn),
		  IR_ident_string (IR_unique_ident (IR_ident (IR_decl (cn)))));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_local_var_occurrence:
	  printf (": %d <- %d (%s)", IR_occurrence_result_num (cn),
		  IR_local_var_num (cn),
		  IR_ident_string (IR_unique_ident (IR_ident (IR_decl (cn)))));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_func:
	case IR_NM_class:
	  printf (" %s (%d%s)",
		  IR_ident_string (IR_unique_ident (IR_ident (cn))),
		  IR_parameters_number (cn), IR_args_flag (cn) ? ", ..." : "");
	  cn = IR_next_stmt (cn);
	  break;
	default:
	  /* Other nodes should not occur here.  */
	  d_unreachable ();
	}
      if (line_flag)
	printf ("\n");
    }
}

pc_t
test_context (IR_node_t first_program_stmt_ptr)
{
  IR_node_t *curr_ext_ptr;
  IR_node_t decl;

  curr_scope = NULL;
  VLO_CREATE (possible_table_extensions, 0);
  process_block_decl_unique_ident (destroy_unique_ident);
  first_program_stmt_ptr = first_block_passing (first_program_stmt_ptr, 0);
  /* Check that there are no extensions in the table. */
  for (curr_ext_ptr = (IR_node_t *) VLO_BEGIN (possible_table_extensions);
       (char *) curr_ext_ptr <= (char *) VLO_END (possible_table_extensions);
       curr_ext_ptr++)
    {
      decl = *(IR_node_t *) find_table_entry (*curr_ext_ptr, FALSE);
      if (decl != NULL && IR_IS_OF_TYPE (decl, IR_NM_ext))
	error (FALSE, IR_pos (IR_ident (*curr_ext_ptr)),
	       ERR_extension_without_class_or_func,
	       IR_ident_string (IR_unique_ident (IR_ident (*curr_ext_ptr))));
    }
  VLO_DELETE (possible_table_extensions);
  /* first_block_passing include declarations into table.  The
     environment scope is the first stmt (which is always block). */
  set_environment_declarations (first_program_stmt_ptr);
  curr_scope = curr_real_scope = NULL;
  second_block_passing (first_program_stmt_ptr);
  curr_scope = NULL;
  prev_pc = curr_pc = NULL;
  for_finish = NULL;
  third_block_passing (first_program_stmt_ptr);
  /* Some optimizations */
  fourth_block_passing (first_program_stmt_ptr);
  if (number_of_errors == 0 && dump_flag)
    {
      initiate_node_tab ();
      last_goto_flag = FALSE;
      dump_code (0, first_program_stmt_ptr, NULL);
      finish_node_tab ();
    }
  /* Remember that first program stmt is always block (see grammar
     description on yacc). */
  return PC (first_program_stmt_ptr);
}
