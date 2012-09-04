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

#include "d_common.h"
#include "d_ir.h"
#include "d_runtab.h"
#include "d_context.h"

/* Pointer to block node which opens current scope. */
static IR_node_t current_scope;

/* Current program counter.  This value is used to connect
   commands for interpreter (see comments for typedef pc_t). */
static pc_t current_pc;

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

/* The following recursive func passes (correctly setting up
   SOURCE_POSITION) EXPR (it may be NULL) and processes idents in
   operations PERIOD.  See also commentaries for abstract data
   block_decl_idents_tables.  The func may modify member
   temporary_vars_number in surrounding block.  The func also sets up
   members parts_number and class_func_thread_call_parameters_number
   in vector node (table node) and class_func_thread_call node. It is
   considered that evaluation order is left-to-right.
   CURRENT_TEMP_VARS_NUMBER is current number of temporary vars which
   are in stack now. */
static void
first_expr_processing (IR_node_t expr, int current_temp_vars_number)
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
      current_temp_vars_number++;
      if (current_temp_vars_number > IR_temporary_vars_number (current_scope))
	IR_set_temporary_vars_number (current_scope, current_temp_vars_number);
      break;
    case IR_NM_ident:
      {
	IR_node_t ident;

	current_temp_vars_number++;
	ident = expr;
	expr = find_decl (expr, current_scope);
	if (expr != NULL && IR_scope (expr) != NULL)
	  process_block_decl_unique_ident (IR_unique_ident (ident));
	if (current_temp_vars_number
	    > IR_temporary_vars_number (current_scope))
	  IR_set_temporary_vars_number (current_scope,
					current_temp_vars_number);
	break;
      }
    case IR_NM_period:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_left_operand (expr), current_temp_vars_number);
      if (IR_right_operand (expr) != NULL)
	{
	  process_block_decl_unique_ident
	    (IR_unique_ident (IR_right_operand (expr)));
	  IR_set_right_block_decl_ident_number
	    (expr,
	     IR_block_decl_ident_number (IR_unique_ident
					 (IR_right_operand (expr))));
	}
      break;
    case IR_NM_logical_or:
    case IR_NM_logical_and:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_left_operand (expr), current_temp_vars_number);
      first_expr_processing (IR_right_operand (expr),
			     current_temp_vars_number);
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
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_left_operand (expr),
			     current_temp_vars_number++);
      first_expr_processing (IR_right_operand (expr),
			     current_temp_vars_number);
      break;
    case IR_NM_unary_plus:
    case IR_NM_unary_minus:
    case IR_NM_length:
    case IR_NM_bitwise_not:
    case IR_NM_not:
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
      first_expr_processing (IR_operand (expr), current_temp_vars_number);
      break;
    case IR_NM_format_vectorof:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_operand (expr), current_temp_vars_number++);
      first_expr_processing (IR_format (expr), current_temp_vars_number);
      break;
    case IR_NM_cond:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_cond (expr), current_temp_vars_number);
      first_expr_processing (IR_left_operand (expr), current_temp_vars_number);
      first_expr_processing (IR_right_operand (expr),
			     current_temp_vars_number);
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
	    first_expr_processing (IR_repetition_key (elist),
				   current_temp_vars_number++);
	    first_expr_processing (IR_expr (elist),
				   current_temp_vars_number++);
	  }
	IR_set_parts_number (expr, elements_number);
	break;
      }
    case IR_NM_index:
    case IR_NM_key_index:
      SET_SOURCE_POSITION (expr);
      /* Two stack entries are necessary for reference. */
      first_expr_processing (IR_left_operand (expr),
			     current_temp_vars_number++);
      first_expr_processing (IR_right_operand (expr),
			     current_temp_vars_number++);
    break;
  case IR_NM_class_func_thread_call:
    SET_SOURCE_POSITION (expr);
    {
      IR_node_t elist;
      int parameters_number;
      
      first_expr_processing (IR_func_expr (expr),
			     current_temp_vars_number++);
      for (parameters_number = 0, elist = IR_actuals (expr); elist != NULL;
           elist = IR_next_elist (elist))
	{
	  parameters_number++;
	  SET_SOURCE_POSITION (elist);
	  assert (IR_repetition_key (elist) == NULL);
	  first_expr_processing (IR_expr (elist), current_temp_vars_number++);
	}
      IR_set_class_func_thread_call_parameters_number (expr,
						       parameters_number);
      break;
    }
  default:
    assert (FALSE);
  }
}

/* This func includes DECL into the hash table.  The funcs
   also fixes repeated decl error.  In this case the func returns
   FALSE.  Otherwise the func returns TRUE, sets up
   decl_number_in_block and increases decls_number of the block
   in which the decl is immediately placed. */
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
      if (!IR_IS_OF_TYPE (decl, IR_NM_ext))
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
				      int current_block_level);

/* The function moving EXT_BLOCK stmts into the corresponding
   ORIGIN_BLOCK with possible processing when
   PROCESS_EXT_BLOCK_FLAG. */
static void
process_extension_block (IR_node_t ext_block, IR_node_t origin_block,
			 int process_ext_block_flag)
{
  IR_node_t curr_stmt;
  IR_node_t saved_current_scope;
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
      saved_current_scope = current_scope;
      current_scope = origin_block;
      first_stmt = first_block_passing (first_stmt,
					IR_block_level (origin_block) + 1);
      current_scope = saved_current_scope;
    }
  else if (IR_temporary_vars_number (origin_block)
	   < IR_temporary_vars_number (ext_block))
    IR_set_temporary_vars_number (origin_block,
				  IR_temporary_vars_number (ext_block));
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

/* Number of additional variables for different purpose. */

#define START_TEMP_VARS_NUMBER 2

/* This recursive func passes all stmts and exprs (correctly
   setting up SOURCE_POSITION and current_scope (before first call of
   the func current_scope is to be NULL)) on the same
   stmt nesting level, includes decls into the hash table, sets up
   decl block_level of any block node, sets up
   decl_number_in_block, increases decls_number and
   vars_number of the block in which the decl (var) is
   immediately placed, processes idents in operations PERIOD
   create class decls idents table for new class decl
   (see also commentaries for corresponding abstract data) and sets up
   member procedure_call_pars_number in procedure_call node.
   The funcs also fixes repeated decl error.
   FIRST_LEVEL_STMT (it may be NULL) is first stmt of the processed
   stmt nesting level.  CURRENT_BLOCK_LEVEL is number of blocks which
   contain given stmt list.
   The function also removes extensions (moving its statements into
   the extended func or class) and their blocks from the statement
   list.  Therefore the function returns new first_level_stmt which is
   different from the original one oly if an extension is the first in
   the list. */
static IR_node_t
first_block_passing (IR_node_t first_level_stmt, int current_block_level)
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
	  first_expr_processing (IR_assignment_var (stmt),
				 START_TEMP_VARS_NUMBER);
	  /* Two stack entries are necessary for reference
             represenatation. */
	  first_expr_processing (IR_assignment_expr (stmt),
				 START_TEMP_VARS_NUMBER + 2);
	  break;
	case IR_NM_swap:
	  first_expr_processing (IR_swap_var1 (stmt),
				 START_TEMP_VARS_NUMBER);
	  /* Two stack entries are necessary for reference
             represenatation2. */
	  first_expr_processing (IR_swap_var2 (stmt),
				 START_TEMP_VARS_NUMBER + 2);
	  break;
	  
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_rem_assign:
	case IR_NM_plus_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	  /* We need additional temp stack entry for reference for
             transformation a += e into a = a + e. */
	  first_expr_processing (IR_assignment_var (stmt),
				 START_TEMP_VARS_NUMBER);
	  first_expr_processing (IR_assignment_expr (stmt),
				 START_TEMP_VARS_NUMBER + 4);
	  break;
	case IR_NM_procedure_call:
	  {
	    IR_node_t elist;
	    int temporary_vars_number;
	    int parameters_number;
	    
	    temporary_vars_number = START_TEMP_VARS_NUMBER;
	    first_expr_processing (IR_proc_expr (stmt),
				   temporary_vars_number++);
	    for (parameters_number = 0, elist = IR_proc_actuals (stmt);
		 elist != NULL; elist = IR_next_elist (elist))
	      {
		parameters_number++;
		SET_SOURCE_POSITION (elist);
		assert (IR_repetition_key (elist) == NULL);
		first_expr_processing (IR_expr (elist),
				       temporary_vars_number++);
	      }
	    IR_set_procedure_call_pars_number (stmt, parameters_number);
	    break;  
	  }
	case IR_NM_if_stmt:
	  first_expr_processing (IR_if_expr (stmt), START_TEMP_VARS_NUMBER);
	  IR_set_if_part
	    (stmt,
	     first_block_passing (IR_if_part (stmt), current_block_level));
	  IR_set_else_part
	    (stmt,
	     first_block_passing (IR_else_part (stmt), current_block_level));
	  break;
	case IR_NM_for_stmt:
	  IR_set_for_initial_stmt
	    (stmt, first_block_passing (IR_for_initial_stmt (stmt),
					current_block_level));
	  first_expr_processing (IR_for_guard_expr (stmt),
				 START_TEMP_VARS_NUMBER);
	  IR_set_for_iterate_stmt
	    (stmt, first_block_passing (IR_for_iterate_stmt (stmt),
					current_block_level));
	  IR_set_for_stmts
	    (stmt,
	     first_block_passing (IR_for_stmts (stmt), current_block_level));
	  break;
	case IR_NM_foreach_stmt:
	  /* Three stack entries are necessary for start flag and
             reference represenatation. */
	  first_expr_processing (IR_foreach_designator (stmt),
				 START_TEMP_VARS_NUMBER + 1);
	  first_expr_processing (IR_foreach_table (stmt),
				 START_TEMP_VARS_NUMBER + 3);
	  IR_set_foreach_stmts (stmt,
				first_block_passing (IR_foreach_stmts (stmt),
						     current_block_level));
	  break;
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	case IR_NM_return_without_result:
	  break;
	case IR_NM_return_with_result:
	  first_expr_processing (IR_returned_expr (stmt),
				 START_TEMP_VARS_NUMBER);
	  break;
	case IR_NM_throw:
	  first_expr_processing (IR_throw_expr (stmt),
				 START_TEMP_VARS_NUMBER);
	  break;
	case IR_NM_wait_stmt:
	  first_expr_processing (IR_wait_guard_expr (stmt),
				 START_TEMP_VARS_NUMBER);
	  IR_set_wait_stmt (stmt,
			    first_block_passing (IR_wait_stmt (stmt),
						 current_block_level));
	  break;
	case IR_NM_block:
	  {
	    IR_node_t saved_current_scope = current_scope;
	    IR_node_t curr_except;

	    IR_set_decls_number (stmt, 0);
	    IR_set_vars_number (stmt, 0);
	    current_scope = stmt;
	    IR_set_block_level (stmt, current_block_level);
	    if (max_block_level < current_block_level)
	      max_block_level = current_block_level;
	    IR_set_block_stmts
	      (stmt, first_block_passing (IR_block_stmts (stmt),
					  current_block_level + 1));
	    current_scope = saved_current_scope;
	    IR_set_block_number (stmt, new_block ());
	    if (IR_func_class_ext (stmt) == NULL
		&& IR_decls_number (stmt) == 0
		&& IR_exceptions (stmt) == NULL)
	      {
		IR_node_t curr_block;

		for (curr_block = current_scope;
		     IR_decls_number (curr_block) == 0
		       && IR_exceptions (curr_block) == NULL
		       && IR_func_class_ext (curr_block) == NULL;
		     curr_block = IR_block_scope (curr_block))
		  ;
		if (IR_temporary_vars_number (stmt)
		    > IR_temporary_vars_number (curr_block))
		  IR_set_temporary_vars_number
		    (curr_block, IR_temporary_vars_number (stmt));
		IR_set_simple_block_flag (stmt, TRUE);
	      }
	    for (curr_except = IR_exceptions (stmt);
		 curr_except != NULL;
		 curr_except = IR_next_exception (curr_except))
	      {
		first_expr_processing (IR_exception_class_expr (curr_except),
				       START_TEMP_VARS_NUMBER);
		if (IR_catch_block (curr_except) != NULL)
		  IR_set_catch_block
		    (curr_except,
		     first_block_passing (IR_catch_block (curr_except),
					  current_block_level));
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
	  assert (FALSE);
	}
    }
  return first_level_stmt;
}

/* This recursive func passes (correctly setting up SOURCE_POSITION
   and current_scope (before first call of the func current_scope is
   to be NULL)) all stmts on the same stmt nesting level and sets up
   elements values of class decls idents tables (see also commentaries
   for corresponding abstract data).  FIRST_LEVEL_STMT (it may be
   NULL) is the first stmt of the processed stmt nesting level.
   
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
	case IR_NM_swap:
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_rem_assign:
	case IR_NM_plus_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	case IR_NM_procedure_call:
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
	  second_block_passing (IR_foreach_stmts (stmt));
	  break;
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
	    IR_node_t saved_current_scope = current_scope;
	    IR_node_t curr_stmt;
	    IR_node_t curr_access_ident;
	    IR_node_t decl;
	    IR_node_t friend;
	    IR_node_t curr_except;
	    int public_flag;

	    current_scope = stmt;
	    second_block_passing (IR_block_stmts (stmt));
	    public_flag = (IR_func_class_ext (current_scope) != NULL
			   && IR_IS_OF_TYPE (IR_func_class_ext (current_scope),
					     IR_NM_class));
	    for (curr_stmt = IR_block_stmts (stmt); curr_stmt != NULL;
		 curr_stmt = IR_next_stmt (curr_stmt))
	      if (IR_IS_OF_TYPE (curr_stmt, IR_NM_decl))
		IR_set_public_flag (curr_stmt, public_flag);
	    for (curr_access_ident = IR_access_list (current_scope);
		 curr_access_ident != NULL;
		 curr_access_ident = IR_next_access_ident (curr_access_ident))
	      if (IR_friend_flag (curr_access_ident))
		{
		  assert (!IR_access_ident_public_flag (curr_access_ident));
		  decl = find_decl (IR_ident (curr_access_ident),
				    current_scope);
		  if (decl == NULL)
		    error (FALSE, IR_pos (IR_ident (curr_access_ident)),
			   ERR_udenclared_ident_access_list,
			   IR_ident_string (IR_unique_ident
					    (IR_ident
					     (curr_access_ident))));
		  else if (!IR_IS_OF_TYPE (decl, IR_NM_func_or_class))
		    error (FALSE, IR_pos (IR_ident (curr_access_ident)),
			   ERR_invalid_friend,
			   IR_ident_string (IR_unique_ident
					    (IR_ident
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
		  assert (!IR_friend_flag (curr_access_ident));
		  decl
		    = find_decl_in_given_scope (IR_ident (curr_access_ident),
						current_scope);
		  if (decl == NULL)
		    error (FALSE, IR_pos (IR_ident (curr_access_ident)),
			   ERR_udenclared_ident_access_list,
			   IR_ident_string (IR_unique_ident
					    (IR_ident
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
		      error (FALSE, IR_pos (IR_ident (curr_access_ident)),
			     ERR_contradicted_ident_access_list,
			     IR_ident_string (IR_unique_ident
					      (IR_ident (curr_access_ident))));
		      append_message
			(IR_pos (IR_ident (IR_access_ident (decl))),
			 ERR_previous_access_location,
			 IR_ident_string (IR_unique_ident
					  (IR_ident (curr_access_ident))));
		    }
		}
	    current_scope = saved_current_scope;
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
	    assert (block != NULL && IR_NODE_MODE (block) == IR_NM_block);
	    define_block_decl (stmt, block);
	    break;
	  }
	default:
	  assert (FALSE);
	}
    }
}

/* The following func returns pointer to first class decl
   surrounding given SCOPE (it may be NULL).  the func return NULL if
   there is not such class decl. */
static IR_node_t
surrounding_class (IR_node_t scope)
{
  IR_node_t func_class_ext;

  for (; scope != NULL; scope = IR_block_scope (scope))
    {
      func_class_ext = IR_func_class_ext (scope);
      if (func_class_ext != NULL && IR_NODE_MODE (func_class_ext) == IR_NM_class)
	return func_class_ext;
    }
  return NULL;
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
   be changed by corresponding decl occurrence. */
static int
value_type (IR_node_t expr)
{
  if (expr == NULL)
    return EVT_UNKNOWN;
  switch (IR_NODE_MODE (expr))
    {
    case IR_NM_char:
      return EVT_CHAR;
    case IR_NM_int:
      return EVT_INT;
    case IR_NM_float:
      return EVT_FLOAT;
    case IR_NM_string:
      return EVT_VECTOR;
    case IR_NM_nil:
      return EVT_UNKNOWN;
    case IR_NM_char_type:
    case IR_NM_int_type:
    case IR_NM_float_type:
    case IR_NM_vector_type:
    case IR_NM_hide_type:
    case IR_NM_hideblock_type:
    case IR_NM_table_type:
    case IR_NM_func_type:
    case IR_NM_thread_type:
    case IR_NM_class_type:
    case IR_NM_stack_type:
    case IR_NM_process_type:
    case IR_NM_instance_type:
    case IR_NM_type_type:
      return EVT_TYPE;
    case IR_NM_var_occurrence:
    case IR_NM_local_var_occurrence:
    case IR_NM_external_var_occurrence:
      return EVT_UNKNOWN;
    case IR_NM_func_occurrence:
      return EVT_FUNC;
    case IR_NM_class_occurrence:
      return EVT_CLASS;
    case IR_NM_logical_or:
    case IR_NM_logical_and:
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
    case IR_NM_period:
    case IR_NM_index:
    case IR_NM_key_index:
    case IR_NM_no_testing_period: /* Remember it may be already created. */
    case IR_NM_new:
    case IR_NM_const:
    case IR_NM_not:
    case IR_NM_unary_plus:
    case IR_NM_unary_minus:
    case IR_NM_bitwise_not:
    case IR_NM_length:
    case IR_NM_typeof:
    case IR_NM_charof:
    case IR_NM_intof:
    case IR_NM_floatof:
    case IR_NM_vectorof:
    case IR_NM_format_vectorof:
    case IR_NM_tableof:
    case IR_NM_funcof:
    case IR_NM_threadof:
    case IR_NM_classof:
    case IR_NM_cond:
      return IR_value_type (expr);
    case IR_NM_vector:
      return EVT_VECTOR;
    case IR_NM_table:
      return EVT_TABLE;
    case IR_NM_class_func_thread_call:
      return EVT_UNKNOWN;
    }
  return EVT_UNKNOWN; /* No warnings (e.g. error has been fixed). */
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

/* This macro is used to connect commands for interpreter (see comments for
   typedef pc_t) into chain by members next_pc. */
#define SET_PC(node)\
   do {\
     if (current_pc != NULL)\
       IR_set_next_pc (IR_POINTER (current_pc), PC (node));\
     current_pc = PC (node);\
   } while (0)

/* See comments for the function third_expr_processing.  This variable
   is used for searching for side effects in wait stmt expression. */
static int there_is_function_call_in_expr;

/* The following recursive func passes (correctly setting up
   SOURCE_POSITION) EXPR (it may be NULL) and
   changes idents on corresponding decls occurrences
   according to languge visibility rules.
   Also the func tests that ident in period operations is
   defined in any class.
   Therefore the func returns pointer to (possibly)
   modified expr.  See also commentaries for abstract data
   block_decl_idents_tables. 
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
third_expr_processing (IR_node_t expr, int func_class_assign_p)
{
  if (expr == NULL)
    return expr;
  switch (IR_NODE_MODE (expr))
    {
    case IR_NM_char:
      IR_set_ch_val (expr, IR_char_value (IR_unique_char (expr)));
      SET_PC (expr);
      break;
    case IR_NM_int:
      IR_set_i_val (expr, IR_int_value (IR_unique_int (expr)));
      SET_PC (expr);
      break;
    case IR_NM_float:
      IR_set_f_val (expr, IR_float_value (IR_unique_float (expr)));
      SET_PC (expr);
      break;
    case IR_NM_string:
      IR_set_str_val (expr, IR_string_value (IR_unique_string (expr)));
      SET_PC (expr);
      break;
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
      SET_PC (expr);
      break;
    case IR_NM_ident:
      {
	IR_node_t ident;
	IR_node_t decl;
	IR_node_t scope;

	ident = expr;
	decl = find_decl (expr, current_scope);
	SET_SOURCE_POSITION (expr);
	if (decl == NULL)
 	  error (FALSE, source_position, ERR_undeclared_ident,
		 IR_ident_string (IR_unique_ident (ident)));
	else
	  {
	    for (scope = current_scope;
		 IR_simple_block_flag (scope);
		 scope = IR_block_scope (scope))
	      ;
	    expr = create_occurrence_node
	           (decl, IR_pos (ident),
		    scope == IR_scope (decl)
		    && (IR_func_class_ext (scope) == NULL
			|| ! IR_IS_OF_TYPE (IR_func_class_ext (scope),
					    IR_NM_class)));
	    if (func_class_assign_p
		&& (IR_NODE_MODE (expr) == IR_NM_func_occurrence
		    || IR_NODE_MODE (expr) == IR_NM_class_occurrence))
	      IR_set_extended_life_context_flag (current_scope, TRUE);
	  }
	SET_PC (expr);
	break;
      }
    case IR_NM_period:
      SET_SOURCE_POSITION (expr);
      IR_set_left_operand (expr,
			   third_expr_processing (IR_left_operand (expr),
						  FALSE));
      SET_PC (expr);
      assert (IR_right_operand (expr) != NULL
	      && IR_NODE_MODE (IR_right_operand (expr)) == IR_NM_ident);
      if (IR_left_operand (expr) != NULL) {
	if (!IR_it_is_declared_in_block (IR_unique_ident
					 (IR_right_operand (expr))))
	  error (FALSE, source_position, ERR_decl_is_absent_in_a_block);
      }
      break;
    case IR_NM_logical_or:
    case IR_NM_logical_and:
      {
	IR_node_t logical_end;

	SET_SOURCE_POSITION (expr);
	IR_set_left_operand (expr,
			     third_expr_processing (IR_left_operand (expr),
						    FALSE));
	SET_PC (expr);
	logical_end = create_node_with_pos ((IR_NODE_MODE (expr)
					     == IR_NM_logical_or
					     ? IR_NM_logical_or_end
					     : IR_NM_logical_and_end),
					    source_position);
        IR_set_short_path_pc (IR_POINTER (current_pc), PC (logical_end));
	IR_set_right_operand
	  (expr, third_expr_processing (IR_right_operand (expr), FALSE));
	IR_set_pos (logical_end, source_position);
	type_test (IR_left_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_logical_operation_operand_type);
	type_test (IR_right_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_logical_operation_operand_type);
	IR_set_value_type (expr, EVT_INT);
	SET_PC (logical_end);
      }
      break;
    case IR_NM_not:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_STRING_MASK,
		 ERR_invalid_logical_operation_operand_type);
      IR_set_value_type (expr, EVT_INT);
      break;
    case IR_NM_eq:
    case IR_NM_ne:
    case IR_NM_identity:
    case IR_NM_unidentity:
      SET_SOURCE_POSITION (expr);
      IR_set_left_operand
	(expr, third_expr_processing (IR_left_operand (expr), FALSE));
      IR_set_right_operand
	(expr, third_expr_processing (IR_right_operand (expr), FALSE));
      if ((IR_NODE_MODE (expr) == IR_NM_eq || IR_NODE_MODE (expr) == IR_NM_ne)
	  && IR_left_operand (expr) != NULL
	  && IR_right_operand (expr) != NULL
	  && value_type (IR_left_operand (expr)) != EVT_UNKNOWN
	  && (!IT_IS_OF_TYPE (IR_right_operand (expr),
			      value_type (IR_left_operand (expr)))
	      && (!IT_IS_OF_TYPE (IR_right_operand (expr),
				  EVT_NUMBER_STRING_MASK)
		  || !IT_IS_OF_TYPE (IR_right_operand (expr),
				     EVT_NUMBER_STRING_MASK))))
	error (FALSE, source_position,
	       ERR_invalid_comparison_operation_operand_type);
      SET_PC (expr);
      IR_set_value_type (expr, EVT_INT);
      break;
    case IR_NM_lt:
    case IR_NM_gt:
    case IR_NM_le:
    case IR_NM_ge:
      SET_SOURCE_POSITION (expr);
      IR_set_left_operand
	(expr, third_expr_processing (IR_left_operand (expr), FALSE));
      IR_set_right_operand
	(expr, third_expr_processing (IR_right_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_left_operand (expr), EVT_NUMBER_STRING_MASK,
		 ERR_invalid_order_comparison_operation_operand_type);
      type_test (IR_right_operand (expr), EVT_NUMBER_STRING_MASK,
		 ERR_invalid_order_comparison_operation_operand_type);
      IR_set_value_type (expr,  EVT_INT);
      break;
    case IR_NM_concat:
      SET_SOURCE_POSITION (expr);
      IR_set_left_operand
	(expr, third_expr_processing (IR_left_operand (expr), FALSE));
      IR_set_right_operand
	(expr, third_expr_processing (IR_right_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_left_operand (expr), EVT_NUMBER_VECTOR_MASK,
		 ERR_invalid_concat_operation_operand_type);
      type_test (IR_right_operand (expr), EVT_NUMBER_VECTOR_MASK,
		 ERR_invalid_concat_operation_operand_type);
      IR_set_value_type (expr, EVT_VECTOR);
      break;
    case IR_NM_in:
      SET_SOURCE_POSITION (expr);
      IR_set_left_operand
	(expr, third_expr_processing (IR_left_operand (expr), FALSE));
      IR_set_right_operand
	(expr, third_expr_processing (IR_right_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_right_operand (expr), EVT_TABLE, ERR_invalid_table_type);
      IR_set_value_type (expr, EVT_INT);
      break;
    case IR_NM_or:
    case IR_NM_xor:
    case IR_NM_and:
    case IR_NM_lshift:
    case IR_NM_rshift:
    case IR_NM_ashift:
      SET_SOURCE_POSITION (expr);
      IR_set_left_operand
	(expr, third_expr_processing (IR_left_operand (expr), FALSE));
      IR_set_right_operand
	(expr, third_expr_processing (IR_right_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_left_operand (expr), EVT_NUMBER_STRING_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      type_test (IR_right_operand (expr), EVT_NUMBER_STRING_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      IR_set_value_type (expr, EVT_INT);
      break;
    case IR_NM_plus:
    case IR_NM_minus:
    case IR_NM_mult:
    case IR_NM_div:
    case IR_NM_mod:
      {
	type_mask_t type_mask1;
	type_mask_t type_mask2;
	
	SET_SOURCE_POSITION (expr);
	IR_set_left_operand
	  (expr, third_expr_processing (IR_left_operand (expr), FALSE));
	IR_set_right_operand
	  (expr, third_expr_processing (IR_right_operand (expr), FALSE));
	SET_PC (expr);
	type_test (IR_left_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_arithmetic_operation_operand_type);
	type_test (IR_right_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_arithmetic_operation_operand_type);
	type_mask1 = value_type (IR_left_operand (expr));
	type_mask2 = value_type (IR_right_operand (expr));
	if (type_mask1 == EVT_FLOAT || type_mask2 == EVT_FLOAT)
	  IR_set_value_type (expr, EVT_FLOAT);
	else if ((type_mask1 == EVT_INT || type_mask1 == EVT_CHAR)
		 && (type_mask2 == EVT_INT || type_mask2 == EVT_CHAR))
	  IR_set_value_type (expr, EVT_INT);
	else
	  IR_set_value_type (expr, EVT_NUMBER_MASK);
      }
      break;
    case IR_NM_unary_plus:
    case IR_NM_unary_minus:
      {
	type_mask_t type_mask;
	
	SET_SOURCE_POSITION (expr);
	IR_set_operand
	  (expr, third_expr_processing (IR_operand (expr), FALSE));
	SET_PC (expr);
	type_test (IR_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_arithmetic_operation_operand_type);
	type_mask = value_type (IR_operand (expr));
	IR_set_value_type (expr, (type_mask == EVT_UNKNOWN
				  ? EVT_NUMBER_MASK : type_mask));
      }
      break;
    case IR_NM_bitwise_not:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_STRING_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      IR_set_value_type (expr, EVT_INT);
      break;
    case IR_NM_length:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_VECTOR_TABLE_MASK,
		 ERR_invalid_length_operand_type);
      IR_set_value_type (expr, EVT_INT);
      break;
    case IR_NM_typeof:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      IR_set_value_type (expr, EVT_TYPE);
      break;
    case IR_NM_charof:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_VECTOR_MASK,
		 ERR_invalid_conversion_to_char_operand_type);
      IR_set_value_type (expr, EVT_CHAR);
      break;
    case IR_NM_intof:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_VECTOR_MASK,
		 ERR_invalid_conversion_to_int_operand_type);
      IR_set_value_type (expr, EVT_INT);
      break;
    case IR_NM_floatof:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_VECTOR_MASK,
		 ERR_invalid_conversion_to_float_operand_type);
      IR_set_value_type (expr, EVT_FLOAT);
      break;
    case IR_NM_vectorof:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_VECTOR_TABLE_MASK,
		 ERR_invalid_conversion_to_vector_operand_type);
      IR_set_value_type (expr, EVT_VECTOR);
      break;
    case IR_NM_format_vectorof:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      IR_set_format (expr, third_expr_processing (IR_format (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_VECTOR_TABLE_MASK,
		 ERR_invalid_conversion_to_vector_operand_type);
      type_test (IR_format (expr), EVT_VECTOR,
		 ERR_invalid_conversion_format_type);
      IR_set_value_type (expr, EVT_VECTOR);
      break;
    case IR_NM_tableof:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      type_test (IR_operand (expr), EVT_NUMBER_VECTOR_TABLE_MASK,
		 ERR_invalid_conversion_to_table_operand_type);
      IR_set_value_type (expr, EVT_TABLE);
      break;
    case IR_NM_funcof:
    case IR_NM_threadof:
    case IR_NM_classof:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), FALSE));
      SET_PC (expr);
      IR_set_value_type (expr, EVT_UNKNOWN); /* May be nil */
      break;
    case IR_NM_new:
    case IR_NM_const:
      SET_SOURCE_POSITION (expr);
      IR_set_operand (expr, third_expr_processing (IR_operand (expr), TRUE));
      SET_PC (expr);
      IR_set_value_type (expr, value_type (IR_operand (expr)));
      break;
    case IR_NM_cond:
      {
	IR_node_t cond_end;
        pc_t true_path_begin_pc;

        SET_SOURCE_POSITION (expr);
        IR_set_cond (expr, third_expr_processing (IR_cond (expr), FALSE));
	SET_PC (expr);
	cond_end = create_node_with_pos (IR_NM_cond_end, source_position);
        IR_set_left_operand
	  (expr, third_expr_processing (IR_left_operand (expr),
					func_class_assign_p));
        if (current_pc == PC (expr))
          true_path_begin_pc = PC (cond_end);
        else
          {
            if (current_pc != NULL)
              IR_set_next_pc (IR_POINTER (current_pc), PC (cond_end));
            true_path_begin_pc = IR_next_pc (expr);
          }
        current_pc = PC (expr);
        IR_set_right_operand
	  (expr, third_expr_processing (IR_right_operand (expr),
					func_class_assign_p));
        if (current_pc == PC (expr))
          IR_set_false_path_pc (expr, PC (cond_end));
        else
          {
            if (current_pc != NULL)
              IR_set_next_pc (IR_POINTER (current_pc), PC (cond_end));
            IR_set_false_path_pc (expr, IR_next_pc (expr));
          }
        IR_set_next_pc (expr, true_path_begin_pc);
        current_pc = PC (cond_end);
        type_test (IR_cond (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_cond_type);
        IR_set_value_type (expr,
                           value_type (IR_left_operand (expr))
                           | value_type (IR_right_operand (expr)));
      }
      break;
    case IR_NM_vector:
    case IR_NM_table:
      SET_SOURCE_POSITION (expr);
      {
	IR_node_t elist;
	
	for (elist = IR_elist (expr);
	     elist != NULL;
	     elist = IR_next_elist (elist))
	  {
	    SET_SOURCE_POSITION (elist);
	    IR_set_repetition_key
	      (elist,
	       third_expr_processing (IR_repetition_key (elist), FALSE));
            if (IR_NODE_MODE (expr) == IR_NM_vector)
              type_test (IR_repetition_key (elist), EVT_NUMBER_STRING_MASK,
                         ERR_invalid_repetition_type);
	    IR_set_expr
	      (elist, third_expr_processing (IR_expr (elist), TRUE));
	  }
	SET_PC (expr);
	break;  
      }
    case IR_NM_index:
    case IR_NM_key_index:
      SET_SOURCE_POSITION (expr);
      IR_set_left_operand
	(expr, third_expr_processing (IR_left_operand (expr), FALSE));
      IR_set_right_operand
	(expr, third_expr_processing (IR_right_operand (expr), FALSE));
      SET_PC (expr);
      if (IR_NODE_MODE (expr) == IR_NM_index)
        {
          type_test (IR_left_operand (expr), EVT_VECTOR,
                     ERR_invalid_vector_type);
          type_test (IR_right_operand (expr), EVT_NUMBER_STRING_MASK,
                     ERR_invalid_index_type);
        }
      else
        type_test (IR_left_operand (expr), EVT_TABLE, ERR_invalid_table_type);
      break;
    case IR_NM_class_func_thread_call:
      {
	IR_node_t elist;
	
	there_is_function_call_in_expr = TRUE;
	SET_SOURCE_POSITION (expr);
	IR_set_func_expr
	  (expr, third_expr_processing (IR_func_expr (expr), FALSE));
	for (elist = IR_actuals (expr);
	     elist != NULL;
	     elist = IR_next_elist (elist))
	  {
	    SET_SOURCE_POSITION (elist);
	    IR_set_expr (elist, third_expr_processing (IR_expr (elist), TRUE));
	  }
	SET_PC (expr);
	if (!IT_IS_OF_TYPE (IR_func_expr (expr), EVT_FUNC)
	    && !IT_IS_OF_TYPE (IR_func_expr (expr), EVT_CLASS))
	  error (FALSE, source_position,
		 ERR_invalid_class_func_thread_designator);
	break;  
      }
    default:
      assert (FALSE);
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
   IR_NM_local_var_occurrence    IR_NM_local_lvalue_var_occurrence(_and_val)
   IR_NM_external_var_occurrence IR_NM_lvalue_external_var_occurrence(_and_val)
   IR_NM_no_testing_period       IR_NM_lvalue_no_testing_period(_and_val)

   If DESIGNATOR is not correct, generate ERROR_MESSAGE. */

static IR_node_mode_t
make_designator_lvalue (IR_node_t designator, const char *error_message,
			int val_p)
{
    switch (IR_NODE_MODE (designator))
      {
      case IR_NM_period:
	return (val_p
		? IR_NM_lvalue_period_and_val : IR_NM_lvalue_period);
      case IR_NM_index:
	return (val_p
		? IR_NM_lvalue_index_and_val : IR_NM_lvalue_index);
      case IR_NM_key_index:
	return (val_p
		? IR_NM_lvalue_key_index_and_val : IR_NM_lvalue_key_index);
      case IR_NM_var_occurrence:
	return (val_p
		? IR_NM_lvalue_var_occurrence_and_val
		: IR_NM_lvalue_var_occurrence);
      case IR_NM_local_var_occurrence:
	return (val_p
		? IR_NM_local_lvalue_var_occurrence_and_val
		: IR_NM_local_lvalue_var_occurrence);
      case IR_NM_external_var_occurrence:
	return (val_p
		? IR_NM_lvalue_external_var_occurrence_and_val
		: IR_NM_lvalue_external_var_occurrence);
      case IR_NM_no_testing_period:
	return (val_p
		? IR_NM_lvalue_no_testing_period_and_val
		: IR_NM_lvalue_no_testing_period);
      default:
	error (FALSE, IR_pos (designator), error_message);
	return IR_NODE_MODE (designator);
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


/* This recursive func passes (correctly setting up SOURCE_POSITION
   and current_scope (before first call of the func current_scope is
   to be NULL)) all stmts on the same stmt nesting level and sets up
   elements values of class decls idents tables (see also commentaries
   for corresponding abstract data).  FIRST_LEVEL_STMT (it may be
   NULL) is first stmt of the processed stmt nesting level.  The func
   creates chain by members next_pc, (for_,foreach_)body_pc,
   else_part_pc and start_wait_guard_expr_pc to execution of the
   processed stmts.  The func may create new nodes block_finish node,
   if_finish node, for_finish node, node and pop_func_call node and
   include them into the chain.  The func also change mode of node
   given as ASSIGN NODE (stmt)->assignment_var in assignment stmt or
   foreach stmt (with the aid of `make_designator_lvalue').  It is
   needed for the evaluator.

   The funcs also fixes repeated errors ERR_continue_is_not_in_loop,
   ERR_break_is_not_in_loop. */
static void
third_block_passing (IR_node_t first_level_stmt)
{
  IR_node_t stmt;
  IR_node_t temp;
  IR_node_mode_t mode;

  for (stmt = first_level_stmt; stmt != NULL; stmt = IR_next_stmt (stmt))
    {
      SET_SOURCE_POSITION (stmt);
      switch (IR_NODE_MODE (stmt))
	{
	case IR_NM_assign:
	case IR_NM_var_assign:
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_rem_assign:
	case IR_NM_plus_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	  temp = third_expr_processing (IR_assignment_var (stmt), FALSE);
	  if (temp != NULL)
	    {
	      mode = IR_NODE_MODE (temp);
	      IR_SET_MODE (temp,
			   make_designator_lvalue
			   (temp, ERR_non_variable_in_assignment,
			    ! IR_IS_OF_TYPE (stmt, IR_NM_assign)
			    && ! IR_IS_OF_TYPE (stmt, IR_NM_var_assign)));
	    }
	  if (IR_NODE_MODE (stmt) != IR_NM_var_assign && temp != NULL
	      && (IR_IS_OF_TYPE (temp, IR_NM_lvalue_var_occurrence)
		  || IR_IS_OF_TYPE (temp, IR_NM_local_lvalue_var_occurrence)
		  || IR_IS_OF_TYPE (temp,
				    IR_NM_lvalue_external_var_occurrence)
		  || IR_IS_OF_TYPE (temp, IR_NM_lvalue_var_occurrence_and_val)
		  || IR_IS_OF_TYPE (temp,
				    IR_NM_local_lvalue_var_occurrence_and_val)
		  || IR_IS_OF_TYPE
 		     (temp, IR_NM_lvalue_external_var_occurrence_and_val))
	      && (IR_IS_OF_TYPE (IR_decl (temp), IR_NM_var)
		  || IR_IS_OF_TYPE (IR_decl (temp), IR_NM_external_var))
	      && IR_const_flag (IR_decl (temp)))
	    error (FALSE, IR_pos (temp), ERR_const_assignment,
		   IR_ident_string (IR_unique_ident
				    (IR_ident (IR_decl (temp)))));
	  if (temp != NULL && IR_NODE_MODE (temp) != mode)
	    IR_set_assignment_var (stmt, temp);
	  IR_set_assignment_expr
	    (stmt, third_expr_processing (IR_assignment_expr (stmt), TRUE));
	  SET_PC (stmt);
	  break;
	case IR_NM_par_assign:
	  {
	    IR_node_t par_assign_test, par_assign_end;

	    temp = third_expr_processing (IR_assignment_var (stmt), FALSE);
	    assert (temp != NULL);
	    IR_SET_MODE (temp,
			 make_designator_lvalue
			 (temp, ERR_non_variable_in_assignment, FALSE));
	    IR_set_assignment_var (stmt, temp);
	    par_assign_test = create_node_with_pos (IR_NM_par_assign_test,
						    source_position);
	    par_assign_end = create_node_with_pos (IR_NM_par_assign_end,
						   source_position);
	    IR_set_skip_par_assign_path_pc (par_assign_test, par_assign_end);
	    SET_PC (par_assign_test);
	    IR_set_assignment_expr
	      (stmt, third_expr_processing (IR_assignment_expr (stmt), TRUE));
	    SET_PC (stmt);
	    SET_PC (par_assign_end);
	  }
	  break;
	case IR_NM_swap:
	  temp = third_expr_processing (IR_swap_var1 (stmt), FALSE);
	  if (temp != NULL)
	    {
	      mode = IR_NODE_MODE (temp);
	      IR_SET_MODE (temp,
			   make_designator_lvalue (temp,
						   ERR_non_variable_in_swap,
						   FALSE));
	    }
	  if (IR_NODE_MODE (stmt) != IR_NM_var_assign && temp != NULL
	      && (IR_IS_OF_TYPE (temp, IR_NM_lvalue_var_occurrence)
		  || IR_IS_OF_TYPE (temp, IR_NM_local_lvalue_var_occurrence)
		  || IR_IS_OF_TYPE (temp,
				    IR_NM_lvalue_external_var_occurrence))
	      && (IR_IS_OF_TYPE (IR_decl (temp), IR_NM_var)
		  || IR_IS_OF_TYPE (IR_decl (temp), IR_NM_external_var))
	      && IR_const_flag (IR_decl (temp)))
	    error (FALSE, IR_pos (temp), ERR_const_swap,
		   IR_ident_string (IR_unique_ident
				    (IR_ident (IR_decl (temp)))));
	  if (temp != NULL && IR_NODE_MODE (temp) != mode)
	    IR_set_swap_var1 (stmt, temp);
	  temp = third_expr_processing (IR_swap_var2 (stmt), FALSE);
	  if (temp != NULL)
	    {
	      mode = IR_NODE_MODE (temp);
	      IR_SET_MODE (temp,
			   make_designator_lvalue (temp,
						   ERR_non_variable_in_swap,
						   FALSE));
	    }
	  if (IR_NODE_MODE (stmt) != IR_NM_var_assign && temp != NULL
	      && (IR_IS_OF_TYPE (temp, IR_NM_lvalue_var_occurrence)
		  || IR_IS_OF_TYPE (temp, IR_NM_local_lvalue_var_occurrence)
		  || IR_IS_OF_TYPE (temp,
				    IR_NM_lvalue_external_var_occurrence))
	      && (IR_IS_OF_TYPE (IR_decl (temp), IR_NM_var)
		  || IR_IS_OF_TYPE (IR_decl (temp), IR_NM_external_var))
	      && IR_const_flag (IR_decl (temp)))
	    error (FALSE, IR_pos (temp), ERR_const_swap,
		   IR_ident_string (IR_unique_ident
				    (IR_ident (IR_decl (temp)))));
	  if (temp != NULL && IR_NODE_MODE (temp) != mode)
	    IR_set_swap_var2 (stmt, temp);
	  SET_PC (stmt);
	  break;
	case IR_NM_procedure_call:
	  {
	    IR_node_t elist, pop_func_call;
	    
	    IR_set_proc_expr
	      (stmt, third_expr_processing (IR_proc_expr (stmt), FALSE));
	    for (elist = IR_proc_actuals (stmt);
		 elist != NULL;
		 elist = IR_next_elist (elist))
	      {
		SET_SOURCE_POSITION (elist);
		IR_set_expr
		  (elist, third_expr_processing (IR_expr (elist), TRUE));
	      }
	    SET_PC (stmt);
	    pop_func_call = create_node_with_pos (IR_NM_pop_func_call,
						  IR_pos (stmt));
	    SET_PC (pop_func_call);
	    break;  
	  }
	case IR_NM_if_stmt:
	  {
	    IR_node_t if_finish;
	    pc_t if_part_begin_pc;
	    
	    IR_set_if_expr
	      (stmt, third_expr_processing (IR_if_expr (stmt), FALSE));
	    type_test (IR_if_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_if_expr_type);
	    SET_PC (stmt);
	    if_finish = create_node_with_pos (IR_NM_if_finish,
					      source_position);
	    IR_set_common_part_pc (stmt, PC (if_finish));
	    third_block_passing (IR_if_part (stmt));
	    if (current_pc == PC (stmt))
	      if_part_begin_pc = PC (if_finish);
	    else
	      {
		if (current_pc != NULL)
		  IR_set_next_pc (IR_POINTER (current_pc), PC (if_finish));
		if_part_begin_pc = IR_next_pc (stmt);
	      }
	    current_pc = PC (stmt);
	    third_block_passing (IR_else_part (stmt));
	    IR_set_pos (if_finish, source_position);
	    if (current_pc == PC (stmt))
	      IR_set_else_part_pc (stmt, PC (if_finish));
	    else
	      {
		if (current_pc != NULL)
		  IR_set_next_pc (IR_POINTER (current_pc), PC (if_finish));
		IR_set_else_part_pc (stmt, IR_next_pc (stmt));
	      }
	    IR_set_next_pc (stmt, if_part_begin_pc);
	    current_pc = PC (if_finish);
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
	    assert (current_pc != NULL);
	    before_guard_expr = current_pc;
	    IR_set_for_guard_expr
	      (stmt, third_expr_processing (IR_for_guard_expr (stmt), FALSE));
	    type_test (IR_for_guard_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_for_guard_expr_type);
	    SET_PC (stmt);
	    third_block_passing (IR_for_iterate_stmt (stmt));
	    for_iterate_start = IR_next_pc (stmt);
	    for_iterate_finish = current_pc;
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
	    current_pc = PC (stmt);
	    third_block_passing (IR_for_stmts (stmt));
	    IR_set_pos (for_finish, source_position);
	    /* The following guard is needed because the last stmt
	       may be break or continue. */
	    if (current_pc != NULL)
	      IR_set_next_pc (current_pc, start_next_iteration_pc);
	    IR_set_for_body_pc (stmt, IR_next_pc (stmt));
	    current_pc = PC (stmt);
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
	    before_in_expr = current_pc;
	    temp = third_expr_processing (IR_foreach_designator (stmt), FALSE);
	    if (temp != NULL)
	      {
		mode = IR_NODE_MODE (temp);
		IR_SET_MODE (temp,
			     make_designator_lvalue
			     (temp, ERR_non_variable_in_foreach, FALSE));
	      }
	    if (temp != NULL && IR_NODE_MODE (temp) != mode)
	      IR_set_foreach_designator (stmt, temp);
	    IR_set_foreach_table
	      (stmt, third_expr_processing (IR_foreach_table (stmt), FALSE));
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
	    if (current_pc != NULL)
	      IR_set_next_pc (current_pc, start_next_iteration_pc);
	    IR_set_foreach_body_pc (stmt, IR_next_pc (stmt));
	    current_pc = PC (stmt);
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
		   IR_NODE_MODE (stmt) == IR_NM_continue_stmt
		   ? ERR_continue_is_not_in_loop : ERR_break_is_not_in_loop);
	  else
	    {
	      SET_PC (stmt);
	      IR_set_number_of_surrounding_blocks
		(stmt, number_of_surrounding_blocks);
	      if (IR_NODE_MODE (stmt) == IR_NM_continue_stmt)
		SET_PC (IR_POINTER (start_next_iteration_pc));
	      else
		SET_PC (for_finish);
	      current_pc = NULL;
	    }
	  break;
	case IR_NM_return_without_result:
	case IR_NM_return_with_result:
	  {
	    IR_node_t func_class_ext;

	    func_class_ext = find_covered_func_class_ext (current_scope);
	    if (func_class_ext == NULL)
	      error (FALSE, source_position,
		     ERR_return_outside_func_class_ext);
	    else if (IR_NODE_MODE (stmt) == IR_NM_return_with_result)
	      {
		if (IR_IS_OF_TYPE (func_class_ext, IR_NM_class))
		  error (FALSE, source_position,
			 ERR_return_with_result_in_class);
		else if (IR_IS_OF_TYPE (func_class_ext, IR_NM_func)
			 && IR_thread_flag (func_class_ext))
		  error (FALSE, source_position,
			 ERR_return_with_result_in_thread);
		IR_set_returned_expr
		  (stmt,
		   third_expr_processing (IR_returned_expr (stmt), TRUE));
	      }
	    SET_PC (stmt);
	    break;
	  }
	case IR_NM_wait_stmt:
	  {
	    pc_t before_wait_guard_expr, wait_finish;
	    
	    before_wait_guard_expr = current_pc;
	    there_is_function_call_in_expr = FALSE;
	    IR_set_wait_guard_expr
	      (stmt, third_expr_processing (IR_wait_guard_expr (stmt), FALSE));
	    type_test (IR_wait_guard_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_wait_guard_expr_type);
	    if (there_is_function_call_in_expr)
	      error (FALSE, source_position, ERR_function_call_in_wait_stmt);
	    IR_set_start_wait_guard_expr_pc
	      (stmt, IR_next_pc (IR_POINTER (before_wait_guard_expr)));
	    SET_PC (stmt);
	    wait_finish = create_node_with_pos (IR_NM_wait_finish,
						source_position);
	    third_block_passing (IR_wait_stmt (stmt));
	    IR_set_pos (wait_finish, source_position);
	    SET_PC (wait_finish);
	    break;
	  }
	case IR_NM_throw:
	  IR_set_throw_expr
	    (stmt, third_expr_processing (IR_throw_expr (stmt), FALSE));
	  type_test (IR_throw_expr (stmt), EVT_UNKNOWN,
		     ERR_invalid_throw_expr_type);
	  SET_PC (stmt);
	  break;
	case IR_NM_block:
	  {
	    IR_node_t saved_current_scope = current_scope;
	    IR_node_t curr_except;
	    IR_node_t last_except_with_block;
	    pc_t saved_current_pc = current_pc;
	    pc_t block_finish;
	    pc_t catches_finish;
	    pc_t previous_node_catch_list_pc;
	    IR_node_t func_class;

	    current_scope = stmt;
	    if ((func_class = IR_func_class_ext (stmt)) != NULL)
	      current_pc = PC (stmt);
	    else
	      SET_PC (stmt);
	    if (!IR_simple_block_flag (stmt))
	      number_of_surrounding_blocks++;
	    third_block_passing (IR_block_stmts (stmt));
	    if (!IR_simple_block_flag (stmt))
	      number_of_surrounding_blocks--;
	    current_scope = saved_current_scope;
	    block_finish = PC (create_node_with_pos (IR_NM_block_finish,
						     source_position));
	    IR_set_block (IR_POINTER (block_finish), stmt);
	    IR_set_simple_block_finish_flag (IR_POINTER (block_finish),
					     IR_simple_block_flag (stmt));
	    SET_PC (block_finish);
	    if (func_class != NULL && IR_IS_OF_TYPE (func_class, IR_NM_class)
		&& IR_IS_OF_TYPE (IR_POINTER (IR_next_pc (PC (stmt))),
				  IR_NM_block_finish))
	      IR_set_simple_class_flag (func_class, TRUE);
	    IR_set_catch_list_pc (stmt, NULL);
	    if (func_class != NULL)
	      current_pc = saved_current_pc;
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
		    assert (current_pc == catches_finish);
		    assert (IR_IS_OF_TYPE (previous_node_catch_list_pc,
					   IR_NM_block)
			    || IR_IS_OF_TYPE (previous_node_catch_list_pc,
					      IR_NM_exception));
		    IR_set_exception_class_expr
		      (curr_except,
		       third_expr_processing
		       (IR_exception_class_expr (curr_except), FALSE));
		    type_test (IR_exception_class_expr (curr_except),
			       EVT_CLASS, ERR_invalid_catch_expr_type);
		    IR_set_catch_list_pc (previous_node_catch_list_pc,
					  IR_next_pc (catches_finish));
		    SET_PC (curr_except);
		    previous_node_catch_list_pc = current_pc;
		    if (IR_catch_block (curr_except) != NULL)
		      {
			last_except_with_block = curr_except;
			third_block_passing (IR_catch_block (curr_except));
			SET_PC (catches_finish);
		      }
		    else
		      {
			assert (last_except_with_block != NULL);
			IR_set_next_pc (curr_except,
					IR_next_pc (last_except_with_block));
			current_pc = catches_finish;
		      }
		  }
		assert (current_pc == catches_finish);
		IR_set_next_pc (catches_finish, NULL);
	      }
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
	    assert (block != NULL && IR_NODE_MODE (block) == IR_NM_block);
	    define_block_decl (stmt, block);
	    break;
	  }
	default:
	  assert (FALSE);
	}
    }
}

#ifndef NO_OPTIMIZE
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
      case IR_NM_logical_or_end:
      case IR_NM_logical_and_end:
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

/* The following recursive function processes short path operations to
   remove nodes unnecessary for execution. */
static void
fourth_expr_processing (IR_node_t expr)
{
  if (expr == NULL)
    return;
  if (IR_IS_OF_TYPE (expr, IR_NM_generic_pos))
    IR_set_next_pc (expr, go_through (IR_next_pc (expr)));
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
    case IR_NM_ident:
    case IR_NM_var_occurrence:
    case IR_NM_lvalue_var_occurrence:
    case IR_NM_lvalue_var_occurrence_and_val:
    case IR_NM_local_var_occurrence:
    case IR_NM_local_lvalue_var_occurrence:
    case IR_NM_local_lvalue_var_occurrence_and_val:
    case IR_NM_external_var_occurrence:
    case IR_NM_lvalue_external_var_occurrence:
    case IR_NM_lvalue_external_var_occurrence_and_val:
    case IR_NM_external_func_occurrence:
    case IR_NM_func_occurrence:
    case IR_NM_class_occurrence:
      break;
    case IR_NM_period:
    case IR_NM_lvalue_period:
    case IR_NM_lvalue_period_and_val:
      fourth_expr_processing (IR_left_operand (expr));
      break;
    case IR_NM_not:
    case IR_NM_unary_plus:
    case IR_NM_unary_minus:
    case IR_NM_bitwise_not:
    case IR_NM_length:
    case IR_NM_typeof:
    case IR_NM_charof:
    case IR_NM_intof:
    case IR_NM_floatof:
    case IR_NM_vectorof:
    case IR_NM_tableof:
    case IR_NM_funcof:
    case IR_NM_threadof:
    case IR_NM_classof:
    case IR_NM_new:
    case IR_NM_const:
      fourth_expr_processing (IR_operand (expr));
      break;
    case IR_NM_format_vectorof:
      fourth_expr_processing (IR_operand (expr));
      fourth_expr_processing (IR_format (expr));
      break;
    case IR_NM_logical_or:
    case IR_NM_logical_and:
      fourth_expr_processing (IR_left_operand (expr));
      fourth_expr_processing (IR_right_operand (expr));
      IR_set_short_path_pc (expr, go_through (IR_short_path_pc (expr)));
      break;
    case IR_NM_eq:
    case IR_NM_ne:
    case IR_NM_identity:
    case IR_NM_unidentity:
    case IR_NM_lt:
    case IR_NM_gt:
    case IR_NM_le:
    case IR_NM_ge:
    case IR_NM_concat:
    case IR_NM_in:
    case IR_NM_or:
    case IR_NM_xor:
    case IR_NM_and:
    case IR_NM_lshift:
    case IR_NM_rshift:
    case IR_NM_ashift:
    case IR_NM_plus:
    case IR_NM_minus:
    case IR_NM_mult:
    case IR_NM_div:
    case IR_NM_mod:
    case IR_NM_index:
    case IR_NM_lvalue_index:
    case IR_NM_lvalue_index_and_val:
    case IR_NM_key_index:
    case IR_NM_lvalue_key_index:
    case IR_NM_lvalue_key_index_and_val:
      fourth_expr_processing (IR_left_operand (expr));
      fourth_expr_processing (IR_right_operand (expr));
      break;
    case IR_NM_cond:
      fourth_expr_processing (IR_cond (expr));
      fourth_expr_processing (IR_left_operand (expr));
      fourth_expr_processing (IR_right_operand (expr));
      IR_set_false_path_pc (expr, go_through (IR_false_path_pc (expr)));
      break;
    case IR_NM_vector:
    case IR_NM_table:
      {
	IR_node_t elist;
	
	for (elist = IR_elist (expr);
	     elist != NULL;
	     elist = IR_next_elist (elist))
	  {
	    fourth_expr_processing (IR_repetition_key (elist));
            fourth_expr_processing (IR_expr (elist));
	  }
	break;  
      }
    case IR_NM_class_func_thread_call:
      {
	IR_node_t elist;
	
        fourth_expr_processing (IR_func_expr (expr));
	for (elist = IR_actuals (expr);
	     elist != NULL;
	     elist = IR_next_elist (elist))
          fourth_expr_processing (IR_expr (elist));
	break;  
      }
    default:
      assert (FALSE);
    }
  return;
}

/* The following recursive function removes nodes unnecessary for
   execution. */
static void
fourth_block_passing (IR_node_t first_level_stmt)
{
  IR_node_t stmt;

  for (stmt = first_level_stmt; stmt != NULL; stmt = IR_next_stmt (stmt))
    {
      switch (IR_NODE_MODE (stmt))
	{
	case IR_NM_assign:
	case IR_NM_var_assign:
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_rem_assign:
	case IR_NM_plus_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	case IR_NM_par_assign:
          fourth_expr_processing (IR_assignment_var (stmt));
	  fourth_expr_processing (IR_assignment_expr (stmt));
	  break;
	case IR_NM_procedure_call:
	  {
	    IR_node_t elist;
	    
	    fourth_expr_processing (IR_proc_expr (stmt));
	    for (elist = IR_proc_actuals (stmt);
		 elist != NULL;
		 elist = IR_next_elist (elist))
              fourth_expr_processing (IR_expr (elist));
	    break;  
	  }
	case IR_NM_if_stmt:
          fourth_expr_processing (IR_if_expr (stmt));
          fourth_block_passing (IR_if_part (stmt));
          fourth_block_passing (IR_else_part (stmt));
          IR_set_else_part_pc (stmt, go_through (IR_else_part_pc (stmt)));
          IR_set_common_part_pc (stmt, go_through (IR_common_part_pc (stmt)));
          break;
	case IR_NM_for_stmt:
          fourth_block_passing (IR_for_initial_stmt (stmt));
          fourth_expr_processing (IR_for_guard_expr (stmt));
          fourth_block_passing (IR_for_iterate_stmt (stmt));
          fourth_block_passing (IR_for_stmts (stmt));
          IR_set_for_body_pc (stmt, go_through (IR_for_body_pc (stmt)));
          break;
	case IR_NM_foreach_stmt:
          fourth_expr_processing (IR_foreach_designator (stmt));
          fourth_block_passing (IR_foreach_stmts (stmt));
          IR_set_foreach_body_pc (stmt,
				  go_through (IR_foreach_body_pc (stmt)));
          break;
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	  break;
	case IR_NM_return_without_result:
	case IR_NM_return_with_result:
	  {
	    if (IR_NODE_MODE (stmt) == IR_NM_return_with_result)
              fourth_expr_processing (IR_returned_expr (stmt));
            break;
	  }
	case IR_NM_wait_stmt:
          fourth_expr_processing (IR_wait_guard_expr (stmt));
          IR_set_start_wait_guard_expr_pc
            (stmt, go_through (IR_start_wait_guard_expr_pc (stmt)));
	  fourth_block_passing (IR_wait_stmt (stmt));
          break;
	case IR_NM_throw:
	  fourth_expr_processing (IR_throw_expr (stmt));
	  break;
	case IR_NM_block:
	  {
	    IR_node_t curr_except;

	    fourth_block_passing (IR_block_stmts (stmt));
            IR_set_catch_list_pc (stmt, go_through (IR_catch_list_pc (stmt)));
            for (curr_except = IR_exceptions (stmt);
                 curr_except != NULL;
                 curr_except = IR_next_exception (curr_except))
              {
                fourth_expr_processing (IR_exception_class_expr (curr_except));
                fourth_block_passing (IR_catch_block (curr_except));
                IR_set_catch_list_pc
                  (curr_except, go_through (IR_catch_list_pc (curr_except)));
	      }
	    break;
	  }
	case IR_NM_var:
	case IR_NM_external_var:
	case IR_NM_external_func:
	case IR_NM_func:
	case IR_NM_class:
          break;
	default:
	  assert (FALSE);
	}
      assert (IR_IS_OF_TYPE (stmt, IR_NM_generic_pos));
      IR_set_next_pc (stmt, go_through (IR_next_pc (stmt)));
    }
}
#endif

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
static int current_node_num;

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
  entry = find_hash_table_entry (node_info_tab, &node_info, FALSE);
  if (*entry != NULL)
    return *(struct node_info **) entry;
  OS_TOP_EXPAND (node_info_os, sizeof (struct node_info));
  ni = OS_TOP_BEGIN (node_info_os);
  OS_TOP_FINISH (node_info_os);
  ni->n = n;
  ni->traverse_flag = FALSE;
  ni->num = ++current_node_num;
  *entry = ni;
  return ni;
}

/* Create the node info table. */
static void
initiate_node_tab (void)
{
  OS_CREATE (node_info_os, 0);
  node_info_tab = create_hash_table (400, node_info_hash, node_info_eq);
  current_node_num = 0;
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
  int first_p = TRUE;
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
      if (! first_p && IR_pos (cn).file_name == ENVIRONMENT_PSEUDO_FILE_NAME)
	{
	  cn = IR_next_pc (cn);
	  continue;
	}
      first_p = FALSE;
      node_mode = IR_NODE_MODE (cn);
      print_indent (indent);
      printf ("%6d %s", ni->num, IR_node_name[node_mode]);
      last_goto_flag = FALSE;
      assert (indent >= 0);
      if (curr_line_number != IR_pos (cn).line_number)
	{
	  curr_line_number = IR_pos (cn).line_number;
	  printf ("(line=%u)", curr_line_number);
	}
      line_flag = TRUE;
      switch (node_mode)
	{
	case IR_NM_char:
	  printf ("(%x)", IR_ch_val (cn));
	  if (isgraph (IR_ch_val (cn)))
	    printf ("(%c -- %x)", IR_ch_val (cn), IR_ch_val (cn));
	  else
	    printf ("(%x)", IR_ch_val (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_int:
	  printf ("(%d)", IR_i_val (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_float:
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
	case IR_NM_nil:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_string:
	  {
	    const char *s;

	    printf ("(\"");
	    for (s = IR_str_val (cn); *s != '\0'; s++)
	      printf ("%s", get_ch_repr (*s));
	    printf ("\")");
	    cn = IR_next_pc (cn);
	    break;
	  }
	case IR_NM_period:
	case IR_NM_no_testing_period:
	case IR_NM_lvalue_period:
	case IR_NM_lvalue_no_testing_period:
	case IR_NM_lvalue_period_and_val:
	case IR_NM_lvalue_no_testing_period_and_val:
	  printf ("(%s, block_decl_ident_num=%d)",
		  IR_ident_string (IR_unique_ident (IR_right_operand (cn))),
		  IR_right_block_decl_ident_number (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_logical_or:
	case IR_NM_logical_and:
	  printf ("(%d)", find_node_info (IR_short_path_pc (cn))->num);
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_logical_or_end:
	case IR_NM_logical_and_end:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_in:
	case IR_NM_not:
	case IR_NM_bitwise_not:
	case IR_NM_eq:
	case IR_NM_ne:
	case IR_NM_identity:
	case IR_NM_unidentity:
	case IR_NM_lt:
	case IR_NM_ge:
	case IR_NM_gt:
	case IR_NM_le:
	case IR_NM_unary_plus:
	case IR_NM_unary_minus:
	case IR_NM_length:
	case IR_NM_const:
	case IR_NM_new:
	case IR_NM_typeof:
	case IR_NM_charof:
	case IR_NM_intof:
	case IR_NM_floatof:
	case IR_NM_format_vectorof:
	case IR_NM_vectorof:
	case IR_NM_tableof:
	case IR_NM_funcof:
	case IR_NM_threadof:
	case IR_NM_classof:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_cond:
	  printf ("(false=%d)", find_node_info (IR_false_path_pc (cn)));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_cond_end:
	case IR_NM_par_assign_end:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_vector:
	case IR_NM_table:
	  printf ("(parts=%d)", IR_parts_number (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_index:
	case IR_NM_lvalue_index:
	case IR_NM_lvalue_index_and_val:
	case IR_NM_key_index:
	case IR_NM_lvalue_key_index:
	case IR_NM_lvalue_key_index_and_val:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_class_func_thread_call:
	  printf ("(pars=%d)",
		  IR_class_func_thread_call_parameters_number (cn));
	  cn = IR_next_pc (cn);
	  break;
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
	case IR_NM_mult_assign:
	case IR_NM_div_assign:
	case IR_NM_rem_assign:
	case IR_NM_minus_assign:
	case IR_NM_concat_assign:
	case IR_NM_lshift_assign:
	case IR_NM_rshift_assign:
	case IR_NM_ashift_assign:
	case IR_NM_and_assign:
	case IR_NM_xor_assign:
	case IR_NM_or_assign:
	case IR_NM_plus_assign:
	case IR_NM_assign:
	case IR_NM_var_assign:
	case IR_NM_par_assign:
	case IR_NM_swap:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_par_assign_test:
	  printf ("(skip=%d)",
		  find_node_info (IR_skip_par_assign_path_pc (cn))->num);
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_procedure_call:
	  printf ("(pars=%d)", IR_procedure_call_pars_number (cn));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_if_stmt:
	  {
	    IR_node_t cp;

	    indent += 2;
	    printf ("(else=%d)\n", find_node_info (IR_else_part_pc (cn))->num);
	    cp = IR_common_part_pc (cn);
	    find_node_info (cp)->traverse_flag = TRUE;
	    dump_code (indent, IR_next_pc (cn), NULL);
	    find_node_info (cp)->traverse_flag = FALSE;
	    dump_code (indent, IR_else_part_pc (cn), cp);
	    line_flag = FALSE;
	    indent -= 2;
	    cn = cp;
	    break;
	  }
	case IR_NM_for_stmt:
#ifndef NO_OPTIMIZE
	  indent += 2;
#endif
	  printf ("(end=%d)\n", find_node_info (IR_next_pc (cn))->num);
	  dump_code (indent, IR_for_body_pc (cn), NULL);
	  line_flag = FALSE;
#ifndef NO_OPTIMIZE
	  indent -= 2;
#endif
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
	  printf ("(end=%d)\n", find_node_info (IR_next_pc (cn))->num);
	  dump_code (indent, IR_foreach_body_pc (cn), NULL);
	  line_flag = FALSE;
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_block_finish:
#ifndef NO_OPTIMIZE
	  if (!IR_simple_block_finish_flag (cn))
#endif
	    indent -= 2;
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_return_without_result:
	case IR_NM_return_with_result:
	  cn = IR_next_pc (cn);
	  break;

	case IR_NM_wait_stmt:
	  printf ("(guard_expr=%d)",
		  find_node_info (IR_start_wait_guard_expr_pc (cn))->num);
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_wait_finish:
          cn = IR_next_pc (cn);
	  break;
	case IR_NM_block:
#ifndef NO_OPTIMIZE
	  if (!IR_simple_block_flag (cn))
#endif
	    {
	      IR_node_t stmt;
	      bool_t first_p = TRUE;

	      indent += 2;
	      for (stmt = IR_block_stmts (cn);
		   stmt != NULL;
		   stmt = IR_next_stmt (stmt))
		if (IR_IS_OF_TYPE (stmt, IR_NM_func_or_class)
		    && IR_pos (stmt).file_name != ENVIRONMENT_PSEUDO_FILE_NAME)
		  {
		    if (first_p)
		      {
			printf ("\n");
			first_p = FALSE;
		      }
		    dump_code (indent, stmt, NULL);
		  }
	    }
	  if ((cl = IR_catch_list_pc (cn)) == NULL)
	    cn = IR_next_pc (cn);
	  else
	    {
	      IR_node_t fn;

	      for (fn = IR_next_pc (cn); fn != NULL; fn = IR_next_pc (fn))
		if (IR_NODE_MODE (fn) == IR_NM_block_finish && IR_block (fn) == cn)
		  break;
	      assert (fn != NULL);
	      printf ("(catch = %d)\n", find_node_info (cl)->num);
	      cf = IR_next_pc (fn);
	      dump_code (indent, IR_next_pc (cn), cf);
	      line_flag = FALSE;
	      cn = cl;
	    }
	  break;
	case IR_NM_throw:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_exception:
	  if ((cl = IR_catch_list_pc (cn)) == NULL)
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
	case IR_NM_local_var_occurrence:
	case IR_NM_local_lvalue_var_occurrence:
	case IR_NM_local_lvalue_var_occurrence_and_val:
	case IR_NM_var_occurrence:
	case IR_NM_lvalue_var_occurrence:
	case IR_NM_lvalue_var_occurrence_and_val:
	case IR_NM_external_var_occurrence:
	case IR_NM_lvalue_external_var_occurrence:
	case IR_NM_lvalue_external_var_occurrence_and_val:
	case IR_NM_external_func_occurrence:
	case IR_NM_func_occurrence:
	case IR_NM_class_occurrence:
	  printf ("(%s)", IR_ident_string (IR_unique_ident
					   (IR_ident (IR_decl (cn)))));
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_if_finish:
	case IR_NM_for_finish:
	case IR_NM_catches_finish:
#ifdef NO_OPTIMIZE
	  indent -= 2;
#endif
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_pop_func_call:
	  cn = IR_next_pc (cn);
	  break;
	case IR_NM_func:
	case IR_NM_class:
	  printf (" %s (%d%s)",
		  IR_ident_string (IR_unique_ident (IR_ident (cn))),
		  IR_parameters_number (cn), IR_args_flag (cn) ? ", ..." : "");
	  cn = IR_next_stmt (cn);
	default:
	  assert (FALSE);
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

  current_scope = NULL;
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
  current_scope = NULL;
  second_block_passing (first_program_stmt_ptr);
  current_scope = NULL;
  current_pc = NULL;
  for_finish = NULL;
  third_block_passing (first_program_stmt_ptr);
  /* Some optimizations */
#ifndef NO_OPTIMIZE
  fourth_block_passing (first_program_stmt_ptr);
#endif
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
