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
#include "d_bc.h"
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
static BC_node_t for_finish;

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
    case IR_NM_this:
      {
	IR_node_t scope;

	for (scope = curr_scope; scope != NULL; scope = IR_block_scope (scope))
	  if (IR_func_class_ext (scope) != NULL)
	    break;
	if (scope == NULL)
	  error (FALSE, IR_pos (expr), ERR_this_outside_func_class_ext);
	else
	  IR_set_extended_life_context_flag (curr_scope, TRUE);
	break;
      }
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
    case IR_NM_concat:
    case IR_NM_mult:
    case IR_NM_div:
    case IR_NM_mod:
    case IR_NM_format_vectorof:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_left_operand (expr));
      first_expr_processing (IR_right_operand (expr));
      break;
    case IR_NM_minus:
      {
	IR_node_t r = IR_right_operand (expr);

	SET_SOURCE_POSITION (expr);
	first_expr_processing (IR_left_operand (expr));
	if (r != NULL && IR_IS_OF_TYPE (r, IR_NM_int)
	    && IR_int_value (IR_unique_int (r)) != MAX_INT)
	  {
	    IR_set_right_operand
	      (expr,
	       get_int_node (-IR_int_value (IR_unique_int (r)), IR_pos (r)));
	    IR_SET_MODE (expr, IR_NM_plus);
	  }
	first_expr_processing (IR_right_operand (expr));
	break;
      }
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
	case IR_NM_minus_assign:
	  {
	    IR_node_t expr = IR_assignment_expr (stmt);
	    
	    first_expr_processing (IR_assignment_var (stmt));
	    if (expr != NULL && IR_IS_OF_TYPE (expr, IR_NM_int)
		&& IR_int_value (IR_unique_int (expr)) != MAX_INT)
	      {
		IR_set_assignment_expr
		  (stmt,
		   get_int_node (-IR_int_value (IR_unique_int (expr)),
				 IR_pos (expr)));
		IR_SET_MODE (stmt, IR_NM_plus_assign);
	      }
	    first_expr_processing (IR_assignment_expr (stmt));
	    break;
	  }
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
	  set_func_class_id (stmt);
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
#define ADD_TEMP_VARS_NUMBER 0

/* Make pc_t value from byte code NODE.  */
#define PC(node) ((pc_t) node)

/* This macro is used to connect byte code commands for interpreter
   (see comments for typedef pc_t) into chain by members next. */
#define SET_PC(node)\
   do {\
     prev_pc = curr_pc;\
     if (curr_pc != NULL)\
       BC_set_next (curr_pc, node);\
     curr_pc = node;\
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

static BC_node_t do_inline
new_bc_node (BC_node_mode_t bnm, IR_node_t origin)
{
  BC_node_t res = BC_create_node (bnm);

  bc_insns_num++;
  BC_set_origin (res, origin);
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
  BC_node_mode_t bc_node_mode;
  BC_node_t bc;

  SET_SOURCE_POSITION (op);
  op_result = -1;
  IR_set_operand (op, third_expr_processing (IR_operand (op), FALSE,
					     &op_result, &temp_vars_num,
					     FALSE));
  switch (IR_NODE_MODE (op))
    {
    case IR_NM_not: bc_node_mode = BC_NM_not; break;
    case IR_NM_unary_plus: bc_node_mode = BC_NM_plus; break;
    case IR_NM_unary_minus: bc_node_mode = BC_NM_minus; break;
    case IR_NM_bitwise_not: bc_node_mode = BC_NM_bnot; break;
    case IR_NM_length: bc_node_mode = BC_NM_length; break;
    case IR_NM_fold_plus: bc_node_mode = BC_NM_fadd; break;
    case IR_NM_fold_mult: bc_node_mode = BC_NM_fmult; break;
    case IR_NM_fold_and: bc_node_mode = BC_NM_fand; break;
    case IR_NM_fold_xor: bc_node_mode = BC_NM_fxor; break;
    case IR_NM_fold_or: bc_node_mode = BC_NM_for; break;
    case IR_NM_typeof: bc_node_mode = BC_NM_tpof; break;
    case IR_NM_charof: bc_node_mode = BC_NM_chof; break;
    case IR_NM_intof: bc_node_mode = BC_NM_iof; break;
    case IR_NM_floatof: bc_node_mode = BC_NM_fof; break;
    case IR_NM_vectorof: bc_node_mode = BC_NM_vecof; break;
    case IR_NM_tableof: bc_node_mode = BC_NM_tabof; break;
    case IR_NM_funcof: bc_node_mode = BC_NM_funcof; break;
    case IR_NM_threadof: bc_node_mode = BC_NM_threadof; break;
    case IR_NM_classof: bc_node_mode = BC_NM_classof; break;
    case IR_NM_new: bc_node_mode = BC_NM_new; break;
    case IR_NM_const: bc_node_mode = BC_NM_const; break;
    default:
      d_unreachable ();
    }
  bc = new_bc_node (bc_node_mode, op);
  BC_set_op2 (bc, op_result);
  BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
  SET_PC (bc);
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

/* Generate code for binary OP using NEW_OP_MODE (or REV_OP_MODE if we
   exchange operands) if an operand is an int constant (it is always
   processed second even if it is actually the first operand).  Update
   *CURR_TEMP_VARS_NUM and put place of result in *RESULT.  */
static void do_inline
process_binary_op (IR_node_t op, int *result, int *curr_temp_vars_num,
		   BC_node_mode_t new_op_mode, BC_node_mode_t rev_op_mode)
{
  int skip_left_p, skip_right_p;
  int l_op_result = -1, r_op_result = -1;
  int temp_vars_num = *curr_temp_vars_num;
  BC_node_mode_t bc_node_mode;
  BC_node_t bc;
  IR_node_t l = IR_left_operand (op), r = IR_right_operand (op);

  SET_SOURCE_POSITION (op);
  skip_left_p = skip_right_p = FALSE;
  if (new_op_mode != BC_NM__error)
    {
      if (l != NULL && IR_IS_OF_TYPE (l, IR_NM_int))
	{
	  IR_set_right_operand
	    (op, third_expr_processing (r, FALSE,
					&r_op_result, &temp_vars_num, FALSE));
	  skip_right_p = TRUE;
	  if (unary_slice_p (IR_right_operand (op)))
	    ;
	  else
	    {
	      skip_left_p = TRUE;
	      bc = new_bc_node (rev_op_mode, op);
	      BC_set_op3 (bc, IR_int_value (IR_unique_int (l)));
	      BC_set_op2 (bc, r_op_result);
	    }
	}
      else if (r != NULL && IR_IS_OF_TYPE (r, IR_NM_int))
	{
	  IR_set_left_operand
	    (op, third_expr_processing (l, FALSE,
					&l_op_result, &temp_vars_num, FALSE));
	  skip_left_p = TRUE;
	  if (unary_slice_p (IR_left_operand (op)))
	    ;
	  else
	    {
	      skip_right_p = TRUE;
	      bc = new_bc_node (new_op_mode, op);
	      BC_set_op3 (bc, IR_int_value (IR_unique_int (r)));
	      BC_set_op2 (bc, l_op_result);
	    }
	}
    }
  if (! skip_left_p || ! skip_right_p)
    {
      switch (IR_NODE_MODE (op))
	{
	case IR_NM_eq: bc_node_mode = BC_NM_eq; break;
	case IR_NM_ne: bc_node_mode = BC_NM_ne; break;
	case IR_NM_identity: bc_node_mode = BC_NM_id; break;
	case IR_NM_unidentity: bc_node_mode = BC_NM_unid; break;
	case IR_NM_lt: bc_node_mode = BC_NM_lt; break;
	case IR_NM_gt: bc_node_mode = BC_NM_gt; break;
	case IR_NM_le: bc_node_mode = BC_NM_le; break;
	case IR_NM_ge: bc_node_mode = BC_NM_ge; break;
	case IR_NM_concat: bc_node_mode = BC_NM_concat; break;
	case IR_NM_in: bc_node_mode = BC_NM_in; break;
	case IR_NM_or: bc_node_mode = BC_NM_or; break;
	case IR_NM_xor: bc_node_mode = BC_NM_xor; break;
	case IR_NM_and: bc_node_mode = BC_NM_and; break;
	case IR_NM_lshift: bc_node_mode = BC_NM_lsh; break;
	case IR_NM_rshift: bc_node_mode = BC_NM_rsh; break;
	case IR_NM_ashift: bc_node_mode = BC_NM_ash; break;
	case IR_NM_plus: bc_node_mode = BC_NM_add; break;
	case IR_NM_minus: bc_node_mode = BC_NM_sub; break;
	case IR_NM_mult: bc_node_mode = BC_NM_mult; break;
	case IR_NM_div: bc_node_mode = BC_NM_div; break;
	case IR_NM_mod: bc_node_mode = BC_NM_mod; break;
	case IR_NM_format_vectorof: bc_node_mode = BC_NM_fmtvecof; break;
	default:
	  d_unreachable ();
	}
      bc = new_bc_node (bc_node_mode, op);
      if (! skip_left_p)
	IR_set_left_operand
	  (op, third_expr_processing (l, FALSE,
				      &l_op_result, &temp_vars_num, FALSE));
      BC_set_op2 (bc, l_op_result);
      if (! skip_right_p)
	IR_set_right_operand
	  (op, third_expr_processing (r, FALSE,
				      &r_op_result, &temp_vars_num, FALSE));
      BC_set_op3 (bc, r_op_result);
    }
  BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
  SET_PC (bc);
}

/* Check EXPR whose value in OP_NUM for a slice and, if it is so, add
   flatten node.  */
static do_inline void
add_flatten_node_if_necessary (IR_node_t expr, int op_num)
{
  BC_node_t flat;

  if (expr != NULL && IR_value_type (expr) == EVT_SLICE)
    {
      flat = new_bc_node (BC_NM_flat, expr);
      BC_set_op1 (flat, op_num);
      SET_PC (flat);
    }
}

/* The func returns occurrence mode (var occurrence and etc.) for
   corresponding DECL.  DECL must be only var, func or class. */
static BC_node_mode_t
bc_decl_mode (IR_node_t decl)
{
  d_assert (decl != NULL);
  if (IR_NODE_MODE (decl) == IR_NM_var)
    return BC_NM_var;
  else if (IR_NODE_MODE (decl) == IR_NM_external_var)
    return BC_NM_evar;
  else if (IR_NODE_MODE (decl) == IR_NM_external_func)
    return BC_NM_efunc;
  else if (IR_NODE_MODE (decl) == IR_NM_func)
    return BC_NM_func;
  else if (IR_NODE_MODE (decl) == IR_NM_class)
    return BC_NM_class;
  else
    d_unreachable ();
  return BC_NM_var; /* No warnings */
}

/* The func creates bc_decl_node (var, func or class) for
   corresponding DECL.  DECL must be only var, func or class. */
static BC_node_t
create_bc_ident (IR_node_t ident)
{
  BC_node_t bc_ident;
  BC_node_mode_t node_mode;

  node_mode = bc_decl_mode (IR_decl (ident));
  bc_ident = new_bc_node (node_mode, ident);
  return bc_ident;
}

/* Process actual parameters ACTUALS and add flatten nodes if
   necessary.  Return number of actuals.  */
static int
process_actuals (IR_node_t func_decl, int func_op_num,
		 IR_node_t actuals, int *temp_vars_num)
{
  int par_num, n;
  IR_node_t elist;

  for (n = 0, elist = actuals;
       elist != NULL;
       elist = IR_next_elist (elist), n++)
    {
      SET_SOURCE_POSITION (elist);
      par_num = *temp_vars_num + curr_vars_number;
      IR_set_expr (elist, third_expr_processing (IR_expr (elist), TRUE,
						 NULL, temp_vars_num, FALSE));
      add_flatten_node_if_necessary (IR_expr (elist), par_num);
    }
  return n;
}

/* Add move of VAR (source) to the pc chain.  Define VAR_NUM and
   RESULT_NUM for the move.  */
static void
add_move (IR_node_t ident, int result_num, int var_num)
{
  BC_node_t bc = new_bc_node (BC_NM_move, ident);

  BC_set_op1 (bc, result_num);
  BC_set_op2 (bc, var_num);
  SET_PC (bc);
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
   extended_life_context). */

static IR_node_t
third_expr_processing (IR_node_t expr, int func_class_assign_p,
		       int *result, int *curr_temp_vars_num, int lvalue_p)
{
  BC_node_t bc;
  IR_node_mode_t node_mode;
  IR_node_t l, r;
  int temp_vars_num = *curr_temp_vars_num;

  if (expr == NULL)
    return expr;
  node_mode = IR_NODE_MODE (expr);
  switch (node_mode)
    {
    case IR_NM_char:
      bc = new_bc_node (BC_NM_ldch, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_op2 (bc, IR_char_value (IR_unique_char (expr)));
      IR_set_value_type (expr, EVT_CHAR);
      SET_PC (bc);
      break;
    case IR_NM_int:
      bc = new_bc_node (BC_NM_ldi, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_op2 (bc, IR_int_value (IR_unique_int (expr)));
      IR_set_value_type (expr, EVT_INT);
      SET_PC (bc);
      break;
    case IR_NM_float:
      bc = new_bc_node (BC_NM_ldf, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_f (bc, IR_float_value (IR_unique_float (expr)));
      IR_set_value_type (expr, EVT_FLOAT);
      SET_PC (bc);
      break;
    case IR_NM_string:
      bc = new_bc_node (BC_NM_lds, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_str (bc, IR_string_value (IR_unique_string (expr)));
      IR_set_value_type (expr, EVT_VECTOR);
      SET_PC (bc);
      break;
    case IR_NM_nil:
      bc = new_bc_node (BC_NM_ldnil, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_UNKNOWN);
      SET_PC (bc);
      break;
    case IR_NM_this:
      bc = new_bc_node (BC_NM_ldthis, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_UNKNOWN);
      SET_PC (bc);
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
      bc = new_bc_node (BC_NM_ldtp, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_op2 (bc, ir2er_type (node_mode));
      IR_set_value_type (expr, EVT_TYPE);
      SET_PC (bc);
      break;
    case IR_NM_ident:
      {
	IR_node_t ident;
	IR_node_t decl;

	ident = expr;
	decl = find_decl (expr, curr_scope);
	IR_set_decl (expr, decl);
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
	if (IR_NODE_MODE (decl) == IR_NM_var
	    && curr_real_scope == IR_scope (decl)
	    && (IR_func_class_ext (curr_real_scope) == NULL
		|| ! IR_IS_OF_TYPE (IR_func_class_ext (curr_real_scope),
				    IR_NM_class)))
	  {
	    /* local var */
	    if (result == NULL)
	      add_move (ident,
			setup_result_var_number (result, curr_temp_vars_num),
			IR_var_number_in_block (decl));
	    else
	      *result = IR_var_number_in_block (decl);
	  }
	else
	  {
	    bc = create_bc_ident (ident);
	    if (! lvalue_p)
	      {
		BC_set_op1
		  (bc, setup_result_var_number (result, curr_temp_vars_num));
		SET_PC (bc);
	      }
	    else
	      {
		d_assert (result != NULL && *result < 0);
		/* We need 2 stack slots for non local var occurrence
		   lvalue representation.  */
		BC_set_op1
		  (bc, setup_result_var_number (result, curr_temp_vars_num));
		get_temp_stack_slot (curr_temp_vars_num);
		SET_PC (bc);
	      }
	  }
	if (IR_NODE_MODE (decl) == IR_NM_func)
	  IR_set_value_type (expr, EVT_FUNC);
	else if (IR_NODE_MODE (decl) == IR_NM_class)
	  IR_set_value_type (expr, EVT_CLASS);
	else
	  IR_set_value_type (expr, EVT_UNKNOWN);
	if (func_class_assign_p
	    && (IR_IS_OF_TYPE (decl, IR_NM_func)
		|| IR_IS_OF_TYPE (decl, IR_NM_class)))
	  IR_set_extended_life_context_flag (curr_real_scope, TRUE);
	break;
      }
    case IR_NM_period:
      {
	int op_result = -1;

	d_assert (! lvalue_p || result == NULL || *result < 0);
	SET_SOURCE_POSITION (expr);
	bc = new_bc_node (BC_NM_fld, expr);
	IR_set_designator
	  (expr, third_expr_processing (IR_designator (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	BC_set_op2 (bc, op_result);
	if (lvalue_p)
	  {
	    BC_set_op1 (bc, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  BC_set_op1
	    (bc, setup_result_var_number (result, curr_temp_vars_num));
	BC_set_op3 (bc, IR_right_block_decl_ident_number (expr));
	SET_PC (bc);
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
	BC_node_t lconv;
	int op_result;

	SET_SOURCE_POSITION (expr);
	op_result = (result == NULL ? -1 : *result);
	IR_set_operand
	  (expr, third_expr_processing (IR_operand (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	bc = new_bc_node (node_mode == IR_NM_logical_or ? BC_NM_brts : BC_NM_brfs,
			  expr);
	BC_set_op1 (bc, op_result);
	BC_set_res (bc, setup_result_var_number (result, curr_temp_vars_num));
	SET_PC (bc);
	lconv = new_bc_node (BC_NM_lconv, expr);
        BC_set_pc (bc, lconv);
	op_result = (result == NULL ? -1 : *result);
	temp_vars_num = *curr_temp_vars_num;
	IR_set_cont_operand
	  (expr, third_expr_processing (IR_cont_operand (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	BC_set_op2 (lconv, op_result);
	BC_set_op1 (lconv, BC_op1 (bc));
	type_test (IR_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_logical_operation_operand_type);
	type_test (IR_cont_operand (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_logical_operation_operand_type);
	IR_set_value_type (expr, EVT_INT);
	SET_PC (lconv);
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
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_eqi, BC_NM_eqi);
      goto common_eq;
    case IR_NM_ne:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_nei, BC_NM_nei);
      goto common_eq;
    case IR_NM_identity:
    case IR_NM_unidentity:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
    common_eq:
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_lt:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_lti, BC_NM_gei);
      goto common_cmp;
    case IR_NM_gt:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_gti, BC_NM_lei);
      goto common_cmp;
    case IR_NM_le:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_lei, BC_NM_gti);
      goto common_cmp;
    case IR_NM_ge:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_gei, BC_NM_lti);
    common_cmp:
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_order_comparison_operation_operand_type);
      type_test (r, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_order_comparison_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_concat:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_concat_operation_operand_type);
      type_test (r, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_concat_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_VECTOR);
      break;
    case IR_NM_in:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
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
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      type_test (r, EVT_NUMBER_VECTOR_SLICE_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_plus:
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_addi, BC_NM_addi);
      goto common_ar_op;
    case IR_NM_minus:
    case IR_NM_mult:
    case IR_NM_div:
    case IR_NM_mod:
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
    common_ar_op:
      {
	type_mask_t type_mask1;
	type_mask_t type_mask2;
	
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
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
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
	BC_node_t cend;
        pc_t true_path_begin_pc, saved_prev_pc;
	int op_result;

        SET_SOURCE_POSITION (expr);
	op_result = -1;
        IR_set_cond_expr
	  (expr, third_expr_processing (IR_cond_expr (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	bc = new_bc_node (BC_NM_bfni, expr);
	BC_set_op1 (bc, op_result);
	saved_prev_pc = curr_pc;
	SET_PC (bc);
	cend = new_bc_node (BC_NM_cend, expr);
	IR_set_cond_common_part_pc (expr, cend);
	temp_vars_num = *curr_temp_vars_num;
	/* Put it on stack.  */
        IR_set_true_expr
	  (expr, third_expr_processing (IR_true_expr (expr),
					func_class_assign_p,
					NULL, &temp_vars_num, FALSE));
	d_assert (temp_vars_num = *curr_temp_vars_num + 1);
	SET_PC (cend);
	true_path_begin_pc = BC_next (bc);
	prev_pc = saved_prev_pc;
        curr_pc = PC (bc);
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
	SET_PC (cend);
	BC_set_pc (bc, BC_next (bc));
        BC_set_next (bc, true_path_begin_pc);
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
	int n = 0;
	IR_node_t elist;
	
	SET_SOURCE_POSITION (expr);
	bc = new_bc_node (node_mode == IR_NM_vector ? BC_NM_vec : BC_NM_tab,
			  expr);
	BC_set_op2 (bc, *curr_temp_vars_num + curr_vars_number);
	BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
	for (elist = IR_elist (expr);
	     elist != NULL;
	     elist = IR_next_elist (elist))
	  {
	    n++;
	    SET_SOURCE_POSITION (elist);
	    IR_set_repetition_key
	      (elist,
	       third_expr_processing (IR_repetition_key (elist), FALSE,
				      NULL, &temp_vars_num, FALSE));
            if (node_mode == IR_NM_vector)
              type_test (IR_repetition_key (elist), EVT_NUMBER_STRING_MASK,
                         ERR_invalid_repetition_type);
	    IR_set_expr
	      (elist, third_expr_processing (IR_expr (elist), TRUE,
					     NULL, &temp_vars_num, FALSE));
	  }
	BC_set_op3 (bc, n);
	IR_set_value_type (expr,
			   node_mode == IR_NM_vector
			   ? EVT_VECTOR : EVT_TABLE);
	SET_PC (bc);
	break; 
      }
    case IR_NM_index:
    case IR_NM_key_index:
      {
	int op_result = -1;

	d_assert (! lvalue_p || result == NULL || *result < 0);
	SET_SOURCE_POSITION (expr);
	bc = new_bc_node (node_mode == IR_NM_index ? BC_NM_ind : BC_NM_key,
			  expr);
	IR_set_designator
	  (expr, third_expr_processing (IR_designator (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	BC_set_op2 (bc, op_result);
	if (lvalue_p)
	  *curr_temp_vars_num = temp_vars_num;
	op_result = -1;
	IR_set_component
	  (expr, third_expr_processing (IR_component (expr), FALSE,
					&op_result, &temp_vars_num, FALSE));
	BC_set_op3 (bc, op_result);
	if (lvalue_p)
	  {
	    BC_set_op1 (bc, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
	SET_PC (bc);
	if (node_mode == IR_NM_index)
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
	bc = new_bc_node (BC_NM_sl, expr);
	if (des != NULL)
	  {
	    BC_set_op3 (bc, 1); /* dimension */
	    BC_set_op2 (bc, temp_vars_num + curr_vars_number);
	    des = third_expr_processing (des, FALSE, NULL, &temp_vars_num,
					 IR_IS_OF_TYPE (des, IR_NM_slice));
	    if (des != NULL && IR_IS_OF_TYPE (des, IR_NM_slice))
	      {
		d_assert (BC_NODE_MODE (curr_pc) == BC_NM_sl);
		BC_set_op3 (bc, BC_op3 (curr_pc) + 1); /* dimension */
		BC_set_op2 (bc, BC_op2 (curr_pc));
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
	    BC_set_op1 (bc, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
	SET_PC (bc);
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
	int pars_num, func_op_num;
	pc_t saved_prev_pc;
	IR_node_t elist, flatten, func_decl = NULL;

	there_is_function_call_in_expr = TRUE;
	SET_SOURCE_POSITION (expr);
	func_op_num = temp_vars_num + curr_vars_number;
	saved_prev_pc = curr_pc;
	IR_set_func_expr
	  (expr, third_expr_processing (IR_func_expr (expr), FALSE,
					NULL, &temp_vars_num, FALSE));
	if (BC_NODE_MODE (curr_pc) == BC_NM_func
	    && ! IR_thread_flag (IR_decl (BC_origin (curr_pc))))
	  {
	    /* Remove load func value.  */
	    d_assert (func_op_num - curr_vars_number + 1 == temp_vars_num);
	    temp_vars_num--;
	    func_decl = IR_decl (BC_origin (curr_pc));
	    curr_pc = prev_pc;
	    prev_pc = saved_prev_pc;
	  }
	pars_num = process_actuals (func_decl, func_op_num,
				    IR_actuals (expr), &temp_vars_num);
	if (result != NULL)
	  *result = -1;
	setup_result_var_number (result, curr_temp_vars_num);
	if (func_decl == NULL)
	  bc = new_bc_node (BC_NM_call, expr);
	else
	  {
	    if (pars_num == 0)
	      get_temp_stack_slot (&temp_vars_num); /* for result */
	    bc = new_bc_node (IR_block_scope (IR_scope (func_decl)) == NULL
			      ? BC_NM_ticall
			      : IR_scope (func_decl) == curr_real_scope
			      ? BC_NM_cicall : BC_NM_icall,
			      expr);
	    BC_set_func (bc, func_decl);
	  }
	BC_set_op1 (bc, func_op_num);
	BC_set_op2 (bc, pars_num);
	SET_PC (bc);
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

   BC_NM_fld                  BC_NM_lfld(v)
   BC_NM_ind                  BC_NM_lindv
   BC_NM_key                  BC_NM_lkeyv
   BC_NM_var                  BC_NM_lvar(v)
   BC_NM_evar                 BC_NM_levar(v)

   There is no change for local_var.  If DESIGNATOR is not
   correct, generate ERROR_MESSAGE. */

static IR_node_mode_t
make_designator_lvalue (BC_node_t designator, const char *error_message,
			int val_p)
{
  switch (BC_NODE_MODE (designator))
    {
    case BC_NM_fld:
      return (val_p ? BC_NM_lfldv : BC_NM_lfld);
    case BC_NM_ind:
      return (val_p ? BC_NM_lindv : BC_NM_ind);
    case BC_NM_sl:
      return (val_p ? BC_NM_lslv : BC_NM_sl);
    case BC_NM_key:
      return (val_p ? BC_NM_lkeyv : BC_NM_key);
    case BC_NM_var:
      return (val_p ? BC_NM_lvarv : BC_NM_lvar);
    case BC_NM_evar:
      return (val_p ? BC_NM_levarv : BC_NM_lvar);
    default:
      error (FALSE, IR_pos (BC_origin (designator)), error_message);
      return BC_NODE_MODE (designator);
    }
}

/* Return the corresponding binary operation mode to an assignment
   node ASSIGN.  */
static BC_node_mode_t
make_op_mode (IR_node_t assign)
{
  switch (IR_NODE_MODE (assign))
    {
    case IR_NM_mult_assign: return BC_NM_mult;
    case IR_NM_div_assign: return BC_NM_div;
    case IR_NM_mod_assign: return BC_NM_mod;
    case IR_NM_plus_assign: return BC_NM_add;
    case IR_NM_minus_assign: return BC_NM_sub;
    case IR_NM_concat_assign: return BC_NM_concat;
    case IR_NM_lshift_assign: return BC_NM_lsh;
    case IR_NM_rshift_assign: return BC_NM_rsh;
    case IR_NM_ashift_assign: return BC_NM_ash;
    case IR_NM_and_assign: return BC_NM_and;
    case IR_NM_xor_assign: return BC_NM_xor;
    case IR_NM_or_assign: return BC_NM_or;
    default:
      d_unreachable ();
    }
}

/* Return the corresponding slice assign mode for assignment ASSIGN.  */
static BC_node_mode_t
slice_assign_mode (BC_node_t assign)
{
  switch (BC_NODE_MODE (assign))
    {
    case BC_NM_mult_st: return BC_NM_mult_slst;
    case BC_NM_div_st: return BC_NM_div_slst;
    case BC_NM_mod_st: return BC_NM_mod_slst;
    case BC_NM_add_st: return BC_NM_add_slst;
    case BC_NM_sub_st: return BC_NM_sub_slst;
    case BC_NM_concat_st: return BC_NM_concat_slst;
    case BC_NM_lsh_st: return BC_NM_lsh_slst;
    case BC_NM_rsh_st: return BC_NM_rsh_slst;
    case BC_NM_ash_st: return BC_NM_ash_slst;
    case BC_NM_and_st: return BC_NM_and_slst;
    case BC_NM_xor_st: return BC_NM_xor_slst;
    case BC_NM_or_st: return BC_NM_or_slst;
    default: return BC_NM_slst;
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


/* Return location and lvalue (through CONTAINER_NUM, INDEX_NUM, and
   LVALUE_VAL_NUM) of a lvalue and value DESIGNATOR.  If the
   designator is local var occurrence, create first corresponding
   lvarv node and add it to the pc chain.  */
static void
get_lvalue_and_val_location (BC_node_t designator, IR_node_t ir_des,
			     int *temp_vars_num, int *container_num,
			     int *index_num, int *lvalue_val_num)
{
  BC_node_t bc;
  
  if (designator == NULL)
    {
      d_assert (ir_des != NULL && IR_IS_OF_TYPE (ir_des, IR_NM_ident));
      bc = new_bc_node (BC_NM_lvarv, ir_des);
      SET_PC (bc);
      BC_set_op1 (bc, get_temp_stack_slot (temp_vars_num));
      *container_num = BC_op1 (bc);
      *index_num = get_temp_stack_slot (temp_vars_num);
      *lvalue_val_num = get_temp_stack_slot (temp_vars_num);
    }
  else if (BC_IS_OF_TYPE (designator, BC_NM_lindv)
	   || BC_IS_OF_TYPE (designator, BC_NM_lkeyv))
    {
      *container_num = BC_op2 (designator);
      *index_num = BC_op3 (designator);
      *lvalue_val_num = get_temp_stack_slot (temp_vars_num);
      BC_set_op1 (designator, *lvalue_val_num);
    }
  else if (BC_IS_OF_TYPE (designator, BC_NM_lfldv))
    {
      BC_set_op1 (designator, get_temp_stack_slot (temp_vars_num));
      *container_num = BC_op1 (designator);
      *index_num = get_temp_stack_slot (temp_vars_num);
      *lvalue_val_num = get_temp_stack_slot (temp_vars_num);
    }
  else
    {
      *container_num = BC_op1 (designator);
      *index_num = BC_op1 (designator) + 1;
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

/* All declarations of funcs and classes with blocks.  */
static vlo_t all_funcs_and_classes;

/* This recursive func passes (correctly setting up SOURCE_POSITION
   and curr_scope (before first call of the func curr_scope is to be
   NULL)) all stmts on the same stmt nesting level and sets up
   elements values of class decls idents tables (see also commentaries
   for corresponding abstract data).  FIRST_LEVEL_STMT (it may be
   NULL) is first stmt of the processed stmt nesting level.  

   The func collects all funcs and classes in all_funcs_and_classes.

   The func creates the byte code for execution of the processed
   stmts.

   The funcs also fixes repeated errors ERR_continue_is_not_in_loop,
   ERR_break_is_not_in_loop. */
static void
third_block_passing (IR_node_t first_level_stmt)
{
  BC_node_t bc, var_bc, before_pc;
  BC_node_mode_t bc_node_mode, var_mode;
  IR_node_t stmt;
  IR_node_t op, temp;
  IR_node_mode_t stmt_mode;
  int result, var_result, temp_vars_num, val_p;
  int container_num, index_num, lvalue_val_num;

  for (stmt = first_level_stmt; stmt != NULL; stmt = IR_next_stmt (stmt))
    {
      temp_vars_num = 0;
      SET_SOURCE_POSITION (stmt);
      switch (stmt_mode = IR_NODE_MODE (stmt))
	{
	case IR_NM_assign:
	case IR_NM_var_assign:
	  bc_node_mode = BC_NM_st;
	  goto common_assign;
	case IR_NM_mult_assign:
	  bc_node_mode = BC_NM_mult_st;
	  goto common_assign;
	case IR_NM_div_assign:
	  bc_node_mode = BC_NM_div_st;
	  goto common_assign;
	case IR_NM_mod_assign:
	  bc_node_mode = BC_NM_mod_st;
	  goto common_assign;
	case IR_NM_plus_assign:
	  bc_node_mode = BC_NM_add_st;
	  goto common_assign;
	case IR_NM_minus_assign:
	  bc_node_mode = BC_NM_sub_st;
	  goto common_assign;
	case IR_NM_concat_assign:
	  bc_node_mode = BC_NM_concat_st;
	  goto common_assign;
	case IR_NM_lshift_assign:
	  bc_node_mode = BC_NM_lsh_st;
	  goto common_assign;
	case IR_NM_rshift_assign:
	  bc_node_mode = BC_NM_rsh_st;
	  goto common_assign;
	case IR_NM_ashift_assign:
	  bc_node_mode = BC_NM_ash_st;
	  goto common_assign;
	case IR_NM_and_assign:
	  bc_node_mode = BC_NM_and_st;
	  goto common_assign;
	case IR_NM_xor_assign:
	  bc_node_mode = BC_NM_xor_st;
	  goto common_assign;
	case IR_NM_or_assign:
	  bc_node_mode = BC_NM_or_st;
	common_assign:
	  var_result = -1;
	  before_pc = curr_pc;
	  temp = third_expr_processing (IR_assignment_var (stmt), FALSE,
					&var_result, &temp_vars_num, TRUE);
	  if (temp != NULL)
	    IR_set_assignment_var (stmt, temp);
	  var_bc = before_pc != curr_pc ? curr_pc : NULL; /* local var */
	  bc = NULL;
	  val_p = stmt_mode != IR_NM_assign && stmt_mode != IR_NM_var_assign;
	  if (temp != NULL && var_bc != NULL)
	    {
	      bc = new_bc_node (bc_node_mode, stmt);
	      var_mode = BC_NODE_MODE (var_bc);
	      BC_SET_MODE (var_bc,
			   make_designator_lvalue
			   (var_bc, ERR_non_variable_in_assignment, val_p));
	      if (IR_IS_OF_TYPE (temp, IR_NM_index_designator))
		{
		  BC_set_op1 (bc, BC_op2 (var_bc));
		  BC_set_op2 (bc, BC_op3 (var_bc));
		  if (IR_IS_OF_TYPE (temp, IR_NM_slice))
		    BC_SET_MODE (bc, slice_assign_mode (bc));
		}
	      else if (var_mode == BC_NM_var || var_mode == BC_NM_evar)
		{
		  BC_set_op1 (bc, BC_op1 (var_bc));
		  BC_set_op2 (bc, BC_op1 (var_bc) + 1);
		}
	      if (val_p && (var_mode == BC_NM_var || var_mode == BC_NM_evar))
		BC_set_op4 (bc, get_temp_stack_slot (&temp_vars_num));
	      else if (var_mode == BC_NM_fld)
		{
		  /* container */
		  BC_set_op1 (var_bc, get_temp_stack_slot (&temp_vars_num));
		  BC_set_op1 (bc, BC_op1 (var_bc));
		  /* index: */
		  BC_set_op2 (bc, get_temp_stack_slot (&temp_vars_num));
		  if (val_p)
		    BC_set_op4 (bc, get_temp_stack_slot (&temp_vars_num));
		}
	      else if (val_p && (var_mode == BC_NM_ind
				 || var_mode == BC_NM_sl
				 || var_mode == BC_NM_key))
		{
		  BC_set_op4 (bc, get_temp_stack_slot (&temp_vars_num));
		  BC_set_op1 (var_bc, BC_op4 (bc));
		}
	      else if (val_p)
		BC_set_op4 (bc, -1);
	      if (! val_p && (var_mode == BC_NM_ind
			      || var_mode == BC_NM_sl
			      || var_mode == BC_NM_key))
		curr_pc = prev_pc;
	    }
	  if (stmt_mode != IR_NM_var_assign && temp != NULL
	      && IR_IS_OF_TYPE (temp, IR_NM_ident)
	      && (IR_IS_OF_TYPE (IR_decl (temp), IR_NM_var)
		  || IR_IS_OF_TYPE (IR_decl (temp), IR_NM_external_var))
	      && IR_const_flag (IR_decl (temp)))
	    error (FALSE, IR_pos (temp), ERR_const_assignment,
		   IR_ident_string (IR_unique_ident
				    (IR_ident (IR_decl (temp)))));
	  if (temp != NULL && var_bc == NULL
	      && (stmt_mode == IR_NM_assign || stmt_mode == IR_NM_var_assign))
	    result = var_result;
	  else
	    result = -1;
	  if (var_bc != NULL)
	    {
	      IR_set_assignment_expr
		(stmt,
		 third_expr_processing (IR_assignment_expr (stmt), TRUE,
					&result, &temp_vars_num, FALSE));
	      BC_set_op3 (bc, result);
	      if (temp != NULL && ! IR_IS_OF_TYPE (temp, IR_NM_slice))
		add_flatten_node_if_necessary (temp, result);
	      SET_PC (bc);
	    }
	  else /* assignment to the local var.  */
	    {
	      IR_node_t val = IR_assignment_expr (stmt);

	      if (stmt_mode == IR_NM_assign || stmt_mode == IR_NM_var_assign)
		{
		  IR_set_assignment_expr
		    (stmt,
		     third_expr_processing (val, TRUE,
					    &result, &temp_vars_num, FALSE));
		  if (temp != NULL && result != var_result)
		    add_move (temp, var_result, result);
		}
	      else
		{
		  IR_node_mode_t op_mode = IR_NODE_MODE (stmt);

		  if (op_mode == IR_NM_plus_assign
		      && IR_IS_OF_TYPE (val, IR_NM_int)
		      && temp != NULL && ! IR_IS_OF_TYPE (temp, IR_NM_slice))
		    {
		      bc = new_bc_node (BC_NM_addi, stmt);
		      BC_set_op3 (bc, IR_int_value (IR_unique_int (val)));
		      BC_set_op2 (bc, var_result);
		    }
		  else
		    {
		      IR_set_assignment_expr
			(stmt,
			 third_expr_processing (val, TRUE,
						&result, &temp_vars_num, FALSE));
		      /* Generate op (var_result, var_result, result).  */
		      bc = new_bc_node (make_op_mode (stmt), stmt);
		      BC_set_op2 (bc, var_result);
		      BC_set_op3 (bc, result);
		    }
		  BC_set_op1 (bc, var_result);
		  SET_PC (bc);
		}
	      if (temp != NULL && ! IR_IS_OF_TYPE (temp, IR_NM_slice))
		add_flatten_node_if_necessary (val, var_result);
	    }
	  break;
	case IR_NM_par_assign:
	  {
	    BC_node_t st_bc, aend_bc, lvalue_bc;

	    var_result = -1;
	    before_pc = curr_pc;
	    temp = third_expr_processing (IR_assignment_var (stmt), FALSE,
					  &var_result, &temp_vars_num, FALSE);
	    if (temp != NULL)
	      {
		d_assert (IR_IS_OF_TYPE (temp, IR_NM_ident));
		IR_set_assignment_var (stmt, temp);
	      }
	    var_bc = before_pc != curr_pc ? curr_pc : NULL; /* local var */
	    d_assert (temp == NULL
		      || ((var_bc == NULL
			   && var_result >= 0 && temp_vars_num == 0)
			  || var_bc != NULL));
	    if (var_bc != NULL)
	      curr_pc = prev_pc; /* Remove var.  */
	    bc = new_bc_node (BC_NM_btdef, IR_assignment_var (stmt));
	    aend_bc = new_bc_node (BC_NM_cend, stmt);
	    BC_set_brid_pc (bc, aend_bc);
	    SET_PC (bc);
	    temp_vars_num = 0;
	    lvalue_bc = NULL;
	    if (temp != NULL)
	      {
		if (var_bc == NULL)
		  result = var_result;
		else
		  {
		    result = -1;
		    lvalue_bc
		      = (new_bc_node
			 (make_designator_lvalue
			  (var_bc, ERR_non_variable_in_assignment,
			   FALSE), temp));
		    /* We need two stack slots for lvalue var occurrence.  */
		    BC_set_op1 (lvalue_bc, get_temp_stack_slot (&temp_vars_num));
		    get_temp_stack_slot (&temp_vars_num);
		    SET_PC (lvalue_bc);
		  }
	      }
	    IR_set_assignment_expr
	      (stmt, third_expr_processing (IR_assignment_expr (stmt), TRUE,
					    &result, &temp_vars_num, FALSE));
	    add_flatten_node_if_necessary (IR_assignment_expr (stmt), result);
	    if (lvalue_bc != NULL)
	      {
		st_bc = new_bc_node (BC_NM_st, stmt);
		BC_set_op1 (st_bc, BC_op1 (lvalue_bc));
		BC_set_op2 (st_bc, BC_op1 (lvalue_bc) + 1);
		BC_set_op3 (st_bc, result);
		SET_PC (st_bc);
	      }
	    else if (result != var_result)
	      add_move (temp, var_result, result);
	    SET_PC (aend_bc);
	  }
	  break;
	case IR_NM_proc_call:
	  {
	    int pars_num, func_op_num = temp_vars_num + curr_vars_number;
	    pc_t saved_prev_pc;
	    IR_node_t func_decl = NULL;

	    saved_prev_pc = curr_pc;
	    IR_set_proc_expr
	      (stmt, third_expr_processing (IR_proc_expr (stmt), FALSE,
					    NULL, &temp_vars_num, FALSE));
	    if (BC_NODE_MODE (curr_pc) == BC_NM_func
		&& ! IR_thread_flag (IR_decl (BC_origin (curr_pc))))
	      {
		/* Remove load func value.  */
		d_assert (func_op_num - curr_vars_number + 1 == temp_vars_num);
		temp_vars_num--;
		func_decl = IR_decl (BC_origin (curr_pc));
		curr_pc = prev_pc;
		prev_pc = saved_prev_pc;
	      }
	    pars_num = process_actuals (func_decl, func_op_num,
					IR_proc_actuals (stmt),
					&temp_vars_num);
	    if (func_decl == NULL)
	      bc = new_bc_node (BC_NM_pcall, stmt);
	    else
	      {
		if (pars_num == 0)
		  get_temp_stack_slot (&temp_vars_num); /* for result */
		bc = new_bc_node (IR_block_scope (IR_scope (func_decl)) == NULL
				  ? BC_NM_tipcall
				  : IR_scope (func_decl) == curr_real_scope
				  ? BC_NM_cipcall : BC_NM_ipcall,
				  stmt);
		BC_set_func (bc, func_decl);
	      }
	    BC_set_op1 (bc, func_op_num);
	    BC_set_op2 (bc, pars_num);
	    SET_PC (bc);
	  }
	  break;  
	case IR_NM_if_stmt:
	  {
	    BC_node_t if_finish;
	    pc_t if_part_begin_pc, saved_prev_pc;
	    
	    result = -1;
	    IR_set_if_expr
	      (stmt, third_expr_processing (IR_if_expr (stmt), FALSE,
					    &result, &temp_vars_num, FALSE));
	    bc = new_bc_node (BC_NM_bf, stmt);
	    BC_set_op1 (bc, result);
	    type_test (IR_if_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_if_expr_type);
	    saved_prev_pc = curr_pc;
	    SET_PC (bc);
	    if_finish = new_bc_node (BC_NM_cend, stmt);
	    IR_set_if_common_part_pc (stmt, if_finish);
	    third_block_passing (IR_if_part (stmt));
	    SET_PC (if_finish);
	    if_part_begin_pc = BC_next (bc);
	    prev_pc = saved_prev_pc;
	    curr_pc = PC (bc);
	    third_block_passing (IR_else_part (stmt));
	    SET_PC (if_finish);
	    BC_set_pc (bc, BC_next (bc));
	    BC_set_next (bc, if_part_begin_pc);
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
	    bc = new_bc_node (BC_NM_bt, stmt);
	    BC_set_op1 (bc, result);
	    type_test (IR_for_guard_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_for_guard_expr_type);
	    SET_PC (bc);
	    third_block_passing (IR_for_iterate_stmt (stmt));
	    for_iterate_start = BC_next (bc);
	    for_iterate_finish = curr_pc;
	    if (for_iterate_finish != PC (bc))
	      {
		/* There is break (although it is semantically invalid). */
		if (for_iterate_finish != NULL) 
		  BC_set_next (for_iterate_finish, BC_next (before_guard_expr));
		start_next_iteration_pc = for_iterate_start;
	      }
	    else
	      start_next_iteration_pc = BC_next (before_guard_expr);
	    for_finish = new_bc_node (BC_NM_cend, stmt);
	    curr_pc = PC (bc);
	    third_block_passing (IR_for_stmts (stmt));
	    /* The following guard is needed because the last stmt
	       may be break or continue. */
	    if (curr_pc != NULL)
	      BC_set_next (curr_pc, start_next_iteration_pc);
	    BC_set_pc (bc, BC_next (bc));
	    curr_pc = PC (bc);
	    SET_PC (for_finish);
	    number_of_surrounding_blocks = saved_number_of_surrounding_blocks;
	    start_next_iteration_pc = saved_start_next_iteration_pc;
	    for_finish = saved_for_finish;
	    break;
	  }
	case IR_NM_foreach_stmt:
	  {
	    BC_node_t foreach_start;
	    BC_node_t foreach_next_iteration;
	    pc_t before_in_expr;
	    pc_t saved_start_next_iteration_pc, saved_for_finish;
	    int saved_number_of_surrounding_blocks;
	    
	    saved_number_of_surrounding_blocks = number_of_surrounding_blocks;
	    number_of_surrounding_blocks = 0;
	    saved_start_next_iteration_pc = start_next_iteration_pc;
	    saved_for_finish = for_finish;
	    foreach_start = new_bc_node (BC_NM_pushi, stmt);
	    BC_set_op1 (foreach_start, 0);
	    SET_PC (foreach_start);
	    before_in_expr = curr_pc;
	    var_result = -1;
	    /* We need one stack slot for a flag set up by
	       foreach_start and foreach_next_iteration node
	       execution.  */
	    temp_vars_num++;
	    before_pc = curr_pc;
	    temp = third_expr_processing (IR_foreach_designator (stmt), FALSE,
					  &var_result, &temp_vars_num, TRUE);
	    var_bc = before_pc != curr_pc ? curr_pc : NULL; /* local var */
	    bc = new_bc_node (BC_NM_foreach, stmt);
	    if (var_bc != NULL)
	      BC_SET_MODE (var_bc,
			   make_designator_lvalue
			   (var_bc, ERR_non_variable_in_foreach, TRUE));
	    if (temp != NULL)
	      {
		IR_set_foreach_designator (stmt, temp);
		/* In order not to generate different foreach_stmt
		   corresponding to local non-local variables, we use
		   a general solution by representing local variables
		   by nodes lvalue_var_occurrence_and_val.  */
		get_lvalue_and_val_location (var_bc, temp, &temp_vars_num,
					     &container_num, &index_num,
					     &lvalue_val_num);
		BC_set_op2 (bc, container_num);
		BC_set_op3 (bc, index_num);
		BC_set_op4 (bc, lvalue_val_num);
	      }
	    result = -1;
	    IR_set_foreach_table
	      (stmt, third_expr_processing (IR_foreach_table (stmt), FALSE,
					    &result, &temp_vars_num, FALSE));
	    BC_set_op1 (bc, result);
	    type_test (IR_foreach_table (stmt), EVT_TABLE,
		       ERR_invalid_foreach_table_type);
	    SET_PC (bc);
	    foreach_next_iteration = new_bc_node (BC_NM_pushi, stmt);
	    BC_set_op1 (foreach_next_iteration, 1);
	    BC_set_next (foreach_next_iteration, BC_next (before_in_expr));
	    start_next_iteration_pc = PC (foreach_next_iteration);
	    for_finish = new_bc_node (BC_NM_cend, stmt);
	    third_block_passing (IR_foreach_stmts (stmt));
	    /* The following guard is needed because the last stmt may
	       be break or continue. */
	    if (curr_pc != NULL)
	      BC_set_next (curr_pc, start_next_iteration_pc);
	    BC_set_body_pc (bc, BC_next (bc));
	    curr_pc = PC (bc);
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
	      bc = new_bc_node (BC_NM_out, stmt);
	      SET_PC (bc);
	      BC_set_op1 (bc, number_of_surrounding_blocks);
	      if (stmt_mode == IR_NM_continue_stmt)
		SET_PC (start_next_iteration_pc);
	      else
		SET_PC (for_finish);
	      curr_pc = NULL;
	    }
	  break;
	case IR_NM_return_without_result:
	case IR_NM_return_with_result:
	  {
	    IR_node_t func_class_ext;
	    BC_node_mode_t bc_mode;

	    func_class_ext = find_covered_func_class_ext (curr_scope);
	    if (func_class_ext == NULL)
	      error (FALSE, source_position,
		     ERR_return_outside_func_class_ext);
	    else if (stmt_mode == IR_NM_return_without_result)
	      {
		bc = new_bc_node (BC_NM_leave, stmt);
		SET_PC (bc);
	      }
	    else
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
		bc = new_bc_node (BC_NM_ret, stmt);
		BC_set_op1 (bc, result);
		/* Check tail call.  */
		if (curr_pc != NULL)
		  {
		    if (BC_NODE_MODE (curr_pc) == BC_NM_icall
			&& BC_op1 (curr_pc) == result)
		      bc_mode = BC_NM_itcall;
		    else if (BC_NODE_MODE (curr_pc) == BC_NM_ticall
			     && BC_op1 (curr_pc) == result)
		      bc_mode = BC_NM_titcall;
		    else if (BC_NODE_MODE (curr_pc) == BC_NM_cicall
			     && BC_op1 (curr_pc) == result)
		      bc_mode = BC_NM_citcall;
		    else
		      bc_mode = BC_NODE_MODE (curr_pc);
		    if (BC_NODE_MODE (curr_pc) != bc_mode
			&& IR_block_scope (curr_real_scope) != NULL
			&& IR_implementation_func (BC_func (curr_pc)) == NULL
			&& ! IR_IS_OF_TYPE (BC_func (curr_pc),
					    IR_NM_external_func))
		      BC_SET_MODE (curr_pc, bc_mode);
		  }
		SET_PC (bc);
	      }
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
	    bc = new_bc_node (BC_NM_wait, stmt);
	    BC_set_op1 (bc, result);
	    type_test (IR_wait_guard_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_wait_guard_expr_type);
	    if (there_is_function_call_in_expr)
	      error (FALSE, source_position, ERR_function_call_in_wait_stmt);
	    BC_set_pc (bc, BC_next (before_wait_guard_expr));
	    SET_PC (bc);
	    wait_finish = new_bc_node (BC_NM_waitend, stmt);
	    third_block_passing (IR_wait_stmt (stmt));
	    SET_PC (wait_finish);
	    break;
	  }
	case IR_NM_throw:
	  bc = new_bc_node (BC_NM_throw, stmt);
	  BC_set_op1 (bc, temp_vars_num + curr_vars_number);
	  IR_set_throw_expr
	    (stmt, third_expr_processing (IR_throw_expr (stmt), FALSE,
					  NULL, &temp_vars_num, FALSE));
	  type_test (IR_throw_expr (stmt), EVT_UNKNOWN,
		     ERR_invalid_throw_expr_type);
	  SET_PC (bc);
	  break;
	case IR_NM_block:
	  {
	    IR_node_t saved_curr_scope = curr_scope;
	    IR_node_t saved_curr_real_scope = curr_real_scope;
	    int saved_curr_vars_number = curr_vars_number;
	    IR_node_t curr_except;
	    pc_t saved_curr_pc = curr_pc;
	    pc_t block_finish;
	    pc_t catches_finish;
	    pc_t previous_node_catch_list_pc;
	    IR_node_t func_class;
	    BC_node_t except_bc, last_except_with_block;

	    curr_scope = stmt;
	    bc = new_bc_node (BC_NM_block, stmt);
	    IR_set_bc_block (stmt, bc);
	    if ((func_class = IR_func_class_ext (stmt)) != NULL)
	      {
		/* Class instance lives after the constrictor finish.  */
		if (IR_IS_OF_TYPE (func_class, IR_NM_class))
		  IR_set_extended_life_context_flag (stmt, TRUE);
		curr_pc = PC (bc);
	      }
	    else
	      SET_PC (bc);
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
	    block_finish = PC (new_bc_node (BC_NM_leave, stmt));
	    SET_PC (block_finish);
	    if (func_class != NULL && IR_IS_OF_TYPE (func_class, IR_NM_class)
		&& BC_IS_OF_TYPE (BC_next (bc), BC_NM_leave))
	      IR_set_simple_class_flag (func_class, TRUE);
	    BC_set_excepts (bc, NULL);
	    IR_set_block_end_pc (stmt, NULL);
	    if (func_class != NULL)
	      curr_pc = saved_curr_pc;
	    else if (IR_exceptions (stmt) != NULL)
	      {
		catches_finish = PC (new_bc_node (BC_NM_cend, stmt));
		previous_node_catch_list_pc = PC (bc); /* block */
		SET_PC (catches_finish);
		last_except_with_block = NULL;
		for (curr_except = IR_exceptions (stmt);
		     curr_except != NULL;
		     curr_except = IR_next_exception (curr_except))
		  {
		    d_assert (curr_pc == catches_finish);
		    d_assert (BC_IS_OF_TYPE (previous_node_catch_list_pc,
					     BC_NM_block)
			      || BC_IS_OF_TYPE (previous_node_catch_list_pc,
						BC_NM_except));
		    result = -1;
		    temp_vars_num = 0;
		    except_bc = new_bc_node (BC_NM_except, curr_except);
		    /* The exception instance in the first local
		       temporary variable.  */
		    BC_set_op1 (except_bc,
				setup_result_var_number (NULL, &temp_vars_num));
		    IR_set_exception_class_expr
		      (curr_except,
		       third_expr_processing
		       (IR_exception_class_expr (curr_except),
			FALSE, &result, &temp_vars_num, FALSE));
		    BC_set_op2 (except_bc, result);
		    type_test (IR_exception_class_expr (curr_except),
			       EVT_CLASS, ERR_invalid_catch_expr_type);
		    if (BC_IS_OF_TYPE (previous_node_catch_list_pc,
				       BC_NM_block))
		      BC_set_excepts (previous_node_catch_list_pc,
				      BC_next (catches_finish));
		    else
		      BC_set_next_except (previous_node_catch_list_pc,
					  BC_next (catches_finish));
		    SET_PC (except_bc);
		    previous_node_catch_list_pc = curr_pc;
		    if (IR_catch_block (curr_except) != NULL)
		      {
			last_except_with_block = except_bc;
			third_block_passing (IR_catch_block (curr_except));
			SET_PC (catches_finish);
		      }
		    else
		      {
			d_assert (last_except_with_block != NULL);
			BC_set_next (except_bc,
				     BC_next (last_except_with_block));
			curr_pc = catches_finish;
		      }
		  }
		d_assert (curr_pc == catches_finish);
		IR_set_block_end_pc (stmt, catches_finish);
		BC_set_next (catches_finish, NULL);
	      }
	    if (!IR_simple_block_flag (stmt))
	      IR_set_temporary_vars_number
		(stmt,
		 IR_temporary_vars_number (stmt) + ADD_TEMP_VARS_NUMBER);
	    break;
	  }
	case IR_NM_func:
	case IR_NM_class:
	  if (IR_next_stmt (stmt) != NULL)
	    VLO_ADD_MEMORY (all_funcs_and_classes, &stmt, sizeof (stmt));
	  goto common_decl;
	case IR_NM_var:
	case IR_NM_external_var:
	case IR_NM_external_func:
	common_decl:
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



/* Info about node used to dump the program code.  */
struct node_info
{
  int traverse_check; /* check used to find repeated processing. */
  int_t num; /* node number used for printing */
  BC_node_t n; /* node itself */
  BC_node_t subst; /* substitution.  */
};

/* Current value for traverse check.  */
static int curr_traverse_check;
/* Max value used for traverse check so far.  */
static int max_traverse_check;

/* Initiate traversing.  */
static inline
init_traverse_check (void)
{
  curr_traverse_check = max_traverse_check = 1;
}

/* Be ready for next traversing.  */
static inline
next_traverse_check (void)
{
  max_traverse_check++;
  curr_traverse_check = max_traverse_check;
}

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
find_node_info (BC_node_t n)
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
  ni->traverse_check = 0;
  ni->subst = NULL;
  ni->num = ++curr_node_num;
  *entry = ni;
  return ni;
}

/* Create the node info table. */
static void
initiate_node_tab (void)
{
  OS_CREATE (node_info_os, 0);
  node_info_tab = create_hash_table (3 * bc_insns_num / 2, node_info_hash,
				     node_info_eq);
  curr_node_num = 0;
}

/* Delete the node info table. */
static void
finish_node_tab (void)
{
  delete_hash_table (node_info_tab);
  OS_DELETE (node_info_os);
}

/* The following function passes through nodes unnecessary for
   execution.  Update {cond, if}_common_part_pc fields.  Also do insn
   combining.  */
static BC_node_t
go_through (BC_node_t start_pc)
{
  BC_node_mode_t node_mode;
  BC_node_t skip_end_pc, first_pc, second_pc, pc, next_pc;
  IR_node_t stmt;
  int bt_p, op1 = -1;
  struct node_info *ni;

  if (start_pc == NULL)
    return NULL;
  for (pc = start_pc; pc != NULL; pc = BC_next (pc))
    if (BC_NODE_MODE (pc) == BC_NM_cend)
      ;
    else if (BC_NODE_MODE (pc) == BC_NM_block
	     || BC_NODE_MODE (pc) == BC_NM_leave)
      {
	stmt = BC_origin (pc);
        if (! IR_IS_OF_TYPE (stmt, IR_NM_block)
	    || ! IR_simple_block_flag (stmt))
	  break;
      }
    else
      break;
  if (pc == NULL)
    return NULL;
  ni = find_node_info (pc);
  if (ni->subst != NULL)
    return ni->subst;
  skip_end_pc = second_pc = first_pc = pc;
  if (BC_NODE_MODE (first_pc) == BC_NM_addi
      && BC_op1 (first_pc) == BC_op2 (first_pc))
    {
      op1 = BC_op1 (first_pc);
      second_pc = BC_next (first_pc);
      if (second_pc == NULL
	  || (next_pc = BC_next (second_pc)) == NULL
	  || BC_NODE_MODE (next_pc) != BC_NM_bt
	  || BC_NODE_MODE (second_pc) == BC_NM_eqi
	  || BC_NODE_MODE (second_pc) == BC_NM_nei
	  || BC_NODE_MODE (second_pc) == BC_NM_gei
	  || BC_NODE_MODE (second_pc) == BC_NM_lti
	  || BC_NODE_MODE (second_pc) == BC_NM_lei
	  || BC_NODE_MODE (second_pc) == BC_NM_gti)
	{
	  op1 = -1;
	  second_pc = first_pc;
	}
    }
  node_mode = BC_NM__error;
  if ((next_pc = BC_next (second_pc)) != NULL
      && ((bt_p = BC_NODE_MODE (next_pc) == BC_NM_bt)
	  || BC_NODE_MODE (next_pc) == BC_NM_bf))
    switch (BC_NODE_MODE (second_pc))
      {
      case BC_NM_eq:
	node_mode = op1 >= 0 ? BC_NM_bteqinc : bt_p ? BC_NM_bteq : BC_NM_btne;
	break;
      case BC_NM_ne:
	node_mode = op1 >= 0 ? BC_NM_btneinc : bt_p ? BC_NM_btne : BC_NM_bteq;
	break;
      case BC_NM_ge:
	node_mode = op1 >= 0 ? BC_NM_btgeinc : bt_p ? BC_NM_btge : BC_NM_btlt;
	break;
      case BC_NM_lt:
	node_mode = op1 >= 0 ? BC_NM_btltinc : bt_p ? BC_NM_btlt : BC_NM_btge;
	break;
      case BC_NM_le:
	node_mode = op1 >= 0 ? BC_NM_btleinc : bt_p ? BC_NM_btle : BC_NM_btgt;
	break;
      case BC_NM_gt:
	node_mode = op1 >= 0 ? BC_NM_btgtinc : bt_p ? BC_NM_btgt : BC_NM_btle;
	break;
      case BC_NM_eqi:
	node_mode = bt_p ? BC_NM_bteqi : BC_NM_btnei;
	break;
      case BC_NM_nei:
	node_mode = bt_p ? BC_NM_btnei : BC_NM_bteqi;
	break;
      case BC_NM_gei:
	node_mode = bt_p ? BC_NM_btgei : BC_NM_btlti;
	break;
      case BC_NM_lti:
	node_mode = bt_p ? BC_NM_btlti : BC_NM_btgei;
	break;
      case BC_NM_lei:
	node_mode = bt_p ? BC_NM_btlei : BC_NM_btgti;
	break;
      case BC_NM_gti:
	node_mode = bt_p ? BC_NM_btgti : BC_NM_btlei;
	break;
      }
  if (node_mode != BC_NM__error
      && (op1 < 0 || BC_op2 (second_pc) == op1)
      && BC_op1 (second_pc) >= real_block_vars_number (curr_scope)
      && ! unary_slice_p (BC_origin (second_pc)))
    {
      pc = new_bc_node (node_mode, BC_origin (next_pc));
      if (op1 >= 0)
	BC_set_binc_inc (pc, BC_op3 (first_pc));
      BC_set_op1 (pc, BC_op2 (second_pc));
      BC_set_bcmp_op2 (pc, BC_op3 (second_pc));
      BC_set_bcmp_res (pc, BC_op1 (second_pc));
      BC_set_next (pc, BC_next (next_pc) == first_pc ? pc : BC_next (next_pc));
      BC_set_pc (pc, BC_pc (next_pc) == first_pc ? pc : BC_pc (next_pc));
    }
  for (; start_pc != skip_end_pc; start_pc = BC_next (start_pc))
    if (BC_NODE_MODE (start_pc) == BC_NM_cend)
      {
	stmt = BC_origin (start_pc);
	if (IR_IS_OF_TYPE (stmt, IR_NM_if_stmt))
	  IR_set_if_common_part_pc (stmt, pc);
	else if (IR_IS_OF_TYPE (stmt, IR_NM_cond))
	  IR_set_cond_common_part_pc (stmt, pc);
	else if (IR_IS_OF_TYPE (stmt, IR_NM_block))
	  IR_set_block_end_pc (stmt, pc);
      }
  ni->subst = pc;
  return pc;
}

/* Process node BC (if it is not processed yet) and all its
   successors.  Changing program counters by MODIFY_PC and processing
   nodes by process_node.  */
static BC_node_t
traverse_bc (BC_node_t bc, BC_node_t stop,
	     BC_node_t (*modify_pc) (BC_node_t),
	     BC_node_t (*process_node) (BC_node_t, struct node_info *))
{
  BC_node_t next_pc, first = NULL;
  struct node_info *ni;

  while (bc != stop)
    {
      ni = find_node_info (bc);
      if (ni->traverse_check == curr_traverse_check)
	return first;
      ni->traverse_check = curr_traverse_check;
      if (process_node != NULL)
	bc = process_node (bc, ni);
      if (first == NULL)
	first = bc;
      if (modify_pc != NULL)
	BC_set_next (bc, modify_pc (BC_next (bc)));
      if (BC_IS_OF_TYPE (bc, BC_NM_br))
	{
	  if (modify_pc != NULL)
	    BC_set_pc (bc, modify_pc (BC_pc (bc)));
	  traverse_bc (BC_pc (bc), stop, modify_pc, process_node);
	}
      else if (BC_IS_OF_TYPE (bc, BC_NM_brid))
	{
	  if (modify_pc != NULL)
	    BC_set_brid_pc (bc, modify_pc (BC_brid_pc (bc)));
	  traverse_bc (BC_brid_pc (bc), stop, modify_pc, process_node);
	}
      else if (BC_IS_OF_TYPE (bc, BC_NM_block))
	{
	  if (modify_pc != NULL)
	    BC_set_excepts (bc, modify_pc (BC_excepts (bc)));
	  traverse_bc (BC_excepts (bc), stop, modify_pc, process_node);
	}
      else if (BC_IS_OF_TYPE (bc, BC_NM_except))
	{
	  if (modify_pc != NULL)
	    BC_set_next_except (bc, modify_pc (BC_next_except (bc)));
	  traverse_bc (BC_next_except (bc), stop, modify_pc, process_node);
	}
      else if (BC_IS_OF_TYPE (bc, BC_NM_foreach))
	{
	  if (modify_pc != NULL)
	    BC_set_body_pc (bc, modify_pc (BC_body_pc (bc)));
	  traverse_bc (BC_body_pc (bc), stop, modify_pc, process_node);
	}
      bc = BC_next (bc);
    }
  return first;
}

/* Modify call BC to tail calls if it is possible.  */
static BC_node_t
process_imcall (BC_node_t bc, struct node_info *ni)
{
  BC_node_t next_pc;

  if (BC_IS_OF_TYPE (bc, BC_NM_imcall)
      && (next_pc = BC_next (bc)) != NULL
      && BC_NODE_MODE (next_pc) == BC_NM_leave)
    {
      BC_node_mode_t bc_mode;
      
      if (BC_NODE_MODE (bc) == BC_NM_ipcall)
	bc_mode = BC_NM_itpcall;
      else if (BC_NODE_MODE (bc) == BC_NM_tipcall)
	bc_mode = BC_NM_titpcall;
      else if (BC_NODE_MODE (bc) == BC_NM_cipcall)
	bc_mode = BC_NM_citpcall;
      else
	bc_mode = BC_NM__error;
      if (bc_mode != BC_NM__error
	  && IR_implementation_func (BC_func (bc)) == NULL
	  && ! IR_IS_OF_TYPE (BC_func (bc), IR_NM_external_func))
	BC_SET_MODE (bc, bc_mode);
    }
  return bc;
}

/* Process nodes from START generating tail calls, removing
   uneccessary nodes, and combining byte code insns.  */
static void
process_bc (BC_node_t start)
{
  traverse_bc (start, NULL, go_through, process_imcall);
}

/* Increment operand locations of BC by INC.  */
static void
inc_nops (BC_node_t bc, int_t inc)
{
  if (BC_IS_OF_TYPE (bc, BC_NM_op1))
    {
      if (! BC_IS_OF_TYPE (bc, BC_NM_op1i)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op2i12))
	BC_set_op1 (bc, BC_op1 (bc) + inc);
      if (BC_IS_OF_TYPE (bc, BC_NM_op2)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op2i12)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op2i)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op3i2)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op4i2))
	BC_set_op2 (bc, BC_op2 (bc) + inc);
      if (BC_IS_OF_TYPE (bc, BC_NM_op3) && ! BC_IS_OF_TYPE (bc, BC_NM_op3i))
	BC_set_op3 (bc, BC_op3 (bc) + inc);
      if (BC_IS_OF_TYPE (bc, BC_NM_op4))
	BC_set_op2 (bc, BC_op4 (bc) + inc);
      if (BC_IS_OF_TYPE (bc, BC_NM_brs))
	{
	  BC_set_res (bc, BC_res (bc) + inc);
	  if (BC_IS_OF_TYPE (bc, BC_NM_bcmp))
	    {
	      if (! BC_IS_OF_TYPE (bc, BC_NM_bcmpi))
		BC_set_bcmp_op2 (bc, BC_bcmp_op2 (bc) + inc);
	      BC_set_bcmp_res (bc, BC_bcmp_res (bc) + inc);
	    }
	}
    }
}

/* Make copy of BC and set up subst of the correspoding node info
   NI.  */
static BC_node_t
copy_bc_node (BC_node_t bc, struct node_info *ni)
{
  BC_node_t copy = BC_create_node (BC_NODE_MODE (bc));

  d_assert (ni->n == bc);
  memcpy (copy, bc, BC_node_size[BC_NODE_MODE (bc)]);
  ni->subst = copy;
  return bc;
}

/* Return copy of BC.  */
static BC_node_t
get_subst (BC_node_t bc)
{
  return find_node_info (bc)->subst;
}

/* Current call for inlining.  */
static BC_node_t curr_imcall;

/* Modify call BC to tail calls if it is possible.  */
static BC_node_t
process_inlined_bc_node (BC_node_t bc, struct node_info *ni)
{
  BC_node_t old_bc = bc;
  BC_node_t cont = BC_next (curr_imcall);

  if (BC_IS_OF_TYPE (bc, BC_NM_block) && BC_origin (bc) == curr_scope)
    {
      int temp_vars_num, inlined_vars_num;

      bc = BC_create_node (BC_NM_bstart);
      BC_set_op1 (bc, BC_op2 (curr_imcall));
      BC_set_op2 (bc, IR_vars_number (curr_scope));
      BC_set_next (bc, BC_next (old_bc));
      temp_vars_num = IR_temporary_vars_number (curr_real_scope);
      inlined_vars_num = (BC_op1 (curr_imcall) /* pars start */
			  + IR_vars_number (curr_scope)
			  + IR_temporary_vars_number (curr_scope)
			  - IR_vars_number (curr_real_scope));
      if (temp_vars_num < inlined_vars_num)
	IR_set_temporary_vars_number (curr_real_scope, inlined_vars_num);
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_leave) && BC_origin (bc) == curr_scope)
    {
      bc = BC_create_node (BC_IS_OF_TYPE (curr_imcall, BC_NM_impcall)
			   ? BC_NM_pbend : BC_NM_fbend);
      BC_set_op1 (bc, IR_vars_number (curr_scope));
      BC_set_next (bc, cont);
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_ret))
    {
      bc = BC_create_node (BC_NM_bret);
      BC_set_op1 (bc, BC_op1 (old_bc));
      BC_set_op2 (bc, IR_vars_number (curr_scope));
      BC_set_next (bc, cont);
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_out))
    {
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_var))
    {
      bc = BC_create_node (BC_NM_move);
      BC_set_op1 (bc, BC_op1 (old_bc));
      BC_set_op2 (bc,
		  IR_var_number_in_block (IR_decl (BC_origin (old_bc)))
		  - BC_op1 (curr_imcall));
      BC_set_next (bc, BC_next (old_bc));
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_lvar)
	   || BC_IS_OF_TYPE (bc, BC_NM_lvarv))
    {
    }
  if (old_bc != bc)
    BC_set_origin (bc, BC_origin (old_bc));
  else
    {
      BC_node_t copy = BC_create_node (BC_NODE_MODE (bc));
      
      memcpy (copy, bc, BC_node_size[BC_NODE_MODE (bc)]);
      bc = copy;
    }
  d_assert (ni->n == old_bc);
  ni->subst = bc;
  inc_nops (bc, BC_op1 (curr_imcall));
  return bc;
}

/* Inline block of calling IMCALL.  */
static BC_node_t
inline_func_block (BC_node_t imcall)
{
  BC_node_t saved_imcall;
  BC_node_t bc, next_pc;
  IR_node_t func = BC_func (imcall);
  BC_node_t cont = BC_next (imcall);

  curr_scope = IR_next_stmt (func);
  bc = IR_bc_block (curr_scope);
  next_traverse_check ();
  saved_imcall = curr_imcall;
  curr_imcall = imcall;
  bc = traverse_bc (bc, cont, NULL, process_inlined_bc_node);
  next_traverse_check ();
  curr_imcall = imcall;
  find_node_info (cont)->subst = cont;
  traverse_bc (bc, cont, get_subst, NULL);
  curr_imcall = saved_imcall;
  return bc;
}

/* Make inlining for BC if it is an immediate call.  */
static BC_node_t
inline_imcall (BC_node_t bc, struct node_info *ni)
{
  int saved_check;
  BC_node_t imcall;

  if (BC_IS_OF_TYPE (bc, BC_NM_imcall)
      && *IR_pos (BC_func (bc)).file_name != '<')
    {
      saved_check = curr_traverse_check;
      imcall = bc;
      bc = inline_func_block (imcall);
      find_node_info (imcall)->subst = bc;
      curr_traverse_check = saved_check;
    }
  return bc;
}

/* Make BC substitution for PC if it was inlined imcall.  */
static BC_node_t
imcall_subst (BC_node_t pc)
{
  struct node_info *ni;

  
  if (pc == NULL)
    return NULL;
  if (BC_NODE_MODE (pc) != BC_NM_bstart
      && BC_NODE_MODE (pc) != BC_NM_pbend
      && BC_NODE_MODE (pc) != BC_NM_fbend
      && (!BC_IS_OF_TYPE (pc, BC_NM_imcall)
	  || *IR_pos (BC_func (pc)).file_name == '<'))
    return pc;
  ni = find_node_info (pc);
  if (ni->subst != NULL)
    pc = ni->subst;
  if (BC_NODE_MODE (pc) == BC_NM_bstart && BC_op1 (pc) == 0 && BC_op2 (pc) == 0)
    pc = BC_next (pc);
  if ((BC_NODE_MODE (pc) == BC_NM_pbend || BC_NODE_MODE (pc) == BC_NM_fbend)
      && BC_op1 (pc) == 0)
    pc = BC_next (pc);
  return pc;
}

/* Process nodes from START generating tail calls, removing
   uneccessary nodes, and combining byte code insns.  */
static void
inline_bc (BC_node_t start)
{
  next_traverse_check ();
  curr_real_scope = curr_scope;
  traverse_bc (start, NULL, NULL, inline_imcall);
  next_traverse_check ();
  traverse_bc (start, NULL, imcall_subst, NULL);
}

/* Print INDENT spaces into stdout.  */
static void
print_indent (int indent)
{
  int i;

  for (i = 0; i < indent; i++)
    printf (" ");
}

/* True if a goto was last printed. */
static bool_t last_goto_flag;
/* The line number of the last printed node. */
static unsigned curr_line_number = 0;

/* Print code from node CN to STOP (not including it) using
   INDENT.  */
static void
dump_code (int indent, BC_node_t cn, BC_node_t stop)
{
  BC_node_mode_t node_mode;
  BC_node_t cl, cf = NULL;
  struct node_info *ni;
  bool_t line_flag;

  for (; cn != stop;)
    {
      ni = find_node_info (cn);
      if (ni->traverse_check == curr_traverse_check)
	{
	  if (! last_goto_flag)
	    {
	      print_indent (indent);
	      printf ("       goto %d\n", ni->num);
	    }
	  last_goto_flag = TRUE;
	  break;
	}
      ni->traverse_check = curr_traverse_check;
      node_mode = BC_NODE_MODE (cn);
      if ((node_mode != BC_NM_block || indent != 0)
	  && dump_flag == 1
	  && strcmp (IR_pos (BC_origin (cn)).file_name,
		     ENVIRONMENT_PSEUDO_FILE_NAME) == 0)
	{
	  cn = BC_next (cn);
	  continue;
	}
      if (node_mode == BC_NM_leave)
	indent -= 2;
      print_indent (indent);
      printf ("%6d %s", ni->num, BC_node_name[node_mode]);
      last_goto_flag = FALSE;
      d_assert (indent >= 0);
      if (curr_line_number != IR_pos (BC_origin (cn)).line_number)
	{
	  curr_line_number = IR_pos (BC_origin (cn)).line_number;
	  printf ("(line=%u)", curr_line_number);
	}
      line_flag = TRUE;
      switch (node_mode)
	{
	case BC_NM_cend: /* For debugging purposes */
	  cn = BC_next (cn);
	  break;
	case BC_NM_ldch:
	  printf (": %d <- ", BC_op1 (cn));
	  printf ("(%x)", BC_op2 (cn));
	  if (isgraph (BC_op2 (cn)))
	    printf ("(%c -- %x)", BC_op2 (cn), BC_op2 (cn));
	  else
	    printf ("(%x)", BC_op2 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_ldi:
	  printf (": %d <- ", BC_op1 (cn));
	  printf ("(%d)", BC_op2 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_ldf:
	  printf (": %d <- ", BC_op1 (cn));
	  printf ("(%g)", BC_f (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_ldtp:
	  printf (": %d <- %s", BC_op1 (cn), er_type_name (BC_op2 (cn)));
	  cn = BC_next (cn);
	  break;
	case BC_NM_ldnil:
	case BC_NM_ldthis:
	  printf (": %d <- ", BC_op1 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_lds:
	  {
	    const char *s;

	    printf (": %d <- ", BC_op1 (cn));
	    printf ("(\"");
	    for (s = BC_str (cn); *s != '\0'; s++)
	      printf ("%s", get_ch_repr (*s));
	    printf ("\")");
	    cn = BC_next (cn);
	    break;
	  }
	case BC_NM_fld:
	case BC_NM_lfld:
	case BC_NM_lfldv:
	  printf (": %d <- %d (%s, block_decl_ident_num=%d)",
		  BC_op1 (cn), BC_op2 (cn),
		  IR_ident_string (IR_unique_ident
				   (IR_component (BC_origin (cn)))),
 		  BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_brts:
	case BC_NM_brfs:
	  {
	    struct node_info *fni = find_node_info (BC_next (cn));
	    struct node_info *tni = find_node_info (BC_pc (cn));

	    printf (": %d <- %d (true = %d, false = %d)",
		    BC_res (cn), BC_op1 (cn), tni->num, fni->num);
	    cn = BC_next (cn);
	  }
	  break;
	case BC_NM_lconv:
	  printf (": %d <- %d", BC_op1 (cn), BC_op2 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_not:
	case BC_NM_bnot:
	case BC_NM_plus:
	case BC_NM_minus:
	case BC_NM_length:
	case BC_NM_fadd:
	case BC_NM_fmult:
	case BC_NM_fand:
	case BC_NM_fxor:
	case BC_NM_for:
	case BC_NM_const:
	case BC_NM_new:
	case BC_NM_tpof:
	case BC_NM_chof:
	case BC_NM_iof:
	case BC_NM_fof:
	case BC_NM_vecof:
	case BC_NM_tabof:
	case BC_NM_funcof:
	case BC_NM_threadof:
	case BC_NM_classof:
	  printf (": %d <- %d", BC_op1 (cn), BC_op2 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_vec:
	case BC_NM_tab:
	  printf (": %d <- (els=%d, parts=%d)",
		  BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_flat:
	  printf (": %d", BC_op1 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_call:
	case BC_NM_pcall:
	case BC_NM_tcall:
	case BC_NM_tpcall:
	  printf (": start=%d npars=%d", BC_op1 (cn), BC_op2 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_ind:
	case BC_NM_lindv:
	  printf (": %d <- %d[%d]", BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_icall:
	case BC_NM_itcall:
	case BC_NM_ticall:
	case BC_NM_titcall:
	case BC_NM_cicall:
	case BC_NM_citcall:
	case BC_NM_ipcall:
	case BC_NM_itpcall:
	case BC_NM_tipcall:
	case BC_NM_titpcall:
	case BC_NM_cipcall:
	case BC_NM_citpcall:
	  printf (" %s: start=%d npars=%d",
		  IR_ident_string (IR_unique_ident (IR_ident (BC_func (cn)))),
		  BC_op1 (cn), BC_op2 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_sl:
	case BC_NM_lslv:
	  printf (": %d <- %d (dim=%d)", BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_key:
	case BC_NM_lkeyv:
	  printf (": %d <- %d{%d}", BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_in:
	case BC_NM_eq:
	case BC_NM_ne:
	case BC_NM_id:
	case BC_NM_unid:
	case BC_NM_lt:
	case BC_NM_ge:
	case BC_NM_gt:
	case BC_NM_le:
	case BC_NM_mult:
	case BC_NM_div:
	case BC_NM_mod:
	case BC_NM_add:
	case BC_NM_sub:
	case BC_NM_concat:
	case BC_NM_lsh:
	case BC_NM_rsh:
	case BC_NM_ash:
	case BC_NM_and:
	case BC_NM_xor:
	case BC_NM_or:
	case BC_NM_fmtvecof:
	  printf (": %d <- %d %d", BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_eqi:
	case BC_NM_nei:
	case BC_NM_lei:
	case BC_NM_gei:
	case BC_NM_gti:
	case BC_NM_lti:
	  printf (": %d <- %d (%d)", BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_addi:
	  printf (": %d <- %d (%d)", BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_mult_st:
	case BC_NM_div_st:
	case BC_NM_mod_st:
	case BC_NM_sub_st:
	case BC_NM_concat_st:
	case BC_NM_lsh_st:
	case BC_NM_rsh_st:
	case BC_NM_ash_st:
	case BC_NM_and_st:
	case BC_NM_xor_st:
	case BC_NM_or_st:
	case BC_NM_add_st:
	  printf (": %d(%d) <- %d op %d", BC_op1 (cn), BC_op2 (cn),
		  BC_op4 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_st:
	  printf (": %d(%d) <- %d", BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_slst:
	  printf (": %d(dim=%d) <- %d", BC_op1 (cn), BC_op2 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_mult_slst:
	case BC_NM_div_slst:
	case BC_NM_mod_slst:
	case BC_NM_sub_slst:
	case BC_NM_concat_slst:
	case BC_NM_lsh_slst:
	case BC_NM_rsh_slst:
	case BC_NM_ash_slst:
	case BC_NM_and_slst:
	case BC_NM_xor_slst:
	case BC_NM_or_slst:
	case BC_NM_add_slst:
	  printf (": %d(dim=%d) <- %d op %d", BC_op1 (cn),
		  BC_op2 (cn), BC_op4 (cn), BC_op3 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_btdef:
	  {
	    struct node_info *fni = find_node_info (BC_next (cn));
	    struct node_info *tni = find_node_info (BC_brid_pc (cn));

	    printf (": %s? (true=%d, false=%d)",
		    IR_ident_string (IR_unique_ident (BC_origin (cn))),
		    tni->num, fni->num);
	    cn = BC_next (cn);
	    last_goto_flag = TRUE;
	  }
	  break;
	case BC_NM_bf: /* if_stmt */
	case BC_NM_bfni: /* cond */
	  {
	    IR_node_t irn;
	    BC_node_t cp;
	    struct node_info *fni = find_node_info (BC_next (cn));
	    struct node_info *tni = find_node_info (BC_pc (cn));

	    indent += 2;
	    printf (": %d (true=%d, false=%d)\n", BC_op1 (cn),
		    tni->num, fni->num);
	    irn = BC_origin (cn);
	    if (IR_NODE_MODE (irn) == IR_NM_if_stmt)
	      cp = IR_if_common_part_pc (irn);
	    else
	      cp = IR_cond_common_part_pc (irn);
	    dump_code (indent, BC_next (cn), cp);
	    if (BC_pc (cn) != cp)
	      {
		print_indent (indent);
		printf ("       goto %d\n", find_node_info (cp)->num);
	      }
	    dump_code (indent, BC_pc (cn), cp);
	    line_flag = FALSE;
	    indent -= 2;
	    cn = cp;
	    break;
	  }
	case BC_NM_bteq:
	case BC_NM_btne:
	case BC_NM_btle:
	case BC_NM_btgt:
	case BC_NM_btge:
	case BC_NM_btlt:
	  printf (": %d %d (temp_res=%d)",
		  BC_op1 (cn), BC_bcmp_op2 (cn), BC_bcmp_res (cn),
		  find_node_info (BC_pc (cn))->num);
	  goto common_bt;
	case BC_NM_bteqi:
	case BC_NM_btnei:
	case BC_NM_btlei:
	case BC_NM_btgti:
	case BC_NM_btgei:
	case BC_NM_btlti:
	  printf (": %d (%d) (temp_res=%d)",
		  BC_op1 (cn), BC_bcmp_op2 (cn), BC_bcmp_res (cn),
		  find_node_info (BC_pc (cn))->num);
	  goto common_bt;
	case BC_NM_bteqinc:
	case BC_NM_btneinc:
	case BC_NM_btleinc:
	case BC_NM_btgtinc:
	case BC_NM_btgeinc:
	case BC_NM_btltinc:
	  printf (": %d+(%d) %d (temp_res=%d)",
		  BC_op1 (cn), BC_binc_inc (cn),
		  BC_bcmp_op2 (cn), BC_bcmp_res (cn));
	  goto common_bt;
	case BC_NM_bt:
	  printf (": %d", BC_op1 (cn));
	common_bt:
	  {
	    IR_node_t irn;
	    BC_node_t cp;
	    struct node_info *fni = find_node_info (BC_next (cn));
	    struct node_info *tni = find_node_info (BC_pc (cn));

	    printf (" (true=%d, false=%d)\n", tni->num, fni->num);
	    last_goto_flag = TRUE;
	    indent += 2;
	    irn = BC_origin (cn);
	    if (IR_NODE_MODE (irn) == IR_NM_if_stmt)
	      {
		cp = IR_if_common_part_pc (irn);
		dump_code (indent, BC_pc (cn), cp);
		if (BC_pc (cn) != cp)
		  {
		    print_indent (indent);
		    printf ("       goto %d\n", find_node_info (cp)->num);
		  }
		dump_code (indent, BC_next (cn), cp);
		cn = cp;
	      }
	    else
	      {
		dump_code (indent, BC_pc (cn), BC_next (cn));
		cn = BC_next (cn);
	      }
	    line_flag = FALSE;
	    indent -= 2;
	  }
	  break;
	case BC_NM_pushi: /* foreach start/foreach next iteration */
	  if (BC_op1 (cn) == 0)
	    indent += 2; /* foreach start */
	  printf (": (%d)", BC_op1 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_foreach:
	  printf (": %d(%d) (val=%d) in %d (end=%d)\n",
		  BC_op2 (cn), BC_op3 (cn), BC_op4 (cn), BC_op1 (cn),
		  find_node_info (BC_next (cn))->num);
	  /* It is finishing on code already traversed.  */
	  dump_code (indent, BC_body_pc (cn), NULL);
	  line_flag = FALSE;
	  cn = BC_next (cn);
	  break;
	case BC_NM_out:
	  printf (": of %d blocks", BC_op1 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_leave:
	  cn = BC_next (cn);
	  break;
	case BC_NM_ret:
	  printf (": %d", BC_op1 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_wait:
	  printf (": %d (guard expr start=%d)", BC_op1 (cn),
		  find_node_info (BC_pc (cn))->num);
	  cn = BC_next (cn);
	  break;
	case BC_NM_waitend:
          cn = BC_next (cn);
	  break;
	case BC_NM_block:
	  {
	    IR_node_t block_stmt, stmt;

	    block_stmt = BC_origin (cn);
	    d_assert (! IR_simple_block_flag (block_stmt));
	    printf (": vars=%d, tvars=%d%s", real_block_vars_number (block_stmt),
		    IR_temporary_vars_number (block_stmt),
		    IR_extended_life_context_flag (block_stmt) ? ", ext." : "");
	    indent += 2;
	    for (stmt = IR_block_stmts (block_stmt);
		 stmt != NULL;
		 stmt = IR_next_stmt (stmt))
	      if (IR_IS_OF_TYPE (stmt, IR_NM_func_or_class)
		  && (dump_flag > 1
		      || strcmp (IR_pos (stmt).file_name,
				 ENVIRONMENT_PSEUDO_FILE_NAME) != 0))
		{
		  printf ("\n");
		  print_indent (indent);
		  printf ("%s %s (%d%s)\n",
			  IR_IS_OF_TYPE (stmt, IR_NM_func) ? "func" : "class",
			  IR_ident_string (IR_unique_ident (IR_ident (stmt))),
			  IR_parameters_number (stmt),
			  IR_args_flag (stmt) ? ", ..." : "");
		  dump_code (indent, IR_bc_block (IR_next_stmt (stmt)), NULL);
		}
	    if ((cl = BC_excepts (cn)) == NULL)
	      cn = BC_next (cn);
	    else
	      {
		BC_node_t fn;
		
		fn = IR_block_end_pc (block_stmt);
		d_assert (fn != NULL);
		printf ("(except = %d)\n", find_node_info (cl)->num);
		cf = BC_next (fn);
		dump_code (indent, BC_next (cn), cf);
		line_flag = FALSE;
		cn = cl;
	      }
	    break;
	  }
	case BC_NM_bstart:
	  printf (": npars=%d, nvars=%d", BC_op1 (cn), BC_op2 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_pbend:
	case BC_NM_fbend:
	  printf (": npops=%d", BC_op1 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_bret:
	  printf (": ret=%d, npops=%d", BC_op1 (cn), BC_op2 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_throw:
	  printf (": %d", BC_op1 (cn));
	  cn = BC_next (cn);
	  break;
	case BC_NM_except:
	  printf (": %d inside %d ", BC_op1 (cn), BC_op2 (cn));
	  if ((cl = BC_next_except (cn)) == NULL)
	    {
	      cn = BC_next (cn);
	      cf = NULL;
	    }
	  else
	    {
	      printf ("(catch = %d)\n", find_node_info (cl)->num);
	      dump_code (indent, BC_next (cn), cf);
	      line_flag = FALSE;
	      cn = cl;
	    }
	  break;
	case BC_NM_var:
	case BC_NM_lvar:
	case BC_NM_lvarv:
	case BC_NM_evar:
	case BC_NM_levar:
	case BC_NM_levarv:
	case BC_NM_efunc:
	case BC_NM_func:
	case BC_NM_class:
	  printf (": %d <- (%s)", BC_op1 (cn),
		  IR_ident_string (IR_unique_ident (BC_origin (cn))));
	  cn = BC_next (cn);
	  break;
	case BC_NM_move:
	  printf (": %d <- %d (%s)", BC_op1 (cn), BC_op2 (cn),
		  IR_ident_string (IR_unique_ident (BC_origin (cn))));
	  cn = BC_next (cn);
	  break;
	default:
	  /* Other nodes should not occur here.  */
	  d_unreachable ();
	}
      if (line_flag)
	printf ("\n");
      if (node_mode == BC_NM_out)
	return;
    }
}

pc_t
test_context (IR_node_t first_program_stmt_ptr)
{
  IR_node_t *node_ptr;
  IR_node_t decl;

  bc_insns_num = 0;
  curr_scope = NULL;
  VLO_CREATE (possible_table_extensions, 0);
  process_block_decl_unique_ident (destroy_unique_ident);
  first_program_stmt_ptr = first_block_passing (first_program_stmt_ptr, 0);
  /* Check that there are no extensions in the table. */
  for (node_ptr = (IR_node_t *) VLO_BEGIN (possible_table_extensions);
       (char *) node_ptr <= (char *) VLO_END (possible_table_extensions);
       node_ptr++)
    {
      decl = *(IR_node_t *) find_table_entry (*node_ptr, FALSE);
      if (decl != NULL && IR_IS_OF_TYPE (decl, IR_NM_ext))
	error (FALSE, IR_pos (IR_ident (*node_ptr)),
	       ERR_extension_without_class_or_func,
	       IR_ident_string (IR_unique_ident (IR_ident (*node_ptr))));
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
  VLO_CREATE (all_funcs_and_classes, 0);
  third_block_passing (first_program_stmt_ptr);
  /* Never shrink the top block and always put it into stack area.  */
  IR_set_extended_life_context_flag (first_program_stmt_ptr, FALSE);
  if (number_of_errors == 0)
    {
      initiate_node_tab ();
      init_traverse_check ();
      /* Some optimizations: */
      curr_scope = first_program_stmt_ptr;
      process_bc (IR_bc_block (curr_scope));
      for (node_ptr = (IR_node_t *) VLO_BEGIN (all_funcs_and_classes);
	   (char *) node_ptr <= (char *) VLO_END (all_funcs_and_classes);
	   node_ptr++)
	{
	  curr_scope = IR_next_stmt (*node_ptr);
	  process_bc (IR_bc_block (curr_scope));
	}
#if 0
      /* Inline optimizations: */
     curr_scope = first_program_stmt_ptr;
      inline_bc (IR_bc_block (curr_scope));
      for (node_ptr = (IR_node_t *) VLO_BEGIN (all_funcs_and_classes);
	   (char *) node_ptr <= (char *) VLO_END (all_funcs_and_classes);
	   node_ptr++)
	{
	  curr_scope = IR_next_stmt (*node_ptr);
	  inline_bc (IR_bc_block (curr_scope));
	}
#endif
      if (dump_flag)
	{
	  last_goto_flag = FALSE;
	  next_traverse_check ();
	  dump_code (0, IR_bc_block (first_program_stmt_ptr), NULL);
	}
      finish_node_tab ();
    }
  VLO_DELETE (all_funcs_and_classes);
  /* Remember that first program stmt is always block (see grammar
     description on yacc). */
  return PC (IR_bc_block (first_program_stmt_ptr));
}
