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

#include <setjmp.h>
#include "d_common.h"
#include "d_ir.h"
#include "d_bc.h"
#include "d_run.h"
#include "d_context.h"
#include "d_bcio.h"
#include "d_inference.h"

/* Pointer to block node which opens current scope. */
static IR_node_t curr_scope;

/* Pointer to block node which opens current scope. */
static BC_node_t curr_bc_scope;

/* Current and previous program counter.  The values are used to
   connect byte code commands for interpreter (see comments for
   typedef pc_t). */
static pc_t curr_pc, prev_pc;

/* The current info node.  The value is used to form byte code
   chain.  */
static BC_node_t curr_info;

/* Byte code starting new iteration of current for stmt.  This value is
   used to connect continue stmt. */
static BC_node_t start_next_iteration;

/* Program counter of finishing of current for stmt.
   This value is for_finish node and is used to connect break stmt.
   If this value is equal to NULL then there is not surrounding
   for-stmt. */
static BC_node_t for_finish;

/* Number of blocks inside current for-stmt.  This value is used to
   determine corresponding members in break_stmt node
   or continue_stmt node. */
static int number_of_surrounding_blocks;

/* Jump buffer for exit from the contex pass. */
static jmp_buf context_exit_longjump_buff;

/* Context error.  */
static void
cont_err (position_t pos, const char *format, ...)
{
  va_list arguments;

  va_start (arguments, format);
  d_verror (FALSE, pos, format, arguments);
  va_end (arguments);
  if (repl_flag)
    longjmp (context_exit_longjump_buff, 1);
}

/* Contex error followed by an append message.  */
static void
cont_err_start (position_t pos, const char *format, ...)
{
  va_list arguments;

  va_start (arguments, format);
  d_verror (FALSE, pos, format, arguments);
  va_end (arguments);
}

/* Final append message.  */
static void
cont_err_finish (position_t pos, const char *format, const char *str)
{
  if (! repl_flag)
    append_message (pos, format, str);
  else
    {
      d_error (FALSE, pos, format, str);
      longjmp (context_exit_longjump_buff, 1);
    }
}



/* Stack foreach statemets for processing.  */
static vlo_t foreach_stmts;

static void
initiate_foreach_stmts (void)
{
  VLO_CREATE (foreach_stmts, 0);
}

/* Return index for new foreach stmts.  */
static int
get_foreach_stmts_bound (void)
{
  return VLO_LENGTH (foreach_stmts) / sizeof (IR_node_t);
}

/* Assign slots to vars of block foreach stmts which start from
   START_INDEX.  */
static void
finish_block_foreach_stmts (int start_index)
{
  IR_node_t *foreach_ptr;
  int slot_num = IR_vars_number (curr_scope);
  int n = 0;

  for (foreach_ptr = &((IR_node_t *) VLO_BEGIN (foreach_stmts)) [start_index];
       foreach_ptr < (IR_node_t *) VLO_BOUND (foreach_stmts);
       foreach_ptr++)
    {
      n++;
      if (*foreach_ptr == NULL)
	slot_num -= 2;
      else
	{
	  IR_set_foreach_tab_place (*foreach_ptr, slot_num);
	  IR_set_foreach_search_start_place (*foreach_ptr, slot_num + 1);
	  slot_num += 2;
	  if (slot_num > IR_vars_number (curr_scope))
	    IR_set_vars_number (curr_scope, slot_num);
	}
    }
  VLO_SHORTEN (foreach_stmts, n * sizeof (IR_node_t));
}

static void
finish_foreach_stmts (void)
{
  VLO_DELETE (foreach_stmts);
}



/* Report error MSG if DES is a slice.  Use position of POS_NODE for
   this.  */
static void
check_slice (IR_node_t pos_node, IR_node_t des, const char *msg)
{
  if (des == NULL || ! IR_IS_OF_TYPE (des, IR_NM_slice))
    return;
  d_assert (pos_node != NULL);
  cont_err (IR_pos (pos_node), msg);
}

/* This func includes DECL into the hash table.  The funcs also sets
   up DECLS_FLAG of the block in which the decl is immediately placed
   if the decl has a position.  The function return the previous table
   element value.  */
static IR_node_t
include_decl (IR_node_t decl)
{
  IR_node_t *table_entry, result;

  table_entry = find_table_entry (decl, TRUE);
  result = *table_entry;
  if (IR_pos (decl).file_name != NULL)
    IR_set_decls_flag (IR_scope (decl), TRUE);
  *table_entry = decl;
  return result;
}

/* The last used number to enumerate unique indentifiers used as
   fields.  */
static int last_uniq_field_ident_num;

/* Setup FIELD_IDENT_NUMBER for UNIQUE_IDENT.  */
static int
set_field_ident_number (IR_node_t unique_ident)
{
  if (IR_field_ident_number (unique_ident) >= 0)
    return IR_field_ident_number (unique_ident);
  if (destroy_unique_ident != unique_ident)
    {
      last_uniq_field_ident_num++;
      IR_set_field_ident_number (unique_ident, last_uniq_field_ident_num);
    }
}

/* The following recursive func passes (correctly setting up
   SOURCE_POSITION) EXPR (it may be NULL) sets up members parts_number
   and class_func_thread_call_parameters_number in vector node (table
   node) and class_func_thread_call node. */
static void
first_expr_processing (IR_node_t expr)
{
  if (expr == NULL)
    return;
  switch (IR_NODE_MODE (expr))
    {
    case IR_NM_char:
    case IR_NM_int:
    case IR_NM_long:
    case IR_NM_float:
    case IR_NM_string:
    case IR_NM_nil:
    case IR_NM_char_type:
    case IR_NM_int_type:
    case IR_NM_long_type:
    case IR_NM_float_type:
    case IR_NM_hide_type:
    case IR_NM_hideblock_type:
    case IR_NM_vec_type:
    case IR_NM_tab_type:
    case IR_NM_fun_type:
    case IR_NM_class_type:
    case IR_NM_thread_type:
    case IR_NM_stack_type:
    case IR_NM_process_type:
    case IR_NM_type_type:
      break;
    case IR_NM_this:
      {
	IR_node_t scope;

	for (scope = curr_scope; scope != NULL; scope = IR_block_scope (scope))
	  if (IR_fun_class (scope) != NULL)
	    break;
	if (scope == NULL)
	  cont_err (IR_pos (expr), ERR_this_outside_fun_class);
	else
	  IR_set_extended_life_context_flag (curr_scope, TRUE);
	break;
      }
    case IR_NM_ident:
      {
	IR_node_t decl;

	decl = find_decl (expr, curr_scope);
	IR_set_decl (expr, decl);
	SET_SOURCE_POSITION (expr);
	if (decl == NULL)
	  {
	    decl = create_node_with_pos (IR_NM_var, no_position);
	    IR_set_scope (decl, NULL); /* Mark it.  */
	    IR_set_ident (decl, expr);
	    include_decl (decl);
	    cont_err (source_position, ERR_undeclared_ident,
		      IR_ident_string (IR_unique_ident (expr)));
	  }
	break;
      }
    case IR_NM_period:
      SET_SOURCE_POSITION (expr);
      first_expr_processing (IR_designator (expr));
      set_field_ident_number (IR_unique_ident (IR_component (expr)));
      check_slice (expr, IR_designator (expr),
		   ERR_period_ident_applied_to_slice);
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
    case IR_NM_format_vecof:
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
	    && IR_int_value (IR_unique_int (r)) != MAX_RINT)
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
    case IR_NM_longof:
    case IR_NM_floatof:
    case IR_NM_vecof:
    case IR_NM_tabof:
    case IR_NM_funof:
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
    case IR_NM_vec:
    case IR_NM_tab:
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
      SET_SOURCE_POSITION (expr);
      /* Two stack entries are necessary for reference. */
      first_expr_processing (IR_designator (expr));
      check_slice (expr, IR_designator (expr),
		   ERR_vec_tab_element_access_applied_to_slice);
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
    case IR_NM_class_fun_thread_call:
      SET_SOURCE_POSITION (expr);
      {
	IR_node_t elist;
	int parameters_number;
	
	first_expr_processing (IR_fun_expr (expr));
	check_slice (expr, IR_fun_expr (expr), ERR_call_applied_to_slice);
	for (parameters_number = 0, elist = IR_actuals (expr); elist != NULL;
	     elist = IR_next_elist (elist))
	  {
	    parameters_number++;
	    SET_SOURCE_POSITION (elist);
	    d_assert (IR_repetition_key (elist) == NULL);
	    first_expr_processing (IR_expr (elist));
	  }
	IR_set_class_fun_thread_call_parameters_number
	  (expr, parameters_number);
	break;
      }
    default:
      d_unreachable ();
    }
}

static IR_node_t first_block_passing (IR_node_t first_level_stmt,
				      int curr_block_level);

/* Process friend list of BLOCK and create the friends for
   the current scope. */
static void
process_friend_list (IR_node_t block)
{
  IR_node_t curr_friend_ident, friend, decl;
  IR_node_t scope = block;

  for (curr_friend_ident = IR_friend_list (block);
       curr_friend_ident != NULL;
       curr_friend_ident = IR_next_friend_ident (curr_friend_ident))
    {
      decl = find_decl (IR_ident_in_clause (curr_friend_ident),	scope);
      if (decl == NULL)
	cont_err (IR_pos (IR_ident_in_clause (curr_friend_ident)),
		  ERR_undeclared_ident_friend_list,
		  IR_ident_string (IR_unique_ident
				   (IR_ident_in_clause
				    (curr_friend_ident))));
      else if (!IR_IS_OF_TYPE (decl, IR_NM_fun_or_class))
	cont_err (IR_pos (IR_ident_in_clause (curr_friend_ident)),
		  ERR_invalid_friend,
		  IR_ident_string (IR_unique_ident
				   (IR_ident_in_clause
				    (curr_friend_ident))));
      else
	{
	  friend = create_node (IR_NM_friend);
	  IR_set_friend_decl (friend, decl);
	  IR_set_next_friend (friend, IR_friends (curr_scope));
	  IR_set_friends (curr_scope, friend);
	}
    }
}

/* The following value is maximal number of IR_block_level. */
static int max_block_level;

/* Set LEVEL for BLOCK and update MAX_BLOCK_LEVEL if necessary.  */
static void
set_block_level (IR_node_t block, int level)
{
  IR_set_block_level (block, level);
  if (max_block_level < level)
    max_block_level = level;
}

/* Copied redirs created during processing use-clauses for unfinished
   block processing.  */
static vlo_t copied_redirs;

/* Update field TO of copied redirs starting with index START.  */
static void
update_copied_redirs (int start)
{
  int i, n;
  IR_node_t *redirs, redir, old;

  n = VLO_LENGTH (copied_redirs) / sizeof (IR_node_t);
  redirs = (IR_node_t *) VLO_BEGIN (copied_redirs);
  /* Update to field of copied redirs.  */
  for (i = start; i < n; i++)
    {
      redir = redirs[i];
      old = IR_to (redir);
      IR_set_to (redir, IR_redir_to_subst (old));
    }
}

/* Make copied redirs starting with index START flat.  It means make
   redirs not refering for other redirs.  Update redir_origin too for
   non-flat redirs.  */
static void
make_flat_redirs (int start)
{
  int i, n;
  IR_node_t *redirs, redir, to;

  n = VLO_LENGTH (copied_redirs) / sizeof (IR_node_t);
  redirs = (IR_node_t *) VLO_BEGIN (copied_redirs);
  /* Update to field of copied redirs.  */
  for (i = start; i < n; i++)
    {
      redir = redirs[i];
      to = IR_to (redir);
      if (! IR_IS_OF_TYPE (to, IR_NM_decl_redir))
	continue;
      IR_set_redir_origin (redir, IR_redir_origin (to));
      to = IR_to (to);
      d_assert (! IR_IS_OF_TYPE (to, IR_NM_decl_redir));
      IR_set_to (redir, to);
    }
}

/* Use items for currently processed use clause.  */
static vlo_t use_items;
/* Start of use items in the previous container for the current
   block.  */
static size_t curr_use_items_start;

/* Use items comparison for their sorting.  */
static int
use_item_cmp (const void *p1, const void *p2)
{
  int diff;
  const IR_node_t i1 = *(const IR_node_t *) p1, i2 = *(const IR_node_t *) p2;
  const char *id1 = IR_ident_string (IR_unique_ident (IR_use_item_ident (i1)));
  const char *id2 = IR_ident_string (IR_unique_ident (IR_use_item_ident (i2)));

  if ((diff = strcmp (id1, id2)) != 0)
    return diff;
  /* It means item from the same use-clause will be together.  */
  return compare_positions (IR_pos (i1), IR_pos (i2));
}

/* Binary search of an item with IDENT and USE_CLAUSE (if it is not
   NULL) in sorted use_items.  Return index of first item with IDENT
   and USE_CLAUSE or -1 if there is no any such item. */
static long int
find_use_item_index (IR_node_t ident, IR_node_t use_clause)
{
  long int l, r, m;
  int cmp;
  IR_node_t item, muid;
  IR_node_t uid = IR_unique_ident (ident);
  const char *ms, *s = IR_ident_string (uid);

  l = curr_use_items_start;
  r = VLO_LENGTH (use_items) / sizeof (IR_node_t) - 1;
  while (l <= r)
    {
      m = (l + r) / 2;
      item = ((IR_node_t *) VLO_BEGIN (use_items)) [m];
      muid = IR_unique_ident (IR_use_item_ident (item));
      ms = IR_ident_string (muid);
      if ((cmp = strcmp (s, ms)) == 0)
	{
	  /* Find the very first item in USE_ITEMS with UID.  */
	  for (m--; l <= m; m--)
	    {
	      item = ((IR_node_t *) VLO_BEGIN (use_items)) [m];
	      if (uid != IR_unique_ident (IR_use_item_ident (item)))
		break;
	    }
	  if (use_clause == NULL)
	    return m + 1;
	  /* Find the very first item in USE_ITEMS with UID and
	     USE_CLAUSE.  */
	  for (m++; m <= r; m++)
	    {
	      item = ((IR_node_t *) VLO_BEGIN (use_items)) [m];
	      if (uid != IR_unique_ident (IR_use_item_ident (item)))
		return -1;
	      if (IR_item_use_clause (item) == use_clause)
		return m;
	    }
	  return -1;
	}
      else if (cmp < 0)
	r = m - 1;
      else
	l = m + 1;
    }
  return -1;
}

/* Check that PREV_DECL and DECL are matching.  Return TRUE if
   everything is ok.  Otherwise, print error and return FALSE.  */
static int
check_matching (IR_node_t prev_decl, IR_node_t decl)
{
  if (IR_NODE_MODE (decl) != IR_NODE_MODE (prev_decl)
      || (IR_NODE_MODE (decl) == IR_NM_fun
	  && IR_thread_flag (prev_decl) != IR_thread_flag (decl)))
    {
      cont_err_start
	(IR_pos (decl),
	 ERR_forward_and_matched_decls_are_different_entities,
	 IR_ident_string (IR_unique_ident (IR_ident (decl))));
      cont_err_finish
	(IR_pos (IR_ident (prev_decl)),
	 ERR_previous_decl_location,
	 IR_ident_string (IR_unique_ident (IR_ident (decl))));
      return FALSE;
    }
  if ((IR_IS_OF_TYPE (decl, IR_NM_fun_class)
       && IR_final_flag (prev_decl) != IR_final_flag (decl))
      || IR_access (prev_decl) != IR_access (decl))
    {
      cont_err_start
	(IR_pos (IR_ident (decl)),
	 ERR_forward_and_matched_decls_have_different_attrs,
	 IR_ident_string (IR_unique_ident (IR_ident (decl))));
      cont_err_finish
	(IR_pos (IR_ident (prev_decl)),
	 ERR_previous_decl_location,
	 IR_ident_string (IR_unique_ident (IR_ident (decl))));
      return FALSE;
    }
  /* ??? Should we check parameters too. */
  return TRUE;
}

/* Set up next_stmt of STMT to NEXT and forward_prev of NEXT if
   necessary.  Don't use IR_next_stmt directly in this file.  */
static void
set_next_stmt (IR_node_t stmt, IR_node_t next)
{
  IR_set_next_stmt (stmt, next);
  if (next != NULL
      && IR_IS_OF_TYPE (next, IR_NM_fun_or_class)
      && IR_forward_decl_flag (next))
    IR_set_forward_prev (next, stmt);
}

/* Add STMTS to statement TO.  If TO is NULL, set up FIRST to
   STMTS.  */
static void
add_to_stmt (IR_node_t stmts, IR_node_t to, IR_node_t *first)
{
  if (to == NULL)
    *first = stmts;
  else
    set_next_stmt (to, stmts);
}

/* Create and add alias (if necessary) for ITEM from ORIG_DECL
   processing USE_CLAUSE where last stmt is LAST, and first stmt is
   *RES.  Check that the alias does not redefine a previous
   definition.  Return the new last stmt.  */
static IR_node_t
make_alias (IR_node_t item, IR_node_t orig_decl, IR_node_t use_clause,
	    IR_node_t last, IR_node_t *res)
{
  IR_node_t ident, decl, copy;

  if ((ident = IR_alias (item)) != NULL)
    {
      decl = find_decl_in_given_scope (ident, curr_scope);
      if (decl != NULL
	  && (! IR_alias_flag (decl) || IR_use_clause (decl) != use_clause))
	{
	  cont_err_start (IR_pos (use_clause), ERR_alias_redefines_prev_one,
			  IR_ident_string (IR_unique_ident (ident)));
	  cont_err_finish (IR_pos (IR_ident (decl)),
			   ERR_previous_decl_location,
			   IR_ident_string (IR_unique_ident (ident)));
	}
      else
	{
	  copy = IR_copy_node (orig_decl);
	  IR_set_alias_flag (copy, TRUE);
	  IR_set_ident (copy, ident);
	  IR_set_redir_to_subst (orig_decl, copy);
	  IR_set_use_clause (copy, use_clause);
	  IR_set_scope (copy, curr_scope);
	  IR_set_origin_decl (copy, orig_decl);
	  add_to_stmt (copy, last, res); last = copy;
	}
    }
  return last;
}

/* The function processes USE_CLAUSE which refers to ORIGIN_BLOCK
   copying the original block decls and setting NEXT_STMT as their
   tail.  Return the copied decls.  Here are some more details:
   o Check that idents in the use items are unique.  Remove
     duplications and already redefined items from previous
     use-clauses in the block.
   o Check that the use ident is declared as finished fun/class in
     upper scopes.
   o Check that if there is an ident definition before the use and the
     definition in the used fun/class, the ident is represented in a
     former item.
   o Check that the definition before the use clause and all the
     corresponding used definitions are matched.  Do not copy the used
     definitions in this case.  Put redir instead of the copy.
     Setup redefine_flag of the corresponding former item.
   o When copying decl, setup origin_decl, scope, and use_clause of
     the copy.
   o When copying fun/decl, do not copy the block.  If the fun/decl is
     mentioned in an later item, don't make the decl copy, just put a
     new redir and chain it to all redirs of the corresponding item.
   o Check that all idents in former items have declaration before the
     use clause (through the item redefine_flag).
   o Remove all former items at the end of function. */
static IR_node_t
process_use_clause (IR_node_t use_clause, IR_node_t origin_block,
		    IR_node_t next_stmt)
{
  IR_node_t item, *item_ptr, *res_item_ptr;
  IR_node_t ident, decl, stmt, res, last, copy, redir;
  int err_p, n, start;

  /* Collect items:  */
  for (item = IR_use_items (use_clause);
       item != NULL;
       item = IR_next_use_item (item))
    {
      IR_set_item_use_clause (item, use_clause);
      VLO_ADD_MEMORY (use_items, &item, sizeof (IR_node_t));
    }
  qsort ((IR_node_t *) VLO_BEGIN (use_items) + curr_use_items_start,
	 VLO_LENGTH (use_items) / sizeof (IR_node_t) - curr_use_items_start,
	 sizeof (IR_node_t), use_item_cmp);
  /* Check repeated identifier occurence and set up decls references,
     remove duplicates and already bound later items.  */
  err_p = FALSE;
  for (n = 0, res_item_ptr = item_ptr = ((IR_node_t *) VLO_BEGIN (use_items)
					 + curr_use_items_start);
       item_ptr < (IR_node_t *) VLO_BOUND (use_items);
       item_ptr++)
    if (IR_redefine_flag (*item_ptr))
      {
	/* It could be only later items as we remove former items at
	   the function end.  */
	d_assert (IR_IS_OF_TYPE (*item_ptr, IR_NM_later_item));
	n++;
      }
    else if ((item_ptr + 1) < (IR_node_t *) VLO_BOUND (use_items)
	     && ! IR_redefine_flag (item_ptr[1])
	     && (IR_item_use_clause (item_ptr[0])
		 == IR_item_use_clause (item_ptr[1]))
	     && (IR_unique_ident (IR_use_item_ident (item_ptr[0]))
		 == IR_unique_ident (IR_use_item_ident (item_ptr[1]))))
      {
	n++;
	if (! err_p)
	  {
	    err_p = TRUE;
	    cont_err_start
	      (IR_pos (IR_use_item_ident (*item_ptr)),
	       ERR_repeated_use_item_ident_occurence_in_use,
	       IR_ident_string (IR_unique_ident
				(IR_use_item_ident (*item_ptr))));
	    cont_err_finish
	      (IR_pos (IR_use_item_ident (item_ptr[1])),
	       ERR_previous_use_item_ident_location,
	       IR_ident_string (IR_unique_ident
				(IR_use_item_ident (item_ptr[1]))));
	  }
      }
    else
      {
	err_p = FALSE;
	*res_item_ptr++ = *item_ptr;
	ident = IR_use_item_ident (*item_ptr);
	decl = find_decl_in_given_scope (ident, origin_block);
	if (decl == NULL)
	  cont_err (IR_pos (ident), ERR_undefined_use_item_ident,
		    IR_ident_string (IR_unique_ident (ident)));
      }
  VLO_SHORTEN (use_items, n * sizeof (IR_node_t));
  start = VLO_LENGTH (copied_redirs) / sizeof (IR_node_t);
  /* Copy and insert the oirginal block declarations.  */
  for (res = last = NULL, stmt = IR_block_stmts (origin_block);
       stmt != NULL;
       stmt = IR_next_stmt (stmt))
    if (IR_IS_OF_TYPE (stmt, IR_NM_decl_redir))
      {
	copy = IR_copy_node (stmt);
	add_to_stmt (copy, last, &res); last = copy;
	VLO_ADD_MEMORY (copied_redirs, &copy, sizeof (IR_node_t));
      }
    else if (IR_IS_OF_TYPE (stmt, IR_NM_decl))
      {
	ident = IR_ident (stmt);
	decl = find_decl_in_given_scope (ident, curr_scope);
	n = find_use_item_index (ident, use_clause);
	item = n < 0 ? NULL : ((IR_node_t *) VLO_BEGIN (use_items)) [n];
	if (decl != NULL && IR_use_clause (decl) != use_clause)
	  {
	    if (item == NULL || ! IR_IS_OF_TYPE (item, IR_NM_former_item))
	      {
		cont_err_start
		  (IR_pos (use_clause),
		   ERR_used_decl_not_mentioned_in_former_redefines_prev_one,
		   IR_ident_string (IR_unique_ident (ident)));
		cont_err_finish (IR_pos (IR_ident (decl)),
				 ERR_previous_decl_location,
				 IR_ident_string (IR_unique_ident (ident)));
	      }
	    else
	      {
		check_matching (decl, stmt);
		IR_set_redefine_flag (item, TRUE);
		/* Put redir instead of used decl.  */
		redir = create_node_with_pos (IR_NM_decl_redir, IR_pos (stmt));
		IR_set_redir_to_subst (stmt, redir);
		IR_set_to (redir, decl);
		IR_set_redir_origin (redir, stmt);
		IR_set_next_redir (redir, NULL);
		add_to_stmt (redir, last, &res); last = redir;
		last = make_alias (item, stmt, use_clause, last, &res);
	      }
	    continue;
	  }
	if (item != NULL && IR_IS_OF_TYPE (item, IR_NM_later_item))
	  {
	    redir = create_node_with_pos (IR_NM_decl_redir, IR_pos (stmt));
	    IR_set_redir_to_subst (stmt, redir);
	    IR_set_redir_origin (redir, stmt);
	    IR_set_next_redir (redir, IR_redirs (item));
	    IR_set_redirs (item, redir);
	    add_to_stmt (redir, last, &res); last = redir;
	    last = make_alias (item, stmt, use_clause, last, &res);
	  }
	else
	  {
	    copy = IR_copy_node (stmt);
	    IR_set_redir_to_subst (stmt, copy);
	    IR_set_use_clause (copy, use_clause);
	    IR_set_scope (copy, curr_scope);
	    IR_set_origin_decl (copy, stmt);
	    add_to_stmt (copy, last, &res); last = copy;
	  }
      }
  if (last != NULL)
    set_next_stmt (last, next_stmt);
  /* Check that all former items are bound and remove all former
     items.  */
  for (n = 0, res_item_ptr = item_ptr = ((IR_node_t *) VLO_BEGIN (use_items)
					 + curr_use_items_start);
       item_ptr < (IR_node_t *) VLO_BOUND (use_items);
       item_ptr++)
    if (! IR_IS_OF_TYPE (*item_ptr, IR_NM_former_item))
      *res_item_ptr++ = *item_ptr;
    else
      {
	n++;
	if (! IR_redefine_flag (*item_ptr))
	  cont_err (IR_pos (IR_use_item_ident (*item_ptr)),
		    ERR_ident_in_former_item_is_not_declared_before_use,
		    IR_ident_string (IR_unique_ident
				     (IR_use_item_ident (*item_ptr))));
      }
  VLO_SHORTEN (use_items, n * sizeof (IR_node_t));
  update_copied_redirs (start);
  return res;
}

/* Copy attributes of non-forward fun/class decl FROM to fun/class
   decl TO.  */
static void
copy_fun_class_attrs (IR_node_t to, IR_node_t from)
{
  IR_node_t next_stmt = IR_next_stmt (from);

  d_assert (IR_IS_OF_TYPE (to, IR_NM_fun_or_class)
	    && IR_IS_OF_TYPE (from, IR_NM_fun_or_class)
	    && ! IR_forward_decl_flag (from)
	    && next_stmt != NULL && IR_NODE_MODE (next_stmt) == IR_NM_block);
  IR_set_forward_decl_flag (to, FALSE); /* Make it non-forward */
  IR_set_parameters_number (to, IR_parameters_number (from));
  IR_set_min_actual_parameters_number
    (to, IR_min_actual_parameters_number (from));
  IR_set_args_flag (to, IR_args_flag (from));
  IR_set_fun_class (next_stmt, to);
}

/* Process CURR_DECL whose previous and next statement are PREV_STMT
   and NEXT_STMT (in a chain starting with FIRST_STMT) and whose
   previous decl with the same identifier is PREV_DECL (may be NULL).
   Update redefine_flag for the corresponding item if any.  We do
   nothing when PREV_DECL is in the same use clause (or the both are
   from an used clause).  Return FALSE when nothing is done, TRUE
   otherwise.  The process consists of

   o Check that ident of redefinition in given fun/class of the used
     definition is mentioned in an later item of an use clause
     already processed in the current block.
   o Check that the definition and the redefinition are matched.
   o Set field TO of the corresponding redirs created for the later
     item to CURR_DECL.
   o Update CURR_DECL, NEXT_STMT, and FIRST_STMT

  Before the function execution we have only unique, not processed,
  later items in USE_ITEMS for the current block.  */
static int
process_redecl_after (IR_node_t prev_decl, IR_node_t prev_stmt,
		      IR_node_t *curr_decl, IR_node_t *next_stmt,
		      IR_node_t *first_stmt)
{
  long int n;
  IR_node_t id = IR_ident (*curr_decl);
  IR_node_t uid = IR_unique_ident (id);
  IR_node_t item, redir;

  d_assert (IR_IS_OF_TYPE (*curr_decl, IR_NM_decl)
	    && (prev_decl == NULL
		|| (IR_IS_OF_TYPE (prev_decl, IR_NM_decl)
		    && uid == IR_unique_ident (IR_ident (prev_decl)))));
  if (prev_decl != NULL
      && ((IR_alias_flag (prev_decl) && ! IR_alias_flag (*curr_decl))
	  || IR_use_clause (prev_decl) != IR_use_clause (*curr_decl)))
    {
      cont_err_start (IR_pos (id), ERR_alias_redefinition,
		      IR_ident_string (uid));
      cont_err_finish (IR_pos (IR_ident (prev_decl)),
		       ERR_previous_decl_location, IR_ident_string (uid));
      return TRUE;
    }

  n = find_use_item_index (id, NULL);
  if (n >= 0)
    {
      /* Set up field 'to' of the unbound later use items.  */
      for (; n * sizeof (IR_node_t) < VLO_LENGTH (use_items); n++)
	{
	  item = ((IR_node_t *) VLO_BEGIN (use_items)) [n];
	  d_assert (IR_IS_OF_TYPE (item, IR_NM_later_item));
	  if (uid != IR_unique_ident (IR_use_item_ident (item)))
	    break;
	  if (IR_redefine_flag (item))
	    continue;
	  IR_set_redefine_flag (item, TRUE);
	  for (redir = IR_redirs (item);
	       redir != NULL;
	       redir = IR_next_redir (redir))
	    if (check_matching (IR_redir_origin (redir), *curr_decl))
	      IR_set_to (redir, *curr_decl);
	}
    }
  if (prev_decl == NULL
      || IR_use_clause (prev_decl) == IR_use_clause (*curr_decl))
    return FALSE;
  /* We can not have prev_decl and decl from different use-clauses or
     from orginal scope and from an use-clause correspondingly.
     Function process_use_clause guarantees it.  */
  d_assert (IR_use_clause (prev_decl) != NULL
	    && IR_use_clause (*curr_decl) == NULL);
  cont_err_start
    (IR_pos (IR_ident (*curr_decl)),
     ERR_used_decl_redefinition_not_mentioned_in_later, IR_ident_string (uid));
  cont_err_finish (IR_pos (IR_use_clause (prev_decl)),
		   ERR_here_is_corresponding_use_clause, IR_ident_string (uid));
  return TRUE;
}

/* Add DECL to the current block.  Add also all uses of DECL block
   recursively. */
static void
add_block_use (IR_node_t decl)
{
  IR_node_t use, block;

  d_assert (IR_IS_OF_TYPE (decl, IR_NM_fun_class));
  block = IR_next_stmt (decl);
  d_assert (block != NULL && IR_IS_OF_TYPE (block, IR_NM_block));
  for (use = IR_block_uses (curr_scope);
       use != NULL;
       use = IR_next_block_use (use))
    if (IR_use_decl (use) == decl)
      return; /* Already there  */
  use = create_node (IR_NM_block_use);
  IR_set_use_decl (use, decl);
  IR_set_next_block_use (use, IR_block_uses (curr_scope));
  IR_set_block_uses (curr_scope, use);
  for (use = IR_block_uses (block); use != NULL; use = IR_next_block_use (use))
    add_block_use (IR_use_decl (use));
}

/* Return true if BLOCK is simple in other words it will be part of
   some outer block.  */
static int
simple_block_p (IR_node_t block)
{
  return (IR_fun_class (block) == NULL && ! IR_decls_flag (block)
	  && IR_exceptions (block) == NULL && IR_block_scope (block) != NULL);
}

/* This recursive func passes all stmts and exprs (correctly setting
   up SOURCE_POSITION and curr_scope (before first call of the func
   curr_scope is to be NULL)) on the same stmt nesting level, includes
   decls into the hash table, sets up decl block_level of any block
   node, sets up decls_flags and increases vars_number of the block in
   which the decl (var) is immediately placed, sets up acces flag for
   decls with default access.  The funcs also fixes repeated decl
   error.  FIRST_LEVEL_STMT (it may be NULL) is first stmt of the
   processed stmt nesting level.  CURR_BLOCK_LEVEL is number of blocks
   which contain given stmt list.  Return the result stmt list.  */
static IR_node_t
first_block_passing (IR_node_t first_level_stmt, int curr_block_level)
{
  IR_node_t stmt;
  IR_node_t prev_stmt;
  IR_node_t next_stmt;
  IR_node_mode_t node_mode;

  for (prev_stmt = NULL, stmt = first_level_stmt;
       stmt != NULL;
       prev_stmt = stmt, stmt = next_stmt)
    {
      next_stmt = IR_next_stmt (stmt);
      SET_SOURCE_POSITION (stmt);
      node_mode = IR_NODE_MODE (stmt);
      switch (node_mode)
	{
	case IR_NM_expr_stmt:
	  first_expr_processing (IR_stmt_expr (stmt));
	  break;
	case IR_NM_assign:
	  first_expr_processing (IR_assignment_var (stmt));
	  first_expr_processing (IR_assignment_expr (stmt));
	  break;
	case IR_NM_var_assign:
	case IR_NM_par_assign:
	  first_expr_processing (IR_assignment_var (stmt));
	  /* var/val initialization is already processed -- see
	     IR_NM_var processing.  */
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
		&& IR_int_value (IR_unique_int (expr)) != MAX_RINT)
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
	  {
	    IR_node_t t;
	    IR_node_t index_des = IR_foreach_index_designator (stmt);
	    IR_node_t value_des = IR_foreach_value_designator (stmt);

	    first_expr_processing (index_des);
	    if (index_des != NULL && IR_IS_OF_TYPE (index_des, IR_NM_slice))
	      {
		IR_set_foreach_index_designator (stmt, NULL);
		cont_err (IR_pos (index_des), ERR_slice_as_foreach_index_designator);
	      }
	    if (value_des != NULL)
	      {
		first_expr_processing (value_des);
		if (value_des != NULL && IR_IS_OF_TYPE (value_des, IR_NM_slice))
		  {
		    IR_set_foreach_value_designator (stmt, NULL);
		    cont_err (IR_pos (value_des), ERR_slice_as_foreach_value_designator);
		  }
	      }
	    VLO_ADD_MEMORY (foreach_stmts, &stmt, sizeof (stmt));
	    first_expr_processing (IR_foreach_tab (stmt));
	    IR_set_foreach_stmts (stmt,
				  first_block_passing (IR_foreach_stmts (stmt),
						       curr_block_level));
	    t = NULL;
	    VLO_ADD_MEMORY (foreach_stmts, &t, sizeof (t));
	    break;
	  }
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
	    int saved_curr_use_items_start = curr_use_items_start;
	    int copied_redirs_start;
	    IR_node_t curr_except;
	    IR_node_t *item_ptr;
	    int block_foreach_start;

	    copied_redirs_start = VLO_LENGTH (copied_redirs) / sizeof (IR_node_t);
	    curr_use_items_start = VLO_LENGTH (use_items) / sizeof (IR_node_t);
	    curr_scope = stmt;
	    set_block_level (stmt, curr_block_level);
	    block_foreach_start = get_foreach_stmts_bound ();
	    IR_set_block_stmts
	      (stmt, first_block_passing (IR_block_stmts (stmt),
					  curr_block_level + 1));
	    
	    
	    /* Now we can say does the block have var decls and is the
	       block simple or in other words in what scope the vars
	       will be placed.  */
	    if (! simple_block_p (stmt))
	      finish_block_foreach_stmts (block_foreach_start);
	    process_friend_list (stmt);
	    curr_scope = saved_curr_scope;
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
	    /* Check that all idents in later items have corresponding
	       definitions after the use clauses in the block. */
	    for (item_ptr = ((IR_node_t *) VLO_BEGIN (use_items)
			     + curr_use_items_start);
		 item_ptr < (IR_node_t *) VLO_BOUND (use_items);
		 item_ptr++)
	      {
		d_assert (IR_IS_OF_TYPE (*item_ptr, IR_NM_later_item));
		if (! IR_redefine_flag (*item_ptr))
		  cont_err (IR_pos (IR_use_item_ident (*item_ptr)),
			    ERR_ident_in_later_item_is_not_declared_after_use,
			    IR_ident_string (IR_unique_ident
					     (IR_use_item_ident (*item_ptr))));
	      }
	    make_flat_redirs (copied_redirs_start);
	    /* Restore use items and redirs state as it was before the
	       block. */
	    VLO_SHORTEN (use_items,
			 VLO_LENGTH (use_items)
			 - curr_use_items_start * sizeof (IR_node_t));
	    curr_use_items_start = saved_curr_use_items_start;
	    VLO_SHORTEN (copied_redirs,
			 VLO_LENGTH (copied_redirs)
			 - copied_redirs_start * sizeof (IR_node_t));
	    break;
	  }
	case IR_NM_fun:
	case IR_NM_class:
	case IR_NM_var:
	case IR_NM_external_var:
	case IR_NM_external_fun:
	  {
	    IR_node_t block, prev_decl, redir, ident = IR_ident (stmt);

	    if (node_mode == IR_NM_var && next_stmt != NULL
		&& (IR_NODE_MODE (next_stmt) == IR_NM_var_assign
		    || IR_NODE_MODE (next_stmt) == IR_NM_par_assign))
	      /* Process idents in init expr before the
		 declaration.  */
	      first_expr_processing (IR_assignment_expr (next_stmt));
	    IR_set_it_is_declared_in_block (IR_unique_ident (ident), TRUE);
	    if (IR_access (stmt) == DEFAULT_ACCESS)
	      IR_set_access
		(stmt,
		 IR_fun_class (curr_scope) != NULL
		 && IR_IS_OF_TYPE (IR_fun_class (curr_scope), IR_NM_class)
		 ? PUBLIC_ACCESS : PRIVATE_ACCESS);
	    if (IR_IS_OF_TYPE (stmt, IR_NM_fun_or_class)
		&& IR_forward_decl_flag (stmt))
	      IR_set_forward_prev (stmt, prev_stmt);
	    block = IR_scope (stmt);
	    d_assert (block != NULL && IR_NODE_MODE (block) == IR_NM_block);
	    prev_decl = find_decl_in_given_scope (ident, block);
	    if (process_redecl_after (prev_decl, prev_stmt, &stmt, &next_stmt,
				      &first_level_stmt))
	      break;
	    d_assert (prev_decl == NULL
		      || IR_use_clause (prev_decl) == IR_use_clause (stmt));
	    if (node_mode == IR_NM_var)
	      {
		IR_set_var_number_in_block (stmt, IR_vars_number (block));
		IR_set_vars_number (block, IR_vars_number (block) + 1);
		include_decl (stmt); /* Redefinition */
	      }
	    else if (prev_decl == NULL
		     || ! IR_IS_OF_TYPE (prev_decl, IR_NM_fun_or_class)
		     || ! IR_forward_decl_flag (prev_decl))
	      {
		/* First ident occurrence or redefinition: */
		include_decl (stmt);
	      }
	    else if (! check_matching (prev_decl, stmt))
	      ;
	    else if (IR_forward_decl_flag (stmt))
	      /* Two forward declarations. */
	      ;
	    else
	      {
		IR_node_t prev;

		/* A forward fun/class decl followed by non-forward one
		   with definition.  Change the curr decl on the prev
		   decl. */
		d_assert (next_stmt != NULL
			  && IR_IS_OF_TYPE (next_stmt, IR_NM_block));
		redir = create_node_with_pos (IR_NM_decl_redir, IR_pos (prev_decl));
		IR_set_redir_origin (redir, prev_decl);
		IR_set_to (redir, prev_decl);
		IR_set_next_redir (redir, NULL);
		set_next_stmt (redir, IR_next_stmt (prev_decl));
		prev = IR_forward_prev (prev_decl);
		add_to_stmt (redir, prev, &first_level_stmt);
		if (prev_stmt == prev_decl)
		  prev_stmt = redir;
		copy_fun_class_attrs (prev_decl, stmt);
		/* Prev decl is not forward anymore.  */
		IR_set_forward_prev (prev_decl, NULL);
		IR_set_fun_class (next_stmt, prev_decl);
		add_to_stmt (prev_decl, prev_stmt, &first_level_stmt);
		set_next_stmt (prev_decl, next_stmt);
		set_next_stmt (stmt, NULL);
		stmt = prev_decl;
	      }
	    break;
	  }
	case IR_NM_use:
	  {
	    IR_node_t decl, subst = next_stmt;
	    IR_node_t scope, ident = IR_use_ident (stmt);

	    decl = find_decl (ident, curr_scope);
	    if (decl == NULL)
	      cont_err (IR_pos (stmt), ERR_use_before_definition,
			IR_ident_string (IR_unique_ident (ident)));
	    else if (! IR_IS_OF_TYPE (decl, IR_NM_fun_class))
	      cont_err (IR_pos (stmt),
			ERR_use_of_non_fun_class,
			IR_ident_string (IR_unique_ident (ident)));
	    else if (IR_forward_decl_flag (decl))
	      cont_err (IR_pos (stmt),
			ERR_use_of_forward_declaration,
			IR_ident_string (IR_unique_ident (ident)));
	    else if (IR_final_flag (decl))
	      cont_err (IR_pos (stmt), ERR_use_of_final,
			IR_ident_string (IR_unique_ident (ident)));
	    else
	      {
		for (scope = curr_scope;
		     scope != NULL;
		     scope = IR_block_scope (scope))
		  if (IR_fun_class (scope) == decl)
		    {
		      cont_err (IR_pos (ident),
				ERR_use_of_class_inside_the_class,
				IR_ident_string (IR_unique_ident (ident)));
		      break;
		    }
		if (scope == NULL)
		  {
		    subst = process_use_clause (stmt, IR_next_stmt (decl),
						next_stmt);
		    add_block_use (decl);
		  }
	      }
	    /* Remove the use-clasue from the chain of stmts. See
	       epilog of the loop. */
	    if (prev_stmt == NULL)
	      first_level_stmt = subst == NULL ? next_stmt : subst;
	    else
	      set_next_stmt (prev_stmt, subst);
	    /* Process the inserted decls. */
	    stmt = prev_stmt;
	    if (subst != NULL)
	      /* The used clause might be empty.  */
	      next_stmt = subst;
	  }
	  break;
	case IR_NM_decl_redir:
	  break;
	default:
	  d_unreachable ();
	}
    }
  return first_level_stmt;
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
    cont_err (source_position, message);
}

/* Pointer to non simple block node containing the current scope. */
static IR_node_t curr_real_scope;
/* Vars number in the current real scope.  */
static int curr_vars_number;

/* Number of additional variables for different purpose. */
#define ADD_TEMP_VARS_NUMBER 0

/* Link INFO to the info chain.  It should be not chained before.  */
static void inline
link_info (BC_node_t info)
{
  d_assert (info != NULL && BC_IS_OF_TYPE (info, BC_NM_info)
	    && BC_prev_info (info) == NULL && BC_next_info (info) == NULL);
  if (curr_info != NULL)
    BC_set_next_info (curr_info, info);
  BC_set_prev_info (info, curr_info);
  curr_info = info;
}

/* Unlink the last info node.  */
static void inline
unlink_last_info (void)
{
  BC_node_t prev_info;

  d_assert (curr_info != NULL);
  prev_info = BC_prev_info (curr_info);
  BC_set_next_info (curr_info, NULL);
  BC_set_prev_info (curr_info, NULL);
  curr_info = prev_info;
  BC_set_next_info (curr_info, NULL);
}

/* Add NODE with info to the generated byte code.  It can be done only
   once for the NODE.  */
static void inline
add_to_bcode (BC_node_t node)
{
  link_info (BC_info (node));
  prev_pc = curr_pc;
  if (curr_pc != NULL)
    BC_set_next (curr_pc, node);
  curr_pc = node;
}

/* See comments for the function second_expr_processing.  This variable
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

static const int not_defined_result = INT_MIN;

static int do_inline
setup_result_var_number (int *result, int *temp_vars_num)
{
  int new_p = result == NULL || *result == not_defined_result;
  int res = (new_p ? get_temp_stack_slot (temp_vars_num) : *result);

  if (result != NULL)
    *result = res;
  return res;
}

/* Remove INFO from the list of infos.  It could be done several times
   safely.  */
static inline void
unlink_info (BC_node_t info)
{
  BC_node_t prev, next;

  prev = BC_prev_info (info);
  next = BC_next_info (info);
  if (prev != NULL)
    BC_set_next_info (prev, next);
  if (next != NULL)
    BC_set_prev_info (next, prev);
  BC_set_next_info (info, NULL);
  BC_set_prev_info (info, NULL);
}

/* Insert INFO into the info list BEFOE. */
static inline void
insert_info (BC_node_t info, BC_node_t before)
{
  BC_node_t next;

  d_assert (before != NULL);
  next = BC_next_info (before);
  BC_set_next_info (before, info);
  if (next != NULL)
    BC_set_prev_info (next, info);
  BC_set_next_info (info, next);
  BC_set_prev_info (info, before);
}

/* Create and return new source bcode node with BNM and POS.  */
static BC_node_t do_inline
new_bc_node (BC_node_mode_t bnm, position_t pos)
{
  BC_node_t res = BC_create_node (bnm);

  bc_nodes_num++;
  BC_set_pos (res, pos);
  return res;
}

/* Create and return new non-source bcode node with BNM and info
   ORIGIN.  Attach the info node.  */
static BC_node_t do_inline
new_bc_code (BC_node_mode_t bnm, BC_node_t origin)
{
  BC_node_t res = BC_create_node (bnm);

  bc_nodes_num++;
  BC_set_info (res, origin);
  attach_info (res);
  return res;
}

/* Create and return new non-source bcode node with BNM.  Create
   source node with position from ORIGIN and attach the info node.  */
static BC_node_t do_inline
new_bc_code_with_src (BC_node_mode_t bnm, IR_node_t origin)
{
  BC_node_t res = BC_create_node (bnm);

  bc_nodes_num++;
  BC_set_info (res, new_bc_node (BC_NM_source, IR_pos (origin)));
  attach_info (res);
  return res;
}

static IR_node_t
second_expr_processing (IR_node_t expr, int fun_class_assign_p,
		       int *result, int *curr_temp_vars_num, int lvalue_p,
		       BC_node_t false_pc, BC_node_t true_pc, int next_false_p);

static void do_inline
process_unary_op (IR_node_t op, int *result, int *curr_temp_vars_num)
{
  int op_result = not_defined_result;
  int temp_vars_num = *curr_temp_vars_num;
  BC_node_mode_t bc_node_mode;
  BC_node_t bc, src;

  SET_SOURCE_POSITION (op);
  op_result = not_defined_result;
  IR_set_operand (op, second_expr_processing (IR_operand (op), FALSE,
					      &op_result, &temp_vars_num,
					      FALSE, NULL, NULL, FALSE));
  switch (IR_NODE_MODE (op))
    {
    case IR_NM_not: bc_node_mode = BC_NM_not; break;
    case IR_NM_unary_plus: bc_node_mode = BC_NM_plus; break;
    case IR_NM_unary_minus: bc_node_mode = BC_NM_minus; break;
    case IR_NM_bitwise_not: bc_node_mode = BC_NM_bnot; break;
    case IR_NM_length: bc_node_mode = BC_NM_length; break;
    case IR_NM_fold_plus: bc_node_mode = BC_NM_fold_add; break;
    case IR_NM_fold_mult: bc_node_mode = BC_NM_fold_mult; break;
    case IR_NM_fold_and: bc_node_mode = BC_NM_fold_and; break;
    case IR_NM_fold_xor: bc_node_mode = BC_NM_fold_xor; break;
    case IR_NM_fold_or: bc_node_mode = BC_NM_fold_or; break;
    case IR_NM_typeof: bc_node_mode = BC_NM_tpof; break;
    case IR_NM_charof: bc_node_mode = BC_NM_chof; break;
    case IR_NM_intof: bc_node_mode = BC_NM_iof; break;
    case IR_NM_longof: bc_node_mode = BC_NM_lof; break;
    case IR_NM_floatof: bc_node_mode = BC_NM_fof; break;
    case IR_NM_vecof: bc_node_mode = BC_NM_vecof; break;
    case IR_NM_tabof: bc_node_mode = BC_NM_tabof; break;
    case IR_NM_new: bc_node_mode = BC_NM_new; break;
    case IR_NM_const: bc_node_mode = BC_NM_const; break;
    default:
      d_unreachable ();
    }
  src = new_bc_node (BC_NM_source2, IR_pos (op));
  if (IR_operand (op) != NULL)
    BC_set_pos2 (src, IR_pos (IR_operand (op)));
  else
    BC_set_pos2 (src, IR_pos (op));
  bc = new_bc_code (bc_node_mode, src);
  BC_set_op2 (bc, op_result);
  BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
  add_to_bcode (bc);
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

static do_inline int
int_p (rint_t i)
{
  return INT_MIN <= i && i <= INT_MAX;
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
  int l_op_result = not_defined_result, r_op_result = not_defined_result;
  int temp_vars_num = *curr_temp_vars_num;
  BC_node_mode_t bc_node_mode;
  BC_node_t bc, src;
  IR_node_t l = IR_left_operand (op), r = IR_right_operand (op);

  SET_SOURCE_POSITION (op);
  skip_left_p = skip_right_p = FALSE;
  src = new_bc_node (BC_NM_source3, IR_pos (op));
  BC_set_pos2 (src, IR_pos (op));
  BC_set_pos3 (src, IR_pos (op));
  if (new_op_mode != BC_NM__error)
    {
      if (l != NULL && IR_IS_OF_TYPE (l, IR_NM_int))
	{
	  IR_set_right_operand
	    (op, second_expr_processing (r, FALSE,
					 &r_op_result, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	  skip_right_p = TRUE;
	  if (! int_p (IR_int_value (IR_unique_int (l)))
	      || unary_slice_p (IR_right_operand (op)))
	    ;
	  else
	    {
	      skip_left_p = TRUE;
	      bc = new_bc_code (rev_op_mode, src);
	      BC_set_op3 (bc, IR_int_value (IR_unique_int (l)));
	      BC_set_op2 (bc, r_op_result);
	    }
	}
      else if (r != NULL && IR_IS_OF_TYPE (r, IR_NM_int))
	{
	  IR_set_left_operand
	    (op, second_expr_processing (l, FALSE,
					 &l_op_result, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	  skip_left_p = TRUE;
	  if (! int_p (IR_int_value (IR_unique_int (r)))
	      || unary_slice_p (IR_left_operand (op)))
	    ;
	  else
	    {
	      skip_right_p = TRUE;
	      bc = new_bc_code (new_op_mode, src);
	      BC_set_op3 (bc, IR_int_value (IR_unique_int (r)));
	      BC_set_op2 (bc, l_op_result);
	    }
	}
    }
  if (IR_left_operand (op) != NULL)
    BC_set_pos2 (src, IR_pos (IR_left_operand (op)));
  if (IR_right_operand (op) != NULL)
    BC_set_pos3 (src, IR_pos (IR_right_operand (op)));
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
	case IR_NM_format_vecof: bc_node_mode = BC_NM_fmtvecof; break;
	default:
	  d_unreachable ();
	}
      if (! skip_left_p)
	IR_set_left_operand
	  (op, second_expr_processing (l, FALSE,
				      &l_op_result, &temp_vars_num, FALSE,
				      NULL, NULL, FALSE));
      if (! skip_right_p)
	IR_set_right_operand
	  (op, second_expr_processing (r, FALSE,
				      &r_op_result, &temp_vars_num, FALSE,
				      NULL, NULL, FALSE));
      bc = new_bc_code (bc_node_mode, src);
      BC_set_op2 (bc, l_op_result);
      BC_set_op3 (bc, r_op_result);
    }
  BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
  add_to_bcode (bc);
}

/* Check EXPR whose value in OP_NUM for a slice and, if it is so, add
   flatten node.  */
static do_inline void
add_flatten_node_if_necessary (IR_node_t expr, int op_num)
{
  BC_node_t flat;

  if (expr != NULL && IR_value_type (expr) == EVT_SLICE)
    {
      flat = new_bc_code (BC_NM_flat,
			  new_bc_node (BC_NM_source, IR_pos (expr)));
      BC_set_op1 (flat, op_num);
      add_to_bcode (flat);
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
  else if (IR_NODE_MODE (decl) == IR_NM_external_fun)
    return BC_NM_efun;
  else if (IR_NODE_MODE (decl) == IR_NM_fun) // ??? ifunc
    return BC_NM_fun;
  else if (IR_NODE_MODE (decl) == IR_NM_class)
    return BC_NM_class;
  else
    d_unreachable ();
  return BC_NM_var; /* No warnings */
}

/* Decl substitutions: map orginal decl num -> decl substitution.  */
static vlo_t decl_subst;

/* Initiate work with decl substitutions.  */
static void
initiate_decl_subst (void)
{
  VLO_CREATE (decl_subst, 1<<14);
}

/* Set up substitution SUBST for DECL.  */
static void
set_decl_subst (BC_node_t decl, BC_node_t subst)
{
  int decl_num = BC_decl_num (decl);
  BC_node_t null = NULL;

  while (VLO_LENGTH (decl_subst) <= decl_num * sizeof (BC_node_t))
    VLO_ADD_MEMORY (decl_subst, &null, sizeof (null));
  ((BC_node_t *) VLO_BEGIN (decl_subst))[decl_num] = subst;
}

/* Return the substitution of DECL if any or NULL otherwise.  */
static BC_node_t
get_decl_subst (BC_node_t decl)
{
  int decl_num = BC_decl_num (decl);

  if (VLO_LENGTH (decl_subst) <= decl_num * sizeof (BC_node_t))
    return NULL;
  return ((BC_node_t *) VLO_BEGIN (decl_subst))[decl_num];
}

/* Finish work with decl substitutions.  */
static void
finish_decl_subst (void)
{
  VLO_DELETE (decl_subst);
}

/* The last used number to enumerate decls.  */
static int last_decl_num;

/* Set unique decl_num for BC_DECL.  */
static void
set_new_decl_num (BC_node_t bc_decl)
{
  last_decl_num++;
  BC_set_decl_num (bc_decl, last_decl_num);
}

/* The func returns byte code decl node for corresponding DECL.  DECL
   must be only var, func or class. */
static BC_node_t
get_bcode_decl (IR_node_t decl)
{
  BC_node_t bc_decl, bc_block;
  IR_node_t unique_ident;

  d_assert (decl != NULL);
  if ((bc_decl = IR_bc_decl (decl)) != NULL)
    return bc_decl;
  if (IR_NODE_MODE (decl) == IR_NM_var)
    {
      bc_decl = new_bc_node (BC_NM_vdecl, IR_pos (decl));
      BC_set_var_num (bc_decl, IR_var_number_in_block (decl));
    }
  else if (IR_NODE_MODE (decl) == IR_NM_external_var)
    bc_decl = new_bc_node (BC_NM_evdecl, IR_pos (decl));
  else if (IR_NODE_MODE (decl) == IR_NM_external_fun)
    bc_decl = new_bc_node (BC_NM_efdecl, IR_pos (decl));
  else if (IR_NODE_MODE (decl) == IR_NM_fun)
    bc_decl = new_bc_node (BC_NM_fdecl, IR_pos (decl));
  else if (IR_NODE_MODE (decl) == IR_NM_class)
    bc_decl = new_bc_node (BC_NM_fdecl, IR_pos (decl));
  else
    d_unreachable ();
  set_new_decl_num (bc_decl);
  unique_ident = IR_unique_ident (IR_ident (decl));
  BC_set_ident (bc_decl, IR_ident_string (unique_ident));
  BC_set_fldid_num (bc_decl, IR_field_ident_number (unique_ident));
  bc_block = IR_bc_block (IR_scope (decl));
  d_assert (bc_block != NULL);
  BC_set_decl_scope (bc_decl, bc_block);
  BC_set_public_p (bc_decl, IR_access (decl) == PUBLIC_ACCESS);
  IR_set_bc_decl (decl, bc_decl);
  return bc_decl;
}

/* The func creates bc_decl_node (var, func or class) for
   corresponding DECL.  DECL must be only var, func or class. */
static BC_node_t
create_bc_ident (IR_node_t ident)
{
  IR_node_t decl = IR_decl (ident);
  BC_node_t bc, bc_decl = get_bcode_decl (decl);

  bc = new_bc_code_with_src (bc_decl_mode (decl), ident);
  BC_set_decl (bc, bc_decl);
  return bc;
}

/* Process actual parameters ACTUALS and add flatten nodes if
   necessary.  Return number of actuals.  */
static int
process_actuals (IR_node_t fun_decl, int fun_op_num,
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
      IR_set_expr (elist, second_expr_processing (IR_expr (elist), TRUE,
						 NULL, temp_vars_num, FALSE,
						 NULL, NULL, FALSE));
      add_flatten_node_if_necessary (IR_expr (elist), par_num);
    }
  return n;
}

/* Add move of var/function DECL with position of POS_NODE to the pc
   chain.  Define VAR_NUM and RESULT_NUM for the move.  */
static void
add_move (IR_node_t pos_node, BC_node_t decl, int result_num, int var_num)
{
  BC_node_t bc = new_bc_code_with_src (BC_NM_move, pos_node);

  BC_set_op1 (bc, result_num);
  BC_set_op2 (bc, var_num);
  BC_set_move_decl (bc, decl);
  add_to_bcode (bc);
}

/* Return ER type of corresponding IR type given by IRNM.  */
static int
ir2er_type (IR_node_mode_t irnm)
{
  switch (irnm)
    {
    case IR_NM_char_type: return type_char;
    case IR_NM_int_type: return type_int;
    case IR_NM_long_type: return type_long;
    case IR_NM_float_type: return type_float;
    case IR_NM_hide_type: return type_hide;
    case IR_NM_hideblock_type: return type_hideblock;
    case IR_NM_vec_type: return type_vect;
    case IR_NM_tab_type: return type_tab;
    case IR_NM_fun_type: return type_fun;
    case IR_NM_class_type: return type_class;
    case IR_NM_thread_type: return type_thread;
    case IR_NM_stack_type: return type_obj;
    case IR_NM_process_type: return type_process;
    case IR_NM_type_type: return type_type;
    default:
      d_unreachable ();
    }
}

/* Create and return copy of a bcode ORIGIN_BCODE.  It also creates a
   copy of info node.  */
static BC_node_t
copy_bcode (BC_node_t origin_bcode)
{
  BC_node_t bc = BC_copy_node (origin_bcode);
  BC_node_t info = BC_copy_node (BC_info (bc));

  BC_set_info (bc, info);
  BC_set_bc (info, bc);
  BC_set_prev_info (info, NULL);
  BC_set_next_info (info, NULL);
  return bc;
}

/* All funcs and classes blocks.  */
static vlo_t all_fblocks;

/* Get fblock of func class declaration FUN_DECL.  Create (or copy the
   origin fblock) if it was not created yet.  */
static inline BC_node_t
get_fblock (IR_node_t fun_decl)
{
  BC_node_t bc, fdecl;
  IR_node_t src, origin, block = IR_next_stmt (fun_decl);
  
  src = block;
  if (block == NULL || ! IR_IS_OF_TYPE (block, IR_NM_block)
      || IR_fun_class (block) != fun_decl)
    {
      /* Its is a forward declaration or used declaration. */
      d_assert (IR_forward_decl_flag (fun_decl)
		|| IR_origin_decl (fun_decl) != NULL);
      src = fun_decl;
    }
  if ((bc = IR_fdecl_bc_block (fun_decl)) == NULL)
    {
      fdecl = get_bcode_decl (fun_decl);
      IR_set_bc_decl (fun_decl, fdecl);
      if ((origin = IR_origin_decl (fun_decl)) == NULL)
	bc = new_bc_code_with_src (BC_NM_fblock, src);
      else
	{
	  bc = copy_bcode (IR_fdecl_bc_block (origin));
	  BC_set_source (BC_info (bc),
			 new_bc_node (BC_NM_source, IR_pos (src)));
	}
      VLO_ADD_MEMORY (all_fblocks, &bc, sizeof (bc));
      BC_set_fblock (fdecl, bc);
      IR_set_fdecl_bc_block (fun_decl, bc);
      if (src == block)
	IR_set_bc_block (block, bc);
      BC_set_decls (bc, NULL);
      BC_set_fdecl (bc, fdecl);
    }
  return bc;
}

static BC_node_t
gen_int_load (rint_t i, IR_node_t src)
{
  BC_node_t bc;

  if (int_p (i))
    {
      bc = new_bc_code_with_src (BC_NM_ldi, src);
      BC_set_op2 (bc, i);
    }
  else
    {
      bc = new_bc_code_with_src (BC_NM_ldbi, src);
      BC_set_bi (bc, i);
    }
  return bc;
}

/* The following recursive func passes (correctly setting up
   SOURCE_POSITION) EXPR (it may be NULL) and changes idents on
   corresponding decls occurrences according to languge visibility
   rules.  Also the func tests that ident in period operations is
   defined in any class.  Therefore the func returns pointer to
   (possibly) modified expr.  See also commentaries for abstract data
   block_decl_tables.  The func also tests operands types in
   operation.  The func creates chain by member next to evaluate the
   processed expr.  The chain forms polish postfix order.

   The function sets up flag THERE_IS_FUNCTION_CALL_IN_EXPR if there
   is a function call in EXPR.  Otherwise, the function does not
   change the variable value.

   The function also sets up flag extended_life_context depending on
   fun_class_assign_p.

   When we translate logical condition use FALSE_PC, TRUE_PC, and
   NEXT_FALSE_PC.  */

static IR_node_t
second_expr_processing (IR_node_t expr, int fun_class_assign_p,
		       int *result, int *curr_temp_vars_num, int lvalue_p,
		       BC_node_t false_pc, BC_node_t true_pc, int next_false_p)
{
  BC_node_t bc;
  IR_node_mode_t node_mode;
  IR_node_t l, r;
  int temp_vars_num = *curr_temp_vars_num;

  d_assert ((false_pc != NULL && true_pc != NULL)
	    || (false_pc == NULL && true_pc == NULL));
  if (expr == NULL)
    return expr;
  node_mode = IR_NODE_MODE (expr);
  switch (node_mode)
    {
    case IR_NM_char:
      bc = new_bc_code_with_src (BC_NM_ldch, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_op2 (bc, IR_char_value (IR_unique_char (expr)));
      IR_set_value_type (expr, EVT_CHAR);
      add_to_bcode (bc);
      break;
    case IR_NM_int:
      bc = gen_int_load (IR_int_value (IR_unique_int (expr)), expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_INT);
      add_to_bcode (bc);
      break;
    case IR_NM_long:
      bc = new_bc_code_with_src (BC_NM_ldl, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      mpz_init (*BC_mpz_ptr (bc));
      mpz_set (*BC_mpz_ptr (bc), *IR_mpz_ptr (IR_unique_long (expr)));
      IR_set_value_type (expr, EVT_INT);
      add_to_bcode (bc);
      break;
    case IR_NM_float:
      bc = new_bc_code_with_src (BC_NM_ldf, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_f (bc, IR_float_value (IR_unique_float (expr)));
      IR_set_value_type (expr, EVT_FLOAT);
      add_to_bcode (bc);
      break;
    case IR_NM_string:
      bc = new_bc_code_with_src (BC_NM_lds, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_str (bc, IR_string_value (IR_unique_string (expr)));
      IR_set_value_type (expr, EVT_VEC);
      add_to_bcode (bc);
      break;
    case IR_NM_nil:
      bc = new_bc_code_with_src (BC_NM_ldnil, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_UNKNOWN);
      add_to_bcode (bc);
      break;
    case IR_NM_this:
      bc = new_bc_code_with_src (BC_NM_ldthis, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      IR_set_value_type (expr, EVT_UNKNOWN);
      add_to_bcode (bc);
      break;
    case IR_NM_char_type:
    case IR_NM_int_type:
    case IR_NM_long_type:
    case IR_NM_float_type:
    case IR_NM_hide_type:
    case IR_NM_hideblock_type:
    case IR_NM_vec_type:
    case IR_NM_tab_type:
    case IR_NM_fun_type:
    case IR_NM_class_type:
    case IR_NM_thread_type:
    case IR_NM_stack_type:
    case IR_NM_process_type:
    case IR_NM_type_type:
      bc = new_bc_code_with_src (BC_NM_ldtp, expr);
      BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
      BC_set_op2 (bc, ir2er_type (node_mode));
      IR_set_value_type (expr, EVT_TYPE);
      add_to_bcode (bc);
      break;
    case IR_NM_ident:
      {
	IR_node_t ident;
	IR_node_t decl;

	ident = expr;
	decl = IR_decl (ident);
	SET_SOURCE_POSITION (expr);
	if (decl == NULL || IR_scope (decl) == NULL
	    || IR_pos (decl).file_name == NULL)
	  return NULL; /* no_position.  */
	if (IR_NODE_MODE (decl) == IR_NM_var
	    && curr_real_scope == IR_scope (decl))
	  {
	    /* local var */
	    if (result == NULL)
	      add_move (ident, get_bcode_decl (decl),
			setup_result_var_number (result, curr_temp_vars_num),
			IR_var_number_in_block (decl));
	    else
	      *result = IR_var_number_in_block (decl);
	  }
	else if (IR_NODE_MODE (decl) == IR_NM_var
		 && IR_block_scope (IR_scope (decl)) == NULL)
	  {
	    /* global var */
	    if (result == NULL)
	      add_move (ident, get_bcode_decl (decl),
			setup_result_var_number (result, curr_temp_vars_num),
			-IR_var_number_in_block (decl) - 1);
	    else
	      *result = -IR_var_number_in_block (decl) - 1;
	  }
	else
	  {
	    bc = create_bc_ident (ident);
	    if (! lvalue_p)
	      {
		BC_set_op1
		  (bc, setup_result_var_number (result, curr_temp_vars_num));
		add_to_bcode (bc);
	      }
	    else
	      {
		d_assert (result != NULL && *result == not_defined_result);
		/* We need 2 stack slots for non local var occurrence
		   lvalue representation.  */
		BC_set_op1
		  (bc, setup_result_var_number (result, curr_temp_vars_num));
		get_temp_stack_slot (curr_temp_vars_num);
		add_to_bcode (bc);
	      }
	  }
	if (IR_NODE_MODE (decl) == IR_NM_fun)
	  IR_set_value_type (expr, EVT_FUN);
	else if (IR_NODE_MODE (decl) == IR_NM_class)
	  IR_set_value_type (expr, EVT_CLASS);
	else
	  IR_set_value_type (expr, EVT_UNKNOWN);
	if (fun_class_assign_p
	    /* This is not the highest level block.  */
	    && IR_block_scope (curr_real_scope) != NULL
	    && (IR_IS_OF_TYPE (decl, IR_NM_fun)
		|| IR_IS_OF_TYPE (decl, IR_NM_class)))
	  {
	    IR_set_extended_life_context_flag (curr_real_scope, TRUE);
	    BC_set_ext_life_p (IR_bc_block (curr_real_scope), TRUE);
	  }
	break;
      }
    case IR_NM_period:
      {
	int op_result = not_defined_result;

	d_assert (! lvalue_p
		  || result == NULL || *result == not_defined_result);
	SET_SOURCE_POSITION (expr);
	bc = new_bc_code_with_src (BC_NM_fld, expr); // ??? should be decl
	BC_set_fldid (bc,
		      IR_ident_string (IR_unique_ident (IR_component (expr))));
	IR_set_designator
	  (expr, second_expr_processing (IR_designator (expr), FALSE,
					 &op_result, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	BC_set_op2 (bc, op_result);
	if (lvalue_p)
	  {
	    BC_set_op1 (bc, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  BC_set_op1
	    (bc, setup_result_var_number (result, curr_temp_vars_num));
	d_assert (IR_IS_OF_TYPE (IR_component (expr), IR_NM_ident));
	BC_set_op3
	  (bc, IR_field_ident_number (IR_unique_ident (IR_component (expr))));
	add_to_bcode (bc);
	d_assert (IR_component (expr) != NULL
		  && IR_NODE_MODE (IR_component (expr)) == IR_NM_ident);
	if (IR_designator (expr) != NULL)
	  {
	    if (!IR_it_is_declared_in_block (IR_unique_ident
					     (IR_component (expr))))
	      cont_err (source_position, ERR_decl_is_absent_in_a_block);
	  }
	break;
      }
    case IR_NM_logical_or:
    case IR_NM_logical_and:
      {
	BC_node_t lconv;
	int op_result;
	BC_node_t nop;

	bc = lconv = nop = NULL;
	SET_SOURCE_POSITION (expr);
	op_result = (result == NULL ? not_defined_result : *result);
	if (false_pc != NULL)
	  nop = new_bc_code_with_src (BC_NM_nop, expr);
	IR_set_operand
	  (expr,
	   second_expr_processing
	   (IR_operand (expr), FALSE, &op_result, &temp_vars_num, FALSE,
	    node_mode == IR_NM_logical_or ? nop : false_pc,
	    node_mode == IR_NM_logical_or ? true_pc : nop,
	    node_mode == IR_NM_logical_or));
	if (false_pc != NULL)
	  add_to_bcode (nop);
	else
	  {
	    bc = new_bc_code_with_src (node_mode == IR_NM_logical_or
				       ? BC_NM_brts : BC_NM_brfs,
				       expr);
	    BC_set_op1 (bc, op_result);
	    BC_set_res
	      (bc, setup_result_var_number (result, curr_temp_vars_num));
	    add_to_bcode (bc);
	    lconv = new_bc_code_with_src (BC_NM_lconv, expr);
	    BC_set_pc (bc, lconv);
	  }
	temp_vars_num = *curr_temp_vars_num;
	op_result = (result == NULL ? not_defined_result : *result);
	IR_set_cont_operand
	  (expr, second_expr_processing (IR_cont_operand (expr), FALSE,
					 &op_result, &temp_vars_num, FALSE,
					 false_pc, true_pc, FALSE));
	if (false_pc == NULL)
	  {
	    BC_set_op2 (lconv, op_result);
	    BC_set_op1 (lconv, BC_op1 (bc));
	    type_test (IR_operand (expr), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_logical_operation_operand_type);
	    type_test (IR_cont_operand (expr), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_logical_operation_operand_type);
	    add_to_bcode (lconv);
	  }
	false_pc = true_pc = NULL;
	IR_set_value_type (expr, EVT_INT);
      }
      break;
    case IR_NM_not:
      if (false_pc != NULL)
	{
	  int op_result = (result == NULL ? not_defined_result : *result);

	  second_expr_processing (IR_operand (expr), fun_class_assign_p,
				  &op_result, curr_temp_vars_num, lvalue_p,
				  true_pc, false_pc, TRUE);
	  IR_set_value_type (expr, EVT_INT);
	  false_pc = true_pc = NULL;
	}
      else
	{
	  process_unary_op (expr, result, curr_temp_vars_num);
	  l = IR_operand (expr);
	  type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		     ERR_invalid_logical_operation_operand_type);
	  IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_INT);
	}
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
			 BC_NM_lti, BC_NM_gti);
      goto common_cmp;
    case IR_NM_gt:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_gti, BC_NM_lti);
      goto common_cmp;
    case IR_NM_le:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_lei, BC_NM_gei);
      goto common_cmp;
    case IR_NM_ge:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_gei, BC_NM_lei);
    common_cmp:
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_order_comparison_operation_operand_type);
      type_test (r, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_order_comparison_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_concat:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_concat_operation_operand_type);
      type_test (r, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_concat_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_VEC);
      break;
    case IR_NM_in:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (r, EVT_TAB_SLICE_MASK, ERR_invalid_tab_type);
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
      type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      type_test (r, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_plus:
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_addi, BC_NM_addi);
      goto common_ar_op;
    case IR_NM_mult:
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM_multi, BC_NM_multi);
      goto common_ar_op;
    case IR_NM_minus:
    case IR_NM_div:
    case IR_NM_mod:
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
    common_ar_op:
      {
	type_mask_t type_mask1;
	type_mask_t type_mask2;
	
	type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		   ERR_invalid_arithmetic_operation_operand_type);
	type_test (r, EVT_NUMBER_VEC_SLICE_MASK,
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
	type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
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
      type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_arithmetic_operation_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_length:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VEC_TAB_SLICE_MASK,
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
      type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_conversion_to_char_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_CHAR);
      break;
    case IR_NM_intof:
    case IR_NM_longof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		 node_mode == IR_NM_intof
		 ? ERR_invalid_conversion_to_int_operand_type
		 : ERR_invalid_conversion_to_long_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_INT);
      break;
    case IR_NM_floatof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VEC_SLICE_MASK,
		 ERR_invalid_conversion_to_float_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_FLOAT);
      break;
    case IR_NM_vecof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VEC_TAB_SLICE_MASK,
		 ERR_invalid_conversion_to_vector_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_VEC);
      break;
    case IR_NM_format_vecof:
      process_binary_op (expr, result, curr_temp_vars_num,
			 BC_NM__error, BC_NM__error);
      l = IR_left_operand (expr); r = IR_right_operand (expr);
      type_test (l, EVT_NUMBER_VEC_TAB_SLICE_MASK,
		 ERR_invalid_conversion_to_vector_operand_type);
      type_test (r, EVT_VEC_SLICE_MASK, ERR_invalid_conversion_format_type);
      IR_set_value_type (expr, bin_slice_p (l, r) ? EVT_SLICE : EVT_VEC);
      break;
    case IR_NM_tabof:
      process_unary_op (expr, result, curr_temp_vars_num);
      l = IR_operand (expr);
      type_test (l, EVT_NUMBER_VEC_TAB_SLICE_MASK,
		 ERR_invalid_conversion_to_table_operand_type);
      IR_set_value_type (expr, unary_slice_p (l) ? EVT_SLICE : EVT_TAB);
      break;
    case IR_NM_funof:
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
	BC_node_t nop, true_path_end;
	int op_result;

        SET_SOURCE_POSITION (expr);
	op_result = not_defined_result;
        IR_set_cond_expr
	  (expr, second_expr_processing (IR_cond_expr (expr), FALSE,
					 &op_result, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	bc = new_bc_code_with_src (BC_NM_bfni, expr);
	BC_set_op1 (bc, op_result);
	add_to_bcode (bc);
	nop = new_bc_code_with_src (BC_NM_nop, expr);
	temp_vars_num = *curr_temp_vars_num;
	/* Put it on stack.  */
        IR_set_true_expr
	  (expr, second_expr_processing (IR_true_expr (expr),
					 fun_class_assign_p,
					 NULL, &temp_vars_num, FALSE,
					NULL, NULL, FALSE));
	d_assert (temp_vars_num = *curr_temp_vars_num + 1);
	true_path_end = BC_bc (curr_info);
	temp_vars_num = *curr_temp_vars_num;
	/* Put it on stack.  */
        IR_set_false_expr
	  (expr, second_expr_processing (IR_false_expr (expr),
					 fun_class_assign_p,
					 NULL, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	if (result != NULL)
	  *result = not_defined_result;
	setup_result_var_number (result, curr_temp_vars_num);
	/* Overall result should be on stack.  */
	d_assert (result == NULL
		  || (*result + 1 == temp_vars_num + curr_vars_number
		      && *curr_temp_vars_num == temp_vars_num));
	add_to_bcode (nop);
	BC_set_pc (bc, BC_next (true_path_end));
	BC_set_next (true_path_end, BC_bc (curr_info));
        type_test (IR_cond_expr (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_cond_type);
        IR_set_value_type (expr,
                           value_type (IR_true_expr (expr))
                           | value_type (IR_false_expr (expr)));
      }
      break;
    case IR_NM_vec:
    case IR_NM_tab:
      {
	int n = 0;
	IR_node_t elist;
	
	SET_SOURCE_POSITION (expr);
	bc = new_bc_code_with_src (node_mode == IR_NM_vec
				   ? BC_NM_vec : BC_NM_tab,
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
	       second_expr_processing (IR_repetition_key (elist), FALSE,
				       NULL, &temp_vars_num,
				       FALSE, NULL, NULL, FALSE));
            if (node_mode == IR_NM_vec)
              type_test (IR_repetition_key (elist), EVT_NUMBER_STRING_MASK,
                         ERR_invalid_repetition_type);
	    IR_set_expr
	      (elist, second_expr_processing (IR_expr (elist), TRUE,
					      NULL, &temp_vars_num, FALSE,
					      NULL, NULL, FALSE));
	  }
	BC_set_op3 (bc, n);
	IR_set_value_type (expr, node_mode == IR_NM_vec ? EVT_VEC : EVT_TAB);
	add_to_bcode (bc);
	break; 
      }
    case IR_NM_index:
      {
	int op_result = not_defined_result;

	d_assert (! lvalue_p
		  || result == NULL || *result == not_defined_result);
	SET_SOURCE_POSITION (expr);
	bc = new_bc_code_with_src (BC_NM_ind, expr);
	IR_set_designator
	  (expr, second_expr_processing (IR_designator (expr), FALSE,
					 &op_result, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	BC_set_op2 (bc, op_result);
	if (lvalue_p)
	  *curr_temp_vars_num = temp_vars_num;
	op_result = not_defined_result;
	IR_set_component
	  (expr, second_expr_processing (IR_component (expr), FALSE,
					 &op_result, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	BC_set_op3 (bc, op_result);
	if (lvalue_p)
	  {
	    BC_set_op1 (bc, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
	add_to_bcode (bc);
	type_test (IR_designator (expr), EVT_VEC_TAB_SLICE_MASK,
		   ERR_invalid_vec_tab_type);
	break;
      }
    case IR_NM_slice:
      {
	IR_node_t des = IR_designator (expr);

	d_assert (! lvalue_p
		  || result == NULL || *result == not_defined_result);
	SET_SOURCE_POSITION (expr);
	bc = new_bc_code_with_src (BC_NM_sl, expr);
	if (des != NULL)
	  {
	    BC_set_op3 (bc, 1); /* dimension */
	    BC_set_op2 (bc, temp_vars_num + curr_vars_number);
	    des = second_expr_processing (des, FALSE, NULL, &temp_vars_num,
					  IR_IS_OF_TYPE (des, IR_NM_slice),
					  NULL, NULL, FALSE);
	    if (des != NULL && IR_IS_OF_TYPE (des, IR_NM_slice))
	      {
		d_assert (BC_NODE_MODE (curr_pc) == BC_NM_sl);
		BC_set_op3 (bc, BC_op3 (curr_pc) + 1); /* dimension */
		BC_set_op2 (bc, BC_op2 (curr_pc));
		curr_pc = prev_pc;
		curr_info = BC_prev_info (curr_info);
	      }
	  }
	IR_set_designator (expr, des);
	IR_set_component
	  (expr, second_expr_processing (IR_component (expr), FALSE,
					 NULL, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	IR_set_bound
	  (expr, second_expr_processing (IR_bound (expr), FALSE,
					 NULL, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	IR_set_step
	  (expr, second_expr_processing (IR_step (expr), FALSE,
					 NULL, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	if (lvalue_p)
	  {
	    BC_set_op1 (bc, -1);
	    *curr_temp_vars_num = temp_vars_num;
	  }
	else
	  BC_set_op1 (bc, setup_result_var_number (result, curr_temp_vars_num));
	add_to_bcode (bc);
	type_test (IR_designator (expr), EVT_VEC_SLICE_MASK,
		   ERR_invalid_vec_slice_type);
	type_test (IR_component (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_slice_start_type);
	type_test (IR_bound (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_slice_bound_type);
	type_test (IR_step (expr), EVT_NUMBER_STRING_MASK,
		   ERR_invalid_slice_step_type);
	IR_set_value_type (expr, EVT_SLICE);
	break;
      }
    case IR_NM_class_fun_thread_call:
      {
	int pars_num, fun_op_num;
	pc_t saved_prev_pc;
	IR_node_t fun_decl = NULL;
	int general_p = TRUE, env_p = FALSE, top_p = FALSE;
	    
	there_is_function_call_in_expr = TRUE;
	SET_SOURCE_POSITION (expr);
	fun_op_num = temp_vars_num + curr_vars_number;
	saved_prev_pc = curr_pc;
	IR_set_fun_expr
	  (expr, second_expr_processing (IR_fun_expr (expr), FALSE,
					 NULL, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	if (BC_NODE_MODE (curr_pc) == BC_NM_fun)
	  {
	    d_assert (IR_NODE_MODE (IR_fun_expr (expr)) == IR_NM_ident);
	    fun_decl = IR_decl (IR_fun_expr (expr));
	    env_p = strcmp (IR_pos (fun_decl).file_name,
			    ENVIRONMENT_PSEUDO_FILE_NAME) == 0;
	    top_p = IR_block_scope (IR_scope (fun_decl)) == NULL;
	    general_p
	      = ((! env_p
		  && (IR_args_flag (fun_decl)
		      || (IR_IS_OF_TYPE (fun_decl, IR_NM_fun)
			  && IR_thread_flag (fun_decl))
		      || (IR_class_fun_thread_call_parameters_number (expr)
			  != IR_parameters_number (fun_decl))))
		 || (env_p && ! top_p));
	    if (! general_p)
	      {
		/* Remove load func value.  */
		d_assert (fun_op_num - curr_vars_number + 1
			  == temp_vars_num);
		temp_vars_num--;
		curr_pc = prev_pc;
		prev_pc = saved_prev_pc;
		curr_info = BC_prev_info (curr_info);
	      }
	  }
	pars_num = process_actuals (fun_decl, fun_op_num,
				    IR_actuals (expr), &temp_vars_num);
	if (result != NULL)
	  *result = not_defined_result;
	setup_result_var_number (result, curr_temp_vars_num);
	if (general_p)
	  bc = new_bc_code_with_src (BC_NM_call, expr);
	else
	  {
	    IR_node_t block;
	    BC_node_t fblock;

	    d_assert (!env_p || top_p);
	    if (pars_num == 0)
	      get_temp_stack_slot (&temp_vars_num); /* for result */
	    bc = new_bc_code_with_src (env_p
				       ? BC_NM_ibcall
				       : top_p
				       ?  BC_NM_ticall
				       : IR_scope (fun_decl) == curr_real_scope
				       ? BC_NM_cicall : BC_NM_icall,
				       expr);
	    fblock = get_fblock (fun_decl);
	    BC_set_cfblock (bc, fblock);
	    if (fun_decl != NULL && !IR_forward_decl_flag (fun_decl)
		&& (block = IR_next_stmt (fun_decl)) != NULL
		&& IR_IS_OF_TYPE (block, IR_NM_block)
		&& IR_fun_class (block) == fun_decl
		&& IR_hint (block) == INLINE_HINT)
	      BC_set_inline_p (BC_info (bc), TRUE);
	  }
	BC_set_op1 (bc, fun_op_num);
	BC_set_op2 (bc, pars_num);
	add_to_bcode (bc);
	if (!IT_IS_OF_TYPE (IR_fun_expr (expr), EVT_FUN)
	    && !IT_IS_OF_TYPE (IR_fun_expr (expr), EVT_CLASS))
	  cont_err (source_position,
		    ERR_invalid_class_fun_thread_designator);
	IR_set_value_type (expr, EVT_UNKNOWN);
	break;
      }
    default:
      d_unreachable ();
    }
  if (false_pc != NULL)
    {
      int op_result = setup_result_var_number (node_mode != IR_NM_ident
					       ? result : NULL,
					       curr_temp_vars_num);

      if (IR_value_type (expr) != EVT_INT)
	{
	  bc = new_bc_code_with_src (BC_NM_lconv, expr);
	  BC_set_op1 (bc, op_result);
	  BC_set_op2 (bc, *result);
	  add_to_bcode (bc);
	  type_test (expr, EVT_NUMBER_VEC_SLICE_MASK,
		     ERR_invalid_logical_operation_operand_type);
	  IR_set_value_type (expr, EVT_INT);
	}
      if (next_false_p)
	{
	  bc = new_bc_code_with_src (BC_NM_bt, expr);
	  BC_set_pc (bc, true_pc);
	  BC_set_next (bc, false_pc);
	}
      else
	{
	  bc = new_bc_code_with_src (BC_NM_bf, expr);
	  BC_set_pc (bc, false_pc);
	  BC_set_next (bc, true_pc);
	}
      BC_set_op1 (bc, op_result);
      add_to_bcode (bc);
      curr_pc = NULL;
    }
  return expr;
}

/* Return new mode of designator node.  Difference between value and
   lvalue is needed for the evaluator.  The following table are used
   for that

   BC_NM_fld                  BC_NM_lfld(v)
   BC_NM_ind                  BC_NM_ind
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
      return BC_NM_ind;
    case BC_NM_sl:
      return (val_p ? BC_NM_lslv : BC_NM_sl);
    case BC_NM_var:
      return (val_p ? BC_NM_lvarv : BC_NM_lvar);
    case BC_NM_evar:
      return (val_p ? BC_NM_levarv : BC_NM_lvar);
    default:
      cont_err (BC_pos (BC_source (BC_info (designator))),
		error_message);
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

/* Find and return first fun block covering block SCOPE.  A block
   always covers itself.  */
static IR_node_t
find_covered_fun_class (IR_node_t scope)
{
  while (scope != NULL && IR_fun_class (scope) == NULL)
    scope = IR_block_scope (scope);
  if (scope != NULL)
    return IR_fun_class (scope);
  else
    return NULL;
}


/* Return location (through CONTAINER_NUM and INDEX_NUM) of a lvalue
   DESIGNATOR.  If the designator is local var occurrence, create
   first corresponding lvarv node and add it to the pc chain.  */
static void
get_lvalue_location (BC_node_t designator, IR_node_t ir_des,
		     int *temp_vars_num, int *container_num,
		     int *index_num)
{
  BC_node_t bc, bc_decl;
  
  if (designator == NULL)
    {
      d_assert (ir_des != NULL && IR_IS_OF_TYPE (ir_des, IR_NM_ident));
      bc_decl = get_bcode_decl (IR_decl (ir_des));
      bc = new_bc_code (BC_NM_lvar, bc_decl);
      BC_set_decl (bc, bc_decl);
      add_to_bcode (bc);
      BC_set_op1 (bc, get_temp_stack_slot (temp_vars_num));
      *container_num = BC_op1 (bc);
      *index_num = get_temp_stack_slot (temp_vars_num);
    }
  else if (BC_IS_OF_TYPE (designator, BC_NM_ind))
    {
      /* Remove the bc code: */
      d_assert (curr_pc == designator);
      curr_pc = prev_pc;
      curr_info = BC_prev_info (curr_info);
      *container_num = BC_op2 (designator);
      *index_num = BC_op3 (designator);
    }
  else if (BC_IS_OF_TYPE (designator, BC_NM_lfld))
    {
      BC_set_op1 (designator, get_temp_stack_slot (temp_vars_num));
      *container_num = BC_op1 (designator);
      *index_num = get_temp_stack_slot (temp_vars_num);
    }
  else
    {
      *container_num = BC_op1 (designator);
      *index_num = BC_op1 (designator) + 1;
    }
}

/* Return pointer to the block of first function decl surrounding
   given SCOPE (it may be NULL).  Return NULL if there is not such
   function decl.  */
static IR_node_t
surrounding_fun_block (IR_node_t scope)
{
  IR_node_t fun_class;

  for (; scope != NULL; scope = IR_block_scope (scope))
    {
      fun_class = IR_fun_class (scope);
      if (fun_class != NULL && IR_NODE_MODE (fun_class) == IR_NM_fun)
	return scope;
    }
  return NULL;
}

/* Add BC_DECL to cycle decl list of BC_BLOCK.  */
static void inline
add_decl_to_block (BC_node_t bc_decl, BC_node_t bc_block)
{
  BC_node_t last;

  if ((last = BC_decls (bc_block)) == NULL)
    BC_set_next_decl (bc_decl, bc_decl);
  else
    {
      BC_set_next_decl (bc_decl, BC_next_decl (last));
      BC_set_next_decl (last, bc_decl);
    }
  BC_set_decls (bc_block, bc_decl);
}

/* All BC copies created during copy_fun_bc_block.  */
static vlo_t bc_copies;

/* Make a copy of bcode BC, link it to the current chain, put it into
   bc_copies, and set up it as substituation of BC.  */
static void
copy_and_link_bcode (BC_node_t bc)
{
  BC_node_t new_bc;

  new_bc = copy_bcode (bc);
  BC_set_subst (BC_info (bc), new_bc);
  link_info (BC_info (new_bc));
  VLO_ADD_MEMORY (bc_copies, &new_bc, sizeof (new_bc));
}

static void copy_bc_block (BC_node_t origin_bc_block, BC_node_t bc_block);

static void
copy_bc_decls (BC_node_t bc_block, BC_node_t origin_bc_block, int var_base)
{
  BC_node_t bc, bc_decl, last_bc_decl, next_bc_decl, origin_bc_decl;

  for (last_bc_decl = BC_decls (bc_block);
       last_bc_decl != NULL
	 && (next_bc_decl = BC_next_decl (last_bc_decl)) != NULL;
       last_bc_decl = next_bc_decl)
    ;
  /* Make decls copy:  */
  for (origin_bc_decl = BC_decls (origin_bc_block);
       origin_bc_decl != NULL;
       origin_bc_decl = BC_next_decl (origin_bc_decl))
    {
      bc_decl = BC_copy_node (origin_bc_decl);
      BC_set_next_decl (bc_decl, NULL);
      set_new_decl_num (bc_decl);
      set_decl_subst (origin_bc_decl, bc_decl);
      VLO_ADD_MEMORY (bc_copies, &bc_decl, sizeof (bc_decl));
      if (last_bc_decl == NULL)
	BC_set_decls (bc_block, bc_decl);
      else
	BC_set_next_decl (last_bc_decl, bc_decl);
      last_bc_decl = bc_decl;
      if (BC_IS_OF_TYPE (bc_decl, BC_NM_vdecl))
	BC_set_var_num (bc_decl, BC_var_num (bc_decl) + var_base);
      else if (BC_IS_OF_TYPE (bc_decl, BC_NM_fdecl))
	{
	  bc = copy_bcode (BC_fblock (origin_bc_decl));
	  BC_set_fblock (bc_decl, bc);
	  BC_set_fdecl (bc, bc_decl);
	  VLO_ADD_MEMORY (all_fblocks, &bc, sizeof (bc));
	  copy_bc_block (BC_fblock (origin_bc_decl), bc);
	}
    }
}

/* Make a copy of bcode and decls of ORIGIN_BC_BLOCK and add it to
   BC_BLOCK.  It is done without changing BC fields.  */
static void
copy_bc_block (BC_node_t origin_bc_block, BC_node_t bc_block)
{
  BC_node_t info, bc;
  BC_node_t saved_curr_info = curr_info;
  int level = 0;

  BC_set_subst (BC_info (origin_bc_block), bc_block);
  curr_info = NULL;
  link_info (BC_info (bc_block));
  VLO_ADD_MEMORY (bc_copies, &bc_block, sizeof (bc_block));
  /* Make bcode copy:  */
  for (info = BC_next_info (BC_info (origin_bc_block));;
       info = BC_next_info (info))
    {
      bc = BC_bc (info);
      copy_and_link_bcode (bc);
      if (BC_IS_OF_TYPE (bc, BC_NM_block))
	level++;
      else if (BC_IS_OF_TYPE (bc, BC_NM_bend))
	{
	  if (level == 0)
	    break;
	  level--;
	}
    }
  d_assert (BC_IS_OF_TYPE (bc, BC_NM_fbend));
  BC_set_decls (bc_block, NULL);
  copy_bc_decls (bc_block, origin_bc_block, 0);
  curr_info = saved_curr_info;
}

/* Modify all pc and decl fields of copied nodes correspondingly.
   Exclude ones for changing cfblock and decl fields if EXCEPT returns
   TRUE.  */
static void
modify_copied_pc (int (*except) (BC_node_t))
{
  BC_node_t fv, bc, *bc_ptr;
  
  for (bc_ptr = (BC_node_t *) VLO_BEGIN (bc_copies);
       bc_ptr < (BC_node_t *) VLO_BOUND (bc_copies);
       bc_ptr++)
    {
      bc = *bc_ptr;
      if (BC_IS_OF_TYPE (bc, BC_NM_bcode) && (fv = BC_next (bc)) != NULL
	  && (fv = BC_subst (BC_info (fv))) != NULL)
	BC_set_next (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_decl) && (fv = BC_decl_scope (bc)) != NULL
	  && (fv = BC_subst (BC_info (fv))) != NULL)
	BC_set_decl_scope (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_block))
	{
	  if ((fv = BC_scope (bc)) != NULL
	      && (fv = BC_subst (BC_info (fv))) != NULL)
	    BC_set_scope (bc, fv);
	  if ((fv = BC_excepts (bc)) != NULL
	      && (fv = BC_subst (BC_info (fv))) != NULL)
	    BC_set_excepts (bc, fv);
	  if ((fv = BC_friends (bc)) != NULL
	      && (fv = BC_subst (BC_info (fv))) != NULL)
	    BC_set_friends (bc, fv);
	  if ((fv = BC_uses (bc)) != NULL
	      && (fv = BC_subst (BC_info (fv))) != NULL)
	    BC_set_uses (bc, fv);
	}
      if (BC_IS_OF_TYPE (bc, BC_NM_except) && (fv = BC_next_except (bc)) != NULL
	  && (fv = BC_subst (BC_info (fv))) != NULL)
	BC_set_next_except (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_fdecl) && (fv = BC_fblock (bc)) != NULL
	  && (fv = BC_subst (BC_info (fv))) != NULL)
	BC_set_fblock (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_op1_decl) && (fv = BC_decl (bc)) != NULL
	  && (except == NULL || ! except (fv))
	  && (fv = get_decl_subst (fv)) != NULL)
	BC_set_decl (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_br) && (fv = BC_pc (bc)) != NULL
	  && (fv = BC_subst (BC_info (fv))) != NULL)
	BC_set_pc (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_imcall) && (fv = BC_cfblock (bc)) != NULL
	  && (except == NULL || ! except (fv))
	  && (fv = BC_subst (BC_info (fv))) != NULL)
	BC_set_cfblock (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_foreach) && (fv = BC_body_pc (bc)) != NULL
	  && (fv = BC_subst (BC_info (fv))) != NULL)
	BC_set_body_pc (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_move) && (fv = BC_move_decl (bc)) != NULL
	  && (fv = get_decl_subst (fv)) != NULL)
	BC_set_move_decl (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_ret) && (fv = BC_ret_decl (bc)) != NULL
	  && (fv = get_decl_subst (fv)) != NULL)
	BC_set_ret_decl (bc, fv);
      if (BC_IS_OF_TYPE (bc, BC_NM_bend) && (fv = BC_block (bc)) != NULL
	  && (fv = BC_subst (BC_info (fv))) != NULL)
	BC_set_block (bc, fv);
    }
}

/* Make copy of ORIGIN_FUN BC and add it to FUN.  Change all necessary
   fields in the copied nodes.  This function is very sensitive to
   changes BC description.  So modify it approprietly after BC
   description changes.  */
static void
copy_fun_bc_block (IR_node_t fun, IR_node_t original_fun)
{
  BC_node_t bc_block, bc;
  
  VLO_NULLIFY (bc_copies);
  bc = get_fblock (fun);
  BC_set_scope (bc, curr_bc_scope);
  bc_block = IR_fdecl_bc_block (original_fun);
  copy_bc_block (bc_block, bc);
  modify_copied_pc (NULL);
}

/* This recursive func passes (correctly setting up SOURCE_POSITION
   and curr_scope (before first call of the func curr_scope is to be
   NULL)) all stmts on the same stmt nesting level and sets up
   elements values of class decls idents tables (see also commentaries
   for corresponding abstract data).  FIRST_LEVEL_STMT (it may be
   NULL) is first stmt of the processed stmt nesting level.  

   The func collects all funcs and classes blocks in all_fblocks.

   The func creates the byte code for execution of the processed
   stmts.

   The funcs also fixes repeated errors ERR_continue_is_not_in_loop,
   ERR_break_is_not_in_loop. */
static void
second_block_passing (IR_node_t first_level_stmt, int block_p)
{
  BC_node_t bc, var_bc, before_pc, src;
  BC_node_mode_t bc_node_mode, var_mode;
  IR_node_t stmt, next_stmt, decl;
  IR_node_t temp;
  IR_node_mode_t stmt_mode;
  int result, var_result, temp_vars_num, val_p;
  int container_num, index_num;

  for (stmt = first_level_stmt; stmt != NULL; stmt = next_stmt)
    {
      next_stmt = IR_next_stmt (stmt);
      temp_vars_num = 0;
      SET_SOURCE_POSITION (stmt);
      switch (stmt_mode = IR_NODE_MODE (stmt))
	{
	case IR_NM_expr_stmt:
	  result = not_defined_result;
	  IR_set_stmt_expr
	    (stmt,
	     second_expr_processing (IR_stmt_expr (stmt), TRUE,
				     &result, &temp_vars_num, FALSE,
				     NULL, NULL, FALSE));
	  bc = NULL;
	  if (repl_flag)
	    bc = (new_bc_code_with_src
		  (IR_IS_OF_TYPE (IR_stmt_expr (stmt),
				  IR_NM_class_fun_thread_call)
		   ? BC_NM_rpr_def : BC_NM_rpr, stmt));
	  /* Last expr-stmt of fun is returned.  */
	  else if (block_p
		   && next_stmt == NULL
		   && (temp = IR_fun_class (curr_scope)) != NULL
		   && IR_IS_OF_TYPE (temp, IR_NM_fun)
		   && ! IR_thread_flag (temp))
	    {
	      bc = new_bc_code_with_src (BC_NM_ret, stmt);
	      /* Make tail calls  */
	      if (BC_NODE_MODE (curr_pc) == BC_NM_icall)
		BC_SET_MODE (curr_pc, BC_NM_itcall);
	      else if (BC_NODE_MODE (curr_pc) == BC_NM_ticall)
		BC_SET_MODE (curr_pc, BC_NM_titcall);
	      else if (BC_NODE_MODE (curr_pc) == BC_NM_cicall)
		BC_SET_MODE (curr_pc, BC_NM_citcall);
	    }
	  if (bc != NULL)
	    {
	      BC_set_op1 (bc, result);
	      add_to_bcode (bc);
	    }
	  break;
	case IR_NM_assign:
	case IR_NM_var_assign:
	  bc_node_mode = BC_NM_sts;
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
	  var_result = not_defined_result;
	  before_pc = curr_pc;
	  src = new_bc_node (BC_NM_source2, IR_pos (stmt));
	  BC_set_pos2 (src, IR_pos (stmt));
	  temp = second_expr_processing (IR_assignment_var (stmt), FALSE,
					 &var_result, &temp_vars_num, TRUE,
					 NULL, NULL, FALSE);
	  if (temp != NULL)
	    IR_set_assignment_var (stmt, temp);
	  var_bc = before_pc != curr_pc ? curr_pc : NULL; /* local var */
	  bc = NULL;
	  val_p = stmt_mode != IR_NM_assign && stmt_mode != IR_NM_var_assign;
	  if (temp != NULL && var_bc != NULL)
	    {
	      var_mode = BC_NODE_MODE (var_bc);
	      if (bc_node_mode == BC_NM_sts)
		{
		  if (IR_IS_OF_TYPE (temp, IR_NM_index))
		    bc_node_mode = BC_NM_stvt;
		  else if (var_mode == BC_NM_evar)
		    bc_node_mode = BC_NM_ste;
		}
	      bc = new_bc_code (bc_node_mode, src);
	      BC_set_pos2 (src, IR_pos (IR_assignment_var (stmt)));
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
		  BC_set_op2 (bc, BC_op1 (var_bc) + 1); /* not used */
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
				 || var_mode == BC_NM_sl))
		{
		  BC_set_op4 (bc, get_temp_stack_slot (&temp_vars_num));
		  BC_set_op1 (var_bc, BC_op4 (bc));
		}
	      else if (val_p)
		BC_set_op4 (bc, -1);
	      if (! val_p && (var_mode == BC_NM_ind
			      || var_mode == BC_NM_sl))
		{
		  curr_pc = prev_pc;
		  curr_info = BC_prev_info (curr_info);
		}
	    }
	  if (stmt_mode != IR_NM_var_assign && temp != NULL
	      && IR_IS_OF_TYPE (temp, IR_NM_ident)
	      && (IR_IS_OF_TYPE (IR_decl (temp), IR_NM_var)
		  || IR_IS_OF_TYPE (IR_decl (temp), IR_NM_external_var))
	      && IR_const_flag (IR_decl (temp)))
	    cont_err (IR_pos (temp), ERR_const_assignment,
		      IR_ident_string (IR_unique_ident
				       (IR_ident (IR_decl (temp)))));
	  if (temp != NULL && var_bc == NULL
	      && (stmt_mode == IR_NM_assign || stmt_mode == IR_NM_var_assign))
	    result = var_result;
	  else
	    result = not_defined_result;
	  if (var_bc != NULL)
	    {
	      IR_node_t expr;

	      IR_set_assignment_expr
		(stmt,
		 second_expr_processing (IR_assignment_expr (stmt), TRUE,
					 &result, &temp_vars_num, FALSE,
					 NULL, NULL, FALSE));
	      expr = IR_assignment_expr (stmt);
	      BC_set_op3 (bc, result);
	      if (temp != NULL && ! IR_IS_OF_TYPE (temp, IR_NM_slice))
		add_flatten_node_if_necessary (temp, result);
	      if (expr != NULL
		  /* Local or top level variable or call: */
		  && (result < IR_vars_number (curr_real_scope)
		      || IR_IS_OF_TYPE (expr, IR_NM_class_fun_thread_call)))
		{
		  if (bc_node_mode == BC_NM_sts)
		    BC_SET_MODE (bc, BC_NM_stsu);
		  else if (bc_node_mode == BC_NM_stvt)
		    BC_SET_MODE (bc, BC_NM_stvtu);
		  else if (bc_node_mode == BC_NM_ste)
		    BC_SET_MODE (bc, BC_NM_steu);
		}
	      add_to_bcode (bc);
	    }
	  else /* assignment to the local var.  */
	    {
	      IR_node_t val = IR_assignment_expr (stmt);

	      if (stmt_mode == IR_NM_assign || stmt_mode == IR_NM_var_assign)
		{
		  IR_set_assignment_expr
		    (stmt,
		     second_expr_processing (val, TRUE,
					     &result, &temp_vars_num, FALSE,
					     NULL, NULL, FALSE));
		  if (temp != NULL && result != var_result)
		    {
		      IR_node_t ident = IR_assignment_expr (stmt);

		      if (! IR_IS_OF_TYPE (ident, IR_NM_ident))
			ident = temp;
		      add_move (stmt, get_bcode_decl (IR_decl (ident)),
				var_result, result);
		    }
		}
	      else
		{
		  IR_node_mode_t op_mode = IR_NODE_MODE (stmt);

		  if ((op_mode == IR_NM_plus_assign || op_mode == IR_NM_mult_assign)
		      && IR_IS_OF_TYPE (val, IR_NM_int)
		      && temp != NULL && ! IR_IS_OF_TYPE (temp, IR_NM_slice))
		    {
		      bc = new_bc_code (op_mode == IR_NM_plus_assign
					? BC_NM_addi : BC_NM_multi,
					src);
		      BC_set_op3 (bc, IR_int_value (IR_unique_int (val)));
		      BC_set_op2 (bc, var_result);
		    }
		  else
		    {
		      IR_set_assignment_expr
			(stmt,
			 second_expr_processing (val, TRUE,
						 &result, &temp_vars_num, FALSE,
						 NULL, NULL, FALSE));
		      /* Generate op (var_result, var_result, result).  */
		      bc = new_bc_code (make_op_mode (stmt), src);
		      BC_set_op2 (bc, var_result);
		      BC_set_op3 (bc, result);
		    }
		  if (IR_assignment_var (stmt) != NULL)
		    BC_set_pos2 (src, IR_pos (IR_assignment_var (stmt)));
		  BC_set_op1 (bc, var_result);
		  add_to_bcode (bc);
		}
	      if (temp != NULL && ! IR_IS_OF_TYPE (temp, IR_NM_slice))
		add_flatten_node_if_necessary (val, var_result);
	    }
	  break;
	case IR_NM_par_assign:
	  {
	    BC_node_t st_bc, aend_bc, lvalue_bc;

	    var_result = not_defined_result;
	    before_pc = curr_pc;
	    temp = second_expr_processing (IR_assignment_var (stmt), FALSE,
					   &var_result, &temp_vars_num, FALSE,
					   NULL, NULL, FALSE);
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
	      {
		curr_pc = prev_pc; /* Remove var.  */
		curr_info = BC_prev_info (curr_info);
	      }
	    bc = new_bc_code_with_src (BC_NM_btdef, IR_assignment_var (stmt));
	    BC_set_op1 (bc, var_result);
	    aend_bc = new_bc_code_with_src (BC_NM_nop, stmt);
	    BC_set_pc (bc, aend_bc);
	    add_to_bcode (bc);
	    temp_vars_num = 0;
	    lvalue_bc = NULL;
	    if (temp != NULL)
	      {
		if (var_bc == NULL)
		  result = var_result;
		else
		  {
		    result = not_defined_result;
		    bc_node_mode
		      = make_designator_lvalue (var_bc,
						ERR_non_variable_in_assignment,
						FALSE);
		    if (IR_IS_OF_TYPE (temp, IR_NM_ident))
		      lvalue_bc	= new_bc_code (bc_node_mode,
					       get_bcode_decl (IR_decl (temp)));
		    else
		      lvalue_bc = new_bc_code_with_src (bc_node_mode, temp);
		    BC_set_decl (lvalue_bc, BC_decl (var_bc));
		    /* We need two stack slots for lvalue var
		       occurrence.  */
		    BC_set_op1 (lvalue_bc,
				get_temp_stack_slot (&temp_vars_num));
		    get_temp_stack_slot (&temp_vars_num);
		    add_to_bcode (lvalue_bc);
		  }
	      }
	    IR_set_assignment_expr
	      (stmt, second_expr_processing (IR_assignment_expr (stmt), TRUE,
					     &result, &temp_vars_num, FALSE,
					     NULL, NULL, FALSE));
	    add_flatten_node_if_necessary (IR_assignment_expr (stmt), result);
	    if (lvalue_bc != NULL)
	      {
		st_bc = new_bc_code_with_src (BC_NM_sts, stmt);
		BC_set_op1 (st_bc, BC_op1 (lvalue_bc));
		BC_set_op2 (st_bc, BC_op1 (lvalue_bc) + 1);
		BC_set_op3 (st_bc, result);
		add_to_bcode (st_bc);
	      }
	    else if (result != var_result)
	      add_move (stmt, get_bcode_decl (IR_decl (temp)),
			var_result, result);
	    add_to_bcode (aend_bc);
	  }
	  break;
	case IR_NM_if_stmt:
	  {
	    BC_node_t if_finish, if_part_end_info;
	    BC_node_t false_pc = new_bc_code_with_src (BC_NM_nop, stmt);
	    BC_node_t true_pc = new_bc_code_with_src (BC_NM_nop, stmt);
	    
	    result = not_defined_result;
	    IR_set_if_expr
	      (stmt, second_expr_processing (IR_if_expr (stmt), FALSE,
					     &result, &temp_vars_num, FALSE,
					     false_pc, true_pc, FALSE));
	    add_to_bcode (true_pc);
	    if_finish = new_bc_code_with_src (BC_NM_nop, stmt);
	    second_block_passing (IR_if_part (stmt), FALSE);
	    if_part_end_info = curr_info;
	    add_to_bcode (false_pc);
	    second_block_passing (IR_else_part (stmt), FALSE);
	    add_to_bcode (if_finish);
	    bc = BC_bc (if_part_end_info);
	    if (BC_NODE_MODE (bc) != BC_NM_out) 
	      /* It is not a break or continue  */
	      BC_set_next (bc, BC_bc (curr_info));
	    break;
	  }
	case IR_NM_for_stmt:
	  {
	    BC_node_t before_guard_expr, before_body, initial_end;
	    BC_node_t saved_start_next_iteration, saved_for_finish;
	    int saved_number_of_surrounding_blocks;
	    
	    saved_number_of_surrounding_blocks = number_of_surrounding_blocks;
	    number_of_surrounding_blocks = 0;
	    saved_start_next_iteration = start_next_iteration;
	    saved_for_finish = for_finish;
	    second_block_passing (IR_for_initial_stmt (stmt), FALSE);
	    initial_end = BC_bc (curr_info);
	    start_next_iteration = new_bc_code_with_src (BC_NM_nop, stmt);
	    before_body = new_bc_code_with_src (BC_NM_nop, stmt);
	    for_finish = new_bc_code_with_src (BC_NM_nop, stmt);
	    add_to_bcode (before_body);
	    second_block_passing (IR_for_stmts (stmt), FALSE);
	    add_to_bcode (start_next_iteration);
	    second_block_passing (IR_for_iterate_stmt (stmt), FALSE);
	    d_assert (curr_info != NULL);
	    before_guard_expr = BC_bc (curr_info);
	    result = not_defined_result;
	    IR_set_for_guard_expr
	      (stmt, second_expr_processing (IR_for_guard_expr (stmt), FALSE,
					     &result, &temp_vars_num, FALSE,
					     for_finish, before_body,
					     TRUE));
	    type_test (IR_for_guard_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_for_guard_expr_type);
	    add_to_bcode (for_finish);
	    BC_set_next (initial_end, BC_next (before_guard_expr));
	    number_of_surrounding_blocks = saved_number_of_surrounding_blocks;
	    start_next_iteration = saved_start_next_iteration;
	    for_finish = saved_for_finish;
	    break;
	  }
	case IR_NM_foreach_stmt:
	  {
	    BC_node_t src, before_loop_start, ldi_bc;
	    BC_node_t saved_start_next_iteration, saved_for_finish;
	    int saved_number_of_surrounding_blocks;
	    IR_node_t val_des = IR_foreach_value_designator (stmt);
	    IR_node_t tab = IR_foreach_tab (stmt);

	    saved_number_of_surrounding_blocks = number_of_surrounding_blocks;
	    number_of_surrounding_blocks = 0;
	    saved_start_next_iteration = start_next_iteration;
	    saved_for_finish = for_finish;
	    result = IR_foreach_tab_place (stmt);
	    IR_set_foreach_tab
	      (stmt, second_expr_processing (tab, FALSE,
					     &result, &temp_vars_num, FALSE,
					     NULL, NULL, FALSE));
	    if (tab != NULL)
	      source_position = IR_pos (tab);
	    type_test (tab, EVT_TAB, ERR_invalid_foreach_table_type);
	    src = new_bc_node (val_des == NULL ? BC_NM_source2 : BC_NM_source3,
			       IR_pos (stmt));
	    if (IR_foreach_index_designator (stmt) != NULL)
	      BC_set_pos2 (src, IR_pos (IR_foreach_index_designator (stmt)));
	    else
	      BC_set_pos2 (src, IR_pos (stmt));
	    if (val_des != NULL)
	      BC_set_pos3 (src, IR_pos (val_des));
	    ldi_bc = new_bc_code (BC_NM_ldi, src);
	    BC_set_op1 (ldi_bc, IR_foreach_search_start_place (stmt));
	    BC_set_op2 (ldi_bc, 0);
	    add_to_bcode (ldi_bc);
	    before_loop_start = BC_bc (curr_info);
	    var_result = not_defined_result;
	    before_pc = curr_pc;
	    temp = second_expr_processing (IR_foreach_index_designator (stmt), FALSE,
					   &var_result, &temp_vars_num, TRUE,
					   NULL, NULL, FALSE);
	    var_bc = before_pc != curr_pc ? curr_pc : NULL; /* local var */
	    bc = new_bc_code (val_des == NULL
			      ? BC_NM_foreach : BC_NM_foreach_val, src);
	    BC_set_op1 (bc, result);
	    BC_set_op4 (bc, BC_op1 (ldi_bc));
	    if (temp != NULL)
	      source_position = IR_pos (temp);
	    if (var_bc != NULL)
	      BC_SET_MODE (var_bc,
			   make_designator_lvalue
			   (var_bc, ERR_non_variable_in_foreach_index, FALSE));
	    if (temp != NULL)
	      {
		IR_set_foreach_index_designator (stmt, temp);
		/* In order not to generate different foreach_stmt
		   corresponding to local non-local variables, we use
		   a general solution by representing local variables
		   by nodes lvalue_var_occurrence_and_val.  */
		get_lvalue_location (var_bc, temp, &temp_vars_num,
				     &container_num, &index_num);
		BC_set_op2 (bc, container_num);
		BC_set_op3 (bc, index_num);
	      }
	    before_pc = curr_pc;
	    if (val_des != NULL)
	      {
		var_result = not_defined_result;
		temp = second_expr_processing (val_des, FALSE,
					       &var_result, &temp_vars_num, TRUE,
					       NULL, NULL, FALSE);
		var_bc = before_pc != curr_pc ? curr_pc : NULL; /* local var */
		if (temp != NULL)
		  source_position = IR_pos (temp);
		if (var_bc != NULL)
		  BC_SET_MODE
		    (var_bc,
		     make_designator_lvalue
		     (var_bc, ERR_non_variable_in_foreach_value, FALSE));
		if (temp != NULL)
		  {
		    IR_set_foreach_value_designator (stmt, temp);
		    /* In order not to generate different foreach_stmt
		       corresponding to local non-local variables, we
		       use a general solution by representing local
		       variables by nodes
		       lvalue_var_occurrence_and_val.  */
		    get_lvalue_location (var_bc, temp, &temp_vars_num,
					 &container_num, &index_num);
		    BC_set_vcontainer (bc, container_num);
		    BC_set_vindex (bc, index_num);
		  }
	      }
	    add_to_bcode (bc);
	    start_next_iteration = BC_next (before_loop_start);
	    for_finish = new_bc_code (BC_NM_nop, src);
	    second_block_passing (IR_foreach_stmts (stmt), FALSE);
	    before_pc = curr_pc;
	    add_to_bcode (for_finish);
	    BC_set_body_pc (bc,
			    BC_next (bc) != for_finish
			    ? BC_next (bc) : start_next_iteration);
	    BC_set_next (before_pc, start_next_iteration);
	    BC_set_next (bc, for_finish);
	    number_of_surrounding_blocks = saved_number_of_surrounding_blocks;
	    start_next_iteration = saved_start_next_iteration;
	    for_finish = saved_for_finish;
	    break;
	  }
	case IR_NM_break_stmt:
	case IR_NM_continue_stmt:
	  if (for_finish == NULL)
	    cont_err (source_position,
		      stmt_mode == IR_NM_continue_stmt
		      ? ERR_continue_is_not_in_loop : ERR_break_is_not_in_loop);
	  else
	    {
	      bc = new_bc_code_with_src (BC_NM_out, stmt);
	      add_to_bcode (bc);
	      BC_set_op1 (bc, number_of_surrounding_blocks);
	      if (stmt_mode == IR_NM_continue_stmt)
		BC_set_next (bc, start_next_iteration);
	      else
		BC_set_next (bc, for_finish);
	      curr_pc = NULL;
	    }
	  break;
	case IR_NM_return_without_result:
	case IR_NM_return_with_result:
	  {
	    IR_node_t fun_class;
	    BC_node_mode_t bc_mode;

	    fun_class = find_covered_fun_class (curr_scope);
	    if (fun_class == NULL)
	      cont_err (source_position,
			ERR_return_outside_fun_class);
	    else if (stmt_mode == IR_NM_return_without_result)
	      {
		bc = new_bc_code_with_src (BC_NM_leave, stmt);
		add_to_bcode (bc);
	      }
	    else
	      {
		if (IR_IS_OF_TYPE (fun_class, IR_NM_class))
		  cont_err (source_position,
			    ERR_return_with_result_in_class);
		else if (IR_IS_OF_TYPE (fun_class, IR_NM_fun)
			 && IR_thread_flag (fun_class))
		  cont_err (source_position,
			    ERR_return_with_result_in_thread);
		result = not_defined_result;
		IR_set_returned_expr
		  (stmt,
		   second_expr_processing (IR_returned_expr (stmt), TRUE,
					   &result, &temp_vars_num, FALSE,
					   NULL, NULL, FALSE));
		add_flatten_node_if_necessary (IR_returned_expr (stmt), result);
		bc = new_bc_code_with_src (BC_NM_ret, stmt);
		BC_set_op1 (bc, result);
		BC_set_ret_decl (bc, NULL);
		if (IR_returned_expr (stmt) != NULL
		    && IR_IS_OF_TYPE (IR_returned_expr (stmt), IR_NM_ident))
		  BC_set_ret_decl
		    (bc, get_bcode_decl (IR_decl (IR_returned_expr (stmt))));
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
			&& IR_block_scope (curr_real_scope) != NULL)
		      {
			
			d_assert (BC_IS_OF_TYPE (BC_cfblock (curr_pc),
						 BC_NM_fblock));
			BC_SET_MODE (curr_pc, bc_mode);
		      }
		  }
		add_to_bcode (bc);
	      }
	    break;
	  }
	case IR_NM_wait_stmt:
	  {
	    pc_t before_wait_guard_expr, wait_finish;
	    
	    before_wait_guard_expr = curr_pc;
	    there_is_function_call_in_expr = FALSE;
	    result = not_defined_result;
	    IR_set_wait_guard_expr
	      (stmt, second_expr_processing (IR_wait_guard_expr (stmt), FALSE,
					     &result, &temp_vars_num, FALSE,
					     NULL, NULL, FALSE));
	    bc = new_bc_code_with_src (BC_NM_wait, stmt);
	    BC_set_op1 (bc, result);
	    type_test (IR_wait_guard_expr (stmt), EVT_NUMBER_STRING_MASK,
		       ERR_invalid_wait_guard_expr_type);
	    if (there_is_function_call_in_expr)
	      cont_err (source_position, ERR_function_call_in_wait_stmt);
	    BC_set_pc (bc, BC_next (before_wait_guard_expr));
	    add_to_bcode (bc);
	    wait_finish = new_bc_code_with_src (BC_NM_waitend, stmt);
	    second_block_passing (IR_wait_stmt (stmt), FALSE);
	    add_to_bcode (wait_finish);
	    break;
	  }
	case IR_NM_throw:
	  bc = new_bc_code_with_src (BC_NM_throw, stmt);
	  BC_set_op1 (bc, temp_vars_num + curr_vars_number);
	  IR_set_throw_expr
	    (stmt, second_expr_processing (IR_throw_expr (stmt), FALSE,
					   NULL, &temp_vars_num, FALSE,
					   NULL, NULL, FALSE));
	  type_test (IR_throw_expr (stmt), EVT_UNKNOWN,
		     ERR_invalid_throw_expr_type);
	  add_to_bcode (bc);
	  break;
	case IR_NM_block:
	  {
	    IR_node_t saved_curr_scope = curr_scope;
	    IR_node_t saved_curr_real_scope = curr_real_scope;
	    int saved_curr_vars_number = curr_vars_number;
	    IR_node_t curr_except;
	    pc_t saved_curr_pc = curr_pc;
	    BC_node_t saved_curr_info = curr_info;
	    pc_t block_finish;
	    pc_t catches_finish;
	    pc_t previous_node_catch_list_pc;
	    IR_node_t fun_class, friend, use;
	    BC_node_t except_bc, last_except_with_block;
	    int simple_block_flag = simple_block_p (stmt);

	    curr_scope = stmt;
	    fun_class = IR_fun_class (stmt);
	    if (fun_class == NULL)
	      {
		if ((bc = IR_bc_block (stmt)) == NULL)
		  bc = new_bc_code (BC_NM_block,
				    new_bc_node (BC_NM_source, IR_pos (stmt)));
		BC_set_ext_life_p (bc, IR_extended_life_context_flag (stmt));
		BC_set_scope (bc, curr_bc_scope);
		IR_set_bc_block (stmt, bc);
	      }
	    else
	      {
		bc = get_fblock (fun_class);
		BC_set_scope (bc, curr_bc_scope);
		BC_set_simple_p (bc, FALSE);
		BC_set_fun_p (bc,
			      IR_IS_OF_TYPE (fun_class, IR_NM_fun)
			      && ! IR_thread_flag (fun_class));
		BC_set_class_p (bc, IR_IS_OF_TYPE (fun_class, IR_NM_class));
		BC_set_thread_p (bc,
				 IR_IS_OF_TYPE (fun_class, IR_NM_fun)
				 && IR_thread_flag (fun_class));
		BC_set_args_p (bc, IR_args_flag (fun_class));
		BC_set_pars_num (bc, IR_parameters_number (fun_class));
		BC_set_min_pars_num
		  (bc, IR_min_actual_parameters_number (fun_class));
		BC_set_ext_life_p (bc,
				   (IR_IS_OF_TYPE (fun_class, IR_NM_class)
				    || IR_thread_flag (fun_class)
				    || IR_extended_life_context_flag (stmt)));
		BC_set_fmode
		  (bc, IR_hint (stmt) == JIT_HINT ? BC_gen : BC_no_gen);
		BC_set_pure_fun_p (bc, IR_hint (stmt) == PURE_HINT);
	      }
	    add_to_bcode (bc);
	    curr_bc_scope = bc;
	    BC_set_friends (bc, NULL);
	    for (friend = IR_friends (stmt);
		 friend != NULL;
		 friend = IR_next_friend (friend))
	      {
		BC_node_t bc_friend = BC_create_node (BC_NM_friend);

		BC_set_friend (bc_friend, get_fblock (IR_friend_decl (friend)));
		BC_set_next_friend (bc_friend, BC_friends (bc));
		BC_set_friends (bc, bc_friend);
	      }
	    BC_set_uses (bc, NULL);
	    for (use = IR_block_uses (stmt);
		 use != NULL;
		 use = IR_next_block_use (use))
	      {
		BC_node_t bc_use = BC_create_node (BC_NM_use);

		BC_set_use (bc_use, get_fblock (IR_use_decl (use)));
		BC_set_next_use (bc_use, BC_uses (bc));
		BC_set_uses (bc, bc_use);
	      }
	    if (! simple_block_flag)
	      {
 		curr_real_scope = stmt;
		curr_vars_number = IR_vars_number (stmt);
		number_of_surrounding_blocks++;
	      }
	    second_block_passing (IR_block_stmts (stmt), TRUE);
	    if (fun_class == NULL && saved_curr_scope != NULL
		&& simple_block_flag)
	      block_finish = new_bc_code (BC_NM_sbend,
					  new_bc_node (BC_NM_source,
						       source_position));
	    else
	      {
		/* Function or the implicit top block.  */
		block_finish = new_bc_code (fun_class == NULL
					    ? BC_NM_bend : BC_NM_fbend,
					    new_bc_node (BC_NM_source,
							 source_position));
		BC_set_block (block_finish, curr_bc_scope);
	      }
	    if (BC_IS_OF_TYPE (block_finish, BC_NM_fbend)
		&& ! BC_IS_OF_TYPE (curr_pc, BC_NM_ret)
		&& ! BC_IS_OF_TYPE (curr_pc, BC_NM_leave))
	      {
		/* Type inference requires that fbend is reached only
		   through ret or leave.  */
		BC_node_t leave = new_bc_code_with_src (BC_NM_leave, stmt);

		add_to_bcode (leave);
	      }
	    add_to_bcode (block_finish);
	    curr_scope = saved_curr_scope;
	    curr_bc_scope = BC_scope (bc);
	    if (! simple_block_flag)
	      {
		number_of_surrounding_blocks--;
		curr_real_scope = saved_curr_real_scope;
		curr_vars_number = saved_curr_vars_number;
	      }
	    if (fun_class != NULL && IR_IS_OF_TYPE (fun_class, IR_NM_class)
		&& (BC_IS_OF_TYPE (BC_next (bc), BC_NM_leave)
		    || BC_IS_OF_TYPE (BC_next (bc), BC_NM_fbend)))
	      BC_set_simple_p (bc, TRUE);
	    BC_set_excepts (bc, NULL);
	    if (fun_class != NULL)
	      {
		curr_pc = saved_curr_pc;
		curr_info = saved_curr_info;
	      }
	    else if (IR_exceptions (stmt) != NULL)
	      {
		catches_finish = new_bc_code_with_src (BC_NM_nop, stmt);
		previous_node_catch_list_pc = bc; /* block */
		add_to_bcode (catches_finish);
		unlink_last_info ();
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
		    result = not_defined_result;
		    temp_vars_num = 0;
		    except_bc = new_bc_code_with_src (BC_NM_except,
						      curr_except);
		    /* The exception instance in the first local
		       temporary variable.  */
		    BC_set_op1 (except_bc,
				setup_result_var_number (NULL, &temp_vars_num));
		    IR_set_exception_class_expr
		      (curr_except,
		       second_expr_processing
		       (IR_exception_class_expr (curr_except),
			FALSE, &result, &temp_vars_num, FALSE,
			NULL, NULL, FALSE));
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
		    add_to_bcode (except_bc);
		    previous_node_catch_list_pc = curr_pc;
		    if (IR_catch_block (curr_except) != NULL)
		      {
			last_except_with_block = except_bc;
			second_block_passing (IR_catch_block (curr_except), FALSE);
			add_to_bcode (catches_finish);
			unlink_last_info ();
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
		BC_set_next (catches_finish, NULL);
		link_info (BC_info (catches_finish));
	      }
	    if (simple_block_flag)
	      BC_set_vars_num (bc, -1);
	    else
	      {
		IR_set_temporary_vars_number
		  (stmt,
		   IR_temporary_vars_number (stmt) + ADD_TEMP_VARS_NUMBER);
		BC_set_vars_num (bc, IR_vars_number (stmt));
	      }
	    BC_set_tvars_num (bc, IR_temporary_vars_number (stmt));
	    /* Uncycle decls: */
	    if (BC_decls (bc) != NULL)
	      {
		BC_node_t first = BC_next_decl (BC_decls (bc));

		BC_set_next_decl (BC_decls (bc), NULL);
		BC_set_decls (bc, first);
	      }
	    break;
	  }
	case IR_NM_fun:
	case IR_NM_class:
	case IR_NM_var:
	case IR_NM_external_var:
	case IR_NM_external_fun:
	  {
	    IR_node_t block, curr_decl = stmt;
	    IR_node_mode_t curr_decl_mode = stmt_mode;
	    BC_node_t bc_block, bc_decl;
	    
	    block = IR_scope (curr_decl);
	    d_assert (block != NULL && IR_NODE_MODE (block) == IR_NM_block);
	    IR_set_it_is_declared_in_block (IR_unique_ident (IR_ident (curr_decl)),
					    TRUE);
	    /* Add decl to the block */
	    bc_block = IR_bc_block (block);
	    bc_decl = get_bcode_decl (curr_decl);
	    add_decl_to_block (bc_decl, bc_block);
	    if (curr_decl_mode == IR_NM_class
		&& (block = surrounding_fun_block (block)) != NULL)
	      /* Objects created from the class inside a function will
		 need the function context.  */
	      IR_set_extended_life_context_flag (block, TRUE);
	    if ((decl = IR_origin_decl (curr_decl)) != NULL)
	      {
		set_decl_subst (IR_bc_decl (decl), bc_decl);
		d_assert (IR_use_clause (curr_decl) != NULL);
		if (IR_IS_OF_TYPE (decl, IR_NM_fun_or_class))
		  {
		    BC_set_subst (BC_info (get_fblock (decl)),
				  get_fblock (curr_decl));
		    copy_fun_bc_block (stmt, decl);
		  }
	      }
	    break;
	  }
	case IR_NM_decl_redir:
	  {
	    IR_node_t curr_decl = IR_to (stmt);
	    IR_node_t origin = IR_redir_origin (stmt);
	    BC_node_t bc_decl = get_bcode_decl (curr_decl);

	    set_decl_subst (IR_bc_decl (origin), bc_decl);
	    if (IR_IS_OF_TYPE (origin, IR_NM_fun_or_class))
	      BC_set_subst (BC_info (get_fblock (origin)),
			    get_fblock (curr_decl));
	  }
	  break;
	default:
	  d_unreachable ();
	}
    }
}

/* Currently processed fun block (and top-level block).  */
static BC_node_t curr_bc_block;

/* Return TRUE if the execution of BC does actually nothing.  */
static inline int
nop_p (BC_node_t bc)
{
  if (BC_NODE_MODE (bc) == BC_NM_nop)
    return TRUE;
  else if (BC_NODE_MODE (bc) == BC_NM_block && BC_vars_num (bc) < 0)
    return TRUE;
  else if (BC_NODE_MODE (bc) == BC_NM_sbend)
    return TRUE;
  else if (BC_NODE_MODE (bc) == BC_NM_out && BC_op1 (bc) == 0)
    return TRUE;
  return FALSE;
}

static BC_node_t
skip_nops (BC_node_t bc)
{
  for (; bc != NULL && nop_p (bc); bc = BC_next (bc))
    ;
  return bc;
}

static BC_node_t
branch_combine (BC_node_t bc)
{
  BC_node_mode_t node_mode;
  BC_node_t first_bc, second_bc, next_bc, info;
  int bt_p, op1 = not_defined_result;

  info = BC_info (bc);
  second_bc = first_bc = bc;
  if ((BC_NODE_MODE (first_bc) == BC_NM_addi
       || BC_NODE_MODE (first_bc) == BC_NM_iaddi)
      && BC_op1 (first_bc) == BC_op2 (first_bc))
    {
      op1 = BC_op1 (first_bc);
      second_bc = BC_next (first_bc);
      if (second_bc == NULL
	  || (next_bc = BC_next (second_bc)) == NULL
	  /* We generate bt for loops so there is no sense to consider
	     bf here for increment variants.  */
	  || (BC_NODE_MODE (next_bc) != BC_NM_bt
	      && BC_NODE_MODE (next_bc) != BC_NM_ibt)
	  /* Immediate is only increment for now: */
	  || BC_NODE_MODE (second_bc) == BC_NM_eqi
	  || BC_NODE_MODE (second_bc) == BC_NM_nei
	  || BC_NODE_MODE (second_bc) == BC_NM_gei
	  || BC_NODE_MODE (second_bc) == BC_NM_lti
	  || BC_NODE_MODE (second_bc) == BC_NM_lei
	  || BC_NODE_MODE (second_bc) == BC_NM_gti
	  || BC_NODE_MODE (second_bc) == BC_NM_ieqi
	  || BC_NODE_MODE (second_bc) == BC_NM_inei
	  || BC_NODE_MODE (second_bc) == BC_NM_igei
	  || BC_NODE_MODE (second_bc) == BC_NM_ilti
	  || BC_NODE_MODE (second_bc) == BC_NM_ilei
	  || BC_NODE_MODE (second_bc) == BC_NM_igti)
	{
	  op1 = not_defined_result;
	  second_bc = first_bc;
	}
    }
  node_mode = BC_NM__error;
  if ((next_bc = BC_next (second_bc)) != NULL
      && ((bt_p = (BC_NODE_MODE (next_bc) == BC_NM_bt
		   || BC_NODE_MODE (next_bc) == BC_NM_ibt))
	  || BC_NODE_MODE (next_bc) == BC_NM_bf
	  || BC_NODE_MODE (next_bc) == BC_NM_ibf))
    switch (BC_NODE_MODE (second_bc))
      {
      case BC_NM_eq:
	node_mode = op1 != not_defined_result ? BC_NM_bteqinc : bt_p ? BC_NM_bteq : BC_NM_btne;
	break;
      case BC_NM_ne:
	node_mode = op1 != not_defined_result ? BC_NM_btneinc : bt_p ? BC_NM_btne : BC_NM_bteq;
	break;
      case BC_NM_ge:
	node_mode = op1 != not_defined_result ? BC_NM_btgeinc : bt_p ? BC_NM_btge : BC_NM_btlt;
	break;
      case BC_NM_lt:
	node_mode = op1 != not_defined_result ? BC_NM_btltinc : bt_p ? BC_NM_btlt : BC_NM_btge;
	break;
      case BC_NM_le:
	node_mode = op1 != not_defined_result ? BC_NM_btleinc : bt_p ? BC_NM_btle : BC_NM_btgt;
	break;
      case BC_NM_gt:
	node_mode = op1 != not_defined_result ? BC_NM_btgtinc : bt_p ? BC_NM_btgt : BC_NM_btle;
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
      case BC_NM_ieq:
	node_mode = op1 != not_defined_result ? BC_NM_ibteqinc : bt_p ? BC_NM_ibteq : BC_NM_ibtne;
	break;
      case BC_NM_ine:
	node_mode = op1 != not_defined_result ? BC_NM_ibtneinc : bt_p ? BC_NM_ibtne : BC_NM_ibteq;
	break;
      case BC_NM_ige:
	node_mode = op1 != not_defined_result ? BC_NM_ibtgeinc : bt_p ? BC_NM_ibtge : BC_NM_ibtlt;
	break;
      case BC_NM_ilt:
	node_mode = op1 != not_defined_result ? BC_NM_ibtltinc : bt_p ? BC_NM_ibtlt : BC_NM_ibtge;
	break;
      case BC_NM_ile:
	node_mode = op1 != not_defined_result ? BC_NM_ibtleinc : bt_p ? BC_NM_ibtle : BC_NM_ibtgt;
	break;
      case BC_NM_igt:
	node_mode = op1 != not_defined_result ? BC_NM_ibtgtinc : bt_p ? BC_NM_ibtgt : BC_NM_ibtle;
	break;
      case BC_NM_ieqi:
	node_mode = bt_p ? BC_NM_ibteqi : BC_NM_ibtnei;
	break;
      case BC_NM_inei:
	node_mode = bt_p ? BC_NM_ibtnei : BC_NM_ibteqi;
	break;
      case BC_NM_igei:
	node_mode = bt_p ? BC_NM_ibtgei : BC_NM_ibtlti;
	break;
      case BC_NM_ilti:
	node_mode = bt_p ? BC_NM_ibtlti : BC_NM_ibtgei;
	break;
      case BC_NM_ilei:
	node_mode = bt_p ? BC_NM_ibtlei : BC_NM_ibtgti;
	break;
      case BC_NM_igti:
	node_mode = bt_p ? BC_NM_ibtgti : BC_NM_ibtlei;
	break;
      default:
	break;
      }
  if (node_mode != BC_NM__error
      && (op1 == not_defined_result || BC_op2 (second_bc) == op1)
      && BC_op1 (second_bc) >= BC_vars_num (curr_bc_block))
    {
      bc = new_bc_code (node_mode, BC_source (BC_info (next_bc)));
      insert_info (BC_info (bc), info);
      if (op1 != not_defined_result)
	BC_set_binc_inc (bc, BC_op3 (first_bc));
      BC_set_op1 (bc, BC_op2 (second_bc));
      BC_set_bcmp_op2 (bc, BC_op3 (second_bc));
      BC_set_bcmp_res (bc, BC_op1 (second_bc));
      BC_set_next (bc, BC_next (next_bc) == first_bc ? bc : BC_next (next_bc));
      BC_set_pc (bc, BC_pc (next_bc) == first_bc ? bc : BC_pc (next_bc));
    }
  return bc;
}

static BC_node_t
madd_combine (BC_node_t bc)
{
  BC_node_t info, next_bc;
  int op1, op2, op3, op4;

  op1 = not_defined_result;
  if (BC_NODE_MODE (bc) == BC_NM_mult
      && BC_op1 (bc) >= BC_vars_num (curr_bc_block)
      && (next_bc = BC_next (bc)) != NULL
      && BC_NODE_MODE (next_bc) == BC_NM_add)
    {
      op1 = BC_op1 (next_bc);
      op2 = BC_op2 (bc);
      op3 = BC_op3 (bc);
      if (BC_op2 (next_bc) == BC_op1 (bc))
	op4 = BC_op3 (next_bc);
      else if (BC_op3 (next_bc) == BC_op1 (bc))
	op4 = BC_op2 (next_bc);
      else
	op1 = not_defined_result;
    }
  else if (BC_NODE_MODE (bc) == BC_NM_add
	   && (next_bc = BC_next (bc)) != NULL && BC_NODE_MODE (next_bc) == BC_NM_mult
	   && BC_op1 (next_bc) >= BC_vars_num (curr_bc_block))
    {
      op1 = BC_op1 (next_bc);
      op2 = BC_op2 (bc);
      op3 = BC_op3 (bc);
      if (BC_op2 (next_bc) == BC_op1 (bc))
	op4 = BC_op3 (next_bc);
      else if (BC_op3 (next_bc) == BC_op1 (bc))
	op4 = BC_op2 (next_bc);
      else
	op1 = not_defined_result;
    }
  if (op1 != not_defined_result)
    {
      info = BC_info (bc);
      bc = new_bc_code (BC_NM_madd, BC_source (BC_info (next_bc)));
      insert_info (BC_info (bc), info);
      BC_set_op1 (bc, op1);
      BC_set_op2 (bc, op2);
      BC_set_op3 (bc, op3);
      BC_set_op4 (bc, op4);
      BC_set_next (bc, BC_next (next_bc));
    }
  return bc;
}

static BC_node_t
ind_combine (BC_node_t bc)
{
  BC_node_t info, next_bc;
  int op1, op2, op3, op4;

  if (BC_NODE_MODE (bc) == BC_NM_ind
      && BC_op1 (bc) >= BC_vars_num (curr_bc_block)
      && (next_bc = BC_next (bc)) != NULL
      && BC_NODE_MODE (next_bc) == BC_NM_ind
      && BC_op2 (next_bc) == BC_op1 (bc)
      /* We should have the same result.  If it is not it is used
	 somewhere and we can not combine the indexation.  */
      && BC_op1 (next_bc) == BC_op1 (bc))
    {
      op1 = BC_op1 (next_bc);
      op2 = BC_op2 (bc);
      op3 = BC_op3 (bc);
      op4 = BC_op3 (next_bc);
      info = BC_info (bc);
      bc = new_bc_code (BC_NM_ind2, BC_source (BC_info (next_bc)));
      insert_info (BC_info (bc), info);
      BC_set_op1 (bc, op1);
      BC_set_op2 (bc, op2);
      BC_set_op3 (bc, op3);
      BC_set_op4 (bc, op4);
      BC_set_next (bc, BC_next (next_bc));
    }
  return bc;
}

/* The following function passes through nodes unnecessary for
   execution.  Also do byte code combining.  */
static BC_node_t
go_through (BC_node_t start_bc)
{
  BC_node_t bc, subst, info;

  if (start_bc == NULL)
    return NULL;
  bc = skip_nops (start_bc);
  if (bc == NULL)
    return NULL;
  info = BC_info (bc);
  if (BC_subst (info) != NULL)
    return BC_subst (info);
  subst = branch_combine (bc);
  if (subst == bc)
    subst = madd_combine (bc);
  if (subst == bc)
    subst = ind_combine (bc);
  BC_set_subst (info, subst);
  return subst;
}

/* Modify call BC to tail calls if it is possible.  */
static void
process_tail_imcall (BC_node_t bc)
{
  BC_node_t next_pc;

  if (BC_IS_OF_TYPE (bc, BC_NM_imcall)
      && (next_pc = BC_next (bc)) != NULL
      && (BC_NODE_MODE (next_pc) == BC_NM_leave
	  || BC_NODE_MODE (next_pc) == BC_NM_fbend))
    {
      BC_node_mode_t bc_mode;
      
      if (BC_NODE_MODE (bc) == BC_NM_icall)
	bc_mode = BC_NM_itcall;
      else if (BC_NODE_MODE (bc) == BC_NM_ticall)
	bc_mode = BC_NM_titcall;
      else if (BC_NODE_MODE (bc) == BC_NM_cicall)
	bc_mode = BC_NM_citcall;
      else
	bc_mode = BC_NM__error;
      if (bc_mode != BC_NM__error)
	{
	  d_assert (BC_IS_OF_TYPE (BC_cfblock (bc), BC_NM_fblock));
	  BC_SET_MODE (bc, bc_mode);
	}
    }
}

/* Mark info of any reachable code from BC.  */
static void
mark_reachable_info (BC_node_t bc)
{
  BC_node_t info;

  for (; bc != NULL; bc = BC_next (bc))
    {
      info = BC_info (bc);
      if (BC_reachable_p (info))
	return;
      BC_set_reachable_p (info, TRUE);
      if (BC_IS_OF_TYPE (bc, BC_NM_br))
	mark_reachable_info (BC_pc (bc));
      if (BC_IS_OF_TYPE (bc, BC_NM_foreach))
	mark_reachable_info (BC_body_pc (bc));
      if (BC_IS_OF_TYPE (bc, BC_NM_block))
	mark_reachable_info (BC_excepts (bc));
      if (BC_IS_OF_TYPE (bc, BC_NM_except))
	mark_reachable_info (BC_next_except (bc));
    }
}

/* Modify op fields of copied nodes correspondingly.  */
static void
modify_copied_ops (int base)
{
  int op;
  BC_node_t bc, *bc_ptr;
  
  for (bc_ptr = (BC_node_t *) VLO_BEGIN (bc_copies);
       bc_ptr < (BC_node_t *) VLO_BOUND (bc_copies);
       bc_ptr++)
    {
      bc = *bc_ptr;
      if (BC_IS_OF_TYPE (bc, BC_NM_op1) && ! BC_IS_OF_TYPE (bc, BC_NM_op1i)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op2i12) && (op = BC_op1 (bc)) >= 0)
	BC_set_op1 (bc, op + base);
      if (BC_IS_OF_TYPE (bc, BC_NM_op2) && ! BC_IS_OF_TYPE (bc, BC_NM_op2i)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op2i12)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op3i2)
	  && ! BC_IS_OF_TYPE (bc, BC_NM_op4i2) && (op = BC_op2 (bc)) >= 0)
	BC_set_op2 (bc, op + base);
      if (BC_IS_OF_TYPE (bc, BC_NM_op3) && ! BC_IS_OF_TYPE (bc, BC_NM_op3i)
	  && (op = BC_op3 (bc)) >= 0)
	BC_set_op3 (bc, op + base);
      if (BC_IS_OF_TYPE (bc, BC_NM_op4) && (op = BC_op4 (bc)) >= 0)
	BC_set_op4 (bc, op + base);
      if (BC_IS_OF_TYPE (bc, BC_NM_brs) && (op = BC_res (bc)) >= 0)
	BC_set_res (bc, op + base);
      if (BC_IS_OF_TYPE (bc, BC_NM_bcmp))
	{
	  if (! BC_IS_OF_TYPE (bc, BC_NM_bcmpi) && (op = BC_bcmp_op2 (bc)) >= 0)
	    BC_set_bcmp_op2 (bc, op + base);
	  if ((op = BC_bcmp_res (bc)) >= 0)
	    BC_set_bcmp_res (bc, op + base);
	}
    }
}

/* Stack of fblocks being currently inlined.  */
static vlo_t inline_stack;

/* Return true if FBLOCK is in inline stack.  */
static int
in_inline_stack_p (BC_node_t fblock)
{
  BC_node_t *fblock_ptr;

  for (fblock_ptr = (BC_node_t *) VLO_BEGIN (inline_stack);
       fblock_ptr < (BC_node_t *) VLO_BOUND (inline_stack);
       fblock_ptr++)
    if (*fblock_ptr == fblock)
      return TRUE;
  return FALSE;
}

/* Current number of inlined calls.  */
unsigned int inlined_calls_num;

/* Container for copied returns. */
static vlo_t inline_returns;

/* Inline call with INFO.  Return the start of the inlined code or
   INFO if we failed to inline. */
static BC_node_t
inline_call (BC_node_t info)
{
  int level;
  unsigned int len;
  int tvars_num, var_base;
  BC_node_t bc, fblock, start, finish, finish2, res, last, *bc_ptr;
  BC_node_t call_bc = BC_bc (info);

  d_assert (BC_IS_OF_TYPE (call_bc, BC_NM_imcall));
  fblock = BC_cfblock (call_bc);
  /* Don't inline fun with extended life as we need it's stack for
     some purposes.  To prevent cycling and code blow up don't inline
     recursively or function call inside the function. */
  if (BC_ext_life_p (fblock)
      || in_inline_stack_p (fblock) || fblock == curr_bc_block)
    /* To prevent infinite inlininig of recursive functions. */
    return info;
  inlined_calls_num++;
  VLO_ADD_MEMORY (inline_stack, &fblock, sizeof (fblock));
  var_base = BC_op1 (call_bc);
  start = new_bc_code (BC_NM_stinc,
		       new_bc_node (BC_NM_source, BC_pos (BC_source (info))));
  BC_set_op1 (start, 0); /* var_base will be added */
  BC_set_op2 (start, BC_pars_num (fblock)); /* var_base will be added */
  BC_set_op3 (start, BC_vars_num (fblock));
  tvars_num = (var_base - BC_vars_num (curr_bc_block)
	       + BC_vars_num (fblock) + BC_tvars_num (fblock));
  if (tvars_num > BC_tvars_num (curr_bc_block))
    BC_set_tvars_num (curr_bc_block, tvars_num);
  curr_info = NULL;
  res = BC_info (start);
  link_info (res);
  finish = new_bc_code (BC_NM_stdecu,
			new_bc_node (BC_NM_source, no_position));
  BC_set_op1 (finish, 0); /* will be fixed to var_base */
  BC_set_next (finish, BC_next (fblock));
  VLO_NULLIFY (bc_copies);
  VLO_ADD_MEMORY (bc_copies, &start, sizeof (start));
  VLO_ADD_MEMORY (bc_copies, &finish, sizeof (finish));
  /* Reset substitution fields: */
  for (info = BC_next_info (BC_info (fblock));
       info != NULL;
       info = BC_next_info (info))
    BC_set_subst (info, NULL);
  /* Make bcode copy:  */
  level = 0;
  VLO_NULLIFY (inline_returns);
  for (info = BC_next_info (BC_info (fblock));; info = BC_next_info (info))
    {
      bc = BC_bc (info);
      if (BC_IS_OF_TYPE (bc, BC_NM_fbend)
	  || BC_IS_OF_TYPE (bc, BC_NM_leave))
	{
	  d_assert (level == 0);
	  BC_set_subst (BC_info (bc), finish);
	  if (BC_IS_OF_TYPE (bc, BC_NM_fbend))
	    {
	      BC_set_pos (BC_source (BC_info (finish)),
			  BC_pos (BC_source (info)));
	      break;
	    }
	}
      else if (BC_IS_OF_TYPE (bc, BC_NM_ret))
	{
	  d_assert (level == 0);
	  finish2 = new_bc_code (BC_NM_stdec,
				 new_bc_node (BC_NM_source,
					      BC_pos (BC_source (info))));
	  VLO_ADD_MEMORY (inline_returns, &finish2, sizeof (finish2));
	  BC_set_op1 (finish2, 0); /* will be fixed to var_base */
	  BC_set_op2 (finish2, BC_op1 (bc));
	  BC_set_subst (BC_info (bc), finish2);
	  link_info (BC_info (finish2));
	  VLO_ADD_MEMORY (bc_copies, &finish2, sizeof (finish));
	}
      else
	{
	  BC_node_mode_t bc_mode;
	  
	  copy_and_link_bcode (bc);
	  last = ((BC_node_t *) VLO_BOUND (bc_copies)) [-1];
	  /* We should remove tail calls as we still need to execute a
	     code after the call.  */
	  if (BC_NODE_MODE (last) == BC_NM_tcall)
	    bc_mode = BC_NM_call;
	  else if (BC_NODE_MODE (last) == BC_NM_itcall)
	    bc_mode = BC_NM_icall;
	  else if (BC_NODE_MODE (last) == BC_NM_titcall)
	    bc_mode = BC_NM_ticall;
	  else if (BC_NODE_MODE (last) == BC_NM_citcall)
	    bc_mode = BC_NM_cicall;
	  else
	    bc_mode = BC_NM__error;
	  if (bc_mode != BC_NM__error)
	    BC_SET_MODE (last, bc_mode);
	}
      if (BC_IS_OF_TYPE (bc, BC_NM_block))
	level++;
      else if (BC_IS_OF_TYPE (bc, BC_NM_bend))
	{
	  d_assert (level != 0);
	  level--;
	}
    }
  d_assert (BC_IS_OF_TYPE (bc, BC_NM_fbend));
  last = BC_info (finish);
  link_info (last);
  BC_set_next (start, BC_bc (BC_next_info (res)));
  BC_set_subst (BC_info (fblock), curr_bc_block);
  len = VLO_LENGTH (bc_copies);
  copy_bc_decls (curr_bc_block, fblock, var_base);
  modify_copied_pc (in_inline_stack_p);
  /* Do not change in ops in copied fblocks. */
  len = VLO_LENGTH (bc_copies) - len;
  VLO_SHORTEN (bc_copies, len);
  modify_copied_ops (var_base);
  BC_set_subst (BC_info (call_bc), start);
  BC_set_next (finish, BC_next (call_bc));
  for (bc_ptr = (BC_node_t*) VLO_BEGIN (inline_returns);
       bc_ptr < (BC_node_t*) VLO_BOUND (inline_returns);
       bc_ptr++)
    BC_set_next (*bc_ptr, BC_next (call_bc));
  info = BC_prev_info (BC_info (call_bc));
  BC_set_next_info (info, res);
  BC_set_prev_info (res, info);
  info = BC_next_info (BC_info (call_bc));
  BC_set_next_info (last, info);
  BC_set_prev_info (info, last);
  return res;
}

/* Process nodes from START generating tail calls, removing
   uneccessary nodes, and combining byte code insns.  */
static void
process_bc (BC_node_t start)
{ 
  BC_node_t info, bc, next, next_info;
  
  curr_bc_block = start;
  VLO_NULLIFY (inline_stack);
  for (info = BC_info (start); info != NULL; info = BC_next_info (info))
    {
      BC_set_subst (info, NULL);
      BC_set_reachable_p (info, FALSE);
      if (BC_inline_p (info))
	{
	  d_assert (BC_IS_OF_TYPE (BC_bc (info), BC_NM_imcall));
	  info = inline_call (info);
	}
      bc = BC_bc (info);
      if (BC_IS_OF_TYPE (bc, BC_NM_stdecu))
	VLO_SHORTEN (inline_stack, sizeof (BC_node_t));
    }
  d_assert (VLO_LENGTH (inline_stack) == 0);
  for (info = BC_info (start); info != NULL; info = BC_next_info (info))
    {
      bc = BC_bc (info);
      if ((next = BC_next (bc)) != NULL)
	BC_set_next (bc, go_through (next));
      if (BC_IS_OF_TYPE (bc, BC_NM_br) && (next = BC_pc (bc)) != NULL)
	BC_set_pc (bc, go_through (next));
      if (BC_IS_OF_TYPE (bc, BC_NM_foreach)
	  && (next = BC_body_pc (bc)) != NULL)
	BC_set_body_pc (bc, go_through (next));
      if (BC_IS_OF_TYPE (bc, BC_NM_block)
	  && (next = BC_excepts (bc)) != NULL)
	BC_set_excepts (bc, go_through (next));
      if (BC_IS_OF_TYPE (bc, BC_NM_except)
	  && (next = BC_next_except (bc)) != NULL)
	BC_set_next_except (bc, go_through (next));
      process_tail_imcall (bc);
    }
  mark_reachable_info (start);
  /* Remove unreachable infos.  Setup unqiue ident numbers.  */
  for (info = BC_info (start); info != NULL; info = next_info)
    {
      next_info = BC_next_info (info);
      if (! BC_reachable_p (info)
	  /* Don't remove bend we need this for type inference
	     optimization.  */
	  && ! BC_IS_OF_TYPE (BC_bc (info), BC_NM_bend))
	unlink_info (info);
    }
}

/* Process semantics of IR starting with FIRST_PROGRAM_STMT_PTR and
   generate byte code.  */
void
test_context (IR_node_t first_program_stmt_ptr, int first_p)
{
  BC_node_t *bc_ptr;

  if (setjmp (context_exit_longjump_buff) != 0)
    return;
  curr_scope = NULL;
  first_program_stmt_ptr = first_block_passing (first_program_stmt_ptr, 0);
  /* first_block_passing include declarations into table.  The
     environment scope is the first stmt (which is always block). */
  curr_scope = curr_real_scope = NULL;
  curr_bc_scope = NULL;
  curr_info = NULL;
  prev_pc = curr_pc = NULL;
  for_finish = NULL;
  VLO_NULLIFY (all_fblocks);
  if (! first_p)
    {
      BC_node_t bc_block = IR_bc_block (first_program_stmt_ptr);
      BC_node_t info = BC_info (bc_block);

      d_assert (repl_flag && bc_block != NULL);
      BC_set_decls (bc_block, NULL);
      /* Info node might be removed.  So restore it.  */
      if (! BC_IS_OF_TYPE (info, BC_NM_info))
	attach_info (bc_block);
      else
	{
	  /* It might happen in REPL when previous bunch of stmt had
	     context errors.  */
	  BC_set_next_info (info, NULL);
	  BC_set_prev_info (info, NULL);
	}
    }
  second_block_passing (first_program_stmt_ptr, FALSE);
  /* Never shrink the top block and always put it into stack area.  */
  IR_set_extended_life_context_flag (first_program_stmt_ptr, FALSE);
  if (number_of_errors == 0)
    {
      first_program_bc = IR_bc_block (first_program_stmt_ptr);
      /* Some optimizations: */
      process_bc (first_program_bc);
      for (bc_ptr = (BC_node_t *) VLO_BEGIN (all_fblocks);
	   bc_ptr < (BC_node_t *) VLO_BOUND (all_fblocks);
	   bc_ptr++)
	process_bc (*bc_ptr);
      if (dump_flag)
	{
	  int idn = 0, decl_num = 0;
	  
	  enumerate_infoed_bcode (first_program_bc, &idn, &decl_num);
	}
      if (optimize_flag)
	inference_pass (first_program_bc, &all_fblocks);
      if (dump_flag)
	dump_code (BC_info (first_program_bc), 0);
    }
}

void
initiate_context (void)
{
  bc_nodes_num = 0;
  max_block_level = 0;
  VLO_CREATE (all_fblocks, 0);
  curr_use_items_start = 0;
  VLO_CREATE (use_items, 0);
  VLO_CREATE (copied_redirs, 0);
  VLO_CREATE (bc_copies, 0);
  VLO_CREATE (inline_returns, 0);
  VLO_CREATE (inline_stack, 0);
  initiate_decl_subst ();
  last_uniq_field_ident_num = DESTROY_FLDID_NUM;
  last_decl_num = 0;
  initiate_foreach_stmts ();
  if (optimize_flag)
    initiate_inference_pass ();
}

void
finish_context (void)
{
  if (optimize_flag)
    finish_inference_pass ();
  finish_foreach_stmts ();
  finish_decl_subst ();
  VLO_DELETE (inline_stack);
  VLO_DELETE (inline_returns);
  VLO_DELETE (bc_copies);
  VLO_DELETE (use_items);
  VLO_DELETE (copied_redirs);
  VLO_DELETE (all_fblocks);
}
