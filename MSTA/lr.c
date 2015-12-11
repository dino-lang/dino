/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool MSTA.

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

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include "vlobject.h"
#include "hashtab.h"
#include "ticker.h"
#include "common.h"
#include "ird.h"
#include "gen-comm.h"
#include "contexts.h"
#include "relation.h"
#include "lr-sets.h"
#include "lr.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif



static void
insert_LR_situation_context (IR_node_t LR_situation)
{
  if (IR_context (LR_situation) == NULL)
    return;
  IR_set_context (LR_situation,
                  insert_or_free_context (IR_context (LR_situation)));
}

static void
insert_LR_set_contexts (IR_node_t LR_set)
{
  IR_node_t current_LR_situation;

  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    insert_LR_situation_context (current_LR_situation);
}

static void
free_unimportant_LR_situations (IR_node_t LR_set)
{
  IR_node_t current_LR_situation;
  IR_node_t last_important_LR_situation;

  current_LR_situation = IR_LR_situation_list (LR_set);
  do
    {
      if (IR_next_symbol_LR_situation (current_LR_situation) != NULL
          && !IR_important_LR_situation_flag (IR_next_symbol_LR_situation
                                              (current_LR_situation)))
        IR_set_next_symbol_LR_situation (current_LR_situation, NULL);
      last_important_LR_situation = current_LR_situation;
      current_LR_situation = IR_next_LR_situation (current_LR_situation);
    }
  while (current_LR_situation != NULL
         && IR_important_LR_situation_flag (current_LR_situation));
  free_LR_situations_list (IR_next_LR_situation (last_important_LR_situation));
  IR_set_next_LR_situation (last_important_LR_situation, NULL);
}

static void
add_LR_core_to_cyclic_list (IR_node_t LR_core)
{
  IR_set_next_LR_core (IR_LR_core_list (description), LR_core);
  IR_set_LR_core_list (description, LR_core);
}

static void
add_LR_set_to_cyclic_list (IR_node_t LR_set, IR_node_t LR_core)
{
  if (IR_LR_set_list (LR_core) == NULL)
    {
      /* New LR-core. */
      IR_set_next_LR_set (LR_set, LR_set);
      IR_set_LR_set_list (LR_core, LR_set);
    }
  else
    {
      IR_set_next_LR_set (LR_set, IR_next_LR_set (IR_LR_set_list (LR_core)));
      IR_set_next_LR_set (IR_LR_set_list (LR_core), LR_set);
      IR_set_LR_set_list (LR_core, LR_set);
    }
}

static int
compare_LR_situations_order (const void *LR_situation_1_ptr,
                             const void *LR_situation_2_ptr)
{
  IR_node_t canonical_rule_1;
  IR_node_t canonical_rule_2;
  IR_node_t element_after_dot_1;
  IR_node_t element_after_dot_2;
  int prefix_length_1;
  int prefix_length_2;

  element_after_dot_1
    = IR_element_after_dot (* (IR_node_t *) LR_situation_1_ptr);
  element_after_dot_2
    = IR_element_after_dot (*(IR_node_t *) LR_situation_2_ptr);
  assert (element_after_dot_1 != element_after_dot_2);
  canonical_rule_1 = IR_canonical_rule (element_after_dot_1);
  canonical_rule_2 = IR_canonical_rule (element_after_dot_2);
  prefix_length_1
    = canonical_rule_right_hand_side_prefix_length (canonical_rule_1,
                                                    element_after_dot_1);
  prefix_length_2
    = canonical_rule_right_hand_side_prefix_length (canonical_rule_2,
                                                    element_after_dot_2);
  if (prefix_length_1 < prefix_length_2)
    return 1;
  else if (prefix_length_1 > prefix_length_2)
    return (-1);
  else if (IR_canonical_rule_order_number (canonical_rule_1)
           < IR_canonical_rule_order_number (canonical_rule_2))
    return -1;
  else
    {
      assert (IR_canonical_rule_order_number (canonical_rule_1)
              != IR_canonical_rule_order_number (canonical_rule_2));
      return 1;
    }
}

static IR_node_t
get_shifted_LR_situation_list (IR_node_t first_symbol_LR_situation)
{
  IR_node_t current_new_LR_situation;
  IR_node_t first_new_LR_situation;
  IR_node_t last_new_LR_situation;
  IR_node_t current_symbol_LR_situation;
  vlo_t situations;
  IR_node_t *LR_situation_ptr;

  assert (IR_first_symbol_LR_situation (first_symbol_LR_situation)
          && !IR_IS_OF_TYPE (IR_element_after_dot (first_symbol_LR_situation),
                             IR_NM_canonical_rule_end));
  VLO_CREATE (situations, 100);
  for (current_symbol_LR_situation = first_symbol_LR_situation;
       current_symbol_LR_situation != NULL;
       current_symbol_LR_situation
       = IR_next_symbol_LR_situation (current_symbol_LR_situation))
    {
      current_new_LR_situation
        = get_new_LR_situation
          (IR_next_right_hand_side_element
           (IR_element_after_dot (current_symbol_LR_situation)),
           IR_context (current_symbol_LR_situation), NULL, NULL, TRUE);
#if 0
      if (IR_cp_flag (IR_element_after_dot (current_symbol_LR_situation)))
        IR_set_under_control_point_flag (current_symbol_LR_situation, TRUE);
#endif
      VLO_ADD_MEMORY (situations, &current_new_LR_situation,
                      sizeof (IR_node_t));
    }
  qsort (VLO_BEGIN (situations), VLO_LENGTH (situations) / sizeof (IR_node_t),
         sizeof (IR_node_t), compare_LR_situations_order);
  last_new_LR_situation = first_new_LR_situation = NULL;
  for (LR_situation_ptr = VLO_BEGIN (situations);
       LR_situation_ptr <= (IR_node_t *) VLO_END (situations);
       LR_situation_ptr++)
    {
      if (last_new_LR_situation != NULL)
        IR_set_next_LR_situation
          (last_new_LR_situation, *LR_situation_ptr);
      last_new_LR_situation = *LR_situation_ptr;
      if (first_new_LR_situation == NULL)
        first_new_LR_situation = last_new_LR_situation;
    }
  VLO_DELETE (situations);
  return first_new_LR_situation;
}


/* Given LR-set must be full and contexts of given LR-set must be in
   the table. */

static void
set_up_goto_LR_set_fields (IR_node_t LR_set, int any_LR_core_set_flag)
{
  IR_node_t current_LR_situation;
  IR_node_t first_new_LR_situation;
  IR_node_t goto_LR_set;
  IR_node_t LR_core;

  /* Set up fields `goto_LR_set'. */
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    if (IR_first_symbol_LR_situation (current_LR_situation)
        && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                           IR_NM_canonical_rule_end))
      {
        first_new_LR_situation
          = get_shifted_LR_situation_list (current_LR_situation);
        LR_core = find_LR_core (first_new_LR_situation);
        assert (LR_core != NULL);
        /* This is new LR-core. */
        if (any_LR_core_set_flag)
          /* Any LR-set of the LR-core. */
          goto_LR_set = IR_LR_set_list (LR_core);
        else
          goto_LR_set = find_LR_set (LR_core, first_new_LR_situation);
        /* The LR-set can be found by `find_LR_set' after merging
           LR-sets of a LR-core on LALR-optimization. */
        if (goto_LR_set == NULL
            && IR_next_LR_set (IR_LR_set_list (LR_core)) == NULL)
          goto_LR_set = IR_LR_set_list (LR_core);
        assert (goto_LR_set != NULL);
        free_LR_situations_list (first_new_LR_situation);
        /* Set up field `goto_LR_set'. */
        IR_set_goto_LR_set (current_LR_situation, goto_LR_set);
      }
}

static void
make_full_LR_set (IR_node_t LR_set, int with_context_flag)
{
  IR_node_t current_LR_situation;
  IR_node_t last_LR_situation;
  vlo_t LR_situations_stack;
  IR_node_t single_definition;
  IR_node_t current_canonical_rule;
  context_t context;
  IR_node_t last_reduce_LR_situation_processed;
  int process_flag;
  int under_control_point_flag;
  context_t new_context;

  VLO_CREATE (LR_situations_stack, 1000);
  assert (IR_LR_situation_list (LR_set) != NULL);
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    {
      assert (IR_important_LR_situation_flag (current_LR_situation));
      VLO_ADD_MEMORY (LR_situations_stack, &current_LR_situation,
                      sizeof (IR_node_t));
      last_LR_situation = current_LR_situation;
    }
  while (VLO_LENGTH (LR_situations_stack) != 0)
    {
      current_LR_situation
        = ((IR_node_t *) VLO_BEGIN (LR_situations_stack))
          [VLO_LENGTH (LR_situations_stack) / sizeof (IR_node_t) - 1];
      VLO_SHORTEN (LR_situations_stack, sizeof (IR_node_t));
      if (IR_cp_flag (IR_element_after_dot (current_LR_situation)))
        IR_set_under_control_point_flag (current_LR_situation, TRUE);
      under_control_point_flag
        = IR_under_control_point_flag (current_LR_situation);
      if (!IR_important_LR_situation_flag (current_LR_situation))
        IR_set_situation_in_stack_flag (current_LR_situation, FALSE);
      /* Process the situation */
      if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                         IR_NM_canonical_rule_end))
        continue;
      single_definition = IR_element_itself (IR_element_after_dot
                                             (current_LR_situation));
      if (!IR_IS_OF_TYPE (single_definition, IR_NM_single_nonterm_definition))
        continue;
      if (with_context_flag)
        context = FIRST_of_tail (IR_next_right_hand_side_element
                                 (IR_element_after_dot (current_LR_situation)),
                                 IR_context (current_LR_situation),
                                 max_look_ahead_number);
      for (current_canonical_rule
           = IR_nonterm_canonical_rule_list (single_definition);
           current_canonical_rule != NULL;
           current_canonical_rule
           = IR_next_nonterm_canonical_rule (current_canonical_rule))
        {
          process_flag = FALSE;
          if (IR_canonical_rule_LR_situation (current_canonical_rule) != NULL)
            {
              if (with_context_flag)
                {
                  current_LR_situation
                    = IR_canonical_rule_LR_situation (current_canonical_rule);
                  process_flag
                    = !context_in (context, IR_context (current_LR_situation));
                  if (process_flag)
                    context_or (IR_context (current_LR_situation), context);
                }
              if (!under_control_point_flag)
                {
                  process_flag
                    = (process_flag
                       || IR_under_control_point_flag (current_LR_situation));
                  IR_set_under_control_point_flag (current_LR_situation,
                                                   FALSE);
                }
            }
          else
            {
              if (with_context_flag)
                {
                  new_context = get_null_context ();
                  context_copy (new_context, context);
                }
              current_LR_situation
                = get_new_LR_situation
                  (IR_right_hand_side (current_canonical_rule),
                   (with_context_flag ? new_context : NULL),
                   LR_set, NULL, FALSE);
              IR_set_under_control_point_flag (current_LR_situation,
                                               under_control_point_flag);
              IR_set_next_LR_situation (last_LR_situation,
                                        current_LR_situation);
              last_LR_situation = current_LR_situation;
              process_flag = TRUE;
              IR_set_canonical_rule_LR_situation (current_canonical_rule,
                                                  current_LR_situation);
            }
          if (process_flag
              && !IR_situation_in_stack_flag (current_LR_situation))
            {
              IR_set_situation_in_stack_flag (current_LR_situation, TRUE);
              VLO_ADD_MEMORY (LR_situations_stack, &current_LR_situation,
                              sizeof (IR_node_t));
            }
        }
      if (with_context_flag)
        free_context (context);
    }
  VLO_DELETE (LR_situations_stack);
  /* Set up values of fields `first_symbol_LR_situation'. */
  last_reduce_LR_situation_processed = NULL;
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    {
      IR_set_canonical_rule_LR_situation
        (IR_canonical_rule (IR_element_after_dot (current_LR_situation)),
         NULL);
      if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                         IR_NM_canonical_rule_end))
        {
          if (last_reduce_LR_situation_processed == NULL)
            IR_set_first_symbol_LR_situation (current_LR_situation, TRUE);
          else
            {
              IR_set_first_symbol_LR_situation (current_LR_situation, FALSE);
              IR_set_next_symbol_LR_situation
                (last_reduce_LR_situation_processed, current_LR_situation);
            }
          last_reduce_LR_situation_processed = current_LR_situation;
        }
      else
        {
          single_definition = IR_element_itself (IR_element_after_dot
                                                 (current_LR_situation));
          if (IR_last_symbol_LR_situation_processed (single_definition)
              == NULL)
            IR_set_first_symbol_LR_situation (current_LR_situation, TRUE);
          else
            {
              IR_set_first_symbol_LR_situation (current_LR_situation, FALSE);
              IR_set_next_symbol_LR_situation
                (IR_last_symbol_LR_situation_processed (single_definition),
                 current_LR_situation);
            }
          IR_set_last_symbol_LR_situation_processed
            (single_definition, current_LR_situation);
        }
    }
  /* Restore fields `last_symbol_LR_situation_processed'. */
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    if (!IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                        IR_NM_canonical_rule_end))
      IR_set_last_symbol_LR_situation_processed
        (IR_element_itself (IR_element_after_dot (current_LR_situation)),
         FALSE);
#ifndef NDEBUG
  for (single_definition = IR_single_definition_list (description);
       single_definition != NULL;
       single_definition = IR_next_single_definition (single_definition))
    if (IR_last_symbol_LR_situation_processed (single_definition) != NULL)
      abort ();
  for (current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    if (IR_canonical_rule_LR_situation (current_canonical_rule) != NULL)
      abort ();
#endif
  if (with_context_flag)
    /* Insert new LR-situation contexts into the context table. */
    insert_LR_set_contexts (LR_set);
}

static IR_node_t
create_start_LR_set (int with_context_flag)
{
  IR_node_t start_LR_situation;
  context_t start_context;
  IR_node_t start_LR_set;
  IR_node_t start_LR_core;

  /* Create start LR-set `$accept : . axiom $end'. */
  if (with_context_flag)
    start_context = null_context_in_table;
  else
    start_context = NULL;
  start_LR_situation
    = get_new_LR_situation
      (IR_right_hand_side (IR_canonical_rule_list (description)),
       start_context, NULL, NULL, TRUE);
  if (IR_cp_flag (IR_element_after_dot (start_LR_situation)))
    IR_set_under_control_point_flag (start_LR_situation, TRUE);
  start_LR_set = IR_new_LR_set (start_LR_situation, NULL, NULL);
  IR_set_LR_set (start_LR_situation, start_LR_set);
  /* Make cycle LR-set list. */
  IR_set_next_LR_set (start_LR_set, start_LR_set);
  start_LR_core = IR_new_LR_core (start_LR_set, NULL);
  (void) insert_LR_core (start_LR_core);
  IR_set_LR_core_list (description, start_LR_core);
  /* Make cycle LR-core list. */
  IR_set_next_LR_core (start_LR_core, start_LR_core);
  IR_set_LR_core (start_LR_set, start_LR_core);
  return start_LR_set;
}

static void
make_uncyclic_LR_set_and_core_lists (void)
{
  IR_node_t LR_set;
  IR_node_t LR_core;

  /* Make uncyclic core list. */
  LR_core = IR_LR_core_list (description);
  IR_set_LR_core_list (description, IR_next_LR_core (LR_core));
  IR_set_next_LR_core (LR_core, NULL);
  /* Make uncyclic LR set lists. */
  for (LR_core = IR_LR_core_list (description); LR_core != NULL;
       LR_core = IR_next_LR_core (LR_core))
    {
      LR_set = IR_LR_set_list (LR_core);
      IR_set_LR_set_list (LR_core, IR_next_LR_set (LR_set));
      IR_set_next_LR_set (LR_set, NULL);
    }
}

static void
count_LR_set_situations (IR_node_t LR_set, int *LR_sets_number,
                         int *important_LR_situations_number,
                         int *LR_situations_number)
{
  IR_node_t current_LR_situation;

  (*LR_sets_number)++;
  current_LR_situation = IR_LR_situation_list (LR_set);
  do
    {
      (*LR_situations_number)++;
      (*important_LR_situations_number)++;
      current_LR_situation = IR_next_LR_situation (current_LR_situation);
    }
  while (current_LR_situation != NULL
         && IR_important_LR_situation_flag (current_LR_situation));
  while (current_LR_situation != NULL)
    {
      (*LR_situations_number)++;
      current_LR_situation = IR_next_LR_situation (current_LR_situation);
    }
}



static void
add_conflict (IR_node_t LR_set, IR_node_t used_LR_situation,
              IR_node_t unused_LR_situation, token_string_t token_string)
{
  IR_node_t conflict;
  IR_node_t last_conflict;
  
  conflict = IR_new_conflict (used_LR_situation, unused_LR_situation, 
                              token_string, NULL);
  last_conflict = IR_conflicts_list (LR_set);
  if (last_conflict == NULL)
    IR_set_conflicts_list (LR_set, conflict);
  else
    {
      while (IR_next_conflict (last_conflict) != NULL)
        last_conflict = IR_next_conflict (last_conflict);
      IR_set_next_conflict (last_conflict, conflict);
    }
}

static int
contains_error (token_string_t token_string)
{
  IR_node_t single_term_definition;
  int n;

  for (n = 0;; n++)
    {
      single_term_definition = get_n_th_token (token_string, n);
      if (single_term_definition == NULL)
	break;
      if (single_term_definition == error_single_definition)
	return TRUE;
    }
  return FALSE;
}

/* ??? */
static int fold_is_used_in_conflict_resolution;
static IR_node_t conflict_resolution_fold_LR_situation;
static IR_node_t another_conflict_resolution_LR_situation;

static void
add_context_conflicts (token_string_t token_string)
{
  if ((!IR_IS_OF_TYPE (IR_element_after_dot
		       (another_conflict_resolution_LR_situation),
		       IR_NM_canonical_rule_end)
       && !(IR_first_symbol_LR_situation
	    (another_conflict_resolution_LR_situation)))
      || (!error_conflict_flag && contains_error (token_string)
	  && (characteristic_symbol_of_LR_set
	      (IR_LR_set (another_conflict_resolution_LR_situation))
	      == error_single_definition)))
    return;
  if (fold_is_used_in_conflict_resolution)
    add_conflict (IR_LR_set (another_conflict_resolution_LR_situation),
                  conflict_resolution_fold_LR_situation,
                  another_conflict_resolution_LR_situation, token_string);
  else
    add_conflict (IR_LR_set (another_conflict_resolution_LR_situation),
                  another_conflict_resolution_LR_situation,
                  conflict_resolution_fold_LR_situation, token_string);
}

static void
add_terminal_and_context_concatenation (context_t *target_context,
                                        IR_node_t terminal,
                                        context_t added_context)
{
  context_t context;

  context = get_null_context ();
  set_context_element_value (context, get_new_token_string (&terminal, 1), 1);
  context_concat (context, added_context, max_look_ahead_number);
  context_or (*target_context, context);
  free_context (context);
}

static hash_table_t dependence_nodes_table;
static IR_node_t work_LR_set_dependence;
static IR_node_t work_shift_LR_situation_dependence;

static unsigned
dependence_nodes_table_hash_function (hash_table_entry_t dependence_node)
{
  if (IR_IS_OF_TYPE ((IR_node_t) dependence_node, IR_NM_LR_set_dependence))
    return
      (unsigned long) IR_LR_set ((IR_node_t) dependence_node)
      + IR_back_distance ((IR_node_t) dependence_node)
      + IR_look_ahead ((IR_node_t) dependence_node)
      + (unsigned long) IR_nonterm ((IR_node_t) dependence_node);
#ifndef NDEBUG
  else if (!IR_IS_OF_TYPE ((IR_node_t) dependence_node,
                           IR_NM_shift_LR_situation_dependence))
    assert (FALSE);
#endif
  else
    return
      (unsigned long) IR_shift_LR_situation ((IR_node_t) dependence_node)
      + IR_look_ahead ((IR_node_t) dependence_node);
}

static int
dependence_nodes_table_eq_function (hash_table_entry_t dependence_node_1,
                                    hash_table_entry_t dependence_node_2)
{
  if (IR_NODE_MODE ((IR_node_t) dependence_node_1)
      != IR_NODE_MODE ((IR_node_t) dependence_node_2))
    return FALSE;
  if (IR_IS_OF_TYPE ((IR_node_t) dependence_node_1, IR_NM_LR_set_dependence))
    return ((IR_LR_set ((IR_node_t) dependence_node_1)
             == IR_LR_set ((IR_node_t) dependence_node_2))
            && (IR_back_distance ((IR_node_t) dependence_node_1)
                == IR_back_distance ((IR_node_t) dependence_node_2))
            && (IR_look_ahead ((IR_node_t) dependence_node_1)
                == IR_look_ahead ((IR_node_t) dependence_node_2))
            && (IR_nonterm ((IR_node_t) dependence_node_1)
                == IR_nonterm ((IR_node_t) dependence_node_2)));
#ifndef NDEBUG
  else if (!IR_IS_OF_TYPE ((IR_node_t) dependence_node_1,
                           IR_NM_shift_LR_situation_dependence))
    assert (FALSE);
#endif
  else
    return ((IR_shift_LR_situation ((IR_node_t) dependence_node_1)
             == IR_shift_LR_situation ((IR_node_t) dependence_node_2))
            && (IR_look_ahead ((IR_node_t) dependence_node_1)
                == IR_look_ahead ((IR_node_t) dependence_node_2)));
}

static int unique_dependence_node_number;

static void
set_unique_dependence_node_number (IR_node_t dependence_node)
{
  IR_set_unique_number (dependence_node, unique_dependence_node_number);
  unique_dependence_node_number++;
}

static IR_node_t
get_LR_set_dependence_node (IR_node_t LR_set, int back_distance,
                            int look_ahead, IR_node_t nonterm, int *new_flag)
{
  hash_table_entry_t *entry_ptr;

  IR_set_LR_set (work_LR_set_dependence, LR_set);
  IR_set_back_distance (work_LR_set_dependence, back_distance);
  IR_set_look_ahead (work_LR_set_dependence, look_ahead);
  IR_set_nonterm (work_LR_set_dependence, nonterm);
  entry_ptr = find_hash_table_entry (dependence_nodes_table,
                                     work_LR_set_dependence, TRUE);
  *new_flag = *entry_ptr == NULL;
  if (*new_flag)
    {
      *entry_ptr
        = (hash_table_entry_t) IR_new_LR_set_dependence (look_ahead, LR_set,
                                                         back_distance,
                                                         nonterm);
      set_unique_dependence_node_number ((IR_node_t) *entry_ptr);
    }
  return (IR_node_t) *entry_ptr;
}

static IR_node_t
get_shift_LR_situation_dependence_node (IR_node_t shift_LR_situation,
                                        int look_ahead, int *new_flag)
{
  hash_table_entry_t *entry_ptr;

  assert (IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                            (shift_LR_situation)),
                         IR_NM_single_term_definition)
          && IR_first_symbol_LR_situation (shift_LR_situation));
  IR_set_shift_LR_situation (work_shift_LR_situation_dependence,
                             shift_LR_situation);
  IR_set_look_ahead (work_shift_LR_situation_dependence, look_ahead);
  entry_ptr = find_hash_table_entry (dependence_nodes_table,
                                     work_shift_LR_situation_dependence, TRUE);
  *new_flag = *entry_ptr == NULL;
  if (*new_flag)
    {
      *entry_ptr
        = ((hash_table_entry_t)
           IR_new_shift_LR_situation_dependence
           (look_ahead,
            IR_element_itself (IR_element_after_dot (shift_LR_situation)),
            shift_LR_situation));
      set_unique_dependence_node_number ((IR_node_t) *entry_ptr);
    }
  return (IR_node_t) *entry_ptr;
}

static void
initiate_dependence_nodes_table (void)
{
  work_LR_set_dependence = IR_create_node (IR_NM_LR_set_dependence);
  work_shift_LR_situation_dependence
    = IR_create_node (IR_NM_shift_LR_situation_dependence);
  unique_dependence_node_number = 0;
  dependence_nodes_table
    = create_hash_table (15000, dependence_nodes_table_hash_function,
                         dependence_nodes_table_eq_function);
}

static void
finish_dependence_nodes_table (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Dependence nodes table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (dependence_nodes_table),
       hash_table_elements_number (dependence_nodes_table),
       get_collisions (dependence_nodes_table));
#endif
  delete_hash_table (dependence_nodes_table);
}

#ifndef NDEBUG

#include "output.h"

static void
print_dependence_node (IR_node_t dependence_node, int context_flag)
{
  IR_node_t current_dependence_list_element;
  
  if (context_flag)
    {
      fprintf (stderr, "\n%d dependence context: ",
               IR_unique_number (dependence_node));
      output_context (stderr, IR_context (dependence_node));
    }
  else
    {
      if (IR_IS_OF_TYPE (dependence_node, IR_NM_shift_dependence))
        {
          fprintf (stderr, "\n%d shift dependence (la %d): ",
                   IR_unique_number (dependence_node),
                   IR_look_ahead (dependence_node));
          output_single_definition (stderr, IR_token (dependence_node));
        }
#ifndef NDEBUG
      else if (!IR_IS_OF_TYPE (dependence_node, IR_NM_LR_set_dependence))
        assert (FALSE);
#endif
      else
        {
          fprintf (stderr, "\n%d LR-set dependence: back %d, la %d, ",
                   IR_unique_number (dependence_node),
                   IR_back_distance (dependence_node),
                   IR_look_ahead (dependence_node));
          output_single_definition (stderr, IR_nonterm (dependence_node));
        }
      fprintf (stderr, "\n");
      fprintf (stderr, "dependencies:");
      for (current_dependence_list_element = IR_dependencies (dependence_node);
           current_dependence_list_element != NULL;
           current_dependence_list_element
             = IR_next_dependence_list_element
               (current_dependence_list_element))
        fprintf (stderr, " %d",
                 IR_unique_number (IR_dependence
                                   (current_dependence_list_element)));
    }
  fprintf (stderr, "\n");
}
#endif

/* The following structure describes history stack elements. */
struct stack_element
{
  /* LR-set which is know will be on the stack during processing the
     current LR-set. */
  IR_node_t LR_set;
  /* look ahead when we serached for the LR-set. */
  int look_ahead;
};

static IR_node_t
process_reduces (IR_node_t LR_set, int look_ahead, vlo_t *history_stack,
                 vlo_t *dependence_nodes_stack, vlo_t *auxiliary_stack)
{
  IR_node_t current_LR_situation;
  IR_node_t first_element;
  IR_node_t last_element;
  IR_node_t dependencies;
  IR_node_t canonical_rule;
  IR_node_t nonterm_definition;
  IR_node_t goto_LR_set;
  IR_node_t dependence_node;
  struct stack_element elem, *elem_ptr;
  int rule_length;
  int back_distance;
  int new_flag;
  int n;

  if (look_ahead < 1)
    return NULL;
  /* We don't want to drop in cycle here therfore we searching for
     LR-set which processed with given look ahead. */
  for (n = 0, elem_ptr = VLO_BEGIN (*history_stack);
       (char *) elem_ptr < (char *) VLO_BOUND (*history_stack);
       elem_ptr++)
    if (elem_ptr->LR_set == LR_set && elem_ptr->look_ahead == look_ahead)
      {
	/* We need at least processing twice for correct  conflict
           searching. */
	if (n == 1)
	  return NULL;
	n++;
      }
  elem.LR_set = LR_set;
  elem.look_ahead = look_ahead;
  VLO_ADD_MEMORY (*history_stack, &elem, sizeof (elem));
  first_element = NULL;
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation
         = IR_next_LR_situation (current_LR_situation))
    if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                       IR_NM_canonical_rule_end))
      {
        canonical_rule
          = IR_canonical_rule (IR_element_after_dot (current_LR_situation));
        nonterm_definition = IR_left_hand_side (canonical_rule);
        rule_length
          = canonical_rule_right_hand_side_prefix_length (canonical_rule,
                                                          NULL);
        back_distance
	  = (rule_length + 1
	     - VLO_LENGTH (*history_stack) / sizeof (struct stack_element));
        if (back_distance >= 0)
          {
            dependence_node
              = get_LR_set_dependence_node
    	        (((struct stack_element *) VLO_BEGIN (*history_stack))->LR_set,
                 back_distance, look_ahead, nonterm_definition, &new_flag);
            if (new_flag)
              VLO_ADD_MEMORY (*dependence_nodes_stack,
                              &dependence_node, sizeof (IR_node_t));
            first_element = IR_new_dependence_list_element (dependence_node,
                                                            first_element);
          }
        else
          {
            if (rule_length != 0)
              VLO_ADD_MEMORY (*auxiliary_stack,
                              (char *) VLO_BOUND (*history_stack)
                              - rule_length * sizeof (struct stack_element),
                              rule_length * sizeof (struct stack_element));
            VLO_SHORTEN (*history_stack,
			 rule_length * sizeof (struct stack_element));
            goto_LR_set
              = goto_by_nonterminal
                (((struct stack_element *) VLO_BEGIN (*history_stack))
                 [VLO_LENGTH (*history_stack) / sizeof (struct stack_element)
		 - 1].LR_set,
                 nonterm_definition);
            dependencies = process_reduces (goto_LR_set, look_ahead,
                                            history_stack,
                                            dependence_nodes_stack,
                                            auxiliary_stack);
            if (rule_length != 0)
              {
                VLO_ADD_MEMORY (*history_stack,
                                (char *) VLO_BOUND (*auxiliary_stack)
                                - rule_length * sizeof (struct stack_element),
                                rule_length * sizeof (struct stack_element));
                VLO_SHORTEN (*auxiliary_stack,
                             rule_length * sizeof (struct stack_element));
              }
            if (dependencies != NULL)
              {
                for (last_element = dependencies;
                     IR_next_dependence_list_element (last_element) != NULL;
                     last_element
                       = IR_next_dependence_list_element (last_element))
                  ;
                IR_set_next_dependence_list_element (last_element,
                                                     first_element);
                first_element = dependencies;
              }
          }
      }
    else if (IR_first_symbol_LR_situation (current_LR_situation)
             && IR_IS_OF_TYPE (IR_element_itself
                               (IR_element_after_dot
                                (current_LR_situation)),
                               IR_NM_single_term_definition))
      {
        dependence_node = IR_new_shift_dependence (look_ahead,
                                                   IR_element_itself
                                                   (IR_element_after_dot
                                                    (current_LR_situation)));
        set_unique_dependence_node_number (dependence_node);
        IR_set_dependencies (dependence_node,
                             process_reduces (IR_goto_LR_set
                                              (current_LR_situation),
                                              look_ahead - 1, history_stack,
                                              dependence_nodes_stack,
                                              auxiliary_stack));
        assert (look_ahead == 1 || IR_dependencies (dependence_node) != NULL);
#ifndef NDEBUG
        if (debug_level >= 2)
          print_dependence_node (dependence_node, FALSE);
#endif
        first_element = IR_new_dependence_list_element (dependence_node,
                                                        first_element);
      }
  VLO_SHORTEN (*history_stack, sizeof (struct stack_element));
  return first_element;
}

static void
process_dependencies (vlo_t *dependence_nodes_stack,
                      vlo_t *auxiliary_stack, vlo_t *history_stack)
{
  IR_node_t first_element;
  IR_node_t dependence_node;
  IR_node_t new_dependence_node;
  IR_node_t temp_LR_set;
  IR_node_t goto_LR_set;
  IR_node_t LR_situation_of_immediate_LR_set_predecessor;
  IR_double_link_t LR_situation_reference;
  struct stack_element elem;
  int new_flag;

  while (VLO_LENGTH (*dependence_nodes_stack) != 0)
    {
      dependence_node
        = ((IR_node_t *) VLO_BEGIN (*dependence_nodes_stack))
          [VLO_LENGTH (*dependence_nodes_stack) / sizeof (IR_node_t) - 1];
      VLO_SHORTEN (*dependence_nodes_stack, sizeof (IR_node_t));
      if (IR_back_distance (dependence_node) != 0)
        {
          first_element = NULL;
          for (LR_situation_reference
                 = IR__first_double_link (IR_LR_set (dependence_node));
               LR_situation_reference != NULL;
               LR_situation_reference
                 = IR__next_double_link (LR_situation_reference))
            {
              LR_situation_of_immediate_LR_set_predecessor
                = IR__owner (LR_situation_reference);
              if (IR_IS_OF_TYPE (LR_situation_of_immediate_LR_set_predecessor,
                                 IR_NM_LR_situation))
                {
                  /* See comments in file `ird.sprut'. */
                  assert (IR_first_symbol_LR_situation
                          (LR_situation_of_immediate_LR_set_predecessor));
                  new_dependence_node
                    = get_LR_set_dependence_node
                      (IR_LR_set
                       (LR_situation_of_immediate_LR_set_predecessor),
                       IR_back_distance (dependence_node) - 1,
                       IR_look_ahead (dependence_node),
                       IR_nonterm (dependence_node),
                       &new_flag);
                  if (new_flag)
                    VLO_ADD_MEMORY (*dependence_nodes_stack,
                                    &new_dependence_node, sizeof (IR_node_t));
                  first_element
                    = IR_new_dependence_list_element (new_dependence_node,
                                                      first_element);
                }
            }
          IR_set_dependencies (dependence_node, first_element);
        }
      else
        {
          VLO_NULLIFY (*history_stack);
          temp_LR_set = IR_LR_set (dependence_node);
	  elem.LR_set = temp_LR_set;
	  elem.look_ahead = IR_look_ahead (dependence_node);
          VLO_ADD_MEMORY (*history_stack, &elem,
			  sizeof (struct stack_element));
          goto_LR_set = goto_by_nonterminal (temp_LR_set,
                                             IR_nonterm (dependence_node));
          IR_set_dependencies (dependence_node,
                               process_reduces
                               (goto_LR_set, IR_look_ahead (dependence_node),
                                history_stack, dependence_nodes_stack,
                                auxiliary_stack));
        }
#ifndef NDEBUG
      if (debug_level >= 2)
        print_dependence_node (dependence_node, FALSE);
#endif
    }
}

static IR_node_t
build_graph_from_reduce (IR_node_t reduce_LR_situation, int look_ahead)
{
  IR_node_t result;
  IR_node_t LR_set;
  IR_node_t nonterm;
  IR_node_t canonical_rule;
  int back_distance;
  int new_flag;
  vlo_t dependence_nodes_stack;
  vlo_t auxiliary_stack;
  vlo_t history_stack;

  LR_set = IR_LR_set (reduce_LR_situation);
  canonical_rule
    = IR_canonical_rule (IR_element_after_dot (reduce_LR_situation));
  back_distance = canonical_rule_right_hand_side_prefix_length (canonical_rule,
                                                                NULL);
  nonterm = IR_left_hand_side (canonical_rule);
  result
    = get_LR_set_dependence_node (LR_set, back_distance, look_ahead, nonterm,
                                  &new_flag);
  if (!new_flag)
    return result;
  VLO_CREATE (dependence_nodes_stack, 0);
  VLO_CREATE (history_stack, 0);
  VLO_CREATE (auxiliary_stack, 0);
  VLO_ADD_MEMORY (dependence_nodes_stack, &result, sizeof (IR_node_t));
  process_dependencies (&dependence_nodes_stack,
                        &auxiliary_stack, &history_stack);
  VLO_DELETE (dependence_nodes_stack);
  VLO_DELETE (history_stack);
  VLO_DELETE (auxiliary_stack);
  return result;
}

static IR_node_t
build_graph_from_shift (IR_node_t shift_LR_situation, int look_ahead)
{
  IR_node_t result;
  IR_node_t LR_set;
  int new_flag;
  struct stack_element elem;
  vlo_t dependence_nodes_stack;
  vlo_t auxiliary_stack;
  vlo_t history_stack;
  
  assert (IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                            (shift_LR_situation)),
                         IR_NM_single_term_definition)
          && IR_first_symbol_LR_situation (shift_LR_situation));
  result = get_shift_LR_situation_dependence_node (shift_LR_situation,
                                                   look_ahead, &new_flag);
  if (!new_flag)
    return result;
  VLO_CREATE (dependence_nodes_stack, 0);
  VLO_CREATE (history_stack, 0);
  VLO_CREATE (auxiliary_stack, 0);
  LR_set = IR_LR_set (shift_LR_situation);
  elem.LR_set = LR_set;
  elem.look_ahead = look_ahead;
  VLO_ADD_MEMORY (history_stack, &elem, sizeof (struct stack_element));
  IR_set_dependencies (result,
                       process_reduces (IR_goto_LR_set (shift_LR_situation),
                                        look_ahead - 1, &history_stack,
                                        &dependence_nodes_stack,
                                        &auxiliary_stack));
  process_dependencies (&dependence_nodes_stack,
                        &auxiliary_stack, &history_stack);
  VLO_DELETE (dependence_nodes_stack);
  VLO_DELETE (history_stack);
  VLO_DELETE (auxiliary_stack);
  return result;
}


static int
evaluate_node_context (IR_node_t dependence_node, int pass_number)
{
  IR_node_t current_dependence_list_element;
  IR_node_t current_dependence;
  IR_node_t terminal;
  context_t context;
  context_t dependence_context;
  int change_flag;

  if (IR_context_has_been_evaluated (dependence_node)
      || IR_pass_number (dependence_node) == pass_number)
    return FALSE;
  IR_set_pass_number (dependence_node, pass_number);
  context = get_null_context ();
  change_flag = FALSE;
  for (current_dependence_list_element = IR_dependencies (dependence_node);
       current_dependence_list_element != NULL;
       current_dependence_list_element
         = IR_next_dependence_list_element (current_dependence_list_element))
    {
      current_dependence = IR_dependence (current_dependence_list_element);
      if (evaluate_node_context (current_dependence, pass_number))
        change_flag = TRUE;
      dependence_context = IR_context (current_dependence);
      if (IR_IS_OF_TYPE (dependence_node, IR_NM_LR_set_dependence))
        {
          if (dependence_context != NULL)
            context_or (context, dependence_context);
        }
#ifndef NDEBUG
      else if (!IR_IS_OF_TYPE (dependence_node, IR_NM_shift_dependence))
        assert (FALSE);
#endif
      else
        {
          assert (dependence_context != NULL);
          if (!it_is_zero_context (dependence_context))
            add_terminal_and_context_concatenation
              (&context, IR_token (dependence_node), dependence_context);
        }
    }
  if (IR_dependencies (dependence_node) == NULL)
    {
      assert (IR_IS_OF_TYPE (dependence_node, IR_NM_shift_dependence));
      terminal = IR_token (dependence_node);
      set_context_element_value (context,
                                 get_new_token_string (&terminal, 1), 1);
    }
  if (IR_context (dependence_node) == NULL
      || !context_in (context, IR_context (dependence_node)))
    {
      IR_set_context (dependence_node, insert_or_free_context (context));
      change_flag = TRUE;
    }
  else
    free_context (context);
  return change_flag;
}

static void
set_evaluation_flags (IR_node_t dependence_node)
{
  IR_node_t current_dependence_list_element;

  if (IR_context_has_been_evaluated (dependence_node))
    return;
  IR_set_context_has_been_evaluated (dependence_node, TRUE);
#ifndef NDEBUG
  if (debug_level >= 2)
    print_dependence_node (dependence_node, TRUE);
#endif
  for (current_dependence_list_element = IR_dependencies (dependence_node);
       current_dependence_list_element != NULL;
       current_dependence_list_element
         = IR_next_dependence_list_element (current_dependence_list_element))
    set_evaluation_flags (IR_dependence (current_dependence_list_element));
}

static int
evaluate_graph_context (IR_node_t dependence_node, int pass_number)
{
#ifndef NDEBUG
  if (debug_level >= 2)
    fprintf (stderr, "\n**** Top evaluation of %d dependence context ****\n",
	     IR_unique_number (dependence_node));
#endif
  while (evaluate_node_context (dependence_node, pass_number++))
    ;
  set_evaluation_flags (dependence_node);
  return pass_number;
}


static int pass_number = 0;

static context_t
LALR_situation_context (IR_node_t LR_situation, int look_ahead,
                        vlo_t *history_stack)
{
  IR_node_t dependence_node;

  assert (look_ahead > 0);
  if (IR_IS_OF_TYPE (IR_element_after_dot (LR_situation),
                     IR_NM_canonical_rule_end))
    dependence_node = build_graph_from_reduce (LR_situation, look_ahead);
#ifndef NDEBUG
  else if (!IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                              (LR_situation)),
                           IR_NM_single_term_definition)
           || !IR_first_symbol_LR_situation (LR_situation))
    assert (FALSE);
#endif
  else
    dependence_node = build_graph_from_shift (LR_situation, look_ahead);
  pass_number = evaluate_graph_context (dependence_node, pass_number);
  assert (IR_context (dependence_node) != NULL);
  return IR_context (dependence_node);
}

/* The following variable value is new context being formed by shortening
   some token string. */

static context_t new_LR_situation_context;

/* The following variable is equal to length of the following array
   and token strings of the processed context. */

static int max_LR_situation_conflict_context_look_ahead;

/* The following variable contains all conflicts fixed on various look
   ahead numbers.  Zero index element corresponds to look ahead number
   equal to 1. */

static context_t *LR_situation_conflict_contexts_array;

static void
shorten_LR_situation_context_token_string (token_string_t token_string)
{
  int current_token_string_length;
  token_string_t minimal_length_context_token_string_without_conflict;

#if 0
  if (max_LR_situation_conflict_context_look_ahead
      != token_string_length (token_string))
    output_token_string (token_string, stderr);
#endif
  assert (max_LR_situation_conflict_context_look_ahead
          == token_string_length (token_string));
  minimal_length_context_token_string_without_conflict = token_string;
  for (current_token_string_length
       = max_LR_situation_conflict_context_look_ahead;
       current_token_string_length > 1;
       current_token_string_length--)
    if (it_is_in_context
        (token_string,
         LR_situation_conflict_contexts_array
         [current_token_string_length - 1]))
      break;
    else
      {
        minimal_length_context_token_string_without_conflict = token_string;
        token_string
          = token_string_shortening (token_string,
                                     current_token_string_length - 1);
      }
  set_context_element_value
    (new_LR_situation_context,
     minimal_length_context_token_string_without_conflict, 1);
}

static void
shorten_LR_context_token_strings (IR_node_t LR_situation,
                                  context_t *conflict_contexts_array,
                                  int max_LR_situation_context_look_ahead)
{
  assert (IR_IS_OF_TYPE (IR_element_after_dot (LR_situation),
                         IR_NM_canonical_rule_end)
          || (IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                                (LR_situation)),
                             IR_NM_single_term_definition)
              && IR_first_symbol_LR_situation (LR_situation)));
  new_LR_situation_context = get_null_context ();
  max_LR_situation_conflict_context_look_ahead
    = max_LR_situation_context_look_ahead;
  LR_situation_conflict_contexts_array = conflict_contexts_array;
  process_context_token_strings
    (IR_look_ahead_context (LR_situation),
     shorten_LR_situation_context_token_string);
  IR_set_look_ahead_context
    (LR_situation, insert_or_free_context (new_LR_situation_context));
}

static int
process_LR_set_conflicts (int DeRemer_flag, IR_node_t LR_set,
                          context_t *look_ahead_conflict_contexts_array)
{
  IR_node_t another_LR_situation;
  IR_node_t current_symbol_LR_situation;
  IR_node_t current_LR_situation;
  int current_look_ahead;
  IR_node_t single_term_definition;
  context_t current_LR_situation_context;
  context_t context;
  context_t conflicts;
  context_t prohibited_look_ahead_token_strings;
  int last_current_LR_situation_context_look_ahead;
  int there_is_conflict;
  int current_LR_situation_number;
  int another_LR_situation_number;
  token_string_t token_string;
  int max_current_LR_situation_context_look_ahead;
  int modification_flag;
  int max_current_look_ahead_number;
  int la;
  vlo_t empty_history_stack;

  modification_flag = FALSE;
#ifndef NDEBUG
  if (debug_level >= 2)
    {
      fprintf (stderr, "process LR-set conflicts\n");
      output_LR_set_situations (stderr, LR_set, "\t");
    }
#endif
  if (DeRemer_flag)
    VLO_CREATE (empty_history_stack, 4);
  /* 1. Loop on all reduce or first symbol non-reduce LR_situations. */
  for (current_LR_situation_number = 0,
       current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    {
      current_LR_situation_number++;
      if (!IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                          IR_NM_canonical_rule_end)
          && (!IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                                 (current_LR_situation)),
                              IR_NM_single_term_definition)
              || !IR_first_symbol_LR_situation (current_LR_situation)))
        continue;
      last_current_LR_situation_context_look_ahead = 0;
      max_current_LR_situation_context_look_ahead = 0;
      prohibited_look_ahead_token_strings = get_null_context ();
      /* 2. Loop on all reduce or first symbol non-reduce LR_situations
         when combinations
         reduce-reduce, reduce-nonreduce, nonreduce-reduce. */
      for (another_LR_situation_number = 0,
           another_LR_situation = IR_LR_situation_list (LR_set);
           another_LR_situation != NULL;
           another_LR_situation = IR_next_LR_situation (another_LR_situation))
        {
          another_LR_situation_number++;
          if (another_LR_situation == current_LR_situation
              ||
              (!IR_IS_OF_TYPE (IR_element_after_dot (another_LR_situation),
                               IR_NM_canonical_rule_end)
               && (!IR_IS_OF_TYPE (IR_element_after_dot
                                   (current_LR_situation),
                                   IR_NM_canonical_rule_end)
                   || !IR_IS_OF_TYPE (IR_element_itself
                                      (IR_element_after_dot
                                       (another_LR_situation)),
                                      IR_NM_single_term_definition)
                   || !IR_first_symbol_LR_situation (another_LR_situation))))
            continue;
          if (!IR_IS_OF_TYPE (IR_element_after_dot (another_LR_situation),
                              IR_NM_canonical_rule_end))
            single_term_definition
              = IR_element_itself (IR_element_after_dot
                                   (another_LR_situation));
	  max_current_look_ahead_number = look_ahead_number;
	  la = IR_max_look_ahead_value (IR_canonical_rule
					(IR_element_after_dot
					 (current_LR_situation)));
	  if (max_current_look_ahead_number < la)
	    max_current_look_ahead_number = la;
	  la = IR_max_look_ahead_value (IR_canonical_rule
					(IR_element_after_dot
					 (another_LR_situation)));
	  if (max_current_look_ahead_number < la)
	    max_current_look_ahead_number = la;
          /* Loop on look-ahead string lengths. */
          for (current_look_ahead = 1;
               current_look_ahead <= max_current_look_ahead_number;
               current_look_ahead++)
            {
              if (last_current_LR_situation_context_look_ahead
                  != current_look_ahead)
                {
                  /* Evaluation of look-ahead 1st LR-situation. */
                  if (IR_IS_OF_TYPE (IR_element_after_dot
                                     (current_LR_situation),
                                     IR_NM_canonical_rule_end))
                    {
                      if (DeRemer_flag)
                        current_LR_situation_context
                          = LALR_situation_context
                            (current_LR_situation, current_look_ahead,
                             &empty_history_stack);
                      else
                        current_LR_situation_context
                          = insert_or_free_context
                            (context_shortening
                             (IR_context (current_LR_situation),
                              current_look_ahead));
                    }
                  else
                    {
                      if (DeRemer_flag)
                        current_LR_situation_context
                          = LALR_situation_context
                            (current_LR_situation, current_look_ahead,
                             &empty_history_stack);
                      else
                        current_LR_situation_context
                          = insert_or_free_context
                            (FIRST_of_tail
                             (IR_element_after_dot (current_LR_situation),
                              IR_context (current_LR_situation),
                              current_look_ahead));
                    }
                  last_current_LR_situation_context_look_ahead
                    = current_look_ahead;
                  if (max_current_LR_situation_context_look_ahead
                      < current_look_ahead)
                    {
                      zero_context (look_ahead_conflict_contexts_array
                                    [current_look_ahead - 1]);
                      IR_set_look_ahead_context
                        (current_LR_situation, current_LR_situation_context);
                      max_current_LR_situation_context_look_ahead
                        = current_look_ahead;
                    }
                }
              /* Evaluation of look-ahead 2nd LR-situation. */
              if (IR_IS_OF_TYPE (IR_element_after_dot (another_LR_situation),
                                 IR_NM_canonical_rule_end))
                {
                  if (DeRemer_flag)
                    context
                      = LALR_situation_context
                        (another_LR_situation, current_look_ahead,
                         &empty_history_stack);
                  else
                    context
                      = insert_or_free_context
                        (context_shortening
                         (IR_context(another_LR_situation),
                          current_look_ahead));
                  if (!IR_IS_OF_TYPE (IR_element_after_dot
                                      (current_LR_situation),
                                      IR_NM_canonical_rule_end)
                      && IR_rule_priority (IR_canonical_rule
                                           (IR_element_after_dot
                                            (another_LR_situation))) >= 0
                      && IR_priority (IR_element_itself
                                      (IR_element_after_dot
                                       (current_LR_situation))) >= 0)
                    {
                      /* First iteration of loop on look ahead
                         lengths.  Conflict resolution on the base of
                         priorities (%left, ...). */
                      assert (current_look_ahead == 1);
                      single_term_definition
                        = IR_element_itself (IR_element_after_dot
                                             (current_LR_situation));
                      token_string
                        = get_new_token_string (&single_term_definition, 1);
                      if (it_is_in_context (token_string, context))
                        {
                          set_context_element_value
                            (look_ahead_conflict_contexts_array [0],
                             token_string, 1);
                          if (IR_priority (single_term_definition)
                              < IR_rule_priority (IR_canonical_rule
                                                  (IR_element_after_dot
                                                   (another_LR_situation)))
                              ||
                              ((IR_priority (single_term_definition)
                                == IR_rule_priority (IR_canonical_rule
                                                     (IR_element_after_dot
                                                      (another_LR_situation))))
                               &&
                               (IR_left_assoc_flag (single_term_definition)
                                || IR_nonassoc_flag (single_term_definition))))
                            {
                              /* Prohibit shift. */
                              IR_set_goto_arc_has_been_removed
                                (current_LR_situation, TRUE);
                              modification_flag = TRUE;
                            }
                        }
                      break;
                    }
                  else if (IR_under_control_point_flag (current_LR_situation)
                           &&
                           IR_under_control_point_flag (another_LR_situation))
                    {
                      /* Possible bactracking: shift-reduce,
                         reduce-reduce conflicts */
                      assert (current_look_ahead == 1);
                      IR_set_back_tracking_flag (LR_set, TRUE);
                      IR_set_back_tracking_exists (description, TRUE);
                      break;
                    }
                }
              else
                {
                  assert (IR_IS_OF_TYPE (IR_element_after_dot
                                         (current_LR_situation),
                                         IR_NM_canonical_rule_end));
                  if (IR_rule_priority (IR_canonical_rule
                                        (IR_element_after_dot
                                         (current_LR_situation))) >= 0
                      && IR_priority (single_term_definition) >= 0)
                    {
                      /* First iteration of loop on look ahead
                         lengths.  Conflict resolution on the base of
                         priorities (%left, ...). */
                      assert (current_look_ahead == 1);
                      token_string
                        = get_new_token_string (&single_term_definition, 1);
                      if (it_is_in_context (token_string,
                                            current_LR_situation_context))
                        {
                          set_context_element_value
                            (look_ahead_conflict_contexts_array [0],
                             token_string, 1);
                          if (IR_priority (single_term_definition)
                              > IR_rule_priority (IR_canonical_rule
                                                  (IR_element_after_dot
                                                   (current_LR_situation)))
                              ||
                              ((IR_priority (single_term_definition)
                                == IR_rule_priority (IR_canonical_rule
                                                     (IR_element_after_dot
                                                      (current_LR_situation))))
                               &&
                               (IR_right_assoc_flag (single_term_definition)
                                || IR_nonassoc_flag (single_term_definition))))
                            /* Prohibit reduce. */
                            set_context_element_value
                              (prohibited_look_ahead_token_strings,
                               token_string, 1);
                        }
                      break;
                    }
                  else if (IR_under_control_point_flag (current_LR_situation)
                           &&
                           IR_under_control_point_flag (another_LR_situation))
                    {
                      /* Possible bactracking: reduce-shift conflicts */
                      assert (current_look_ahead == 1);
                      IR_set_back_tracking_flag (LR_set, TRUE);
                      IR_set_back_tracking_exists (description, TRUE);
                      break;
                    }
                  else if (DeRemer_flag)
                    context
                      = LALR_situation_context (another_LR_situation,
                                                current_look_ahead,
                                                &empty_history_stack);
                  else
                    context
                      = insert_or_free_context
                        (FIRST_of_tail
                         (IR_element_after_dot (another_LR_situation),
                          IR_context (another_LR_situation),
                          current_look_ahead));
                }
              conflicts = get_null_context ();
              context_copy (conflicts, context);
              context_and (conflicts, current_LR_situation_context);
              there_is_conflict = !it_is_zero_context (conflicts);
              if (there_is_conflict)
                context_or (look_ahead_conflict_contexts_array
                            [current_look_ahead - 1], conflicts);
              /* Remember: shift-reduce will be processed on the next
                 iterations as reduce-shift when 2nd LR-situation
                 become 1st one */
              if (there_is_conflict
                  && IR_IS_OF_TYPE (IR_element_after_dot
                                    (current_LR_situation),
                                    IR_NM_canonical_rule_end)
                  && current_look_ahead == max_current_look_ahead_number)
                {
                  /* The last iteration of loop on look ahead strings.
                     Default conflict resolution (shift over
                     reduce).*/
                  conflict_resolution_fold_LR_situation
                    = current_LR_situation;
                  if (!IR_IS_OF_TYPE (IR_element_after_dot
                                      (another_LR_situation),
                                      IR_NM_canonical_rule_end))
                    {
                      /* reduce - shift */
                      context_or (prohibited_look_ahead_token_strings,
                                  conflicts);
                      fold_is_used_in_conflict_resolution = FALSE;
                      /* register all conflicts with the same symbol
                         as the symbol for 2nd LR-situation. */
                      for (current_symbol_LR_situation = another_LR_situation;
                           current_symbol_LR_situation != NULL;
                           current_symbol_LR_situation
                           = IR_next_LR_situation
                           (current_symbol_LR_situation))
                        if (!IR_IS_OF_TYPE
                            (IR_element_after_dot
                             (current_symbol_LR_situation),
                             IR_NM_canonical_rule_end)
                            && (IR_element_itself (IR_element_after_dot
                                                   (another_LR_situation))
                                ==
                                IR_element_itself
                                (IR_element_after_dot
                                 (current_symbol_LR_situation))))
                          {
                            another_conflict_resolution_LR_situation
                              = current_symbol_LR_situation;
                            process_context_token_strings
                              (conflicts, add_context_conflicts);
                          }
                    }
                  else
                    {
                      /* reduce - reduce */
                      fold_is_used_in_conflict_resolution
                        = (IR_canonical_rule_order_number
                           (IR_canonical_rule (IR_element_after_dot
                                               (current_LR_situation)))
                           <
                           IR_canonical_rule_order_number
                           (IR_canonical_rule (IR_element_after_dot
                                               (another_LR_situation))));
                      if (!fold_is_used_in_conflict_resolution)
                        context_or (prohibited_look_ahead_token_strings,
                                    conflicts);
                      /* The folowing disables repeated message about
                         reduce/reduce conflict on the next
                         iterations. */
                      if (another_LR_situation_number
                          < current_LR_situation_number)
                        {
                          another_conflict_resolution_LR_situation
                            = another_LR_situation;
                          process_context_token_strings
                            (conflicts, add_context_conflicts);
                        }
                    }
                }
              free_context (conflicts);
              if (!there_is_conflict)
                break;
            }
        }
      if (!it_is_zero_context (prohibited_look_ahead_token_strings))
        {
          modification_flag = TRUE;
          assert (max_current_LR_situation_context_look_ahead == 1
                  || (max_current_LR_situation_context_look_ahead
                      == max_current_look_ahead_number));
          context = get_null_context ();
          context_copy (context,
                        IR_look_ahead_context (current_LR_situation));
          context_subtraction (context, prohibited_look_ahead_token_strings);
          IR_set_look_ahead_context (current_LR_situation,
                                     insert_or_free_context (context));
        }
      free_context (prohibited_look_ahead_token_strings);
      /* Shorten some token strings of the given LR-situation
         context to minimal length necessary for conflict
         resolution. */
      if (max_current_LR_situation_context_look_ahead != 0)
        shorten_LR_context_token_strings
          (current_LR_situation, look_ahead_conflict_contexts_array,
           max_current_LR_situation_context_look_ahead);
    }
  if (DeRemer_flag)
    VLO_DELETE (empty_history_stack);
  return modification_flag;
}

static void
mark_reachable_canonical_LR_sets (IR_node_t LR_set)
{
  IR_node_t current_LR_situation;

  if (IR_reachable_flag (LR_set))
    return;
#ifndef NDEBUG
  if (debug_level >= 2)
    {
      fprintf (stderr, "mark canonical LR set as reachable\n");
      output_LR_set_situations (stderr, LR_set, "\t");
    }
#endif
  IR_set_reachable_flag (LR_set, TRUE);
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    {
      if (IR_first_symbol_LR_situation (current_LR_situation)
          && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                             IR_NM_canonical_rule_end)
          /* The following guard for LR-situation `$accept : axiom . $end'. */
          && IR_goto_LR_set (current_LR_situation) != NULL
          && !IR_goto_arc_has_been_removed (current_LR_situation))
        mark_reachable_canonical_LR_sets (IR_goto_LR_set
                                          (current_LR_situation));
    }
}

static void
mark_unreachable_canonical_LR_sets (void)
{
  IR_node_t start_LR_set;
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;

  /* Clear reachable flags. */
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      IR_set_reachable_flag (current_LR_set, FALSE);
  start_LR_set = IR_LR_set_list (IR_LR_core_list (description));
  assert (IR_next_LR_set (start_LR_set) == NULL);
  mark_reachable_canonical_LR_sets (start_LR_set);
}

void
process_conflicts (int DeRemer_flag)
{
  int current_look_ahead;
  IR_node_t LR_core;
  IR_node_t current_LR_set;
  int modification_flag;
  vlo_t look_ahead_conflict_contexts;
  context_t empty_context;
  context_t *look_ahead_conflict_contexts_array;

  VLO_CREATE (look_ahead_conflict_contexts,
              sizeof (context_t) * max_look_ahead_number);
  for (current_look_ahead = 1;
       current_look_ahead <= max_look_ahead_number;
       current_look_ahead++)
    {
      empty_context = get_null_context ();
      VLO_ADD_MEMORY (look_ahead_conflict_contexts, &empty_context,
                      sizeof (context_t));
    }
  look_ahead_conflict_contexts_array
    = VLO_BEGIN (look_ahead_conflict_contexts);
  modification_flag = FALSE;
  for (LR_core = IR_LR_core_list (description); LR_core != NULL;
       LR_core = IR_next_LR_core (LR_core))
    for (current_LR_set = IR_LR_set_list (LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      modification_flag
        = process_LR_set_conflicts (DeRemer_flag, current_LR_set,
                                    look_ahead_conflict_contexts_array)
          || modification_flag;
  for (current_look_ahead = 1;
       current_look_ahead <= max_look_ahead_number;
       current_look_ahead++)
    free_context (look_ahead_conflict_contexts_array
                  [current_look_ahead - 1]);
  VLO_DELETE (look_ahead_conflict_contexts);
  if (modification_flag)
    {
      /* The LR-graph has been modified. */
      mark_unreachable_canonical_LR_sets ();
    }
}

/* The following variable value is number of canonical LR-sets (before
   any optimization). */

int original_LR_sets_number;

/* The following variable value is number of canonical LR-cores (before
   any optimization). */

int original_LR_cores_number;

/* The following variable value is number of important LR-situations in
   canonical LR-sets (before any optimization). */

int important_original_LR_situations_number;

/* The following variable value is number of all LR-situations in
   canonical LR-sets (before any optimization). */

int all_original_LR_situations_number;

void
create_LALR_sets (void)
{
  IR_node_t current_LR_set;
  IR_node_t start_LR_set;
  IR_node_t LR_core;
  IR_node_t first_new_LR_situation;
  IR_node_t current_new_LR_situation;
  IR_node_t current_LR_situation;
  IR_node_t goto_LR_set;
  vlo_t LR0_sets_stack;
  ticker_t temp_ticker;

  temp_ticker = create_ticker ();
  /* Create LR(0)-sets. */
  VLO_CREATE (LR0_sets_stack, 5000);
  start_LR_set = create_start_LR_set (FALSE);
  VLO_ADD_MEMORY (LR0_sets_stack, &start_LR_set, sizeof (IR_node_t));
  while (VLO_LENGTH (LR0_sets_stack) != 0)
    {
      current_LR_set
        = ((IR_node_t *) VLO_BEGIN (LR0_sets_stack))
          [VLO_LENGTH (LR0_sets_stack) / sizeof (IR_node_t) - 1];
      VLO_SHORTEN (LR0_sets_stack, sizeof (IR_node_t));
      make_full_LR_set (current_LR_set, FALSE);
      /* Create new LR0-set achieved from the current LR0-set. */
      for (current_LR_situation = IR_LR_situation_list (current_LR_set);
           current_LR_situation != NULL;
           current_LR_situation
             = IR_next_LR_situation (current_LR_situation))
        if (IR_first_symbol_LR_situation (current_LR_situation)
            && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                               IR_NM_canonical_rule_end))
          {
            first_new_LR_situation
              = get_shifted_LR_situation_list (current_LR_situation);
            LR_core = find_LR_core (first_new_LR_situation);
            if (LR_core == NULL)
              {
                /* This is new LR-core and LR-set. */
                LR_core
                  = IR_new_LR_core (NULL, IR_next_LR_core (IR_LR_core_list
                                                           (description)));
                add_LR_core_to_cyclic_list (LR_core);
                goto_LR_set
                  = IR_new_LR_set (first_new_LR_situation, LR_core, NULL);
                add_LR_set_to_cyclic_list (goto_LR_set, LR_core);
                (void) insert_LR_core (LR_core);
                for (current_new_LR_situation = first_new_LR_situation;
                     current_new_LR_situation != NULL;
                     current_new_LR_situation
                       = IR_next_LR_situation (current_new_LR_situation))
                  IR_set_LR_set (current_new_LR_situation, goto_LR_set);
                /* Add new LR-set (it contain only important
                   LR-situation here) to the stack. */
                VLO_ADD_MEMORY (LR0_sets_stack, &goto_LR_set,
                                sizeof (IR_node_t));
              }
            else
              {
                goto_LR_set = IR_LR_set_list (LR_core);
                free_LR_situations_list (first_new_LR_situation);
              }
            /* Set up field `goto_LR_set'. */
            IR_set_goto_LR_set (current_LR_situation, goto_LR_set);
          }
    }
  VLO_DELETE (LR0_sets_stack);
  make_uncyclic_LR_set_and_core_lists ();
  original_LR_sets_number = 0;
  important_original_LR_situations_number = 0;
  all_original_LR_situations_number = 0;
  original_LR_cores_number = 0;
  for (LR_core = IR_LR_core_list (description); LR_core != NULL;
       LR_core = IR_next_LR_core (LR_core))
    {
      original_LR_cores_number++;
      for (current_LR_set = IR_LR_set_list (LR_core);
           current_LR_set != NULL;
           current_LR_set = IR_next_LR_set (current_LR_set))
        count_LR_set_situations (current_LR_set, &original_LR_sets_number,
                                 &important_original_LR_situations_number,
                                 &all_original_LR_situations_number);
    }
  if (time_flag)
    fprintf (stderr, "      Creating LR(0)-sets -- %ssec\n",
             active_time_string (temp_ticker));
  initiate_dependence_nodes_table ();
  process_conflicts (TRUE);
  finish_dependence_nodes_table ();
  if (time_flag)
    fprintf (stderr, "      Conflicts processing -- %ssec\n",
             active_time_string (temp_ticker));
}

/* This function is used only when it is neccesary to output
   LR-situations contexts into the parser description file. */

void
create_LALR_sets_with_all_contexts (void)
{
  IR_node_t current_LR_set;
  IR_node_t *modified_LR_set_ptr;
  IR_node_t start_LR_set;
  IR_node_t LR_core;
  IR_node_t current_new_LR_situation;
  IR_node_t first_new_LR_situation;
  IR_node_t current_goto_LR_situation;
  IR_node_t current_LR_situation;
  int new_LR_core_flag;
  IR_node_t goto_LR_set;
  int context_modification_flag;
  context_t new_context;
  vlo_t LALR_sets_stack;
  vlo_t modified_LALR_sets_stack;

  VLO_CREATE (LALR_sets_stack, 5000);
  start_LR_set = create_start_LR_set (TRUE);
  VLO_ADD_MEMORY (LALR_sets_stack, &start_LR_set, sizeof (IR_node_t));
  IR_set_it_is_in_LALR_sets_stack (start_LR_set, TRUE);
  VLO_CREATE (modified_LALR_sets_stack, 5000);
  while (VLO_LENGTH (LALR_sets_stack) != 0)
    {
      current_LR_set
        = ((IR_node_t *) VLO_BEGIN (LALR_sets_stack))
          [VLO_LENGTH (LALR_sets_stack) / sizeof (IR_node_t) - 1];
      VLO_SHORTEN (LALR_sets_stack, sizeof (IR_node_t));
      IR_set_it_is_in_LALR_sets_stack (current_LR_set, FALSE);
      make_full_LR_set (current_LR_set, TRUE);
      /* Create new LALR-set achieved from the current LALR-set. */
      for (current_LR_situation = IR_LR_situation_list (current_LR_set);
           current_LR_situation != NULL;
           current_LR_situation
             = IR_next_LR_situation (current_LR_situation))
        if (IR_first_symbol_LR_situation (current_LR_situation)
            && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                               IR_NM_canonical_rule_end))
          {
            first_new_LR_situation
              = get_shifted_LR_situation_list (current_LR_situation);
            LR_core = find_LR_core (first_new_LR_situation);
            if (LR_core == NULL)
                {
                  /* This is new LR-core. */
                  LR_core
                    = IR_new_LR_core (NULL, IR_next_LR_core (IR_LR_core_list
                                                             (description)));
                  add_LR_core_to_cyclic_list (LR_core);
                  new_LR_core_flag = TRUE;
                }
            else
              new_LR_core_flag = FALSE;
            goto_LR_set = IR_LR_set_list (LR_core);
            if (goto_LR_set == NULL)
              {
                /* This is new LR-set. */
                goto_LR_set
                  = IR_new_LR_set (first_new_LR_situation, LR_core, NULL);
                add_LR_set_to_cyclic_list (goto_LR_set, LR_core);
                if (new_LR_core_flag)
                  (void) insert_LR_core (LR_core);
                for (current_new_LR_situation = first_new_LR_situation;
                     current_new_LR_situation != NULL;
                     current_new_LR_situation
                       = IR_next_LR_situation (current_new_LR_situation))
                  IR_set_LR_set (current_new_LR_situation, goto_LR_set);
                /* Add modified LR-set (it contain only important
                   LR-situation here) to the stack. */
                VLO_ADD_MEMORY (modified_LALR_sets_stack, &goto_LR_set,
                                sizeof (IR_node_t));
              }
            else
              {
                /* Change contexts of important LALR-situations. */
                context_modification_flag = FALSE;
                for (current_goto_LR_situation
                       = IR_LR_situation_list (goto_LR_set),
                       current_new_LR_situation = first_new_LR_situation;
                     current_new_LR_situation != NULL;
                     current_goto_LR_situation
                       = IR_next_LR_situation (current_goto_LR_situation),
                       current_new_LR_situation
                       = IR_next_LR_situation (current_new_LR_situation))
                  {
                    assert
                      (IR_important_LR_situation_flag
                       (current_goto_LR_situation)
                       && (IR_element_after_dot (current_new_LR_situation)
                           == (IR_element_after_dot
                               (current_goto_LR_situation))));
                    if (!context_in (IR_context (current_new_LR_situation),
                                     IR_context (current_goto_LR_situation)))
                      {
                        context_modification_flag = TRUE;
                        break;
                      }
                  }
                if (context_modification_flag)
                  {
                    for (current_goto_LR_situation
                           = IR_LR_situation_list (goto_LR_set),
                           current_new_LR_situation = first_new_LR_situation;
                         current_new_LR_situation != NULL;
                         current_goto_LR_situation
                           = IR_next_LR_situation (current_goto_LR_situation),
                           current_new_LR_situation
                           = IR_next_LR_situation (current_new_LR_situation))
                      {
                        /* The following is necessary because contexts
                           of the LR_situations in the context table. */
                        new_context = get_null_context ();
                        context_copy
                          (new_context,
                           IR_context (current_goto_LR_situation));
                        context_or (new_context,
                                    IR_context (current_new_LR_situation));
                        IR_set_context (current_goto_LR_situation,
                                        new_context);
                      }
                    /* Add modified LR-set to the stack. */
                    VLO_ADD_MEMORY (modified_LALR_sets_stack, &goto_LR_set,
                                    sizeof (IR_node_t));
                  }
                free_LR_situations_list (first_new_LR_situation);
              }
            /* Set up field `goto_LR_set'. */
            IR_set_goto_LR_set (current_LR_situation, goto_LR_set);
          }
      for (modified_LR_set_ptr = VLO_BEGIN (modified_LALR_sets_stack);
           (char *) modified_LR_set_ptr
           <= (char *) VLO_END (modified_LALR_sets_stack);
           modified_LR_set_ptr++)
        if (!IR_it_is_in_LALR_sets_stack (*modified_LR_set_ptr))
          {
            free_unimportant_LR_situations (*modified_LR_set_ptr);
            VLO_ADD_MEMORY (LALR_sets_stack, modified_LR_set_ptr,
                            sizeof (IR_node_t));
            IR_set_it_is_in_LALR_sets_stack (*modified_LR_set_ptr, TRUE);
          }
      VLO_NULLIFY (modified_LALR_sets_stack);
    }
  VLO_DELETE (modified_LALR_sets_stack);
  VLO_DELETE (LALR_sets_stack);
  make_uncyclic_LR_set_and_core_lists ();
  original_LR_sets_number = 0;
  important_original_LR_situations_number = 0;
  all_original_LR_situations_number = 0;
  original_LR_cores_number = 0;
  for (LR_core = IR_LR_core_list (description); LR_core != NULL;
       LR_core = IR_next_LR_core (LR_core))
    {
      original_LR_cores_number++;
      for (current_LR_set = IR_LR_set_list (LR_core);
           current_LR_set != NULL;
           current_LR_set = IR_next_LR_set (current_LR_set))
        count_LR_set_situations (current_LR_set, &original_LR_sets_number,
                                 &important_original_LR_situations_number,
                                 &all_original_LR_situations_number);
    }
}

void
create_LR_sets (void)
{
  IR_node_t current_LR_situation;
  IR_node_t current_LR_set;
  IR_node_t LR_core;
  IR_node_t first_new_LR_situation;
  IR_node_t current_new_LR_situation;
  int new_LR_core_flag;
  IR_node_t goto_LR_set;
  IR_node_t start_LR_set;
  vlo_t first_LR_sets_of_LR_core;
  vlo_t LR_sets_stack;

  /* Two stacks as used only for generation the same parser
     description file as when function `create_LALR_sets' is used. */
  VLO_CREATE (first_LR_sets_of_LR_core, 3000);
  VLO_CREATE (LR_sets_stack, 6000);
  start_LR_set = create_start_LR_set (TRUE);
  VLO_ADD_MEMORY (first_LR_sets_of_LR_core, &start_LR_set, sizeof (IR_node_t));
  original_LR_sets_number = 0;
  important_original_LR_situations_number = 0;
  all_original_LR_situations_number = 0;
  while (VLO_LENGTH (first_LR_sets_of_LR_core) != 0
         || VLO_LENGTH (LR_sets_stack) != 0)
    {
      if (VLO_LENGTH (first_LR_sets_of_LR_core) != 0)
        {
          current_LR_set
            = ((IR_node_t *) VLO_BEGIN (first_LR_sets_of_LR_core))
              [VLO_LENGTH (first_LR_sets_of_LR_core) / sizeof (IR_node_t) - 1];
          VLO_SHORTEN (first_LR_sets_of_LR_core, sizeof (IR_node_t));
        }
      else
        {
          current_LR_set
            = ((IR_node_t *) VLO_BEGIN (LR_sets_stack))
              [VLO_LENGTH (LR_sets_stack) / sizeof (IR_node_t) - 1];
          VLO_SHORTEN (LR_sets_stack, sizeof (IR_node_t));
        }
      make_full_LR_set (current_LR_set, TRUE);
      /* Create new LR-sets achieved from the current set. */
      for (current_LR_situation = IR_LR_situation_list (current_LR_set);
           current_LR_situation != NULL;
           current_LR_situation
             = IR_next_LR_situation (current_LR_situation))
        if (IR_first_symbol_LR_situation (current_LR_situation)
            && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                               IR_NM_canonical_rule_end))
          {
            first_new_LR_situation
              = get_shifted_LR_situation_list (current_LR_situation);
            LR_core = find_LR_core (first_new_LR_situation);
            if (LR_core == NULL)
              {
                /* This is new LR-core. */
                LR_core
                  = IR_new_LR_core (NULL, IR_next_LR_core (IR_LR_core_list
                                                           (description)));
                add_LR_core_to_cyclic_list (LR_core);
                new_LR_core_flag = TRUE;
              }
            else
              new_LR_core_flag = FALSE;
            goto_LR_set = find_LR_set (LR_core, first_new_LR_situation);
            if (goto_LR_set == NULL)
              {
                /* This is new LR-set. */
                goto_LR_set
                  = IR_new_LR_set (first_new_LR_situation, LR_core, NULL);
                add_LR_set_to_cyclic_list (goto_LR_set, LR_core);
                if (new_LR_core_flag)
                  (void) insert_LR_core (LR_core);
                for (current_new_LR_situation = first_new_LR_situation;
                     current_new_LR_situation != NULL;
                     current_new_LR_situation
                       = IR_next_LR_situation (current_new_LR_situation))
                  IR_set_LR_set (current_new_LR_situation, goto_LR_set);
                /* Add new LR-set (it contain only important
                   LR-situation here) to the stack. */
                if (new_LR_core_flag)
                  VLO_ADD_MEMORY (first_LR_sets_of_LR_core, &goto_LR_set,
                                  sizeof (IR_node_t));
                else
                  VLO_ADD_MEMORY (LR_sets_stack, &goto_LR_set,
                                  sizeof (IR_node_t));
#ifndef NDEBUG
                assert (goto_LR_set == insert_LR_set (goto_LR_set));
#else
                (void) insert_LR_set (goto_LR_set);
#endif
              }
            else
              free_LR_situations_list (first_new_LR_situation);
            /* Set up field `goto_LR_set'. */
            IR_set_goto_LR_set (current_LR_situation, goto_LR_set);
          }
      count_LR_set_situations (current_LR_set, &original_LR_sets_number,
                               &important_original_LR_situations_number,
                               &all_original_LR_situations_number);
      free_unimportant_LR_situations (current_LR_set);
    }
  VLO_DELETE (LR_sets_stack);
  VLO_DELETE (first_LR_sets_of_LR_core);
  make_uncyclic_LR_set_and_core_lists ();
  original_LR_cores_number = 0;
  for (LR_core = IR_LR_core_list (description); LR_core != NULL;
       LR_core = IR_next_LR_core (LR_core))
    original_LR_cores_number++;
}



static void
merge_LR_sets (IR_node_t LR_core)
{
  context_t new_context;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;
  IR_node_t next_LR_situation;
  IR_node_t first_LR_set;
  IR_node_t current_first_LR_situation;
  IR_double_link_t LR_situation_reference;
  IR_double_link_t next_LR_situation_reference;

  if (IR_next_LR_set (IR_LR_set_list (LR_core)) == NULL)
    /* LR-core contains only one LR-set. */
    return;
  first_LR_set = IR_LR_set_list (LR_core);
  for (current_first_LR_situation = IR_LR_situation_list (first_LR_set);
       current_first_LR_situation != NULL;
       current_first_LR_situation
       = IR_next_LR_situation (current_first_LR_situation))
    {
      new_context = get_null_context ();
      context_copy (new_context, IR_context (current_first_LR_situation));
      IR_set_context (current_first_LR_situation, new_context);
    }
  for (current_LR_set = IR_next_LR_set (first_LR_set);
       current_LR_set != NULL;
       current_LR_set = IR_next_LR_set (current_LR_set))
    {
      delete_LR_set_from_table (current_LR_set);
      /* Change all references to the LR-set will be freed. */
      for (LR_situation_reference = IR__first_double_link (current_LR_set);
           LR_situation_reference != NULL;
           LR_situation_reference = next_LR_situation_reference)
        {
          next_LR_situation_reference
            = IR__next_double_link (LR_situation_reference);
          IR__set_double_link (LR_situation_reference, first_LR_set);
        }
      for (current_LR_situation = IR_LR_situation_list (current_LR_set),
           current_first_LR_situation = IR_LR_situation_list (first_LR_set);
           current_LR_situation != NULL;
           current_LR_situation = next_LR_situation,
           current_first_LR_situation
           = IR_next_LR_situation (current_first_LR_situation))
        {
          context_or (IR_context (current_first_LR_situation),
                      IR_context (current_LR_situation));
          next_LR_situation = IR_next_LR_situation (current_LR_situation),
          /* Context is not freed because it can be in the table and
             other LR-situations can refer to it. */
          free_LR_situation (current_LR_situation);
        }
    }
  insert_LR_set_contexts (first_LR_set);
  IR_set_next_LR_set (first_LR_set, NULL);
}

static IR_node_t
get_LR_situation (int LR_situation_number, IR_node_t LR_set)
{
  IR_node_t current_LR_situation;

  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    {
      if (LR_situation_number == 0)
        break;
      LR_situation_number--;
    }
  return current_LR_situation;
}

/* All contexts are defined in the following function. */

static int
new_conflicts_will_after_merging (IR_node_t LR_core)
{
  context_t conflicts_before_merging;
  context_t first_LALR_situation_context;
  context_t second_LALR_situation_context;
  IR_node_t current_LR_set;
  IR_node_t first_LR_situation;
  IR_node_t second_LR_situation;
  int first_LR_situation_number;
  int second_LR_situation_number;
  context_t context;

  if (IR_next_LR_set (IR_LR_set_list (LR_core)) == NULL)
    /* LR-core contains only one LR-set. */
    return FALSE;
  for (first_LR_situation_number = 0;; first_LR_situation_number++)
    {
      first_LR_situation = get_LR_situation (first_LR_situation_number,
                                             IR_LR_set_list (LR_core));
      if (first_LR_situation == NULL)
        break;
      if (IR_IS_OF_TYPE (IR_element_after_dot (first_LR_situation),
                         IR_NM_canonical_rule_end))
        for (second_LR_situation_number = 0;; second_LR_situation_number++)
          if (second_LR_situation_number != first_LR_situation_number)
            {
              if (get_LR_situation (second_LR_situation_number,
                                    IR_LR_set_list (LR_core)) == NULL)
                break;
              conflicts_before_merging = get_null_context ();
              first_LALR_situation_context = get_null_context ();
              second_LALR_situation_context = get_null_context ();
              assert (IR_LR_set_list (LR_core) != NULL);
              for (current_LR_set = IR_LR_set_list (LR_core);
                   current_LR_set != NULL;
                   current_LR_set = IR_next_LR_set (current_LR_set))
                {
                  first_LR_situation
                    = get_LR_situation (first_LR_situation_number,
                                        current_LR_set);
                  second_LR_situation
                    = get_LR_situation (second_LR_situation_number,
                                        current_LR_set);
                  if (IR_IS_OF_TYPE (IR_element_after_dot
                                     (second_LR_situation),
                                     IR_NM_canonical_rule_end))
                    context = IR_context (second_LR_situation);
                  else if (max_look_ahead_number > 1)
                    context = FIRST_of_tail (IR_element_after_dot
                                             (second_LR_situation),
                                             IR_context (second_LR_situation),
                                             max_look_ahead_number);
                  else
                    break;
                  context_or_of_and (conflicts_before_merging,
                                     IR_context (first_LR_situation), context);
                  context_or (first_LALR_situation_context,
                              IR_context (first_LR_situation));
                  context_or (second_LALR_situation_context, context);
                  if (!IR_IS_OF_TYPE (IR_element_after_dot
                                      (second_LR_situation),
                                      IR_NM_canonical_rule_end))
                    free_context (context);
                }
              if (current_LR_set == NULL)
                {
                  context_and (first_LALR_situation_context,
                               second_LALR_situation_context);
                  if (!context_eq (conflicts_before_merging,
                                   first_LALR_situation_context))
                    {
                      free_context (conflicts_before_merging);
                      free_context (first_LALR_situation_context);
                      free_context (second_LALR_situation_context);
                      return TRUE;
                    }
                }
              free_context (conflicts_before_merging);
              free_context (first_LALR_situation_context);
              free_context (second_LALR_situation_context);
            }
    }
  return FALSE;
}

void
make_LALR_optimization (void)
{
  IR_node_t current_LR_core;
  int first_pass_flag;
  int finish_flag;
  int goto_LR_sets_can_be_merged;
  IR_node_t current_LR_situation;
  IR_node_t goto_LR_core;
  IR_node_t current_LR_set;
  IR_node_t first_LR_set;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    if (IR_next_LR_set (IR_LR_set_list (current_LR_core)) != NULL)
      {
        /* LR-core contains more one LR-set. */
        for (current_LR_set = IR_LR_set_list (current_LR_core);
             current_LR_set != NULL;
             current_LR_set = IR_next_LR_set (current_LR_set))
          make_full_LR_set (current_LR_set, TRUE);
        IR_set_first_pass_flag
          (current_LR_core,
           !new_conflicts_will_after_merging (current_LR_core));
        for (current_LR_set = IR_LR_set_list (current_LR_core);
             current_LR_set != NULL;
             current_LR_set = IR_next_LR_set (current_LR_set))
          free_unimportant_LR_situations (current_LR_set);
      }
  first_pass_flag = TRUE;
  do
    {
      finish_flag = TRUE;
      for (current_LR_core = IR_LR_core_list (description);
           current_LR_core != NULL;
           current_LR_core = IR_next_LR_core (current_LR_core))
        if (IR_next_LR_set (IR_LR_set_list (current_LR_core)) != NULL)
          {
            if ((first_pass_flag && IR_first_pass_flag (current_LR_core))
                || (!first_pass_flag && IR_second_pass_flag (current_LR_core)))
              {
                goto_LR_sets_can_be_merged = TRUE;
                first_LR_set = IR_LR_set_list (current_LR_core);
                make_full_LR_set (first_LR_set, FALSE);
                set_up_goto_LR_set_fields (first_LR_set, TRUE);
                for (current_LR_situation
                     = IR_LR_situation_list (first_LR_set);
                     current_LR_situation != NULL;
                     current_LR_situation
                     = IR_next_LR_situation (current_LR_situation))
                  if (IR_first_symbol_LR_situation (current_LR_situation)
                      && IR_goto_LR_set (current_LR_situation) != NULL)
                    {
                      goto_LR_core
                        = IR_LR_core (IR_goto_LR_set (current_LR_situation));
                      if (IR_next_LR_set (IR_LR_set_list (goto_LR_core))
                          != NULL)
                        {
                          if (first_pass_flag)
                            goto_LR_sets_can_be_merged
                              = goto_LR_sets_can_be_merged
                                && IR_first_pass_flag (goto_LR_core);
                          else
                            goto_LR_sets_can_be_merged
                              = goto_LR_sets_can_be_merged
                                && IR_second_pass_flag (goto_LR_core);
                          if (!goto_LR_sets_can_be_merged)
                            break;
                        }
                    }
                free_unimportant_LR_situations (first_LR_set);
                finish_flag = finish_flag && goto_LR_sets_can_be_merged;
              }
            else
              goto_LR_sets_can_be_merged = FALSE;
            if (first_pass_flag)
              IR_set_second_pass_flag (current_LR_core,
                                       goto_LR_sets_can_be_merged);
            else
              IR_set_first_pass_flag (current_LR_core,
                                      goto_LR_sets_can_be_merged);
            
          }
      first_pass_flag = !first_pass_flag;
    }
  while (!finish_flag);
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    if (IR_next_LR_set (IR_LR_set_list (current_LR_core)) != NULL
        && ((first_pass_flag && IR_first_pass_flag (current_LR_core))
            || (!first_pass_flag && IR_second_pass_flag (current_LR_core))))
      merge_LR_sets (current_LR_core);
}



void
make_full_LR_sets (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      {
        make_full_LR_set (current_LR_set, TRUE);
        set_up_goto_LR_set_fields (current_LR_set, FALSE);
      }
}

