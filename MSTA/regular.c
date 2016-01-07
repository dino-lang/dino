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

/* Make equivalent reduces???? Case ouput regular arc action which
   corresponds shift and reducing by empty rule????  Additional
   criteria of regular arcs equivalence is absence of $n when n
   <=0???? */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* #ifdef HAVE_CONFIG_H */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "position.h"
#include "vlobject.h"
#include "ticker.h"
#include "common.h"
#include "ird.h"
#include "gen-comm.h"
#include "output.h"
#include "contexts.h"
#include "lr-sets.h"
#include "regular.h"

#include <assert.h>



/* The following function returns TRUE iff given LR-set has only one
   action which is reduce. */

static int
LR_set_has_only_one_action_reduce (IR_node_t LR_set,
                                   IR_node_t *fold_LR_situation)
{
  IR_node_t current_LR_situation;
  int reduce_situations_number;

  assert (IR_reachable_flag (LR_set));
  reduce_situations_number = 0;
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                       IR_NM_canonical_rule_end))
      {
        /* Except for axiom rule. */
        if (IR_canonical_rule (IR_element_after_dot (current_LR_situation))
            == IR_canonical_rule_list (description)
            || reduce_situations_number != 0)
          return FALSE;
        *fold_LR_situation = current_LR_situation;
        reduce_situations_number++;
      }
    else if (IR_IS_OF_TYPE
             (IR_element_itself (IR_element_after_dot (current_LR_situation)),
              IR_NM_single_term_definition))
      return FALSE;
  return TRUE;
}

/* The following variable value is TRUE iff look ahead context being
   checked contains token strings of one length. */

static int one_length_token_strings_flag;

static void
check_token_string_length (token_string_t token_string)
{
  if (one_length_token_strings_flag)
    one_length_token_strings_flag = token_string_length (token_string) <= 1;
}

/* The following function returns TRUE iff given LR-situation has
   look-ahead contexts of no more one length. */

static int
look_ahead_contexts_contain_only_tokens (IR_node_t LR_situation)
{
  one_length_token_strings_flag = TRUE;
  /* See comments in file `ird.sprut'. */
  assert (IR_first_symbol_LR_situation (LR_situation));
  if (IR_look_ahead_context (LR_situation) != NULL)
    process_context_token_strings
      (IR_look_ahead_context (LR_situation), check_token_string_length);
  return one_length_token_strings_flag;
}


/* The following variable is used for counting number of LR-set
   predecessors (see function `count_LR_set_predecessors'). */

static int LR_set_predecessors_number;

/* The following function is used through function
   `traverse_all_LR_set_predecessors' for counting number of LR-set
   predecessors of a LR-set. */

static int
count_LR_set_predecessors (IR_node_t LR_set)
{
  LR_set_predecessors_number++;
  return TRUE;
}

/* The following function is used through function
   `traverse_all_LR_set_predecessors' for marking LR-sets which
   will be in the state stack. */

static int
mark_LR_set_as_pushed (IR_node_t LR_set)
{
  IR_set_it_is_pushed_LR_set (LR_set, TRUE);
  assert (IR_reachable_flag (LR_set));
  return TRUE;
}

/* The following function is used for creation and adding rule list
   element with given canonical rule to given regular arc. */

static void
add_rule_list_element (IR_node_t regular_arc,
                       IR_node_t reduced_canonical_rule)
{
  if (IR_first_rule_list_element (regular_arc) == NULL)
    {
      IR_set_first_rule_list_element
        (regular_arc,
         IR_new_rule_list_element (reduced_canonical_rule, NULL));
      IR_set_last_rule_list_element
        (regular_arc, IR_first_rule_list_element (regular_arc));
    }
  else
    {
      IR_set_next_rule_list_element
        (IR_last_rule_list_element (regular_arc),
         IR_new_rule_list_element (reduced_canonical_rule, NULL));
      IR_set_last_rule_list_element
        (regular_arc,
         IR_next_rule_list_element (IR_last_rule_list_element
                                     (regular_arc)));
    }
}

/* The function is called from traverse function.  The function clears
   visit_flag of given LR-state. */

static int
clear_visit_flag (IR_node_t LR_set)
{
  IR_set_visit_flag (LR_set, FALSE);
  return TRUE;
}

/* The following variable is set up when a loop is recognized on
   traversed paths. */

static int path_loop_has_been_recognized;

/* The function is called from traverse function in order to recognize
   loop on traversed path. */

static int
visit_LR_set_for_loop_recognition (IR_node_t LR_set)
{
  if (IR_visit_flag (LR_set))
    {
      path_loop_has_been_recognized = TRUE;
      return FALSE;
    }
  IR_set_visit_flag (LR_set, TRUE);
  return TRUE;
}

/* The following variable value is LR set in which we check a reduce
   for loop in chain of reduces. */

static IR_node_t start_reduce_loop_recognition_LR_set;

/* The following variable value is LHS of current reduce in checked
   chain of reduces. */

static IR_node_t current_lhs_nonterminal_of_reduce_loop_recognition;

/* The following variable contains LR-sets whose visit_flag has been
   set up during checking loop in a chain reduces.  It may contain
   duplicate values for LR-sets containg reduce with empty rhs. */

static vlo_t visited_LR_sets_for_reduce_loop_recognition;

/* The following recursive function is used for recognition of loops
   in chain of reduces. */

static int
visit_LR_set_for_reduce_loop_recognition (IR_node_t LR_set)
{
  IR_node_t saved_current_lhs_nonterminal_of_reduce_loop_recognition;
  IR_node_t current_LR_situation;
  IR_node_t canonical_rule;
  int canonical_rule_right_hand_side_length;

  LR_set
    = goto_by_nonterminal (LR_set,
			   current_lhs_nonterminal_of_reduce_loop_recognition);
  path_loop_has_been_recognized
    = LR_set == start_reduce_loop_recognition_LR_set;
  if (path_loop_has_been_recognized)
    return FALSE;
  if (IR_visit_flag (LR_set))
    return TRUE;
  IR_set_visit_flag (LR_set, TRUE);
  VLO_ADD_MEMORY (visited_LR_sets_for_reduce_loop_recognition,
		  &LR_set, sizeof (LR_set));
  saved_current_lhs_nonterminal_of_reduce_loop_recognition
    = current_lhs_nonterminal_of_reduce_loop_recognition;
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
		       IR_NM_canonical_rule_end)
	/* Except for axiom rule. */
	&& (IR_canonical_rule (IR_element_after_dot (current_LR_situation))
	    != IR_canonical_rule_list (description)))
      {
	canonical_rule
	  = IR_canonical_rule (IR_element_after_dot (current_LR_situation));
	canonical_rule_right_hand_side_length
	  = canonical_rule_right_hand_side_prefix_length (canonical_rule,
							  NULL);
	current_lhs_nonterminal_of_reduce_loop_recognition
	  = IR_left_hand_side (canonical_rule);
	if (canonical_rule_right_hand_side_length != 0)
	  traverse_all_LR_set_predecessors
	    (LR_set, canonical_rule_right_hand_side_length,
	     visit_LR_set_for_reduce_loop_recognition);
	else
	  {
	    IR_set_visit_flag (LR_set, FALSE);
	    visit_LR_set_for_reduce_loop_recognition (LR_set);
	  }
	if (path_loop_has_been_recognized)
	  break;
      }
  current_lhs_nonterminal_of_reduce_loop_recognition
    = saved_current_lhs_nonterminal_of_reduce_loop_recognition;
  return !path_loop_has_been_recognized;
}

/* The following function returns TRUE if the all states on the given
   length paths from the predecessors of given state can be splitted
   in order to make unambigous target LR-set from each predecessor of
   given LR-state.  Criterium of posibility of splitting is the loop
   absence on the paths and absence of loop in a chain of reduces
   without shifts starting with given LR-set. */

static int
splitting_is_possible (IR_node_t reduce_LR_set, IR_node_t lhs_nonterminal,
		       int right_hand_side_length)
{
  IR_node_t *current_LR_set_ptr;

  assert (right_hand_side_length >= 1);
  traverse_all_LR_set_predecessor_paths
    (reduce_LR_set, right_hand_side_length,
     clear_visit_flag, right_hand_side_length);
  path_loop_has_been_recognized = FALSE;
  traverse_all_LR_set_predecessor_paths
    (reduce_LR_set, right_hand_side_length,
     visit_LR_set_for_loop_recognition, right_hand_side_length);
  if (!path_loop_has_been_recognized)
    {
      traverse_all_LR_set_predecessor_paths
	(reduce_LR_set, right_hand_side_length,
	 clear_visit_flag, right_hand_side_length);
      start_reduce_loop_recognition_LR_set = reduce_LR_set;
      current_lhs_nonterminal_of_reduce_loop_recognition = lhs_nonterminal;
      VLO_NULLIFY (visited_LR_sets_for_reduce_loop_recognition);
      traverse_all_LR_set_predecessors
	(reduce_LR_set, right_hand_side_length,
	 visit_LR_set_for_reduce_loop_recognition);
      for (current_LR_set_ptr
	     = VLO_BEGIN (visited_LR_sets_for_reduce_loop_recognition);
	   (char *) current_LR_set_ptr
	     <= (char *) VLO_END (visited_LR_sets_for_reduce_loop_recognition);
	   current_LR_set_ptr++)
	IR_set_visit_flag (*current_LR_set_ptr, FALSE);
    }
  return !path_loop_has_been_recognized;
}

/* The following function splits given LR-set in order to given LR-set
   have only one immediate LR-set predecessor.  Of course, it is
   possible because check it early by function
   `splitting_is_possible'.  The function is called by function
   `split_states_on_the_path' through function
   `reverse_traverse_all_LR_set_predecessor_paths'. */

static void
split_LR_set (IR_node_t LR_set)
{
  IR_node_t LR_situation_of_immediate_LR_set_predecessor;
  IR_node_t new_LR_set_node;
  IR_node_t current_LR_situation;
  IR_node_t previous_LR_situation;
  IR_double_link_t LR_situation_reference;
  vlo_t predecessors;
  int predecessors_number;
  int current_predecessor_number;

  VLO_CREATE (predecessors, 100);
  predecessors_number = 0;
  /* Collect predecessors (more accurately LR-situations) of given
     LR-set. */
  for (LR_situation_reference = IR__first_double_link (LR_set);
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
          VLO_ADD_MEMORY (predecessors,
                          &LR_situation_of_immediate_LR_set_predecessor,
                          sizeof (IR_node_t));
          predecessors_number++;
        }
    }
  /* Split given LR-set so that copies of the LR-set have only one
     predecessor. */
#ifndef NDEBUG
  if (debug_level >= 2 && predecessors_number > 1)
    {
      fprintf (stderr,
               "splitting LR-set %d times:\n", predecessors_number - 1);
      output_LR_set_situations (stderr, LR_set, "\t");
    }
#endif
  if (predecessors_number > 1 && IR_first_splitted_LR_set (LR_set) == NULL)
    {
      IR_set_first_splitted_LR_set (LR_set, LR_set);
      IR_set_next_splitted_LR_set (LR_set, LR_set);
    }
  for (current_predecessor_number = 1;
       current_predecessor_number < predecessors_number;
       current_predecessor_number++)
    {
      IR_set_splitted_LR_sets_number
        (description, IR_splitted_LR_sets_number (description) + 1);
      new_LR_set_node = IR_copy_node (LR_set);
      IR_set_next_splitted_LR_set (LR_set, new_LR_set_node);
      /* Copy LR-situations. */
      previous_LR_situation = NULL;
      assert (IR_LR_situation_list (LR_set) != NULL);
      for (current_LR_situation = IR_LR_situation_list (LR_set);
           current_LR_situation != NULL;
           current_LR_situation = IR_next_LR_situation (current_LR_situation))
        {
          if (previous_LR_situation != NULL)
            {
              IR_set_next_LR_situation
                (previous_LR_situation, IR_copy_node (current_LR_situation));
              previous_LR_situation
                = IR_next_LR_situation (previous_LR_situation);
            }
          else
            {
              previous_LR_situation = IR_copy_node (current_LR_situation);
              IR_set_LR_situation_list (new_LR_set_node,
                                        previous_LR_situation);
            }
          IR_set_LR_set (previous_LR_situation, new_LR_set_node);
        }
      LR_situation_of_immediate_LR_set_predecessor
        = (((IR_node_t *) VLO_BEGIN (predecessors))
           [current_predecessor_number]);
      IR_set_goto_LR_set (LR_situation_of_immediate_LR_set_predecessor,
                          new_LR_set_node);
      IR_set_next_LR_set (new_LR_set_node, IR_next_LR_set (LR_set));
      IR_set_next_LR_set (LR_set, new_LR_set_node);
    }
  VLO_DELETE (predecessors);
  return;
}

/* The following function splits LR-sets on given path so that each
   LR-set on the path will have only one predecessor. */

static void
split_states_on_the_path (IR_node_t reduce_LR_set, int right_hand_side_length)
{
  assert (right_hand_side_length >= 1);
  reverse_traverse_all_LR_set_predecessor_paths
    (reduce_LR_set, right_hand_side_length - 1,
     split_LR_set, right_hand_side_length - 1);
}

/* The following function aims to split LR-sets in order to maximum
   number of LR-sets which has reduce LR-situations have only one
   LR-set predecessor achieved as the result of reducing by reduce
   LR-situations. */

static void
split_LR_sets (void)
{
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;
  IR_node_t current_LR_situation;
  IR_node_t reduced_canonical_rule;
  int reduce_canonical_rule_right_hand_side_length;
  int split_flag;
  int iteration_number;

  iteration_number = 0;
  VLO_CREATE (visited_LR_sets_for_reduce_loop_recognition, 0);
  do
    {
      iteration_number++;
#ifndef NDEBUG
      if (debug_level >= 1)
        fprintf (stderr, "split iteration %d\n", iteration_number);
#endif
      split_flag = FALSE;
      for (current_LR_core = IR_LR_core_list (description);
           current_LR_core != NULL;
           current_LR_core = IR_next_LR_core (current_LR_core))
        for (current_LR_set = IR_LR_set_list (current_LR_core);
             current_LR_set != NULL;
             current_LR_set = IR_next_LR_set (current_LR_set))
          if (IR_reachable_flag (current_LR_set))
            for (current_LR_situation = IR_LR_situation_list (current_LR_set);
                 current_LR_situation != NULL;
                 current_LR_situation
                   = IR_next_LR_situation (current_LR_situation))
              if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                                 IR_NM_canonical_rule_end)
                  /* Except for axiom rule. */
                  && (IR_canonical_rule (IR_element_after_dot
                                         (current_LR_situation))
                      != IR_canonical_rule_list (description)))
                {
                  reduced_canonical_rule
                    = IR_canonical_rule (IR_element_after_dot
                                         (current_LR_situation));
                  reduce_canonical_rule_right_hand_side_length
                    = canonical_rule_right_hand_side_prefix_length
                      (reduced_canonical_rule, NULL);
                  if (reduce_canonical_rule_right_hand_side_length != 0)
                    {
                      /* Count the number of LR-states into which we can
                         achieve after reducing by given rule. */
                      LR_set_predecessors_number = 0;
                      traverse_all_LR_set_predecessors
                        (current_LR_set,
                         reduce_canonical_rule_right_hand_side_length,
                         count_LR_set_predecessors);
                      if (LR_set_predecessors_number != 1)
                        {
                          assert (LR_set_predecessors_number != 0);
                          if (splitting_is_possible
                              (current_LR_set,
			       IR_left_hand_side (reduced_canonical_rule),
                               reduce_canonical_rule_right_hand_side_length))
                            {
                              split_states_on_the_path
                                (current_LR_set,
                                 reduce_canonical_rule_right_hand_side_length);
                              split_flag = TRUE;
                            }
                        }
                    }
                }
    }
  while (split_flag);
  VLO_DELETE (visited_LR_sets_for_reduce_loop_recognition);
}

/* The following function aims to create regular arcs for maximum
   number of reduce LR-situations.  The function also mark all
   LR-states achieved by the reduce as pushed when it is not
   possible. */

static void
make_regular_arcs (void)
{
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;
  IR_node_t current_LR_situation;
  IR_node_t reduced_canonical_rule;
  IR_node_t regular_arc;
  IR_node_t to_LR_set;
  int reduced_canonical_rule_right_hand_side_length;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                             IR_NM_canonical_rule_end)
              /* Except for axiom rule. */
              && (IR_canonical_rule (IR_element_after_dot
                                     (current_LR_situation))
                  != IR_canonical_rule_list (description)))
            {
              reduced_canonical_rule
                = IR_canonical_rule (IR_element_after_dot
                                     (current_LR_situation));
              reduced_canonical_rule_right_hand_side_length
                = canonical_rule_right_hand_side_prefix_length
                  (reduced_canonical_rule, NULL);
              LR_set_predecessors_number = 0;
              traverse_all_LR_set_predecessors
                (current_LR_set,
                 reduced_canonical_rule_right_hand_side_length,
                 count_LR_set_predecessors);
              if (LR_set_predecessors_number == 1)
                {
                  to_LR_set
                    = goto_by_nonterminal
                      (get_the_single_LR_set_predecessor
                       (current_LR_set,
                        reduced_canonical_rule_right_hand_side_length),
                       IR_left_hand_side (reduced_canonical_rule));
#ifndef NDEBUG
                  if (debug_level >= 2)
                    {
                      fprintf (stderr, "make regular arc. from:\n");
                      output_LR_set_situations (stderr, current_LR_set, "\t");
                      fprintf (stderr, "to:\n");
                      output_LR_set_situations (stderr, to_LR_set, "\t");
                    }
#endif
                  regular_arc
                    = IR_new_regular_arc (current_LR_situation, to_LR_set,
                                          NULL);
                  add_rule_list_element (regular_arc,
                                         reduced_canonical_rule);
                  IR_set_corresponding_regular_arc (current_LR_situation,
                                                    regular_arc);
                }
            }
}

/* The following function changes arcs to reflect unconditional
   transition by sequential regular arcs. */

static void
merge_regular_arcs (void)
{
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;
  IR_node_t current_LR_situation;
  IR_node_t LR_situation;
  IR_node_t to_LR_set;
  IR_node_t fold_LR_situation;
  IR_node_t current_regular_arc;
  IR_node_t next_regular_arc;
  IR_node_t current_rule_list_element;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          {
            assert
              (IR_corresponding_regular_arc (current_LR_situation) == NULL
               || IR_IS_OF_TYPE (IR_element_after_dot
                                 (current_LR_situation),
                                 IR_NM_canonical_rule_end)
               || (IR_first_symbol_LR_situation (current_LR_situation)
                   && IR_goto_arc_has_been_removed (current_LR_situation)));
            if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                               IR_NM_canonical_rule_end)
                /* Except for axiom rule. */
                && (IR_canonical_rule (IR_element_after_dot
                                       (current_LR_situation))
                    != IR_canonical_rule_list (description)))
              for (current_regular_arc
                     = IR_corresponding_regular_arc (current_LR_situation),
                     fold_LR_situation = current_LR_situation;
                   IR_corresponding_regular_arc (fold_LR_situation) != NULL;)
                {
                  to_LR_set = IR_to_LR_set (current_regular_arc);
                  if (LR_set_has_only_one_action_reduce (to_LR_set,
                                                         &LR_situation)
                      && IR_corresponding_regular_arc (LR_situation) != NULL)
                    {
                      next_regular_arc
                        = IR_corresponding_regular_arc (LR_situation);
#ifndef NDEBUG
                      if (debug_level >= 2)
                        {
                          fprintf (stderr, "merging regular arc. source: ");
                          output_LR_situation (stderr, current_LR_situation,
                                               "\t", TRUE);
                          fprintf (stderr, " its target LR-set:\n");
                          output_LR_set_situations
                            (stderr, IR_to_LR_set (current_regular_arc), "\t");
                          fprintf (stderr, "  new targer LR-set:\n");
                          output_LR_set_situations
                            (stderr, IR_to_LR_set (next_regular_arc), "\t");
                        }
#endif
                      IR_set_to_LR_set (current_regular_arc,
                                        IR_to_LR_set (next_regular_arc));
                      for (current_rule_list_element
                             = IR_first_rule_list_element (next_regular_arc);
                           current_rule_list_element != NULL;
                           current_rule_list_element
                             = IR_next_rule_list_element
                             (current_rule_list_element))
                        add_rule_list_element
                          (current_regular_arc,
                           IR_canonical_rule (current_rule_list_element));
                      fold_LR_situation = LR_situation;
                    }
                  else
                    break;
                }
          }
}

/* The following function creates regular arcs which reflect shifting
   by terminal and subsequent transition by regular arcs. */

static void
merge_shifts_and_regular_arcs (void)
{
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;
  IR_node_t current_LR_situation;
  IR_node_t LR_situation;
  IR_node_t goto_LR_set;
  IR_node_t regular_arc;
  IR_node_t new_regular_arc;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          if (IR_first_symbol_LR_situation (current_LR_situation)
              && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                                 IR_NM_canonical_rule_end)
              && !IR_goto_arc_has_been_removed (current_LR_situation)
              && IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                                   (current_LR_situation)),
                                IR_NM_single_term_definition)
              /* Except for axiom rule. */
              && (IR_canonical_rule (IR_element_after_dot
                                     (current_LR_situation))
                  != IR_canonical_rule_list (description))
              /* Except for shift on error. */
              && (IR_element_itself (IR_element_after_dot
                                     (current_LR_situation))
                  != error_single_definition)
              &&
              look_ahead_contexts_contain_only_tokens (current_LR_situation))
            {
              goto_LR_set = IR_goto_LR_set (current_LR_situation);
              if (LR_set_has_only_one_action_reduce (goto_LR_set,
                                                     &LR_situation)
                  && IR_corresponding_regular_arc (LR_situation) != NULL)
                {
#ifndef NDEBUG
                  if (debug_level >= 2)
                    {
                      fprintf (stderr, "merging shift: ");
                      output_LR_situation (stderr, current_LR_situation,
                                           "\t", TRUE);
                      fprintf (stderr, "  with reduce: ");
                      output_LR_situation (stderr, LR_situation, "\t", TRUE);
                    }
#endif
                  regular_arc = IR_corresponding_regular_arc (LR_situation);
                  IR_set_goto_arc_has_been_removed (current_LR_situation,
                                                    TRUE);
                  new_regular_arc
                    = IR_new_regular_arc
                      (current_LR_situation, IR_to_LR_set (regular_arc),
                       IR_element_itself (IR_element_after_dot
                                          (current_LR_situation)));
                  IR_set_corresponding_regular_arc (current_LR_situation,
                                                    new_regular_arc);
                  IR_set_first_rule_list_element
                    (new_regular_arc,
                     IR_first_rule_list_element (regular_arc));
                  IR_set_last_rule_list_element
                    (new_regular_arc, IR_last_rule_list_element (regular_arc));
                }
            }
}

/* This is the major function which transforms LR-graph (creates
   regular arc). */

static void
transform_LR_graph (void)
{
  ticker_t temp_ticker;

  temp_ticker = create_ticker ();
  if (split_lr_sets_flag)
    {
      traverse_cache_off ();
      split_LR_sets ();
      traverse_cache_on ();
      if (time_flag)
        fprintf (stderr,
                 "      splitting LR-sets for regular optimization -- %ssec\n",
                 active_time_string (temp_ticker));
    }
  make_regular_arcs ();
  merge_regular_arcs ();
  merge_shifts_and_regular_arcs ();
}



static int
mark_LR_set_with_used_attribute (IR_node_t LR_set)
{
  IR_set_attribute_is_used (LR_set, TRUE);
  return TRUE;
}

static int
mark_all_target_as_pushed (IR_node_t LR_set, IR_node_t canonical_rule,
                           int attribute_flag);

/* The following variable value is LR-set which contains folding
   canonical rule being processed in function
   `mark_LR_set_corresponding_to_attribute'. */

static IR_node_t current_LR_set_containing_canonical_rule_folding;

/* Remember that correctness of attributes has been already
   checked. */

static void
mark_LR_set_corresponding_to_attribute
  (IR_node_t canonical_rule, position_t attribute_position,
   const char *tag_name, const char *attribute_name)
{
  IR_node_t current_right_hand_side_element;
  int attribute_number;
  IR_node_t original_canonical_rule;
  IR_node_t bound_right_hand_side_element;
  int predecessors_number;

  if (strcmp (attribute_name, "$") == 0)
    mark_all_target_as_pushed
      (current_LR_set_containing_canonical_rule_folding, canonical_rule, TRUE);
  else
    {
      bound_right_hand_side_element
        = IR_original_code_insertion_place (canonical_rule);
      if (bound_right_hand_side_element != NULL)
        original_canonical_rule
          = IR_canonical_rule (bound_right_hand_side_element);
      else
        original_canonical_rule = canonical_rule;
      predecessors_number = 0;
      for (current_right_hand_side_element
             = IR_right_hand_side (original_canonical_rule);
           !IR_IS_OF_TYPE (current_right_hand_side_element,
                           IR_NM_canonical_rule_end)
           && current_right_hand_side_element != bound_right_hand_side_element;
           current_right_hand_side_element
             = IR_next_right_hand_side_element
               (current_right_hand_side_element))
        predecessors_number++;
      if (isdigit (*attribute_name) || *attribute_name == '-')
        attribute_number = atoi (attribute_name);
      else
        attribute_number
          = attribute_name_to_attribute_number (attribute_name,
                                                original_canonical_rule,
                                                bound_right_hand_side_element);
      traverse_all_LR_set_predecessors
        (current_LR_set_containing_canonical_rule_folding,
         predecessors_number - attribute_number,
         mark_LR_set_with_used_attribute);
    }
}

static void
dummy_processing_char (char ch)
{
}

/* The usage means any occurence (except as `$$') of attribute in a
   action. */

static void
mark_LR_sets_with_used_attribute (void)
{
  IR_node_t current_LR_situation;
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          /* For given processing, We can ignore presence regular arc
             for the fold LR-situation here. */
          if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                             IR_NM_canonical_rule_end)
              /* Except for axiom rule. */
              && (IR_canonical_rule (IR_element_after_dot
                                     (current_LR_situation))
                  != IR_canonical_rule_list (description))
              && (IR_look_ahead_context (current_LR_situation) == NULL
                  || !it_is_zero_context (IR_look_ahead_context
                                          (current_LR_situation))))
            {
              current_LR_set_containing_canonical_rule_folding
                = current_LR_set;
              process_canonical_rule_action
                (IR_canonical_rule (IR_element_after_dot
                                    (current_LR_situation)),
                 dummy_processing_char,
                 mark_LR_set_corresponding_to_attribute);
            }
}

static void mark_used_LR_sets_and_nonterm_goto_arc (IR_node_t LR_set);

/* ??? */
static IR_node_t reduced_single_nonterm_definition;

static int
process_nonterm_goto_arc (IR_node_t LR_set)
{
  IR_node_t current_LR_situation;
  
#ifndef NDEBUG
  if (debug_level >= 2)
    {
      fprintf (stderr, "mark used nonterm `");
      output_single_definition (stderr, reduced_single_nonterm_definition);
      fprintf (stderr, "' goto arcs in LR-set:\n");
      output_LR_set_situations (stderr, LR_set, "\t");
    }
#endif
  mark_used_LR_sets_and_nonterm_goto_arc (LR_set);
  current_LR_situation
    = find_goto_LR_situation (LR_set, reduced_single_nonterm_definition);
  assert (current_LR_situation != NULL);
  IR_set_goto_arc_has_been_removed (current_LR_situation, FALSE);
  mark_used_LR_sets_and_nonterm_goto_arc
    (IR_goto_LR_set (current_LR_situation));
  return TRUE;
}

static void
mark_used_LR_sets_and_nonterm_goto_arc (IR_node_t LR_set)
{
  IR_node_t current_LR_situation;
  IR_node_t canonical_rule;
  IR_node_t saved_reduced_single_nonterm_definition;

  if (IR_reachable_flag (LR_set))
    return;
#ifndef NDEBUG
  if (debug_level >= 2)
    {
      fprintf (stderr, "mark LR set as reachable\n");
      output_LR_set_situations (stderr, LR_set, "\t");
    }
#endif
  IR_set_reachable_flag (LR_set, TRUE);
  /* Process basic LR-arcs. */
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    {
      if (IR_corresponding_regular_arc (current_LR_situation) != NULL)
        mark_used_LR_sets_and_nonterm_goto_arc
          (IR_to_LR_set (IR_corresponding_regular_arc (current_LR_situation)));
      if (IR_first_symbol_LR_situation (current_LR_situation)
          && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                             IR_NM_canonical_rule_end)
          && !IR_goto_arc_has_been_removed (current_LR_situation)
          && IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                               (current_LR_situation)),
                            IR_NM_single_term_definition))
        mark_used_LR_sets_and_nonterm_goto_arc (IR_goto_LR_set
                                                (current_LR_situation));
    }
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                       IR_NM_canonical_rule_end)
        /* Except for axiom rule. */
        && (IR_canonical_rule (IR_element_after_dot (current_LR_situation))
            != IR_canonical_rule_list (description))
        && IR_corresponding_regular_arc (current_LR_situation) == NULL
        && (IR_look_ahead_context (current_LR_situation) == NULL
            || !it_is_zero_context (IR_look_ahead_context
                                    (current_LR_situation))))
      {
        /* This is reduce. */
        canonical_rule
          = IR_canonical_rule (IR_element_after_dot (current_LR_situation));
        saved_reduced_single_nonterm_definition
          = reduced_single_nonterm_definition;
        reduced_single_nonterm_definition
          = IR_left_hand_side (canonical_rule);
#ifndef NDEBUG
        if (debug_level >= 2)
          {
            fprintf
              (stderr,
               "  process reduce LR-set for marking used nonterm goto arc:\n");
            output_LR_set_situations (stderr, LR_set, "\t");
          }
#endif
        traverse_all_LR_set_predecessors
          (LR_set,
           canonical_rule_right_hand_side_prefix_length (canonical_rule, NULL),
           process_nonterm_goto_arc);
        reduced_single_nonterm_definition
          = saved_reduced_single_nonterm_definition;
      }
}

static void
remove_unused_nonterminal_LR_arcs_and_mark_unreachable_LR_sets (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;

  /* Mark all LR-set as ureachable and all nonterminal arcs as removed. */
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      {
        IR_set_reachable_flag (current_LR_set, FALSE);
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
             = IR_next_LR_situation (current_LR_situation))
          if (IR_first_symbol_LR_situation (current_LR_situation)
              && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                                 IR_NM_canonical_rule_end)
              && IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                                   (current_LR_situation)),
                                IR_NM_single_nonterm_definition))
            IR_set_goto_arc_has_been_removed (current_LR_situation, TRUE);
      }
  mark_used_LR_sets_and_nonterm_goto_arc (IR_LR_set_list (IR_LR_core_list
                                                          (description)));
}

static void
mark_semantic_pushed_LR_sets (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;
  IR_node_t symbol_definition;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          if (characteristic_symbol_of_LR_set (current_LR_set)
              == error_single_definition
              || IR_back_tracking_flag (current_LR_set))
            {
              /* Parser algorithm believes that all error or
                 backtracking states are pushed and are using
                 attribute. */
              IR_set_it_is_pushed_LR_set (current_LR_set, TRUE);
              IR_set_attribute_is_used (current_LR_set, TRUE);
            }
          for (current_LR_situation = IR_LR_situation_list (current_LR_set);
               current_LR_situation != NULL;
               current_LR_situation
                 = IR_next_LR_situation (current_LR_situation))
            if (!IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                                IR_NM_canonical_rule_end))
              {
                if (IR_first_symbol_LR_situation (current_LR_situation))
                  {
                    symbol_definition
                      = IR_element_itself (IR_element_after_dot 
                                           (current_LR_situation));
                    if (symbol_definition == error_single_definition)
		      {
			/* It is necessary for error recovery. */
			IR_set_it_is_pushed_LR_set (current_LR_set, TRUE);
			IR_set_it_is_errored_LR_set (current_LR_set, TRUE);
		      }
                  }
              }
            else if (IR_canonical_rule (IR_element_after_dot
                                        (current_LR_situation))
                     != IR_canonical_rule_list (description)
                     && (IR_corresponding_regular_arc (current_LR_situation)
                         == NULL)
                     && (IR_look_ahead_context (current_LR_situation) == NULL
                         || !it_is_zero_context (IR_look_ahead_context
                                                 (current_LR_situation))))
              {
                traverse_all_LR_set_predecessors
                  (current_LR_set,
                   canonical_rule_right_hand_side_prefix_length
                   (IR_canonical_rule (IR_element_after_dot
                                       (current_LR_situation)), NULL),
                   mark_LR_set_as_pushed);
              }
        }
}

static void
determine_pushed_scale (IR_node_t LR_set, int path_length,
                        char *pushed_scale, int attribute_flag)
{
  IR_node_t LR_situation_of_immediate_LR_set_predecessor;
  IR_double_link_t LR_situation_reference;

  assert (path_length >= 0);
  if ((IR_it_is_pushed_LR_set (LR_set) && !attribute_flag)
      || (IR_attribute_is_used (LR_set) && attribute_flag))
    *pushed_scale = TRUE;
  if (path_length != 0)
    {
      /* Process all immediate LR-predecessors of given LR-set. */
      for (LR_situation_reference = IR__first_double_link (LR_set);
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
              determine_pushed_scale
                (IR_LR_set (LR_situation_of_immediate_LR_set_predecessor),
                 path_length - 1, pushed_scale + 1, attribute_flag);
            }
        }
    }
}

static int
mark_all_LR_sets_on_scale_as_pushed (IR_node_t LR_set, int path_length,
                                     char *pushed_scale, int attribute_flag)
{
  IR_node_t LR_situation_of_immediate_LR_set_predecessor;
  IR_double_link_t LR_situation_reference;
  int new_pushed_flag;

  assert (path_length >= 0);
  new_pushed_flag = FALSE;
  if (*pushed_scale)
    {
#ifndef NDEBUG
      if (debug_level >= 2)
        {
          fprintf (stderr, (attribute_flag
                            ? "mark attribute of LR-set as pushed:\n"
                            : "mark LR-set as pushed:\n"));
          output_LR_set_situations (stderr, LR_set, "\t");
        }
#endif
      if (attribute_flag)
        {
          if (!IR_attribute_is_used (LR_set))
            {
              new_pushed_flag = TRUE;
              IR_set_attribute_is_used (LR_set, TRUE);
            }
        }
      else if (!IR_it_is_pushed_LR_set (LR_set))
        {
          new_pushed_flag = TRUE;
          IR_set_it_is_pushed_LR_set (LR_set, TRUE);
        }
    }
  if (path_length != 0)
    {
      /* Process all immediate LR-predecessors of given LR-set. */
      for (LR_situation_reference = IR__first_double_link (LR_set);
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
              new_pushed_flag
                = (mark_all_LR_sets_on_scale_as_pushed
                   (IR_LR_set (LR_situation_of_immediate_LR_set_predecessor),
                    path_length - 1, pushed_scale + 1, attribute_flag))
                  || new_pushed_flag;
            }
        }
    }
  return new_pushed_flag;
}

static int target_attribute_flag;
static int there_is_a_pushed_target_flag;
static IR_node_t target_nonterminal;

static int
check_a_pushed_target (IR_node_t LR_set)
{
  IR_node_t target;

  assert (target_attribute_flag || IR_reachable_flag (LR_set));
  assert (target_attribute_flag || IR_it_is_pushed_LR_set (LR_set));
  if (!there_is_a_pushed_target_flag)
    {
      target = goto_by_nonterminal (LR_set, target_nonterminal);
      assert (target_attribute_flag || IR_reachable_flag (target));
      if (target_attribute_flag)
        there_is_a_pushed_target_flag = IR_attribute_is_used (target);
      else
        there_is_a_pushed_target_flag = IR_it_is_pushed_LR_set (target);
    }
  return TRUE;
}

static int
there_is_a_pushed_target (IR_node_t LR_set, IR_node_t canonical_rule,
                          int attribute_flag)
{
  target_attribute_flag = attribute_flag;
  there_is_a_pushed_target_flag = FALSE;
  target_nonterminal = IR_left_hand_side (canonical_rule);
  traverse_all_LR_set_predecessors
    (LR_set,
     canonical_rule_right_hand_side_prefix_length (canonical_rule, NULL),
     check_a_pushed_target);
  return there_is_a_pushed_target_flag;
}

static int
set_a_pushed_target (IR_node_t LR_set)
{
  IR_node_t target;

  assert (target_attribute_flag || IR_reachable_flag (LR_set));
  assert (target_attribute_flag || IR_it_is_pushed_LR_set (LR_set));
  target = goto_by_nonterminal (LR_set, target_nonterminal);
  assert (target_attribute_flag || IR_reachable_flag (LR_set));
  if (target_attribute_flag)
    {
      if (!IR_attribute_is_used (target))
        {
          there_is_a_pushed_target_flag = TRUE;
          IR_set_attribute_is_used (target, TRUE);
        }
    }
  else if (!IR_it_is_pushed_LR_set (target))
    {
      there_is_a_pushed_target_flag = TRUE;
      IR_set_it_is_pushed_LR_set (target, TRUE);
    }
  return TRUE;
}

static int
mark_all_target_as_pushed (IR_node_t LR_set, IR_node_t canonical_rule,
                           int attribute_flag)
{
  target_attribute_flag = attribute_flag;
  there_is_a_pushed_target_flag = FALSE;
  target_nonterminal = IR_left_hand_side (canonical_rule);
  traverse_all_LR_set_predecessors
    (LR_set,
     canonical_rule_right_hand_side_prefix_length (canonical_rule, NULL),
     set_a_pushed_target);
  return there_is_a_pushed_target_flag;
}

static void
mark_concordance_pushed_LR_sets_or_attributes (int attribute_flag)
{
  int new_states_were_marked;
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;
  IR_node_t current_symbol_LR_situation;
  IR_node_t canonical_rule;
  int right_hand_side_length;
  char *char_ptr;
  int popped_number;
  vlo_t pushed_scale;

  VLO_CREATE (pushed_scale, 0);
  do
    {
      new_states_were_marked = FALSE;
      for (current_LR_core = IR_LR_core_list (description);
           current_LR_core != NULL;
           current_LR_core = IR_next_LR_core (current_LR_core))
        for (current_LR_set = IR_LR_set_list (current_LR_core);
             current_LR_set != NULL;
             current_LR_set = IR_next_LR_set (current_LR_set))
          if (IR_reachable_flag (current_LR_set) || attribute_flag)
            {
              for (current_LR_situation
                     = IR_LR_situation_list (current_LR_set);
                   current_LR_situation != NULL;
                   current_LR_situation
                     = IR_next_LR_situation (current_LR_situation))
                if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                                   IR_NM_canonical_rule_end)
                    /* Remember that LR-situation with regular arc has
                       only one predecessor achived by the reduce.
                       Therefore such LR-situations are not
                       processed. */
                    && (IR_corresponding_regular_arc (current_LR_situation)
                        == NULL)
                    && (IR_canonical_rule (IR_element_after_dot
                                           (current_LR_situation))
                        != IR_canonical_rule_list (description)))
                  break;
              if (current_LR_situation == NULL)
                /* There are not reduced LR-situations. */
                break;
              assert (IR_first_symbol_LR_situation (current_LR_situation));
              for (current_symbol_LR_situation = current_LR_situation;
                   current_symbol_LR_situation != NULL;
                   current_symbol_LR_situation
                     = IR_next_LR_situation (current_symbol_LR_situation))
                if (IR_IS_OF_TYPE (IR_element_after_dot
                                   (current_symbol_LR_situation),
                                   IR_NM_canonical_rule_end)
                    && (IR_corresponding_regular_arc
                        (current_symbol_LR_situation) == NULL)
                    &&
                    (IR_look_ahead_context (current_symbol_LR_situation)
                     == NULL
                     || !it_is_zero_context (IR_look_ahead_context
                                             (current_symbol_LR_situation))))
                  {
                    canonical_rule
                      = IR_canonical_rule (IR_element_after_dot
                                           (current_symbol_LR_situation));
                    right_hand_side_length
                      = canonical_rule_right_hand_side_prefix_length
                        (canonical_rule, NULL);
                    VLO_NULLIFY (pushed_scale);
                    VLO_EXPAND (pushed_scale,
                                (right_hand_side_length + 1) * sizeof (char));
                    for (char_ptr = (char *) VLO_BEGIN (pushed_scale);
                         char_ptr <= (char *) VLO_END (pushed_scale);
                         char_ptr++)
                      *char_ptr = FALSE;
                    determine_pushed_scale
                      (current_LR_set, right_hand_side_length,
                       (char *) VLO_BEGIN (pushed_scale), attribute_flag);
                    for (popped_number = 0,
                         char_ptr = (char *) VLO_BEGIN (pushed_scale);
                         char_ptr < (char *) VLO_END (pushed_scale);
                         char_ptr++)
                      if (*char_ptr)
                        popped_number++;
                    if (attribute_flag)
                      IR_set_popped_attributes_number
                        (current_symbol_LR_situation, popped_number);
                    else
                      IR_set_popped_LR_sets_number
                        (current_symbol_LR_situation, popped_number);
                    if (mark_all_LR_sets_on_scale_as_pushed
                        (current_LR_set, right_hand_side_length,
                         (char *) VLO_BEGIN (pushed_scale), attribute_flag))
                      new_states_were_marked = TRUE;
                    if (there_is_a_pushed_target (current_LR_set,
                                                  canonical_rule,
                                                  attribute_flag)
                        && mark_all_target_as_pushed (current_LR_set,
                                                      canonical_rule,
                                                      attribute_flag))
                      new_states_were_marked = TRUE;
                  }
            }
    }
  while (new_states_were_marked);
  VLO_DELETE (pushed_scale);
}

static void
set_up_popped_LR_sets_and_attributes_for_regular_arcs (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;
  IR_node_t current_regular_arc;
  IR_node_t current_rule_list_element;
  IR_node_t canonical_rule;
  IR_node_t LR_set_predecessor;
  IR_node_t current_reduce_LR_set;
  IR_node_t *current_rule_list_element_or_regular_arc_ptr;
  int rule_length;
  int states_stack_displacement;
  int attributes_stack_displacement;
  int max_states_stack_displacement;
  int max_attributes_stack_displacement;
  int duplicated_actions;
  int pop_states_number;
  vlo_t states_stack;

  VLO_CREATE (states_stack, 0);
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          if (IR_corresponding_regular_arc (current_LR_situation) != NULL)
            {
              max_states_stack_displacement = 0;
              states_stack_displacement = 0;
              max_attributes_stack_displacement = 0;
              attributes_stack_displacement = 0;
              current_regular_arc
                = IR_corresponding_regular_arc (current_LR_situation);
              VLO_NULLIFY (states_stack);
              if (IR_terminal_marking_arc (current_regular_arc) != NULL)
                {
                  current_reduce_LR_set
                    = IR_goto_LR_set (current_LR_situation);
                  if (IR_it_is_pushed_LR_set (current_reduce_LR_set))
                    {
                      states_stack_displacement++;
		      if (IR_first_rule_list_element (current_regular_arc)
			  == NULL)
			max_states_stack_displacement++;
                      VLO_ADD_MEMORY (states_stack, &current_regular_arc,
                                      sizeof (IR_node_t));
                    }
                  if (IR_attribute_is_used (current_reduce_LR_set))
                    {
                      attributes_stack_displacement++;
                      max_attributes_stack_displacement++;
                    }
                }
              else
                current_reduce_LR_set = current_LR_set;
              for (current_rule_list_element
                     = IR_first_rule_list_element (current_regular_arc);
                   current_rule_list_element != NULL;
                   current_rule_list_element
                     = IR_next_rule_list_element (current_rule_list_element))
                {
                  canonical_rule
                    = IR_canonical_rule (current_rule_list_element);
                  if (IR_action (canonical_rule) != NULL)
                    IR_set_output_action_number
                      (canonical_rule,
                       IR_output_action_number (canonical_rule) + 1);
                  rule_length
                    = canonical_rule_right_hand_side_prefix_length
                      (canonical_rule, NULL);
                  pop_states_number
                    = pushed_LR_sets_or_attributes_number_on_path
                       (current_reduce_LR_set, rule_length, FALSE);
                  states_stack_displacement -= pop_states_number;
                  if (pop_states_number * sizeof (IR_node_t)
                      >= VLO_LENGTH (states_stack))
                    VLO_NULLIFY (states_stack);
                  else
                    VLO_SHORTEN (states_stack,
                                 pop_states_number * sizeof (IR_node_t));
                  attributes_stack_displacement
                    -= pushed_LR_sets_or_attributes_number_on_path
                       (current_reduce_LR_set, rule_length, TRUE);
#ifndef NDEBUG
                  if (debug_level >= 2)
                    {
                      fprintf
                        (stderr,
                         "evaluate popped LR-states and attributes numbers for LR-set:\n");
                      output_LR_set_situations (stderr, current_reduce_LR_set,
                                                "\t");
                    }
#endif
                  LR_set_predecessor
                    = get_the_single_LR_set_predecessor (current_reduce_LR_set,
                                                         rule_length);
#ifndef NDEBUG
                  if (debug_level >= 2)
                    {
                      fprintf (stderr, "   its target LR-set:\n");
                      output_LR_set_situations (stderr, LR_set_predecessor,
                                                "\t");
                    }
#endif
                  current_reduce_LR_set
                    = goto_by_nonterminal
                      (LR_set_predecessor, IR_left_hand_side (canonical_rule));
                  if (IR_it_is_pushed_LR_set (current_reduce_LR_set))
                    {
                      states_stack_displacement++;
                      VLO_ADD_MEMORY (states_stack, &current_rule_list_element,
                                      sizeof (IR_node_t));
                    }
		  if (states_stack_displacement
		      > max_states_stack_displacement
		      && (IR_next_rule_list_element
			  (current_rule_list_element) == NULL))
		    max_states_stack_displacement
		      = states_stack_displacement;
                  if (IR_attribute_is_used (current_reduce_LR_set))
                    {
                      attributes_stack_displacement++;
                      if (attributes_stack_displacement
                          > max_attributes_stack_displacement)
                        max_attributes_stack_displacement
                          = attributes_stack_displacement;
                    }
                }
              for (current_rule_list_element_or_regular_arc_ptr
                     = VLO_BEGIN (states_stack);
                   (char *) current_rule_list_element_or_regular_arc_ptr
                     <= (char *) VLO_END (states_stack);
                   current_rule_list_element_or_regular_arc_ptr++)
                IR_set_result_LR_set_will_be_on_the_stack
                  (*current_rule_list_element_or_regular_arc_ptr, TRUE);
              IR_set_regular_arc_popped_LR_sets_number
                (current_regular_arc, -states_stack_displacement);
              IR_set_regular_arc_popped_attributes_number
                (current_regular_arc, -attributes_stack_displacement);
              IR_set_max_states_stack_increment
                (current_regular_arc, max_states_stack_displacement);
              IR_set_max_attributes_stack_increment
                (current_regular_arc, max_attributes_stack_displacement);
            }
  VLO_DELETE (states_stack);
  duplicated_actions = 0;
  for (canonical_rule = IR_canonical_rule_list (description);
       canonical_rule != NULL;
       canonical_rule = IR_next_canonical_rule (canonical_rule))
    if (IR_action (canonical_rule) != NULL
        && IR_output_action_number (canonical_rule) != 0)
      duplicated_actions += IR_output_action_number (canonical_rule) - 1;
  IR_set_duplicated_actions (description, duplicated_actions);
}

static void
set_up_right_hand_side_used_attributes_numbers (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_reduce_LR_set;
  IR_node_t current_LR_situation;
  IR_node_t current_regular_arc;
  IR_node_t current_rule_list_element;
  IR_node_t canonical_rule;
  IR_node_t LR_set_predecessor;
  int rule_length;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          if (IR_corresponding_regular_arc (current_LR_situation) != NULL)
            {
              current_regular_arc
                = IR_corresponding_regular_arc (current_LR_situation);
              if (IR_terminal_marking_arc (current_regular_arc) != NULL)
                current_reduce_LR_set = IR_goto_LR_set (current_LR_situation);
              else
                current_reduce_LR_set = current_LR_set;
              for (current_rule_list_element
                     = IR_first_rule_list_element (current_regular_arc);
                   current_rule_list_element != NULL;
                   current_rule_list_element
                     = IR_next_rule_list_element (current_rule_list_element))
                {
                  canonical_rule
                    = IR_canonical_rule (current_rule_list_element);
                  rule_length
                    = canonical_rule_right_hand_side_prefix_length
                      (canonical_rule, NULL);
                  IR_set_right_hand_side_used_attributes_number
                    (current_rule_list_element,
                     pushed_LR_sets_or_attributes_number_on_path
                     (current_reduce_LR_set, rule_length, TRUE));
#ifndef NDEBUG
                  if (debug_level >= 2)
                    {
                      fprintf
                        (stderr,
                         "evaluate pushed attributes number for LR-set:\n");
                      output_LR_set_situations (stderr, current_reduce_LR_set,
                                                "\t");
                    }
#endif
                  LR_set_predecessor
                    = get_the_single_LR_set_predecessor (current_reduce_LR_set,
                                                         rule_length);
#ifndef NDEBUG
                  if (debug_level >= 2)
                    {
                      fprintf (stderr, "   its target LR-set:\n");
                      output_LR_set_situations (stderr, LR_set_predecessor,
                                                "\t");
                    }
#endif
                  current_reduce_LR_set
                    = goto_by_nonterminal
                      (LR_set_predecessor, IR_left_hand_side (canonical_rule));
                  IR_set_lhs_nonterm_attribute_is_used
                    (current_rule_list_element,
                     IR_attribute_is_used (current_reduce_LR_set));
                }
            }
}

int
regular_arcs_are_equivalent (IR_node_t arc1, IR_node_t arc2)
{
  IR_node_t current_LR_set1;
  IR_node_t current_LR_set2;
  IR_node_t LR_set_predecessor1;
  IR_node_t LR_set_predecessor2;
  IR_node_t current_element1;
  IR_node_t current_element2;
  IR_node_t canonical_rule1;
  IR_node_t canonical_rule2;
  IR_node_t next_LR_set;
  IR_node_t bound_right_hand_side_element;
  IR_node_t original_canonical_rule;
  IR_node_t *current_LR_set_ptr1;
  IR_node_t *current_LR_set_ptr2;
  int stack_displacement1;
  int stack_displacement2;
  int rule_length1;
  int rule_length2;
  int result;
  int i;
  vlo_t scale1;
  vlo_t scale2;
  vlo_t states_stack1;
  vlo_t states_stack2;

  if (IR_to_LR_set (arc1) != IR_to_LR_set (arc2)
      || (IR_terminal_marking_arc (arc1) == NULL
          && IR_terminal_marking_arc (arc2) != NULL)
      || (IR_terminal_marking_arc (arc1) != NULL
          && IR_terminal_marking_arc (arc2) == NULL)
      || (IR_regular_arc_popped_LR_sets_number (arc1)
          != IR_regular_arc_popped_LR_sets_number (arc2))
      || (IR_max_attributes_stack_increment (arc1)
          != IR_max_attributes_stack_increment (arc2)))
    return FALSE;
  current_LR_set1 = IR_LR_set (IR_from_LR_situation (arc1));
  current_LR_set2 = IR_LR_set (IR_from_LR_situation (arc2));
  assert (IR_reachable_flag (current_LR_set1)
          && IR_reachable_flag (current_LR_set2));
  stack_displacement1 = 0;
  stack_displacement2 = 0;
  VLO_CREATE (states_stack1, 0);
  VLO_CREATE (states_stack2, 0);
  if (IR_terminal_marking_arc (arc1) != NULL)
    {
      current_LR_set1 = IR_goto_LR_set (IR_from_LR_situation (arc1));
      current_LR_set2 = IR_goto_LR_set (IR_from_LR_situation (arc2));
      result = ((IR_attribute_is_used (current_LR_set1)
                 && IR_attribute_is_used (current_LR_set2))
                || (!IR_attribute_is_used (current_LR_set1)
                    && !IR_attribute_is_used (current_LR_set2)));
      if (IR_result_LR_set_will_be_on_the_stack (arc1))
        VLO_ADD_MEMORY (states_stack1, &current_LR_set1, sizeof (IR_node_t));
      if (IR_result_LR_set_will_be_on_the_stack (arc2))
        VLO_ADD_MEMORY (states_stack2, &current_LR_set2, sizeof (IR_node_t));
    }
  else
    result = FALSE;
  VLO_CREATE (scale1, 0);
  VLO_CREATE (scale2, 0);
  if (result)
    for (current_element1 = IR_first_rule_list_element (arc1),
           current_element2 = IR_first_rule_list_element (arc2);
         current_element1 != NULL || current_element2 != NULL;)
      {
        while (current_element1 != NULL)
          {
            canonical_rule1 = IR_canonical_rule (current_element1);
            if (IR_result_LR_set_will_be_on_the_stack (current_element1)
                || IR_action (canonical_rule1) != NULL)
              break;
            rule_length1
              = canonical_rule_right_hand_side_prefix_length (canonical_rule1,
                                                              NULL);
            LR_set_predecessor1
              = get_the_single_LR_set_predecessor (current_LR_set1,
                                                   rule_length1);
            if (pushed_LR_sets_or_attributes_number_on_path (current_LR_set1,
                                                             rule_length1,
                                                             TRUE) != 0)
              break;
            next_LR_set
              = goto_by_nonterminal (LR_set_predecessor1,
                                     IR_left_hand_side (canonical_rule1));
            if (IR_attribute_is_used (next_LR_set))
              break;
            current_LR_set1 = next_LR_set;
            current_element1 = IR_next_rule_list_element (current_element1);
          }
        while (current_element2 != NULL)
          {
            canonical_rule2 = IR_canonical_rule (current_element2);
            if (IR_result_LR_set_will_be_on_the_stack (current_element2)
                || IR_action (canonical_rule2) != NULL)
              break;
            rule_length2
              = canonical_rule_right_hand_side_prefix_length (canonical_rule2,
                                                              NULL);
            LR_set_predecessor2
              = get_the_single_LR_set_predecessor (current_LR_set2,
                                                   rule_length2);
            if (pushed_LR_sets_or_attributes_number_on_path (current_LR_set2,
                                                             rule_length2,
                                                             TRUE) != 0)
              break;
            next_LR_set
              = goto_by_nonterminal (LR_set_predecessor2,
                                     IR_left_hand_side (canonical_rule2));
            if (IR_attribute_is_used (next_LR_set))
              break;
            current_LR_set2 = next_LR_set;
            current_element2 = IR_next_rule_list_element (current_element2);
          }
        if (current_element1 != NULL)
          {
            canonical_rule1 = IR_canonical_rule (current_element1);
            rule_length1
              = canonical_rule_right_hand_side_prefix_length (canonical_rule1,
                                                              NULL);
            LR_set_predecessor1
              = get_the_single_LR_set_predecessor (current_LR_set1,
                                                   rule_length1);
            stack_displacement1
              -= pushed_LR_sets_or_attributes_number_on_path (current_LR_set1,
                                                              rule_length1,
                                                              TRUE);
            if (IR_action (canonical_rule1) != NULL)
              {
                bound_right_hand_side_element
                  = IR_original_code_insertion_place (canonical_rule1);
                if (bound_right_hand_side_element != NULL)
                  original_canonical_rule
                    = IR_canonical_rule (bound_right_hand_side_element);
                else
                  original_canonical_rule = canonical_rule1;
                rule_length1
                  = canonical_rule_right_hand_side_prefix_length
                    (original_canonical_rule, bound_right_hand_side_element);
                VLO_NULLIFY (scale1);
                VLO_EXPAND (scale1, rule_length1 + 1);
                for (i = 0; i < rule_length1; i++)
                  ((char *) VLO_BEGIN (scale1)) [i] = 0;
                determine_pushed_scale (current_LR_set1, rule_length1,
                                        (char *) VLO_BEGIN (scale1), TRUE);
              }
            current_LR_set1
              = goto_by_nonterminal (LR_set_predecessor1,
                                     IR_left_hand_side (canonical_rule1));
            if (IR_result_LR_set_will_be_on_the_stack (current_element1))
              VLO_ADD_MEMORY (states_stack1, &current_LR_set1,
                              sizeof (IR_node_t));
            if (IR_attribute_is_used (current_LR_set1))
              stack_displacement1++;
          }
        if (current_element2 != NULL)
          {
            canonical_rule2 = IR_canonical_rule (current_element2);
            rule_length2
              = canonical_rule_right_hand_side_prefix_length (canonical_rule2,
                                                              NULL);
            LR_set_predecessor2
              = get_the_single_LR_set_predecessor (current_LR_set2,
                                                   rule_length2);
            stack_displacement2
              -= pushed_LR_sets_or_attributes_number_on_path (current_LR_set2,
                                                              rule_length2,
                                                              TRUE);
            if (IR_action (canonical_rule2) != NULL)
              {
                bound_right_hand_side_element
                  = IR_original_code_insertion_place (canonical_rule2);
                if (bound_right_hand_side_element != NULL)
                  original_canonical_rule
                    = IR_canonical_rule (bound_right_hand_side_element);
                else
                  original_canonical_rule = canonical_rule2;
                rule_length2
                  = canonical_rule_right_hand_side_prefix_length
                    (original_canonical_rule, bound_right_hand_side_element);
                VLO_NULLIFY (scale2);
                VLO_EXPAND (scale2, rule_length2 + 1);
                for (i = 0; i < rule_length2; i++)
                  ((char *) VLO_BEGIN (scale2)) [i] = 0;
                determine_pushed_scale (current_LR_set2, rule_length2,
                                        (char *) VLO_BEGIN (scale2), TRUE);
              }
            current_LR_set2
              = goto_by_nonterminal (LR_set_predecessor2,
                                     IR_left_hand_side (canonical_rule2));
            if (IR_result_LR_set_will_be_on_the_stack (current_element2))
              VLO_ADD_MEMORY (states_stack2, &current_LR_set2,
                              sizeof (IR_node_t));
            if (IR_attribute_is_used (current_LR_set2))
              stack_displacement2++;
          }
        if ((current_element1 == NULL && current_element2 != NULL)
            || (current_element1 != NULL && current_element2 == NULL)
            || stack_displacement1 != stack_displacement2
            || (current_element1 != NULL && current_element2 != NULL
                && (IR_action (canonical_rule1) != IR_action (canonical_rule2)
                    || (IR_action (canonical_rule1) != NULL
                        && memcmp (VLO_BEGIN (scale1), VLO_BEGIN (scale2),
                                   rule_length1)))))
          {
            result = FALSE;
            break;
          }
        if (current_element1 != NULL)
          current_element1 = IR_next_rule_list_element (current_element1);
        if (current_element2 != NULL)
          current_element2 = IR_next_rule_list_element (current_element2);
      }
  if (VLO_LENGTH (states_stack1) != VLO_LENGTH (states_stack2))
    result = FALSE;
  else
    for (current_LR_set_ptr1 = VLO_BEGIN (states_stack1),
           current_LR_set_ptr2 = VLO_BEGIN (states_stack2);
         (char *) current_LR_set_ptr1 <= (char *) VLO_END (states_stack1);
         current_LR_set_ptr1++, current_LR_set_ptr2++)
      if (*current_LR_set_ptr1 != *current_LR_set_ptr2)
        {
          result = FALSE;
          break;
        }
  assert (!result
          || (stack_displacement2 == stack_displacement1
              && (IR_regular_arc_popped_attributes_number (arc1)
                  == IR_regular_arc_popped_attributes_number (arc2))));
  VLO_DELETE (states_stack1);
  VLO_DELETE (states_stack2);
  VLO_DELETE (scale1);
  VLO_DELETE (scale2);
  return result;
}

static void
set_up_regular_arcs_equivalence (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_regular_arc;
  IR_double_link_t regular_arc_reference;
  IR_node_t *regular_arc_class;
  vlo_t classes;

  VLO_CREATE (classes, 0);
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          VLO_NULLIFY (classes);
          for (regular_arc_reference = IR__first_double_link (current_LR_set);
               regular_arc_reference != NULL;
               regular_arc_reference
                 = IR__next_double_link (regular_arc_reference))
            {
              current_regular_arc = IR__owner (regular_arc_reference);
              if (IR_IS_OF_TYPE (current_regular_arc, IR_NM_regular_arc)
                  && IR_reachable_flag (IR_LR_set (IR_from_LR_situation
                                                   (current_regular_arc))))
                {
                  for (regular_arc_class = VLO_BEGIN (classes);
                       (char *) regular_arc_class
                         <= (char *) VLO_END (classes);
                       regular_arc_class++)
                    if (regular_arcs_are_equivalent (current_regular_arc,
                                                     *regular_arc_class))
                      break;
                  if ((char *) regular_arc_class <= (char *) VLO_END (classes))
                    {
                      IR_set_next_equivalent_regular_arc
                        (current_regular_arc,
                         IR_next_equivalent_regular_arc (*regular_arc_class));
                      IR_set_next_equivalent_regular_arc
                        (*regular_arc_class, current_regular_arc);
                    }
                  else
                    {
                      /* New equivalence class. */
                      VLO_ADD_MEMORY (classes, &current_regular_arc,
                                      sizeof (current_regular_arc));
                      IR_set_first_equivalent_regular_arc_flag
                        (current_regular_arc, TRUE);
                      /* Form cyclic list. */
                      IR_set_next_equivalent_regular_arc
                        (current_regular_arc, current_regular_arc);
                    }
                }
            }
        }
  VLO_DELETE (classes);
}

static void
enumerate_pushed_LR_sets (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  int number;

  number = 0;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set)
          && IR_it_is_pushed_LR_set (current_LR_set))
        number++;
  IR_set_pushed_LR_sets_number (description, number);
}

void
make_regular_optimization (void)
{
#ifndef NDEBUG
  ticker_t temp_ticker;
#endif

#ifndef NDEBUG
  temp_ticker = create_ticker ();
#endif
  mark_LR_sets_with_used_attribute ();
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr, "      marking LR-sets with used attributes -- %ssec\n",
             active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  transform_LR_graph ();
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr, "      transforming LR-graph -- %ssec\n",
             active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  remove_unused_nonterminal_LR_arcs_and_mark_unreachable_LR_sets ();
#ifndef NDEBUG
  if (time_flag)
    fprintf
      (stderr,
       "      removal unused nonterm. LR-arcs & marking unreachable LR-sets -- %ssec\n",
       active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  mark_semantic_pushed_LR_sets ();
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr, "      marking semantically pushed LR-sets -- %ssec\n",
             active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  mark_concordance_pushed_LR_sets_or_attributes (FALSE);
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr,
             "      making concordance between pushed LR-sets -- %ssec\n",
             active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  mark_concordance_pushed_LR_sets_or_attributes (TRUE);
#ifndef NDEBUG
  if (time_flag)
    fprintf
      (stderr,
       "      making concordance between pushed LR-set attributes -- %ssec\n",
       active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  set_up_popped_LR_sets_and_attributes_for_regular_arcs ();
#ifndef NDEBUG
  if (time_flag)
    fprintf
      (stderr,
       "      evaluating popped LR-sets & attributes of regular arcs -- %ssec\n",
       active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  set_up_right_hand_side_used_attributes_numbers ();
#ifndef NDEBUG
  if (time_flag)
    fprintf
      (stderr,
       "      stack displacement for used attributes evaluation -- %ssec\n",
       active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  set_up_regular_arcs_equivalence ();
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr, "      searching for regular arcs equivalence -- %ssec\n",
             active_time_string (temp_ticker));
#endif
  enumerate_pushed_LR_sets ();
}

