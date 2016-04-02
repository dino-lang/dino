/*
   Copyright (C) 1997-2016 Vladimir Makarov.

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
#include "position.h"
#include "errors.h"
#include "vlobject.h"
#include "ticker.h"
#include "common.h"
#include "ird.h"
#include "gen-comm.h"
#include "output.h"
#include "contexts.h"
#include "relation.h"
#include "lr-sets.h"
#include "lr.h"
#include "regular.h"
#include "la-trie.h"
#include "descript.h"
#include "parser.h"
#include "gen.h"

#include <assert.h>



/* Add start rule `$accept : axiom $end ...' as the first rule in the
   canonical rules list.  `$end' will have value equals to 0.  `$end'
   and `$accept' are added to the end of the single definition list
   and will have the last order number. */

static void
add_start_rule (void)
{
  IR_node_t end_marker_identifier;
  IR_node_t start_nonterm_identifier;
  IR_node_t single_definition;
  IR_node_t last_single_definition;
  IR_node_t start_nonterm_single_definition;
  IR_node_t start_rule;
  IR_node_t end_marker_list;
  int i;

  assert (IR_single_definition_list (description) != NULL);
  for (single_definition = IR_single_definition_list (description);
       single_definition != NULL;
       single_definition = IR_next_single_definition (single_definition))
    last_single_definition = single_definition;
  /* Create single terminal definition for `$end'. */
  end_marker_identifier = IR_new_identifier (no_position, "$end", FALSE);
  end_marker_single_definition
    = IR_new_single_term_definition (no_position, end_marker_identifier, NULL);
  IR_set_value (end_marker_single_definition, 0);
  IR_set_accessibility_flag (end_marker_single_definition, TRUE);
  IR_set_token_order_number (end_marker_single_definition,
                             IR_tokens_number (description));
  IR_set_tokens_number (description, IR_tokens_number (description) + 1);
  IR_set_next_single_definition (last_single_definition,
                                 end_marker_single_definition);
  last_single_definition = end_marker_single_definition;
  /* Create single nonterm definition for `$accept'. */
  start_nonterm_identifier = IR_new_identifier (no_position, "$accept", FALSE);
  start_nonterm_single_definition
    = IR_new_single_nonterm_definition
      (no_position, start_nonterm_identifier, NULL, NULL);
  IR_set_accessibility_flag (start_nonterm_single_definition, TRUE);
  IR_set_derivation_ability_flag
    (start_nonterm_single_definition,
     IR_derivation_ability_flag (IR_axiom_definition (description)));
  /* Generally speaking this value is incorrect, but it is
     unimportant. */
  IR_set_minimal_derived_string_length
    (start_nonterm_single_definition, max_look_ahead_number);
  IR_set_nonterm_order_number (start_nonterm_single_definition,
                               IR_nonterminals_number (description));
  IR_set_nonterminals_number (description,
                              IR_nonterminals_number (description) + 1);
  IR_set_next_single_definition (last_single_definition,
                                 start_nonterm_single_definition);
  /* Create start rule `$accept : axiom $end ...'. */
  start_rule
    = IR_new_canonical_rule (no_position, start_nonterm_single_definition,
                             NULL, NULL, NULL,
                             IR_canonical_rule_list (description));
  IR_set_canonical_rule_list (description, start_rule);
  IR_set_nonterm_canonical_rule_list (start_nonterm_single_definition,
                                      start_rule);
  IR_set_canonical_rule_order_number (start_rule, 0);
  IR_set_canonical_rules_number (description,
                                 IR_canonical_rules_number (description) + 1);
  end_marker_list = NULL;
  for (i = 0; i < max_look_ahead_number; i++)
    {
      end_marker_list
        = IR_new_canonical_rule_element
          (no_position, start_rule, end_marker_list,
           end_marker_single_definition, NULL,
           IR_single_definition_usage_list (end_marker_single_definition));
      IR_set_single_definition_usage_list (end_marker_single_definition,
                                           end_marker_list);
    }
  IR_set_right_hand_side
    (start_rule,
     IR_new_canonical_rule_element
     (no_position, start_rule, end_marker_list,
      IR_axiom_definition (description), NULL,
      IR_single_definition_usage_list (IR_axiom_definition (description))));
  IR_set_axiom_identifier (description, start_nonterm_identifier);
  IR_set_single_definition_usage_list (IR_axiom_definition (description),
                                       IR_right_hand_side (start_rule));
  IR_set_axiom_definition (description, start_nonterm_single_definition);
}

static void
add_canonical_rule_end (void)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_right_hand_side_element;
  IR_node_t last_right_hand_side_element;

  for (current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    {
      last_right_hand_side_element = NULL;
      for (current_right_hand_side_element
           = IR_right_hand_side (current_canonical_rule);
           current_right_hand_side_element != NULL;
           current_right_hand_side_element
           = IR_next_right_hand_side_element (current_right_hand_side_element))
        last_right_hand_side_element = current_right_hand_side_element;
      if (last_right_hand_side_element != NULL)
        IR_set_next_right_hand_side_element
          (last_right_hand_side_element,
           IR_new_canonical_rule_end (no_position, current_canonical_rule,
                                      NULL));
      else
        IR_set_right_hand_side
          (current_canonical_rule,
           IR_new_canonical_rule_end (no_position, current_canonical_rule,
                                      NULL));
    }
}

static void
set_cp_flags (void)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_right_hand_side_element;
  IR_node_t next_right_hand_side_element;
  int cp_flag_is_started;

  for (current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    {
      cp_flag_is_started = FALSE;
      if (IR_next_cp_flag (current_canonical_rule))
        {
          IR_set_cp_start_flag (IR_right_hand_side (current_canonical_rule),
                                TRUE);
          IR_set_cp_flag (IR_right_hand_side (current_canonical_rule), TRUE);
          cp_flag_is_started = TRUE;
        }
      for (current_right_hand_side_element
           = IR_right_hand_side (current_canonical_rule);
           current_right_hand_side_element != NULL;
           current_right_hand_side_element = next_right_hand_side_element)
        {
          next_right_hand_side_element
            = IR_next_right_hand_side_element
              (current_right_hand_side_element);
          if (next_right_hand_side_element != NULL
              && IR_next_cp_flag (current_right_hand_side_element))
            {
              IR_set_cp_start_flag (next_right_hand_side_element, TRUE);
              cp_flag_is_started = TRUE;
            }
          if (next_right_hand_side_element != NULL && cp_flag_is_started)
            IR_set_cp_flag (next_right_hand_side_element, TRUE);
        }
    }
}



static void
set_up_LR_set_some_field_standard_values (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;
  IR_node_t symbol_definition;

  assert (!regular_optimization_flag);
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          if (characteristic_symbol_of_LR_set (current_LR_set)
              == error_single_definition)
	    IR_set_it_is_errored_LR_set (current_LR_set, TRUE);
          IR_set_attribute_is_used (current_LR_set, TRUE);
          IR_set_it_is_pushed_LR_set (current_LR_set, TRUE);
          for (current_LR_situation = IR_LR_situation_list (current_LR_set);
               current_LR_situation != NULL;
               current_LR_situation
                 = IR_next_LR_situation (current_LR_situation))
            if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                               IR_NM_canonical_rule_end))
              {
                IR_set_popped_LR_sets_number
                  (current_LR_situation, 
                   canonical_rule_right_hand_side_prefix_length
                   (IR_canonical_rule (IR_element_after_dot
                                       (current_LR_situation)), NULL));
                IR_set_popped_attributes_number
                  (current_LR_situation, 
                   canonical_rule_right_hand_side_prefix_length
                   (IR_canonical_rule (IR_element_after_dot
                                       (current_LR_situation)), NULL));
              }
	    else
	      {
                if (IR_first_symbol_LR_situation (current_LR_situation))
                  {
                    symbol_definition
                      = IR_element_itself (IR_element_after_dot 
                                           (current_LR_situation));
                    if (symbol_definition == error_single_definition)
		      /* It is necessary for error recovery. */
		      IR_set_it_is_errored_LR_set (current_LR_set, TRUE);
                  }
	      }
        }
}



static void
enumerate_LR_sets (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  int order_number;

  order_number = 0;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          IR_set_LR_set_order_number (current_LR_set, order_number);
          order_number++;
        }
  IR_set_LR_sets_number (description, order_number);
}

static void
set_term_arcs_number (void)
{
  int term_arcs_number;
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;
  IR_node_t current_regular_arc;

  term_arcs_number = 0;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          for (current_LR_situation = IR_LR_situation_list (current_LR_set);
               current_LR_situation != NULL;
               current_LR_situation
                 = IR_next_LR_situation (current_LR_situation))
            {
              current_regular_arc
                = IR_corresponding_regular_arc (current_LR_situation);
              if (IR_first_symbol_LR_situation (current_LR_situation)
                  && !IR_IS_OF_TYPE (IR_element_after_dot
                                     (current_LR_situation),
                                     IR_NM_canonical_rule_end)
                  && IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                                       (current_LR_situation)),
                                    IR_NM_single_term_definition)
                  && !IR_goto_arc_has_been_removed (current_LR_situation))
                {
                  assert (current_regular_arc == NULL);
                  term_arcs_number++;
                }
              if (current_regular_arc != NULL)
                term_arcs_number++;
            }
          IR_set_term_arcs_number (current_LR_set, term_arcs_number);
        }
}

static void
set_nonterm_arcs_number (void)
{
  int nonterm_arcs_number;
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;

  nonterm_arcs_number = 0;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          for (current_LR_situation = IR_LR_situation_list (current_LR_set);
               current_LR_situation != NULL;
               current_LR_situation
                 = IR_next_LR_situation (current_LR_situation))
            if (IR_first_symbol_LR_situation (current_LR_situation)
                && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                                   IR_NM_canonical_rule_end)
                && IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                                     (current_LR_situation)),
                                  IR_NM_single_nonterm_definition)
                && !IR_goto_arc_has_been_removed (current_LR_situation))
              nonterm_arcs_number++;
          IR_set_nonterm_arcs_number (current_LR_set, nonterm_arcs_number);
        }
}

static void
enumerate_regular_arcs (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;
  IR_node_t regular_arc;
  int number_of_regular_arcs;
  int all_number_of_regular_arcs;

  number_of_regular_arcs = 0;
  all_number_of_regular_arcs = 0;
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
            regular_arc = IR_corresponding_regular_arc (current_LR_situation);
            if (regular_arc != NULL)
              {
                all_number_of_regular_arcs++;
                if (IR_first_equivalent_regular_arc_flag (regular_arc))
                  {
                    IR_set_number_of_regular_arc (regular_arc,
                                                  number_of_regular_arcs);
                    for (regular_arc
                           = IR_next_equivalent_regular_arc (regular_arc);
                         !IR_first_equivalent_regular_arc_flag (regular_arc);
                         regular_arc
                           = IR_next_equivalent_regular_arc (regular_arc))
                      IR_set_number_of_regular_arc (regular_arc,
                                                    number_of_regular_arcs);
                    number_of_regular_arcs++;
                  }
              }
          }
  IR_set_number_of_regular_arcs (description, number_of_regular_arcs);
  IR_set_all_number_of_regular_arcs (description, all_number_of_regular_arcs);
}



static IR_node_t
insert_token_into_equivalence_class
  (IR_node_t token, IR_node_t cyclic_equivalence_class_token_list)
{
  if (cyclic_equivalence_class_token_list == NULL)
    IR_set_next_equivalence_class_token (token, token);
  else
    {
      IR_set_next_equivalence_class_token
        (token,
         IR_next_equivalence_class_token
         (cyclic_equivalence_class_token_list));
      IR_set_next_equivalence_class_token (cyclic_equivalence_class_token_list,
                                           token);
    }
  return token;
}

static void
delete_token_from_equivalence_class (IR_node_t equivalence_class_token)
{
  IR_node_t current_equivalence_class_token;
  IR_node_t previous_equivalence_class_token;

  previous_equivalence_class_token = equivalence_class_token;
  for (current_equivalence_class_token
       = IR_next_equivalence_class_token (equivalence_class_token);
       current_equivalence_class_token != equivalence_class_token;
       current_equivalence_class_token
       = IR_next_equivalence_class_token (current_equivalence_class_token))
    previous_equivalence_class_token = current_equivalence_class_token;
  if (previous_equivalence_class_token != equivalence_class_token)
    IR_set_next_equivalence_class_token
      (previous_equivalence_class_token,
       IR_next_equivalence_class_token (equivalence_class_token));
}

static void
process_token_equivalence_class (IR_node_t token,
                                 IR_node_t *token_actions_array)
{
  IR_node_t next_equivalence_class_token;
  IR_node_t current_equivalence_class_token;
  IR_node_t cyclic_equivalence_class_token_list;
  IR_node_t current_equivalence_class_token_regular_arc;
  IR_node_t token_regular_arc;

  assert (token_actions_array [IR_token_order_number (token)] != NULL);
  current_equivalence_class_token = token;
  /* New class of tokens which are not equivalent to given token. */
  cyclic_equivalence_class_token_list = NULL;
  if (!IR_token_was_processed_on_equivalence (token))
    do
      {
        next_equivalence_class_token
          = IR_next_equivalence_class_token (current_equivalence_class_token);
        token_regular_arc = NULL;
        if (IR_IS_OF_TYPE (token_actions_array [IR_token_order_number (token)],
                           IR_NM_LR_situation))
          token_regular_arc
            = IR_corresponding_regular_arc (token_actions_array
                                            [IR_token_order_number (token)]);
        current_equivalence_class_token_regular_arc = NULL;
        if (token_actions_array [IR_token_order_number
                                (current_equivalence_class_token)] != NULL
            && IR_IS_OF_TYPE (token_actions_array
                              [IR_token_order_number
                              (current_equivalence_class_token)],
                              IR_NM_LR_situation))
          current_equivalence_class_token_regular_arc
            = IR_corresponding_regular_arc
              (token_actions_array [IR_token_order_number
                                    (current_equivalence_class_token)]);
        if ((token_actions_array
             [IR_token_order_number (current_equivalence_class_token)] != NULL
             && token_regular_arc != NULL
             && current_equivalence_class_token_regular_arc != NULL
             /* Two regular arcs. */
             && regular_arcs_are_equivalent
                (token_regular_arc,
                 current_equivalence_class_token_regular_arc))
            /* Here comparing values of trie node elements. */
            || (token_actions_array
                [IR_token_order_number (current_equivalence_class_token)]
                == token_actions_array [IR_token_order_number (token)]))
          IR_set_token_was_processed_on_equivalence
            (current_equivalence_class_token, TRUE);
        else
          {
            delete_token_from_equivalence_class
              (current_equivalence_class_token);
            cyclic_equivalence_class_token_list
              = insert_token_into_equivalence_class
                (current_equivalence_class_token,
                 cyclic_equivalence_class_token_list);
          }
        current_equivalence_class_token = next_equivalence_class_token;
      }
    while (current_equivalence_class_token != token);
}

static void
process_trie_nodes_list_on_token_equivalence
  (IR_node_t trie_nodes_list, IR_node_t *token_actions_array)
{
  IR_node_t current_trie_node;
  int current_token_number;

  for (current_token_number = 0;
       current_token_number < IR_tokens_number (description);
       current_token_number++)
    token_actions_array [current_token_number] = NULL;
  for (current_trie_node = trie_nodes_list;
       current_trie_node != NULL;
       current_trie_node = IR_next_brother (current_trie_node))
    if (IR_first_son (current_trie_node) != NULL
        || IR_corresponding_single_term_definition (current_trie_node) != NULL)
      {
        token_actions_array
          [IR_token_order_number
           (IR_corresponding_single_term_definition (current_trie_node))]
             = (IR_first_son (current_trie_node) != NULL
                ? IR_first_son (current_trie_node)
                : IR_corresponding_LR_situation (current_trie_node));
        IR_set_token_was_processed_on_equivalence
          (IR_corresponding_single_term_definition (current_trie_node), FALSE);
      }
  /* Here elements of `token_actions_array' corresponding to default
     action have NULL value.  Elements corresponding to other nodes of
     trie nodes list have non-NULL values.  It is important for
     correct partition on equivalence classes. */
  for (current_trie_node = trie_nodes_list;
       current_trie_node != NULL
       && IR_corresponding_single_term_definition (current_trie_node) != NULL;
       current_trie_node = IR_next_brother (current_trie_node))
    process_token_equivalence_class (IR_corresponding_single_term_definition
                                     (current_trie_node),
                                     token_actions_array);
  /* Process lower levels of the trie. */
  for (current_trie_node = trie_nodes_list;
       current_trie_node != NULL;
       current_trie_node = IR_next_brother (current_trie_node))
    if (IR_first_son (current_trie_node) != NULL)
      process_trie_nodes_list_on_token_equivalence
        (IR_first_son (current_trie_node), token_actions_array);
}

static void
set_token_equivalence_classes (void)
{
  IR_node_t current_single_definition;
  IR_node_t first_equivalence_class_token;
  IR_node_t current_equivalence_class_token;
  IR_node_t cyclic_equivalence_class_token_list;
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;
  IR_node_t *token_actions_array;
  int token_equivalence_classes_number;
  vlo_t token_actions_vector;

  /* All tokens are included in one equivalence class. */
  cyclic_equivalence_class_token_list = NULL;
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_term_definition))
      cyclic_equivalence_class_token_list
        = insert_token_into_equivalence_class
          (current_single_definition, cyclic_equivalence_class_token_list);
  /* Process tokens in order to make equivalence partition. */
  VLO_CREATE (token_actions_vector, 2000);
  VLO_EXPAND (token_actions_vector,
              sizeof (IR_node_t) * IR_tokens_number (description));
  token_actions_array = VLO_BEGIN (token_actions_vector);
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          /* Process tokens of the look ahead trie. */
          process_trie_nodes_list_on_token_equivalence
            (IR_LR_set_look_ahead_trie (current_LR_set), token_actions_array);
        }
  VLO_DELETE (token_actions_vector);
  /* Enumerate equivalence classes. */
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition, IR_NM_single_term_definition))
      /* Set undefined value. */
      IR_set_equivalence_class_number (current_single_definition, -1);
  token_equivalence_classes_number = 0;
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition, IR_NM_single_term_definition)
        && IR_equivalence_class_number (current_single_definition) < 0)
      {
        first_equivalence_class_token = current_single_definition;
        current_equivalence_class_token = first_equivalence_class_token;
        do
          {
            IR_set_equivalence_class_number (current_equivalence_class_token,
                                             token_equivalence_classes_number);
            current_equivalence_class_token
              = IR_next_equivalence_class_token
                (current_equivalence_class_token);
          }
        while (current_equivalence_class_token
               != first_equivalence_class_token);
        token_equivalence_classes_number++;
      }
  IR_set_token_equivalence_classes_number (description,
                                           token_equivalence_classes_number);
}



static void
generate_parser_graph_representation (void)
{
  ticker_t temp_ticker;
  ticker_t all_ticker;

  all_ticker = create_ticker ();
  initiate_LR_core_table ();
  if (lr_flag || (verbose_flag && lr_situation_context_flag))
    {
      temp_ticker = create_ticker ();
      evaluate_minimal_derived_string_length ();
      evaluate_nonterminals_relation_FIRST ();
      set_FIRST_of_rule_tails ();
      if (time_flag)
        fprintf (stderr, "    Evaluation of nonterminal relations -- %ssec\n",
                 active_time_string (temp_ticker));
    }
  if (!lr_flag)
    {
      temp_ticker = create_ticker ();
      if (verbose_flag && lr_situation_context_flag)
        create_LALR_sets_with_all_contexts ();
      else
        create_LALR_sets ();
      if (time_flag)
        fprintf
          (stderr,
           (verbose_flag && lr_situation_context_flag
            ? "    create LALR-sets with all contexts -- %ssec\n"
            : "    create LALR-sets only with needed contexts -- %ssec\n"),
           active_time_string (temp_ticker));
    }
  else
    {
      temp_ticker = create_ticker ();
      initiate_LR_set_table ();
      create_LR_sets ();
      if (time_flag)
        fprintf (stderr, "    create canonical LR-sets -- %ssec\n",
                 active_time_string (temp_ticker));
      if (lalr_optimization_flag)
        {
          temp_ticker = create_ticker ();
          make_LALR_optimization ();
          if (time_flag)
            fprintf (stderr, "    make LALR-optimization -- %ssec\n",
                     active_time_string (temp_ticker));
        }
      make_full_LR_sets ();
      finish_LR_set_table ();
    }
  finish_LR_core_table ();
  if (lr_flag || (verbose_flag && lr_situation_context_flag))
    {
      temp_ticker = create_ticker ();
      process_conflicts (FALSE);
      if (time_flag)
        fprintf (stderr, "    processing conflicts -- %ssec\n",
                 active_time_string (temp_ticker));
    }
  if (regular_optimization_flag)
    {
      temp_ticker = create_ticker ();
      make_regular_optimization ();
      if (time_flag)
        fprintf (stderr, "    all regular optimization -- %ssec\n",
                 active_time_string (temp_ticker));
    }
  else
    set_up_LR_set_some_field_standard_values ();
  /* The following function is called after regular optimization because
     the optimization can delete some LR-sets. */
  enumerate_LR_sets ();
  set_term_arcs_number ();
  set_nonterm_arcs_number ();
  enumerate_regular_arcs ();
  temp_ticker = create_ticker ();
  create_LR_set_look_ahead_trie ();
  if (time_flag)
    fprintf (stderr, "    making parser look-ahead trie -- %ssec\n",
             active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
  set_token_equivalence_classes ();
  if (time_flag)
    fprintf (stderr, "    making token equivalence classes -- %ssec\n",
             active_time_string (temp_ticker));
  real_look_ahead_number = get_max_look_ahead_string_length ();
  if (time_flag)
    fprintf (stderr,
             "  all generation of internal parser representation -- %ssec\n",
             active_time_string (all_ticker));
}



static void
count_conflicts (int *shift_reduce_conflicts_number,
                 int *reduce_reduce_conflicts_number)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  int LR_set_shift_reduce_conflicts_number;
  int LR_set_reduce_reduce_conflicts_number;

  *shift_reduce_conflicts_number = 0;
  *reduce_reduce_conflicts_number = 0;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          LR_set_conflicts_number (current_LR_set,
                                   &LR_set_shift_reduce_conflicts_number,
                                   &LR_set_reduce_reduce_conflicts_number);
          *shift_reduce_conflicts_number
            += LR_set_shift_reduce_conflicts_number;
          *reduce_reduce_conflicts_number
            += LR_set_reduce_reduce_conflicts_number;
        }
}



void
generate (void)
{
  int shift_reduce_conflicts_number;
  int reduce_reduce_conflicts_number;
  ticker_t output_description_ticker;

  if (!split_lr_sets_flag_is_defined)
    split_lr_sets_flag = IR_scanner_flag (description);
  initiate_output ();
  add_start_rule ();
  add_canonical_rule_end ();
  set_cp_flags (); /* must be only when rule ends have been added. */
  if (verbose_flag)
    {
      output_grammar ();
      output_string (output_description_file, "\n\f\n\n");
    }
  initiate_contexts ();
  initiate_LR_set_look_ahead_tries ();
  initiate_LR_situations ();
  initiate_traverse_cache ();
  initiate_goto_set_cache ();
  fprintf (stderr, "  Max look ahead is %d %s\n", max_look_ahead_number,
           (max_look_ahead_number == 1 ? "token" : "tokens"));
  generate_parser_graph_representation ();
  if (verbose_flag && (lr_flag || lr_situation_context_flag))
    {
      output_first_sets ();
      output_string (output_description_file, "\n\f\n\n");
    }
  fprintf (stderr, "  Real look ahead is %d %s\n", real_look_ahead_number,
           (real_look_ahead_number == 1 ? "token" : "tokens"));
  count_conflicts (&shift_reduce_conflicts_number,
                   &reduce_reduce_conflicts_number);
  output_description_ticker = create_ticker ();
  if (verbose_flag)
    output_states ();
  if (shift_reduce_conflicts_number != 0
      || reduce_reduce_conflicts_number != 0)
    {
      if (shift_reduce_conflicts_number != 0)
        fprintf (stderr, "  %d shift/reduce", shift_reduce_conflicts_number);
      if (reduce_reduce_conflicts_number != 0)
        {
          if (shift_reduce_conflicts_number != 0)
            fprintf (stderr, " and %d reduce/reduce",
                     reduce_reduce_conflicts_number);
          else
            fprintf (stderr, "  %d reduce/reduce",
                     reduce_reduce_conflicts_number);
        }
      if (shift_reduce_conflicts_number + reduce_reduce_conflicts_number != 1)
        fprintf (stderr, " conflicts.\n");
      else
        fprintf (stderr, " conflict.\n");
    }
  if (verbose_flag)
    {
      output_summary ();
      if (time_flag)
        fprintf (stderr, "  outputting description file -- %ssec\n",
                 active_time_string (output_description_ticker));
    }
  output_parser ();
  finish_goto_set_cache ();
  finish_traverse_cache ();
  finish_LR_situations ();
  finish_LR_set_look_ahead_tries ();
  finish_contexts ();
  if (IR_expected_shift_reduce_conflicts_number (description) >= 0
      && (IR_expected_shift_reduce_conflicts_number (description)
          != shift_reduce_conflicts_number))
    error
      (TRUE, no_position,
       "expected shift/reduce conflicts number(%d) is unequal to the real one",
       IR_expected_shift_reduce_conflicts_number (description));
}
