/*
   Copyright (C) 1997-2005 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

#include <stdlib.h>
#include "vlobject.h"
#include "common.h"
#include "ird.h"
#include "gen-comm.h"
#include "output.h"
#include "contexts.h"
#include "lr-sets.h"
#include "lr.h"
#include "descript.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif



static void pattern_representation (IR_node_t pattern, vlo_t *representation);

/* The following function adds representation of given rule sequence
   element to given variable length object. */

static void
sequence_element_representation (IR_node_t sequence_element,
                                 vlo_t *representation)
{
  if (sequence_element == NULL)
    return;
  if (IR_IS_OF_TYPE (sequence_element, IR_NM_control_point))
    VLO_ADD_STRING (*representation, "%cp");
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_default))
    {
      VLO_ADD_STRING (*representation, "[");
      pattern_representation (IR_default_pattern (sequence_element),
                              representation);
      VLO_ADD_STRING (*representation, "]");
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_star_iteration))
    {
      sequence_element_representation (IR_iteration_unit (sequence_element),
                                       representation);
      VLO_ADD_STRING (*representation, "*");
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_plus_iteration))
    {
      sequence_element_representation (IR_iteration_unit (sequence_element),
                                       representation);
      VLO_ADD_STRING (*representation, "+");
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_group))
    {
      VLO_ADD_STRING (*representation, "(");
      pattern_representation (IR_pattern (sequence_element), representation);
      VLO_ADD_STRING (*representation, ")");
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_range_atom))
    {
      identifier_or_literal_representation (IR_left_bound (sequence_element),
                                            FALSE, representation);
      if (IR_IS_OF_TYPE (sequence_element,
                         IR_NM_range_no_left_right_bounds_atom))
        VLO_ADD_STRING (*representation, "<->");
      else if (IR_IS_OF_TYPE (sequence_element,
                              IR_NM_range_no_left_bound_atom))
        VLO_ADD_STRING (*representation, "<-");
      else if (IR_IS_OF_TYPE (sequence_element,
                              IR_NM_range_no_right_bound_atom))
        VLO_ADD_STRING (*representation, "->");
      else
        VLO_ADD_STRING (*representation, "-");
      identifier_or_literal_representation (IR_right_bound (sequence_element),
                                            FALSE, representation);
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_identifier_or_literal_atom))
    identifier_or_literal_representation
      (IR_identifier_or_literal (sequence_element), FALSE, representation);
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_string_atom))
    VLO_ADD_STRING (*representation,
                    IR_string_representation (IR_string (sequence_element)));
  else
    assert (IR_IS_OF_TYPE (sequence_element, IR_NM_code_insertion_atom));
}

/* The following function adds representation of given rule
   alternative to given variable length object. */

static void
alternative_representation (IR_node_t pattern, vlo_t *representation)
{
  IR_node_t current_sequence_element;
  IR_node_t next_sequence_element;

  if (IR_IS_OF_TYPE (pattern, IR_NM_separator_iteration))
    {
      sequence_element_representation (IR_iteration_sequence (pattern),
                                       representation);
      VLO_ADD_STRING (*representation, " / ");
      sequence_element_representation (IR_separator_sequence (pattern),
                                       representation);
    }
  else
    {
      assert (IR_IS_OF_TYPE (pattern, IR_NM_sequence));
      for (current_sequence_element = IR_sequence (pattern);
           current_sequence_element != NULL;
           current_sequence_element = next_sequence_element)
        {
          if (current_sequence_element != IR_sequence (pattern))
            VLO_ADD_STRING (*representation, " ");
          sequence_element_representation (current_sequence_element,
                                           representation);
          next_sequence_element
            = IR_next_sequence_element (current_sequence_element);
        }
    }
}

/* The following function adds representation of given rule pattern to
   given variable length object. */

static void
pattern_representation (IR_node_t pattern, vlo_t *representation)
{
  IR_node_t current_alternative;

  if (IR_IS_OF_TYPE (pattern, IR_NM_sequence_element))
    sequence_element_representation (pattern, representation);
  else if (IR_IS_OF_TYPE (pattern, IR_NM_alternative))
    alternative_representation (pattern, representation);
  else
    {
      assert (IR_IS_OF_TYPE (pattern, IR_NM_pattern));
      /* This part can be called only recursively. */
      current_alternative = IR_alternatives_list (pattern);
      alternative_representation (current_alternative, representation);
      for (;;)
        {
          current_alternative = IR_next_alternative (current_alternative);
          if (current_alternative == NULL)
            break;
          VLO_ADD_STRING (*representation, " | ");
          alternative_representation (current_alternative, representation);
        }
    }
}

/* The following function adds representation of given canonical rule
   to given variable length object.  If value of the parameter
   `full_representation_flag' is TRUE then the representation contains
   pattern instead of name of nonterminal corresponding to the
   pattern. */

static void
canonical_rule_representation (IR_node_t canonical_rule,
                               int full_representation_flag,
                               vlo_t *representation)
{
  IR_node_t left_hand_side;
  IR_node_t current_right_hand_side_element;

  left_hand_side = IR_left_hand_side (canonical_rule);
  single_definition_representation (left_hand_side, representation);
  VLO_ADD_STRING (*representation, " :");
  for (current_right_hand_side_element
       = IR_right_hand_side (canonical_rule);
       !IR_IS_OF_TYPE (current_right_hand_side_element,
                           IR_NM_canonical_rule_end);
       current_right_hand_side_element
       = IR_next_right_hand_side_element (current_right_hand_side_element))
    {
      VLO_ADD_STRING (*representation, " ");
      if (full_representation_flag
          && IR_corresponding_pattern (left_hand_side) != NULL)
        pattern_representation (IR_corresponding_pattern (left_hand_side),
                                representation);
      else
        single_definition_representation
          (IR_element_itself (current_right_hand_side_element),
           representation);
    }
}

void
output_grammar (void)
{
  IR_node_t current_canonical_rule;
  IR_node_t left_hand_side;
  vlo_t representation;

  for (current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    {
      output_decimal_number
        (output_description_file,
         IR_canonical_rule_order_number (current_canonical_rule), 6);
      output_string (output_description_file, "  ");
      left_hand_side = IR_left_hand_side (current_canonical_rule);
      VLO_CREATE (representation, 100);
      canonical_rule_representation (current_canonical_rule,
                                     FALSE, &representation);
      output_string (output_description_file, VLO_BEGIN (representation));
      VLO_DELETE (representation);
      output_char ('\n', output_description_file);
      if (IR_corresponding_pattern (left_hand_side) != NULL
          && !IR_pattern_has_been_output (left_hand_side)
          && !IR_IS_OF_TYPE (IR_corresponding_pattern (left_hand_side),
                             IR_NM_code_insertion_atom))
        {
          /* Output pattern designated given nonterminal. */
          output_string (output_description_file, "          Here ");
          output_single_definition (output_description_file, left_hand_side);
          output_string (output_description_file, " denotes ");
          VLO_CREATE (representation, 100);
          VLO_ADD_STRING (representation, "");
          pattern_representation (IR_corresponding_pattern (left_hand_side),
                                  &representation);
          output_string (output_description_file, VLO_BEGIN (representation));
          VLO_DELETE (representation);
          output_char ('\n', output_description_file);
          IR_set_pattern_has_been_output (left_hand_side, TRUE);
        }
    }
}

static void
output_conflicts (IR_node_t LR_set)
{
  IR_node_t current_conflict;
  IR_node_t goto_LR_set;

  for (current_conflict = IR_conflicts_list (LR_set);
       current_conflict != NULL;
       current_conflict = IR_next_conflict (current_conflict))
    {
      if (IR_IS_OF_TYPE (IR_element_after_dot (IR_used_LR_situation
                                               (current_conflict)),
                         IR_NM_canonical_rule_end)
          && IR_IS_OF_TYPE (IR_element_after_dot (IR_unused_LR_situation
                                                  (current_conflict)),
                            IR_NM_canonical_rule_end))
        {
          output_string (output_description_file,
                         "  reduce/reduce conflict (reduce ");
          output_decimal_number
            (output_description_file,
             IR_canonical_rule_order_number
             (IR_canonical_rule (IR_element_after_dot
                                 (IR_used_LR_situation (current_conflict)))),
             0);
          output_string (output_description_file, ", reduce ");
          output_decimal_number
            (output_description_file,
             IR_canonical_rule_order_number
             (IR_canonical_rule (IR_element_after_dot
                                 (IR_unused_LR_situation (current_conflict)))),
             0);
        }
      else
        {
          output_string (output_description_file, "  shift/reduce conflict (");
          if (IR_IS_OF_TYPE (IR_element_after_dot (IR_used_LR_situation
                                                   (current_conflict)),
                             IR_NM_canonical_rule_end))
            {
              output_string (output_description_file, "reduce ");
              output_decimal_number
                (output_description_file,
                 IR_canonical_rule_order_number
                 (IR_canonical_rule
                  (IR_element_after_dot
                   (IR_used_LR_situation (current_conflict)))),
                 0);
            }
          else
            {
              output_string (output_description_file, "shift ");
              goto_LR_set = IR_goto_LR_set (IR_used_LR_situation
                                            (current_conflict));
              if (goto_LR_set == NULL || !IR_reachable_flag (goto_LR_set))
                output_string (output_description_file,
                               " (state is now unaccesible)");
              else
                output_decimal_number
                  (output_description_file,
                   IR_LR_set_order_number (goto_LR_set), 0);
            }
          if (IR_IS_OF_TYPE (IR_element_after_dot (IR_used_LR_situation
                                                   (current_conflict)),
                             IR_NM_canonical_rule_end))
            {
              output_string (output_description_file, ", shift ");
              goto_LR_set = IR_goto_LR_set (IR_unused_LR_situation
                                            (current_conflict));
              if (goto_LR_set == NULL || !IR_reachable_flag (goto_LR_set))
                output_string (output_description_file,
                               " (state is now unaccesible)");
              else
                output_decimal_number
                  (output_description_file,
                   IR_LR_set_order_number (goto_LR_set), 0);
            }
          else
            {
              output_string (output_description_file, ", reduce ");
              output_decimal_number
                (output_description_file,
                 IR_canonical_rule_order_number
                 (IR_canonical_rule (IR_element_after_dot
                                     (IR_unused_LR_situation
                                      (current_conflict)))),
                 0);
            }
        }
      output_string (output_description_file, ") on ");
      output_token_string (IR_token_string (current_conflict),
                           output_description_file);
      output_char ('\n', output_description_file);
    }
}

/* The following variable contains array of single term definitions
   which is output trie nodes path. */

static vlo_t current_output_trie_nodes_path;

static int
output_trie_nodes_path (void)
{
  IR_node_t *current_single_term_definition_ptr;
  int there_is_output;

  there_is_output = FALSE;
  for (current_single_term_definition_ptr
       = VLO_BEGIN (current_output_trie_nodes_path);
       current_single_term_definition_ptr
       <= (IR_node_t *) VLO_END (current_output_trie_nodes_path);
       current_single_term_definition_ptr++)
    {
      if (there_is_output)
        output_char (' ', output_description_file);
      there_is_output = TRUE;
      output_single_definition (output_description_file,
                                *current_single_term_definition_ptr);
    }
  return there_is_output;
}

static int
actions_are_absent (IR_node_t regular_arc)
{
  IR_node_t current_list_element;

  for (current_list_element = IR_first_rule_list_element (regular_arc);
       current_list_element != NULL;
       current_list_element = IR_next_rule_list_element (current_list_element))
    if (IR_action (IR_canonical_rule (current_list_element)) != NULL)
      return FALSE;
  return TRUE;
}

static int
abs_int (int i)
{
  if (i < 0)
    return -i;
  else
    return i;
}

static void
output_trie_level_transition (IR_node_t first_trie_node)
{
  int default_was_processed;
  IR_node_t current_trie_node;
  IR_node_t current_LR_situation;
  IR_node_t single_term_definition;
  IR_node_t regular_arc;
  IR_node_t current_list_element;
  int actions_absence_flag;

  default_was_processed = FALSE;
  for (current_trie_node = first_trie_node;
       current_trie_node != NULL;
       current_trie_node = IR_next_brother (current_trie_node))
    {
      single_term_definition
        = IR_corresponding_single_term_definition (current_trie_node);
      if (IR_first_son (current_trie_node) != NULL)
        {
          VLO_ADD_MEMORY (current_output_trie_nodes_path,
                          &single_term_definition, sizeof (IR_node_t));
          output_trie_level_transition (IR_first_son (current_trie_node));
          VLO_SHORTEN (current_output_trie_nodes_path, sizeof (IR_node_t));
        }
      else
        {
          output_char ('\t', output_description_file);
          if (output_trie_nodes_path ())
            output_char (' ', output_description_file);
          if (single_term_definition == NULL)
            {
              default_was_processed = TRUE;
              output_char ('.', output_description_file);
            }
          else
            output_single_definition (output_description_file,
                                      single_term_definition);
          current_LR_situation
            = IR_corresponding_LR_situation (current_trie_node);
          if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                             IR_NM_canonical_rule_end))
            {
              if (IR_corresponding_regular_arc (current_LR_situation) == NULL)
                {
                  output_string (output_description_file, "  reduce ");
                  output_decimal_number
                    (output_description_file,
                     IR_canonical_rule_order_number
                     (IR_canonical_rule (IR_element_after_dot
                                         (current_LR_situation))),
                     0);
                }
            }
          else
            {
              if (IR_corresponding_regular_arc (current_LR_situation) == NULL)
                {
                  assert (IR_first_symbol_LR_situation (current_LR_situation));
                  output_string (output_description_file, "  shift ");
                  output_decimal_number
                    (output_description_file,
                     IR_LR_set_order_number
                     (IR_goto_LR_set (current_LR_situation)),
                     0);
                }
            }
          regular_arc = IR_corresponding_regular_arc (current_LR_situation);
          if (regular_arc != NULL)
            {
              actions_absence_flag = actions_are_absent (regular_arc);
              output_char (' ', output_description_file);
              if (IR_regular_arc_popped_LR_sets_number (regular_arc) != 0)
                {
                  if (IR_regular_arc_popped_LR_sets_number (regular_arc) > 0)
                    output_string (output_description_file, " pop ");
                  else
                    output_string (output_description_file, " push ");
                  output_decimal_number
                    (output_description_file,
                     abs_int
                     (IR_regular_arc_popped_LR_sets_number (regular_arc)),
                     0);
                  if (IR_regular_arc_popped_attributes_number (regular_arc)
                      != 0
                      || !actions_absence_flag
                      || IR_terminal_marking_arc (regular_arc) != NULL)
                    output_string (output_description_file, " states,");
                  else
                    output_string (output_description_file, " states and");
                }
              if (IR_regular_arc_popped_attributes_number (regular_arc) != 0)
                {
                  if (IR_regular_arc_popped_attributes_number (regular_arc)
                      < 0)
                    {
                      output_string (output_description_file, " push ");
                      output_decimal_number
                        (output_description_file,
                         - IR_regular_arc_popped_attributes_number
                           (regular_arc),
                         0);
                    }
                  else
                    {
                      output_string (output_description_file, " pop ");
                      output_decimal_number
                        (output_description_file,
                         IR_regular_arc_popped_attributes_number (regular_arc),
                         0);
                    }
                  if (!actions_absence_flag
                      || IR_terminal_marking_arc (regular_arc) != NULL)
                    output_string (output_description_file, " attributes,");
                  else
                    output_string (output_description_file, " attributes and");
                }
              if (IR_terminal_marking_arc (regular_arc) != NULL)
                {
                  output_string (output_description_file, " shift");
                  if (!actions_absence_flag)
                    output_string (output_description_file, ",");
                  else
                    output_string (output_description_file, " and");
                }
              output_string (output_description_file, " goto ");
              output_decimal_number
                (output_description_file,
                 IR_LR_set_order_number (IR_to_LR_set (regular_arc)), 0);
              if (!actions_absence_flag)
                {
                  output_string (output_description_file, " and action for");
                  for (current_list_element
                         = IR_first_rule_list_element (regular_arc);
                       current_list_element != NULL;
                       current_list_element
                         = IR_next_rule_list_element (current_list_element))
                    if (IR_action (IR_canonical_rule (current_list_element))
                        != NULL)
                      {
                        output_char (' ', output_description_file);
                        output_decimal_number
                          (output_description_file,
                           IR_canonical_rule_order_number
                           (IR_canonical_rule (current_list_element)),
                           0);
                      }
                }
            }
          output_char ('\n', output_description_file);
        }
    }
  if (!default_was_processed)
    {
      output_char ('\t', output_description_file);
      if (output_trie_nodes_path ())
        output_char (' ', output_description_file);
      output_string (output_description_file, ".  error\n");
    }
}

static void
output_LR_set_transitions (IR_node_t LR_set)
{
  IR_node_t current_LR_situation;
  int goto_output_flag;

  if (characteristic_symbol_of_LR_set (LR_set) == end_marker_single_definition
      && IR_IS_OF_TYPE (IR_element_after_dot (IR_LR_situation_list (LR_set)),
                                              IR_NM_canonical_rule_end))
    output_string (output_description_file, "\t.  accept\n");
  else
    {
      /* Output actions. */
      VLO_CREATE (current_output_trie_nodes_path, 100);
      output_trie_level_transition (IR_LR_set_look_ahead_trie (LR_set));
      VLO_DELETE (current_output_trie_nodes_path);
    }
  output_char ('\n', output_description_file);
  /* Output gotos .*/
  goto_output_flag = FALSE;
  for (current_LR_situation = IR_LR_situation_list (LR_set);
       current_LR_situation != NULL;
       current_LR_situation = IR_next_LR_situation (current_LR_situation))
    if (!IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                        IR_NM_canonical_rule_end)
        && IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                             (current_LR_situation)),
                          IR_NM_single_nonterm_definition)
        && IR_first_symbol_LR_situation (current_LR_situation)
        && !IR_goto_arc_has_been_removed (current_LR_situation))
      {
        goto_output_flag = TRUE;
        output_char ('\t', output_description_file);
        output_single_definition
          (output_description_file,
           IR_element_itself (IR_element_after_dot (current_LR_situation)));
        output_string (output_description_file, "  goto ");
        output_decimal_number
          (output_description_file,
           IR_LR_set_order_number (IR_goto_LR_set (current_LR_situation)), 0);
        output_char ('\n', output_description_file);
      }
  output_string (output_description_file, (goto_output_flag ? "\n\n" : "\n"));
}

void
output_states (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;

  output_char ('\n', output_description_file);
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          output_conflicts (current_LR_set);
          output_string (output_description_file, "State ");
          output_decimal_number (output_description_file,
                                 IR_LR_set_order_number (current_LR_set), 0);
          if (regular_optimization_flag)
            {
              if (!IR_it_is_pushed_LR_set (current_LR_set))
                output_string (output_description_file,
                               " (is not pushed into the stack)");
              if (!IR_attribute_is_used (current_LR_set))
                output_string (output_description_file,
                               " (attribute is not used)");
            }
          output_string (output_description_file, "\n\n");
          output_LR_set_situations (output_description_file, current_LR_set,
                                    "\t");
          output_LR_set_transitions (current_LR_set);
        }
#ifndef NDEBUG
      else if (debug_level >= 2)
        {
          output_string (stderr, "Unreachable State:");
          if (regular_optimization_flag)
            {
              if (!IR_it_is_pushed_LR_set (current_LR_set))
                output_string (stderr, " (is not pushed into the stack)");
              if (!IR_attribute_is_used (current_LR_set))
                output_string (stderr, " (attribute is not used)");
            }
          output_string (stderr, "\n\n");
          output_LR_set_situations (stderr, current_LR_set, "\t");
        }
#endif
}

void
output_summary (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t last_LR_set;
  int shift_reduce_conflicts_number;
  int reduce_reduce_conflicts_number;
  int output_conflict_flag;
  int unmerged_LR_cores_number;

  output_conflict_flag = FALSE;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          LR_set_conflicts_number (current_LR_set,
                                   &shift_reduce_conflicts_number,
                                   &reduce_reduce_conflicts_number);
          if (shift_reduce_conflicts_number + reduce_reduce_conflicts_number
              != 0)
            {
              output_conflict_flag = TRUE;
              output_string (output_description_file, "State ");
              output_decimal_number
                (output_description_file,
                 IR_LR_set_order_number (current_LR_set), 0);
              output_string (output_description_file, " contains");
              if (shift_reduce_conflicts_number != 0)
                {
                  output_char (' ', output_description_file);
                  output_decimal_number (output_description_file,
                                         shift_reduce_conflicts_number, 0);
                  output_string (output_description_file, " shift/reduce");
                }
              if (reduce_reduce_conflicts_number != 0)
                {
                  output_char (' ', output_description_file);
                  if (shift_reduce_conflicts_number != 0)
                    output_string (output_description_file, " and ");
                  else
                    output_char (' ', output_description_file);
                  output_decimal_number (output_description_file,
                                         reduce_reduce_conflicts_number, 0);
                  output_string (output_description_file, " reduce/reduce");
                }
              if (shift_reduce_conflicts_number
                  + reduce_reduce_conflicts_number != 1)
                output_string (output_description_file, " conflicts.\n");
              else
                output_string (output_description_file, " conflict.\n");
            }
        }
  if (output_conflict_flag)
    output_string (output_description_file, "\n\n");
  if (lr_flag && lalr_optimization_flag)
    {
      unmerged_LR_cores_number = 0;
      for (current_LR_core = IR_LR_core_list (description);
           current_LR_core != NULL;
           current_LR_core = IR_next_LR_core (current_LR_core))
        {
          for (current_LR_set = IR_LR_set_list (current_LR_core);
               current_LR_set != NULL;
               current_LR_set = IR_next_LR_set (current_LR_set))
            last_LR_set = current_LR_set;
          if (last_LR_set != IR_LR_set_list (current_LR_core))
            {
              output_string (output_description_file, "States ");
              output_decimal_number
                (output_description_file,
                 IR_LR_set_order_number (IR_LR_set_list (current_LR_core)), 0);
              output_char ('-', output_description_file);
              output_decimal_number (output_description_file,
                                     IR_LR_set_order_number (last_LR_set), 0);
              output_string
                (output_description_file,
                 " of a LR-core are not merged during LALR-optimization\n");
              unmerged_LR_cores_number++;
            }
        }
      if (unmerged_LR_cores_number != 0)
        {
          output_string (output_description_file, "\n\n");
          output_decimal_number (output_description_file,
                                 unmerged_LR_cores_number, 0);
          output_string
            (output_description_file,
             " LR-cores are not merged during LALR-optimization\n\n");
        }
      else
        output_string
          (output_description_file,
           "All LR-cores are merged during LALR-optimization\n\n");
    }
  output_decimal_number (output_description_file,
                         IR_tokens_number (description), 0);
  output_string (output_description_file,
                 " terminals (including `error' and EOF), ");
  output_decimal_number (output_description_file,
                         IR_token_equivalence_classes_number (description), 0);
  output_string (output_description_file, " terminal equivalence classes\n");
  output_decimal_number (output_description_file,
                         IR_nonterminals_number (description), 0);
  output_string (output_description_file, " nonterminals, ");
  output_decimal_number (output_description_file,
                         IR_canonical_rules_number (description), 0);
  output_string (output_description_file, " rules");
  if (IR_duplicated_patterns_number (description) != 0)
    {
      output_string (output_description_file, ", ");
      output_decimal_number (output_description_file,
			     IR_duplicated_patterns_number (description), 0);
      output_string (output_description_file, " removed duplicated patterns");
    }
  output_string (output_description_file, "\n");
  output_decimal_number (output_description_file,
                         important_original_LR_situations_number, 0);
  if (!lr_flag)
    output_string (output_description_file,
                   " important LALR-situations, ");
  else
    output_string (output_description_file,
                   " important canonical LR-situations, ");
  output_decimal_number (output_description_file,
                         all_original_LR_situations_number, 0);
  if (!lr_flag)
    output_string (output_description_file, " all LALR-situations\n");
  else
    output_string (output_description_file, " all canonical LR-situations\n");
  output_decimal_number (output_description_file, original_LR_sets_number, 0);
  if (!lr_flag)
    output_string (output_description_file, " LALR-sets, ");
  else
    {
      output_string (output_description_file, " canonical LR-sets, ");
      output_decimal_number (output_description_file,
                             original_LR_cores_number, 0);
      output_string (output_description_file, " canonical LR-cores, ");
    }
  if (regular_optimization_flag && split_lr_sets_flag)
    {
      output_decimal_number (output_description_file,
                             IR_splitted_LR_sets_number (description), 0);
      output_string (output_description_file, " new splitted LR-sets, ");
    }
  output_decimal_number (output_description_file,
                         IR_LR_sets_number (description), 0);
  output_string (output_description_file, " final states");
  if (regular_optimization_flag)
    {
      output_string (output_description_file, " (");
      output_decimal_number (output_description_file,
                             IR_pushed_LR_sets_number (description), 0);
      output_string (output_description_file, " pushed states)");
    }
  output_string (output_description_file, "\n");
  if (IR_all_number_of_regular_arcs (description) != 0)
    {
      output_decimal_number (output_description_file,
                             IR_all_number_of_regular_arcs (description), 0);
      output_string (output_description_file, " all regular arcs, ");
      output_decimal_number (output_description_file,
                             IR_number_of_regular_arcs (description), 0);
      output_string (output_description_file,
                     " regular arc equivalence classes\n");
    }
  if (regular_optimization_flag && IR_duplicated_actions (description) != 0)
    {
      output_decimal_number (output_description_file,
                             IR_duplicated_actions (description), 0);
      output_string (output_description_file, " duplicated actions\n");
    }
}

