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
#include "contexts.h"
#include "relation.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif



/* The following variable value is number of the pass to evaluate
   minimal derived string length of a nonterminal. */

static int minimal_derived_string_length_pass_number;

/* Remember that the minimal derived string length of a nonterminal
   may be fully evaluated on the previous passes. */

static int
set_nonterm_minimal_derived_string_length (IR_node_t single_nonterm_definition)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_right_hand_side_element;
  IR_node_t current_single_definition;
  int minimal_derived_string_length;
  int recursive_nonterminal_was_processed;

  recursive_nonterminal_was_processed
    = (IR_pass_number (single_nonterm_definition)
       == minimal_derived_string_length_pass_number);
  if (!IR_process_nonterminal_on_its_process_pass (single_nonterm_definition)
      || recursive_nonterminal_was_processed)
    return recursive_nonterminal_was_processed;
  IR_set_process_nonterminal_on_its_process_pass (single_nonterm_definition,
                                                  FALSE);
  IR_set_pass_number (single_nonterm_definition,
                      minimal_derived_string_length_pass_number);
  for (current_canonical_rule
       = IR_nonterm_canonical_rule_list (single_nonterm_definition);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_nonterm_canonical_rule (current_canonical_rule))
    {
      minimal_derived_string_length = 0;
      for (current_right_hand_side_element
           = IR_right_hand_side (current_canonical_rule);
           !IR_IS_OF_TYPE (current_right_hand_side_element,
                           IR_NM_canonical_rule_end);
           current_right_hand_side_element
           = IR_next_right_hand_side_element (current_right_hand_side_element))
        {
          current_single_definition
            = IR_element_itself (current_right_hand_side_element);
          if (IR_IS_OF_TYPE (current_single_definition,
                             IR_NM_single_nonterm_definition))
            {
              if (IR_pass_number (current_single_definition)
                  == minimal_derived_string_length_pass_number
                  || set_nonterm_minimal_derived_string_length
                     (current_single_definition))
                recursive_nonterminal_was_processed = TRUE;
              if (IR_minimal_derived_string_length (current_single_definition)
                  < 0)
                /* Do not process this recursive rule:
                   other alternatives are not processed. */
                break;
              minimal_derived_string_length
                += IR_minimal_derived_string_length
                   (current_single_definition);
            }
          else
            minimal_derived_string_length++;
        }
      if (IR_IS_OF_TYPE (current_right_hand_side_element,
                         IR_NM_canonical_rule_end)
          && (IR_minimal_derived_string_length (single_nonterm_definition) < 0
              || (IR_minimal_derived_string_length (single_nonterm_definition)
                  > minimal_derived_string_length)))
          IR_set_minimal_derived_string_length (single_nonterm_definition,
                                                minimal_derived_string_length);
    }
  if (recursive_nonterminal_was_processed)
    /* Recursive nonterminal: process the nonterminal on the next
       passes because it is possible that the nonterminal processing
       pass has not been yet.  For example, this is neccessary for the
       following situation:
       s : ... a ... | ... a ...
               |
               |
               v
       a : ... s ... 
       Here `a' is `single_nonterm_definition',
       `s' is `current_single_definition'. */
    IR_set_process_nonterminal_on_its_process_pass (single_nonterm_definition,
                                                    TRUE);
  return recursive_nonterminal_was_processed;
}

void
evaluate_minimal_derived_string_length (void)
{
  IR_node_t current_single_definition;

  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition))
      {
        IR_set_pass_number (current_single_definition, 0); 
        IR_set_process_nonterminal_on_its_process_pass
          (current_single_definition, TRUE);
      }
  for (minimal_derived_string_length_pass_number = 1,
       current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       minimal_derived_string_length_pass_number++,
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition))
      set_nonterm_minimal_derived_string_length (current_single_definition);
}



/* The following variable value is number of the pass to evaluate
   relation FIRST of a nonterminal. */

static int relation_FIRST_pass_number;

/* Remember that the relation FIRST of a nonterminal may be fully
   evaluated on the previous passes. */

static int
set_nonterm_relation_FIRST (IR_node_t single_nonterm_definition)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_right_hand_side_element;
  context_t one_context_string_context;
  token_string_t token_string;
  int minimal_context_length;
  context_t temporary_context;
  IR_node_t current_single_definition;
  int recursive_nonterminal_was_processed;

  recursive_nonterminal_was_processed
    = IR_pass_number (single_nonterm_definition) == relation_FIRST_pass_number;
  if (!IR_process_nonterminal_on_its_process_pass (single_nonterm_definition)
      || recursive_nonterminal_was_processed)
    return recursive_nonterminal_was_processed;
  IR_set_process_nonterminal_on_its_process_pass (single_nonterm_definition,
                                                  FALSE);
  IR_set_pass_number (single_nonterm_definition, relation_FIRST_pass_number);
  for (current_canonical_rule
       = IR_nonterm_canonical_rule_list (single_nonterm_definition);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_nonterm_canonical_rule (current_canonical_rule))
    {
      temporary_context = get_null_context ();
      minimal_context_length = 0;
      for (current_right_hand_side_element
           = IR_right_hand_side (current_canonical_rule);
           minimal_context_length < max_look_ahead_number
           && !IR_IS_OF_TYPE (current_right_hand_side_element,
                              IR_NM_canonical_rule_end);
           current_right_hand_side_element
           = IR_next_right_hand_side_element (current_right_hand_side_element))
        {
          current_single_definition
            = IR_element_itself (current_right_hand_side_element);
          if (IR_IS_OF_TYPE (current_single_definition,
                             IR_NM_single_nonterm_definition))
            {
              if (IR_pass_number (current_single_definition)
                  == relation_FIRST_pass_number
                  || set_nonterm_relation_FIRST (current_single_definition))
                recursive_nonterminal_was_processed = TRUE;
              context_concat (temporary_context,
                              IR_relation_FIRST (current_single_definition),
                              max_look_ahead_number);
              if (minimal_context_length == 0)
                context_or (temporary_context,
                            IR_relation_FIRST (current_single_definition));
              minimal_context_length
                += IR_minimal_derived_string_length
                  (current_single_definition);
            }
          else
            {
              token_string
                = get_new_token_string (&current_single_definition, 1);
              one_context_string_context = get_null_context ();
              set_context_element_value
                (one_context_string_context, token_string, 1);
              context_concat (temporary_context, one_context_string_context,
                              max_look_ahead_number);
              free_context (one_context_string_context);
              if (minimal_context_length == 0)
                set_context_element_value
                  (temporary_context, token_string, 1);
              minimal_context_length++;
            }
        }
      context_or (IR_relation_FIRST (single_nonterm_definition),
                  temporary_context);
      free_context (temporary_context);
    }
  if (recursive_nonterminal_was_processed)
    /* Recursive nonterminal: process the nonterminal on the next
       passes because it is possible that the nonterminal processing
       pass has not been yet.  For example, this is neccessary for the
       following situation:
                   s : ... a ... | ... a ...
                           |
                           |
                           v
                   a : ... s ... 
       Here `a' is `single_nonterm_definition', `s' is
       `current_single_definition'. */
    IR_set_process_nonterminal_on_its_process_pass (single_nonterm_definition,
                                                    TRUE);
  return recursive_nonterminal_was_processed;
}

void
evaluate_nonterminals_relation_FIRST (void)
{
  IR_node_t current_single_definition;

  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition))
      {
        IR_set_pass_number (current_single_definition, 0); 
        IR_set_process_nonterminal_on_its_process_pass
          (current_single_definition, TRUE);
        if (IR_relation_FIRST (current_single_definition) == NULL)
          IR_set_relation_FIRST (current_single_definition,
                                 get_null_context ());
      }
  for (relation_FIRST_pass_number = 1,
       current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       relation_FIRST_pass_number++,
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition))
      set_nonterm_relation_FIRST (current_single_definition);
}



/* The following function returns FIRST (string starting with given
   right hand side element concatenated with given context). */

void
set_FIRST_of_rule_tail (IR_node_t right_hand_side_element)
{
  IR_node_t next_right_hand_side_element;
  context_t context;
  IR_node_t single_definition;
  token_string_t token_string;
  int minimal_context_length;

  context = get_null_context ();
  if (!IR_IS_OF_TYPE (right_hand_side_element, IR_NM_canonical_rule_end))
    {
      single_definition = IR_element_itself (right_hand_side_element);
      if (IR_IS_OF_TYPE (single_definition, IR_NM_single_term_definition))
        {
          token_string = get_new_token_string (&single_definition, 1);
          set_context_element_value (context, token_string, 1);
          minimal_context_length = 1;
        }
      else
        {
          assert (IR_IS_OF_TYPE (single_definition,
                                 IR_NM_single_nonterm_definition));
          context_copy (context, IR_relation_FIRST (single_definition));
          minimal_context_length
            = IR_minimal_derived_string_length (single_definition);
        }
      next_right_hand_side_element
        = IR_next_right_hand_side_element (right_hand_side_element);
      assert (next_right_hand_side_element != NULL);
      if (!IR_IS_OF_TYPE (next_right_hand_side_element,
                          IR_NM_canonical_rule_end))
        {
          if (minimal_context_length < max_look_ahead_number)
            context_concat
              (context, IR_FIRST_of_rule_tail (next_right_hand_side_element),
               max_look_ahead_number);
          if (minimal_context_length == 0)
            context_or (context,
                        IR_FIRST_of_rule_tail (next_right_hand_side_element));
          minimal_context_length
            += IR_minimal_FIRST_of_rule_tail_length
              (next_right_hand_side_element);
        }
    }
  else
    minimal_context_length = 0;
  IR_set_minimal_FIRST_of_rule_tail_length (right_hand_side_element,
                                            minimal_context_length);
  IR_set_FIRST_of_rule_tail (right_hand_side_element,
                             insert_or_free_context (context));
}

void
set_FIRST_of_rule_tails (void)
{
  IR_node_t current_canonical_rule;
  IR_node_t *current_right_hand_side_element_ptr;
  IR_node_t current_right_hand_side_element;
  vlo_t elements_in_reverse_order;

  VLO_CREATE (elements_in_reverse_order, 0);
  for (current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    {
      VLO_NULLIFY (elements_in_reverse_order);
      for (current_right_hand_side_element
           = IR_right_hand_side (current_canonical_rule);
           current_right_hand_side_element != NULL;
           current_right_hand_side_element
           = IR_next_right_hand_side_element (current_right_hand_side_element))
        VLO_ADD_MEMORY (elements_in_reverse_order,
                        &current_right_hand_side_element, sizeof (IR_node_t));
      for (current_right_hand_side_element_ptr
           = (IR_node_t *) ((char *) VLO_END (elements_in_reverse_order)
                            - (sizeof (IR_node_t) - 1));
           current_right_hand_side_element_ptr
           >= (IR_node_t *) VLO_BEGIN (elements_in_reverse_order);
           current_right_hand_side_element_ptr--)
        set_FIRST_of_rule_tail (*current_right_hand_side_element_ptr);
    }
  VLO_DELETE (elements_in_reverse_order);
}

/* The following function returns FIRST (string starting with given
   right hand side element concatenated with given context).  Remember
   that the result is not in the context table. */

context_t
FIRST_of_tail (IR_node_t right_hand_side_element, context_t context,
               int context_length)
{
  context_t result;
  context_t temp_context;
  int minimal_context_length;

  result = get_null_context ();
  context_copy (result, IR_FIRST_of_rule_tail (right_hand_side_element));
  minimal_context_length
    = IR_minimal_FIRST_of_rule_tail_length (right_hand_side_element);
  if (context != NULL && minimal_context_length < context_length)
    {
      context_concat (result, context, context_length);
      if (minimal_context_length == 0)
        context_or (result, context);
    }
  if (context_length != max_look_ahead_number)
    {
      temp_context = result;
      result = context_shortening (result, context_length);
      free_context (temp_context);
    }
  return result;
}
