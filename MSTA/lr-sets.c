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

/* Make equivalent reduces???? Case ouput regular arc action which
   corresponds shift and reducing by empty rule????  Additional
   criteria of regular arcs equivalence is absence of $n when n
   <=0???? */

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#include <stdlib.h>
#include <stdio.h>
#include "hashtab.h"
#include "vlobject.h"
#include "common.h"
#include "ird.h"
#include "output.h"
#include "contexts.h"
#include "lr-sets.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif




/* The following variable value is list of free LR-situations by field
   `next_LR_situation'. */

static IR_node_t list_of_free_LR_situations;

void
initiate_LR_situations (void)
{
  list_of_free_LR_situations = NULL;
}

IR_node_t
get_new_LR_situation (IR_node_t element_after_dot, context_t context,
                      IR_node_t LR_set, IR_node_t next_LR_situation,
                      int important_LR_situation_flag)
{
  IR_node_t result;

  if (list_of_free_LR_situations == NULL)
    result = IR_new_LR_situation (element_after_dot, context, LR_set,
                                  next_LR_situation);
  else
    {
      result = list_of_free_LR_situations;
      list_of_free_LR_situations
        = IR_next_LR_situation (list_of_free_LR_situations);
      IR_set_element_after_dot (result, element_after_dot);
      IR_set_context (result, context);
      IR_set_LR_set (result, LR_set);
      IR_set_next_LR_situation (result, next_LR_situation);
      IR_set_goto_LR_set (result, NULL);
      IR_set_goto_arc_has_been_removed (result, FALSE);
      IR_set_look_ahead_context (result, NULL);
      IR_set_next_symbol_LR_situation (result, NULL);
      IR_set_situation_in_stack_flag (result, FALSE);
      IR_set_corresponding_regular_arc (result, NULL);
    }
  IR_set_important_LR_situation_flag (result, important_LR_situation_flag);
  return result;
}

void
free_LR_situation (IR_node_t LR_situation)
{
  /* The following is necessary because this is double linked field
     (see SPRUT documentation). */
  IR_set_goto_LR_set (LR_situation, NULL);
  IR_set_next_LR_situation (LR_situation, list_of_free_LR_situations);
  list_of_free_LR_situations = LR_situation;
}

void
free_LR_situations_list (IR_node_t LR_situations_list)
{
  IR_node_t current_LR_situation;
  IR_node_t next_LR_situation;

  for (current_LR_situation = LR_situations_list;
       current_LR_situation != NULL;
       current_LR_situation = next_LR_situation)
    {
      next_LR_situation = IR_next_LR_situation (current_LR_situation);
      free_LR_situation (current_LR_situation);
    }
}

void
output_LR_situation (FILE *f, IR_node_t LR_situation, const char *indent,
                     int new_line_flag)
{
  IR_node_t canonical_rule;
  IR_node_t left_hand_side;
  IR_node_t current_right_hand_side_element;

  output_string (f, indent);
  canonical_rule = IR_canonical_rule (IR_element_after_dot (LR_situation));
  left_hand_side = IR_left_hand_side (canonical_rule);
  output_single_definition (f, left_hand_side);
  output_string (f, " :");
  current_right_hand_side_element = IR_right_hand_side (canonical_rule);
  for (;;)
    {
      if (IR_element_after_dot (LR_situation)
          == current_right_hand_side_element)
        output_string
          (f, IR_under_control_point_flag (LR_situation) ? " ?" : " .");
      if (IR_IS_OF_TYPE (current_right_hand_side_element,
                         IR_NM_canonical_rule_end))
        break;
      output_char (' ', f);
      output_single_definition
        (f, IR_element_itself (current_right_hand_side_element));
      current_right_hand_side_element
        = IR_next_right_hand_side_element (current_right_hand_side_element);
    }
  if (lr_situation_context_flag)
    {
      assert (IR_context (LR_situation) != NULL);
      output_string (f, "  ");
      output_context (f, IR_context (LR_situation));
    }
  output_string (f, "  (");
  output_decimal_number (f, IR_canonical_rule_order_number (canonical_rule),
                         0);
  output_char (')', f);
  if (new_line_flag)
    output_char ('\n', f);
}

void
finish_LR_situations (void)
{
}



/* This page contains abstract data `table of LR-cores'.  Elements of
   the table are nodes representing LR-cores.  Key of the table
   elements is important LR-situations (right hand side element after
   dot) of the LR-cores. */

static unsigned
LR_core_hash_function (hash_table_entry_t LR_core)
{
  unsigned hash_value;
  IR_node_t LR_situation_list;

  hash_value = 0;
  LR_situation_list
    = IR_LR_situation_list (IR_LR_set_list ((IR_node_t) LR_core));
  do
    {
      hash_value += (unsigned long) IR_element_after_dot (LR_situation_list);
      LR_situation_list = IR_next_LR_situation (LR_situation_list);
    }
  while (LR_situation_list != NULL
         && IR_important_LR_situation_flag (LR_situation_list));
  return hash_value;
}

static int
LR_core_eq_function (hash_table_entry_t LR_core_1,
                     hash_table_entry_t LR_core_2)
{
  IR_node_t LR_situation_list_1;
  IR_node_t LR_situation_list_2;

  LR_situation_list_1
    = IR_LR_situation_list (IR_LR_set_list ((IR_node_t) LR_core_1));
  LR_situation_list_2
    = IR_LR_situation_list (IR_LR_set_list ((IR_node_t) LR_core_2));
  do
    {
      /* Remember that the canonical rules have not common
         elements. */
      if (IR_element_after_dot (LR_situation_list_1)
          != IR_element_after_dot (LR_situation_list_2))
        return FALSE;
      LR_situation_list_1 = IR_next_LR_situation (LR_situation_list_1);
      LR_situation_list_2 = IR_next_LR_situation (LR_situation_list_2);
    }
  while (LR_situation_list_1 != NULL && LR_situation_list_2 != NULL
         && IR_important_LR_situation_flag (LR_situation_list_1)
         && IR_important_LR_situation_flag (LR_situation_list_2));
  return ((LR_situation_list_1 == NULL
           || !IR_important_LR_situation_flag (LR_situation_list_1))
          && (LR_situation_list_2 == NULL
              || !IR_important_LR_situation_flag (LR_situation_list_2)));
}

/* The LR-core table itself is represented by the following
   variable. */

static hash_table_t LR_core_table;

IR_node_t
insert_LR_core (IR_node_t LR_core)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (LR_core_table, LR_core, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) LR_core;
  return (IR_node_t) *entry_ptr;
}

/* The following variable value is node representing LR-core.  The node
   used for searching LR-core with given important LR-situations. */

static IR_node_t work_LR_core;

/* The following variable value is node representing LR-set.  The node
   used for searching LR-core with given important LR-situations. */

static IR_node_t work_LR_set_for_LR_core_table;

IR_node_t
find_LR_core (IR_node_t LR_situation_list)
{
  hash_table_entry_t *entry_ptr;

  IR_set_LR_situation_list (work_LR_set_for_LR_core_table, LR_situation_list);
  IR_set_LR_set_list (work_LR_core, work_LR_set_for_LR_core_table);
  entry_ptr = find_hash_table_entry (LR_core_table, work_LR_core, FALSE);
  return (IR_node_t) *entry_ptr;
}

void
initiate_LR_core_table (void)
{
  work_LR_core = IR_create_node (IR_NM_LR_core);
  work_LR_set_for_LR_core_table = IR_create_node (IR_NM_LR_set);
  LR_core_table = create_hash_table (3000, LR_core_hash_function,
                                     LR_core_eq_function);
}

void
finish_LR_core_table (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "LR-core table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (LR_core_table),
       hash_table_elements_number (LR_core_table),
       hash_table_collisions (LR_core_table));
#endif
  delete_hash_table (LR_core_table);
}



/* This page contains abstract data `table of LR-sets'.  Elements of
   the table are nodes representing LR-sets.  Key of the table
   elements is important LR-situations (right hand side element after
   dot, and context of LR-situation) of the LR-sets. */

static unsigned
LR_set_hash_function (hash_table_entry_t LR_set)
{
  unsigned hash_value;
  IR_node_t LR_situation_list;

  hash_value = (unsigned long) IR_LR_core ((IR_node_t) LR_set);
  LR_situation_list = IR_LR_situation_list ((IR_node_t) LR_set);
  do
    {
      hash_value += context_hash_value (IR_context (LR_situation_list));
      LR_situation_list = IR_next_LR_situation (LR_situation_list);
    }
  while (LR_situation_list != NULL
         && IR_important_LR_situation_flag (LR_situation_list));
  return hash_value;
}

static int
LR_set_eq_function (hash_table_entry_t LR_set_1, hash_table_entry_t LR_set_2)
{
  IR_node_t LR_situation_list_1;
  IR_node_t LR_situation_list_2;

  if (IR_LR_core ((IR_node_t) LR_set_1) != IR_LR_core ((IR_node_t) LR_set_2))
    return FALSE;
  LR_situation_list_1 = IR_LR_situation_list ((IR_node_t) LR_set_1);
  LR_situation_list_2 = IR_LR_situation_list ((IR_node_t) LR_set_2);
  do
    {
      /* Remember that the canonical rules have not common
         elements. */
      if (!context_eq (IR_context (LR_situation_list_1),
                       IR_context (LR_situation_list_2)))
        return FALSE;
      LR_situation_list_1 = IR_next_LR_situation (LR_situation_list_1);
      LR_situation_list_2 = IR_next_LR_situation (LR_situation_list_2);
    }
  while (LR_situation_list_1 != NULL
         && IR_important_LR_situation_flag (LR_situation_list_1));
  assert ((LR_situation_list_1 == NULL
           || !IR_important_LR_situation_flag (LR_situation_list_1))
          && (LR_situation_list_2 == NULL
              || !IR_important_LR_situation_flag (LR_situation_list_2)));
  return TRUE;
}

/* The LR-set table itself is represented by the following
   variable. */

static hash_table_t LR_set_table;

IR_node_t
insert_LR_set (IR_node_t LR_set)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (LR_set_table, LR_set, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) LR_set;
  return (IR_node_t) *entry_ptr;
}

/* The following variable value is node representing LR-set.  The node
   used for searching LR-set with given important LR-situations. */

static IR_node_t work_LR_set_for_LR_set_table;

IR_node_t
find_LR_set (IR_node_t LR_core, IR_node_t LR_situation_list)
{
  hash_table_entry_t *entry_ptr;

  IR_set_LR_core (work_LR_set_for_LR_set_table, LR_core);
  IR_set_LR_situation_list (work_LR_set_for_LR_set_table, LR_situation_list);
  entry_ptr = find_hash_table_entry (LR_set_table,
                                     work_LR_set_for_LR_set_table, FALSE);
  return (IR_node_t) *entry_ptr;
}

void
delete_LR_set_from_table (IR_node_t LR_set)
{
  remove_element_from_hash_table_entry (LR_set_table, LR_set);
}

void
initiate_LR_set_table (void)
{
  work_LR_set_for_LR_set_table = IR_create_node (IR_NM_LR_set);
  LR_set_table = create_hash_table (10000, LR_set_hash_function,
                                    LR_set_eq_function);
}

void
finish_LR_set_table (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "LR-set table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (LR_set_table),
       hash_table_elements_number (LR_set_table),
       hash_table_collisions (LR_set_table));
#endif
  delete_hash_table (LR_set_table);
}

void
output_LR_set_situations (FILE *f, IR_node_t LR_set, const char *indent)
{
  IR_node_t current_LR_situation;

  current_LR_situation = IR_LR_situation_list (LR_set);
  do
    {
      output_LR_situation (f, current_LR_situation, indent, TRUE);
      current_LR_situation = IR_next_LR_situation (current_LR_situation);
    }
  while (current_LR_situation != NULL
         && (full_lr_set_flag
             || IR_important_LR_situation_flag (current_LR_situation)));
  output_char ('\n', f);
}

