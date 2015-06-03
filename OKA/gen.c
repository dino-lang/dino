/*
   FILE NAME:   gen.c

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool OKA.

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

   TITLE:       generator of OKA (pipeline hazards description translator)

   DESCRIPTION: This file generates interface and implementation file of
                the pipeline hazards description.

   SPECIAL CONSIDERATION:
         The generator is to be called after OKA semantic analyzer only
       if any error was not fixed.
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of the generator.

*/

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_MEMSET
#define HAVE_MEMSET
#endif
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#ifndef HAVE_FLOAT_H
#define HAVE_FLOAT_H
#endif
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "position.h"
#include "errors.h"
#include "hashtab.h"
#include "bits.h"
#include "ticker.h"
#include "vlobject.h"
#include "common.h"
#include "tab.h"
#include "ird.h"
#include "gen.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#else
#define DBL_MAX  1.7976931348623157e+308 /* IEEE double */
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif
#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif
#ifndef SCHAR_MAX
#define SCHAR_MAX 127
#endif
#ifndef SCHAR_MIN
#define SCHAR_MIN (-128)
#endif
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif
#ifndef SHRT_MAX
#define SHRT_MAX 32767
#endif  
#ifndef SHRT_MIN
#define SHRT_MIN (-32768)
#endif
#ifndef UINT_MAX
#define UINT_MAX (INT_MAX * 2U + 1)
#endif
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif  
#ifndef INT_MIN
#define INT_MIN (-INT_MAX-1)
#endif
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

#ifndef HAVE_MEMSET

static void *
memset (void *to, int value, size_t size)
{
  char *cto = (char *) to;

  while (size > 0)
    {
      *cto++ = value;
      size--;
    }
  return to;
}

#endif /* #ifndef HAVE_MEMSET */



/* The following variable value is number of automaton which are
   really being created.  This value is defined on the base of
   argument of option `-split'.  If the variable has zero value the
   number of automatons is defined by the constructions `%automaton'.
   This case occures when option `-split' is absent or has zero
   argument.  If constructions `%automaton' is absent only one
   automaton is being created. */

static int automatons_number;

/* The following variable values are times of
       transformation of regular expressions
       building NDFA
       NDFA -> DFA
       DFA minimization
       building instruction equivalence classes
       all previous ones
       code output
*/

static ticker_t transformation_time;
static ticker_t NDFA_time;
static ticker_t NDFA_to_DFA_time;
static ticker_t minimize_time;
static ticker_t equivalence_time;
static ticker_t automaton_generation_time;
static ticker_t output_time;



static IR_node_t cycle_advancing_single_instruction_declaration;

static void
add_cycle_advancing_single_instruction_declaration (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  IR_node_t cycle_advancing_identifier;
  char *identifier_itself;

  last_single_declaration = NULL;
  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    last_single_declaration = current_single_declaration;
  IR_TOP_ADD_STRING ("$cycle_advancing");
  identifier_itself = insert_identifier (IR_TOP_BEGIN ());
  if (identifier_itself == (char *) IR_TOP_BEGIN ())
    IR_TOP_FINISH ();
  else
    IR_TOP_NULLIFY ();
  cycle_advancing_identifier
    = IR_new_identifier (no_position, identifier_itself, NULL);
  cycle_advancing_single_instruction_declaration
    = IR_new_single_instruction_declaration
      (no_position, cycle_advancing_identifier, NULL);
  if (last_single_declaration == NULL)
    IR_set_single_declaration_list
      (description, cycle_advancing_single_instruction_declaration);
  else
    IR_set_next_single_declaration
      (last_single_declaration,
       cycle_advancing_single_instruction_declaration);
  IR_set_instruction_number (cycle_advancing_single_instruction_declaration,
                             IR_instructions_number (description));
  IR_set_instructions_number (description,
                              IR_instructions_number (description) + 1);
}



/* This page contains functions which are used to output pipeline
   hazards recognizer (PHR).  These function are used to fix output
   errors in time.  All output of PHR is made only through these
   function. */

/* The following variable value is number of current line in the
   description file. */

static int current_description_file_line;

/* The following variable value is number of current line in the
   interface file. */

static int current_interface_file_line;

/* The following variable value is number of current line in the
   implementation file. */

static int current_implementation_file_line;

/* This function outputs string onto interface or implementation file
   and fixes output errors. */

static void
output_string (FILE *f, const char *string)
{
  for (; *string != '\0'; string++)
    {
      if (fputc (*string, f) == EOF)
        {
          if (f == output_description_file)
            system_error (TRUE, no_position, "fatal error -- %s: ",
                          output_description_file_name);
          else if (f == output_interface_file)
            system_error (TRUE, no_position, "fatal_error -- %s: ",
                          output_interface_file_name);
          else if (f == output_implementation_file)
            system_error (TRUE, no_position, "fatal_error -- %s: ",
                          output_implementation_file_name);
          else
            assert (f == stderr);
        }
      if (*string == '\n')
        {
          if (f == output_description_file)
            current_description_file_line++;
          else if (f == output_interface_file)
            current_interface_file_line++;
          else if (f == output_implementation_file)
            current_implementation_file_line++;
        }
    }
}

/* This function outputs character onto interface or implementation
   file and fixes output errors. */

static void
output_char (int ch, FILE *f)
{
  if (fputc (ch, f) == EOF)
    {
      if (f == output_description_file)
        system_error (TRUE, no_position, "fatal_error -- %s: ",
                      output_description_file_name);
      else if (f == output_interface_file)
        system_error (TRUE, no_position, "fatal_error -- %s: ",
                      output_interface_file_name);
      else if (f == output_implementation_file)
        system_error (TRUE, no_position, "fatal_error -- %s: ",
                      output_implementation_file_name);
      else
        assert (f == stderr);
    }
  if (ch == '\n')
    {
      if (f == output_description_file)
        current_description_file_line++;
      else if (f == output_interface_file)
        current_interface_file_line++;
      else if (f == output_implementation_file)
        current_implementation_file_line++;
    }
}

static void
output_decimal_number (FILE *f, int number, int minimum_width)
{
  char format [30];

  assert (minimum_width >= 0);
  sprintf (format, "%%%dd", minimum_width);
  if (fprintf (f, format, number) == EOF)
    {
      if (f == output_description_file)
        system_error (TRUE, no_position, "fatal_error -- %s: ",
                      output_description_file_name);
      else if (f == output_interface_file)
        system_error (TRUE, no_position, "fatal_error -- %s: ",
                      output_interface_file_name);
      else if (f == output_implementation_file)
        system_error (TRUE, no_position, "fatal_error -- %s: ",
                      output_implementation_file_name);
      else
        assert (f == stderr);
    }
}

/* ??? */

static void
initiate_output (void)
{
  current_interface_file_line = 1;
  current_implementation_file_line = 1;
  if (v_flag)
    {
      current_description_file_line = 1;
      output_description_file = fopen (output_description_file_name, "w");
      if (output_description_file == NULL)
        system_error (TRUE, no_position,
                      "fatal error -- %s: ", output_description_file_name);
    }
  output_interface_file = fopen (output_interface_file_name, "w");
  if (output_interface_file == NULL)
    system_error (TRUE, no_position,
                  "fatal error -- %s: ", output_interface_file_name);
  output_implementation_file = fopen (output_implementation_file_name, "w");
  if (output_implementation_file == NULL)
    system_error (TRUE, no_position,
                  "fatal error -- %s: ", output_implementation_file_name);
}



/* Abstract data `alternative states'. */

static IR_node_t first_free_alternative_state;

#ifndef NDEBUG
static int allocated_alternative_states_number = 0;
#endif

static IR_node_t
get_free_alternative_state (void)
{
  IR_node_t result;

  if (first_free_alternative_state != NULL)
    {
      result = first_free_alternative_state;
      first_free_alternative_state
        = IR_next_alternative_state (first_free_alternative_state);
    }
  else
    {
#ifndef NDEBUG
      allocated_alternative_states_number++;
#endif
      result = IR_new_alternative_state (NULL, NULL);
    }
  return result;
}

static void
free_alternative_state (IR_node_t alternative_state)
{
  if (alternative_state == NULL)
    return;
  IR_set_next_alternative_state (alternative_state,
                                 first_free_alternative_state);
  first_free_alternative_state = alternative_state;
}

static void
free_alternative_states_list (IR_node_t alternative_states_list)
{
  IR_node_t current_alternative_state;
  IR_node_t next_alternative_state;

  for (current_alternative_state = alternative_states_list;
       current_alternative_state != NULL;
       current_alternative_state = next_alternative_state)
    {
      next_alternative_state
        = IR_next_alternative_state (current_alternative_state);
      free_alternative_state (current_alternative_state);
    }
}

static int
alternative_state_comparison (const void *alternative_state_ptr_1,
                              const void *alternative_state_ptr_2)
{
  if (IR_unique_number (IR_state (*(IR_node_t *) alternative_state_ptr_1))
      == IR_unique_number (IR_state (*(IR_node_t *) alternative_state_ptr_2)))
    return 0;
  else if (IR_unique_number (IR_state (*(IR_node_t *) alternative_state_ptr_1))
           < IR_unique_number (IR_state
                               (*(IR_node_t *) alternative_state_ptr_2)))
    return -1;
  else
    return 1;
}

static IR_node_t
sort_and_make_unique_alternative_states_list
  (IR_node_t alternative_states_list)
{
  IR_node_t current_alternative_state;
  vlo_t alternative_states;
  int current_index;
  int previous_unique_state_index;
  IR_node_t result;
  IR_node_t *result_ptr;

  VLO_CREATE (alternative_states, 0);
  for (current_alternative_state = alternative_states_list;
       current_alternative_state != NULL;
       current_alternative_state
       = IR_next_alternative_state (current_alternative_state))
    VLO_ADD_MEMORY (alternative_states, &current_alternative_state,
                    sizeof (current_alternative_state));
  qsort (VLO_BEGIN (alternative_states),
         VLO_LENGTH (alternative_states) / sizeof (IR_node_t),
         sizeof (IR_node_t),
         alternative_state_comparison);
  if (VLO_LENGTH (alternative_states) == 0)
    result = NULL;
  else
    {
      result_ptr = (IR_node_t *) VLO_BEGIN (alternative_states);
      for (previous_unique_state_index = 0, current_index = 1;
           current_index
           < VLO_LENGTH (alternative_states) / sizeof (IR_node_t);
           current_index++)
        if (IR_state (result_ptr [previous_unique_state_index])
            != IR_state (result_ptr [current_index]))
          {
            previous_unique_state_index++;
            result_ptr [previous_unique_state_index]
              = result_ptr [current_index];
          }
      for (current_index = previous_unique_state_index + 1;
           current_index
           < VLO_LENGTH (alternative_states) / sizeof (IR_node_t);
           current_index++)
        free_alternative_state (result_ptr [current_index]);
      VLO_SHORTEN (alternative_states,
                   (current_index - previous_unique_state_index - 1)
                   * sizeof (IR_node_t *));
      result_ptr = (IR_node_t *) VLO_BEGIN (alternative_states);
      for (current_index = 1;
           current_index
           < VLO_LENGTH (alternative_states) / sizeof (IR_node_t);
           current_index++)
        IR_set_next_alternative_state (result_ptr [current_index - 1],
                                       result_ptr [current_index]);
      IR_set_next_alternative_state (result_ptr [current_index - 1], NULL);
      result = *result_ptr;
    }
  VLO_DELETE (alternative_states);
  return result;
}

/* Remember that the lists must be already sorted. */

static int
alternative_state_list_eq (IR_node_t alternative_state_list_1,
                           IR_node_t alternative_state_list_2)
{
  while (alternative_state_list_1 != NULL && alternative_state_list_2 != NULL
         && alternative_state_comparison (&alternative_state_list_1,
                                          &alternative_state_list_2) == 0)
    {
      alternative_state_list_1
        = IR_next_alternative_state (alternative_state_list_1);
      alternative_state_list_2
        = IR_next_alternative_state (alternative_state_list_2);
    }
  return alternative_state_list_1 == alternative_state_list_2;
}

static void
initiate_alternative_states (void)
{
  first_free_alternative_state = NULL;
}

static void
finish_alternative_states (void)
{
}




/* This page contains abstract data `state'. */

static int max_cycles_number;

static int elements_in_cycle_reservation;

static int elements_in_reservations;

static vlo_t units_container;

static IR_node_t *units_array;

/* The state table itself is represented by the following variable. */

static hash_table_t state_table;

static vlo_t free_states;

static int current_unique_state_number;

#ifndef NDEBUG
static int allocated_states_number = 0;
#endif

static reservation_sets_list_t
get_empty_reservation_sets_list (void)
{
  reservation_sets_list_t result;

  IR_TOP_EXPAND (elements_in_reservations
                 * sizeof (unit_reservation_set_element_t));
  result = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  memset (result, 0,
          elements_in_reservations * sizeof (unit_reservation_set_element_t));
  return result;
}

static unsigned
reservation_sets_list_hash_value (reservation_sets_list_t reservations)
{
  unsigned int hash_value;
  int unit_reservation_set_elements_number;
  unit_reservation_set_element_t *unit_reservation_set_element_ptr;

  hash_value = 0;
  unit_reservation_set_elements_number = elements_in_reservations;
  unit_reservation_set_element_ptr = reservations;
  while (unit_reservation_set_elements_number != 0)
    {
      unit_reservation_set_elements_number--;
      hash_value
        = ((hash_value >> (sizeof (unsigned) - 1) * CHAR_BIT)
           | (hash_value << CHAR_BIT))
          + *unit_reservation_set_element_ptr;
      unit_reservation_set_element_ptr++;
    }
  return hash_value;
}

/* ??? */
static int
reservation_sets_lists_comparison (reservation_sets_list_t reservations_1,
                                   reservation_sets_list_t reservations_2)
{
  int unit_reservation_set_elements_number;
  unit_reservation_set_element_t *unit_reservation_set_element_ptr_1;
  unit_reservation_set_element_t *unit_reservation_set_element_ptr_2;

  assert (reservations_1 != NULL && reservations_2 != NULL);
  unit_reservation_set_elements_number = elements_in_reservations;
  unit_reservation_set_element_ptr_1 = reservations_1;
  unit_reservation_set_element_ptr_2 = reservations_2;
  while (unit_reservation_set_elements_number != 0
         && (*unit_reservation_set_element_ptr_1
             == *unit_reservation_set_element_ptr_2))
    {
      unit_reservation_set_elements_number--;
      unit_reservation_set_element_ptr_1++;
      unit_reservation_set_element_ptr_2++;
    }
  if (unit_reservation_set_elements_number == 0)
    return 0;
  else if (*unit_reservation_set_element_ptr_1
           < *unit_reservation_set_element_ptr_2)
    return -1;
  else
    return 1;
}

static int
reservation_sets_lists_eq (reservation_sets_list_t reservations_1,
                           reservation_sets_list_t reservations_2)
{
  return
    reservation_sets_lists_comparison (reservations_1, reservations_2) == 0;
}

static void
set_up_unit_reservation (reservation_sets_list_t reservations,
                         int cycle_number, int unit_number)
{
  assert (cycle_number < max_cycles_number);
  SET_BIT (reservations,
           cycle_number * elements_in_cycle_reservation
	   * sizeof (unit_reservation_set_element_t) * CHAR_BIT
	   + unit_number, 1);
}

#if 0
static int
it_is_empty_reservation_sets_list (reservation_sets_list_t operand)
{
  unit_reservation_set_element_t *unit_reservation_set_element_ptr;
  int unit_reservation_set_elements_number;

  assert (operand != NULL);
  for (unit_reservation_set_elements_number = elements_in_reservations,
       unit_reservation_set_element_ptr = operand;
       unit_reservation_set_elements_number != 0;
       unit_reservation_set_element_ptr++,
       unit_reservation_set_elements_number--)
    if (*unit_reservation_set_element_ptr != 0)
      return FALSE;
  return TRUE;
}
#endif

static reservation_sets_list_t
  get_exclusion_set (reservation_sets_list_t in_set);

static int
reservation_sets_lists_are_intersected (reservation_sets_list_t operand_1,
                                        reservation_sets_list_t operand_2)
{
  unit_reservation_set_element_t *current_unit_set_element_ptr_1;
  unit_reservation_set_element_t *current_unit_set_element_ptr_2;
  unit_reservation_set_element_t *current_cycle_ptr_1;
  unit_reservation_set_element_t *current_cycle_ptr_2;

  assert (operand_1 != NULL && operand_2 != NULL);
  for (current_unit_set_element_ptr_1 = operand_1,
       current_unit_set_element_ptr_2 = operand_2;
       current_unit_set_element_ptr_1 < operand_1 + elements_in_reservations;
       current_unit_set_element_ptr_1++, current_unit_set_element_ptr_2++)
    if (*current_unit_set_element_ptr_1 & *current_unit_set_element_ptr_2)
      return TRUE;
  for (current_cycle_ptr_1 = operand_1,
       current_cycle_ptr_2 = operand_2;
       current_cycle_ptr_1 < operand_1 + elements_in_reservations;
       current_cycle_ptr_1 += elements_in_cycle_reservation,
       current_cycle_ptr_2 += elements_in_cycle_reservation)
    for (current_unit_set_element_ptr_1 = current_cycle_ptr_1,
         current_unit_set_element_ptr_2
	   = get_exclusion_set (current_cycle_ptr_2);
	 current_unit_set_element_ptr_1
	   < current_cycle_ptr_1 + elements_in_cycle_reservation;
	 current_unit_set_element_ptr_1++, current_unit_set_element_ptr_2++)
      if (*current_unit_set_element_ptr_1 & *current_unit_set_element_ptr_2)
	return TRUE;
  return FALSE;
}

static void
reservation_sets_list_shift (reservation_sets_list_t result,
                             reservation_sets_list_t operand)
{
  int i;

  assert (result != NULL && operand != NULL && result != operand);
  for (i = elements_in_cycle_reservation; i < elements_in_reservations; i++)
    result [i - elements_in_cycle_reservation] = operand [i];
}

static void
reservation_sets_list_or (reservation_sets_list_t result,
                          reservation_sets_list_t operand_1,
                          reservation_sets_list_t operand_2)
{
  unit_reservation_set_element_t *current_unit_set_element_ptr_1;
  unit_reservation_set_element_t *current_unit_set_element_ptr_2;
  unit_reservation_set_element_t *current_result_unit_set_element_ptr;

  assert (result != NULL && operand_1 != NULL && operand_2 != NULL);
  for (current_unit_set_element_ptr_1 = operand_1,
       current_unit_set_element_ptr_2 = operand_2,
       current_result_unit_set_element_ptr = result;
       current_unit_set_element_ptr_1 < operand_1 + elements_in_reservations;
       current_unit_set_element_ptr_1++, current_unit_set_element_ptr_2++,
       current_result_unit_set_element_ptr++)
    *current_result_unit_set_element_ptr
      = *current_unit_set_element_ptr_1 | *current_unit_set_element_ptr_2;
}

#if 0
static void
reservation_sets_list_and (reservation_sets_list_t result,
                           reservation_sets_list_t operand_1,
                           reservation_sets_list_t operand_2)
{
  unit_reservation_set_element_t *current_unit_set_element_ptr_1;
  unit_reservation_set_element_t *current_unit_set_element_ptr_2;
  unit_reservation_set_element_t *current_result_unit_set_element_ptr;

  assert (result != NULL && operand_1 != NULL && operand_2 != NULL);
  for (current_unit_set_element_ptr_1 = operand_1,
       current_unit_set_element_ptr_2 = operand_2,
       current_result_unit_set_element_ptr = result;
       current_unit_set_element_ptr_1 < operand_1 + elements_in_reservations;
       current_unit_set_element_ptr_1++, current_unit_set_element_ptr_2++,
       current_result_unit_set_element_ptr++)
    *current_result_unit_set_element_ptr
      = *current_unit_set_element_ptr_1 & *current_unit_set_element_ptr_2;
}
#endif

static void
output_cycle_reservations (FILE *f, reservation_sets_list_t reservations,
                           int start_cycle, int repetition_number)
{
  int current_unit_number;
  int reserved_units_number;

  reserved_units_number = 0;
  for (current_unit_number = 0;
       current_unit_number < IR_units_number (description);
       current_unit_number++)
    if (BIT (reservations,
             start_cycle * elements_in_cycle_reservation
	     * sizeof (unit_reservation_set_element_t) * CHAR_BIT
             + current_unit_number))
      reserved_units_number++;
  assert (repetition_number > 0);
  if (repetition_number != 1 && reserved_units_number > 1)
    output_char ('(', f);
  reserved_units_number = 0;
  for (current_unit_number = 0;
       current_unit_number < IR_units_number (description);
       current_unit_number++)
    if (BIT (reservations,
             start_cycle * elements_in_cycle_reservation
	     * sizeof (unit_reservation_set_element_t) * CHAR_BIT
             + current_unit_number))
      {
        if (reserved_units_number != 0)
          output_char ('+', f);
        reserved_units_number++;
        output_string (f,
                       IR_identifier_itself
                       (IR_identifier (units_array [current_unit_number])));
      }
  if (reserved_units_number == 0)
    output_string (f, "%nothing");
  assert (repetition_number > 0);
  if (reserved_units_number != 0 && repetition_number != 1)
    {
      if (reserved_units_number > 1)
        output_char (')', f);
      output_char ('*', f);
      output_decimal_number (f, repetition_number, 0);
    }
}

static void
output_reservation_sets_list (FILE *f, reservation_sets_list_t reservations)
{
  int start_cycle;
  int current_cycle_number;
  int repetition_number;
  int first_cycle_output_flag;

  repetition_number = 0;
  first_cycle_output_flag = FALSE;
  for (current_cycle_number = 0;
       current_cycle_number < max_cycles_number;
       current_cycle_number++)
    if (repetition_number == 0)
      {
        repetition_number++;
        start_cycle = current_cycle_number;
      }
    else if (bit_string_comparison
             (reservations,
	      start_cycle * elements_in_cycle_reservation
	      * sizeof (unit_reservation_set_element_t) * CHAR_BIT,
              reservations,
              current_cycle_number * elements_in_cycle_reservation
	      * sizeof (unit_reservation_set_element_t) * CHAR_BIT,
              IR_units_number (description)) == 0)
      repetition_number++;
    else
      {
        if (first_cycle_output_flag)
          output_string (f, "  ");
        output_cycle_reservations (f, reservations, start_cycle,
                                   repetition_number);
        first_cycle_output_flag = TRUE;
        repetition_number = 1;
        start_cycle = current_cycle_number;
      }
  if (!is_zero_bit_string (reservations,
                           start_cycle * elements_in_cycle_reservation
			   * sizeof (unit_reservation_set_element_t)
			   * CHAR_BIT,
                           IR_units_number (description))
      || !first_cycle_output_flag)
    {
      if (first_cycle_output_flag)
        output_string (f, "  ");
      output_cycle_reservations (f, reservations, start_cycle,
                                 repetition_number);
    }
}


static IR_node_t
get_free_state (int with_reservations, IR_node_t automaton)
{
  IR_node_t result;

  assert (max_cycles_number > 0 && automaton != NULL);
  if (VLO_LENGTH (free_states) != 0)
    {
      result
        = ((IR_node_t *) VLO_BEGIN (free_states))
          [VLO_LENGTH (free_states) / sizeof (IR_node_t) - 1];
      VLO_SHORTEN (free_states, sizeof (IR_node_t));
      IR_set_automaton (result, automaton);
      IR_set_first_out_arc (result, NULL);
      IR_set_it_was_placed_in_stack_for_NDFA_forming (result, FALSE);
      IR_set_it_was_placed_in_stack_for_DFA_forming (result, FALSE);
      IR_set_component_states (result, NULL);
    }
  else
    {
#ifndef NDEBUG
      allocated_states_number++;
#endif
      result = IR_new_state (NULL, current_unique_state_number, automaton);
      current_unique_state_number++;
    }
  if (with_reservations)
    {
      if (IR_reservations (result) == NULL)
        IR_set_reservations (result, get_empty_reservation_sets_list ());
      else
        memset
          (IR_reservations (result), 0,
           elements_in_reservations * sizeof (unit_reservation_set_element_t));
    }
  return result;
}

static void
free_state (IR_node_t state)
{
  free_alternative_states_list (IR_component_states (state));
  VLO_ADD_MEMORY (free_states, &state, sizeof (state));
}

static unsigned
state_hash_function (hash_table_entry_t state)
{
  unsigned int hash_value;
  IR_node_t current_alternative_state;

  if (IR_component_states ((IR_node_t) state) == NULL)
    hash_value
      = reservation_sets_list_hash_value (IR_reservations ((IR_node_t) state));
  else
    {
      hash_value = 0;
      for (current_alternative_state = IR_component_states ((IR_node_t) state);
           current_alternative_state != NULL;
           current_alternative_state
           = IR_next_alternative_state (current_alternative_state))
        hash_value
          = ((hash_value >> (sizeof (unsigned) - 1) * CHAR_BIT)
             | (hash_value << CHAR_BIT))
            + IR_unique_number (IR_state (current_alternative_state));
    }
  hash_value
    = ((hash_value >> (sizeof (unsigned) - 1) * CHAR_BIT)
       | (hash_value << CHAR_BIT))
      + IR_automaton_order_number (IR_automaton ((IR_node_t) state));
  return hash_value;
}

/* ??? */
static int
state_eq_function (hash_table_entry_t state_1, hash_table_entry_t state_2)
{
  IR_node_t current_alternative_state_1;
  IR_node_t current_alternative_state_2;

  if (IR_automaton ((IR_node_t) state_1) != IR_automaton ((IR_node_t) state_2))
    return FALSE;
  else if (IR_component_states ((IR_node_t) state_1) == NULL
           && IR_component_states ((IR_node_t) state_2) == NULL)
    return reservation_sets_lists_eq (IR_reservations ((IR_node_t) state_1),
                                      IR_reservations ((IR_node_t) state_2));
  else if (IR_component_states ((IR_node_t) state_1) != NULL
           && IR_component_states ((IR_node_t) state_2) != NULL)
    {
      for (current_alternative_state_1
           = IR_component_states ((IR_node_t) state_1),
           current_alternative_state_2
           = IR_component_states ((IR_node_t) state_2);
           current_alternative_state_1 != NULL
           && current_alternative_state_2 != NULL;
           current_alternative_state_1
           = IR_next_alternative_state (current_alternative_state_1),
           current_alternative_state_2
           = IR_next_alternative_state (current_alternative_state_2))
        /* All state in the list must be already in the hash table.
           Also the lists must be sorted. */
        if (IR_state (current_alternative_state_1)
            != IR_state (current_alternative_state_2))
          return FALSE;
      return current_alternative_state_1 == current_alternative_state_2;
    }
  else
    return FALSE;
}

/* ??? */
static IR_node_t
insert_state (IR_node_t state)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr
    = find_hash_table_entry (state_table, (hash_table_entry_t) state, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) state;
  return (IR_node_t) *entry_ptr;
}

#ifndef NDEBUG

static IR_node_t
find_state (IR_node_t state)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr
    = find_hash_table_entry (state_table, (hash_table_entry_t) state, FALSE);
  return (IR_node_t) *entry_ptr;
}

#endif

static void
set_up_state_reservation (IR_node_t state, int cycle_number, int unit_number)
{
  set_up_unit_reservation (IR_reservations (state), cycle_number, unit_number);
}

#if 0
static int
it_is_state_without_reservations (IR_node_t state)
{
  return it_is_empty_reservation_sets_list (IR_reservations (state));
}
#endif

static int
state_reservations_are_intersected (IR_node_t state_1, IR_node_t state_2)
{
  assert (IR_automaton (state_1) == IR_automaton (state_2));
  return reservation_sets_lists_are_intersected (IR_reservations (state_1),
                                                 IR_reservations (state_2));
}

static IR_node_t
states_union (IR_node_t state_1, IR_node_t state_2)
{
  IR_node_t result;
  IR_node_t state_in_table;

  assert (IR_automaton (state_1) == IR_automaton (state_2));
  result = get_free_state (TRUE, IR_automaton (state_1));
  reservation_sets_list_or
    (IR_reservations (result),
     IR_reservations (state_1), IR_reservations (state_2));
  state_in_table = insert_state (result);
  if (result != state_in_table)
    {
      free_state (result);
      result = state_in_table;
    }
  return result;
}

static IR_node_t
state_shift (IR_node_t state)
{
  IR_node_t result;
  IR_node_t state_in_table;

  result = get_free_state (TRUE, IR_automaton (state));
  reservation_sets_list_shift (IR_reservations (result),
                               IR_reservations (state));
  state_in_table = insert_state (result);
  if (result != state_in_table)
    {
      free_state (result);
      result = state_in_table;
    }
  return result;
}

/* ??? */
static void
initiate_states (void)
{
  IR_node_t current_single_declaration;

  VLO_CREATE (units_container, 0);
  VLO_EXPAND (units_container,
              IR_units_number (description) * sizeof (IR_node_t));
  units_array = VLO_BEGIN (units_container);
  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_unit_declaration))
      {
        units_array [IR_unit_number (current_single_declaration)]
          = current_single_declaration;
      }
  max_cycles_number = IR_max_instruction_reservation_cycles (description);
  if (max_cycles_number == 0)
    max_cycles_number++;
  elements_in_cycle_reservation
    = ((IR_units_number (description)
	+ sizeof (unit_reservation_set_element_t) * CHAR_BIT - 1)
       / (sizeof (unit_reservation_set_element_t) * CHAR_BIT));
  elements_in_reservations = elements_in_cycle_reservation * max_cycles_number;
  current_unique_state_number = 0;
  initiate_alternative_states ();
  VLO_CREATE (free_states, 5000);
  state_table = create_hash_table (5000, state_hash_function,
                                   state_eq_function);
}

/* ??? */
static void
finish_states (void)
{
  VLO_DELETE (units_container);
  delete_hash_table (state_table);
  VLO_DELETE (free_states);
  finish_alternative_states ();
}



/* Abstract data `arcs'. */

static IR_node_t first_free_arc;

#ifndef NDEBUG
static int allocated_arcs_number = 0;
#endif

static void
free_arc (IR_node_t arc)
{
  IR_set_next_out_arc (arc, first_free_arc);
  first_free_arc = arc;
}

static void
remove_arc (IR_node_t from_state, IR_node_t arc)
{
  IR_node_t previous_arc;
  IR_node_t current_arc;

  assert (arc != NULL);
  for (previous_arc = NULL, current_arc = IR_first_out_arc (from_state);
       current_arc != NULL;
       previous_arc = current_arc, current_arc = IR_next_out_arc (current_arc))
    if (current_arc == arc)
      break;
  assert (current_arc != NULL);
  if (previous_arc == NULL)
    IR_set_first_out_arc (from_state, IR_next_out_arc (arc));
  else
    IR_set_next_out_arc (previous_arc, IR_next_out_arc (arc));
  free_arc (arc);
}

/* Such arc must be absent. */

static void
add_arc (IR_node_t from_state, IR_node_t to_state,
         IR_node_t automaton_instruction_declaration)
{
  IR_node_t new_arc;

  if (first_free_arc == NULL)
    {
#ifndef NDEBUG
      allocated_arcs_number++;
#endif
      new_arc = IR_new_arc (NULL, NULL, NULL);
    }
  else
    {
      new_arc = first_free_arc;
      first_free_arc = IR_next_out_arc (first_free_arc);
    }
  IR_set_to_state (new_arc, to_state);
  IR_set_instruction (new_arc, automaton_instruction_declaration);
  IR_set_next_out_arc (new_arc, IR_first_out_arc (from_state));
  IR_set_first_out_arc (from_state, new_arc);
  IR_set_next_arc_marked_by_instruction (new_arc, NULL);
}

static IR_node_t
first_out_arc (IR_node_t state)
{
  return IR_first_out_arc (state);
}

static IR_node_t
next_out_arc (IR_node_t arc)
{
  return IR_next_out_arc (arc);
}

static void
initiate_arcs (void)
{
  first_free_arc = NULL;
}

static void
finish_arcs (void)
{
}



/* The following variable refers to an exclusion set returned by
   get_exclusion_set. */

static reservation_sets_list_t exclusion_set;

/* The table contains inclusion sets for each unit. */

static reservation_sets_list_t *unit_exclusion_set_table;

static void
initiate_exclusion_sets (void)
{
  IR_node_t current_single_declaration;
  reservation_sets_list_t unit_exclusion_set;
  IR_node_t current_unit_set_element;

  IR_TOP_EXPAND (elements_in_cycle_reservation
		 * sizeof (unit_reservation_set_element_t));
  exclusion_set = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_EXPAND (IR_units_number (description)
		 * sizeof (reservation_sets_list_t));
  unit_exclusion_set_table = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  /* Evaluate unit exclusion sets. */
  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
	 = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_unit_declaration))
      {
	IR_TOP_EXPAND (elements_in_cycle_reservation
		       * sizeof (unit_reservation_set_element_t));
	unit_exclusion_set = IR_TOP_BEGIN ();
	IR_TOP_FINISH ();
	memset (unit_exclusion_set, 0,
		elements_in_cycle_reservation
		* sizeof (unit_reservation_set_element_t));
	for (current_unit_set_element
	       = IR_exclusion_list (current_single_declaration);
	     current_unit_set_element != NULL;
	     current_unit_set_element
	       = IR_next_unit_set_element (current_unit_set_element))
	  SET_BIT (unit_exclusion_set,
		   IR_unit_number (IR_single_unit_declaration
				   (current_unit_set_element)),
		   1);
	unit_exclusion_set_table [IR_unit_number (current_single_declaration)]
	  = unit_exclusion_set;
      }
}

static reservation_sets_list_t
get_exclusion_set (reservation_sets_list_t in_set)
{
  int exclusion_char_number;
  int chars_number;
  int i;
  int unit_number;
  int current_unit_number;

  chars_number
    = elements_in_cycle_reservation * sizeof (unit_reservation_set_element_t);
  memset (exclusion_set, 0,chars_number) ;
  for (exclusion_char_number = 0;
       exclusion_char_number < chars_number;
       exclusion_char_number++)
    if (((unsigned char *) in_set) [exclusion_char_number])
      for (i = CHAR_BIT - 1; i >= 0; i--)
	if ((((unsigned char *) in_set) [exclusion_char_number] >> i) & 1)
	  {
	    unit_number = (exclusion_char_number + 1) * CHAR_BIT - i - 1;
	    if (unit_number >= IR_units_number (description))
	      return exclusion_set;
	    for (current_unit_number = 0;
		 current_unit_number < elements_in_cycle_reservation;
		 current_unit_number++)
	      {
		exclusion_set [current_unit_number]
		  |= unit_exclusion_set_table
                     [unit_number] [current_unit_number];
	      }
	  }
  return exclusion_set;
}



static IR_node_t
copy_instruction_expression_and_change_expression_identifiers
  (IR_node_t expression)
{
  IR_node_t result;

  if (IR_IS_OF_TYPE (expression, IR_NM_expression_atom))
    {
      if (IR_IS_OF_TYPE (IR_single_declaration (expression),
                         IR_NM_single_reservation_declaration))
        result
          = copy_instruction_expression_and_change_expression_identifiers
            (IR_expression (IR_single_declaration (expression)));
      else
        {
          assert (IR_IS_OF_TYPE (IR_single_declaration (expression),
                                 IR_NM_single_unit_declaration));
          result = IR_copy_node (expression);
        }
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_one_operand_expression))
    {
      result = IR_copy_node (expression);
      IR_set_operand
        (result,
         copy_instruction_expression_and_change_expression_identifiers
         (IR_operand (expression)));
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_two_operand_expression))
    {
      result = IR_copy_node (expression);
      IR_set_left_operand
        (result,
         copy_instruction_expression_and_change_expression_identifiers
         (IR_left_operand (expression)));
      IR_set_right_operand
        (result,
         copy_instruction_expression_and_change_expression_identifiers
         (IR_right_operand (expression)));
    }
  else
    {
      assert (IR_IS_OF_TYPE (expression, IR_NM_nothing));
      result = IR_copy_node (expression);
    }
  return result;
}

static int expression_is_transformed;

/* [A] -> ( | A)
   (A * B) -> ((A A) A)...
*/

static IR_node_t
transformation_function_1 (IR_node_t expression)
{
  if (IR_IS_OF_TYPE (expression, IR_NM_repetition))
    {
      /* Transformation: (A * B) -> ((A A) A)... */
      int current_expression_number;
      int repetition_number;
      IR_node_t operand;

      repetition_number = IR_number_value (IR_repetition_number (expression));
      if (repetition_number <= 0)
        expression = NULL;
      else
        {
          operand = IR_operand (expression);
          expression = operand;
          for (current_expression_number = 0;
               current_expression_number < repetition_number - 1;
               current_expression_number++)
            expression
              = IR_new_new_cycle_concatenation
                (IR_position (operand), expression,
                 copy_instruction_expression_and_change_expression_identifiers
                 (operand));
        }
      expression_is_transformed = TRUE;
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_optional_expression))
    {
      /* Transformation: [A] -> ( | A) */
      expression
        = IR_new_alternative
          (IR_position (expression), NULL, IR_operand (expression));
      expression_is_transformed = TRUE;
    }
  return expression;
}

/* (A | B)   C  ->  A   C | B   C
   (A | B) + C  ->  A + C | B + C */

static IR_node_t
transformation_function_2 (IR_node_t expression)
{
  if (IR_IS_OF_TYPE (expression, IR_NM_new_cycle_concatenation)
      && IR_IS_OF_TYPE (IR_left_operand (expression), IR_NM_alternative))
    {
      /* Transformation:
         (A | B) C -> A C | B C
         (  | B) C -> C   | B C */
      assert (IR_right_operand (IR_left_operand (expression)) != NULL);
      if (IR_left_operand (IR_left_operand (expression)) == NULL)
        expression
          = IR_new_alternative
            (IR_position (IR_left_operand (expression)),
             IR_right_operand (expression),
             IR_new_new_cycle_concatenation
             (IR_position (expression),
              IR_right_operand (IR_left_operand (expression)),
              copy_instruction_expression_and_change_expression_identifiers
              (IR_right_operand (expression))));
      else
        expression
          = IR_new_alternative
            (IR_position (IR_left_operand (expression)),
             IR_new_new_cycle_concatenation
             (IR_position (expression),
              IR_left_operand (IR_left_operand (expression)),
              IR_right_operand (expression)),
             IR_new_new_cycle_concatenation
             (IR_position (expression),
              IR_right_operand (IR_left_operand (expression)),
              copy_instruction_expression_and_change_expression_identifiers
              (IR_right_operand (expression))));
      expression_is_transformed = TRUE;
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_concatenation)
           && IR_IS_OF_TYPE (IR_left_operand (expression), IR_NM_alternative))
    {
      /* Transformation:
         (A | B) + C -> A + C | B + C
         (  | B) + C -> C     | B + C */
      assert (IR_right_operand (IR_left_operand (expression)) != NULL);
      if (IR_left_operand (IR_left_operand (expression)) == NULL)
        expression
          = IR_new_alternative
            (IR_position (IR_left_operand (expression)),
             IR_right_operand (expression),
             IR_new_concatenation
             (IR_position (expression),
              IR_right_operand (IR_left_operand (expression)),
              copy_instruction_expression_and_change_expression_identifiers
              (IR_right_operand (expression))));
      else
        expression
          = IR_new_alternative
            (IR_position (IR_left_operand (expression)),
             IR_new_concatenation
             (IR_position (expression),
              IR_left_operand (IR_left_operand (expression)),
              IR_right_operand (expression)),
             IR_new_concatenation
             (IR_position (expression),
              IR_right_operand (IR_left_operand (expression)),
              copy_instruction_expression_and_change_expression_identifiers
              (IR_right_operand (expression))));
      expression_is_transformed = TRUE;
    }
  return expression;
}

/* C   (A | B)  ->  C   A | C   B
   C + (A | B)  ->  C + A | C + B */

static IR_node_t
transformation_function_3 (IR_node_t expression)
{
  if (IR_IS_OF_TYPE (expression, IR_NM_new_cycle_concatenation)
      && IR_IS_OF_TYPE (IR_right_operand (expression), IR_NM_alternative))
    {
      /* Transformation:
         C   (A | B)  ->  C   A | C   B
         C   (  | B)  ->  C     | C   B */
      assert (IR_right_operand (IR_right_operand (expression)) != NULL);
      if (IR_left_operand (IR_right_operand (expression)) == NULL)
        expression
          = IR_new_alternative
            (IR_position (IR_right_operand (expression)),
             IR_left_operand (expression),
             IR_new_new_cycle_concatenation
             (IR_position (expression),
              copy_instruction_expression_and_change_expression_identifiers
              (IR_left_operand (expression)),
              IR_right_operand (IR_right_operand (expression))));
      else
        expression
          = IR_new_alternative
            (IR_position (IR_right_operand (expression)),
             IR_new_new_cycle_concatenation
             (IR_position (expression),
              IR_left_operand (expression),
              IR_left_operand (IR_right_operand (expression))),
             IR_new_new_cycle_concatenation
             (IR_position (expression),
              copy_instruction_expression_and_change_expression_identifiers
              (IR_left_operand (expression)),
              IR_right_operand (IR_right_operand (expression))));
      expression_is_transformed = TRUE;
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_concatenation)
           && IR_IS_OF_TYPE (IR_right_operand (expression), IR_NM_alternative))
    {
      /* Transformation:
         C + (A | B)  ->  C + A | C + B
         C + (  | B)  ->  C     | C + B */
      assert (IR_right_operand (IR_right_operand (expression)) != NULL);
      if (IR_left_operand (IR_right_operand (expression)) == NULL)
        expression
          = IR_new_alternative
            (IR_position (IR_right_operand (expression)),
             IR_left_operand (expression),
             IR_new_concatenation
             (IR_position (expression),
              copy_instruction_expression_and_change_expression_identifiers
              (IR_left_operand (expression)),
              IR_right_operand (IR_right_operand (expression))));
      else
        expression
          = IR_new_alternative
            (IR_position (IR_right_operand (expression)),
             IR_new_concatenation
             (IR_position (expression),
              IR_left_operand (expression),
              IR_left_operand (IR_right_operand (expression))),
             IR_new_concatenation
             (IR_position (expression),
              copy_instruction_expression_and_change_expression_identifiers
              (IR_left_operand (expression)),
              IR_right_operand (IR_right_operand (expression))));
      expression_is_transformed = TRUE;
    }
  return expression;
}

static int
guard_transformation_function (IR_node_t expression)
{
  return IR_IS_TYPE (IR_NODE_MODE (expression), IR_NM_expression);
}

static IR_node_t
transform_expression (IR_node_t expression)
{
  expression = IR_transform_dag (expression, FALSE,
                                 guard_transformation_function,
                                 transformation_function_1);  
  do
    {
      expression_is_transformed = FALSE;
      expression = IR_transform_dag (expression, FALSE,
                                     guard_transformation_function,
                                     transformation_function_2);  
      expression = IR_transform_dag (expression, FALSE,
                                     guard_transformation_function,
                                     transformation_function_3);
    }
  while (expression_is_transformed);
  return expression;
}

static void
transform_instruction_expressions (void)
{
  IR_node_t current_single_instruction_declaration;

  for (current_single_instruction_declaration
       = IR_single_declaration_list (description);
       current_single_instruction_declaration != NULL;
       current_single_instruction_declaration
       = IR_next_single_declaration (current_single_instruction_declaration))
    if (IR_IS_OF_TYPE (current_single_instruction_declaration,
                       IR_NM_single_instruction_declaration)
        && (current_single_instruction_declaration
            != cycle_advancing_single_instruction_declaration))
      IR_set_transformed_expression
        (current_single_instruction_declaration,
         transform_expression
         (copy_instruction_expression_and_change_expression_identifiers
          (IR_expression (current_single_instruction_declaration))));
}



/* The following variable is an array indexed by cycle.  Each element
   contains cyclic list of units which should be in the same cycle.  */
static IR_node_t *the_same_automaton_lists;

/* The function processes all alternative reservations on CYCLE in
   given EXPRESSION to check the SINGLE_UNIT_DECLARATION is not
   reserved on the all alternatives.  If it is true, the unit should
   be in the same automaton with other analogous units reserved on
   CYCLE in given EXPRESSION.  The function returns the next cycle
   after the last cycle if reservation EXPRESSION through
   NEXT_CYCLE. */
static int
unit_on_all_alternatives
  (IR_node_t single_unit_declaration, IR_node_t expression, int cycle,
   int curr_cycle, int *next_cycle)
{
  int temp, flag;

  assert (expression != NULL);
  if (IR_IS_OF_TYPE (expression, IR_NM_alternative))
    {
      /* We don't care about next_cycle because alternatives are on
	 the top of expression trees. */
      *next_cycle = 0;
      return
	(unit_on_all_alternatives
	 (single_unit_declaration, IR_left_operand (expression),
	  cycle, curr_cycle, &temp)
	 && unit_on_all_alternatives
	    (single_unit_declaration, IR_right_operand (expression),
	     cycle, curr_cycle, &temp));
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_concatenation))
    {
      flag = unit_on_all_alternatives (single_unit_declaration,
				       IR_left_operand (expression),
				       cycle, curr_cycle, &temp);
      flag = (unit_on_all_alternatives (single_unit_declaration,
				       IR_right_operand (expression),
					cycle, temp - 1, next_cycle)
	      || flag);
      return flag;
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_new_cycle_concatenation))
    {
      flag = unit_on_all_alternatives (single_unit_declaration,
				       IR_left_operand (expression),
				       cycle, curr_cycle, &temp);
      flag = (unit_on_all_alternatives (single_unit_declaration,
				       IR_right_operand (expression),
				       cycle, temp, next_cycle)
	      || flag);
      return flag;
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_nothing))
    {
      *next_cycle = curr_cycle + 1;
      return FALSE;
    }
  else
    {
      assert (IR_IS_OF_TYPE (expression, IR_NM_expression_atom)
	      && IR_IS_OF_TYPE (IR_single_declaration (expression),
				IR_NM_single_unit_declaration));
      *next_cycle = curr_cycle + 1;
      return (single_unit_declaration == IR_single_declaration (expression)
	      && curr_cycle == cycle);
    }
}

/* The function processes given EXPRESSION starting with CYCLE to find
   units which should be in the same automaton.  The function results
   the next cycle after the last expression cycle.  */
static int
form_the_same_automaton_unit_lists_from_expression (IR_node_t expression,
						    IR_node_t sub_expression,
						    int cycle)
{
  IR_node_t single_unit_declaration, last;

  assert (sub_expression != NULL);
  if (IR_IS_OF_TYPE (sub_expression, IR_NM_alternative))
    {
      form_the_same_automaton_unit_lists_from_expression
	(expression, IR_left_operand (sub_expression), cycle);
      form_the_same_automaton_unit_lists_from_expression
	(expression, IR_right_operand (sub_expression), cycle);
      return 0; /* We don't care about the result because alternatives
                   are on the top of expression trees. */
    }
  else if (IR_IS_OF_TYPE (sub_expression, IR_NM_concatenation))
    {
      return
	form_the_same_automaton_unit_lists_from_expression
	(expression, IR_right_operand (sub_expression),
	 form_the_same_automaton_unit_lists_from_expression
	 (expression, IR_left_operand (sub_expression), cycle) - 1);
    }
  else if (IR_IS_OF_TYPE (sub_expression, IR_NM_new_cycle_concatenation))
    {
      return
	form_the_same_automaton_unit_lists_from_expression
	(expression, IR_right_operand (sub_expression),
	 form_the_same_automaton_unit_lists_from_expression
	 (expression, IR_left_operand (sub_expression), cycle));
    }
  else if (IR_IS_OF_TYPE (sub_expression, IR_NM_nothing))
    return cycle + 1;
  else 
    {
      int temp;

      assert (IR_IS_OF_TYPE (sub_expression, IR_NM_expression_atom)
	      && IR_IS_OF_TYPE (IR_single_declaration (sub_expression),
				IR_NM_single_unit_declaration));
      single_unit_declaration = IR_single_declaration (sub_expression);
      if (!unit_on_all_alternatives (single_unit_declaration, expression,
				     cycle, 0, &temp))
	{
	  if (the_same_automaton_lists [cycle] == NULL)
	    the_same_automaton_lists [cycle] = single_unit_declaration;
	  else
	    {
	      for (last = the_same_automaton_lists [cycle];;)
		{
		  if (last == single_unit_declaration)
		    return cycle + 1;
		  if (IR_the_same_automaton_unit (last)
		      == the_same_automaton_lists [cycle])
		    break;
		  last = IR_the_same_automaton_unit (last);
		}
	      IR_set_the_same_automaton_unit
		(last, IR_the_same_automaton_unit (single_unit_declaration));
	      IR_set_the_same_automaton_unit
		(single_unit_declaration, the_same_automaton_lists [cycle]);
	    }
	}
      return cycle + 1;
    }
}

/* The function initializes data to search for units which should be
   in the same automaton and call function
   `form_the_same_automaton_unit_lists_from_expression' for each insn
   reservation.  */
static void
form_the_same_automaton_unit_lists (void)
{
  IR_node_t current_single_instruction_declaration;
  vlo_t the_same_automaton_lists_container;
  int i;

  VLO_CREATE (the_same_automaton_lists_container, 0);
  VLO_EXPAND (the_same_automaton_lists_container,
	      IR_max_instruction_reservation_cycles (description)
	      * sizeof (IR_node_t));
  the_same_automaton_lists = VLO_BEGIN (the_same_automaton_lists_container);
  for (current_single_instruction_declaration
       = IR_single_declaration_list (description);
       current_single_instruction_declaration != NULL;
       current_single_instruction_declaration
       = IR_next_single_declaration (current_single_instruction_declaration))
    if (IR_IS_OF_TYPE (current_single_instruction_declaration,
                       IR_NM_single_unit_declaration))
      {
	IR_set_the_same_automaton_message_reported_p
	  (current_single_instruction_declaration, FALSE);
	IR_set_the_same_automaton_unit
	  (current_single_instruction_declaration,
	   current_single_instruction_declaration);
      }
  for (current_single_instruction_declaration
       = IR_single_declaration_list (description);
       current_single_instruction_declaration != NULL;
       current_single_instruction_declaration
       = IR_next_single_declaration (current_single_instruction_declaration))
    if (IR_IS_OF_TYPE (current_single_instruction_declaration,
                       IR_NM_single_instruction_declaration)
        && (current_single_instruction_declaration
            != cycle_advancing_single_instruction_declaration))
      {
	for (i = 0;
	     i < IR_max_instruction_reservation_cycles (description);
	     i++)
	  the_same_automaton_lists [i] = NULL;
	form_the_same_automaton_unit_lists_from_expression
	  (IR_transformed_expression (current_single_instruction_declaration),
	   IR_transformed_expression (current_single_instruction_declaration),
	   0);
      }
  VLO_DELETE (the_same_automaton_lists_container);
}

/* The function finds units which should be in the same automaton and,
   if they are not, reports about it.  */
static void
check_unit_distributions_to_automata (void)
{
  IR_node_t current_single_instruction_declaration;
  IR_node_t start_unit, unit;

  form_the_same_automaton_unit_lists ();
  for (current_single_instruction_declaration
       = IR_single_declaration_list (description);
       current_single_instruction_declaration != NULL;
       current_single_instruction_declaration
       = IR_next_single_declaration (current_single_instruction_declaration))
    if (IR_IS_OF_TYPE (current_single_instruction_declaration,
                       IR_NM_single_unit_declaration))
      {
	start_unit = current_single_instruction_declaration;
	if (!IR_the_same_automaton_message_reported_p (start_unit))
	  for (unit = IR_the_same_automaton_unit (start_unit);
	       unit != start_unit;
	       unit = IR_the_same_automaton_unit (unit))
	    if (IR_single_automaton_declaration (start_unit)
		!= IR_single_automaton_declaration (unit))
	      {
		error (FALSE, IR_position (IR_identifier (start_unit)),
		       "Units `%s' and `%s' should be in the same automaton",
		       IR_identifier_itself (IR_identifier (start_unit)),
		       IR_identifier_itself (IR_identifier (unit)));
		IR_set_the_same_automaton_message_reported_p (unit, TRUE);
	      }
      }
}



static IR_node_t state_being_formed;

static IR_node_t alternative_state_being_formed;
 
static int
process_concatenations_for_forming_states (IR_node_t expression,
                                           IR_node_t automaton,
                                           int current_cycle_number)
{
  if (expression == NULL)
    return current_cycle_number;
  else if (IR_IS_OF_TYPE (expression, IR_NM_expression_atom))
    {
      assert (IR_IS_OF_TYPE (IR_single_declaration (expression),
                             IR_NM_single_unit_declaration));
      if (IR_corresponding_automaton_number (IR_single_declaration
                                             (expression))
          == IR_automaton_order_number (automaton))
        set_up_state_reservation
          (state_being_formed, current_cycle_number,
           IR_unit_number (IR_single_declaration (expression)));
      return current_cycle_number;
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_new_cycle_concatenation)
           || IR_IS_OF_TYPE (expression, IR_NM_concatenation))
    {
      current_cycle_number
        = process_concatenations_for_forming_states
          (IR_left_operand (expression), automaton, current_cycle_number);
      if (IR_IS_OF_TYPE (expression, IR_NM_new_cycle_concatenation))
        return
          process_concatenations_for_forming_states
            (IR_right_operand (expression), automaton,
             current_cycle_number + 1);
      else
        return
          process_concatenations_for_forming_states
            (IR_right_operand (expression), automaton, current_cycle_number);
    }
  else
    {
      assert (IR_IS_OF_TYPE (expression, IR_NM_nothing));
      return current_cycle_number;
    }
}

static void
finish_forming_alternative_state (IR_node_t alternative_state,
                                  IR_node_t automaton)
{
  IR_node_t state_in_table;
  IR_node_t corresponding_state;

  corresponding_state = IR_state (alternative_state);
  assert (corresponding_state != find_state (corresponding_state));
  state_in_table = insert_state (corresponding_state);
  if (state_in_table != corresponding_state)
    {
      free_state (corresponding_state);
      IR_set_state (alternative_state, state_in_table);
    }
}

static IR_node_t current_automaton_instruction_declaration;

static void
process_alternatives_for_forming_states (IR_node_t expression,
                                         IR_node_t automaton)
{
  if (expression == NULL || !IR_IS_OF_TYPE (expression, IR_NM_alternative))
    {
      alternative_state_being_formed = get_free_alternative_state ();
      state_being_formed = get_free_state (TRUE, automaton);
      IR_set_state (alternative_state_being_formed, state_being_formed);
      IR_set_next_alternative_state
        (alternative_state_being_formed,
         IR_alternative_state_list
         (current_automaton_instruction_declaration));
      IR_set_alternative_state_list (current_automaton_instruction_declaration,
                                     alternative_state_being_formed);
      (void) process_concatenations_for_forming_states (expression,
                                                        automaton, 0);
      finish_forming_alternative_state (alternative_state_being_formed,
                                        automaton);
    }
  else
    {
      process_alternatives_for_forming_states (IR_left_operand (expression),
                                               automaton);
      process_alternatives_for_forming_states (IR_right_operand (expression),
                                               automaton);
    }
}

/* ??? */
static void
create_alternative_state_list (IR_node_t automaton)
{
  IR_node_t current_single_instruction_declaration;

  for (current_automaton_instruction_declaration
       = IR_automaton_instruction_declaration_list (automaton);
       current_automaton_instruction_declaration != NULL;
       current_automaton_instruction_declaration
       = IR_next_automaton_instruction_declaration
         (current_automaton_instruction_declaration))
    {
      current_single_instruction_declaration
        = IR_single_instruction_declaration
          (current_automaton_instruction_declaration);
      if (current_single_instruction_declaration
          != cycle_advancing_single_instruction_declaration)
        {
          IR_set_alternative_state_list
            (current_automaton_instruction_declaration, NULL);
          process_alternatives_for_forming_states
            (IR_transformed_expression
             (current_single_instruction_declaration),
             automaton);
          IR_set_alternative_state_list
            (current_automaton_instruction_declaration,
             sort_and_make_unique_alternative_states_list
             (IR_alternative_state_list
              (current_automaton_instruction_declaration)));
        }
    }
}



static void
form_lists_of_automaton_instructions_with_the_same_reservations
  (IR_node_t automaton)
{
  IR_node_t current_automaton_instruction_declaration;
  int current_index;
  vlo_t first_instructions_with_the_same_reservations;
  vlo_t last_instructions_with_the_same_reservations;

  VLO_CREATE (first_instructions_with_the_same_reservations, 0);
  VLO_CREATE (last_instructions_with_the_same_reservations, 0);
  for (current_automaton_instruction_declaration
       = IR_automaton_instruction_declaration_list (automaton);
       current_automaton_instruction_declaration != NULL;
       current_automaton_instruction_declaration
       = IR_next_automaton_instruction_declaration
         (current_automaton_instruction_declaration))
    if (IR_single_instruction_declaration
        (current_automaton_instruction_declaration)
        == cycle_advancing_single_instruction_declaration)
      {
        IR_set_next_the_same_reservations_instruction
          (current_automaton_instruction_declaration, NULL);
        IR_set_first_instruction_with_the_same_reservations
          (current_automaton_instruction_declaration, TRUE);
      }
    else
      {
        for (current_index = 0;
             current_index
             < (VLO_LENGTH (first_instructions_with_the_same_reservations)
                / sizeof (IR_node_t));
             current_index++)
          if (alternative_state_list_eq
              (IR_alternative_state_list
               (current_automaton_instruction_declaration),
               IR_alternative_state_list
               (((IR_node_t *)
                 VLO_BEGIN (first_instructions_with_the_same_reservations))
                [current_index])))
            break;
        IR_set_next_the_same_reservations_instruction
          (current_automaton_instruction_declaration, NULL);
        if (current_index
            < (VLO_LENGTH (first_instructions_with_the_same_reservations)
               / sizeof (IR_node_t)))
          {
            IR_set_first_instruction_with_the_same_reservations
              (current_automaton_instruction_declaration, FALSE);
            IR_set_next_the_same_reservations_instruction
              (((IR_node_t *)
                VLO_BEGIN (last_instructions_with_the_same_reservations))
               [current_index],
               current_automaton_instruction_declaration);
            ((IR_node_t *)
             VLO_BEGIN (last_instructions_with_the_same_reservations))
              [current_index] = current_automaton_instruction_declaration;
          }
        else
          {
            VLO_ADD_MEMORY
              (first_instructions_with_the_same_reservations,
               &current_automaton_instruction_declaration, sizeof (IR_node_t));
            VLO_ADD_MEMORY
              (last_instructions_with_the_same_reservations,
               &current_automaton_instruction_declaration, sizeof (IR_node_t));
            IR_set_first_instruction_with_the_same_reservations
              (current_automaton_instruction_declaration, TRUE);
          }
      }
  VLO_DELETE (first_instructions_with_the_same_reservations);
  VLO_DELETE (last_instructions_with_the_same_reservations);
}



/* ??? */
static void
make_NDFA (IR_node_t automaton)
{
  IR_node_t current_automaton_instruction_declaration;
  IR_node_t current_single_instruction_declaration;
  IR_node_t current_alternative_state;
  IR_node_t state;
  IR_node_t start_state;
  IR_node_t another_state;
  IR_node_t cycle_advancing_automaton_instruction_declaration;
  vlo_t state_stack;

  VLO_CREATE (state_stack, 0);
  /* Create the start state (empty state). */
  start_state = insert_state (get_free_state (TRUE, automaton));
  IR_set_start_state (automaton, start_state);
  IR_set_it_was_placed_in_stack_for_NDFA_forming (start_state, TRUE);
  VLO_ADD_MEMORY (state_stack, &start_state, sizeof (state));
  while (VLO_LENGTH (state_stack) != 0)
    {
      state
        = ((IR_node_t *) VLO_BEGIN (state_stack))
          [VLO_LENGTH (state_stack) / sizeof (state) - 1];
      VLO_SHORTEN (state_stack, sizeof (state));
      cycle_advancing_automaton_instruction_declaration = NULL;
      for (current_automaton_instruction_declaration
           = IR_automaton_instruction_declaration_list (automaton);
           current_automaton_instruction_declaration != NULL;
           current_automaton_instruction_declaration
           = IR_next_automaton_instruction_declaration
             (current_automaton_instruction_declaration))
        if (IR_first_instruction_with_the_same_reservations
            (current_automaton_instruction_declaration))
          {
            current_single_instruction_declaration
              = IR_single_instruction_declaration
                (current_automaton_instruction_declaration);
            if (current_single_instruction_declaration
                != cycle_advancing_single_instruction_declaration)
              {
                for (current_alternative_state
                     = IR_alternative_state_list
                     (current_automaton_instruction_declaration);
                     current_alternative_state != NULL;
                     current_alternative_state
                     = IR_next_alternative_state (current_alternative_state))
                  {
                    another_state = IR_state (current_alternative_state);
                    if (!state_reservations_are_intersected (state,
                                                             another_state))
                      {
                        another_state = states_union (state, another_state);
                        if (!IR_it_was_placed_in_stack_for_NDFA_forming
                            (another_state))
                          {
                            IR_set_it_was_placed_in_stack_for_NDFA_forming
                              (another_state, TRUE);
                            VLO_ADD_MEMORY (state_stack, &another_state,
                                            sizeof (another_state));
                          }
                        add_arc (state, another_state,
                                 current_automaton_instruction_declaration);
                      }
                  }
              }
            else
              cycle_advancing_automaton_instruction_declaration
                = current_automaton_instruction_declaration;
          }
      /* Add transition to advance cycle. */
      another_state = state_shift (state);
      if (!IR_it_was_placed_in_stack_for_NDFA_forming (another_state))
        {
          IR_set_it_was_placed_in_stack_for_NDFA_forming (another_state, TRUE);
          VLO_ADD_MEMORY (state_stack, &another_state, sizeof (another_state));
        }
      assert (cycle_advancing_automaton_instruction_declaration != NULL);
      add_arc (state, another_state,
               cycle_advancing_automaton_instruction_declaration);
    }
  VLO_DELETE (state_stack);
}

static void
form_arcs_marked_by_instruction (IR_node_t state)
{
  IR_node_t current_single_declaration;
  IR_node_t current_arc;

  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_instruction_declaration))
      IR_set_arcs_marked_by_instruction (current_single_declaration, NULL);
  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    {
      assert (IR_instruction (current_arc) != NULL);
      IR_set_next_arc_marked_by_instruction
        (current_arc,
         IR_arcs_marked_by_instruction
         (IR_single_instruction_declaration (IR_instruction (current_arc))));
      IR_set_arcs_marked_by_instruction
        (IR_single_instruction_declaration (IR_instruction (current_arc)),
         current_arc);
    }
}

static int
there_is_arc (IR_node_t from_state, IR_node_t to_state, IR_node_t instruction)
{
  IR_node_t current_arc;

  for (current_arc = first_out_arc (from_state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    {
      if (IR_to_state (current_arc) == to_state
          && IR_instruction (current_arc) == instruction)
        return TRUE;
    }
  return FALSE;
}

static void
create_composed_state (IR_node_t original_state,
                       IR_node_t arcs_marked_by_instruction,
                       vlo_t *state_stack)
{
  IR_node_t current_state;
  IR_node_t current_alternative_state;
  IR_node_t new_alternative_state;
  IR_node_t current_arc;
  IR_node_t next_arc;
  IR_node_t state_in_table;
  IR_node_t temp_state;
  IR_node_t canonical_alternative_states_list;

  if (arcs_marked_by_instruction == NULL)
    return;
  if (IR_next_arc_marked_by_instruction (arcs_marked_by_instruction) == NULL)
    current_state = IR_to_state (arcs_marked_by_instruction);
  else
    {
      /* Create composed state. */
      current_state
        = get_free_state
          (FALSE, IR_automaton (IR_to_state (arcs_marked_by_instruction)));
      current_alternative_state = NULL;
      for (current_arc = arcs_marked_by_instruction;
           current_arc != NULL;
           current_arc = IR_next_arc_marked_by_instruction (current_arc))
        {
          new_alternative_state = get_free_alternative_state ();
          IR_set_next_alternative_state (new_alternative_state,
                                         current_alternative_state);
          IR_set_state (new_alternative_state, IR_to_state (current_arc));
          current_alternative_state = new_alternative_state;
        }
      /* There are not identical sets in the alternative state list. */
      canonical_alternative_states_list
        = sort_and_make_unique_alternative_states_list
          (current_alternative_state);
      if (IR_next_alternative_state (canonical_alternative_states_list)
          == NULL)
        {
          temp_state = current_state;
          current_state = IR_state (canonical_alternative_states_list);
          free_state (temp_state);
        }
      else
        {
          IR_set_component_states
            (current_state, canonical_alternative_states_list);
          state_in_table = insert_state (current_state);
          if (state_in_table != current_state)
            {
              assert (IR_it_was_placed_in_stack_for_DFA_forming
                      (state_in_table));
              free_state (current_state);
              current_state = state_in_table;
            }
          else
            {
              assert
                (!IR_it_was_placed_in_stack_for_DFA_forming (current_state));
              for (current_alternative_state
                   = IR_component_states (current_state);
                   current_alternative_state != NULL;
                   current_alternative_state
                   = IR_next_alternative_state (current_alternative_state))
                for (current_arc
                     = first_out_arc (IR_state (current_alternative_state));
                     current_arc != NULL;
                     current_arc = next_out_arc (current_arc))
                  if (!there_is_arc (current_state, IR_to_state (current_arc),
                                     IR_instruction (current_arc)))
                    add_arc (current_state, IR_to_state (current_arc),
                             IR_instruction (current_arc));
            }
          IR_set_to_state (arcs_marked_by_instruction, current_state);
          for (current_arc
               = IR_next_arc_marked_by_instruction
                 (arcs_marked_by_instruction);
               current_arc != NULL;
               current_arc = next_arc)
            {
              next_arc = IR_next_arc_marked_by_instruction (current_arc);
              remove_arc (original_state, current_arc);
            }
        }
    }
  if (!IR_it_was_placed_in_stack_for_DFA_forming (current_state))
    {
      IR_set_it_was_placed_in_stack_for_DFA_forming (current_state, TRUE);
      VLO_ADD_MEMORY (*state_stack, &current_state, sizeof (current_state));
    }
}

/* ??? */
static void
NDFA_to_DFA (IR_node_t automaton)
{
  IR_node_t start_state;
  IR_node_t state;
  IR_node_t current_single_declaration;
  vlo_t state_stack;

  VLO_CREATE (state_stack, 0);
  /* Create the start state (empty state). */
  start_state = IR_start_state (automaton);
  IR_set_it_was_placed_in_stack_for_DFA_forming (start_state, TRUE);
  VLO_ADD_MEMORY (state_stack, &start_state, sizeof (state));
  while (VLO_LENGTH (state_stack) != 0)
    {
      state
        = ((IR_node_t *) VLO_BEGIN (state_stack))
          [VLO_LENGTH (state_stack) / sizeof (state) - 1];
      VLO_SHORTEN (state_stack, sizeof (state));
      form_arcs_marked_by_instruction (state);
      for (current_single_declaration
           = IR_single_declaration_list (description);
           current_single_declaration != NULL;
           current_single_declaration
           = IR_next_single_declaration (current_single_declaration))
        if (IR_IS_OF_TYPE (current_single_declaration,
                           IR_NM_single_instruction_declaration))
          create_composed_state
            (state, IR_arcs_marked_by_instruction (current_single_declaration),
             &state_stack);
    }
  VLO_DELETE (state_stack);
}

/* The following variable value is current number (1, 2, ...) of
   passing graph of states. */

static int current_state_graph_pass_number;

static void
pass_state_graph (IR_node_t start_state,
                  void (*applied_function) (IR_node_t state))
{
  IR_node_t current_arc;

  if (IR_pass_number (start_state) == current_state_graph_pass_number)
    return;
  IR_set_pass_number (start_state, current_state_graph_pass_number);
  (*applied_function) (start_state);
  for (current_arc = first_out_arc (start_state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    pass_state_graph (IR_to_state (current_arc), applied_function);
}

static void
pass_states (IR_node_t automaton, void (*applied_function) (IR_node_t state))
{
  current_state_graph_pass_number++;
  pass_state_graph (IR_start_state (automaton), applied_function);
}

static void
initiate_pass_states (void)
{
  current_state_graph_pass_number = 0;
}

static vlo_t all_achieved_states;

static void
add_achieved_state (IR_node_t state)
{
  VLO_ADD_MEMORY (all_achieved_states, &state, sizeof (state));
}

static int
set_up_equivalence_number_of_output_arcs_instructions (IR_node_t state,
                                                       int odd_iteration_flag)
{
  int state_out_arcs_number;
  IR_node_t current_arc;

  state_out_arcs_number = 0;
  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    {
      state_out_arcs_number++;
      IR_set_equivalence_class_number
        (IR_single_instruction_declaration (IR_instruction (current_arc)),
         (odd_iteration_flag
          ? IR_equivalence_class_number_1 (IR_to_state (current_arc))
          : IR_equivalence_class_number_2 (IR_to_state (current_arc))));
    }
  return state_out_arcs_number;
}

static void
zero_equivalence_number_of_output_arcs_instructions (IR_node_t state)
{
  IR_node_t current_arc;

  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    IR_set_equivalence_class_number
      (IR_single_instruction_declaration (IR_instruction (current_arc)), 0);
}

static void
copy_equivalence_class (vlo_t *to, const vlo_t *from)
{
  IR_node_t *current_class_ptr;

  VLO_NULLIFY (*to);
  for (current_class_ptr = VLO_BEGIN (*from);
       (char *) current_class_ptr
       < (char *) VLO_BEGIN (*from) + VLO_LENGTH (*from);
       current_class_ptr++)
    VLO_ADD_MEMORY (*to, current_class_ptr, sizeof (IR_node_t));
}

static int
state_is_differed (IR_node_t state, int original_state_out_arcs_number,
                   int odd_iteration_flag)
{
  IR_node_t current_arc;
  int state_out_arcs_number;

  state_out_arcs_number = 0;
  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    {
      state_out_arcs_number++;
      if ((odd_iteration_flag
           ? IR_equivalence_class_number_1 (IR_to_state (current_arc))
           : IR_equivalence_class_number_2 (IR_to_state (current_arc)))
          != IR_equivalence_class_number (IR_single_instruction_declaration
                                          (IR_instruction (current_arc))))
        return TRUE;
    }
  return state_out_arcs_number != original_state_out_arcs_number;
}

static IR_node_t
form_original_equivalence_class (IR_node_t *states, int states_number)
{
  IR_node_t *current_state_ptr;
  IR_node_t result_equivalence_class;

  result_equivalence_class = NULL;
  for (current_state_ptr = states;
       current_state_ptr < states + states_number;
       current_state_ptr++)
    {
      IR_set_equivalence_class_number_1 (*current_state_ptr, 1);
      IR_set_next_equivalence_class_state (*current_state_ptr,
                                           result_equivalence_class);
      result_equivalence_class = *current_state_ptr;
    }
  return result_equivalence_class;
}

/* ??? */
static void
evaluate_equivalence_classes (IR_node_t automaton, vlo_t *equivalence_classes)
{
  IR_node_t new_equivalence_class;
  int new_equivalence_class_number;
  int odd_iteration_flag;
  int finish_flag;
  vlo_t next_iteration_classes;
  IR_node_t first_state;
  IR_node_t current_state;
  IR_node_t previous_state;
  IR_node_t next_state;
  IR_node_t *current_equivalence_class_ptr;
  int first_state_out_arcs_number;
  
  VLO_CREATE (all_achieved_states, 5000);
  pass_states (automaton, add_achieved_state);
  new_equivalence_class
    = form_original_equivalence_class
      (VLO_BEGIN (all_achieved_states),
       VLO_LENGTH (all_achieved_states) / sizeof (IR_node_t));
  odd_iteration_flag = FALSE;
  new_equivalence_class_number = 1;
  VLO_CREATE (next_iteration_classes, 0);
  VLO_ADD_MEMORY (next_iteration_classes, &new_equivalence_class,
                  sizeof (new_equivalence_class));
  do
    {
      odd_iteration_flag = !odd_iteration_flag;
      finish_flag = TRUE;
      copy_equivalence_class (equivalence_classes, &next_iteration_classes);
      for (current_equivalence_class_ptr
           = VLO_BEGIN (*equivalence_classes);
           (char *) current_equivalence_class_ptr
           < ((char *) VLO_BEGIN (*equivalence_classes)
              + VLO_LENGTH (*equivalence_classes));
           current_equivalence_class_ptr++)
        {
          assert (*current_equivalence_class_ptr != NULL);
          for (first_state = *current_equivalence_class_ptr;
               first_state != NULL;
               first_state = new_equivalence_class)
            {
              new_equivalence_class = NULL;
              if (IR_next_equivalence_class_state (first_state) != NULL)
                {
                  /* There are more one states in the class equivalence. */
                  first_state_out_arcs_number
                    = set_up_equivalence_number_of_output_arcs_instructions
                      (first_state, odd_iteration_flag);
                  for (previous_state = first_state,
                       current_state
                       = IR_next_equivalence_class_state (first_state);
                       current_state != NULL;
                       current_state = next_state)
                    {
                      next_state
                        = IR_next_equivalence_class_state (current_state);
                      if (state_is_differed (current_state,
                                             first_state_out_arcs_number,
                                             odd_iteration_flag))
                        {
                          /* Remove current state from the class
                             equivalence. */
                          IR_set_next_equivalence_class_state (previous_state,
                                                               next_state);
                          /* Add current state to the new class equivalence. */
                          IR_set_next_equivalence_class_state
                            (current_state, new_equivalence_class);
                          if (new_equivalence_class == NULL)
                            new_equivalence_class_number++;
                          if (odd_iteration_flag)
                            IR_set_equivalence_class_number_2
                              (current_state, new_equivalence_class_number);
                          else
                            IR_set_equivalence_class_number_1
                              (current_state, new_equivalence_class_number);
                          new_equivalence_class = current_state;
                          finish_flag = FALSE;
                        }
                      else
                        previous_state = current_state;
                    }
                  zero_equivalence_number_of_output_arcs_instructions
                    (first_state);
                }
              if (new_equivalence_class != NULL)
                VLO_ADD_MEMORY  (next_iteration_classes,
                                 &new_equivalence_class,
                                 sizeof (new_equivalence_class));
            }
        }
    }
  while (!finish_flag);
  VLO_DELETE (next_iteration_classes);
  VLO_DELETE (all_achieved_states);
}

static void
merge_states (IR_node_t automaton, vlo_t *equivalence_classes)
{
  IR_node_t *current_equivalence_class_ptr;
  IR_node_t current_state;
  IR_node_t new_state;
  IR_node_t first_class_state;
  IR_node_t alternative_state_list;
  IR_node_t new_alternative_state;
  IR_node_t current_arc;
  IR_node_t next_arc;

  /* Create states corresponding to equivalence classes containing two
     or more states. */
  for (current_equivalence_class_ptr
       = VLO_BEGIN (*equivalence_classes);
       (char *) current_equivalence_class_ptr
       < ((char *) VLO_BEGIN (*equivalence_classes)
          + VLO_LENGTH (*equivalence_classes));
       current_equivalence_class_ptr++)
    if (IR_next_equivalence_class_state (*current_equivalence_class_ptr)
        != NULL)
      {
        /* There are more one states in the class equivalence. */
        /* Create new compound state. */
        new_state = get_free_state (FALSE, automaton);
        alternative_state_list = NULL;
        first_class_state = *current_equivalence_class_ptr;
        for (current_state = first_class_state;
             current_state != NULL;
             current_state = IR_next_equivalence_class_state (current_state))
          {
            IR_set_equivalence_class_state (current_state, new_state);
            new_alternative_state = get_free_alternative_state ();
            IR_set_state (new_alternative_state, current_state);
            IR_set_next_alternative_state (new_alternative_state,
                                           alternative_state_list);
            alternative_state_list = new_alternative_state;
          }
        IR_set_component_states (new_state, alternative_state_list);
      }
    else
      IR_set_equivalence_class_state (*current_equivalence_class_ptr,
                                      *current_equivalence_class_ptr);
  for (current_equivalence_class_ptr
       = VLO_BEGIN (*equivalence_classes);
       (char *) current_equivalence_class_ptr
       < ((char *) VLO_BEGIN (*equivalence_classes)
          + VLO_LENGTH (*equivalence_classes));
       current_equivalence_class_ptr++)
    if (IR_next_equivalence_class_state (*current_equivalence_class_ptr)
        != NULL)
      {
        first_class_state = *current_equivalence_class_ptr;
        /* Create new arcs output from the state corresponding to
           equivalence class. */
        for (current_arc = first_out_arc (first_class_state);
             current_arc != NULL;
             current_arc = next_out_arc (current_arc))
          add_arc (IR_equivalence_class_state (first_class_state),
                   IR_equivalence_class_state (IR_to_state (current_arc)),
                   IR_instruction (current_arc));
        /* Delete output arcs from states of given class equivalence. */
        for (current_state = first_class_state;
             current_state != NULL;
             current_state = IR_next_equivalence_class_state (current_state))
          {
            if (IR_start_state (automaton) == current_state)
              IR_set_start_state (automaton,
                                  IR_equivalence_class_state (current_state));
            /* Delete the state and its output arcs. */
            for (current_arc = first_out_arc (current_state);
                 current_arc != NULL;
                 current_arc = next_arc)
              {
                next_arc = next_out_arc (current_arc);
                free_arc (current_arc);
              }
          }
      }
    else
      {
        /* Change `to_state' of arcs output from the single state of given
           equivalence class. */
        for (current_arc = first_out_arc (*current_equivalence_class_ptr);
             current_arc != NULL;
             current_arc = next_out_arc (current_arc))
          IR_set_to_state
            (current_arc,
             IR_equivalence_class_state (IR_to_state (current_arc)));
      }
}

static void
minimize_DFA (IR_node_t automaton)
{
  vlo_t equivalence_classes;

  VLO_CREATE (equivalence_classes, 5000);
  evaluate_equivalence_classes (automaton, &equivalence_classes);
  merge_states (automaton, &equivalence_classes);
  VLO_DELETE (equivalence_classes);
}

static int current_counted_states_number;
static int current_counted_arcs_number;

static void
increment_states_and_arcs_numbers (IR_node_t state)
{
  IR_node_t current_arc;

  current_counted_states_number++;
  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    current_counted_arcs_number++;
}

static void
count_states_and_arcs (IR_node_t automaton,
                       int *states_number, int *arcs_number)
{
  current_counted_states_number = 0;
  current_counted_arcs_number = 0;
  pass_states (automaton, increment_states_and_arcs_numbers);
  *states_number = current_counted_states_number;
  *arcs_number = current_counted_arcs_number;
}

/* ??? */
static void
make_automaton (IR_node_t automaton)
{
  int states_number;
  int arcs_number;

  ticker_on (&NDFA_time);
  make_NDFA (automaton);
  ticker_off (&NDFA_time);
  count_states_and_arcs (automaton, &states_number, &arcs_number);
  IR_set_NDFA_states_number (automaton, states_number);
  IR_set_NDFA_arcs_number (automaton, arcs_number);
  ticker_on (&NDFA_to_DFA_time);
  NDFA_to_DFA (automaton);
  ticker_off (&NDFA_to_DFA_time);
  count_states_and_arcs (automaton, &states_number, &arcs_number);
  IR_set_DFA_states_number (automaton, states_number);
  IR_set_DFA_arcs_number (automaton, arcs_number);
  if (!no_minimization_flag)
    {
      ticker_on (&minimize_time);
      minimize_DFA (automaton);
      ticker_off (&minimize_time);
      count_states_and_arcs (automaton, &states_number, &arcs_number);
      IR_set_minimal_DFA_states_number (automaton, states_number);
      IR_set_minimal_DFA_arcs_number (automaton, arcs_number);
    }
}



static int current_state_order_number;

static void
set_order_state_number (IR_node_t state)
{
  IR_set_order_state_number (state, current_state_order_number);
  current_state_order_number++;
}

static void
enumerate_states (IR_node_t automaton)
{
  current_state_order_number = 0;
  pass_states (automaton, set_order_state_number);
  IR_set_achieved_states_number (automaton, current_state_order_number);
}



static IR_node_t
insert_automaton_instruction_into_equivalence_class
  (IR_node_t instruction, IR_node_t cyclic_equivalence_class_instruction_list)
{
  if (cyclic_equivalence_class_instruction_list == NULL)
    IR_set_next_equivalence_class_instruction (instruction, instruction);
  else
    {
      IR_set_next_equivalence_class_instruction
        (instruction,
         IR_next_equivalence_class_instruction
         (cyclic_equivalence_class_instruction_list));
      IR_set_next_equivalence_class_instruction
        (cyclic_equivalence_class_instruction_list, instruction);
    }
  return instruction;
}

static void
delete_automaton_instruction_from_equivalence_class
(IR_node_t equivalence_class_instruction)
{
  IR_node_t current_equivalence_class_instruction;
  IR_node_t previous_equivalence_class_instruction;

  previous_equivalence_class_instruction = equivalence_class_instruction;
  for (current_equivalence_class_instruction
       = IR_next_equivalence_class_instruction (equivalence_class_instruction);
       current_equivalence_class_instruction != equivalence_class_instruction;
       current_equivalence_class_instruction
       = IR_next_equivalence_class_instruction
         (current_equivalence_class_instruction))
    previous_equivalence_class_instruction
      = current_equivalence_class_instruction;
  if (previous_equivalence_class_instruction != equivalence_class_instruction)
    IR_set_next_equivalence_class_instruction
      (previous_equivalence_class_instruction,
       IR_next_equivalence_class_instruction (equivalence_class_instruction));
}

static void
process_instruction_equivalence_class (IR_node_t instruction,
                                       IR_node_t *instruction_arcs_array)
{
  IR_node_t next_equivalence_class_instruction;
  IR_node_t current_equivalence_class_instruction;
  IR_node_t cyclic_equivalence_class_instruction_list;
  
  assert (instruction_arcs_array
          [IR_instruction_number
           (IR_single_instruction_declaration (instruction))] != NULL);
  current_equivalence_class_instruction = instruction;
  /* New class of instructions which are not equivalent to given
     instruction. */
  cyclic_equivalence_class_instruction_list = NULL;
  do
    {
      next_equivalence_class_instruction
        = IR_next_equivalence_class_instruction
          (current_equivalence_class_instruction);
      if (instruction_arcs_array
          [IR_instruction_number
           (IR_single_instruction_declaration
            (current_equivalence_class_instruction))] == NULL
          || (IR_to_state
              (instruction_arcs_array [IR_instruction_number
                                       (IR_single_instruction_declaration
                                        (instruction))])
              != IR_to_state (instruction_arcs_array
                              [IR_instruction_number
                               (IR_single_instruction_declaration
                                (current_equivalence_class_instruction))])))
        {
          delete_automaton_instruction_from_equivalence_class
            (current_equivalence_class_instruction);
          cyclic_equivalence_class_instruction_list
            = insert_automaton_instruction_into_equivalence_class
              (current_equivalence_class_instruction,
               cyclic_equivalence_class_instruction_list);
        }
      current_equivalence_class_instruction
        = next_equivalence_class_instruction;
    }
  while (current_equivalence_class_instruction != instruction);
}

static void
process_state_for_instruction_equivalence_partition (IR_node_t state)
{
  IR_node_t current_arc;
  IR_node_t *instruction_arcs_array;
  int current_instruction_number;
  vlo_t instruction_arcs_vector;

  VLO_CREATE (instruction_arcs_vector, 2000);
  VLO_EXPAND (instruction_arcs_vector,
              sizeof (IR_node_t) * IR_instructions_number (description));
  instruction_arcs_array = VLO_BEGIN (instruction_arcs_vector);
  /* Process instructions of the arcs. */
  for (current_instruction_number = 0;
       current_instruction_number < IR_instructions_number (description);
       current_instruction_number++)
    instruction_arcs_array [current_instruction_number] = NULL;
  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    instruction_arcs_array
      [IR_instruction_number (IR_single_instruction_declaration
                              (IR_instruction (current_arc)))] = current_arc;
  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    process_instruction_equivalence_class (IR_instruction (current_arc),
                                           instruction_arcs_array);
  VLO_DELETE (instruction_arcs_vector);
}

static void
set_instruction_equivalence_classes (IR_node_t automaton)
{
  IR_node_t current_automaton_instruction_declaration;
  IR_node_t first_equivalence_class_instruction;
  IR_node_t current_equivalence_class_instruction;
  IR_node_t cyclic_equivalence_class_instruction_list;
  IR_node_t current_instruction_with_the_same_reservations;
  int equivalence_classes_number;

  /* All instructions are included in one equivalence class. */
  cyclic_equivalence_class_instruction_list = NULL;
  for (current_automaton_instruction_declaration
       = IR_automaton_instruction_declaration_list (automaton);
       current_automaton_instruction_declaration != NULL;
       current_automaton_instruction_declaration
       = IR_next_automaton_instruction_declaration
         (current_automaton_instruction_declaration))
    if (IR_first_instruction_with_the_same_reservations
        (current_automaton_instruction_declaration))
      cyclic_equivalence_class_instruction_list
        = insert_automaton_instruction_into_equivalence_class
          (current_automaton_instruction_declaration,
           cyclic_equivalence_class_instruction_list);
  /* Process instructions in order to make equivalence partition. */
  pass_states (automaton, process_state_for_instruction_equivalence_partition);
  /* Enumerate equivalence classes. */
  for (current_automaton_instruction_declaration
       = IR_automaton_instruction_declaration_list (automaton);
       current_automaton_instruction_declaration != NULL;
       current_automaton_instruction_declaration
       = IR_next_automaton_instruction_declaration
         (current_automaton_instruction_declaration))
    /* Set undefined value. */
    IR_set_instruction_equivalence_class_number
      (current_automaton_instruction_declaration, -1);
  equivalence_classes_number = 0;
  for (current_automaton_instruction_declaration
       = IR_automaton_instruction_declaration_list (automaton);
       current_automaton_instruction_declaration != NULL;
       current_automaton_instruction_declaration
       = IR_next_automaton_instruction_declaration
         (current_automaton_instruction_declaration))
    if (IR_instruction_equivalence_class_number
        (current_automaton_instruction_declaration) < 0)
      {
        first_equivalence_class_instruction
          = current_automaton_instruction_declaration;
        assert (IR_first_instruction_with_the_same_reservations
                (first_equivalence_class_instruction));
        IR_set_first_out_arc_with_given_equialence_number
          (first_equivalence_class_instruction, TRUE);
        current_equivalence_class_instruction
          = first_equivalence_class_instruction;
        do
          {
            for (current_instruction_with_the_same_reservations
                 = current_equivalence_class_instruction;
                 current_instruction_with_the_same_reservations != NULL;
                 current_instruction_with_the_same_reservations
                 = IR_next_the_same_reservations_instruction
                   (current_instruction_with_the_same_reservations))
              IR_set_instruction_equivalence_class_number
                (current_instruction_with_the_same_reservations,
                 equivalence_classes_number);
            current_equivalence_class_instruction
              = IR_next_equivalence_class_instruction
                (current_equivalence_class_instruction);
          }
        while (current_equivalence_class_instruction
               != first_equivalence_class_instruction);
        equivalence_classes_number++;
      }
  IR_set_instruction_equivalence_classes_number (automaton,
                                                 equivalence_classes_number);
}



static double
root (double argument, int root_number)
{
  assert (argument > 0 && root_number >= 1);
  return exp (log (argument) / root_number);
}

static double
evaluate_one_automaton_estimation_bound (void)
{
  IR_node_t current_single_declaration;
  double one_automaton_estimation_bound;
  double root_value;

  one_automaton_estimation_bound = 1.0;
  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_unit_declaration))
      {
        root_value
          = root (IR_max_occurrence_cycle_number (current_single_declaration)
                  + 1.0, automatons_number);
        if (DBL_MAX / root_value > one_automaton_estimation_bound)
          one_automaton_estimation_bound *= root_value;
      }
  return one_automaton_estimation_bound;
}

static int
compare_max_occurrence_cycle_numbers (const void *single_unit_declaration_1,
                                      const void *single_unit_declaration_2)
{
  if (IR_max_occurrence_cycle_number (*(IR_node_t *) single_unit_declaration_1)
      < IR_max_occurrence_cycle_number (*(IR_node_t *)
                                        single_unit_declaration_2))
    return 1;
  else if (IR_max_occurrence_cycle_number (*(IR_node_t *)
                                           single_unit_declaration_1)
           == IR_max_occurrence_cycle_number (*(IR_node_t *)
                                              single_unit_declaration_2))
    return 0;
  else
    return -1;
}

static void
distribute_automatically_units_to_automatons (void)
{
  double one_automaton_estimation_bound;
  IR_node_t current_single_declaration;
  IR_node_t *current_single_unit_declaration_ptr;
  int current_automaton_number;
  int rest_units_number;
  double current_bound_value;
  vlo_t unit_declarations;

  if (IR_units_number (description) == 0)
    return;
  one_automaton_estimation_bound = evaluate_one_automaton_estimation_bound ();
  VLO_CREATE (unit_declarations, 0);
  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_unit_declaration))
      VLO_ADD_MEMORY (unit_declarations, &current_single_declaration,
                      sizeof (current_single_declaration));
  qsort (VLO_BEGIN (unit_declarations),
         VLO_LENGTH (unit_declarations) / sizeof (IR_node_t),
         sizeof (IR_node_t), compare_max_occurrence_cycle_numbers);
  current_automaton_number = 0;
  current_single_unit_declaration_ptr = VLO_BEGIN (unit_declarations);
  current_bound_value
    = IR_max_occurrence_cycle_number (*current_single_unit_declaration_ptr);
  IR_set_corresponding_automaton_number
    (*current_single_unit_declaration_ptr, current_automaton_number);
  for (current_single_unit_declaration_ptr++;
       (char *) current_single_unit_declaration_ptr
       <= (char *) VLO_END (unit_declarations);
       current_single_unit_declaration_ptr++)
    {
      rest_units_number
        = (IR_node_t *) ((char *) VLO_END (unit_declarations) + 1)
          - current_single_unit_declaration_ptr;
      assert (automatons_number - current_automaton_number - 1
              <= rest_units_number);
      if (current_automaton_number < automatons_number - 1
          && ((automatons_number - current_automaton_number - 1
               == rest_units_number)
              || (current_bound_value
                  > (one_automaton_estimation_bound
                     / (IR_max_occurrence_cycle_number
                        (*current_single_unit_declaration_ptr))))))
        {
          current_bound_value
            = IR_max_occurrence_cycle_number
              (*current_single_unit_declaration_ptr);
          current_automaton_number++;
        }
      else
        current_bound_value
          *= IR_max_occurrence_cycle_number
            (*current_single_unit_declaration_ptr);
      IR_set_corresponding_automaton_number
        (*current_single_unit_declaration_ptr, current_automaton_number);
    }
  assert (current_automaton_number == automatons_number - 1);
  VLO_DELETE (unit_declarations);
}


static IR_node_t
create_automaton_instruction_declaration_list (void)
{
  IR_node_t current_single_instruction_declaration;
  IR_node_t first_automaton_instruction_declaration;
  IR_node_t current_automaton_instruction_declaration;
  IR_node_t previous_automaton_instruction_declaration;

  first_automaton_instruction_declaration = NULL;
  for (previous_automaton_instruction_declaration = NULL,
       current_single_instruction_declaration
       = IR_single_declaration_list (description);
       current_single_instruction_declaration != NULL;
       current_single_instruction_declaration
       = IR_next_single_declaration (current_single_instruction_declaration))
    if (IR_IS_OF_TYPE (current_single_instruction_declaration,
                       IR_NM_single_instruction_declaration))
      {
        current_automaton_instruction_declaration
          = IR_new_automaton_instruction_declaration
            (current_single_instruction_declaration, NULL);
        if (previous_automaton_instruction_declaration == NULL)
          first_automaton_instruction_declaration
            = current_automaton_instruction_declaration;
        else
          IR_set_next_automaton_instruction_declaration
            (previous_automaton_instruction_declaration,
             current_automaton_instruction_declaration);
        previous_automaton_instruction_declaration
          = current_automaton_instruction_declaration;
      }
  return first_automaton_instruction_declaration;
}

static void
distribute_units_to_automatons_according_to_description (void)
{
  IR_node_t  current_single_declaration;

  for (current_single_declaration
       = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_unit_declaration))
      {
        if (IR_single_automaton_declaration (current_single_declaration)
            == NULL
            || (IR_corresponding_automaton
                (IR_single_automaton_declaration (current_single_declaration))
                == NULL))
          /* Distribute to the first automaton. */
          IR_set_corresponding_automaton_number (current_single_declaration,
                                                 0);
        else
          IR_set_corresponding_automaton_number
            (current_single_declaration,
             IR_automaton_order_number
             (IR_corresponding_automaton
              (IR_single_automaton_declaration (current_single_declaration))));
      }
}


static void
create_automatons (void)
{
  IR_node_t current_automaton;
  IR_node_t previous_automaton;
  IR_node_t  current_single_declaration;
  int current_automaton_number;

  if (automatons_number != 0)
    {
      distribute_automatically_units_to_automatons ();
      for (previous_automaton = NULL, current_automaton_number = 0;
           current_automaton_number < automatons_number;
           current_automaton_number++, previous_automaton = current_automaton)
        {
          current_automaton
            = IR_new_automaton
              (create_automaton_instruction_declaration_list (), NULL, NULL);
          IR_set_automaton_order_number (current_automaton,
                                         current_automaton_number);
          if (previous_automaton == NULL)
            IR_set_first_automaton (description, current_automaton);
          else
            IR_set_next_automaton (previous_automaton, current_automaton);
        }
    }
  else
    {
      current_automaton_number = 0;
      previous_automaton = NULL;
      for (current_single_declaration
           = IR_single_declaration_list (description);
           current_single_declaration != NULL;
           current_single_declaration
           = IR_next_single_declaration (current_single_declaration))
        if (IR_IS_OF_TYPE (current_single_declaration,
                           IR_NM_single_automaton_declaration)
            && IR_automaton_is_used (current_single_declaration))
          {
            current_automaton
              = IR_new_automaton
                (create_automaton_instruction_declaration_list (),
                 current_single_declaration, NULL);
            IR_set_corresponding_automaton (current_single_declaration,
                                            current_automaton);
            IR_set_automaton_order_number (current_automaton,
                                           current_automaton_number);
            if (previous_automaton == NULL)
              IR_set_first_automaton (description, current_automaton);
            else
              IR_set_next_automaton (previous_automaton, current_automaton);
            current_automaton_number++;
            previous_automaton = current_automaton;
          }
      if (current_automaton_number == 0)
        IR_set_first_automaton
          (description,
           IR_new_automaton
           (create_automaton_instruction_declaration_list (), NULL, NULL));
      distribute_units_to_automatons_according_to_description ();
    }
  NDFA_time = create_ticker ();
  ticker_off (&NDFA_time);
  NDFA_to_DFA_time = create_ticker ();
  ticker_off (&NDFA_to_DFA_time);
  minimize_time = create_ticker ();
  ticker_off (&minimize_time);
  equivalence_time = create_ticker ();
  ticker_off (&equivalence_time);
  for (current_automaton = IR_first_automaton (description);
       current_automaton != NULL;
       current_automaton = IR_next_automaton (current_automaton))
    {
      create_alternative_state_list (current_automaton);
      form_lists_of_automaton_instructions_with_the_same_reservations
        (current_automaton);
      make_automaton (current_automaton);
      enumerate_states (current_automaton);
      ticker_on (&equivalence_time);
      set_instruction_equivalence_classes (current_automaton);
      ticker_off (&equivalence_time);
    }
}



/* ??? Is it used. */

/* This page contains functions which are used to output parameterized
   names of PHR objects.  Usually the parameter is prefix given in OKA
   command line (see commentaries for variable `prefix'). */

/* Base name of debug macro parameter used to separate debug code in
   PHR.  Full name of the macro parameter is `__OKA_DEBUG__' (see
   function `output_ifdef_parameter_name'). */

#define DEBUG_PARAMETER_NAME "DEBUG"

/* This function outputs name of debug macro parameter used to
 separate debug code in PHR.  The name is output as
 `__<prefix>_<base_name>__' where prefix is value of variable `prefix'
 and base name is value of the second parameter.  */

static void
output_ifdef_parameter_name (FILE *f, const char *ifdef_parameter_name)
{
  output_string (f, "__");
  output_string (f, prefix);
  output_char ('_', f);
  output_string (f, ifdef_parameter_name);
  output_string (f, "__");
}


/* Is it used? */

/* This page contains functions for output of C preprocessors lines
   (`#ifdef ...', `#ifndef ', `#endif ... ', and `#line ...'). */


/* This function outputs C preprocessor line `#ifdef ...'  which
   contains parameter with given base name (see function
   `output_ifdef_parameter_name'). */

#if 0
static void
output_ifdef (FILE *f, const char *ifdef_parameter_base_name)
{
  output_string (f, "#ifdef ");
  output_ifdef_parameter_name (f, ifdef_parameter_base_name);
  output_char ('\n', f);
}
#endif

/* This function outputs C preprocessor line `#ifndef ...'  which
   contains parameter with given base name (see function
   `output_ifdef_parameter_name'). */

static void
output_ifndef (FILE *f, const char *ifdef_parameter_base_name)
{
  output_string (f, "#ifndef ");
  output_ifdef_parameter_name (f, ifdef_parameter_base_name);
  output_char ('\n', f);
}

/* This function outputs C preprocessor line `#endif ...'  which
   contains parameter with given base name (see function
   `output_ifdef_parameter_name'). */

static void
output_endif (FILE *f, const char *ifdef_parameter_base_name)
{
  output_string (f, "#endif /* ");
  output_ifdef_parameter_name (f, ifdef_parameter_base_name);
  output_string (f, " */\n");
}

/* This function outputs C preprocessor line `#line ...'  which
   contains given line number and given file name as C string
   constant. */

static void
output_line (FILE *f, int line_number, const char *file_name)
{
  output_string (f, "\n#line ");
  output_decimal_number (f, line_number, 0);
  output_string (f, " \"");
  output_string (f, file_name);
  output_string (f, "\"\n");
}

/* ??? */

static void
output_current_line (FILE *f)
{
  output_string (f, "\n#line ");
  if (f == output_interface_file)
    {
      output_decimal_number (f, current_interface_file_line + 1, 0);
      output_string (f, " \"");
      output_string (f, output_interface_file_name);
    }
  else
    {
      assert (f == output_implementation_file);
      output_decimal_number (f, current_implementation_file_line + 1, 0);
      output_string (f, " \"");
      output_string (f, output_implementation_file_name);
    }
  output_string (f, "\"\n");
}



/* ??? */
static void
output_start_code_insertions (void)
{
  IR_node_t current_declaration;

  if (IR_declaration_list (description) == NULL)
    return;
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_import_code))
      {
        output_line (output_interface_file,
                     IR_position (current_declaration).line_number,
                     IR_position (current_declaration).file_name);
        output_string (output_interface_file,
                       IR_code_insertion_itself (IR_code_itself
                                                 (current_declaration)));
        output_char ('\n', output_interface_file);
        output_current_line (output_interface_file);
      }
    else if (IR_IS_OF_TYPE (current_declaration, IR_NM_local_code))
      {
        output_line (output_implementation_file,
                     IR_position (current_declaration).line_number,
                     IR_position (current_declaration).file_name);
        output_string (output_implementation_file,
                       IR_code_insertion_itself (IR_code_itself
                                                 (current_declaration)));
        output_char ('\n', output_implementation_file);
        output_current_line (output_implementation_file);
      }
}

static void
output_range_type (FILE *f, long int min_range_value, long int max_range_value)
{
  if (min_range_value >= 0 && max_range_value <= UCHAR_MAX)
    output_string (f, "unsigned char");
  else if (min_range_value >= SCHAR_MIN && max_range_value <= SCHAR_MAX)
    output_string (f, "signed char");
  else if (min_range_value >= 0 && max_range_value <= USHRT_MAX)
    output_string (f, "unsigned short");
  else if (min_range_value >= SHRT_MIN && max_range_value <= SHRT_MAX)
    output_string (f, "short");
  else
    {
      assert (min_range_value >= INT_MIN && max_range_value <= INT_MAX);
      output_string (f, "int");
    }
}

typedef int vector_element_t;

static void
output_vector_element_type (vector_element_t min_vector_element_value,
                            vector_element_t max_vector_element_value)
{
  output_range_type (output_implementation_file, min_vector_element_value,
                     max_vector_element_value);
}

static void
output_vector (vector_element_t *vector, int vector_length)
{
  int elements_on_line;

  elements_on_line = 1;
  if (vector_length == 0)
    {
      output_decimal_number (output_implementation_file, 0, 0);
      output_string
        (output_implementation_file,
         " /* This is dummy element because the vector is empty */");
    }
  else
    {
      do
        {
          output_decimal_number (output_implementation_file, *vector, 5);
          vector_length--;
          if (elements_on_line == 10)
          {
            elements_on_line = 0;
            output_string (output_implementation_file, ",\n");
          }
          else if (vector_length != 0)
            output_string (output_implementation_file, ", ");
          elements_on_line++;
          vector++;
        }
      while (vector_length != 0);
    }
}

static void
output_chip_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_chip");
}

static void
output_chip_member_name (FILE *f, IR_node_t automaton)
{
  output_string (f, prefix);
  output_char ('_', f);
  if (IR_corresponding_single_automaton_declaration (automaton) == NULL)
    output_decimal_number (f, IR_automaton_order_number (automaton), 0);
  else
    output_string
      (f,
       IR_identifier_itself
       (IR_identifier
        (IR_corresponding_single_automaton_declaration (automaton))));
  output_string (f, "_automaton_state");
}

static void
output_temporary_chip_member_name (FILE *f, IR_node_t automaton)
{
  output_char ('_', f);
  output_chip_member_name (f, automaton);
}

static void
output_instruction_value_name (FILE *f, const char *instruction_identifier)
{
  output_string (f, prefix);
  output_char ('_', f);
  output_string (f, instruction_identifier);
}

static void
output_advancing_cycle_value_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "__ADVANCE_CYCLE");
}

static void
output_undefined_internal_instruction_code_name (FILE *f, IR_node_t automaton)
{
  output_string (f, prefix);
  output_char ('_', f);
  if (IR_corresponding_single_automaton_declaration (automaton) == NULL)
    output_decimal_number (f, IR_automaton_order_number (automaton), 0);
  else
    output_string
      (f,
       IR_identifier_itself
       (IR_identifier
        (IR_corresponding_single_automaton_declaration (automaton))));
  output_string (f, "_UNDEFINED_INTERNAL_INSTRUCTION_CODE");
}

static void
output_translate_vector_name (FILE *f, IR_node_t automaton)
{
  output_string (f, prefix);
  output_char ('_', f);
  if (IR_corresponding_single_automaton_declaration (automaton) == NULL)
    output_decimal_number (f, IR_automaton_order_number (automaton), 0);
  else
    output_string
      (f,
       IR_identifier_itself
       (IR_identifier
        (IR_corresponding_single_automaton_declaration (automaton))));
  output_string (f, "_translate");
}

static void
output_transition_comb_vector_name (FILE *f, IR_node_t automaton)
{
  output_string (f, prefix);
  output_char ('_', f);
  if (IR_corresponding_single_automaton_declaration (automaton) == NULL)
    output_decimal_number (f, IR_automaton_order_number (automaton), 0);
  else
    output_string
      (f,
       IR_identifier_itself
       (IR_identifier
        (IR_corresponding_single_automaton_declaration (automaton))));
  output_string (f, "_transitions");
}

static void
output_transition_check_vector_name (FILE *f, IR_node_t automaton)
{
  output_string (f, prefix);
  output_char ('_', f);
  if (IR_corresponding_single_automaton_declaration (automaton) == NULL)
    output_decimal_number (f, IR_automaton_order_number (automaton), 0);
  else
    output_string
      (f,
       IR_identifier_itself
       (IR_identifier
        (IR_corresponding_single_automaton_declaration (automaton))));
  output_string (f, "_check");
}

static void
output_transition_base_vector_name (FILE *f, IR_node_t automaton)
{
  output_string (f, prefix);
  output_char ('_', f);
  if (IR_corresponding_single_automaton_declaration (automaton) == NULL)
    output_decimal_number (f, IR_automaton_order_number (automaton), 0);
  else
    output_string
      (f,
       IR_identifier_itself
       (IR_identifier
        (IR_corresponding_single_automaton_declaration (automaton))));
  output_string (f, "_base");
}

static void
output_is_dead_locked_vector_name (FILE *f, IR_node_t automaton)
{
  output_string (f, prefix);
  output_char ('_', f);
  if (IR_corresponding_single_automaton_declaration (automaton) == NULL)
    output_decimal_number (f, IR_automaton_order_number (automaton), 0);
  else
    output_string
      (f,
       IR_identifier_itself
       (IR_identifier
        (IR_corresponding_single_automaton_declaration (automaton))));
  output_string (f, "_is_dead_locked");
}

static void
output_transition_function_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_transition");
}

static void
output_is_dead_lock_function_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_is_dead_lock");
}

static void
output_reset_function_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_reset");
}

static void
output_chip_function_name (FILE *f)
{
  assert (cpp_flag);
  output_chip_name (f);
}

static void
output_chip_parameter_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_chip");
}

static void
output_instruction_parameter_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_instruction");
}

static void
output_internal_instruction_code_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_internal_code");
}

static void
output_temporary_variable_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_temp");
}

static void
output_state_struct_member_type (FILE *f, IR_node_t automaton)
{
  output_range_type (f, 0, IR_achieved_states_number (automaton));

}

static void 
output_transition_function_declaration (int interface_flag)
{
  FILE *f;

  f = (interface_flag ? output_interface_file : output_implementation_file);
  if (interface_flag && !cpp_flag)
    output_string (f, "extern ");
  output_string (f, "int ");
  if (!interface_flag && cpp_flag)
    {
      output_chip_name (f);
      output_string (f, "::");
    }
  output_transition_function_name (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_chip_name (f);
      output_string (f, " *");
      output_chip_parameter_name (f);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_instruction_parameter_name (f);
  if (interface_flag)
    output_string (f, ");\n");
  else
    output_string (f, ")\n");
}

static void 
output_is_dead_lock_function_declaration (int interface_flag)
{
  FILE *f;

  f = (interface_flag ? output_interface_file : output_implementation_file);
  if (interface_flag && !cpp_flag)
    output_string (f, "extern ");
  output_string (f, "int ");
  if (!interface_flag && cpp_flag)
    {
      output_chip_name (f);
      output_string (f, "::");
    }
  output_is_dead_lock_function_name (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_chip_name (f);
      output_string (f, " *");
      output_chip_parameter_name (f);
    }
  else
    output_string (f, "void");
  if (interface_flag)
    output_string (f, ");\n");
  else
    output_string (f, ")\n");
}

static void 
output_reset_function_declaration (int interface_flag)
{
  FILE *f;

  f = (interface_flag ? output_interface_file : output_implementation_file);
  if (interface_flag && !cpp_flag)
    output_string (f, "extern ");
  output_string (f, "void ");
  if (!interface_flag && cpp_flag)
    {
      output_chip_name (f);
      output_string (f, "::");
    }
  output_reset_function_name (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_chip_name (f);
      output_string (f, " *");
      output_chip_parameter_name (f);
    }
  else
    output_string (f, "void");
  if (interface_flag)
    output_string (f, ");\n");
  else
    output_string (f, ")\n");
}

static void 
output_chip_function_declaration (int interface_flag)
{
  FILE *f;

  assert (cpp_flag);
  f = (interface_flag ? output_interface_file : output_implementation_file);
  if (!interface_flag)
    {
      output_chip_name (f);
      output_string (f, "::");
    }
  output_chip_function_name (f);
  output_string (f, " (void)");
  if (interface_flag)
    output_string (f, ";\n");
  else
    output_string (f, "\n");
}

static void
output_chip_definitions (void)
{
  IR_node_t current_automaton;

  if (cpp_flag)
    output_string (output_interface_file, "class ");
  else
    output_string (output_interface_file, "struct ");
  output_chip_name (output_interface_file);
  output_string (output_interface_file, "\n{\n");
  for (current_automaton = IR_first_automaton (description);
       current_automaton != NULL;
       current_automaton = IR_next_automaton (current_automaton))
    {
      output_string (output_interface_file, "  ");
      output_state_struct_member_type (output_interface_file,
                                       current_automaton);
      output_char (' ' , output_interface_file);
      output_chip_member_name (output_interface_file, current_automaton);
      output_string (output_interface_file, ";\n");
    }
  if (!cpp_flag)
    {
      output_string (output_interface_file, "};\n\n");
      output_string (output_interface_file, "typedef struct ");
      output_chip_name (output_interface_file);
      output_char (' ' , output_interface_file);
      output_chip_name (output_interface_file);
      output_string (output_interface_file, ";\n\n");
      output_transition_function_declaration (TRUE);
      output_is_dead_lock_function_declaration (TRUE);
      output_reset_function_declaration (TRUE);
    }
  else
    {
      output_string (output_interface_file, "public:\n  ");
      output_transition_function_declaration (TRUE);
      output_string (output_interface_file, "  ");
      output_is_dead_lock_function_declaration (TRUE);
      output_string (output_interface_file, "  ");
      output_reset_function_declaration (TRUE);
      output_string (output_interface_file, "  ");
      output_chip_function_declaration (TRUE);
      output_string (output_interface_file, "};\n\n");
    }
}

static void
output_instruction_definitions (FILE *f)
{
  IR_node_t current_single_declaration;

  if (enum_flag)
    {
      output_string (f, "enum ");
      
      output_string (f, "{\n");
    }
  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_instruction_declaration))
      {
        if (enum_flag)
          output_string (f, "  ");
        else
          output_string (f, "#define ");
        if (current_single_declaration
            == cycle_advancing_single_instruction_declaration)
          output_advancing_cycle_value_name (f);
        else
          output_instruction_value_name
            (f, IR_identifier_itself (IR_identifier
                                      (current_single_declaration)));
        if (enum_flag)
          {
            if (IR_next_single_declaration (current_single_declaration)
                != NULL)
              output_char (',', f);
          }
        else
          {
            output_char (' ', f);
            output_decimal_number
              (f, IR_instruction_number (current_single_declaration), 0);
          }
        output_char ('\n', f);
      }
  if (enum_flag)
    output_string (f, "};\n");
  output_char ('\n', f);
}

/* ???. */
static void
output_translate_vector (IR_node_t automaton)
{
  IR_node_t current_automaton_instruction_declaration;
  int current_instruction_value;
  vlo_t translate_vector;

  VLO_CREATE (translate_vector, 1000);
  VLO_EXPAND
    (translate_vector,
     IR_instructions_number (description) * sizeof (vector_element_t));
  for (current_instruction_value = 0;
       current_instruction_value <= IR_instructions_number (description);
       current_instruction_value++)
    /* Undefined value */
    ((vector_element_t *) VLO_BEGIN (translate_vector))
      [current_instruction_value]
        = IR_instruction_equivalence_classes_number (automaton);
  for (current_automaton_instruction_declaration
       = IR_automaton_instruction_declaration_list (automaton);
       current_automaton_instruction_declaration != NULL;
       current_automaton_instruction_declaration
       = IR_next_automaton_instruction_declaration
         (current_automaton_instruction_declaration))
    ((vector_element_t *) VLO_BEGIN (translate_vector))
      [IR_instruction_number (IR_single_instruction_declaration
                              (current_automaton_instruction_declaration))]
        = IR_instruction_equivalence_class_number
          (current_automaton_instruction_declaration);
  output_string
    (output_implementation_file,
     "/* Vector translating external instruction codes to internal ones.*/\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type
    (0, IR_instruction_equivalence_classes_number (automaton));
  output_char (' ', output_implementation_file);
  output_translate_vector_name (output_implementation_file, automaton);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (translate_vector),
                 VLO_LENGTH (translate_vector)
                 / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  VLO_DELETE (translate_vector);
  output_string (output_implementation_file, "#define ");
  output_undefined_internal_instruction_code_name (output_implementation_file,
                                                   automaton);
  output_string (output_implementation_file, "  ");
  output_decimal_number
    (output_implementation_file,
     IR_instruction_equivalence_classes_number (automaton), 0);
  output_string (output_implementation_file, "\n\n");
}

static int empty_transition_vector_element_value;

static int
add_transition_vector (IR_node_t automaton, vlo_t *comb_vector,
                       vlo_t *check_vector, int vector_number,
                       vector_element_t *transition_vector, int vector_length)
{
  int real_vector_length;
  vector_element_t *comb_vector_start;
  vector_element_t *check_vector_start;
  int comb_vector_index;
  int comb_vector_elements_number;
  int vector_index;
  int first_unempty_transition_vector_index;
  int additional_elements_number;
  int no_state_value;
  vector_element_t vector_element;

  assert (vector_length != 0);
  real_vector_length = IR_instruction_equivalence_classes_number (automaton);
  assert (transition_vector [vector_length - 1]
          != empty_transition_vector_element_value
          && VLO_LENGTH (*comb_vector) == VLO_LENGTH (*check_vector));
  comb_vector_start = VLO_BEGIN (*comb_vector);
  comb_vector_elements_number
    = VLO_LENGTH (*comb_vector) / sizeof (vector_element_t);
  for (first_unempty_transition_vector_index = 0;
       first_unempty_transition_vector_index < vector_length;
       first_unempty_transition_vector_index++)
    if (transition_vector [first_unempty_transition_vector_index]
        != empty_transition_vector_element_value)
      break;
  /* Search for the place in comb vector for the inserted vector. */
  for (comb_vector_index = (-first_unempty_transition_vector_index);
       comb_vector_index < comb_vector_elements_number;
       comb_vector_index++)
    {
      for (vector_index = first_unempty_transition_vector_index;
           vector_index < vector_length
           &&
           vector_index + comb_vector_index < comb_vector_elements_number;
           vector_index++)
        if (transition_vector [vector_index]
            != empty_transition_vector_element_value
            && (comb_vector_start [vector_index + comb_vector_index]
                != empty_transition_vector_element_value))
          break;
      if (vector_index >= vector_length
          ||
          vector_index + comb_vector_index >= comb_vector_elements_number)
        break;
    }
  /* Slot was found. */
  additional_elements_number
    = comb_vector_index + real_vector_length
      - comb_vector_elements_number;
  if (additional_elements_number < 0)
    additional_elements_number = 0;
  /* Expand comb and check vectors. */
  vector_element = empty_transition_vector_element_value;
  no_state_value = IR_achieved_states_number (automaton);
  while (additional_elements_number > 0)
    {
      VLO_ADD_MEMORY (*comb_vector, &vector_element,
                      sizeof (vector_element_t));
      VLO_ADD_MEMORY (*check_vector, &no_state_value,
                      sizeof (vector_element_t));
      additional_elements_number--;
    }
  comb_vector_start = VLO_BEGIN (*comb_vector);
  check_vector_start = VLO_BEGIN (*check_vector);
  assert (VLO_LENGTH (*comb_vector) / sizeof (vector_element_t)
          >= comb_vector_index + real_vector_length);
  /* Fill comb and check vectors. */
  for (vector_index = 0; vector_index < vector_length; vector_index++)
    if (transition_vector [vector_index]
        != empty_transition_vector_element_value)
      {
        assert (comb_vector_start [comb_vector_index + vector_index]
                == empty_transition_vector_element_value);
        comb_vector_start [comb_vector_index + vector_index]
          = transition_vector [vector_index];
        check_vector_start [comb_vector_index + vector_index] = vector_number;
      }
  return comb_vector_index;
}

static int
output_state_arcs_number (IR_node_t state)
{
  int result;
  IR_node_t current_arc;

  result = 0;
  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    {
      assert (IR_instruction (current_arc) != NULL);
      if (IR_first_out_arc_with_given_equialence_number (IR_instruction
                                                         (current_arc)))
        result++;
    }
  return result;
}

static int
compare_transition_elements_number (const void *state_ptr_1,
                                    const void *state_ptr_2)
{
  int transition_elements_number_1;
  int transition_elements_number_2;

  transition_elements_number_1
    = output_state_arcs_number (*(IR_node_t *) state_ptr_1);
  transition_elements_number_2
    = output_state_arcs_number (*(IR_node_t *) state_ptr_2);
  if (transition_elements_number_1 < transition_elements_number_2)
    return 1;
  else if (transition_elements_number_1 == transition_elements_number_2)
    return 0;
  else
    return -1;
}

static int
add_transition_vector_element (vlo_t *vector,
                               IR_node_t automaton_instruction_declaration,
                               int element_value,
                               int max_transition_vector_element_value)
{
  int equivalence_class_number;
  int vector_index;

  assert (automaton_instruction_declaration != NULL);
  equivalence_class_number
    = IR_instruction_equivalence_class_number
      (automaton_instruction_declaration);
  for (vector_index = VLO_LENGTH (*vector) / sizeof (vector_element_t);
       vector_index <= equivalence_class_number;
       vector_index++)
    VLO_ADD_MEMORY (*vector, &empty_transition_vector_element_value,
                    sizeof (vector_element_t));
  if (max_transition_vector_element_value < element_value)
    max_transition_vector_element_value = element_value;
  ((vector_element_t *) VLO_BEGIN (*vector)) [equivalence_class_number]
    = element_value;
  return max_transition_vector_element_value;
}

static vlo_t output_states_vector;

static void
add_states_vector_element (IR_node_t state)
{
  VLO_ADD_MEMORY (output_states_vector, &state, sizeof (state));
}

#ifndef NDEBUG
static int transition_table_elements;
static int transition_comb_vector_elements;
#endif

static void
output_transition_table (IR_node_t automaton)
{
  vector_element_t min_base_vector_element_value;
  vector_element_t max_base_vector_element_value;
  vector_element_t max_transition_vector_element_value;
  vector_element_t base_value;
  IR_node_t *current_state_ptr;
  IR_node_t current_arc;
  vlo_t comb_vector;
  vlo_t check_vector;
  vlo_t base_vector;
  vlo_t transition_vector;

  empty_transition_vector_element_value
    = IR_achieved_states_number (automaton) + 1;
  /* Create vector of pointers to states ordered by number of
     transitions from the state (state with the maximum number is the
     first). */
  VLO_CREATE (output_states_vector, 5000);
  pass_states (automaton, add_states_vector_element);
  qsort (VLO_BEGIN (output_states_vector),
         VLO_LENGTH (output_states_vector) / sizeof (IR_node_t),
         sizeof (IR_node_t), compare_transition_elements_number);
  /* Create base, transition, and check vectors. */
  VLO_CREATE (comb_vector, 50000);
  VLO_CREATE (check_vector, 50000);
  VLO_CREATE (base_vector, 5000);
  VLO_EXPAND (base_vector, VLO_LENGTH (output_states_vector));
  VLO_CREATE (transition_vector, 2000);
  max_transition_vector_element_value
    = IR_achieved_states_number (automaton) + 1;
  min_base_vector_element_value = 0;
  max_base_vector_element_value = 0;
  for (current_state_ptr = VLO_BEGIN (output_states_vector);
       (char *) current_state_ptr <= (char *) VLO_END (output_states_vector);
       current_state_ptr++)
    {
      VLO_NULLIFY (transition_vector);
      for (current_arc = first_out_arc (*current_state_ptr);
           current_arc != NULL;
           current_arc = next_out_arc (current_arc))
        {
          assert (IR_instruction (current_arc) != NULL);
          if (IR_first_out_arc_with_given_equialence_number
              (IR_instruction (current_arc)))
            max_transition_vector_element_value
              = add_transition_vector_element
                (&transition_vector, IR_instruction (current_arc),
                 IR_order_state_number (IR_to_state (current_arc)),
                 max_transition_vector_element_value);
        }
      base_value
        = add_transition_vector
          (automaton, &comb_vector, &check_vector,
           IR_order_state_number (*current_state_ptr),
           (int *) VLO_BEGIN (transition_vector),
           VLO_LENGTH (transition_vector) / sizeof (vector_element_t));
      if (max_base_vector_element_value < base_value)
        max_base_vector_element_value = base_value;
      if (min_base_vector_element_value > base_value)
        min_base_vector_element_value = base_value;
      ((vector_element_t *) VLO_BEGIN (base_vector))
        [IR_order_state_number (*current_state_ptr)] = base_value;
    }
  output_string (output_implementation_file,
                 "/* Comb vector for transitions. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, max_transition_vector_element_value);
  output_char (' ', output_implementation_file);
  output_transition_comb_vector_name (output_implementation_file, automaton);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (comb_vector),
                 VLO_LENGTH (comb_vector) / sizeof (vector_element_t));
#ifndef NDEBUG
  transition_comb_vector_elements
    += VLO_LENGTH (comb_vector) / sizeof (vector_element_t);
  transition_table_elements
    += IR_achieved_states_number (automaton)
      * IR_instruction_equivalence_classes_number (automaton);
#endif
  output_string (output_implementation_file, "};\n\n");
  output_string (output_implementation_file,
                 "/* Check vector for transitions. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, IR_achieved_states_number (automaton));
  output_char (' ', output_implementation_file);
  output_transition_check_vector_name (output_implementation_file, automaton);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (check_vector),
                 VLO_LENGTH (check_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  output_string (output_implementation_file,
                 "/* Base vector for transitions. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (min_base_vector_element_value,
                              max_base_vector_element_value);
  output_char (' ', output_implementation_file);
  output_transition_base_vector_name (output_implementation_file, automaton);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (base_vector),
                 VLO_LENGTH (base_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  VLO_DELETE (output_states_vector);
  VLO_DELETE (transition_vector);
  VLO_DELETE (base_vector);
  VLO_DELETE (check_vector);
  VLO_DELETE (comb_vector);
}

#ifndef NDEBUG
static int locked_states_number;
#endif

static void
output_is_dead_locked_vector (IR_node_t automaton)
{
  IR_node_t *current_state_ptr;
  IR_node_t current_arc;
  vlo_t is_dead_locked_vector;

  /* Create vector of pointers to states ordered by number of
     transitions from the state (state with the maximum number is the
     first). */
  VLO_CREATE (output_states_vector, 5000);
  pass_states (automaton, add_states_vector_element);
  /* Create base, transition, and check vectors. */
  VLO_CREATE (is_dead_locked_vector, 5000);
  VLO_EXPAND (is_dead_locked_vector, VLO_LENGTH (output_states_vector));
  for (current_state_ptr = VLO_BEGIN (output_states_vector);
       (char *) current_state_ptr <= (char *) VLO_END (output_states_vector);
       current_state_ptr++)
    {
      current_arc = first_out_arc (*current_state_ptr);
      assert (current_arc != NULL);
      ((vector_element_t *) VLO_BEGIN (is_dead_locked_vector))
        [IR_order_state_number (*current_state_ptr)]
        = (next_out_arc (current_arc) == NULL
           && (IR_single_instruction_declaration (IR_instruction (current_arc))
               == cycle_advancing_single_instruction_declaration)
           ? 1 : 0);
#ifndef NDEBUG
      if (((vector_element_t *) VLO_BEGIN (is_dead_locked_vector))
          [IR_order_state_number (*current_state_ptr)])
        locked_states_number++;
#endif
    }
  output_string (output_implementation_file,
                 "/* Vector for locked state flags. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, 1);
  output_char (' ', output_implementation_file);
  output_is_dead_locked_vector_name (output_implementation_file, automaton);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (is_dead_locked_vector),
                 VLO_LENGTH (is_dead_locked_vector)
                 / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  VLO_DELETE (is_dead_locked_vector);
}

/* ??? */
static void
output_tables (void)
{
  IR_node_t current_automaton;

  if (export_flag)
    output_instruction_definitions (output_interface_file);
  else
    output_instruction_definitions (output_implementation_file);
#ifndef NDEBUG
  transition_table_elements = 0;
  transition_comb_vector_elements = 0;
  locked_states_number = 0;
#endif
  for (current_automaton = IR_first_automaton (description);
       current_automaton != NULL;
       current_automaton = IR_next_automaton (current_automaton))
    {
      output_translate_vector (current_automaton);
      output_transition_table (current_automaton);
      output_is_dead_locked_vector (current_automaton);
    }
}

static void
output_transition_function (void)
{
  IR_node_t current_automaton;
  IR_node_t next_automaton;

  output_transition_function_declaration (FALSE);
  output_string (output_implementation_file, "{\n");
  output_string (output_implementation_file, "  int ");
  output_internal_instruction_code_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  int ");
  output_temporary_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  if (IR_first_automaton (description) != NULL)
    for (current_automaton = IR_first_automaton (description);;
         current_automaton = next_automaton)
      {
        next_automaton = IR_next_automaton (current_automaton);
        if (next_automaton == NULL)
          break;
        output_string (output_implementation_file, "  ");
        output_state_struct_member_type (output_implementation_file,
                                         current_automaton);
        output_char (' ' , output_implementation_file);
        output_temporary_chip_member_name (output_implementation_file,
                                           current_automaton);
        output_string (output_implementation_file, ";\n");
      }
  output_string (output_implementation_file, "\n");
  output_string (output_implementation_file, "  assert (");
  output_instruction_parameter_name (output_implementation_file);
  output_string (output_implementation_file, " >= 0 && ");
  output_instruction_parameter_name (output_implementation_file);
  output_string (output_implementation_file, " <= ");
  output_advancing_cycle_value_name (output_implementation_file);
  output_string (output_implementation_file, ");\n");
  for (current_automaton = IR_first_automaton (description);
       current_automaton != NULL;
       current_automaton = IR_next_automaton (current_automaton))
    {
      output_string (output_implementation_file, "  ");
      output_internal_instruction_code_name (output_implementation_file);
      output_string (output_implementation_file, " = ");
      output_translate_vector_name (output_implementation_file,
                                    current_automaton);
      output_string (output_implementation_file, " [");
      output_instruction_parameter_name (output_implementation_file);
      output_string (output_implementation_file, "];\n");
      output_string (output_implementation_file, "  assert (");
      output_internal_instruction_code_name (output_implementation_file);
      output_string (output_implementation_file, " != ");
      output_undefined_internal_instruction_code_name
        (output_implementation_file, current_automaton);
      output_string (output_implementation_file, ");\n  ");
      output_temporary_variable_name (output_implementation_file);
      output_string (output_implementation_file, " = ");
      output_transition_base_vector_name (output_implementation_file,
                                          current_automaton);
      output_string (output_implementation_file, " [");
      if (!cpp_flag)
        {
          output_chip_parameter_name (output_implementation_file);
          output_string (output_implementation_file, "->");
        }
      output_chip_member_name (output_implementation_file, current_automaton);
      output_string (output_implementation_file, "] + ");
      output_internal_instruction_code_name (output_implementation_file);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "  if (");
      output_transition_check_vector_name (output_implementation_file,
                                           current_automaton);
      output_string (output_implementation_file, " [");
      output_temporary_variable_name (output_implementation_file);
      output_string (output_implementation_file, "] != ");
      if (!cpp_flag)
        {
          output_chip_parameter_name (output_implementation_file);
          output_string (output_implementation_file, "->");
        }
      output_chip_member_name (output_implementation_file, current_automaton);
      output_string (output_implementation_file, ")\n");
      output_string (output_implementation_file, "    return 0/* FALSE */;\n");
      output_string (output_implementation_file, "  else\n");
      output_string (output_implementation_file, "    ");
      if (IR_next_automaton (current_automaton) != NULL)
        output_temporary_chip_member_name (output_implementation_file,
                                           current_automaton);
      else
        {
          if (!cpp_flag)
            {
              output_chip_parameter_name (output_implementation_file);
              output_string (output_implementation_file, "->");
            }
          output_chip_member_name (output_implementation_file,
                                   current_automaton);
        }
      output_string (output_implementation_file, " = ");
      output_transition_comb_vector_name (output_implementation_file,
                                          current_automaton);
      output_string (output_implementation_file, " [");
      output_temporary_variable_name (output_implementation_file);
      output_string (output_implementation_file, "];\n");
    }
  if (IR_first_automaton (description) != NULL)
    for (current_automaton = IR_first_automaton (description);;
         current_automaton = next_automaton)
      {
        next_automaton = IR_next_automaton (current_automaton);
        if (next_automaton == NULL)
          break;
        output_string (output_implementation_file, "  ");
        if (!cpp_flag)
          {
            output_chip_parameter_name (output_implementation_file);
            output_string (output_implementation_file, "->");
          }
        output_chip_member_name (output_implementation_file,
                                 current_automaton);
        output_string (output_implementation_file, " = ");
        output_temporary_chip_member_name (output_implementation_file,
                                           current_automaton);
        output_string (output_implementation_file, ";\n");
      }
  output_string (output_implementation_file, "  return 1/* TRUE */;\n");
  output_string (output_implementation_file, "}\n\n");
}

static void
output_is_dead_lock_function (void)
{
  IR_node_t current_automaton;

  output_is_dead_lock_function_declaration (FALSE);
  output_string (output_implementation_file, "{\n");
  for (current_automaton = IR_first_automaton (description);
       current_automaton != NULL;
       current_automaton = IR_next_automaton (current_automaton))
    {
      /* ??? */
      output_string (output_implementation_file, "  if (");
      output_is_dead_locked_vector_name (output_implementation_file,
                                         current_automaton);
      output_string (output_implementation_file, " [");
      if (!cpp_flag)
        {
          output_chip_parameter_name (output_implementation_file);
          output_string (output_implementation_file, "->");
        }
      output_chip_member_name (output_implementation_file, current_automaton);
      output_string (output_implementation_file,
                     "])\n    return 1/* TRUE */;\n");
    }
  output_string (output_implementation_file, "  return 0/* FALSE */;\n}\n\n");
}

static void
output_reset_function (void)
{
  IR_node_t current_automaton;

  output_reset_function_declaration (FALSE);
  output_string (output_implementation_file, "{\n");
  for (current_automaton = IR_first_automaton (description);
       current_automaton != NULL;
       current_automaton = IR_next_automaton (current_automaton))
    {
      output_string (output_implementation_file, "  ");
      if (!cpp_flag)
        {
          output_chip_parameter_name (output_implementation_file);
          output_string (output_implementation_file, "->");
        }
      output_chip_member_name (output_implementation_file, current_automaton);
      output_string (output_implementation_file, " = 0;\n");
    }
  output_string (output_implementation_file, "}\n\n");
}

static void
output_chip_function (void)
{
  assert (cpp_flag);
  output_chip_function_declaration (FALSE);
  output_string (output_implementation_file, "{\n");
  output_string (output_implementation_file, "  ");
  output_reset_function_name (output_implementation_file);
  output_string (output_implementation_file, " ();\n}\n\n");
}

/* ??? */
static void
output_finish_code_insertions (void)
{
  IR_node_t current_declaration;

  if (IR_declaration_list (description) == NULL)
    return;
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_export_code))
      {
        output_line (output_interface_file,
                     IR_position (current_declaration).line_number,
                     IR_position (current_declaration).file_name);
        output_string (output_interface_file,
                       IR_code_insertion_itself (IR_code_itself
                                                 (current_declaration)));
        output_char ('\n', output_interface_file);
        output_current_line (output_interface_file);
      }
}

/* ??? */
static void
output_additional_code (void)
{
  IR_node_t additional_code;

  additional_code = IR_additional_code (description);
  assert (additional_code != NULL
          && IR_IS_OF_TYPE (additional_code, IR_NM_additional_code));
  output_char ('\n', output_implementation_file);
  output_line (output_implementation_file,
               IR_position (additional_code).line_number,
               IR_position (additional_code).file_name);
  output_string (output_implementation_file,
                 IR_additional_code_itself (additional_code));
  output_char ('\n', output_implementation_file);
  output_current_line (output_implementation_file);
}

#define LOWEST_PRIORITY          0
#define CONCATENATION_PRIORITY   1
#define REPETITION_PRIORITY      2

static void
output_expression (IR_node_t expression, int upper_operation_priority)
{
  int current_operation_priority;

  if (IR_IS_OF_TYPE (expression, IR_NM_one_operand_expression))
    {
      if (IR_IS_OF_TYPE (expression, IR_NM_optional_expression))
        {
          output_char ('[', output_description_file);
          output_expression (IR_operand (expression), LOWEST_PRIORITY);
          output_char (']', output_description_file);
        }
      else
        {
          assert (IR_IS_OF_TYPE (expression, IR_NM_repetition));
          output_expression (IR_operand (expression), REPETITION_PRIORITY);
          output_string (output_description_file, "*");
          output_decimal_number
            (output_description_file,
             IR_number_value (IR_repetition_number (expression)), 0);
        }
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_two_operand_expression))
    {
      if (upper_operation_priority == REPETITION_PRIORITY
          || (upper_operation_priority == CONCATENATION_PRIORITY
	      && IR_IS_OF_TYPE (expression, IR_NM_alternative)))
        output_char ('(', output_description_file);
      if (IR_IS_OF_TYPE (expression, IR_NM_alternative))
        current_operation_priority = LOWEST_PRIORITY;
      else
        current_operation_priority = CONCATENATION_PRIORITY;
      output_expression (IR_left_operand (expression),
                         current_operation_priority);
      if (IR_IS_OF_TYPE (expression, IR_NM_new_cycle_concatenation))
        output_string (output_description_file, "  ");
      else if (IR_IS_OF_TYPE (expression, IR_NM_concatenation))
        output_string (output_description_file, " + ");
      else
        {
          assert (IR_IS_OF_TYPE (expression, IR_NM_alternative));
          output_string (output_description_file, " | ");
        }
      output_expression (IR_right_operand (expression),
                         current_operation_priority);
      if (upper_operation_priority == REPETITION_PRIORITY
          || (upper_operation_priority == CONCATENATION_PRIORITY
	      && IR_IS_OF_TYPE (expression, IR_NM_alternative)))
        output_char (')', output_description_file);
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_expression_atom))
    output_string (output_description_file,
                   IR_identifier_itself (IR_expression_identifier
                                         (expression)));
  else
    {
      assert (IR_IS_OF_TYPE (expression, IR_NM_nothing));
      output_string (output_description_file, "%nothing");
    }
}

static void
output_description (void)
{
  IR_node_t current_expression_definition;

  for (current_expression_definition
       = IR_expression_definition_list (description);
       current_expression_definition != NULL;
       current_expression_definition
       = IR_next_expression_definition (current_expression_definition))
    {
      output_string (output_description_file, 
                     IR_identifier_itself (IR_expression_identifier
                                           (current_expression_definition)));
      output_string (output_description_file, ": ");
      output_expression (IR_expression (current_expression_definition),
                         LOWEST_PRIORITY);
      output_char ('\n', output_description_file); 
    }
  output_string (output_description_file, "\n\f\n");
}

static void
output_automaton_name (FILE *f, IR_node_t automaton)
{
  if (IR_corresponding_single_automaton_declaration (automaton) == NULL)
    {
      output_char ('#', f);
      output_decimal_number (f, IR_automaton_order_number (automaton), 0);
    }
  else
    {
      output_char ('`', f);
      output_string
        (f,
         IR_identifier_itself
         (IR_identifier (IR_corresponding_single_automaton_declaration
                         (automaton))));
      output_char ('\'', f);
    }
}

#define MAX_LINE_LENGTH 70

static void
output_automaton_units (IR_node_t automaton)
{
  IR_node_t current_single_declaration;
  char *identifier_string;
  int current_line_length;
  int there_is_an_automaton_unit;

  output_string (output_description_file, "\n  Coresponding units:\n");
  output_string (output_description_file, "    ");
  current_line_length = 4;
  there_is_an_automaton_unit = FALSE;
  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_unit_declaration)
        && (IR_corresponding_automaton_number (current_single_declaration)
            == IR_automaton_order_number (automaton)))
      {
        there_is_an_automaton_unit = TRUE;
        identifier_string
          = IR_identifier_itself (IR_identifier (current_single_declaration));
        if (current_line_length + strlen (identifier_string) + 1
            > MAX_LINE_LENGTH )
          {
            current_line_length = strlen (identifier_string) + 4;
            output_string (output_description_file, "\n    ");
          }
        else
          {
            current_line_length += strlen (identifier_string) + 1;
            output_char (' ', output_description_file);
          }
        output_string (output_description_file, identifier_string);
      }
  if (!there_is_an_automaton_unit)
    output_string (output_description_file, "<None>");
  output_string (output_description_file, "\n\n");
}

static vlo_t state_reservations;

static void
add_state_reservations (IR_node_t state)
{
  IR_node_t current_alternative_state;
  reservation_sets_list_t reservations;

  if (IR_component_states (state) != NULL)
    for (current_alternative_state = IR_component_states (state);
         current_alternative_state != NULL;
         current_alternative_state
         = IR_next_alternative_state (current_alternative_state))
      add_state_reservations (IR_state (current_alternative_state));
  else
    {
      reservations = IR_reservations (state);
      VLO_ADD_MEMORY (state_reservations, &reservations,
                      sizeof (reservations));
    }
}

static void
output_state_arcs (IR_node_t state)
{
  IR_node_t current_arc;
  IR_node_t current_single_automaton_declaration;
  char *instruction_identifier;
  int current_line_length;

  for (current_arc = first_out_arc (state);
       current_arc != NULL;
       current_arc = next_out_arc (current_arc))
    {
      current_single_automaton_declaration = IR_instruction (current_arc);
      assert (IR_first_instruction_with_the_same_reservations
              (current_single_automaton_declaration));
      output_string (output_description_file, "    ");
      current_line_length = 4;
      while (current_single_automaton_declaration != NULL)
        {
          instruction_identifier
            = IR_identifier_itself (IR_identifier
                                    (IR_single_instruction_declaration
                                     (current_single_automaton_declaration)));
          if (current_line_length + strlen (instruction_identifier)
              > MAX_LINE_LENGTH)
            {
              if (current_single_automaton_declaration
                  != IR_instruction (current_arc))
                {
                  output_string (output_description_file, ",\n      ");
                  current_line_length = strlen (instruction_identifier) + 6;
                }
              else
                current_line_length += strlen (instruction_identifier);
            }
          else
            {
              current_line_length += strlen (instruction_identifier);
              if (current_single_automaton_declaration
                  != IR_instruction (current_arc))
                {
                  current_line_length += 2;
                  output_string (output_description_file, ", ");
                }
            }
          output_string (output_description_file, instruction_identifier);
          current_single_automaton_declaration
            = IR_next_the_same_reservations_instruction
              (current_single_automaton_declaration);
        }
      output_string (output_description_file, "    ");
      output_decimal_number (output_description_file,
                             IR_order_state_number (IR_to_state (current_arc)),
                             0);
      output_char ('\n', output_description_file);
    }
  output_char ('\n', output_description_file);
}

static int
state_reservations_comparison (const void *reservations_ptr_1,
                               const void *reservations_ptr_2)
{
  return
    reservation_sets_lists_comparison
      (*(reservation_sets_list_t *) reservations_ptr_1,
       *(reservation_sets_list_t *) reservations_ptr_2);
}

static void
remove_state_duplicate_reservations (void)
{
  reservation_sets_list_t *current_reservations_ptr;
  reservation_sets_list_t *last_formed_reservations_ptr;

  last_formed_reservations_ptr = NULL;
  for (current_reservations_ptr = VLO_BEGIN (state_reservations);
       (char *) current_reservations_ptr
       <= (char *) VLO_END (state_reservations);
       current_reservations_ptr++)
    if (last_formed_reservations_ptr == NULL)
      last_formed_reservations_ptr = current_reservations_ptr;
    else if (reservation_sets_lists_comparison
             (*last_formed_reservations_ptr, *current_reservations_ptr) != 0)
      {
        ++last_formed_reservations_ptr;
        *last_formed_reservations_ptr = *current_reservations_ptr;
      }
  VLO_SHORTEN (state_reservations,
               (current_reservations_ptr - last_formed_reservations_ptr - 1)
               * sizeof (reservation_sets_list_t *));
}

static void
output_state (IR_node_t state)
{
  reservation_sets_list_t *current_reservations_ptr;

  VLO_CREATE (state_reservations, 0);
  output_string (output_description_file, "  State #");
  output_decimal_number (output_description_file,
                         IR_order_state_number (state), 0);
  output_char ('\n', output_description_file);
  add_state_reservations (state);
  qsort (VLO_BEGIN (state_reservations),
         VLO_LENGTH (state_reservations) / sizeof (reservation_sets_list_t),
         sizeof (reservation_sets_list_t),
         state_reservations_comparison);
  remove_state_duplicate_reservations ();
  for (current_reservations_ptr = VLO_BEGIN (state_reservations);
       (char *) current_reservations_ptr
       <= (char *) VLO_END (state_reservations);
       current_reservations_ptr++)
    {
      output_string (output_description_file, "    ");
      output_reservation_sets_list (output_description_file,
                                    *current_reservations_ptr);
      output_char ('\n', output_description_file);
    }
  output_char ('\n', output_description_file);
  output_state_arcs (state);
  VLO_DELETE (state_reservations);
}

/* ??? */
static void
output_automaton_descriptions (void)
{
  IR_node_t current_automaton;

  for (current_automaton = IR_first_automaton (description);
       current_automaton != NULL;
       current_automaton = IR_next_automaton (current_automaton))
    {
      output_string (output_description_file, "\nAutomaton ");
      output_automaton_name (output_description_file, current_automaton);
      output_char ('\n', output_description_file);
      output_automaton_units (current_automaton);
      pass_states (current_automaton, output_state);
    }
}


static void
output_statistics (FILE *f)
{
  IR_node_t current_automaton;

  for (current_automaton = IR_first_automaton (description);
       current_automaton != NULL;
       current_automaton = IR_next_automaton (current_automaton))
    {
      output_string (f, "\nAutomaton ");
      output_automaton_name (f, current_automaton);
      output_char ('\n', f);
      output_string (f, "    ");
      output_decimal_number (f, IR_NDFA_states_number (current_automaton), 6);
      output_string (f, " NDFA states,          ");
      output_decimal_number (f, IR_NDFA_arcs_number (current_automaton), 6);
      output_string (f, " NDFA arcs\n");
      output_string (f, "    ");
      output_decimal_number (f, IR_DFA_states_number (current_automaton), 6);
      output_string (f, " DFA states,           ");
      output_decimal_number (f, IR_DFA_arcs_number (current_automaton), 6);
      output_string (f, " DFA arcs\n");
      if (!no_minimization_flag)
        {
          output_string (f, "    ");
          output_decimal_number
            (f, IR_minimal_DFA_states_number (current_automaton), 6);
          output_string (f, " minimal DFA states,   ");
          output_decimal_number
            (f, IR_minimal_DFA_arcs_number (current_automaton), 6);
          output_string (f, " minimal DFA arcs\n");
        }
      output_string (f, "    ");
      output_decimal_number (f, IR_instructions_number (description), 6);
      output_string (f, " all instructions      ");
      output_decimal_number
        (f, IR_instruction_equivalence_classes_number (current_automaton), 6);
      output_string (f, " instruction equivalence classes\n");
    }
#ifndef NDEBUG
  output_char ('\n', f);
  output_decimal_number (f, allocated_states_number, 6);
  output_string (f, " all allocated states,     ");
  output_decimal_number (f, allocated_arcs_number, 6);
  output_string (f, " all allocated arcs\n");
  output_decimal_number (f, allocated_alternative_states_number, 6);
  output_string (f, " all allocated alternative states\n");
  output_decimal_number (f, transition_comb_vector_elements, 6);
  output_string (f, " all comb vector elements, ");
  output_decimal_number (f, transition_table_elements, 6);
  output_string (f, " all transition table elements\n");
  output_decimal_number (f, locked_states_number, 6);
  output_string (f, " locked states number\n");
#endif
}

static void
output_time_statistics (FILE *f)
{
  fprintf (f, "\n  transformation: %s, ",
           active_time_string (transformation_time));
  fprintf (f, "building NDFA: %s, ", active_time_string (NDFA_time));
  fprintf (f, "NDFA -> DFA: %s\n", active_time_string (NDFA_to_DFA_time));
  fprintf (f, "  DFA minimization: %s, ", active_time_string (minimize_time));
  fprintf (f, "making instruction equivalence: %s\n",
           active_time_string (equivalence_time));
  fprintf (f, "all automaton generation: %s, ",
           active_time_string (automaton_generation_time));
  fprintf (f, "output: %s\n", active_time_string (output_time));
}

/* ???No errors must be fixed before this function call. */

void
generate (void)
{
  automatons_number = split_argument;
  if (IR_units_number (description) < automatons_number)
    automatons_number = IR_units_number (description);
  initiate_states ();
  initiate_arcs ();
  initiate_pass_states ();
  initiate_exclusion_sets ();
  automaton_generation_time = create_ticker ();
  transformation_time = create_ticker ();
  add_cycle_advancing_single_instruction_declaration ();
  transform_instruction_expressions ();
  ticker_off (&transformation_time);
  check_unit_distributions_to_automata ();
  if (number_of_errors == 0)
    {
      create_automatons ();
      ticker_off (&automaton_generation_time);
      output_time = create_ticker ();
      initiate_output ();
      output_ifndef (output_interface_file, description_name);
      output_string (output_interface_file, "#define ");
      output_ifdef_parameter_name (output_interface_file, description_name);
      output_string (output_interface_file, "\n\n");
      if (debug_flag)
	{
	  output_string (output_implementation_file, "#define ");
	  output_ifdef_parameter_name (output_implementation_file,
				       DEBUG_PARAMETER_NAME);
	  output_string (output_implementation_file, "\n\n");
	}
      /* ??? */
      output_string (output_implementation_file, "#include <assert.h>\n");
      output_string (output_implementation_file, "#include <stdio.h>\n");
      output_string (output_implementation_file, "#include \"");
      output_string (output_implementation_file, output_interface_file_name);
      output_string (output_implementation_file, "\"\n\n");
      output_start_code_insertions ();
      output_tables ();
      output_chip_definitions ();
      output_transition_function ();
      output_is_dead_lock_function ();
      output_reset_function ();
      if (cpp_flag)
	output_chip_function ();
      output_finish_code_insertions ();
      output_additional_code ();
      output_endif (output_interface_file, description_name);
      if (v_flag)
	{
	  output_description ();
	  output_automaton_descriptions ();
	  output_statistics (output_description_file);
	}
      output_statistics (stderr);
      ticker_off (&output_time);
      output_time_statistics (stderr);
    }
  finish_states ();
  finish_arcs ();
}
