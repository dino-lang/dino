/*
   FILE NAME:   anal.c

   TITLE:       Semantic analyzer of OKA (pipeline hazards description
                translator)

   DESCRIPTION: This file tests semantically all description built by
                the OKA parser.

   SPECIAL CONSIDERATION:
         The analyzer is to be called only after OKA parser.
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of the analyzer.
*/

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>

#include "position.h"
#include "errors.h"
#include "vlobject.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
#include "anal.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

/* The following function inserts given single declaration as the last
   element into cyclic list of single decalartions of the
   description. */

static void
add_single_declaration (IR_node_t single_declaration)
{
  if (IR_single_declaration_list (description) != NULL)
    {
      /* Add element to cyclic declaration list. */
      IR_set_next_single_declaration
        (single_declaration,
         IR_next_single_declaration (IR_single_declaration_list
                                     (description)));
      IR_set_next_single_declaration
        (IR_single_declaration_list (description), single_declaration);
    }
  else
    /* Make cycle. */
    IR_set_next_single_declaration (single_declaration, single_declaration);
  IR_set_single_declaration_list (description, single_declaration);
}

/* Checking IDENTIFIER_LIST in exclusion clause and returning formed
   unit_set_element_list. */

static IR_node_t
process_exclusion_list (IR_node_t identifier_list)
{
  IR_node_t unit_set_element_list;
  IR_node_t last_unit_set_element;
  IR_node_t current_identifier;
  IR_node_t declaration_in_table;

  unit_set_element_list = NULL;
  last_unit_set_element = NULL;
  for (current_identifier = identifier_list;
       current_identifier != NULL;
       current_identifier = IR_next_identifier (current_identifier))
    {
      declaration_in_table
	= find_single_declaration (current_identifier);
      if (declaration_in_table == NULL)
	error (FALSE, IR_position (current_identifier),
	       "unit `%s' in %%exclusion is not declared",
	       IR_identifier_itself (current_identifier));
      else if (!IR_IS_OF_TYPE (declaration_in_table,
			       IR_NM_single_unit_declaration))
	error (FALSE, IR_position (current_identifier),
	       "`%s' in %%exclusion is not unit",
	       IR_identifier_itself (current_identifier));
      else
	{
	  if (last_unit_set_element == NULL)
	    unit_set_element_list = last_unit_set_element
	      = IR_new_unit_set_element (IR_position (current_identifier),
					 declaration_in_table, NULL);
	  else
	    {
	      IR_set_next_unit_set_element
		(last_unit_set_element,
		 IR_new_unit_set_element (IR_position (current_identifier),
					  declaration_in_table, NULL));
	      last_unit_set_element
		= IR_next_unit_set_element (last_unit_set_element);
	    }
	}
    }
  return unit_set_element_list;
}

/* The function adds each element from SOURCE_LIST to the exclusion
   list of the each element from dest_list.  Checking situation "unit
   excludes itself". */

static void
add_exclusion_list (IR_node_t dest_list, IR_node_t source_list)
{
  IR_node_t current_dest_unit_set_element;
  IR_node_t current_source_unit_set_element;
  IR_node_t current_exclusion_list_unit;
  IR_node_t last_exclusion_list_unit;
  IR_node_t copy;

  for (current_dest_unit_set_element = dest_list;
       current_dest_unit_set_element != NULL;
       current_dest_unit_set_element
	 = IR_next_unit_set_element (current_dest_unit_set_element))
    for (current_source_unit_set_element = source_list;
	 current_source_unit_set_element != NULL;
	 current_source_unit_set_element
	   = IR_next_unit_set_element (current_source_unit_set_element))
      {
	if (IR_single_unit_declaration (current_dest_unit_set_element)
	    == IR_single_unit_declaration (current_source_unit_set_element))
	  {
	    error (FALSE,
		   IR_position (current_dest_unit_set_element),
		   "unit `%s' excludes itself",
		   IR_identifier_itself (IR_identifier
					 (IR_single_unit_declaration
					  (current_dest_unit_set_element))));
	    continue;
	  }
	for (current_exclusion_list_unit
	       = IR_exclusion_list (IR_single_unit_declaration
				    (current_dest_unit_set_element)),
	       last_exclusion_list_unit = NULL;
	     current_exclusion_list_unit != NULL;
	     last_exclusion_list_unit = current_exclusion_list_unit,
	       current_exclusion_list_unit
	       = IR_next_unit_set_element (current_exclusion_list_unit))
	  if (IR_single_unit_declaration (current_source_unit_set_element)
	      == IR_single_unit_declaration (current_exclusion_list_unit))
	    break;
	if (current_exclusion_list_unit == NULL)
	  {
	    /* Element not found - insert. */
	    copy = IR_copy_node (current_source_unit_set_element);
	    IR_set_next_unit_set_element (copy, NULL);
	    if (last_exclusion_list_unit == NULL)
	      IR_set_exclusion_list (IR_single_unit_declaration
				     (current_dest_unit_set_element),
				     copy);
	    else
	      IR_set_next_unit_set_element (last_exclusion_list_unit, copy);
	}
    }
}

/* The following function creates single declarations corresponding to
   the description declarations.  The function fixes errors
   `... already declared' and warnings `... repeated declaration'.
   The function also fixes occurences of undeclared automatons
   identifier in unit declarations or absence of automaton identifier
   in unit declarations if there are automaton declarations. */

static void
process_declarations (void)
{
  IR_node_t current_declaration;
  IR_node_t current_identifier;
  IR_node_t single_declaration;
  IR_node_t single_automaton_declaration;
  IR_node_t declaration_in_table;
  IR_node_t last_single_declaration;
  int automaton_presence;

  automaton_presence = FALSE;
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_automaton_declaration))
      {
        automaton_presence = TRUE;
        for (current_identifier = IR_identifier_list (current_declaration);
             current_identifier != NULL;
             current_identifier = IR_next_identifier (current_identifier))
          {
            declaration_in_table
              = find_single_automaton_declaration (current_identifier);
            if (declaration_in_table == NULL)
              {
                single_automaton_declaration
                  = IR_new_single_automaton_declaration
                    (IR_position (current_identifier), current_identifier,
                     NULL);
                declaration_in_table
                  = insert_single_automaton_declaration
                    (single_automaton_declaration);
                assert (declaration_in_table == single_automaton_declaration);
                add_single_declaration (single_automaton_declaration);
              }
            else
              {
                warning (IR_position (current_identifier),
                         "warning: repeated declaration of automaton `%s'",
                         IR_identifier_itself (current_identifier));
                append_message (IR_position (declaration_in_table),
                                "here the first declaration");
              }
          }
      }
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (!IR_IS_OF_TYPE (current_declaration, IR_NM_automaton_declaration)
	&& !IR_IS_OF_TYPE (current_declaration, IR_NM_exclusion_clause))
      {
        single_automaton_declaration = NULL;
        if (IR_IS_OF_TYPE (current_declaration, IR_NM_unit_declaration))
          {
            if (IR_automaton_identifier (current_declaration) != NULL)
              {
                single_automaton_declaration
                  = find_single_automaton_declaration (IR_automaton_identifier
                                                       (current_declaration));
                if (single_automaton_declaration == NULL)
                  error (FALSE, IR_position (IR_automaton_identifier
                                             (current_declaration)),
                         "automaton `%s' is not declared",
                         IR_identifier_itself (IR_automaton_identifier
                                               (current_declaration)));
                else
                  IR_set_automaton_is_used (single_automaton_declaration,
                                            TRUE);
              }
            else if (automaton_presence)
#define LONG_ERROR_MESSAGE\
  "there are automatons, but an automaton is absent in the unit declaration"
              error (FALSE, IR_position (current_declaration),
                     LONG_ERROR_MESSAGE);
          }
        for (current_identifier = IR_identifier_list (current_declaration);
             current_identifier != NULL;
             current_identifier = IR_next_identifier (current_identifier))
          {
            declaration_in_table
              = find_single_declaration (current_identifier);
            if (declaration_in_table == NULL)
              {
                if (IR_IS_OF_TYPE (current_declaration,
				   IR_NM_unit_declaration))
                  {
                    single_declaration
                      = IR_new_single_unit_declaration
                        (IR_position (current_identifier), current_identifier,
                         NULL);
                    IR_set_unit_number (single_declaration,
                                        IR_units_number (description));
                    IR_set_units_number (description,
                                         IR_units_number (description) + 1);
                    IR_set_single_automaton_declaration
                      (single_declaration, single_automaton_declaration);
                  }
                else if (IR_IS_OF_TYPE (current_declaration,
                                        IR_NM_instruction_declaration))
                  {
                    single_declaration
                      = IR_new_single_instruction_declaration
                        (IR_position (current_identifier), current_identifier,
                         NULL);
                    IR_set_instruction_number
                      (single_declaration,
                       IR_instructions_number (description));
                    IR_set_instructions_number
                      (description, IR_instructions_number (description) + 1);
                  }
                else
                  {
                    assert (IR_IS_OF_TYPE (current_declaration,
                                           IR_NM_reservation_declaration));
                    single_declaration
                      = IR_new_single_reservation_declaration
                        (IR_position (current_identifier), current_identifier,
                         NULL);
                  }
                declaration_in_table
                  = insert_single_declaration (single_declaration);
                assert (declaration_in_table == single_declaration);
                add_single_declaration (single_declaration);
              }
            else
              {
                if (IR_IS_OF_TYPE (current_declaration, IR_NM_unit_declaration))
                  {
                    if (IR_IS_OF_TYPE (declaration_in_table,
                                       IR_NM_single_expression_declaration))
                      error (FALSE, IR_position (current_identifier),
                             "unit `%s' is already declared",
                             IR_identifier_itself (current_identifier));
                    else
                      {
                        assert (IR_IS_OF_TYPE (current_declaration,
                                               IR_NM_unit_declaration));
                        warning (IR_position (current_identifier),
                                 "warning: repeated declaration of unit `%s'",
                                 IR_identifier_itself (current_identifier));
                      }
                  }
                else if (IR_IS_OF_TYPE (current_declaration,
                                        IR_NM_instruction_declaration))
                  {
                    if (!IR_IS_OF_TYPE (declaration_in_table,
                                        IR_NM_single_instruction_declaration))
                      error (FALSE, IR_position (current_identifier),
                             "instruction `%s' is already declared",
                             IR_identifier_itself (current_identifier));
                    else
                      {
                        warning
                          (IR_position (current_identifier),
                           "warning: repeated declaration of instruction `%s'",
                           IR_identifier_itself (current_identifier));
                      }
                  }
                else
                  {
                    assert (IR_IS_OF_TYPE (current_declaration,
                                           IR_NM_reservation_declaration));
                    if (!IR_IS_OF_TYPE (declaration_in_table,
                                        IR_NM_single_reservation_declaration))
                      error (FALSE, IR_position (current_identifier),
                             "reservation `%s' is already declared",
                             IR_identifier_itself (current_identifier));
                    else
                      {
                        warning
                          (IR_position (current_identifier),
                           "warning: repeated declaration of reservation `%s'",
                           IR_identifier_itself (current_identifier));
                      }
                  }
                append_message (IR_position (declaration_in_table),
                                "here the first declaration");
              }
          }
      }
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_exclusion_clause))
      {
	IR_node_t unit_set_element_list;
	IR_node_t unit_set_element_list_2;

	unit_set_element_list
	  = process_exclusion_list (IR_identifier_list (current_declaration));
	unit_set_element_list_2
	  = process_exclusion_list (IR_identifier_list_2
				    (current_declaration));
	add_exclusion_list (unit_set_element_list, unit_set_element_list_2);
	add_exclusion_list (unit_set_element_list_2, unit_set_element_list);
      }
  /* Make uncyclic single declaration list. */
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      IR_set_single_declaration_list
        (description, IR_next_single_declaration (last_single_declaration));
      IR_set_next_single_declaration (last_single_declaration, NULL);
    }
}

/* The following function checks that declared automaton is used.  If the
   automaton is not used, the function fixes error.  The following function
   must be called only after `process_declarations'. */

static void
check_automaton_usage (void)
{
  IR_node_t current_single_declaration;

  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_automaton_declaration)
        && !IR_automaton_is_used (current_single_declaration))
      warning (IR_position (current_single_declaration),
               "warning: automaton `%s' is not used",
               IR_identifier_itself (IR_identifier
                                     (current_single_declaration)));
}

/* The following recursive function process all expression atoms in
   order to fix usage of units and to fix errors of undeclared
   identifier or usage of instruction identifier in expression.  The
   function also sets up field of `single_declaration' of nodes of
   type `expression_atom'. */

static void
process_expression (IR_node_t expression)
{
  IR_node_t expression_identifier;
  IR_node_t declaration_in_table;

  if (IR_IS_OF_TYPE (expression, IR_NM_expression_atom))
    {
      expression_identifier = IR_expression_identifier (expression);
      declaration_in_table = find_single_declaration (expression_identifier);
      if (declaration_in_table == NULL)
        error (FALSE,  IR_position (expression_identifier),
               "undeclared unit or reservation `%s'",
               IR_identifier_itself (expression_identifier));
      else if (IR_IS_OF_TYPE (declaration_in_table,
                              IR_NM_single_instruction_declaration))
        {
          error (FALSE, IR_position (expression_identifier),
                 "instruction `%s' in expression",
                 IR_identifier_itself (expression_identifier));
          declaration_in_table = NULL;
        }
      else if (IR_IS_OF_TYPE (declaration_in_table,
                              IR_NM_single_unit_declaration))
        IR_set_unit_is_used (declaration_in_table, TRUE);
      IR_set_single_declaration (expression, declaration_in_table);
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_one_operand_expression))
    process_expression (IR_operand (expression));
  else if (IR_IS_OF_TYPE (expression, IR_NM_two_operand_expression))
    {
      process_expression (IR_left_operand (expression));
      process_expression (IR_right_operand (expression));
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_result))
    {
      
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_input))
    {
      
    }
  else
    assert (IR_IS_OF_TYPE (expression, IR_NM_nothing));
}

/* The following function processes expression definition identifiers
   in order to fix errors of undeclared identifier or usage of unit
   identifier.  The function also sets up field of `expression' of
   single declarations of instructions and reservations.  The function
   also checks expression with the aid of function `process_expression'. */

static void
process_expression_definitions (void)
{
  IR_node_t current_expression_definition;
  IR_node_t expression_identifier;
  IR_node_t declaration_in_table;

  for (current_expression_definition
       = IR_expression_definition_list (description);
       current_expression_definition != NULL;
       current_expression_definition
       = IR_next_expression_definition (current_expression_definition))
    {
      expression_identifier
        = IR_expression_identifier (current_expression_definition);
      declaration_in_table = find_single_declaration (expression_identifier);
      if (declaration_in_table == NULL)
        error (FALSE, IR_position (expression_identifier),
               "undeclared instruction or reservation `%s'",
               IR_identifier_itself (expression_identifier));
      else if (IR_IS_OF_TYPE (declaration_in_table,
                              IR_NM_single_unit_declaration))
        error (FALSE, IR_position (expression_identifier),
               "unit `%s' in left hand side of expression definition",
               IR_identifier_itself (expression_identifier));
      else if (IR_expression (declaration_in_table) != NULL)
        {
          error (FALSE, IR_position (expression_identifier),
                 (IR_IS_OF_TYPE (declaration_in_table,
                                 IR_NM_single_instruction_declaration)
                  ? "repeated definition of instruction `%s'"
                  : "repeated definition of reservation `%s'"),
                 IR_identifier_itself (expression_identifier));
          append_message (IR_position (declaration_in_table),
                          "here the first definition");
        }
      else
        IR_set_expression (declaration_in_table,
                           IR_expression (current_expression_definition));
      process_expression (IR_expression (current_expression_definition));
    }
}

/* The following function checks that declared unit is used.  If the
   unit is not used, the function fixes warning.  The following
   function must be called only after `process_declarations',
   `process_expression_definitions'. */

static void
check_unit_usage (void)
{
  IR_node_t current_single_declaration;

  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_unit_declaration)
        && !IR_unit_is_used (current_single_declaration))
      warning (IR_position (current_single_declaration),
               "warning: unit `%s' is not used",
               IR_identifier_itself (IR_identifier
                                     (current_single_declaration)));
}

/* The following function checks that there is expression definition
   for declared instruction and reservation.  If the expression
   definition is absent, the function fixes error.  The following
   function must be called only after `process_declarations',
   `process_expression_definitions'. */

static void
check_instruction_reservation_expression_definitions (void)
{
  IR_node_t current_single_declaration;

  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    {
      if (IR_IS_OF_TYPE (current_single_declaration,
                         IR_NM_single_expression_declaration)
          && IR_expression (current_single_declaration) == NULL)
        {
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_instruction_declaration))
            error (FALSE, IR_position (current_single_declaration),
                   "reservation for instruction `%s' is not defined",
                   IR_identifier_itself (IR_identifier
                                         (current_single_declaration)));
          else
            {
              assert (IR_IS_OF_TYPE (current_single_declaration,
                                     IR_NM_single_reservation_declaration));
              error (FALSE, IR_position (current_single_declaration),
                     "reservation `%s' is not defined",
                     IR_identifier_itself (IR_identifier
                                           (current_single_declaration)));
            }
        }
    }
}

/* The following variable value is number of instruction or
   reservation being processed on cycles. */

static int current_cycle_checking_pass_number;

/* The following recursive function returns TRUE if given expression
   contains given single declaration or reservations in given
   expression refers for given declaration. */

static int
cycle_in_expression (IR_node_t expression, IR_node_t start_single_declaration)
{
  if (expression == NULL)
    return FALSE;
  if (IR_IS_OF_TYPE (expression, IR_NM_expression_atom))
    {
      if (IR_single_declaration (expression) == NULL
          || IR_IS_OF_TYPE (IR_single_declaration (expression),
                            IR_NM_single_unit_declaration))
        return FALSE;
      else if (IR_single_declaration (expression) == start_single_declaration)
        return TRUE;
      else if (IR_cycle_checking_pass_number
               (IR_single_declaration (expression))
               == current_cycle_checking_pass_number)
        /* Single declaration has been processed. */
        return FALSE;
      else
        {
          IR_set_cycle_checking_pass_number
            (IR_single_declaration (expression),
             current_cycle_checking_pass_number);
          return cycle_in_expression (IR_expression (IR_single_declaration
                                                     (expression)),
                                      start_single_declaration);
        }
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_one_operand_expression))
    return cycle_in_expression (IR_operand (expression),
                                start_single_declaration);
  else if (IR_IS_OF_TYPE (expression, IR_NM_two_operand_expression))
    {
      return (cycle_in_expression (IR_left_operand (expression),
                                   start_single_declaration)
              || cycle_in_expression (IR_right_operand (expression),
                                      start_single_declaration));
    }
  else
    {
      assert (IR_IS_OF_TYPE (expression, IR_NM_no_unit));
      return FALSE;
    }
}

/* The following function fixes errors "cycle in definition ...".  The
   function uses function `cycle_in_expression' for that. */

static void
check_cycle_absence_in_expressions (void)
{
  IR_node_t current_single_declaration;

  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_expression_declaration))
      IR_set_cycle_checking_pass_number (current_single_declaration, 0);
  for (current_cycle_checking_pass_number = 1,
       current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_cycle_checking_pass_number++,
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_expression_declaration))
      {
        IR_set_cycle_checking_pass_number (current_single_declaration,
                                           current_cycle_checking_pass_number);
        if (cycle_in_expression (IR_expression (current_single_declaration),
                                 current_single_declaration))
          {
            assert (IR_expression (current_single_declaration) != NULL);
            error (FALSE,
                   IR_position (IR_expression (current_single_declaration)),
                   (IR_IS_OF_TYPE (current_single_declaration,
                                   IR_NM_single_instruction_declaration)
                    ? "cycle in definition of instruction `%s'"
                    : "cycle in definition of reservation `%s'"),
                   IR_identifier (current_single_declaration));
          }
      }
}

static int
process_expression_cycles (IR_node_t expression, int start_cycle)
{
  if (IR_IS_OF_TYPE (expression, IR_NM_expression_atom))
    {
      if (IR_IS_OF_TYPE (IR_single_declaration (expression),
                         IR_NM_single_unit_declaration))
        {
          if (IR_max_occurrence_cycle_number (IR_single_declaration
                                              (expression)) < start_cycle + 1)
            IR_set_max_occurrence_cycle_number
              (IR_single_declaration (expression), start_cycle + 1);
          return start_cycle + 1;
        }
      else
        {
          assert (IR_IS_OF_TYPE (IR_single_declaration (expression),
                                 IR_NM_single_reservation_declaration));
          return
            process_expression_cycles
              (IR_expression (IR_single_declaration (expression)),
               start_cycle);
        }
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_optional_expression))
    return process_expression_cycles (IR_operand (expression), start_cycle);
  else if (IR_IS_OF_TYPE (expression, IR_NM_repetition))
    {
      int current_repetition_number;

      for (current_repetition_number = 0;
           current_repetition_number
           < IR_number_value (IR_repetition_number (expression));
           current_repetition_number++)
        start_cycle = process_expression_cycles (IR_operand (expression),
                                                 start_cycle);
      return start_cycle;
    }
  else if (IR_IS_OF_TYPE (expression, IR_NM_concatenation))
    return
      process_expression_cycles
        (IR_right_operand (expression),
         process_expression_cycles (IR_left_operand (expression),
                                    start_cycle)) - 1;
  else if (IR_IS_OF_TYPE (expression, IR_NM_new_cycle_concatenation))
    return
      process_expression_cycles
        (IR_right_operand (expression),
         process_expression_cycles (IR_left_operand (expression),
                                    start_cycle));
  else if (IR_IS_OF_TYPE (expression, IR_NM_alternative))
    {
      int left_operand_cycles;
      int right_operand_cycles;

      left_operand_cycles
        = process_expression_cycles (IR_left_operand (expression),
                                     start_cycle);
      right_operand_cycles
        = process_expression_cycles (IR_right_operand (expression),
                                     start_cycle);
      return (left_operand_cycles < right_operand_cycles
              ? right_operand_cycles : left_operand_cycles);
    }
  else
    {
      assert (IR_IS_OF_TYPE (expression, IR_NM_no_unit));
      return start_cycle + 1;
    }
}

/* The following function is called only for correct program. */

static void
calculate_max_instruction_reservation_and_unit_occurrence_cycles (void)
{
  IR_node_t current_single_declaration;
  int max_instruction_cycles_number;

  IR_set_max_instruction_reservation_cycles (description, 0);
  for (current_single_declaration = IR_single_declaration_list (description);
       current_single_declaration != NULL;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_instruction_declaration))
      {
        max_instruction_cycles_number
          = process_expression_cycles
            (IR_expression (current_single_declaration), 0);
        if (IR_max_instruction_reservation_cycles (description)
            < max_instruction_cycles_number)
          IR_set_max_instruction_reservation_cycles
            (description, max_instruction_cycles_number);
      }
}

/* The following function calls functions for semantic analysis of all
   description. */

void
analyze_description (void)
{
  IR_set_single_declaration_list (description, NULL);
  process_declarations ();
  check_automaton_usage ();
  process_expression_definitions ();
  check_unit_usage ();
  check_instruction_reservation_expression_definitions ();
  check_cycle_absence_in_expressions ();
  if (number_of_errors == 0)
    calculate_max_instruction_reservation_and_unit_occurrence_cycles ();
}
