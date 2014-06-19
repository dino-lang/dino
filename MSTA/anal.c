/*
   Copyright (C) 1997-2007 Vladimir Makarov.

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
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include "position.h"
#include "errors.h"
#include "vlobject.h"
#include "hashtab.h"
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

#define ERROR_DEFAULT_VALUE  256

#define START_TOKEN_VALUE    257

static vlo_t last_identifier_or_literal_representation;

static void
initiate_identifier_or_literal_representations (void)
{
  VLO_CREATE (last_identifier_or_literal_representation, 128);
}

static const char *
identifier_or_literal_representation (IR_node_t identifier_or_literal)
{
  if (IR_IS_OF_TYPE (identifier_or_literal, IR_NM_literal))
    return IR_character_representation (identifier_or_literal);
  else
    {
      VLO_NULLIFY (last_identifier_or_literal_representation);
      VLO_ADD_STRING (last_identifier_or_literal_representation, "`");
      VLO_ADD_STRING (last_identifier_or_literal_representation,
                      IR_identifier_itself (identifier_or_literal));
      VLO_ADD_STRING (last_identifier_or_literal_representation, "'");
      return VLO_BEGIN (last_identifier_or_literal_representation);
    }
}

static const char *
single_definition_representation (IR_node_t single_definition)
{
  if (!IR_IS_OF_TYPE (single_definition, IR_NM_literal_range_definition))
    return identifier_or_literal_representation (IR_identifier_or_literal
                                                 (single_definition));
  else
    {
      VLO_NULLIFY (last_identifier_or_literal_representation);
      VLO_ADD_STRING (last_identifier_or_literal_representation,
                      IR_character_representation
                      (IR_identifier_or_literal (single_definition)));
      VLO_ADD_STRING (last_identifier_or_literal_representation, "-");
      VLO_ADD_STRING (last_identifier_or_literal_representation,
                      IR_character_representation
                      (IR_right_range_bound_literal (single_definition)));
      return VLO_BEGIN (last_identifier_or_literal_representation);
    }
}

static void
finish_identifier_or_literal_representations (void)
{
  VLO_DELETE (last_identifier_or_literal_representation);
}

static IR_node_t
check_tag (IR_node_t tag)
{
  if (tag != NULL)
    {
      if (!IR_IS_OF_TYPE (tag, IR_NM_identifier))
        {
          assert (IR_IS_OF_TYPE (tag, IR_NM_literal));
          error (FALSE, IR_position (tag), "tag %s can not be literal",
                 identifier_or_literal_representation (tag));
          tag = NULL;
        }
      else if (IR_dot_presence_flag (tag))
        error (FALSE, IR_position (tag),
               "tag `%s' is not correct C identifier",
               IR_identifier_itself (tag));
    }
  return tag;
}

static void
add_single_definition_to_cyclic_list (IR_node_t single_definition)
{
  if (IR_single_definition_list (description) == NULL)
    /* Make cycle. */
    IR_set_next_single_definition (single_definition, single_definition);
  else
    {
      IR_set_next_single_definition
        (single_definition,
         IR_next_single_definition (IR_single_definition_list (description)));
      IR_set_next_single_definition
        (IR_single_definition_list (description), single_definition);
    }
  IR_set_single_definition_list (description, single_definition);
}

static int current_priority;

static void
process_a_token_definition (IR_node_t definition)
{
  IR_node_t tag;
  IR_node_t current_symbol;
  IR_node_t single_definition_in_table;
  IR_node_t single_definition;
  int first_definition_flag;
  
  tag = check_tag (IR_tag (definition));
  for (current_symbol = IR_symbol_list (definition);
       current_symbol != NULL;
       current_symbol = IR_next_symbol (current_symbol))
    {
      single_definition_in_table
        = find_single_definition (IR_identifier_or_literal (current_symbol));
      if (single_definition_in_table != NULL
          && IR_IS_OF_TYPE (single_definition_in_table,
                            IR_NM_single_nonterm_definition))
        {
          error (FALSE,
                 IR_position (IR_identifier_or_literal (current_symbol)),
                 "`%s' is already defined as nonterminal",
                 IR_identifier_itself (IR_identifier_or_literal
                                       (single_definition_in_table)));
          append_message (IR_position (single_definition_in_table),
                          "here previous definition");
        }
      else
        {
          if (single_definition_in_table == NULL)
            {
              /* The first definition. */
              first_definition_flag = TRUE;
              single_definition
                = IR_new_single_term_definition
                  (IR_position (IR_identifier_or_literal (current_symbol)),
                   IR_identifier_or_literal (current_symbol), NULL);
              single_definition_in_table
                = insert_single_definition (single_definition);
              assert (single_definition == single_definition_in_table);
              add_single_definition_to_cyclic_list (single_definition);
            }
          else
            first_definition_flag = FALSE;
          if (tag != NULL)
            {
              if (IR_type (single_definition_in_table) != NULL)
                {
                  /* Repeated definition. */
                  assert (!first_definition_flag);
                  if (!w_flag)
                    {
                      warning (IR_position (IR_identifier_or_literal
                                            (current_symbol)),
                               "warning: type of token %s is already defined",
                               identifier_or_literal_representation
                               (IR_identifier_or_literal (current_symbol)));
                      append_message (IR_position (single_definition_in_table),
                                      "here previous definition");
                    }
                }
              else
                IR_set_type (single_definition_in_table, tag);
            }
          if (IR_number (current_symbol) != NULL)
            {
              if (!first_definition_flag)
                {
                  /* Repeated definition. */
                  if (!w_flag)
                    {
                      warning
                        (IR_position (IR_identifier_or_literal
                                      (current_symbol)),
                         "warning: value of token %s is already assigned",
                         identifier_or_literal_representation
                         (IR_identifier_or_literal (current_symbol)));
                      append_message (IR_position (single_definition_in_table),
                                      "here previous definition");
                    }
                }
              else if (IR_number_value (IR_number (current_symbol)) == 0)
                error (FALSE, IR_position (IR_number (current_symbol)),
                       "token %s has zero value",
                       identifier_or_literal_representation
                       (IR_identifier_or_literal (current_symbol)));
              else
                {
                  IR_set_value (single_definition_in_table,
                                IR_number_value (IR_number (current_symbol)));
                  if (IR_IS_OF_TYPE (IR_identifier_or_literal
                                     (single_definition_in_table),
                                     IR_NM_literal))
                    insert_literal_definition (single_definition_in_table);
                }
            }
          if (IR_IS_OF_TYPE (definition, IR_NM_left_definition)
              || IR_IS_OF_TYPE (definition, IR_NM_right_definition)
              || IR_IS_OF_TYPE (definition, IR_NM_nonassoc_definition))
            {
              if (IR_priority (single_definition_in_table) >= 0)
                {
                  /* Repeated definition. */
                  assert (!first_definition_flag);
                  if (!w_flag)
                    {
                      warning
                        (IR_position (IR_identifier_or_literal
                                      (current_symbol)),
                         "warning: priority of token %s is already defined",
                         identifier_or_literal_representation
                         (IR_identifier_or_literal (current_symbol)));
                      append_message (IR_position (single_definition_in_table),
                                      "here previous definition");
                    }
                }
              else
                {
                  IR_set_priority (single_definition_in_table,
                                   current_priority);
                  if (IR_IS_OF_TYPE (definition, IR_NM_left_definition))
                    IR_set_left_assoc_flag (single_definition_in_table, TRUE);
                  else if (IR_IS_OF_TYPE (definition, IR_NM_right_definition))
                    IR_set_right_assoc_flag (single_definition_in_table, TRUE);
                  else
                    IR_set_nonassoc_flag (single_definition_in_table, TRUE);
                }
            }
        }
    }
  if (IR_IS_OF_TYPE (definition, IR_NM_left_definition)
      || IR_IS_OF_TYPE (definition, IR_NM_right_definition)
      || IR_IS_OF_TYPE (definition, IR_NM_nonassoc_definition))
    current_priority++;
}

/* The following function is called only when flag `-strict' and
   `-yacc-input' is used.  In this case %type clauses are processed in
   the same sequency as other definitions.   As the result this
   corresponds the behaviour of POSIX YACC. */
static void
process_type_definition_1 (IR_node_t definition)
{
  IR_node_t tag;
  IR_node_t current_symbol;
  IR_node_t single_definition_in_table;
  IR_node_t single_definition;
  
  assert (yacc_input_flag && strict_flag);
  tag = check_tag (IR_tag (definition));
  if (tag == NULL)
    error (FALSE, IR_position (definition),
           "%%type definition without type tag");
  for (current_symbol = IR_symbol_list (definition);
       current_symbol != NULL;
       current_symbol = IR_next_symbol (current_symbol))
    {
      if (IR_IS_OF_TYPE (IR_identifier_or_literal (current_symbol),
                         IR_NM_literal))
        error (FALSE,
               IR_position (IR_identifier_or_literal (current_symbol)),
               "literal %s used as token is ignored",
               identifier_or_literal_representation
               (IR_identifier_or_literal (current_symbol)));
      else
        {
          if (IR_number (current_symbol) != NULL)
            error (FALSE, IR_position (IR_number (current_symbol)),
                   "nonterminal `%s' can not have value",
                   IR_identifier_itself (IR_identifier_or_literal
                                         (current_symbol)));
          single_definition_in_table
            = find_single_definition
              (IR_identifier_or_literal (current_symbol));
          if (single_definition_in_table != NULL
              && IR_IS_OF_TYPE (single_definition_in_table,
                                IR_NM_single_term_definition))
            {
              error (FALSE,
                     IR_position (IR_identifier_or_literal (current_symbol)),
                     "`%s' is already defined as token",
                     IR_identifier_itself (IR_identifier_or_literal
                                           (single_definition_in_table)));
              append_message (IR_position (single_definition_in_table),
                              "here previous definition");
            }
          else
            {
              if (single_definition_in_table == NULL)
                {
                  /* The first definition. */
                  single_definition
                    = IR_new_single_nonterm_definition
                      (IR_position (IR_identifier_or_literal (current_symbol)),
                       IR_identifier_or_literal (current_symbol), NULL, NULL);
                  IR_set_type (single_definition, tag);
                  single_definition_in_table
                    = insert_single_definition (single_definition);
                  assert (single_definition == single_definition_in_table);
                  add_single_definition_to_cyclic_list (single_definition);
                }
              else if (!w_flag)
                {
                  assert (IR_IS_OF_TYPE (single_definition_in_table,
                                         IR_NM_single_nonterm_definition));
                  warning
                    (IR_position (IR_identifier_or_literal (current_symbol)),
                     "warning: type of nonterminal `%s' is already defined",
                     IR_identifier_itself
                     (IR_identifier_or_literal (current_symbol)));
                  append_message (IR_position (single_definition_in_table),
                                  "here previous definition");
                }
            }
        }
    }
}

/* The following function is called only when flag `-strict' or
   `-yacc-input' is used.  In this case %type clauses are processed
   only after other definitions has been processed.  As the result this
   corresponds the behaviour of Bison and Byacc. */
static void
process_type_definition_2 (IR_node_t definition)
{
  IR_node_t tag;
  IR_node_t current_symbol;
  IR_node_t single_definition_in_table;
  IR_node_t single_definition;
  
  assert (!yacc_input_flag || !strict_flag);
  tag = check_tag (IR_tag (definition));
  if (tag == NULL)
    error (FALSE, IR_position (definition),
           "%%type definition without type tag");
  for (current_symbol = IR_symbol_list (definition);
       current_symbol != NULL;
       current_symbol = IR_next_symbol (current_symbol))
    {
      if (IR_number (current_symbol) != NULL)
        error (FALSE, IR_position (IR_number (current_symbol)),
               "symbol `%s' can not have value in %%type clause",
               IR_identifier_itself (IR_identifier_or_literal
                                     (current_symbol)));
      single_definition_in_table
        = find_single_definition (IR_identifier_or_literal (current_symbol));
      if (yacc_input_flag && !w_flag
          && (IR_IS_OF_TYPE (IR_identifier_or_literal (current_symbol),
                             IR_NM_literal)
              || (single_definition_in_table != NULL
		  && IR_IS_OF_TYPE (single_definition_in_table,
				    IR_NM_single_term_definition))))
        warning
          (IR_position (IR_identifier_or_literal (current_symbol)),
           "warning: token %s used in %%type (not POSIX YACC)",
           identifier_or_literal_representation
           (IR_identifier_or_literal (current_symbol)));
      if (single_definition_in_table == NULL)
        {
          /* The first definition of nonterminal or literal. */
          if (IR_IS_OF_TYPE (IR_identifier_or_literal (current_symbol),
                             IR_NM_literal))
            single_definition
              = IR_new_single_term_definition
                (IR_position (IR_identifier_or_literal (current_symbol)),
                 IR_identifier_or_literal (current_symbol), NULL);
          else
            single_definition
              = IR_new_single_nonterm_definition
                (IR_position (IR_identifier_or_literal (current_symbol)),
                 IR_identifier_or_literal (current_symbol), NULL, NULL);
          IR_set_type (single_definition, tag);
          single_definition_in_table
            = insert_single_definition (single_definition);
          assert (single_definition == single_definition_in_table);
          add_single_definition_to_cyclic_list (single_definition);
        }
      else if (IR_type (single_definition_in_table) != NULL)
        {
          if (!w_flag)
            {
              warning
                (IR_position (IR_identifier_or_literal (current_symbol)),
                 "warning: type of symbol `%s' is already defined",
                 IR_identifier_itself
                 (IR_identifier_or_literal (current_symbol)));
              append_message (IR_position (single_definition_in_table),
                              "here previous definition");
            }
        }
      else
        IR_set_type (single_definition_in_table, tag);
    }
}

static void
process_definition_list (void)
{
  IR_node_t current_definition;
  IR_node_t error_identifier;

  current_priority = 0;
  IR_set_single_definition_list (description, NULL);
  for (current_definition = IR_definition_list (description);
       current_definition != NULL;
       current_definition = IR_next_definition (current_definition))
    if (IR_IS_OF_TYPE (current_definition, IR_NM_start_definition))
      {
        if (!IR_IS_OF_TYPE (IR_identifier (current_definition),
                            IR_NM_identifier))
          error (FALSE, IR_position (IR_identifier (current_definition)),
                 "identifier must be after `%start'");
        else if (IR_axiom_identifier (description) != NULL)
          {
            if (!w_flag)
              warning (IR_position (current_definition),
                       "warning: repeated `%start' clause is ignored");
          }
        else
          IR_set_axiom_identifier (description,
                                   IR_identifier (current_definition));
      }
    else if (IR_IS_OF_TYPE (current_definition, IR_NM_scanner_definition))
      {
        if (yacc_input_flag)
          error (FALSE, IR_position (current_definition),
                 "`%%scanner' clause is absent in Posix YACC");
        else if (IR_scanner_flag (description))
          {
            if (!w_flag)
              warning (IR_position (current_definition),
                       "warning: repeated `%%scanner' clause");
          }
        IR_set_scanner_flag (description, TRUE);
      }
    else if (IR_IS_OF_TYPE (current_definition, IR_NM_expect_definition))
      {
        if (yacc_input_flag)
          error (FALSE, IR_position (current_definition),
                 "`%%expect' clause is absent in Posix YACC");
        else if (IR_expected_shift_reduce_conflicts_number (description) >= 0)
          {
            if (!w_flag)
              warning (IR_position (current_definition),
                       "warning: repeated `%%expect' clause is ignored");
          }
        IR_set_expected_shift_reduce_conflicts_number
          (description,
           IR_number_value (IR_expected_number (current_definition)));
      }
    else if (IR_IS_OF_TYPE (current_definition, IR_NM_token_definition)
             || IR_IS_OF_TYPE (current_definition, IR_NM_left_definition)
             || IR_IS_OF_TYPE (current_definition, IR_NM_right_definition)
             || IR_IS_OF_TYPE (current_definition, IR_NM_nonassoc_definition))
      process_a_token_definition (current_definition);
    else if (IR_IS_OF_TYPE (current_definition, IR_NM_type_definition))
      {
        if (yacc_input_flag && strict_flag)
          process_type_definition_1 (current_definition);
      }
    else
      {
        assert (IR_IS_OF_TYPE (current_definition, IR_NM_code));
        if (IR_IS_OF_TYPE (current_definition, IR_NM_union_code))
          {
            if (IR_union_code (description) != NULL)
              error (FALSE, IR_position (current_definition),
                     "repeated occurence of `%%union' code");
            else
              IR_set_union_code (description, current_definition);
          }
        else
          {
            if (yacc_input_flag)
              {
                if (IR_IS_OF_TYPE (current_definition, IR_NM_local_code))
                  error
                    (FALSE, IR_position (current_definition),
                     "construction `%%local {...}' is absent in Posix YACC");
                else if (IR_IS_OF_TYPE (current_definition, IR_NM_import_code))
                  error
                    (FALSE, IR_position (current_definition),
                     "construction `%%import {...}' is absent in Posix YACC");
                else if (IR_IS_OF_TYPE (current_definition, IR_NM_export_code))
                  error
                    (FALSE, IR_position (current_definition),
                     "construction `%%export {...}' is absent in Posix YACC");
              }
          }
      }
  error_identifier = IR_new_identifier (no_position, "error", FALSE);
  if ((error_single_definition = find_single_definition (error_identifier))
      == NULL)
    {
      /* Create single terminal definition for `error'. */
      error_single_definition
        = IR_new_single_term_definition (no_position, error_identifier, NULL);
#ifndef NDEBUG
      assert (error_single_definition
              == insert_single_definition (error_single_definition));
#else
      insert_single_definition (error_single_definition);
#endif
      IR_set_value (error_single_definition, ERROR_DEFAULT_VALUE);
      /* Check up dupicated values in function
         `check_duplicated_token_values'. */
      add_single_definition_to_cyclic_list (error_single_definition);
    }
  for (current_definition = IR_definition_list (description);
       current_definition != NULL;
       current_definition = IR_next_definition (current_definition))
    if (IR_IS_OF_TYPE (current_definition, IR_NM_type_definition)
        && (!yacc_input_flag || !strict_flag))
      process_type_definition_2 (current_definition);
}

static IR_node_t
insert_implicit_definition (IR_node_t identifier_or_literal)
{
  IR_node_t single_definition;
  IR_node_t single_definition_in_table;
  
  single_definition = find_single_definition (identifier_or_literal);
  if (single_definition == NULL)
    {
      if (IR_IS_OF_TYPE (identifier_or_literal, IR_NM_identifier))
        single_definition
          = IR_new_single_nonterm_definition
            (IR_position (identifier_or_literal), identifier_or_literal,
             NULL, NULL);
      else
        single_definition
          = IR_new_single_term_definition
            (IR_position (identifier_or_literal), identifier_or_literal, NULL);
      single_definition_in_table
        = insert_single_definition (single_definition);
      assert (single_definition == single_definition_in_table);
      add_single_definition_to_cyclic_list (single_definition);
    }
  return single_definition;
}

static IR_node_t
create_literal (int literal_code, position_t position,
                int character_representation_flag);

static IR_node_t
insert_literal_range_definition (int left_range_value, int right_range_value,
                                 IR_node_t range_atom,
                                 int bounds_have_explicit_values)
{
  IR_node_t single_definition;
  IR_node_t left_literal;
  IR_node_t right_literal;

  assert (left_range_value < right_range_value);
  single_definition = find_literal_definition (left_range_value,
                                               right_range_value);
  if (single_definition == NULL)
    {
      left_literal = IR_left_bound (range_atom);
      right_literal = IR_right_bound (range_atom);
      if (IR_IS_OF_TYPE (range_atom, IR_NM_range_no_left_right_bounds_atom))
        {
          left_literal = create_literal (left_range_value,
                                         IR_position (range_atom), TRUE);
          right_literal = create_literal (right_range_value,
                                          IR_position (range_atom), TRUE);
        }
      else if (IR_IS_OF_TYPE (range_atom, IR_NM_range_no_left_bound_atom))
        left_literal = create_literal (left_range_value,
                                       IR_position (range_atom), TRUE);
      else if (IR_IS_OF_TYPE (range_atom, IR_NM_range_no_right_bound_atom))
        right_literal = create_literal (right_range_value,
                                        IR_position (range_atom), TRUE);
      single_definition
        = IR_new_literal_range_definition
          (IR_position (range_atom), left_literal, NULL, right_literal);
      IR_set_value (single_definition, left_range_value);
      IR_set_right_range_bound_value (single_definition, right_range_value);
      IR_set_bounds_have_explicit_values (single_definition,
                                          bounds_have_explicit_values);
      add_single_definition_to_cyclic_list (single_definition);
      (void) insert_literal_definition (single_definition);
    }
  return single_definition;
}

static void
create_character_representation (int literal_code)
{
  const char *str;
  char representation [10];

  assert (literal_code >= 0 && literal_code <= UCHAR_MAX);
  if (isprint (literal_code) && literal_code != '\''
      && literal_code != '\\')
    {
      IR_TOP_ADD_BYTE ('\'');
      IR_TOP_ADD_BYTE (literal_code);
      IR_TOP_ADD_BYTE (0);
      IR_TOP_ADD_STRING ("'");
    }
  else
    {
      if (literal_code == '\n')
        str = "'\\n'";
      else if (literal_code == '\t')
        str = "'\\t'";
      else if (literal_code == '\v')
        str = "'\\v'";
      else if (literal_code == '\b')
        str = "'\\b'";
      else if (literal_code == '\r')
        str = "'\\r'";
      else if (literal_code == '\f')
        str = "'\\f'";
      else if (literal_code == '\\')
        str = "'\\\\'";
      else if (literal_code == '\'')
        str = "'\\''";
      else
        {
          sprintf (representation, "'\\%o'", literal_code);
          str = representation;
        }
      IR_TOP_ADD_STRING (str);
    }
}

static IR_node_t
create_literal (int literal_code, position_t position,
                int character_representation_flag)
{
  char *character_representation;

  if (character_representation_flag)
    create_character_representation (literal_code);
  else
    IR_TOP_ADD_BYTE ('\0');
  character_representation = insert_string (IR_TOP_BEGIN ());
  if (character_representation == (char *) IR_TOP_BEGIN ())
    IR_TOP_FINISH ();
  else
    IR_TOP_NULLIFY ();
  return IR_new_literal (position, character_representation, literal_code);
}

static IR_node_t
implicit_literal_definition (int literal_code, position_t position)
{
  return
    insert_implicit_definition (create_literal (literal_code, position, TRUE));
}



/* This page contains table of patterns. */

static unsigned
pattern_hash_function (hash_table_entry_t pattern)
{
  IR_node_t p = (IR_node_t) pattern;
  unsigned int hash_value = 0;
  IR_node_t current_alternative;
  IR_node_t current_sequence_element;

  if (p == NULL)
    return 0;
  if (IR_IS_OF_TYPE (p, IR_NM_pattern))
    {
      for (current_alternative = IR_alternatives_list (p);
	   current_alternative != NULL;
	   current_alternative = IR_next_alternative (current_alternative))
	hash_value += pattern_hash_function (current_alternative);
      hash_value += 3;
    }
  else if (IR_IS_OF_TYPE (p, IR_NM_sequence))
    {
      for (current_sequence_element = IR_sequence (p);
	   current_sequence_element != NULL;
	   current_sequence_element
	     = IR_next_sequence_element (current_sequence_element))
	hash_value += pattern_hash_function (current_sequence_element);
      if (IR_max_look_ahead_number (p) != NULL)
        hash_value += IR_number_value (IR_max_look_ahead_number (p));
      if (IR_precedence_identifier_or_literal (p) != NULL)
        hash_value
          += (ptrdiff_t)
             (IR_IS_OF_TYPE (IR_precedence_identifier_or_literal (p),
                             IR_NM_identifier)
              ? IR_identifier_itself (IR_precedence_identifier_or_literal (p))
              : IR_character_representation
                (IR_precedence_identifier_or_literal (p)));
      hash_value += 37;
    }
  else if (IR_IS_OF_TYPE (p, IR_NM_separator_iteration))
    {
      hash_value
        = (pattern_hash_function (IR_iteration_sequence (p))
           + pattern_hash_function (IR_separator_sequence (p)));
      if (IR_iteration_max_look_ahead_number (p) != NULL)
        hash_value += IR_number_value (IR_iteration_max_look_ahead_number (p));
      if (IR_separator_max_look_ahead_number (p) != NULL)
        hash_value += IR_number_value (IR_separator_max_look_ahead_number (p));
      if (IR_iteration_precedence_identifier_or_literal (p) != NULL)
        hash_value
          += (ptrdiff_t)
             (IR_IS_OF_TYPE (IR_iteration_precedence_identifier_or_literal (p),
                             IR_NM_identifier)
              ? IR_identifier_itself
  	        (IR_iteration_precedence_identifier_or_literal (p))
              : IR_character_representation
                (IR_iteration_precedence_identifier_or_literal (p)));
      if (IR_separator_precedence_identifier_or_literal (p) != NULL)
        hash_value
          += (ptrdiff_t)
             (IR_IS_OF_TYPE (IR_separator_precedence_identifier_or_literal (p),
                             IR_NM_identifier)
              ? IR_identifier_itself
  	        (IR_separator_precedence_identifier_or_literal (p))
              : IR_character_representation
                (IR_separator_precedence_identifier_or_literal (p)));
      hash_value += 121;
    }
  else if (IR_IS_OF_TYPE (p, IR_NM_control_point))
    hash_value = 17; /* I like it */
  else if (IR_IS_OF_TYPE (p, IR_NM_default))
    hash_value = 1023 + pattern_hash_function (IR_default_pattern (p));
  else if (IR_IS_OF_TYPE (p, IR_NM_star_iteration))
    hash_value = 4123 + pattern_hash_function (IR_iteration_unit (p));
  else if (IR_IS_OF_TYPE (p, IR_NM_plus_iteration))
    hash_value = 2173 + pattern_hash_function (IR_iteration_unit (p));
  else if (IR_IS_OF_TYPE (p, IR_NM_code_insertion_atom))
    hash_value = 257 + (ptrdiff_t) IR_code_insertion (p);
  else if (IR_IS_OF_TYPE (p, IR_NM_group))
    hash_value = 887 + pattern_hash_function (IR_pattern (p));
  else if (IR_IS_OF_TYPE (p, IR_NM_range_atom))
    {
      hash_value = 419;
      if (IR_left_bound (p) != NULL)
        hash_value
          += (ptrdiff_t) IR_character_representation (IR_left_bound (p));
      if (IR_right_bound (p) != NULL)
        hash_value
          += (ptrdiff_t) IR_character_representation (IR_right_bound (p));
      if (IR_IS_OF_TYPE (p, IR_NM_range_no_left_bound_atom))
	hash_value += 19;
      else if (IR_IS_OF_TYPE (p, IR_NM_range_no_right_bound_atom))
	hash_value += 29;
      else if (IR_IS_OF_TYPE (p, IR_NM_range_no_left_right_bounds_atom))
	hash_value += 67;
    }
  else if (IR_IS_OF_TYPE (p, IR_NM_identifier_or_literal_atom))
    {
      hash_value
	= (ptrdiff_t)
	  (IR_IS_OF_TYPE (IR_identifier_or_literal (p), IR_NM_identifier)
	   ? IR_identifier_itself (IR_identifier_or_literal (p))
	   : IR_character_representation (IR_identifier_or_literal
					  (p))) + 671;
    }
  else if (IR_IS_OF_TYPE (p, IR_NM_string_atom))
    hash_value = 553 + (ptrdiff_t) IR_string_itself (IR_string (p));
  else
    assert (FALSE);
  return hash_value;
}

static int
pattern_eq_function (hash_table_entry_t pattern_1,
		     hash_table_entry_t pattern_2)
{
  IR_node_t p1 = (IR_node_t) pattern_1;
  IR_node_t p2 = (IR_node_t) pattern_2;
  IR_node_t current_alternative_1;
  IR_node_t current_alternative_2;
  IR_node_t current_sequence_element_1;
  IR_node_t current_sequence_element_2;

  if (p1 == p2)
    return TRUE; /* case p1 == p2 == NULL here. */
  else if (p1 == NULL || p2 == NULL)
    return FALSE;
  if (IR_NODE_MODE (p1) != IR_NODE_MODE (p2))
    return FALSE;
  if (IR_IS_OF_TYPE (p1, IR_NM_pattern))
    {
      for (current_alternative_1 = IR_alternatives_list (p1),
	   current_alternative_2 = IR_alternatives_list (p2);
	   current_alternative_1 != NULL && current_alternative_2 != NULL;
	   current_alternative_1 = IR_next_alternative (current_alternative_1),
	   current_alternative_2 = IR_next_alternative (current_alternative_2))
	if (!pattern_eq_function (current_alternative_1,
				  current_alternative_2))
	  return FALSE;
      return current_alternative_1 == current_alternative_2;
    }
  else if (IR_IS_OF_TYPE (p1, IR_NM_sequence))
    {
      for (current_sequence_element_1 = IR_sequence (p1),
	   current_sequence_element_2 = IR_sequence (p2);
	   current_sequence_element_1 != NULL
	     && current_sequence_element_2 != NULL;
	   current_sequence_element_1
	     = IR_next_sequence_element (current_sequence_element_1),
	   current_sequence_element_2
	     = IR_next_sequence_element (current_sequence_element_2))
	if (!pattern_eq_function (current_sequence_element_1,
				  current_sequence_element_2))
	  return FALSE;
      if ((IR_precedence_identifier_or_literal (p1) != NULL
	   && IR_precedence_identifier_or_literal (p2) == NULL)
	  || (IR_precedence_identifier_or_literal (p1) == NULL
	      && IR_precedence_identifier_or_literal (p2) != NULL))
	return FALSE;
      if ((IR_max_look_ahead_number (p1) == NULL
	   && IR_max_look_ahead_number (p2) != NULL)
          || (IR_max_look_ahead_number (p1) != NULL
	      && IR_max_look_ahead_number (p2) == NULL)
          || (IR_max_look_ahead_number (p1) != NULL
	      && (IR_number_value (IR_max_look_ahead_number (p1))
		  != IR_number_value (IR_max_look_ahead_number (p2)))))
        return FALSE;
      return
	((IR_precedence_identifier_or_literal (p1) == NULL
          || strcmp (IR_IS_OF_TYPE
                     (IR_precedence_identifier_or_literal (p1),
                      IR_NM_identifier)
                     ? IR_identifier_itself
                       (IR_precedence_identifier_or_literal (p1))
                     : IR_character_representation
                       (IR_precedence_identifier_or_literal (p1)),
                     IR_IS_OF_TYPE
                     (IR_precedence_identifier_or_literal (p2),
                      IR_NM_identifier)
                     ? IR_identifier_itself
                       (IR_precedence_identifier_or_literal (p2))
                     : IR_character_representation
                       (IR_precedence_identifier_or_literal (p2))) == 0));
    }
  else if (IR_IS_OF_TYPE (p1, IR_NM_separator_iteration))
    {
      if ((IR_iteration_precedence_identifier_or_literal (p1) != NULL
	   && IR_iteration_precedence_identifier_or_literal (p2) == NULL)
	  || (IR_iteration_precedence_identifier_or_literal (p1) == NULL
	      && IR_iteration_precedence_identifier_or_literal (p2) != NULL)
	  || (IR_separator_precedence_identifier_or_literal (p1) != NULL
	      && IR_separator_precedence_identifier_or_literal (p2) == NULL)
	  || (IR_separator_precedence_identifier_or_literal (p1) == NULL
	      && IR_separator_precedence_identifier_or_literal (p2) != NULL))
	return FALSE;
      if ((IR_iteration_max_look_ahead_number (p1) == NULL
	   && IR_iteration_max_look_ahead_number (p2) != NULL)
          || (IR_iteration_max_look_ahead_number (p1) != NULL
	      && IR_iteration_max_look_ahead_number (p2) == NULL)
          || (IR_iteration_max_look_ahead_number (p1) != NULL
	      && (IR_number_value (IR_iteration_max_look_ahead_number (p1))
		  != IR_number_value (IR_iteration_max_look_ahead_number (p2)))))
        return FALSE;
      if ((IR_separator_max_look_ahead_number (p1) == NULL
	   && IR_separator_max_look_ahead_number (p2) != NULL)
          || (IR_separator_max_look_ahead_number (p1) != NULL
	      && IR_separator_max_look_ahead_number (p2) == NULL)
          || (IR_separator_max_look_ahead_number (p1) != NULL
	      && (IR_number_value (IR_separator_max_look_ahead_number (p1))
		  != IR_number_value (IR_separator_max_look_ahead_number (p2)))))
        return FALSE;
      return
	(pattern_eq_function (IR_iteration_sequence (p1),
			      IR_iteration_sequence (p2))
	 && pattern_eq_function (IR_separator_sequence (p1),
				 IR_separator_sequence (p2))
	 && (IR_iteration_precedence_identifier_or_literal (p1) == NULL
	     || strcmp (IR_IS_OF_TYPE
			(IR_iteration_precedence_identifier_or_literal (p1),
			 IR_NM_identifier)
			? IR_identifier_itself
			  (IR_iteration_precedence_identifier_or_literal (p1))
			: IR_character_representation
			  (IR_iteration_precedence_identifier_or_literal (p1)),
                        IR_IS_OF_TYPE
			(IR_iteration_precedence_identifier_or_literal (p2),
			 IR_NM_identifier)
			? IR_identifier_itself
			  (IR_iteration_precedence_identifier_or_literal (p2))
			: IR_character_representation
			  (IR_iteration_precedence_identifier_or_literal (p2)))
                == 0)
	 && (IR_separator_precedence_identifier_or_literal (p1) == NULL
	     || strcmp (IR_IS_OF_TYPE
			(IR_separator_precedence_identifier_or_literal (p1),
			 IR_NM_identifier)
			? IR_identifier_itself
			  (IR_separator_precedence_identifier_or_literal (p1))
			: IR_character_representation
			  (IR_separator_precedence_identifier_or_literal (p1)),
                        IR_IS_OF_TYPE
			(IR_separator_precedence_identifier_or_literal (p2),
			 IR_NM_identifier)
			? IR_identifier_itself
			  (IR_separator_precedence_identifier_or_literal (p2))
			: IR_character_representation
			  (IR_separator_precedence_identifier_or_literal (p2)))
	        == 0));
    }
  else if (IR_IS_OF_TYPE (p1, IR_NM_control_point))
    return TRUE;
  else if (IR_IS_OF_TYPE (p1, IR_NM_default))
    return pattern_eq_function (IR_default_pattern (p1),
				IR_default_pattern (p2));
  else if (IR_IS_OF_TYPE (p1, IR_NM_star_iteration)
	   || IR_IS_OF_TYPE (p1, IR_NM_plus_iteration))
    return pattern_eq_function (IR_iteration_unit (p1),
				IR_iteration_unit (p2));
  else if (IR_IS_OF_TYPE (p1, IR_NM_code_insertion_atom))
    return p1 == p2;
  else if (IR_IS_OF_TYPE (p1, IR_NM_group))
    return pattern_eq_function (IR_pattern (p1), IR_pattern (p2));
  else if (IR_IS_OF_TYPE (p1, IR_NM_range_atom))
    {
      if ((IR_left_bound (p1) == NULL && IR_left_bound (p2) != NULL)
	  || (IR_left_bound (p1) != NULL && IR_left_bound (p2) == NULL)
	  || (IR_right_bound (p1) == NULL && IR_right_bound (p2) != NULL)
	  || (IR_right_bound (p1) != NULL && IR_right_bound (p2) == NULL))
	return FALSE;
      return
	((IR_left_bound (p1) == NULL
	  || strcmp (IR_character_representation (IR_left_bound (p1)),
		     IR_character_representation (IR_left_bound (p2))) == 0)
	 &&
	 (IR_right_bound (p1) == NULL
	  || strcmp (IR_character_representation (IR_right_bound (p1)),
		     IR_character_representation (IR_right_bound (p2))) == 0));
    }
  else if (IR_IS_OF_TYPE (p1, IR_NM_identifier_or_literal_atom))
    {
      if ((IR_IS_OF_TYPE (IR_identifier_or_literal (p1),
			  IR_NM_identifier)
	   && !IR_IS_OF_TYPE (IR_identifier_or_literal (p2),
			      IR_NM_identifier))
	  || (!IR_IS_OF_TYPE (IR_identifier_or_literal (p1),
			      IR_NM_identifier)
	      && IR_IS_OF_TYPE (IR_identifier_or_literal (p2),
				IR_NM_identifier)))
	return FALSE;
      if (IR_IS_OF_TYPE (IR_identifier_or_literal (p1),
			 IR_NM_identifier))
	return
	  strcmp (IR_identifier_itself (IR_identifier_or_literal (p1)),
		  IR_identifier_itself (IR_identifier_or_literal (p2))) == 0;
      else
	return
	  (strcmp (IR_character_representation (IR_identifier_or_literal (p1)),
		   IR_character_representation (IR_identifier_or_literal (p2)))
	   == 0);
    }
  else if (IR_IS_OF_TYPE (p1, IR_NM_string_atom))
    return strcmp (IR_string_itself (IR_string (p1)),
		   IR_string_itself (IR_string (p2)));
  else
    assert (FALSE);
  return TRUE;
}

/* The pattern table itself is represented by the following variable. */

static hash_table_t pattern_table;

static void
initiate_pattern_table (void)
{
  pattern_table = create_hash_table (500, pattern_hash_function,
				     pattern_eq_function);
}

static IR_node_t
insert_pattern (IR_node_t pattern)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (pattern_table, pattern, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) pattern;
  return (IR_node_t) *entry_ptr;
}

static void
finish_pattern_table (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Pattern table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (pattern_table),
       hash_table_elements_number (pattern_table),
       hash_table_collisions (pattern_table));
#endif
  delete_hash_table (pattern_table);
}



static int current_number_of_impicit_nonterminals;

static IR_node_t
create_implicit_nonterminal (IR_node_t pattern_or_sequence_element,
			     int *new_pattern_flag)
{
  IR_node_t implicit_single_nonterm_definition;
  IR_node_t single_definition_in_table;
  IR_node_t implicit_nonterm_identifier;
  IR_node_t pattern_in_table;
  char *identifier_itself;
  char string [20];

  assert (pattern_or_sequence_element != NULL);
  pattern_in_table = insert_pattern (pattern_or_sequence_element);
  *new_pattern_flag = FALSE;
  if (pattern_equiv_flag && pattern_in_table != pattern_or_sequence_element)
    {
      implicit_single_nonterm_definition
	= IR_corresponding_single_nonterm_definition (pattern_in_table);
      IR_set_duplicated_patterns_number
	(description, IR_duplicated_patterns_number (description) + 1);
    }
  else
    {
      *new_pattern_flag = TRUE;
      current_number_of_impicit_nonterminals++;
      sprintf (string, "$%d", current_number_of_impicit_nonterminals);
      IR_TOP_ADD_STRING (string);
      identifier_itself = insert_string (IR_TOP_BEGIN ());
      if (identifier_itself == (char *) IR_TOP_BEGIN ())
	IR_TOP_FINISH ();
      else
	IR_TOP_NULLIFY ();
      implicit_nonterm_identifier
	= IR_new_identifier (IR_position (pattern_or_sequence_element), 
			     identifier_itself, FALSE);
      implicit_single_nonterm_definition
	= IR_new_single_nonterm_definition
	  (IR_position (pattern_or_sequence_element),
	   implicit_nonterm_identifier, NULL, pattern_or_sequence_element);
      single_definition_in_table
	= insert_single_definition (implicit_single_nonterm_definition);
      assert (implicit_single_nonterm_definition
	      == single_definition_in_table);
      add_single_definition_to_cyclic_list
	(implicit_single_nonterm_definition);
      IR_set_corresponding_single_nonterm_definition
	(pattern_in_table,
	 implicit_single_nonterm_definition);
    }
  return implicit_single_nonterm_definition;
}

static IR_node_t
create_canonical_rule (IR_node_t left_hand_side_nonterminal,
                       IR_node_t original_code_insertion_place)
{
  IR_node_t current_canonical_rule;
  IR_node_t last_canonical_rule;

  current_canonical_rule
    = IR_new_canonical_rule (IR_position (left_hand_side_nonterminal),
                             left_hand_side_nonterminal, NULL, NULL,
                             original_code_insertion_place, NULL);
  last_canonical_rule = IR_canonical_rule_list (description);
  if (last_canonical_rule == NULL)
    /* Make cyclic list */
    IR_set_next_canonical_rule (current_canonical_rule,
                                current_canonical_rule);
  else
    {
      IR_set_next_canonical_rule
        (current_canonical_rule, IR_next_canonical_rule (last_canonical_rule));
      IR_set_next_canonical_rule (last_canonical_rule, current_canonical_rule);
    }
  IR_set_canonical_rule_list (description, current_canonical_rule);
  last_canonical_rule
    = IR_nonterm_canonical_rule_list (left_hand_side_nonterminal);
  if (last_canonical_rule == NULL)
    /* Make cycle. */
    IR_set_next_nonterm_canonical_rule (current_canonical_rule,
                                        current_canonical_rule);
  else
    {
      IR_set_next_nonterm_canonical_rule
        (current_canonical_rule,
         IR_next_nonterm_canonical_rule (last_canonical_rule));
      IR_set_next_nonterm_canonical_rule (last_canonical_rule,
                                          current_canonical_rule);
    }
  IR_set_nonterm_canonical_rule_list (left_hand_side_nonterminal,
                                      current_canonical_rule);
  return current_canonical_rule;
}

static IR_node_t
copy_right_hand_side_element_list (IR_node_t original_right_hand_side_element,
                                   IR_node_t new_canonical_rule)
{
  IR_node_t current_right_hand_side_element;
  IR_node_t previous_right_hand_side_element;
  IR_node_t first_right_hand_side_element;

  for (first_right_hand_side_element = previous_right_hand_side_element = NULL;
       original_right_hand_side_element != NULL;
       original_right_hand_side_element
       = IR_next_right_hand_side_element (original_right_hand_side_element))
    {
      current_right_hand_side_element
        = IR_copy_node (original_right_hand_side_element);
      IR_set_canonical_rule (current_right_hand_side_element,
                             new_canonical_rule);
      if (previous_right_hand_side_element != NULL)
        IR_set_next_right_hand_side_element (previous_right_hand_side_element,
                                             current_right_hand_side_element);
      else
        first_right_hand_side_element = current_right_hand_side_element;
      previous_right_hand_side_element = current_right_hand_side_element;
    }
  return first_right_hand_side_element;
}

static void
add_right_hand_side_element (IR_node_t current_canonical_rule,
                             position_t element_position,
                             IR_node_t element_identifier,
                             IR_node_t element_itself,
                             IR_node_t *last_right_hand_side_element)
{
  IR_node_t current_right_hand_side_element;

  if (current_canonical_rule == NULL)
    return;
  current_right_hand_side_element
    = IR_new_canonical_rule_element
      (element_position, current_canonical_rule, NULL, element_itself,
       element_identifier, IR_single_definition_usage_list (element_itself));
  IR_set_single_definition_usage_list (element_itself,
                                       current_right_hand_side_element);
  if (*last_right_hand_side_element == NULL)
    {
      if (current_canonical_rule != NULL)
        IR_set_right_hand_side (current_canonical_rule,
                                current_right_hand_side_element);
    }
  else
    IR_set_next_right_hand_side_element (*last_right_hand_side_element,
                                         current_right_hand_side_element);
  *last_right_hand_side_element = current_right_hand_side_element; 
}
                             
static void
  process_pattern (IR_node_t pattern, IR_node_t left_hand_side_nonterminal);

static int
literal_code_value (IR_node_t literal, int *explicit_code_flag)
{
  IR_node_t single_definition;

  assert (IR_IS_OF_TYPE (literal, IR_NM_literal));
  single_definition = find_single_definition (literal);
  *explicit_code_flag = FALSE;
  if (single_definition != NULL && IR_value (single_definition) >= 0)
    {
      *explicit_code_flag = TRUE;
      return IR_value (single_definition);
    }
  else
    return IR_literal_code (literal);
}

static void
process_sequence_element (IR_node_t sequence_element,
                          IR_node_t current_canonical_rule,
                          IR_node_t *current_last_right_hand_side_element_ptr)
{
  IR_node_t sequence_element_identifier;
  IR_node_t canonical_rule;
  IR_node_t implicit_nonterminal;
  IR_node_t last_right_hand_side_element;
  int new_pattern_flag;

  sequence_element_identifier
    = IR_sequence_element_identifier (sequence_element);
  if (sequence_element_identifier != NULL)
    {
      if (yacc_input_flag)
        error (FALSE, IR_position (sequence_element),
               "construction `... @ <identifier>' is absent in Posix YACC");
      if (!IR_IS_OF_TYPE (sequence_element_identifier, IR_NM_identifier)
          || IR_dot_presence_flag (sequence_element_identifier))
        {
          error (FALSE, IR_position (sequence_element_identifier),
                 "%s after `@' must have syntax of C identifier",
                 identifier_or_literal_representation
                 (sequence_element_identifier));
          sequence_element_identifier = NULL;
        }
    }
  if (IR_IS_OF_TYPE (sequence_element, IR_NM_group))
    {
      if (yacc_input_flag)
        error (FALSE, IR_position (sequence_element),
               "construction `(...)' is absent in Posix YACC");
      implicit_nonterminal = create_implicit_nonterminal (sequence_element,
							  &new_pattern_flag);
      add_right_hand_side_element
        (current_canonical_rule, IR_position (sequence_element),
         sequence_element_identifier, implicit_nonterminal,
         current_last_right_hand_side_element_ptr);
      if (new_pattern_flag)
	process_pattern (IR_pattern (sequence_element), implicit_nonterminal);
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_control_point))
    {
      if (yacc_input_flag)
        error (FALSE, IR_position (sequence_element),
               "construction `%cp' is absent in Posix YACC");
      if (*current_last_right_hand_side_element_ptr == NULL)
        IR_set_next_cp_flag (current_canonical_rule, TRUE);
      else
        IR_set_next_cp_flag (*current_last_right_hand_side_element_ptr, TRUE);
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_default))
    {
      if (yacc_input_flag)
        error (FALSE, IR_position (sequence_element),
               "construction `[...]' is absent in Posix YACC");
      implicit_nonterminal = create_implicit_nonterminal (sequence_element,
							  &new_pattern_flag);
      add_right_hand_side_element
        (current_canonical_rule, IR_position (sequence_element),
         sequence_element_identifier, implicit_nonterminal,
         current_last_right_hand_side_element_ptr);
      if (new_pattern_flag)
	{
	  process_pattern (IR_default_pattern (sequence_element),
			   implicit_nonterminal);
	  /* Rule with empty right hand side. */
	  (void) create_canonical_rule (implicit_nonterminal, NULL);
	}
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_star_iteration)
           || IR_IS_OF_TYPE (sequence_element, IR_NM_plus_iteration))
    {
      IR_node_t unit;

      if (yacc_input_flag)
        error (FALSE, IR_position (sequence_element),
               "iteration `+' or `*' is absent in Posix YACC");
      implicit_nonterminal = create_implicit_nonterminal (sequence_element,
							  &new_pattern_flag);
      add_right_hand_side_element
        (current_canonical_rule, IR_position (sequence_element),
         sequence_element_identifier, implicit_nonterminal,
         current_last_right_hand_side_element_ptr);
      if (new_pattern_flag)
	{
	  /* Create rule: Implicit : Implicit Unit */
	  canonical_rule = create_canonical_rule (implicit_nonterminal, NULL);
	  last_right_hand_side_element = NULL;
	  add_right_hand_side_element
	    (canonical_rule, IR_position (implicit_nonterminal),
	     NULL, implicit_nonterminal, &last_right_hand_side_element);
	  process_sequence_element
	    (IR_iteration_unit (sequence_element),
	     canonical_rule, &last_right_hand_side_element);
	  unit = IR_next_right_hand_side_element (IR_right_hand_side
						  (canonical_rule));
	  /* Create rule with empty right hand side. */
	  canonical_rule = create_canonical_rule (implicit_nonterminal, NULL);
	  if (IR_IS_OF_TYPE (sequence_element, IR_NM_plus_iteration))
	    /* Create rule: Implicit : Unit */
	    IR_set_right_hand_side
	      (canonical_rule,
	       copy_right_hand_side_element_list (unit, canonical_rule));
	}
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_range_atom))
    {
      int error_flag = FALSE;
      
      if (yacc_input_flag)
        {
          if (IR_IS_OF_TYPE (sequence_element,
                             IR_NM_range_no_left_right_bounds_atom))
            error (FALSE, IR_position (sequence_element),
                   "range `<->' is absent in Posix YACC");
          else if (IR_IS_OF_TYPE (sequence_element,
                                  IR_NM_range_no_left_bound_atom))
            error (FALSE, IR_position (sequence_element),
                   "range `<-' is absent in Posix YACC");
          else if (IR_IS_OF_TYPE (sequence_element,
                                  IR_NM_range_no_right_bound_atom))
            error (FALSE, IR_position (sequence_element),
                   "range `->' is absent in Posix YACC");
          else
            error (FALSE, IR_position (sequence_element),
                   "range `-' is absent in Posix YACC");
          error_flag = TRUE;
        }
      else
        {
          if (IR_IS_OF_TYPE (IR_left_bound (sequence_element),
                             IR_NM_identifier))
            {
              error (FALSE, IR_position (IR_left_bound (sequence_element)),
                     "left range bound `%s' must be literal",
                     IR_identifier_itself (IR_left_bound (sequence_element)));
              error_flag = TRUE;
            }
          if (IR_IS_OF_TYPE (IR_right_bound (sequence_element),
                             IR_NM_identifier))
            {
              error (FALSE, IR_position (IR_right_bound (sequence_element)),
                     "right range bound `%s' must be literal",
                     IR_identifier_itself (IR_right_bound (sequence_element)));
              error_flag = TRUE;
            }
        }
      if (!error_flag)
        {
          int start_code;
          int finish_code;
          int start_explicit_code_flag;
          int finish_explicit_code_flag;
          IR_node_t literal;

          start_code = literal_code_value (IR_left_bound (sequence_element),
                                           &start_explicit_code_flag);
          finish_code = literal_code_value (IR_right_bound (sequence_element),
                                            &finish_explicit_code_flag);
          if (!w_flag && start_explicit_code_flag != finish_explicit_code_flag)
            warning
              (IR_position (sequence_element),
               "warning: one literal of range has explicit code, another not");
          if (IR_IS_OF_TYPE (sequence_element,
                             IR_NM_range_no_left_right_bounds_atom))
            {
              start_code++;
              finish_code--;
              start_explicit_code_flag = TRUE;
              finish_explicit_code_flag = TRUE;
              literal = create_literal (start_code,
                                        IR_position (sequence_element),
                                        TRUE);
            }
          else if (IR_IS_OF_TYPE (sequence_element,
                                  IR_NM_range_no_left_bound_atom))
            {
              start_code++;
              start_explicit_code_flag = TRUE;
              literal = IR_right_bound (sequence_element);
            }
          else if (IR_IS_OF_TYPE (sequence_element,
                                  IR_NM_range_no_right_bound_atom))
            {
              finish_code--;
              finish_explicit_code_flag = TRUE;
              literal = IR_left_bound (sequence_element);
            }
          else
            literal = IR_left_bound (sequence_element);
          if (start_code > finish_code)
            error (FALSE, IR_position (sequence_element), "empty range");
          else
            {
              implicit_nonterminal
                = create_implicit_nonterminal (sequence_element,
					       &new_pattern_flag);
              add_right_hand_side_element
                (current_canonical_rule, IR_position (sequence_element),
                 sequence_element_identifier, implicit_nonterminal,
                 current_last_right_hand_side_element_ptr);
	      if (new_pattern_flag)
		{
		  canonical_rule
		    = create_canonical_rule (implicit_nonterminal, NULL);
		  last_right_hand_side_element = NULL;
		  add_right_hand_side_element
		    (canonical_rule, IR_position (sequence_element), NULL,
		     (start_code == finish_code
		      ? insert_implicit_definition (literal)
		      : insert_literal_range_definition
		        (start_code, finish_code, sequence_element,
			 start_explicit_code_flag
			 || finish_explicit_code_flag)),
		     &last_right_hand_side_element);
		}
            }
        }
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_identifier_or_literal_atom))
    {
      add_right_hand_side_element
        (current_canonical_rule, IR_position (sequence_element),
         sequence_element_identifier,
         insert_implicit_definition
         (IR_identifier_or_literal (sequence_element)),
         current_last_right_hand_side_element_ptr);
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_string_atom))
    {
      IR_node_t string;
      const char *current_character_ptr;
      
      string = IR_string (sequence_element);
      if (yacc_input_flag)
        error (FALSE, IR_position (sequence_element),
               "string is absent in Posix YACC");
      else if (*IR_string_itself (string) == '\0')
        error (FALSE, IR_position (string), "empty string");
      for (current_character_ptr = IR_string_itself (string);
           *current_character_ptr != '\0'; current_character_ptr++)
        add_right_hand_side_element
          (current_canonical_rule, IR_position (string),
           sequence_element_identifier,
           implicit_literal_definition (*current_character_ptr,
                                        IR_position (string)),
           current_last_right_hand_side_element_ptr);
    }
  else if (IR_IS_OF_TYPE (sequence_element, IR_NM_code_insertion_atom))
    {
      if (IR_next_sequence_element (sequence_element) == NULL)
        IR_set_action (current_canonical_rule,
                       IR_code_insertion (sequence_element));
      else
        {
          implicit_nonterminal
            = create_implicit_nonterminal (sequence_element,
					   &new_pattern_flag);
          add_right_hand_side_element
            (current_canonical_rule, IR_position (sequence_element),
             sequence_element_identifier, implicit_nonterminal,
             current_last_right_hand_side_element_ptr);
	  if (new_pattern_flag)
	    {
	      /* Rule with empty right hand side. */
	      canonical_rule
		= create_canonical_rule
		(implicit_nonterminal,
		 *current_last_right_hand_side_element_ptr);
	      IR_set_action (canonical_rule,
			     IR_code_insertion (sequence_element));
	    }
        }
    }
  else
    assert (FALSE);
}

static IR_node_t
process_sequence (IR_node_t first_sequence_element, IR_node_t canonical_rule,
                  IR_node_t *last_right_hand_side_element_ptr)
{
  IR_node_t current_sequence_element;
  IR_node_t previous_right_hand_side_element;

  previous_right_hand_side_element = *last_right_hand_side_element_ptr;
  for (current_sequence_element = first_sequence_element;
       current_sequence_element != NULL;
       current_sequence_element
       = IR_next_sequence_element (current_sequence_element))
    process_sequence_element (current_sequence_element, canonical_rule,
                              last_right_hand_side_element_ptr);
  return (previous_right_hand_side_element == *last_right_hand_side_element_ptr
          ? NULL : *last_right_hand_side_element_ptr);
}

static IR_node_t
check_precedence (IR_node_t precedence_identifier_or_literal)
{
  IR_node_t single_definition_in_table;
  IR_node_t single_precedence_term_definition;

  single_precedence_term_definition = NULL;
  if (precedence_identifier_or_literal != NULL)
    {
      single_definition_in_table
        = find_single_definition (precedence_identifier_or_literal);
      if (single_definition_in_table != NULL
          && IR_IS_OF_TYPE (single_definition_in_table,
                            IR_NM_single_nonterm_definition))
        {
          error (FALSE,
                 IR_position (precedence_identifier_or_literal),
                 "`%s' after %%prec is defined as nonterminal",
                 IR_identifier_itself (IR_identifier_or_literal
                                       (single_definition_in_table)));
          append_message (IR_position (single_definition_in_table),
                          "here previous definition");
        }
      else if (single_definition_in_table == NULL)
        {
          if (IR_IS_OF_TYPE (precedence_identifier_or_literal,
                             IR_NM_identifier))
            error (FALSE,
                   IR_position (precedence_identifier_or_literal),
                   "`%s' after %%prec is not defined as token",
                   IR_identifier_itself (IR_identifier_or_literal
                                         (single_definition_in_table)));
          else
            {
              single_precedence_term_definition
                = insert_implicit_definition
                  (precedence_identifier_or_literal);
              assert (IR_IS_OF_TYPE (precedence_identifier_or_literal,
                                     IR_NM_literal));
            }
        }
      else
        {
          assert (IR_IS_OF_TYPE (single_definition_in_table,
                                 IR_NM_single_term_definition));
          single_precedence_term_definition = single_definition_in_table;
        }
    }
  return single_precedence_term_definition;
}

static void
process_pattern (IR_node_t pattern, IR_node_t left_hand_side_nonterminal)
{
  IR_node_t current_alternative;
  IR_node_t current_canonical_rule;
  IR_node_t implicit_nonterminal;
  IR_node_t implicit_nonterminal_right_hand_side_element;
  IR_node_t last_right_hand_side_element;
  int new_pattern_flag;

  for (current_alternative = IR_alternatives_list (pattern);
       current_alternative != NULL;
       current_alternative = IR_next_alternative (current_alternative))
    {
      if (left_hand_side_nonterminal != NULL)
        current_canonical_rule
          = create_canonical_rule (left_hand_side_nonterminal, NULL);
      else
        current_canonical_rule = NULL;
      if (IR_IS_OF_TYPE (current_alternative, IR_NM_separator_iteration))
        {
          IR_node_t last_separator_element;

          if (yacc_input_flag)
            error (FALSE, IR_position (current_alternative),
                   "iteration with `/' is absent in Posix YACC");
          implicit_nonterminal
            = create_implicit_nonterminal (current_alternative,
					   &new_pattern_flag);
          last_right_hand_side_element = NULL;
          add_right_hand_side_element
            (current_canonical_rule, IR_position (current_alternative),
             NULL, implicit_nonterminal, &last_right_hand_side_element);
	  if (new_pattern_flag)
	    {
	      current_canonical_rule
		= create_canonical_rule (implicit_nonterminal, NULL);
	      last_right_hand_side_element = NULL;
	      add_right_hand_side_element
		(current_canonical_rule, IR_position (implicit_nonterminal),
		 NULL, implicit_nonterminal, &last_right_hand_side_element);
	      implicit_nonterminal_right_hand_side_element
		= last_right_hand_side_element;
	      last_separator_element
		= process_sequence (IR_separator_sequence
				    (current_alternative),
				    current_canonical_rule,
				    &last_right_hand_side_element);
	      (void) process_sequence (IR_iteration_sequence
				       (current_alternative),
				       current_canonical_rule,
				       &last_right_hand_side_element);
	      IR_set_precedence_single_term_definition
		(current_canonical_rule,
		 check_precedence
		 (IR_separator_precedence_identifier_or_literal
		  (current_alternative)));
	      if (IR_separator_max_look_ahead_number (current_alternative)
		  != NULL)
		IR_set_max_look_ahead_value
		  (current_canonical_rule,
		   IR_number_value (IR_separator_max_look_ahead_number
				    (current_alternative)));
	      current_canonical_rule
		= create_canonical_rule (implicit_nonterminal, NULL);
	      IR_set_precedence_single_term_definition
		(current_canonical_rule,
		 check_precedence
		 (IR_iteration_precedence_identifier_or_literal
		  (current_alternative)));
	      if (IR_iteration_max_look_ahead_number (current_alternative)
		  != NULL)
		IR_set_max_look_ahead_value
		  (current_canonical_rule,
		   IR_number_value (IR_iteration_max_look_ahead_number
				    (current_alternative)));
	      IR_set_right_hand_side
		(current_canonical_rule,
		 copy_right_hand_side_element_list
		 (IR_next_right_hand_side_element
		  ((last_separator_element != NULL
		    ? last_separator_element
		    : implicit_nonterminal_right_hand_side_element)),
		  current_canonical_rule));
	    }
	}
      else
        {
          assert (IR_IS_OF_TYPE (current_alternative, IR_NM_sequence));
          last_right_hand_side_element = NULL;
          (void) process_sequence (IR_sequence (current_alternative),
                                   current_canonical_rule,
                                   &last_right_hand_side_element);
          IR_set_precedence_single_term_definition
            (current_canonical_rule,
             check_precedence (IR_precedence_identifier_or_literal
                               (current_alternative)));
	  if (IR_max_look_ahead_number (current_alternative) != NULL)
	    IR_set_max_look_ahead_value
	      (current_canonical_rule,
	       IR_number_value (IR_max_look_ahead_number
				(current_alternative)));
        }
    }
}

static void
process_rule_list (void)
{
  IR_node_t current_rule;
  IR_node_t single_definition;

  initiate_pattern_table ();
  for (current_rule = IR_rule_list (description); current_rule != NULL;
       current_rule = IR_next_rule (current_rule))
    {
      single_definition
        = insert_implicit_definition (IR_nonterm_identifier (current_rule));
      if (IR_IS_OF_TYPE (single_definition, IR_NM_single_term_definition))
        {
          error (FALSE, IR_position (IR_nonterm_identifier (current_rule)),
                 "nonterminal `%s' is declared as token early",
                 IR_identifier_itself (IR_identifier_or_literal
                                       (single_definition)));
          if (compare_positions (IR_position (single_definition),
                                 no_position) != 0)
            append_message (IR_position (single_definition),
                            "here definition of token `%s'",
                            IR_identifier_itself (IR_identifier_or_literal
                                                  (single_definition)));
          single_definition = NULL;
        }
      else
	process_pattern (IR_pattern (current_rule), single_definition);
    }
  finish_pattern_table ();
}

static void
transform_cyclic_definition_list (void)
{
  IR_node_t last_single_definition;

  if (IR_single_definition_list (description) != NULL)
    {
      last_single_definition = IR_single_definition_list (description);
      IR_set_single_definition_list
        (description, IR_next_single_definition (last_single_definition));
      IR_set_next_single_definition (last_single_definition, NULL);
    }
}

static void
transform_cyclic_canonical_rule_list (void)
{
  IR_node_t current_single_definition;
  IR_node_t last_canonical_rule;

  if (IR_canonical_rule_list (description) != NULL)
    {
      last_canonical_rule = IR_canonical_rule_list (description);
      IR_set_canonical_rule_list
        (description, IR_next_canonical_rule (last_canonical_rule));
      IR_set_next_canonical_rule (last_canonical_rule, NULL);
    }
  /* Transform cyclic list of rule lists of nonterminals. */
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition)
        && IR_nonterm_canonical_rule_list (current_single_definition) != NULL)
      {
        last_canonical_rule
          = IR_nonterm_canonical_rule_list (current_single_definition);
        IR_set_nonterm_canonical_rule_list
          (current_single_definition,
           IR_next_nonterm_canonical_rule (last_canonical_rule));
        IR_set_next_nonterm_canonical_rule (last_canonical_rule, NULL);
      }
}

static void
check_canonical_rule_element_identifiers_difference (void)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_right_hand_side_element;
  IR_node_t next_right_hand_side_element;

  for (current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    for (current_right_hand_side_element
         = IR_right_hand_side (current_canonical_rule);
         current_right_hand_side_element != NULL;
         current_right_hand_side_element
         = IR_next_right_hand_side_element (current_right_hand_side_element))
      if (IR_element_identifier (current_right_hand_side_element) != NULL)
        for (next_right_hand_side_element
             = IR_next_right_hand_side_element
             (current_right_hand_side_element);
             next_right_hand_side_element != NULL;
             next_right_hand_side_element
             = IR_next_right_hand_side_element (next_right_hand_side_element))
          if (IR_element_identifier (next_right_hand_side_element) != NULL
              && strcmp (IR_identifier_itself (IR_element_identifier
                                               (next_right_hand_side_element)),
                         IR_identifier_itself
                         (IR_element_identifier
                          (current_right_hand_side_element))) == 0)
            {
              error (FALSE,
                     IR_position (IR_element_identifier
                                  (next_right_hand_side_element)),
                     "rule element identifier `%s' is not unique",
                     IR_identifier_itself (IR_element_identifier
                                           (next_right_hand_side_element)));
              append_message (IR_position (IR_element_identifier
                                           (current_right_hand_side_element)),
                              "here previous identifier definition");
            }
}

static int current_attribute_occurrence;

static void
process_nonterm_single_definition_usage_predecessors
  (IR_node_t nonterm_single_definition, int attribute_number,
   position_t attribute_position,
   const char *tag_name, const char *attribute_name)
{
  IR_node_t current_usage;
  IR_node_t canonical_rule;
  IR_node_t current_right_hand_side_element;
  IR_node_t single_definition;
  int predecessor_number;
  int corresponding_element_number;

  assert (attribute_number <= 0);
  if (IR_pass_number (nonterm_single_definition)
      == current_attribute_occurrence)
    return;
  IR_set_pass_number (nonterm_single_definition, current_attribute_occurrence);
  for (current_usage
       = IR_single_definition_usage_list (nonterm_single_definition);
       current_usage != NULL;
       current_usage = IR_next_single_definition_usage (current_usage))
    {
      canonical_rule = IR_canonical_rule (current_usage);
      predecessor_number = 0;
      for (current_right_hand_side_element
           = IR_right_hand_side (canonical_rule);
           current_right_hand_side_element != current_usage;
           current_right_hand_side_element
           = IR_next_right_hand_side_element (current_right_hand_side_element))
        predecessor_number++;
      corresponding_element_number = attribute_number;
      if (predecessor_number != 0)
        corresponding_element_number += predecessor_number;
      if (corresponding_element_number <= 0)
        {
          if (IR_left_hand_side (canonical_rule)
              != IR_axiom_definition (description))
            process_nonterm_single_definition_usage_predecessors
              (IR_left_hand_side (canonical_rule),
               corresponding_element_number,
               attribute_position, tag_name, attribute_name);
          else if (!w_flag)
            warning
              (attribute_position,
               "warning: attribute `$%s' can refer below paser stack bottom",
               attribute_name);
        }
      else
        {
          for (current_right_hand_side_element
               = IR_right_hand_side (canonical_rule);
               corresponding_element_number != 1;
               current_right_hand_side_element
               = IR_next_right_hand_side_element
                 (current_right_hand_side_element))
            corresponding_element_number--;
          single_definition
            = IR_element_itself (current_right_hand_side_element);
          if (!w_flag && tag_name != NULL && *tag_name != '\0'
              && (IR_IS_OF_TYPE (single_definition,
                                 IR_NM_single_term_definition)
                  || IR_corresponding_pattern (single_definition) == NULL)
              && (IR_type (single_definition) == NULL
                  || strcmp (tag_name,
                             IR_identifier_itself
                             (IR_type (single_definition))) != 0))
            {
              assert (!IR_IS_OF_TYPE (single_definition,
                                      IR_NM_literal_range_definition));
              warning (attribute_position,
                       "warning: attribute `$%s' of %s has not type `%s'",
                       attribute_name,
                       identifier_or_literal_representation
                       (IR_identifier_or_literal (single_definition)),
                       tag_name);
            }
        }
    }
}

static void
dummy_processing_char (char ch)
{
}

static void
check_attribute (IR_node_t canonical_rule, position_t attribute_position,
                 const char *tag_name, const char *attribute_name)
{
  IR_node_t current_right_hand_side_element;
  IR_node_t single_definition;
  int attribute_number;
  IR_node_t original_canonical_rule;
  IR_node_t bound_right_hand_side_element;

  current_attribute_occurrence++;
  single_definition = NULL;
  bound_right_hand_side_element
    = IR_original_code_insertion_place (canonical_rule);
  if (bound_right_hand_side_element != NULL)
    original_canonical_rule
      = IR_canonical_rule (bound_right_hand_side_element);
  else
    original_canonical_rule = canonical_rule;
  if (isdigit (*attribute_name) || *attribute_name == '-')
    {
      attribute_number = atoi (attribute_name);
      if (attribute_number > 0)
        {
          for (current_right_hand_side_element
               = IR_right_hand_side (original_canonical_rule);
               current_right_hand_side_element != bound_right_hand_side_element
               && attribute_number != 1;
               current_right_hand_side_element
               = IR_next_right_hand_side_element
                 (current_right_hand_side_element))
            attribute_number--;
          if (current_right_hand_side_element == bound_right_hand_side_element)
            {
              if (bound_right_hand_side_element == NULL)
                error (FALSE, attribute_position,
                       "`$%s' beyond the end of rule", attribute_name);
              else
                error (FALSE, attribute_position,
                       "`$%s' beyond the code insertion", attribute_name);
            }
          else
            single_definition
              = IR_element_itself (current_right_hand_side_element);
        }
      else
        process_nonterm_single_definition_usage_predecessors
          (IR_left_hand_side (original_canonical_rule),
           attribute_number, attribute_position, tag_name, attribute_name);
    }
  else if (strcmp (attribute_name, "$") == 0)
    single_definition = IR_left_hand_side (canonical_rule);
  else
    {
      for (current_right_hand_side_element
           = IR_right_hand_side (original_canonical_rule);
           current_right_hand_side_element != bound_right_hand_side_element;
           current_right_hand_side_element
           = IR_next_right_hand_side_element (current_right_hand_side_element))
        if (IR_element_identifier (current_right_hand_side_element) != NULL
            && strcmp (attribute_name,
                       IR_identifier_itself
                       (IR_element_identifier
                        (current_right_hand_side_element))) == 0)
          break;
      if (current_right_hand_side_element == bound_right_hand_side_element)
        error (FALSE, attribute_position,
               "can not find attribute `$%s' in the rule", attribute_name);
      else
        single_definition
          = IR_element_itself (current_right_hand_side_element);
    }
  if (tag_name != NULL && *tag_name != '\0'
      && single_definition != NULL
      && (IR_IS_OF_TYPE (single_definition, IR_NM_single_term_definition)
          || IR_corresponding_pattern (single_definition) == NULL)
      && (IR_type (single_definition) == NULL
          || strcmp (tag_name,
                     IR_identifier_itself (IR_type (single_definition))) != 0))
    {
      assert (!IR_IS_OF_TYPE (single_definition,
                              IR_NM_literal_range_definition));
      if (strict_flag)
        error (FALSE, attribute_position,
               "attribute `$%s' of %s has not type `%s'", attribute_name,
               identifier_or_literal_representation (IR_identifier_or_literal
                                                     (single_definition)),
               tag_name);
      else if (!w_flag)
        warning
          (attribute_position,
           "warning: attribute `$%s' of %s has not type `%s'", attribute_name,
           identifier_or_literal_representation (IR_identifier_or_literal
                                                 (single_definition)),
           tag_name);
    }
}

static void
check_attributes_in_actions (void)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_single_definition;

  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition))
      IR_set_pass_number (current_single_definition, 0); 
  current_attribute_occurrence = 0;
  for (current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    process_canonical_rule_action
      (current_canonical_rule, dummy_processing_char, check_attribute);
}

static void
change_token_value_table_entries (IR_node_t old_definition,
                                  IR_node_t new_definition)
{
  int current_value;
  int left_value;
  int right_value;
  
  left_value = IR_value (new_definition);
  if (IR_IS_OF_TYPE (new_definition, IR_NM_literal_range_definition))
    right_value = IR_right_range_bound_value (new_definition);
  else
    right_value = left_value;
  assert (left_value <= right_value);
  for (current_value = left_value;
       current_value <= right_value;
       current_value++)
    if (find_token_code_value (current_value) == old_definition)
      insert_token_code_value_with_overwriting (new_definition, current_value);
}

static void
insert_in_definition_list (IR_node_t inserted_definition,
                           IR_node_t previous_definition)
{
  assert (previous_definition != NULL);
  IR_set_next_single_definition
    (inserted_definition, IR_next_single_definition (previous_definition));
  IR_set_next_single_definition (previous_definition, inserted_definition);
}

static IR_node_t
literal_range_or_term_definition
 (int left_range_value, int right_range_value,
  IR_node_t original_literal_range_definition, int *new_definition_flag)
{
  IR_node_t result;
  IR_node_t literal_definition;
  IR_node_t right_literal_definition;

  assert (IR_IS_OF_TYPE (original_literal_range_definition,
                         IR_NM_literal_range_definition)
          && IR_type (original_literal_range_definition) == NULL
          && IR_priority (original_literal_range_definition) == -1
          && !IR_left_assoc_flag (original_literal_range_definition)
          && !IR_right_assoc_flag (original_literal_range_definition)
          && !IR_nonassoc_flag (original_literal_range_definition));
  assert (left_range_value <= right_range_value);
  *new_definition_flag = FALSE;
  if (left_range_value == right_range_value)
    {
      result = find_literal_definition (left_range_value, right_range_value);
      if (result == NULL)
        {
          *new_definition_flag = TRUE;
          literal_definition
            = create_literal (left_range_value,
                              IR_position (original_literal_range_definition),
                              !IR_bounds_have_explicit_values
                              (original_literal_range_definition));
          result
            = IR_new_single_term_definition
              (IR_position (original_literal_range_definition),
               literal_definition, NULL);
          IR_set_value (result, left_range_value);
#ifdef NDEBUG
          (void) insert_single_definition (result);
#else
          assert (result == insert_single_definition (result));
#endif
          (void) insert_literal_definition (result);
        }
    }
  else
    {
      result = find_literal_definition (left_range_value, right_range_value);
      if (result == NULL)
        {
          *new_definition_flag = TRUE;
          literal_definition
            = create_literal
              (left_range_value,
               IR_position (original_literal_range_definition),
               !IR_bounds_have_explicit_values
               (original_literal_range_definition));
          right_literal_definition
            = create_literal
              (right_range_value,
               IR_position (original_literal_range_definition),
               !IR_bounds_have_explicit_values
               (original_literal_range_definition));
          result
            = IR_new_literal_range_definition
              (IR_position (original_literal_range_definition),
               literal_definition, NULL, right_literal_definition);
          IR_set_value (result, left_range_value);
          IR_set_right_range_bound_value (result, right_range_value);
          IR_set_bounds_have_explicit_values
            (result,
             IR_bounds_have_explicit_values
             (original_literal_range_definition));
          (void) insert_literal_definition (result);
        }
    }
  /* For make_unintersected_literal_range_definitions_partition. */
  IR_set_deletion_flag (result, FALSE);
  return result;
}

static IR_node_t
create_and_insert_canonical_rule (IR_node_t left_hand_side_nonterminal,
                                  IR_node_t original_code_insertion_place,
                                  IR_node_t previous_canonical_rule)
{
  IR_node_t current_canonical_rule;

  assert (left_hand_side_nonterminal
          == IR_left_hand_side (previous_canonical_rule));
  current_canonical_rule
    = IR_new_canonical_rule (IR_position (left_hand_side_nonterminal),
                             left_hand_side_nonterminal, NULL, NULL,
                             original_code_insertion_place,
                             IR_next_canonical_rule (previous_canonical_rule));
  IR_set_next_canonical_rule (previous_canonical_rule, current_canonical_rule);
  IR_set_next_nonterm_canonical_rule
    (current_canonical_rule,
     IR_next_nonterm_canonical_rule (previous_canonical_rule));
  IR_set_next_nonterm_canonical_rule (previous_canonical_rule,
                                      current_canonical_rule);
  return current_canonical_rule;
}

static void
change_definition_in_rules (IR_node_t single_definition,
                            IR_node_t first_part_definition,
                            IR_node_t second_part_definition)
{
  IR_node_t current_single_definition_usage;
  IR_node_t next_single_definition_usage;
  IR_node_t last_right_hand_side_element;
  IR_node_t canonical_rule;

  assert (single_definition != NULL
          && IR_IS_OF_TYPE (single_definition, IR_NM_single_term_definition)
          && first_part_definition != NULL
          && IR_IS_OF_TYPE (first_part_definition,
                            IR_NM_single_term_definition)
          && (second_part_definition == NULL
              || IR_IS_OF_TYPE (second_part_definition,
                                IR_NM_single_term_definition)));
  assert (single_definition != first_part_definition
          || second_part_definition == NULL);
  if (single_definition != first_part_definition)
    {
      for (current_single_definition_usage
           = IR_single_definition_usage_list (single_definition);
           current_single_definition_usage != NULL;
           current_single_definition_usage = next_single_definition_usage)
        {
          next_single_definition_usage
            = IR_next_single_definition_usage
              (current_single_definition_usage);
          IR_set_element_itself (current_single_definition_usage,
                                 first_part_definition);
          IR_set_next_single_definition_usage
            (current_single_definition_usage,
             IR_single_definition_usage_list (first_part_definition));
          IR_set_single_definition_usage_list
            (first_part_definition, current_single_definition_usage);
          assert (IR_element_identifier (current_single_definition_usage)
                  == NULL);
          if (second_part_definition != NULL)
            {
              last_right_hand_side_element = NULL;
              canonical_rule
                = create_and_insert_canonical_rule
                  (IR_left_hand_side (IR_canonical_rule
                                      (current_single_definition_usage)),
                   IR_original_code_insertion_place
                   (IR_canonical_rule (current_single_definition_usage)),
                   IR_canonical_rule (current_single_definition_usage));
              add_right_hand_side_element
                (canonical_rule,
                 IR_position (IR_right_hand_side
                              (IR_canonical_rule
                               (current_single_definition_usage))),
                 NULL, second_part_definition, &last_right_hand_side_element);
#ifndef NDEBUG
              if (debug_level > 1)
                {
                  fprintf (stderr, "addition of %s : ",
                           single_definition_representation
                           (IR_left_hand_side (canonical_rule)));
                  fprintf (stderr, "%s\n",
                           single_definition_representation
                           (second_part_definition));
                }
#endif
            }
        }
      IR_set_deletion_flag (single_definition, TRUE);
      IR_set_single_definition_usage_list (single_definition, NULL);
      change_token_value_table_entries (single_definition,
                                        first_part_definition);
      if (second_part_definition != NULL)
        change_token_value_table_entries (single_definition,
                                          second_part_definition);
    }
}

static void
make_unintersected_literal_range_definitions_partition (void)
{
  IR_node_t current_single_definition;
  IR_node_t next_single_definition;
  IR_node_t previous_single_definition;
  IR_node_t single_definition_in_table;
  IR_node_t first_part_definition;
  IR_node_t second_part_definition;
  int current_range_value;
  int left_range_value;
  int right_range_value;
  int left_table_range_value;
  int right_table_range_value;
  int new_definition_flag;
  int new_definition_flag_1;
  int change_flag;

  do
    {
#ifndef NDEBUG
      if (debug_level >= 1)
        fprintf (stderr, "new literal spitting iteration\n");
#endif
      change_flag = FALSE;
      for (current_single_definition = IR_single_definition_list (description);
           current_single_definition != NULL;
           current_single_definition = next_single_definition)
        {
          next_single_definition
            = IR_next_single_definition (current_single_definition);
          if (IR_IS_OF_TYPE (current_single_definition,
                             IR_NM_single_term_definition)
              && IR_IS_OF_TYPE (IR_identifier_or_literal
                                (current_single_definition), IR_NM_literal)
              && !IR_deletion_flag (current_single_definition))
            {
#ifndef NDEBUG
              if (debug_level > 1)
                fprintf (stderr, "current definition %s\n",
                         single_definition_representation
                         (current_single_definition));
#endif
              left_range_value = IR_value (current_single_definition);
              if (IR_IS_OF_TYPE (current_single_definition,
                                 IR_NM_literal_range_definition))
                right_range_value
                  = IR_right_range_bound_value (current_single_definition);
              else
                right_range_value = left_range_value;
              assert (right_range_value >= 0 && left_range_value >= 0);
              for (current_range_value = left_range_value;
                   current_range_value <= right_range_value;
                   current_range_value++)
                {
                  single_definition_in_table
                    = find_token_code_value (current_range_value);
                  if (single_definition_in_table != current_single_definition
                      && IR_IS_OF_TYPE (IR_identifier_or_literal
                                        (single_definition_in_table),
                                        IR_NM_literal))
                    break;
                }
              if (current_range_value <= right_range_value)
                {
                  change_flag = TRUE;
                  /* Intersection. */
                  left_table_range_value
                    = IR_value (single_definition_in_table);
                  if (IR_IS_OF_TYPE (single_definition_in_table,
                                     IR_NM_literal_range_definition))
                    right_table_range_value
                      = IR_right_range_bound_value
                        (single_definition_in_table);
                  else
                    right_table_range_value = left_table_range_value;
                  assert (current_range_value >= left_table_range_value);
#ifndef NDEBUG
                  if (debug_level > 1)
                    {
                      fprintf (stderr, "intersection of %s with ",
                               single_definition_representation
                               (current_single_definition));
                      fprintf (stderr, "%s\n",
                               single_definition_representation
                               (single_definition_in_table));
                    }
#endif
                  if (left_table_range_value < left_range_value)
                    {
                      /*
                        |--------------....    : single_definition_in_table
                          |--------------....    : current_single_definition
                        */
                      assert (current_range_value == left_range_value);
                      first_part_definition
                        = literal_range_or_term_definition
                          (left_table_range_value, left_range_value - 1,
                           single_definition_in_table, &new_definition_flag);
                      if (new_definition_flag)
                        insert_in_definition_list (first_part_definition,
                                                   single_definition_in_table);
                      second_part_definition
                        = literal_range_or_term_definition
                          (left_range_value, right_table_range_value,
                           single_definition_in_table, &new_definition_flag);
                      if (new_definition_flag)
                        insert_in_definition_list
                          (second_part_definition, single_definition_in_table);
                      change_definition_in_rules (single_definition_in_table,
                                                  first_part_definition,
                                                  second_part_definition);
#ifndef NDEBUG
                      if (debug_level > 1)
                        {
                          fprintf (stderr, "  new %s and ",
                                   single_definition_representation
                                   (first_part_definition));
                          fprintf (stderr, "%s\n",
                                   single_definition_representation
                                   (second_part_definition));
                        }
#endif
                      next_single_definition = current_single_definition;
                    }
                  else if (left_range_value < left_table_range_value)
                    {
                      /*
                           |--------------....  : single_definition_in_table
                        |--------------....      : current_single_definition
                        */
                      first_part_definition
                        = literal_range_or_term_definition
                          (left_range_value, left_table_range_value - 1,
                           current_single_definition, &new_definition_flag_1);
                      if (new_definition_flag_1)
                        {
                          insert_in_definition_list
                            (first_part_definition, current_single_definition);
                          next_single_definition = first_part_definition;
                        }
                      second_part_definition
                        = literal_range_or_term_definition
                          (left_table_range_value, right_range_value,
                           current_single_definition, &new_definition_flag);
                      if (new_definition_flag)
                        {
                          if (!new_definition_flag_1)
                            {
                              insert_in_definition_list
                                (second_part_definition,
                                 current_single_definition);
                              next_single_definition = second_part_definition;
                            }
                          else
                            insert_in_definition_list (second_part_definition,
                                                       first_part_definition);
                        }
#ifndef NDEBUG
                      if (debug_level > 1)
                        {
                          fprintf (stderr, "  new %s and ",
                                   single_definition_representation
                                   (first_part_definition));
                          fprintf (stderr, "%s\n",
                                   single_definition_representation
                                   (second_part_definition));
                        }
#endif
                      change_definition_in_rules (current_single_definition,
                                                  first_part_definition,
                                                  second_part_definition);
                    }
                  else if (right_table_range_value > right_range_value)
                    {
                      /*
                        |----------------------|  : single_definition_in_table
                        |-----------------|       : current_single_definition
                        */
                      first_part_definition
                        = literal_range_or_term_definition
                          (left_table_range_value, right_range_value,
                           single_definition_in_table, &new_definition_flag);
                      if (new_definition_flag)
                        insert_in_definition_list (first_part_definition,
                                                   single_definition_in_table);
                      second_part_definition
                        = literal_range_or_term_definition
                          (right_range_value + 1, right_table_range_value,
                           single_definition_in_table, &new_definition_flag);
                      if (new_definition_flag)
                        insert_in_definition_list
                          (second_part_definition, single_definition_in_table);
#ifndef NDEBUG
                      if (debug_level > 1)
                        {
                          fprintf (stderr, "  new %s and ",
                                   single_definition_representation
                                   (first_part_definition));
                          fprintf (stderr, "%s\n",
                                   single_definition_representation
                                   (second_part_definition));
                        }
#endif
                      change_definition_in_rules (single_definition_in_table,
                                                  first_part_definition,
                                                  second_part_definition);
                      change_definition_in_rules (current_single_definition,
                                                  first_part_definition, NULL);
                    }
                  else
                    {
                      /*
                        |-------------------|     : single_definition_in_table
                        |----------------------|  : current_single_definition
                        */
                      assert (right_table_range_value < right_range_value);
                      first_part_definition
                        = literal_range_or_term_definition
                          (right_table_range_value + 1, right_range_value,
                           current_single_definition, &new_definition_flag);
                      if (new_definition_flag)
                        {
                          insert_in_definition_list
                            (first_part_definition, current_single_definition);
                          next_single_definition = first_part_definition;
                        }
#ifndef NDEBUG
                      if (debug_level > 1)
                        {
                          fprintf (stderr, "  new %s and ",
                                   single_definition_representation
                                   (single_definition_in_table));
                          fprintf (stderr, "%s\n",
                                   single_definition_representation
                                   (first_part_definition));
                        }
#endif
                      change_definition_in_rules (current_single_definition,
                                                  single_definition_in_table,
                                                  first_part_definition);
                    }
                }
            }
        }
    }
  while (change_flag);
  /* Remove deleted literal range definitions. */
  for (previous_single_definition = NULL,
       current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition = next_single_definition)
    {
      next_single_definition
        = IR_next_single_definition (current_single_definition);
      if (IR_IS_OF_TYPE (current_single_definition,
                         IR_NM_single_term_definition)
          && IR_deletion_flag (current_single_definition))
        {
          if (previous_single_definition == NULL)
            IR_set_single_definition_list (description,
                                           next_single_definition);
          else
            IR_set_next_single_definition (previous_single_definition,
                                           next_single_definition);
        }
      else
        previous_single_definition = current_single_definition;
    }
}

static void
check_duplicated_token_values (void)
{
  IR_node_t current_single_definition;
  IR_node_t single_definition_in_table;
  int current_range_value;
  int left_range_value;
  int right_range_value;

  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition, IR_NM_single_term_definition)
        && IR_value (current_single_definition) >= 0)
      {
        left_range_value = IR_value (current_single_definition);
        if (IR_IS_OF_TYPE (current_single_definition,
                           IR_NM_literal_range_definition))
          right_range_value
            = IR_right_range_bound_value (current_single_definition);
        else
          right_range_value = left_range_value;
        for (current_range_value = left_range_value;
             current_range_value <= right_range_value;
             current_range_value++)
          {
            single_definition_in_table
              = insert_token_code_value (current_single_definition,
                                         current_range_value);
            if (!w_flag
                && single_definition_in_table != current_single_definition)
              {
                /* Remember that `no_position' (token `error') may be
                   here. */
                warning (IR_position (current_single_definition),
                         "warning: tokens %s has the duplicated number",
                         single_definition_representation
                         (current_single_definition));
                if (compare_positions
                    (IR_position (single_definition_in_table),
                     no_position) != 0)
                  append_message
                    (IR_position (single_definition_in_table),
                     "here definition of %s with the duplicated number",
                     single_definition_representation
                     (single_definition_in_table));
                else
                  {
                    assert (!IR_IS_OF_TYPE (single_definition_in_table,
                                            IR_NM_literal_range_definition));
                    append_message
                      (no_position,
                       "the first definition with this number is %s",
                       identifier_or_literal_representation
                       (IR_identifier_or_literal
                        (single_definition_in_table)));
                  }
              }
          }
      }
}

static void
process_single_term_definitions (void)
{
  IR_node_t current_single_definition;
  int current_token_value;
  int current_range_value;
  int left_range_value;
  int right_range_value;
  int explicit_code_flag;

  /* First, insert values into token value table only for literals.
     It is necessary for correct work of function
     `make_unintersected_literal_range_definitions_partition'. */
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition, IR_NM_single_term_definition)
        && IR_IS_OF_TYPE (IR_identifier_or_literal
                          (current_single_definition), IR_NM_literal)
        && IR_value (current_single_definition) >= 0)
      {
        left_range_value = IR_value (current_single_definition);
        if (IR_IS_OF_TYPE (current_single_definition,
                           IR_NM_literal_range_definition))
          {
            right_range_value
              = IR_right_range_bound_value (current_single_definition);
            if (!IR_scanner_flag (description)
                && (left_range_value == 0 || right_range_value == 0))
              error (FALSE, IR_position (current_single_definition),
                     "range contains zero value");
          }
        else
          {
            right_range_value = left_range_value;
            if (left_range_value == 0 && !IR_scanner_flag (description))
              error (FALSE, IR_position (current_single_definition),
                     "null character %s",
                     IR_character_representation
                     (IR_identifier_or_literal (current_single_definition)));
          }
        for (current_range_value = left_range_value;
             current_range_value <= right_range_value;
             current_range_value++)
          (void) insert_token_code_value (current_single_definition,
                                          current_range_value);
      }
  /* Second, insert values into token value table only for token
     identifier. */
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition, IR_NM_single_term_definition)
        && IR_IS_OF_TYPE (IR_identifier_or_literal
                          (current_single_definition), IR_NM_identifier)
        && IR_value (current_single_definition) >= 0)
      (void) insert_token_code_value (current_single_definition,
                                      IR_value (current_single_definition));
  current_token_value = START_TOKEN_VALUE;
  /* Third, set up and insert default values into token value table
     only for other token identifiers or literals. */
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition, IR_NM_single_term_definition)
        && IR_value (current_single_definition) < 0)
      {
        assert (!IR_IS_OF_TYPE (current_single_definition,
                                IR_NM_literal_range_definition));
        /* Set default value. */
        if (IR_IS_OF_TYPE (IR_identifier_or_literal
                           (current_single_definition), IR_NM_literal))
          {
            left_range_value
              = literal_code_value (IR_identifier_or_literal
                                    (current_single_definition),
                                    &explicit_code_flag);
            assert (!explicit_code_flag);
            IR_set_value (current_single_definition, left_range_value);
            (void) insert_token_code_value (current_single_definition,
                                            left_range_value);
            if (IR_IS_OF_TYPE (IR_identifier_or_literal
                               (current_single_definition), IR_NM_literal))
              (void) insert_literal_definition (current_single_definition);
            if (left_range_value == 0 && !IR_scanner_flag (description))
              error (FALSE, IR_position (current_single_definition),
                     "null character %s",
                     IR_character_representation
                     (IR_identifier_or_literal (current_single_definition)));
          }
        else
          {
            assert (!IR_IS_OF_TYPE (current_single_definition,
                                    IR_NM_literal_range_definition));
            while (find_token_code_value (current_token_value) != NULL)
              current_token_value++;
            IR_set_value (current_single_definition, current_token_value);
            (void) insert_token_code_value (current_single_definition,
                                            current_token_value);
          }
      }
  make_unintersected_literal_range_definitions_partition ();
  check_duplicated_token_values ();
}

static void
process_axiom_identifier (void)
{
  IR_node_t single_definition;

  if (IR_axiom_identifier (description) == NULL
      && IR_rule_list (description) != NULL)
    IR_set_axiom_identifier
      (description, IR_nonterm_identifier (IR_rule_list (description)));
  if (IR_axiom_identifier (description) != NULL)
    {
      single_definition =
        find_single_definition (IR_axiom_identifier (description));
      if (single_definition == NULL)
        error (FALSE, IR_position (IR_axiom_identifier (description)),
               "start nonterminal `%s' is not used",
               IR_identifier_itself (IR_axiom_identifier (description)));
      else if (IR_IS_OF_TYPE (single_definition, IR_NM_single_term_definition))
        {
          error (FALSE, IR_position (IR_axiom_identifier (description)),
                 "start nonterminal `%s' is defined as token",
                 IR_identifier_itself (IR_axiom_identifier (description)));
          if (compare_positions (IR_position (single_definition),
                                 no_position) == 0)
            append_message (IR_position (single_definition),
                            "here definition of token `%s'",
                            IR_identifier_itself (IR_axiom_identifier
                                                  (description)));
        }
      else
        IR_set_axiom_definition (description, single_definition);
    }
}

static void
enumerate_single_definitions (void)
{
  IR_node_t current_single_definition;

  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_term_definition))
      {
        IR_set_token_order_number (current_single_definition,
                                   IR_tokens_number (description));
        IR_set_tokens_number (description,
                              IR_tokens_number (description) + 1);
      }
    else
      {
        assert (IR_IS_OF_TYPE (current_single_definition,
                               IR_NM_single_nonterm_definition));
        IR_set_nonterm_order_number (current_single_definition,
                                     IR_nonterminals_number (description));
        IR_set_nonterminals_number (description,
                                    IR_nonterminals_number (description) + 1);
      }
}

static void
enumerate_canonical_rules (void)
{
  IR_node_t current_canonical_rule;
  int current_number;

  for (current_number = 1,
       current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_number++,
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    {
      IR_set_canonical_rule_order_number (current_canonical_rule,
                                          current_number);
      IR_set_canonical_rules_number (description, current_number);
    }
}



static void
set_accessibility (IR_node_t single_definition)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_right_hand_side_element;

  if (IR_accessibility_flag (single_definition))
    return;
  IR_set_accessibility_flag (single_definition, TRUE);
  if (IR_IS_OF_TYPE (single_definition, IR_NM_single_nonterm_definition))
    for (current_canonical_rule
         = IR_nonterm_canonical_rule_list (single_definition);
         current_canonical_rule != NULL;
         current_canonical_rule
         = IR_next_nonterm_canonical_rule (current_canonical_rule))
      for (current_right_hand_side_element
           = IR_right_hand_side (current_canonical_rule);
           current_right_hand_side_element != NULL;
           current_right_hand_side_element
           = IR_next_right_hand_side_element (current_right_hand_side_element))
        set_accessibility (IR_element_itself
                           (current_right_hand_side_element));
}

static void
check_nonterminals_accessibility (void)
{
  IR_node_t current_single_definition;
  int implicit_nonterminal_error_flag;
  int explicit_nonterminal_error_flag;

  assert (IR_IS_OF_TYPE (IR_axiom_definition (description),
                         IR_NM_single_nonterm_definition));
  set_accessibility (IR_axiom_definition (description));
  explicit_nonterminal_error_flag = FALSE;
  implicit_nonterminal_error_flag = FALSE;
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (!IR_accessibility_flag (current_single_definition))
      {
        if (IR_IS_OF_TYPE (current_single_definition,
                           IR_NM_single_nonterm_definition))
          {
            if (IR_corresponding_pattern (current_single_definition) == NULL)
              {
                error (FALSE, IR_position (current_single_definition),
                       "nonterminal `%s' is not accessible from axiom `%s'",
                       IR_identifier_itself (IR_identifier_or_literal
                                             (current_single_definition)),
                       IR_identifier_itself (IR_axiom_identifier
                                             (description)));
                explicit_nonterminal_error_flag = TRUE;
              }
            else
              implicit_nonterminal_error_flag = TRUE;
          }
        else
          {
            assert (IR_IS_OF_TYPE (current_single_definition,
                                   IR_NM_single_term_definition));
            if (!w_flag
                && (!IR_IS_OF_TYPE (IR_identifier_or_literal
                                    (current_single_definition),
                                    IR_NM_identifier)
                    || strcmp (IR_identifier_itself
                               (IR_identifier_or_literal
                                (current_single_definition)),
                               "error") != 0))
              warning (IR_position (current_single_definition),
                       "warning: token %s is not accessible from axiom `%s'",
                       single_definition_representation
                       (current_single_definition),
                       IR_identifier_itself (IR_axiom_identifier
                                             (description)));
          }
      }
  assert (!implicit_nonterminal_error_flag || explicit_nonterminal_error_flag);
}



static void
set_canonical_rule_priorities (void)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_right_hand_side_element;
  IR_node_t current_single_definition;
  IR_node_t priority_single_term_definition;

  for (current_canonical_rule = IR_canonical_rule_list (description);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_canonical_rule (current_canonical_rule))
    {
      priority_single_term_definition
        = IR_precedence_single_term_definition (current_canonical_rule);
      if (priority_single_term_definition == NULL)
        for (current_right_hand_side_element
             = IR_right_hand_side (current_canonical_rule);
             current_right_hand_side_element != NULL;
             current_right_hand_side_element
             = IR_next_right_hand_side_element
               (current_right_hand_side_element))
          {
            current_single_definition
              = IR_element_itself (current_right_hand_side_element);
            if (IR_IS_OF_TYPE (current_single_definition,
                               IR_NM_single_term_definition))
              priority_single_term_definition = current_single_definition;
          }
      if (priority_single_term_definition != NULL)
        IR_set_rule_priority (current_canonical_rule,
                              IR_priority (priority_single_term_definition));
    }
}



/* The following variable value is number of the pass to evaluate
   derivation ability of a nonterminal. */

static int derivation_ability_pass_number;

/* Remember that the derivation ability of a nonterminal may be fully
   evaluated on the previous passes. */

static void
set_nonterm_derivation_ability (IR_node_t single_nonterm_definition)
{
  IR_node_t current_canonical_rule;
  IR_node_t current_right_hand_side_element;
  IR_node_t current_single_definition;
  int rule_right_hand_derive_ability_flag;

  if (IR_pass_number (single_nonterm_definition)
      == derivation_ability_pass_number
      || IR_derivation_ability_flag (single_nonterm_definition))
    return;
  IR_set_pass_number (single_nonterm_definition,
                      derivation_ability_pass_number);
  for (current_canonical_rule
       = IR_nonterm_canonical_rule_list (single_nonterm_definition);
       current_canonical_rule != NULL;
       current_canonical_rule
       = IR_next_nonterm_canonical_rule (current_canonical_rule))
    {
      rule_right_hand_derive_ability_flag = TRUE;
      for (current_right_hand_side_element
           = IR_right_hand_side (current_canonical_rule);
           current_right_hand_side_element != NULL;
           current_right_hand_side_element
           = IR_next_right_hand_side_element (current_right_hand_side_element))
        {
          current_single_definition
            = IR_element_itself (current_right_hand_side_element);
          if (IR_IS_OF_TYPE (current_single_definition,
                             IR_NM_single_nonterm_definition))
            {
              set_nonterm_derivation_ability (current_single_definition);
              if (rule_right_hand_derive_ability_flag)
                rule_right_hand_derive_ability_flag
                  = IR_derivation_ability_flag (current_single_definition);
            }
        }
      if (!IR_derivation_ability_flag (single_nonterm_definition))
        IR_set_derivation_ability_flag (single_nonterm_definition,
                                        rule_right_hand_derive_ability_flag);
    }
}

static void
check_nonterminals_derivation_ability (void)
{
  IR_node_t current_single_definition;
  int implicit_nonterminal_error_flag;
  int explicit_nonterminal_error_flag;

  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition))
      IR_set_pass_number (current_single_definition, 0); 
  for (derivation_ability_pass_number = 1,
       current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       derivation_ability_pass_number++,
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition))
      set_nonterm_derivation_ability (current_single_definition);
  explicit_nonterminal_error_flag = FALSE;
  implicit_nonterminal_error_flag = FALSE;
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_nonterm_definition)
        && !IR_derivation_ability_flag (current_single_definition))
      {
        if (IR_corresponding_pattern (current_single_definition) == NULL)
          {
            error (FALSE, IR_position (current_single_definition),
                   "nonterminal `%s' does not derive any token string",
                   IR_identifier_itself (IR_identifier_or_literal
                                         (current_single_definition)));
            explicit_nonterminal_error_flag = TRUE;
          }
        else
          implicit_nonterminal_error_flag = TRUE;
      }
  assert (!implicit_nonterminal_error_flag || explicit_nonterminal_error_flag);
}

void
analyze_description (void)
{
  if (description == NULL)
    /* This is possible only after error recovery stopping on EOF. */
    description = IR_new_description (no_position, NULL, NULL, NULL);
  current_number_of_impicit_nonterminals = 0;
  initiate_identifier_or_literal_representations ();
  process_definition_list ();
  process_rule_list ();
  /* The following function is called only after `process_rule_list'
     because function `process_rule_list' may create implicit
     definitions of nonterminals. */
  transform_cyclic_definition_list ();
  transform_cyclic_canonical_rule_list ();
  check_canonical_rule_element_identifiers_difference ();
  process_single_term_definitions ();
  process_axiom_identifier ();
  enumerate_single_definitions ();
  enumerate_canonical_rules ();
  /* The following function must be called only after function
     `process_axiom_identifier'. */
  check_attributes_in_actions ();
  if (number_of_errors == 0)
    {
      set_canonical_rule_priorities ();
      check_nonterminals_accessibility ();
      check_nonterminals_derivation_ability ();
    }
  finish_identifier_or_literal_representations ();
}
