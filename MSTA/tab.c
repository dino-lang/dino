/*
   Copyright (C) 1997-2002 Vladimir Makarov.

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

#include <string.h>
#include "hashtab.h"
#include "common.h"
#include "ird.h"
#include "tab.h"

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



/* This page contains common functions of some abstract data. */

static unsigned
string_hash_value (const char *string)
{
  unsigned result, i;

  for (result = i = 0;*string++ != '\0'; i++)
    result += ((unsigned char) *string << (i % CHAR_BIT));
  return result;
}

static int
string_equality (const char *string_1, const char *string_2)
{
  return strcmp (string_1, string_2) == 0;
}



/* This page contains abstract data `tables of string and
   identifiers'.  Elements of the table is C strings representing
   strings and identifiers. */

static unsigned
string_hash_function (hash_table_entry_t string)
{
  return string_hash_value (string);
}

static int
string_eq_function (hash_table_entry_t string_1, hash_table_entry_t string_2)
{
  return string_equality (string_1, string_2);
}

/* The string table itself is represented by the following variable. */

static hash_table_t string_table;

char *
insert_string (const char *string)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (string_table, string, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) string;
  else
    assert (strcmp (string, *entry_ptr) == 0);
  return (char *) *entry_ptr;
}

void
initiate_string_table (void)
{
  string_table = create_hash_table (1000, string_hash_function,
                                    string_eq_function);
}

void
finish_string_table (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "String table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (string_table),
       hash_table_elements_number (string_table),
       hash_table_collisions (string_table));
#endif
  delete_hash_table (string_table);
}



/* This page contains abstract data `table of single definitions'
   Elements of the table are nodes representing single definitions of
   terminals and nonterminals.  Key of the table elements is
   identifier strings (or code of literal terminal) of given terminal
   or nonterminal. */

static unsigned
single_definition_hash_function (hash_table_entry_t single_definition)
{
  assert
    (IR_identifier_or_literal ((IR_node_t) single_definition) != NULL
     && (!IR_IS_OF_TYPE (IR_identifier_or_literal
                         ((IR_node_t) single_definition),
                         IR_NM_identifier)
         || IR_identifier_itself (IR_identifier_or_literal
                                  ((IR_node_t) single_definition)) != NULL));
  if (IR_IS_OF_TYPE (IR_identifier_or_literal ((IR_node_t) single_definition),
                     IR_NM_identifier))
    return string_hash_value (IR_identifier_itself
                              (IR_identifier_or_literal ((IR_node_t)
                                                         single_definition)));
  else
    return IR_literal_code (IR_identifier_or_literal ((IR_node_t)
                                                      single_definition));
}

static int
single_definition_eq_function (hash_table_entry_t single_definition_1,
                               hash_table_entry_t single_definition_2)
{
  assert
    (single_definition_1 != NULL
     && (!IR_IS_OF_TYPE (IR_identifier_or_literal
                         ((IR_node_t) single_definition_1),
                         IR_NM_identifier)
         || IR_identifier_itself (IR_identifier_or_literal
                                  ((IR_node_t) single_definition_1)) != NULL)
     && single_definition_2 != NULL
     && (!IR_IS_OF_TYPE (IR_identifier_or_literal
                         ((IR_node_t) single_definition_2),
                         IR_NM_identifier)
         || IR_identifier_itself (IR_identifier_or_literal
                                  ((IR_node_t) single_definition_2)) != NULL));
  return 
    (IR_NODE_MODE (IR_identifier_or_literal ((IR_node_t) single_definition_1))
     == IR_NODE_MODE (IR_identifier_or_literal ((IR_node_t)
                                                single_definition_2))
     && (IR_IS_OF_TYPE (IR_identifier_or_literal
                        ((IR_node_t) single_definition_1),
                        IR_NM_identifier)
         ? string_equality (IR_identifier_itself
                            (IR_identifier_or_literal ((IR_node_t)
                                                       single_definition_1)),
                            IR_identifier_itself
                            (IR_identifier_or_literal ((IR_node_t)
                                                       single_definition_2)))
         : (IR_literal_code (IR_identifier_or_literal
                             ((IR_node_t) single_definition_1))
            == IR_literal_code (IR_identifier_or_literal
                                ((IR_node_t) single_definition_2)))));
}

/* The single definition table itself is represented by the following
   variable. */

static hash_table_t single_definition_table;

IR_node_t
insert_single_definition (IR_node_t single_definition)
{
  hash_table_entry_t *entry_ptr;

  assert (!IR_IS_OF_TYPE (single_definition, IR_NM_literal_range_definition));
  entry_ptr = find_hash_table_entry (single_definition_table,
                                     single_definition, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) single_definition;
  return (IR_node_t) *entry_ptr;
}

/* The following variable value is node representing single
   definition.  The node used for searching single definition with
   given identifier or literal (only for terminal). */

static IR_node_t work_single_definition;

IR_node_t
find_single_definition (IR_node_t identifier_or_literal)
{
  hash_table_entry_t *entry_ptr;

  IR_set_identifier_or_literal (work_single_definition, identifier_or_literal);
  entry_ptr = find_hash_table_entry (single_definition_table,
                                     work_single_definition, FALSE);
  return (IR_node_t) *entry_ptr;
}

void
initiate_single_definition_table (void)
{
  work_single_definition = IR_create_node (IR_NM_single_term_definition);
  single_definition_table
    = create_hash_table (1000, single_definition_hash_function,
                         single_definition_eq_function);
}

void
finish_single_definition_table (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Definition table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (single_definition_table),
       hash_table_elements_number (single_definition_table),
       hash_table_collisions (single_definition_table));
#endif
  delete_hash_table (single_definition_table);
}



/* This page contains abstract data `table of literal definitions'.
   Elements of the table are single term definitions denoting a
   literal and literal range definitions.  Key of the table token
   value(s). */

static hash_table_t literal_definition_table;

static unsigned
literal_definition_hash_function (hash_table_entry_t literal_definition)
{
  assert (!IR_IS_OF_TYPE ((IR_node_t) literal_definition,
                          IR_NM_literal_range_definition)
          || (IR_value ((IR_node_t) literal_definition)
              < IR_right_range_bound_value ((IR_node_t) literal_definition)));
  return (IR_value ((IR_node_t) literal_definition)
          * (IR_IS_OF_TYPE ((IR_node_t) literal_definition,
                            IR_NM_literal_range_definition)
             ? IR_right_range_bound_value ((IR_node_t) literal_definition)
             : 1));
}

static int
literal_definition_eq_function (hash_table_entry_t literal_definition_1,
                                hash_table_entry_t literal_definition_2)
{
  assert ((!IR_IS_OF_TYPE ((IR_node_t) literal_definition_1,
                           IR_NM_literal_range_definition)
           || (IR_value ((IR_node_t) literal_definition_1)
               < IR_right_range_bound_value ((IR_node_t)
                                             literal_definition_1)))
          && (!IR_IS_OF_TYPE ((IR_node_t) literal_definition_2,
                              IR_NM_literal_range_definition)
              || (IR_value ((IR_node_t) literal_definition_2)
                  < IR_right_range_bound_value ((IR_node_t)
                                                literal_definition_2))));
  return (IR_NODE_MODE ((IR_node_t) literal_definition_1)
          == IR_NODE_MODE ((IR_node_t) literal_definition_2)
          && (IR_value ((IR_node_t) literal_definition_1)
              == IR_value ((IR_node_t) literal_definition_2))
          && (!IR_IS_OF_TYPE ((IR_node_t) literal_definition_1,
                              IR_NM_literal_range_definition)
              || (IR_right_range_bound_value ((IR_node_t) literal_definition_1)
                  == IR_right_range_bound_value ((IR_node_t)
                                                 literal_definition_2))));
}

IR_node_t
insert_literal_definition (IR_node_t literal_definition)
{
  hash_table_entry_t *entry_ptr;

  assert (IR_IS_OF_TYPE (IR_identifier_or_literal (literal_definition),
                         IR_NM_literal));
  entry_ptr = find_hash_table_entry (literal_definition_table,
                                     literal_definition, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) literal_definition;
  return (IR_node_t) *entry_ptr;
}

static IR_node_t work_single_term_definition;
static IR_node_t work_literal_range_definition;

IR_node_t
find_literal_definition (int left_range_value, int right_range_value)
{
  hash_table_entry_t *entry_ptr;

  assert (left_range_value <= right_range_value);
  if (left_range_value == right_range_value)
    {
      IR_set_value (work_single_term_definition, left_range_value);
      entry_ptr = find_hash_table_entry (literal_definition_table,
                                         work_single_term_definition, FALSE);
    }
  else
    {
      IR_set_value (work_literal_range_definition, left_range_value);
      IR_set_right_range_bound_value (work_literal_range_definition,
                                      right_range_value);
      entry_ptr = find_hash_table_entry (literal_definition_table,
                                         work_literal_range_definition, FALSE);
    }
  if (*entry_ptr == NULL)
    return NULL;
  else
    return (IR_node_t) *entry_ptr;
}

void
initiate_literal_definition_table (void)
{
  work_single_term_definition = IR_create_node (IR_NM_single_term_definition);
  work_literal_range_definition
    = IR_create_node (IR_NM_literal_range_definition);
  literal_definition_table
    = create_hash_table (1000, literal_definition_hash_function,
                         literal_definition_eq_function);
}

void
finish_literal_definition_table (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Literal table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (literal_definition_table),
       hash_table_elements_number (literal_definition_table),
       hash_table_collisions (literal_definition_table));
#endif
  delete_hash_table (literal_definition_table);
}



/* This page contains abstract data `table of values of terminals
   (including literal ranges)'.  Elements of the table is single
   terminal definition and literal range definitions. */

struct token_code_value
{
  /* Key */
  int value;
  /* Different structure can refer for the same teminal definition of
     course if the definition represents a literal range. */
  IR_node_t single_term_definition;
};

typedef struct token_code_value *token_code_value_t;

static os_t token_code_values;
static hash_table_t token_code_value_table;

static unsigned
token_code_value_hash_function (hash_table_entry_t token_code_value)
{
  assert (((token_code_value_t) token_code_value)->value >= 0);
  return ((token_code_value_t) token_code_value)->value;
}

static int
token_code_value_eq_function (hash_table_entry_t token_code_value_1,
                              hash_table_entry_t token_code_value_2)
{
  assert (((token_code_value_t) token_code_value_1)->value >= 0
          && ((token_code_value_t) token_code_value_2)->value >= 0);
  return (((token_code_value_t) token_code_value_1)->value
          == ((token_code_value_t) token_code_value_2)->value);
}

IR_node_t
insert_token_code_value (IR_node_t token_definition, int value)
{
  hash_table_entry_t *entry_ptr;
  token_code_value_t token_code_value;

  assert (value >= 0 && token_definition != NULL);
  token_code_value = OS_TOP_BEGIN (token_code_values);
  token_code_value->value = value;
  token_code_value->single_term_definition = token_definition;
  entry_ptr = find_hash_table_entry (token_code_value_table, token_code_value,
                                     TRUE);
  if (*entry_ptr == NULL)
    {
      *entry_ptr = (hash_table_entry_t) token_code_value;
      OS_TOP_FINISH (token_code_values);
      OS_TOP_EXPAND (token_code_values, sizeof (struct token_code_value)); 
    }
  return ((token_code_value_t) *entry_ptr)->single_term_definition;
}

void
insert_token_code_value_with_overwriting (IR_node_t token_definition,
                                          int value)
{
  hash_table_entry_t *entry_ptr;
  token_code_value_t token_code_value;

  assert (value >= 0 && token_definition != NULL);
  token_code_value = OS_TOP_BEGIN (token_code_values);
  token_code_value->value = value;
  token_code_value->single_term_definition = token_definition;
  entry_ptr = find_hash_table_entry (token_code_value_table, token_code_value,
                                     TRUE);
  *entry_ptr = (hash_table_entry_t) token_code_value;
  OS_TOP_FINISH (token_code_values);
  OS_TOP_EXPAND (token_code_values, sizeof (struct token_code_value)); 
}

IR_node_t
find_token_code_value (int value)
{
  hash_table_entry_t *entry_ptr;
  token_code_value_t token_code_value;

  assert (value >= 0);
  token_code_value = OS_TOP_BEGIN (token_code_values);
  token_code_value->value = value;
  entry_ptr = find_hash_table_entry (token_code_value_table,
                                     token_code_value, FALSE);
  if (*entry_ptr == NULL)
    return NULL;
  else
    return ((token_code_value_t) *entry_ptr)->single_term_definition;
}

void
initiate_token_code_value_table (void)
{
  OS_CREATE (token_code_values, 0);
  OS_TOP_EXPAND (token_code_values, sizeof (struct token_code_value)); 
  token_code_value_table
    = create_hash_table (1000, token_code_value_hash_function,
                         token_code_value_eq_function);
}

void
finish_token_code_value_table (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Token code table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (token_code_value_table),
       hash_table_elements_number (token_code_value_table),
       hash_table_collisions (token_code_value_table));
#endif
  delete_hash_table (token_code_value_table);
  OS_DELETE (token_code_values);
}
