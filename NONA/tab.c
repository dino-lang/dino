/*
   FILE NAME:   tab.c

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool NONA.

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

   TITLE:       All tables of NONA (code selector description translator)

   DESCRIPTION: This file contains abstract data which implement
                the following tables
                      table of identifiers
                      table of single declarations

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of the tables.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* #ifdef HAVE_CONFIG_H */


#include <string.h>
#include "hashtab.h"
#include "common.h"
#include "ird.h"
#include "tab.h"

#include <assert.h>
#include <limits.h>



/* This page contains common functions of some abstract data. */

/* The following function evaluates hash value of string.  The
   function returns hash value (0..UINT_MAX) of given string. */

static unsigned
string_hash_value (const char *string)
{
  unsigned result, i;

  for (result = i = 0;*string++ != '\0'; i++)
    result += ((unsigned char) *string << (i % CHAR_BIT));
  return result;
}

/* The following function tests strings on equality.  The function
   returns 1 if the strings are equal, 0 otherwise. */

static int
string_equality (const char *string_1, const char *string_2)
{
  return strcmp (string_1, string_2) == 0;
}



/* This page contains abstract data `tables of identifiers'.  Elements
   of the table is strings representing identifiers. */

/* The following function evaluates hash value of identifier string.
   The function is used by abstract data `hash table'.  The function
   returns hash value (0..UINT_MAX) of given identifier string. */

static unsigned
identifier_hash_function (hash_table_entry_t identifier)
{
  return string_hash_value (identifier);
}

/* The following function tests identifier strings on equality.  The
   function is used by abstract data `hash table'.  The function
   returns 1 if the identifier strings are equal, 0 otherwise. */

static int
identifier_eq_function (hash_table_entry_t identifier_1,
                        hash_table_entry_t identifier_2)
{
  return string_equality (identifier_1, identifier_2);
}

/* The identifier table itself is represented by the following variable. */

static hash_table_t identifier_table;

/* The following function inserts identifier string into the table.
   The function does nothing if an identifier string with the same key
   exists already in the table.  The function returns identifier
   string in the table with the same key as given identifier. */

char *
insert_identifier (const char *identifier)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (identifier_table, identifier, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) identifier;
  else
    assert (strcmp (identifier, *entry_ptr) == 0);
  return (char *) *entry_ptr;
}

/* The following function creates empty identifier table.  The
   function must be called only once before any work with the
   identifier table. */

void
initiate_identifier_table (void)
{
  identifier_table = create_hash_table (1000, identifier_hash_function,
                                        identifier_eq_function);
}

/* The following function deletes given identifier table.  Only call
   of function `initiate_identifier_table' is possible immediately
   after this function call. */

void
finish_identifier_table (void)
{
  delete_hash_table (identifier_table);
}



/* This page contains abstract data `table of single declarations'
   Elements of the table is nodes representing single declarations of
   terminals and nonterminals.  Key of the table elements is identifier
   strings of given terminal or nonterminal. */

/* The following function evaluates hash value of a single
   declaration.  The function is used by abstract data `hash table'.
   the function returns hash value (0..UINT_MAX) of given single
   declaration. */

static unsigned
single_declaration_hash_function (hash_table_entry_t single_declaration)
{
  assert (IR_identifier ((IR_node_t) single_declaration) != NULL
          && IR_identifier_itself (IR_identifier
                                   ((IR_node_t) single_declaration)) != NULL);
  return string_hash_value (IR_identifier_itself
                            (IR_identifier ((IR_node_t) single_declaration)));
}

/* The following function tests single declarations on equality of
   their keys.  The function is used by abstract data `hash table'.
   The function rewturns 1 if the single declarations have the same
   key, 0 otherwise. */

static int
single_declaration_eq_function (hash_table_entry_t single_declaration_1,
                                hash_table_entry_t single_declaration_2)
{
  assert
    (single_declaration_1 != NULL
     && IR_identifier_itself (IR_identifier
                              ((IR_node_t) single_declaration_1)) != NULL
     && single_declaration_2 != NULL
     && IR_identifier_itself (IR_identifier
                              ((IR_node_t) single_declaration_2)) != NULL);
  return string_equality (IR_identifier_itself
                          (IR_identifier ((IR_node_t) single_declaration_1)),
                          IR_identifier_itself
                          (IR_identifier ((IR_node_t) single_declaration_2)));
}

/* The single declaration table itself is represented by the following
   variable. */

static hash_table_t single_declaration_table;

/* The following function inserts single declaration into the table.
   The function does nothing if a single declaration with the same key
   exists already in the table.  The function returns single
   declaration node in the table with the same key as given single
   declaration node. */

IR_node_t
insert_single_declaration (IR_node_t single_declaration)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (single_declaration_table,
                                     single_declaration, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) single_declaration;
  return (IR_node_t) *entry_ptr;
}

/* The following variable value is node representing single declaration.
   The node used for searching single declaration with given identifier. */

static IR_node_t work_single_declaration;

/* The following function searches for single_declaration in the table
   with the same key as node representing identifier of the single
   declaration.  The function returns node found in the table, NULL if
   such node does not exist in the table. */

IR_node_t
find_single_declaration (IR_node_t identifier)
{
  hash_table_entry_t *entry_ptr;

  IR_set_identifier (work_single_declaration, identifier);
  entry_ptr = find_hash_table_entry (single_declaration_table,
                                     work_single_declaration, FALSE);
  return (IR_node_t) *entry_ptr;
}

/* The following function creates empty single declaration table and
   node representing single declaration and used for searching single
   declaration with given identifier.  The function must be called
   only once before any work with the single declaration table. */

void
initiate_single_declaration_table (void)
{
  work_single_declaration = IR_create_node (IR_NM_single_term_declaration);
  single_declaration_table
    = create_hash_table (10, single_declaration_hash_function,
                         single_declaration_eq_function);
}

/* The following function deletes the single declaration table.  Only
   call of function `initiate_single_declaration_table' is possible
   immediately after this function call. */

void
finish_single_declaration_table (void)
{
  delete_hash_table (single_declaration_table);
}
