/*
   FILE NAME:   tab.c

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool SPRUT.

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

   TITLE:       All tables of SPRUT (internal representation description
                translator)

   DESCRIPTION: This file contains abstract data which implement
                the following tables
                      table of identifiers
                      table of double declaration identifiers
                      table of predefined types and node types
                      table of fields           (key is the field identifier)
                      table of node type fields (key is the field identifier
                                                 and the field node type)

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
#include "ird.h"
#include "common.h"
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
   retruns 1 if the strings are equal, 0 otherwise. */

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



/* This page contains abstract data `table of double declaration
   identifiers'.  Elements of the table is nodes representing
   identifiers in double node type declarations.  Key of the table elements
   is identifier strings of given nodes. */

/* The following function evaluates hash value of double node type
   declaration identifier.  The function is used by abstract data
   `hash table'.  The function returns hash value (0..UINT_MAX) of
   given double node type declaration identifier. */

static unsigned
double_declaration_identifier_hash_function (hash_table_entry_t identifier)
{
  char *str;
  unsigned result, i;

  assert (identifier != NULL
          && IR_identifier_itself ((IR_node_t) identifier) != NULL);
  for (str = IR_identifier_itself ((IR_node_t) identifier), result = i = 0;
       *str++ != '\0'; i++)
    result += ((unsigned char) *str << (i % CHAR_BIT));
  return result;
}

/* The following function tests double node type declaration
   identifiers on equality of their keys.  The function is used by
   abstract data `hash table'.  The function returns 1 if the double
   node type declaration identifiers have the same key, 0
   otherwise. */

static int
double_declaration_identifier_eq_function (hash_table_entry_t identifier_1,
                                           hash_table_entry_t identifier_2)
{
  assert (identifier_1 != NULL
          && IR_identifier_itself ((IR_node_t) identifier_1) != NULL
          && identifier_2 != NULL
          && IR_identifier_itself ((IR_node_t) identifier_2) != NULL);
  return strcmp (IR_identifier_itself ((IR_node_t) identifier_1),
                 IR_identifier_itself ((IR_node_t) identifier_2)) == 0;
}

/* The double declaration identifier table itself is represented by
   the following variable. */

static hash_table_t double_declaration_identifier_table;

/* The following function inserts double node type declaration
   identifier into the table.  The function does nothing if an
   identifier with the same key exists already in the table.  The
   function returns identifier node in the table with the same key as
   given identifier node. */

IR_node_t
insert_double_declaration_identifier (IR_node_t identifier)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (double_declaration_identifier_table,
                                     identifier, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) identifier;
  return (IR_node_t) *entry_ptr;
}

/* The following function searches for double node type declaration
   identifier in the table with the same key as given identifier node.
   The function retruns identifier node in the table with the same key
   as given identifier node, NULL if such identifier node does not
   exist in the table. */

IR_node_t
find_double_declaration_identifier (IR_node_t identifier)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (double_declaration_identifier_table,
                                     identifier, FALSE);
  assert (*entry_ptr == NULL
          || strcmp (IR_identifier_itself (identifier),
                     IR_identifier_itself ((IR_node_t) *entry_ptr)) == 0);
  return (IR_node_t) *entry_ptr;
}

/* The following function creates empty double node type declaration
   identifier table.  The function must be called only once before any
   work with the double node type declaration identifier table. */

void
initiate_double_declaration_identifier_table (void)
{
  double_declaration_identifier_table
    = create_hash_table (10, double_declaration_identifier_hash_function,
                         double_declaration_identifier_eq_function);
}

/* The following function deletes given double node type declaration
   identifier table.  Only call of function
   `initiate_double_declaration_identifier_table' is possible
   immediately after this function call. */

void
finish_double_declaration_identifier_table (void)
{
  delete_hash_table (double_declaration_identifier_table);
}



/* This page contains abstract data `table of types'.  Elements of the
   table is nodes representing node types and predefined types.  Key of
   the table elements is identifier strings of nodes representing identifiers
   of the types. */

/* The following function evaluates hash value of node type or
   predefined type.  The function is used by abstract data
   `hash table'.  The function returns hash value (0..UINT_MAX) of
   given node type or predefined type. */

static unsigned
type_hash_function (hash_table_entry_t type)
{
  IR_node_t node = (IR_node_t) type;

  assert (node != NULL
          && (IR_NODE_MODE (node) == IR_NM_node_type
              || IR_NODE_MODE (node) == IR_NM_predefined_type));
  return (string_hash_value (IR_identifier_itself (IR_type_identifier
                                                   (node))));
}

/* The following function tests node types or/and predefined types on
   equality of their keys.  The function is used by abstract data
   `hash table'.  The function returns 1 if the types have the same
   key, 0 otherwise. */

static int
type_eq_function (hash_table_entry_t type_1, hash_table_entry_t type_2)
{
  IR_node_t node_1 = (IR_node_t) type_1;
  IR_node_t node_2 = (IR_node_t) type_2;

  assert
    (node_1 != NULL && (IR_NODE_MODE (node_1) == IR_NM_node_type
                        || IR_NODE_MODE (node_1) == IR_NM_predefined_type)
     && node_2 != NULL && (IR_NODE_MODE (node_2) == IR_NM_node_type
                           || IR_NODE_MODE (node_2) == IR_NM_predefined_type));
  return (string_equality (IR_identifier_itself (IR_type_identifier (node_1)),
                           IR_identifier_itself (IR_type_identifier
                                                 (node_2))));
}

/* The type table itself is represented by the following variable. */

static hash_table_t type_table;

/* The following function inserts node type or predefined type into
   the table.  The function does nothing if an type with the same key
   exists already in the table.  The function returns node in the
   table with the same key as node representing given node type or
   predefined type. */

IR_node_t
insert_type (IR_node_t type)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (type_table, type, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) type;
  return (IR_node_t) *entry_ptr;
}

/* The following variable value is node representing node type.  The node
   used for searching type with given identifier. */

static IR_node_t work_node_type;

/* The following function searches for node type or/and predefined
   type in the table with the same key as node representing identifier
   of the type.  The function returns node found in the table, NULL if
   such node does not exist in the table. */

IR_node_t
find_type (IR_node_t identifier)
{
  hash_table_entry_t *entry_ptr;

  IR_set_type_identifier (work_node_type, identifier);
  entry_ptr = find_hash_table_entry (type_table, work_node_type, FALSE);
  return (IR_node_t) *entry_ptr;
}

/* The following function creates empty node type or/and predefined
   type table and node representing node type and used for searching
   type with given identifier.  The function must be called only once
   before any work with the type table. */

void
initiate_type_table (void)
{
  work_node_type = IR_create_node (IR_NM_node_type);
  type_table = create_hash_table (200, type_hash_function, type_eq_function);
}

/* The following function deletes given type table.  Only call of
   function `initiate_type_table' is possible immediately after this
   function call. */

void
finish_type_table (void)
{
  delete_hash_table (type_table);
}



/* This page contains abstract data `table of fields'.  Elements of the
   table is nodes representing fields.  Key of the table elements is
   identifier strings of nodes representing identifiers
   of the fields. */

/* The follwoing function evaluates hash value of field.  The function
   is used by abstract data `hash table'.  The function returns hash
   value (0..UINT_MAX) of given field. */

static unsigned
field_hash_function (hash_table_entry_t node_field)
{
  IR_node_t node = (IR_node_t) node_field;

  assert (node != NULL && IR_NODE_MODE (node) == IR_NM_field);
  return (string_hash_value (IR_identifier_itself (IR_field_identifier
                                                   (node))));
}

/* The following function tests fields on equality of their keys.  The
   function is used by abstract data `hash table'.  The function
   returns 1 if the fields have the same key, 0 otherwise. */

static int
field_eq_function (hash_table_entry_t node_field_1,
                   hash_table_entry_t node_field_2)
{
  IR_node_t node_1 = (IR_node_t) node_field_1;
  IR_node_t node_2 = (IR_node_t) node_field_2;

  assert (node_1 != NULL && IR_NODE_MODE (node_1) == IR_NM_field
          && node_2 != NULL && IR_NODE_MODE (node_2) == IR_NM_field);
  return (string_equality (IR_identifier_itself (IR_field_identifier (node_1)),
                           IR_identifier_itself (IR_field_identifier
                                                 (node_2))));
}

/* The field table itself is represented by the following variable. */

static hash_table_t field_table;

/* The following function inserts field into the table.  The function
   does nothing if an field with the same key exists already in the
   table.  The function returns node in the table with the same key as
   node representing given field. */

IR_node_t
insert_field (IR_node_t node_field)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (field_table, node_field, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) node_field;
  return (IR_node_t) *entry_ptr;
}

/* The following variable value is node representing field.  The node
   used for searching field with given identifier. */

static IR_node_t work_field;

/* The following function searches for field in the table with the
   same key as node representing identifier of the field.  The
   function returns node found in the table, NULL if such node does
   not exist in the table. */

IR_node_t
find_field (IR_node_t field_identifier)
{
  hash_table_entry_t *entry_ptr;

  IR_set_field_identifier (work_field, field_identifier);
  entry_ptr = find_hash_table_entry (field_table, work_field, FALSE);
  return (IR_node_t) *entry_ptr;
}

/* The following function creates empty field table and node
   representing field and used for searching field with given
   identifier.  The function must be called only once before any work
   with the field table. */

void
initiate_field_table (void)
{
  work_field = IR_create_node (IR_NM_field);
  field_table = create_hash_table (500, field_hash_function,
                                   field_eq_function);
}

/* The following function deletes given field table.  Only call of
   function `initiate_field_table' is possible immediately after this
   function call. */

void
finish_field_table (void)
{
  delete_hash_table (field_table);
}



/* This page contains abstract data `table of node fields'.  Elements of the
   table is nodes representing fields.  Key of the table elements is
   identifier strings of nodes representing identifier of the field and
   identifier of corresponding node type of the field. */

/* The following function evaluates hash value of a node field.  The
   function is used by abstract data `hash table'.  The function
   returns hash value (0..UINT_MAX) of given field of some node
   type. */

static unsigned
node_field_hash_function (hash_table_entry_t node_field)
{
  IR_node_t node = (IR_node_t) node_field;

  assert (node != NULL && IR_NODE_MODE (node) == IR_NM_field);
  return (string_hash_value (IR_identifier_itself (IR_field_identifier (node)))
          ^
          string_hash_value (IR_identifier_itself (IR_type_identifier
                                                   (IR_node_type (node)))));
}

/* The following function tests fields of some node type on equality
   of their keys.  The function is used by abstract data `hash table'.
   The function returns 1 if the fields of some node type have the
   same key, 0 otherwise. */

static int
node_field_eq_function (hash_table_entry_t node_field_1,
                        hash_table_entry_t node_field_2)
{
  IR_node_t node_1 = (IR_node_t) node_field_1;
  IR_node_t node_2 = (IR_node_t) node_field_2;

  assert (node_1 != NULL && IR_NODE_MODE (node_1) == IR_NM_field
          && node_2 != NULL && IR_NODE_MODE (node_2) == IR_NM_field);
  return (string_equality (IR_identifier_itself (IR_field_identifier (node_1)),
                           IR_identifier_itself (IR_field_identifier (node_2)))
          &&
          string_equality (IR_identifier_itself (IR_type_identifier
                                                 (IR_node_type (node_1))),
                           IR_identifier_itself (IR_type_identifier
                                                 (IR_node_type (node_2)))));
}

/* The node field table itself is represented by the following variable. */

static hash_table_t node_field_table;

/* The following function inserts field of some node type into the
   table.  The function does nothing if an node field with the same
   key exists already in the table.  The function returns node in the
   table with the same key as node representing given field of some
   node type. */

IR_node_t
insert_node_field (IR_node_t node_field)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (node_field_table, node_field, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) node_field;
  return (IR_node_t) *entry_ptr;
}

/* The following variable value is node representing field of some node type.
   The node used for searching field with given identifier in given node
   type. */

static IR_node_t work_node_field;

/* The following function searches for field of some node type in the
   table with the same key as nodes representing identifier of the
   field and corresponding node type.  The function returns node found
   in the table, NULL if such node does not exist in the table. */

IR_node_t
find_node_field (IR_node_t field_identifier, IR_node_t node_type)
{
  hash_table_entry_t *entry_ptr;

  IR_set_field_identifier (work_node_field, field_identifier);
  IR_set_node_type (work_node_field, node_type);
  entry_ptr = find_hash_table_entry (node_field_table, work_node_field, FALSE);
  return (IR_node_t) *entry_ptr;
}

/* The following function creates empty node field table and node
   representing field of some node type and used for searching field
   with given identifier in given node type.  The function must be
   called only once before any work with the node field table. */

void
initiate_node_field_table (void)
{
  work_node_field = IR_create_node (IR_NM_field);
  node_field_table = create_hash_table (500, node_field_hash_function,
                                        node_field_eq_function);
}

/* The following function deletes given node field table.  Only call
   of function `initiate_node_field_table' is possible immediately
   after this function call. */

void
finish_node_field_table (void)
{
  delete_hash_table (node_field_table);
}
