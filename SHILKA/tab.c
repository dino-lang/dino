/*
   FILE NAME:   tab.c

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool SHILKA.

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

   TITLE:       All tables of Shilka (keywords description translator)

   DESCRIPTION: This file contains abstract data which implement
                the following tables
                      table of strings (and identifiers)
                      table of keywords
                      table of keyword names

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of the tables.
   
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

/* The function evaluates hash value (0..UINT_MAX) of string. */

static unsigned
string_hash_value (const char *string)
{
  unsigned result, i;

  for (result = i = 0;*string++ != '\0'; i++)
    result += ((unsigned char) *string << (i % CHAR_BIT));
  return result;
}

/* The function evaluates hash value (0..UINT_MAX) of string ignoring
   the letters case. */

static unsigned
string_case_hash_value (const char *string)
{
  unsigned result, i;

  for (result = i = 0;*string++ != '\0'; i++)
    result += ((unsigned char) our_tolower (*string) << (i % CHAR_BIT));
  return result;
}

/* The function tests strings on equality (1 if the strings are equal,
   0 otherwise). */

static int
string_equality (const char *string_1, const char *string_2)
{
  return strcmp (string_1, string_2) == 0;
}

/* The function tests strings on equality (1 if the strings are equal,
   0 otherwise) ignoring the letters case. */

static int
string_case_equality (const char *string_1, const char *string_2)
{
  do
    {
      if (our_tolower (*string_1) != our_tolower (*string_2))
        return 0; /* FALSE */
      else
        {
          string_1++;
          string_2++;
        }
    }
  while (*string_1 != '\0' || *string_2 != '\0');
  return 1; /* TRUE */
}



/* This page contains abstract data `tables of strings'.  Elements
   of the table is strings representing strings, identifiers. */

/* The function evaluates hash value of string.  The function is used
   by abstract data `hash-table'.  The function returns hash value
   (0..UINT_MAX) of given string.  */

static unsigned
string_hash_function (hash_table_entry_t string)
{
  return string_hash_value (string);
}

/* The function tests strings on equality.  The function is used by
   abstract data `hash-table'.  the function returns 1 if the
   strings are equal, 0 otherwise. */

static int
string_eq_function (hash_table_entry_t string_1,
                    hash_table_entry_t string_2)
{
  return string_equality (string_1, string_2);
}

/* The string table itself is represented by the following variable. */

static hash_table_t string_table;

/* The function inserts string into the table.  The function does
   nothing if an string with the same key exists already in the table.
   The function returns string in the table with the same key as given
   string. */

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

/* The function creates empty string table.  The function must be
   called only once before any work with the string table.  */

void
initiate_string_table (void)
{
  string_table = create_hash_table (1000, string_hash_function,
                                    string_eq_function);
}

/* The function deletes given string table.  Only call of function
   `initiate_string_table' is possible immediately after this function
   call. */

void
finish_string_table (void)
{
  delete_hash_table (string_table);
}



/* This page contains abstract data `table of keywords'.  Elements of
   the table is nodes representing keywords.  Key of the table
   elements is string of given keyword. */


/* The function evaluates hash value of a keyword.  The function is
   used by abstract data `hash-table'.  The function returns hash
   value (0..UINT_MAX) of given keyword.  */

static unsigned
keyword_hash_function (hash_table_entry_t keyword)
{
  assert (keyword != NULL);
  if (case_flag)
    return string_case_hash_value (keyword_string ((IR_node_t) keyword));
  else
    return string_hash_value (keyword_string ((IR_node_t) keyword));
}

/* The function tests keywords on equality of their keys.  The
   function is used by abstract data `hash-table'.  The function
   returns 1 if the keywords have the same key, 0 otherwise.  */

static int
keyword_eq_function (hash_table_entry_t keyword_1,
                     hash_table_entry_t keyword_2)
{
  const char *string_1;
  const char *string_2;

  assert (keyword_1 != NULL && keyword_2 != NULL);
  string_1 = keyword_string ((IR_node_t) keyword_1);
  string_2 = keyword_string ((IR_node_t) keyword_2);
  assert (string_1 != NULL && string_2 != NULL);
  if (case_flag)
    return string_case_equality (string_1, string_2);
  else
    return string_equality (string_1, string_2);
}

/* The keyword table itself is represented by the following
   variable. */

static hash_table_t keyword_table;

/* The function inserts keyword into the table.  The function does
   nothing if a keyword with the same key exists already in the table.
   The function returns keyword node in the table with the same key as
   given keyword node. */

IR_node_t
insert_keyword (IR_node_t keyword)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (keyword_table, keyword, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) keyword;
  return (IR_node_t) *entry_ptr;
}

/* The function creates empty the table.  The function must be called
   only once before any work with the keyword table.  */

void
initiate_keyword_table (void)
{
  keyword_table = create_hash_table (1000, keyword_hash_function,
                                     keyword_eq_function);
}

/* The function deletes the keyword table.  Only call of function
   `initiate_keyword_table' is possible immediately after this
   function call. */

void
finish_keyword_table (void)
{
  delete_hash_table (keyword_table);
}



/* This page contains abstract data `table of keyword names'.  Elements of
   the table is nodes representing keywords.  Key of the table
   elements is name of given keyword. */


/* The function evaluates hash value of a keyword.  The function is
   used by abstract data `hash-table'.  The function returns hash
   value (0..UINT_MAX) of given keyword name.  */

static unsigned
keyword_name_hash_function (hash_table_entry_t keyword)
{
  assert (keyword != NULL);
  return string_hash_value (IR_identifier_itself
                            (IR_name ((IR_node_t) keyword)));
}

/* The function tests keywords on equality of their keys.  The
   function is used by abstract data `hash-table'.  The function
   returns 1 if the keywords have the same key, 0 otherwise.  */

static int
keyword_name_eq_function (hash_table_entry_t keyword_1,
                          hash_table_entry_t keyword_2)
{
  const char *string_1;
  const char *string_2;

  assert (keyword_1 != NULL && keyword_2 != NULL);
  string_1 = IR_identifier_itself (IR_name ((IR_node_t) keyword_1));
  string_2 = IR_identifier_itself (IR_name ((IR_node_t) keyword_2));
  assert (string_1 != NULL && string_2 != NULL);
  return string_equality (string_1, string_2);
}

/* The keyword name table itself is represented by the following
   variable. */

static hash_table_t keyword_name_table;

/* The function inserts keyword into the table.  The function does
   nothing if a keyword with the same key exists already in the table.
   The function returns keyword node in the table with the same key as
   given keyword node. */

IR_node_t
insert_keyword_name (IR_node_t keyword)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (keyword_name_table, keyword, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) keyword;
  return (IR_node_t) *entry_ptr;
}

/* The function creates empty the table.  The function must be called
   only once before any work with the keyword name table.  */

void
initiate_keyword_name_table (void)
{
  keyword_name_table = create_hash_table (1000, keyword_name_hash_function,
                                          keyword_name_eq_function);
}

/* The function deletes the keyword name table.  Only call of function
   `initiate_keyword_name_table' is possible immediately after this
   function call. */

void
finish_keyword_name_table (void)
{
  delete_hash_table (keyword_name_table);
}
