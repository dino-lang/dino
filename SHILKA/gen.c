/*
   FILE NAME:   gen.c

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

   TITLE:       generator of Shilka (keywords description translator)

   DESCRIPTION: This file generates interface and implementation file of
                the keywords recognizer.

   SPECIAL CONSIDERATION:
         The generator is to be called after Shilka semantic analyzer only
       if any error was not fixed.
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of the generator.

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
#include <stdlib.h>
#include <string.h>


#include "hashtab.h"
#include "bits.h"
#include "ticker.h"
#include "vlobject.h"
#include "common.h"
#include "tab.h"
#include "ird.h"
#include "gen.h"

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

/* The following variable values are times of
       building pruned O-trie
       code output
*/

static ticker_t pruned_O_trie_time;
static ticker_t output_time;



/* This page contains functions which are used to output the keywords
   recognizer.  These function are used to fix output errors in time.
   All output of the recognizer is made only through these
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

/* This function outputs string onto description, interface, or
   implementation file and fixes output errors.  The function can
   output in regime when letters are output only in lower case. */

static void
generalized_output_string (FILE *f, const char *string, int tolower_flag)
{
  for (; *string != '\0'; string++)
    {
      if (tolower_flag && fputc (our_tolower (*string), f) == EOF
          || !tolower_flag && fputc (*string, f) == EOF)
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

/* This function outputs string onto interface or implementation file
   and fixes output errors. */

static void
output_string (FILE *f, const char *string)
{
  generalized_output_string (f, string, FALSE);
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

static void
output_octal_number (FILE *f, int number, int minimum_width)
{
  char format [30];

  assert (minimum_width >= 0);
  sprintf (format, "%%%do", minimum_width);
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
  if (interface_flag)
    {
      output_interface_file = fopen (output_interface_file_name, "w");
      if (output_interface_file == NULL)
        system_error (TRUE, no_position,
                      "fatal error -- %s: ", output_interface_file_name);
    }
  output_implementation_file = fopen (output_implementation_file_name, "w");
  if (output_implementation_file == NULL)
    system_error (TRUE, no_position,
                  "fatal error -- %s: ", output_implementation_file_name);
}



static void
output_character_representation (FILE *f, int char_code)
{
  char representation [20];
  char *str;

  if (isprint (char_code) && char_code != '\\' && char_code != '\'')
    {
      representation[0] = '\'';
      representation[1] = char_code;
      representation[2] = '\'';
      representation[3] = '\0';
      str = representation;
    }
  else
    {
      if (char_code == '\n')
        str = "'\\n'";
      else if (char_code == '\t')
        str = "'\\t'";
      else if (char_code == '\v')
        str = "'\\v'";
      else if (char_code == '\b')
        str = "'\\b'";
      else if (char_code == '\r')
        str = "'\\r'";
      else if (char_code == '\f')
        str = "'\\f'";
      else if (char_code == '\\')
        str = "'\\\\'";
      else if (char_code == '\'')
        str = "'\\''";
      else
        {
          sprintf (representation, "'\\%o'", char_code);
          str = representation;
        }
    }
  output_string (f, str);
}



static void
make_the_same_name_keyword_lists (void)
{
  IR_node_t current_keyword;
  IR_node_t keyword_in_table;
  IR_node_t next;

  initiate_keyword_name_table ();
  IR_set_keyword_names_number (description, 0);
  for (current_keyword = IR_keyword_list (description);
       current_keyword != NULL;
       current_keyword = IR_next_keyword (current_keyword))
    {
      keyword_in_table = insert_keyword_name (current_keyword);
      IR_set_first_the_same_name_keyword_flag
        (current_keyword, keyword_in_table == current_keyword);
      if (IR_first_the_same_name_keyword_flag (current_keyword))
        {
          IR_set_keyword_name_index (current_keyword, 0);
          IR_set_next_the_same_name_keyword (current_keyword, NULL);
          IR_set_keyword_names_number
            (description, IR_keyword_names_number (description) + 1);
        }
      else
        {
          next = IR_next_the_same_name_keyword (keyword_in_table);
          IR_set_keyword_name_index
            (current_keyword,
             (next == NULL ? 1 : IR_keyword_name_index (next) + 1));
          IR_set_next_the_same_name_keyword (current_keyword, next);
          IR_set_next_the_same_name_keyword (keyword_in_table,
                                             current_keyword);
        }
    }
  finish_keyword_name_table ();
}



static int
the_same_length_keyword_list_length (IR_node_t list)
{
  int result;
  IR_node_t current_keyword;
  
  result = 0;
  for (current_keyword = list;
       current_keyword != NULL;
       current_keyword = IR_next_the_same_length_keyword (current_keyword))
    result++;
  return result;
}

/* The following function makes partition of all keywords on the sets
   of the same length keywords. */

static void
make_the_same_length_keyword_partition (void)
{
  IR_node_t current_keyword;
  IR_node_t null_value;
  IR_node_t keyword_set;
  int keyword_length;
  int vector_length;
  int i;
  vlo_t the_same_length_keyword_vector;
  IR_node_t list;
  int keywords_number;

  VLO_CREATE (the_same_length_keyword_vector, 0);
  null_value = NULL;
  keywords_number = 0;
  for (current_keyword = IR_keyword_list (description);
       current_keyword != NULL;
       current_keyword = IR_next_keyword (current_keyword))
    {
      keywords_number++;
      keyword_length = strlen (keyword_string (current_keyword));
      vector_length = (VLO_LENGTH (the_same_length_keyword_vector)
                       / sizeof (IR_node_t));
      if (vector_length <= keyword_length)
        {
          /* Expand the vector */
          for (i = 0; i <= keyword_length - vector_length; i++)
            VLO_ADD_MEMORY (the_same_length_keyword_vector, &null_value,
                            sizeof (IR_node_t));
        }
      IR_set_next_the_same_length_keyword
        (current_keyword,
         ((IR_node_t *) VLO_BEGIN (the_same_length_keyword_vector))
         [keyword_length]);
      ((IR_node_t *) VLO_BEGIN (the_same_length_keyword_vector))
        [keyword_length] = current_keyword;
    }
  IR_set_keywords_number (description, keywords_number);
  IR_set_keyword_set_list (description, NULL);
  for (keyword_length = (VLO_LENGTH (the_same_length_keyword_vector)
                         / sizeof (IR_node_t) - 1);
       keyword_length >= 0;
       keyword_length--)
    {
      list = (((IR_node_t *) VLO_BEGIN (the_same_length_keyword_vector))
              [keyword_length]);
      if (list != NULL)
        {
          keyword_set
            = IR_new_keyword_set
              (keyword_length, list,
               the_same_length_keyword_list_length (list),
               IR_keyword_set_list (description));
          IR_set_keyword_set_list (description, keyword_set);
        }
    }
  VLO_DELETE (the_same_length_keyword_vector);
}



/* Abstract data `trie nodes'. */

static IR_node_t first_free_trie_node;

#ifndef NDEBUG
static int allocated_trie_nodes_number = 0;
#endif

static void
free_trie_node (IR_node_t trie_node)
{
  IR_set_next_trie_node (trie_node, first_free_trie_node);
  first_free_trie_node = trie_node;
}

static void
free_trie_list_and_below (IR_node_t trie_node_list)
{
  IR_node_t current_trie_node;
  IR_node_t next_trie_node;

  for (current_trie_node = trie_node_list;
       current_trie_node != NULL;
       current_trie_node = next_trie_node)
    {
      next_trie_node  = IR_next_trie_node (current_trie_node);
      free_trie_list_and_below (IR_trie_node_list (current_trie_node));
      free_trie_node (current_trie_node);
    }
}

static IR_node_t
get_free_trie_node (void)
{
  IR_node_t new_trie_node;

  if (first_free_trie_node == NULL)
    {
#ifndef NDEBUG
      allocated_trie_nodes_number++;
#endif
      new_trie_node = IR_new_trie_node ();
    }
  else
    {
      new_trie_node = first_free_trie_node;
      first_free_trie_node = IR_next_trie_node (first_free_trie_node);
    }
  IR_set_next_trie_node (new_trie_node, NULL);
  return new_trie_node;
}

static void
initiate_trie_nodes (void)
{
  first_free_trie_node = NULL;
}

static void
finish_trie_nodes (void)
{
}



static void
swap_index (int *index_array, int i, int j)
{
  int temp;

  temp = index_array [i];
  index_array [i] = index_array [j];
  index_array [j] = temp;
}

static int key_char_index;

static int
keyword_comparison (const void *keyword_1, const void *keyword_2)
{
  int char_1;
  int char_2;

  char_1 = keyword_string (*(IR_node_t *) keyword_1) [key_char_index];
  char_2 = keyword_string (*(IR_node_t *) keyword_2) [key_char_index];
  if (char_1 > char_2)
    return 1;
  else if (char_1 < char_2)
    return -1;
  else
    return 0;
}

static void
make_partition (IR_node_t *keyword_array, int keyword_array_length,
                vlo_t *partition, int char_index)
{
  int keyword_index;
  int previous_char;

  assert (keyword_array_length > 1);
  key_char_index = char_index;
  qsort (keyword_array, keyword_array_length, sizeof (IR_node_t),
         keyword_comparison);
  VLO_NULLIFY (*partition);
  previous_char = keyword_string (*keyword_array) [char_index];
  for (keyword_index = 0;
       keyword_index < keyword_array_length;
       keyword_index++)
    if (previous_char
        != keyword_string (keyword_array [keyword_index]) [char_index])
      {
        VLO_ADD_MEMORY (*partition, &keyword_index, sizeof (int));
        previous_char
          = keyword_string (keyword_array [keyword_index]) [char_index];
      }
  VLO_ADD_MEMORY (*partition, &keyword_index, sizeof (int));
}

#if 0

static void
set_the_heuristic_best_index (IR_node_t *keyword_array,
                              int keyword_array_length, 
                              int *index_array, int index_array_length)
{
  vlo_t partition;
  int index_index;
  int *current_bound_ptr;
  int start;
  unsigned int sum;
  unsigned int minimum_sum;
  int i;
  int the_best_index;

  VLO_CREATE (partition, 0);
  minimum_sum = UINT_MAX;
  for (index_index = 0; index_index < index_array_length; index_index++)
    {
      make_partition (keyword_array, keyword_array_length,
                      &partition, index_array [index_index]);
      sum = 0;
      for (start = 0, current_bound_ptr = VLO_BEGIN (partition);
           (char *) current_bound_ptr <= (char *) VLO_END (partition);
           start = *current_bound_ptr, current_bound_ptr++)
        if (*current_bound_ptr - start > 1)
          for (i = start; i < *current_bound_ptr; i++)
            sum += IR_frequency (keyword_array [i]);
      if (minimum_sum > sum)
        {
          minimum_sum = sum;
          the_best_index = index_index;
          if (sum == 0)
            break;
        }
    }
  assert (minimum_sum != UINT_MAX);
  swap_index (index_array, the_best_index, index_array_length - 1);
  VLO_DELETE (partition);
}

#endif

static IR_node_t
find_trie (int max_cost, int original_accumulated_cost,
           IR_node_t *keyword_array, int keyword_array_length, 
           int *index_array, int index_array_length)
{
  int *current_bound_ptr;
  int start;
  int minimum_recognition_cost;
  int accumulated_cost;
  int keyword_index;
  int index_index;
  int new_index_flag;
  IR_node_t result;
  IR_node_t trie_node;
  IR_node_t first_trie_node;
  IR_node_t current_trie_node;
  vlo_t partition;

  assert (keyword_array_length > 0);
  if (keyword_array_length == 1)
    {
      trie_node = get_free_trie_node ();
      IR_set_recognition_cost (trie_node, 0);
      IR_set_keyword (trie_node, *keyword_array);
      IR_set_trie_node_list (trie_node, NULL);
      return trie_node;
    }
  assert (index_array_length > 0);
  minimum_recognition_cost = 0;
  for (keyword_index = 0;
       keyword_index < keyword_array_length;
       keyword_index++)
    minimum_recognition_cost += IR_frequency (keyword_array [keyword_index]);
  if (original_accumulated_cost + minimum_recognition_cost >= max_cost)
    return NULL;
  result = NULL;
#if 0
  set_the_heuristic_best_index (keyword_array, keyword_array_length, 
                                index_array, index_array_length);
#endif
  VLO_CREATE (partition, 0);
  for (index_index = 0; index_index < index_array_length; index_index++)
    {
      make_partition (keyword_array, keyword_array_length,
                      &partition, index_array [index_index]);
      accumulated_cost = original_accumulated_cost + minimum_recognition_cost;
      first_trie_node = NULL;
      new_index_flag = FALSE;
      for (start = 0, current_bound_ptr = VLO_BEGIN (partition);
           (char *) current_bound_ptr <= (char *) VLO_END (partition);
           start = *current_bound_ptr, current_bound_ptr++)
        {
          if (accumulated_cost >= max_cost)
            {
              new_index_flag = TRUE;
              break;
            }
          swap_index (index_array, index_index, index_array_length - 1);
          trie_node
            = find_trie (max_cost, accumulated_cost,
                         keyword_array + start, *current_bound_ptr - start,
                         index_array, index_array_length - 1);
          swap_index (index_array, index_index, index_array_length - 1);
          if (trie_node == NULL)
            {
              new_index_flag = TRUE;
              break;
            }
          IR_set_character_code
            (trie_node,
             keyword_string (keyword_array [start])
             [index_array [index_index]]);
          IR_set_next_trie_node (trie_node, first_trie_node);
          first_trie_node = trie_node;
          accumulated_cost += IR_recognition_cost (trie_node);
        }
      if (new_index_flag)
        {
          free_trie_list_and_below (first_trie_node);
          continue;
        }
      trie_node = get_free_trie_node ();
      IR_set_recognition_cost
        (trie_node, accumulated_cost - original_accumulated_cost);
      if (max_cost > accumulated_cost)
        max_cost = accumulated_cost;
      IR_set_keyword (trie_node, NULL);
      IR_set_trie_node_list (trie_node, first_trie_node);
      for (current_trie_node = IR_trie_node_list (trie_node);
           current_trie_node != NULL;
           current_trie_node = IR_next_trie_node (current_trie_node))
        IR_set_upper_trie_node_level (current_trie_node, trie_node);
      IR_set_trie_node_list_index (trie_node, index_array [index_index]);
      if (result == NULL
          || IR_recognition_cost (trie_node) < IR_recognition_cost (result))
        {
          if (result != NULL)
            free_trie_list_and_below (result);
          result = trie_node;
        }
      else
        free_trie_list_and_below (trie_node);
      if (accumulated_cost
          == original_accumulated_cost + minimum_recognition_cost)
        break; /* The best partition is already achieved. */
    }
  VLO_DELETE (partition);
  return result;
}

/* ??? */
static void
build_pruned_O_trie (void)
{
  vlo_t keywords;
  vlo_t indexes;
  int i;
  IR_node_t trie_node;
  IR_node_t current_keyword;
  IR_node_t current_keyword_set;
  IR_node_t current_trie_node;

  VLO_CREATE (keywords, 0);
  VLO_CREATE (indexes, 0);
  for (current_keyword_set = IR_keyword_set_list (description);
       current_keyword_set != NULL;
       current_keyword_set = IR_next_keyword_set (current_keyword_set))
    {
#ifndef NDEBUG
      if (debug_flag)
        fprintf (stderr, "keywords of length = %d\n",
                 IR_length (current_keyword_set));
#endif
      VLO_NULLIFY (keywords);
      for (current_keyword
             = IR_the_same_length_keyword_list (current_keyword_set);
           current_keyword != NULL;
           current_keyword = IR_next_the_same_length_keyword (current_keyword))
        VLO_ADD_MEMORY (keywords, &current_keyword, sizeof (IR_node_t));
      if (VLO_LENGTH (keywords) / sizeof (IR_node_t) <= 1)
        {
          IR_set_trie_node_list (current_keyword_set, NULL);
          continue;
        }
      VLO_NULLIFY (indexes);
      for (i = 0; i < IR_length (current_keyword_set); i++)
        VLO_ADD_MEMORY (indexes, &i, sizeof (int));
      trie_node = find_trie (INT_MAX, 0,
                             (IR_node_t *) VLO_BEGIN (keywords),
                             VLO_LENGTH (keywords) / sizeof (IR_node_t),
                             (int *) VLO_BEGIN (indexes),
                             VLO_LENGTH (indexes) / sizeof (int));
      assert (trie_node != NULL);
      IR_set_recognition_cost (current_keyword_set,
                               IR_recognition_cost (trie_node));
      IR_set_trie_node_list (current_keyword_set,
                             IR_trie_node_list (trie_node));
      IR_set_trie_node_list_index (current_keyword_set,
                                   IR_trie_node_list_index (trie_node));
      for (current_trie_node = IR_trie_node_list (current_keyword_set);
           current_trie_node != NULL;
           current_trie_node = IR_next_trie_node (current_trie_node))
        IR_set_upper_trie_node_level (current_trie_node, current_keyword_set);
      free_trie_node (trie_node);
    }
  VLO_DELETE (keywords);
  VLO_DELETE (indexes);
}



/* ??? Is it used. */

/* This page contains functions which are used to output parameterized
   names of keywords recognizer objects.  Usually the parameter is
   prefix given in Shilka command line (see commentaries for variable
   `prefix'). */

/* Base name of debug macro parameter used to separate debug code in
   keywords recognizer.  Full name of the macro parameter is
   `__KR_DEBUG__' (see function `output_ifdef_parameter_name'). */

#define DEBUG_PARAMETER_NAME "DEBUG"

/* This function outputs name of debug macro parameter used to
 separate debug code in the keywords recognizer.  The name is output
 as `__<prefix><base_name>__' where prefix is value of variable
 `prefix' and base name is value of the second parameter.  */

static void
output_ifdef_parameter_name (FILE *f, const char *ifdef_parameter_name)
{
  output_string (f, "__");
  output_string (f, prefix);
  output_string (f, ifdef_parameter_name);
  output_string (f, "__");
}



/* Is it used? */

/* This page contains functions for output of C preprocessors lines
   (`#ifdef ...', `#ifndef ', `#endif ... ', and `#line ...'). */


/* This function outputs C preprocessor line `#ifdef ...'  which
   contains parameter with given base name (see function
   `output_ifdef_parameter_name'). */

static void
output_ifdef (FILE *f, const char *ifdef_parameter_base_name)
{
  output_string (f, "#ifdef ");
  output_ifdef_parameter_name (f, ifdef_parameter_base_name);
  output_char ('\n', f);
}

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
output_interface_start_code_insertions (void)
{
  IR_node_t current_declaration;
  FILE *import_code_file;

  if (IR_declaration_list (description) == NULL)
    return;
  if (interface_flag)
    import_code_file = output_interface_file;
  else
    import_code_file = output_implementation_file;
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_import_code))
      {
        output_line (import_code_file,
                     IR_position (current_declaration).line_number,
                     IR_position (current_declaration).file_name);
        output_string (import_code_file,
                       IR_code_insertion_itself (IR_code_itself
                                                 (current_declaration)));
        output_char ('\n', import_code_file);
        output_current_line (import_code_file);
      }
}

static void
output_recognizer_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "recognizer");
}

static void
output_keyword_name (FILE *f, const char *keyword_name)
{
  output_string (f, prefix);
  output_string (f, keyword_name);
}

static void
output_not_found_value (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_not_found");
}

static void
output_keyword_usage_name (FILE *f, IR_node_t keyword)
{
  const char *keyword_name;

  keyword_name = IR_identifier_itself (IR_name (keyword));
  output_string (f, prefix);
  output_string (f, keyword_name);
  if (IR_keyword_name_index (keyword) != 0)
    {
      output_char ('_', f);
      output_decimal_number (f, IR_keyword_name_index (keyword), 0);
    }
  output_string (f, "_usage");
}

static void
output_keyword_usage_reference (FILE *f, IR_node_t keyword)
{
  if (!cpp_flag)
    {
      output_recognizer_name (f);
      output_string (f, ".");
    }
  output_keyword_usage_name (f, keyword);
}

static void
output_not_found_value_usage (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_not_found_usage");
}

static void
output_not_found_value_usage_reference (FILE *f)
{
  if (!cpp_flag)
    {
      output_recognizer_name (f);
      output_string (f, ".");
    }
  output_not_found_value_usage (f);
}

static void
output_strcaseq_function_name (FILE *f)
{
  output_string (f, prefix);
  if (length_flag)
    output_string (f, "strncaseq");
  else
    output_string (f, "strcaseq");
}

static void
output_find_keyword_function_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "find_keyword");
}

static void
output_reset_function_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "reset");
}

static void
output_statistics_function_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "output_statistics");
}

static void
output_constructor_function_name (FILE *f)
{
  assert (cpp_flag);
  output_recognizer_name (f);
}

static void
output_destructor_function_name (FILE *f)
{
  assert (cpp_flag);
  output_char ('~', f);
  output_recognizer_name (f);
}

static void
output_keyword_parameter_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "keyword");
}

static void
output_low_string_parameter_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "tolower");
}

static void
output_length_parameter_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "length");
}

static void 
output_find_keyword_function_declaration (int declaration_flag)
{
  FILE *f;

  f = (interface_flag && !declaration_flag
       ? output_interface_file : output_implementation_file);
  if (!declaration_flag && inline_flag)
    output_string (f, "inline ");
  if (!cpp_flag)
    {
      if (!interface_flag)
        output_string (f, "static ");
      else if (!declaration_flag)
        output_string (f, "extern ");
    }
  if (IR_keyword_type (description) == NULL)
    output_string (f, "int ");
  else
    {
      output_string (f, IR_identifier_itself (IR_keyword_type (description)));
      output_char (' ', f);
    }
  if (declaration_flag && cpp_flag)
    {
      output_recognizer_name (f);
      output_string (f, "::");
    }
  output_find_keyword_function_name (f);
  output_string (f, " (");
  output_string (f, "const char *");
  output_keyword_parameter_name (f);
  output_string (f, ", ");
  output_string (f, "int ");
  output_length_parameter_name (f);
  if (declaration_flag)
    output_string (f, ")\n");
  else
    output_string (f, ");\n");
}

static void
output_reset_function_declaration (int declaration_flag)
{
  FILE *f;

  f = (interface_flag && !declaration_flag
       ? output_interface_file : output_implementation_file);
  if (!cpp_flag)
    {
      if (!interface_flag)
        output_string (f, "static ");
      else if (!declaration_flag)
        output_string (f, "extern ");
    }
  output_string (f, "void ");
  if (declaration_flag && cpp_flag)
    {
      output_recognizer_name (f);
      output_string (f, "::");
    }
  output_reset_function_name (f);
  output_string (f, " (void)");
  if (declaration_flag)
    output_string (f, "\n");
  else
    output_string (f, ";\n");
}

static void
output_statistics_function_declaration (int declaration_flag)
{
  FILE *f;

  f = (interface_flag && !declaration_flag
       ? output_interface_file : output_implementation_file);
  if (!cpp_flag)
    {
      if (!interface_flag)
        output_string (f, "static ");
      else if (!declaration_flag)
        output_string (f, "extern ");
    }
  output_string (f, "void ");
  if (declaration_flag && cpp_flag)
    {
      output_recognizer_name (f);
      output_string (f, "::");
    }
  output_statistics_function_name (f);
  output_string (f, " (void)");
  if (declaration_flag)
    output_string (f, "\n");
  else
    output_string (f, ";\n");
}

static void
output_constructor_function_declaration (int declaration_flag)
{
  FILE *f;

  assert (cpp_flag);
  f = (interface_flag && !declaration_flag
       ? output_interface_file : output_implementation_file);
  if (declaration_flag)
    {
      output_recognizer_name (f);
      output_string (f, "::");
    }
  output_constructor_function_name (f);
  output_string (f, " (void)");
  if (declaration_flag)
    output_string (f, "\n");
  else
    output_string (f, ";\n");
}

static void
output_destructor_function_declaration (int declaration_flag)
{
  FILE *f;

  assert (cpp_flag);
  f = (interface_flag && !declaration_flag
       ? output_interface_file : output_implementation_file);
  if (declaration_flag)
    {
      output_recognizer_name (f);
      output_string (f, "::");
    }
  output_destructor_function_name (f);
  output_string (f, " (void)");
  if (declaration_flag)
    output_string (f, "\n");
  else
    output_string (f, ";\n");
}

static void
output_recognizer_definitions (void)
{
  IR_node_t current_keyword;
  FILE *f;

  if (!cpp_flag)
    {
      if (!strip_flag)
        {
          output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
          output_string (output_implementation_file, "static struct");
          output_string (output_implementation_file, "\n{\n");
          for (current_keyword = IR_keyword_list (description);
               current_keyword != NULL;
               current_keyword = IR_next_keyword (current_keyword))
            {
              output_string (output_implementation_file, "  unsigned int ");
              output_keyword_usage_name (output_implementation_file,
                                         current_keyword);
              output_string (output_implementation_file, ";\n");
            }
          output_string (output_implementation_file, "  unsigned int ");
          output_not_found_value_usage (output_implementation_file);
          output_string (output_implementation_file, ";\n} ");
          output_recognizer_name (output_implementation_file);
          output_string (output_implementation_file, ";\n");
          output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
          output_string (output_implementation_file, "\n");
        }
      output_find_keyword_function_declaration (FALSE);
      output_reset_function_declaration (FALSE);
      output_statistics_function_declaration (FALSE);
      output_string ((interface_flag
                      ? output_interface_file : output_implementation_file),
                     "\n");
    }
  else
    {
      f = (interface_flag
           ? output_interface_file : output_implementation_file);
      output_string (f, "class ");
      output_recognizer_name (f);
      output_string (f, "\n{\n");
      if (!strip_flag)
        {
          output_ifdef (f, DEBUG_PARAMETER_NAME);
          for (current_keyword = IR_keyword_list (description);
               current_keyword != NULL;
               current_keyword = IR_next_keyword (current_keyword))
            {
              output_string (f, "  unsigned int ");
              output_keyword_usage_name (f, current_keyword);
              output_string (f, ";\n");
            }
          output_string (f, "  unsigned int ");
          output_not_found_value_usage (f);
          output_string (f, ";\n");
          output_endif (f, DEBUG_PARAMETER_NAME);
        }
      output_string (f, "\npublic:\n  ");
      output_find_keyword_function_declaration (FALSE);
      output_string (f, "  ");
      output_reset_function_declaration (FALSE);
      output_string (f, "  ");
      output_statistics_function_declaration (FALSE);
      output_string (f, "  ");
      output_constructor_function_declaration (FALSE);
      output_string (f, "  ");
      output_destructor_function_declaration (FALSE);
      output_string (f, "};\n\n");
    }
}

/* ??? */
static void
output_interface_finish_code_insertions (void)
{
  IR_node_t current_declaration;
  FILE *f;

  if (IR_declaration_list (description) == NULL)
    return;
  f = (interface_flag ? output_interface_file : output_implementation_file);
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_export_code))
      {
        output_line (f, IR_position (current_declaration).line_number,
                     IR_position (current_declaration).file_name);
        output_string (f, IR_code_insertion_itself (IR_code_itself
                                                    (current_declaration)));
        output_char ('\n', f);
        output_current_line (f);
      }
}

/* ??? */
static void
output_implementation_start_code_insertions (void)
{
  IR_node_t current_declaration;

  if (IR_declaration_list (description) == NULL)
    return;
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_local_code))
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
output_keyword_name_definitions (FILE *f)
{
  IR_node_t current_keyword;
  int current_number;

  if (enum_flag)
    {
      output_string (f, "enum ");
      output_string (f, "{\n");
      output_string (f, "  ");
    }
  else
    output_string (f, "#define ");
  output_not_found_value (f);
  if (!enum_flag)
    {
      output_char (' ', f);
      output_decimal_number (f, 0, 0);
      output_char ('\n', f);
    }
  current_number = 0;
  for (current_keyword = IR_keyword_list (description);
       current_keyword != NULL;
       current_keyword = IR_next_keyword (current_keyword))
    if (IR_first_the_same_name_keyword_flag (current_keyword))
      {
        current_number++;
        if (enum_flag)
          {
            output_string (f, ",\n");
            output_string (f, "  ");
          }
        else
          output_string (f, "#define ");
        output_keyword_name
          (f, IR_identifier_itself (IR_name (current_keyword)));
        if (!enum_flag)
          {
            output_char (' ', f);
            output_decimal_number (f, current_number, 0);
            output_char ('\n', f);
          }
      }
  if (enum_flag)
    output_string (f, "\n};\n");
  output_char ('\n', f);
}

static void
output_indent (FILE *f, int nest_level)
{
  output_string (f, "  ");
  while (nest_level > 0)
    {
      output_string (f, "    ");
      nest_level--;
    }
}

static void
output_C_char_representation (FILE *f, int ch)
{
  output_string (f, "'");
  if (ch  == '\n')
    output_string (f, "\\n");
  else if (ch  == '\t')
    output_string (f, "\\t");
  else if (ch  == '\v')
    output_string (f, "\\v");
  else if (ch  == '\b')
    output_string (f, "\\b");
  else if (ch  == '\r')
    output_string (f, "\\r");
  else if (ch  == '\f')
    output_string (f, "\\f");
  else if (ch  == '\\')
    output_string (f, "\\\\");
  else if (ch  == '\\')
    output_string (f, "\\\\");
  else if (ch  == '\'')
    output_string (f, "\\'");
  else if (ch  == '\"')
    output_string (f, "\\\"");
  else if (!isprint (ch))
    {
      output_string (f, "\\");
      output_octal_number (f, ch, 0);
    }
  else
    output_char (ch, f);
  output_string (f, "'");
}

static void
output_C_keyword_representation (FILE *f, IR_node_t keyword, int tolower_flag)
{
  if (IR_IS_OF_TYPE (keyword, IR_NM_string_keyword))
    generalized_output_string (f, keyword_representation (keyword),
                               tolower_flag);
  else
    {
      output_char ('"', f);
      generalized_output_string (f, keyword_representation (keyword),
                                 tolower_flag);
      output_char ('"', f);
    }
}

static void
output_keyword_recognition (IR_node_t keyword, int nest_level,
                            vlo_t *rest_indexes)
{
  int *index_ptr;
  int ch;

  output_indent (output_implementation_file, nest_level);
  if (strip_flag && IR_action (keyword) == NULL)
    output_string (output_implementation_file, "    return (");
  else if (VLO_LENGTH (*rest_indexes) != 0)
    output_string (output_implementation_file, "    if (");
  if (VLO_LENGTH (*rest_indexes) / sizeof (int) <= fast_number)
    {
      for (index_ptr = VLO_BEGIN (*rest_indexes);
           (char *) index_ptr <= (char *) VLO_END (*rest_indexes);
           index_ptr++)
        {
          if ((char *) index_ptr != (char *) VLO_BEGIN (*rest_indexes))
            {
              output_string (output_implementation_file, "\n");
              output_indent (output_implementation_file, nest_level);
              if (strip_flag && IR_action (keyword) == NULL)
                output_string (output_implementation_file, "    ");
              output_string (output_implementation_file, "        && ");
            }
          ch = keyword_string (keyword) [*index_ptr];
          if (case_flag && isalpha (ch))
	    output_string (output_implementation_file, "(");
          output_keyword_parameter_name (output_implementation_file);
          output_string (output_implementation_file, "[");
          output_decimal_number (output_implementation_file, *index_ptr, 0);
          output_string (output_implementation_file, "] == ");
          output_C_char_representation (output_implementation_file, ch);
          if (case_flag && isalpha (ch))
            {
              output_string (output_implementation_file, " || ");
              output_keyword_parameter_name (output_implementation_file);
              output_string (output_implementation_file, "[");
              output_decimal_number (output_implementation_file,
                                     *index_ptr, 0);
              output_string (output_implementation_file, "] == ");
              output_C_char_representation
                (output_implementation_file,
                 (islower (ch) ? toupper (ch) : tolower (ch)));
	      output_string (output_implementation_file, ")");
            }
        }
    }
  else
    {
      if (case_flag)
        output_strcaseq_function_name (output_implementation_file);
      else
        output_string (output_implementation_file,
                       (length_flag ? "strncmp" : "strcmp"));
      output_string (output_implementation_file, " (");
      output_keyword_parameter_name (output_implementation_file);
      output_string (output_implementation_file, ", ");
      output_C_keyword_representation (output_implementation_file, keyword,
                                       case_flag);
      if (length_flag)
        {
          output_string (output_implementation_file, ", ");
          output_decimal_number (output_implementation_file,
                                 strlen (keyword_string (keyword)), 0);
        }
      if (case_flag)
        output_string (output_implementation_file, ")");
      else
        output_string (output_implementation_file, ") == 0");
    }
  if (strip_flag && IR_action (keyword) == NULL)
    {
      if (VLO_LENGTH (*rest_indexes) != 0)
        {
          output_string (output_implementation_file, "\n");
          output_indent (output_implementation_file, nest_level);
          output_string (output_implementation_file, "            ? ");
        }
      output_keyword_name
        (output_implementation_file,
         IR_identifier_itself (IR_name (keyword)));
      if (VLO_LENGTH (*rest_indexes) != 0)
        {
          output_string (output_implementation_file, " : ");
          output_not_found_value (output_implementation_file);
        }
      output_string (output_implementation_file, ");\n");
    }
  else
    {
      if (VLO_LENGTH (*rest_indexes) != 0)
        {
          output_string (output_implementation_file, ")\n");
          output_indent (output_implementation_file, nest_level);
        }
      if (VLO_LENGTH (*rest_indexes) != 0)
        output_string (output_implementation_file, "  ");
      output_string (output_implementation_file, "    {\n");
      if (!strip_flag)
        {
          output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
          output_indent (output_implementation_file, nest_level);
          if (VLO_LENGTH (*rest_indexes) != 0)
            output_string (output_implementation_file, "  ");
          output_string (output_implementation_file, "      ");
          output_keyword_usage_reference (output_implementation_file, keyword);
          output_string (output_implementation_file, "++;\n");
          output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
        }
      if (IR_action (keyword) != NULL)
        {
          output_string (output_implementation_file, "{");
          output_line (output_implementation_file,
                       IR_position (IR_action (keyword)).line_number,
                       IR_position (IR_action (keyword)).file_name);
          output_string (output_implementation_file,
                         IR_code_insertion_itself (IR_action (keyword)));
          output_char ('\n', output_implementation_file);
          output_current_line (output_implementation_file);
          output_string (output_implementation_file, "}\n");
        }
      else
        {
          output_indent (output_implementation_file, nest_level);
          if (VLO_LENGTH (*rest_indexes) != 0)
            output_string (output_implementation_file, "  ");
          output_string (output_implementation_file, "      return ");
          output_keyword_name (output_implementation_file,
                               IR_identifier_itself (IR_name (keyword)));
          output_string (output_implementation_file, ";\n");
        }
      output_indent (output_implementation_file, nest_level);
      if (VLO_LENGTH (*rest_indexes) != 0)
        output_string (output_implementation_file, "  ");
      output_string (output_implementation_file, "    }\n");
      if (VLO_LENGTH (*rest_indexes) != 0)
        {
          output_indent (output_implementation_file, nest_level);
          output_string (output_implementation_file, "    else\n");
          output_indent (output_implementation_file, nest_level);
          output_string (output_implementation_file, "      {\n");
          if (!strip_flag)
            {
              output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
              output_indent (output_implementation_file, nest_level);
              output_string (output_implementation_file, "        ");
              output_not_found_value_usage_reference
                (output_implementation_file);
              output_string (output_implementation_file, "++;\n");
              output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
            }
          if (!IR_other_is_present (description)
              || IR_other_action (description) == NULL)
            {
              output_indent (output_implementation_file, nest_level);
              output_string (output_implementation_file, "        return ");
              output_not_found_value (output_implementation_file);
              output_string (output_implementation_file, ";\n");
            }
          else
            {
              output_string (output_implementation_file, "{");
              output_line
                (output_implementation_file,
                 IR_position (IR_other_action (description)).line_number,
                 IR_position (IR_other_action (description)).file_name);
              output_string (output_implementation_file,
                             IR_code_insertion_itself
                             (IR_other_action (description)));
              output_char ('\n', output_implementation_file);
              output_current_line (output_implementation_file);
              output_string (output_implementation_file, "}\n");
            }
          output_indent (output_implementation_file, nest_level);
          output_string (output_implementation_file, "      }\n");
        }
    }
}

static void
get_rest_indexes (IR_node_t trie_node, vlo_t *rest_indexes)
{
  int index;
  int i;
  vlo_t indexes;

  VLO_CREATE (indexes, 64);
  for (;;)
    {
      trie_node = IR_upper_trie_node_level (trie_node);
      index = IR_trie_node_list_index (trie_node);
      for (i = VLO_LENGTH (indexes); i <= index; i++)
        VLO_ADD_BYTE (indexes, 1);
      ((char *) VLO_BEGIN (indexes)) [index] = 0;
      if (IR_IS_OF_TYPE (trie_node, IR_NM_keyword_set))
        break;
    }
  for (index = 0; index < IR_length (trie_node); index++)
    if (index >= VLO_LENGTH (indexes)
        || ((char *) VLO_BEGIN (indexes)) [index])
      VLO_ADD_MEMORY (*rest_indexes, &index, sizeof (index));
  VLO_DELETE (indexes);
}

static void
output_trie_node_switch (IR_node_t trie_node, int nest_level)
{
  IR_node_t current_trie_node;
  vlo_t rest_indexes;

  assert (trie_node != NULL && IR_trie_node_list (trie_node) != NULL);
  output_indent (output_implementation_file, nest_level);
  output_string (output_implementation_file, "switch (");
  if (case_flag)
    output_string (output_implementation_file, "tolower (");
  output_keyword_parameter_name (output_implementation_file);
  output_string (output_implementation_file, " [");
  output_decimal_number (output_implementation_file,
                         IR_trie_node_list_index (trie_node), 0);
  if (case_flag)
    output_string (output_implementation_file, "]))\n");
  else
    output_string (output_implementation_file, "])\n");
  output_indent (output_implementation_file, nest_level);
  output_string (output_implementation_file, "  {\n");
  for (current_trie_node = IR_trie_node_list (trie_node);
       current_trie_node != NULL;
       current_trie_node = IR_next_trie_node (current_trie_node))
    {
      output_indent (output_implementation_file, nest_level);
      output_string (output_implementation_file, "  case ");
      output_character_representation
        (output_implementation_file,
         (case_flag ? tolower (IR_character_code (current_trie_node))
          : IR_character_code (current_trie_node)));
      output_string (output_implementation_file, ":\n");
      if (IR_keyword (current_trie_node) != NULL)
        {
          VLO_CREATE (rest_indexes, 256);
          get_rest_indexes (current_trie_node, &rest_indexes);
          output_keyword_recognition (IR_keyword (current_trie_node),
                                      nest_level, &rest_indexes);
          VLO_DELETE (rest_indexes);
        }
      else
        output_trie_node_switch (current_trie_node, nest_level + 1);
    }
  output_indent (output_implementation_file, nest_level);
  output_string (output_implementation_file, "  default:\n");
  if (!strip_flag)
    {
      output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
      output_indent (output_implementation_file, nest_level);
      output_string (output_implementation_file, "    ");
      output_not_found_value_usage_reference (output_implementation_file);
      output_string (output_implementation_file, "++;\n");
      output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
    }
  if (!IR_other_is_present (description)
      || IR_other_action (description) == NULL)
    {
      output_indent (output_implementation_file, nest_level);
      output_string (output_implementation_file, "    return ");
      output_not_found_value (output_implementation_file);
      output_string (output_implementation_file, ";\n");
    }
  else
    {
      output_string (output_implementation_file, "{");
      output_line (output_implementation_file,
                   IR_position (IR_other_action (description)).line_number,
                   IR_position (IR_other_action (description)).file_name);
      output_string (output_implementation_file,
                     IR_code_insertion_itself (IR_other_action (description)));
      output_char ('\n', output_implementation_file);
      output_current_line (output_implementation_file);
      output_string (output_implementation_file, "}\n");
    }
  output_indent (output_implementation_file, nest_level);
  output_string (output_implementation_file, "  }\n");
}

static void
output_strcaseq_function ()
{
  output_string (output_implementation_file, "static int\n");
  output_strcaseq_function_name (output_implementation_file);
  output_string (output_implementation_file, " (const char *");
  output_keyword_parameter_name (output_implementation_file);
  output_string (output_implementation_file, ", const char *");
  output_low_string_parameter_name (output_implementation_file);
  if (length_flag)
    {
      output_string (output_implementation_file, ", size_t ");
      output_length_parameter_name (output_implementation_file);
    }
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, "{\n");
  output_string (output_implementation_file, "  for (;;)\n");
  output_string (output_implementation_file, "    {\n");
  if (length_flag)
    {
      output_string (output_implementation_file, "      if (");
      output_length_parameter_name (output_implementation_file);
      output_string (output_implementation_file, "-- >= 0)\n");
      output_string (output_implementation_file, "        return 1;\n");
    }
  output_string (output_implementation_file, "      if (tolower (*");
  output_keyword_parameter_name (output_implementation_file);
  output_string (output_implementation_file, ") != *");
  output_low_string_parameter_name (output_implementation_file);
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, "        return 0;\n");
  output_string (output_implementation_file, "      if (*");
  output_keyword_parameter_name (output_implementation_file);
  output_string (output_implementation_file, " == '\\0')\n");
  output_string (output_implementation_file, "        return 1;\n");
  output_string (output_implementation_file, "      ");
  output_keyword_parameter_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n");
  output_string (output_implementation_file, "      ");
  output_low_string_parameter_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n");
  output_string (output_implementation_file, "    }\n");
  output_string (output_implementation_file, "}\n\n");
}

static void
output_find_keyword_function (void)
{
  int index;
  IR_node_t current_keyword_set;
  vlo_t rest_indexes;

  output_find_keyword_function_declaration (TRUE);
  output_string (output_implementation_file, "{\n");
  output_string (output_implementation_file, "\n");
  output_string (output_implementation_file, "  switch (");
  output_length_parameter_name (output_implementation_file);
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, "    {\n");
  for (current_keyword_set = IR_keyword_set_list (description);
       current_keyword_set != NULL;
       current_keyword_set = IR_next_keyword_set (current_keyword_set))
    {
      output_string (output_implementation_file, "    case ");
      output_decimal_number (output_implementation_file,
                             IR_length (current_keyword_set), 0);
      output_string (output_implementation_file, ":\n");
      if (IR_next_the_same_length_keyword (IR_the_same_length_keyword_list
                                           (current_keyword_set)) == NULL)
        {
          VLO_CREATE (rest_indexes, 128);
          for (index = 0; index < IR_length (current_keyword_set); index++)
            VLO_ADD_MEMORY (rest_indexes, &index, sizeof (int));
          output_keyword_recognition
            (IR_the_same_length_keyword_list (current_keyword_set), 1,
             &rest_indexes);
          VLO_DELETE (rest_indexes);
        }
      else
        output_trie_node_switch (current_keyword_set, 1);
    }
  output_string (output_implementation_file, "    default:\n");
  if (!strip_flag)
    {
      output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
      output_string (output_implementation_file, "      ");
      output_not_found_value_usage_reference (output_implementation_file);
      output_string (output_implementation_file, "++;\n");
      output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
    }
  if (!IR_other_is_present (description)
      || IR_other_action (description) == NULL)
    {
      output_string (output_implementation_file, "      return ");
      output_not_found_value (output_implementation_file);
      output_string (output_implementation_file, ";\n");
    }
  else
    {
      output_string (output_implementation_file, "{");
      output_line (output_implementation_file,
                   IR_position (IR_other_action (description)).line_number,
                   IR_position (IR_other_action (description)).file_name);
      output_string (output_implementation_file,
                     IR_code_insertion_itself (IR_other_action (description)));
      output_char ('\n', output_implementation_file);
      output_current_line (output_implementation_file);
      output_string (output_implementation_file, "}\n");
    }
  output_string (output_implementation_file, "    }\n");
  output_string (output_implementation_file, "}\n\n");
}

static void
output_reset_function (void)
{
  IR_node_t current_keyword;

  output_reset_function_declaration (TRUE);
  output_string (output_implementation_file, "{\n");
  if (!strip_flag)
    {
      output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
      for (current_keyword = IR_keyword_list (description);
           current_keyword != NULL;
           current_keyword = IR_next_keyword (current_keyword))
        {
          output_string (output_implementation_file, "  ");
          output_keyword_usage_reference
            (output_implementation_file, current_keyword);
          output_string (output_implementation_file, " = 0;\n");
        }
      output_string (output_implementation_file, "  ");
      output_not_found_value_usage_reference (output_implementation_file);
      output_string (output_implementation_file, " = 0;\n");
      output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
    }
  output_string (output_implementation_file, "}\n\n");
}

static void
output_statistics_function (void)
{
  IR_node_t current_keyword;

  output_statistics_function_declaration (TRUE);
  output_string (output_implementation_file, "{\n");
  if (!strip_flag)
    {
      output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
      output_string (output_implementation_file,
                     "  unsigned int sum = 1;\n\n");
      for (current_keyword = IR_keyword_list (description);
           current_keyword != NULL;
           current_keyword = IR_next_keyword (current_keyword))
        {
          output_string (output_implementation_file, "  sum += ");
          output_keyword_usage_reference (output_implementation_file,
                                          current_keyword);
          output_string (output_implementation_file, ";\n");
        }
      for (current_keyword = IR_keyword_list (description);
           current_keyword != NULL;
           current_keyword = IR_next_keyword (current_keyword))
        {
          output_string (output_implementation_file, "  ");
          output_string (output_implementation_file,
                         "printf (\"%s: %8u occurrences %8u frequency\\n\", ");
          output_C_keyword_representation (output_implementation_file,
                                           current_keyword, FALSE);
          output_string (output_implementation_file, ",\n");
          output_string (output_implementation_file, "          ");
          output_keyword_usage_reference (output_implementation_file,
                                          current_keyword);
          output_string (output_implementation_file, ",\n");
          output_string (output_implementation_file, "          ");
          output_decimal_number (output_implementation_file,
                                 100 * IR_keywords_number (description), 0);
          output_string (output_implementation_file, "*(");
          output_keyword_usage_reference (output_implementation_file,
                                          current_keyword);
          output_string (output_implementation_file, "+1)/sum");
          output_string (output_implementation_file, ");\n");
        }
      output_string (output_implementation_file, "  ");
      output_string
        (output_implementation_file,
         "printf (\"+++ %8u found occurrences, %8u not found ones\\n\",\n");
      output_string (output_implementation_file, "          sum - 1, ");
      output_not_found_value_usage_reference (output_implementation_file);
      output_string (output_implementation_file, ");\n");
      output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
    }
  output_string (output_implementation_file, "}\n\n");
}

static void
output_constructor_function (void)
{
  assert (cpp_flag);
  output_constructor_function_declaration (TRUE);
  output_string (output_implementation_file, "{\n");
  output_string (output_implementation_file, "  ");
  output_reset_function_name (output_implementation_file);
  output_string (output_implementation_file, " ();\n}\n\n");
}

static void
output_destructor_function (void)
{
  assert (cpp_flag);
  output_destructor_function_declaration (TRUE);
  output_string (output_implementation_file, "{\n");
  output_string (output_implementation_file, "  ");
  output_statistics_function_name (output_implementation_file);
  output_string (output_implementation_file, " ();\n}\n\n");
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
static void
output_keyword_description (IR_node_t keyword, int nest_level)
{
  output_indent (output_description_file, nest_level);
  output_string (output_description_file, keyword_representation (keyword));
  output_string (output_description_file, " -> ");
  output_decimal_number (output_description_file, nest_level - 1, 0);
  output_string (output_description_file, " indexing, freq. ");
  output_decimal_number (output_description_file, IR_frequency (keyword), 0);
  output_string (output_description_file, "\n");
}


static void
output_trie_node_description (IR_node_t trie_node, int nest_level)
{
  IR_node_t current_trie_node;

  assert (trie_node != NULL && IR_trie_node_list (trie_node) != NULL);
  output_indent (output_description_file, nest_level);
  output_string (output_description_file, "char index ");
  output_decimal_number (output_description_file,
                         IR_trie_node_list_index (trie_node), 0);
  output_string (output_description_file, " (overall recognition cost ");
  output_decimal_number (output_description_file,
                         IR_recognition_cost (trie_node), 0);
  output_string (output_description_file, "):\n");
  for (current_trie_node = IR_trie_node_list (trie_node);
       current_trie_node != NULL;
       current_trie_node = IR_next_trie_node (current_trie_node))
    {
      output_indent (output_description_file, nest_level);
      output_string (output_description_file, "  ");
      output_character_representation
        (output_description_file,
         (case_flag ? tolower (IR_character_code (current_trie_node))
          : IR_character_code (current_trie_node)));
      output_string (output_description_file, ": ");
      if (IR_keyword (current_trie_node) != NULL)
        output_keyword_description (IR_keyword (current_trie_node),
                                    nest_level + 1);
      else
        {
          output_string (output_description_file, "\n");
          output_trie_node_description (current_trie_node, nest_level + 1);
        }
    }
}

/* ??? */
static void
output_description (void)
{
  IR_node_t current_keyword_set;

  for (current_keyword_set = IR_keyword_set_list (description);
       current_keyword_set != NULL;
       current_keyword_set = IR_next_keyword_set (current_keyword_set))
    {
      output_string (output_description_file, "\nKeywords of length ");
      output_decimal_number (output_description_file,
                             IR_length (current_keyword_set), 0);
      output_string (output_description_file, ":\n");
      if (IR_next_the_same_length_keyword (IR_the_same_length_keyword_list
                                           (current_keyword_set)) == NULL)
        output_keyword_description
          (IR_the_same_length_keyword_list (current_keyword_set), 1);
      else
        output_trie_node_description (current_keyword_set, 1);
    }
}


static void
output_statistics (FILE *f)
{
  int cost;
  int minimal_cost;
  int keyword_sets_size;
  int weighted_keyword_sets_size;
  int average_length;
  int length_sum;
  int weighted_length_sum;
  int size;
  int weighted_size;
  IR_node_t current_keyword_set;
  IR_node_t current_keyword;

  output_char ('\n', f);
#ifndef NDEBUG
  output_decimal_number (f, allocated_trie_nodes_number, 0);
  output_string (f, " all allocated trie nodes\n");
#endif
  output_decimal_number (f, IR_keywords_number (description), 0);
  output_string (f, " keywords, ");
  output_decimal_number (f, IR_keyword_names_number (description), 0);
  output_string (f, " different keyword names\n");
  cost = 0;
  minimal_cost = 0;
  length_sum = 0;
  weighted_length_sum = 0;
  keyword_sets_size = 0;
  weighted_keyword_sets_size = 0;
  for (current_keyword_set = IR_keyword_set_list (description);
       current_keyword_set != NULL;
       current_keyword_set = IR_next_keyword_set (current_keyword_set))
    {
      size = 0;
      weighted_size = 0;
      for (current_keyword
             = IR_the_same_length_keyword_list (current_keyword_set);
           current_keyword != NULL;
           current_keyword
             = IR_next_the_same_length_keyword (current_keyword))
        {
          size++;
          weighted_size += IR_frequency (current_keyword);
        }
      keyword_sets_size += size;
      weighted_keyword_sets_size += weighted_size;
      length_sum += (IR_length (current_keyword_set) * size);
      weighted_length_sum += (IR_length (current_keyword_set) * weighted_size);
      if (IR_next_the_same_length_keyword (IR_the_same_length_keyword_list
                                           (current_keyword_set)) != NULL)
        {
          cost += IR_recognition_cost (current_keyword_set);
          for (current_keyword
                 = IR_the_same_length_keyword_list (current_keyword_set);
               current_keyword != NULL;
               current_keyword
                 = IR_next_the_same_length_keyword (current_keyword))
            minimal_cost += IR_frequency (current_keyword);
        }
    }
  if (keyword_sets_size == 0)
    keyword_sets_size = 1;
  average_length = 100 * length_sum / keyword_sets_size;
  output_decimal_number (f, average_length / 100, 0);
  output_string (f, ".");
  output_decimal_number (f, average_length % 100, 0);
  output_string (f, " average keywords length, ");
  if (weighted_keyword_sets_size == 0)
    weighted_keyword_sets_size = 1;
  average_length = 100 * weighted_length_sum / weighted_keyword_sets_size;
  output_decimal_number (f, average_length / 100, 0);
  output_string (f, ".");
  output_decimal_number (f, average_length % 100, 0);
  output_string (f, " weighted average keywords length\n");
  output_decimal_number (f, cost, 0);
  output_string (f, " achieved trie cost, ");
  output_decimal_number (f, minimal_cost, 0);
  output_string (f, " minimal cost, ");
  if (minimal_cost == 0)
    minimal_cost = 1;
  output_decimal_number (f, 100 * cost / minimal_cost, 0);
  output_string (f, " % ratio\n");
}

static void
output_time_statistics (FILE *f)
{
  fprintf (f, "\n  building pruned O-trie: %s, ",
           active_time_string (pruned_O_trie_time));
  fprintf (f, "output: %s\n", active_time_string (output_time));
}

void
generate (void)
{
  pruned_O_trie_time = create_ticker ();
  initiate_trie_nodes ();
  make_the_same_name_keyword_lists ();
  make_the_same_length_keyword_partition ();
  build_pruned_O_trie ();
  ticker_off (&pruned_O_trie_time);
  output_time = create_ticker ();
  initiate_output ();
  if (interface_flag)
    {
      output_ifndef (output_interface_file, description_name);
      output_string (output_interface_file, "#define ");
      output_ifdef_parameter_name (output_interface_file, description_name);
      output_string (output_interface_file, "\n\n");
    }
  if (statistics_flag)
    {
      output_string (output_implementation_file, "#define ");
      output_ifdef_parameter_name (output_implementation_file,
                                   DEBUG_PARAMETER_NAME);
      output_string (output_implementation_file, "\n\n");
    }
  /* ??? */
  output_string (output_implementation_file, "#include <stdio.h>\n");
  if (case_flag)
    output_string (output_implementation_file, "#include <ctype.h>\n");
  else
    output_string (output_implementation_file, "#include <string.h>\n");
  if (interface_flag)
    {
      output_string (output_implementation_file, "#include \"");
      output_string (output_implementation_file, output_interface_file_name);
      output_string (output_implementation_file, "\"\n");
    }
  output_string (output_implementation_file, "\n");
  output_interface_start_code_insertions ();
  if (!no_definitions_flag)
    output_keyword_name_definitions (export_flag && interface_flag
                                     ? output_interface_file
                                     : output_implementation_file);
  output_recognizer_definitions ();
  output_interface_finish_code_insertions ();
  output_implementation_start_code_insertions ();
  if (case_flag)
    output_strcaseq_function (length_flag);
  output_find_keyword_function ();
  output_reset_function ();
  output_statistics_function ();
  if (cpp_flag)
    {
      output_constructor_function ();
      output_destructor_function ();
    }
  output_additional_code ();
  if (interface_flag)
    output_endif (output_interface_file, description_name);
  if (v_flag)
    {
      output_description ();
      output_statistics (output_description_file);
    }
  output_statistics (stderr);
  ticker_off (&output_time);
  output_time_statistics (stderr);
  finish_trie_nodes ();
}
