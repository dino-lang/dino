/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

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


#include <stdio.h>
#include <stdlib.h>
#include "bits.h"
#include "hashtab.h"
#include "vlobject.h"
#include "common.h"
#include "ird.h"
#include "output.h"
#include "contexts.h"

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


/* This page contains abstract data `token string'. */

/* The token string table itself is represented by the following
   variable. */

static hash_table_t token_string_table;

/* The following variable is used for storing pointers to token
   strings which are in the table. */

static vlo_t token_strings;

/* The following variable is number of token strings which are in the
   table. */

static unsigned int current_number_of_token_strings;

/* The following variable is used for storing intermediate result of
   token string concatenation operation as array. */

static vlo_t token_string_array_representation;

/* The following macro value is the base of token string element
   value.  Remember that the table element must have non-zero
   value. */

#define TOKEN_STRING_TABLE_ELEMENT_BASE 8

static unsigned
token_string_hash_function (hash_table_entry_t token_string)
{
  unsigned hash_value;
  IR_node_t *string_token_ptr;
  int token_string_length;
  token_string_t temp;

  temp = (token_string_t) token_string - TOKEN_STRING_TABLE_ELEMENT_BASE;
  token_string = (hash_table_entry_t) temp;
  assert ((token_string_t) token_string >= 0
          && (token_string_t) token_string <= current_number_of_token_strings);
  string_token_ptr = ((IR_node_t *) VLO_BEGIN (token_strings)
                      + max_look_ahead_number * (token_string_t) token_string);
  token_string_length = max_look_ahead_number;
  hash_value = 0;
  while (token_string_length != 0 && *string_token_ptr != NULL)
    {
      token_string_length--;
      hash_value
        = ((hash_value >> (sizeof (unsigned) - 1) * CHAR_BIT)
           | (hash_value << CHAR_BIT))
          + IR_token_order_number (*string_token_ptr);
      string_token_ptr++;
    }
  return hash_value;
}

int token_string_comparison (token_string_t token_string_1,
                             token_string_t token_string_2);

static int
token_string_eq_function (hash_table_entry_t token_string_1,
                          hash_table_entry_t token_string_2)
{
  return token_string_comparison ((token_string_t) token_string_1
                                  - TOKEN_STRING_TABLE_ELEMENT_BASE,
                                  (token_string_t) token_string_2
                                  - TOKEN_STRING_TABLE_ELEMENT_BASE) == 0;
}

static token_string_t
insert_token_string (token_string_t token_string)
{
  hash_table_entry_t *entry_ptr;

  assert (token_string >= 0
          && token_string <= current_number_of_token_strings);
  entry_ptr
    = find_hash_table_entry
      (token_string_table,
       (hash_table_entry_t) (token_string + TOKEN_STRING_TABLE_ELEMENT_BASE),
       TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) (token_string
                                       + TOKEN_STRING_TABLE_ELEMENT_BASE);
  return (token_string_t) *entry_ptr - TOKEN_STRING_TABLE_ELEMENT_BASE;
}

token_string_t
get_new_token_string (IR_node_t *tokens, int tokens_number)
{
  token_string_t result;
  int token_string_length;
  IR_node_t empty;

  assert (tokens_number >= 0);
  empty = NULL;
  for (token_string_length = max_look_ahead_number;
       token_string_length != 0;
       token_string_length--)
    {
      if (tokens_number == 0)
        VLO_ADD_MEMORY (token_strings, &empty, sizeof (IR_node_t));
      else
        {
          VLO_ADD_MEMORY (token_strings, tokens, sizeof (IR_node_t));
          tokens_number--;
          tokens++;
        }
    }
  assert (tokens_number == 0);
  result
    = insert_token_string ((token_string_t) current_number_of_token_strings);
  if (result != current_number_of_token_strings)
    VLO_SHORTEN (token_strings, max_look_ahead_number * sizeof (IR_node_t));
  else
    current_number_of_token_strings++;
  return result;
}

IR_node_t
get_n_th_token (token_string_t token_string, int n)
{
  if (n >= max_look_ahead_number)
    return NULL;
  else
    return ((IR_node_t *) VLO_BEGIN (token_strings)
            + max_look_ahead_number * token_string) [n];
}

int
token_string_comparison (token_string_t token_string_1,
                         token_string_t token_string_2)
{
  IR_node_t *string_token_ptr_1;
  IR_node_t *string_token_ptr_2;
  int token_string_length;

  assert (token_string_1 >= 0
          && token_string_1 <= current_number_of_token_strings
          && token_string_2 >= 0
          && token_string_2 <= current_number_of_token_strings);
  string_token_ptr_1 = ((IR_node_t *) VLO_BEGIN (token_strings)
                        + max_look_ahead_number * token_string_1);
  string_token_ptr_2 = ((IR_node_t *) VLO_BEGIN (token_strings)
                        + max_look_ahead_number * token_string_2);
  token_string_length = max_look_ahead_number;
  while (token_string_length != 0
         && *string_token_ptr_1 == *string_token_ptr_2)
    {
      token_string_length--;
      string_token_ptr_1++;
      string_token_ptr_2++;
    }
  if (token_string_length == 0 || *string_token_ptr_1 == *string_token_ptr_2)
    return 0;
  else if (*string_token_ptr_1 == NULL
           || (*string_token_ptr_2 != NULL
               /* Remember that token values (token range values) are
                  not intersected here. */
               && (IR_value (*string_token_ptr_1)
                   < IR_value (*string_token_ptr_2))))
    return -1;
  else
    return 1;
}

int
token_string_length (token_string_t token_string)
{
  IR_node_t *current_string_token_ptr;
  int length;

  assert (token_string >= 0 && token_string < current_number_of_token_strings);
  length = 0;
  for (current_string_token_ptr = ((IR_node_t *) VLO_BEGIN (token_strings)
                                   + max_look_ahead_number * token_string);
       length != max_look_ahead_number && *current_string_token_ptr != NULL;
       current_string_token_ptr++)
    length++;
  return length;
}

token_string_t
token_string_shortening (token_string_t token_string,
                         int maximum_result_token_string_length)
{
  int length;
  IR_node_t *bound;
  IR_node_t *current_token_string_ptr;

  length = token_string_length (token_string);
  assert (maximum_result_token_string_length >= 1
          && maximum_result_token_string_length <= max_look_ahead_number);
  if (length <= maximum_result_token_string_length)
    return token_string;
  VLO_NULLIFY (token_string_array_representation);
  current_token_string_ptr = ((IR_node_t *) VLO_BEGIN (token_strings)
                              + max_look_ahead_number * token_string);
  bound = current_token_string_ptr + maximum_result_token_string_length;
  do
    {
      VLO_ADD_MEMORY (token_string_array_representation,
                      current_token_string_ptr, sizeof (IR_node_t));
      current_token_string_ptr++;
    }
  while (current_token_string_ptr < bound);
  return get_new_token_string (VLO_BEGIN (token_string_array_representation),
                               maximum_result_token_string_length);
}

static token_string_t
token_string_concat (token_string_t token_string_1,
                     token_string_t token_string_2,
                     int maximum_result_token_string_length)
{
  int result_length;
  IR_node_t *current_token_string_ptr_1;
  IR_node_t *current_token_string_ptr_2;

  VLO_NULLIFY (token_string_array_representation);
  result_length = 0;
  current_token_string_ptr_1 = ((IR_node_t *) VLO_BEGIN (token_strings)
                                + max_look_ahead_number * token_string_1);
  while (*current_token_string_ptr_1 != NULL
         && result_length < maximum_result_token_string_length)
    {
      result_length++;
      VLO_ADD_MEMORY (token_string_array_representation,
                      current_token_string_ptr_1, sizeof (IR_node_t));
      current_token_string_ptr_1++;
    }
  current_token_string_ptr_2 = ((IR_node_t *) VLO_BEGIN (token_strings)
                                + max_look_ahead_number * token_string_2);
  while (*current_token_string_ptr_2 != NULL
         && result_length < maximum_result_token_string_length)
    {
      result_length++;
      VLO_ADD_MEMORY (token_string_array_representation,
                      current_token_string_ptr_2, sizeof (IR_node_t));
      current_token_string_ptr_2++;
    }
  return get_new_token_string (VLO_BEGIN (token_string_array_representation),
                               result_length);
}

void
output_token_string (token_string_t token_string, FILE *f)
{
  IR_node_t *string_token_ptr;
  int token_string_length;
  int first_token_flag;

  string_token_ptr = ((IR_node_t *) VLO_BEGIN (token_strings)
                      + max_look_ahead_number * token_string);
  if (*string_token_ptr == NULL)
    {
      output_string (f, " <e>");
      return;
    }
  token_string_length = max_look_ahead_number;
  first_token_flag = TRUE;
  while (token_string_length != 0 && *string_token_ptr != NULL)
    {
      if (!first_token_flag)
        output_char (' ', f);
      first_token_flag = FALSE;
      output_single_definition (f, *string_token_ptr);
      token_string_length--;
      string_token_ptr++;
    }
}

static void
initiate_token_strings (void)
{
  VLO_CREATE (token_strings, 5000);
  VLO_CREATE (token_string_array_representation, 100);
  current_number_of_token_strings = 0;
  token_string_table = create_hash_table (1000, token_string_hash_function,
                                          token_string_eq_function);
}

static void
finish_token_strings (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Token string table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (token_string_table),
       hash_table_elements_number (token_string_table),
       get_collisions (token_string_table));
#endif
  delete_hash_table (token_string_table);
  VLO_DELETE (token_string_array_representation);
  VLO_DELETE (token_strings);
}



/* This page contains abstract data `LR-situations contexts'. */


/* The following variable refers to the first free bit string
   representing LR-situation contexts.  Pointer to the next free bit
   string is contained at the start of given free bit string. */

static context_t first_free_context;

context_t
get_null_context (void)
{
  context_t result;
  int initial_bit_string_length;

  if (first_free_context == NULL)
    {
      IR_TOP_EXPAND (sizeof (struct context_t));
      result = IR_TOP_BEGIN ();
      IR_TOP_FINISH ();
      initial_bit_string_length
        = (current_number_of_token_strings
           + current_number_of_token_strings / 2)
          / CHAR_BIT + 1;
      VLO_CREATE (result->bit_string, initial_bit_string_length);
    }
  else
    {
      result = first_free_context;
      first_free_context
        = *(context_t *) VLO_BEGIN (first_free_context->bit_string);
      VLO_NULLIFY (result->bit_string);
    }
  return result;
}

void
free_context (context_t context)
{
  VLO_NULLIFY (context->bit_string);
  VLO_ADD_MEMORY (context->bit_string, &first_free_context,
                  sizeof (context_t));
  first_free_context = context;
}

static void
expand_context (context_t context, unsigned int additional_elements_number)
{
  bit_string_element_t *current_bit_string_element_ptr;
  bit_string_element_t *initiated_region_bound;

  VLO_EXPAND (context->bit_string,
              sizeof (bit_string_element_t) * additional_elements_number);
  initiated_region_bound
    = (bit_string_element_t *) ((char *) VLO_BEGIN (context->bit_string)
                                + VLO_LENGTH (context->bit_string));
  current_bit_string_element_ptr
    = initiated_region_bound - additional_elements_number;
  while (current_bit_string_element_ptr < initiated_region_bound)
    {
      *current_bit_string_element_ptr++ = 0;
    }
}

static void
make_the_same_size (context_t context_1, context_t context_2)
{
  int difference;

  difference
    = VLO_LENGTH (context_1->bit_string) - VLO_LENGTH (context_2->bit_string);
  if (difference == 0)
    return;
  else if (difference < 0)
    expand_context (context_1,
                    (unsigned) (-difference) / sizeof (bit_string_element_t));
  else
    expand_context (context_2,
                    (unsigned) difference / sizeof (bit_string_element_t));
}

int
it_is_in_context (int order_number, context_t context)
{
  if (order_number >= VLO_LENGTH (context->bit_string) * CHAR_BIT)
    return 0;
  else
    return BIT (VLO_BEGIN (context->bit_string), order_number);
}

void
set_context_element_value (context_t context, token_string_t element_number,
                           int element_value)
{
  if (element_number >= VLO_LENGTH (context->bit_string) * CHAR_BIT)
    expand_context
      (context,
       (unsigned) element_number / (sizeof (bit_string_element_t) * CHAR_BIT)
       + 1 - ((unsigned) VLO_LENGTH (context->bit_string)
              / sizeof (bit_string_element_t)));
  if (element_value)
    element_value = 1;
  SET_BIT (VLO_BEGIN (context->bit_string), element_number, element_value);
}

void
context_copy (context_t to, context_t from)
{
  bit_string_element_t *from_bit_string_bound;
  bit_string_element_t *from_bit_string;
  bit_string_element_t *to_bit_string;

  make_the_same_size (to, from);
  from_bit_string = VLO_BEGIN (from->bit_string);
  to_bit_string = VLO_BEGIN (to->bit_string);
  from_bit_string_bound
    = (bit_string_element_t *) ((char *) from_bit_string
                                + VLO_LENGTH (from->bit_string));
  while (from_bit_string < from_bit_string_bound)
    {
      *to_bit_string++ = *from_bit_string++;
    }
}

void
zero_context (context_t context)
{
  VLO_NULLIFY (context->bit_string);
}

int
context_size (context_t context)
{
  bit_string_element_t *bit_string;
  int bits_number;
  int bit_string_element_number;
  int result;

  result = 0;
  bit_string = VLO_BEGIN (context->bit_string);
  for (bit_string_element_number
       = ((unsigned) VLO_LENGTH (context->bit_string)
          / sizeof (bit_string_element_t));
       bit_string_element_number > 0;
       bit_string_element_number--, bit_string++)
    if (*bit_string)
       {
         for (bits_number = sizeof (bit_string_element_t) * CHAR_BIT - 1;
              bits_number >= 0;
              bits_number--)
           if (BIT (bit_string, bits_number))
             result++;
       }
  return result;
}

int
it_is_zero_context (context_t context)
{
  bit_string_element_t *bit_string_bound;
  bit_string_element_t *bit_string;

  bit_string = VLO_BEGIN (context->bit_string);
  bit_string_bound
    = (bit_string_element_t *) ((char *) bit_string
                                + VLO_LENGTH (context->bit_string));
  while (bit_string < bit_string_bound)
    {
      if (*bit_string++ != 0) 
        return FALSE;
    }
  return TRUE;
}

int
context_in (context_t context_1, context_t context_2)
{
  bit_string_element_t *bit_string_bound_1;
  bit_string_element_t *bit_string_1;
  bit_string_element_t *bit_string_2;

  make_the_same_size (context_1, context_2);
  bit_string_1 = VLO_BEGIN (context_1->bit_string);
  bit_string_2 = VLO_BEGIN (context_2->bit_string);
  bit_string_bound_1
    = (bit_string_element_t *) ((char *) bit_string_1
                                + VLO_LENGTH (context_1->bit_string));
  while (bit_string_1 < bit_string_bound_1)
    {
      if (*bit_string_2 != (*bit_string_2 | *bit_string_1++)) 
        return FALSE;
      bit_string_2++;
    }
  return TRUE;
}

void
context_or (context_t context_1, context_t context_2)
{
  bit_string_element_t *bit_string_bound_1;
  bit_string_element_t *bit_string_1;
  bit_string_element_t *bit_string_2;

  make_the_same_size (context_1, context_2);
  bit_string_1 = VLO_BEGIN (context_1->bit_string);
  bit_string_2 = VLO_BEGIN (context_2->bit_string);
  bit_string_bound_1
    = (bit_string_element_t *) ((char *) bit_string_1
                                + VLO_LENGTH (context_1->bit_string));
  while (bit_string_1 < bit_string_bound_1)
    {
      *bit_string_1 = *bit_string_1 | *bit_string_2++;
      bit_string_1++;
    }
}

void
context_and (context_t context_1, context_t context_2)
{
  bit_string_element_t *bit_string_bound_1;
  bit_string_element_t *bit_string_1;
  bit_string_element_t *bit_string_2;

  make_the_same_size (context_1, context_2);
  bit_string_1 = VLO_BEGIN (context_1->bit_string);
  bit_string_2 = VLO_BEGIN (context_2->bit_string);
  bit_string_bound_1
    = (bit_string_element_t *) ((char *) bit_string_1
                                + VLO_LENGTH (context_1->bit_string));
  while (bit_string_1 < bit_string_bound_1)
    {
      *bit_string_1 = *bit_string_1 & *bit_string_2++;
      bit_string_1++;
    }
}

void
context_or_of_and (context_t or_context,
                   context_t and_context_1, context_t and_context_2)
{
  bit_string_element_t *or_bit_string_bound;
  bit_string_element_t *or_bit_string;
  bit_string_element_t *and_bit_string_1;
  bit_string_element_t *and_bit_string_2;

  make_the_same_size (and_context_1, and_context_2);
  make_the_same_size (and_context_1, or_context);
  make_the_same_size (and_context_2, or_context);
  or_bit_string = VLO_BEGIN (or_context->bit_string);
  and_bit_string_1 = VLO_BEGIN (and_context_1->bit_string);
  and_bit_string_2 = VLO_BEGIN (and_context_2->bit_string);
  or_bit_string_bound
    = (bit_string_element_t *) ((char *) or_bit_string
                                + VLO_LENGTH (or_context->bit_string));
  while (or_bit_string < or_bit_string_bound)
    {
      *or_bit_string++ |= *and_bit_string_1++ & *and_bit_string_2++;
    }
}

void
context_subtraction (context_t context_1, context_t context_2)
{
  bit_string_element_t *bit_string_bound_1;
  bit_string_element_t *bit_string_1;
  bit_string_element_t *bit_string_2;

  make_the_same_size (context_1, context_2);
  bit_string_1 = VLO_BEGIN (context_1->bit_string);
  bit_string_2 = VLO_BEGIN (context_2->bit_string);
  bit_string_bound_1
    = (bit_string_element_t *) ((char *) bit_string_1
                                + VLO_LENGTH (context_1->bit_string));
  while (bit_string_1 < bit_string_bound_1)
    {
      *bit_string_1 = *bit_string_1 & ~(*bit_string_2++);
      bit_string_1++;
    }
}

void
context_concat (context_t context_1, context_t context_2,
                int maximum_result_token_string_length)
{
  bit_string_element_t *bit_string_2;
  int bits_number_2;
  int bit_string_element_number_2;
  bit_string_element_t *bit_string_1;
  token_string_t token_string_2;
  int number_of_bit_string_elements_2;
  int bits_number_1;
  int bit_string_element_number_1;
  int number_of_bit_string_elements_1;
  token_string_t token_string_1;
  context_t result;

  if (it_is_zero_context (context_2))
    {
      zero_context (context_1);
      return;
    }
  bit_string_1 = VLO_BEGIN (context_1->bit_string);
  number_of_bit_string_elements_1
    = (unsigned) VLO_LENGTH (context_1->bit_string)
      / sizeof (bit_string_element_t);
  bit_string_2 = VLO_BEGIN (context_2->bit_string);
  number_of_bit_string_elements_2
    = (unsigned) VLO_LENGTH (context_2->bit_string)
      / sizeof (bit_string_element_t);
  result = get_null_context ();
  for (bit_string_element_number_1 = 0;
       bit_string_element_number_1 < number_of_bit_string_elements_1;
       bit_string_element_number_1++)
    if (bit_string_1 [bit_string_element_number_1])
       {
         for (bits_number_1 = sizeof (bit_string_element_t) * CHAR_BIT - 1;
              bits_number_1 >= 0;
              bits_number_1--)
           if (BIT (bit_string_1 + bit_string_element_number_1, bits_number_1))
             {
               token_string_1
                 = bit_string_element_number_1 * sizeof (bit_string_element_t)
                   * CHAR_BIT + bits_number_1;
               if (maximum_result_token_string_length
                   <= token_string_length (token_string_1))
		 {
		   set_context_element_value (result, token_string_1, 1);
		   continue;
		 }
               for (bit_string_element_number_2 = 0;
                    bit_string_element_number_2
                    < number_of_bit_string_elements_2;
                    bit_string_element_number_2++)
                 if (bit_string_2 [bit_string_element_number_2])
                   {
                     for (bits_number_2
                          = sizeof (bit_string_element_t) * CHAR_BIT - 1;
                          bits_number_2 >= 0;
                          bits_number_2--)
                       if (BIT (bit_string_2 + bit_string_element_number_2,
                                bits_number_2))
                         {
                           token_string_2
                             = bit_string_element_number_2
                               * sizeof (bit_string_element_t) * CHAR_BIT
                                 + bits_number_2;
                           set_context_element_value
                             (result,
                              token_string_concat
                              (token_string_1, token_string_2,
                               maximum_result_token_string_length), 1);
                         }
                   }
             }
       }
  context_copy (context_1, result);
  free_context (result);
}

void
process_context_token_strings
  (context_t context,
   void (*applied_function) (token_string_t token_string))
{
  bit_string_element_t *bit_string;
  int bits_number;
  int bit_string_element_number;
  int number_of_bit_string_elements;

  bit_string = VLO_BEGIN (context->bit_string);
  number_of_bit_string_elements
    = (unsigned) VLO_LENGTH (context->bit_string)
      / sizeof (bit_string_element_t);
  for (bit_string_element_number = 0;
       bit_string_element_number < number_of_bit_string_elements;
       bit_string_element_number++)
    if (bit_string [bit_string_element_number])
       {
         for (bits_number = sizeof (bit_string_element_t) * CHAR_BIT - 1;
              bits_number >= 0;
              bits_number--)
           if (BIT (bit_string + bit_string_element_number, bits_number))
             (*applied_function) (bit_string_element_number
                                  * sizeof (bit_string_element_t)
                                  * CHAR_BIT + bits_number);
       }
}

/* The following variable value is context which will be the result of
   context truncation being fulfilled. */

static context_t shortened_context;

/* The following variable value is maximal length of token strings of
   context which will be the result of context truncation being
   fulfilled. */

static int shortened_context_token_string_length;

static void
context_token_string_shortening (token_string_t token_string)
{
  set_context_element_value
    (shortened_context,
     token_string_shortening (token_string,
                              shortened_context_token_string_length),
     1);
}

context_t
context_shortening (context_t context, int maximum_result_token_string_length)
{
  shortened_context = get_null_context ();
  shortened_context_token_string_length = maximum_result_token_string_length;
  process_context_token_strings
    (context, context_token_string_shortening);
  return shortened_context;
}

/* The function must return the same hash key independently from bit
   string length. */

unsigned
context_hash_value (context_t context)
{
  unsigned result;
  bit_string_element_t *bit_string_bound;
  bit_string_element_t *bit_string;

  bit_string = VLO_BEGIN (context->bit_string);
  bit_string_bound
    = (bit_string_element_t *) ((char *) bit_string
                                + VLO_LENGTH (context->bit_string));
  result = 0;
  while (bit_string < bit_string_bound)
    {
      result += *bit_string++;
    }
  return result;
}

int
context_eq (context_t context_1, context_t context_2)
{
  bit_string_element_t *bit_string_bound_1;
  bit_string_element_t *bit_string_1;
  bit_string_element_t *bit_string_2;

  make_the_same_size (context_1, context_2);
  bit_string_1 = VLO_BEGIN (context_1->bit_string);
  bit_string_2 = VLO_BEGIN (context_2->bit_string);
  bit_string_bound_1
    = (bit_string_element_t *) ((char *) bit_string_1
                                + VLO_LENGTH (context_1->bit_string));
  while (bit_string_1 < bit_string_bound_1)
    {
      if (*bit_string_1++ != *bit_string_2++)
        return FALSE;
    }
  return TRUE;
}

/* The bit string table itself is represented by the following
   variable. */

static hash_table_t context_table;

/* The following variable value is null context which has been
   inserted into the table. */

context_t null_context_in_table;

static unsigned
context_hash_function (hash_table_entry_t context)
{
  return context_hash_value ((context_t) context);
}

static int
context_eq_function (hash_table_entry_t context_1,
                     hash_table_entry_t context_2)
{
  return context_eq ((context_t) context_1, (context_t) context_2);
}

static context_t
insert_context (context_t context)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (context_table, context, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) context;
  return (context_t) *entry_ptr;
}

context_t
insert_or_free_context (context_t context_outside_table)
{
  context_t context_in_table;

  context_in_table = insert_context (context_outside_table);
  if (context_in_table != context_outside_table)
    free_context (context_outside_table);
  return context_in_table;
}

static int first_context_element_output_flag;
static FILE *output_context_file;

static void
output_context_token_string (token_string_t token_string)
{
  if (!first_context_element_output_flag)
    output_string (output_context_file, ", ");
  first_context_element_output_flag = FALSE;
  output_token_string (token_string, output_context_file);
}

void
output_context (FILE *f, context_t context)
{
  output_context_file = f;
  output_char ('[', f);
  first_context_element_output_flag = TRUE;
  process_context_token_strings (context, output_context_token_string);
  output_char (']', f);
}

void
initiate_contexts (void)
{
  initiate_token_strings ();
  first_free_context = NULL;
  context_table = create_hash_table (5000, context_hash_function,
                                     context_eq_function);
  null_context_in_table = get_null_context ();
#ifndef NDEBUG
  assert (null_context_in_table == insert_context (null_context_in_table));
#else
  insert_context (null_context_in_table);
#endif
}

void
finish_contexts (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Context table: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (context_table),
       hash_table_elements_number (context_table),
       get_collisions (context_table));
#endif
  delete_hash_table (context_table);
  finish_token_strings ();
}

