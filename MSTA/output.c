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

/* This file contains common functions for output of parser code and
   parser decsription. */

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include "position.h"
#include "errors.h"
#include "vlobject.h"
#include "common.h"
#include "ird.h"
#include "output.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif



/* This page contains functions which are used to output MSTA.  These
   function are used to fix output errors in time.  All output of MSTA
   is made only through these function. */

/* The following variable value is number of current line in the
   description file. */

static int current_description_file_line;

/* The following variable value is number of current line in the
   interface file. */

static int current_interface_file_line;

/* The following variable value is number of current line in the
   implementation file. */

static int current_implementation_file_line;


void
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
            system_error (TRUE, no_position, "fatal error -- %s: ",
                          output_interface_file_name);
          else if (f == output_implementation_file)
            system_error (TRUE, no_position, "fatal_error -- %s: ",
                          output_implementation_file_name);
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

void
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

void
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
      else
        {
          assert (f == output_implementation_file);
          system_error (TRUE, no_position, "fatal_error -- %s: ",
                        output_implementation_file_name);
        }
    }
}

void
initiate_output (void)
{
  current_interface_file_line = 1;
  current_implementation_file_line = 1;
  if (verbose_flag)
    {
      current_description_file_line = 1;
      output_description_file = fopen (output_description_file_name, "w");
      if (output_description_file == NULL)
        system_error (TRUE, no_position,
                      "fatal error -- %s: ", output_description_file_name);
    }
  if (define_flag)
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



/* The following function adds representation of given identifier or
   literal to given variable length object.  The function returns
   length of the representation. */

int
identifier_or_literal_representation (IR_node_t identifier_or_literal,
                                      int in_string_flag,
                                      vlo_t *representation)
{
  int output_length;
  char *str;

  if (IR_IS_OF_TYPE (identifier_or_literal, IR_NM_identifier))
    str = IR_identifier_itself (identifier_or_literal);
  else
    str = IR_character_representation (identifier_or_literal);
  /* Add zero byte if it is absent. */
  VLO_ADD_STRING (*representation, "");
  /* Remove zero byte. */
  VLO_SHORTEN (*representation, 1);
  for (output_length = 0; *str != '\0'; str++)
    {
      assert (isprint (*str));
      if (in_string_flag && (*str == '\\' || *str == '"'))
        {
          VLO_ADD_BYTE (*representation, '\\');
          output_length++;
        }
      VLO_ADD_BYTE (*representation, *str);
      output_length++;
    }
  VLO_ADD_BYTE (*representation, '\0');
  return output_length;
}

int
output_identifier_or_literal (FILE *f, IR_node_t identifier_or_literal,
                              int in_string_flag)
{
  int output_length;
  vlo_t representation;

  VLO_CREATE (representation, 100);
  output_length
    = identifier_or_literal_representation (identifier_or_literal,
                                            in_string_flag, &representation);
  output_string (f, VLO_BEGIN (representation));
  VLO_DELETE (representation);
  return output_length;
}

/* The following function adds representation of given single
   definition to given variable length object. */

void
single_definition_representation (IR_node_t single_definition,
                                  vlo_t *representation)
{
  char number_representation [30];

  identifier_or_literal_representation
    (IR_identifier_or_literal (single_definition), FALSE, representation);
  if (IR_IS_OF_TYPE (IR_identifier_or_literal (single_definition),
                     IR_NM_literal))
    {
      VLO_ADD_STRING (*representation, "(");
      sprintf (number_representation, "%d", IR_value (single_definition));
      VLO_ADD_STRING (*representation, number_representation);
      VLO_ADD_STRING (*representation, ")");
    }
  if (IR_IS_OF_TYPE (single_definition, IR_NM_literal_range_definition))
    {
      VLO_ADD_STRING (*representation, "-");
      identifier_or_literal_representation
        (IR_right_range_bound_literal (single_definition), FALSE,
         representation);
      VLO_ADD_STRING (*representation, "(");
      sprintf (number_representation, "%d",
               IR_right_range_bound_value (single_definition));
      VLO_ADD_STRING (*representation, number_representation);
      VLO_ADD_STRING (*representation, ")");
    }
}

void
output_single_definition (FILE *f, IR_node_t single_definition)
{
  vlo_t representation;

  VLO_CREATE (representation, 100);
  single_definition_representation (single_definition, &representation);
  output_string (f, VLO_BEGIN (representation));
  VLO_DELETE (representation);
}



void
output_line (FILE *f, int line_number, const char *file_name)
{
  output_string (f, "\n#line ");
  output_decimal_number (f, line_number, 0);
  output_string (f, " \"");
  output_string (f, file_name);
  output_string (f, "\"\n");
}

void
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

