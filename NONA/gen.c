/*
   FILE NAME:   gen.c

   Copyright (C) 1997-2016 Vladimir Makarov.

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

   TITLE:       generator of NONA (machine description translator)

   DESCRIPTION: This file generates interface and implementation file of
                the code selector description.

   SPECIAL CONSIDERATION:
         The generator is to be called after NONA semantic analyzer only
       if any error was not fixed.
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of the generator.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>
#include <stdio.h>

#include "position.h"
#include "errors.h"
#include "common.h"
#include "ird.h"
#include "gen.h"

#include <assert.h>



/* This page contains functions which are used to output CS.  These
   function are used to fix output errors in time.  All output of CS is
   made only through these function. */

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
          if (f == output_interface_file)
            system_error (TRUE, no_position, "fatal_error -- %s: ",
                          output_interface_file_name);
          else
            {
              assert (f == output_implementation_file);
              system_error (TRUE, no_position, "fatal_error -- %s: ",
                            output_implementation_file_name);
            }
        }
      if (*string == '\n')
        {
          if (f == output_interface_file)
            current_interface_file_line++;
          else
            {
              assert (f == output_implementation_file);
              current_implementation_file_line++;
            }
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
      if (f == output_interface_file)
        system_error (TRUE, no_position, "fatal_error -- %s: ",
                      output_interface_file_name);
      else
        {
          assert (f == output_implementation_file);
          system_error (TRUE, no_position, "fatal_error -- %s: ",
                        output_implementation_file_name);
        }
    }
  if (ch == '\n')
    {
      if (f == output_interface_file)
        current_interface_file_line++;
      else
        {
          assert (f == output_implementation_file);
          current_implementation_file_line++;
        }
    }
}

/* This function outputs integer number according to format `%d' onto
   interface or implementation file and fixes output errors. */

static void
output_decimal_number (FILE *f, int number)
{
  if (fprintf (f, "%d", number) == EOF)
    {
      if (f == output_interface_file)
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
/* ??? */
static void
initiate_output (void)
{
  current_interface_file_line = 1;
  current_implementation_file_line = 1;
  output_interface_file = fopen (output_interface_file_name, "w");
  if (output_interface_file == NULL)
    system_error (TRUE, no_position,
                  "fatal error -- %s: ", output_interface_file_name);
  output_implementation_file = fopen (output_implementation_file_name, "w");
  if (output_implementation_file == NULL)
    system_error (TRUE, no_position,
                  "fatal error -- %s: ", output_implementation_file_name);
}



/* This page contains functions which are used to output parameterized names
   of CS objects.  Usually the parameter is prefix given in NONA command
   line (see commentaries for variable `prefix'). */

/* Base name of debug macro parameter used to separate debug code in
   CS.  Full name of the macro parameter is `__CS_DEBUG__' (see function
   `output_ifdef_parameter_name'). */

#define DEBUG_PARAMETER_NAME "DEBUG"

/* This function outputs name of debug macro parameter used to
   separate debug code in CS.  The name is output as
   `__<prefix>_<base_name>__' where prefix is value of variable
   `prefix' and base name is value of the second parameter. */

static void
output_ifdef_parameter_name (FILE *f, const char *ifdef_parameter_name)
{
  output_string (f, "__");
  output_string (f, prefix);
  output_char ('_', f);
  output_string (f, ifdef_parameter_name);
  output_string (f, "__");
}
/* ??? */
static void
output_nonterminal_name (FILE *f, const char *nonterminal_name)
{
  output_string (f, prefix);
  output_string (f, "NT");
  output_char ('_', f);
  output_string (f, nonterminal_name);
}
/* ??? */
static void
output_decode_nonterminal_rule_vector_name (FILE *f,
                                            const char *nonterminal_name)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_char ('_', f);
  output_string (f, nonterminal_name);
  output_string (f, "_decode_rule_vector");
}
/* ??? */
static void
output_cs_node_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_NODE");
}
/* ??? */
static void
output_cs_node_type_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_node");
}
/* ??? */
static void
output_cs_operation_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_OPERATION");
}
/* ??? */
static void
output_cs_attribute_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_ATTRIBUTE");
}
/* ??? */
static void
output_cs_type_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_TYPE");
}
/* ??? */
static void
output_cs_error_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_ERROR");
}
/* ??? */
static void
output_cs_start_alloc_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_START_ALLOC");
}
/* ??? */
static void
output_cs_alloc_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_ALLOC");
}
/* ??? */
static void
output_cs_free_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_FREE");
}
/* ??? */
static void
output_cs_finish_alloc_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_FINISH_ALLOC");
}
/* ??? */
static void
output_operand_macro_name (FILE *f, int operand_number, int arity)
{
  output_string (f, prefix);
  output_string (f, "_OPERAND_");
  output_decimal_number (f, operand_number);
  output_string (f, "_OF_");
  output_decimal_number (f, arity);
}
/* ??? */
static void
output_cs_state_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_STATE");
}
/* ??? */
static void
output_internal_state_macro_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_STATE");
}
/* ??? */
static void
output_cs_set_state_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_SET_STATE");
}
/* ??? */
static void
output_cs_cover_type_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_cover");
}
/* ??? */
static void
output_state_struct_class_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_state");
}
/* ??? */
static void
output_cost_type_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_cost");
}
/* ??? */
static void
output_name_of_cs_closure_function (FILE *f, const char *nonterm_name)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_closure_");
  output_string (f, nonterm_name);
}
/* ??? */
static void
output_name_of_cs_state_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_state_");
}
/* ??? */
static void
output_name_of_cs_print_state_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_print_node_state");
}
/* ??? */
static void
output_name_of_cs_find_cover_pass_function (FILE *f, int first_pass_flag)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_find_cover_pass_");
  if (first_pass_flag)
    output_char ('1', f);
  else
    output_char ('2', f);
}
/* ??? */
static void
output_name_of_cs_find_cover_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_find_cover");
}
/* ??? */
static void
output_name_of_cs_nonterminal_rule_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_nonterminal_rule");
}
/* ??? */
static void
output_name_of_cs_it_is_axiom_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_it_is_axiom");
}
/* ??? */
static void
output_name_of_cs_pass_of_traverse_cover_function (FILE *f,
                                                   int first_pass_flag)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_traverse_cover_");
  if (first_pass_flag)
    output_char ('1', f);
  else
    output_char ('2', f);
}
/* ??? */
static void
output_name_of_cs_traverse_cover_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_traverse_cover");
}
/* ??? */
static void
output_name_of_cs_delete_cover_function (FILE *f, int internal_function_flag)
{
  if (internal_function_flag)
    output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "_delete_cover");
}
/* ??? */
static void
output_name_of_cs_start_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_start");
}
/* ??? */
static void
output_name_of_cs_finish_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_finish");
}



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
  output_decimal_number (f, line_number);
  output_string (f, " \"");
  output_string (f, file_name);
  output_string (f, "\"\n");
}
/*???*/
static void
output_current_line (FILE *f)
{
  output_string (f, "\n#line ");
  if (f == output_interface_file)
    {
      output_decimal_number (f, current_interface_file_line + 1);
      output_string (f, " \"");
      output_string (f, output_interface_file_name);
    }
  else
    {
      assert (f == output_implementation_file);
      output_decimal_number (f, current_implementation_file_line + 1);
      output_string (f, " \"");
      output_string (f, output_implementation_file_name);
    }
  output_string (f, "\"\n");
}



#define RESULT_ATTRIBUTE_NAME  "result"

#define NONTERMINAL_ATTRIBUTE_PREFIX_NAME  "nonterm_attr"

/*???*/
static void
output_pattern_access (IR_node_t pattern, const char *node_variable_name)
{
  IR_node_t father;
  IR_node_t current_pattern;
  int operand_number;

  father = IR_father (pattern);
  if (father == NULL)
    output_string (output_implementation_file, node_variable_name);
  else
    {
      for (operand_number = 1, current_pattern = IR_pattern_list (father);
           current_pattern != NULL && current_pattern != pattern;
           current_pattern = IR_next_pattern (current_pattern))
        operand_number++;
      assert (current_pattern != NULL);
      output_operand_macro_name
        (output_implementation_file,
         operand_number, IR_arity (IR_single_declaration (father)));
      output_string (output_implementation_file, " (");
      output_pattern_access (father, node_variable_name);
      output_char (')', output_implementation_file);
    }
}
/*???*/
static void
output_nonterm_attribute_variable_name (FILE *f, int nonterm_attribute_number)
{
  output_string (f, NONTERMINAL_ATTRIBUTE_PREFIX_NAME);
  output_char ('_', f);
  output_decimal_number (f, nonterm_attribute_number);
}

static IR_node_t
find_pattern (IR_node_t textual_pattern_list, int number)
{
  IR_node_t current_textual_pattern;
  IR_node_t last_textual_pattern;

  last_textual_pattern = textual_pattern_list;
  assert (last_textual_pattern != NULL);
  current_textual_pattern = last_textual_pattern;
  do
    {
      current_textual_pattern
        = IR_next_textual_pattern (current_textual_pattern);
      if (number == IR_original_number (current_textual_pattern))
        return current_textual_pattern;
    }
  while (current_textual_pattern != last_textual_pattern);
  return NULL;
}
/*???*/
static void
output_attribute (FILE *f, IR_node_t rule, int attribute_number,
                  const char *node_variable_name)
{
  IR_node_t pattern;
  IR_node_t single_declaration;

  if (attribute_number == 0)
    {
      single_declaration = IR_single_nonterm_declaration (rule);
      output_string (output_implementation_file, RESULT_ATTRIBUTE_NAME);
    }
  else
    {
      assert (attribute_number >= 0);
      pattern = find_pattern (IR_textual_pattern_list (rule),
                              attribute_number);
      assert (pattern != NULL);
      single_declaration = IR_single_declaration (pattern);
      if (IR_IS_OF_TYPE (single_declaration, IR_NM_single_term_declaration))
        {
          output_cs_attribute_macro_name (output_implementation_file);
          output_string (output_implementation_file, " (");
          output_pattern_access (pattern, node_variable_name);
          output_char (')', output_implementation_file);
        }
      else
        {
          assert (IR_IS_OF_TYPE (single_declaration,
                                 IR_NM_single_nonterm_declaration));
          output_nonterm_attribute_variable_name
            (output_implementation_file,
             IR_textual_nonterminal_pattern_number (pattern));
        }
    }
  if (IR_type (single_declaration) != NULL)
    {
      output_char ('.', f);
      output_string (output_implementation_file,
                     IR_identifier_itself (IR_type (single_declaration)));
    }
}
/*???*/
static void
output_rule_code (FILE *f, string_t code, IR_node_t rule,
                  const char *node_variable_name)
{

  assert (code != NULL);
  /* !! This code is bound to scanner */
  for (; *code != '\0'; code++)
    switch (*code)
      {
      case '$':
        if (code [1] == '$')
          {
            code++;
            output_attribute (f, rule, 0, node_variable_name);
          }
        else if (isdigit (code [1]))
          {
            output_attribute (f, rule, atoi (code + 1), node_variable_name);
            while (isdigit (code [1]))
              code++;
          }
        else
          output_char (*code, f);
        break;
      case '/':
        output_char (*code, f);
        if (code [1] == '*')
          {
            /* C comment. */
            code++;
            output_char (*code, f);
            for (;;)
              {
                code++;
                if (*code == '*')
                  {
                    output_char (*code, f);
                    code++;
                    if (*code == '/')
                      {
                        output_char (*code, f);
                        break;
                      }
                    else
                      {
                        code--;
                        continue;
                      }
                  }
                else if (*code == '\0')
                  {
                    code--;
                    break;
                  }
                else
                  output_char (*code, f);
              }
          }
        break;
      case '\'':
        output_char (*code, f);
        code++;
        if (*code == '\0' || *code == '\n')
          code--;
        else if (*code != '\'')
          {
            if (*code == '\\')
              {
                output_char (*code, f);
                code++;
                if (*code == 'n' || *code == 't' || *code == 'v'
                    || *code == 'b' || *code == 'r' || *code == 'f'
                    || *code == '\\' || *code == '\'' || *code == '\"')
                  output_char (*code, f);
                else if (*code == '\n')
                  output_char (*code, f);
                else if (isdigit (*code) && *code != '8' && *code != '9')
                  {
                    output_char (*code, f);
                    if (isdigit (code [1]) && code [1] != '8'
                        && code [1] != '9')
                      {
                        code++;
                        output_char (*code, f);
                        if (isdigit (code [1]) && code [1] != '8'
                            && code [1] != '9')
                          {
                            code++;
                            output_char (*code, f);
                          }
                      }
                  }
                else
                  code--;
              }
            else
              output_char (*code, f);
          }
        else
          output_char (*code, f);
        if (code [1] == '\'')
          {
            code++;
            output_char (*code, f);
          }
        break;
      case '\"':
        output_char (*code, f);
        for (;;)
          {
            code++;
            if (*code == '\"')
              {
                output_char (*code, f);
                break;
              }
            else if (*code == '\0' || *code == '\n')
              {
                code--;
                break;
              }
            else
              {
                if (*code == '\\')
                  {
                    output_char (*code, f);
                    code++;
                    if (*code == 'n' || *code == 't' || *code == 'v'
                        || *code == 'b' || *code == 'r' || *code == 'f'
                        || *code == '\\' || *code == '\'' || *code == '\"')
                      output_char (*code, f);
                    else if (*code == '\n')
                      output_char (*code, f);
                    else if (isdigit (*code) && *code != '8' && *code != '9')
                      {
                        output_char (*code, f);
                        if (isdigit (code [1]) && code [1] != '8'
                            && code [1] != '9')
                          {
                            code++;
                            output_char (*code, f);
                            if (isdigit (code [1]) && code [1] != '8'
                                && code [1] != '9')
                              {
                                code++;
                                output_char (*code, f);
                              }
                          }
                      }
                    else
                      code--;
                  }
                else
                  output_char (*code, f);
              }
          }
        break;
      default:
        output_char (*code, f);
        break;
      }
}
/*???*/
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

/*???*/
static int
output_union (void)
{
  IR_node_t current_declaration;

  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_union_code))
      {
        output_string (output_interface_file, "typedef union\n{");
        output_line (output_interface_file,
                     IR_position (current_declaration).line_number,
                     IR_position (current_declaration).file_name);
        output_string (output_interface_file,
                       IR_code_insertion_itself (IR_code_itself
                                                 (current_declaration)));
        output_char ('\n', output_interface_file);
        output_string (output_interface_file, "} ");
        output_cs_type_macro_name (output_interface_file);
        output_string (output_interface_file, ";\n");
        output_current_line (output_interface_file);
        return TRUE;
      }
  return FALSE;
}
/*???*/
static void
output_interface_file_macros (void)
{
  /* Output definition of macro `CS_NODE'. */
  output_string (output_interface_file, "#ifndef ");
  output_cs_node_macro_name (output_interface_file);
  output_string (output_interface_file, "\n#define ");
  output_cs_node_macro_name (output_interface_file);
  output_string (output_interface_file,
                 "  struct IR_node_struct *\n#endif\n\n");
  if (!output_union ())
    {
      /* Output definition of macro `CS_TYPE'. */
      output_string (output_interface_file, "#ifndef ");
      output_cs_type_macro_name (output_interface_file);
      output_string (output_interface_file, "\n#define ");
      output_cs_type_macro_name (output_interface_file);
      output_string (output_interface_file, "  ");
      output_cs_node_type_name (output_interface_file);
      output_string (output_interface_file, "\n#endif\n\n");
    }
}
/*???*/
static int
it_is_new_arity_terminal (IR_node_t single_term_declaration)
{
  IR_node_t current_single_declaration;
  int terminal_arity;

  assert (single_term_declaration != NULL);
  terminal_arity = IR_arity (single_term_declaration);
  for (current_single_declaration
       = IR_next_single_declaration (IR_single_declaration_list (description));
       single_term_declaration != current_single_declaration;
       current_single_declaration
       = IR_next_single_declaration (current_single_declaration))
    if (IR_IS_OF_TYPE (current_single_declaration,
                       IR_NM_single_term_declaration)
        && IR_arity (current_single_declaration) == terminal_arity)
      return FALSE;
  return TRUE;
}
/*???*/
static void
output_cs_operand_macros (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  int operand_number;
  int terminal_arity;

  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_term_declaration)
              && it_is_new_arity_terminal (current_single_declaration))
            {
              terminal_arity = IR_arity (current_single_declaration);
              for (operand_number = 1;
                   operand_number <= terminal_arity;
                   operand_number++)
                {
                  output_string (output_implementation_file, "#ifndef ");
                  output_operand_macro_name (output_implementation_file,
                                             operand_number, terminal_arity);
                  output_string (output_implementation_file, "\n#define ");
                  output_operand_macro_name (output_implementation_file,
                                             operand_number, terminal_arity);
                  output_string (output_implementation_file,
                                 "(node)  IR_operand");
                  if (terminal_arity > 1)
                    {
                      output_char ('_', output_implementation_file);
                      output_decimal_number (output_implementation_file,
                                             operand_number);
                    }
                  output_string (output_implementation_file,
                                 " (node)\n#endif\n\n");
                }
            }
        }
      while (current_single_declaration != last_single_declaration);
      output_char ('\n', output_implementation_file);
    }
}
/*???*/
static void
output_implementation_file_macros (void)
{
  /* Output definition of macro `CS_OPERATION'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_operation_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_operation_macro_name (output_implementation_file);
  output_string (output_implementation_file,
                 "(node)  IR_NODE_MODE (node)\n#endif\n\n");
  /* Output CS_OPERAND... macros. */
  output_cs_operand_macros ();
  /* Output definition of macro `CS_STATE'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_state_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_state_macro_name (output_implementation_file);
  output_string (output_implementation_file,
                 "(node)  IR_state (node)\n#endif\n\n");
  /* Output definition of macro `CS_SET_STATE'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_set_state_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_set_state_macro_name (output_implementation_file);
  output_string (output_implementation_file,
                 "(node, state)  IR_set_state (node, state)\n#endif\n\n");
  /* Output definition of macro `CS_ATTRIBUTE'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_attribute_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_attribute_macro_name (output_implementation_file);
  output_string (output_implementation_file, "(node)  (node)\n#endif\n\n");
  /* Output definition of macro `CS_ERROR'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_error_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_error_macro_name (output_implementation_file);
  output_string (output_implementation_file,
                 "(str)  fprintf (stderr, \"%s\\n\", str)\n#endif\n\n");
  /* Output definition of macro `CS_START_ALLOC'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_start_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_start_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "()\n#endif\n\n");
  /* Output definition of macro `CS_FINISH_ALLOC'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_finish_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_finish_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "()\n#endif\n\n");
  /* Output definition of macro `CS_ALLOC'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_alloc_macro_name (output_implementation_file);
  output_string
    (output_implementation_file,
     "(ptr, size, ptr_type)  ((ptr) = (ptr_type) malloc (size))\n#endif\n\n");
  /* Output definition of macro `CS_FREE'. */
  output_string (output_implementation_file, "#ifndef ");
  output_cs_free_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_cs_free_macro_name (output_implementation_file);
  output_string (output_implementation_file,
                 "(ptr, size)  free (ptr)\n#endif\n\n");
  /* Output definition of internal macro `_CS_STATE'. */
  output_string (output_implementation_file, "\n#define ");
  output_internal_state_macro_name (output_implementation_file);
  if (!cpp_flag)
    output_string (output_implementation_file, "(node)  ((struct ");
  else
    output_string (output_implementation_file, "(node)  ((class ");
  output_state_struct_class_name (output_implementation_file);
  output_string (output_implementation_file, " *) ");
  output_cs_state_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (node))\n\n");
}
/*???*/
static void
output_macros (void)
{
  output_interface_file_macros ();
  output_implementation_file_macros ();
}

#define NODE_STRUCT_STATE_MEMBER_NAME  "node"

#define UNION_STRUCT_STATE_MEMBER_NAME "costs_or_attrs"

#define NONTERMINAL_ATTRIBUTE_UNION_MEMBER_NAME  "nonterminal_attribute"

#define COST_UNION_MEMBER_NAME  "cost"

#define RULE_STRUCT_STATE_MEMBER_NAME  "rule"

#define PASS_FLAG_STRUCT_STATE_MEMBER_NAME  "pass_flag"
/*???*/
static void
output_nonterminal_attribute_member (FILE *f)
{
  output_string (f, UNION_STRUCT_STATE_MEMBER_NAME);
  output_char ('.', f);
  output_string (f, NONTERMINAL_ATTRIBUTE_UNION_MEMBER_NAME);
}
/*???*/
static void
output_cost_member (FILE *f)
{
  output_string (f, UNION_STRUCT_STATE_MEMBER_NAME);
  output_char ('.', f);
  output_string (f, COST_UNION_MEMBER_NAME);
}
/*???*/
static void
output_nonterminal_member_name (FILE *f, const char *nonterminal_name)
{
  output_string (f, "_CS_");
  output_string (f, nonterminal_name);
}
/*???*/
static void
output_terminal_definitions (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  int current_terminal_number;

  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration == NULL)
    return;
  current_single_declaration = last_single_declaration;
  current_terminal_number = 1;
  do
    {
      current_single_declaration
        = IR_next_single_declaration (current_single_declaration);
      if (IR_IS_OF_TYPE (current_single_declaration,
                         IR_NM_single_term_declaration))
        {
          output_string (output_interface_file, "#define ");
          output_string (output_interface_file,
                         IR_identifier_itself (IR_identifier
                                               (current_single_declaration)));
          output_char (' ', output_interface_file);
          output_decimal_number (output_interface_file,
                                 current_terminal_number);
          current_terminal_number++;
          output_char ('\n', output_interface_file);
        }
    }
  while (current_single_declaration != last_single_declaration);
  output_char ('\n', output_interface_file);
}
/*???*/
static void
output_nonterminal_definitions (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  int current_nonterminal_number;

  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration == NULL)
    return;
  current_single_declaration = last_single_declaration;
  current_nonterminal_number = 1;
  do
    {
      current_single_declaration
        = IR_next_single_declaration (current_single_declaration);
      if (IR_IS_OF_TYPE (current_single_declaration,
                         IR_NM_single_nonterm_declaration))
        {
          output_string (output_interface_file, "#define ");
          output_nonterminal_name
            (output_interface_file,
             IR_identifier_itself
             (IR_identifier (current_single_declaration)));
          output_char (' ', output_interface_file);
          output_decimal_number (output_interface_file,
                                 current_nonterminal_number);
          current_nonterminal_number++;
          output_char ('\n', output_interface_file);
        }
    }
  while (current_single_declaration != last_single_declaration);
  output_char ('\n', output_interface_file);
}

#define COST_TYPE_REPRESENTATION "int"
/*???*/
static void
output_cost_type_definition (FILE *f)
{
  output_string (f, "typedef ");
  output_string (f, COST_TYPE_REPRESENTATION);
  output_char (' ', f);
  output_cost_type_name (f);
  output_string (f, ";\n\n");
}

#define RULE_DECODE_VECTOR_ELEMENT_TYPE_REPRESENTATION  "short"

#define NUMBER_OF_RULES_ON_LINE  20
/*???*/
static void
output_decode_vector_of_nonterminal_rule (IR_node_t single_nonterm_declaration)
{
  IR_node_t current_nonterm_rule;
  IR_node_t last_nonterm_rule;
  int current_nonterm_rule_number;

  output_string (output_implementation_file, "static ");
  output_string (output_implementation_file,
                 RULE_DECODE_VECTOR_ELEMENT_TYPE_REPRESENTATION);
  output_char (' ', output_implementation_file);
  output_decode_nonterminal_rule_vector_name
    (output_implementation_file,
     IR_identifier_itself (IR_identifier (single_nonterm_declaration)));
  output_string (output_implementation_file, " [] =\n{\n  0");
  last_nonterm_rule = IR_nonterm_rule_list (single_nonterm_declaration);
  if (last_nonterm_rule != NULL)
    {
      current_nonterm_rule = last_nonterm_rule;
      current_nonterm_rule_number = 1;
      do
        {
          current_nonterm_rule
            = IR_next_nonterm_rule (current_nonterm_rule);
          if (current_nonterm_rule_number % NUMBER_OF_RULES_ON_LINE == 0)
            output_string (output_implementation_file, ",\n  ");
          else
            output_string (output_implementation_file, ", ");
          assert (current_nonterm_rule_number
                  == IR_nonterm_rule_number (current_nonterm_rule));
          output_decimal_number (output_implementation_file,
                                 IR_rule_number (current_nonterm_rule));
          current_nonterm_rule_number++;
        }
      while (current_nonterm_rule != last_nonterm_rule);
    }
  output_string (output_implementation_file, "\n};\n\n");
}
/*???*/
static int
power_of_two (int number)
{
  int result;
  
  result = 0;
  do
    {
      number /= 2;
      result++;
    }
  while (number != 0);
  return result;
}

static void output_title_of_cs_closure_function (FILE *f,
                                                 const char *nonterm_name,
                                                 int def_flag);
static void output_title_of_cs_state_function (FILE *f);
static void output_title_of_cs_print_state_function (FILE *f, int def_flag);
static void output_title_of_cs_find_cover_pass_function (FILE *f,
                                                         int first_pass_flag);
static void output_title_of_cs_nonterminal_rule_function (FILE *f,
                                                          int def_flag);
static void
output_title_of_cs_pass_of_traverse_cover_function (FILE *f,
                                                    int first_pass_flag,
                                                    int def_flag);
static void
output_title_of_cs_delete_cover_function (FILE *f, int internal_function_flag,
                                          int def_flag);
static void output_title_of_cs_start_function (FILE *f);
static void output_title_of_cs_finish_function (FILE *f);
static void output_title_of_cs_find_cover_function (FILE *f);
static void output_title_of_cs_it_is_axiom_function (FILE *f, int def_flag);
static void output_title_of_cs_traverse_cover_function (FILE *f, int def_flag);

/*???*/
static void
output_state_struct_class (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  FILE *output_file;

  output_file = cpp_flag ? output_interface_file : output_implementation_file;
  output_cost_type_definition (output_file);
  output_string (output_file, cpp_flag ? "class " : "struct ");
  output_state_struct_class_name (output_file);
  output_string (output_file, "\n{\n  ");
  output_cs_node_type_name (output_file);
  output_char (' ', output_file);
  output_string (output_file, NODE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_file, ";\n");
  output_string (output_file, "  union\n  {\n    ");
  output_cs_type_macro_name (output_file);
  output_char (' ', output_file);
  output_string (output_file, NONTERMINAL_ATTRIBUTE_UNION_MEMBER_NAME);
  output_string (output_file, " [");
  output_decimal_number (output_file, IR_number_of_nonterminals (description) + 1);
  output_string (output_file, "];\n");
  output_string (output_file, "    ");
  output_cost_type_name (output_file);
  output_char (' ', output_file);
  output_string (output_file, COST_UNION_MEMBER_NAME);
  output_string (output_file, " [");
  output_decimal_number (output_file, IR_number_of_nonterminals (description) + 1);
  output_string (output_file, "];\n");
  output_string (output_file, "  } ");
  output_string (output_file, UNION_STRUCT_STATE_MEMBER_NAME);
  output_string (output_file, ";\n");
  /* Output nonterminal pass flags. */
  output_string (output_file, "  char ");
  output_string (output_file, PASS_FLAG_STRUCT_STATE_MEMBER_NAME);
  output_string (output_file, " [");
  output_decimal_number (output_file, IR_number_of_nonterminals (description) + 1);
  output_string (output_file, "];\n");
  /* Output nonterminal rules. */
  output_string (output_file, "  struct\n  {\n");
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_nonterm_declaration))
            {
              output_string (output_file, "    unsigned int ");
              output_nonterminal_member_name
                (output_file,
                 IR_identifier_itself
                 (IR_identifier (current_single_declaration)));
              output_string (output_file, ": ");
              output_decimal_number
                (output_file, power_of_two (IR_number_of_nonterminal_rules
                                            (current_single_declaration)));
              output_string (output_file, ";\n");
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
  output_string (output_file, "  } ");
  output_string (output_file, RULE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_file, ";\n");
  if (cpp_flag)
    {
      /* Output closure functions declarations. */
      last_single_declaration = IR_single_declaration_list (description);
      if (last_single_declaration != NULL)
        {
          current_single_declaration = last_single_declaration;
          do
            {
              current_single_declaration
                = IR_next_single_declaration (current_single_declaration);
              if (IR_IS_OF_TYPE (current_single_declaration,
                                 IR_NM_single_nonterm_declaration)
                  && IR_chain_rule_list (current_single_declaration) != NULL)
                {
                  output_title_of_cs_closure_function
                    (output_file,
                     IR_identifier_itself (IR_identifier
                                           (current_single_declaration)),
                     FALSE);
                  output_string (output_file, ";\n\n");
                }
            }
          while (current_single_declaration != last_single_declaration);
        }
      output_string (output_file, "  friend ");
      output_title_of_cs_state_function (output_file);
      output_string (output_file, ";\n  ");
      output_title_of_cs_print_state_function (output_file, FALSE);
      output_string (output_file, ";\n");
      output_string (output_file, "  friend ");
      output_title_of_cs_find_cover_pass_function (output_file, FALSE);
      output_string (output_file, ";\n  ");
      output_title_of_cs_nonterminal_rule_function (output_file, FALSE);
      output_string (output_file, ";\n  ");
      output_title_of_cs_pass_of_traverse_cover_function (output_file,
                                                          TRUE, FALSE);
      output_string (output_file, ";\n  ");
      output_title_of_cs_pass_of_traverse_cover_function (output_file,
                                                          FALSE, FALSE);
      output_string (output_file, ";\n  ");
      output_title_of_cs_delete_cover_function (output_file, TRUE, FALSE);
      output_string (output_file, ";\n");
      output_string (output_file, "  ");
      output_state_struct_class_name (output_file);
      output_string (output_file, " ();\n");
      output_string (output_file, "  ");
      output_state_struct_class_name (output_file);
      output_string (output_file, " (const ");
      output_state_struct_class_name (output_file);
      output_string (output_file, "&);\n");
      output_string (output_file, "  ~");
      output_state_struct_class_name (output_file);
      output_string (output_file, " ();\n\n");
      output_string (output_file, "public:\n\n");
      output_string (output_file, "  friend ");
      output_title_of_cs_start_function (output_file);
      output_string (output_file, ";\n");
      output_string (output_file, "  friend ");
      output_title_of_cs_finish_function (output_file);
      output_string (output_file, ";\n");
      output_string (output_file, "  friend ");
      output_title_of_cs_find_cover_function (output_file);
      output_string (output_file, ";\n  ");
      output_title_of_cs_delete_cover_function (output_file, FALSE, FALSE);
      output_string (output_file, ";\n  ");
      output_title_of_cs_it_is_axiom_function (output_file, FALSE);
      output_string (output_file, ";\n  ");
      output_title_of_cs_traverse_cover_function (output_file, FALSE);
      output_string (output_file, ";\n");
    }
  output_string (output_file, "\n};\n\n");
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_nonterm_declaration))
            output_decode_vector_of_nonterminal_rule
              (current_single_declaration);
        }
      while (current_single_declaration != last_single_declaration);
    }
}
/*???*/
static void
output_cs_node_type_definition (void)
{
  output_string (output_interface_file, "typedef ");
  output_cs_node_macro_name (output_interface_file);
  output_char (' ', output_interface_file);
  output_cs_node_type_name (output_interface_file);
  output_string (output_interface_file, ";\n\n");
}
/*???*/
static void
output_cs_cover_type_definition (void)
{
  if (!cpp_flag)
    output_string (output_interface_file, "typedef struct ");
  else
    output_string (output_interface_file, "typedef class ");
  output_state_struct_class_name (output_interface_file);
  output_string (output_interface_file, " *");
  output_cs_cover_type_name (output_interface_file);
  output_string (output_interface_file, ";\n\n");
}

/*???*/
static void
output_type_definitions (void)
{
  if (export_flag)
    output_terminal_definitions ();
  output_nonterminal_definitions ();
  output_cs_node_type_definition ();
  output_cs_cover_type_definition ();
  output_state_struct_class ();
}

#define CS_CLOSURE_FUNCTION_STATE_PARAMETER_NAME  (cpp_flag ? "this" : "state")

#define CS_CLOSURE_FUNCTION_NODE_PARAMETER_NAME   "node"

#define CS_CLOSURE_FUNCTION_COST_PARAMETER_NAME   "cost"

#define CS_CLOSURE_FUNCTION_COST_VARIABLE_NAME    "all_cost"
/*???*/
static void
output_title_of_cs_closure_function (FILE *f, const char *nonterm_name,
                                     int def_flag)
{
  if (!cpp_flag)
    output_string (f, "static ");
  else
    output_string (f, "  ");
  output_string (f, "void ");
  if (cpp_flag && def_flag)
    {
      output_state_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_cs_closure_function (f, nonterm_name);
  if (!cpp_flag)
    {
      output_string (f, "\n  (struct ");
      output_state_struct_class_name (f);
      output_string (f, " *");
      output_string (f, CS_CLOSURE_FUNCTION_STATE_PARAMETER_NAME);
      output_string (f, ", ");
    }
  else
    output_string (f, "\n    (");
  output_cs_node_type_name (f);
  output_char (' ', f);
  output_string (f, CS_CLOSURE_FUNCTION_NODE_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f, CS_CLOSURE_FUNCTION_COST_PARAMETER_NAME);
  output_char (')', f);
}

static void
output_pattern (IR_node_t pattern)
{
  IR_node_t current_pattern;

  output_string (output_implementation_file,
                 IR_identifier_itself (IR_identifier (pattern)));
  for (current_pattern = IR_pattern_list (pattern);
       current_pattern != NULL;
       current_pattern = IR_next_pattern (current_pattern))
    {
      if (current_pattern == IR_pattern_list (pattern))
        output_string (output_implementation_file, " ( ");
      else
        output_string (output_implementation_file, ", ");
      output_pattern (current_pattern);
    }
  if (IR_pattern_list (pattern) != NULL)
    output_char (')', output_implementation_file);
}

static void
output_rule (IR_node_t rule)
{
  output_string (output_implementation_file,
                 IR_identifier_itself (IR_nonterm_identifier (rule)));
  output_string (output_implementation_file, " : ");
  output_pattern (IR_pattern (rule));
}

static void
output_rule_cost_addition (IR_node_t rule, const char *node_variable_name)
{
  if (IR_optional_cost (rule) != NULL)
    {
      output_line
        (output_implementation_file,
         IR_position (IR_optional_cost (rule)).line_number,
         IR_position (IR_optional_cost (rule)).file_name);
      output_string (output_implementation_file, " + ");
      output_rule_code
        (output_implementation_file,
         IR_expression_itself (IR_optional_cost (rule)), rule,
         node_variable_name);
      output_char (';', output_implementation_file);
      output_current_line (output_implementation_file);
    }
  else
    output_string (output_implementation_file, ";\n");
}


static void
output_pattern_nonterminal_cost_additions (IR_node_t pattern,
                                           const char *node_variable_name,
                                           const char *indent)
{
  IR_node_t current_pattern;

  assert (pattern != NULL);
  for (current_pattern = IR_pattern_list (pattern); current_pattern != NULL;
       current_pattern = IR_next_pattern (current_pattern))
    {
      if (IR_IS_OF_TYPE (IR_single_declaration (current_pattern),
                         IR_NM_single_term_declaration))
        output_pattern_nonterminal_cost_additions (current_pattern,
                                                   node_variable_name, indent);
      else
        {
          assert (IR_IS_OF_TYPE (IR_single_declaration (current_pattern),
                                 IR_NM_single_nonterm_declaration));
          output_char ('\n', output_implementation_file);
          output_string (output_implementation_file, indent);
          output_string (output_implementation_file, "    + ");
          output_internal_state_macro_name (output_implementation_file);
          output_string (output_implementation_file, " (");
          output_pattern_access (current_pattern, node_variable_name);
          output_string (output_implementation_file, ")->");
          output_cost_member (output_implementation_file);
          output_string (output_implementation_file, " [");
          output_nonterminal_name
            (output_implementation_file,
             IR_identifier_itself (IR_identifier
                                   (IR_single_declaration (current_pattern))));
          output_string (output_implementation_file, "]");
        }
    }
}

static void
output_matching_with_cost
  (IR_node_t rule, const char *state_variable_name,
   const char *node_variable_name, const char *cost_variable_name,
   const char *indent)
{
  IR_node_t lhs_single_nonterm_declaration;

  lhs_single_nonterm_declaration = IR_single_nonterm_declaration (rule);
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "if (");
  output_string (output_implementation_file, cost_variable_name);
  output_string (output_implementation_file, " < ");
  output_string (output_implementation_file, state_variable_name);
  output_string (output_implementation_file, "->");
  output_cost_member (output_implementation_file);
  output_string (output_implementation_file, " [");
  output_nonterminal_name (output_implementation_file,
                           IR_identifier_itself
                           (IR_identifier (lhs_single_nonterm_declaration)));
  output_string (output_implementation_file, "]");
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "  {\n");
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "    /* ");
  output_rule (rule);
  output_string (output_implementation_file, " */\n");
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "    ");
  output_string (output_implementation_file, state_variable_name);
  output_string (output_implementation_file, "->");
  output_cost_member (output_implementation_file);
  output_string (output_implementation_file, " [");
  output_nonterminal_name (output_implementation_file,
                           IR_identifier_itself
                           (IR_identifier (lhs_single_nonterm_declaration)));
  output_string (output_implementation_file, "] = ");
  output_string (output_implementation_file, cost_variable_name);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "    ");
  output_string (output_implementation_file, state_variable_name);
  output_string (output_implementation_file, "->");
  output_string (output_implementation_file, RULE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_implementation_file, ".");
  output_nonterminal_member_name
    (output_implementation_file,
     IR_identifier_itself (IR_identifier (lhs_single_nonterm_declaration)));
  output_string (output_implementation_file, " = ");
  output_decimal_number (output_implementation_file,
                         IR_nonterm_rule_number (rule));
  output_string (output_implementation_file, ";\n");
  if (IR_chain_rule_list (lhs_single_nonterm_declaration) != NULL)
    {
      output_string (output_implementation_file, indent);
      output_string (output_implementation_file, "    ");
      if (cpp_flag)
        {
          output_string (output_implementation_file, state_variable_name);
          output_string (output_implementation_file, "->");
        }
      output_name_of_cs_closure_function
        (output_implementation_file,
         IR_identifier_itself
         (IR_identifier (lhs_single_nonterm_declaration)));
      output_string (output_implementation_file, " (");
      if (!cpp_flag)
        {
          output_string (output_implementation_file, state_variable_name);
          output_string (output_implementation_file, ", ");
        }
      output_string (output_implementation_file, node_variable_name);
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file, cost_variable_name);
      output_string (output_implementation_file, ");\n");
    }
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "  }\n");
}

static void
output_closure_functions (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  IR_node_t current_chain_rule;
  IR_node_t last_chain_rule;

  /* Output closure functions forward declarations which are needed
     because of possibility of cycle in chain rules. */
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_nonterm_declaration)
              && IR_chain_rule_list (current_single_declaration) != NULL)
            {
              output_title_of_cs_closure_function
                (output_implementation_file,
                 IR_identifier_itself (IR_identifier
                                       (current_single_declaration)), FALSE);
              output_string (output_implementation_file, ";\n\n");
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
  /* Output closure functions theirself. */
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_nonterm_declaration))
            {
              last_chain_rule
                = IR_chain_rule_list (current_single_declaration);
              if (last_chain_rule != NULL)
                {
                  const char *indent;

                  current_chain_rule = last_chain_rule;
                  output_title_of_cs_closure_function
                    (output_implementation_file,
                     IR_identifier_itself (IR_identifier
                                           (current_single_declaration)),
                     TRUE);
                  output_string (output_implementation_file,
                                 "\n{\n  int ");
                  output_string (output_implementation_file,
                                 CS_CLOSURE_FUNCTION_COST_VARIABLE_NAME);
                  output_string (output_implementation_file, ";\n\n");
                  do
                    {
                      current_chain_rule
                        = IR_next_chain_rule (current_chain_rule);
                      if (IR_optional_constraint (current_chain_rule) != NULL)
                        {
                          output_string (output_implementation_file, "  if (");
                          output_line
                            (output_implementation_file,
                             IR_position (IR_optional_constraint
                                          (current_chain_rule)).line_number,
                             IR_position (IR_optional_constraint
                                          (current_chain_rule)).file_name);
                          output_rule_code
                            (output_implementation_file,
                             IR_expression_itself (IR_optional_constraint
                                                   (current_chain_rule)),
                             current_chain_rule,
                             CS_CLOSURE_FUNCTION_NODE_PARAMETER_NAME);
                          output_char (')', output_implementation_file);
                          output_current_line (output_implementation_file);
                          output_string (output_implementation_file,
                                         "    {\n");
                          indent = "      ";
                        }
                      else
                        indent = "  ";
                      output_string (output_implementation_file, indent);
                      output_string (output_implementation_file,
                                     CS_CLOSURE_FUNCTION_COST_VARIABLE_NAME);
                      output_string (output_implementation_file, " = ");
                      output_string (output_implementation_file,
                                     CS_CLOSURE_FUNCTION_COST_PARAMETER_NAME);
                      output_rule_cost_addition
                        (current_chain_rule,
                         CS_CLOSURE_FUNCTION_NODE_PARAMETER_NAME);
                      output_matching_with_cost
                        (current_chain_rule,
                         CS_CLOSURE_FUNCTION_STATE_PARAMETER_NAME,
                         CS_CLOSURE_FUNCTION_NODE_PARAMETER_NAME,
                         CS_CLOSURE_FUNCTION_COST_VARIABLE_NAME, indent);
                      if (IR_optional_constraint (current_chain_rule) != NULL)
                        output_string (output_implementation_file, "    }\n");
                    }
                  while (current_chain_rule != last_chain_rule);
                  output_string (output_implementation_file, "}\n\n");
                }
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
}

#define CS_STATE_FUNCTION_PARAMETER_NAME       "node"

#define CS_STATE_FUNCTION_COST_VARIABLE_NAME   "cost"

#define CS_STATE_FUNCTION_STATE_VARIABLE_NAME  "state"

static void
output_title_of_cs_state_function (FILE *f)
{
  output_string (f, (cpp_flag ? "class " : "struct "));
  output_state_struct_class_name (f);
  output_string (f, " *");
  output_name_of_cs_state_function (f);
  output_string (f, " (");
  output_cs_node_type_name (f);
  output_char (' ', f);
  output_string (f, CS_STATE_FUNCTION_PARAMETER_NAME);
  output_char (')', f);
}

static void
output_pattern_guard (IR_node_t pattern, const char *node_variable_name,
                      const char *indent)
{
  IR_node_t current_pattern;

  assert (pattern != NULL && IR_pattern_list (pattern) != NULL);
  for (current_pattern = IR_pattern_list (pattern); current_pattern != NULL;
       current_pattern = IR_next_pattern (current_pattern))
    {
      if (current_pattern != IR_pattern_list (pattern))
        {
          output_char ('\n', output_implementation_file);
          output_string (output_implementation_file, indent);
          output_string (output_implementation_file, "&& ");
        }
      if (IR_IS_OF_TYPE (IR_single_declaration (current_pattern),
                         IR_NM_single_term_declaration))
        {
          output_cs_operation_macro_name (output_implementation_file);
          output_string (output_implementation_file, " (");
          output_pattern_access (current_pattern, node_variable_name);
          output_string (output_implementation_file, ") == ");
          output_string
            (output_implementation_file,
             IR_identifier_itself (IR_identifier (current_pattern)));
          if (IR_pattern_list (current_pattern) != NULL)
            {
              output_char ('\n', output_implementation_file);
              output_string (output_implementation_file, indent);
              output_string (output_implementation_file, "&& ");
              output_pattern_guard (current_pattern,
                                    node_variable_name, indent);
            }
        }
      else
        {
          assert (IR_IS_OF_TYPE (IR_single_declaration (current_pattern),
                                 IR_NM_single_nonterm_declaration));
          output_internal_state_macro_name (output_implementation_file);
          output_string (output_implementation_file, " (");
          output_pattern_access (current_pattern, node_variable_name);
          output_string (output_implementation_file, ")->");
          output_string (output_implementation_file,
                         RULE_STRUCT_STATE_MEMBER_NAME);
          output_string (output_implementation_file, ".");
          output_nonterminal_member_name
            (output_implementation_file,
             IR_identifier_itself (IR_identifier (current_pattern)));
        }
    }
}

static void
output_matching_with_rule (IR_node_t term_rule)
{
  const char *indent;

  if (IR_pattern_list (IR_pattern (term_rule)) != NULL)
    {
      indent = "          ";
      output_string (output_implementation_file, "      if (");
      output_pattern_guard (IR_pattern (term_rule),
                            CS_STATE_FUNCTION_PARAMETER_NAME, indent);
      if (IR_optional_constraint (term_rule) != NULL)
        output_string (output_implementation_file, " &&");
    }
  else if (IR_optional_constraint (term_rule) != NULL)
    {
      output_string (output_implementation_file, "      if (");
      indent = "          ";
    }
  else
    indent = "      ";
  if (IR_optional_constraint (term_rule) != NULL)
    {
      output_line
        (output_implementation_file,
         IR_position (IR_optional_constraint (term_rule)).line_number,
         IR_position (IR_optional_constraint (term_rule)).file_name);
      output_rule_code
        (output_implementation_file,
         IR_expression_itself (IR_optional_constraint (term_rule)),
         term_rule, CS_STATE_FUNCTION_PARAMETER_NAME);
      output_char (')', output_implementation_file);
      output_current_line (output_implementation_file);
      output_string (output_implementation_file, "        {\n");
    }
  else if (IR_pattern_list (IR_pattern (term_rule)) != NULL)
    output_string (output_implementation_file, ")\n        {\n");
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file,
                 CS_STATE_FUNCTION_COST_VARIABLE_NAME);
  output_char ('\n', output_implementation_file);
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "  = 0");
  output_pattern_nonterminal_cost_additions
    (IR_pattern (term_rule), CS_STATE_FUNCTION_PARAMETER_NAME, indent);
  output_rule_cost_addition (term_rule, CS_STATE_FUNCTION_PARAMETER_NAME);
  output_matching_with_cost
    (term_rule, CS_STATE_FUNCTION_STATE_VARIABLE_NAME,
     CS_STATE_FUNCTION_PARAMETER_NAME, CS_STATE_FUNCTION_COST_VARIABLE_NAME,
     indent);
  if (IR_pattern_list (IR_pattern (term_rule)) != NULL
      || IR_optional_constraint (term_rule) != NULL)
    output_string (output_implementation_file, "        }\n");
}

static void
output_cs_state_function (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  IR_node_t current_term_rule;
  IR_node_t last_term_rule;
  int current_nonterminal_number;

  output_closure_functions ();
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_cs_state_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n  int ");
  output_string (output_implementation_file,
                 CS_STATE_FUNCTION_COST_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  if (!cpp_flag)
    output_string (output_implementation_file, "  struct ");
  else
    output_string (output_implementation_file, "  class ");
  output_state_struct_class_name (output_implementation_file);
  output_string (output_implementation_file, " *");
  output_string (output_implementation_file,
                 CS_STATE_FUNCTION_STATE_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n\n");
  output_string (output_implementation_file, "  ");
  output_cs_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file,
                 CS_STATE_FUNCTION_STATE_VARIABLE_NAME);
  if (!cpp_flag)
    output_string (output_implementation_file, ", sizeof (struct ");
  else
    output_string (output_implementation_file, ", sizeof (class ");
  output_state_struct_class_name (output_implementation_file);
  if (!cpp_flag)
    output_string (output_implementation_file, "), struct ");
  else
    output_string (output_implementation_file, "), class ");
  output_state_struct_class_name (output_implementation_file);
  output_string (output_implementation_file, " *);\n");
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 CS_STATE_FUNCTION_STATE_VARIABLE_NAME);
  output_string (output_implementation_file, "->");
  output_string (output_implementation_file, NODE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, CS_STATE_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  memset ((char *) &");
  output_string (output_implementation_file,
                 CS_STATE_FUNCTION_STATE_VARIABLE_NAME);
  output_string (output_implementation_file, "->");
  output_string (output_implementation_file, RULE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_implementation_file, ", 0, sizeof (");
  output_string (output_implementation_file,
                 CS_STATE_FUNCTION_STATE_VARIABLE_NAME);
  output_string (output_implementation_file, "->");
  output_string (output_implementation_file, RULE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_implementation_file, "));\n");
  for (current_nonterminal_number = 1;
       current_nonterminal_number <= IR_number_of_nonterminals (description);
       current_nonterminal_number++)
    {
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file,
                     CS_STATE_FUNCTION_STATE_VARIABLE_NAME);
      output_string (output_implementation_file, "->");
      output_cost_member (output_implementation_file);
      output_string (output_implementation_file, " [");
      output_decimal_number (output_implementation_file,
                             current_nonterminal_number);
      output_string (output_implementation_file, "] = 2147483647;\n");
    }
  output_string (output_implementation_file, "  switch (");
  output_cs_operation_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, CS_STATE_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n    {\n");
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_term_declaration))
            {
              last_term_rule = IR_term_rule_list (current_single_declaration);
              output_string (output_implementation_file, "    case ");
              output_string (output_implementation_file,
                             IR_identifier_itself
                             (IR_identifier (current_single_declaration)));
              output_string (output_implementation_file, ":\n");
              if (last_term_rule != NULL)
                {
                  current_term_rule = last_term_rule;
                  do
                    {
                      current_term_rule
                        = IR_next_term_rule (current_term_rule);
                      output_matching_with_rule (current_term_rule);
                    }
                  while (current_term_rule != last_term_rule);
                }
              output_string (output_implementation_file, "      break;\n");
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
  output_string (output_implementation_file, "    default:\n");
  output_string (output_implementation_file, "      abort ();\n");
  output_string (output_implementation_file, "      break;\n    }\n");
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file,
                 CS_STATE_FUNCTION_STATE_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "}\n\n");
}

#define CS_PRINT_STATE_FUNCTION_PARAMETER_NAME (cpp_flag ? "this" : "state")

#define CS_STATE_FUNCTION_STATE_VARIABLE_NAME  "state"

static void
output_title_of_cs_print_state_function (FILE *f, int def_flag)
{
  output_string (f, "void ");
  if (cpp_flag && def_flag)
    {
      output_state_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_cs_print_state_function (f);
  if (!cpp_flag)
    {
      output_string (f, " (struct ");
      output_state_struct_class_name (f);
      output_string (f, " *");
      output_string (f, CS_PRINT_STATE_FUNCTION_PARAMETER_NAME);
    }
  else
    output_string (f, "(void");
  output_char (')', f);
}

static void
output_cs_print_function (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  IR_node_t current_nonterm_rule;
  IR_node_t last_nonterm_rule;

  output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_cs_print_state_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, "\n{\n");
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_nonterm_declaration))
            {
              output_string (output_implementation_file, "  switch (");
              output_string (output_implementation_file,
                             CS_PRINT_STATE_FUNCTION_PARAMETER_NAME);
              output_string (output_implementation_file, "->");
              output_string (output_implementation_file,
                             RULE_STRUCT_STATE_MEMBER_NAME);
              output_string (output_implementation_file, ".");
              output_nonterminal_member_name
                (output_implementation_file,
                 IR_identifier_itself
                 (IR_identifier (current_single_declaration)));
              output_string (output_implementation_file, ")\n    {\n");
              last_nonterm_rule
                = IR_nonterm_rule_list (current_single_declaration);
              current_nonterm_rule = last_nonterm_rule;
              do
                {
                  current_nonterm_rule
                    = IR_next_nonterm_rule (current_nonterm_rule);
                  output_string (output_implementation_file, "    case ");
                  output_decimal_number
                    (output_implementation_file,
                     IR_nonterm_rule_number (current_nonterm_rule));
                  output_string (output_implementation_file, ":\n");
                  output_string
                    (output_implementation_file,
                     "      fprintf (stderr, \"       rule `");
                  output_rule (current_nonterm_rule);
                  output_string (output_implementation_file, "'\");\n");
                  output_string
                    (output_implementation_file,
                     "      fprintf (stderr, \", overall cost %d\\n\", ");
                  output_string (output_implementation_file,
                                 CS_PRINT_STATE_FUNCTION_PARAMETER_NAME);
                  output_string (output_implementation_file, "->");
                  output_cost_member (output_implementation_file);
                  output_string (output_implementation_file, " [");
                  output_nonterminal_name
                    (output_implementation_file, 
                     IR_identifier_itself (IR_identifier
                                           (current_single_declaration)));
                  output_string (output_implementation_file,"]);\n");
                  output_string (output_implementation_file, "      break;\n");
                }
              while (current_nonterm_rule != last_nonterm_rule);
              output_string
                (output_implementation_file,
                 "    default:\n      /* Nothing */\n      break;\n    }\n");
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
  output_string (output_implementation_file, "}\n");
  output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
  output_char ('\n', output_implementation_file);
}

#define CS_FIND_COVER_PARAMETER_NAME "node"

static void
output_title_of_cs_find_cover_pass_function (FILE *f, int first_pass_flag)
{
  if (!cpp_flag)
    output_string (f, "static ");
  if (first_pass_flag)
    output_string (f, "void");
  else
    output_cs_cover_type_name (f);
  output_char (' ', f);
  output_name_of_cs_find_cover_pass_function (f, first_pass_flag);
  output_string (f, " (");
  output_cs_node_type_name (f);
  output_char (' ', f);
  output_string (f, CS_FIND_COVER_PARAMETER_NAME);
  output_char (')', f);
}

static void
output_cs_find_cover_pass_function (int first_pass_flag)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  int current_operand_number;

  output_title_of_cs_find_cover_pass_function (output_implementation_file,
                                               first_pass_flag);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
  output_string (output_implementation_file, " == NULL)\n    {\n      ");
  output_cs_error_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (\"");
  output_name_of_cs_find_cover_function (output_implementation_file);
  output_string (output_implementation_file, ": tree is not complete\");\n");
  output_string (output_implementation_file, "      abort ();\n    }\n");
  if (!first_pass_flag)
    {
      output_string (output_implementation_file, "  if (");
      output_cs_state_macro_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, ") != NULL)\n    return (");
      output_cs_cover_type_name (output_implementation_file);
      output_string (output_implementation_file, ") ");
      output_cs_state_macro_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, ");\n");
    }      
  output_string (output_implementation_file, "  switch (");
  output_cs_operation_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n    {\n");
  /* Output processing children of node. */
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_term_declaration))
            {
              output_string (output_implementation_file, "    case ");
              output_string (output_implementation_file,
                             IR_identifier_itself
                             (IR_identifier (current_single_declaration)));
              output_string (output_implementation_file, ":\n");
              for (current_operand_number = 1;
                   current_operand_number
                   <= IR_arity (current_single_declaration);
                   current_operand_number++)
                {
                  output_string (output_implementation_file, "      ");
                  output_name_of_cs_find_cover_pass_function
                    (output_implementation_file, first_pass_flag);
                  output_string (output_implementation_file, " (");
                  output_operand_macro_name
                    (output_implementation_file, current_operand_number,
                     IR_arity (current_single_declaration));
                  output_string (output_implementation_file, " (");
                  output_string (output_implementation_file,
                                 CS_FIND_COVER_PARAMETER_NAME);
                  output_string (output_implementation_file, "));\n");
                }
              if (!first_pass_flag)
                {
                  output_ifdef (output_implementation_file,
                                DEBUG_PARAMETER_NAME);
                  output_string (output_implementation_file,
                                 "      fprintf (stderr, \"");
                  output_name_of_cs_find_cover_function
                    (output_implementation_file);
                  output_string (output_implementation_file, ": terminal ");
                  output_string (output_implementation_file,
                                 IR_identifier_itself
                                 (IR_identifier (current_single_declaration)));
                  output_string (output_implementation_file,
                                 ", state:\\n\");\n");
                  output_endif (output_implementation_file,
                                DEBUG_PARAMETER_NAME);
                }
              output_string (output_implementation_file, "      break;\n");
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
  output_string (output_implementation_file, "    default:\n      ");
  output_cs_error_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (\"");
  output_name_of_cs_find_cover_function (output_implementation_file);
  output_string (output_implementation_file, ": unknown operation code\");\n");
  output_string (output_implementation_file, "      abort ();\n");
  output_string (output_implementation_file, "      break;\n    }\n  ");
  output_cs_set_state_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
  if (first_pass_flag)
    output_string (output_implementation_file, ", NULL);\n}\n\n");
  else
    {
      output_string (output_implementation_file, ", ");
      output_name_of_cs_state_function (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "));\n");
      output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
      output_string (output_implementation_file, "  ");
      if (cpp_flag)
        {
          output_string (output_implementation_file,
                         CS_FIND_COVER_PARAMETER_NAME);
          output_string (output_implementation_file, "->");
        }
      output_name_of_cs_print_state_function (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_internal_state_macro_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      if (!cpp_flag)
        output_string (output_implementation_file,
                       CS_FIND_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "));\n");
      output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
      output_string (output_implementation_file, "  return (");
      output_cs_cover_type_name (output_implementation_file);
      output_string (output_implementation_file, ") ");
      output_cs_state_macro_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, ");\n}\n\n");
    }
}

static void
output_title_of_cs_find_cover_function (FILE *f)
{
  output_cs_cover_type_name (f);
  output_char (' ', f);
  output_name_of_cs_find_cover_function (f);
  output_string (f, " (");
  output_cs_node_type_name (f);
  output_char (' ', f);
  output_string (f, CS_FIND_COVER_PARAMETER_NAME);
  output_char (')', f);
}

static void
output_cs_find_cover_function (void)
{
  output_cs_state_function ();
  output_cs_print_function ();
  output_cs_find_cover_pass_function (TRUE);
  output_cs_find_cover_pass_function (FALSE);
  if (!cpp_flag)
    {
      /* Extern definition of find cover function */
      output_string (output_interface_file, "extern ");
      output_title_of_cs_find_cover_function (output_interface_file);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_cs_find_cover_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_cs_find_cover_pass_function (output_implementation_file,
                                              TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  output_string (output_implementation_file, "  return ");
  output_name_of_cs_find_cover_pass_function (output_implementation_file,
                                              FALSE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, CS_FIND_COVER_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n}\n\n");
}

#define CS_NONTERMINAL_RULE_COVER_PARAMETER_NAME  (cpp_flag ? "this" : "cover")

#define CS_NONTERMINAL_RULE_NONTERMINAL_PARAMETER_NAME  "nonterminal"

static void
output_title_of_cs_nonterminal_rule_function (FILE *f, int def_flag)
{
  output_string (f, "int ");
  if (cpp_flag && def_flag)
    {
      output_state_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_cs_nonterminal_rule_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_cs_cover_type_name (f);
      output_char (' ', f);
      output_string (f, CS_NONTERMINAL_RULE_COVER_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, CS_NONTERMINAL_RULE_NONTERMINAL_PARAMETER_NAME);
  output_char (')', f);
}

static void
output_cs_nonterminal_rule_function (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;

  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_cs_nonterminal_rule_function (output_implementation_file,
                                                TRUE);
  output_string (output_implementation_file, "\n{\n");
  if (! cpp_flag)
    {
      output_string (output_implementation_file, "  if (");
      output_string (output_implementation_file,
		     CS_NONTERMINAL_RULE_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, " == NULL)\n    return 0;\n");
    }
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 CS_NONTERMINAL_RULE_NONTERMINAL_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_nonterm_declaration))
            {
              output_string (output_implementation_file, "    case ");
              output_nonterminal_name
                (output_implementation_file,
                 IR_identifier_itself
                 (IR_identifier (current_single_declaration)));
              output_string (output_implementation_file, ":\n");
              output_string (output_implementation_file, "      return ");
              output_decode_nonterminal_rule_vector_name
                (output_implementation_file,
                 IR_identifier_itself
                 (IR_identifier (current_single_declaration)));
              output_string (output_implementation_file,
                             cpp_flag ? "\n               [((class "
                             : "\n               [((struct ");
              output_state_struct_class_name (output_implementation_file);
              output_string (output_implementation_file, " *) ");
              output_string (output_implementation_file,
                             CS_NONTERMINAL_RULE_COVER_PARAMETER_NAME);
              output_string (output_implementation_file, ")->");
              output_string (output_implementation_file,
                             RULE_STRUCT_STATE_MEMBER_NAME);
              output_string (output_implementation_file, ".");
              output_nonterminal_member_name
                (output_implementation_file,
                 IR_identifier_itself
                 (IR_identifier (current_single_declaration)));
              output_string (output_implementation_file, "];\n");
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
  output_string (output_implementation_file,
                 "    default:\n      return 0;\n    }\n");
  output_string (output_implementation_file, "}\n\n");
}

#define CS_IT_IS_AXIOM_COVER_PARAMETER_NAME   (cpp_flag ? "this" : "cover")

#define CS_IT_IS_AXIOM_NONTERMINAL_PARAMETER_NAME  "nonterminal"

static void
output_title_of_cs_it_is_axiom_function (FILE *f, int def_flag)
{
  output_string (f, "int ");
  if (cpp_flag && def_flag)
    {
      output_state_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_cs_it_is_axiom_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_cs_cover_type_name (f);
      output_char (' ', f);
      output_string (f, CS_IT_IS_AXIOM_COVER_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, CS_IT_IS_AXIOM_NONTERMINAL_PARAMETER_NAME);
  output_char (')', f);
}

static void
output_cs_it_is_axiom_function (void)
{
  output_cs_nonterminal_rule_function ();
  if (!cpp_flag)
    {
      /* Extern definition */
      output_string (output_interface_file, "extern ");
      output_title_of_cs_it_is_axiom_function (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_cs_it_is_axiom_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, "\n{\n");
  if (! cpp_flag)
    {
      output_string (output_implementation_file, "  if (");
      output_string (output_implementation_file,
		     CS_IT_IS_AXIOM_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, " == NULL)\n    {\n      ");
      output_cs_error_macro_name (output_implementation_file);
      output_string (output_implementation_file, " (\"");
      output_name_of_cs_it_is_axiom_function (output_implementation_file);
      output_string (output_implementation_file, ": incorrect cover\");\n");
      output_string (output_implementation_file, "      abort ();\n    }\n");
    }
  output_string (output_implementation_file, "  return ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_IT_IS_AXIOM_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_name_of_cs_nonterminal_rule_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_IT_IS_AXIOM_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
    }
  output_string (output_implementation_file,
                 CS_IT_IS_AXIOM_NONTERMINAL_PARAMETER_NAME);
  output_string (output_implementation_file, ") != 0;\n");
  output_string (output_implementation_file, "}\n\n");
}

#define CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME \
  (cpp_flag ? "this" : "cover")

#define CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME  "nonterminal"

static void
output_title_of_cs_traverse_cover_function (FILE *f, int def_flag)
{
  output_cs_type_macro_name (f);
  output_char (' ', f);
  if (cpp_flag && def_flag)
    {
      output_state_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_cs_traverse_cover_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_cs_cover_type_name (f);
      output_char (' ', f);
      output_string (f, CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
  output_char (')', f);
}

#define INTERNAL_CS_TRAVERSE_COVER_FUNCTION_NODE_VARIABLE_NAME     "node"

static void
output_title_of_cs_pass_of_traverse_cover_function (FILE *f,
                                                    int first_pass_flag,
                                                    int def_flag)
{
  if (!cpp_flag)
    output_string (f, "static ");
  if (first_pass_flag)
    output_string (f, "void");
  else
    output_cs_type_macro_name (f);
  output_char (' ', f);
  if (cpp_flag && def_flag)
    {
      output_state_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_cs_pass_of_traverse_cover_function (f, first_pass_flag);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_cs_cover_type_name (f);
      output_char (' ', f);
      output_string (f, CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
  output_char (')', f);
}

static void
output_nonterminal_attribute_evaluation (IR_node_t rule, int first_pass_flag)
{
  IR_node_t current_textual_pattern;
  IR_node_t last_textual_pattern;
  IR_node_t single_declaration;

  last_textual_pattern = IR_textual_pattern_list (rule);
  assert (last_textual_pattern != NULL);
  current_textual_pattern = last_textual_pattern;
  do
    {
      current_textual_pattern
        = IR_next_textual_pattern (current_textual_pattern);
      single_declaration = IR_single_declaration (current_textual_pattern);
      if (IR_IS_OF_TYPE (single_declaration, IR_NM_single_nonterm_declaration))
        {
          output_string (output_implementation_file, "      ");
          if (!first_pass_flag)
            {
              output_nonterm_attribute_variable_name
                (output_implementation_file,
                 IR_textual_nonterminal_pattern_number
                 (current_textual_pattern));
              output_string (output_implementation_file, "\n        = ");
            }
          if (cpp_flag)
            {
              output_internal_state_macro_name (output_implementation_file);
              output_string (output_implementation_file, " (");
              output_pattern_access
                (current_textual_pattern,
                 INTERNAL_CS_TRAVERSE_COVER_FUNCTION_NODE_VARIABLE_NAME);
              output_string (output_implementation_file, ")->");
            }
          output_name_of_cs_pass_of_traverse_cover_function
            (output_implementation_file, first_pass_flag);
          output_string (output_implementation_file, "\n          (");
          if (!cpp_flag)
            {
              output_internal_state_macro_name (output_implementation_file);
              output_string (output_implementation_file, " (");
              output_pattern_access
                (current_textual_pattern,
                 INTERNAL_CS_TRAVERSE_COVER_FUNCTION_NODE_VARIABLE_NAME);
              output_string (output_implementation_file, "), ");
            }
          output_nonterminal_name
            (output_implementation_file,
             IR_identifier_itself (IR_identifier (single_declaration)));
          output_string (output_implementation_file, ");\n");
        }
    }
  while (current_textual_pattern != last_textual_pattern);
}

static void
output_cs_traverse_cover_pass_function (int first_pass_flag)
{
  IR_node_t current_rule;
  int current_nonterminal_attribute_number;

  output_title_of_cs_pass_of_traverse_cover_function
    (output_implementation_file, first_pass_flag, TRUE);
  output_string (output_implementation_file, "\n{\n  ");
  output_cs_node_type_name (output_implementation_file);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file,
                 INTERNAL_CS_TRAVERSE_COVER_FUNCTION_NODE_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  if (!first_pass_flag)
    {
      output_string (output_implementation_file, "  ");
      output_cs_type_macro_name (output_implementation_file);
      output_char (' ', output_implementation_file);
      output_string (output_implementation_file, RESULT_ATTRIBUTE_NAME);
      output_string (output_implementation_file, ";\n");
      for (current_nonterminal_attribute_number = 1;
           current_nonterminal_attribute_number
           <= IR_max_number_of_rule_nonterminals (description);
           current_nonterminal_attribute_number++)
        {
          output_string (output_implementation_file, "  ");
          output_cs_type_macro_name (output_implementation_file);
          output_char (' ', output_implementation_file);
          output_nonterm_attribute_variable_name
            (output_implementation_file, current_nonterminal_attribute_number);
          output_string (output_implementation_file, ";\n");
        }
    }
  output_string (output_implementation_file, "\n");
  if (! cpp_flag)
    {
      output_string (output_implementation_file, "  if (");
      output_string (output_implementation_file,
		     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, " == NULL)\n    {\n      ");
      output_cs_error_macro_name (output_implementation_file);
      output_string (output_implementation_file, " (\"");
      output_name_of_cs_traverse_cover_function (output_implementation_file);
      output_string (output_implementation_file, ": incorrect cover\");\n");
      output_string (output_implementation_file, "      abort ();\n    }\n");
    }
  if (!first_pass_flag)
    {
      output_string (output_implementation_file, "  if (");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file,
                     PASS_FLAG_STRUCT_STATE_MEMBER_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
      output_string (output_implementation_file, "] != 0");
      output_string (output_implementation_file, ")\n    return ");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_nonterminal_attribute_member (output_implementation_file);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
      output_string (output_implementation_file, "];\n");
    }
  else
    {
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file,
                     PASS_FLAG_STRUCT_STATE_MEMBER_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
      output_string (output_implementation_file, "] = 0;\n");
    }
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_CS_TRAVERSE_COVER_FUNCTION_NODE_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file,
                 CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_string (output_implementation_file, NODE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  switch (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_name_of_cs_nonterminal_rule_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
    }
  output_string (output_implementation_file,
                 CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n    {\n");
  for (current_rule = IR_rule_list (description);
       current_rule != NULL;
       current_rule = IR_next_rule (current_rule))
    {
      output_string (output_implementation_file, "    case ");
      output_decimal_number (output_implementation_file,
                             IR_rule_number (current_rule));
      output_string (output_implementation_file, ":\n");
      if (first_pass_flag)
        {
          output_string (output_implementation_file, "      /* ");
          output_rule (current_rule);
          output_string (output_implementation_file, ". */\n");
        }
      output_nonterminal_attribute_evaluation (current_rule, first_pass_flag);
      if (!first_pass_flag)
        {
          output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
          output_string (output_implementation_file,
                         "      fprintf (stderr, \"");
          output_name_of_cs_traverse_cover_function
            (output_implementation_file);
          output_string (output_implementation_file, ": action for `");
          output_rule (current_rule);
          output_string (output_implementation_file, "'\\n\");\n");
          output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
        }
      if (!first_pass_flag && IR_optional_action (current_rule) != NULL)
        {
          output_line
            (output_implementation_file,
             IR_position (IR_optional_action (current_rule)).line_number,
             IR_position (IR_optional_action (current_rule)).file_name);
          output_char ('{', output_implementation_file);
          output_rule_code
            (output_implementation_file,
             IR_code_insertion_itself
             (IR_optional_action (current_rule)), current_rule,
             INTERNAL_CS_TRAVERSE_COVER_FUNCTION_NODE_VARIABLE_NAME);
          output_char ('}', output_implementation_file);
          output_current_line (output_implementation_file);
        }
      output_string (output_implementation_file, "      break;\n");
    }
  output_string (output_implementation_file, "    default:\n");
  output_string (output_implementation_file, "      ");
  output_cs_error_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (\"");
  output_name_of_cs_traverse_cover_function (output_implementation_file);
  output_string (output_implementation_file, ": incorrect cover\");\n");
  output_string (output_implementation_file, "      abort ();\n");
  output_string (output_implementation_file, "      break;\n");
  output_string (output_implementation_file, "    }\n");
  if (!first_pass_flag)
    {
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_nonterminal_attribute_member (output_implementation_file);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
      output_string (output_implementation_file, "] = ");
      output_string (output_implementation_file, RESULT_ATTRIBUTE_NAME);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file,
                     PASS_FLAG_STRUCT_STATE_MEMBER_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
      output_string (output_implementation_file, "] = 1;\n");
      output_string (output_implementation_file, "  return ");
      output_string (output_implementation_file, RESULT_ATTRIBUTE_NAME);
      output_string (output_implementation_file, ";\n");
    }
  output_string (output_implementation_file, "}\n\n");
}

static void
output_cs_traverse_cover_function (void)
{
  output_cs_traverse_cover_pass_function (TRUE);
  output_cs_traverse_cover_pass_function (FALSE);
  if (!cpp_flag)
    {
      /* Extern definition */
      output_string (output_interface_file, "extern ");
      output_title_of_cs_traverse_cover_function (output_interface_file,
                                                  FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_cs_traverse_cover_function (output_implementation_file,
                                              TRUE);
  output_string (output_implementation_file, "\n{\n");
  if (! cpp_flag)
    {
      output_string (output_implementation_file, "  if (");
      output_string (output_implementation_file,
		     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, " == NULL)\n    {\n      ");
      output_cs_error_macro_name (output_implementation_file);
      output_string (output_implementation_file, " (\"");
      output_name_of_cs_traverse_cover_function (output_implementation_file);
      output_string (output_implementation_file, ": incorrect cover\");\n");
      output_string (output_implementation_file, "      abort ();\n    }\n");
    }
  output_string (output_implementation_file, "  ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_name_of_cs_pass_of_traverse_cover_function
    (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
    }
  output_string (output_implementation_file,
                 CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  output_string (output_implementation_file, "  return ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_name_of_cs_pass_of_traverse_cover_function
    (output_implementation_file, FALSE);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_TRAVERSE_COVER_FUNCTION_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
    }
  output_string (output_implementation_file,
                 CS_TRAVERSE_COVER_FUNCTION_NONTERMINAL_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  output_string (output_implementation_file, "}\n\n");
}

#define CS_DELETE_COVER_PARAMETER_NAME (cpp_flag ? "this" : "cover")

#define INTERNAL_CS_DELETE_COVER_VARIABLE_NAME  "node"

static void
output_title_of_cs_delete_cover_function (FILE *f, int internal_function_flag,
                                          int def_flag)
{
  if (!cpp_flag && internal_function_flag)
    output_string (f, "static ");
  output_string (f, "void ");
  if (cpp_flag && def_flag)
    {
      output_state_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_cs_delete_cover_function (f, internal_function_flag);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_cs_cover_type_name (f);
      output_char (' ', f);
      output_string (f, CS_DELETE_COVER_PARAMETER_NAME);
    }
  else
    output_string (f, "void");
  output_char (')', f);
}

static void
output_internal_cs_delete_cover_function (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  int current_operand_number;

  /* Function itself */
  output_title_of_cs_delete_cover_function (output_implementation_file, TRUE,
                                            TRUE);
  output_string (output_implementation_file, "\n{\n  ");
  output_cs_node_type_name (output_implementation_file);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file,
                 INTERNAL_CS_DELETE_COVER_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n\n");
  if (! cpp_flag)
    {
      output_string (output_implementation_file, "  if (");
      output_string (output_implementation_file, CS_DELETE_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, " == NULL)\n");
      output_string (output_implementation_file,
		     "    /* Corresponding node has been processed. */\n");
      output_string (output_implementation_file, "    return;\n");
    }
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, CS_DELETE_COVER_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_string (output_implementation_file, NODE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_implementation_file, " == NULL)\n    {\n      ");
  output_cs_error_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (\"");
  output_name_of_cs_delete_cover_function (output_implementation_file, FALSE);
  output_string (output_implementation_file, ": incorrect cover\");\n");
  output_string (output_implementation_file, "      abort ();\n    }\n");
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_CS_DELETE_COVER_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, CS_DELETE_COVER_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_string (output_implementation_file, NODE_STRUCT_STATE_MEMBER_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  switch (");
  output_cs_operation_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file,
                 INTERNAL_CS_DELETE_COVER_VARIABLE_NAME);
  output_string (output_implementation_file, "))\n    {\n");
  /* Output processing children of node. */
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_term_declaration))
            {
              output_string (output_implementation_file, "    case ");
              output_string (output_implementation_file,
                             IR_identifier_itself
                             (IR_identifier (current_single_declaration)));
              output_string (output_implementation_file, ":\n");
              for (current_operand_number
                   = IR_arity (current_single_declaration);
                   current_operand_number > 0; current_operand_number--)
                {
                  output_string (output_implementation_file, "      ");
                  if (cpp_flag)
                    {
                      output_string (output_implementation_file, "((");
                      output_cs_cover_type_name (output_implementation_file);
                      output_string (output_implementation_file, ") ");
                      output_cs_state_macro_name (output_implementation_file);
                      output_string (output_implementation_file, " (");
                      output_operand_macro_name
                        (output_implementation_file, current_operand_number,
                         IR_arity (current_single_declaration));
                      output_string (output_implementation_file, " (");
                      output_string (output_implementation_file,
                                     INTERNAL_CS_DELETE_COVER_VARIABLE_NAME);
                      output_string (output_implementation_file, ")))->");
                    }
                  output_name_of_cs_delete_cover_function
                    (output_implementation_file, TRUE);
                  output_string (output_implementation_file, " (");
                  if (!cpp_flag)
                    {
                      output_string (output_implementation_file, "(");
                      output_cs_cover_type_name (output_implementation_file);
                      output_string (output_implementation_file, ") ");
                      output_cs_state_macro_name (output_implementation_file);
                      output_string (output_implementation_file, " (");
                      output_operand_macro_name
                        (output_implementation_file, current_operand_number,
                         IR_arity (current_single_declaration));
                      output_string (output_implementation_file, " (");
                      output_string (output_implementation_file,
                                     INTERNAL_CS_DELETE_COVER_VARIABLE_NAME);
                      output_string (output_implementation_file, "))");
                    }
                  output_string (output_implementation_file, ");\n");
                }
              output_string (output_implementation_file, "      break;\n");
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
  output_string (output_implementation_file, "    default:\n      ");
  output_cs_error_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (\"");
  output_name_of_cs_delete_cover_function (output_implementation_file, FALSE);
  output_string (output_implementation_file, ": unknown operation code\");\n");
  output_string (output_implementation_file, "      abort ();\n");
  output_string (output_implementation_file, "      break;\n    }\n  ");
  output_cs_free_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, CS_DELETE_COVER_PARAMETER_NAME);
  output_string (output_implementation_file,
                 cpp_flag ? ", sizeof (class " : ", sizeof (struct ");
  output_state_struct_class_name (output_implementation_file);
  output_string (output_implementation_file, "));\n  ");
  output_cs_set_state_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file,
                 INTERNAL_CS_DELETE_COVER_VARIABLE_NAME);
  output_string (output_implementation_file, ", NULL);\n}\n\n");
}

static void
output_cs_delete_cover_function (void)
{
  if (!cpp_flag)
    {
      /* Extern definition */
      output_string (output_interface_file, "extern ");
      output_title_of_cs_delete_cover_function (output_interface_file, FALSE,
                                                FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Output internal function for deletion of cover. */
  output_internal_cs_delete_cover_function ();
  /* Function itself */
  output_title_of_cs_delete_cover_function (output_implementation_file, FALSE,
                                            TRUE);
  output_string (output_implementation_file, "\n{\n");
  if (! cpp_flag)
    {
      output_string (output_implementation_file, "  if (");
      output_string (output_implementation_file, CS_DELETE_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, " == NULL)\n    {\n      ");
      output_cs_error_macro_name (output_implementation_file);
      output_string (output_implementation_file, " (\"");
      output_name_of_cs_delete_cover_function (output_implementation_file, FALSE);
      output_string (output_implementation_file, ": incorrect cover\");\n");
      output_string (output_implementation_file, "      abort ();\n    }\n  ");
    }
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     CS_DELETE_COVER_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_name_of_cs_delete_cover_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, CS_DELETE_COVER_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n}\n\n");
}

static void
output_title_of_cs_start_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_cs_start_function (f);
  output_string (f, " (void)");
}

static void
output_cs_start_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_cs_start_function (output_interface_file);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_cs_start_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  ");
  output_cs_start_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, " ();\n}\n\n");
}

static void
output_title_of_cs_finish_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_cs_finish_function (f);
  output_string (f, " (void)");
}

static void
output_cs_finish_function (void)
{
      /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_cs_finish_function (output_interface_file);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_cs_finish_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  ");
  output_cs_finish_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, " ();\n}\n\n");
}

static void
output_functions (void)
{
  output_cs_find_cover_function ();
  output_cs_it_is_axiom_function ();
  output_cs_traverse_cover_function ();
  output_cs_delete_cover_function ();
  output_cs_start_function ();
  output_cs_finish_function ();
}

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
output_statistics (void)
{
  int terminal_number;
  int commutative_terminal_number;
  int nonterminal_number;
  int rule_number;
  int new_commutative_rule_number;
  int chain_rule_number;
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  IR_node_t current_rule;

  terminal_number = 0;
  commutative_terminal_number = 0;
  nonterminal_number = 0;
  rule_number = 0;
  new_commutative_rule_number = 0;
  chain_rule_number = 0;
  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration != NULL)
    {
      current_single_declaration = last_single_declaration;
      do
        {
          current_single_declaration
            = IR_next_single_declaration (current_single_declaration);
          if (IR_IS_OF_TYPE (current_single_declaration,
                             IR_NM_single_term_declaration))
            {
              terminal_number++;
              if (IR_commutative_flag (current_single_declaration))
                commutative_terminal_number++;
            }
          else
            {
              assert (IR_IS_OF_TYPE (current_single_declaration,
                                     IR_NM_single_nonterm_declaration));
              nonterminal_number++;
            }
        }
      while (current_single_declaration != last_single_declaration);
    }
  for (current_rule = IR_rule_list (description);
       current_rule != NULL;
       current_rule = IR_next_rule (current_rule))
    {
      rule_number++;
      if (IR_new_commutative_rule_flag (current_rule))
        new_commutative_rule_number++;
      if (IR_pattern_list (IR_pattern (current_rule)) == NULL
          && IR_IS_OF_TYPE (IR_single_declaration (IR_pattern (current_rule)),
                            IR_NM_single_nonterm_declaration))
      chain_rule_number++;
    }
  printf ("nona: terminals:    all - %3d, commutative     - %3d\n",
          terminal_number, commutative_terminal_number);
  printf ("      nonterminals: all - %3d\n", nonterminal_number);
  printf
    ("      rules:        all - %3d, new commutative - %3d, chain - %3d\n",
     rule_number, new_commutative_rule_number, chain_rule_number);
}


/*
   FUNCTION NAME:   generate

   DESCRIPTION: 

   INPUT DATA:
       <None>

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
       No errors must be fixed before this function call.

   ANCHOR: <>
   SOURCE: <>
*/

void
generate (void)
{
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
  output_string (output_implementation_file, "#include <stdio.h>\n");
  output_string (output_implementation_file, "#include <string.h>\n");
  output_string (output_implementation_file, "#include <stdlib.h>\n");
  output_string (output_implementation_file, "#include \"");
  output_string (output_implementation_file, output_interface_file_name);
  output_string (output_implementation_file, "\"\n\n");
  output_start_code_insertions ();
  output_macros ();
  output_type_definitions ();
  output_functions ();
  output_finish_code_insertions ();
  output_additional_code ();
  output_endif (output_interface_file, description_name);
  if (v_flag)
    output_statistics ();
}
