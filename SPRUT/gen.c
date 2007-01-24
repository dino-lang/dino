/*
   FILE NAME:   gen.c

   Copyright (C) 1997-2005 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

   TITLE:       Standard procedural interface (SPI) generator
                of SPRUT (internal representation description translator)

   DESCRIPTION: This file generates SPI.

   SPECIAL CONSIDERATION:
         The generator is to be called after SPRUT semantic analyzer only
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

#include "allocate.h"
#include "vlobject.h"
#include "position.h"
#include "errors.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
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


/* This page contains macros common for all generator. */

/* The following macro value is string which represents type of a
   vlaue of graph pass number. */

#define GRAPH_PASS_NUMBER_TYPE_NAME "int"

/* The following macro value is string used in SPI instead of name `%root'. */

#define ROOT_NAME  "_root"

/* The following macro value is string used in SPI for error nodes and their
   type. */

#define ERROR_NAME  "_error"

/* The rest macros represent names of members of the following structure
   which implements double linked fields

typedef struct _IR_double_link *IR_double_link_t;

struct _IR_double_link
  {
    IR_node_t field_itself;
    IR_node_t link_owner;
    void (* set_function) (IR_node_t, IR_node_t);
    IR_double_link_t previous_link;
    IR_double_link_t next_link;
  }

or 

typedef class _IR_double_link *IR_double_link_t;

class _IR_double_link
  {
    IR_node_t field_itself;
    IR_node_t link_owner;
    void (IR_node::*set_function) (IR_node_t);
    class _IR_double_link *previous_link;
    class _IR_double_link *next_link;
  public:
    IR_double_link_t IR__next_double_link (void);
    IR_double_link_t IR__previous_double_link (void);
    IR_node_t IR__owner (void);
    void IR__set_double_link (IR_node_t value, int disp, int flag);
  }

*/

/* The following macro value is name of member representing field itself
   for double linked field.  The double linked field is represented
   by structure. */

#define DOUBLE_LINK_FIELD_NAME        "field_itself"

/* The following macro value is name of member representing pointer to
   node in which the double linked field is placed. */

#define DOUBLE_LINK_OWNER_NAME        "link_owner"

/* The following macro value is pointer to function for modification
   of this double linked field value. */

#define DOUBLE_LINK_SET_FUNCTION_NAME "set_function"

/* The following macro value is name of member representing pointer to
   previous double linked field which refers to the same node, i.e.
   has the same value of member representing field itself. */

#define DOUBLE_LINK_PREVIOUS_NAME     "previous_link"

/* The following macro value is name of member representing pointer to
   next double linked field which refers to the same node, i.e.
   has the same value of member representing field itself. */

#define DOUBLE_LINK_NEXT_NAME         "next_link"



/* This page contains functions used by the generator for traversing
   internal representations. */

/* The following variable contains path of node types which are being
   processed during traversing node types (with the aid of all
   traversing functions). */

static vlo_t traverse_node_type_path;

/* The following function initiates traversing internal
   representation. */

static void
initiate_traversing_internal_representation (void)
{
  VLO_CREATE (traverse_node_type_path, 0);
}

/* This function processes all node types of current (one-level)
   description.  Remember that one level description contains only
   node type declarations which are in the table of types and such
   node type declarations contains all fields of the description (see
   file `anal.c'). */

static void
traverse_node_types (void (*applied_function) (IR_node_t node_type))
{
  IR_node_t node_type;

  if (IR_last_type (current_description_part) == NULL)
    return;
  for (node_type = IR_next_type (IR_last_type (current_description_part));;
       node_type = IR_next_type (node_type))
    {
      if (IR_NODE_MODE (node_type) == IR_NM_node_type)
        {
          VLO_ADD_MEMORY (traverse_node_type_path,
                          &node_type, sizeof (node_type));
          (*applied_function) (node_type);
          VLO_SHORTEN (traverse_node_type_path, sizeof (node_type));
        }
      else
        assert (IR_NODE_MODE (node_type) == IR_NM_predefined_type);
      if (node_type == IR_last_type (current_description_part))
        break;
    }
}

/* This function processes all fields of current (one-level)
   description.  Remember that one level description contains only
   node type declarations which are in the table of types and such
   node type declarations contains all fields of the description (see
   file `anal.c'). */

static void
traverse_all_fields (void (*applied_function) (IR_node_t field))
{
  IR_node_t node_type;
  IR_node_t current_field;

  if (IR_last_type (current_description_part) == NULL)
    return;
  for (node_type = IR_next_type (IR_last_type (current_description_part));;
       node_type = IR_next_type (node_type))
    {
      if (IR_NODE_MODE (node_type) == IR_NM_node_type)
        {
          VLO_ADD_MEMORY (traverse_node_type_path,
                          &node_type, sizeof (node_type));
          if (IR_last_field (node_type) != NULL)
            for (current_field = IR_next_field (IR_last_field (node_type));;
                 current_field = IR_next_field (current_field))
              {
                if (IR_NODE_MODE (current_field) == IR_NM_field)
                  (*applied_function) (current_field);
                else
                  assert (IR_NODE_MODE (current_field) == IR_NM_action
                          || IR_NODE_MODE (current_field) == IR_NM_constraint);
                if (current_field == IR_last_field (node_type))
                  break;
              }
          VLO_SHORTEN (traverse_node_type_path, sizeof (node_type));
        }
      else
        assert (IR_NODE_MODE (node_type) == IR_NM_predefined_type);
      if (node_type == IR_last_type (current_description_part))
        break;
    }
}

/* This function processes all fields of given node type.  The
   function must be called (through several intermediate functions)
   only from function `traverse_all_node_types'. */

static void
traverse_all_node_type_fields (IR_node_t node_type,
                               void (*applied_function) (IR_node_t field))
{
  IR_node_t super_type;
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  for (curr_supertype_list_element
       = IR_first_super_type_list_element (node_type);
       curr_supertype_list_element != NULL;
       curr_supertype_list_element
       = IR_next_super_type_list_element (curr_supertype_list_element))
    if (IR_immediate_super_type (curr_supertype_list_element) != NULL)
      {
        super_type = IR_immediate_super_type (curr_supertype_list_element);
        VLO_ADD_MEMORY (traverse_node_type_path,
                        &super_type, sizeof (super_type));
        traverse_all_node_type_fields (super_type, applied_function);
        VLO_SHORTEN (traverse_node_type_path, sizeof (super_type));
      }
  if (IR_last_field (node_type) != NULL)
    for (current_field = IR_next_field (IR_last_field (node_type));;
         current_field = IR_next_field (current_field))
      {
        assert (IR_NODE_MODE (current_field) == IR_NM_field
                || IR_NODE_MODE (current_field) == IR_NM_action
                || IR_NODE_MODE (current_field) == IR_NM_constraint);
        (*applied_function) (current_field);
        if (current_field == IR_last_field (node_type))
          break;
      }
}

/* This function processes all fields of given node type in reverse
   order (the last declared field is processed by the first).  The
   function must be called (through several intermediate functions)
   only from function `traverse_all_node_types'. */

static void
reverse_traverse_all_node_type_fields
  (IR_node_t node_type, void (*applied_function) (IR_node_t field))
{
  IR_node_t super_type;
  IR_node_t current_field;
  IR_node_t *current_field_ref;
  IR_node_t curr_supertype_list_element;
  vlo_t fields_in_reverse_order;

  assert (VLO_LENGTH (traverse_node_type_path) != 0);
  /* Output node type field definitions */
  if (IR_last_field (node_type) != NULL)
    {
      VLO_CREATE (fields_in_reverse_order, 40);
      for (current_field = IR_next_field (IR_last_field (node_type));;
           current_field = IR_next_field (current_field))
        {
          VLO_ADD_MEMORY (fields_in_reverse_order, &current_field,
                          sizeof (current_field));
          if (current_field == IR_last_field (node_type))
            break;
        }
      current_field_ref
        = (IR_node_t *) ((char *) VLO_END (fields_in_reverse_order)
                         + 1 - sizeof (current_field));
      for (;;current_field_ref--)
        {
          assert (IR_NODE_MODE (*current_field_ref) == IR_NM_field
                  || IR_NODE_MODE (*current_field_ref) == IR_NM_action
                  || IR_NODE_MODE (*current_field_ref) == IR_NM_constraint);
          (*applied_function) (*current_field_ref);
          if (current_field_ref
              == (IR_node_t *) VLO_BEGIN (fields_in_reverse_order))
            break;
        }
      VLO_DELETE (fields_in_reverse_order);
    }
  for (curr_supertype_list_element
       = IR_first_super_type_list_element (node_type);
       curr_supertype_list_element != NULL;
       curr_supertype_list_element
       = IR_next_super_type_list_element (curr_supertype_list_element))
    if (IR_immediate_super_type (curr_supertype_list_element) != NULL)
      {
        super_type = IR_immediate_super_type (curr_supertype_list_element);
        VLO_ADD_MEMORY (traverse_node_type_path,
                        &super_type, sizeof (super_type));
        reverse_traverse_all_node_type_fields (super_type, applied_function);
        VLO_SHORTEN (traverse_node_type_path, sizeof (super_type));
      }
}

/* The following function finishes traversing internal
   representation. */

static void
finish_traversing_internal_representation (void)
{
  VLO_DELETE (traverse_node_type_path);
}




/* This page contains function used for searching last field in super
   types of given node type. */

/* This recursive function used for searching for the last (class or
   non-class) field in super types of given node type.  The function
   returns The last (class or non-class) field in super types of given
   node type. */

static IR_node_t
last_field_in_super_type (IR_node_t node_type, int class_field_flag)
{
  IR_node_t current_field;
  vlo_t super_types_in_reverse_order;
  IR_node_t result;
  IR_node_t curr_supertype_list_element;
  IR_node_t *curr_supertype_list_element_ref;

  VLO_CREATE (super_types_in_reverse_order, 40);
  for (curr_supertype_list_element
       = IR_first_super_type_list_element (node_type);
       curr_supertype_list_element != NULL;
       curr_supertype_list_element
       = IR_next_super_type_list_element (curr_supertype_list_element))
    VLO_ADD_MEMORY (super_types_in_reverse_order,
                    &curr_supertype_list_element,
                    sizeof (curr_supertype_list_element));
  curr_supertype_list_element_ref
    = (IR_node_t *) ((char *) VLO_END (super_types_in_reverse_order)
                     + 1 - sizeof (curr_supertype_list_element));
  result = NULL;
  if (IR_first_super_type_list_element (node_type) != NULL)
    for (;;curr_supertype_list_element_ref--)
      {
        node_type = IR_immediate_super_type (*curr_supertype_list_element_ref);
        if (node_type != NULL)
          {
            if (IR_last_field (node_type) != NULL)
              for (current_field = IR_next_field (IR_last_field (node_type));;
                   current_field = IR_next_field (current_field))
                {
                  if (IR_NODE_MODE (current_field) == IR_NM_field)
                    {
                      if ((IR_declaration_part (current_field) != DP_CLASS)
                          == !class_field_flag)
                        result = current_field;
                    }
                  else
                    assert
                      (IR_NODE_MODE (current_field) == IR_NM_action
                       || IR_NODE_MODE (current_field) == IR_NM_constraint);
                  if (current_field == IR_last_field (node_type))
                    break;
                }
            if (result == NULL)
              result = last_field_in_super_type (node_type, class_field_flag);
            else
              break;
          }
        if (curr_supertype_list_element_ref
            == (IR_node_t *) VLO_BEGIN (super_types_in_reverse_order))
          break;
      }
  VLO_DELETE (super_types_in_reverse_order);
  return result;
}



/* This page contains functions which are used to output SPI.  These
   function are used to fix output errors in time.  All output of SPI is
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
          else if (f == output_implementation_file)
            current_implementation_file_line++;
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
      else if (f == output_implementation_file)
        current_implementation_file_line++;
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
   of SPI objects.  Usually the parameter is prefix given in SPRUT command
   line (see commentaries for variable `prefix'). */

/* Base name of debug macro parameter used to separate debug code in
   SPI.  Full name of the macro parameter is `__IR_DEBUG__' (see function
   `output_ifdef_parameter_name'). */

#define DEBUG_PARAMETER_NAME "DEBUG"

/* This function outputs name of debug macro parameter used to
   separate debug code in SPI.  The name is output as
   `__<prefix><base_name>__' where prefix is value of variable
   `prefix' and base name is value of the second parameter. */

static void
output_ifdef_parameter_name (FILE *f, const char *ifdef_parameter_name)
{
  output_string (f, "__");
  output_string (f, prefix);
  output_string (f, ifdef_parameter_name);
  output_string (f, "__");
}

/* This function outputs name of macro used to evaluation of member
   offset in structure.  The name is output as `_<prefix>offsetof'
   where prefix is value of variable `prefix'. */

static void
output_offsetof_macro_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "offsetof");
}

/* This function outputs name of enumeration constant which represents
   node type (mode) with given name.  The name is output as
   `<prefix>NM_<node_type_name>' where prefix is value of variable
   `prefix', and `node_type_name' is value of the second parameter. */

static void
output_node_mode_type_value (FILE *f, const char *node_type_name)
{
  output_string (f, prefix);
  output_string (f, "NM_");
  output_string (f, node_type_name);
}

/* This function outputs name of enumeration which represents all node
   types (modes).  The name is output as `<prefix>node_mode_enum'
   where prefix is value of variable `prefix'. */

static void
output_node_mode_enumeration_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "node_mode_enum");
}

/* This function outputs name of enumeration type which represents all
   node types (modes).  The name is output as `<prefix>node_mode_t'
   where prefix is value of variable `prefix'. */

static void
output_node_mode_t_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "node_mode_t");
}

/* This function outputs name of structure which describes double
   linked fields.  The name is output as `_<prefix>double_link' where
   prefix is value of variable `prefix'. */

static void
output_double_link_structure_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "double_link");
}

/* This function outputs name of type (typedef) which describes double
   linked fields.  The name is output as `<prefix>double_link_t'
   where prefix is value of variable `prefix'. */

static void
output_double_link_t_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "double_link_t");
}

/* This function outputs name of macro or inline function whose value
   of node mode.  The name is output as `<prefix>NODE_MODE' or
   `<prefix>node_mode' where prefix is value of variable `prefix'. */

static void
output_node_mode_name (FILE *f)
{
  output_string (f, prefix);
  if (!cpp_flag)
    output_string (f, "NODE_MODE");
  else
    output_string (f, "node_mode");
}

/* This function outputs name of member of structure representing node
   whose value is node mode.  The name is output as
   `_<prefix>node_mode' where prefix is value of variable
   `prefix'. */

static void
output_node_mode_member_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "node_mode");
}

/* This function outputs name of macro or inline function whose value
   is description declaration level of given node (type) mode.  The
   name is output as `<prefix>NODE_LEVEL' or `<prefix>node_level'
   where prefix is value of variable `prefix'. */

static void
output_node_level_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, cpp_flag ? "node_level" : "NODE_LEVEL");
}

/* This function outputs name of array which is used for evaluation of
   macro or function whose value is description declaration level of
   given node type (mode).  The name is output as
   `_<prefix>node_level' where prefix is value of variable
   `prefix'. */

static void
output_node_level_array_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "node_level");
}

/* This function outputs name of variable or parameter representing
   guard function for traversing graph continuation.  This function is
   used by functions for traversing graphs.  The name is output as
   `_<prefix>traverse_guard_function_variable' or
   `_<prefix>traverse_guard_function_parameter' where prefix is value
   of variable `prefix'. */

static void
output_name_of_traverse_guard_function (FILE *f, int variable_flag)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "traverse_guard_function_");
  if (variable_flag)
    output_string (f, "variable");
  else
    output_string (f, "parameter");
}

/* This function outputs name of variable representing current pass
   number.  This variable is used by functions for traversing graphs.
   The name is output as `_<prefix>current_graph_pass_number' where
   prefix is value of variable `prefix'. */

static void
output_current_graph_pass_number_variable_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "current_graph_pass_number");
}

/* This function outputs name of member of structure representing
   node.  This member is used by functions for traversing graphs.  The
   name is output as `_<prefix>last_graph_pass_number' where prefix
   is value of variable `prefix'. */

static void
output_last_graph_pass_number_member_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "last_graph_pass_number");
}

/* This function outputs name of member of structure representing
   node.  This member is used by functions for traversing graphs.  The
   name is output as `_<prefix>temporary' where prefix is value of
   variable `prefix'. */

static void
output_temporary_member_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "temporary");
}

/* This function outputs name of member of structure representing
   given super type.  The name is output as `_<prefix>M_<given super
   type name>' where prefix is value of variable `prefix'. */

static void
output_super_type_member_name (FILE *f, const char *super_type_name)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "M_");
  output_string (f, super_type_name);
}

/* This function outputs name of member of structure representing
   double node.  This member value is pointer to first double linked
   field which refers to given node.  The name is output as
   `_<prefix>first_link' where prefix is value of variable
   `prefix'. */

static void
output_first_link_member_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "first_link");
}

/* This function outputs name of type (typedef) which represents any
   node.  The name is output as `<prefix>node_t' where prefix is
   value of variable `prefix'. */

static void
output_node_t_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "node_t");
}

/* This function outputs name of structure (class) which represents
   root node.  The name is output as `<prefix>node' where
   prefix is value of variable `prefix'. */

static void
output_root_struct_class_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "node");
}

/* This function outputs name of type (typedef) of structure which
   represents node with given node type name.  The name is output as
   `_<prefix><node_type_name>' where prefix is value of variable
   `prefix', and `node_type_name' is value of the second parameter. */

static void
output_node_type_name (FILE *f, const char *node_type_name)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, node_type_name);
}

/* This function outputs name of structure which represents node with
   given node type name.  The name is output as
   `_<prefix>S_<node_type_name>' where prefix is value of variable
   `prefix', and `node_type_name' is value of the second parameter. */

static void
output_node_structure_name (FILE *f, const char *node_type_name)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "S_");
  output_string (f, node_type_name);
}

/* This function outputs name of structure which represents class
   fields of node type with given name.  The name is output as
   `_<prefix>CF_<node_type_name>' where prefix is value of variable
   `prefix', and `node_type_name' is value of the second parameter. */

static void
output_node_type_class_structure_name (FILE *f, const char *node_type_name)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "CF_");
  output_string (f, node_type_name);
}

/* This function outputs name of array which contains pointers to all
   structure representing class fields.  The name is output as
   `_<prefix>class_structure' where prefix is value of variable
   `prefix'. */

static void
output_class_structure_array_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "class_structure_array");

}

/* This function outputs name of array which contains sizes of
   structures describing all node types.  The name is output as
   `<prefix>node_size' where prefix is value of variable `prefix'. */

static void
output_node_size_array_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "node_size");
}

/* This function outputs name of array which contains names of all
   node types.  The name is output as `<prefix>node_name' where
   prefix is value of variable `prefix'. */

static void
output_node_name_array_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "node_name");
}

/* This function outputs name of array which exists for each synonym
   field (i.e. for each field name which is name of more one field)
   and contains displacements of the field for each node mode.  The
   name is output as `_<prefix>D_<field_name>' where prefix is value
   of variable `prefix', and `field_name' is name of the synonym
   field. */

static void
output_different_displacement_field_displacement_array_name
  (FILE *f, const char *field_name)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "D_");
  output_string (f, field_name);
}

/* This function outputs name of array which exists for each node mode
   and contains flags of that given node type is super type of
   corresponding node mode.  The name is output as
   `_<prefix>SF_<node_type_name>' where prefix is value of variable
   `prefix', and `node_type_name' is value of the second parameter. */

static void
output_subtype_flag_array_name (FILE *f, const char *node_type_name)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "SF_");
  output_string (f, node_type_name);
}

/* This function outputs name of array which contains pointers to all
   subtype flag arrays.  This array is used to evaluation macro
   `IR_IS_TYPE' or function `IR_is_type'.  The name is output as
   `_<prefix>is_type' where prefix is value of variable `prefix'. */

static void
output_is_type_array_name (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "is_type");
}

/* This function outputs name of macro `IR_IS_TYPE' or inline function
   `IR_is_type'.  The name is output as `<prefix>IS_TYPE' or
   `<prefix>is_type' where prefix is value of variable `prefix'. */

static void
output_is_type_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, cpp_flag ? "is_type" : "IS_TYPE");
}

/* This function outputs name of macro `IR_IS_OF_TYPE' or inline
   function `IR_is_of_type'.  The name is output as
   `<prefix>IS_OF_TYPE' or `<prefix>is_of_type' where prefix is value
   of variable `prefix'. */

static void
output_is_of_type_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, cpp_flag ? "is_of_type" : "IS_OF_TYPE");
}

/* This function outputs name of modification function for field with
   given name.  The name is output as `<prefix>set_<field_name>' or
   `<prefix>F_set_<field_name>' where prefix is value of variable
   `prefix', and `field_name' is name of thefield. */

static void
output_name_of_set_function (FILE *f, const char *field_name,
                             int function_prefix_flag)
{
  output_string (f, prefix);
  if (function_prefix_flag)
    output_string (f, "F_");
  output_string (f, "set_");
  output_string (f, field_name);
}

/* This function outputs name of function for getting first double
   linked field.  The name is output as `<prefix>_first_double_link'
   where prefix is value of variable `prefix'. */

static void
output_name_of_first_double_link_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_first_double_link");
}

/* This function outputs name of function for getting next double
   linked field.  The name is output as `<prefix>next_double_link'
   or `<prefix>F__next_double_link' where prefix is value of variable
   `prefix'. */

static void
output_name_of_next_double_link_function (FILE *f, int function_prefix_flag)
{
  output_string (f, prefix);
  if (function_prefix_flag)
    output_string (f, "F__next_double_link");
  else
    output_string (f, "_next_double_link");
}

/* This function outputs name of function for getting previous double
   linked field.  The name is output as
   `<prefix>_previous_double_link' or
   `<prefix>F__previous_double_link' where prefix is value of variable
   `prefix'. */

static void
output_name_of_previous_double_link_function (FILE *f,
                                              int function_prefix_flag)
{
  output_string (f, prefix);
  if (function_prefix_flag)
    output_string (f, "F__previous_double_link");
  else
    output_string (f, "_previous_double_link");
}

/* This function outputs name of function for getting node which
   contains given double linked field.  The name is output as
   `<prefix>_owner' or `<prefix>F__owner' where prefix is value of
   variable `prefix'. */

static void
output_name_of_owner_function (FILE *f, int function_prefix_flag)
{
  output_string (f, prefix);
  if (function_prefix_flag)
    output_string (f, "F__owner");
  else
    output_string (f, "_owner");
}

/* This function outputs name of function for getting node which
   contains given double linked field.  The name is output as
   `<prefix>_set_double_link' where prefix is value of variable
   `prefix'. */

static void
output_name_of_set_double_link_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "_set_double_link");
}

/* This function outputs name of function for setting up value of
   double linked field.  The name is output as
   `_<prefix>set_double_field_value' where prefix is value of
   variable `prefix'. */


static void
output_name_of_set_double_field_value_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "set_double_field_value");
}

/* This function outputs name of access function (or macro) for field
   with given name.  The name is output as `<prefix><field_name>' or
   `<prefix>F_<field_name>' where prefix is value of variable
   `prefix', and `field_name' is name of the field. */

static void
output_name_of_access_function (FILE *f, const char *field_name,
                                int function_prefix_flag)
{
  output_string (f, prefix);
  if (function_prefix_flag)
    output_string (f, "F_");
  output_string (f, field_name);
}

/* This function outputs name of representation type (typedef) for
   field with given type.  This name is output as name of predefined
   type if (the type is a predefined type) otherwise function
   `output_node_t_name' is called. */

static void
output_field_type_name (FILE *f, IR_node_t field_type)
{
  assert (field_type != NULL);
  if (IR_NODE_MODE (field_type) == IR_NM_predefined_type)
    output_string (f, IR_identifier_itself (IR_type_identifier (field_type)));
  else
    {
      assert (IR_NODE_MODE (field_type) == IR_NM_node_type);
      output_node_t_name (f);
    }
}

/* This function outputs name of macro `IR_START_ALLOC' (see SPRUT
   documentation).  The name is output as `<prefix>START_ALLOC' where
   prefix is value of variable `prefix'. */

static void
output_start_alloc_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "START_ALLOC");
}

/* This function outputs name of macro `IR_STOP_ALLOC' (see SPRUT
   documentation).  The name is output as `<prefix>STOP_ALLOC' where
   prefix is value of variable `prefix'. */

static void
output_stop_alloc_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "STOP_ALLOC");
}

/* This function outputs name of macro `IR_ALLOC' (see SPRUT
   documentation).  The name is output as `<prefix>ALLOC' where
   prefix is value of variable `prefix'. */

static void
output_alloc_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "ALLOC");
}

/* This function outputs name of macro `IR_FREE' (see SPRUT
   documentation).  The name is output as `<prefix>FREE' where prefix
   is value of variable `prefix'. */

static void
output_free_macro_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "FREE");
}

/* This function outputs name prefix of field type specific
   initialization macro (see SPRUT documentation).  The name is output
   as `<prefix>BEGIN_' where prefix is value of variable `prefix'. */

static void
output_field_type_initialization_macro_name_prefix (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "BEGIN_");
}

/* This function outputs name prefix of field type specific
   finalization macro (see SPRUT documentation).  The name is output as
   `<prefix>END_' where prefix is value of variable `prefix'. */

static void
output_field_type_finalization_macro_name_prefix (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "END_");
}

/* This function outputs name prefix of field type specific copy macro
   (see SPRUT documentation).  The name is output as `<prefix>COPY_'
   where prefix is value of variable `prefix'. */

static void
output_field_type_copy_macro_name_prefix (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "COPY_");
}

/* This function outputs name prefix of field type specific equality
   macro (see SPRUT documentation).  The name is output as
   `<prefix>EQUAL_' where prefix is value of variable `prefix'. */

static void
output_field_type_equality_macro_name_prefix (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "EQUAL_");
}

/* This function outputs name prefix of field type specific print
   macro (see SPRUT documentation).  The name is output as
   `<prefix>PRINT_' where prefix is value of variable `prefix'. */

static void
output_field_type_print_macro_name_prefix (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "PRINT_");
}

/* This function outputs name prefix of field type specific output
   macro (see SPRUT documentation).  The name is output as
   `<prefix>OUTPUT_' where prefix is value of variable `prefix'. */

static void
output_field_type_output_macro_name_prefix (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "OUTPUT_");
}

/* This function outputs name prefix of field type specific input
   macro (see SPRUT documentation).  The name is output as
   `<prefix>INPUT_' where prefix is value of variable `prefix'. */

static void
output_field_type_input_macro_name_prefix (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "INPUT_");
}

/* This function outputs name of internal function for node field
   initiation.  The name is output as
   `_<prefix>node_field_initiation' where prefix is value of variable
   `prefix'. */

static void
output_name_of_field_initiation_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "node_field_initiation");
}

/* This function outputs name of function for creation node .  The
   name is output as `<prefix>create_node' where prefix is value of
   variable `prefix'. */

static void
output_name_of_creation_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "create_node");
}

/* This function outputs name of node type specific creation function.
   The name is output as `<prefix>new_<node_type_name>' where prefix
   is value of variable `prefix', and `node_type_name' is value of the
   second parameter. */

static void
output_name_of_new_function (FILE *f, const char *node_type_name)
{
  output_string (f, prefix);
  output_string (f, "new_");
  output_string (f, node_type_name);
}

/* This function outputs name of function for deleting node fields.
   The name is output as `_<prefix>free_node_fields' where prefix is
   value of variable `prefix'. */

static void
output_name_of_field_deletion_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "free_node_fields");
}

/* This function outputs name of function for deleting node.  The name
   is output as `<prefix>free_node' where prefix is value of variable
   `prefix'. */

static void
output_name_of_node_deletion_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "free_node");
}

/* This function outputs name of (internal or external) function for
   deleting graph.  The name is output as `_<prefix>free_graph' or
   `<prefix>free_graph' where prefix is value of variable
   `prefix'. */

static void
output_name_of_graph_deletion_function (FILE *f, int internal_function_flag)
{
  if (internal_function_flag)
    output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "free_graph");
}

/* This function outputs name of function for conditional deleting
   graph.  The name is output as `<prefix>conditional_free_graph'
   where prefix is value of variable `prefix'. */

static void
output_name_of_conditional_deletion_of_graph_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "conditional_free_graph");
}

/* This function outputs name of function for copying node fields.
   The name is output as `_<prefix>copy_node_fields' where prefix is
   value of variable `prefix'. */

static void
output_name_of_field_copy_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "copy_node_fields");
}

/* This function outputs name of function for copying node.  The name
   is output as `<prefix>copy_node' where prefix is value of variable
   `prefix'. */

static void
output_name_of_node_copy_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "copy_node");
}

/* This function outputs name of (internal or external) function for
   copying graph.  The name is output as `_<prefix>copy_graph' or
   `<prefix>copy_graph' where prefix is value of variable
   `prefix'. */

static void
output_name_of_graph_copy_function (FILE *f, int internal_function_flag)
{
  if (internal_function_flag)
    output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "copy_graph");
}

/* This function outputs name of function for conditional copying
   graph.  The name is output as `<prefix>conditional_copy_graph'
   where prefix is value of variable `prefix'. */

static void
output_name_of_conditional_copy_of_graph_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "conditional_copy_graph");
}

/* This function outputs name of function for comparing node fields.
   The name is output as `_<prefix>node_field_equality' where prefix
   is value of variable `prefix'. */

static void
output_name_of_field_equality_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "node_field_equality");
}

/* This function outputs name of function for comparing nodes.  The
   name is output as `<prefix>is_equal_node' where prefix is value of
   variable `prefix'. */

static void
output_name_of_node_equality_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "is_equal_node");
}

/* This function outputs name of (internal or external) function for
   comparing graphs.  The name is output as `_<prefix>is_equal_graph'
   or `<prefix>is_equal_graph' where prefix is value of variable
   `prefix'. */

static void
output_name_of_graph_equality_function (FILE *f, int internal_function_flag)
{
  if (internal_function_flag)
    output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "is_equal_graph");
}

/* This function outputs name of function for conditional comparing
   graphs.  The name is output as
   `<prefix>conditional_is_equal_graph' where prefix is value of
   variable `prefix'. */

static void
output_name_of_conditional_graph_equality_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "conditional_is_equal_graph");
}

/* This function outputs name of function for checking node fields.
   The name is output as `<prefix>check_node_fields' where prefix is
   value of variable `prefix'. */

static void
output_name_of_field_check_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "check_node_fields");
}

/* This function outputs name of function for printing node fields for
   check function.  The name is output as
   `<prefix>print_node_fields_for_check' where prefix is value of
   variable `prefix'. */

static void
output_name_of_field_print_for_check_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "print_node_fields_for_check");
}

/* This function outputs name of function for checking node.  The name
   is output as `<prefix>check_node' where prefix is value of
   variable `prefix'. */

static void
output_name_of_node_check_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "check_node");
}

/* This function outputs name of (internal or external) function for
   checking graph.  The name is output as `_<prefix>check_graph' or
   `<prefix>check_graph' where prefix is value of variable
   `prefix'. */

static void
output_name_of_graph_check_function (FILE *f, int internal_function_flag)
{
  if (internal_function_flag)
    output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "check_graph");
}

/* This function outputs name of function for conditional checking
   graph.  The name is output as `<prefix>conditional_check_graph'
   where prefix is value of variable `prefix'. */

static void
output_name_of_conditional_check_of_graph_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "conditional_check_graph");
}

/* This function outputs name of function for printing node fields.
   The name is output as `_<prefix>print_node_fields' where prefix is
   value of variable `prefix'. */

static void
output_name_of_field_print_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "print_node_fields");
}

/* This function outputs name of function for printing node.  The name
   is output as `<prefix>print_node' where prefix is value of
   variable `prefix'. */

static void
output_name_of_node_print_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "print_node");
}

/* This function outputs name of function for output of node fields.
   The name is output as `_<prefix>output_node_fields' where prefix
   is value of variable `prefix'. */

static void
output_field_output_name_of_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "output_node_fields");
}

/* This function outputs name of function for output of node.  The
   name is output as `<prefix>output_node' where prefix is value of
   variable `prefix'. */

static void
output_node_output_name_of_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "output_node");
}

/* This function outputs name of function for input of node fields.
   The name is output as `_<prefix>input_node_fields' where prefix is
   value of variable `prefix'. */

static void
output_name_of_field_input_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "input_node_fields");
}

/* This function outputs name of function for input of node.  The name
   is output as `<prefix>input_node' where prefix is value of
   variable `prefix'. */

static void
output_name_of_node_input_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "input_node");
}

/* This function outputs name of (internal or external) functions for
   traversing of graph.  The name is output as
   `_<prefix>node_field_traverse_depth_first' or
   `_<prefix>node_field_traverse_reverse_depth_first' or depending on
   the second parameter value. Here prefix is value of variable
   `prefix'. */

static void
output_name_of_field_traverse_function (FILE *f, int reverse_flag)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "node_field");
  output_string (f, "_traverse");
  if (reverse_flag)
    output_string (f, "_reverse");
  output_string (f, "_depth_first");
}

/* This function outputs name of (internal or external) functions for
   traversing of graph.  The name is output as
   `_<prefix>traverse_depth_first' or
   `_<prefix>traverse_reverse_depth_first' or
   `<prefix>traverse_depth_first' or
   `<prefix>traverse_reverse_depth_first' depending on the second and
   third parameter values. Here prefix is value of variable
   `prefix'. */

static void
output_name_of_traverse_function (FILE *f, int reverse_flag,
                                  int internal_function_flag)
{
  if (internal_function_flag)
    output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "traverse");
  if (reverse_flag)
    output_string (f, "_reverse");
  output_string (f, "_depth_first");
}

/* This function outputs name of function for node field
   transformation.  The name is output as
   `_<prefix>node_field_transformation'. Here prefix is value of
   variable `prefix'. */

static void
output_name_of_field_transformation_function (FILE *f)
{
  output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "node_field_transformation");
}

/* This function outputs name of (internal or external) functions for
   transformation of graph.  The name is output as
   `_<prefix>transform_dag' or `<prefix>transform_dag' depending on
   the second parameter value. Here prefix is value of variable
   `prefix'. */

static void
output_name_of_transformation_function (FILE *f, int internal_function_flag)
{
  if (internal_function_flag)
    output_char ('_', f);
  output_string (f, prefix);
  output_string (f, "transform_dag");
}

/* This function outputs name of definition of auxiliary node
   necessary for initiation of class fields.  The name is output as
   `<prefix>aux_node' where prefix is value of variable `prefix'. */


static void
output_auxiliary_node_name (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "aux_node");
}

/* This function outputs name of functions for initiation or
   finalization of class fields.  The name is output as
   `_<prefix>class_fields_initiation' or
   `_<prefix>class_fields_finalization' where prefix is value of
   variable `prefix'. */

static void
output_name_of_class_field_initiation_finalization_function
  (FILE *f, int initiation_flag)
{
  output_char ('_', f);
  output_string (f, prefix);
  if (initiation_flag)
    output_string (f, "class_fields_initiation");
  else
    output_string (f, "class_fields_finalization");
}

/* This function outputs name of function for initiation of work with
   internal representation.  The name is output as `<prefix>start'
   where prefix is value of variable `prefix'. */

static void
output_name_of_start_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "start");
}

/* This function outputs name of function for finalization of work
   with internal representation.  The name is output as
   `<prefix>stop' where prefix is value of variable `prefix'. */

static void
output_name_of_stop_function (FILE *f)
{
  output_string (f, prefix);
  output_string (f, "stop");
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

/* This function output preprocessor `#line' with number which is
   actual number of the next line of given file. */

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



/* This page contains functions for output of some fragments of C code
   common for SPI functions. */

/* This function outputs part of declaration of variable or parameter
   representing guard function for traversing graph continuation.  The
   most probable function output is following
      `int (* _IR_traverse_guard_function_variable) (IR_node_t node)'
   or
      `int (* _IR_traverse_guard_function_parameter)(IR_node_t node)'
   where prefix is value of variable `prefix'. */

static void
output_definition_of_traverse_guard_function (FILE *f, int variable_flag)
{
  
  output_string (f, "int (* ");
  output_name_of_traverse_guard_function (f, variable_flag);
  output_string (f, ") (");
  output_node_t_name (f);
  output_string (f, " node)");
}

/* This function outputs code for given field access of node with
   given name.  The function output the following fragments

        `(*(<field_type_name> *)
          ((char *) <class_structure_array_name>
                    [ <node_mode_macro> (<node_name>) ]
           + <displacement_array_name> [<node_mode_macro>
                                        (<node_name>)])' 
        or
        `(*(<field_type_name> *)
          ((char *) <class_structure_array_name>
                    [ <node_name>-><node_mode_macro> () ]
           + <displacement_array_name> [<node_name>-><node_mode_macro>
                                        ()])' 
           for class synonym field or for field which is in node type
           used as secondary supertype (if `as_synonym_field' is TRUE);
        `(((struct <node_type_class_structure_name> *)
           <class_structure_array_name>
           [ <node_mode_macro> (<node_name>) ])
          -><super_type_name>.<super_type_name>....<field_name>)'
           for class field;
        `(*(<field_type_name> *) ((char *) <node_name>
              + <displacement_array_name> [<node_mode_macro> 
                                           (<node_name>)]))' 
        or
        `(((struct <node_type_class_structure_name> *)
           <class_structure_array_name>
           [ <node_name>-><node_mode_macro> () ])
          -><super_type_name>.<super_type_name>....<field_name>)'
           for class field;
        `(*(<field_type_name> *) ((char *) <node_name>
              + <displacement_array_name> [<node_mode_macro> 
                                           (<node_name>)]))' 
        or
        `(*(<field_type_name> *) ((char *) <node_name>
             + <displacement_array_name> [<node_name>-><node_mode_macro> ()]))' 
           for non-class synonym field or for field which is in node
           type used as secondary super type (if `as_synonym_field'
           is TRUE);
        `(((<node_type_name> *) <node_name>)
          ->(<node_structure_name>. if non-flat implementation is used)
            <super_type_name>.<super_type_name>....<field_name>)'
           for non-class field.

   If the field is double linked field and `as_double_link_flag' is
   FALSE then the function additionally outputs
   `.<double_link_field_name>'.  Here <node_type_class_structure_name>
   is output by function `output_node_type_class_structure_name',
   <node_name> is value of the second parameter, <field_name> is name
   of given field, <field_type_name> is output by function
   `output_double_link_t_name' or `output_field_type_name',
   <displacement_array_name> is output by function
   `output_different_displacement_field_displacement_array_name',
   <node_type_name> is output by function `output_node_type_name',
   <super_type_name> is output by function
   `output_super_type_member_name', and `double_link_field_name' is
   value of macro `DOUBLE_LINK_FIELD_NAME'. */

static void
output_field_access (FILE *f, const char *node_ref_variable_name,
                     IR_node_t field, int any_node_type_field_flag,
                     int as_double_link_flag)
{
  IR_node_t source_node_type;
  IR_node_t *current_super_type_ref;

  output_char ('(', f);
  if (any_node_type_field_flag
      && (IR_previous_synonym_field (field) != field
          || IR_secondary_super_type_flag (IR_node_type (field))))
    {
      output_string (f, "*(");
      if (IR_double_field_flag (field))
        output_double_link_t_name (f);
      else
        {
          output_field_type_name (f, IR_field_type (field));
          output_string (f, " *");
        }
      output_string (f, ") ((char *) ");
      if (IR_declaration_part (field) != DP_CLASS)
        output_string (f, node_ref_variable_name);
      else
        {
          output_class_structure_array_name (f);
          output_string (f, " [");
          if (cpp_flag)
            {
              output_string (f, node_ref_variable_name);
              output_string (f, "->");
            }
          output_node_mode_name (f);
          output_string (f, " (");
          if (!cpp_flag)
            output_string (f, node_ref_variable_name);
          output_string (f, " )]");
        }
      output_string (f, " + ");
      output_different_displacement_field_displacement_array_name
        (f, IR_identifier_itself (IR_field_identifier (field)));
      output_string (f, " [");
      if (cpp_flag)
        {
          output_string (f, node_ref_variable_name);
          output_string (f, "->");
        }
      output_node_mode_name (f);
      output_string (f, " (");
      if (!cpp_flag)
        output_string (f, node_ref_variable_name);
      output_string (f, ")])");
    }
  else
    {
      if (!any_node_type_field_flag)
        {
          assert (VLO_LENGTH (traverse_node_type_path) != 0);
          source_node_type
            = *(IR_node_t *) VLO_BEGIN (traverse_node_type_path);
        }
      output_string (f, "((");
      if (IR_declaration_part (field) != DP_CLASS)
        output_node_type_name
          (f, IR_identifier_itself (IR_type_identifier
                                    (any_node_type_field_flag
                                     ? IR_node_type (field)
                                     : source_node_type)));
      else
        {
          output_string (f, "struct ");
          assert (IR_node_type (field) != root_node_type);
          output_node_type_class_structure_name
            (f, IR_identifier_itself (IR_type_identifier
                                      (any_node_type_field_flag
                                       ? IR_node_type (field)
                                       : source_node_type)));
        }
      output_string (f, " *) ");
      if (IR_declaration_part (field) == DP_CLASS)
        {
          output_class_structure_array_name (f);
          output_string (f, " [");
          if (cpp_flag)
            {
              output_string (f, node_ref_variable_name);
              output_string (f, "->");
            }
          output_node_mode_name (f);
          output_string (f, " (");
        }
      if (!cpp_flag || IR_declaration_part (field) != DP_CLASS)
        output_string (f, node_ref_variable_name);
      if (IR_declaration_part (field) == DP_CLASS)
        output_string (f, " )]");
      output_string (f, ")->");
      if (!flat_structure_flag && IR_declaration_part (field) != DP_CLASS)
        {
          output_node_structure_name
            (f,
             IR_identifier_itself (IR_type_identifier (any_node_type_field_flag
                                                       ? IR_node_type (field)
                                                       : source_node_type)));
          output_char ('.', f);
        }
      if (!any_node_type_field_flag)
        for (current_super_type_ref
             = (IR_node_t *) VLO_BEGIN (traverse_node_type_path);;)
          {
            current_super_type_ref++;
            if (current_super_type_ref
                > (IR_node_t *) VLO_END (traverse_node_type_path))
              break;
            output_super_type_member_name
              (f, IR_identifier_itself (IR_type_identifier
                                        (*current_super_type_ref)));
            output_char ('.', f);
          }
      output_string (f, IR_identifier_itself (IR_field_identifier (field)));
    }
  output_char (')', f);
  if (IR_double_field_flag (field) && !as_double_link_flag)
    {
      output_char ('.', f);
      output_string (f, DOUBLE_LINK_FIELD_NAME);
    }
}

/* This function outputs code for given class field access of given
   node mode.  The function outputs the following fragment
        `((struct <node type class structure name in which the field
                   was really declared> *)
         <class structure ref of given node mode>)-><field_name>' */

static void
output_class_structure_field_access (FILE *f, const char *node_mode_string,
                                     IR_node_t field)
{
  output_string (f, "((struct ");
  assert (IR_declaration_part (field) == DP_CLASS
          && IR_node_type (field) != root_node_type);
  output_node_type_class_structure_name
    (f, IR_identifier_itself (IR_type_identifier (IR_node_type (field))));
  output_string (f, " *) ");
  output_class_structure_array_name (f);
  output_string (f, " [");
  output_string (f, node_mode_string);
  output_string (f, "])->");
  output_string (f, IR_identifier_itself (IR_field_identifier (field)));
}

/* This function outputs code for access to a member of structure
   representing given double linked field.  The function uses function
   `output_field_access'.  Given field must be double linked field and
   given member name of name of a member of structure representing
   double linked field. */

static void
output_double_link_member_access
 (FILE *f, const char *node_ref_variable_name, IR_node_t field,
  int any_node_type_field_flag, const char *member_name)
{
  output_field_access (f, node_ref_variable_name, field,
                       any_node_type_field_flag, TRUE);
  output_char ('.', f);
  output_string (f, member_name);
}

/* If given field is a double linked field the function outputs code
   for initiation of all members (except for one representing field
   itself) of structure (class) representing double linked field.
   Values of fields representing next and previous double links are
   not initiated because they are defined and used only after
   inclusion of their into a double link list. */

static void
output_other_double_link_members_initiation
  (FILE *f, const char *node_ref_variable_name, IR_node_t field)
{
  if (IR_double_field_flag (field))
    {
      output_string (f, "      ");
      output_double_link_member_access
        (f, node_ref_variable_name, field, FALSE, DOUBLE_LINK_OWNER_NAME);
      output_string (f, " = ");
      output_string (f, node_ref_variable_name);
      output_string (f, ";\n");
      if (set_flag)
        {
          output_string (f, "      ");
          output_double_link_member_access
            (f, node_ref_variable_name, field, FALSE,
             DOUBLE_LINK_SET_FUNCTION_NAME);
          output_string (f, " = ");
          if (cpp_flag)
            {
              output_string (f, "&");
              output_root_struct_class_name (f);
              output_string (f, "::");
            }
          output_name_of_set_function
            (f, IR_identifier_itself (IR_field_identifier (field)),
             !cpp_flag && (macro_flag || only_macro_flag));
          output_string (f, ";\n");
        }
    }
}

/* If given field is a double linked field the function outputs start
   code for setting value of the field.  The most probable code
   fragment are following
     `_IR_set_double_field_value
        (&((<node_type_name_of_given_field_type> *)<field_itself_access>), '
     or
     `_IR_set_double_field_value (&((<node_type_name_of_given_field_type> *)<field_itself_access>), ' */

static void
output_start_of_setting_double_link_field_value
  (FILE *f, const char * indent, const char *node_ref_variable_name,
   IR_node_t field, int any_node_type_field_flag, int in_macro_flag)
{
  if (IR_double_field_flag (field))
    {
      output_string (f, indent);
      output_name_of_set_double_field_value_function (f);
      if (!in_macro_flag)
        output_string (f, "\n");
      output_string (f, indent);
      output_string (f, "  (&(");
      output_field_access (f, node_ref_variable_name, field,
                           any_node_type_field_flag, TRUE);
      output_string (f, "), ");
    }
}

/* If given field is a double linked field the function outputs finish
   code for setting the field value.  The most probable code fragment
   are following
     `,
      _IR_offsetof (<node type name of given field type or root name
                     for nonflat implementation>, _IR_first_link),
      <1 or 0>);'
      or
     `, _IR_offsetof (<node type name of given field type or root name
                      for nonflat implementation>, _IR_first_link), <1 or 0>)'
*/

static void
output_finish_of_setting_double_link_field_value
  (FILE *f, const char * indent, IR_node_t field, int only_inclusion_flag,
   int in_macro_flag)
{
  if (IR_double_field_flag (field))
    {
      if (in_macro_flag)
        output_string (f, ", ");
      else
          {
            output_string (f, ",\n");
            output_string (f, indent);
            output_string (f, "   ");
          }
      output_offsetof_macro_name (f);
      output_string (f, " (");
      if (IR_field_type (field) == root_node_type || !flat_structure_flag)
        {
          if (cpp_flag)
            output_string (f, "class ");
          else
            output_string (f, "struct ");
          output_root_struct_class_name (f);
        }
      else
        output_node_type_name
          (f, IR_identifier_itself (IR_field_type_identifier (field)));
      output_string (f, ", ");
      output_first_link_member_name (f);
      if (only_inclusion_flag)
        output_string (f, "), 1");
      else
        output_string (f, "), 0");
      if (in_macro_flag)
        output_string (f, ")");
      else
        output_string (f, ");\n");
    }
}

/* The following function output the following fragment to given file
     (IR_node_t) ((char *) &((<name of given node type> *)
                             <given node type name>)
                            -><member name for given immediate super type>
                  -_IR_offsetof(<name of given immediate node type>,
                                <name of structure of given immediate
                                 node type>)))
   or
     (IR_node_t) ((char *) &((<name of given node type> *)
                             <given node type name>)
                            -><name of structure of given node type>
                            .<member name for given immediate super type>
                  -_IR_offsetof(<name of given node type>,
                                <name of structure of given immediate
                                 node type>)))

*/

static void
output_immediate_super_type_node_address (FILE *f, IR_node_t node_type,
                                          IR_node_t immediate_super_type,
                                          const char *node_type_name)
{
  assert (node_type != NULL && immediate_super_type != NULL);
  output_char ('(', f);
  output_node_t_name (f);
  output_string (f, ") ((char *) &((");
  output_node_type_name (f, IR_identifier_itself (IR_type_identifier
                                                  (node_type)));
  output_string (f, " *) ");
  output_string (f, node_type_name);
  output_string (f, ")->");
  if (!flat_structure_flag)
    {
      output_node_structure_name (f, IR_identifier_itself (IR_type_identifier
                                                           (node_type)));
      output_char ('.', f);
    }
  output_super_type_member_name
    (f, IR_identifier_itself (IR_type_identifier (immediate_super_type)));
  output_string (f, " - ");
  output_offsetof_macro_name (f);
  output_string (f, " (");
  output_node_type_name (f, IR_identifier_itself (IR_type_identifier
                                                  (immediate_super_type)));
  output_string (f, ", ");
  output_node_structure_name (f,
                              IR_identifier_itself (IR_type_identifier
                                                    (immediate_super_type)));
  output_string (f, "))");
}

/* The following variable is used by function `output_code' for output
   field access instead of `$'.  If the value is NULL the construction `$'
   is not changed.   The variable is to be set up before any call of 
   function `output_code'. */

static IR_node_t last_processed_field;

/* The following function outputs code of given constraint or action.
   The constructions `$$' and `$' are changed by needed code.  The
   function processes C constructions (comments, string and character
   constants) analogously to the SPRUT scanner. */

static void
output_code (FILE *f, IR_node_t action_or_constraint,
             const char *two_dollars_name, int class_initiation_flag)
{
  char *code;

  assert (action_or_constraint != NULL);
  if (IR_NODE_MODE (action_or_constraint) == IR_NM_action)
    code = IR_code_insertion_itself (IR_action_code (action_or_constraint));
  else if (IR_NODE_MODE (action_or_constraint) == IR_NM_constraint)
    code = IR_expression_itself (IR_constraint_code (action_or_constraint));
  else
    assert (FALSE);
  if (!no_line_flag)
    {
      output_char ('\n', f);
      output_line (f, IR_position (action_or_constraint).line_number,
                   IR_position (action_or_constraint).file_name);
    }
  /* !! This code is bound to scanner */
  for (; *code != '\0'; code++)
    switch (*code)
      {
      case '$':
        if (last_processed_field == NULL
            || (IR_declaration_part (last_processed_field)
                != IR_declaration_part (action_or_constraint)))
          output_char (*code, f);
        else if (code [1] == '$')
          {
            code++;
            if (!class_initiation_flag)
              output_string (f, two_dollars_name);
            else
              {
                output_char ('(', f);
                output_auxiliary_node_name (f);
                output_char ('.', f);
                output_node_mode_member_name (f);
                output_string (f, " = ");
                output_string (f, two_dollars_name);
                output_string (f, ", &");
                output_auxiliary_node_name (f);
                output_char (')', f);
              }
          }
        else
          {
            if (!class_initiation_flag)
              output_field_access (f, two_dollars_name, last_processed_field,
                                   FALSE, FALSE);
            else
              output_class_structure_field_access (f, two_dollars_name,
                                                   last_processed_field);
          }
        break;
      case '/':
        output_char (*code, f);
        if (code [1] == '*')
          {
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
                output_char (*code, f);
              }
          }
        break;
      case '\'':
        output_char (*code, f);
        code++;
        if (*code == '\'')
          output_char (*code, f);
        else if (*code == '\0' || *code == '\n')
          {
            code--;
            break;
          }
        else
          {
            output_char (*code, f);
            if (*code == '\\')
              {
                code++;
                output_char (*code, f);
                if (isdigit (*code) && *code != '8' && *code != '9'
                    && isdigit (code [1]) && code [1] != '8'
                    && code [1] != '9')
                  {
                    code++;
                    output_char (*code, f);
                    code++;
                    if (!isdigit (*code) || *code == '8' || *code == '9')
                      code--;
                    else
                      output_char (*code, f);
                  }
              }
          }
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
                output_char (*code, f);
                if (*code == '\\')
                  {
                    code++;
                    output_char (*code, f);
                    if (isdigit (*code) && *code != '8' && *code != '9'
                        && isdigit (code [1]) && code [1] != '8'
                        && code [1] != '9')
                      {
                        code++;
                        output_char (*code, f);
                        code++;
                        if (!isdigit (*code) || *code == '8' || *code == '9')
                          code--;
                        else
                          output_char (*code, f);
                      }
                  }
              }
          }
        break;
      default:
        output_char (*code, f);
        break;
      }
  if (!no_line_flag)
    {
      output_char ('\n', f);
      output_current_line (f);
    }
}



/* This page contains function to output all import and local codes
   of one level description. */

/* The following function outputs all import and local codes of one
   level description.  Code is output without any modification. */

static void
output_start_code_insertions (void)
{
  IR_node_t current_code_insertion;

  if (IR_last_code_insertion (current_description_part) == NULL)
    return;
  for (current_code_insertion
       = IR_next_code_insertion (IR_last_code_insertion
                                 (current_description_part));;
       current_code_insertion
       = IR_next_code_insertion (current_code_insertion))
    {
      if (IR_NODE_MODE (current_code_insertion) == IR_NM_import_code)
        {
          if (!no_line_flag)
            output_line (output_interface_file,
                         IR_position (current_code_insertion).line_number,
                         IR_position (current_code_insertion).file_name);
          output_string (output_interface_file,
                         IR_code_insertion_itself (IR_code_itself
                                                   (current_code_insertion)));
          output_char ('\n', output_interface_file);
          if (!no_line_flag)
            output_current_line (output_interface_file);
        }
      else if (IR_NODE_MODE (current_code_insertion) == IR_NM_local_code)
        {
          if (!no_line_flag)
            output_line (output_implementation_file,
                         IR_position (current_code_insertion).line_number,
                         IR_position (current_code_insertion).file_name);
          output_string (output_implementation_file,
                         IR_code_insertion_itself (IR_code_itself
                                                   (current_code_insertion)));
          output_char ('\n', output_implementation_file);
          if (!no_line_flag)
            output_current_line (output_implementation_file);
        }
      else
        assert (IR_NODE_MODE (current_code_insertion) == IR_NM_export_code);
      if (current_code_insertion
          == IR_last_code_insertion (current_description_part))
        break;
    }
}



/* This page contains functions for output of default definitions
   of macros for storage management and type specific macros (for
   all predefined types and type representing all node types). */

/* The following function outputs default definitions of type all
   specific macros for given predefined type or type representing all
   node types.  Type representing all node types can be given by any
   node type. */

static void
output_a_type_specific_macros (IR_node_t type)
{
  /* Initialization, finalization macros */
  output_string (output_implementation_file, "#ifndef ");
  output_field_type_initialization_macro_name_prefix
    (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "\n#define ");
  output_field_type_initialization_macro_name_prefix
    (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file,
                 "(a) memset (&(a), 0, sizeof (a))\n#endif\n\n");
  output_string (output_implementation_file, "#ifndef ");
  output_field_type_finalization_macro_name_prefix
    (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "\n#define ");
  output_field_type_finalization_macro_name_prefix
    (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "(a)\n#endif\n\n");
  /* Copy macro */
  output_string (output_implementation_file, "#ifndef ");
  output_field_type_copy_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "\n#define ");
  output_field_type_copy_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file,
                 "(a, b) ((a) = (b))\n#endif\n\n");
  /* Equal macro */
  output_string (output_implementation_file, "#ifndef ");
  output_field_type_equality_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "\n#define ");
  output_field_type_equality_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file,
                 "(a, b) ((a) == (b))\n#endif\n\n");
  /* Print macro */
  output_string (output_implementation_file, "#ifndef ");
  output_field_type_print_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "\n#define ");
  output_field_type_print_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "(a)");
  if (IR_NODE_MODE (type) == IR_NM_node_type)
    output_string (output_implementation_file,
                   " printf (\"%lx\", (unsigned long) (a))");
  output_string (output_implementation_file, "\n#endif\n\n");
  /* Input/output macros */
  output_string (output_implementation_file, "#ifndef ");
  output_field_type_input_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "\n#define ");
  output_field_type_input_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string
    (output_implementation_file,
     "(file, field) (fread (&(field), sizeof (field), 1, (file))");
  output_string (output_implementation_file,
                 " != sizeof (field))\n#endif\n\n");
  output_string (output_implementation_file, "#ifndef ");
  output_field_type_output_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string (output_implementation_file, "\n#define ");
  output_field_type_output_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, type);
  output_string
    (output_implementation_file,
     "(file, field) (fwrite (&(field), sizeof (field), 1, (file))");
  output_string (output_implementation_file,
                 " != sizeof (field))\n#endif\n\n");
}

/* The following function outputs all default type specific macros for
   all predefined types of one level description.  To make this the
   function calls function `output_a_type_specific_macros'.  Type
   representing all node types can be given by any node type. */

static void
output_predefined_type_macros (void)
{
  IR_node_t type;

  if (IR_last_type (current_description_part) == NULL)
    return;
  for (type = IR_next_type (IR_last_type (current_description_part));;
       type = IR_next_type (type))
    {
      if (IR_NODE_MODE (type) == IR_NM_predefined_type)
        {
          if (type == find_type (IR_type_identifier (type)))
            output_a_type_specific_macros (type);
        }
      else
        assert (IR_NODE_MODE (type) == IR_NM_node_type);
      if (type == IR_last_type (current_description_part))
        break;
    }
}

/* The following function outputs all default macros for storage
   management, all predefined type specific macros (with the aid of
   function `output_predefined_type_macros'), and type specific macros
   for type representing all node types (with the aid of function
   `output_a_type_specific_macros').  Type representing all node types
   can be given by any node type. */

static void
output_default_macros (void)
{
  /* Memory allocation macros */
  output_string (output_implementation_file, "#ifndef ");
  output_start_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_start_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "()\n#endif\n\n");
  output_string (output_implementation_file, "#ifndef ");
  output_stop_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_stop_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "()\n#endif\n\n");
  output_string (output_implementation_file, "#ifndef ");
  output_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_alloc_macro_name (output_implementation_file);
  output_string
    (output_implementation_file,
     "(ptr, size, ptr_type) ((ptr) = (ptr_type) malloc (size))\n#endif\n\n");
  output_string (output_implementation_file, "#ifndef ");
  output_free_macro_name (output_implementation_file);
  output_string (output_implementation_file, "\n#define ");
  output_free_macro_name (output_implementation_file);
  output_string (output_implementation_file,
                 "(ptr, size) free (ptr)\n#endif\n\n");
  /* Predefined macros for `IR_node_t' and all predefined types. */
  output_a_type_specific_macros (root_node_type);
  output_predefined_type_macros ();
}



/* This page contains functions for output definition of enumeration
   type whose enumeration constants represent node types (modes). */

/* The following variable is used for testing correctness of
   numeration of node types.  The number of node type must be the same as
   enumeration constant which represents the node type (mode). */

#ifndef NDEBUG
static int current_node_type_number;
#endif

/* The following function outputs the following line
       `<enumeration constant which represents node type (mode)>,' */

static void
output_node_mode_enumeration_constant_name (IR_node_t node_type)
{
#ifndef NDEBUG
  assert (IR_node_type_number (node_type) == current_node_type_number);
  current_node_type_number++;
#endif
  output_string (output_interface_file, "  ");
  output_node_mode_type_value
    (output_interface_file,
     IR_identifier_itself (IR_type_identifier (node_type)));
  output_string (output_interface_file, ",\n");
}

/* The following function outputs definition of enumeration type whose
   enumeration constants represent node types (modes).  To output
   enumeration constant the function
   `output_node_mode_enumeration_constant_name' is used. */

static void
output_node_mode_type_definition (void)
{
  output_string (output_interface_file, "\ntypedef enum ");
  output_node_mode_enumeration_name (output_interface_file);
  output_string (output_interface_file, "\n{\n");
#ifndef NDEBUG
  current_node_type_number = 0;
#endif
  traverse_node_types (output_node_mode_enumeration_constant_name);
  output_string (output_interface_file, "  ");
  output_node_mode_type_value (output_interface_file, ROOT_NAME);
  output_string (output_interface_file, ",\n");
  output_string (output_interface_file, "  ");
  output_node_mode_type_value (output_interface_file, ERROR_NAME);
  output_string (output_interface_file, "\n} ");
  output_node_mode_t_name (output_interface_file);
  output_string (output_interface_file, ";\n\n");
}



/* This page contains functions for output typedef `IR_node_t', macro
   `IR_NODE_MODE', and structures and typedefs which represent double
   linked fields and all node types. */

/* The following variable value is TRUE when common last graph pass
   number member is generated. */

static int graph_pass_number_member_flag;

/* The following recursive function returns TRUE if there is a
   non-class field in given node type or in its super types if the
   second parameter value is TRUE.e
 */
static int
node_type_field_presence (IR_node_t node_type, int recursive_flag)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (recursive_flag)
    for (curr_supertype_list_element
         = IR_first_super_type_list_element (node_type);
         curr_supertype_list_element != NULL;
         curr_supertype_list_element
         = IR_next_super_type_list_element (curr_supertype_list_element))
      if (IR_immediate_super_type (curr_supertype_list_element) != NULL
          && node_type_field_presence (IR_immediate_super_type
                                       (curr_supertype_list_element),
                                       recursive_flag))
        return TRUE;
  if (IR_last_field (node_type) != NULL)
    for (current_field = IR_next_field (IR_last_field (node_type));;
         current_field = IR_next_field (current_field))
      {
        if (IR_NODE_MODE (current_field) == IR_NM_field)
          {
            if (IR_declaration_part (current_field) != DP_CLASS)
              return TRUE;
          }
        else
          assert (IR_NODE_MODE (current_field) == IR_NM_action
                  || IR_NODE_MODE (current_field) == IR_NM_constraint);
        if (current_field == IR_last_field (node_type))
          break;
      }
  return FALSE;
}

/* The following function outputs structure typedef which represents
   double linked fields.  The most probable output is following

            typedef struct _IR_double_link  *IR_double_link_t;

            struct _IR_double_link
              {
                IR_node_t field_itself;
                IR_node_t link_owner;
                void (* set_function) (IR_node_t, IR_node_t);
                IR_double_link_t previous_link;
                IR_double_link_t next_link;
              }

or (for C++)

            typedef class _IR_double_link *IR_double_link_t;
            
            class _IR_double_link
              {
                IR_node_t field_itself;
                IR_node_t link_owner;
                void (* set_function) (IR_node_t, IR_node_t);
                IR_double_link_t previous_link;
                IR_double_link_t next_link;
              public:
                IR_double_link_t IR__next_double_link (void);
                IR_double_link_t IR__previous_double_link (void);
                IR_node_t IR__owner (void);
                void IR__set_double_link (IR_node_t value, int disp, int flag);
              }
                    
   The member set function is generated only if field modification
   functions are generated. */

static void output_rest_double_link_functions_definitions (void);
static void output_set_double_link_function_definition (void);
static void output_title_of_set_double_field_value_function (FILE *f);
static void output_title_of_field_initiation_function (FILE *f);
static void output_title_of_class_field_initiation_finalization_function
  (FILE *f, int initiation_flag);
static void output_title_of_field_deletion_function (FILE *f);
static void output_title_of_graph_deletion_function
  (FILE *f, int internal_function_flag, int class_prefix_flag);
static void output_title_of_field_copy_function (FILE *f);
static void output_title_of_graph_copy_function (FILE *f,
                                                 int internal_function_flag,
                                                 int class_prefix_flag);
static void output_title_of_field_equality_function (FILE *f);
static void output_title_of_graph_equality_function
  (FILE *f, int internal_function_flag, int class_prefix_flag);
static void output_title_of_field_check_function (FILE *f);
static void output_title_of_field_print_for_check_function (FILE *f);
static void output_title_of_graph_check_function
  (FILE *f, int internal_function_flag, int class_prefix_flag);
static void output_title_of_field_print_function (FILE *f);
static void output_title_of_field_output_function (FILE *f);
static void output_title_of_field_traverse_function (FILE *f,
                                                     int reverse_flag);
static void output_title_of_traverse_function (FILE *f, int reverse_flag,
                                               int internal_function_flag,
                                               int class_prefix_flag);
static void output_title_of_transformation_function
   (FILE *f, int internal_function_flag, int class_prefix_flag);
static void output_title_of_field_transformation_function (FILE *f);
static void output_title_of_field_input_function (FILE *f);
static void output_title_of_node_input_function (FILE *f);


/* The following variable value is TRUE iff there is a double linked
   nodes. */

static int double_node_types_existence_flag;

/* The following function is called from function
   `traverse_node_types' in order to set flag
   `double_node_types_existence_flag'. */

static void
check_double_node_types_existence (IR_node_t node_type)
{
  if (IR_double_node_flag (node_type))
    double_node_types_existence_flag = TRUE;
}

/* The following variable value is TRUE iff there is a class field in
   the description. */

static int there_is_a_class_field;

/* The following function is called from function `traverse_all_fields'
   in order to set flag `there_is_a_class_field'. */

static void
check_class_field_presence (IR_node_t field)
{
  if (IR_declaration_part (field) == DP_CLASS)
    there_is_a_class_field = TRUE;
}


/* The following function outputs declarations of friend classes and
   functions in classes IR_double_link and IR_node. */

static void
output_friends (int in_node_class)
{
  assert (cpp_flag);
  if (in_node_class)
    {
      output_string (output_interface_file, "  friend class ");
      output_double_link_structure_name (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  else
    {
      output_string (output_interface_file, "  friend class ");
      output_root_struct_class_name (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  output_string (output_interface_file, "  friend  ");
  output_title_of_set_double_field_value_function (output_interface_file);
  output_string (output_interface_file, ";\n");
  if (there_is_a_class_field)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_initiation_function (output_interface_file);
      output_string (output_interface_file, ";\n  friend  ");
      output_title_of_class_field_initiation_finalization_function
        (output_interface_file, TRUE);
      output_string (output_interface_file, ";\n  friend  ");
      output_title_of_class_field_initiation_finalization_function
        (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n");
    }
  if (free_flag || free_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_deletion_function (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (free_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_graph_deletion_function (output_interface_file,
                                               TRUE, FALSE);
      output_string (output_interface_file, ";\n");
    }
  if (copy_flag || copy_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_copy_function (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (copy_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_graph_copy_function (output_interface_file, TRUE,
                                           FALSE);
      output_string (output_interface_file, ";\n");
    }
  if (equal_flag || equal_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_equality_function (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (equal_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_graph_equality_function (output_interface_file,
                                               TRUE, FALSE);
      output_string (output_interface_file, ";\n");
    }
  if (check_flag || check_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_check_function (output_interface_file);
      output_string (output_interface_file, ";\n  friend  ");
      output_title_of_field_print_for_check_function (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (check_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_graph_check_function (output_interface_file,
                                            TRUE, FALSE);
      output_string (output_interface_file, ";\n");
    }
  if (print_flag || check_flag || check_graph_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_print_function (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (output_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_output_function (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (traverse_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_traverse_function (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n  friend  ");
      output_title_of_traverse_function (output_interface_file,
                                         FALSE, TRUE, FALSE);
      output_string (output_interface_file, ";\n");
    }
  if (reverse_traverse_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_traverse_function (output_interface_file, TRUE);
      output_string (output_interface_file, ";\n  friend  ");
      output_title_of_traverse_function (output_interface_file,
                                         TRUE, TRUE, FALSE);
      output_string (output_interface_file, ";\n");
    }
  if (transform_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_transformation_function (output_interface_file,
                                               TRUE, FALSE);
      output_string (output_interface_file, ";\n  friend  ");
      output_title_of_field_transformation_function (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (input_flag)
    {
      output_string (output_interface_file, "  friend  ");
      output_title_of_field_input_function (output_interface_file);
      output_string (output_interface_file, ";\n  friend  ");
      output_title_of_node_input_function (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  output_string (output_interface_file, "\n");
}

/* The following function outputs definition of struct or class
   `IR_double_link'. */

static void
output_double_link_type_definition (void)
{
  if (!cpp_flag)
    output_string (output_interface_file, "typedef struct ");
  else
    output_string (output_interface_file, "typedef class ");
  output_double_link_structure_name (output_interface_file);
  output_string(output_interface_file, " *");
  output_double_link_t_name (output_interface_file);
  output_string (output_interface_file, ";\n\n");
  if (cpp_flag)
    output_string (output_interface_file, "class ");
  else
    output_string (output_interface_file, "struct ");
  output_double_link_structure_name (output_interface_file);
  output_string (output_interface_file, "\n{\n");
  if (cpp_flag)
    output_friends (FALSE);
  output_string (output_interface_file, "  ");
  output_node_t_name (output_interface_file);
  /* Field itself */
  output_string (output_interface_file, " ");
  output_string (output_interface_file, DOUBLE_LINK_FIELD_NAME);
  output_string (output_interface_file, ";\n  ");
  /* Owner of link. */
  output_node_t_name (output_interface_file);
  output_string (output_interface_file, " ");
  output_string (output_interface_file, DOUBLE_LINK_OWNER_NAME);
  output_string (output_interface_file, ";\n  ");
  if (set_flag)
    {
      /* Set function of link. */
      if (cpp_flag)
        {
          output_string (output_interface_file, "void (");
          output_root_struct_class_name (output_interface_file);
          output_string (output_interface_file, "::*");
        }
      else
        output_string (output_interface_file, "void (*");
      output_string (output_interface_file, DOUBLE_LINK_SET_FUNCTION_NAME);
      output_string (output_interface_file, ") (");
      if (!cpp_flag)
        {
          output_node_t_name (output_interface_file);
          output_string (output_interface_file, ", ");
        }
      output_node_t_name (output_interface_file);
      output_string (output_interface_file, ");\n  ");
    }
  /* Previous link */
  output_double_link_t_name (output_interface_file);
  output_string (output_interface_file, " ");
  output_string (output_interface_file, DOUBLE_LINK_PREVIOUS_NAME);
  output_string (output_interface_file, ";\n  ");
  /* Next link */
  output_double_link_t_name (output_interface_file);
  output_string (output_interface_file, " ");
  output_string (output_interface_file, DOUBLE_LINK_NEXT_NAME);
  output_string (output_interface_file, ";\n");
  if (cpp_flag)
    {
      output_string (output_interface_file, "public:\n\n");
      if (access_flag)
        output_rest_double_link_functions_definitions ();
      if (set_flag)
        output_set_double_link_function_definition ();
    }
  output_string (output_interface_file, "};\n\n");
}

/* The following function outputs definitions of fields which are
   common all structures representing node types.  The most probable
   output is following
     IR_node_mode_t  <node mode member name>;
     IR_node_t  <name of temporary member used for traverse>;
     IR_double_link_t  <name for first double linked node>; */

static void 
output_common_field_definitions (void)
{
  output_string (output_interface_file, "  ");
  output_node_mode_t_name (output_interface_file);
  output_char (' ', output_interface_file);
  output_node_mode_member_name (output_interface_file);
  output_string (output_interface_file, ";\n");
  if (graph_pass_number_member_flag)
    {
      output_string (output_interface_file, "  ");
      output_string (output_interface_file, GRAPH_PASS_NUMBER_TYPE_NAME);
      output_char (' ', output_interface_file);
      output_last_graph_pass_number_member_name (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (copy_graph_flag || transform_flag)
    {
      output_string (output_interface_file, "  ");
      output_node_t_name (output_interface_file);
      output_char (' ', output_interface_file);
      output_temporary_member_name (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
  if (double_node_types_existence_flag)
    {
      /* All class structures have member for for double links when one
         of them has one. */
      output_string (output_interface_file, "  ");
      output_double_link_t_name (output_interface_file);
      output_char (' ', output_interface_file);
      output_first_link_member_name (output_interface_file);
      output_string (output_interface_file, ";\n");
    }
}

/* This recursive function traverses all super types of given node
   type and outputs structure (for non-flat model) or members of the
   structure representing non-class fields of given node type.  To
   make this the function `output_node_type_field_definition' is
   used. */

static void
output_node_type_definitions_and_structures (IR_node_t node_type, int level)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;
  int there_are_fields;
  int i;

  if (!flat_structure_flag && IR_fields_structure_has_been_output (node_type)
      || node_type == root_node_type)
    return;
  IR_set_fields_structure_has_been_output (node_type, TRUE);
  if (!flat_structure_flag)
    for (curr_supertype_list_element
         = IR_first_super_type_list_element (node_type);
         curr_supertype_list_element != NULL;
         curr_supertype_list_element
         = IR_next_super_type_list_element (curr_supertype_list_element))
      if (IR_immediate_super_type (curr_supertype_list_element) != NULL)
        output_node_type_definitions_and_structures
          (IR_immediate_super_type (curr_supertype_list_element), 0);
  there_are_fields = node_type_field_presence (node_type, TRUE);
  if (level == 0)
    {
      if (flat_structure_flag)
        {
          output_string (output_interface_file, "typedef struct\n{\n");
          output_common_field_definitions ();
        }
      else if (there_are_fields)
        {
          output_string (output_interface_file, "struct ");
          output_node_structure_name (output_interface_file,
                                      IR_identifier_itself (IR_type_identifier
                                                            (node_type)));
          output_string (output_interface_file, "\n{\n");
          IR_set_node_structure_has_been_output (node_type, TRUE);
        }
    }
  for (curr_supertype_list_element
       = IR_first_super_type_list_element (node_type);
       curr_supertype_list_element != NULL;
       curr_supertype_list_element
       = IR_next_super_type_list_element (curr_supertype_list_element))
    if (IR_immediate_super_type (curr_supertype_list_element) != NULL
        && node_type_field_presence (IR_immediate_super_type
                                     (curr_supertype_list_element), TRUE))
      {
        if (!flat_structure_flag)
          {
            output_string (output_interface_file, "  struct ");
            output_node_structure_name
              (output_interface_file,
               IR_identifier_itself (IR_type_identifier
                                     (IR_immediate_super_type
                                      (curr_supertype_list_element))));
          }
        else
          {
            for (i = 0; i <= level; i++)
              output_string (output_interface_file, "  ");
            output_string (output_interface_file, "struct {\n");
            output_node_type_definitions_and_structures
              (IR_immediate_super_type (curr_supertype_list_element),
               level + 1);
            for (i = 0; i <= level; i++)
              output_string (output_interface_file, "  ");
            output_string (output_interface_file, "}");
          }
        output_char (' ', output_interface_file);
        output_super_type_member_name
          (output_interface_file,
           IR_identifier_itself (IR_type_identifier
                                 (IR_immediate_super_type
                                  (curr_supertype_list_element))));
        output_string (output_interface_file, ";\n");
      }
  /* Output node type field definitions */
  if (IR_last_field (node_type) != NULL)
    for (current_field = IR_next_field (IR_last_field (node_type));;
         current_field = IR_next_field (current_field))
      {
        if (IR_NODE_MODE (current_field) == IR_NM_field)
          {
            if (IR_declaration_part (current_field) != DP_CLASS)
              {
                for (i = 0; i <= level; i++)
                  output_string (output_interface_file, "  ");
                if (!IR_double_field_flag (current_field))
                  output_field_type_name (output_interface_file,
                                          IR_field_type (current_field));
                else
                  {
                    output_string (output_interface_file, "struct ");
                    output_double_link_structure_name (output_interface_file);
                  }
                output_string (output_interface_file, "  ");
                output_string (output_interface_file,
                               IR_identifier_itself (IR_field_identifier
                                                     (current_field)));
                output_string (output_interface_file, ";\n");
              }
          }
        else
          assert (IR_NODE_MODE (current_field) == IR_NM_action
                  || IR_NODE_MODE (current_field) == IR_NM_constraint);
        if (current_field == IR_last_field (node_type))
          break;
      }
  if (level == 0)
    {
      if (flat_structure_flag)
        {
          output_string (output_interface_file, "} ");
          output_node_type_name (output_interface_file,
                                 IR_identifier_itself (IR_type_identifier
                                                       (node_type)));
          output_string (output_interface_file, ";\n\n");
        }
      else
        {
          if (there_are_fields)
            output_string (output_interface_file, "};\n\n");
          output_string (output_interface_file, "typedef struct\n{\n");
          output_common_field_definitions ();
          if (there_are_fields)
            {
              output_string (output_interface_file, "  struct ");
              output_node_structure_name
                (output_interface_file,
                 IR_identifier_itself (IR_type_identifier (node_type)));
              output_char (' ', output_interface_file);
              output_node_structure_name
                (output_interface_file,
                 IR_identifier_itself (IR_type_identifier (node_type)));
              output_string (output_interface_file, ";\n");
            }
          output_string (output_interface_file, "} ");
          output_node_type_name (output_interface_file,
                                 IR_identifier_itself (IR_type_identifier
                                                       (node_type)));
          output_string (output_interface_file, ";\n\n");
        }
    }
}

/* This function traverses all super types of given node type and
   outputs structure (for non-flat model) or members of the structure
   representing non-class fields of given node type.  To make this the
   function `output_node_type_definitions_and_structures' is used.
   The function is called from function `traverse_node_types'. */

static void
output_node_type_definition (IR_node_t node_type)
{
  output_node_type_definitions_and_structures (node_type, 0);
}

static void output_first_double_link_function_declaration (void);
static void output_field_access_function_definition (IR_node_t field);
static void output_field_modification_function_definition (IR_node_t field);
static void output_node_type_macros_functions (void);
static void output_node_deletion_function_declaration (void);
static void output_creation_function_declaration ();
static void
 output_node_type_specific_creation_function_declaration (IR_node_t node_type);
static void output_graph_deletion_function_declaration (void);
static void output_conditional_graph_deletion_function_declaration (void);
static void output_title_of_node_copy_function (FILE *f,
                                                int class_prefix_flag);
static void output_title_of_conditional_copy_of_graph_function
  (FILE *f, int class_prefix_flag);
static void output_title_of_node_equality_function (FILE *f,
                                                    int class_prefix_flag);
static void output_title_of_conditional_equality_of_graph_function
  (FILE *f, int class_prefix_flag);
static void output_title_of_node_check_function (FILE *f,
                                                 int class_prefix_flag);
static void output_title_of_conditional_graph_check_function
  (FILE *f, int class_prefix_flag);
static void output_title_of_node_print_function (FILE *f,
                                                 int class_prefix_flag);
static void output_title_of_node_output_function (FILE *f,
                                                  int class_prefix_flag);
static void output_title_of_traverse_function (FILE *f, int reverse_flag,
                                               int internal_function_flag,
                                               int class_prefix);

/* The following function outputs definitions of type representing all
   node types, structure (class) and typedef representing double
   linked fields (with the aid of function
   `output_double_link_type_definition'), structures (classes) and
   typedefs representing node types `%root' and error nodes (with the
   aid of function `output_common_field_definitions'), structures
   (classes) and typedefs representing all node types (with the aid of
   function `output_all_node_type_field_definitions'), and definition
   of macros `IR_NODE_MODE' (C interface). */

static void
output_node_type_definitions (void)
{
  /* IR_node_t */
  if (cpp_flag)
    output_string (output_interface_file, "typedef class ");
  else
    output_string (output_interface_file, "typedef struct ");
  output_root_struct_class_name (output_interface_file);
  output_string (output_interface_file, " *");
  output_node_t_name (output_interface_file);
  output_string (output_interface_file, ";\n\n");
  /* IR_double_link_t */
  output_double_link_type_definition ();
  /* Typedef for node types */
  traverse_node_types (output_node_type_definition);
  /* Typedef for root */
  if (cpp_flag)
    output_string (output_interface_file, "typedef class ");
  else
    output_string (output_interface_file, "typedef struct ");
  output_root_struct_class_name (output_interface_file);
  output_string (output_interface_file, "\n{\n");
  if (cpp_flag)
    output_friends (TRUE);
  output_common_field_definitions ();
  if (cpp_flag)
    {
      /* Disable standard constructors and destructors */
      output_string (output_interface_file, "\nprivate:\n  ");
      output_root_struct_class_name (output_interface_file);
      output_string (output_interface_file, " (void) {}\n  ");
      output_root_struct_class_name (output_interface_file);
      output_string (output_interface_file, " (int i) {}\n  ");
      output_root_struct_class_name (output_interface_file);
      output_string (output_interface_file, " (const ");
      output_root_struct_class_name (output_interface_file);
      output_string (output_interface_file, "& n) {}\n  ~");
      output_root_struct_class_name (output_interface_file);
      output_string (output_interface_file, " (void) {}\n\n");
      output_string (output_interface_file, "public:\n");
      output_creation_function_declaration ();
      if (free_flag || free_graph_flag)
        output_node_deletion_function_declaration ();
      if (new_flag)
        traverse_node_types
          (output_node_type_specific_creation_function_declaration);
      /* Inline function IR_node_mode */
      output_string (output_interface_file, "  inline ");
      output_node_mode_t_name (output_interface_file);
      output_string (output_interface_file, " ");
      output_node_mode_name (output_interface_file);
      output_string (output_interface_file,
                     " (void)\n    {\n      return ");
      output_node_mode_member_name (output_interface_file);
      output_string (output_interface_file, ";\n    }\n\n");
      output_node_type_macros_functions ();
      if (access_flag)
        {
	  output_first_double_link_function_declaration ();
          traverse_all_fields (output_field_access_function_definition);
        }
      if (set_flag)
        traverse_all_fields (output_field_modification_function_definition);
      if (free_graph_flag)
        {
          output_graph_deletion_function_declaration ();
          output_conditional_graph_deletion_function_declaration ();
        }
      if (copy_flag || copy_graph_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_node_copy_function (output_interface_file, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (copy_graph_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_graph_copy_function (output_interface_file,
                                               FALSE, FALSE);
          output_string (output_interface_file, "  ");
          output_string (output_interface_file, ";\n  ");
          output_title_of_conditional_copy_of_graph_function
            (output_interface_file, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (equal_flag || equal_graph_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_node_equality_function (output_interface_file,
                                                  FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (equal_graph_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_graph_equality_function (output_interface_file,
                                                   FALSE, FALSE);
          output_string (output_interface_file, "  ");
          output_string (output_interface_file, ";\n  ");
          output_title_of_conditional_equality_of_graph_function
            (output_interface_file, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (check_flag || check_graph_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_node_check_function (output_interface_file, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (check_graph_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_graph_check_function (output_interface_file,
                                                FALSE, FALSE);
          output_string (output_interface_file, ";\n  ");
          output_title_of_conditional_graph_check_function
            (output_interface_file, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (print_flag || check_flag || check_graph_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_node_print_function (output_interface_file, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (output_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_node_output_function (output_interface_file, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (traverse_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_traverse_function (output_interface_file, FALSE,
                                             FALSE, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (reverse_traverse_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_traverse_function (output_interface_file, TRUE,
                                             FALSE, FALSE);
          output_string (output_interface_file, ";\n");
        }
      if (transform_flag)
        {
          output_string (output_interface_file, "  ");
          output_title_of_transformation_function (output_interface_file,
                                                   FALSE, FALSE);
          output_string (output_interface_file, ";\n");
        }
    }
  output_string (output_interface_file, "} ");
  output_node_type_name (output_interface_file, ROOT_NAME);
  output_string (output_interface_file, ";\n\n");
  /* Typedef for error */
  output_string (output_interface_file, "typedef struct\n{\n");
  output_common_field_definitions ();
  output_string (output_interface_file, "} ");
  output_node_type_name (output_interface_file, ERROR_NAME);
  output_string (output_interface_file, ";\n\n");
  if (!cpp_flag)
    {
      /* Macro IR_NODE_MODE */
      output_string (output_interface_file, "#define ");
      output_node_mode_name (output_interface_file);
      output_string (output_interface_file, "(t) ((t)->");
      output_node_mode_member_name (output_interface_file);
      output_string (output_interface_file, ")\n\n");
    }
}



/* This page contains functions for output of structures representing
   class fields and array of pointers to these structures.  The index
   of array corresponds node modes.*/

/* This recursive function can traverse all super types of given node
   type and determines presence of class fields in node of given type.
   The function returns TRUE iff a class field exists in node of given
   type. */

static int
node_type_class_field_presence (IR_node_t node_type, int recursive_flag)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (recursive_flag)
    for (curr_supertype_list_element
         = IR_first_super_type_list_element (node_type);
         curr_supertype_list_element != NULL;
         curr_supertype_list_element
         = IR_next_super_type_list_element (curr_supertype_list_element))
      if (IR_immediate_super_type (curr_supertype_list_element) != NULL
          && node_type_class_field_presence (IR_immediate_super_type
                                             (curr_supertype_list_element),
                                             recursive_flag))
        return TRUE;
  if (IR_last_field (node_type) != NULL)
    for (current_field = IR_next_field (IR_last_field (node_type));;
         current_field = IR_next_field (current_field))
      {
        if (IR_NODE_MODE (current_field) == IR_NM_field)
          {
            if (IR_declaration_part (current_field) == DP_CLASS)
              return TRUE;
          }
        else
          assert (IR_NODE_MODE (current_field) == IR_NM_action
                  || IR_NODE_MODE (current_field) == IR_NM_constraint);
        if (current_field == IR_last_field (node_type))
          break;
      }
  return FALSE;
}

/* This function outputs declaration of variables which contains class
   fields for node type with given name.  The function outputs the
   following fragment
           struct <node_type_class_structure_name>
                  <node_type_class_structure_name>; */

static void
output_declaration_of_node_type_class_structure (FILE *f,
                                                 const char *node_type_name)
{
  output_string (f, "struct ");
  output_node_type_class_structure_name (f, node_type_name);
  output_char (' ', f);
  output_node_type_class_structure_name (f, node_type_name);
  output_string (f, ";\n\n");
}

/* The following function outputs structures representing given node
   type.  The indentation is defined by the second parameter. */

static void
output_node_type_class_structures (IR_node_t node_type, int level)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;
  int there_are_class_fields;
  int i;

  if (!flat_structure_flag
      && IR_class_fields_structure_has_been_output (node_type)
      || node_type == root_node_type)
    return;
  IR_set_class_fields_structure_has_been_output (node_type, TRUE);
  if (!flat_structure_flag)
    for (curr_supertype_list_element
         = IR_first_super_type_list_element (node_type);
         curr_supertype_list_element != NULL;
         curr_supertype_list_element
         = IR_next_super_type_list_element (curr_supertype_list_element))
      if (IR_immediate_super_type (curr_supertype_list_element) != NULL)
        output_node_type_class_structures
          (IR_immediate_super_type (curr_supertype_list_element), 0);
  there_are_class_fields = node_type_class_field_presence (node_type, TRUE);
  if (there_are_class_fields && level == 0)
    {
      output_string (output_interface_file, "struct ");
      output_node_type_class_structure_name
        (output_interface_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_interface_file, "\n{\n");
    }
  for (curr_supertype_list_element
       = IR_first_super_type_list_element (node_type);
       curr_supertype_list_element != NULL;
       curr_supertype_list_element
       = IR_next_super_type_list_element (curr_supertype_list_element))
    if (IR_immediate_super_type (curr_supertype_list_element) != NULL
        && node_type_class_field_presence (IR_immediate_super_type
                                           (curr_supertype_list_element),
                                           TRUE))
      {
        if (!flat_structure_flag)
          {
            output_string (output_interface_file, "  struct ");
            output_node_type_class_structure_name
              (output_interface_file,
               IR_identifier_itself (IR_type_identifier
                                     (IR_immediate_super_type
                                      (curr_supertype_list_element))));
          }
        else
          {
            for (i = 0; i <= level; i++)
              output_string (output_interface_file, "  ");
            output_string (output_interface_file, "struct {\n");
            output_node_type_class_structures
              (IR_immediate_super_type (curr_supertype_list_element),
               level + 1);
            for (i = 0; i <= level; i++)
              output_string (output_interface_file, "  ");
            output_string (output_interface_file, "}");
          }
        output_char (' ', output_interface_file);
        output_super_type_member_name
          (output_interface_file,
           IR_identifier_itself (IR_type_identifier
                                 (IR_immediate_super_type
                                  (curr_supertype_list_element))));
        output_string (output_interface_file, ";\n");
      }
  /* Output node type class field definitions */
  if (IR_last_field (node_type) != NULL)
    for (current_field = IR_next_field (IR_last_field (node_type));;
         current_field = IR_next_field (current_field))
      {
        if (IR_NODE_MODE (current_field) == IR_NM_field)
          {
            if (IR_declaration_part (current_field) == DP_CLASS)
              {
                for (i = 0; i < level; i++)
                  output_string (output_interface_file, "  ");
                output_string (output_interface_file, "  ");
                output_field_type_name (output_interface_file,
                                        IR_field_type (current_field));
                output_string (output_interface_file, "  ");
                output_string (output_interface_file,
                               IR_identifier_itself (IR_field_identifier
                                                     (current_field)));
                output_string (output_interface_file, ";\n");
              }
          }
        else
          assert (IR_NODE_MODE (current_field) == IR_NM_action
                  || IR_NODE_MODE (current_field) == IR_NM_constraint);
        if (current_field == IR_last_field (node_type))
          break;
      }
  if (there_are_class_fields && level == 0)
    {
      output_string (output_interface_file, "};\n\n");
      if (!IR_abstract_flag (node_type))
        {
          /* Extern definition of the structure variable */
          output_string (output_interface_file, "extern ");
          output_declaration_of_node_type_class_structure
            (output_interface_file,
             IR_identifier_itself (IR_type_identifier (node_type)));
          /* Definition of the structure variable */
          output_declaration_of_node_type_class_structure
            (output_implementation_file,
             IR_identifier_itself (IR_type_identifier (node_type)));
        }
    }
}

/* This function outputs part declaration of array which contains
   pointers to all structures representing class fields.  The function
   outputs the following fragment
            void *<class_structure_array_name> [] */

static void
output_class_structure_array_declaration (FILE *f)
{
  output_string (f, "void *");
  output_class_structure_array_name (f);
  output_string (f, " []");
}

/* If given node type contains class fields the function outputs class
   structure for given node type with the aid of function
   `output_node_type_class_structures'.  If given node type is not
   abstract node type then the function also outputs extern definition
   and declaration of variable of type of the class structure with the
   aid of function `output_finish_of_node_type_class_structure'.  The
   function is called from function `traverse_node_types'. */

static void
output_class_structure_of_node_type (IR_node_t node_type)
{
  output_node_type_class_structures (node_type, 0);
}

/* The following function outputs address of class structure for given
   node type if node of given type has class fields, otherwise the
   function outputs NULL.  The function outputs the following fragment
   `<address of corresponding class structure>,' The function is
   called from function `traverse_node_types'. */

static void
output_class_structure_refs_of_node_type (IR_node_t node_type)
{
  output_string (output_implementation_file, "  ");
  if (!IR_abstract_flag (node_type)
      && node_type_class_field_presence (node_type, TRUE))
    {
      output_char ('&', output_implementation_file);
      output_node_type_class_structure_name
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
    }
  else
    output_string (output_implementation_file, "NULL");
  output_string (output_implementation_file, ",\n");
}

/* The following function outputs all class structures (with the aid
   of function `output_class_structures') and array of pointers to the
   class structures (with the aid of functions
   `output_class_structure_refs_of_node_type' and
   `output_class_structure_array_declaration'). */

static void
output_class_structures (void)
{
  /* Extern definition of class structure array */
  output_string (output_interface_file, "extern ");
  output_class_structure_array_declaration (output_interface_file);
  output_string (output_interface_file, ";\n\n");
  traverse_node_types (output_class_structure_of_node_type);
  /* Definition of class structure array */
  output_class_structure_array_declaration (output_implementation_file);
  output_string (output_implementation_file, " =\n{\n");
  traverse_node_types (output_class_structure_refs_of_node_type);
  output_string (output_implementation_file, "};\n\n");
}



/* This page contains functions for output of array containing names
   of all node types. */

/* The following function outputs name of give node type as C string.
   The function outputs the following fragment
            `"<node type name>",' */

static void
output_node_name (IR_node_t node_type)
{
  output_string (output_implementation_file, "  \"");
  output_string (output_implementation_file,
                 IR_identifier_itself (IR_type_identifier (node_type)));
  output_string (output_implementation_file, "\",\n");
}

/* The following function outputs declaration and extern definition of
   array containing names of all node types. */

static void
output_node_names (void)
{
  /* Extern definition for `IR_node_name' */
  output_string (output_interface_file, "extern char *");
  output_node_name_array_name (output_interface_file);
  output_string (output_interface_file, "[];\n\n");
  /* Declaration for `IR_node_name' */
  output_string (output_implementation_file, "char *");
  output_node_name_array_name (output_implementation_file);
  output_string (output_implementation_file, "[] =\n{\n");
  traverse_node_types (output_node_name);
  output_string (output_implementation_file, "  \"%root\",\n");
  output_string (output_implementation_file, "  \"%error\"\n");
  output_string (output_implementation_file, "};\n\n");
}



/* This page contains functions for output of array containing sizes
   of nodes of all node types. */

/* The following function outputs size of give node type.  The
   function outputs the following fragment
                    `sizeof (<node type name>),'
   The function is called from function `traverse_node_types'. */

static void
output_node_size (IR_node_t node_type)
{
  output_string (output_implementation_file, "  sizeof (");
  output_node_type_name (output_implementation_file,
                         IR_identifier_itself (IR_type_identifier
                                               (node_type)));
  output_string (output_implementation_file, "),\n");
}

/* The following function outputs declaration and extern definition of
   array containing sizes of all node types. */

static void
output_node_sizes (void)
{
  /* Extern definition for IR_node_size */
  if (long_node_size_flag)
    output_string (output_interface_file, "extern unsigned long ");
  else if (short_node_size_flag)
    output_string (output_interface_file, "extern unsigned short ");
  else
    output_string (output_interface_file, "extern unsigned char ");
  output_node_size_array_name (output_interface_file);
  output_string (output_interface_file, "[];\n\n");
  /* Declaration for IR_node_size */
  if (long_node_size_flag)
    output_string (output_implementation_file, "unsigned long ");
  else if (short_node_size_flag)
    output_string (output_implementation_file, "unsigned short ");
  else
    output_string (output_implementation_file, "unsigned char ");
  output_node_size_array_name (output_implementation_file);
  output_string (output_implementation_file, "[] =\n{\n");
  traverse_node_types (output_node_size);
  output_string (output_implementation_file, "  sizeof (");
  output_node_type_name (output_implementation_file, ROOT_NAME);
  output_string (output_implementation_file, "),\n");
  output_string (output_implementation_file, "  sizeof (");
  output_node_type_name (output_implementation_file, ERROR_NAME);
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, "};\n\n");
}



/* This page contains functions for output arrays needed for determine
   relation type sub-type and macros `NODE_LEVEL', `IR_IS_TYPE', and
   `IR_IS_OF_TYPE'. */

/* The following function outputs level of give node type.  The
   function outputs the following fragment
            `<level of first declaration of give node type>,'
   The function is called from function `traverse_node_types'. */

static void
output_node_type_level_array_element (IR_node_t node_type)
{
  output_string (output_implementation_file, "  ");
  output_decimal_number (output_implementation_file,
                         IR_first_declaration_level (node_type));
  output_string (output_implementation_file, ",\n");
}

/* The following variable is array [0..number_of_node_types-1] of
                               array  [0..number_of_node_types-1] of boolean.
   The array contains flags of that node type corresponding to to the
   second index of the array is sub type of node type corresponding to
   the first index of the array. */

static char *is_type_flag_array;

/* The following recursive function collects information about that
   given node type is subtype of other node types. */

static void
determine_is_type_flag_array_elements_for_node_and_super_types
  (IR_node_t node_type, IR_node_t super_type)
{
  IR_node_t curr_supertype_list_element;

  assert (node_type != NULL && super_type != NULL);
  is_type_flag_array [IR_node_type_number (super_type) * number_of_node_types
                      + IR_node_type_number (node_type)] = 1;
  for (curr_supertype_list_element
       = IR_first_super_type_list_element (super_type);
       curr_supertype_list_element != NULL;
       curr_supertype_list_element
       = IR_next_super_type_list_element (curr_supertype_list_element))
    if (IR_immediate_super_type (curr_supertype_list_element) != NULL)
      determine_is_type_flag_array_elements_for_node_and_super_types
        (node_type, IR_immediate_super_type (curr_supertype_list_element));
}

/* The following function forms array of type subtype relation.  The
   function is called from function `traverse_node_types'. */

static void
determine_is_type_flag_array_elements_for_node (IR_node_t node_type)
{
  determine_is_type_flag_array_elements_for_node_and_super_types (node_type,
                                                                  node_type);
}

/* The following macro value is maximum number of output flags on a line. */

#define NUMBER_OF_FLAGS_ON_LINE 20

/* The following function outputs given array of flags of subtype
   relation for node type with given name. */

static void
output_subtype_flag_array_definition (const char *subtype_flag_array,
                                      const char *node_type_name)
{
  int current_node_type_number;
  int flags_byte;
  
  /* Extern definition for super type flag array */
  output_string (output_interface_file, "extern unsigned char ");
  output_subtype_flag_array_name (output_interface_file, node_type_name);
  output_string (output_interface_file, " [];\n\n");
  /* Declaration for super type flag array */
  output_string (output_implementation_file, "unsigned char ");
  output_subtype_flag_array_name (output_implementation_file, node_type_name);
  output_string (output_implementation_file, " [] =\n{");
  if (!fast_flag)
    {
      for (flags_byte =0, current_node_type_number = 0;
           current_node_type_number < number_of_node_types;
           current_node_type_number++)
        {
          flags_byte
            |= ((subtype_flag_array [current_node_type_number] ? 1 : 0)
                << (current_node_type_number % CHAR_BIT));
          if ((current_node_type_number + 1) % CHAR_BIT == 0)
            {
              output_decimal_number (output_implementation_file, flags_byte);
              if (current_node_type_number + 1 < number_of_node_types)
                {
                  output_string (output_implementation_file, ", ");
                  if (((current_node_type_number + 1) / CHAR_BIT)
                      % NUMBER_OF_FLAGS_ON_LINE == 0)
                    output_string (output_implementation_file, "\n  ");
                }
              flags_byte = 0;
            }
        }
      if (current_node_type_number % CHAR_BIT != 0)
        output_decimal_number (output_implementation_file, flags_byte);
    }
  else
    {
      for (current_node_type_number = 0;
           current_node_type_number < number_of_node_types;
           current_node_type_number++)
        {
          if (current_node_type_number != 0)
            output_string (output_implementation_file, ", ");
          if (current_node_type_number % NUMBER_OF_FLAGS_ON_LINE == 0)
            output_string (output_implementation_file, "\n  ");
          output_char ((subtype_flag_array [current_node_type_number]
                        ? '1' : '0'), output_implementation_file);
        }
    }
  output_string (output_implementation_file, "\n};\n\n");
}

/* The following function outputs array of flags of subtype relation
   for given node type with the aid of function
   `output_subtype_flag_array_definition'.  The function is called
   from function `traverse_node_types'. */

static void
output_subtype_flag_array_for_node (IR_node_t node_type)
{
  output_subtype_flag_array_definition
    (is_type_flag_array
     + IR_node_type_number (node_type) * number_of_node_types,
     IR_identifier_itself (IR_type_identifier (node_type)));
}

/* The following function forms and outputs arrays of flags of subtype
   relation for all node types with the aid of functions
   `output_subtype_flag_array_definition' and
   `output_subtype_flag_array_for_node'.  The function returns
   According to the formed array error node type is subtype only node
   type `%root'. */

static void
output_all_subtype_flag_arrays (void)
{
  int current_node_type_number;

  /* Form is_type_flag_array */
  CALLOC (is_type_flag_array, 1, number_of_node_types * number_of_node_types);
  traverse_node_types (determine_is_type_flag_array_elements_for_node);
  /* Root is super type of all node types */
  for (current_node_type_number = 0;
       current_node_type_number < number_of_node_types;
       current_node_type_number++)
    is_type_flag_array [IR_node_type_number (root_node_type)
                        * number_of_node_types + current_node_type_number] = 1;
  /* Error is super type of itself. */
  is_type_flag_array [number_of_node_types * number_of_node_types - 1] = 1;
  /* Definitions for all subtype flag arrays. */
  traverse_node_types (output_subtype_flag_array_for_node);
  /* For `%root' */
  output_subtype_flag_array_definition
    (is_type_flag_array
     + IR_node_type_number (root_node_type) * number_of_node_types, ROOT_NAME);
  /* For access to error nodes */
  output_subtype_flag_array_definition
    (is_type_flag_array + number_of_node_types * (number_of_node_types - 1),
     ERROR_NAME);
  FREE (is_type_flag_array);
}

/* The following function outputs array address of flags of subtype
   relation for given node type.  The function outputs the following
   fragment
            `<array address>,'
   The function is called from function `traverse_node_types'. */

static void
output_subtype_flag_array_address_for_node (IR_node_t node_type)
{
  output_subtype_flag_array_name (output_implementation_file,
                                  IR_identifier_itself (IR_type_identifier
                                                        (node_type)));
  output_string (output_implementation_file, ",\n");
}

/* The following function outputs arrays needed for determine relation
   type sub-type and macros `NODE_LEVEL' and `IR_IS_TYPE',
   `IR_IS_OF_TYPE'. */

static void
output_node_type_flag_arrays (void)
{
  /* Extern definition for node type level array */
  output_string (output_interface_file, "extern short ");
  output_node_level_array_name (output_interface_file);
  output_string (output_interface_file, " [];\n\n");
  /* Declaration for node type level array */
  output_string (output_implementation_file, "short ");
  output_node_level_array_name (output_implementation_file);
  output_string (output_implementation_file, " [] =\n{\n");
  traverse_node_types (output_node_type_level_array_element);
  /* For `%root' */
  output_string (output_implementation_file, "  0,\n");
  /* For  of error nodes */
  output_string (output_implementation_file, "  0\n};\n\n");
  /* Output subtype flag array for all node types. */
  output_all_subtype_flag_arrays ();
  /* Extern definition for is type flag array */
  output_string (output_interface_file, "extern unsigned char *");
  output_is_type_array_name (output_interface_file);
  output_string (output_interface_file, "[];\n\n");
  /* Declaration for is type flag array */
  output_string (output_implementation_file, "unsigned char *");
  output_is_type_array_name (output_implementation_file);
  output_string (output_implementation_file, "[] =\n{\n");
  traverse_node_types (output_subtype_flag_array_address_for_node);
  /* For `%root' */
  output_subtype_flag_array_name (output_implementation_file, ROOT_NAME);
  output_string (output_implementation_file, ",\n");
  /* For error nodes */
  output_subtype_flag_array_name (output_implementation_file, ERROR_NAME);
  output_string (output_implementation_file, "\n};\n\n");
}

/* The function outputs macro `<prefix>NODE_LEVEL' (C interface) or
   inline function `<prefix>node_level' (see documentation of
   SPRUT). */

static void
output_node_type_macros_functions (void)
{
  /* Macro `IR_NODE_LEVEL' or function `IR_node_level'. */
  if (!cpp_flag)
    output_string (output_interface_file, "#define ");
  else
    output_string (output_interface_file, "  int ");
  output_node_level_name (output_interface_file);
  output_string (output_interface_file, cpp_flag ? " (void)" : "(node) ");
  if (cpp_flag)
    {
      output_string (output_interface_file, "\n    {\n");
      output_ifdef (output_interface_file, DEBUG_PARAMETER_NAME);
      output_string (output_interface_file,
                     "      if (this == NULL)\n        abort ();\n");
      output_endif (output_interface_file, DEBUG_PARAMETER_NAME);
      output_string (output_interface_file, "      return ");
    }
  output_node_level_array_name (output_interface_file);
  output_string (output_interface_file, " [");
  if (cpp_flag)
    {
      output_string (output_interface_file, " this->");
      output_node_mode_member_name (output_interface_file);
    }
  else
  output_node_mode_name (output_interface_file);
  if (cpp_flag)
    output_string (output_interface_file, "];\n    }\n\n");
  else
    output_string (output_interface_file, " (node)]\n\n");
  /* Definition for macro `IR_IS_TYPE' or function `IR_is_type'. */
  if (!cpp_flag)
    output_string (output_interface_file, "#define ");
  else
    output_string (output_interface_file, "  static int ");
  output_is_type_name (output_interface_file);
  if (cpp_flag)
    {
      output_string (output_interface_file, " (");
      output_node_mode_t_name (output_interface_file);
      output_string (output_interface_file, " ");
    }
  else
    output_string (output_interface_file, "(");
  output_string (output_interface_file, "type, ");
  if (cpp_flag)
    {
      output_node_mode_t_name (output_interface_file);
      output_string (output_interface_file, " ");
    }
  output_string (output_interface_file, "super)");
  if (cpp_flag)
    output_string (output_interface_file, "\n    {\n      return ");
  if (!fast_flag)
    {
      output_string (output_interface_file, " ((");
      output_is_type_array_name (output_interface_file);
      output_string (output_interface_file, " [super] [type /");
      output_decimal_number (output_interface_file, CHAR_BIT);
      output_string (output_interface_file, "] >> (type % ");
      output_decimal_number (output_interface_file, CHAR_BIT);
      output_string (output_interface_file, ")) & 1)");
    }
  else
    {
      output_string (output_interface_file, " (");
      output_is_type_array_name (output_interface_file);
      output_string (output_interface_file, " [super] [type])");
    }
  if (cpp_flag)
    output_string (output_interface_file, ";\n    }");
  output_string (output_interface_file, "\n\n");
  /* Definition for macro `IR_IS_OF_TYPE' or function `IR_is_of_type'. */
  if (!cpp_flag)
    output_string (output_interface_file, "#define ");
  else
    {
      output_string (output_interface_file, "  int ");
    }
  output_is_of_type_name (output_interface_file);
  output_string (output_interface_file, cpp_flag ? " (" : "(");
  if (!cpp_flag)
    output_string (output_interface_file, "node, ");
  if (cpp_flag)
    {
      output_node_mode_t_name (output_interface_file);
      output_string (output_interface_file, " ");
    }
  output_string (output_interface_file, "super)");
  if (cpp_flag)
    {
      output_string (output_interface_file, "\n    {\n");
      output_ifdef (output_interface_file, DEBUG_PARAMETER_NAME);
      output_string (output_interface_file,
                     "      if (this == NULL)\n        abort ();\n");
      output_endif (output_interface_file, DEBUG_PARAMETER_NAME);
      output_string (output_interface_file, "      return ");
    }
  output_is_type_name (output_interface_file);
  output_string (output_interface_file, " (");
  if (cpp_flag)
    output_string (output_interface_file, "this->");
  output_node_mode_name (output_interface_file);
  if (cpp_flag)
    output_string (output_interface_file, " (), super);\n    }\n\n");
  else
    output_string (output_interface_file, " (node), super)\n\n");
}



/* This page contains functions for output of arrays containing
   displacements of synonym fields and fields which are in node types
   used as secondary super types.  Index of arrays corresponds to node
   types (modes). */


/* The following macro value is maximum number of output flags on a line. */

#define MAX_NUMBER_OF_DISPLACEMENTS_ON_LINE 20

/* The following variable refers to all node types array indexes of which
   are their numbers. */

static IR_node_t *node_type_array;

/* The following function fills `node_type_array' element
   corresponding to given node type.  The function is called from
   function `traverse_node_types'. */

static void
fill_node_type_array_element (IR_node_t node_type)
{
  node_type_array [IR_node_type_number (node_type)] = node_type;
}
/* The following recursive functions searches for path from given node
   type to its super type which contains given field.  The function
   returns TRUE if the field is found, FALSE otherwise. */

static int
find_field_node_type_path (IR_node_t field, IR_node_t node_type,
                           vlo_t *field_node_type_path)
{
  IR_node_t curr_supertype_list_element;

  VLO_ADD_MEMORY (*field_node_type_path, &node_type, sizeof (node_type));
  if (node_type != NULL)
    {
      if (find_node_field (IR_field_identifier (field), node_type) != NULL)
        return TRUE;
      for (curr_supertype_list_element
           = IR_first_super_type_list_element (node_type);
           curr_supertype_list_element != NULL;
           curr_supertype_list_element
           = IR_next_super_type_list_element (curr_supertype_list_element))
        if (find_field_node_type_path
            (field, IR_immediate_super_type (curr_supertype_list_element),
             field_node_type_path))
          return TRUE;
    }
  VLO_SHORTEN (*field_node_type_path, sizeof (node_type));
  return FALSE;
}

/* If given field is synonym field or field which is in node type used
   as secondary super type, the function outputs array containing
   displacements of given fields.  If node type does not contain given
   field then its value is 0.  The function is called from function
   `traverse_all_fields'. */

static void
output_array_of_different_displacement_field_displacement (IR_node_t field)
{
  int number_of_displacements_on_line;
  int current_node_type_number;
  vlo_t field_node_type_path;
  IR_node_t source_node_type;
  IR_node_t *current_super_type_ref;

  assert (IR_NODE_MODE (field) == IR_NM_field);
  if (field == find_field (IR_field_identifier (field))
      && (field != IR_previous_synonym_field (field)
          || IR_secondary_super_type_flag (IR_node_type (field))))
    {
      /* Extern definition */
      if (long_node_size_flag)
        output_string (output_interface_file, "extern unsigned long ");
      else if (short_node_size_flag)
        output_string (output_interface_file, "extern unsigned short ");
      else
        output_string (output_interface_file, "extern unsigned char ");
      output_different_displacement_field_displacement_array_name
        (output_interface_file, IR_identifier_itself (IR_field_identifier
                                                      (field)));
      output_string (output_interface_file, " [];\n\n");
      /* Output declaration */
      if (long_node_size_flag)
        output_string (output_implementation_file, "unsigned long ");
      else if (short_node_size_flag)
        output_string (output_implementation_file, "unsigned short ");
      else
        output_string (output_implementation_file, "unsigned char ");
      output_different_displacement_field_displacement_array_name
        (output_implementation_file, IR_identifier_itself (IR_field_identifier
                                                           (field)));
      output_string (output_implementation_file, " [] =\n{\n  ");
      number_of_displacements_on_line = 0;
      current_node_type_number = 0;
      VLO_CREATE (field_node_type_path, 0);
      for (;;)
        /* For each node type. */
        {
          if (number_of_displacements_on_line
              == MAX_NUMBER_OF_DISPLACEMENTS_ON_LINE)
            output_string (output_implementation_file, "\n  ");
          VLO_NULLIFY (field_node_type_path);
          if (find_field_node_type_path
              (field, node_type_array [current_node_type_number],
               &field_node_type_path))
            {
              assert (VLO_LENGTH (field_node_type_path) != 0);
              if (number_of_displacements_on_line
                  != MAX_NUMBER_OF_DISPLACEMENTS_ON_LINE)
                output_string (output_implementation_file, "\n  ");
              output_offsetof_macro_name (output_implementation_file);
              output_string (output_implementation_file, " (");
              source_node_type
                = *(IR_node_t *) VLO_BEGIN (field_node_type_path);
              if (IR_declaration_part (field) == DP_CLASS)
                {
                  output_string (output_implementation_file, "struct ");
                  output_node_type_class_structure_name
                    (output_implementation_file,
                     IR_identifier_itself (IR_type_identifier
                                           (source_node_type)));
                }
              else
                {
                  output_node_type_name
                    (output_implementation_file,
                     IR_identifier_itself (IR_type_identifier
                                           (source_node_type)));
                  if (!flat_structure_flag)
                    {
                      output_string (output_implementation_file, ", ");
                      output_node_structure_name
                        (output_implementation_file,
                         IR_identifier_itself (IR_type_identifier
                                               (source_node_type)));
                      output_string (output_implementation_file, ")\n    + ");
                      output_offsetof_macro_name (output_implementation_file);
                      output_string (output_implementation_file, " (struct ");
                      output_node_structure_name
                        (output_implementation_file,
                         IR_identifier_itself (IR_type_identifier
                                               (source_node_type)));
                    }
                }
              for (current_super_type_ref
                   = (IR_node_t *) VLO_BEGIN (field_node_type_path);;)
                {
                  output_string (output_implementation_file, ", ");
                  current_super_type_ref++;
                  if (current_super_type_ref
                      > (IR_node_t *) VLO_END (field_node_type_path))
                    break;
                  output_super_type_member_name
                    (output_implementation_file,
                     IR_identifier_itself (IR_type_identifier
                                           (*current_super_type_ref)));
                  output_string (output_implementation_file, ")\n    + ");
                  output_offsetof_macro_name (output_implementation_file);
                  output_string (output_implementation_file, " (");
                  if (IR_declaration_part (field) == DP_CLASS)
                    {
                      output_string (output_implementation_file, "struct ");
                      output_node_type_class_structure_name
                        (output_implementation_file,
                         IR_identifier_itself (IR_type_identifier
                                               (*current_super_type_ref)));
                    }
                  else if (!flat_structure_flag)
                    {
                      output_string (output_implementation_file, "struct ");
                      output_node_structure_name
                        (output_implementation_file,
                         IR_identifier_itself (IR_type_identifier
                                               (*current_super_type_ref)));
                    }
                  else
                    output_node_type_name
                      (output_implementation_file,
                       IR_identifier_itself (IR_type_identifier
                                             (*current_super_type_ref)));
                }
              output_string (output_implementation_file,
                             IR_identifier_itself (IR_field_identifier
                                                   (field)));
              output_char (')', output_implementation_file);
              number_of_displacements_on_line
                = MAX_NUMBER_OF_DISPLACEMENTS_ON_LINE - 1;
            }
          else
            output_char ('0', output_implementation_file);
          number_of_displacements_on_line++;
          if (number_of_displacements_on_line
              > MAX_NUMBER_OF_DISPLACEMENTS_ON_LINE)
            number_of_displacements_on_line = 0;
          current_node_type_number++;
          if (current_node_type_number == number_of_node_types)
            break;
          output_string (output_implementation_file, ", ");
        }
      VLO_DELETE (field_node_type_path);
      output_string (output_implementation_file, "\n};\n\n");
    }
}

/* The following function outputs arrays containing displacements of
   all fields with different displacements with the aid of function
   `output_array_of_different_displacement_field_displacement'. */

static void
output_arrays_of_different_displacement_field_displacement (void)
{
  CALLOC (node_type_array, 1, number_of_node_types * sizeof (IR_node_t));
  traverse_node_types (fill_node_type_array_element);
  traverse_all_fields
    (output_array_of_different_displacement_field_displacement);
  FREE (node_type_array);
}



/* This page contains function for setting up double link field values. */

/* The following macro values are names of parameter of function
   `_IR_set_double_field_value'. */

#define SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME "link"

#define SET_DOUBLE_FIELD_VALUE_FUNCTION_VALUE_PARAMETER_NAME "value"

#define SET_DOUBLE_FIELD_VALUE_FUNCTION_DISPLACEMENT_PARAMETER_NAME "disp"

#define SET_DOUBLE_FIELD_VALUE_FUNCTION_FLAG_PARAMETER_NAME "flag"

/* The following function outputs title of function returning first
   double linked field for a node.  The most probable function output
   is following
     `void _IR_set_double_field_value (IR_double_link_t link,
                                       IR_node_t value, int disp, int flag)' */

static void
output_title_of_set_double_field_value_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_set_double_field_value_function (f);
  output_string (f, "\n  (");
  output_double_link_t_name (f);
  output_char (' ', f);
  output_string (f, SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, SET_DOUBLE_FIELD_VALUE_FUNCTION_VALUE_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f,
                 SET_DOUBLE_FIELD_VALUE_FUNCTION_DISPLACEMENT_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f, SET_DOUBLE_FIELD_VALUE_FUNCTION_FLAG_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs function (and its external
   definition for C interface) setting up value of double field.  The
   most probable function output is following
        `void _IR_set_double_field_value
           (IR_double_link_t link, IR_node_t value, int disp, int flag)
           {
             if (!flag)
               {
                 if (link->field_itself != NULL)
                   { 
                     if (link->previous_link != NULL)
                       link->previous_link->next_link = link->next_link;
                     else
                       *(IR_double_link_t *) ((char *) link->field_itself
                                              + disp)
                         = link->next_link;
                     if (link->next_link != NULL)
                       link->next_link->previous_link = link->previous_link;
                   }
                 link->field_itself = value;
               }
             if (link->field_itself != NULL)
               {
                 link->next_link
                   = *(IR_double_link_t *) ((char *) link->field_itself
                                            + disp);
                 if (*(IR_double_link_t *) ((char *) link->field_itself
                                            + disp) != NULL)
                   (*(IR_double_link_t *) ((char *) link->field_itself
                                           + disp))->previous_link = link;
                 link->previous_link = NULL;
                 *(IR_double_link_t *) ((char *) link->field_itself
                                        + disp) = link;
               }
            }'
*/

static void
output_set_double_field_value_function (void)
{
  /* External definition (it is necessary because the function can be
     used in modification macros): */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_set_double_field_value_function (output_interface_file);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_set_double_field_value_function
    (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  if (double_node_types_existence_flag)
    {
      output_string (output_implementation_file, "  if (!");
      output_string (output_implementation_file,
		     SET_DOUBLE_FIELD_VALUE_FUNCTION_FLAG_PARAMETER_NAME);
      output_string (output_implementation_file, ")\n    {\n");
      /* Deletion from double linked field list */
      output_string (output_implementation_file,
		     "      /* Deletion from double linked field list */\n");
      output_string (output_implementation_file, "      if (");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_FIELD_NAME);
      output_string (output_implementation_file, " != NULL)\n        {\n");
      output_string (output_implementation_file, "          if (");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_PREVIOUS_NAME);
      output_string (output_implementation_file, " != NULL)\n            ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_PREVIOUS_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_NEXT_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_NEXT_NAME);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "          else\n");
      output_string (output_implementation_file, "            *(");
      output_double_link_t_name (output_implementation_file);
      output_string (output_implementation_file, " *) ((char *) ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_FIELD_NAME);
      output_string (output_implementation_file, " + ");
      output_string
	(output_implementation_file,
	 SET_DOUBLE_FIELD_VALUE_FUNCTION_DISPLACEMENT_PARAMETER_NAME);
      output_string (output_implementation_file, ") = ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_NEXT_NAME);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "          if (");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_NEXT_NAME);
      output_string (output_implementation_file, " != NULL)\n            ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_NEXT_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_PREVIOUS_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_PREVIOUS_NAME);
      output_string (output_implementation_file, ";\n        }\n");
      /* Setting up field access. */
      output_string (output_implementation_file, "      ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_FIELD_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_VALUE_PARAMETER_NAME);
      output_string (output_implementation_file, ";\n    }\n");
      /* Include into new double link list */
      output_string (output_implementation_file,
                     "  /* Inclusion into double linked field list */\n");
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file, "if (");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_FIELD_NAME);
      output_string (output_implementation_file, " != NULL)\n    {\n      ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_NEXT_NAME);
      output_string (output_implementation_file, " = *(");
      output_double_link_t_name (output_implementation_file);
      output_string (output_implementation_file, " *) ((char *) ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_FIELD_NAME);
      output_string (output_implementation_file, " + ");
      output_string
	(output_implementation_file,
	 SET_DOUBLE_FIELD_VALUE_FUNCTION_DISPLACEMENT_PARAMETER_NAME);
      output_string (output_implementation_file, ");\n");
      output_string (output_implementation_file, "      if (");
      output_string (output_implementation_file, "*(");
      output_double_link_t_name (output_implementation_file);
      output_string (output_implementation_file, " *) ((char *) ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_FIELD_NAME);
      output_string (output_implementation_file, " + ");
      output_string
	(output_implementation_file,
	 SET_DOUBLE_FIELD_VALUE_FUNCTION_DISPLACEMENT_PARAMETER_NAME);
      output_string (output_implementation_file, ") != NULL)\n");
      output_string (output_implementation_file, "        ");
      output_string (output_implementation_file, "(*(");
      output_double_link_t_name (output_implementation_file);
      output_string (output_implementation_file, " *) ((char *) ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_FIELD_NAME);
      output_string (output_implementation_file, " + ");
      output_string
	(output_implementation_file,
	 SET_DOUBLE_FIELD_VALUE_FUNCTION_DISPLACEMENT_PARAMETER_NAME);
      output_string (output_implementation_file, "))->");
      output_string (output_implementation_file, DOUBLE_LINK_PREVIOUS_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, ";\n      ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_PREVIOUS_NAME);
      output_string (output_implementation_file, " = NULL;\n");
      output_string (output_implementation_file, "      *(");
      output_double_link_t_name (output_implementation_file);
      output_string (output_implementation_file, " *) ((char *) ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_string (output_implementation_file, DOUBLE_LINK_FIELD_NAME);
      output_string (output_implementation_file, " + ");
      output_string
	(output_implementation_file,
	 SET_DOUBLE_FIELD_VALUE_FUNCTION_DISPLACEMENT_PARAMETER_NAME);
      output_string (output_implementation_file, ") = ");
      output_string (output_implementation_file,
                     SET_DOUBLE_FIELD_VALUE_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_implementation_file, ";\n    }\n");
    }
  output_string (output_implementation_file, "}\n\n");
}



/* This page contains functions for output of all field access functions
   and functions for work with double linked fields. */

/* The following macro value is name of parameter of function
   `IR__first_double_link' (for C interaface) or the corrseponding
   node (C++ interface). */

#define FIRST_DOUBLE_LINK_FUNCTION_PARAMETER_NAME (cpp_flag ? "this" : "node")

/* The following function outputs title of function returning first
   double linked field for a node.  The most probable function output
   is following
        `IR_double_link_t IR__first_double_link (IR_node_t node)'
        or
        `IR_double_link_t IR__first_double_link (void)'
        or
        `IR_double_link_t IR_node::IR__first_double_link ()'. */

static void
output_title_of_first_double_link_function (FILE *f, int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  output_double_link_t_name (f);
  output_char (' ', f);
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_first_double_link_function (f);
  output_string (f, " (");
  if (cpp_flag)
    output_string (f, "void");
  else
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, FIRST_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
    }
  output_char (')', f);
}

/* If given node type is not abstract node type the function outputs
   fragment of code needed for returning first double linked field of
   a node.  The function outputs the following fragment
            case <node mode value of given node type>:
              return <access code to corresponding member
                 or NULL if given node type is not double>;

   The function is called from function `traverse_node_types'. */

static void
output_node_type_first_double_link (IR_node_t node_type)
{
  if (!IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      output_string (output_implementation_file, "      return ");
      if (IR_double_node_flag (node_type))
        {
          output_string (output_implementation_file, "((");
          output_node_type_name
            (output_implementation_file,
             (flat_structure_flag
              ? IR_identifier_itself (IR_type_identifier (node_type))
              : ROOT_NAME));
          output_string (output_implementation_file, " *) ");
          output_string (output_implementation_file,
                         FIRST_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
          output_string (output_implementation_file, ")->");
          output_first_link_member_name (output_implementation_file);
        }
      else
        output_string (output_implementation_file, "NULL");
      output_string (output_implementation_file, ";\n");
    }
}

/* The following macro value is name of parameter of function
   `IR__next_double_link' (C interface) or the corresponding double
   link (C++ interface). */

#define NEXT_DOUBLE_LINK_FUNCTION_PARAMETER_NAME \
  (cpp_flag ? "this" : "prev_double_link")

/* The following macro value must be value of previous macro plus
   parentheses. */

#define NEXT_DOUBLE_LINK_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS\
  "(prev_double_link)"

/* The following function outputs title of function returning next
   double linked field.  The most probable function output is
   following
        `IR_double_link_t IR__next_double_link (IR_double_link_t
                                                prev_double_link)' or
        `IR_F_double_link_t IR__next_double_link (IR_double_link_t
                                                  prev_double_link)' or
        `IR__next_double_link(prev_double_link)' or
        `IR_double_link_t IR__next_double_link (void)'. */

static void
output_title_of_next_double_link_function (FILE *f, int as_macro_flag,
                                           int function_prefix_flag)
{
  assert (!cpp_flag || !as_macro_flag && !function_prefix_flag);
  if (!as_macro_flag)
    {
      output_double_link_t_name (f);
      output_char (' ', f);
    }
  output_name_of_next_double_link_function
    (f, !as_macro_flag && function_prefix_flag);
  if (!as_macro_flag)
    {
      output_string (f, " (");
      if (cpp_flag)
        output_string (f, "void");
      else
        {
          output_double_link_t_name (f);
          output_char (' ', f);
          output_string (f, NEXT_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
        }
      output_char (')', f);
    }
  else
    output_string (f, NEXT_DOUBLE_LINK_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS);
}

/* The following macro value is name of parameter of function
   `IR__previous_double_link' (C interface) or the corresponding
   double link. */

#define PREVIOUS_DOUBLE_LINK_FUNCTION_PARAMETER_NAME \
   (cpp_flag ? "this" : "next_double_link")

/* The following macro value must be value of previous macro plus
   parentheses. */

#define PREVIOUS_DOUBLE_LINK_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS\
   "(next_double_link)"

/* The following function outputs title of function returning previous
   double linked field.  The most probable function output is
   following
        `IR_double_link_t IR__previous_double_link (IR_double_link_t
                                                    next_double_link)'
        or
        `IR_double_link_t IRF__previous_double_link (IR_double_link_t
                                                     next_double_link)'
        or
        `IR__previous_double_link(next_double_link)' or
        `IR_double_link_t IR__previous_double_link (void)'. */

static void
output_title_of_previous_double_link_function (FILE *f, int as_macro_flag,
                                               int function_prefix_flag)
{
  assert (!cpp_flag || !as_macro_flag && !function_prefix_flag);
  if (!as_macro_flag)
    {
      output_double_link_t_name (f);
      output_char (' ', f);
    }
  output_name_of_previous_double_link_function
    (f, !as_macro_flag && function_prefix_flag);
  if (!as_macro_flag)
    {
      output_string (f, " (");
      if (cpp_flag)
        output_string (f, "void");
      else
        {
          output_double_link_t_name (f);
          output_char (' ', f);
          output_string (f, PREVIOUS_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
        }
      output_char (')', f);
    }
  else
    output_string
      (f, PREVIOUS_DOUBLE_LINK_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS);
}

/* The following macro value is name of parameter of function
   `IR__owner'. */

#define OWNER_FUNCTION_PARAMETER_NAME (cpp_flag ? "this" : "link")

/* The following macro value must be value of previous macro plus
   parentheses. */

#define OWNER_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS "(link)"

/* The following function outputs title of function returning node
   which contains double linked field.  The most probable function
   output is following
                `IR_node_t IR__owner (IR_double_link_t link)' or
                `IR_node_t IRF__owner (IR_double_link_t link)' or
                `IR__owner(link)' or
                `IR__owner(void)'. */

static void
output_title_of_owner_function (FILE *f, int as_macro_flag,
                                int function_prefix_flag)
{
  assert (!cpp_flag || !as_macro_flag && !function_prefix_flag);
  if (!as_macro_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
    }
  output_name_of_owner_function (f, !as_macro_flag && function_prefix_flag);
  if (!as_macro_flag)
    {
      output_string (f, " (");
      if (cpp_flag)
        output_string (f, "void");
      else
        {
          output_double_link_t_name (f);
          output_char (' ', f);
          output_string (f, OWNER_FUNCTION_PARAMETER_NAME);
        }
      output_char (')', f);
    }
  else
    output_string (f, OWNER_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS);
}

/* This function outputs the declaration of the function
   `IR__first_double_link' as extern (C interface) definition or as
   definition in IR_node class. */

static void
output_first_double_link_function_declaration (void)
{
  /* Extern definition of `IR__first_double_link' or in class. */
  if (!cpp_flag)
    output_string (output_interface_file, "extern ");
  else
    output_string (output_interface_file, "  ");
  output_title_of_first_double_link_function (output_interface_file, FALSE);
  output_string (output_interface_file, ";\n\n");
}

/* This function outputs the definition of function
   `IR__first_double_link'. */

static void
output_first_double_link_function_definition (void)
{
  /* Function itself */
  output_title_of_first_double_link_function (output_implementation_file,
                                              cpp_flag);
  output_string (output_implementation_file, "\n{\n");
  if (double_node_types_existence_flag)
    {
      output_string (output_implementation_file, "  switch (");
      if (cpp_flag)
	{
	  output_string (output_implementation_file,
			 FIRST_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
	  output_string (output_implementation_file, "->");
	}
      output_node_mode_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      if (!cpp_flag)
	output_string (output_implementation_file,
		       FIRST_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
      output_string (output_implementation_file, "))\n    {\n");
      traverse_node_types (output_node_type_first_double_link);
      output_string (output_implementation_file,
		     "    default:\n      abort ();\n      break;\n    }\n");
    }
  else
    output_string (output_implementation_file, "  return NULL;\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}


/* This function outputs declarations of functions
   `IR__next_double_link', `IR__previous_double_link' and `IR__owner'
   or/and the corresponding macros.  The function is used only for
   generation of C interface. */

static void
output_rest_double_link_functions_declarations (void)
{
  assert (!cpp_flag);
  /* `IR__next_double_link'. */
  if (macro_flag || only_macro_flag)
    {
      output_string (output_interface_file, "#define ");
      output_title_of_next_double_link_function (output_interface_file,
                                                 !cpp_flag, FALSE);
      output_string (output_interface_file, " (");
      output_string
	(output_interface_file,
	 NEXT_DOUBLE_LINK_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS);
      output_string (output_interface_file, "->");
      output_string (output_interface_file, DOUBLE_LINK_NEXT_NAME);
      output_string (output_interface_file, ")\n\n");
    }
  if (!only_macro_flag)
    {
      /* Extern definition of `IR__next_double_link'. */
      output_string (output_interface_file, "extern ");
      output_title_of_next_double_link_function (output_interface_file, FALSE,
                                                 macro_flag);
      output_string (output_interface_file, ";\n\n");
    }
  /* `IR_previous_double_link'. */
  if (macro_flag || only_macro_flag)
    {
      output_string (output_interface_file, "#define ");
      output_title_of_previous_double_link_function (output_interface_file,
                                                     !cpp_flag, FALSE);
      output_string (output_interface_file, " (");
      output_string
	(output_interface_file,
	 PREVIOUS_DOUBLE_LINK_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS);
      output_string (output_interface_file, "->");
      output_string (output_interface_file, DOUBLE_LINK_PREVIOUS_NAME);
      output_string (output_interface_file, ")\n\n");
    }
  if (!only_macro_flag)
    {
      /* Extern definition of `IR__previous_double_link'. */
      output_string (output_interface_file, "extern ");
      output_title_of_previous_double_link_function (output_interface_file,
                                                     FALSE, macro_flag);
      output_string (output_interface_file, ";\n\n");
    }
  /* `IR__owner'. */
  if (macro_flag || only_macro_flag)
    {
      output_string (output_interface_file, "#define ");
      output_title_of_owner_function (output_interface_file, !cpp_flag,
                                      FALSE);
      output_string (output_interface_file, " (");
      output_string (output_interface_file,
		     OWNER_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS);
      output_string (output_interface_file, "->");
      output_string (output_interface_file, DOUBLE_LINK_OWNER_NAME);
      output_string (output_interface_file, ")\n\n");
    }
  if (!only_macro_flag)
    {
      /* Extern definition of `IR_owner'. */
      output_string (output_interface_file, "extern ");
      output_title_of_owner_function (output_interface_file, FALSE,
                                      macro_flag);
      output_string (output_interface_file, ";\n\n");
    }
}

/* This function outputs definitions of functions
   `IR__next_double_link', `IR__previous_double_link' and `IR__owner'.
   In C++ interface these functions are inline functions. */

static void
output_rest_double_link_functions_definitions (void)
{
  FILE *output_file;

  output_file = (cpp_flag
                 ? output_interface_file : output_implementation_file);
  /* `IR__next_double_link'. */
  if (cpp_flag || !only_macro_flag)
    {
      /* Function itself */
      if (cpp_flag)
        output_string (output_file, "  inline ");
      output_title_of_next_double_link_function
        (output_file, FALSE, !cpp_flag && macro_flag);
      if (cpp_flag)
        output_string (output_file, "\n    {\n");
      else
        output_string (output_file, "\n{\n");
      output_ifdef (output_file, DEBUG_PARAMETER_NAME);
      if (cpp_flag)
	output_string (output_file, "    ");
      output_string (output_file, "  if (");
      output_string (output_file, NEXT_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
      if (cpp_flag)
	output_string (output_file, " == NULL)\n        abort ();\n");
      else
	output_string (output_file, " == NULL)\n    abort ();\n");
      output_endif (output_file, DEBUG_PARAMETER_NAME);
      if (cpp_flag)
            output_string (output_file, "    ");
      output_string (output_file, "  return ");
      output_string (output_file,
		     NEXT_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
      output_string (output_file, "->");
      output_string (output_file, DOUBLE_LINK_NEXT_NAME);
      if (cpp_flag)
        output_string (output_file, ";\n    }\n\n");
      else
        output_string (output_file, ";\n}\n\n");
    }
  /* `IR_previous_double_link'. */
  if (cpp_flag || !only_macro_flag)
    {
      /* Function itself */
      if (cpp_flag)
        output_string (output_file, "  inline ");
      output_title_of_previous_double_link_function
        (output_file, FALSE, !cpp_flag && macro_flag);
      if (cpp_flag)
	output_string (output_file, "\n    {\n");
      else
	output_string (output_file, "\n{\n");
      output_ifdef (output_file, DEBUG_PARAMETER_NAME);
      if (cpp_flag)
	output_string (output_file, "    ");
      output_string (output_file, "  if (");
      output_string (output_file,
		     PREVIOUS_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
      if (cpp_flag)
	output_string (output_file, " == NULL)\n        abort ();\n");
      else
	output_string (output_file, " == NULL)\n    abort ();\n");
      output_endif (output_file, DEBUG_PARAMETER_NAME);
      if (cpp_flag)
	output_string (output_file, "    ");
      output_string (output_file, "  return ");
      output_string (output_file,
		     PREVIOUS_DOUBLE_LINK_FUNCTION_PARAMETER_NAME);
      output_string (output_file, "->");
      output_string (output_file, DOUBLE_LINK_PREVIOUS_NAME);
      if (cpp_flag)
        output_string (output_file, ";\n    }\n\n");
      else
        output_string (output_file, ";\n}\n\n");
    }
  /* `IR__owner'. */
  if (cpp_flag || !only_macro_flag)
    {
      /* Function itself */
      if (cpp_flag)
        output_string (output_file, "  inline ");
      output_title_of_owner_function (output_file, FALSE,
                                      !cpp_flag && macro_flag);
      if (cpp_flag)
        output_string (output_file, "\n    {\n");
      else
        output_string (output_file, "\n{\n");
      output_ifdef (output_file, DEBUG_PARAMETER_NAME);
      if (cpp_flag)
	output_string (output_file, "    ");
      output_string (output_file, "  if (");
      output_string (output_file, OWNER_FUNCTION_PARAMETER_NAME);
      if (cpp_flag)
	output_string (output_file, " == NULL)\n        abort ();\n");
      else
	output_string (output_file, " == NULL)\n    abort ();\n");
      output_endif (output_file, DEBUG_PARAMETER_NAME);
      if (cpp_flag)
	output_string (output_file, "    ");
      output_string (output_file, "  return ");
      output_string (output_file, OWNER_FUNCTION_PARAMETER_NAME);
      output_string (output_file, "->");
      output_string (output_file, DOUBLE_LINK_OWNER_NAME);
      if (cpp_flag)
        output_string (output_file, ";\n    }\n\n");
      else
        output_string (output_file, ";\n}\n\n");
    }
}

/* The following macro value is name of parameter of field access
   functions.  Character '_' is used for resolving conflicts with
   possible field name `node' in access macro.  This is for C interface.

   As for C++ interface the function the following macro value is
   simply "this". */

#define ACCESS_FUNCTION_PARAMETER_NAME (cpp_flag ? "this" : "_node")

/* The following macro value must be value of previous macro plus
   parentheses. */

#define ACCESS_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS "(_node)"

/* The following function outputs title of function returning given
   field value.  The most probable function output is following
           `<field type> IR_<field name> (IR_node_t node)' or
           `<field type> IRF_<field name> (IR_node_t node)' or
           `<field type> IR_<field name> (void)' or
           `IR_<field name>(node)'. */

static void
output_title_of_field_access_function (FILE *f, IR_node_t field,
                                       int as_macro_flag,
                                       int function_prefix_flag)
{
  assert (!cpp_flag || !as_macro_flag && !function_prefix_flag);
  if (!as_macro_flag)
    {
      output_field_type_name (f, IR_field_type (field));
      output_char (' ', f);
    }
  output_name_of_access_function (f, IR_identifier_itself (IR_field_identifier
                                                           (field)),
                                  !as_macro_flag && function_prefix_flag);
  if (!as_macro_flag)
    {
      output_string (f, " (");
      if (cpp_flag)
        output_string (f, "void");
      else
        {
          output_node_t_name (f);
          output_char (' ', f);
          output_string (f, ACCESS_FUNCTION_PARAMETER_NAME);
        }
      output_char (')', f);
    }
  else
    output_string (f, ACCESS_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS);
}

/* The following function outputs external declaration of access
   function for given field.  The access function can be implemented
   as macro.  The function is called from function
   `traverse_all_fields'. This is used only for C interface. */

static void
output_field_access_function_declaration (IR_node_t field)
{
  assert (!cpp_flag && IR_NODE_MODE (field) == IR_NM_field);
  if (field == find_field (IR_field_identifier (field)))
    {
      assert (IR_node_type (field) != root_node_type);
      if (!only_macro_flag)
        {
          /* Extern definition */
          output_string (output_interface_file, "extern ");
          output_title_of_field_access_function (output_interface_file,
                                                 field, FALSE, macro_flag);
          output_string (output_interface_file, ";\n\n");
        }
    }
}

/* The following function outputs definition of access function for
   given field.  The access function is implemented as inline function
   for C++ interface.

   Remember that implementation of class fields, double linked fields
   and the rest fields are different. */

static void
output_field_access_function_definition (IR_node_t field)
{
  FILE *f;

  assert (IR_NODE_MODE (field) == IR_NM_field);
  if (field == find_field (IR_field_identifier (field)))
    {
      assert (IR_node_type (field) != root_node_type);
      if (!cpp_flag && (macro_flag || only_macro_flag))
        {
          output_string (output_interface_file, "#define ");
          output_title_of_field_access_function (output_interface_file,
                                                 field, TRUE, FALSE);
          output_char (' ', output_interface_file);
          output_field_access (output_interface_file,
                               ACCESS_FUNCTION_PARAMETER_NAME_IN_PARENTHESIS,
                               field, TRUE, FALSE);
          output_string (output_interface_file, "\n\n");
        }
      if (cpp_flag || !only_macro_flag)
        {
          /* Function itself */
          f = (cpp_flag ? output_interface_file : output_implementation_file);
          if (cpp_flag)
            output_string (f, "  inline ");
          output_title_of_field_access_function (f, field, FALSE,
                                                 !cpp_flag && macro_flag);
          output_string (f, cpp_flag ? "\n    {\n" : "\n{\n");
          /* Test node */
          output_ifdef (f, DEBUG_PARAMETER_NAME);
          output_string (f, cpp_flag ? "      if (" : "  if (");
          output_string (f, ACCESS_FUNCTION_PARAMETER_NAME);
          output_string (f, " == NULL || !");
          if (field == IR_previous_synonym_field (field)
              && !IR_secondary_super_type_flag (IR_node_type (field)))
            {
              if (!fast_flag)
                output_string (f, "((");
              output_subtype_flag_array_name
                (f, IR_identifier_itself (IR_type_identifier (IR_node_type
                                                              (field))));
              output_string (f, " [");
              if (cpp_flag)
                {
                  output_string (f, ACCESS_FUNCTION_PARAMETER_NAME);
                  output_string (f, "->");
                }
              output_node_mode_name (f);
              output_string (f, " (");
              if (!cpp_flag)
                output_string (f, ACCESS_FUNCTION_PARAMETER_NAME);
              if (!fast_flag)
                {
                  output_string (f, ") / ");
                  output_decimal_number (f, CHAR_BIT);
                  output_string (f, "] >> (");
                  if (cpp_flag)
                    {
                      output_string (f, ACCESS_FUNCTION_PARAMETER_NAME);
                      output_string (f, "->");
                    }
                  output_node_mode_name (f);
                  output_string (f, " (");
                  if (!cpp_flag)
                    output_string (f, ACCESS_FUNCTION_PARAMETER_NAME);
                  output_string (f, ") % ");
                  output_decimal_number (f, CHAR_BIT);
                  output_string (f,
                                 cpp_flag ? ")) & 1))\n    abort ();\n"
                                 : ")) & 1))\n        abort ();\n");
                }
              else
                output_string (f,
                               cpp_flag ? ")])\n        abort ();\n"
                               : ")])\n    abort ();\n");
            }
          else
            {
              output_different_displacement_field_displacement_array_name
                (f, IR_identifier_itself (IR_field_identifier (field)));
              output_string (f, " [");
              if (cpp_flag)
                {
                  output_string (f, ACCESS_FUNCTION_PARAMETER_NAME);
                  output_string (f, "->");
                }
              output_node_mode_name (f);
              output_string (f, " (");
              if (!cpp_flag)
                output_string (f, ACCESS_FUNCTION_PARAMETER_NAME);
              output_string (f,
                             cpp_flag ? ")])\n        abort ();\n"
                             : ")])\n    abort ();\n");
            }
          output_endif (f, DEBUG_PARAMETER_NAME);
          /* Field access itself */
          output_string (f, cpp_flag ? "      return " : "  return ");
          output_field_access (f, ACCESS_FUNCTION_PARAMETER_NAME, field,
                               TRUE, FALSE);
          output_string (f, ";\n");
          /* Function epilogue */
          output_string (f, cpp_flag ? "    }\n\n" : "}\n\n");
        }
    }
}



/* This page contains functions for output of all field modification
   functions. */

/* The following macro value is name of the first parameter of
   function `IR__set_double_link' (for C interaface) or the
   corrseponding double link (C++ interface). */

#define SET_DOUBLE_LINK_FUNCTION_LINK_PARAMETER_NAME \
           (cpp_flag ? "this" : "link")

/* The following macro value is name of the second parameter of function
   `IR__set_double_link'. */

#define SET_DOUBLE_LINK_FUNCTION_VALUE_PARAMETER_NAME "value"

/* The following function outputs title of function for modifiction of
   double link field.  The most probable function output is following
             `void IR__set_double_link (IR_double_link_t link,
                                        IR_node_t value)'
   or
             `void IR__set_double_link (IR_node_t value)'. */

static void
output_title_of_set_double_link_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_set_double_link_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_double_link_t_name (f);
      output_char (' ', f);
      output_string (f, SET_DOUBLE_LINK_FUNCTION_LINK_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, SET_DOUBLE_LINK_FUNCTION_VALUE_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs external declaration of set double
   link function needed for work with double linked fields.  This
   function is used only for C interface. */

static void
output_set_double_link_function_declaration (void)
{
  assert (!cpp_flag);
  /* Extern definition. */
  output_string (output_interface_file, "extern ");
  output_title_of_set_double_link_function (output_interface_file);
  output_string (output_interface_file, ";\n\n");
}

/* The following function outputs external definition (for C
   interface) or definition in class IR_node (for C++ interface) of
   set double link function needed for work with double linked
   fields. */

static void
output_set_double_link_function_definition (void)
{
  FILE *output_file;

  /* Function itself */
  output_file
    = (cpp_flag ? output_interface_file : output_implementation_file);
  if (cpp_flag)
    output_string (output_file, "  ");
  output_title_of_set_double_link_function (output_file);
  output_string (output_file, cpp_flag ? "\n    {\n" : "\n{\n");
  /*if (double_node_types_existence_flag)*/
    {
      output_ifdef (output_file, DEBUG_PARAMETER_NAME);
      output_string (output_file, cpp_flag ? "      if (" : "  if (");
      output_string (output_file,
		     SET_DOUBLE_LINK_FUNCTION_LINK_PARAMETER_NAME);
      output_string (output_file, (cpp_flag ? " == NULL)\n        abort ();\n"
                                   : " == NULL)\n    abort ();\n"));
      output_endif (output_file, DEBUG_PARAMETER_NAME);
      if (cpp_flag)
        {
          output_string (output_file, "      (this->");
          output_string (output_file, DOUBLE_LINK_OWNER_NAME);
          output_string (output_file, "->*this->");
        }
      else
        {
          output_string (output_file, "  (*");
          output_string (output_file,
                         SET_DOUBLE_LINK_FUNCTION_LINK_PARAMETER_NAME);
          output_string (output_file, "->");
        }
      output_string (output_file, DOUBLE_LINK_SET_FUNCTION_NAME);
      output_string (output_file, ") (");
      if (!cpp_flag)
        {
          output_string (output_file,
                         SET_DOUBLE_LINK_FUNCTION_LINK_PARAMETER_NAME);
          output_string (output_file, "->");
          output_string (output_file, DOUBLE_LINK_OWNER_NAME);
          output_string (output_file, ", ");
        }
      output_string (output_file,
		     SET_DOUBLE_LINK_FUNCTION_VALUE_PARAMETER_NAME);
      output_string (output_file, ");\n");
    }
  /* Function epilogue */
  output_string (output_file, cpp_flag ? "    }\n\n" : "}\n\n");
}


/* The following macro value is name of the first parameter of field
   modification functions.  Character '_' is used for resolving
   conflicts with possible field name `node' in modification macro (C
   interface). */

#define MODIFICATION_FUNCTION_NODE_PARAMETER_NAME (cpp_flag ? "this" : "_node")

/* The following macro value is name in parenthesis of the first
   parameter of field modification macros. */

#define MODIFICATION_FUNCTION_NODE_PARAMETER_NAME_IN_PARENTHESIS "(_node)"

/* The following macro value is name of the second parameter of field
   modification functions.  Character '_' is used for resolving
   conflicts with possible field name `value' in modification
   macro. */

#define MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME "_value"

/* The following macro value is name parenthesis of the second
   parameter of field modification macros. */

#define MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME_IN_PARENTHESIS "(_value)"

/* The following function outputs title of function or/and macro which
   sets up value of given field.  The most probable function output is
   following
          `void IR_set_<field name> (IR_node_t node,
                                      <field type> value)' or
          `void IRF_set_<field name> (IR_node_t node,
                                       <field type> value)' or
          `void IR_set_<field name> (<field type> value)' or
          `IR_set_<field name>(node, value)'. */

static void
output_title_of_field_modification_function (FILE *f, IR_node_t field,
                                             int as_macro_flag,
                                             int function_prefix_flag)
{
  assert (!cpp_flag || !as_macro_flag && !function_prefix_flag);
  if (!as_macro_flag)
    output_string (f, "void ");
  output_name_of_set_function (f, IR_identifier_itself (IR_field_identifier
                                                        (field)),
                               !as_macro_flag && function_prefix_flag);
  if (!as_macro_flag)
    {
      output_string (f, " (");
      if (!cpp_flag)
        {
          output_node_t_name (f);
          output_char (' ', f);
          output_string (f, MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
          output_string (f, ", ");
        }
      output_field_type_name (f, IR_field_type (field));
      output_char (' ', f);
      output_string (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
      output_char (')', f);
    }
  else
    {
      output_char ('(', f);
      output_string (f, MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
      output_string (f, ", ");
      output_string (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
      output_char (')', f);
    }
}

/* The following function outputs macro and/or external definition of
   modification function for given field.  The access function can be
   implemented as macro.  The function is called from function
   `traverse_all_fields'.  Remember that implementation of class
   fields, double linked fields and the rest fields are different.
   This function is used only for C interface. */

static void
output_field_modification_function_declaration (IR_node_t field)
{
  assert (!cpp_flag && IR_NODE_MODE (field) == IR_NM_field);
  if (field == find_field (IR_field_identifier (field)))
    {
      assert (IR_node_type (field) != root_node_type);
      /* Function is output always for double field because it is used
         in inititiations of the double field. */
      if (!only_macro_flag || IR_double_field_flag (field))
        {
          /* Extern definition */
          output_string (output_interface_file, "extern ");
          output_title_of_field_modification_function
            (output_interface_file, field, FALSE,
             macro_flag || only_macro_flag);
          output_string (output_interface_file, ";\n\n");
        }
    }
}

/* The following function outputs macro and/or external declaration (C
   intreface) or class declaration (C++ interface) and declaration of
   modification function for given field.  The access function can be
   implemented as macro (C interface).  The function is called from
   function `traverse_all_fields'.  Remember that implementation of
   class fields, double linked fields and the rest fields are
   different. */

static void
output_field_modification_function_definition (IR_node_t field)
{
  FILE *f;

  assert (IR_NODE_MODE (field) == IR_NM_field);
  if (field == find_field (IR_field_identifier (field)))
    {
      assert (IR_node_type (field) != root_node_type);
      if (!cpp_flag && (macro_flag || only_macro_flag))
        {
          output_string (output_interface_file, "#define ");
          output_title_of_field_modification_function (output_interface_file,
                                                       field, TRUE, FALSE);
          output_string (output_interface_file, " (");
          if (IR_double_field_flag (field))
            {
              output_start_of_setting_double_link_field_value
                (output_interface_file, "",
                 MODIFICATION_FUNCTION_NODE_PARAMETER_NAME, field, TRUE, TRUE);
            }
          else
            {
              output_field_access
                (output_interface_file,
                 MODIFICATION_FUNCTION_NODE_PARAMETER_NAME_IN_PARENTHESIS,
                 field, TRUE, FALSE);
              output_string (output_interface_file, " = ");
            }
          output_string
            (output_interface_file,
             MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME_IN_PARENTHESIS);
          if (IR_double_field_flag (field))
            {
              output_finish_of_setting_double_link_field_value
                (output_interface_file, "  ", field, FALSE, TRUE);
            }
          output_string (output_interface_file, ")\n\n");
        }
      /* Function is output always for double field because it is used
         in inititiations of the double field. */
      if (cpp_flag || !only_macro_flag || IR_double_field_flag (field))
        {
          /* Function itself */
          f = (cpp_flag ? output_interface_file : output_implementation_file);
          if (cpp_flag)
            output_string (f, "  inline ");
          output_title_of_field_modification_function
            (f, field, FALSE, !cpp_flag && (macro_flag || only_macro_flag));
          output_string (f, cpp_flag ? "\n    {\n" : "\n{\n");
          /* Test node and value */
          output_ifdef (f, DEBUG_PARAMETER_NAME);
          output_string (f, cpp_flag ? "      if (" : "  if (");
          output_string (f, MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
          output_string (f, " == NULL");
          if (IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
            {
              output_string (f, cpp_flag ? "\n          || " : "\n      || ");
              output_string (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
              output_string (f, " != NULL && !");
              if (!fast_flag)
                output_string (f, "((");
              if (IR_field_type (field) == root_node_type)
                output_subtype_flag_array_name (f, ROOT_NAME);
              else
                output_subtype_flag_array_name
                  (f, IR_identifier_itself (IR_field_type_identifier (field)));
              output_string (f, " [");
              if (cpp_flag)
                {
                  output_string
                    (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
                  output_string (f, "->");
                }
              output_node_mode_name (f);
              output_string (f, " (");
              if (!cpp_flag)
                output_string (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
              if (!fast_flag)
                {
                  output_string (f, ") / ");
                  output_decimal_number (f, CHAR_BIT);
                  output_string (f, "] >> (");
                  if (cpp_flag)
                    {
                      output_string
                        (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
                      output_string (f, "->");
                    }
                  output_node_mode_name (f);
                  output_string (f, " (");
                  if (!cpp_flag)
                    output_string
                      (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
                  output_string (f, ") % ");
                  output_decimal_number (f, CHAR_BIT);
                  output_string
                    (f,
                     cpp_flag
                     ? ")) & 1)\n             && " : ")) & 1)\n         && ");
                }
              else
                output_string
                  (f, cpp_flag ? ")]\n             && " : ")]\n         && ");
              if (cpp_flag)
                {
                  output_string
                    (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
                  output_string (f, "->");
                }
              output_node_mode_name (f);
              output_string (f, " (");
              if (!cpp_flag)
                output_string (f, MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
              output_string (f, ") != ");
              output_node_mode_type_value (f, ERROR_NAME);
            }
          output_string (f, cpp_flag ? "\n          || !" : "\n      || !");
          if (field == IR_previous_synonym_field (field)
              && !IR_secondary_super_type_flag (IR_node_type (field)))
            {
              if (!fast_flag)
                output_string (f, "((");
              output_subtype_flag_array_name
                (f, IR_identifier_itself (IR_type_identifier (IR_node_type
                                                              (field))));
              output_string (f, " [");
              if (cpp_flag)
                {
                  output_string (f, MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
                  output_string (f, "->");
                }
              output_node_mode_name (f);
              output_string (f, " (");
              if (!cpp_flag)
                output_string (f, MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
              if (!fast_flag)
                {
                  output_string (f, ") / ");
                  output_decimal_number (f, CHAR_BIT);
                  output_string (f, "] >> (");
                  if (cpp_flag)
                    {
                      output_string
                        (f, MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
                      output_string (f, "->");
                    }
                  output_node_mode_name (f);
                  output_string (f, " (");
                  if (!cpp_flag)
                    output_string (f,
                                   MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
                  output_string (f, ") % ");
                  output_decimal_number (f, CHAR_BIT);
                  output_string (f, (cpp_flag
                                     ? ")) & 1))\n        abort ();\n"
                                     : ")) & 1))\n    abort ();\n"));
                }
              else
                output_string (f, (cpp_flag
                                   ? ")])\n        abort ();\n"
                                   : ")])\n    abort ();\n"));
            }
          else
            {
              output_different_displacement_field_displacement_array_name
                (f, IR_identifier_itself (IR_field_identifier (field)));
              output_string (f, " [");
              if (cpp_flag)
                {
                  output_string (f, MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
                  output_string (f, "->");
                }
              output_node_mode_name (f);
              output_string (f, " (");
              if (!cpp_flag)
                output_string (f, MODIFICATION_FUNCTION_NODE_PARAMETER_NAME);
              output_string (f, (cpp_flag
                                 ? ")])\n        abort ();\n"
                                 : ")])\n    abort ();\n"));
            }
          output_endif (f, DEBUG_PARAMETER_NAME);
          if (IR_double_field_flag (field))
            output_start_of_setting_double_link_field_value
              (f, cpp_flag ? "      " : "  ",
               MODIFICATION_FUNCTION_NODE_PARAMETER_NAME, field, TRUE, FALSE);
          else
            {
              output_string (f, (cpp_flag ? "      " : "  "));
              output_field_access (f,
                                   MODIFICATION_FUNCTION_NODE_PARAMETER_NAME,
                                   field, TRUE, FALSE);
              output_string (f, " = ");
            }
          output_string (f,
                         MODIFICATION_FUNCTION_VALUE_PARAMETER_NAME);
          if (IR_double_field_flag (field))
            output_finish_of_setting_double_link_field_value
              (f, cpp_flag ? "      " : "  ", field, FALSE, FALSE);
          else
            output_string (f, ";\n");
          /* Function epilogue */
          output_string (f, cpp_flag ? "    }\n\n" : "}\n\n");
        }
    }
}



/* This page contains functions for output of function of creation of
   node and node type specific creation functions. */

/* The following macro value is name of the first parameter of
   function `_IR_node_field_initiatiation'. */

#define FIELD_INITIATION_FUNCTION_NODE_MODE_PARAMETER_NAME "node_mode"

/* The following macro value is name of the second parameter of
   function `_IR_node_field_initiatiation'. */

#define FIELD_INITIATION_FUNCTION_NODE_PARAMETER_NAME "node"

/* The following function outputs title of function for initiation
   node fields.  The most probable function output is following
          `void _IR_node_field_initiation
           (IR_node_mode_t node_mode, IR_node_t node)'. */

static void
output_title_of_field_initiation_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_field_initiation_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, FIELD_INITIATION_FUNCTION_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, FIELD_INITIATION_FUNCTION_NODE_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs code of given non-class action or
   type specific initiation code of given non class field.  Remember
   that the code is output with `{' and `}'.  The function is called
   from function `traverse_all_node_type_fields' or
   `output_node_type_initiation'. Remember about special initiation of
   double linked field. */

static void
output_node_type_field_initiation (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field)
    {
      if (IR_declaration_part (field) != DP_CLASS)
        {
          /* Type specific initiation */
          output_string (output_implementation_file, "      ");
          output_field_type_initialization_macro_name_prefix
            (output_implementation_file);
          output_field_type_name (output_implementation_file,
                                  IR_field_type (field));
          output_string (output_implementation_file, " (");
          output_field_access (output_implementation_file,
                               FIELD_INITIATION_FUNCTION_NODE_PARAMETER_NAME,
                               field, FALSE, FALSE);
          output_string (output_implementation_file, ");\n");
          output_other_double_link_members_initiation
            (output_implementation_file,
             FIELD_INITIATION_FUNCTION_NODE_PARAMETER_NAME, field);
          last_processed_field = field;
        }
    }
  else if (IR_NODE_MODE (field) == IR_NM_action)
    {
      if (IR_declaration_part (field) != DP_CLASS)
        {
          output_char ('{', output_implementation_file);
          output_code (output_implementation_file, field,
                       FIELD_INITIATION_FUNCTION_NODE_PARAMETER_NAME, FALSE);
          output_string (output_implementation_file, "}\n");
        }
    }
}

/* If flat structures is used for implementation of nodes, the
   function outputs the following fragment
      ((_root *) <node_name>)-><first link member name> = NULL;

   Otherwise, If given node type is not abstract node type or non-flat
   model and it is double node, the function outputs the following
   fragment
      ((<node type name> *) <node name>)-><first link memeber name> = NULL;
*/

static void
output_first_link_member_initiation (IR_node_t node_type,
                                     const char *node_name)
{
  /* Remember that the first link member is placed in structure for root
     node type in non-flat structure implementation. */
  if (!flat_structure_flag)
    {
      output_string (output_implementation_file, "  ((");
      output_node_type_name (output_implementation_file, ROOT_NAME);
      output_string (output_implementation_file, " *) ");
      output_string (output_implementation_file, node_name);
      output_string (output_implementation_file, ")->");
      output_first_link_member_name (output_implementation_file);
      output_string (output_implementation_file, " = NULL;\n");
    }
  else
    {
      assert (node_type != NULL);
      if ((!flat_flag || !IR_abstract_flag (node_type))
           && IR_double_node_flag (node_type))
        {
          /* Remeber that all nodes have first double link member in
             non-flat structure implementation.  Therefore the member is
             initiated in another function for the non-flat
             implementation. */
          output_string (output_implementation_file, "      ((");
          output_node_type_name (output_implementation_file,
                                 IR_identifier_itself (IR_type_identifier
                                                       (node_type)));
          output_string (output_implementation_file, " *) ");
          output_string (output_implementation_file, node_name);
          output_string (output_implementation_file, ")->");
          output_first_link_member_name (output_implementation_file);
          output_string (output_implementation_file, " = NULL;\n");
        }
    }
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for initiation of
   all fields of node of given type.  The function outputs the
   following fragment
            case <node mode value of given node type>:
              <code of initiation of all fields>
              break;
   The function is called from function `traverse_node_types'.
   Remember about initiation of additional member for double nodes. */

static void
output_node_type_initiation (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_structure_flag && IR_double_node_flag (node_type))
        /* Remeber that all nodes have first double link member
           in non-flat structure implementation.  Therefore the
           member is initiated in another function for the
           non-flat implementation. */
        output_first_link_member_initiation
          (node_type, FIELD_INITIATION_FUNCTION_NODE_PARAMETER_NAME);
      if (flat_flag)
        {
          last_processed_field = NULL;
          traverse_all_node_type_fields (node_type,
                                         output_node_type_field_initiation);
        }
      else
        {
          last_processed_field = last_field_in_super_type (node_type, FALSE);
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      ");
                output_name_of_field_initiation_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   FIELD_INITIATION_FUNCTION_NODE_PARAMETER_NAME);
                output_string (output_implementation_file, ");\n");
              }
          if (IR_last_field (node_type) != NULL)
            for (current_field = IR_next_field (IR_last_field (node_type));;
                 current_field = IR_next_field (current_field))
              {
                assert (IR_NODE_MODE (current_field) == IR_NM_field
                        || IR_NODE_MODE (current_field) == IR_NM_action
                        || IR_NODE_MODE (current_field) == IR_NM_constraint);
                output_node_type_field_initiation (current_field);
                if (current_field == IR_last_field (node_type))
                  break;
              }
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_node_field_initiation'. */

static void
output_field_initiation_function (void)
{
  /* Output field initiation function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_initiation_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Node type fields initialization */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 FIELD_INITIATION_FUNCTION_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_initiation);
  /* Empty entry for field initiation of error node. */
  output_string (output_implementation_file, "    case ");
  output_node_mode_type_value (output_implementation_file, ERROR_NAME);
  output_string (output_implementation_file, ":\n      break;\n");
  /* Check up other node types. */
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}


/* The following macro value is name of the first parameter of
   function `IR_create_node'. */

#define CREATION_FUNCTION_PARAMETER_NAME "node_mode"

/* The following macro value is name of variable which will be result of
   function `IR_create_node'. */

#define CREATION_FUNCTION_RESULT_NAME "_result"

/* The following function outputs title of function for creating node.
   The most probable function output is following
        `IR_node_t IR_create_node (IR_node_mode_t node_mode)'. */

static void
output_title_of_creation_function (FILE *f)
{
  output_node_t_name (f);
  output_char (' ', f);
  output_name_of_creation_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, CREATION_FUNCTION_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs declaration of functions
   `output_field_initiation_function' (with the aid of function
   `output_field_initiation_function') and `IR_create_node' (also its
   external definition). */

static void
output_creation_function_declaration (void)
{
  output_string (output_interface_file, cpp_flag ? "  friend " : "extern ");
  output_title_of_creation_function (output_interface_file);
  output_string (output_interface_file, ";\n\n");
}

/* The following function outputs definitions of functions
   `output_field_initiation_function' (with the aid of function
   `output_field_initiation_function') and `IR_create_node' (also its
   external definition). */

static void
output_creation_function_definition (void)
{
  /* Output field initiation function. */
  output_field_initiation_function ();
  /* Function itself */
  output_title_of_creation_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n ");
  output_node_t_name (output_implementation_file);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, CREATION_FUNCTION_RESULT_NAME);
  output_string (output_implementation_file, ";\n\n  ");
  output_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, CREATION_FUNCTION_RESULT_NAME);
  output_string (output_implementation_file, ", ");
  output_node_size_array_name (output_implementation_file);
  output_string (output_implementation_file, " [");
  output_string (output_implementation_file, CREATION_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, "], ");
  output_node_t_name (output_implementation_file);
  output_string (output_implementation_file, ");\n");
  output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, CREATION_FUNCTION_RESULT_NAME);
  output_string (output_implementation_file, " == NULL)\n    abort ();\n");
  output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
  /* Node mode member */
  output_string (output_implementation_file, "  ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     CREATION_FUNCTION_RESULT_NAME);
      output_string (output_implementation_file, "->");
      output_node_mode_member_name (output_implementation_file);
      output_string (output_implementation_file, " = ");
    }
  else
    {
      output_node_mode_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file,
                     CREATION_FUNCTION_RESULT_NAME);
      output_string (output_implementation_file, ") = ");
    }
  output_string (output_implementation_file, CREATION_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ";\n");
  if (graph_pass_number_member_flag)
    {
      /* Previous graph pass number member */
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file,
                     CREATION_FUNCTION_RESULT_NAME);
      output_string (output_implementation_file, "->");
      output_last_graph_pass_number_member_name (output_implementation_file);
      output_string (output_implementation_file, " = 0;\n");
    }
  /* Remember that the first link member is placed in structure for root
     node type in non-flat structure implementation. */
  if (!flat_structure_flag && double_node_types_existence_flag)
    output_first_link_member_initiation (NULL, CREATION_FUNCTION_RESULT_NAME);
  /* Node type fields initialization */
  output_string (output_implementation_file, "  ");
  output_name_of_field_initiation_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, CREATION_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, CREATION_FUNCTION_RESULT_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file, CREATION_FUNCTION_RESULT_NAME);
  output_string (output_implementation_file, ";\n}\n\n");
}

/* This recursive function traverse all super types of given node
   types and outputs parameter declarations for skeleton fields.  The
   most probable function output is following
                   <field type> <field name>,
                   ...
   The function returns FALSE iff there are not skeleton fields in
   node of given node type. */

static int
output_skeleton_field_parameters (FILE *f, IR_node_t node_type,
                                  int continuation_flag, const char *indent)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  for (curr_supertype_list_element
       = IR_first_super_type_list_element (node_type);
       curr_supertype_list_element != NULL;
       curr_supertype_list_element
       = IR_next_super_type_list_element (curr_supertype_list_element))
    if (IR_immediate_super_type (curr_supertype_list_element) != NULL)
      continuation_flag
        |= output_skeleton_field_parameters (f,
                                             IR_immediate_super_type
                                             (curr_supertype_list_element),
                                             continuation_flag, indent);
  /* Output node type field definitions */
  if (IR_last_field (node_type) != NULL)
    for (current_field = IR_next_field (IR_last_field (node_type));;
         current_field = IR_next_field (current_field))
      {
        if (IR_NODE_MODE (current_field) == IR_NM_field)
          {
            if (IR_declaration_part (current_field) == DP_SKELETON)
              {
                if (continuation_flag)
                  {
                    output_string (f, ",\n ");
                    output_string (f, indent);
                  }
                continuation_flag = TRUE;
                output_field_type_name (f, IR_field_type (current_field));
                output_char (' ', f);
                output_string (f, IR_identifier_itself (IR_field_identifier
                                                        (current_field)));
              }
          }
        else
          assert (IR_NODE_MODE (current_field) == IR_NM_constraint
                  || IR_NODE_MODE (current_field) == IR_NM_action);
        if (current_field == IR_last_field (node_type))
          break;
      }
  return continuation_flag;
}

/* The following macro value is name of variable which will be result of
   a node type specific creation function. */

#define NEW_FUNCTION_RESULT_NAME "_result"

/* The following function outputs title of node type specific creation
   function for given node type.  The most probable function output is
   following
                   `IR_node_t IR_new_<node type name>
                           (<parameters for skeleton fields>)'. */

static void
output_title_of_node_type_specific_creation_function (FILE *f,
                                                      IR_node_t node_type,
                                                      int class_prefix_flag)
{
  assert (cpp_flag || !class_prefix_flag);
  output_node_t_name (f);
  output_char (' ', f);
  output_name_of_new_function (f, IR_identifier_itself (IR_type_identifier
                                                        (node_type)));
  output_string (f, cpp_flag && !class_prefix_flag ? "\n    (" : "\n(");
  (void) output_skeleton_field_parameters
    (f, node_type, FALSE, cpp_flag && !class_prefix_flag ? "    " : "");
  output_char (')', f);
}

/* If given field is skeleton field the function outputs code of
   initiation of the skeleton field by the corresponding parameter
   value.  The function is called from function
   `traverse_all_node_type_fields'.  Remember about special initiation
   of double linked field. */

static void
output_skeleton_field_initiation (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) == DP_SKELETON)
    {
      output_string (output_implementation_file, "  ");
      output_field_access (output_implementation_file,
                           NEW_FUNCTION_RESULT_NAME, field, FALSE, FALSE);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
                     IR_identifier_itself (IR_field_identifier (field)));
      output_string (output_implementation_file, ";\n");
      output_start_of_setting_double_link_field_value
        (output_implementation_file, "  ", NEW_FUNCTION_RESULT_NAME, field,
         FALSE, FALSE);
      if (IR_double_field_flag (field))
        output_string (output_implementation_file, "NULL");
      output_finish_of_setting_double_link_field_value
        (output_implementation_file, "  ", field, TRUE, FALSE);
    }
}

/* If given node type is not abstract the function outputs external or
   friend declaration of node type specific creation function for
   given node type.  The function is called from function
   `traverse_node_types'.  Remember that node type specific creation
   functions use `IR_create_node'. */

static void
output_node_type_specific_creation_function_declaration (IR_node_t node_type)
{
  if (!IR_abstract_flag (node_type))
    {
      output_string (output_interface_file,
                     cpp_flag ? "  friend " : "extern ");
      output_title_of_node_type_specific_creation_function
        (output_interface_file, node_type, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
}

/* If given node type is not abstract the function outputs definition
   of node type specific creation function for given node type.  The
   function is called from function `traverse_node_types'.  Remember
   that node type specific creation functions use `IR_create_node'. */

static void
output_node_type_specific_creation_function_definition (IR_node_t node_type)
{
  if (!IR_abstract_flag (node_type))
    {
      /* Function itself */
      output_title_of_node_type_specific_creation_function
        (output_implementation_file, node_type, cpp_flag);
      output_string (output_implementation_file, "\n{\n  ");
      output_node_t_name (output_implementation_file);
      output_char (' ', output_implementation_file);
      output_string (output_implementation_file, NEW_FUNCTION_RESULT_NAME);
      output_string (output_implementation_file, ";\n\n  ");
      output_string (output_implementation_file, NEW_FUNCTION_RESULT_NAME);
      output_string (output_implementation_file, " = ");
      output_name_of_creation_function (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_node_mode_type_value (output_implementation_file,
                                   IR_identifier_itself (IR_type_identifier
                                                         (node_type)));
      output_string (output_implementation_file, ");\n");
      traverse_all_node_type_fields (node_type,
                                     output_skeleton_field_initiation);
      /* Function epilogue */
      output_string (output_implementation_file, "  return ");
      output_string (output_implementation_file, NEW_FUNCTION_RESULT_NAME);
      output_string (output_implementation_file, ";\n}\n\n");
    }
}



/* This page contains functions for output of functions of deletion of
   node and graph. */

/* The following macro value is name of the first parameter of
   `_IR_free_node_fields'. */

#define FIELD_DELETION_NODE_MODE_PARAMETER_NAME  "node_mode"

/* The following macro value is name of parameter of function
   `IR_free_node' and the second parameter of
   `_IR_free_node_fields'. */

#define NODE_DELETION_PARAMETER_NAME "node"

/* The following function outputs title of function for deleting node
   fields.  The most probable function output is following `void
   _IR_free_node_fields (IR_node_mode_t node_mode, IR_node_t
   node)'. */

static void
output_title_of_field_deletion_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_field_deletion_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, FIELD_DELETION_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, NODE_DELETION_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs code of finalization of given non
   class field.  The function is called from function
   `reverse_traverse_all_node_type_fields' or
   `output_node_type_fields_deletion'. */

static void
output_field_deletion (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS)
    {
      output_start_of_setting_double_link_field_value
        (output_implementation_file, "      ", NODE_DELETION_PARAMETER_NAME,
         field, FALSE, FALSE);
      if (IR_double_field_flag (field))
        output_string (output_implementation_file, "NULL");
      output_finish_of_setting_double_link_field_value
        (output_implementation_file, "      ", field, FALSE, FALSE);
      output_string (output_implementation_file, "      ");
      output_field_type_finalization_macro_name_prefix
        (output_implementation_file);
      output_field_type_name (output_implementation_file,
                              IR_field_type (field));
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           NODE_DELETION_PARAMETER_NAME, field, FALSE, FALSE);
      output_string (output_implementation_file, ");\n");
    }
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for finalization
   of all fields of node of given type.  The function outputs the
   following fragment
                    case <node mode value of given node type>:
                      <code of finalization of all fields>
                      break;
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_deletion (IR_node_t node_type)
{
  IR_node_t current_field;
  vlo_t fields_in_reverse_order;
  vlo_t super_types_in_reverse_order;
  IR_node_t *current_field_ref;
  IR_node_t curr_supertype_list_element;
  IR_node_t *curr_supertype_list_element_ref;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        reverse_traverse_all_node_type_fields (node_type,
                                               output_field_deletion);
      else
        {
          if (IR_last_field (node_type) != NULL)
            {
              VLO_CREATE (fields_in_reverse_order, 40);
              for (current_field = IR_next_field (IR_last_field (node_type));;
                   current_field = IR_next_field (current_field))
                {
                  VLO_ADD_MEMORY (fields_in_reverse_order, &current_field,
                                  sizeof (current_field));
                  if (current_field == IR_last_field (node_type))
                    break;
                }
              current_field_ref
                = (IR_node_t *) ((char *) VLO_END (fields_in_reverse_order)
                                 + 1 - sizeof (current_field));
              for (;;current_field_ref--)
                {
                  output_field_deletion (*current_field_ref);
                  if (current_field_ref
                      == (IR_node_t *) VLO_BEGIN (fields_in_reverse_order))
                    break;
                }
              VLO_DELETE (fields_in_reverse_order);
            }
          VLO_CREATE (super_types_in_reverse_order, 40);
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            VLO_ADD_MEMORY (super_types_in_reverse_order,
                            &curr_supertype_list_element,
                            sizeof (curr_supertype_list_element));
          curr_supertype_list_element_ref
            = (IR_node_t *) ((char *) VLO_END (super_types_in_reverse_order)
                             + 1 - sizeof (curr_supertype_list_element));
          if (IR_first_super_type_list_element (node_type) != NULL)
            for (;;curr_supertype_list_element_ref--)
              {
                if (IR_immediate_super_type (*curr_supertype_list_element_ref)
                    != root_node_type
                    && (IR_node_structure_has_been_output
                        (IR_immediate_super_type
                         (*curr_supertype_list_element_ref))))
                  {
                    output_string (output_implementation_file, "      ");
                    output_name_of_field_deletion_function
                      (output_implementation_file);
                    output_string (output_implementation_file, " (");
                    output_node_mode_type_value
                      (output_implementation_file,
                       IR_identifier_itself
                       (IR_type_identifier
                        (IR_immediate_super_type
                         (*curr_supertype_list_element_ref))));
                    output_string (output_implementation_file, ", ");
                    output_immediate_super_type_node_address
                      (output_implementation_file, node_type,
                       IR_immediate_super_type
                       (*curr_supertype_list_element_ref),
                       NODE_DELETION_PARAMETER_NAME);
                    output_string (output_implementation_file, ");\n");
                  }
                if (curr_supertype_list_element_ref
                    == (IR_node_t *) VLO_BEGIN (super_types_in_reverse_order))
                  break;
              }
          VLO_DELETE (super_types_in_reverse_order);
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_free_node_fields'. */

static void
output_field_deletion_function (void)
{
  /* Output field deletion function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_deletion_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Delete fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 FIELD_DELETION_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_deletion);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for deleting node.
   The most probable function output is following
   `void IR_free_node (IR_node_t node)'. */

static void
output_title_of_node_deletion_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_node_deletion_function (f);
  output_string (f, " (");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, NODE_DELETION_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs declaration of function
   `IR_free_node' as external definition or as friend function of
   `IR_node'. */

static void
output_node_deletion_function_declaration (void)
{
  output_string (output_interface_file, cpp_flag ? "  friend " : "extern ");
  output_title_of_node_deletion_function (output_interface_file);
  output_string (output_interface_file, ";\n\n");
}

/* The following function outputs definitions of node field deletion
   function and the function `IR_free_node'. */

static void
output_node_deletion_function_definition (void)
{
  /* Output field deletion function. */
  output_field_deletion_function ();
  /* Function itself */
  output_title_of_node_deletion_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, NODE_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, " == NULL)\n    return;\n");
  /* Field freeing */
  output_string (output_implementation_file, "  ");
  output_name_of_field_deletion_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file, NODE_DELETION_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file, NODE_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Memory node freeing */
  output_string (output_implementation_file, "  ");
  output_free_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, NODE_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_node_size_array_name (output_implementation_file);
  output_string (output_implementation_file, " [");
  if (cpp_flag)
    {
      output_string (output_implementation_file, NODE_DELETION_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, ")]);\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following macro is name of parameter of function
   `_IR_free_graph'. */

#define INTERNAL_GRAPH_DELETION_PARAMETER_NAME "graph"

/* The following macro value is name of parameter (C interface) or the
   corresponding node (C++ interface) of functions `IR_free_graph' and
   `IR_conditional_free_graph'. */

#define GRAPH_DELETION_PARAMETER_NAME \
   (cpp_flag ? "this" : INTERNAL_GRAPH_DELETION_PARAMETER_NAME)

/* The following function outputs title of (internal or external)
   function for deleting graph.  The most probable function output is
   following `[static ]void _IR_create_graph (IR_node_t graph)' or `void
   IR_create_graph (IR_node_t graph)' or
   `void IR_create_graph (void)'
   or `void IR_node::IR_create_graph (void)'. */

static void
output_title_of_graph_deletion_function (FILE *f, int internal_function_flag,
                                         int class_prefix_flag)
{
  assert (cpp_flag || !class_prefix_flag);
  assert (!class_prefix_flag || !internal_function_flag);
  if (internal_function_flag && !cpp_flag)
    output_string (f, "static ");
  output_string (f, "void ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_graph_deletion_function (f, internal_function_flag);
  output_string (f, " (");
  if (!cpp_flag || internal_function_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f,
                     internal_function_flag
                     ? INTERNAL_GRAPH_DELETION_PARAMETER_NAME
                     : GRAPH_DELETION_PARAMETER_NAME);
    }
  else
    output_string (f, "void");
  output_char (')', f);
}

/* The following function outputs code of for deletion of graph whose
   top node is value of given non-class field.  The function is called
   from function `traverse_all_node_type_fields'. */

static void
output_graph_field_deletion (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
    {
      output_string (output_implementation_file, "      ");
      output_name_of_graph_deletion_function (output_implementation_file,
                                              TRUE);
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           INTERNAL_GRAPH_DELETION_PARAMETER_NAME,
                           field, FALSE, FALSE);
      output_string (output_implementation_file, ");\n");
    }
}

/* If given node type is not abstract node type the function outputs
   fragment of code needed for deletion of all graphs whose top nodes
   are values of fields of given node type.  The function outputs the
   following fragment
                    case <node mode value of given node type>:
                      <code of deletion of all graphs whose top nodes are
                       values of fields of given node type>
                      break;
   The function is called from function `traverse_node_types'. */

static void
output_graph_node_type_fields_deletion (IR_node_t node_type)
{
  if (!IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      traverse_all_node_type_fields (node_type, output_graph_field_deletion);
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of internal function
   `_IR_free_graph'.  Remember about previous graph pass number member
   needed for correct processing graph and that function
   `_IR_free_graph' uses `IR_free_node'. */

static void
output_internal_graph_deletion_function (void)
{
  output_title_of_graph_deletion_function (output_implementation_file, TRUE,
                                           FALSE);
  output_string (output_implementation_file, "\n{\n");
  /* Test graph cycle */
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file,
                 " == NULL\n      || ");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  assert (graph_pass_number_member_flag);
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n    return;\n");
  /* Set up previous graph pass number member */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  /* Check guard. */
  output_string (output_implementation_file, "  if (");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " == NULL\n      || (*");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, ") (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n");
  /* Traverse fields of node type */
  output_string (output_implementation_file, "  switch (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n    {\n");
  traverse_node_types (output_graph_node_type_fields_deletion);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Memory node freeing */
  output_string (output_implementation_file, "  ");
  output_free_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_node_size_array_name (output_implementation_file);
  output_string (output_implementation_file, " [");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   INTERNAL_GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, ")]);\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs declaration of function
   `IR_free_graph'. */

static void
output_graph_deletion_function_declaration (void)
{
  output_string (output_interface_file, cpp_flag ? "  " : "extern ");
  output_title_of_graph_deletion_function (output_interface_file, FALSE,
                                           FALSE);
  output_string (output_interface_file, ";\n\n");
}

/* The following function outputs definition of function
   `IR_free_graph'.  Remember about previous graph pass number member
   needed for correct processing graph. */

static void
output_graph_deletion_function_definition (void)
{
  /* Function itself */
  output_title_of_graph_deletion_function (output_implementation_file, FALSE,
                                           cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " = NULL;\n");
  output_string (output_implementation_file, "  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n  ");
  output_name_of_graph_deletion_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for conditional
   deletion of graph.  The most probable function output is following
        `void IR_conditional_free_graph (IR_node_t graph,
           int (*IR_traverse_guard_function_parameter)(IR_node_t node))'
        or
        `void IR_conditional_free_graph (
           int (*IR_traverse_guard_function_parameter)(IR_node_t node))'
        or
        `void IR_node::IR_conditional_free_graph (
           int (*IR_traverse_guard_function_parameter)(IR_node_t node))'. */

static void
output_title_of_conditional_deletion_of_graph_function (FILE *f,
                                                        int class_prefix_flag)
{
  assert (cpp_flag || !class_prefix_flag);
  output_string (f, "void ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_conditional_deletion_of_graph_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, GRAPH_DELETION_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_definition_of_traverse_guard_function (f, FALSE);
  output_char (')', f);
}

/* The following function outputs declaration of function
   `IR_conditional_free_graph'. */

static void
output_conditional_graph_deletion_function_declaration (void)
{
  output_string (output_interface_file, cpp_flag ? "  " : "extern ");
  output_title_of_conditional_deletion_of_graph_function
    (output_interface_file, FALSE);
  output_string (output_interface_file, ";\n\n");
}

/* The following function outputs definition of function
   `IR_conditional_free_graph'.  Remember about previous graph pass
   number member needed for correct processing graph. */

static void
output_conditional_graph_deletion_function_definition (void)
{
  /* Function itself */
  output_title_of_conditional_deletion_of_graph_function
    (output_implementation_file, cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " = ");
  output_name_of_traverse_guard_function (output_implementation_file, FALSE);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n  ");
  output_name_of_graph_deletion_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, GRAPH_DELETION_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}



/* This page contains functions for output of functions of copying
   node and graph. */

/* The following macro value is name of the first parameter of
   `_IR_copy_node_fields'. */

#define COPY_NODE_MODE_PARAMETER_NAME  "node_mode"

/* The following macro value is name of the second parameter of
   `_IR_copy_node_fields'. */

#define COPY_TO_PARAMETER_NAME  "to"

/* The following macro value is name of the third parameter of
   `_IR_copy_node_fields'. */

#define COPY_FROM_PARAMETER_NAME  "from"

/* The following function outputs title of function for copying node
   fields.  The most probable function output is following
        `void _IR_copy_node_fields (IR_node_mode_t node_mode,
                                    IR_node_t to, IR_node_t from)'. */

static void
output_title_of_field_copy_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_field_copy_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, COPY_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, COPY_TO_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, COPY_FROM_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs code of copying given non class
   field.  The function is called from function
   `traverse_all_node_type_fields' or
   `output_node_type_fields_copy'. */

static void
output_field_copy (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS)
    {
      output_string (output_implementation_file, "      ");
      output_field_type_copy_macro_name_prefix (output_implementation_file);
      output_field_type_name (output_implementation_file,
                              IR_field_type (field));
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           COPY_TO_PARAMETER_NAME, field, FALSE, FALSE);
      output_string (output_implementation_file, ", ");
      output_field_access (output_implementation_file,
                           COPY_FROM_PARAMETER_NAME, field, FALSE, FALSE);
      output_string (output_implementation_file, ");\n");
      output_other_double_link_members_initiation
        (output_implementation_file, COPY_TO_PARAMETER_NAME, field);
      output_start_of_setting_double_link_field_value
        (output_implementation_file, "      ", COPY_TO_PARAMETER_NAME, field,
         FALSE, FALSE);
      if (IR_double_field_flag (field))
        output_string (output_implementation_file, "NULL");
      output_finish_of_setting_double_link_field_value
        (output_implementation_file, "      ", field, TRUE, FALSE);
    }
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for copying all
   fields of node of given type.  The function outputs the following
   fragment
                    case <node mode value of given node type>:
                      <code of copying all non-class fields>
                      break;
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_copy (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_structure_flag && double_node_types_existence_flag)
        /* For non-flat structure implementation the code initiating
           the first link member exists in one exemplar in copy node
           function itself. */
        output_first_link_member_initiation (node_type,
                                             COPY_TO_PARAMETER_NAME);
      if (flat_flag)
        traverse_all_node_type_fields (node_type, output_field_copy);
      else
        {
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element
		 (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      ");
                output_name_of_field_copy_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   COPY_TO_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   COPY_FROM_PARAMETER_NAME);
                output_string (output_implementation_file, ");\n");
              }
          if (IR_last_field (node_type) != NULL)
            for (current_field = IR_next_field (IR_last_field (node_type));;
                 current_field = IR_next_field (current_field))
              {
                assert (IR_NODE_MODE (current_field) == IR_NM_field
                        || IR_NODE_MODE (current_field) == IR_NM_action
                        || IR_NODE_MODE (current_field) == IR_NM_constraint);
                output_field_copy (current_field);
                if (current_field == IR_last_field (node_type))
                  break;
              }
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_copy_node_fields'. */

static void
output_field_copy_function (void)
{
  /* Output field copy function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_copy_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Copy fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file, COPY_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_copy);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following macro value is name of parameter of function
   `IR_copy_node'. */

#define NODE_COPY_PARAMETER_NAME (cpp_flag ? "this" : "node")

/* The following macro value is name of variable which will be result of
   function `IR_copy_node'. */

#define NODE_COPY_RESULT_NAME "_result"

/* The following function outputs title of function for copying node.
   The most probable function output is following
                `IR_node_t IR_copy_node (IR_node_t node)'
                or `IR_node_t IR_copy_node (void)'
                or `IR_node_t IR_node::IR_copy_node (IR_node_t node)'. */

static void
output_title_of_node_copy_function (FILE *f, int class_prefix_flag)
{
  assert (cpp_flag || !class_prefix_flag);
  output_node_t_name (f);
  output_char (' ', f);
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_node_copy_function (f);
  output_string (f, " (");
  if (cpp_flag)
    output_string (f, "void");
  else
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, NODE_COPY_PARAMETER_NAME);
    }
  output_char (')', f);
}

/* The following function outputs definition of node field copying
   function (with the aid of `output_field_copy_function') and
   external definition and definition of function `IR_copy_node'. */

static void
output_node_copy_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_node_copy_function (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Output field copy function. */
  output_field_copy_function ();
  /* Function itself */
  output_title_of_node_copy_function (output_implementation_file, cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_node_t_name (output_implementation_file);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, NODE_COPY_RESULT_NAME);
  output_string (output_implementation_file, ";\n\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, NODE_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, " == NULL)\n    return NULL;\n");
  /* Memory allocation */
  output_string (output_implementation_file, "  ");
  output_alloc_macro_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, NODE_COPY_RESULT_NAME);
  output_string (output_implementation_file, ", ");
  output_node_size_array_name (output_implementation_file);
  output_string (output_implementation_file, " [");
  if (cpp_flag)
    {
      output_string (output_implementation_file, NODE_COPY_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, ")], ");
  output_node_t_name (output_implementation_file);
  output_string (output_implementation_file, ");\n");
  output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, NODE_COPY_RESULT_NAME);
  output_string (output_implementation_file, " == NULL)\n    abort ();\n");
  output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
  /* Copy node mode member */
  output_string (output_implementation_file, "  ");
  if (cpp_flag)
    {
      output_string (output_implementation_file, NODE_COPY_RESULT_NAME);
      output_string (output_implementation_file, "->");
      output_node_mode_member_name (output_implementation_file);
    }
  else
    {
      output_node_mode_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file, NODE_COPY_RESULT_NAME);
      output_string (output_implementation_file, ")");
    }
  output_string (output_implementation_file, " = ");
  if (cpp_flag)
    {
      output_string (output_implementation_file, NODE_COPY_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  if (graph_pass_number_member_flag)
    {
      /* Set up previous graph pass number member */
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file, NODE_COPY_RESULT_NAME);
      output_string (output_implementation_file, "->");
      output_last_graph_pass_number_member_name (output_implementation_file);
      output_string (output_implementation_file, " = 0;\n");
    }
  /* Initiate node's first link member. */
  if (!flat_structure_flag && double_node_types_existence_flag)
    output_first_link_member_initiation (NULL, NODE_COPY_RESULT_NAME);
  /* Copying fields */
  output_string (output_implementation_file, "  ");
  output_name_of_field_copy_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file, NODE_COPY_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file, NODE_COPY_RESULT_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file, NODE_COPY_RESULT_NAME);
  output_string (output_implementation_file, ";\n}\n\n");
}

/* The following macro is name of parameter of function
   `_IR_copy_graph'. */

#define INTERNAL_GRAPH_COPY_PARAMETER_NAME "graph"

/* The following macro value is name of parameter (C interface) or the
   corresponding node (C++ interface) of functions `IR_copy_graph' and
   `IR_conditional_copy_graph'. */

#define GRAPH_COPY_PARAMETER_NAME \
   (cpp_flag ? "this" : INTERNAL_GRAPH_COPY_PARAMETER_NAME)

/* The following macro value is name of variable which will be result of
   function `_IR_copy_graph'. */

#define GRAPH_COPY_RESULT_NAME "_result"

/* The following function outputs title of (internal or external)
   function for copying graph.  The most probable function output is
   following
        `[static ]IR_node_t _IR_copy_graph (IR_node_t graph)'.
        or
        `IR_node_t IR_copy_graph (IR_node_t graph)'
        or
        `IR_node_t IR_copy_graph (void)'
        or
        `IR_node_t IR_node::IR_copy_graph (void)'. */

static void
output_title_of_graph_copy_function (FILE *f, int internal_function_flag,
                                     int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  if (internal_function_flag && !cpp_flag)
    output_string (f, "static ");
  output_node_t_name (f);
  output_char (' ', f);
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_graph_copy_function (f, internal_function_flag);
  output_string (f, " (");
  if (cpp_flag && !internal_function_flag)
    output_string (f, "void");
  else
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f,
                     internal_function_flag
                     ? INTERNAL_GRAPH_COPY_PARAMETER_NAME
                     : GRAPH_COPY_PARAMETER_NAME);
    }
  output_char (')', f);
}

/* The following function outputs code of for copying graph whose top
   node is value of given non-class field.  The function is called
   from function `traverse_all_node_type_fields'. */

static void
output_graph_field_copy (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
    {
      output_string (output_implementation_file, "      ");
      output_field_access (output_implementation_file,
                           GRAPH_COPY_RESULT_NAME, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, "\n        = ");
      output_name_of_graph_copy_function (output_implementation_file, TRUE);
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           INTERNAL_GRAPH_COPY_PARAMETER_NAME, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, ");\n");
    }
}

/* If given node type is not abstract node type the function outputs
   fragment of code needed for copying all graphs whose top nodes are
   values of fields of given node type.  The function outputs the
   following fragment
            case <node mode value of given node type>:
              <code of copying all graphs whose top nodes are
               values of fields of given node type>
              break;
   The function is called from function `traverse_node_types'. */

static void
output_copy_of_graph_node_type_fields (IR_node_t node_type)
{
  if (!IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      traverse_all_node_type_fields (node_type, output_graph_field_copy);
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of internal function
   `_IR_copy_graph'.  Remember about previous graph pass number member
   and temporary member needed for correct processing graph and that
   function `_IR_copy_graph' uses `IR_copy_node'. */

static void
output_internal_graph_copy_function (void)
{
  output_title_of_graph_copy_function (output_implementation_file,
                                       TRUE, FALSE);
  output_string (output_implementation_file, "\n{\n  ");
  output_node_t_name (output_implementation_file);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, GRAPH_COPY_RESULT_NAME);
  output_string (output_implementation_file, ";\n\n");
  /* Test graph cycle */
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file,
                 " == NULL)\n    return NULL;\n  else if (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  assert (graph_pass_number_member_flag);
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n    return ");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  /* Copy graph top */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, GRAPH_COPY_RESULT_NAME);
  output_string (output_implementation_file, " = ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_COPY_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_name_of_node_copy_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   INTERNAL_GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Set up previous graph pass number member */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  /* Set up temporary member */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, GRAPH_COPY_RESULT_NAME);
  output_string (output_implementation_file, ";\n");
  /* Check guard. */
  output_string (output_implementation_file, "  if (");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " == NULL\n      || (*");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, ") (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n");
  /* Traverse fields of node type */
  output_string (output_implementation_file, "  switch (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_COPY_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   INTERNAL_GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n    {\n");
  traverse_node_types (output_copy_of_graph_node_type_fields);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file, GRAPH_COPY_RESULT_NAME);
  output_string (output_implementation_file, ";\n}\n\n");
}

/* The following function outputs external definition (C interface)
   and definition of function `IR_copy_graph'.  Remember about
   previous graph pass number member and temporary member needed for
   correct processing graph. */

static void
output_graph_copy_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_graph_copy_function (output_interface_file, FALSE,
                                           FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_graph_copy_function (output_implementation_file,
                                       FALSE, cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " = NULL;\n");
  output_string (output_implementation_file, "  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n");
  output_string (output_implementation_file, "  return ");
  output_name_of_graph_copy_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for conditional
   copying graph.  The most probable function output is following
        `IR_node_t IR_conditional_copy_graph (IR_node_t graph,
        int (*IR_traverse_guard_function_parameter)(IR_node_t node))'
     or
        `IR_node_t IR_conditional_copy_graph (
         int (*IR_traverse_guard_function_parameter)(IR_node_t node))'
     or
        `IR_node_t IR_node::IR_conditional_copy_graph (
         int (*IR_traverse_guard_function_parameter)(IR_node_t node))'. */

static void
output_title_of_conditional_copy_of_graph_function (FILE *f,
                                                    int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  output_node_t_name (f);
  output_char (' ', f);
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_conditional_copy_of_graph_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, GRAPH_COPY_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_definition_of_traverse_guard_function (f, FALSE);
  output_char (')', f);
}

/* The following function outputs external definition (C interface)
   and definition of function `IR_conditional_copy_graph'.  Remember
   about previous graph pass number member and temporary member needed
   for correct processing graph. */

static void
output_conditional_graph_copy_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_conditional_copy_of_graph_function
        (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_conditional_copy_of_graph_function
    (output_implementation_file, cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " = ");
  output_name_of_traverse_guard_function (output_implementation_file, FALSE);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n");
  output_string (output_implementation_file, "  return ");
  output_name_of_graph_copy_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, GRAPH_COPY_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs function of copying node (with the
   aid of function `output_node_copy_function') and function of
   copying graph (with the aid of function
   `output_graph_copy_function'). */

static void
output_copy_functions (void)
{
  if (copy_flag || copy_graph_flag)
    output_node_copy_function ();
  if (copy_graph_flag)
    {
      output_internal_graph_copy_function ();
      output_graph_copy_function ();
      output_conditional_graph_copy_function ();
    }
}



/* This page contains functions for output of functions of comparisons of
   nodes and graphs. */

/* The following macro value is name of the first parameter of
   `_IR_node_field_equality'. */

#define NODE_MODE_EQUALITY_PARAMETER_NAME  "node_mode"

/* The following macro is name of parameter of function
   `_IR_node_field_equality'. */

#define INTERNAL_NODE_EQUALITY_PARAMETER_NAME_1 "node_1"

/* The following macro value is name of the first parameter of
   function `IR_is_equal_node' and the second parameter of
   `_IR_node_field_equality'. */

#define NODE_EQUALITY_PARAMETER_NAME_1 \
  (cpp_flag ? "this" : INTERNAL_NODE_EQUALITY_PARAMETER_NAME_1)

/* The following macro value is name of the parameter (C interface) or
   the corresponding node (C++ interface) of function
   `IR_is_equal_node' and `_IR_node_field_equality'. */

#define NODE_EQUALITY_PARAMETER_NAME_2 "node_2"

/* The following function outputs title of function for comparison of
   node fields.  The most probable function output is following
        `int _IR_node_field_equality (IR_node_mode_t node_mode,
         IR_node_t node_1, IR_node_t node_2)'. */

static void
output_title_of_field_equality_function (FILE *f)
{
  output_string (f, "int ");
  output_name_of_field_equality_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, NODE_MODE_EQUALITY_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, INTERNAL_NODE_EQUALITY_PARAMETER_NAME_1);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, NODE_EQUALITY_PARAMETER_NAME_2);
  output_char (')', f);
}

/* The following function outputs code for comparison of non class
   field values.  The function is called from function
   `traverse_all_node_type_fields' or
   `output_node_type_fields_equality'. */

static void
output_field_equality (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS)
    {
      output_string (output_implementation_file, "      if (!");
      output_field_type_equality_macro_name_prefix
        (output_implementation_file);
      output_field_type_name (output_implementation_file,
                              IR_field_type (field));
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           INTERNAL_NODE_EQUALITY_PARAMETER_NAME_1, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, ", ");
      output_field_access (output_implementation_file,
                           NODE_EQUALITY_PARAMETER_NAME_2, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, "))\n        return 0;\n");
    }
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for comparison of
   all non-class fields of node of given type.  The function outputs
   the following fragment
                    case <node mode value of given node type>:
                      <comparison of all non class fields>
                      break;
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_equality (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        traverse_all_node_type_fields (node_type, output_field_equality);
      else
        {
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      if (!");
                output_name_of_field_equality_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   INTERNAL_NODE_EQUALITY_PARAMETER_NAME_1);
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   NODE_EQUALITY_PARAMETER_NAME_2);
                output_string (output_implementation_file,
                               "))\n        return 0;\n");
              }
          if (IR_last_field (node_type) != NULL)
            for (current_field = IR_next_field (IR_last_field (node_type));;
                 current_field = IR_next_field (current_field))
              {
                assert (IR_NODE_MODE (current_field) == IR_NM_field
                        || IR_NODE_MODE (current_field) == IR_NM_action
                        || IR_NODE_MODE (current_field) == IR_NM_constraint);
                output_field_equality (current_field);
                if (current_field == IR_last_field (node_type))
                  break;
              }
        }
      output_string (output_implementation_file, "      return 1;\n");
    }
}

/* The following function outputs definition of function
   `_IR_node_field_equality'. */

static void
output_field_equality_function (void)
{
  /* Output field equality function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_equality_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Check equality fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 NODE_MODE_EQUALITY_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_equality);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for comparison of
   nodes.  The most probable function output is following
        `int IR_is_equal_node (IR_node_t node_1, IR_node_t node_2)' or
        `int IR_is_equal_node (IR_node_t node_2)' or
        `int IR_node::IR_is_equal_node (IR_node_t node_2)'. */

static void
output_title_of_node_equality_function (FILE *f, int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  output_string (f, "int ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_node_equality_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, NODE_EQUALITY_PARAMETER_NAME_1);
      output_string (f, ", ");
    }
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, NODE_EQUALITY_PARAMETER_NAME_2);
  output_char (')', f);
}

/* The folowing function outputs definition of node fields equality
   functions (with the aid of function
   `output_field_equality_function') and external definition (C
   interface) and definition of function `IR_is_equal_node'. */

static void
output_node_equality_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_node_equality_function (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Output field equality function. */
  output_field_equality_function ();
  /* Function itself */
  output_title_of_node_equality_function (output_implementation_file,
                                          cpp_flag);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, " == ");
  output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, ")\n    return 1;\n");
  output_string (output_implementation_file, "  else if (");
  output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, " == NULL || ");
  output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, " == NULL\n           || ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_EQUALITY_PARAMETER_NAME_1);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, ") != ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_EQUALITY_PARAMETER_NAME_2);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, "))\n    return ");
  output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, " == ");
  output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, ";\n");
  /* Field comparison */
  output_string (output_implementation_file, "  return ");
  output_name_of_field_equality_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_EQUALITY_PARAMETER_NAME_1);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following macro is name of parameter of function
   `_IR_is_equal_graph'. */

#define INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1 "graph_1"

/* The following macro value is name of the first parameter (C
    interface) or the corresponding node (C++ interface) of functions
    `IR_is_equal_graph' and `IR_conditional_is_equal_graph'. */

#define GRAPH_EQUALITY_PARAMETER_NAME_1 \
   (cpp_flag ? "this" : INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1)

/* The following macro value is name of the second parameter of functions
   `_IR_is_equal_graph' and `IR_is_equal_graph'
   and `IR_conditional_is_equal_graph'. */

#define GRAPH_EQUALITY_PARAMETER_NAME_2 "graph_2"

/* The following macro value is name of variable which will be result of
   function `_IR_is_equal_graph'. */

#define GRAPH_EQUALITY_RESULT_NAME "_result"

/* The following function outputs title of (internal or external)
   function for comparison of graphs.  The most probable function
   output is following
        `[static ]int _IR_is_equal_graph (IR_node_t graph_1,
                                        IR_node_t graph_2)'
        or
        `int IR_is_equal_graph (IR_node_t graph_1, IR_node_t graph_2)'
        or
        `int _IR_is_equal_graph (IR_node_t graph_2)'
        or
        `int IR_node::IR_is_equal_graph (IR_node_t graph_2)'. */

static void
output_title_of_graph_equality_function (FILE *f, int internal_function_flag,
                                         int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  assert (!class_prefix_flag || !internal_function_flag);
  if (internal_function_flag && !cpp_flag)
    output_string (f, "static ");
  output_string (f, "int ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_graph_equality_function (f, internal_function_flag);
  output_string (f, " (");
  if (!cpp_flag || internal_function_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f,
                     internal_function_flag
                     ? INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1
                     : GRAPH_EQUALITY_PARAMETER_NAME_1);
      output_string (f, ", ");
    }
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_char (')', f);
}

/* The following function outputs code of for comparison of graphs
   whose top nodes are value of given non-class fields.  The function
   is called from function `traverse_all_node_type_fields'. */

static void
output_graph_field_equality (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
    {
      output_string (output_implementation_file, "        ");
      output_string (output_implementation_file, GRAPH_EQUALITY_RESULT_NAME);
      output_string (output_implementation_file, " = ");
      output_name_of_graph_equality_function (output_implementation_file,
                                              TRUE);
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, ", ");
      output_field_access (output_implementation_file,
                           GRAPH_EQUALITY_PARAMETER_NAME_2, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, ");\n");
      output_string (output_implementation_file, "        if (!");
      output_string (output_implementation_file, GRAPH_EQUALITY_RESULT_NAME);
      output_string (output_implementation_file, ")\n          break;\n");
    }
}

/* If given node type is not abstract node type the function outputs
   fragment of code needed for comparison of all graphs whose top
   nodes are values of fields of given node type.  The function
   outputs the following fragment
            case <node mode value of given node type>:
              <code of comparison of all graphs whose top nodes are
               values of fields of given node type>
              break;
   The function is called from function `traverse_node_types'. */

static void
output_graph_node_type_fields_equality (IR_node_t node_type)
{
  if (!IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "      case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      traverse_all_node_type_fields (node_type, output_graph_field_equality);
      output_string (output_implementation_file, "        break;\n");
    }
}

/* The following function outputs definition of internal function
   `_IR_is_equal_graph'.  Remember about previous graph pass number
   member needed for correct processing graph and that function
   `_IR_is_equal_graph' uses `IR_is_equal_node'. */

static void
output_internal_graph_equality_function (void)
{
  output_title_of_graph_equality_function (output_implementation_file, TRUE,
                                           FALSE);
  output_string (output_implementation_file, "\n{\n  int ");
  output_string (output_implementation_file, GRAPH_EQUALITY_RESULT_NAME);
  output_string (output_implementation_file, ";\n\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, " == ");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, ")\n    return 1;\n");
  output_string (output_implementation_file, "  else if (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, " == NULL || ");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, " == NULL)\n    return 0;\n");
  /* Test that graphs are simultaneously cycled */
  output_string (output_implementation_file, "  else if (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, "->");
  assert (graph_pass_number_member_flag);
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "\n           && ");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n    return 1;\n");
  /* Test that graph is cycled */
  output_string (output_implementation_file, "  else if (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "\n           || ");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n    return 0;\n");
  /* Top nodes comparison */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, GRAPH_EQUALITY_RESULT_NAME);
  output_string (output_implementation_file, " = ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
      output_string (output_implementation_file, "->");
    }
  output_name_of_node_equality_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
      output_string (output_implementation_file, ", ");
    }
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, ");\n");
  /* Set up previous graph pass number members */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  /* Traverse fields of node type and check guard. */
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, GRAPH_EQUALITY_RESULT_NAME);
  output_string (output_implementation_file, "\n      && (");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " == NULL\n          || (*");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, ") (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, ")))\n");
  output_string (output_implementation_file, "    switch (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   INTERNAL_GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, "))\n      {\n");
  traverse_node_types (output_graph_node_type_fields_equality);
  output_string
    (output_implementation_file,
     "      default:\n        abort ();\n        break;\n      }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file, GRAPH_EQUALITY_RESULT_NAME);
  output_string (output_implementation_file, ";\n}\n\n");
}

/* The following function outputs external definition (C interface)
   and definition of function `IR_is_equal_graph'.  Remember about
   previous graph pass number member needed for correct processing
   graph. */

static void
output_graph_equality_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_graph_equality_function (output_interface_file, FALSE,
                                               FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_graph_equality_function (output_implementation_file, FALSE,
                                           cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " = NULL;\n");
  output_string (output_implementation_file, "  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n");
  output_string (output_implementation_file, "  return ");
  output_name_of_graph_equality_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for conditional
   comparison of graphs.  The most probable function output is
   following
        `int IR_conditional_is_equal_graph
        (IR_node_t graph_1, IR_node_t graph_2,
        int (*IR_traverse_guard_function_parameter)(IR_node_t node))'
   or
        `int IR_conditional_is_equal_graph
          (IR_node_t graph_2,
           int (*IR_traverse_guard_function_parameter)(IR_node_t node))'
   or
        `int IR_node::IR_conditional_is_equal_graph
           (IR_node_t graph_2,
            int (*IR_traverse_guard_function_parameter)(IR_node_t node))'. */

static void
output_title_of_conditional_equality_of_graph_function (FILE *f,
                                                        int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  output_string (f, "int ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_conditional_graph_equality_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, GRAPH_EQUALITY_PARAMETER_NAME_1);
      output_string (f, ", ");
    }
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (f, ", ");
  output_definition_of_traverse_guard_function (f, FALSE);
  output_char (')', f);
}

/* The following function outputs external definition (C interface)
   and definition of function `IR_conditional_is_equal_graph'.
   Remember about previous graph pass number member needed for correct
   processing graph. */

static void
output_conditional_graph_equality_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_conditional_equality_of_graph_function
        (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_conditional_equality_of_graph_function
    (output_implementation_file, cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " = ");
  output_name_of_traverse_guard_function (output_implementation_file, FALSE);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n");
  output_string (output_implementation_file, "  return ");
  output_name_of_graph_equality_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_1);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, GRAPH_EQUALITY_PARAMETER_NAME_2);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs function of comparison of nodes
   (with the aid of function `output_node_equality_function') and
   function of comparison of graphs (with the aid of function
   `output_graph_equality_function'). */

static void
output_equality_functions (void)
{
  if (equal_flag || equal_graph_flag)
    output_node_equality_function ();
  if (equal_graph_flag)
    {
      output_internal_graph_equality_function ();
      output_graph_equality_function ();
      output_conditional_graph_equality_function ();
    }
}



/* This page contains functions for output of functions of checking
   node and graph. */

/* The following macro value is name of the first parameter of
   `_IR_check_node_fields' and of `_IR_print_node_fields_for_check'. */

#define CHECK_NODE_MODE_PARAMETER_NAME  "node_mode"

#define INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME "node"

/* The following macro value is name of the first parameter of
   function `IR_check_node' and the second parameter of
   `_IR_check_node_fields' and of `_IR_print_node_fields_for_check'. */

#define NODE_CHECK_NODE_PARAMETER_NAME \
  (cpp_flag ? "this" : INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME)

/* The following macro value is name of the second parameter of
   function `IR_check_node' and the third parameter of
   `_IR_check_node_fields' and of `_IR_print_node_fields_for_check'. */

#define NODE_CHECK_FLAG_PARAMETER_NAME  "class_field_flag"

/* The following function outputs title of function for checking node.
   The most probable function output is following
     `int _IR_check_node_fields (IR_node_mode_t node_mode,
                                 IR_node_t node, int class_field_flag)'. */

static void
output_title_of_field_check_function (FILE *f)
{
  output_string (f, "int ");
  output_name_of_field_check_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, CHECK_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f, NODE_CHECK_FLAG_PARAMETER_NAME);
  output_char (')', f);
}


/* The following function outputs code of checking given non-class or
   class field and code of given non-class or class constraint.
   Remember that the code is output with `[' and `]'. */

static void
output_field_check (IR_node_t field, int class_field_flag)
{
  if (IR_NODE_MODE (field) == IR_NM_field)
    {
      if (IR_declaration_part (field) != DP_CLASS && !class_field_flag
          || IR_declaration_part (field) == DP_CLASS && class_field_flag)
        {
          if (IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
            {
              output_string (output_implementation_file, "      if (");
              output_field_access (output_implementation_file,
                                   INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME,
                                   field, FALSE, FALSE);
              output_string (output_implementation_file,
                             " != NULL\n          && !");
              if (!fast_flag)
                output_string (output_implementation_file, "((");
              if (IR_field_type (field) == root_node_type)
                output_subtype_flag_array_name (output_implementation_file,
                                                ROOT_NAME);
              else
                output_subtype_flag_array_name
                  (output_implementation_file,
                   IR_identifier_itself (IR_field_type_identifier (field)));
              output_string (output_implementation_file, " [");
              if (cpp_flag)
                {
                  output_field_access (output_implementation_file,
                                       INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME,
                                       field, FALSE, FALSE);
                  output_string (output_implementation_file, "->");
                }
              output_node_mode_name (output_implementation_file);
              output_string (output_implementation_file, " (");
              if (!cpp_flag)
                output_field_access (output_implementation_file,
                                     INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME,
                                     field, FALSE, FALSE);
              if (!fast_flag)
                {
                  output_string (output_implementation_file, ") / ");
                  output_decimal_number (output_implementation_file, CHAR_BIT);
                  output_string (output_implementation_file,
                                 "]\n                >> (");
                  if (cpp_flag)
                    {
                      output_field_access
                        (output_implementation_file,
                         INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME,
                         field, FALSE, FALSE);
                      output_string (output_implementation_file, "->");
                    }
                  output_node_mode_name (output_implementation_file);
                  output_string (output_implementation_file, " (");
                  if (!cpp_flag)
                    output_field_access
                      (output_implementation_file,
                       INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME,
                       field, FALSE, FALSE);
                  output_string (output_implementation_file, ") % ");
                  output_decimal_number (output_implementation_file, CHAR_BIT);
                  output_string (output_implementation_file,
                                 ")) & 1))\n         return 1");
                }
              else
                output_string (output_implementation_file,
                               ")])\n         return 0");
              output_string (output_implementation_file, ";\n");
            }
          last_processed_field = field;
        }
    }
  else if (IR_NODE_MODE (field) == IR_NM_constraint)
    {
      if (IR_declaration_part (field) != DP_CLASS && !class_field_flag
          || IR_declaration_part (field) == DP_CLASS && class_field_flag)
        {
          output_string (output_implementation_file, "      if (!(");
          output_code (output_implementation_file, field,
                       INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME, FALSE);
          output_string (output_implementation_file,
                         "))\n         return 1;\n");
        }
    }
}

/* The following function outputs code of checking given non-class
   field or code of given non-class constraint.  The function uses
   `output_field_check'.  The function is called from function
   `traverse_all_node_type_fields' or
   `output_node_type_fields_check'. */

static void
output_non_class_field_check (IR_node_t field)
{
  output_field_check (field, FALSE);
}

/* The following function outputs code of checking given class field
   or code of given class constraint.  The function uses
   `output_field_check'.  The function is called from function
   `traverse_all_node_type_fields' or
   `output_node_type_fields_check'. */

static void
output_class_field_check (IR_node_t field)
{
  output_field_check (field, TRUE);
}

/* If given node type is not abstract node or non-flat model is used
   type the function outputs fragment of code needed for checking all
   fields of node of given type.  The function outputs the following
   fragment
            case <node mode value of given node type>:
              <code of checking all fields>
              break;
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_check (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        {
          last_processed_field = NULL;
          traverse_all_node_type_fields (node_type,
                                         output_non_class_field_check);
          /* Output processing class fields */
          if (node_type_class_field_presence (node_type, TRUE))
            {
              output_string (output_implementation_file, "      if (!");
              output_string (output_implementation_file,
                             NODE_CHECK_FLAG_PARAMETER_NAME);
              output_string (output_implementation_file,
                             ")\n        return 0;\n");
              traverse_all_node_type_fields (node_type,
                                             output_class_field_check);
            }
        }
      else
        {
          last_processed_field = last_field_in_super_type (node_type, FALSE);
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      if (");
                output_name_of_field_check_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               NODE_CHECK_FLAG_PARAMETER_NAME);
                output_string (output_implementation_file,
                               "))\n        return 1;\n");
              }
          if (IR_last_field (node_type) != NULL)
            {
              for (current_field = IR_next_field (IR_last_field (node_type));;
                   current_field = IR_next_field (current_field))
                {
                  assert (IR_NODE_MODE (current_field) == IR_NM_field
                          || IR_NODE_MODE (current_field) == IR_NM_action
                          || IR_NODE_MODE (current_field) == IR_NM_constraint);
                  output_non_class_field_check (current_field);
                  if (current_field == IR_last_field (node_type))
                    break;
                }
              /* Output processing class fields */
              if (node_type_class_field_presence (node_type, FALSE))
                {
                  output_string (output_implementation_file, "      if (!");
                  output_string (output_implementation_file,
                                 NODE_CHECK_FLAG_PARAMETER_NAME);
                  output_string (output_implementation_file,
                                 ")\n        return 0;\n");
                  for (current_field
                       = IR_next_field (IR_last_field (node_type));;
                       current_field = IR_next_field (current_field))
                    {
                      output_class_field_check (current_field);
                      if (current_field == IR_last_field (node_type))
                        break;
                    }
                }               
            }
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_check_node_fields'. */

static void
output_field_check_function (void)
{
  /* Output field check function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_check_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Check fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file, CHECK_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_check);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  output_string (output_implementation_file, "  return 0;\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for print nodes to
   which given node reffers.  The most probable function output is
   following
                `void _IR_print_node_fields_for_check
                (IR_node_mode_t node_mode, IR_node_t node,
                int class_field_flag)'. */

static void
output_title_of_field_print_for_check_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_field_print_for_check_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, CHECK_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f, NODE_CHECK_FLAG_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs code of for printing node which is
   is value of given non-class field.  The function is called from
   function `traverse_all_node_type_fields' or
   `output_node_type_fields_print_for_check'.  Remember that
   `IR_check_node' uses `IR_print_node'. */

static void
output_field_print_for_check (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
    {
      output_string (output_implementation_file, "      ");
      if (cpp_flag)
        {
          output_field_access (output_implementation_file,
                               INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME, field,
                               FALSE, FALSE);
          output_string (output_implementation_file, "->");
        }
      output_name_of_node_print_function (output_implementation_file);
      output_string (output_implementation_file, " (");
      if (!cpp_flag)
        {
          output_field_access (output_implementation_file,
                               INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME, field,
                               FALSE, FALSE);
          output_string (output_implementation_file, ", ");
        }
      output_string (output_implementation_file,
                     NODE_CHECK_FLAG_PARAMETER_NAME);
      output_string (output_implementation_file, ");\n");
    }
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for printing all
   nodes which are values of fields of given node type.  The function
   outputs the following fragment
                    case <node mode value of given node type>:
                      <code of print all nodes which are
                       values of fields of given node type>
                      break;
  The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_print_for_check (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        traverse_all_node_type_fields (node_type,
                                       output_field_print_for_check);
      else
        {
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      ");
                output_name_of_field_print_for_check_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   INTERNAL_NODE_CHECK_NODE_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               NODE_CHECK_FLAG_PARAMETER_NAME);
                output_string (output_implementation_file, ");\n");
              }
          if (IR_last_field (node_type) != NULL)
            for (current_field = IR_next_field (IR_last_field (node_type));;
                 current_field = IR_next_field (current_field))
              {
                assert (IR_NODE_MODE (current_field) == IR_NM_field
                        || IR_NODE_MODE (current_field) == IR_NM_action
                        || IR_NODE_MODE (current_field) == IR_NM_constraint);
                output_field_print_for_check (current_field);
                if (current_field == IR_last_field (node_type))
                  break;
              }
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_print_node_fields_for_check'. */

static void
output_field_print_for_check_function (void)
{
  /* Output field print function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_print_for_check_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Print fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file, CHECK_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_print_for_check);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for checking node.
   The most probable function output is following
       `int IR_check_node (IR_node_t node, int class_field_flag)' or
       `int IR_check_node (int class_field_flag)' or
       `int IR_node::IR_check_node (int class_field_flag)'. */

static void
output_title_of_node_check_function (FILE *f, int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  output_string (f, "int ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_node_check_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, NODE_CHECK_NODE_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, NODE_CHECK_FLAG_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs definitions of node field check
   function (with the aid of `output_field_check_function'), field
   print for check function (with the aid of
   `output_field_print_for_check_function') and external definition (C
   interface) and definition of function `IR_check_node'.  Remember
   that `IR_check_node' uses `IR_print_node'. */

static void
output_node_check_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_node_check_function (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Output field check function. */
  output_field_check_function ();
  /* Output field print for check function. */
  output_field_print_for_check_function ();
  /* Function itself */
  output_title_of_node_check_function (output_implementation_file, cpp_flag);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, NODE_CHECK_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, " == NULL)\n    return 0;\n");
  /* Field checking */
  output_string (output_implementation_file, "  if (!");
  output_name_of_field_check_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_CHECK_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_CHECK_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file, NODE_CHECK_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_CHECK_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n    return 0;\n");
  /* Print node */
  output_string (output_implementation_file, "  ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_CHECK_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_name_of_node_print_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_CHECK_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
    }
  output_string (output_implementation_file, NODE_CHECK_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Print nodes which are children of given node */
  output_string (output_implementation_file, "  ");
  output_name_of_field_print_for_check_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_CHECK_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_CHECK_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file, NODE_CHECK_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_CHECK_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return 1;\n}\n\n");
}

/* The following macro is name of parameter of function
   `_IR_check_graph'. */

#define INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME "graph"

/* The following macro value is name of the first parameter (C
   interface) or the corresponding node (C++ interface) of functions
   `IR_check_graph' and `IR_conditional_check_graph'. */

#define GRAPH_CHECK_GRAPH_PARAMETER_NAME \
  (cpp_flag ? "this" : INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME)

/* The following macro value is name of the second parameter of
   functions `_IR_check_graph' and `IR_check_graph' and
   `IR_conditional_check_graph'. */

#define GRAPH_CHECK_FLAG_PARAMETER_NAME "class_field_flag"

/* The following macro value is name of variable which will be result of
   function `_IR_check_graph'. */

#define GRAPH_CHECK_RESULT_NAME "_result"

/* The following function outputs title of (internal or external)
   function for checking graph.  The most probable function output is
   following
        `[static ]int _IR_check_graph (IR_node_t graph,
                                       int class_field_flag)'
        or
        `int IR_check_graph (IR_node_t graph, int class_field_flag)'
        or
        `int IR_check_graph (int class_field_flag)'
        or
        `int IR_node::IR_check_graph (int class_field_flag)'. */

static void
output_title_of_graph_check_function (FILE *f, int internal_function_flag,
                                      int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  assert (!class_prefix_flag || !internal_function_flag);
  if (internal_function_flag && !cpp_flag)
    output_string (f, "static ");
  output_string (f, "int ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_graph_check_function (f, internal_function_flag);
  output_string (f, " (");
  if (!cpp_flag || internal_function_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f,
                     internal_function_flag
                     ? INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME
                     : GRAPH_CHECK_GRAPH_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, GRAPH_CHECK_FLAG_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs code of for checking graph whose top
   node is value of given non-class field.  The function is called
   from function `traverse_all_node_type_fields'. */

static void
output_graph_field_check (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
    {
      output_string (output_implementation_file, "      ");
      output_string (output_implementation_file, GRAPH_CHECK_RESULT_NAME);
      output_string (output_implementation_file, " = ");
      output_name_of_graph_check_function (output_implementation_file, TRUE);
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file,
                     GRAPH_CHECK_FLAG_PARAMETER_NAME);
      output_string (output_implementation_file, ") || ");
      output_string (output_implementation_file, GRAPH_CHECK_RESULT_NAME);
      output_string (output_implementation_file, ";\n");
    }
}

/* If given node type is not abstract node type the function outputs
   fragment of code needed for checking all graphs whose top nodes are
   values of fields of given node type.  The function outputs the
   following fragment
            case <node mode value of given node type>:
              <code of checking all graphs whose top nodes are
               values of fields of given node type>
              break;
   The function is called from function `traverse_node_types'. */

static void
output_check_of_graph_node_type_fields (IR_node_t node_type)
{
  if (!IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      traverse_all_node_type_fields (node_type, output_graph_field_check);
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of internal function
   `_IR_check_graph'.  Remember about previous graph pass number
   member needed for correct processing graph and that function
   `_IR_check_graph' uses `IR_check_node'. */

static void
output_internal_graph_check_function (void)
{
  output_title_of_graph_check_function (output_implementation_file, TRUE,
                                        FALSE);
  output_string (output_implementation_file, "\n{\n  int ");
  output_string (output_implementation_file, GRAPH_CHECK_RESULT_NAME);
  output_string (output_implementation_file, ";\n\n");
  /* Test graph cycle */
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file,
                 " == NULL\n      || ");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  assert (graph_pass_number_member_flag);
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n    return 0;\n");
  /* Node itself checking */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, GRAPH_CHECK_RESULT_NAME);
  output_string (output_implementation_file, " = ");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_name_of_node_check_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
    }
  output_string (output_implementation_file, GRAPH_CHECK_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Set up previous graph pass number member */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  /* Check guard. */
  output_string (output_implementation_file, "  if (");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " == NULL\n      || (*");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, ") (");
  output_string (output_implementation_file,
                 INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n");
  /* Traverse fields of node type */
  output_string (output_implementation_file, "  switch (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   INTERNAL_GRAPH_CHECK_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n    {\n");
  traverse_node_types (output_check_of_graph_node_type_fields);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file, GRAPH_CHECK_RESULT_NAME);
  output_string (output_implementation_file, ";\n}\n\n");
}

/* The following function outputs external definition (C interface)
   and definition of function `IR_check_graph'. */

static void
output_graph_check_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_graph_check_function (output_interface_file,
                                            FALSE, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_graph_check_function (output_implementation_file, FALSE,
                                        cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " = NULL;\n");
  output_string (output_implementation_file, "  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n");
  output_string (output_implementation_file, "  return ");
  output_name_of_graph_check_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, GRAPH_CHECK_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, GRAPH_CHECK_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for conditional
   checking graph.  The most probable function output is following
        `int IR_conditional_check_graph (IR_node_t graph,
           int class_field_flag,
           int (*IR_traverse_guard_function_parameter)(IR_node_t node))' or
        `int IR_conditional_check_graph (int class_field_flag,
           int (*IR_traverse_guard_function_parameter)(IR_node_t node))' or
        `int IR_node::IR_conditional_check_graph (int class_field_flag,
           int (*IR_traverse_guard_function_parameter)(IR_node_t node))'. */

static void
output_title_of_conditional_graph_check_function (FILE *f, 
                                                  int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  output_string (f, "int ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_conditional_check_of_graph_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, GRAPH_CHECK_GRAPH_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, GRAPH_CHECK_FLAG_PARAMETER_NAME);
  output_string (f, ", ");
  output_definition_of_traverse_guard_function (f, FALSE);
  output_char (')', f);
}

/* The following function outputs external definition (C interface)
   and definition of function `IR_conditional_check_graph'. */

static void
output_conditional_graph_check_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_conditional_graph_check_function (output_interface_file,
                                                        FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_conditional_graph_check_function
    (output_implementation_file, cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_name_of_traverse_guard_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " = ");
  output_name_of_traverse_guard_function (output_implementation_file, FALSE);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n");
  output_string (output_implementation_file, "  return ");
  output_name_of_graph_check_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, GRAPH_CHECK_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, GRAPH_CHECK_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs function of checking node (with the
   aid of function `output_node_check_function') and function of
   checking graph (with the aid of function
   `output_graph_check_function'). */

static void
output_check_functions (void)
{
  if (check_flag || check_graph_flag)
    output_node_check_function ();
  if (check_graph_flag)
    {
      output_internal_graph_check_function ();
      output_graph_check_function ();
      output_conditional_graph_check_function ();
    }
}



/* This page contains functions for output of functions of print, input and
   output of nodes. */


/* The following macro value is name of the first parameter of
   `_IR_print_node_fields'. */

#define FIELD_PRINT_NODE_MODE_PARAMETER_NAME  "node_mode"

#define INTERNAL_NODE_PRINT_NODE_PARAMETER_NAME "node"

/* The following macro value is name of the first parameter of
   function `IR_print_node' and the second parameter of
   `_IR_print_node_fields'. */

#define NODE_PRINT_NODE_PARAMETER_NAME \
   (cpp_flag ? "this" : INTERNAL_NODE_PRINT_NODE_PARAMETER_NAME)

/* The following macro value is name of the second parameter of
   function `IR_print_node' and the third parameter of
   `_IR_print_node_fields'. */

#define NODE_PRINT_FLAG_PARAMETER_NAME "class_field_flag"

/* The following function outputs title of function for printing node
   fields.  The most probable function output is following
        `void _IR_print_node_fields (IR_node_mode_t node_mode,
        IR_node_t node, int class_field_flag)'. */

static void
output_title_of_field_print_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_field_print_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, FIELD_PRINT_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, INTERNAL_NODE_PRINT_NODE_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f, NODE_PRINT_FLAG_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs code of printing given non-class or
   class field. */

static void
output_field_print (IR_node_t field, int class_field_flag)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && (IR_declaration_part (field) != DP_CLASS && !class_field_flag
          || IR_declaration_part (field) == DP_CLASS && class_field_flag))
    {
      output_string (output_implementation_file,
                     "      printf (\"      ");
      output_string (output_implementation_file,
                     IR_identifier_itself (IR_field_identifier (field)));
      output_string (output_implementation_file, ": \");\n");
      output_string (output_implementation_file, "      ");
      output_field_type_print_macro_name_prefix (output_implementation_file);
      output_field_type_name (output_implementation_file,
                              IR_field_type (field));
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           INTERNAL_NODE_PRINT_NODE_PARAMETER_NAME, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, ");\n");
      output_string (output_implementation_file,
                     "      fputc ('\\n', stdout);\n");
    }
}

/* The following function outputs code of printing given non-class
   field.  The function uses `output_field_print'.  The function is
   called from function `traverse_all_node_type_fields' or
   `output_node_type_fields_print'. */

static void
output_non_class_field_print (IR_node_t field)
{
  output_field_print (field, FALSE);
}

/* The following function outputs code of printing given class field.
   The function uses `output_field_print'.  The function is called
   from function `traverse_all_node_type_fields' or
   `output_node_type_fields_print'. */

static void
output_class_field_print (IR_node_t field)
{
  output_field_print (field, TRUE);
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for printing all
   fields of node of given type.  The function outputs the following
   fragment
            case <node mode value of given node type>:
              <code of printing all fields>
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_print (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        {
          traverse_all_node_type_fields (node_type,
                                         output_non_class_field_print);
          /* Output processing class fields */
          if (node_type_class_field_presence (node_type, TRUE))
            {
              output_string (output_implementation_file, "      if (!");
              output_string (output_implementation_file,
                             NODE_PRINT_FLAG_PARAMETER_NAME);
              output_string (output_implementation_file,
                             ")\n        break;\n");
              traverse_all_node_type_fields (node_type,
                                             output_class_field_print);
            }
        }
      else
        {
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      ");
                output_name_of_field_print_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   INTERNAL_NODE_PRINT_NODE_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               NODE_PRINT_FLAG_PARAMETER_NAME);
                output_string (output_implementation_file, ");\n");
              }
          if (IR_last_field (node_type) != NULL)
            {
              for (current_field = IR_next_field (IR_last_field (node_type));;
                   current_field = IR_next_field (current_field))
                {
                  assert (IR_NODE_MODE (current_field) == IR_NM_field
                          || IR_NODE_MODE (current_field) == IR_NM_action
                          || IR_NODE_MODE (current_field) == IR_NM_constraint);
                  output_non_class_field_print (current_field);
                  if (current_field == IR_last_field (node_type))
                    break;
                }
              /* Output processing class fields */
              if (node_type_class_field_presence (node_type, FALSE))
                {
                  output_string (output_implementation_file, "      if (!");
                  output_string (output_implementation_file,
                                 NODE_PRINT_FLAG_PARAMETER_NAME);
                  output_string (output_implementation_file,
                                 ")\n        return;\n");
                  for (current_field
                       = IR_next_field (IR_last_field (node_type));;
                       current_field = IR_next_field (current_field))
                    {
                      output_class_field_print (current_field);
                      if (current_field == IR_last_field (node_type))
                        break;
                    }
                }               
            }
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_print_node_fields'. */

static void
output_field_print_function (void)
{
  /* Output field print function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_print_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Print fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 FIELD_PRINT_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_print);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of function for printing node.
   The most probable function output is following
        `void IR_print_node (IR_node_t node, int class_field_flag)' or
        `void IR_print_node (int class_field_flag)' or
        `void IR_node::IR_print_node (int class_field_flag)'. */

static void
output_title_of_node_print_function (FILE *f, int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  output_string (f, "void ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_node_print_function (f);
  output_string (f, " (");
  if (!cpp_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, NODE_PRINT_NODE_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, NODE_PRINT_FLAG_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs definition of node fields print
   function (with the aid of `output_field_print_function') and
   external definition (C interface) and definition of function
   `IR_print_node'. */

static void
output_node_print_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_node_print_function (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Output field print function. */
  output_field_print_function ();
  /* Function itself */
  output_title_of_node_print_function (output_implementation_file, cpp_flag);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, NODE_PRINT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, " == NULL)\n    return;\n");
  /* Print node address */
  output_string (output_implementation_file, "  ");
  output_field_type_print_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, root_node_type);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, NODE_PRINT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Print node mode member.  Temporary and previous graph pass number
     members are not printed. */
  output_string (output_implementation_file, "  printf (\" %s :\\n\", ");
  output_node_name_array_name (output_implementation_file);
  output_string (output_implementation_file, " [");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_PRINT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_PRINT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")]);\n");
  /* Print fields of node type */
  output_string (output_implementation_file, "  ");
  output_name_of_field_print_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_PRINT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file, NODE_PRINT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file, NODE_PRINT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_PRINT_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following macro value is name of the first parameter of
   `_IR_output_node_fields'. */

#define FIELD_OUTPUT_NODE_MODE_PARAMETER_NAME  "node_mode"

/* The following macro value is name of the first parameter of
   function `IR_output_node' or the second parameter of
   `_IR_output_node_fields'. */

#define NODE_OUTPUT_FILE_PARAMETER_NAME "output_file"

/* The following macro is name of parameter of function
   `_IR_output_node_fields'. */

#define INTERNAL_NODE_OUTPUT_NODE_PARAMETER_NAME "node"

/* The following macro value is name of the second parameter (C
   interface) or the corresponding node (C++ interface) of function
   `IR_output_node'. */

#define NODE_OUTPUT_NODE_PARAMETER_NAME \
   (cpp_flag ? "this" : INTERNAL_NODE_OUTPUT_NODE_PARAMETER_NAME)

/* The following macro value is name of the third parameter of
   function `IR_output_node' or the fourth parameter of
   `_IR_output_node_fields'. */

#define NODE_OUTPUT_LEVEL_PARAMETER_NAME "level"

/* The following macro value is name of variable which will be result of
   function `IR_output_node' and `_IR_output_node_fields'. */

#define NODE_OUTPUT_RESULT_NAME "_result"

/* The following macro value is name of temporary variable only in c++
   regime in function `IR_output_node'. */

#define NODE_OUTPUT_TEMP_NAME "temp"

/* The following function outputs title of function for outputting
   node fields.  The most probable function output is following
        `int _IR_output_node_fields (IR_node_mode_t node_mode,
        FILE *output_file, IR_node_t node, int level)'. */

static void
output_title_of_field_output_function (FILE *f)
{
  output_string (f, "int ");
  output_field_output_name_of_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, FIELD_OUTPUT_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", FILE *");
  output_string (f, NODE_OUTPUT_FILE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, INTERNAL_NODE_OUTPUT_NODE_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f, NODE_OUTPUT_LEVEL_PARAMETER_NAME);
  output_char (')', f);
}

/* The following variable value is level of last output code for output of
   field.  If the value is negative no one such code was output. */

static int last_output_field_level;

/* The following function outputs code of outputting given non-class.
   The function is called from function
   `traverse_all_node_type_fields' or
   `output_node_type_fields_output'. */

static void
output_field_output (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS)
    {
      if (last_output_field_level != IR_declaration_level (field))
        {
          if (last_output_field_level >= 0)
            output_string (output_implementation_file, "        }\n");
          output_string (output_implementation_file, "      if (");
          output_string (output_implementation_file,
                         NODE_OUTPUT_LEVEL_PARAMETER_NAME);
          output_string (output_implementation_file, " >= ");
          output_decimal_number (output_implementation_file,
                                 IR_declaration_level (field));
          output_string (output_implementation_file, ")\n        {\n");
        }
      output_string (output_implementation_file, "          ");
      output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
      output_string (output_implementation_file, " = ");
      output_field_type_output_macro_name_prefix (output_implementation_file);
      output_field_type_name (output_implementation_file,
                              IR_field_type (field));
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file,
                     NODE_OUTPUT_FILE_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
      output_field_access (output_implementation_file,
                           INTERNAL_NODE_OUTPUT_NODE_PARAMETER_NAME, field,
                           FALSE, FALSE);
      output_string (output_implementation_file, ") || ");
      output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
      output_string (output_implementation_file, ";\n");
      last_output_field_level = IR_declaration_level (field);
    }
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for outputting
   all fields of node of given type.  The function outputs the
   following fragment
            case <node mode value of given node type>:
              <code of outputting all non class fields>
              break;
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_output (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      last_output_field_level = -1;
      if (flat_flag)
        traverse_all_node_type_fields (node_type, output_field_output);
      else
        {
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "          ");
                output_string (output_implementation_file,
                               NODE_OUTPUT_RESULT_NAME);
                output_string (output_implementation_file, " = ");
                output_field_output_name_of_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               NODE_OUTPUT_FILE_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   INTERNAL_NODE_OUTPUT_NODE_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               NODE_OUTPUT_LEVEL_PARAMETER_NAME);
                output_string (output_implementation_file, ") || ");
                output_string (output_implementation_file,
                               NODE_OUTPUT_RESULT_NAME);
                output_string (output_implementation_file, ";\n");
              }
          if (IR_last_field (node_type) != NULL)
            for (current_field = IR_next_field (IR_last_field (node_type));;
                 current_field = IR_next_field (current_field))
              {
                assert (IR_NODE_MODE (current_field) == IR_NM_field
                        || IR_NODE_MODE (current_field) == IR_NM_action
                        || IR_NODE_MODE (current_field) == IR_NM_constraint);
                output_field_output (current_field);
                if (current_field == IR_last_field (node_type))
                  break;
              }
        }
      if (last_output_field_level >= 0)
        output_string (output_implementation_file, "        }\n");
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_output_node_fields'. */

static void
output_field_output_function (void)
{
  /* Output field output function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_output_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  int ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n\n");
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, " = 0;\n");
  /* Output fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 FIELD_OUTPUT_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_output);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n}\n\n");
}

/* The following function outputs title of function for outputting
   node.  The most probable function output is following
        `int IR_output_node (FILE *output_file, IR_node_t node,
                             int level)' or
        `int IR_output_node (FILE *output_file, int level)' or
        `int IR_node::IR_output_node (FILE *output_file, int level)'. */

static void
output_title_of_node_output_function (FILE *f, int class_prefix_flag)
{
  assert (!class_prefix_flag || cpp_flag);
  output_string (f, "int ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_node_output_name_of_function (f);
  output_string (f, " (FILE *");
  output_string (f, NODE_OUTPUT_FILE_PARAMETER_NAME);
  if (!cpp_flag)
    {
      output_string (f, ", ");
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f, NODE_OUTPUT_NODE_PARAMETER_NAME);
    }
  output_string (f, ", int ");
  output_string (f, NODE_OUTPUT_LEVEL_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs definition of node fields output
   function (with the aid of `output_field_output_function') and
   external definition (C interface) and definition of function
   `IR_output_node'. */

static void
output_node_output_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_node_output_function (output_interface_file, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Output field output function. */
  output_field_output_function ();
  /* Function itself */
  output_title_of_node_output_function (output_implementation_file, cpp_flag);
  output_string (output_implementation_file, "\n{\n  int ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n");
  if (cpp_flag)
    {
      output_string (output_implementation_file, "  ");
      output_node_t_name (output_implementation_file);
      output_string (output_implementation_file, " ");
      output_string (output_implementation_file, NODE_OUTPUT_TEMP_NAME);
      output_string (output_implementation_file, ";\n");
    }
  output_string (output_implementation_file, "\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, NODE_OUTPUT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, " == NULL)\n    return 0;\n");
  output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
  output_string (output_implementation_file, "  if (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_level_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   NODE_OUTPUT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, ") > ");
  output_string (output_implementation_file, NODE_OUTPUT_LEVEL_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    abort ();\n");
  output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
  /* Output level parameter */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, " = fwrite (&");
  output_string (output_implementation_file, NODE_OUTPUT_LEVEL_PARAMETER_NAME);
  output_string (output_implementation_file, ", sizeof (int), 1, ");
  output_string (output_implementation_file, NODE_OUTPUT_FILE_PARAMETER_NAME);
  output_string (output_implementation_file, ") != sizeof (int);\n");
  /* Output node address */
  if (cpp_flag)
    {
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file, NODE_OUTPUT_TEMP_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, ";\n");
    }
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, " = ");
  output_field_type_output_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, root_node_type);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, NODE_OUTPUT_FILE_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string
    (output_implementation_file,
     cpp_flag ? NODE_OUTPUT_TEMP_NAME : NODE_OUTPUT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, ") || ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n");
  /* Output node mode member.  Temporary and previous graph pass
     number members are not output. */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, " = fwrite (&");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_node_mode_member_name (output_implementation_file);
    }
  else
    {
      output_node_mode_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, ")");
    }
  output_string (output_implementation_file, ", sizeof (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_node_mode_member_name (output_implementation_file);
    }
  else
    {
      output_node_mode_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, ")");
    }
  output_string (output_implementation_file, "), 1, ");
  output_string (output_implementation_file, NODE_OUTPUT_FILE_PARAMETER_NAME);
  output_string (output_implementation_file, ") != sizeof (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
      output_node_mode_member_name (output_implementation_file);
    }
  else
    {
      output_node_mode_name (output_implementation_file);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, ")");
    }
  output_string (output_implementation_file, ") || ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n");
  /* Output fields of node type */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, " = ");
  output_field_output_name_of_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     NODE_OUTPUT_NODE_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   NODE_OUTPUT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file, NODE_OUTPUT_FILE_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_OUTPUT_NODE_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_OUTPUT_LEVEL_PARAMETER_NAME);
  output_string (output_implementation_file, ") || ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n");
  /* Function epilogue */
  output_string (output_implementation_file, " return ");
  output_string (output_implementation_file, NODE_OUTPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n}\n\n");
}


/* The following macro value is name of the first parameter of
   `_IR_input_node_fields'. */

#define FIELD_INPUT_NODE_MODE_PARAMETER_NAME  "node_mode"

/* The following macro value is name of the first parameter of
   function `IR_input_node' and the second parameter of
   `_IR_input_node_fields'. */

#define NODE_INPUT_FILE_PARAMETER_NAME "input_file"

/* The following macro value is name of the second parameter of
   function `IR_input_node'. */

#define NODE_INPUT_ADDRESS_PARAMETER_NAME "original_address"

/* The following macro value is name of variable which will be result
   of function `IR_input_node' and third parameter of
   `_IR_input_node_fields'. */

#define NODE_INPUT_RESULT_NAME "_result"

/* The following macro value is name of variable which used in
   function `IR_input_node' for storing input node level and of the
   fourth parameter of `_IR_input_node_fields'. */

#define NODE_INPUT_LEVEL_NAME "level"

/* The following macro value is name of variable which used in
   function `IR_input_node' for storing input node mode. */

#define NODE_INPUT_MODE_NAME "node_mode"

/* The following function outputs title of function for inputing node
   fields.  The most probable function output is following
        `[static ]int _IR_input_node_fields
        (IR_node_mode_t node_mode, FILE *input_file,
        IR_node_t result, int level)'. */

static void
output_title_of_field_input_function (FILE *f)
{
  if (!cpp_flag)
    output_string (f, "static ");
  output_string (f, "int ");
  output_name_of_field_input_function (f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, FIELD_INPUT_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", FILE *");
  output_string (f, NODE_INPUT_FILE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, NODE_INPUT_RESULT_NAME);
  output_string (f, ", int ");
  output_string (f, NODE_INPUT_LEVEL_NAME);
  output_char (')', f);
}

/* The following function outputs code of inputing given non-class.
   The function is called from function
   `traverse_all_node_type_fields' or
   `output_node_type_fields_input'. */

static void
output_field_input (IR_node_t field)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && IR_declaration_part (field) != DP_CLASS)
    {
      output_string (output_implementation_file, "      if (");
      if (IR_declaration_level (field) != 0)
        {
          output_string (output_implementation_file, NODE_INPUT_LEVEL_NAME);
          output_string (output_implementation_file, " >= ");
          output_decimal_number (output_implementation_file,
                                 IR_declaration_level (field));
          output_string (output_implementation_file, " && ");
        }
      output_field_type_input_macro_name_prefix (output_implementation_file);
      output_field_type_name (output_implementation_file,
                              IR_field_type (field));
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file,
                     NODE_INPUT_FILE_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
      output_field_access (output_implementation_file,
                           NODE_INPUT_RESULT_NAME, field, FALSE, FALSE);
      output_string (output_implementation_file, "))\n        return 1;\n");
    }
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for inputing all
   fields of node of given type.  The function outputs the following
   fragment
            case <node mode value of given node type>:
              <code of inputing all non class fields>
              break;
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_input (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        traverse_all_node_type_fields (node_type, output_field_input);
      else
        {
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      if (");
                output_name_of_field_input_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               NODE_INPUT_FILE_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   NODE_INPUT_RESULT_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               NODE_INPUT_LEVEL_NAME);
                output_string (output_implementation_file,
                               "))\n        return 1;\n");
              }
          if (IR_last_field (node_type) != NULL)
            for (current_field = IR_next_field (IR_last_field (node_type));;
                 current_field = IR_next_field (current_field))
              {
                assert (IR_NODE_MODE (current_field) == IR_NM_field
                        || IR_NODE_MODE (current_field) == IR_NM_action
                        || IR_NODE_MODE (current_field) == IR_NM_constraint);
                output_field_input (current_field);
                if (current_field == IR_last_field (node_type))
                  break;
              }
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_input_node_fields'. */

static void
output_field_input_function (void)
{
  /* Output field input function. */
  output_title_of_field_input_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Input fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 FIELD_INPUT_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_input);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return 0;\n}\n\n");
}

/* The following function outputs title of function for inputing node.
   The most probable function output is following
        `IR_node_t IR_input_node (FILE *input_file,
                                  IR_node_t *original address)'. */

static void
output_title_of_node_input_function (FILE *f)
{
  output_node_t_name (f);
  output_char (' ', f);
  output_name_of_node_input_function (f);
  output_string (f, " (FILE *");
  output_string (f, NODE_INPUT_FILE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_string (f, " *");
  output_string (f, NODE_INPUT_ADDRESS_PARAMETER_NAME);
  output_char (')', f);
}

/* The following function outputs definition of node fields input
   function (with the aid of `output_field_input_function') and
   external definition (C interface) and definition of function
   `IR_input_node'. */

static void
output_node_input_function (void)
{
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_node_input_function (output_interface_file);
      output_string (output_interface_file, ";\n\n");
    }
  /* Output field input function. */
  output_field_input_function ();
  /* Function itself */
  output_title_of_node_input_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n  ");
  output_node_t_name (output_implementation_file);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, NODE_INPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  ");
  output_node_mode_t_name (output_implementation_file);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, NODE_INPUT_MODE_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  int ");
  output_string (output_implementation_file, NODE_INPUT_LEVEL_NAME);
  output_string (output_implementation_file, ";\n\n");
  /* Input level parameter */
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, "fread (&");
  output_string (output_implementation_file, NODE_INPUT_LEVEL_NAME);
  output_string (output_implementation_file, ", sizeof (int), 1, ");
  output_string (output_implementation_file, NODE_INPUT_FILE_PARAMETER_NAME);
  output_string (output_implementation_file,
                 ") != sizeof (int))\n    return NULL;\n");
  output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
  output_string (output_implementation_file, "  if (");
  output_decimal_number (output_implementation_file,
                         IR_description_part_level (current_description_part));
  output_string (output_implementation_file, " < ");
  output_string (output_implementation_file, NODE_INPUT_LEVEL_NAME);
  output_string (output_implementation_file, ")\n      abort ();\n");
  output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
  /* Input node address */
  output_string (output_implementation_file, "  if (");
  output_field_type_input_macro_name_prefix (output_implementation_file);
  output_field_type_name (output_implementation_file, root_node_type);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, NODE_INPUT_FILE_PARAMETER_NAME);
  output_string (output_implementation_file, ", *");
  output_string (output_implementation_file,
                 NODE_INPUT_ADDRESS_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n    return NULL;\n");
  /* Input node mode member.  Temporary and previous graph pass number
     members are not input. */
  output_string (output_implementation_file, "  if (fread (&");
  output_string (output_implementation_file, NODE_INPUT_MODE_NAME);
  output_string (output_implementation_file, ", sizeof (");
  output_string (output_implementation_file, NODE_INPUT_MODE_NAME);
  output_string (output_implementation_file, "), 1, ");
  output_string (output_implementation_file, NODE_INPUT_FILE_PARAMETER_NAME);
  output_string (output_implementation_file, ") != sizeof (");
  output_string (output_implementation_file, NODE_INPUT_MODE_NAME);
  output_string (output_implementation_file, "))\n    return NULL;\n");
  /* Create node */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, NODE_INPUT_RESULT_NAME);
  output_string (output_implementation_file, " = ");
  output_name_of_creation_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, NODE_INPUT_MODE_NAME);
  output_string (output_implementation_file, ");\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, NODE_INPUT_RESULT_NAME);
  output_string (output_implementation_file, " == NULL)\n    return NULL;\n");
  /* Input fields of node type */
  output_string (output_implementation_file, "  if (");
  output_name_of_field_input_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, NODE_INPUT_MODE_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_INPUT_FILE_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_INPUT_RESULT_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, NODE_INPUT_LEVEL_NAME);
  output_string (output_implementation_file, "))\n    return NULL;\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file, NODE_INPUT_RESULT_NAME);
  output_string (output_implementation_file, ";\n}\n\n");
}

/* The following function outputs function of printing node (with the
   aid of function `output_node_print_function'), function of
   outputting node (with the aid of function
   `output_node_output_function'), and function of inputing node (with
   the aid of function `output_node_input_function'). */

static void
output_io_functions (void)
{
  if (print_flag || check_flag || check_graph_flag)
    output_node_print_function ();
  if (output_flag)
    output_node_output_function ();
  if (input_flag)
    output_node_input_function ();
}



/* This page contains functions for output of functions of traversing
   graph. */

/* The following macro value is name of the first parameter of
   function `_IR_node_field_traverse_depth_first' and
   `_IR_node_field_traverse_reverse_depth_first'. */

#define TRAVERSE_NODE_MODE_PARAMETER_NAME "node_mode"

/* The following macro is name of parameter of functions
   _IR_traverse_depth_first' and `_IR_traverse_reverse_depth_first',
   and `_IR_node_field_traverse_depth_first'
   and `_IR_node_field_traverse_reverse_depth_first'. */

#define INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME "graph"

/* The following macro value is name of the first parameter (C
   interface) or the corresponding node (C++ interface) of functions
   `IR_traverse_depth_first' and `IR_traverse_reverse_depth_first' and
   `_IR_traverse_depth_first' and `_IR_traverse_reverse_depth_first',
   and the second parameter (C interface) or the corresponding node
   (C++ interface) of `_IR_node_field_traverse_depth_first' and
   `_IR_node_field_traverse_reverse_depth_first'. */

#define TRAVERSE_GRAPH_PARAMETER_NAME \
   (cpp_flag ? "this" : INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME)

/* The following macro value is name of the second parameter of functions
   `IR_traverse_depth_first' and `IR_traverse_reverse_depth_first' and
   `_IR_traverse_depth_first' and `_IR_traverse_reverse_depth_first',
   and the third parameter of `_IR_node_field_traverse_depth_first' and
   `_IR_node_field_traverse_reverse_depth_first'. */

#define TRAVERSE_FLAG_PARAMETER_NAME "class_field_flag"

/* The following macro value is name of the third parameter of functions
   `IR_traverse_depth_first' and `IR_traverse_reverse_depth_first' and
   `_IR_traverse_depth_first' and `_IR_traverse_reverse_depth_first',
   and the fourth parameter of `_IR_node_field_traverse_depth_first' and
   `_IR_node_field_traverse_reverse_depth_first'.*/

#define TRAVERSE_FUNCTION_PARAMETER_NAME "function"

/* The following function outputs title of functions for traversing
   node fields.  The most probable function output is following
        `void _IR_node_field_traverse_depth_first
        (IR_node_t graph,
        int class_field_flag, void (*function) (IR_node_t node))'.
        or
        `void _IR_node_field_traverse_reverse_depth_first
        (IR_node_t graph,
         int class_field_flag, int (*function) (IR_node_t node))'. */

static void
output_title_of_field_traverse_function (FILE *f, int reverse_flag)
{
  output_string (f, "void ");
  output_name_of_field_traverse_function (f, reverse_flag);
  output_string (f, "\n  (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, TRAVERSE_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f, TRAVERSE_FLAG_PARAMETER_NAME);
  output_string (f, ", ");
  if (reverse_flag)
    output_string (f, "int (*");
  else
    output_string (f, "void (*");
  output_string (f, TRAVERSE_FUNCTION_PARAMETER_NAME);
  output_string (f, ") (");
  output_node_t_name (f);
  output_string (f, " node))");
}


/* The following variable value is TRUE iff generation of function
   `IR_traverse_reverse_depth_first' is made. */

static int reverse_traverse_generation_flag;

/* The following function outputs code of traversing graph whose value
   is value of given non-class or class field. */

static void
output_field_traverse (IR_node_t field, int class_field_flag)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && (IR_declaration_part (field) != DP_CLASS && !class_field_flag
          || IR_declaration_part (field) == DP_CLASS && class_field_flag)
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
    {
      output_string (output_implementation_file, "      ");
      output_name_of_traverse_function (output_implementation_file,
                                        reverse_traverse_generation_flag,
                                        TRUE);
      output_string (output_implementation_file, " (");
      output_field_access (output_implementation_file,
                           INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME,
                           field, FALSE, FALSE);
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file, TRAVERSE_FLAG_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file,
                     TRAVERSE_FUNCTION_PARAMETER_NAME);
      output_string (output_implementation_file, ");\n");
    }
}

/* The following function outputs code of traversing given non-class
   field.  The function uses `output_field_traverse'.  The function is
   called from function `traverse_all_node_type_fields' or
   `output_node_type_fields_traverse'. */

static void
output_non_class_field_traverse (IR_node_t field)
{
  output_field_traverse (field, FALSE);
}

/* The following function outputs code of traversing given class
   field.  The function uses `output_field_traverse'.  The function is
   called from function `traverse_all_node_type_fields' or
   `output_node_type_fields_traverse'. */

static void
output_class_field_traverse (IR_node_t field)
{
  output_field_traverse (field, TRUE);
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for traversing
   all graphs whose values are value of fields of node of given type.
   The function outputs the following fragment
            case <node mode value of given node type>:
              <code of traversing all fields>
              break;
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_traverse (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        {
          traverse_all_node_type_fields (node_type,
                                         output_non_class_field_traverse);
          /* Output processing class fields */
          if (node_type_class_field_presence (node_type, TRUE))
            {
              output_string (output_implementation_file, "      if (!");
              output_string (output_implementation_file,
                             TRAVERSE_FLAG_PARAMETER_NAME);
              output_string (output_implementation_file,
                             ")\n        break;\n");
              traverse_all_node_type_fields (node_type,
                                             output_class_field_traverse);
            }
        }
      else
        {
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      ");
                output_name_of_field_traverse_function
                  (output_implementation_file,
                   reverse_traverse_generation_flag);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself (IR_type_identifier
                                         (IR_immediate_super_type
                                          (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               TRAVERSE_FLAG_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               TRAVERSE_FUNCTION_PARAMETER_NAME);
                output_string (output_implementation_file, ");\n");
              }
          if (IR_last_field (node_type) != NULL)
            {
              for (current_field = IR_next_field (IR_last_field (node_type));;
                   current_field = IR_next_field (current_field))
                {
                  assert (IR_NODE_MODE (current_field) == IR_NM_field
                          || IR_NODE_MODE (current_field) == IR_NM_action
                          || IR_NODE_MODE (current_field) == IR_NM_constraint);
                  output_non_class_field_traverse (current_field);
                  if (current_field == IR_last_field (node_type))
                    break;
                }
              /* Output processing class fields */
              if (node_type_class_field_presence (node_type, FALSE))
                {
                  output_string (output_implementation_file, "      if (!");
                  output_string (output_implementation_file,
                                 TRAVERSE_FLAG_PARAMETER_NAME);
                  output_string (output_implementation_file,
                                 ")\n        break;\n");
                  for (current_field
                       = IR_next_field (IR_last_field (node_type));;
                       current_field = IR_next_field (current_field))
                    {
                      output_class_field_traverse (current_field);
                      if (current_field == IR_last_field (node_type))
                        break;
                    }
                }               
            }
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_node_field_traverse_depth_first' or
   `_IR_node_field_traverse_reverse_depth_first'. */

static void
output_field_traverse_function (void)
{
  /* Output node field traversing function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_traverse_function (output_implementation_file,
                                           reverse_traverse_generation_flag);
  output_string (output_implementation_file, "\n{\n");
  /* Traverse fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 TRAVERSE_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_traverse);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of (internal or external)
   function for traversing graph.  The most probable function output
   is following
        `static void _IR_traverse_depth_first (IR_node_t graph,
         int class_field_flag, void (*function) (IR_node_t node))'.
        or
        `static void _IR_traverse_reverse_depth_first (IR_node_t graph,
         int class_field_flag, int (*function) (IR_node_t node))'.
        or
        `void IR_traverse_depth_first (IR_node_t graph,
         int class_field_flag, void (*function) (IR_node_t node))'.
        or
        `void IR_traverse_reverse_depth_first (IR_node_t graph,
         int class_field_flag, int (*function) (IR_node_t node))'.
        or
        `void _IR_traverse_depth_first (IR_node_t graph,
         int class_field_flag, void (*function) (IR_node_t node))'.
        or
        `void _IR_traverse_reverse_depth_first (IR_node_t graph,
         int class_field_flag, int (*function) (IR_node_t node))'.
        or
        `void IR_traverse_depth_first (
         int class_field_flag, void (*function) (IR_node_t node))'.
        or
        `void IR_traverse_reverse_depth_first (
         int class_field_flag, int (*function) (IR_node_t node))'.
        or
        `void IR_node::IR_traverse_depth_first (
         int class_field_flag, void (*function) (IR_node_t node))'.
        or
        `void IR_node::IR_traverse_reverse_depth_first (
         int class_field_flag, int (*function) (IR_node_t node))'. */

static void
output_title_of_traverse_function (FILE *f, int reverse_flag,
                                   int internal_function_flag,
                                   int class_prefix_flag)
{
  assert (cpp_flag || !class_prefix_flag);
  assert (!class_prefix_flag || !internal_function_flag);
  if (internal_function_flag && !cpp_flag)
    output_string (f, "static ");
  output_string (f, "void ");
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_traverse_function (f, reverse_flag, internal_function_flag);
  output_string (f, "\n  (");
  if (!cpp_flag || internal_function_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f,
                     internal_function_flag
                     ? INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME
                     : TRAVERSE_GRAPH_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, TRAVERSE_FLAG_PARAMETER_NAME);
  output_string (f, ", ");
  if (reverse_flag)
    output_string (f, "int (*");
  else
    output_string (f, "void (*");
  output_string (f, TRAVERSE_FUNCTION_PARAMETER_NAME);
  output_string (f, ") (");
  output_node_t_name (f);
  output_string (f, " node))");
}

/* The following function outputs forward definition of node field
   traversing function (with the aid of
   `output_field_traverse_function') and internal traverse
   function. */

static void
output_internal_traverse_function (int reverse_flag)
{
  /* Forward declaration of internal traversing function because it
     is used in field traversing function. */
  output_title_of_traverse_function (output_implementation_file,
                                     reverse_flag, TRUE, FALSE);
  output_string (output_implementation_file, ";\n\n");
  /* Output field traversing function. */
  output_field_traverse_function ();
  /* Internal traversing function. */
  output_title_of_traverse_function (output_implementation_file,
                                     reverse_flag, TRUE, FALSE);
  output_string (output_implementation_file, "\n{\n");
  /* Test graph cycle */
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file,
                 INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file,
                 " == NULL\n      || ");
  output_string (output_implementation_file,
                 INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  assert (graph_pass_number_member_flag);
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n    return;\n");
  /* Set up previous graph pass number member */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  /* Process node itself */
  if (reverse_flag)
    {
      output_string (output_implementation_file, "  if ((*");
      output_string (output_implementation_file,
                     TRAVERSE_FUNCTION_PARAMETER_NAME);
      output_string (output_implementation_file, ") (");
      output_string (output_implementation_file,
                     INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
      output_string (output_implementation_file, "))\n");
    }
  /* Traverse fields of node type */
  output_string (output_implementation_file, "  ");
  output_name_of_field_traverse_function (output_implementation_file,
                                          reverse_flag);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file,
                 INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, TRAVERSE_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, TRAVERSE_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Process node itself */
  if (!reverse_flag)
    {
      output_string (output_implementation_file, "  (*");
      output_string (output_implementation_file,
                     TRAVERSE_FUNCTION_PARAMETER_NAME);
      output_string (output_implementation_file, ") (");
      output_string (output_implementation_file,
                     INTERNAL_TRAVERSE_GRAPH_PARAMETER_NAME);
      output_string (output_implementation_file, ");\n");
    }
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs external definition (C interface)
   and definition of a traverse function. */

static void
output_traverse_function (int reverse_flag)
{
  reverse_traverse_generation_flag = reverse_flag;
  output_internal_traverse_function (reverse_flag);
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_traverse_function (output_interface_file, reverse_flag,
                                         FALSE, FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_traverse_function (output_implementation_file,
                                     reverse_flag, FALSE, cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n  ");
  output_name_of_traverse_function (output_implementation_file,
                                    reverse_flag, TRUE);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, TRAVERSE_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, TRAVERSE_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, TRAVERSE_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}



/* This page contains functions for output of function of transforming
   graph. */

/* The following macro value is name of the first parameter of
   function `_IR_node_field_transformation'. */

#define TRANSFORM_NODE_MODE_PARAMETER_NAME "node_mode"

/* The following macro is name of parameter of functions
   `_IR_transform_dag' and `_IR_node_field_transformation'. */

#define INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME "graph"

/* The following macro value is name of the first parameter of
   functions `IR_transform_dag', `_IR_transform_dag', and the second
   parameter of `_IR_node_field_transformation'. */

#define TRANSFORM_GRAPH_PARAMETER_NAME \
   (cpp_flag ? "this" : INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME)

/* The following macro value is name of the second parameter (C
   interface) or the corresponding node (C++ interface) of functions
   `IR_transform_dag', `_IR_transform_dag', and the third parameter (C
   interface) or the corresponding node (C++ interface) of
   `_IR_node_field_transformation'. */

#define TRANSFORM_FLAG_PARAMETER_NAME "class_field_flag"

/* The following macro value is name of the third parameter of
   functions `IR_transform_dag', `_IR_transform_dag', and the fourth
   parameter of `_IR_node_field_transformation'. */

#define TRANSFORM_GUARD_FUNCTION_PARAMETER_NAME "guard_function"

/* The following macro value is name of the fourth parameter of
   functions `IR_transform_dag', `_IR_transform_dag', and the fifth
   parameter of `_IR_node_field_transformation'. */

#define TRANSFORM_TRANSFORMATION_FUNCTION_PARAMETER_NAME\
   "transformation_function"

/* The following function outputs title of function for transformation
   node fields.  The most probable function output is following
        `void _IR_node_field_transformation
        (IR_node_mode_t node_mode, IR_node_t node,
         int class_field_flag, int (*guard_function) (IR_node_t node),
         IR_node_t (*transformation_function) (IR_node_t node))'. */

static void
output_title_of_field_transformation_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_field_transformation_function (f);
  output_string (f, "\n(");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, TRANSFORM_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_t_name (f);
  output_char (' ', f);
  output_string (f, INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (f, ", int ");
  output_string (f, TRANSFORM_FLAG_PARAMETER_NAME);
  output_string (f, ", ");
  output_string (f, "int (*");
  output_string (f, TRANSFORM_GUARD_FUNCTION_PARAMETER_NAME);
  output_string (f, ") (");
  output_node_t_name (f);
  output_string (f, " node),\n ");
  output_node_t_name (f);
  output_string (f, " (*");
  output_string (f, TRANSFORM_TRANSFORMATION_FUNCTION_PARAMETER_NAME);
  output_string (f, ") (");
  output_node_t_name (f);
  output_string (f, " node))");
}

/* The following function outputs code of transforming graph whose
   value is value of given non-class or class field. */

static void
output_field_transformation (IR_node_t field, int class_field_flag)
{
  if (IR_NODE_MODE (field) == IR_NM_field
      && (IR_declaration_part (field) != DP_CLASS && !class_field_flag
          || IR_declaration_part (field) == DP_CLASS && class_field_flag)
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
    {
      if (IR_double_field_flag (field))
        {
          output_start_of_setting_double_link_field_value
            (output_implementation_file, "      ",
             INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME, field, FALSE, FALSE);
          output_string (output_implementation_file, "\n         ");
        }
      else
        {
          output_string (output_implementation_file, "      ");
          output_field_access (output_implementation_file,
                               INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME,
                               field, FALSE, FALSE);
          output_string (output_implementation_file, "\n        = ");
        }
      output_name_of_transformation_function (output_implementation_file,
                                              TRUE);
      output_string (output_implementation_file, " (");
      output_field_access
        (output_implementation_file,
         INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME, field, FALSE, FALSE);
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file,
                     TRANSFORM_FLAG_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file,
                     TRANSFORM_GUARD_FUNCTION_PARAMETER_NAME);
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file,
                     TRANSFORM_TRANSFORMATION_FUNCTION_PARAMETER_NAME);
      output_string (output_implementation_file, ")");
      if (IR_double_field_flag (field))
        output_finish_of_setting_double_link_field_value
          (output_implementation_file, "      ", field, FALSE, FALSE);
      else
        output_string (output_implementation_file, ";\n");
    }
}

/* The following function outputs code of transforming given non-class
   field.  The function uses `output_field_transformation'.  The
   function is called from function `traverse_all_node_type_fields' or
   `output_node_type_fields_transformation'. */

static void
output_non_class_field_transformation (IR_node_t field)
{
  output_field_transformation (field, FALSE);
}

/* The following function outputs code of transforming given class
   field.  The function uses `output_field_transformation'.  The
   function is called from function `traverse_all_node_type_fields' or
   `output_node_type_fields_transformation'. */

static void
output_class_field_transformation (IR_node_t field)
{
  output_field_transformation (field, TRUE);
}

/* If given node type is not abstract node type or non-flat model is
   used the function outputs fragment of code needed for transforming
   all graphs whose values are value of fields of node of given type.
   The function outputs the following fragment
            case <node mode value of given node type>:
              <code of transforming all fields>
              break;
   The function is called from function `traverse_node_types'. */

static void
output_node_type_fields_transformation (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        {
          traverse_all_node_type_fields
            (node_type, output_non_class_field_transformation);
          /* Output processing class fields */
          if (node_type_class_field_presence (node_type, TRUE))
            {
              output_string (output_implementation_file, "      if (!");
              output_string (output_implementation_file,
                             TRANSFORM_FLAG_PARAMETER_NAME);
              output_string (output_implementation_file,
                             ")\n        break;\n");
              traverse_all_node_type_fields
                (node_type, output_class_field_transformation);
            }
        }
      else
        {
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type
                && (IR_node_structure_has_been_output
                    (IR_immediate_super_type (curr_supertype_list_element))))
              {
                output_string (output_implementation_file, "      ");
                output_name_of_field_transformation_function
                  (output_implementation_file);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself
                   (IR_type_identifier
                    (IR_immediate_super_type (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_immediate_super_type_node_address
                  (output_implementation_file, node_type,
                   IR_immediate_super_type (curr_supertype_list_element),
                   INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               TRANSFORM_FLAG_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               TRANSFORM_GUARD_FUNCTION_PARAMETER_NAME);
                output_string (output_implementation_file, ", ");
                output_string
                  (output_implementation_file,
                   TRANSFORM_TRANSFORMATION_FUNCTION_PARAMETER_NAME);
                output_string (output_implementation_file, ");\n");
              }
          if (IR_last_field (node_type) != NULL)
            {
              for (current_field = IR_next_field (IR_last_field (node_type));;
                   current_field = IR_next_field (current_field))
                {
                  assert (IR_NODE_MODE (current_field) == IR_NM_field
                          || IR_NODE_MODE (current_field) == IR_NM_action
                          || IR_NODE_MODE (current_field) == IR_NM_constraint);
                  output_non_class_field_transformation (current_field);
                  if (current_field == IR_last_field (node_type))
                    break;
                }
              /* Output processing class fields */
              if (node_type_class_field_presence (node_type, FALSE))
                {
                  output_string (output_implementation_file, "      if (!");
                  output_string (output_implementation_file,
                                 TRANSFORM_FLAG_PARAMETER_NAME);
                  output_string (output_implementation_file,
                                 ")\n        break;\n");
                  for (current_field
                       = IR_next_field (IR_last_field (node_type));;
                       current_field = IR_next_field (current_field))
                    {
                      output_class_field_transformation (current_field);
                      if (current_field == IR_last_field (node_type))
                        break;
                    }
                }               
            }
        }
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_node_field_transformation'. */

static void
output_field_transformation_function (void)
{
  /* Output field transformation function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_field_transformation_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  /* Transform fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 TRANSFORM_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types (output_node_type_fields_transformation);
  output_string (output_implementation_file,
                 "    default:\n      abort ();\n      break;\n    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of (internal or external)
   function for transforming graph.  The most probable function output
   is following
        `[static ]IR_node_t _IR_transform_dag (IR_node_t graph,
         int class_field_flag, int (*guard_function) (IR_node_t node),
         IR_node_t (*transformation_function) (IR_node_t node))'.
        or
        `IR_node_t IR_transform_dag (IR_node_t graph,
         int class_field_flag, int (*guard_function) (IR_node_t node),
         IR_node_t (*transformation_function) (IR_node_t node))'
        or
        `IR_node_t IR_transform_dag (
         int class_field_flag, int (*guard_function) (IR_node_t node),
         IR_node_t (*transformation_function) (IR_node_t node))'
        or
        `IR_node_t IR_node::IR_transform_dag (
         int class_field_flag, int (*guard_function) (IR_node_t node),
         IR_node_t (*transformation_function) (IR_node_t node))'. */

static void
output_title_of_transformation_function (FILE *f, int internal_function_flag,
                                         int class_prefix_flag)
{
  assert (cpp_flag || !class_prefix_flag);
  assert (!class_prefix_flag || !internal_function_flag);
  if (internal_function_flag && !cpp_flag)
    output_string (f, "static ");
  output_node_t_name (f);
  output_char (' ', f);
  if (class_prefix_flag)
    {
      output_root_struct_class_name (f);
      output_string (f, "::");
    }
  output_name_of_transformation_function (f, internal_function_flag);
  output_string (f, "\n(");
  if (!cpp_flag || internal_function_flag)
    {
      output_node_t_name (f);
      output_char (' ', f);
      output_string (f,
                     internal_function_flag
                     ? INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME
                     : TRANSFORM_GRAPH_PARAMETER_NAME);
      output_string (f, ", ");
    }
  output_string (f, "int ");
  output_string (f, TRANSFORM_FLAG_PARAMETER_NAME);
  output_string (f, ", ");
  output_string (f, "int (*");
  output_string (f, TRANSFORM_GUARD_FUNCTION_PARAMETER_NAME);
  output_string (f, ") (");
  output_node_t_name (f);
  output_string (f, " node),\n ");
  output_node_t_name (f);
  output_string (f, " (*");
  output_string (f, TRANSFORM_TRANSFORMATION_FUNCTION_PARAMETER_NAME);
  output_string (f, ") (");
  output_node_t_name (f);
  output_string (f, " node))");
}

/* The following function outputs forward definitions of node field
   transformation function (with the aid of
   `output_field_transformation_function') and internal transformation
   function. */

static void
output_internal_transformation_function (void)
{
  /* Forward declaration of internal transformation function because it
     is used in field transformation function. */
  output_title_of_transformation_function (output_implementation_file, TRUE,
                                           FALSE);
  output_string (output_implementation_file, ";\n\n");
  /* Output field transformation function. */
  output_field_transformation_function ();
  /* Internal transformation function. */
  output_title_of_transformation_function (output_implementation_file, TRUE,
                                           FALSE);
  output_string (output_implementation_file, "\n{\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file,
                 " == NULL || !(*");
  output_string (output_implementation_file,
                 TRANSFORM_GUARD_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ") (");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "))\n");
  output_string (output_implementation_file, "    return ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "  else if (");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  assert (graph_pass_number_member_flag);
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n    {\n");
  output_string (output_implementation_file, "      if (");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, " == NULL)\n");
  output_string (output_implementation_file,
                 "        /* Graph cycle is occured. */\n");
  output_string (output_implementation_file, "        ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "      return ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, ";\n    }\n");
  /* Set temporary member for recognizing graph cycles. */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, " = NULL;\n");
  /* Set up previous graph pass number member */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_last_graph_pass_number_member_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  /* Output of field transformation function call. */
  output_string (output_implementation_file, "  ");
  output_name_of_field_transformation_function (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (cpp_flag)
    {
      output_string (output_implementation_file,
                     INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
      output_string (output_implementation_file, "->");
    }
  output_node_mode_name (output_implementation_file);
  output_string (output_implementation_file, " (");
  if (!cpp_flag)
    output_string (output_implementation_file,
                   INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "), ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, TRANSFORM_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file,
                 TRANSFORM_GUARD_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file,
                 TRANSFORM_TRANSFORMATION_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Process node itself */
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, " == NULL)\n    {\n");
  output_string (output_implementation_file, "      /* No cycle. */\n");
  output_string (output_implementation_file, "      ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, " = (*");
  output_string (output_implementation_file,
                 TRANSFORM_TRANSFORMATION_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ") (");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  output_string (output_implementation_file, "      ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, "->");
  output_temporary_member_name (output_implementation_file);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file,
                 INTERNAL_TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs external definition (C interface)
   and definition of transformation function. */

static void
output_transformation_function (void)
{
  output_internal_transformation_function ();
  /* Extern definition */
  if (!cpp_flag)
    {
      output_string (output_interface_file, "extern ");
      output_title_of_transformation_function (output_interface_file, FALSE,
                                               FALSE);
      output_string (output_interface_file, ";\n\n");
    }
  /* Function itself */
  output_title_of_transformation_function (output_implementation_file, FALSE,
                                           cpp_flag);
  output_string (output_implementation_file, "\n{\n  ");
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, "++;\n  return\n    ");
  output_name_of_transformation_function (output_implementation_file, TRUE);
  output_string (output_implementation_file, "\n      (");
  output_string (output_implementation_file, TRANSFORM_GRAPH_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, TRANSFORM_FLAG_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file,
                 TRANSFORM_GUARD_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file,
                 TRANSFORM_TRANSFORMATION_FUNCTION_PARAMETER_NAME);
  output_string (output_implementation_file, ");\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}



/* This page contains functions for output of function of initiating and
   finishing work with internal representation. */

/* The following macro value is name of the first parameter of
   function `_IR_class_fields_initiation' or
   `_IR_class_fields_finalization'. */

#define CLASS_FIELD_NODE_MODE_PARAMETER_NAME "node_mode"

/* The following macro value is name of the second parameter of
   function `_IR_class_fields_initiation' or
   `_IR_class_fields_finalization'. */

#define CLASS_FIELD_SOURCE_NODE_MODE_PARAMETER_NAME "source_node_mode"

/* The following function outputs title of class fields initiation or
   finalization function.  The most probable function output is
   following
             `void _IR_class_fields_initiation
             (IR_node_mode_t node_mode,
              IR_node_mode_t source_node_mode)'
        or
             `void _IR_class_fields_finalization
             (IR_node_mode_t node_mode,
              IR_node_mode_t source_node_mode)'. */

static void
output_title_of_class_field_initiation_finalization_function
  (FILE *f, int initiation_flag)
{
  output_string (f, "void ");
  output_name_of_class_field_initiation_finalization_function
    (f, initiation_flag);
  output_char (' ', f);
  output_string (f, " (");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, CLASS_FIELD_NODE_MODE_PARAMETER_NAME);
  output_string (f, ", ");
  output_node_mode_t_name (f);
  output_char (' ', f);
  output_string (f, CLASS_FIELD_SOURCE_NODE_MODE_PARAMETER_NAME);
  output_string (f, ")");
}

/* This function traverses (if flat model is used) all super types of
   given node type and outputs code of initiation (finalization) of
   all class fields of node of given type.  The function also outputs
   code of class action for generation of initiation function.
   Remember that the code is output with `{' and `}'. */

static void
output_node_type_class_field_initiations_finalizations (int initiation_flag,
                                                        IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t curr_supertype_list_element;

  if (flat_flag)
    for (curr_supertype_list_element
         = IR_first_super_type_list_element (node_type);
         curr_supertype_list_element != NULL;
         curr_supertype_list_element
         = IR_next_super_type_list_element (curr_supertype_list_element))
      if (IR_immediate_super_type (curr_supertype_list_element) != NULL)
        output_node_type_class_field_initiations_finalizations
          (initiation_flag,
           IR_immediate_super_type (curr_supertype_list_element));
  /* Output node type class field initiations */
  if (IR_last_field (node_type) != NULL)
    for (current_field = IR_next_field (IR_last_field (node_type));;
         current_field = IR_next_field (current_field))
      {
        if (IR_NODE_MODE (current_field) == IR_NM_field)
          {
            if (IR_declaration_part (current_field) == DP_CLASS)
              {
                if (initiation_flag)
                  last_processed_field = current_field;
                /* Type specific initiation or finalization */
                output_string (output_implementation_file, "      ");
                if (initiation_flag)
                  output_field_type_initialization_macro_name_prefix
                    (output_implementation_file);
                else
                  output_field_type_finalization_macro_name_prefix
                    (output_implementation_file);
                output_field_type_name (output_implementation_file,
                                        IR_field_type (current_field));
                output_string (output_implementation_file, " (");
                output_class_structure_field_access
                  (output_implementation_file,
                   CLASS_FIELD_SOURCE_NODE_MODE_PARAMETER_NAME, current_field);
                output_string (output_implementation_file, ");\n");
              }
          }
        else if (IR_NODE_MODE (current_field) == IR_NM_action)
          {
            if (initiation_flag
                && IR_declaration_part (current_field) == DP_CLASS)
              {
                output_char ('{', output_implementation_file);
                output_code (output_implementation_file, current_field,
                             CLASS_FIELD_SOURCE_NODE_MODE_PARAMETER_NAME,
                             TRUE);
                output_string (output_implementation_file, "}\n");
              }
          }
        else
          assert (IR_NODE_MODE (current_field) == IR_NM_constraint);
        if (current_field == IR_last_field (node_type))
          break;
      }
}

/* The following variable is flag of generation of class fields
   initiation function. */

static int initiation_function_flag;

/* If given node type is not abstract or non-flat model is used the
   function outputs code of initiation or finalization of all class
   fields of node of given type with the aid of function
   `output_node_type_class_field_initiations_finalizations'.  The
   function is called from function `traverse_node_types'. */

static void
output_all_node_type_class_field_initiations_finalizations
  (IR_node_t node_type)
{
  IR_node_t curr_supertype_list_element;

  if (!flat_flag || !IR_abstract_flag (node_type))
    {
      output_string (output_implementation_file, "    case ");
      output_node_mode_type_value
        (output_implementation_file,
         IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file, ":\n");
      if (flat_flag)
        last_processed_field = NULL;
      else
        {
          last_processed_field = last_field_in_super_type (node_type, TRUE);
          for (curr_supertype_list_element
               = IR_first_super_type_list_element (node_type);
               curr_supertype_list_element != NULL;
               curr_supertype_list_element
               = IR_next_super_type_list_element (curr_supertype_list_element))
            if (IR_immediate_super_type (curr_supertype_list_element)
                != root_node_type)
              {
                output_string (output_implementation_file, "      ");
                output_name_of_class_field_initiation_finalization_function
                  (output_implementation_file, initiation_function_flag);
                output_string (output_implementation_file, " (");
                output_node_mode_type_value
                  (output_implementation_file,
                   IR_identifier_itself
                   (IR_type_identifier (IR_immediate_super_type
                                        (curr_supertype_list_element))));
                output_string (output_implementation_file, ", ");
                output_string (output_implementation_file,
                               CLASS_FIELD_SOURCE_NODE_MODE_PARAMETER_NAME);
                output_string (output_implementation_file, ");\n");
              }
        }
      output_node_type_class_field_initiations_finalizations
        (initiation_function_flag, node_type);
      output_string (output_implementation_file, "      break;\n");
    }
}

/* The following function outputs definition of function
   `_IR_class_fields_initiation' or
   `_IR_class_fields_finalization'. */

static void
output_class_field_initiation_finalization_function (int initiation_flag)
{
  initiation_function_flag = initiation_flag;
  /* Output field initiation or finalization function. */
  if (!cpp_flag)
    output_string (output_implementation_file, "static ");
  output_title_of_class_field_initiation_finalization_function
    (output_implementation_file, initiation_flag);
  output_string (output_implementation_file, "\n{\n");
  /* Output definition of auxiliary node necessary for initiation
     of class fields. */
  output_string (output_implementation_file, "  ");
  output_node_type_name (output_implementation_file, ROOT_NAME);
  output_string (output_implementation_file, "  ");
  output_auxiliary_node_name (output_implementation_file);
  output_string (output_implementation_file, ";\n\n");
  /* Check non-null class structure reference. */
  output_string (output_implementation_file, "  if (");
  output_class_structure_array_name (output_implementation_file);
  output_string (output_implementation_file, " [");
  output_string (output_implementation_file,
                 CLASS_FIELD_SOURCE_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, "] == NULL)\n    return;\n");
  /* Initiate or finalize fields of node type */
  output_string (output_implementation_file, "  switch (");
  output_string (output_implementation_file,
                 CLASS_FIELD_NODE_MODE_PARAMETER_NAME);
  output_string (output_implementation_file, ")\n    {\n");
  traverse_node_types
    (output_all_node_type_class_field_initiations_finalizations);
  output_string (output_implementation_file, "    }\n");
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}

/* The following function outputs title of internal representation
   initiation function.  The most probable function output is
   following
                     `void IR_start (void)'. */

static void
output_title_of_start_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_start_function (f);
  output_string (f, " (void)");
}


/* The following function outputs title of internal representation
   finalization function.  The most probable function output is
   following
                     `void IR_stop (void)'. */

static void
output_title_of_stop_function (FILE *f)
{
  output_string (f, "void ");
  output_name_of_stop_function (f);
  output_string (f, " (void)");
}

/* The following function outputs checking size of give node type when
   size not of unsigned long.  The function outputs fragment
    
      `#ifdef ...
       if (sizeof (<node type name>) > 255)
         {
           fprintf (stderr, "Very long node `<node type name>'
                    use option `-long-node-size' or `-short-node-size'\n");
           exit (1);
         }
       #endif ...'
    
      or
    
      `#ifdef ...
       if (sizeof (<node type name>) > 65535)
         {
           fprintf (stderr, "Very long node `<node type name>'
                    use option `-long-node-size'\n");
           exit (1);
         }
       #endif ...'
    
      or nothing.
   The function is called from function `traverse_node_types'. */

static void
output_checking_node_size (IR_node_t node_type)
{
  if (!long_node_size_flag)
    {
      output_ifdef (output_implementation_file, DEBUG_PARAMETER_NAME);
      output_string (output_implementation_file, "  if (sizeof (");
      output_node_type_name (output_implementation_file,
                             IR_identifier_itself (IR_type_identifier
                                                   (node_type)));
      output_string (output_implementation_file, ") > ");
      if (!short_node_size_flag)
        output_string (output_implementation_file, "255)\n");
      else
        output_string (output_implementation_file, "65535)\n");
      output_string (output_implementation_file,
                     "    {\n      fprintf (stderr, \"Very long node `");
      output_string (output_implementation_file,
                     IR_identifier_itself (IR_type_identifier (node_type)));
      output_string (output_implementation_file,
                     "' use option `-long-node-size'");
      if (!short_node_size_flag)
        output_string (output_implementation_file, " or `-short-node-size'");
      output_string (output_implementation_file,
                     "\\n\");\n      exit (1);\n    }\n");
      output_endif (output_implementation_file, DEBUG_PARAMETER_NAME);
    }
}

/* The following macro value is name of variable used in functions
   `IR_start' and `IR_stop'. */

#define CURRENT_NODE_MODE_VARIABLE_NAME "_current_node_mode"

/* The following function outputs external definition and definition
   of internal representation initiation or finalization function. */

static void
output_initiation_finalization_function (int initiation_flag)
{
  /* Extern definition */
  output_string (output_interface_file, "extern ");
  if (initiation_flag)
    output_title_of_start_function (output_interface_file);
  else
    output_title_of_stop_function (output_interface_file);
  output_string (output_interface_file, ";\n\n");
  if (there_is_a_class_field)
    /* Output field initiation function. */
    output_class_field_initiation_finalization_function (initiation_flag);
  /* Function itself */
  if (initiation_flag)
    output_title_of_start_function (output_implementation_file);
  else
    output_title_of_stop_function (output_implementation_file);
  output_string (output_implementation_file, "\n{\n");
  if (there_is_a_class_field)
    {
      /* Output definition of node mode type variable necessary for
         initiation of class fields. */
      output_string (output_implementation_file, "  ");
      output_node_mode_t_name (output_implementation_file);
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file,
                     CURRENT_NODE_MODE_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n\n");
    }
  if (initiation_flag)
    {
      /* Check correct usage of non long node type sizes. */
      traverse_node_types (output_checking_node_size);
      /* Initiate current graph pass number variable. */
      output_string (output_implementation_file, "  ");
      output_current_graph_pass_number_variable_name
        (output_implementation_file);
      output_string (output_implementation_file, " = 0;\n");
      /* Initiate allocator */
      output_string (output_implementation_file, "  ");
      output_start_alloc_macro_name (output_implementation_file);
      output_string (output_implementation_file, " ();\n");
    }
  if (there_is_a_class_field)
    {
      /* Initiate or finalize class fields */
      output_string (output_implementation_file, "  for (");
      output_string (output_implementation_file,
                     CURRENT_NODE_MODE_VARIABLE_NAME);
      output_string (output_implementation_file, " = (");
      output_node_mode_t_name (output_implementation_file);
      output_string (output_implementation_file, ") 0;\n       ");
      output_string (output_implementation_file,
                     CURRENT_NODE_MODE_VARIABLE_NAME);
      output_string (output_implementation_file, " < ");
      output_node_mode_type_value (output_implementation_file, ROOT_NAME);
      output_string (output_implementation_file, ";\n       ");
      output_string (output_implementation_file,
                     CURRENT_NODE_MODE_VARIABLE_NAME);
      output_string (output_implementation_file, " = (");
      output_node_mode_t_name (output_implementation_file);
      output_string (output_implementation_file, ") ((int) ");
      output_string (output_implementation_file,
                     CURRENT_NODE_MODE_VARIABLE_NAME);
      output_string (output_implementation_file, " + 1))\n    ");
      if (initiation_flag)
        output_name_of_class_field_initiation_finalization_function
          (output_implementation_file, TRUE);
      else
        output_name_of_class_field_initiation_finalization_function
          (output_implementation_file, FALSE);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file,
                     CURRENT_NODE_MODE_VARIABLE_NAME);
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file,
                     CURRENT_NODE_MODE_VARIABLE_NAME);
      output_string (output_implementation_file, ");\n");
    }
  if (!initiation_flag)
    {
      /* Finish allocator */
      output_string (output_implementation_file, "  ");
      output_stop_alloc_macro_name (output_implementation_file);
      output_string (output_implementation_file, " ();\n");
    }
  /* Function epilogue */
  output_string (output_implementation_file, "}\n\n");
}



/* This page contains function for output of all SPI functions. */

/* The following function outputs all functions of SPI. */

static void
output_functions (void)
{
  output_set_double_field_value_function ();
  if (access_flag)
    {
      if (!cpp_flag)
	output_first_double_link_function_declaration ();
      output_first_double_link_function_definition ();
      if (!cpp_flag)
        {
	  output_rest_double_link_functions_declarations ();
	  output_rest_double_link_functions_definitions ();
          traverse_all_fields (output_field_access_function_declaration);
          traverse_all_fields (output_field_access_function_definition);
        }
    }
  if (set_flag)
    {
      if (!cpp_flag)
        {
	  output_set_double_link_function_declaration ();
	  output_set_double_link_function_definition ();
          traverse_all_fields
	    (output_field_modification_function_declaration);
          traverse_all_fields (output_field_modification_function_definition);
        }
    }
  if (!cpp_flag)
    output_creation_function_declaration ();
  output_creation_function_definition ();
  if (new_flag)
    {
      if (!cpp_flag)
        traverse_node_types
          (output_node_type_specific_creation_function_declaration);
      traverse_node_types
        (output_node_type_specific_creation_function_definition);
    }
  if (free_flag || free_graph_flag)
    {
      if (!cpp_flag)
        output_node_deletion_function_declaration ();
      output_node_deletion_function_definition ();
    }
  if (free_graph_flag)
    {
      output_internal_graph_deletion_function ();
      if (!cpp_flag)
        output_graph_deletion_function_declaration ();
      output_graph_deletion_function_definition ();
      if (!cpp_flag)
        output_conditional_graph_deletion_function_declaration ();
      output_conditional_graph_deletion_function_definition ();
    }
  output_copy_functions ();
  output_equality_functions ();
  output_check_functions ();
  output_io_functions ();
  if (traverse_flag)
    output_traverse_function (FALSE);
  if (reverse_traverse_flag)
    output_traverse_function (TRUE);
  if (transform_flag)
    output_transformation_function ();
  output_initiation_finalization_function (TRUE);
  output_initiation_finalization_function (FALSE);
}



/* This page contains function to output all export codes
   of one level description. */

/* The following function outputs all export codes of one level
   description.  Code is output without any modification. */

static void
output_finish_code_insertions (void)
{
  IR_node_t current_code_insertion;

  if (IR_last_code_insertion (current_description_part) == NULL)
    return;
  for (current_code_insertion
       = IR_next_code_insertion (IR_last_code_insertion
                                 (current_description_part));;
       current_code_insertion
       = IR_next_code_insertion (current_code_insertion))
    {
      if (IR_NODE_MODE (current_code_insertion) == IR_NM_export_code)
        {
          if (!no_line_flag)
            output_line (output_interface_file,
                         IR_position (current_code_insertion).line_number,
                         IR_position (current_code_insertion).file_name);
          output_string (output_interface_file,
                         IR_code_insertion_itself (IR_code_itself
                                                   (current_code_insertion)));
          output_char ('\n', output_interface_file);
          if (!no_line_flag)
            output_current_line (output_interface_file);
        }
      else
        assert (IR_NODE_MODE (current_code_insertion) == IR_NM_import_code
                || IR_NODE_MODE (current_code_insertion) == IR_NM_local_code);
      if (current_code_insertion
          == IR_last_code_insertion (current_description_part))
        break;
    }
}



/* This page contains function for output of description additional
   codes. */

/* The following function outputs all additional codes of one level
   description.  Code is output without any modification. */

static void
output_additional_codes (void)
{
  IR_node_t current_additional_code;

  if (IR_last_additional_code (current_description_part) == NULL)
    return;
  for (current_additional_code
       = IR_next_additional_code (IR_last_additional_code
                                  (current_description_part));;
       current_additional_code
       = IR_next_additional_code (current_additional_code))
    {
      assert (IR_NODE_MODE (current_additional_code) == IR_NM_additional_code);
      output_char ('\n', output_implementation_file);
      if (!no_line_flag)
        output_line (output_implementation_file,
                     IR_position (current_additional_code).line_number,
                     IR_position (current_additional_code).file_name);
      output_string (output_implementation_file,
                     IR_additional_code_itself (current_additional_code));
      if (current_additional_code
          == IR_last_additional_code (current_description_part))
        break;
    }
}



/* This page contains the only extern function for generation of SPI. */

static int all_node_types_number;
static int abstract_node_types_number;
static int double_node_types_number;
static int all_fields_number;
static int double_fields_number;
static int synonym_fields_number;
static int class_fields_number;
static int skeleton_fields_number;
static int other_fields_number;

/* The following function is used to count number of node type.  The
   function is called through the function `traverse_node_types'. */

static void
count_node_types (IR_node_t node_type)
{
  all_node_types_number++;
  if (IR_abstract_flag (node_type))
    abstract_node_types_number++;
  if (IR_double_node_flag (node_type))
    double_node_types_number++;
}

/* The following function is used to count number of fields.  The
   function is called through the function `traverse_all_fields'. */

static void
count_fields (IR_node_t field)
{
  all_fields_number++;
  if (IR_double_field_flag (field))
    double_fields_number++;
  if (field != IR_previous_synonym_field (field))
    synonym_fields_number++;
  if (IR_declaration_part (field) == DP_CLASS)
    class_fields_number++;
  else if (IR_declaration_part (field) == DP_SKELETON)
    skeleton_fields_number++;
  else
    {
      assert (IR_declaration_part (field) == DP_OTHER);
      other_fields_number++;
    }
}

/* The following function evaluate statistic information about source
   description and prints it. */

static void
output_statistics (void)
{
  all_node_types_number = 0;
  abstract_node_types_number = 0;
  double_node_types_number = 0;
  all_fields_number = 0;
  double_fields_number = 0;
  synonym_fields_number = 0;
  class_fields_number = 0;
  skeleton_fields_number = 0;
  other_fields_number = 0;
  traverse_node_types (count_node_types);
  traverse_all_fields (count_fields);
  printf ("sprut: node types: all - %3d, abstract - %3d, double - %3d\n",
          all_node_types_number, abstract_node_types_number,
          double_node_types_number);
  printf ("       fields:     all - %4d, double - %4d, synonym - %4d\n",
          all_fields_number, double_fields_number, synonym_fields_number);
  printf ("                   class - %4d, skeleton - %4d, others - %4d\n",
          class_fields_number, skeleton_fields_number, other_fields_number);
}

/* The following macro defines base name of macro whose definition is
   sign of C++ interface generation. */

#define MACRO_CPP_FLAG_NAME "CPP"

/* This function is major function of the file.  The functions opens
   implementation and interface files of SPI and outputs all SPI.  No
   errors must be fixed before this function call. */

void
generate_spi (void)
{
  graph_pass_number_member_flag
    = (free_graph_flag || copy_graph_flag || equal_graph_flag
       || check_graph_flag || traverse_flag || reverse_traverse_flag
       || transform_flag);
  initiate_traversing_internal_representation ();
  initiate_output ();
  double_node_types_existence_flag = FALSE;
  traverse_node_types (check_double_node_types_existence);
  there_is_a_class_field = FALSE;
  traverse_all_fields (check_class_field_presence);
  output_ifndef (output_interface_file, original_description_part_name);
  output_string (output_interface_file, "#define ");
  output_ifdef_parameter_name (output_interface_file,
                               original_description_part_name);
  output_string (output_interface_file, "\n\n");
  if (cpp_flag)
    {
      output_ifndef (output_interface_file, MACRO_CPP_FLAG_NAME);
      output_string (output_interface_file, "#define ");
      output_ifdef_parameter_name (output_interface_file, MACRO_CPP_FLAG_NAME);
      output_string (output_interface_file, "\n");
      output_string (output_interface_file, "#endif\n\n");
    }
  if (debug_flag)
    {
      output_string (output_interface_file, "#define ");
      output_ifdef_parameter_name (output_interface_file,
                                   DEBUG_PARAMETER_NAME);
      output_string (output_interface_file, "\n\n");
    }
  output_string (output_implementation_file, "#include <stdio.h>\n");
  output_string (output_implementation_file, "#include <string.h>\n");
  output_string (output_implementation_file, "#include <stdlib.h>\n");
  output_string (output_implementation_file, "#include \"");
  output_string (output_implementation_file, output_interface_file_name);
  output_string (output_implementation_file, "\"\n\n");
  /* The following macro is output to interface file because it is
     used in modification of double fields in modification macros. */
  output_string (output_interface_file, "#include <stddef.h>\n\n");
  output_string (output_interface_file, "#ifdef offsetof\n#define ");
  output_offsetof_macro_name (output_interface_file);
  output_string (output_interface_file,
                 "(type, field) offsetof (type, field)\n");
  output_string (output_interface_file, "#else\n");
  output_string (output_interface_file, "#define ");
  output_offsetof_macro_name (output_interface_file);
  output_string
    (output_interface_file,
     "(type, field) ((char *)&((type *) 64)->field - (char *) 64)\n");
  output_string (output_interface_file, "#endif\n");
  output_string (output_interface_file, "\n");
  output_start_code_insertions ();
  output_default_macros ();
  output_node_mode_type_definition ();
  output_node_type_flag_arrays ();
  output_arrays_of_different_displacement_field_displacement ();
  output_class_structures ();
  output_node_type_definitions ();
  if (!no_node_name_flag || print_flag || check_flag || check_graph_flag)
    output_node_names ();
  output_node_sizes ();
  if (!cpp_flag)
    output_node_type_macros_functions ();
  /* Output declaration of `_IR_traverse_guard_function_variable'. */
  output_string (output_implementation_file, "static ");
  output_definition_of_traverse_guard_function (output_implementation_file,
                                                TRUE);
  output_string (output_implementation_file, ";\n\n");
  /* Output declaration of `_IR_current_graph_pass_number'. */
  output_string (output_implementation_file, "static ");
  output_string (output_implementation_file, GRAPH_PASS_NUMBER_TYPE_NAME);
  output_char (' ', output_implementation_file);
  output_current_graph_pass_number_variable_name (output_implementation_file);
  output_string (output_implementation_file, ";\n\n");
  output_functions ();
  output_finish_code_insertions ();
  output_additional_codes ();
  output_endif (output_interface_file, original_description_part_name);
  if (number_of_errors == 0 && statistics_flag)
    output_statistics ();
  finish_traversing_internal_representation ();
}
