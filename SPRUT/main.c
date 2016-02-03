/*
   FILE NAME:   main.c

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

   TITLE:       Driver of SPRUT (internal representation description
                translator)

   DESCRIPTION: This file processes command line of SPRUT, initiates
                and finishes abstract data common for all SPRUT.
                The file also sets up standard environment of internal
                representation description (root node type and its identifier),
                sets up reaction on fatal errors and user's signals, and
                initiates all SPRUT common variables.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of file work.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include "allocate.h"
#include "commline.h"
#include "position.h"
#include "errors.h"
#include "vlobject.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
#include "yacc.h"
#include "anal.h"
#include "gen.h"

#include <assert.h>

/* The following macro value is directory which are processed always
   after the current directory to search extended file specification.
   The directory contains standard libraries. */

#ifndef STANDARD_LIBRARY_DIRECTORY
#define STANDARD_LIBRARY_DIRECTORY  "/usr/local/lib"
#endif

/* The following macro value is standard prefix of names of standard
   procedural interface (SPI) objects.  If this macro value is changed
   then documentation of sprut and macro OPTIONS must be changed. */

#define STANDARD_PREFIX "IR_"

/* The following macro value is suffix of name of interface file of
   standard procedural interface (SPI).  If this macro value is changed
   then documentation of sprut must be changed. */

#define STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX ".h"

/* The following macro value is suffix of name of implementation file
   of standard procedural interface (SPI) on C.  If this macro value
   is changed then documentation of sprut must be changed. */

#define STANDARD_OUTPUT_IMPLEMENTATION_C_FILE_SUFFIX ".c"

/* The following macro value is suffix of name of implementation file
   of standard procedural interface (SPI) on C++.  If this macro value
   is changed then documentation of sprut must be changed. */

#define STANDARD_OUTPUT_IMPLEMENTATION_CPP_FILE_SUFFIX ".cpp"

/* This function is nonstandard reaction on error `no memory' of
   allocation package. */

static void
error_function_for_allocate (void)
{
  error (TRUE, current_position, "fatal error -- no memory");
}


/* The following function returns suffix of given file name (pointer
   to first char (it is `.') of suffix of given file name, empty
   string if the suffix is absent).  The returned string can not be
   changed.  */

static const char *
file_name_suffix (const char *file_name)
     
{
  const char *last_period;

  for (last_period = NULL; *file_name != '\0'; file_name++)
    if (*file_name == '.')
      last_period = file_name;
  return (last_period == NULL ? file_name : last_period);
}


/* The function returns base name of given file name (pointer to first
   char after last `/' in given file name, given file name itself if
   the directory name is absent).  The returned string can not be
   changed. */

static const char *
base_file_name (const char *file_name)
{
  int directory_name_length;

  directory_name_length = strlen (file_name);
  while (directory_name_length >= 0 && file_name[directory_name_length] != '/')
    directory_name_length--;
  return file_name + directory_name_length + 1;
}


/* The function finishes abstract data common for all SPRUT, closes
   output files (SPI), deletes them if exit will be non-normal, and
   exits with code `1' if parameter value is TRUE or error was fixed,
   otherwise exits with code `0'. */

static void
sprut_finish (int fatal_exit_flag)
{
  int exit_code;

  if (output_interface_file != NULL)
    fclose (output_interface_file);
  if (output_implementation_file != NULL)
    fclose (output_implementation_file);
  finish_identifier_table ();
  finish_double_declaration_identifier_table ();
  finish_type_table ();
  finish_field_table ();
  finish_node_field_table ();
  finish_parser ();
  /* Output of errors only after `finish_parser' because it can flush
     last syntax error. */
  output_errors ();
  finish_errors ();
  finish_positions ();
  IR_stop ();
  /* `number_of_errors' is fetched only after `finish_parser' because
     it can flush last syntax error. */
  exit_code = (number_of_errors != 0 || fatal_exit_flag ? 1 : 0);
  if (exit_code != 0)
    {
      if (output_interface_file != NULL)
          remove (output_interface_file_name);
      if (output_implementation_file != NULL)
          remove (output_implementation_file_name);
    }
  exit (exit_code);
}


/* The function is nonstandard reaction on fatal error of package
   `errors'.  The function calls `sprut_finish' for non-normal exit. */

static void
sprut_fatal_finish (void)
{
  sprut_finish (TRUE);
}


/* The function initiates abstract data common for all SPRUT and some
   SPRUT common variables.  The function also sets up standard
   environment of internal representation description (root node type
   and its identifier) with insertion of them into corresponding
   tables and sets up reaction on fatal errors. */

static void
sprut_start (void)
{
  change_allocation_error_function (error_function_for_allocate);
  initiate_positions ();
  initiate_errors (FALSE);
  IR_start ();
  initiate_identifier_table ();
  initiate_double_declaration_identifier_table ();
  initiate_type_table ();
  initiate_field_table ();
  initiate_node_field_table ();
  initiate_parser ();
  /* Create identifier `%root' and insert it into identifier table. */
  root_identifier = IR_create_node (IR_NM_identifier);
  IR_set_position (root_identifier, current_position);
  IR_TOP_ADD_STRING ("%root");
  IR_set_identifier_itself (root_identifier, IR_TOP_BEGIN ());
  IR_TOP_FINISH ();
  (void) insert_identifier (IR_identifier_itself  (root_identifier));
  /* Create node type `%root' and insert it into type table. */
  root_node_type = IR_create_node (IR_NM_node_type);
  IR_set_position (root_node_type, current_position);
  IR_set_type_identifier (root_node_type, root_identifier);
  IR_set_double_node_flag (root_node_type, FALSE);
  IR_set_first_super_type_list_element (root_node_type, NULL);
  IR_set_last_super_type_list_element (root_node_type, NULL);
  IR_set_basic_super_types_flag (root_node_type, TRUE);
  IR_set_last_field (root_node_type, NULL);
  IR_set_abstract_flag (root_node_type, TRUE);
  (void) insert_type (root_node_type);
  IR_set_next_type (root_node_type, NULL);
  /* Initiation of some SPRUT common variables. */
  current_description_part = NULL;
  fatal_error_function = sprut_fatal_finish;
  output_interface_file = NULL;
  output_implementation_file = NULL;
}


/* The function is reaction on user's signals (interrupt and
   terminate).  The function calls `sprut_finish' for non-normal
   exit. */

static RETSIGTYPE
user_signal_action (int signal_number)
{
  sprut_finish (TRUE);
}

/* The following macro value is description of SPRUT command line (see
   package of `command-line'. */

#define COMMAND_LINE_DESCRIPTION \
"prefix dir\n"\
"%%\n"\
"command line: sprut [option ...] specification-file\n"\
"\n"\
"`-v'            output verbose warning information\n"\
"`-c++'          generation of C++ code instead of C code\n"\
"`-statistics'   output statistics information\n"\
"`-Edir'         directory for search for extended specification files\n"\
"`-flat'         flag of flat implementation of work with node type fields\n"\
"`-flat-structure'\n"\
"                flag of flat implementation of node type structures\n"\
"`-fast'         generation of some big arrays as unpacked for faster work\n"\
"`-long-node-size'  node size is unsigned long\n"\
"`-short-node-size' node size is unsigned short\n"\
"`-macro'        generation of access functions and macros\n"\
"`-only-macro'   generation of macros instead of access functions\n"\
"`-debug'        generation of check code for modification functions\n"\
"`-pprefix'      use 'prefix' instead of 'IR_' for generated object names\n"\
"`-no-line'      do not generate files containing numbered line directives\n"\
"`-all'          generation of all standard procedural interface\n"\
"`-access'       access functions (macros) generation\n"\
"`-set'          field modification functions generation\n"\
"`-new'          generation of node type specific creation functions\n"\
"`-free'         generation of function for freeing node\n"\
"`-free-graph'   generation of functions for freeing node and graph\n"\
"`-copy'         generation of function for copying node\n"\
"`-copy-graph'   generation of functions for copying node and graph\n"\
"`-equal'        node comparison function generation\n"\
"`-equal-graph'  node and graph comparison functions generation\n"\
"`-check'        node check function generation\n"\
"`-check-graph'  node and graph check functions generation\n"\
"`-print'        node print function generation\n"\
"`-input'        node input function generation\n"\
"`-output'       node output function generation\n"\
"`-traverse'     generation of function for traversing graphs\n"\
"`-reverse-traverse' generation of function for reverse traversing graphs\n"\
"`-transform'    generation of function for transforming graphs\n"\
"`-no-node-name' no generation of array containing node names\n"\
"                if it is unnecessary"


/* The function processes command line of SPRUT and initiates some
   common SPRUT variables.  The function also calls parser, semantic
   analyzer, and generator of SPRUT and sets up reaction on user's
   signals. */

int
main (int argc, char **argv)
{
  int i, okay;
  int option_has_argument;
  char *option;
  const char *base_name;
  char *source_file_name;
  char *string;
  vlo_t extended_specification_directories_vector;

  sprut_start ();
  /* Tune the translator on command line arguments. */
  if (!start_command_line_processing (argc, argv, COMMAND_LINE_DESCRIPTION))
    {
      fprintf (stderr, "sprut: invalid command line description\n");
      sprut_finish (TRUE);
    }
  if (argument_count == 1)
    {
      output_command_line_description ();
      sprut_finish (TRUE);
    }
  v_flag = FALSE;
  cpp_flag = FALSE;
  statistics_flag = FALSE;
  VLO_CREATE (extended_specification_directories_vector, 0);
  string = STANDARD_LIBRARY_DIRECTORY;
  VLO_ADD_MEMORY (extended_specification_directories_vector, &string,
                  sizeof (char *));
  flat_flag = FALSE;
  flat_structure_flag = FALSE;
  fast_flag = FALSE;
  long_node_size_flag = FALSE;
  short_node_size_flag = FALSE;
  macro_flag = FALSE;
  only_macro_flag = FALSE;
  debug_flag = FALSE;
  no_line_flag = FALSE;
  access_flag = FALSE;
  set_flag = FALSE;
  new_flag = FALSE;
  free_flag = FALSE;
  free_graph_flag = FALSE;
  copy_flag = FALSE;
  copy_graph_flag = FALSE;
  equal_flag = FALSE;
  equal_graph_flag = FALSE;
  check_flag = FALSE;
  check_graph_flag = FALSE;
  print_flag = FALSE;
  input_flag = FALSE;
  output_flag = FALSE;
  traverse_flag = FALSE;
  reverse_traverse_flag = FALSE;
  transform_flag = FALSE;
  no_node_name_flag = FALSE;
  IR_TOP_ADD_STRING (STANDARD_PREFIX);
  prefix = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  /* Process all command line options. */
  for (i = next_option (TRUE), okay = TRUE; i != 0; i = next_option (FALSE))
    {
      option = option_characteristics (i, &option_has_argument);
      if (option == NULL)
        {
          if (option_has_argument)
            fprintf (stderr, "sprut: flag `%s' without argument\n",
                     argument_vector[i]);
          else
            fprintf (stderr,
                     "sprut: unknown flag `%s'\n", argument_vector[i]);
          okay = FALSE;
        }
      else if (strcmp (option, "-v") == 0)
        v_flag = TRUE;
      else if (strcmp (option, "-c++") == 0)
        cpp_flag = TRUE;
      else if (strcmp (option, "-statistics") == 0)
        statistics_flag = TRUE;
      else if (strcmp (option, "-E") == 0)
        {
          string = argument_vector [i] + 2;
          VLO_ADD_MEMORY (extended_specification_directories_vector,
                          &string, sizeof (char *));
        }
      else if (strcmp (option, "-flat") == 0)
        flat_flag = TRUE;
      else if (strcmp (option, "-flat-structure") == 0)
        flat_structure_flag = TRUE;
      else if (strcmp (option, "-fast") == 0)
        fast_flag = TRUE;
      else if (strcmp (option, "-long-node-size") == 0)
        {
          long_node_size_flag = TRUE;
          if (short_node_size_flag)
            fprintf
              (stderr,
               "sprut:`-short-node-size' is ignored because of `%s' presence\n",
               argument_vector[i]);
          short_node_size_flag = FALSE;
        }
      else if (strcmp (option, "-short-node-size") == 0)
        {
          if (long_node_size_flag)
            fprintf
              (stderr,
               "sprut: `%s' is ignored because of `-long-node-size' presence\n",
               argument_vector[i]);
          else
            short_node_size_flag = TRUE;
        }
      else if (strcmp (option, "-macro") == 0)
        {
          if (only_macro_flag)
            fprintf
              (stderr,
               "sprut: `%s' is ignored because of `-only-macro' presence\n",
               argument_vector[i]);
          else
            macro_flag = TRUE;
        }
      else if (strcmp (option, "-only-macro") == 0)
        {
          if (macro_flag)
            fprintf (stderr,
                     "sprut: `%s' is ignored because of `-macro' presence\n",
                     argument_vector[i]);
          else
            only_macro_flag = TRUE;
        }
      else if (strcmp (option, "-debug") == 0)
        debug_flag = TRUE;
      else if (strcmp (option, "-p") == 0)
        {
          IR_TOP_ADD_STRING (argument_vector[i] + 2);
          prefix = IR_TOP_BEGIN ();
          IR_TOP_FINISH ();
        }
      else if (strcmp (option, "-no-line") == 0)
        no_line_flag = TRUE;
      else if (strcmp (option, "-all") == 0)
        {
          access_flag = TRUE;
          set_flag = TRUE;
          new_flag = TRUE;
          free_flag = TRUE;
          free_graph_flag = TRUE;
          copy_flag = TRUE;
          copy_graph_flag = TRUE;
          equal_flag = TRUE;
          equal_graph_flag = TRUE;
          check_flag = TRUE;
          check_graph_flag = TRUE;
          print_flag = TRUE;
          input_flag = TRUE;
          output_flag = TRUE;
          traverse_flag = TRUE;
          reverse_traverse_flag = TRUE;
          transform_flag = TRUE;
        }
      else if (strcmp (option, "-access") == 0)
        access_flag = TRUE;
      else if (strcmp (option, "-set") == 0)
        set_flag = TRUE;
      else if (strcmp (option, "-new") == 0)
        new_flag = TRUE;
      else if (strcmp (option, "-free") == 0)
        free_flag = TRUE;
      else if (strcmp (option, "-free-graph") == 0)
        free_graph_flag = TRUE;
      else if (strcmp (option, "-copy") == 0)
        copy_flag = TRUE;
      else if (strcmp (option, "-copy-graph") == 0)
        copy_graph_flag = TRUE;
      else if (strcmp (option, "-equal") == 0)
        equal_flag = TRUE;
      else if (strcmp (option, "-equal-graph") == 0)
        equal_graph_flag = TRUE;
      else if (strcmp (option, "-check") == 0)
        check_flag = TRUE;
      else if (strcmp (option, "-check-graph") == 0)
        check_graph_flag = TRUE;
      else if (strcmp (option, "-print") == 0)
        print_flag = TRUE;
      else if (strcmp (option, "-input") == 0)
        input_flag = TRUE;
      else if (strcmp (option, "-output") == 0)
        output_flag = TRUE;
      else if (strcmp (option, "-traverse") == 0)
        traverse_flag = TRUE;
      else if (strcmp (option, "-reverse-traverse") == 0)
        reverse_traverse_flag = TRUE;
      else if (strcmp (option, "-transform") == 0)
        transform_flag = TRUE;
      else if (strcmp (option, "-no-node-name") == 0)
        no_node_name_flag = TRUE;
      else
        assert (FALSE);
    }
  if (number_of_operands () != 1)
    {
      fprintf (stderr,
               "sprut: one specification file must be on command line\n");
      okay = FALSE;
    }
  else
    {
      source_file_name = argument_vector[next_operand (TRUE)];
      if (strcmp (file_name_suffix (source_file_name),
                  STANDARD_INPUT_FILE_SUFFIX) != 0)
        {
          fprintf (stderr,
                   "sprut: specification file must have suffix `%s'\n",
                   STANDARD_INPUT_FILE_SUFFIX);
          okay = FALSE;
        }
    }
  string = NULL;
  VLO_ADD_MEMORY (extended_specification_directories_vector, &string,
                  sizeof (char *));
  extended_specification_directories
    = (const char **) VLO_BEGIN (extended_specification_directories_vector);
  if (!okay)
    sprut_finish (TRUE);
  base_name = base_file_name (source_file_name);
  IR_TOP_ADD_MEMORY
    (base_name, strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  original_description_part_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_ADD_MEMORY
    (base_name, strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  if (cpp_flag)
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_IMPLEMENTATION_CPP_FILE_SUFFIX);
  else
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_IMPLEMENTATION_C_FILE_SUFFIX);
  output_implementation_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_ADD_MEMORY
    (base_name, strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  IR_TOP_ADD_STRING (STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX);
  output_interface_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, user_signal_action);
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, user_signal_action);
  start_parser_file (source_file_name, no_position);
  yyparse ();
  analyze_program ();
  if (number_of_errors == 0)
    generate_spi ();
  sprut_finish (FALSE);
  return 0;
}
