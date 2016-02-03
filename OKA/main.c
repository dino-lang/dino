/*
   FILE NAME:   main.c

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool OKA.

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

   TITLE:       Driver of OKA (pipeline hazards description translator)

   DESCRIPTION: This file processes command line of OKA, initiates and
                finishes abstract data common for all OKA.  The file
                also sets up reaction on fatal errors and user's
                signals, and initiates all OKA common variables.

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
#endif

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include "allocate.h"
#include "commline.h"
#include "position.h"
#include "errors.h"
#include "ticker.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
#include "yacc.h"
#include "anal.h"
#include "gen.h"

#include <assert.h>

/* The following macro value is standard prefix of names of pipeline
   hazards description (PHD) objects.  If this macro value is changed
   then documentation of oka and macro OPTIONS must be changed. */

#define STANDARD_PREFIX "OKA"

/* The following macro value is suffix of name of description file of
   pipeline hazards description translator.  If this macro value is
   changed then documentation of OKA must be changed. */

#define STANDARD_OUTPUT_DESCRIPTION_FILE_SUFFIX ".output"

/* The following macro value is suffix of name of interface file of
   pipeline hazards description.  If this macro value is changed then
   documentation of oka must be changed. */

#define STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX ".h"

/* The following macro value is suffix of name of C implementation
   file of pipeline hazards description.  If this macro value is
   changed then documentation of oka must be changed. */

#define STANDARD_OUTPUT_C_IMPLEMENTATION_FILE_SUFFIX ".c"

/* The following macro value is suffix of name of C++ implementation
   file of pipeline hazards description.  If this macro value is
   changed then documentation of oka must be changed. */

#define STANDARD_OUTPUT_CPP_IMPLEMENTATION_FILE_SUFFIX ".cpp"


/* The function is nonstandard reaction on error `no memory' of
   allocation package.  */

static void
error_function_for_allocate (void)
{
  error (TRUE, current_position, "fatal error -- no memory");
}

/* The function returns suffix of given file name.  The returned
   string can not be changed.  */

static const char *
file_name_suffix (const char *file_name)
     
{
  const char *last_period;

  for (last_period = NULL; *file_name != '\0'; file_name++)
    if (*file_name == '.')
      last_period = file_name;
  return (last_period == NULL ? file_name : last_period);
}

/* The function returns base name of given file name, i.e. pointer to
   first char after last `/' in given file name, given file name
   itself if the directory name is absent.  The returned string can
   not be changed. */

static const char *
base_file_name (const char *file_name)
{
  int directory_name_length;

  directory_name_length = strlen (file_name);
  while (directory_name_length >= 0 && file_name[directory_name_length] != '/')
    directory_name_length--;
  return file_name + directory_name_length + 1;
}

/* The function finishes abstract data common for all OKA, closes
   output files, deletes them if exit will be non-normal, and exits
   with code `1' if parameter value is TRUE or error was fixed,
   otherwise exits with code `0'. */

static void
oka_finish (int fatal_exit_flag)
{
  int exit_code;

  if (output_interface_file != NULL)
    fclose (output_interface_file);
  if (output_implementation_file != NULL)
    fclose (output_implementation_file);
  if (output_description_file != NULL)
    fclose (output_description_file);
  finish_single_automaton_declaration_table ();
  finish_single_declaration_table ();
  finish_identifier_table ();
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
      if (output_description_file != NULL)
        remove (output_description_file_name);
    }
  exit (exit_code);
}

/* The function is nonstandard reaction on fatal error of package
   `errors'.  The function calls `oka_finish' for non-normal exit.  */

static void
oka_fatal_finish (void)
{
  oka_finish (TRUE);
}

/* The function initiates abstract data common for all OKA and some
   OKA common variables and sets up reaction on fatal errors. */

static void
oka_start (void)
{
  change_allocation_error_function (error_function_for_allocate);
  initiate_positions ();
  initiate_errors (FALSE);
  IR_start ();
  initiate_identifier_table ();
  initiate_single_automaton_declaration_table ();
  initiate_single_declaration_table ();
  initiate_parser ();
  /* Initiation of some OKA common variables. */
  fatal_error_function = oka_fatal_finish;
  output_interface_file = NULL;
  output_implementation_file = NULL;
  output_description_file = NULL;
}

/* The function is reaction on user's signals (interrupt and
   terminate).  The function calls `oka_finish' for non-normal
   exit. */

static RETSIGTYPE
user_signal_action (int signal_number)
{
  oka_finish (TRUE);
}

/* The following macro value is description of OKA command line (see
   package of `command-line'. */

#define COMMAND_LINE_DESCRIPTION \
"prefix number\n"\
"%%\n"\
"command line: oka [option ...] specification-file\n"\
"\n"\
"`-c++'     output C++ code instead of C code\n"\
"`-debug'   output debugging information during execution of generated code\n"\
"`-enum'    output enumeration instead of macro definitions\n"\
"`-export'  macros defining identifiers of instructions are in interface file\n"\
"`-no-minimization'\n"\
"           don't make minimization of finite state automaton\n"\
"`-split number'\n"\
"           generate given number of automatons\n"\
"`-pprefix' use 'prefix' instead of 'OKA' for names of generated objects\n"\
"`-time'    output time statistics into stderr\n"\
"`-v'       create description file"


/* The function processes command line of OKA and initiates some
   common OKA variables.  The function also calls parser, semantic
   analyzer, and generator of OKA and sets up reaction on user's
   signals.  */

int
main (int argc, char **argv)
{
  int i, okay;
  int option_has_argument;
  char *option;
  const char *base_name;
  char *source_file_name;
  ticker_t parse_time;
  ticker_t analyze_time;
  ticker_t generation_time;
  ticker_t all_time;

  oka_start ();
  /* Tune the translator on command line arguments. */
  if (!start_command_line_processing (argc, argv, COMMAND_LINE_DESCRIPTION))
    {
      fprintf (stderr, "oka: invalid command line description\n");
      oka_finish (TRUE);
    }
  if (argument_count == 1)
    {
      output_command_line_description ();
      oka_finish (TRUE);
    }
  cpp_flag = FALSE;
  debug_flag = FALSE;
  enum_flag = FALSE;
  export_flag = FALSE;
  split_argument = 0;  /* default value */
  no_minimization_flag = FALSE;
  IR_TOP_ADD_STRING (STANDARD_PREFIX);
  prefix = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  time_flag = FALSE;
  v_flag = FALSE;
  /* Process all command line options. */
  for (i = next_option (TRUE), okay = TRUE; i != 0; i = next_option (FALSE))
    {
      option = option_characteristics (i, &option_has_argument);
      if (option == NULL)
        {
          if (option_has_argument)
            fprintf (stderr, "oka: flag `%s' without argument\n",
                     argument_vector[i]);
          else
            fprintf (stderr, "oka: unknown flag `%s'\n", argument_vector[i]);
          okay = FALSE;
        }
      else if (strcmp (option, "-c++") == 0)
        cpp_flag = TRUE;
      else if (strcmp (option, "-debug") == 0)
        debug_flag = TRUE;
      else if (strcmp (option, "-enum") == 0)
        enum_flag = TRUE;
      else if (strcmp (option, "-export") == 0)
        export_flag = TRUE;
      else if (strcmp (option, "-no-minimization") == 0)
        no_minimization_flag = TRUE;
      else if (strcmp (option, "-split") == 0)
        {
          fprintf (stderr,
                   "oka: option `-split' has not been implemented yet\n");
          /* split_argument = atoi (argument_vector [i + 1]); */
        }
      else if (strcmp (option, "-p") == 0)
        {
          IR_TOP_ADD_STRING (argument_vector[i] + 2);
          prefix = IR_TOP_BEGIN ();
          IR_TOP_FINISH ();
        }
      else if (strcmp (option, "-time") == 0)
        time_flag = TRUE;
      else if (strcmp (option, "-v") == 0)
        v_flag = TRUE;
      else
        assert (FALSE);
    }
  if (split_argument < 0)
    {
      fprintf (stderr, "oka: negative value of option `-split'\n");
      okay = FALSE;
    }
  if (number_of_operands () != 1)
    {
      fprintf (stderr,
               "oka: one specification file must be on command line\n");
      okay = FALSE;
    }
  else
    {
      source_file_name = argument_vector[next_operand (TRUE)];
      if (strcmp (file_name_suffix (source_file_name),
                  STANDARD_INPUT_FILE_SUFFIX) != 0)
        {
          fprintf (stderr,
                   "oka: specification file must have suffix `%s'\n",
                   STANDARD_INPUT_FILE_SUFFIX);
          okay = FALSE;
        }
    }
  if (!okay)
    oka_finish (TRUE);
  all_time = create_ticker ();
  base_name = base_file_name (source_file_name);
  IR_TOP_ADD_MEMORY (base_name,
                     strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  description_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_ADD_MEMORY (base_name,
                     strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  if (cpp_flag)
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_CPP_IMPLEMENTATION_FILE_SUFFIX);
  else
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_C_IMPLEMENTATION_FILE_SUFFIX);
  output_implementation_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_ADD_MEMORY (base_name,
                     strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  IR_TOP_ADD_STRING (STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX);
  output_interface_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_ADD_MEMORY (base_name,
                     strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  IR_TOP_ADD_STRING (STANDARD_OUTPUT_DESCRIPTION_FILE_SUFFIX);
  output_description_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, user_signal_action);
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, user_signal_action);
  parse_time = create_ticker ();
  start_parser_file (source_file_name);
  yyparse ();
  finish_parser ();
  ticker_off (&parse_time);
  if (number_of_errors == 0)
    {
      analyze_time = create_ticker ();
      analyze_description ();
      ticker_off (&analyze_time);
      /* Output of errors only after `finish_parser' because it can
         flush last syntax error. */
      output_errors ();
      generation_time = create_ticker ();
      if (number_of_errors == 0)
        generate ();
      ticker_off (&generation_time);
    }
  if (time_flag)
    {
      fprintf (stderr, "Summary:\n");
      fprintf (stderr, "  parse time %s, ", active_time_string (parse_time));
      fprintf (stderr, "analyze time %s, ", active_time_string (analyze_time));
      fprintf
        (stderr, "generation time %s, ", active_time_string (generation_time));
      fprintf (stderr, "all time:%s\n", active_time_string (all_time));
    }
  oka_finish (FALSE);
  return 0;
}
