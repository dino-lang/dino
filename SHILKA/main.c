/*
   FILE NAME:   main.c

   Copyright (C) 1997-2005 Vladimir Makarov.

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

   TITLE:       Driver of Shilka (keywords description translator)

   DESCRIPTION: This file processes command line of Shilka, initiates and
                finishes abstract data common for all Shilka.  The file
                also sets up reaction on fatal errors and user's
                signals, and initiates all Shilka common variables.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of file work.
   
*/

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
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
#include "ticker.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
#include "yacc.h"
#include "anal.h"
#include "gen.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

/* The following macro value is standard prefix of names of keywords
   recognizer objects.  If this macro value is changed then
   documentation of Shilka and macro OPTIONS must be changed. */

#define STANDARD_PREFIX "KR_"

/* The following macro value is suffix of name of description file of
   keyword description translator.  If this macro value is changed
   then documentation of Shilka must be changed. */

#define STANDARD_OUTPUT_DESCRIPTION_FILE_SUFFIX ".output"

/* The following macro value is suffix of name of interface file of
   keywords recognizer.  If this macro value is changed then
   documentation of Shilka must be changed. */

#define STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX ".h"

/* The following macro value is suffix of name of C implementation
   file of keywords recognizer.  If this macro value is changed then
   documentation of Shilka must be changed. */

#define STANDARD_OUTPUT_C_IMPLEMENTATION_FILE_SUFFIX ".c"

/* The following macro value is suffix of name of C++ implementation
   file of keywords recognizer.  If this macro value is changed then
   documentation of Shilka must be changed. */

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
   first char after last `/' (or `\' for WIN32)in given file name,
   given file name itself if the directory name is absent.  The
   returned string can not be changed. */

static const char *
base_file_name (const char *file_name)
{
  int directory_name_length;

  directory_name_length = strlen (file_name);
#ifdef WIN32
  while (directory_name_length >= 0
         && file_name[directory_name_length] != '/'
         && file_name[directory_name_length] != '\\')
#else
  while (directory_name_length >= 0 && file_name[directory_name_length] != '/')
#endif
    directory_name_length--;
  return file_name + directory_name_length + 1;
}

/* The function finishes abstract data common for all Shilka, closes
   output files, deletes them if exit will be non-normal, and exits
   with code `1' if parameter value is TRUE or error was fixed,
   otherwise exits with code `0'. */

static void
shilka_finish (int fatal_exit_flag)
{
  int exit_code;

  if (output_interface_file != NULL)
    fclose (output_interface_file);
  if (output_implementation_file != NULL)
    fclose (output_implementation_file);
  if (output_description_file != NULL)
    fclose (output_description_file);
  finish_keyword_table ();
  finish_string_table ();
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
   `errors'.  The function calls `shilka_finish' for non-normal exit.  */

static void
shilka_fatal_finish (void)
{
  shilka_finish (TRUE);
}

/* The function initiates abstract data common for all Shilka and some
   Shilka common variables and sets up reaction on fatal errors. */

static void
shilka_start (void)
{
  change_allocation_error_function (error_function_for_allocate);
  initiate_positions ();
  initiate_errors (FALSE);
  IR_start ();
  initiate_string_table ();
  initiate_keyword_table ();
  initiate_parser ();
  /* Initiation of some Shilka common variables. */
  fatal_error_function = shilka_fatal_finish;
  output_interface_file = NULL;
  output_implementation_file = NULL;
  output_description_file = NULL;
}

/* The function is reaction on user's signals (interrupt and
   terminate).  The function calls `shilka_finish' for non-normal
   exit. */

static RETSIGTYPE
user_signal_action (int signal_number)
{
  shilka_finish (TRUE);
}

/* The following macro value is description of Shilka command line (see
   package of `command-line'. */

#ifndef NDEBUG

#define COMMAND_LINE_DESCRIPTION \
"prefix n\n"\
"%%\n"\
"command line: shilka [option ...] specification-file\n"\
"\n"\
"`-c++'        output C++ code instead of C code\n"\
"`-statistics' generate macro switching on gathering and printing\n"\
"              keyword usage statistics\n"\
"`-inline'     generation of inline function (take care with C compiler)\n"\
"`-debug'      output debugging information during Shilka run\n"\
"`-strip'      strip code for gathering and printing statistics\n"\
"`-length'     usage of strncmp instead of strcmp to compare keywords\n"\
"`-case'       keywords case is ignored\n"\
"`-no-definitions'\n"\
"              no generation of macros(enum) defining identifiers of keywords\n"\
"`-interface'  generation of additional interface file (with suffix .h)\n"\
"`-export'     macros defining identifiers of keywords are in interface file\n"\
"`-enum'       output enumeration instead of macro definitions\n"\
"`-pprefix'    use 'prefix' instead of 'KR_' for names of generated objects\n"\
"`-time'       output time statistics into stderr\n"\
"`-fast n'     If number of rest unchecked characters is less or equal than n,\n"\
"              use these characters comparison instead strcmp (default n = 3).\n"\
"`-w'          no warning output\n"\
"`-h', `-help' output this message\n"\
"`-v'          create description file"

#else /* #ifndef NDEBUG */

#define COMMAND_LINE_DESCRIPTION \
"prefix n\n"\
"%%\n"\
"command line: shilka [option ...] specification-file\n"\
"\n"\
"`-c++'        output C++ code instead of C code\n"\
"`-statistics' generate macro switching on gathering and printing\n"\
"              keyword usage statistics\n"\
"`-inline'     generation of inline function (take care with C compiler)\n"\
"`-strip'      strip code for gathering and printing statistics\n"\
"`-length'     usage of strncmp instead of strcmp to compare keywords\n"\
"`-case'       keywords case is ignored\n"\
"`-no-definitions'\n"\
"              no generation of macros(enum) defining identifiers of keywords\n"\
"`-interface'  generation of additional interface file (with suffix .h)\n"\
"`-export'     macros defining identifiers of keywords are in interface file\n"\
"`-enum'       output enumeration instead of macro definitions\n"\
"`-pprefix'    use 'prefix' instead of 'KR_' for names of generated objects\n"\
"`-time'       output time statistics into stderr\n"\
"`-fast n'     If number of rest unchecked characters is less or equal than n,\n"\
"              use these characters comparison instead strcmp (default n = 3).\n"\
"`-h', `-help' output this message\n"\
"`-w'          no warning output\n"\
"`-v'          create description file"

#endif /* #ifndef NDEBUG */

/* The function processes command line of Shilka and initiates some
   common Shilka variables.  The function also calls parser, semantic
   analyzer, and generator of Shilka and sets up reaction on user's
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

  shilka_start ();
  /* Tune the translator on command line arguments. */
  if (!start_command_line_processing (argc, argv, COMMAND_LINE_DESCRIPTION))
    {
      fprintf (stderr, "shilka: invalid command line description\n");
      shilka_finish (TRUE);
    }
  if (argument_count == 1)
    {
      output_command_line_description ();
      shilka_finish (TRUE);
    }
  cpp_flag = FALSE;
#ifndef NDEBUG
  debug_flag = FALSE;
#endif
  statistics_flag = FALSE;
  inline_flag = FALSE;
  strip_flag = FALSE;
  interface_flag = FALSE;
  enum_flag = FALSE;
  length_flag = FALSE;
  case_flag = FALSE;
  no_definitions_flag = FALSE;
  export_flag = FALSE;
  fast_number = 3;
  IR_TOP_ADD_STRING (STANDARD_PREFIX);
  prefix = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  time_flag = FALSE;
  w_flag = FALSE;
  v_flag = FALSE;
  /* Process all command line options. */
  for (i = next_option (TRUE), okay = TRUE; i != 0; i = next_option (FALSE))
    {
      option = option_characteristics (i, &option_has_argument);
      if (option == NULL)
        {
          if (option_has_argument)
            fprintf (stderr, "shilka: flag `%s' without argument\n",
                     argument_vector[i]);
          else
            fprintf (stderr,
                     "shilka: unknown flag `%s'\n", argument_vector[i]);
          okay = FALSE;
        }
      else if (strcmp (option, "-c++") == 0)
        cpp_flag = TRUE;
#ifndef NDEBUG
      else if (strcmp (option, "-debug") == 0)
        debug_flag = TRUE;
#endif
      else if (strcmp (option, "-statistics") == 0)
        statistics_flag = TRUE;
      else if (strcmp (option, "-inline") == 0)
        inline_flag = TRUE;
      else if (strcmp (option, "-strip") == 0)
        strip_flag = TRUE;
      else if (strcmp (option, "-interface") == 0)
        interface_flag = TRUE;
      else if (strcmp (option, "-enum") == 0)
        enum_flag = TRUE;
      else if (strcmp (option, "-length") == 0)
        length_flag = TRUE;
      else if (strcmp (option, "-case") == 0)
        case_flag = TRUE;
      else if (strcmp (option, "-no-definitions") == 0)
        no_definitions_flag = TRUE;
      else if (strcmp (option, "-export") == 0)
        export_flag = TRUE;
      else if (strcmp (option, "-p") == 0)
        {
          IR_TOP_ADD_STRING (argument_vector[i] + 2);
          prefix = IR_TOP_BEGIN ();
          IR_TOP_FINISH ();
        }
      else if (strcmp (option, "-time") == 0)
        time_flag = TRUE;
      else if (strcmp (option, "-fast") == 0)
        {
          fast_number = atoi (argument_vector [i + 1]);
          if (fast_number < 0)
            fast_number = 0;
        }
      else if (strcmp (option, "-w") == 0)
        w_flag = TRUE;
      else if (strcmp (option, "-v") == 0)
        v_flag = TRUE;
      else if (strcmp (option, "-h") == 0 || strcmp (option, "-help") == 0) {
        output_command_line_description ();
        shilka_finish (TRUE);
      }
      else
        assert (FALSE);
    }
  if (export_flag && !interface_flag)
    fprintf
      (stderr, "shilka: `-export' is ignored when `-interface' is absent\n");
  if ((export_flag || enum_flag) && no_definitions_flag)
    fprintf
      (stderr,
       "shilka: `-export' and `-enum' are ignored when `-no-definitions'\n");
  if (number_of_operands () != 1)
    {
      fprintf (stderr,
               "shilka: one specification file must be on command line\n");
      okay = FALSE;
    }
  else
    {
      source_file_name = argument_vector[next_operand (TRUE)];
      if (strcmp (file_name_suffix (source_file_name),
                  STANDARD_INPUT_FILE_SUFFIX) != 0)
        {
          fprintf (stderr,
                   "shilka: specification file must have suffix `%s'\n",
                   STANDARD_INPUT_FILE_SUFFIX);
          okay = FALSE;
        }
    }
  if (!okay)
    shilka_finish (TRUE);
  if (statistics_flag && strip_flag)
    {
      fprintf (stderr, "shilka: `-strip' is ignored when `-statistics'\n");
      strip_flag = FALSE;
    }
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
      fprintf (stderr, "generation time %s, ",
               active_time_string (generation_time));
      fprintf (stderr, "all time:%s\n", active_time_string (all_time));
    }
  shilka_finish (FALSE);
}
