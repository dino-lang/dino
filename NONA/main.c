/*
   FILE NAME:   main.c

   TITLE:       Driver of NONA (code selector description translator)

   DESCRIPTION: This file processes command line of NONA, initiates and
                finishes abstract data common for all NONA.  The file
                also sets up reaction on fatal errors and user's
                signals, and initiates all NONA common variables.

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


/* The following macro value is standard prefix of names of code
   selector description (CS) objects.  If this macro value is changed
   then documentation of nona and macro OPTIONS must be changed. */

#define STANDARD_PREFIX "CS"

/* The following macro value is suffix of name of interface file of
   code selector description.  If this macro value is changed then
   documentation of nona must be changed. */

#define STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX ".h"

/* The following macro value is suffix of name of implementation file
   on C of code selector description.  If this macro value is changed
   then documentation of nona must be changed. */

#define STANDARD_OUTPUT_C_IMPLEMENTATION_FILE_SUFFIX ".c"

/* The following macro value is suffix of name of implementation file
   on C++ of code selector description.  If this macro value is
   changed then documentation of nona must be changed. */

#define STANDARD_OUTPUT_CPP_IMPLEMENTATION_FILE_SUFFIX ".cpp"

/* The following function is nonstandard reaction on error `no memory'
   of allocation package. */

static void
error_function_for_allocate (void)
{
  error (TRUE, current_position, "fatal error -- no memory");
}

/* The following function returns pointer to first char (it is `.') of
   suffix of given file name, empty string if the suffix is absent.
   The returned string can not be changed. */

static const char *
file_name_suffix (const char *file_name)
     
{
  const char *last_period;

  for (last_period = NULL; *file_name != '\0'; file_name++)
    if (*file_name == '.')
      last_period = file_name;
  return (last_period == NULL ? file_name : last_period);
}

/* The function returns pointer to first char after last `/' (or `\'
   for WIN32) in given file name, given file name itself if the
   directory name is absent.  The returned string can not be
   changed. */

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

/* The following function finishes abstract data common for all NONA,
   closes output files, deletes them if exit will be non-normal, and
   exits with code `1' if parameter value is TRUE or error was fixed,
   otherwise exits with code `0'. */

static void
nona_finish (int fatal_exit_flag)
{
  int exit_code;

  if (output_interface_file != NULL)
    fclose (output_interface_file);
  if (output_implementation_file != NULL)
    fclose (output_implementation_file);
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
    }
  exit (exit_code);
}

/* The following function is nonstandard reaction on fatal error of
   package `errors'.  The function calls `nona_finish' for non-normal
   exit. */

static void
nona_fatal_finish (void)
{
  nona_finish (TRUE);
}

/* The following function initiates abstract data common for all NONA
   and some NONA common variables and sets up reaction on fatal
   errors. */

static void
nona_start (void)
{
  change_allocation_error_function (error_function_for_allocate);
  initiate_positions ();
  initiate_errors (FALSE);
  IR_start ();
  initiate_identifier_table ();
  initiate_single_declaration_table ();
  initiate_parser ();
  /* Initiation of some NONA common variables. */
  fatal_error_function = nona_fatal_finish;
  output_interface_file = NULL;
  output_implementation_file = NULL;
}

/* The following function is reaction on user's signals (interrupt and
   terminate).  The function calls `nona_finish' for non-normal
   exit. */

static RETSIGTYPE
user_signal_action (int signal_number)
{
  nona_finish (TRUE);
}

/* The following macro value is description of NONA command line (see
   package of `command-line'. */

#define COMMAND_LINE_DESCRIPTION \
"prefix\n"\
"%%\n"\
"command line: nona [ -v -debug -export -pprefix] specification-file\n"\
"\n"\
"`-c++'      generation of C++ code\n"\
"`-v'        output statistic information to standard output stream\n"\
"`-debug'    output debugging information during execution of generated code\n"\
"`-export'   generation of macros defining identifiers of terminals\n"\
"`-pprefix'  use 'prefix' instead of 'CS' for names of generated objects"

/* The following function processes command line of NONA and initiates
   some common NONA variables.  The function also calls parser,
   semantic analyzer, and generator of NONA and sets up reaction on
   user's signals. */

void
main (int argc, char **argv)
{
  int i, okay;
  int option_has_argument;
  char *option;
  const char *base_name;
  char *source_file_name;

  nona_start ();
  /* Tune the translator on command line arguments. */
  if (!start_command_line_processing (argc, argv, COMMAND_LINE_DESCRIPTION))
    {
      fprintf (stderr, "nona: invalid command line description\n");
      nona_finish (TRUE);
    }
  if (argument_count == 1)
    {
      output_command_line_description ();
      nona_finish (TRUE);
    }
  v_flag = FALSE;
  cpp_flag = FALSE;
  debug_flag = FALSE;
  export_flag = FALSE;
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
            fprintf (stderr, "nona: flag `%s' without argument\n",
                     argument_vector[i]);
          else
            fprintf (stderr, "nona: unknown flag `%s'\n", argument_vector[i]);
          okay = FALSE;
        }
      else if (strcmp (option, "-c++") == 0)
        cpp_flag = TRUE;
      else if (strcmp (option, "-v") == 0)
        v_flag = TRUE;
      else if (strcmp (option, "-debug") == 0)
        debug_flag = TRUE;
      else if (strcmp (option, "-export") == 0)
        export_flag = TRUE;
      else if (strcmp (option, "-p") == 0)
        {
          IR_TOP_ADD_STRING (argument_vector[i] + 2);
          prefix = IR_TOP_BEGIN ();
          IR_TOP_FINISH ();
        }
      else
        assert (FALSE);
    }
  if (number_of_operands () != 1)
    {
      fprintf (stderr,
               "nona: one specification file must be on command line\n");
      okay = FALSE;
    }
  else
    {
      source_file_name = argument_vector[next_operand (TRUE)];
      if (strcmp (file_name_suffix (source_file_name),
                  STANDARD_INPUT_FILE_SUFFIX) != 0)
        {
          fprintf (stderr,
                   "nona: specification file must have suffix `%s'\n",
                   STANDARD_INPUT_FILE_SUFFIX);
          okay = FALSE;
        }
    }
  if (!okay)
    nona_finish (TRUE);
  base_name = base_file_name (source_file_name);
  IR_TOP_ADD_MEMORY (base_name,
                     strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  description_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_ADD_MEMORY (base_name,
                     strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  if (!cpp_flag)
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_C_IMPLEMENTATION_FILE_SUFFIX);
  else
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_CPP_IMPLEMENTATION_FILE_SUFFIX);
  output_implementation_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_ADD_MEMORY (base_name,
                     strlen (base_name) - strlen (STANDARD_INPUT_FILE_SUFFIX));
  IR_TOP_ADD_BYTE ('\0');
  IR_TOP_ADD_STRING (STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX);
  output_interface_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, user_signal_action);
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, user_signal_action);
  start_parser_file (source_file_name);
  yyparse ();
  finish_parser ();
  analyze_program ();
  /* Output of errors only after `finish_parser' because it can flush
     last syntax error. */
  output_errors ();
  if (number_of_errors == 0)
    generate ();
  nona_finish (FALSE);
}
