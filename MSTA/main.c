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

/* The following macro value is standard prefix of all output files
   when MSTA emulates YACC. */

#define STANDARD_FILE_PREFIX "y"

/* The following macro value is standard prefix of names of external
   names produced by MSTA.  The names affected shall include the
   functions yyparse(), yylex(), and yyerror(), and the variables
   yylval, yychar, and yydebug. (In the remainder of this clause, the
   six symbols cited are referenced using their default names only as
   a notational convenience.)  Local names may also be affected by the
   macro value; however, this value shall not affect MSTA-generated
   #define symbols when MSTA emulates YACC. */

#define STANDARD_SYM_PREFIX "yy"

/* The following macro value is suffix of name of the generated header
   file.  If this macro value is changed then documentation of MSTA
   must be changed. */

#define STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX ".h"

/* The following macro value is suffix of name of C generated code
   file.  If this macro value is changed then documentation of MSTA
   must be changed. */

#define STANDARD_OUTPUT_C_IMPLEMENTATION_FILE_SUFFIX ".c"

/* The following macro value is suffix of name of C++ generated code
   file.  If this macro value is changed then documentation of MSTA
   must be changed. */

#define STANDARD_OUTPUT_CPP_IMPLEMENTATION_FILE_SUFFIX ".cpp"

/* The following macro value is suffix of name of the generated parser
   description file.  If this macro value is changed then
   documentation of MSTA must be changed. */

#define STANDARD_OUTPUT_DESCRIPTION_FILE_SUFFIX ".output"

/* The following macro value is name (without suffix) of the generated
   parser interface file when MSTA emulates YACC. */

#define STANDARD_OUTPUT_YACC_INTERFACE_FILE_NAME "y.tab"

/* The following macro value is name (without suffix) of the generated
   parser implementation file when MSTA emulates YACC. */

#define STANDARD_OUTPUT_YACC_IMPLEMENTATION_FILE_NAME\
                            STANDARD_OUTPUT_YACC_INTERFACE_FILE_NAME

/* The following macro value is name (without suffix) of the generated
   parser description file when MSTA emulates YACC. */

#define STANDARD_OUTPUT_YACC_DESCRIPTION_FILE_NAME "y"

static void
error_function_for_allocate (void)
{
  error (TRUE, current_position, "fatal error -- no memory");
}


static const char *
file_name_suffix (const char *file_name)
     
{
  const char *last_period;

  for (last_period = NULL; *file_name != '\0'; file_name++)
    if (*file_name == '.')
      last_period = file_name;
  return (last_period == NULL ? file_name : last_period);
}

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

static void
msta_finish (int fatal_exit_flag)
{
  int exit_code;

  if (output_interface_file != NULL)
    fclose (output_interface_file);
  if (output_implementation_file != NULL)
    fclose (output_implementation_file);
  if (output_description_file != NULL)
    fclose (output_description_file);
  finish_single_definition_table ();
  finish_token_code_value_table ();
  finish_literal_definition_table ();
  finish_string_table ();
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "All hash table collisions - %d%%\n", all_hash_table_collisions ());
#endif
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

static void
msta_fatal_finish (void)
{
  msta_finish (TRUE);
}

static void
msta_start (void)
{
  change_allocation_error_function (error_function_for_allocate);
  initiate_positions ();
  initiate_errors (FALSE);
  IR_start ();
  initiate_string_table ();
  initiate_single_definition_table ();
  initiate_token_code_value_table ();
  initiate_literal_definition_table ();
  initiate_parser ();
  /* Initiation of some MSTA common variables. */
  fatal_error_function = msta_fatal_finish;
  output_interface_file = NULL;
  output_implementation_file = NULL;
  output_description_file = NULL;
}

static RETSIGTYPE
user_signal_action (int signal_number)
{
  msta_finish (TRUE);
}

/* The following macro value is description of MSTA command line (see
   package of `command-line'.  The description is subdivided on three
   parts only because there are constraints on string length in some C
   compilers. */

#ifdef NDEBUG

#define COMMAND_LINE_DESCRIPTION_PART1 \
"file_prefix sym_prefix name number\n"\
"%%\n"\
"command line: msta [ options ...] specification-file\n"\
"\n"\
"YACC compatible options:\n"\
"  `-d', `-define'  Create the header file.\n"\
"  `-l', `-line'    Produce code without #line constructs.\n"\
"  `-t', `-trace'   Permit compilation of debugging code in the code file.\n"\
"  `-v', `-verbose' Write a file containing description of the parser.\n"\
"  `-b file_prefix' Use file_prefix instead of y as the prefix for all\n"\
"                   output filenames (y.tab.c, y.tab.h, y.output).\n"\
"  `-p sym_prefix'  Use sym_prefix instead of yy as the prefix for all\n"\
"                   external names produced by yacc.\n"\
"\n"

#define COMMAND_LINE_DESCRIPTION_PART2 \
"New options: Default options are marked by star (*).\n"\
"  `-h', `-help', or `--help'\n"\
"                   Output this message\n"\
"  `-w'             Inhibit all warning messages\n"\
"  `-c++'           Generate c++ code instead of c\n"\
"  `-enum'          Generate token constants as enumeration instead of macro\n"\
"* `-no-enum'       Generate token constants as macro\n"\
"* `-pattern-equiv' Equiv patterns are denoted by the same nonterminal\n"\
"  `-no-pattern-equiv'\n"\
"                   Equiv patterns are denoted by different nonterminals and\n"\
"                   are expanded into the different set of rules\n"\
"  `-look-ahead number'\n"\
"  `-la number'\n"\
"                   Define k (default value: 1) for LR(k)-,LALR(k)-grammars.\n"\
"  `-lr'            Input grammar is considered as LR(k)-grammar.\n"\
"* `-no-lr'         Input grammar is considered as LALR(k)-grammar.\n"\
"* `-lalr-optimization'\n"\
"                   Extract LALR parts of input LR(k)-grammar and represent\n"\
"                   them by adequate LR-sets (only when option -lr is on)\n"\
"  `-no-lalr-optimization'\n"\
"                   Option is opposite to the previous one.\n"\
"* `-regular-optimization'\n"\
"                   Extract regular parts of input grammar and implement them\n"\
"                   by adequate methods (deterministic finite automaton).\n"\
"  `-no-regular-optimization'\n"\
"                   Option is opposite to the previous one.\n"\
"  `-split-lr-sets' Splitting LR-sets during regular optimization\n"\
"                   in order to extract more regular parts (only when option\n"\
"                   -regular-optimization is on).  Default if %scanner\n"\
"                   is present\n"\
"  `-no-split-lr-sets'\n"\
"                   Option is opposite to the previous one.  Default if\n"\
"                   %scanner is absent\n"

#define COMMAND_LINE_DESCRIPTION_PART3 \
"* `-yacc-error-recovery'\n"\
"                   Use error recover according to Posix YACC standard.\n"\
"  `-local-error-recovery'\n"\
"                   Use a local error recovery method.\n"\
"  `-minimal-error-recovery'\n"\
"                   Use minimal error recovery method.\n"\
"  `-yacc-input'    Only Posix YACC input can be used as input grammar.\n"\
"  `-strict'        Use only strict POSIX YACC input (when `-yacc-input')\n"\
"                   not SYSTEM V one.  Errors are output when nonstrict\n"\
"                   features are used.\n"\
"* `-no-strict'     When `-yacc-input', only warnings about non strict POSIX\n"\
"                   YACC features usage are output.\n"\
"  `-yacc-file-names'\n"\
"                   Output file names are generated according to Posix YACC.\n"\
"* `-no-yacc-file-names'\n"\
"                   Output files will have the same name and suffixes (.h,\n"\
"                   .c, and .output).\n"\
"  `-o name'        Specify names of output files (header file name.h,\n"\
"                   code file name.c(name.cpp), description file name.output)\n"\
"  `-y', `-yacc'    Emulate Posix YACC.  It means that the options\n"\
"                   -no-enum, -yacc-input, -lalr, -yacc-error-recovery,\n"\
"                   -yacc-file-names, and -no-expand are on.\n"\
"  `-full-lr-set'   Write full LR-sets into a file containing description\n"\
"                   of the parser (the option acts only with option -v).\n"\
"  `-lr-situation-context'\n"\
"                   Write context of LR-situations into description of the\n"\
"                   parser (the option acts only with option -v).\n"\
"  `-removed-lr-sets'\n"\
"                   Write LR-sets removed during conflict resolution and\n"\
"                   regular optimization into description of the\n"\
"                   parser (the option acts only with option -v).\n"\
"* `-expand'        Attributes & states in the parser will be expandable.\n"\
"  `-no-expand'     Option is opposite to the previous one.\n"\
"  `-time'          Output time statistics into stderr."

#else /* #ifdef NDEBUG */

#define COMMAND_LINE_DESCRIPTION_PART1 \
"file_prefix sym_prefix name number\n"\
"%%\n"\
"command line: msta [ options ...] specification-file\n"\
"\n"\
"YACC compatible options:\n"\
"  `-d', `-define'  Create the header file.\n"\
"  `-l', `-line'    Produce code without #line constructs.\n"\
"  `-t', `-trace'   Permit compilation of debugging code in the code file.\n"\
"  `-v', `-verbose' Write a file containing description of the parser.\n"\
"  `-b file_prefix' Use file_prefix instead of y as the prefix for all\n"\
"                   output filenames (y.tab.c, y.tab.h, y.output).\n"\
"  `-p sym_prefix'  Use sym_prefix instead of yy as the prefix for all\n"\
"                   external names produced by yacc.\n"\
"\n"

#define COMMAND_LINE_DESCRIPTION_PART2 \
"New options: Default options are marked by star (*).\n"\
"  `-h', `-help', or `--help'\n"\
"                   Output this message\n"\
"  `-w'             Inhibit all warning messages\n"\
"  `-c++'           Generate c++ code instead of c\n"\
"  `-enum'          Generate token constants as enumeration instead of macro\n"\
"* `-no-enum'       Generate token constants as macro\n"\
"* `-pattern-equiv' Equiv patterns are denoted by the same nonterminal\n"\
"  `-no-pattern-equiv'\n"\
"                   Equiv patterns are denoted by different nonterminals and\n"\
"                   are expanded into the different set of rules\n"\
"  `-look-ahead number'\n"\
"  `-la number'\n"\
"                   Define k (default value: 1) for LR(k)-,LALR(k)-grammars.\n"\
"  `-lr'            Input grammar is considered as LR(k)-grammar.\n"\
"* `-no-lr'         Input grammar is considered as LALR(k)-grammar.\n"\
"* `-lalr-optimization'\n"\
"                   Extract LALR parts of input LR(k)-grammar and represent\n"\
"                   them by adequate LR-sets (only when option -lr is on)\n"\
"  `-no-lalr-optimization'\n"\
"                   Option is opposite to the previous one.\n"\
"* `-regular-optimization'\n"\
"                   Extract regular parts of input grammar and implement them\n"\
"                   by adequate methods (deterministic finite automaton).\n"\
"  `-no-regular-optimization'\n"\
"                   Option is opposite to the previous one.\n"\
"  `-split-lr-sets' Splitting LR-sets during regular optimization\n"\
"                   in order to extract more regular parts (only when option\n"\
"                   -regular-optimization is on).  Default if %scanner\n"\
"                   is present\n"\
"  `-no-split-lr-sets'\n"\
"                   Option is opposite to the previous one.  Default if\n"\
"                   %scanner is absent\n"

#define COMMAND_LINE_DESCRIPTION_PART3 \
"* `-yacc-error-recovery'\n"\
"                   Use error recover according to Posix YACC standard.\n"\
"  `-local-error-recovery'\n"\
"                   Use a local error recovery method.\n"\
"  `-minimal-error-recovery'\n"\
"                   Use minimal error recovery method.\n"\
"  `-yacc-input'    Only Posix YACC input can be used as input grammar.\n"\
"* `-no-yacc-input' All input grammar features of MSTA can be used.\n"\
"  `-strict'        Use only strict POSIX YACC input (when `-yacc-input')\n"\
"                   not SYSTEM V one.  Errors are output when nonstrict\n"\
"                   features are used.\n"\
"* `-no-strict'     When `-yacc-input', only warnings about non strict POSIX\n"\
"                   YACC features usage are output.\n"\
"  `-yacc-file-names'\n"\
"                   Output file names are generated according to Posix YACC.\n"\
"* `-no-yacc-file-names'\n"\
"                   Output files will have the same name and suffixes (.h,\n"\
"                   .c, and .output).\n"\
"  `-o name'        Specify names of output files (header file name.h,\n"\
"                   code file name.c(name.cpp), description file name.output)\n"\
"  `-y', `-yacc'    Emulate Posix YACC.  It means that the options\n"\
"                   -no-enum, -yacc-input, -lalr, -yacc-error-recovery,\n"\
"                   and -yacc-file-names are on.\n"\
"  `-full-lr-set'   Write full LR-sets into a file containing description\n"\
"                   of the parser (the option acts only with option -v).\n"\
"  `-lr-situation-context'\n"\
"                   Write context of LR-situations into description of the\n"\
"                   parser (the option acts only with option -v).\n"\
"  `-removed-lr-sets'\n"\
"                   Write LR-sets removed during conflict resolution and\n"\
"                   regular optimization into description of the\n"\
"                   parser (the option acts only with option -v).\n"\
"* `-expand'        Attributes & states in the parser will be expandable.\n"\
"  `-no-expand'     Option is opposite to the previous one.\n"\
"  `-time'          Output time statistics into stderr.\n"\
"  `-debug number'  Output debug information (with given level>=0) into stderr."

#endif /* #ifdef NDEBUG */

void
main (int argc, char **argv)
{
  int i, okay;
  int option_has_argument;
  char *option;
  const char *base_source_file_name;
  ticker_t analyze_ticker, parse_ticker, all_ticker;
  vlo_t command_line_description;

  VLO_CREATE (command_line_description, 6000);
  VLO_ADD_STRING (command_line_description, COMMAND_LINE_DESCRIPTION_PART1);
  VLO_ADD_STRING (command_line_description, COMMAND_LINE_DESCRIPTION_PART2);
  VLO_ADD_STRING (command_line_description, COMMAND_LINE_DESCRIPTION_PART3);
  msta_start ();
  /* Tune the translator on command line arguments. */
  if (!start_command_line_processing
       (argc, argv, (char *) VLO_BEGIN (command_line_description)))
    {
      fprintf
        (stderr,
         "msta: invalid command line description -- type `msta' for help\n");
      msta_finish (TRUE);
    }
  if (argument_count == 1)
    {
      output_command_line_description ();
      msta_finish (TRUE);
    }
  define_flag = FALSE;
  line_flag = FALSE;
  trace_flag = FALSE;
  verbose_flag = FALSE;
  IR_TOP_ADD_STRING (STANDARD_FILE_PREFIX);
  file_prefix = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  IR_TOP_ADD_STRING (STANDARD_SYM_PREFIX);
  sym_prefix = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  full_lr_set_flag = FALSE;
  lr_situation_context_flag = FALSE;
  removed_lr_sets_flag = FALSE;
  w_flag = FALSE;
  cpp_flag = FALSE;
  enum_flag = FALSE;
  pattern_equiv_flag = TRUE;
  look_ahead_number = 1;
  lr_flag = FALSE;
  lalr_optimization_flag = TRUE;
  regular_optimization_flag = TRUE;
  split_lr_sets_flag_is_defined = FALSE;
  msta_error_recovery = YACC_ERROR_RECOVERY;
  yacc_input_flag = FALSE;
  strict_flag = FALSE;
  yacc_file_names_flag = FALSE;
  output_files_name = NULL;
  expand_flag = TRUE;
  time_flag = FALSE;
#ifndef NDEBUG
  debug_level = 0;
#endif
  /* Process generic command line options `-y', `-yacc'. */
  for (i = next_option (TRUE), okay = TRUE; i != 0; i = next_option (FALSE))
    {
      option = option_characteristics (i, &option_has_argument);
      if (option == NULL)
        {
          if (option_has_argument)
            fprintf (stderr, "%s: flag `%s' without argument\n",
                     *argument_vector, argument_vector [i]);
          else
            fprintf (stderr, "%s: unknown flag `%s'\n", *argument_vector,
                     argument_vector [i]);
          okay = FALSE;
        }
      else if (strcmp (option, "-y") == 0 || strcmp (option, "-yacc") == 0)
        {
          look_ahead_number = 1;
          lr_flag = FALSE;
          lalr_optimization_flag = FALSE;
          regular_optimization_flag = FALSE;
          split_lr_sets_flag = FALSE;
          split_lr_sets_flag_is_defined = TRUE;
          msta_error_recovery = YACC_ERROR_RECOVERY;
          enum_flag = FALSE;
          yacc_input_flag = TRUE;
          strict_flag = FALSE;
          yacc_file_names_flag = TRUE;
          cpp_flag = FALSE;
        }
      else if (strcmp (option, "-h") == 0 || strcmp (option, "-help") == 0
               || strcmp (option, "--help") == 0)
        {
          output_command_line_description ();
          msta_finish (TRUE);
        }
    }
  if (!okay)
    msta_finish (TRUE);
  /* Process others command line options. */
  for (i = next_option (TRUE), okay = TRUE; i != 0; i = next_option (FALSE))
    {
      option = option_characteristics (i, &option_has_argument);
      if (option == NULL)
        break;
      if (strcmp (option, "-d") == 0 || strcmp (option, "-define") == 0)
        define_flag = TRUE;
      else if (strcmp (option, "-l") == 0 || strcmp (option, "-line") == 0)
        line_flag = TRUE;
      else if (strcmp (option, "-t") == 0 || strcmp (option, "-trace") == 0)
        trace_flag = TRUE;
      else if (strcmp (option, "-v") == 0 || strcmp (option, "-verbose") == 0)
        verbose_flag = TRUE;
      else if (strcmp (option, "-b") == 0)
        {
          IR_TOP_ADD_STRING (argument_vector[i + 1]);
          file_prefix = IR_TOP_BEGIN ();
          IR_TOP_FINISH ();
        }
      else if (strcmp (option, "-p") == 0)
        {
          IR_TOP_ADD_STRING (argument_vector[i + 1]);
          sym_prefix = IR_TOP_BEGIN ();
          IR_TOP_FINISH ();
        }
      else if (strcmp (option, "-w") == 0)
        w_flag = TRUE;
      else if (strcmp (option, "-c++") == 0)
        cpp_flag = TRUE;
      else if (strcmp (option, "-enum") == 0)
        enum_flag = TRUE;
      else if (strcmp (option, "-no-enum") == 0)
        enum_flag = FALSE;
      else if (strcmp (option, "-pattern-equiv") == 0)
        pattern_equiv_flag = TRUE;
      else if (strcmp (option, "-no-pattern-equiv") == 0)
        pattern_equiv_flag = FALSE;
      else if (strcmp (option, "-full-lr-set") == 0)
        full_lr_set_flag = TRUE;
      else if (strcmp (option, "-lr-situation-context") == 0)
        lr_situation_context_flag = TRUE;
      else if (strcmp (option, "-removed-lr-sets") == 0)
        removed_lr_sets_flag = TRUE;
      else if (strcmp (option, "-look-ahead") == 0
               || strcmp (option, "-la") == 0)
        {
          look_ahead_number = atoi (argument_vector [i + 1]);
          if (look_ahead_number <= 0)
            look_ahead_number = 1;
        }
      else if (strcmp (option, "-lr") == 0)
        lr_flag = TRUE;
      else if (strcmp (option, "-no-lr") == 0)
        lr_flag = FALSE;
      else if (strcmp (option, "-lalr-optimization") == 0)
        lalr_optimization_flag = TRUE;
      else if (strcmp (option, "-no-lalr-optimization") == 0)
        lalr_optimization_flag = FALSE;
      else if (strcmp (option, "-regular-optimization") == 0)
        regular_optimization_flag = TRUE;
      else if (strcmp (option, "-no-regular-optimization") == 0)
        regular_optimization_flag = FALSE;
      else if (strcmp (option, "-split-lr-sets") == 0)
        {
          split_lr_sets_flag = TRUE;
          split_lr_sets_flag_is_defined = TRUE;
        }
      else if (strcmp (option, "-no-split-lr-sets") == 0)
        {
          split_lr_sets_flag = FALSE;
          split_lr_sets_flag_is_defined = TRUE;
        }
      else if (strcmp (option, "-yacc-error-recovery") == 0)
        msta_error_recovery = YACC_ERROR_RECOVERY;
      else if (strcmp (option, "-local-error-recovery") == 0)
        msta_error_recovery = LOCAL_ERROR_RECOVERY;
      else if (strcmp (option, "-minimal-error-recovery") == 0)
        msta_error_recovery = MINIMAL_ERROR_RECOVERY;
      else if (strcmp (option, "-yacc-input") == 0)
        yacc_input_flag = TRUE;
      else if (strcmp (option, "-no-yacc-input") == 0)
        yacc_input_flag = FALSE;
      else if (strcmp (option, "-strict") == 0)
        strict_flag = TRUE;
      else if (strcmp (option, "-no-strict") == 0)
        strict_flag = FALSE;
      else if (strcmp (option, "-yacc-file-names") == 0)
        yacc_file_names_flag = TRUE;
      else if (strcmp (option, "-no-yacc-file-names") == 0)
        yacc_file_names_flag = FALSE;
      else if (strcmp (option, "-time") == 0)
        time_flag = TRUE;
      else if (strcmp (option, "-expand") == 0)
        expand_flag = TRUE;
      else if (strcmp (option, "-no-expand") == 0)
        expand_flag = FALSE;
#ifndef NDEBUG
      else if (strcmp (option, "-debug") == 0)
        debug_level = atoi (argument_vector[i + 1]);
#endif
      else if (strcmp (option, "-o") == 0)
        output_files_name = argument_vector [i + 1];
      else
        assert (strcmp (option, "-y") == 0 || strcmp (option, "-yacc") == 0);
    }
  if (number_of_operands () != 1)
    {
      fprintf (stderr,
               "msta: one specification file must be on command line\n");
      okay = FALSE;
    }
  else
    {
      source_file_name = argument_vector [next_operand (TRUE)];
      if (output_files_name == NULL)
        {
          base_source_file_name = base_file_name (source_file_name);
          IR_TOP_ADD_MEMORY (base_source_file_name,
                             strlen (base_source_file_name)
                             - strlen (file_name_suffix (source_file_name)));
          IR_TOP_ADD_BYTE ('\0');
          output_files_name = IR_TOP_BEGIN ();
          IR_TOP_FINISH ();
        }
      if (!yacc_file_names_flag
          && strcmp (file_name_suffix (source_file_name),
                     STANDARD_INPUT_FILE_SUFFIX) != 0)
        {
          fprintf (stderr,
                   "msta: specification file must have suffix `%s'\n",
                   STANDARD_INPUT_FILE_SUFFIX);
          okay = FALSE;
        }
    }
  if (!okay)
    msta_finish (TRUE);
  max_look_ahead_number = look_ahead_number;
  if (yacc_file_names_flag)
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_YACC_IMPLEMENTATION_FILE_NAME);
  else
    IR_TOP_ADD_STRING (output_files_name);
  if (cpp_flag)
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_CPP_IMPLEMENTATION_FILE_SUFFIX);
  else
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_C_IMPLEMENTATION_FILE_SUFFIX);
  output_implementation_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  if (yacc_file_names_flag)
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_YACC_INTERFACE_FILE_NAME);
  else
    IR_TOP_ADD_STRING (output_files_name);
  IR_TOP_ADD_STRING (STANDARD_OUTPUT_INTERFACE_FILE_SUFFIX);
  output_interface_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  if (yacc_file_names_flag)
    IR_TOP_ADD_STRING (STANDARD_OUTPUT_YACC_DESCRIPTION_FILE_NAME);
  else
    IR_TOP_ADD_STRING (output_files_name);
  IR_TOP_ADD_STRING (STANDARD_OUTPUT_DESCRIPTION_FILE_SUFFIX);
  output_description_file_name = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, user_signal_action);
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, user_signal_action);
  all_ticker = create_ticker ();
  parse_ticker = create_ticker ();
  start_parser_file (source_file_name);
  yyparse ();
  finish_parser ();
  if (time_flag)
    fprintf (stderr, "  parser time            -- %ssec\n",
             active_time_string (parse_ticker));
  analyze_ticker = create_ticker ();
  analyze_description ();
  if (time_flag)
    fprintf (stderr, "  semantic analyzer time -- %ssec\n",
             active_time_string (analyze_ticker));
  /* Output of errors only after `finish_parser' because it can flush
     last syntax error. */
  output_errors ();
  if (number_of_errors == 0)
    generate ();
  if (time_flag)
    fprintf (stderr, "overall time -- %ssec\n",
             active_time_string (all_ticker));
  msta_finish (FALSE);
}
