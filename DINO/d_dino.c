/*
   Copyright (C) 1997-2002 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of interpreter of DINO.

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

#include <setjmp.h>
#include "commline.h"
#include "d_common.h"
#include "d_ir.h"
#include "d_run.h"
#include "d_yacc.h"
#include "d_runtab.h"
#include "d_context.h"
#include "d_eval.h"

#ifdef HAVE_TIME_H
#include <time.h>
#else
extern clock_t clock (void);
#endif



/* This page contains functions common for all package functions. */

#ifndef HAVE_MEMCPY

void *
memcpy (void *to, const void *from, size_t size)
{
  char *cto = (char *) to;
  const char *cfrom = (const char *) from;

  while (size > 0)
    {
      *cto++ = *cfrom;
      size--;
    }
  return to;
}

#endif /* #ifndef HAVE_MEMCPY */

#ifndef HAVE_MEMSET

void *
memset (void *to, int value, size_t size)
{
  char *cto  = (char *) to;

  while (size > 0)
    {
      *cto++ = value;
      size--;
    }
  return to;
}

#endif /* #ifndef HAVE_MEMSET */

#ifndef HAVE_MEMCMP

int
memcmp (const void *mem1, const void *mem2, size_t size)
{
  const unsigned char *m1 = mem1;
  const unsigned char *m2 = mem2;

  while (size > 0)
    {
      if (m1 != m2)
        return (m1 < m2 ? -1 : 1);
      m1++;
      m2++;
      size--;
    }
  return 0;
}

#endif /* #ifndef HAVE_MEMCMP */


#ifndef HAVE_MEMMOVE

/* The following function is an analog of standard C function
   `memmove'.  The function returns the first operand. */

void *
memmove (void *s1, const void *s2, size_t n)
{
  int i;

  assert (n >= 0);
  if ((char *) s1 < (char *) s2 && (char *) s1 + n <= (char *) s2
      || (char *) s2 < (char *) s1 && (char *) s2 + n <= (char *) s1)
    return (void *) memcpy (s1, s2, n);
  if ((char *) s1 < (char *) s2 && (char *) s1 + n > (char *) s2)
    for (i = 0; (size_t) i < n; i++)
      ((char *) s1) [i] = ((char *) s2) [i];
  else
    for (i = n - 1; i >= 0; i--)
      ((char *) s1)[i] = ((char *) s2) [i];
  return s1;
}

#endif /* #ifndef HAVE_MEMMOVE */



/* The following value is array of directories in which we search
   for DINO programs. */
const char **include_path_directories;

/* Place for storing the vector mentioned above. */
static vlo_t include_path_directories_vector;

/* The following value is libraries which we search for DINO extern
   functions. */
const char **libraries;

/* Place for storing the vector mentioned above. */
static vlo_t libraries_vector;

/* The value of the following var is not NULL when the program is
   given on the command line.  In this case its value is the
   program. */
char *command_line_program = NULL;

/* The following variable values is number of dino program arguments,
   arguments themselves, and environment. */
int program_arguments_number;
char **program_arguments;
char **program_environment;

/* The value of the following var is changed for syntactic, semantic
   analyses and generation times.  The var stores current number of
   processed source position of processed language construction.  We
   never refer for varibale value only set it up. */
position_t source_position;

/* Jump buffer for exit. */
static jmp_buf exit_longjump_buff;

/* The following func returns pointer to first char (it is `.') of
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

static int
get_first_nondigit (const char *s)
{
  while (isdigit (*s))
    s++;
  return *s;
}

void
dino_finish (int code)
{
  char unit;
  int size;

  if (code >= 0)
    final_call_destroy_functions ();
#ifndef NO_PROFILE
  if (code == 0 && profile_flag)
    print_profile (first_program_stmt);
#endif
  finish_run_tables ();
  IR_stop ();
  delete_table ();
  finish_scanner ();
  output_errors ();
  finish_errors ();
  finish_positions ();
  VLO_DELETE (include_path_directories_vector);
  VLO_DELETE (libraries_vector);
  if (statistics_flag && code == 0)
    {
      finish_heap ();
      size = heap_size;
      unit = 'b';
      if (size % 1024 == 0)
	{
	  unit = 'k';
	  size /= 1024;
	}
      if (size % 1024 == 0)
	{
	  unit = 'm';
	  size /= 1024;
	}
      fprintf (stderr, "Heap size - %d%c, heap chunks - %d\n",
	       size, unit, heap_chunks_number);
      if (gc_number != 0)
	fprintf (stderr,
		 "GC - %d times, average free memory after GC - %d%%\n",
		 gc_number, free_gc_memory_percent);
      if (tab_collisions != 0)
	fprintf (stderr, "Tables collisions - %d\n", tab_collisions);
      if (tab_expansions != 0)
	fprintf (stderr, "Tables expansions - %d\n", tab_expansions);
    }
  longjmp (exit_longjump_buff, (code == 0 ? -1 : code < 0 ? 1 : code));
}

static void
dino_fatal_finish (void)
{
  dino_finish (1);
}

static void
error_func_for_allocate (void)
{
  error (FALSE, IR_pos (cpc), ERR_no_memory);
  dino_finish (-1);
}

static void
dino_start (void)
{
  change_allocation_error_function (error_func_for_allocate);
  initiate_positions ();
  initiate_errors (FALSE);
  fatal_error_function = dino_fatal_finish;
  initiate_table ();
  initiate_icode (); /* only after initiate table */
  initiate_scanner ();
  source_position = no_position;
  VLO_CREATE (include_path_directories_vector, 0);
  VLO_CREATE (libraries_vector, 0);
  initiate_run_tables ();
}

static void set_exception_action (int signal_number);

/* The following func is signal handler of an exception. */
static void
exception_action (int signal_number)
{
  IR_node_t class;
  const char *message;

  switch (signal_number)
    {
    case SIGINT:
      class = sigint_decl;
      message = ERR_interrupt_exception;
      break;
    case SIGILL:
      class = sigill_decl;
      message = ERR_illegal_instruction_exception;
      break;
    case SIGABRT:
      class = sigabrt_decl;
      message = ERR_abort_exception;
      break;
    case SIGFPE:
#ifdef WIN32
      _fpreset ();
#endif
      class = sigfpe_decl;
      message = ERR_floating_point_exception;
      break;
    case SIGTERM:
      class = sigterm_decl;
      message = ERR_termination_exception;
      break;
    case SIGSEGV:
      class = sigsegv_decl;
      message = ERR_segment_access_violation_exception;
      break;
#if ! defined (NO_PROFILE) && HAVE_SETITIMER
    case SIGVTALRM:
      if (profile_flag)
	{
	  profile_interrupt ();
	  return;
	}
      /* Fall through */
#endif 
    default:
      assert (FALSE);
      break;
    }
  set_exception_action (signal_number);
  if (eval_long_jump_set_flag)
    eval_error (class, signals_decl, IR_pos (cpc), message);
  else if (signal_number != SIGINT && signal_number != SIGTERM)
    error (TRUE, IR_pos (cpc), message);
  else
    dino_finish (1);
}

/* The following func sets up signal handler of an exception. */
static void
set_exception_action (int signal_number)
{
#if defined (HAVE_SIGACTION) && defined (HAVE_SIGEMPTYSET)
  struct sigaction action, old_action;

  action.sa_handler = exception_action;
  sigemptyset (&action.sa_mask);
#ifdef SA_NOMASK
  action.sa_flags = SA_NOMASK;
#else
  action.sa_flags = SA_NODEFER;
#endif
  sigaction (signal_number, &action,  &old_action);
#else
  signal (signal_number, exception_action);
#endif
}

/* Add directories from environment variable DINO_INCLUDE_PATH_NAME_VARIABLE
   (if any). */
static
void add_dino_path (const char *prefix, const char *subdir,
		    const char *string, vlo_t *vector_ptr)
{
  const char *s;
  char bound;
  int len;

  assert (prefix != NULL || subdir == NULL);
  if (prefix != NULL)
    {
      len = strlen (prefix);
#ifdef WIN32
      if (len != 0 && prefix [len - 1] == '\\')
#else
      if (len != 0 && prefix [len - 1] == '/')
#endif
	len--;
      IR_TOP_EXPAND (len + 1 /* '/' */ + 1 /* '\0' */
		     + (subdir == NULL ? 0 : strlen (subdir)));
      memcpy ((char *) IR_TOP_BEGIN (), prefix, len);
#ifdef WIN32
      ((char *) IR_TOP_BEGIN ()) [len] = '\\';
#else
      ((char *) IR_TOP_BEGIN ()) [len] = '/';
#endif
      ((char *) IR_TOP_BEGIN ()) [len + 1] = '\0';
      if (subdir != NULL)
	strcat ((char *) IR_TOP_BEGIN (), subdir);
      prefix = IR_TOP_BEGIN ();
      IR_TOP_FINISH ();
    }

#ifdef WIN32
  bound = ';';
#else
  bound = ':';
#endif
  if (string != NULL)
    for (;;)
      {
	for (s = string; *s != '\0' && *s != bound; s++)
	  ;
	if (s != string)
	  {
            if (prefix == NULL)
              len = -1;
            else
              {
                len = strlen (prefix);
#ifdef WIN32
                if (len != 0 && prefix [len - 1] == '\\')
#else
                if (len != 0 && prefix [len - 1] == '/')
#endif
                  len--;
              }
	    IR_TOP_EXPAND (len + 1 + (s - string) + 1);
	    if (len == -1)
	      len = 0;
	    else
	      {
		memcpy ((char *) IR_TOP_BEGIN (), prefix, len);
#ifdef WIN32
		((char *) IR_TOP_BEGIN ()) [len] = '\\';
#else
		((char *) IR_TOP_BEGIN ()) [len] = '/';
#endif
		len++;
	      }
	    memcpy ((char *) IR_TOP_BEGIN () + len, string, s - string);
	    ((char *) IR_TOP_BEGIN ()) [len + (s - string)] = '\0';
	    string = IR_TOP_BEGIN ();
	    VLO_ADD_MEMORY (*vector_ptr, &string, sizeof (char *));
	    IR_TOP_FINISH ();
	  }
	if (*s == '\0')
	  break;
	string = s + 1;
      }
}

#define COMMAND_LINE_DESCRIPTION \
"program size dirname\n"\
"%%\n"\
"command line: dino [option ...] [program-file] arguments\n"\
"\n"\
"`-c program' execute program\n"\
"`-h size'    set heap chunk size (1m - default, 1000k, or 1000000)\n"\
"`-Idirname'  directory for searching for Dino programs\n"\
"`-Ldirname'  Dino extern libraries\n"\
"`-s'         output statistics to stderr\n"\
"`-p'         output profile information into stderr\n"

#define DEFAULT_HEAP_CHUNK_SIZE  04000000 /* 1024 Kbytes */
#define MINIMAL_HEAP_CHUNK_SIZE  0100000 /* 32  Kbytes */

unsigned int heap_chunk_size;
int statistics_flag;
int profile_flag;

/* CYGWIN reports incorrect start time, we need this for correction of
   clock. */
double start_time;

/* The following value is maximal number of IR_block_level. */
int max_block_level;

int
dino_main (int argc, char *argv[], char *envp[])
{
  pc_t program_start_pc;
  int okay, option_has_argument, i, ch;
  int flag_of_first;
  char *option;
  const char *input_file_name;
  const char *string;
  const char *home;
  int code;

  start_time = clock ();
  max_block_level = 0;
  if ((code = setjmp (exit_longjump_buff)) != 0)
    return (code < 0 ? 0 : code);
  dino_start ();
#ifndef NDEBUG
  if (!start_command_line_processing (argc, argv, COMMAND_LINE_DESCRIPTION))
    {
      fprintf (stderr, "dino: invalid command line description\n");
      dino_finish (1);
    }
#else
  start_command_line_processing (argc, argv, COMMAND_LINE_DESCRIPTION);
#endif
  if (argument_count == 1)
    {
      fprintf (stderr, "Version %.2f\n", DINO_VERSION);
      output_command_line_description ();
      dino_finish (1);
    }
  heap_chunk_size = DEFAULT_HEAP_CHUNK_SIZE;
  statistics_flag = FALSE;
  profile_flag = FALSE;
  eval_long_jump_set_flag = FALSE;
  /* Process all command line options. */
  for (i = next_option (TRUE), okay = TRUE; i != 0; i = next_option (FALSE))
    {
      option = option_characteristics (i, &option_has_argument);
      if (option == NULL)
	{
	  if (option_has_argument)
	    fprintf (stderr, "dino: flag `%s' without argument\n",
		     argument_vector[i]);
	  else
	    fprintf (stderr, "dino: unknown flag `%s'\n", argument_vector[i]);
	  okay = FALSE;
	}
      else if (strcmp (option, "-c") == 0)
	command_line_program = argument_vector [i + 1];
      else if (strcmp (option, "-s") == 0)
	statistics_flag = TRUE;
      else if (strcmp (option, "-p") == 0)
	{
#ifdef NO_PROFILE
	  fprintf (stderr, "dino: option `-p' is not implemented\n");
#else
	  profile_flag = TRUE;
#endif
	}
      else if (strcmp (option, "-h") == 0)
	{
	  heap_chunk_size = atoi (argument_vector [i + 1]);
	  ch = get_first_nondigit (argument_vector [i + 1]);
	  if (ch == 'k')
	    heap_chunk_size *= 1024;
	  else if (ch == 'm')
	    heap_chunk_size *= 1024 * 1024;
	  if (heap_chunk_size < MINIMAL_HEAP_CHUNK_SIZE)
	    heap_chunk_size = MINIMAL_HEAP_CHUNK_SIZE;
	}
      else if (strcmp (option, "-I") == 0)
	{
	  string = argument_vector [i] + 2;
	  VLO_ADD_MEMORY (include_path_directories_vector,
			  &string, sizeof (char *));
	}
      else if (strcmp (option, "-L") == 0)
	{
	  string = argument_vector [i] + 2;
	  VLO_ADD_MEMORY (libraries_vector, &string, sizeof (char *));
	}
      else
	assert (FALSE);
    }
  if (command_line_program == NULL && number_of_operands () == 0)
    {
      fprintf (stderr,
	       "dino: program itself or dino file must be on command line\n");
      okay = FALSE;
    }
  else if (command_line_program == NULL)
    {
      input_file_name = argument_vector [next_operand (TRUE)];
      flag_of_first = FALSE;
      program_arguments_number = number_of_operands () - 1;
      if (strcmp (file_name_suffix (input_file_name),
                  STANDARD_INPUT_FILE_SUFFIX) != 0)
        {
          fprintf (stderr, "dino: specification file must have suffix `%s'\n",
                   STANDARD_INPUT_FILE_SUFFIX);
          okay = FALSE;
        }
    }
  else
    {
      program_arguments_number = number_of_operands ();
      flag_of_first = TRUE;
    }
  home = getenv (DINO_HOME_NAME_VARIABLE);
  /* Include dirs: */
  add_dino_path (NULL, NULL, getenv (DINO_INCLUDE_PATH_NAME_VARIABLE),
		 &include_path_directories_vector);
  add_dino_path (home, NULL,
                 (home == NULL ? STANDARD_DINO_INCLUDE_DIRECTORY : "lib"),
		 &include_path_directories_vector);
  string = NULL;
  VLO_ADD_MEMORY (include_path_directories_vector, &string, sizeof (char *));
  include_path_directories
    = (const char **) VLO_BEGIN (include_path_directories_vector);
  /* Libraries: */
  add_dino_path (NULL, NULL,
                 getenv (DINO_EXTERN_LIBS_NAME_VARIABLE), &libraries_vector);
  add_dino_path ((home == NULL ? STANDARD_DINO_LIB_DIRECTORY : home),
		 (home == NULL ? NULL : "lib"),
                 STANDARD_DINO_EXTERN_LIBS, &libraries_vector);
  string = NULL;
  VLO_ADD_MEMORY (libraries_vector, &string, sizeof (char *));
  libraries = (const char **) VLO_BEGIN (libraries_vector);
  if (!okay)
    dino_finish (1);
  MALLOC (program_arguments, (program_arguments_number + 1) * sizeof (char *));
  for (i = 0; i < program_arguments_number;i++)
    {
      program_arguments [i] = argument_vector [next_operand (flag_of_first)];
      flag_of_first = FALSE;
    }
  program_arguments [i] = NULL;
  program_environment = envp;
  set_exception_action (SIGINT);
  set_exception_action (SIGILL);
  set_exception_action (SIGABRT);
  set_exception_action (SIGFPE);
  set_exception_action (SIGTERM);
  set_exception_action (SIGSEGV);
#if ! defined (NO_PROFILE) && defined (HAVE_SETITIMER)
  if (profile_flag)
    set_exception_action (SIGVTALRM);
#endif
  start_scanner_file
    ((command_line_program == NULL ? input_file_name : ""), no_position);
  yyparse ();
  if (first_program_stmt != NULL)
    program_start_pc = test_context (first_program_stmt);
  output_errors ();
  if (number_of_errors == 0)
    {
      initiate_heap ();
      evaluate_program (program_start_pc);
    }
  dino_finish (number_of_errors != 0);
}
