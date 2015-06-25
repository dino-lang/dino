/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

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
#include "d_context.h"
#include "d_inference.h"
#include "d_eval.h"
#include "d_func.h"

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

  d_assert (n >= 0);
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



/* Nonzero if it is a big endian machine.  */
int big_endian_p;

static void
set_big_endian_flag (void)
{
  unsigned int i = 1;
  big_endian_p = ((char *) &i)[0] == 0;
}



/* This page contains functions for transformation to/from string. */

/* The function returns rint_t value for character number
   representation in BASE.  It always change the value of ERRNO. */
rint_t
a2i (const char *str, int base)
{
  rint_t res;
  long long int l;

  d_assert (base == 8 || base == 10 || base == 16);
  errno = 0;
  d_assert (sizeof (rint_t) <= sizeof (long long int));
#if defined (HAVE_STRTOLL) && defined (HAVE_STRTOULL)
  if (base == 10)
    l = strtoll (str, (char **) NULL, base);
  else
    l = strtoull (str, (char **) NULL, base);
  res = l;
#ifdef ERANGE
  if (res != l)
    errno = ERANGE;
#endif
#else
#error  The system is too old: strtoll/strtoull is required.
#endif
  return res;
}

/* The function returns rfloat_t value for character number
   representation.  It always change the value of ERRNO. */
rfloat_t
a2f (const char *str)
{
  double d;
  rfloat_t res;

  errno = 0;
  d_assert (sizeof (rfloat_t) <= sizeof (double));
#ifdef HAVE_STRTOD
  d = strtod (str, (char **) NULL);
  res = d;
#ifdef ERANGE
  if (res != d)
    errno = ERANGE;
#endif
#else
  res = atof (str);
#endif
  return res;
}

/* Convert int NUMBER into string and return it.  */
const char *
i2a (rint_t number)
{
  static char result [50];

  if (sizeof (rint_t) <= sizeof (long int))
    sprintf (result, "%ld", (long int) number);
  else
    {
      d_assert (sizeof (rint_t) <= sizeof (long long int));
      sprintf (result, "%lld", number);
    }
  return result;
}

/* Convert float NUMBER into string and return it.  */
const char *
f2a (rfloat_t number)
{
  static char result [50];

  d_assert (sizeof (rfloat_t) <= sizeof (double));
  sprintf (result, "%.*g", FORMAT_DOUBLE_DIGS, (double) number);
  return result;
}

static vlo_t repr_vlobj;

/* Convert mpz NUMBER into string of BASE using upper cases if
   UPPER_CASE_P and return it.  */
const char *
mpz2a (mpz_t number, int base, int upper_case_p)
{
  int n = mpz_sizeinbase (number, base) + 2;
  char *str;

  VLO_NULLIFY (repr_vlobj);
  VLO_EXPAND (repr_vlobj, n);
  mpz_get_str (VLO_BEGIN (repr_vlobj), base, number);
  if (upper_case_p)
    for (str = VLO_BEGIN (repr_vlobj); *str != 0; str++)
      *str = toupper (*str);
  return VLO_BEGIN (repr_vlobj);
}

static mpz_t min_rint_mpz;
static mpz_t max_rint_mpz;

int
mpz_ok_for_rint_p (mpz_t number)
{
  d_assert (sizeof (long int) <= sizeof (rint_t));
  if (mpz_fits_slong_p (number))
    return TRUE;
  return (mpz_cmp (min_rint_mpz, number) <= 0
	  && mpz_cmp (number, max_rint_mpz) <= 0);
}

rint_t
mpz2i (mpz_t number)
{
  d_assert (mpz_ok_for_rint_p (number));
  if (mpz_fits_slong_p (number))
    return mpz_get_si (number);
  d_assert (sizeof (long long int) >= sizeof (rint_t));
#ifdef HAVE_STRTOLL
  return strtoll (mpz2a (number, 10, FALSE), (char **) NULL, 10);
#else
#error  The system is too old: strtoll is required.
#endif
}

void
i2mpz (mpz_t mpz, rint_t i)
{
  static char str[30];

  if (LONG_MIN <= i && i <= LONG_MAX)
    mpz_set_si (mpz, (long int) i);
  else
    {
      d_assert (sizeof (rint_t) == sizeof (long long int));
      sprintf (str, "%lld", i);
      mpz_set_str (mpz, str, 10);
    }
}

void
f2mpz (mpz_t mpz, rfloat_t f)
{
  d_assert (sizeof (rfloat_t) == sizeof (double));
  mpz_set_d (mpz, f);
}

/* It is gmp implementation depended function.  It might be rewriten
   lately.  */
size_t
hash_mpz (mpz_t mpz)
{
  mp_limb_t res = 0;
  size_t i, size = mpz_size (mpz);

  for (i = 0; i < size; i++)
    res += mpz->_mp_d[i] & GMP_NUMB_MASK;
  return (size_t) res;
}

static void
mpz_start (void)
{
  mpz_init (min_rint_mpz); i2mpz (min_rint_mpz, MIN_RINT);
  mpz_init (max_rint_mpz); i2mpz (max_rint_mpz, MAX_RINT);
  
}

static void
mpz_finish (void)
{
  mpz_clear (min_rint_mpz); mpz_clear (max_rint_mpz);
  
}



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
const ucode_t *command_line_program = NULL;

/* The value of the following var is not NULL when the program is
   given by dump file on the command line.  In this case its value is
   the dump file. */
FILE *input_dump_file = NULL;

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

/* Return readbale ASCII representation of unicode CH.  The result
   exists until the next function call.  */
char *
get_ucode_ascii_repr (ucode_t ch)
{
  static char str [20];

  if (ch == '\'' || ch == '"' || ch == '\\')
    sprintf (str, "\\%c", ch);
  else if (ch < 128 && isprint (ch))
    sprintf (str, "%c", ch);
  else if (ch == '\n')
    sprintf (str, "\\n");
  else if (ch == '\t')
    sprintf (str, "\\t");
  else if (ch == '\v')
    sprintf (str, "\\v");
  else if (ch == '\a')
    sprintf (str, "\\a");
  else if (ch == '\b')
    sprintf (str, "\\b");
  else if (ch == '\r')
    sprintf (str, "\\r");
  else if (ch == '\f')
    sprintf (str, "\\f");
  else if (ch < 256)
    sprintf (str, "\\x%x", ch);
  else if (ch < (1 << 16))
    sprintf (str, "\\u%04x", ch);
  else
    sprintf (str, "\\U%08x", ch);
  return str;
}

/* The func reads one code (may be composited from some characters,
   e.g. using an escape character sequence) using DINO language
   conventions.  It is supposed that the current character is not end
   marker (string or character constant).  The func returns the code
   value or negative number if error is found.  After the call the
   current char is first char after the code or the same as before
   call if error was found.  Position is position of the char will be
   read next.  Parameter INPUT_CHAR is current input char (the next
   chars may be read by d_getc () and undoing it by d_ungetc).  If the
   code is symbol string breaking, TRUE is passed through parameter
   correct_newln.  This case is error and the error must be processed
   right after the call if character constant is processed.  If we
   have wrong escape code (hex or unicode escape), TRUE is passed
   through parameter wrong_escape_code.  The error must be processed
   after the call too. */
int
read_dino_string_code (int input_char, int *correct_newln,
		       int *wrong_escape_code,
		       int d_getc (void), void d_ungetc (int))
{
  int character_code;

  /* `current_position' corresponds position right after `input_char'
     here. */
  if (input_char == EOF || input_char == '\n')
    {
      current_position.column_number--;
      d_ungetc (input_char);
      return (-1);
    }
  *correct_newln = *wrong_escape_code = FALSE;
  if (input_char == '\\')
    {
      input_char = d_getc ();
      current_position.column_number++;
      if (input_char == 'n')
        input_char = '\n';
      else if (input_char == 't')
        input_char = '\t';
      else if (input_char == 'v')
	input_char = '\v';
      else if (input_char == 'a')
        input_char = '\a';
      else if (input_char == 'b')
        input_char = '\b';
      else if (input_char == 'r')
        input_char = '\r';
      else if (input_char == 'f')
        input_char = '\f';
      else if (input_char == '\\' || input_char == '\'' || input_char == '\"')
        ;
      else if (input_char == '\n')
        {
	  current_position.column_number = 1;
	  current_position.line_number++;
          *correct_newln = TRUE;
        }
      else if (isdigit (input_char) && input_char != '8' && input_char != '9')
	{
	  character_code = value_of_digit (input_char);
	  input_char = d_getc ();
	  if (!isdigit (input_char) || input_char == '8' || input_char == '9')
	    d_ungetc (input_char);
	  else
	    {
	      current_position.column_number++;
	      character_code
		= (character_code * 8 + value_of_digit (input_char));
	      input_char = d_getc ();
	      if (!isdigit (input_char)
		  || input_char == '8' || input_char == '9')
		d_ungetc (input_char);
	      else
		{
		  character_code
		    = (character_code * 8 + value_of_digit (input_char));
		  current_position.column_number++;
		}

	    }
	  input_char = character_code;
	}
      else if (input_char == 'x' || input_char == 'u' || input_char == 'U')
	{
	  /* Hex or Unicode escape code.  */
	  int i, c;
	  
	  character_code = 0;
	  for (i = (input_char == 'x' ? 2 : input_char == 'u' ? 4 : 8);
	       i > 0;
	       i--)
	    {
	      input_char = d_getc ();
	      if (! is_hex_digit (input_char))
		break;
	      c = value_of_hex_digit (input_char);
	      character_code = (character_code << 4) | c;
	    }
	  *wrong_escape_code = i > 0;
	  input_char = character_code;
	}
    }
  return input_char;
}

static vlo_t number_text;

/* Read number using GET_CH and UNGET_CH and already read character C.
   It should be guaranted that the input has a righ prefix
   (+|-)?[0-9].  Return read character number, number representation
   (0x or 0X prefix is removed), base, float and long flag through
   READ_CH_NUM, RESULT, BASE, FLOAT_P, LONG_P.  Return error code.  If
   the number is not ok only READ_CH_NUM should be defined.  */
enum read_number_code
read_dino_number (int c, int get_ch (void), void unget_ch (int),
		  int *read_ch_num,
		  const char **result, int *base, int *float_p, int *long_p)
{
  enum read_number_code err_code = NUMBER_OK;
  int dec_p, hex_p, hex_char_p;

  VLO_NULLIFY (number_text);
  *read_ch_num = 0;
  *base = 10;
  *float_p = *long_p = FALSE;
  if (c == '+' || c == '-')
    {
      VLO_ADD_BYTE (number_text, c);
      c = get_ch ();
      (*read_ch_num)++;
    }
  d_assert ('0' <= c && c <= '9');
  if (c == '0')
    {
      c = get_ch ();
      (*read_ch_num)++;
      if (c != 'x' && c != 'X')
	{
	  *base = 8;
	  unget_ch (c);
	  (*read_ch_num)--;
	  c = '0';
	}
      else
	{
	  c = get_ch ();
	  (*read_ch_num)++;
	  *base = 16;
	}
    }
  dec_p = hex_p = FALSE;
  for (;;)
    {
      VLO_ADD_BYTE (number_text, c);
      c = get_ch ();
      (*read_ch_num)++;
      if (c == '8' || c == '9')
	dec_p = TRUE;
      hex_char_p = (('a' <= c && c <= 'f')
		    || ('A' <= c && c <= 'F'));
      if (! isdigit (c) && (*base != 16 || ! hex_char_p))
	break;
      if (hex_char_p)
	hex_p = TRUE;
    }
  d_assert (*base == 16 || ! hex_p);
  if (c == '.')
    {
      *float_p = TRUE;
      do
	{
	  VLO_ADD_BYTE (number_text, c);
	  c = get_ch ();
	  (*read_ch_num)++;
	}
      while (isdigit (c));
    }
  if (c == 'e' || c == 'E')
    {
      *float_p = TRUE;
      c = get_ch ();
      (*read_ch_num)++;
      if (c != '+' && c != '-' && !isdigit (c))
	err_code = ABSENT_EXPONENT;
      else
	{
	  VLO_ADD_BYTE (number_text, 'e');
	  do
	    {
	      VLO_ADD_BYTE (number_text, c);
	      c = get_ch ();
	      (*read_ch_num)++;
	    }
	  while (isdigit (c));
	}
    }
  else if (! *float_p && (c == 'l' || c == 'L'))
    {
      *long_p = TRUE;
      c = get_ch ();
      (*read_ch_num)++;
    }
  VLO_ADD_BYTE (number_text, '\0');
  unget_ch (c);
  (*read_ch_num)--;
  if (*float_p)
    {
      if (*base == 16)
	err_code = NON_DECIMAL_FLOAT;
    }
  else if (*base == 8 && dec_p)
    err_code = WRONG_OCTAL_INT;
  *result = VLO_BEGIN (number_text);
  return err_code;
}

size_t gmp_memory_size, max_gmp_memory_size;

static void *
gmp_alloc (size_t alloc_size)
{
  void *res;

  gmp_memory_size += alloc_size;
  if (max_gmp_memory_size < gmp_memory_size)
    max_gmp_memory_size = gmp_memory_size;
  MALLOC (res, alloc_size);
  return res;
}

static void *
gmp_realloc (void *ptr, size_t old_size, size_t new_size)
{
  void *res;

  gmp_memory_size += new_size;
  gmp_memory_size -= old_size;
  if (max_gmp_memory_size < gmp_memory_size)
    max_gmp_memory_size = gmp_memory_size;
  REALLOC (res, ptr, new_size);
  return res;
}

static void
gmp_free (void *ptr, size_t size)
{
  d_assert (gmp_memory_size >= size);
  gmp_memory_size -= size;
  FREE (ptr);
}

static size_t
get_size_repr (size_t size, char *unit)
{
  *unit = 'b';
  if (size % 1024 == 0 || size > 16 * 1024)
    {
      *unit = 'k';
      size /= 1024;
    }
  if (size % 1024 == 0 || size > 16 * 1024)
    {
      *unit = 'm';
      size /= 1024;
    }
  return size;
}

/* Container for ucode of command line.  */
static vlo_t command_line_vlo;

static int evaluated_p;

void
dino_finish (int code)
{
  char unit, unit2;
  int size, size2;

  if (evaluated_p)
    {
      if (code >= 0)
	final_call_destroy_functions ();
      if (trace_flag)
	print_trace_stack ();
#ifndef NO_PROFILE
      if (code == 0 && profile_flag)
	print_profile (first_program_bc);
#endif
    }
  finish_run_tables ();
  IR_stop ();
  delete_table ();
  finish_icode ();
  finish_scanner ();
  output_errors ();
  finish_errors ();
  finish_positions ();
  VLO_DELETE (number_text);
  VLO_DELETE (repr_vlobj);
  VLO_DELETE (include_path_directories_vector);
  VLO_DELETE (libraries_vector);
  VLO_DELETE (command_line_vlo);
  if (input_dump_file != NULL)
    finish_read_bc ();
  if (statistics_flag && code == 0)
    {
      if (evaluated_p)
	{
	  finish_heap ();
	  fprintf (stderr, "Created byte code insns - %d\n", bc_nodes_num);
	  size = get_size_repr (heap_size, &unit);
	  size2 = get_size_repr (max_heap_size, &unit2);
	  fprintf (stderr, "Heap size - %d%c (peak %d%c), heap chunks - %d (peak %d), ",
		   size, unit, size2, unit2,
		   heap_chunks_number, max_heap_chunks_number);
	}
      size = get_size_repr (gmp_memory_size, &unit);
      size2 = get_size_repr (max_gmp_memory_size, &unit2);
      fprintf (stderr, "Long ints - %d%c (peak %d%c)\n",
	       size, unit, size2, unit2);
      if (gc_number != 0)
	fprintf (stderr,
		 "GC - %d times, average free memory after GC - %d%%\n",
		 gc_number, free_gc_memory_percent);
      if (tab_collisions != 0)
	fprintf (stderr, "Tables collisions - %d\n", tab_collisions);
      if (tab_expansions != 0)
	fprintf (stderr, "Tables expansions - %d\n", tab_expansions);
      if (generated_c_functions_num != 0)
	fprintf (stderr, "Generated C code functions - %u, their calls - %u\n",
		 generated_c_functions_num, generated_c_function_calls_num);
      if (inlined_calls_num != 0)
	fprintf (stderr, "Inlined calls - %u\n", inlined_calls_num);
    }
  mpz_finish ();
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
  error (FALSE, get_cpos (), ERR_no_memory);
  dino_finish (-1);
}

static void
dino_start (void)
{
  change_allocation_error_function (error_func_for_allocate);
  generated_c_functions_num = generated_c_function_calls_num = 0;
  inlined_calls_num = 0;
  max_gmp_memory_size = gmp_memory_size = 0;
  mp_set_memory_functions (gmp_alloc, gmp_realloc, gmp_free);
  initiate_positions ();
  /* Output errors immediately for REPL.  */
  initiate_errors (repl_flag);
  fatal_error_function = dino_fatal_finish;
  initiate_table ();
  initiate_icode (); /* only after initiate table */
  initiate_scanner ();
  source_position = no_position;
  VLO_CREATE (command_line_vlo, 0);
  VLO_CREATE (include_path_directories_vector, 0);
  VLO_CREATE (libraries_vector, 0);
  VLO_CREATE (repr_vlobj, 0);
  VLO_CREATE (number_text, 0);
  initiate_run_tables ();
  evaluated_p = FALSE;
}

static void set_exception_action (int signal_number);

/* The following func is signal handler of an exception. */
static void
exception_action (int signal_number)
{
  BC_node_t class;
  const char *message;

  switch (signal_number)
    {
    case SIGINT:
      class = sigint_bc_decl;
      message = ERR_interrupt_exception;
      break;
    case SIGILL:
      class = sigill_bc_decl;
      message = ERR_illegal_instruction_exception;
      break;
    case SIGABRT:
      class = sigabrt_bc_decl;
      message = ERR_abort_exception;
      break;
    case SIGFPE:
      class = sigfpe_bc_decl;
      message = ERR_floating_point_exception;
      break;
    case SIGTERM:
      class = sigterm_bc_decl;
      message = ERR_termination_exception;
      break;
    case SIGSEGV:
      class = sigsegv_bc_decl;
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
      d_unreachable ();
    }
  set_exception_action (signal_number);
  if (eval_long_jump_set_flag)
    eval_error (class, get_cpos (), message);
  else if (signal_number != SIGINT && signal_number != SIGTERM)
    error (! repl_flag, get_cpos (), message);
  else if (! repl_flag)
    dino_finish (1);
  d_assert (repl_flag);
  fputs ("\n", stdout);
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

  d_assert (prefix != NULL || subdir == NULL);
  if (prefix != NULL)
    {
      len = strlen (prefix);
      if (len != 0 && prefix [len - 1] == '/')
	len--;
      IR_TOP_EXPAND (len + 1 /* '/' */ + 1 /* '\0' */
		     + (subdir == NULL ? 0 : strlen (subdir)));
      memcpy ((char *) IR_TOP_BEGIN (), prefix, len);
      ((char *) IR_TOP_BEGIN ()) [len] = '/';
      ((char *) IR_TOP_BEGIN ()) [len + 1] = '\0';
      if (subdir != NULL)
	strcat ((char *) IR_TOP_BEGIN (), subdir);
      prefix = IR_TOP_BEGIN ();
      IR_TOP_FINISH ();
    }

  bound = ':';
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
                if (len != 0 && prefix [len - 1] == '/')
                  len--;
              }
	    IR_TOP_EXPAND (len + 1 + (s - string) + 1);
	    if (len == -1)
	      len = 0;
	    else
	      {
		memcpy ((char *) IR_TOP_BEGIN (), prefix, len);
		((char *) IR_TOP_BEGIN ()) [len] = '/';
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
"program size dirname dump\n"\
"%%\n"\
"command line: dino [option ...] [program-file] arguments\n"\
"\n"\
"`-h'           help\n"\
"`-c program'   execute program\n"\
"`-m size'      set heap chunk size (1m - default, 1000k, or 1000000)\n"\
"`-Idirname'    directory for searching for Dino programs\n"\
"`-Ldirname'    Dino extern libraries\n"\
"`-O'           optimize\n"\
"`-s'           output statistics to stderr\n"\
"`-t'           output final trace to stderr\n"\
"`-p'           output profile information into stderr\n"\
"`-d'           dump program IR\n"\
"`-i dump'      read IR instead of program\n"\
"`--save-temps' save temp JIT C and object files\n"

int bc_nodes_num;

#define DEFAULT_HEAP_CHUNK_SIZE  04000000 /* 1024 Kbytes */
#define MINIMAL_HEAP_CHUNK_SIZE  0100000 /* 32  Kbytes */

unsigned int heap_chunk_size;
int repl_flag;
int optimize_flag;
int statistics_flag;
int trace_flag;
int profile_flag;
int dump_flag;
int save_temps_flag;

/* CYGWIN reports incorrect start time, we need this for correction of
   clock. */
double start_time;

static void
set_signal_actions (void)
{
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
}

/* Prompts used in REPL for starting new stmt, for continue type
   the stmt, and prefix used for error output.  */
#define REPL_PROMPT      "dino> "
#define REPL_CONT_PROMPT "    | "
#define ERROR_PREFIX     "      "

/* Two functions for printing prompts.  */
void
print_stmt_prompt (void)
{
  fputs (REPL_PROMPT, stdout);
  fflush (stdout);
}

void
print_stmt_cont_prompt (void)
{
  fputs (REPL_CONT_PROMPT, stdout);
  fflush (stdout);
}

/* Common functions used for output all error messages.  */
void
d_verror (int fatal_error_flag, position_t position,
	  const char *format, va_list ap)
{
#define MAX_MESSAGE_LEN 100
  static char message[MAX_MESSAGE_LEN];

  vsnprintf (message, MAX_MESSAGE_LEN, format, ap);
  if (repl_flag)
    fprintf (stderr, "%s", ERROR_PREFIX);
  if (! repl_flag)
    error (fatal_error_flag, position, "%s", message);
  else if(*position.file_name != '\0')
    error (FALSE, position, "%s", message);
  else
    {
      int i;
      const ucode_t *ln = get_read_line (position.line_number - 1);
      
      print_ucode_string_as_utf8 (ln, stderr);
      fprintf (stderr, "%s", ERROR_PREFIX);
      for (i = 1; i < position.column_number; i++)
	fprintf (stderr, " ");
      fprintf (stderr, "^\n%s%s\n", ERROR_PREFIX, message);
      number_of_errors++;
    }
}

void
d_error (int fatal_error_flag, position_t position,
	 const char *format, ...)
{
  va_list arguments;

  va_start (arguments, format);
  d_verror (fatal_error_flag, position, format, arguments);
  va_end (arguments);
}

/* Copy containing value of vlo *FROM to vlo *TO.  */
void
copy_vlo (vlo_t *to, vlo_t *from)
{
  VLO_NULLIFY (*to);
  VLO_EXPAND (*to, VLO_LENGTH (*from));
  memcpy (VLO_BEGIN (*to), VLO_BEGIN (*from), VLO_LENGTH (*from));
}

/* Put LEN chars of string STR as unicodes into vlo *TO.  */
void
str_to_ucode_vlo (vlo_t *to, const char *from, size_t len)
{
  ucode_t uc;
  size_t i;
  
  VLO_NULLIFY (*to);
  for (i = 0; i < len; i++)
    {
      uc = ((const unsigned char *) from)[i];
      VLO_ADD_MEMORY (*to, &uc, sizeof (ucode_t));
    }
}

/* Read byte C from UTF8 string passed through pointer DATA.
   Increment the pointer.  Used by utf8_str_to_ucode_vlo.  */
static inline int
get_utf8_byte (void *data)
{
  const char *utf8 = *(const char **) data;
  int res = *utf8++;

  if (res == 0)
    return -1;
  *(const char **) data = utf8;
  return res;
}

/* Put UTF8 string STR into vlo *VLO as unicode string.  Return the
   string start in the vlo.  */
static ucode_t *
utf8_str_to_ucode_vlo (const char *utf8, vlo_t *vlo)
{
  VLO_NULLIFY (*vlo);
  for (;;)
    {
      ucode_t u = get_ucode_from_utf8_stream (get_utf8_byte, (void *) &utf8);
      if (u < 0)
	break;
      VLO_ADD_MEMORY (*vlo, &u, sizeof (ucode_t));
    }
  return (ucode_t *) VLO_BEGIN (*vlo);
}

/* Put byte C into vlo passed through DATA.  Used by
   ucode_str_to_utf8_vlo.  */
static inline int
put_utf8_byte (int c, void *data)
{
  VLO_ADD_BYTE (*(vlo_t *) data, c);
  return 1;
}

/* Put ucode string STR into vlo *VLO as UTF8 string.  Return the
   string start in the vlo.  */
char *
ucode_str_to_utf8_vlo (const ucode_t *str, vlo_t *vlo)
{
  size_t i;
  int code;
  
  VLO_NULLIFY (*vlo);
  for (i = 0;; i++)
    {
      code = put_ucode_to_utf8_stream (str[i], put_utf8_byte, (void *) vlo);
      d_assert (code > 0);
      if (str[i] == 0)
	break;
    }
  return (char *) VLO_BEGIN (*vlo);
}

/* Put unicode char C into vlo *VLO as UTF8 string.  Return the string
   start in the vlo.  */
char *
ucode_char_to_utf8_vlo (int c, vlo_t *vlo)
{
  int code;
  
  VLO_NULLIFY (*vlo);
  code = put_ucode_to_utf8_stream (c, put_utf8_byte, (void *) vlo);
  d_assert (code > 0);
  code = put_ucode_to_utf8_stream (0, put_utf8_byte, (void *) vlo);
  d_assert (code > 0);
  return (char *) VLO_BEGIN (*vlo);
  
}

/* Put byte string STR into vlo *VLO as UTF8 string.  Return the
   string start in the vlo.  */
char *
byte_str_to_utf8_vlo (byte_t *str, vlo_t *vlo)
{
  size_t i;
  int code;
  
  for (i = 0; ; i++)
    if (str[i] > 127)
      {
	VLO_NULLIFY (*vlo);
	VLO_EXPAND (*vlo, i);
	memcpy (VLO_BEGIN (*vlo), str, i * sizeof (byte_t));
	break;
      }
    else if (str[i] == 0)
      return str;
  for (;; i++)
    {
      code = put_ucode_to_utf8_stream (str[i], put_utf8_byte, (void *) vlo);
      d_assert (code > 0);
      if (str[i] == 0)
	break;
    }
  return (char *) VLO_BEGIN (*vlo);
  
}

int
dino_main (int argc, char *argv[], char *envp[])
{
  int okay, option_has_argument, i, ch;
  int flag_of_first;
  char *option;
  const char *input_file_name, *input_dump = NULL;
  const char *string;
  const char *home;
  int code;

  start_time = clock ();
  set_big_endian_flag ();
  if ((code = setjmp (exit_longjump_buff)) != 0)
    return (code < 0 ? 0 : code);
#ifndef NDEBUG
  if (!start_command_line_processing (argc, argv, COMMAND_LINE_DESCRIPTION))
    {
      fprintf (stderr, "dino: invalid command line description\n");
      dino_finish (1);
    }
#else
  start_command_line_processing (argc, argv, COMMAND_LINE_DESCRIPTION);
#endif
  repl_flag = argument_count == 1;
  dino_start ();
  heap_chunk_size = DEFAULT_HEAP_CHUNK_SIZE;
  optimize_flag = FALSE;
  statistics_flag = FALSE;
  trace_flag = FALSE;
  profile_flag = FALSE;
  dump_flag = FALSE;
  save_temps_flag = FALSE;
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
      else if (strcmp (option, "-h") == 0)
	{
	  fprintf (stderr, "Version %.2f\n", DINO_VERSION);
	  output_command_line_description ();
	  dino_finish (1);
	}
      else if (strcmp (option, "-c") == 0)
	command_line_program = utf8_str_to_ucode_vlo (argument_vector [i + 1],
						      &command_line_vlo);
      else if (strcmp (option, "-O") == 0)
	optimize_flag = TRUE;
      else if (strcmp (option, "-s") == 0)
	statistics_flag = TRUE;
      else if (strcmp (option, "-t") == 0)
	trace_flag = TRUE;
      else if (strcmp (option, "-p") == 0)
	{
#ifdef NO_PROFILE
	  fprintf (stderr, "dino: option `-p' is not implemented\n");
#else
	  profile_flag = TRUE;
#endif
	}
      else if (strcmp (option, "-d") == 0)
	dump_flag = TRUE;
      else if (strcmp (option, "-i") == 0)
	input_dump = argument_vector [i + 1];
      else if (strcmp (option, "--save-temps") == 0)
	save_temps_flag = TRUE;
      else if (strcmp (option, "-m") == 0)
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
	d_unreachable ();
    }
  if (repl_flag)
    optimize_flag = FALSE;
  else if (input_dump != NULL)
    {
      optimize_flag = FALSE;
      program_arguments_number = number_of_operands ();
      flag_of_first = TRUE;
      input_dump_file = fopen (input_dump, "rb");
      if (input_dump_file == NULL)
	system_error (TRUE, no_position, "fatal error -- `%s': ", 
		      input_dump);
    }
  else if (command_line_program != NULL)
    {
      program_arguments_number = number_of_operands ();
      flag_of_first = TRUE;
    }
  else if (number_of_operands () == 0)
    {
      fprintf (stderr,
	       "dino: program itself or dino file must be on command line\n");
      okay = FALSE;
    }
  else
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
  mpz_start ();
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
  first_program_bc = NULL;
  if (repl_flag)
    {
      int first_p;

      printf ("Dino interpreter, version %.2f\n", DINO_VERSION);
      printf ("Copyright (c) 1997-2015, Vladimir Makarov, vmakarov@gcc.gnu.org\n");
      printf ("Use \"exit(<int>);\" or Ctrl-D to exit\n");
      printf ("Use \";\" for stmt end, for if-stmt w/o else use \";;\", e.g. if (cond) v = e;;\n");
      start_scanner_file ("", no_position);
      initiate_parser ();
      initiate_context ();
      set_signal_actions ();
      for (first_p = TRUE;; first_p = FALSE)
	{
	  int last_p;

	  number_of_errors = 0;
	  /* Skip prompt for environment code. */
	  if (! first_p)
	    {
	      fputs (REPL_PROMPT, stdout);
	      fflush (stdout);
	    }
	  initiate_new_parser_REPL_stmts ();
	  if (yyparse ())
	    skip_line_rest ();
	  last_p = feof (stdin);
	  if (last_p)
	    fputs ("\n", stdout);
	  if (number_of_errors == 0 && first_program_stmt != NULL)
	    test_context (first_program_stmt, first_p);
	  if (last_p)
	    {
	      finish_context ();
	      finish_parser ();
	    }
	  d_assert (! first_p || number_of_errors == 0);
	  if (number_of_errors == 0 && first_program_bc != NULL)
	    {
	      if (first_p)
		init_env_decl_processing ();
	      prepare_block (first_program_bc);
	      evaluated_p = TRUE;
	      evaluate_program (first_program_bc, first_p, last_p);
	      d_assert (!last_p);
	    }
	  else if (last_p)
	    break;
	}
    }
  else 
    {
      if (input_dump_file != NULL)
	{
	  initiate_read_bc ();
	  read_bc_program (input_dump, input_dump_file, dump_flag);
	}
      else
	{
	  start_scanner_file
	    ((command_line_program == NULL ? input_file_name : ""), no_position);
	  initiate_parser ();
	  yyparse ();
	  finish_parser ();
	  if (first_program_stmt != NULL)
	    {
	      initiate_context ();
	      test_context (first_program_stmt, TRUE);
	      finish_context ();
	    }
	}
      output_errors ();
      if (number_of_errors == 0 && first_program_bc != NULL)
	{
	  if (! dump_flag)
	    {
	      if (input_dump_file == NULL)
		{
		  init_env_decl_processing ();
		  prepare_block (first_program_bc);
		}
	      if (! all_env_decls_processed_p ())
		{
		  fprintf (stderr, "fatal error - byte code corrupted\n");
		  exit (1);
		}
	      set_signal_actions ();
	      evaluated_p = TRUE;
	      evaluate_program (first_program_bc, TRUE, TRUE);
	      d_unreachable ();
	    }
	}
    }
  dino_finish (number_of_errors != 0);
  d_unreachable ();
}
