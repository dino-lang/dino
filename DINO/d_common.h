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

#ifndef D_COMMON_H
#define D_COMMON_H 1

#ifdef HAVE_CONFIG_H
#include "d_config.h"
#else /* In this case we are oriented to ANSI C and dfcn.h */
#ifndef HAVE_MEMSET
#define HAVE_MEMSET
#endif
#ifndef HAVE_MEMCPY
#define HAVE_MEMCPY
#endif
#ifndef HAVE_MEMMOVE
#define HAVE_MEMMOVE
#endif
#ifndef HAVE_MEMCMP
#define HAVE_MEMCMP
#endif
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#ifndef HAVE_FLOAT_H
#define HAVE_FLOAT_H
#endif
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#ifndef HAVE_TIME_H
#define HAVE_TIME_H
#endif
#ifndef HAVE_ERRNO_H
#define HAVE_ERRNO_H
#endif
#ifndef HAVE_DLFCN_H
#define HAVE_DLFCN_H
#endif
#ifdef HAVE_SYS_TIME_H
#undef HAVE_SYS_TIME_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stddef.h>
#include <stdlib.h>
#include <signal.h>

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif
#ifndef SCHAR_MAX
#define SCHAR_MAX 127
#endif
#ifndef SCHAR_MIN
#define SCHAR_MIN (-128)
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

#ifdef HAVE_FLOAT_H
#include <float.h>
#else
#define FLT_MAX  3.40282347e+38F         /* IEEE float */
#define DBL_MAX  1.7976931348623157e+308 /* IEEE double */
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

#define d_assert assert

#define d_unreachable() abort ()

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifndef AIX_DLOPEN
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0x0	/* ignore allowing symbols to be global. */
#endif
#else
#ifdef HAVE_DLOPEN
/* Used mode flags for the dlopen routine. */
#define RTLD_LAZY	1	/* lazy function call binding */
#define RTLD_NOW	2	/* immediate function call binding */
#define RTLD_GLOBAL	0x100	/* allow symbols to be global */
void *dlopen (const char *filename, int flag);
const char *dlerror(void);
void *dlsym(void *handle, char *symbol);
int dlclose (void *handle);
#endif
#endif
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#ifndef ITIMER_VIRTUAL
#define ITIMER_VIRTUAL 1
#endif
#if defined (HAVE_SETITIMER) && !defined(SIGVTALRM)
#define SIGVTALRM 26 
#endif
#endif

#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "d_dino.h"
#include "allocate.h"
#include "vlobject.h"
#include "position.h"
#include "hashtab.h"
#include "ticker.h"
#include "bits.h"
#include "d_errors.h"
#include "d_types.h"

#define FALSE 0
#define TRUE 1

/* Determine positions for the tabs. */
#define TAB_STOP 8

/* The following value is used as a variable value which is not char.  */
#define NOT_A_CHAR (-2000)

/* We don't want mess with locale.  */
static int inline isalpha_ascii (int ch) { return ch < 128 && isalpha (ch); }
static int inline isdigit_ascii (int ch) { return ch < 128 && isdigit (ch); }
static int inline isspace_ascii (int ch) { return ch < 128 && isspace (ch); }
static int inline isgraph_ascii (int ch) { return ch < 128 && isgraph (ch); }
static int inline isprint_ascii (int ch) { return ch < 128 && isprint (ch); }

/* True if CH is a hexdecimal digit.  */
static int inline
is_hex_digit (int ch)
{
  return ('0' <= ch && ch <= '9'
	  || 'a' <= (ch) && (ch) <= 'f' || 'A' <= (ch) && (ch) <= 'F');
}

/* Functions returning value of digit or hex digit CH. */

static int inline
value_of_digit (int ch)
{
  d_assert ('0' <= ch && ch <= '9');
  return ch - '0';
}

static int inline
value_of_hex_digit (int ch)
{
  d_assert (is_hex_digit (ch));
  return ('0' <= ch && ch <= '9' ? ch - '0'
	  : 'a' <= (ch) && (ch) <= 'f' ? ch - 'a' + 10 : ch - 'A' + 10);
}

#define STANDARD_INPUT_FILE_SUFFIX ".d"

/* Unique number for field destroy.  */
#define DESTROY_FLDID_NUM 0

extern const char **include_path_directories;
extern const char **libraries;
extern const ucode_t *command_line_program;
extern FILE *input_dump_file;
extern int program_arguments_number;
extern char **program_arguments;
extern char **program_environment;
extern position_t source_position;
extern size_t gmp_memory_size;
extern int bc_nodes_num;
extern unsigned int heap_chunk_size;
extern unsigned int generated_c_functions_num, generated_c_function_calls_num;
extern unsigned int inlined_calls_num;
extern int repl_flag;
extern int optimize_flag;
extern int statistics_flag;
extern int trace_flag;
extern int profile_flag;
extern int dump_flag;
extern int save_temps_flag;
extern double start_time;
extern int big_endian_p;
extern conv_desc_t curr_byte_cd, curr_ucode_cd, curr_reverse_ucode_cd;
extern const char *curr_encoding_name;

extern unsigned str_hash_func (hash_table_entry_t str);
extern int str_compare_func (hash_table_entry_t str1, hash_table_entry_t str2);
extern const char *get_unique_string (const char *str);
extern rint_t a2i (const char *str, int base);
extern rfloat_t a2f (const char *str);
extern const char *i2a (rint_t number);
extern const char *f2a (rfloat_t number);
extern const char *mpz2a (mpz_t number, int base, int upper_case_p);
extern int mpz_ok_for_rint_p (mpz_t number);
extern rint_t mpz2i (mpz_t number);
extern void i2mpz (mpz_t mpz, rint_t i);
extern void f2mpz (mpz_t mpz, rfloat_t f);

extern size_t hash_mpz (mpz_t mpz);

#define RAW_STRING "RAW"
#define UTF8_STRING "UTF-8"
#define LATIN1_STRING "LATIN1"

extern int set_conv_descs (const char *encoding_name,
			   conv_desc_t *byte_cd, conv_desc_t *ucode_cd,
			   conv_desc_t *reverse_ucode_cd);

extern char *get_ucode_ascii_repr (ucode_t ch);
extern int read_dino_string_code (int input_char, int *correct_newln,
				  int *wrong_escape_code, int d_getc (void),
				  void d_ungetc (int));
enum read_number_code
{
  NUMBER_OK,
  NON_DECIMAL_FLOAT,
  ABSENT_EXPONENT,
  WRONG_OCTAL_INT,
};

extern enum read_number_code
read_dino_number (int curr_ch, int get_ch (void), void unget_ch (int),
		  int *read_ch_num, const char **result, int *base,
		  int *float_p, int *long_p);

extern void print_stmt_prompt (void);
extern void print_stmt_cont_prompt (void);
extern void d_verror (int fatal_error_flag, position_t position,
		      const char *format, va_list ap);
extern void d_error (int fatal_error_flag, position_t position,
		     const char *format, ...);
extern void copy_vlo (vlo_t *to, vlo_t *from);
extern void str_to_ucode_vlo (vlo_t *to, const char *from, size_t len);
extern char *encode_byte_str_vlo (byte_t *str, conv_desc_t cd, vlo_t *vlo,
				  size_t *len);
extern char *encode_ucode_str_vlo (ucode_t *str, conv_desc_t cd, vlo_t *vlo,
				   size_t *len);
extern const char *encode_ucode_str_to_raw_vlo (const ucode_t *str, vlo_t *vlo);
extern ucode_t get_ucode_from_stream (int (*get_byte) (void *), conv_desc_t cd,
				      void *data);

extern void dino_finish (int code);

#define SET_SOURCE_POSITION(ref)     (source_position = IR_pos (ref))

#define ENVIRONMENT_PSEUDO_FILE_NAME "<environment>"

#ifndef HAVE_MEMCPY
extern void *memcpy (void *to, const void *from, size_t size);
#endif /* #ifndef HAVE_MEMCPY */

#ifndef HAVE_MEMSET
extern void *memset (void *to, int value, size_t size);
#endif /* #ifndef HAVE_MEMSET */

#ifndef HAVE_MEMCMP
extern int memcmp (const void *mem1, const void *mem2, size_t size);
#endif /* #ifndef HAVE_MEMCMP */

#ifndef HAVE_MEMMOVE
extern void *memmove (void *s1, const void *s2, size_t n);
#endif /* #ifndef HAVE_MEMMOVE */

#if !defined (NO_PROFILE) && !defined (HAVE_SETITIMER)
#define HAVE_SETITIMER 0
#endif

#ifndef INLINE
#ifdef __GNUC__
#define INLINE 1
#else
#define INLINE 0
#endif
#endif

#ifdef __GNUC__
#define __always__ __attribute__ ((__always_inline__))
#else
#define __always__
#endif

#if INLINE && !defined (SMALL_CODE)
#define do_inline inline __always__
#else
#define do_inline
#endif

#define do_always_inline inline __always__

#ifdef __GNUC__
#define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#else
#define ATTRIBUTE_UNUSED
#endif



/* This page contains code for dealing with ASCII, UTF8 and
   UNICODE.  */

static inline int
in_byte_range_p (int ch)
{
  return 0 <= ch && ch <= UCHAR_MAX;
}

static inline int
in_ucode_range_p (int uc)
{
  return 0 <= uc && uc <= (int) 0x10ffff;
}

/* length of ucode string STR.  */
static inline
ucodestrlen (const ucode_t *s)
{
  size_t i;
  
  for (i = 0; s[i] != 0; i++)
    ;
  return i;
}

#include <stdio.h>

/* Read and return by from file given by DATA.  Used by
   get_ucode_from_stream.  */
static inline int
read_byte (void *data)
{
  FILE *f = (FILE *) data;
  
  return fgetc (f);
}

#endif /* #ifndef D_COMMON_H */
