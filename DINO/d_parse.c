/*
   Copyright (C) 1997-2019 Vladimir Makarov.

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

#include "d_common.h"
#include "d_ir.h"
#include "d_yacc.h"

/* The value of following var is first program stmt
   (see member stmt_list in nodes).  The value may be NULL:
   it means empty program. */
IR_node_t first_program_stmt;

extern int yylex (void);

static IR_node_t merge_friend_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_friend_list (IR_node_t list);

static IR_node_t merge_use_item_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_use_item_list (IR_node_t list);

static IR_node_t merge_exception_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_exception_list (IR_node_t list);

static IR_node_t process_var_decl (access_val_t access, IR_node_t ident, int val_flag,
                                   IR_node_t obj_block, IR_node_t expr, position_t expr_pos,
                                   IR_node_mode_t assign);
static IR_node_t process_var_pattern (access_val_t access, IR_node_t pattern, int val_flag,
                                      IR_node_t expr, position_t expr_pos, IR_node_mode_t assign);

static void process_header (int, IR_node_t, IR_node_t);
static IR_node_t process_formal_parameters (IR_node_t, IR_node_t, int);
static IR_node_t process_header_block (IR_node_t, IR_node_t, IR_node_t);

static IR_node_t merge_additional_stmts (IR_node_t);

static IR_node_t process_obj_header (IR_node_t);
static IR_node_t process_obj_block (IR_node_t, IR_node_t, IR_node_t, access_val_t);
static IR_node_t process_fun_start (IR_node_t, int, access_val_t);
static IR_node_t create_except_class (IR_node_t before_list, IR_node_t expr, position_t pos);
static IR_node_t create_catch_block (position_t pos);
static void finish_catch_block (IR_node_t catch_block, IR_node_t block, IR_node_t excepts,
                                IR_node_t friend_list);
static IR_node_t create_try_expr (IR_node_t try_block, IR_node_t stmt, IR_node_t excepts,
                                  position_t lpar_pos, position_t rpar_pos,
                                  IR_node_t additional_stmts_before_try);

/* The following vars are used by yacc analyzer. */

/* True when we can process typed statements in REPL.  */
static int repl_process_flag;

/* Pointer to block node which opens current scope. */
static IR_node_t current_scope;

/* Pointer to cycle list of stmts which should be included before the
   current stmt.  They are generated from anonymous
   functions/classes/fibers or from try-expr in the current
   stmt.  */
static IR_node_t additional_stmts;

/* This var is used as second attribute of nonterminal actual_parameters.
   Its value is source pos of corresponding left parenthesis. Its
   value is to be defined only at rule end because of nesting. */
static position_t actual_parameters_construction_pos;

/* Containers used temporary by the scanner.  */
static vlo_t temp_scanner_vlo;
static vlo_t temp_scanner_vlo2;

/* True if we did not print syntax error yet.  We can not leave
   yyparse by longjmp as we need to finalize some data.  We leave
   YYABORT in most cases but it is guaranted for all yyerors
   calls. Spurious subsequent syntax errors should be suppressed for
   better experience in REPL.  */
static int first_error_p;

/* This page contains abstracr data for reading, storing and
   retrieving lines.  */

/* Container for pointers to read lines.  */
static vlo_t lines_vec;

/* Container for read lines themself.  */
static os_t lines;

/* Initiate the abstract data.  */
static void initiate_lines (void) {
  VLO_CREATE (lines_vec, 0);
  OS_CREATE (lines, 0);
}

/* Read next line from file, store it, and return it.  The non-empty
   read line will always have NL at its end.  The trailing `\r' is
   removed.  The empty line means reaching EOF.  */
static const ucode_t *read_line (FILE *f) {
  int c;
  ucode_t uc;
  const ucode_t *ln;

  for (;;) {
    c = (curr_reverse_ucode_cd == NO_CONV_DESC
           ? dino_getc (f)
           : get_ucode_from_stream (read_byte, curr_reverse_ucode_cd, curr_encoding_type, f));
    if (c == EOF || c == '\n') break;
    if (c == UCODE_BOUND) {
      /* Skip to the end of line: */
      do {
        c = (curr_reverse_ucode_cd == NO_CONV_DESC
               ? dino_getc (f)
               : get_ucode_from_stream (read_byte, curr_reverse_ucode_cd, curr_encoding_type, f));
      } while (c != EOF && c != '\n');
      OS_TOP_NULLIFY (lines);
      d_error (TRUE, no_position, ERR_line_decoding, curr_encoding_name);
    }
    uc = c;
    OS_TOP_ADD_MEMORY (lines, &uc, sizeof (ucode_t));
  }
  if (c != EOF || OS_TOP_LENGTH (lines) > 0) {
    if (OS_TOP_LENGTH (lines) > 0 && *((ucode_t *) OS_TOP_END (lines)) == '\r')
      OS_TOP_SHORTEN (lines, sizeof (ucode_t));
    uc = '\n';
    OS_TOP_ADD_MEMORY (lines, &uc, sizeof (ucode_t));
  }
  uc = '\0';
  OS_TOP_ADD_MEMORY (lines, &uc, sizeof (ucode_t));
  ln = OS_TOP_BEGIN (lines);
  OS_TOP_FINISH (lines);
  VLO_ADD_MEMORY (lines_vec, &ln, sizeof (ucode_t *));
  return ln;
}

/* Return N-th read line.  */
ucode_t *get_read_line (int n) {
  d_assert (n >= 0 && VLO_LENGTH (lines_vec) > n * sizeof (ucode_t *));
  return ((ucode_t **) VLO_BEGIN (lines_vec))[n];
}

/* Finish the abstract data.  */
static void finish_lines (void) {
  OS_DELETE (lines);
  VLO_DELETE (lines_vec);
}

/* This page contains abstract data `istream_stack'.  This abstract
   data opens and closes included files saves and restores scanner
   states corresponding to processed files. */

/* The following structure describes current input stream and scanner
   state and is used for saving and restoring input stream and scanner
   state to process included files. */
struct istream_state {
  /* The following member is defined only for
     `curr_istream_state'. And its value NULL when the current input
     stream is undefined, command line, or REPL stdin.  */
  FILE *file;
  /* File name of given input stream. Null if the current input stream
     is undefined.  Empty string if the current input stream is
     command line or stdin in case of REPL. */
  const char *file_name;
  /* Current position in file corresponding to given input stream.
     The following member is defined only for structures in the input
     stream stack and if it is not command line stream or REPL
     stdin. */
  long file_pos;
  /* The following member contains parameter values of the function
     `add_token_back' calls. */
  vlo_t uninput_tokens;
  vlo_t recorded_tokens;
  /* Name of file encoding.  */
  const char *encoding_name;
  /* Type of file encoding.  */
  encoding_type_t encoding_type;
  /* Conversion descriptor used for the file.  */
  conv_desc_t cd;
};

/* The following structure contains all current input stream and
   scanner state.  If the member `file_name' value is NULL then input
   stream is not defined. */
static struct istream_state curr_istream_state;

/* All input stream stack is implemented by variable length object.
   See package `vl-object'. */
static vlo_t istream_stack;

/* The following variable is used for storing ungotten code. */
static int previous_char;

/* The following function creates empty input stream stack and
   initiates current input stream and scanner state (including cd) as
   undefined. */
static void initiate_istream_stack (void) {
  VLO_CREATE (istream_stack, 0);
  curr_istream_state.file_name = NULL;
  curr_istream_state.file = NULL;
  curr_istream_state.encoding_name = NULL;
  curr_istream_state.cd = NO_CONV_DESC;
  VLO_CREATE (curr_istream_state.uninput_tokens, 0);
  VLO_CREATE (curr_istream_state.recorded_tokens, 0);
}

/* The following function deletes the input stream stack and closes
   current input file and its cd if they are defined.  */
static void finish_istream_stack (void) {
  VLO_DELETE (istream_stack);
  if (curr_istream_state.file != NULL) fclose (curr_istream_state.file);
#ifdef HAVE_ICONV_H
  if (curr_istream_state.cd != NO_CONV_DESC) iconv_close (curr_istream_state.cd);
#endif
}

/* The following function returns height of input stream stack,
   i.e. current file inclusion level. */
int istream_stack_height (void) {
  return VLO_LENGTH (istream_stack) / sizeof (struct istream_state);
}

/* Ucode string containing only 0 element.  */
static ucode_t empty_ucode_string[] = {0};

/* The following function saves current input stream and scanner state
   (if it is defined) in the stack, closes file corresponding to
   current input stream and its cd, opens file corresponding to new
   input stream, and sets up new current input stream and scanner
   state including cd.  The function also checks up absence of loop of
   file inclusions.  The function reports errors if the loop exists or
   new file is not opened.  If ENCODING_NAME is not NULL, use the
   corresponding encoding.  Otherwise use the current encoding.  */
static void push_curr_istream (const char *new_file_name, const char *encoding_name,
                               position_t error_pos) {
  int i;

  if (curr_istream_state.file_name != NULL) {
    /* The current stream is defined. */
    if (*curr_istream_state.file_name != '\0') {
      /* The current stream is not command line or REPL stdin. */
      d_assert (curr_istream_state.file != stdin);
      curr_istream_state.file_pos = ftell (curr_istream_state.file);
      fclose (curr_istream_state.file);
      curr_istream_state.file = NULL;
#ifdef HAVE_ICONV_H
      if (curr_istream_state.cd != NO_CONV_DESC) iconv_close (curr_istream_state.cd);
      curr_istream_state.cd = NO_CONV_DESC;
#endif
    }
    VLO_ADD_MEMORY (istream_stack, &curr_istream_state, sizeof (struct istream_state));
    for (i = 0; i < istream_stack_height (); i++)
      if (strcmp (((struct istream_state *) VLO_BEGIN (istream_stack))[i].file_name, new_file_name)
          == 0)
        error (TRUE, error_pos, "fatal error -- cycle on inclusion of file `%s'", new_file_name);
  }
  curr_istream_state.file_name = new_file_name;
  curr_istream_state.file = NULL;
  curr_istream_state.cd = NO_CONV_DESC;
  if (*new_file_name != '\0') {
    /* The current stream is not commad line or REPL stdin. */
    curr_istream_state.file = fopen (new_file_name, "rb");
    if (curr_istream_state.file == NULL)
      system_error (TRUE, error_pos, "fatal error -- `%s': ", curr_istream_state.file_name);
    curr_istream_state.encoding_name
      = get_unique_string (encoding_name != NULL ? encoding_name : curr_encoding_name);
    if (!set_conv_descs (curr_istream_state.encoding_name, NULL, NULL, &curr_istream_state.cd,
                         &curr_istream_state.encoding_type))
      d_error (TRUE, no_position, ERR_source_file_encoding, curr_istream_state.encoding_name,
               curr_istream_state.file_name);
    else if (!check_encoding_on_ascii (curr_istream_state.encoding_name)) {
#ifdef HAVE_ICONV_H
      if (curr_istream_state.cd != NO_CONV_DESC) iconv_close (curr_istream_state.cd);
#endif
      d_error (TRUE, no_position, ERR_non_ascii_source_file_encoding,
               curr_istream_state.encoding_name, curr_istream_state.file_name);
    }
  } else if (repl_flag)
    /* To read line when we need it. */
    command_line_program = empty_ucode_string;
  previous_char = NOT_A_CHAR;
  start_file_position (curr_istream_state.file_name);
  VLO_CREATE (curr_istream_state.uninput_tokens, 0);
  VLO_CREATE (curr_istream_state.recorded_tokens, 0);
}

/* The following function closes file corresponding to current input
   stream (it must be defined) and its cdq, reopens file corresponding
   to previous input stream (if it is defined) and restores previous
   input stream and scanner state (including file cd).  The function
   can fix error if the file is not reopened. */
static void pop_istream_stack (void) {
  d_assert (curr_istream_state.file_name != NULL);
  if (curr_istream_state.file != NULL) {
    d_assert (istream_stack_height () == 0 || curr_istream_state.file != stdin);
    fclose (curr_istream_state.file);
    curr_istream_state.file = NULL;
#ifdef HAVE_ICONV_H
    if (curr_istream_state.cd != NO_CONV_DESC) iconv_close (curr_istream_state.cd);
    curr_istream_state.cd = NO_CONV_DESC;
#endif
  }
  VLO_DELETE (curr_istream_state.uninput_tokens);
  VLO_DELETE (curr_istream_state.recorded_tokens);
  if (istream_stack_height () != 0) {
    curr_istream_state
      = (((struct istream_state *) VLO_BEGIN (istream_stack))[istream_stack_height () - 1]);
    VLO_SHORTEN (istream_stack, sizeof (struct istream_state));
    if (*curr_istream_state.file_name != '\0') {
      /* It is not command line stream or REPL stdin. */
      curr_istream_state.file = fopen (curr_istream_state.file_name, "rb");
      curr_istream_state.cd = NO_CONV_DESC;
      if (curr_istream_state.file == NULL
          || fseek (curr_istream_state.file, curr_istream_state.file_pos, 0) != 0)
        system_error (TRUE, no_position,
                      "fatal error -- repeated opening file `%s': ", curr_istream_state.file_name);
      if (!set_conv_descs (curr_istream_state.encoding_name, NULL, NULL, &curr_istream_state.cd,
                           &curr_istream_state.encoding_type))
        /* We already used the file encoding before.  */
        d_assert (FALSE);
    }
    finish_file_position ();
  }
}

/* The following field returns directory name of given file name. */
static const char *file_dir_name (const char *file_name) {
  const char *last_slash;
  const char *curr_char_ptr;
  const char *result;

  d_assert (file_name != NULL);
  for (curr_char_ptr = file_name, last_slash = NULL; *curr_char_ptr != '\0'; curr_char_ptr++)
    if (*curr_char_ptr == '/') last_slash = curr_char_ptr;
  if (last_slash == NULL)
    return ""; /* current directory */
  else {
    IR_TOP_ADD_MEMORY (file_name, last_slash - file_name + 1);
    IR_TOP_ADD_BYTE ('\0');
    result = IR_TOP_BEGIN ();
    IR_TOP_FINISH ();
    return result;
  }
}

/* The following field returns full name of file with given directory
   name, file name itself, and suffix. */
static const char *file_path_name (const char *directory_name, const char *file_name,
                                   const char *file_suffix) {
  const char *result;

  d_assert (directory_name != NULL);
  IR_TOP_ADD_STRING (directory_name);
  if (strlen (directory_name) != 0 && directory_name[strlen (directory_name) - 1] != '/')
    IR_TOP_ADD_STRING ("/");
  IR_TOP_ADD_STRING (file_name);
  IR_TOP_ADD_STRING (file_suffix);
  result = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  return result;
}

#include <pwd.h>

static const char *canonical_path_name (const char *name) {
  char buf[PATH_MAX + 1];
  char *p, *str, *result;
  int sep, sep1;
  sep = sep1 = '/';
  if (*name != sep && *name != sep1) {
    getcwd (buf, PATH_MAX);
    IR_TOP_ADD_STRING (buf);
    IR_TOP_SHORTEN (1);
    IR_TOP_ADD_BYTE (sep);
    IR_TOP_ADD_BYTE ('\0');
  }
  IR_TOP_ADD_STRING (name);
  result = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  for (p = result; *p != '\0'; p++)
    if (*p == sep1) *p = sep;
  /* Remove // /. /<something>/.. */
  for (p = result; *p != '\0';)
    if (*p == sep && p[1] == sep)
      memmove (p, p + 1, strlen (p));
    else if (*p == sep && p[1] == '.' && p[2] == sep)
      memmove (p, p + 2, strlen (p + 1));
    else if (p == result && *p == sep && p[1] == '.' && p[2] == '.')
      memmove (p, p + 3, strlen (p + 2));
    else if (*p == sep && p[1] == '.' && p[2] == '.' && p[3] == sep) {
      if (p == result)
        memmove (p, p + 3, strlen (p + 2));
      else {
        for (str = p - 1; *str != sep; str--)
          ;
        memmove (str, p + 3, strlen (p + 2));
        p = str;
      }
    } else
      p++;
  return (const char *) result;
}

static int skip_spaces (const char *str, size_t from) {
  for (; isspace_ascii (str[from]); from++)
    ;
  return from;
}

/* The function finds the first string described by a pattern

   -\*-[ \t]*coding:[ \t]*[_-A-Za-z0-9/.:]+[ \t]*-\*-

  in LN and return the name after "coding:".  Return NULL if we did
  not find encoding.  */
static const char *find_encoding (char *ln) {
  size_t i, len, start, bound;
  static const char *prefix = "coding:";

  len = strlen (prefix);
  for (i = 0; ln[i] != 0; i++)
    if (ln[i] == '-' && ln[i + 1] == '*' && ln[i + 2] == '-') {
      start = i;
      i = skip_spaces (ln, i + 3);
      if (strncmp (ln + i, prefix, len) != 0) {
        /* No "coding:" -- start scanning from second "-" in
           "-*-".  */
        i = start + 1;
        continue;
      }
      start = i = skip_spaces (ln, i + len);
      for (; ln[i] != 0; i++)
        if (!isalpha_ascii (ln[i]) && !isdigit_ascii (ln[i]) && ln[i] != '-' && ln[i] != '_'
            && ln[i] != '/' && ln[i] != '.' && ln[i] != ':')
          break;
      if (ln[i] == 0) return NULL;
      bound = i;
      if (start == bound) return NULL;
      i = skip_spaces (ln, i);
      if (ln[i] == '-' && ln[i + 1] == '*' && ln[i + 2] == '-') {
        ln[bound] = '\0';
        return get_unique_string (ln + start);
      }
      i--;
    }
  return NULL;
}

static char *read_str_line (FILE *f, vlo_t *container) {
  int c;

  VLO_NULLIFY (*container);
  while ((c = dino_getc (f)) != EOF && c != '\n') VLO_ADD_BYTE (*container, c);
  VLO_ADD_BYTE (*container, '\0');
  return VLO_BEGIN (*container);
}

/* Find encoding on the first two lines of file F and return it.  If
   it is not found, return NULL.  */
static const char *read_file_encoding (FILE *f) {
  char *ln;
  const char *name;

  if (((ln = read_str_line (f, &temp_scanner_vlo)) != NULL && (name = find_encoding (ln)) != NULL)
      || ((ln = read_str_line (f, &temp_scanner_vlo)) != NULL
          && (name = find_encoding (ln)) != NULL))
    return name;
  return NULL;
}

/* The following function returns full file name.  To make this
   functions searches for files in
     1. current directory (when the current stream is command line stream)
        or in directory in which source file with the corresponding
        include-clause is placed
     2. and in directories
        given in the command line of DINO and DINO environment variable
        DINO_PATH.
     3. Standard library directory.
   If the file is not found the function returns the extended
   specification file name mentioned in 1.  The function also returns
   file encoding through ENCODING if it can read encoing name in the
   file, or NULL otherwise. */
static const char *get_full_file_and_encoding_name (IR_node_t ir_fname, const char **encoding) {
  const char *fname;
  const char *curr_directory_name;
  const char *real_file_name;
  const char *file_name;
  const char **path_directory_ptr;
  size_t i, len;
  FILE *curr_file;

  curr_directory_name
    = (*curr_istream_state.file_name == '\0' ? "" : file_dir_name (curr_istream_state.file_name));
  VLO_NULLIFY (temp_scanner_vlo);
  VLO_NULLIFY (temp_scanner_vlo2);
  if (IR_IS_OF_TYPE (ir_fname, IR_NM_string)) {
    VLO_ADD_STRING (temp_scanner_vlo2, IR_string_value (IR_unique_string (ir_fname)));
    fname = encode_byte_str_vlo (VLO_BEGIN (temp_scanner_vlo2), curr_byte_cd, curr_encoding_type,
                                 &temp_scanner_vlo, &len);
    if (fname != NULL) {
      /* Check NULL bytes:  */
      for (i = 0; i < len && fname[i] != 0; i++)
        ;
      if (i < len) fname = NULL;
    }
  } else if (curr_ucode_cd != NO_CONV_DESC) /* ucodestr */
  {
    ucodestr_t ustr = IR_ucodestr_value (IR_unique_ucodestr (ir_fname));

    for (len = 0; ustr[len] != 0; len++)
      ;
    VLO_ADD_MEMORY (temp_scanner_vlo2, ustr, sizeof (ucode_t) * (len + 1));
    fname = encode_ucode_str_vlo (VLO_BEGIN (temp_scanner_vlo2), curr_ucode_cd, curr_encoding_type,
                                  &temp_scanner_vlo, &len);
    if (fname != NULL) {
      for (i = 0; i < len && fname[i] != 0; i++)
        ;
      if (i < len) fname = NULL;
    }
  } else
    fname = encode_ucode_str_to_raw_vlo (IR_ucodestr_value (IR_unique_ucodestr (ir_fname)),
                                         &temp_scanner_vlo);
  if (fname == NULL)
    error (TRUE, IR_pos (ir_fname), ERR_file_name_cannot_represented_in_current_encoding);
  real_file_name = file_path_name (curr_directory_name, fname, STANDARD_INPUT_FILE_SUFFIX);
  curr_file = fopen (real_file_name, "rb");
  if (curr_file == NULL)
    for (path_directory_ptr = include_path_directories; *path_directory_ptr != NULL;
         path_directory_ptr++) {
      file_name = file_path_name (*path_directory_ptr, fname, STANDARD_INPUT_FILE_SUFFIX);
      curr_file = fopen (file_name, "rb");
      if (curr_file != NULL) {
        real_file_name = file_name;
        break;
      }
    }
  if (encoding != NULL) {
    *encoding = NULL;
    if (curr_file != NULL) *encoding = read_file_encoding (curr_file);
  }
  if (curr_file != NULL && fclose (curr_file) == EOF)
    system_error (TRUE, no_position, "fatal error -- `%s': ", real_file_name);
  return canonical_path_name (real_file_name);
}

/* Return file encoding found in FNAME.  If the file can not be read
   or there is no encoding in the file, return NULL.  */
const char *source_file_encoding (const char *fname) {
  FILE *f;
  const char *res;

  f = fopen (fname, "rb");
  if (f == NULL) return NULL;
  res = read_file_encoding (f);
  fclose (f);
  return res;
}

/* This page contains abstract data `scanner'.  This abstract data
   divides input stream characters on tokens (tokens).

   There is requirement of back control (from parser to scanner).
   This back control is needed to switch to processing new file after
   recognizing string in construction `include'.  After recognizing
   the first string the parser is to tell scanner that the next token
   must be INCLUSION.  After getting such token the parser is
   switched correctly onto processing new file.

   When EOF is read the scanner returns END_OF_FILE only when input
   stream stack is empty, otherwise returns token
   END_OF_INCLUDE_FILE.  */

/* The following variable referes for current char of parsed
   environment.  If it refers for zero character, the environment has
   been parsed already. */
static const char *environment;

/* The following variable is position which will be set up after the
   parsing environment. */
static position_t after_environment_position;

/* The variable is used for implementation of d_getc when reading from
   the command line string. */
static int curr_char_number;

/* True if we did not see non-blank chars yet for the current REPL
   bunch of stmts.  */
static int first_repl_empty_line_p;

/* Getc for dino.  Reading unicode and replacing "\r\n" onto "\n". */
static int d_getc (void) {
  int result;

  if (*environment != '\0') {
    result = *environment++;
    /* Environment is always Latin-1 string.  */
    d_assert (in_byte_range_p (result));
    if (*environment == 0) {
      /* Restore the position. */
      current_position = after_environment_position;
      if (result == '\n') /* It will be incremented back. */
        current_position.line_number--;
    }
  } else if (curr_istream_state.file != NULL) {
    if (previous_char == NOT_A_CHAR) {
      result
        = (curr_reverse_ucode_cd == NO_CONV_DESC
             ? dino_getc (curr_istream_state.file)
             : get_ucode_from_stream (read_byte, curr_istream_state.cd,
                                      curr_istream_state.encoding_type, curr_istream_state.file));
      if (result == UCODE_BOUND)
        d_error (TRUE, current_position, ERR_file_decoding, curr_istream_state.encoding_name);
    } else {
      result = previous_char;
      previous_char = NOT_A_CHAR;
    }
    if (result == '\r') {
      result
        = (curr_istream_state.cd == NO_CONV_DESC
             ? dino_getc (curr_istream_state.file)
             : get_ucode_from_stream (read_byte, curr_istream_state.cd,
                                      curr_istream_state.encoding_type, curr_istream_state.file));
      if (result == UCODE_BOUND)
        d_error (TRUE, current_position, ERR_file_decoding, curr_istream_state.encoding_name);
      if (result != '\n') {
        previous_char = result;
        result = '\r';
      }
    }
  } else {
    d_assert (command_line_program != NULL);
    result = command_line_program[curr_char_number];
    if (repl_flag && result == 0) {
      command_line_program = read_line (stdin);
      if (first_repl_empty_line_p) {
        int c;

        for (curr_char_number = 0; (c = command_line_program[curr_char_number]) == ' ' || c == '\t'
                                   || c == '\n' || c == '\r' || c == 'f';
             curr_char_number++)
          ;
        first_repl_empty_line_p = c == '\0';
        /* Don't reuse curr_char_number value, we need to process
           positions correctly.  */
      }
      curr_char_number = 0;
      result = *command_line_program;
    }
    if (result == 0)
      result = EOF;
    else
      curr_char_number++;
  }
  return result;
}

/* Ungetc for dino. */
static void d_ungetc (int ch) {
  if (*environment != '\0')
    environment--;
  else if (curr_istream_state.file != NULL) {
    d_assert (previous_char == NOT_A_CHAR);
    previous_char = ch;
  } else {
    d_assert (command_line_program != NULL);
    if (curr_char_number != 0 && ch != EOF) curr_char_number--;
  }
}

/* Used by REPL to skip the all line.  */
void skip_line_rest (void) {
  d_assert (repl_flag && *environment == 0);
  /* Close all include files.  */
  while (istream_stack_height () != 0) {
    previous_char = NOT_A_CHAR;
    pop_istream_stack ();
  }
  command_line_program = empty_ucode_string;
  curr_char_number = 0;
  current_position.line_number++;
}

enum token_code {
  NUMBER = 256,
  CHARACTER,
  STRING,
  IDENT,
  CODE,
  BREAK,
  CASE,
  CATCH,
  CHAR,
  CLASS,
  CONTINUE,
  ELSE,
  EXPOSE,
  EXTERN,
  FIBER,
  FINAL,
  FLOAT,
  FOR,
  FORMER,
  FRIEND,
  FUN,
  HIDE,
  HIDEBLOCK,
  IF,
  IN,
  INT,
  LONG,
  LATER,
  NEW,
  NIL,
  OBJ,
  PMATCH,
  PRIV,
  PUB,
  REQUIRE,
  RETURN,
  RMATCH,
  TAB,
  THIS,
  THREAD,
  THROW,
  TRY,
  TYPE,
  USE,
  VAL,
  VAR,
  VEC,
  WAIT,
  LOGICAL_OR,
  LOGICAL_AND,
  EQ,
  NE,
  IDENTITY,
  UNIDENTITY,
  LE,
  GE,
  LSHIFT,
  RSHIFT,
  ASHIFT,
  MULT_ASSIGN,
  DIV_ASSIGN,
  MOD_ASSIGN,
  PLUS_ASSIGN,
  MINUS_ASSIGN,
  CONCAT_ASSIGN,
  LSHIFT_ASSIGN,
  RSHIFT_ASSIGN,
  ASHIFT_ASSIGN,
  AND_ASSIGN,
  XOR_ASSIGN,
  OR_ASSIGN,
  INCR,
  DECR,
  DOTS,
  FOLD_PLUS,
  FOLD_MULT,
  FOLD_AND,
  FOLD_XOR,
  FOLD_OR,
  FOLD_CONCAT,
  INCLUDE,
  INCLUSION,
  END_OF_FILE,
  END_OF_INCLUDE_FILE,
  WILDCARD,
};

struct token {
  int code;
  position_t pos;
  IR_node_t pointer;
};

static struct token curr_token;

/* Var length string used by function curr_token for text presentation of
   the symbol. */
static vlo_t symbol_text;

#include "d_strtab.h"

static struct str_code kw_tab_els[] = {
  {"_", WILDCARD},
  {"break", BREAK},
  {"case", CASE},
  {"catch", CATCH},
  {"char", CHAR},
  {"class", CLASS},
  {"continue", CONTINUE},
  {"expose", EXPOSE},
  {"extern", EXTERN},
  {"else", ELSE},
  {"fiber", FIBER},
  {"final", FINAL},
  {"float", FLOAT},
  {"for", FOR},
  {"former", FORMER},
  {"friend", FRIEND},
  {"fun", FUN},
  {"hide", HIDE},
  {"hideblock", HIDEBLOCK},
  {"if", IF},
  {"in", IN},
  {"include", INCLUDE},
  {"int", INT},
  {"later", LATER},
  {"long", LONG},
  {"new", NEW},
  {"nil", NIL},
  {"obj", OBJ},
  {"pmatch", PMATCH},
  {"priv", PRIV},
  {"pub", PUB},
  {"require", REQUIRE},
  {"return", RETURN},
  {"rmatch", RMATCH},
  {"tab", TAB},
  {"thread", THREAD},
  {"this", THIS},
  {"throw", THROW},
  {"try", TRY},
  {"type", TYPE},
  {"use", USE},
  {"val", VAL},
  {"var", VAR},
  {"vec", VEC},
  {"wait", WAIT},
};

static hash_table_t kw_tab;

/* The following function recognizes next source symbol from the input
   file, returns its code, modifies var current_position so that its
   value is equal to position of the current character in the input
   file and sets up var source_position so that its value is equal to
   position of the returned symbol start, creates corresponding code
   node (if it is needed) and sets up curr_token to the node address.  The
   function skips all white spaces and commentaries and fixes all
   lexical errors. */
int yylex (void) {
  int wrong_escape_code;
  int input_char;
  ucode_t uc;
  int number_of_successive_error_characters;
  int last_repl_process_flag = repl_process_flag;

  repl_process_flag = FALSE;
  VLO_NULLIFY (symbol_text);
  for (number_of_successive_error_characters = 0;;) {
    input_char = d_getc ();
    /* `current_position' corresponds `input_char' here */
    switch (input_char) {
      /* Break results in skipping all white spaces. */
    case ' ':
    case '\f': current_position.column_number++; break;
    case '\t':
      current_position.column_number
        = ((current_position.column_number - 1) / TAB_STOP + 1) * TAB_STOP + 1;
      break;
    case '\n':
      current_position.column_number = 1;
      current_position.line_number++;
      if (repl_flag) {
        if (last_repl_process_flag) return curr_token.code = END_OF_FILE;
        if (*environment == 0) {
          if (first_repl_empty_line_p)
            print_stmt_prompt ();
          else
            print_stmt_cont_prompt ();
        }
      }
      break;
    case '\r': current_position.column_number++; break;
    case '~':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = XOR_ASSIGN;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '~';
      }
    case '+':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '+') {
        current_position.column_number++;
        return curr_token.code = INCR;
      } else if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = PLUS_ASSIGN;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '+';
      }
    case '-':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '-') {
        current_position.column_number++;
        return curr_token.code = DECR;
      } else if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = MINUS_ASSIGN;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '-';
      }
    case '=':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        input_char = d_getc ();
        if (input_char == '=') {
          current_position.column_number++;
          return curr_token.code = IDENTITY;
        } else {
          d_ungetc (input_char);
          return curr_token.code = EQ;
        }
      } else {
        d_ungetc (input_char);
        return curr_token.code = '=';
      }
    case '@':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = CONCAT_ASSIGN;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '@';
      }
    case '<':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = LE;
      } else if (input_char == '<') {
        current_position.column_number++;
        input_char = d_getc ();
        if (input_char == '=') {
          current_position.column_number++;
          return curr_token.code = LSHIFT_ASSIGN;
        } else {
          d_ungetc (input_char);
          return curr_token.code = LSHIFT;
        }
      } else {
        d_ungetc (input_char);
        return curr_token.code = '<';
      }
    case '>':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = GE;
      } else if (input_char == '>') {
        current_position.column_number++;
        input_char = d_getc ();
        if (input_char == '=') {
          current_position.column_number++;
          return curr_token.code = ASHIFT_ASSIGN;
        } else if (input_char == '>') {
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=') {
            current_position.column_number++;
            return curr_token.code = RSHIFT_ASSIGN;
          } else {
            d_ungetc (input_char);
            return curr_token.code = RSHIFT;
          }
        } else {
          d_ungetc (input_char);
          return curr_token.code = ASHIFT;
        }
      } else {
        d_ungetc (input_char);
        return curr_token.code = '>';
      }
    case '*':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = MULT_ASSIGN;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '*';
      }
    case '/':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = DIV_ASSIGN;
      } else if (input_char == '/') {
        /* commentary */
        for (;;) {
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '\n') {
            current_position.column_number = 1;
            current_position.line_number++;
            break;
          } else if (input_char == EOF)
            break;
        }
        break;
      } else if (input_char == '*') {
        current_position.column_number++;
        for (;;) {
          input_char = d_getc ();
          if (input_char == '*') {
            current_position.column_number++;
            input_char = d_getc ();
            if (input_char == '/') {
              current_position.column_number++;
              break;
            } else
              d_ungetc (input_char);
          } else if (input_char == '\n') {
            current_position.column_number = 1;
            current_position.line_number++;
          } else if (input_char == EOF) {
            error (FALSE, current_position, ERR_eof_in_comment);
            break;
          }
        }
        break;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '/';
      }
    case '&':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '&') {
        current_position.column_number++;
        return curr_token.code = LOGICAL_AND;
      } else if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = AND_ASSIGN;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '&';
      }
    case '|':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '|') {
        current_position.column_number++;
        return curr_token.code = LOGICAL_OR;
      } else if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = OR_ASSIGN;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '|';
      }
    case '%':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = MOD_ASSIGN;
      } else if (input_char != '{') {
        d_ungetc (input_char);
        return curr_token.code = '%';
      } else {
        char *string_value_in_code_memory;
        IR_node_t unique_string_node_ptr;
        conv_desc_t saved_reverse_ucode_cd = curr_reverse_ucode_cd;

        current_position.column_number++;
        curr_reverse_ucode_cd = NO_CONV_DESC;
        VLO_NULLIFY (symbol_text);
        curr_token.pointer = create_node_with_pos (IR_NM_code, current_position);
        for (;;) {
          input_char = d_getc ();
          if (input_char == '%') {
            current_position.column_number++;
            input_char = d_getc ();
            if (input_char == '}') {
              current_position.column_number++;
              break;
            } else {
              d_ungetc (input_char);
              input_char = '%';
            }
          } else if (input_char == '\n') {
            current_position.column_number = 1;
            current_position.line_number++;
          } else if (input_char == EOF) {
            error (FALSE, current_position, ERR_eof_in_C_code);
            break;
          }
          VLO_ADD_BYTE (symbol_text, input_char);
        }
        VLO_ADD_BYTE (symbol_text, '\0');
        curr_reverse_ucode_cd = saved_reverse_ucode_cd;
        IR_set_string_value (temp_unique_string, VLO_BEGIN (symbol_text));
        unique_string_node_ptr = *find_table_entry (temp_unique_string, FALSE);
        if (unique_string_node_ptr == NULL) {
          unique_string_node_ptr
            = create_unique_node_with_string (IR_NM_unique_string, VLO_BEGIN (symbol_text),
                                              VLO_LENGTH (symbol_text),
                                              &string_value_in_code_memory);
          IR_set_string_value (unique_string_node_ptr, string_value_in_code_memory);
          include_to_table (unique_string_node_ptr);
        }
        IR_set_code_string (curr_token.pointer, unique_string_node_ptr);
        return curr_token.code = CODE;
      }
    case '^':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        return curr_token.code = XOR_ASSIGN;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '^';
      }
    case '!':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '=') {
        current_position.column_number++;
        input_char = d_getc ();
        if (input_char == '=') {
          current_position.column_number++;
          return curr_token.code = UNIDENTITY;
        } else {
          d_ungetc (input_char);
          return curr_token.code = NE;
        }
      } else {
        d_ungetc (input_char);
        return curr_token.code = '!';
      }
    case '.':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      if (input_char == '.') {
        current_position.column_number++;
        input_char = d_getc ();
        if (input_char == '.') {
          current_position.column_number++;
          return curr_token.code = DOTS;
        } else {
          current_position.column_number--;
          error (FALSE, current_position, ERR_invalid_input_char);
          current_position.column_number++;
          d_ungetc (input_char);
          return curr_token.code = '.';
        }
      } else if (input_char == '+') {
        current_position.column_number++;
        return curr_token.code = FOLD_PLUS;
      } else if (input_char == '*') {
        current_position.column_number++;
        return curr_token.code = FOLD_MULT;
      } else if (input_char == '&') {
        current_position.column_number++;
        return curr_token.code = FOLD_AND;
      } else if (input_char == '^') {
        current_position.column_number++;
        return curr_token.code = FOLD_XOR;
      } else if (input_char == '|') {
        current_position.column_number++;
        return curr_token.code = FOLD_OR;
      } else if (input_char == '@') {
        current_position.column_number++;
        return curr_token.code = FOLD_CONCAT;
      } else {
        d_ungetc (input_char);
        return curr_token.code = '.';
      }
    case ':':
    case ',':
    case ';':
    case '?':
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
    case '#':
      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      return curr_token.code = input_char;
    case EOF:
      curr_token.pos = source_position = current_position;
      return curr_token.code = (istream_stack_height () == 0 ? END_OF_FILE : END_OF_INCLUDE_FILE);
    case '\'': {
      IR_node_t unique_char_node_ptr;
      int correct_newln;
      int character_code;

      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      input_char = d_getc ();
      current_position.column_number++;
      if (input_char == '\'') {
        current_position.column_number--;
        error (FALSE, current_position, ERR_invalid_char_constant);
        current_position.column_number++;
      } else {
        input_char = read_dino_string_code (input_char, &correct_newln, &wrong_escape_code, d_getc,
                                            d_ungetc);
        if (input_char < 0 || correct_newln) {
          current_position.column_number--;
          error (FALSE, current_position, ERR_invalid_char_constant);
          current_position.column_number++;
        } else if (wrong_escape_code) {
          current_position.column_number--;
          error (FALSE, current_position, ERR_invalid_escape_code);
          current_position.column_number++;
        }
      }
      current_position.column_number++;
      character_code = d_getc ();
      if (character_code != '\'') {
        current_position.column_number--;
        d_ungetc (character_code);
        error (FALSE, current_position, ERR_invalid_char_constant);
      }
      IR_set_char_value (temp_unique_char, input_char);
      unique_char_node_ptr = *find_table_entry (temp_unique_char, FALSE);
      if (unique_char_node_ptr == NULL) {
        unique_char_node_ptr = create_node (IR_NM_unique_char);
        IR_set_char_value (unique_char_node_ptr, input_char);
        include_to_table (unique_char_node_ptr);
      }
      curr_token.pointer = create_node_with_pos (IR_NM_char, source_position);
      IR_set_unique_char (curr_token.pointer, unique_char_node_ptr);
      return curr_token.code = CHARACTER;
    }
    case '\"':
    case '`': {
      int correct_newln, unicode_p;
      IR_node_t unique_string_node_ptr;
      char *string_value_in_code_memory;
      int no_escape_p = input_char == '`';

      curr_token.pos = source_position = current_position;
      current_position.column_number++;
      unicode_p = FALSE;
      for (;;) {
        input_char = d_getc ();
        current_position.column_number++;
        if (no_escape_p) {
          if (input_char == '`') {
            input_char = d_getc ();
            if (input_char == '`')
              current_position.column_number++;
            else {
              d_ungetc (input_char);
              break;
            }
          } else if (input_char == '\n') {
            d_ungetc (input_char);
            error (FALSE, current_position, ERR_string_end_absence);
            break;
          }
        } else {
          if (input_char == '\"') break;
          input_char = read_dino_string_code (input_char, &correct_newln, &wrong_escape_code,
                                              d_getc, d_ungetc);
        }
        if (input_char < 0) {
          error (FALSE, current_position, ERR_string_end_absence);
          break;
        }
        if (!no_escape_p && wrong_escape_code) {
          error (FALSE, current_position, ERR_invalid_escape_code);
          continue;
        }
        if (no_escape_p || !correct_newln) {
          if (!unicode_p && !in_byte_range_p (input_char)) {
            /* Transform accumulated string into ucode
               string: */
            unicode_p = TRUE;
            copy_vlo (&temp_scanner_vlo, &symbol_text);
            str_to_ucode_vlo (&symbol_text, VLO_BEGIN (temp_scanner_vlo), VLO_LENGTH (symbol_text));
          }
          if (unicode_p) {
            uc = input_char;
            VLO_ADD_MEMORY (symbol_text, &uc, sizeof (ucode_t));
          } else
            VLO_ADD_BYTE (symbol_text, input_char);
        }
      }
      if (unicode_p) {
        uc = '\0';
        VLO_ADD_MEMORY (symbol_text, &uc, sizeof (ucode_t));
        IR_set_ucodestr_value (temp_unique_ucodestr, VLO_BEGIN (symbol_text));
        d_assert (VLO_LENGTH (symbol_text) > sizeof (ucode_t));
        IR_set_ucodestr_size (temp_unique_ucodestr, VLO_LENGTH (symbol_text) - sizeof (ucode_t));
        unique_string_node_ptr = *find_table_entry (temp_unique_ucodestr, FALSE);
        if (unique_string_node_ptr == NULL) {
          unique_string_node_ptr
            = create_unique_node_with_string (IR_NM_unique_ucodestr, VLO_BEGIN (symbol_text),
                                              VLO_LENGTH (symbol_text),
                                              &string_value_in_code_memory);
          IR_set_ucodestr_value (unique_string_node_ptr, (ucodestr_t) string_value_in_code_memory);
          IR_set_ucodestr_size (unique_string_node_ptr,
                                VLO_LENGTH (symbol_text) - sizeof (ucode_t));
          include_to_table (unique_string_node_ptr);
        }
        curr_token.pointer = create_node_with_pos (IR_NM_ucodestr, source_position);
        IR_set_unique_ucodestr (curr_token.pointer, unique_string_node_ptr);
      } else {
        VLO_ADD_BYTE (symbol_text, '\0');
        IR_set_string_value (temp_unique_string, VLO_BEGIN (symbol_text));
        unique_string_node_ptr = *find_table_entry (temp_unique_string, FALSE);
        if (unique_string_node_ptr == NULL) {
          unique_string_node_ptr
            = create_unique_node_with_string (IR_NM_unique_string, VLO_BEGIN (symbol_text),
                                              VLO_LENGTH (symbol_text),
                                              &string_value_in_code_memory);
          IR_set_string_value (unique_string_node_ptr, string_value_in_code_memory);
          include_to_table (unique_string_node_ptr);
        }
        curr_token.pointer = create_node_with_pos (IR_NM_string, source_position);
        IR_set_unique_string (curr_token.pointer, unique_string_node_ptr);
      }
      return curr_token.code = STRING;
    }
    default:
      if (isalpha_ascii (input_char) || input_char == '_') {
        int keyword;

        curr_token.pos = source_position = current_position;
        /* Ident recognition. */
        do {
          current_position.column_number++;
          VLO_ADD_BYTE (symbol_text, input_char);
          input_char = d_getc ();
        } while (isalpha_ascii (input_char) || isdigit_ascii (input_char) || input_char == '_');
        d_ungetc (input_char);
        VLO_ADD_BYTE (symbol_text, '\0');
        keyword = find_str_code (kw_tab, VLO_BEGIN (symbol_text), 0);
        if (keyword != 0)
          return curr_token.code = keyword;
        else {
          IR_node_t unique_ident;

          unique_ident = create_unique_ident_node (VLO_BEGIN (symbol_text));
          curr_token.pointer = create_node_with_pos (IR_NM_ident, source_position);
          IR_set_unique_ident (curr_token.pointer, unique_ident);
          return curr_token.code = IDENT;
        }
      } else if (isdigit_ascii (input_char)) {
        /* Recognition numbers. */
        enum read_number_code err_code;
        int read_ch_num, float_p, long_p, base;
        const char *result; /* Number is always ASCII sequence.  */

        curr_token.pos = source_position = current_position;
        current_position.column_number++;
        err_code = read_dino_number (input_char, d_getc, d_ungetc, &read_ch_num, &result, &base,
                                     &float_p, &long_p);
        if (err_code == ABSENT_EXPONENT) {
          error (FALSE, source_position, ERR_exponent_absence);
          curr_token.pointer = get_float_node (0.0, source_position);
        } else if (err_code == NON_DECIMAL_FLOAT) {
          error (FALSE, source_position, ERR_float_value_not_in_decimal_base);
          curr_token.pointer = get_float_node (0.0, source_position);
        } else if (err_code == WRONG_OCTAL_INT) {
          error (FALSE, source_position, ERR_octal_int_value);
          curr_token.pointer = get_int_node (0, source_position);
        } else {
          d_assert (err_code == NUMBER_OK);
          if (long_p)
            curr_token.pointer = get_long_node (result, source_position, base);
          else {
            if (float_p) {
              curr_token.pointer = get_float_node (a2f (result), source_position);
              if (errno) error (FALSE, source_position, ERR_float_value);
            } else {
              curr_token.pointer = get_int_node (a2i (result, base), source_position);
              if (errno) error (FALSE, source_position, ERR_int_value);
            }
          }
        }
        current_position.column_number += read_ch_num;
        return curr_token.code = NUMBER;
      } else {
        number_of_successive_error_characters++;
        if (number_of_successive_error_characters == 1)
          error (FALSE, current_position, ERR_invalid_input_char);
        current_position.column_number++;
      }
    }
  }
}

static IR_node_t get_new_ident (position_t pos) {
  IR_node_t ident, unique_ident;
  char str[50]; /* Enough for integer representation.  */

  VLO_NULLIFY (symbol_text);
  VLO_ADD_STRING (symbol_text, "$anon");
  if (pos.file_name != NULL && *pos.file_name != 0) {
    VLO_ADD_STRING (symbol_text, ".");
    VLO_ADD_STRING (symbol_text, pos.file_name);
  }
  VLO_ADD_STRING (symbol_text, ".ln");
  sprintf (str, "%d", pos.line_number);
  VLO_ADD_STRING (symbol_text, str);
  VLO_ADD_STRING (symbol_text, ".pos");
  sprintf (str, "%d", pos.column_number);
  VLO_ADD_STRING (symbol_text, str);
  unique_ident = create_unique_ident_node (VLO_BEGIN (symbol_text));
  ident = create_node_with_pos (IR_NM_ident, pos);
  IR_set_unique_ident (ident, unique_ident);
  return ident;
}

/* The following function initiates internal state of the scanner.
   The function must be called only once before any work with the
   scanner. */
void initiate_scanner (void) {
  if (repl_flag) initiate_lines ();
  initiate_istream_stack ();
  curr_char_number = 0;
  environment = ENVIRONMENT;
  VLO_CREATE (symbol_text, 500);
  VLO_CREATE (temp_scanner_vlo, 500);
  VLO_CREATE (temp_scanner_vlo2, 500);
}

/* The following function is called to tune the scanner on input
   stream from given file.  The function is needed for nested
   procession of included files.  If the FILE_NAME is empty string
   then it is command line stream.  If it is the first scanner file,
   special code for correct diganostic during and after parsing
   environment is executed. */
void start_scanner_file (const char *new_file_name, const char *encoding_name,
                         position_t error_pos) {
  push_curr_istream (new_file_name, encoding_name, error_pos);
  if (*environment != 0) {
    /* Environment is not processed yet. Save the position. */
    after_environment_position = current_position;
    /* File name for environment. */
    current_position.file_name = ENVIRONMENT_PSEUDO_FILE_NAME;
  }
}

/* The following function restores scanner state concerning previous
   input stream with the aid of abstract data `istream_stack'. */
static void finish_scanner_file (void) { pop_istream_stack (); }

/* The following function is called from the parser and points out
   that token given as parameter will be returned by following
   function `yylex' call. */
static void add_token_back (const struct token *token) {
  VLO_ADD_MEMORY (curr_istream_state.uninput_tokens, token, sizeof (struct token));
}

/* The function frees all memory allocated during the scanner work. */
void finish_scanner (void) {
  VLO_DELETE (symbol_text);
  VLO_DELETE (temp_scanner_vlo);
  VLO_DELETE (temp_scanner_vlo2);
  if (repl_flag) finish_lines ();
  finish_istream_stack ();
}

/* This page contains functions needed for processing
   include-clauses. */

/* Current covering block level (1, 2, ...).  0 means no covering
   block. */
static int block_level = 0;

/* VLO for pointers to names of include files and numbers of include
   file names for each covering blocks. */
static vlo_t include_file_names;

/* Number of include files for the current block. */
static int curr_block_include_file_names_number;

/* The function starts new block.  It should be also called for the
   implicit top-level block. */
static void start_block (void) {
  if (block_level == 0)
    VLO_CREATE (include_file_names, 0);
  else
    VLO_ADD_MEMORY (include_file_names, &curr_block_include_file_names_number,
                    sizeof (curr_block_include_file_names_number));
  curr_block_include_file_names_number = 0;
  block_level++;
}

/* The function finishes the block. */
static void finish_block (void) {
  d_assert (block_level > 0);
  block_level--;
  if (block_level == 0)
    VLO_DELETE (include_file_names);
  else {
    VLO_SHORTEN (include_file_names, curr_block_include_file_names_number * sizeof (char *));
    memcpy (&curr_block_include_file_names_number,
            (char *) VLO_END (include_file_names) - sizeof (curr_block_include_file_names_number)
              + 1,
            sizeof (curr_block_include_file_names_number));
    VLO_SHORTEN (include_file_names, sizeof (curr_block_include_file_names_number));
  }
}

/* The function adds new include file name for the list of include
   files.  The function returns TRUE if the file has been not inserted
   into the list yet.  Otherwise it returns FALSE.  NAME is string in
   the include-clase. */
static int add_include_file (IR_node_t fname) {
  const char *name;
  char **names;
  int i;

  d_assert (block_level > 0);
  name = get_full_file_and_encoding_name (fname, NULL);
  names = (char **) ((char *) VLO_END (include_file_names) + 1
                     - sizeof (char *) * curr_block_include_file_names_number);
  for (i = 0; i < curr_block_include_file_names_number; i++)
    if (strcmp (names[i], name) == 0) break;
  if (i < curr_block_include_file_names_number) return FALSE;
  VLO_ADD_MEMORY (include_file_names, &name, sizeof (name));
  curr_block_include_file_names_number++;
  return TRUE;
}

/* Return true if we can evaluate parsed stmnts in REPL.  */
static int repl_can_process_p (void) {
  return (repl_flag
          && block_level == 1
          /* To exclude include files */
          && istream_stack_height () == 0
          /* To process all environment at once.  Environment[0] is \n
             in case of finishing environment.  */
          && (*environment == '\0' || environment[1] == '\0'));
}

void initiate_parser (void) {
  kw_tab = create_str_code_tab (kw_tab_els, sizeof (kw_tab_els) / sizeof (struct str_code));
  current_scope = create_empty_block (NULL);
  start_block ();
  additional_stmts = NULL;
  first_error_p = TRUE;
}

void initiate_new_parser_REPL_stmts (void) {
  d_assert (repl_flag);
  first_error_p = first_repl_empty_line_p = TRUE;
}

void finish_parser (void) {
  finish_block ();
  finish_str_code_tab (kw_tab);
}

static IR_node_t get_new_ident (position_t);

static void finish_scanner_file (void);

static void start_block (void);
static void finish_block (void);
static int add_include_file (IR_node_t fname);

static int repl_can_process_p (void);

static const IR_node_t err_node = (IR_node_t) 1;

static int record_level;

static void read_token () {
  if (VLO_LENGTH (curr_istream_state.uninput_tokens) == 0) {
    yylex ();
  } else {
    curr_token = ((struct token *) VLO_BOUND (curr_istream_state.uninput_tokens))[-1];
    VLO_SHORTEN (curr_istream_state.uninput_tokens, sizeof (struct token));
    source_position = curr_token.pos;
  }
  d_assert (curr_token.code >= 0);
  if (record_level > 0) {
    VLO_EXPAND (curr_istream_state.recorded_tokens, sizeof (struct token));
    ((struct token *) VLO_BOUND (curr_istream_state.recorded_tokens))[-1] = curr_token;
  }
}

static size_t record_start () {
  assert (record_level >= 0);
  if (curr_token.code < 0) read_token ();
  record_level++;
  if (record_level == 1) {
    VLO_EXPAND (curr_istream_state.recorded_tokens, sizeof (struct token));
    ((struct token *) VLO_BOUND (curr_istream_state.recorded_tokens))[-1] = curr_token;
  }
  return VLO_LENGTH (curr_istream_state.recorded_tokens);
}

static void record_stop (size_t mark, int restore_p) {
  int i, n = (VLO_LENGTH (curr_istream_state.recorded_tokens) - mark) / sizeof (struct token);
  struct token *bound;

  assert (record_level > 0 && n >= 0);
  record_level--;
  if (!restore_p) {
    if (record_level == 0)
      VLO_SHORTEN (curr_istream_state.recorded_tokens, (n + 1) * sizeof (struct token));
    return;
  }
  bound = (struct token *) VLO_BOUND (curr_istream_state.recorded_tokens);
  for (i = 1; i <= n; i++) {
    VLO_EXPAND (curr_istream_state.uninput_tokens, sizeof (struct token));
    ((struct token *) VLO_BOUND (curr_istream_state.uninput_tokens))[-1] = bound[-i];
  }
  curr_token = ((struct token *) VLO_BOUND (curr_istream_state.recorded_tokens))[-n - 1];
  source_position = curr_token.pos;
  if (record_level == 0)
    VLO_SHORTEN (curr_istream_state.recorded_tokens, (n + 1) * sizeof (struct token));
  else
    VLO_SHORTEN (curr_istream_state.recorded_tokens, n * sizeof (struct token));
}

/* This function is called by parser and for fatal error reporting. */
static void syntax_error (position_t pos) {
  if (!repl_flag || first_error_p) d_error (FALSE, pos, "%s", "syntax error");
  first_error_p = FALSE;
}

static void error_recovery (int par_lev, const char *expected) {
  if (record_level != 0) syntax_error (curr_token.pos);
  if (repl_flag) return;
  for (;;) {
    if (curr_token.code == END_OF_FILE || curr_token.code == END_OF_INCLUDE_FILE
        || (par_lev == 0 && curr_token.code == ';'))
      break;
    if (curr_token.code == '{') {
      par_lev++;
    } else if (curr_token.code == '}') {
      if (--par_lev <= 0) break;
    }
    read_token ();
  }
  if (curr_token.code != END_OF_FILE && curr_token.code != END_OF_INCLUDE_FILE) read_token ();
}

#define P(f)                                        \
  do {                                              \
    if ((r = (f) (no_err_p)) == err_node) return r; \
  } while (0)
#define PA(f, a)                                       \
  do {                                                 \
    if ((r = (f) (no_err_p, a)) == err_node) return r; \
  } while (0)
#define PTFAIL(t)                                         \
  do {                                                    \
    if (record_level == 0) syntax_error (curr_token.pos); \
    return err_node;                                      \
  } while (0)

#define PT(t)               \
  do {                      \
    if (!M (t)) PTFAIL (t); \
  } while (0)

#define PTP(t, pos)               \
  do {                            \
    if (!MP (t, pos)) PTFAIL (t); \
  } while (0)

#define PTN(t)                  \
  do {                          \
    if (!MN (t, r)) PTFAIL (t); \
  } while (0)

#define PE(f, l)                                  \
  do {                                            \
    if ((r = (f) (no_err_p)) == err_node) goto l; \
  } while (0)

#define PAE(f, a, l)                                 \
  do {                                               \
    if ((r = (f) (no_err_p, a)) == err_node) goto l; \
  } while (0)

#define PTE(t, pos, l)        \
  do {                        \
    if (!MP (t, pos)) goto l; \
  } while (0)

typedef IR_node_t (*nonterm_func_t) (int);
typedef IR_node_t (*nonterm_arg_func_t) (int, void *);

#define D(f) static IR_node_t f (int no_err_p)
#define DA(f) static IR_node_t f (int no_err_p, void *arg)

static void access_token (void) {
  if (curr_token.code < 0) read_token ();
}

static int check_token (int c) {
  access_token ();
  return c == curr_token.code;
}

#define C(c) check_token (c)

static int match (int c, position_t *pos, IR_node_t *node) {
  if (curr_token.code < 0) read_token ();
  if (curr_token.code != c) return FALSE;
  if (pos != NULL) *pos = curr_token.pos;
  if (node != NULL) *node = curr_token.pointer;
  curr_token.code = -1;
  return TRUE;
}

#define M(c) match (c, NULL, NULL)
#define MP(c, pos) match (c, &(pos), NULL)
#define MN(c, node) match (c, NULL, &(node))

static IR_node_t try_f (nonterm_func_t f) {
  IR_node_t save = additional_stmts;
  int save_flag = repl_process_flag;
  size_t mark = record_start ();
  IR_node_t r = (f) (TRUE);

  record_stop (mark, r == err_node);
  if (r == err_node) {
    additional_stmts = save;
    repl_process_flag = save_flag;
  }
  return r;
}

static IR_node_t try_arg_f (nonterm_arg_func_t f, void *arg) {
  IR_node_t save = additional_stmts;
  int save_flag = repl_process_flag;
  size_t mark = record_start ();
  IR_node_t r = (f) (TRUE, arg);

  record_stop (mark, r == err_node);
  if (r == err_node) {
    additional_stmts = save;
    repl_process_flag = save_flag;
  }
  return r;
}

#define TRY_F(f) try_f (f)
#define TRY_FA(f, arg) try_arg_f (f, arg)

D (expr);

DA (elist_parts_list) {
  /* ElistPartsList = [ Expr [ ":" Expr ] {"," Expr [ ":" Expr ] } ] */
  IR_node_t r, first_expr, el, last, res = NULL;
  position_t pos;
  int first_p, flag = (ptrdiff_t) arg;

  for (first_p = TRUE;; first_p = FALSE) {
    pos = source_position;
    P (expr);
    //    pos = IR_pos (r);
    first_expr = r;
    if (M (':')) {
      P (expr);
      el = create_node_with_pos (IR_NM_elist_element, pos);
      IR_set_repetition_key_flag (el, TRUE);
      IR_set_repetition_key (el, first_expr);
      IR_set_expr (el, r);
    } else {
      el = create_node_with_pos (IR_NM_elist_element, pos);
      if (flag) { /* vec */
        IR_set_repetition_key (el, get_int_node (1, pos));
        IR_set_expr (el, r);
      } else {
        IR_set_repetition_key (el, r);
        IR_set_expr (el, create_node_with_pos (IR_NM_nil, pos));
      }
    }
    if (res == NULL) {
      IR_set_next_elist (el, el);
    } else {
      IR_set_next_elist (el, IR_next_elist (res));
      IR_set_next_elist (res, el);
    }
    res = el;
    if (!M (',')) break;
  }
  last = res;
  res = IR_next_elist (res);
  IR_set_next_elist (last, NULL);
  return res;
}

D (except_class_list) {
  /* ExceptClassList = Expr { "," Expr } */
  IR_node_t r, res = NULL;

  for (;;) {
    P (expr);
    access_token ();
    res = create_except_class (res, r, curr_token.pos);
    if (!M (',')) break;
  }
  return res;
}

D (fun_fiber_class);
DA (formal_parameters);
D (block);

D (anon_func) {
  IR_node_t r, res, pars, body, ffc, fps, hd, hint_ident;
  int dots_p;

  /* AnonFunc = AnonHeader Hint Block
     AnonHeader = FuncFiberClass FormalParameters */
  P (fun_fiber_class);
  ffc = r;
  IR_set_final_flag (ffc, TRUE);
  process_header (TRUE, ffc, get_new_ident (IR_pos (ffc)));
  PA (formal_parameters, &dots_p);
  fps = r;
  hd = process_formal_parameters (ffc, fps, dots_p);
  hint_ident = NULL;
  if (M ('!')) {
    PTN (IDENT);
    hint_ident = r;
  }
  P (block);
  body = process_header_block (hd, r, hint_ident);
  res = IR_next_stmt (body);
  additional_stmts = merge_stmt_lists (additional_stmts, body);
  return IR_ident (res);
}

enum stmt_list_end { DEFAULT_END, PAREN_END, COMMA_PAREN_END, CASE_END };

DA (executive_stmt);

D (term) {
  /* Expr : IDENT | NUMBER | CHARACTER | nil | "(" Expr ")"
            | "["  ElistPartsList "]" | tab "["  ElistPartsList "]" | STRING
            | char | int | long | float | hide | hideblock | vec | tab | fun | fiber | class
            | obj | thread | type | this | char "(" Expr ")" | int "(" Expr ")"
            | long "(" Expr ")" | float "(" Expr ")"
            | vec "(" Expr ["," Expr] ")" | tab "(" Expr ")" | type "(" Expr ")"
            | AnonFunc | try "(" ExecutiveStmt [ ","  ExceptClassList] ")"
            | "_" | "..."
   */
  IR_node_t r, res, first_expr, fun, add_stmts, try_stmt;
  position_t pos, pos2;

  if (MP ('(', pos)) {
    P (expr);
    res = r;
    if (!IR_IS_OF_TYPE (r, IR_NM_paren)) {
      res = create_node_with_pos (IR_NM_paren, pos);
      IR_set_operand (res, r);
    }
    PT (')');
    return res;
  }
  if (MP ('[', pos)) {
    if (C (']')) {
      r = NULL;
    } else {
      PA (elist_parts_list, (void *) TRUE);
    }
    PT (']');
    res = create_node_with_pos (IR_NM_vec, pos);
    IR_set_elist (res, r);
    return res;
  }
  if ((r = TRY_F (anon_func)) != err_node) {
    return r;
  }
  access_token ();
  switch (curr_token.code) {
  case IDENT: PTN (IDENT); return r;
  case NUMBER: PTN (NUMBER); return r;
  case CHARACTER: PTN (CHARACTER); return r;
  case STRING: PTN (STRING); return r;
  case NIL: PTP (NIL, pos); return create_node_with_pos (IR_NM_nil, pos);
  case CHAR:
    PTP (CHAR, pos);
    if (!M ('(')) return create_node_with_pos (IR_NM_char_type, pos);
    res = create_node_with_pos (IR_NM_charof, pos);
    P (expr);
    IR_set_operand (res, r);
    PT (')');
    return res;
  case INT:
    PTP (INT, pos);
    if (!M ('(')) return create_node_with_pos (IR_NM_int_type, pos);
    res = create_node_with_pos (IR_NM_intof, pos);
    P (expr);
    IR_set_operand (res, r);
    PT (')');
    return res;
  case LONG:
    PTP (LONG, pos);
    if (!M ('(')) return create_node_with_pos (IR_NM_long_type, pos);
    res = create_node_with_pos (IR_NM_longof, pos);
    P (expr);
    IR_set_operand (res, r);
    PT (')');
    return res;
  case FLOAT:
    PTP (FLOAT, pos);
    if (!M ('(')) return create_node_with_pos (IR_NM_float_type, pos);
    res = create_node_with_pos (IR_NM_floatof, pos);
    P (expr);
    IR_set_operand (res, r);
    PT (')');
    return res;
  case VEC:
    PTP (VEC, pos);
    if (!M ('(')) return create_node_with_pos (IR_NM_vec_type, pos);
    P (expr);
    first_expr = r;
    if (M (',')) {
      res = create_node_with_pos (IR_NM_format_vecof, pos);
      IR_set_left_operand (res, first_expr);
      P (expr);
      IR_set_right_operand (res, r);
    } else {
      res = create_node_with_pos (IR_NM_vecof, pos);
      IR_set_operand (res, first_expr);
    }
    PT (')');
    return res;
  case TAB:
    PTP (TAB, pos);
    if (M ('[')) {
      if (C (']')) {
        r = NULL;
      } else {
        PA (elist_parts_list, (void *) FALSE);
      }
      PT (']');
      res = create_node_with_pos (IR_NM_tab, pos);
      IR_set_elist (res, r);
      return res;
    }
    if (!M ('(')) return create_node_with_pos (IR_NM_tab_type, pos);
    res = create_node_with_pos (IR_NM_tabof, pos);
    P (expr);
    IR_set_operand (res, r);
    PT (')');
    return res;
  case TYPE:
    PTP (TYPE, pos2);
    if (!M ('(')) return create_node_with_pos (IR_NM_type_type, pos);
    res = create_node_with_pos (IR_NM_typeof, pos);
    P (expr);
    IR_set_operand (res, r);
    PT (')');
    return res;
  case THIS: PTP (THIS, pos); return create_node_with_pos (IR_NM_this, pos);
  case HIDE: PTP (HIDE, pos); return create_node_with_pos (IR_NM_hide_type, pos);
  case HIDEBLOCK: PTP (HIDEBLOCK, pos); return create_node_with_pos (IR_NM_hideblock_type, pos);
  case FUN: PTP (FUN, pos); return create_node_with_pos (IR_NM_fun_type, pos);
  case FIBER: PTP (FIBER, pos); return create_node_with_pos (IR_NM_fiber_type, pos);
  case CLASS: PTP (CLASS, pos); return create_node_with_pos (IR_NM_class_type, pos);
  case OBJ: PTP (OBJ, pos); return create_node_with_pos (IR_NM_stack_type, pos);
  case THREAD: PTP (THREAD, pos); return create_node_with_pos (IR_NM_thread_type, pos);
  case WILDCARD: PTP (WILDCARD, pos); return create_node_with_pos (IR_NM_wildcard, pos);
  case DOTS: PTP (DOTS, pos); return create_node_with_pos (IR_NM_dots, pos);
  case TRY:
    PT (TRY);
    PTP ('(', pos);
    {
      /* Create anonymous fun: */
      fun = create_node (IR_NM_fun);
      IR_set_fiber_flag (fun, FALSE);
      IR_set_pos (fun, pos);
      IR_set_final_flag (fun, TRUE);
      IR_set_args_flag (fun, FALSE);
      process_header (TRUE, fun, get_new_ident (pos));          /* creates fun block */
      start_block ();                                           /* start func block */
      current_scope = res = create_empty_block (current_scope); /* try block */
      IR_set_pos (current_scope, pos);
      start_block (); /* start try block */
      add_stmts = additional_stmts;
      additional_stmts = NULL;
    }
    PA (executive_stmt, (void *) COMMA_PAREN_END);
    try_stmt = r;
    if (!MP (',', pos2))
      r = create_except_class (NULL, get_ident_node (EXCEPT_NAME, pos2), pos2);
    else
      P (except_class_list);
    PTP (')', pos2);
    return create_try_expr (res, try_stmt, r, pos, pos2, add_stmts);
  default: return err_node;
  }
}

D (designator_or_call) {
  /* DesignatorOrCall = DesignatorOrCall "["  Expr "]"
                      | DesignatorOrCall "["  [Expr] ":" [Expr] [":" Expr] "]"
                      | DesignatorOrCall ActualParameters
                      | DesignatorOrCall "."  IDENT
                      | Term
     ActualParameters = "(" [ Expr { "," Expr } ] ")"
  */
  IR_node_t r, des, start, bound, step, res, arguments, last;
  position_t pos, pos2, pos3;

  P (term);
  res = r;
  for (;;) {
    if (MP ('[', pos)) {
      des = res;
      if (C (':')) {
        start = NULL;
      } else {
        P (expr);
        start = r;
      }
      if (start != NULL && C (']')) {
        res = create_node_with_pos (IR_NM_index, pos);
        IR_set_designator (res, des);
        IR_set_component (res, start);
        PT (']');
      } else {
        PTP (':', pos2);
        if (C (':') || C (']')) {
          bound = NULL;
        } else {
          P (expr);
          bound = r;
        }
        step = NULL;
        if (M (':')) {
          P (expr);
          step = r;
        }
        PTP (']', pos3);
        res = create_node_with_pos (IR_NM_slice, pos);
        IR_set_designator (res, des);
        if (start == NULL) start = get_int_node (0, pos2);
        IR_set_component (res, start);
        if (bound == NULL) bound = get_int_node (-1, pos2);
        IR_set_bound (res, bound);
        if (step == NULL) step = get_int_node (1, pos3);
        IR_set_step (res, step);
      }
    } else if (MP ('.', pos)) {
      PTN (IDENT);
      des = res;
      res = create_node_with_pos (IR_NM_period, pos);
      IR_set_designator (res, des);
      IR_set_component (res, r);
    } else if (MP ('(', pos)) {
      des = res;
      res = create_node_with_pos (IR_NM_class_fun_fiber_call, pos);
      IR_set_fun_expr (res, des);
      arguments = NULL;
      if (!M (')')) {
        for (;;) {
          P (expr);
          last = arguments;
          arguments = create_node_with_pos (IR_NM_elist_element, IR_pos (r));
          IR_set_repetition_key (arguments, NULL);
          IR_set_expr (arguments, r);
          if (last == NULL) {
            IR_set_next_elist (arguments, arguments);
          } else {
            IR_set_next_elist (arguments, IR_next_elist (last));
            IR_set_next_elist (last, arguments);
          }
          if (!MP (',', pos)) break;
        }
        last = arguments;
        arguments = IR_next_elist (arguments);
        IR_set_next_elist (last, NULL);
        PT (')');
      }
      IR_set_actuals (res, arguments);
    } else {
      break;
    }
  }
  return res;
}

D (designator) {
  IR_node_t r;

  P (designator_or_call);
  if (!IR_IS_OF_TYPE (r, IR_NM_index) && !IR_IS_OF_TYPE (r, IR_NM_slice)
      && !IR_IS_OF_TYPE (r, IR_NM_period) && !IR_IS_OF_TYPE (r, IR_NM_ident)) {
    if (record_level == 0) syntax_error (IR_pos (r));
    r = err_node;
  }
  return r;
}

static inline IR_node_t setup_right_op (IR_node_t n, IR_node_t op) {
  if (n == NULL) {
    n = op;
  } else {
    IR_set_right_operand (n, op);
  }
  return n;
}

static inline IR_node_t setup_left_op (position_t pos, IR_node_mode_t mode, IR_node_t op) {
  IR_node_t res = create_node_with_pos (mode, pos);

  IR_set_left_operand (res, op);
  return res;
}

D (non_mult_expr) {
  /* Expr : "!" Expr | "+" Expr | "-" Expr | "~" Expr | "#" Expr | ".+" Expr | ".*" Expr
          | ".&" Expr | ".^" Expr | ".|" Expr | ".@" Expr | final Expr | new Expr
   */
  IR_node_mode_t mode;
  IR_node_t r, res = NULL;
  position_t pos;

  access_token ();
  switch (curr_token.code) {
  case '!':
    MP ('!', pos);
    mode = IR_NM_not;
    break;
  case '+':
    MP ('+', pos);
    mode = IR_NM_unary_plus;
    break;
  case '-':
    MP ('-', pos);
    mode = IR_NM_unary_minus;
    break;
  case '#':
    MP ('#', pos);
    mode = IR_NM_length;
    break;
  case '~':
    MP ('~', pos);
    mode = IR_NM_bitwise_not;
    break;
  case FOLD_PLUS:
    MP (FOLD_PLUS, pos);
    mode = IR_NM_fold_plus;
    break;
  case FOLD_MULT:
    MP (FOLD_MULT, pos);
    mode = IR_NM_fold_mult;
    break;
  case FOLD_AND:
    MP (FOLD_AND, pos);
    mode = IR_NM_fold_and;
    break;
  case FOLD_XOR:
    MP (FOLD_XOR, pos);
    mode = IR_NM_fold_xor;
    break;
  case FOLD_OR:
    MP (FOLD_OR, pos);
    mode = IR_NM_fold_or;
    break;
  case FOLD_CONCAT:
    MP (FOLD_CONCAT, pos);
    mode = IR_NM_fold_concat;
    break;
  case FINAL:
    MP (FINAL, pos);
    mode = IR_NM_const;
    break;
  case NEW:
    MP (NEW, pos);
    mode = IR_NM_new;
    break;
  default: P (designator_or_call); return r;
  }
  P (non_mult_expr);
  res = create_node_with_pos (mode, pos);
  IR_set_operand (res, r);
  return res;
}

D (non_add_expr) {
  /* Expr = Expr "*" Expr
     Expr = Expr "/" Expr
     Expr = Expr "%" Expr
   */
  IR_node_mode_t mode;
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_mult_expr);
    res = setup_right_op (res, r);
    mode = (MP ('*', pos) ? IR_NM_mult
                          : MP ('/', pos) ? IR_NM_div : MP ('%', pos) ? IR_NM_mod : IR_NM__error);
    if (mode == IR_NM__error) break;
    res = setup_left_op (pos, mode, res);
  }
  return res;
}

D (non_concat_expr) {
  /* Expr = Expr "+" Expr
     Expr = Expr "-" Expr
   */
  IR_node_mode_t mode;
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_add_expr);
    res = setup_right_op (res, r);
    mode = (MP ('+', pos) ? IR_NM_plus : MP ('-', pos) ? IR_NM_minus : IR_NM__error);
    if (mode == IR_NM__error) break;
    res = setup_left_op (pos, mode, res);
  }
  return res;
}

D (non_shift_expr) {
  /* Expr = Expr "@" Expr
   */
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_concat_expr);
    res = setup_right_op (res, r);
    if (!MP ('@', pos)) break;
    res = setup_left_op (pos, IR_NM_concat, res);
  }
  return res;
}

D (non_cmp_expr) {
  /* Expr = Expr "<<" Expr
     Expr = Expr ">>" Expr
     Expr = Expr ">>>" Expr
   */
  IR_node_mode_t mode;
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_shift_expr);
    res = setup_right_op (res, r);
    mode = (MP (LSHIFT, pos)
              ? IR_NM_lshift
              : MP (RSHIFT, pos) ? IR_NM_rshift : MP (ASHIFT, pos) ? IR_NM_ashift : IR_NM__error);
    if (mode == IR_NM__error) break;
    res = setup_left_op (pos, mode, res);
  }
  return res;
}

D (non_eq_expr) {
  /* Expr = Expr "<" Expr
     Expr = Expr ">" Expr
     Expr = Expr "<=" Expr
     Expr = Expr ">=" Expr
   */
  IR_node_mode_t mode;
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_cmp_expr);
    res = setup_right_op (res, r);
    mode = (MP ('<', pos)
              ? IR_NM_lt
              : MP ('>', pos) ? IR_NM_gt
                              : MP (LE, pos) ? IR_NM_le : MP (GE, pos) ? IR_NM_ge : IR_NM__error);
    if (mode == IR_NM__error) break;
    res = setup_left_op (pos, mode, res);
  }
  return res;
}

D (non_and_expr) {
  /* Expr = Expr "==" Expr
     Expr = Expr "!=" Expr
     Expr = Expr "===" Expr
     Expr = Expr "!==" Expr
   */
  IR_node_mode_t mode;
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_eq_expr);
    res = setup_right_op (res, r);
    mode
      = (MP (EQ, pos) ? IR_NM_eq
                      : MP (NE, pos) ? IR_NM_ne
                                     : MP (IDENTITY, pos)
                                         ? IR_NM_identity
                                         : MP (UNIDENTITY, pos) ? IR_NM_unidentity : IR_NM__error);
    if (mode == IR_NM__error) break;
    res = setup_left_op (pos, mode, res);
  }
  return res;
}

D (non_xor_expr) {
  /* Expr = Expr "&" Expr */
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_and_expr);
    res = setup_right_op (res, r);
    if (!MP ('&', pos)) break;
    res = setup_left_op (pos, IR_NM_and, res);
  }
  return res;
}

D (non_or_expr) {
  /* Expr = Expr "^" Expr */
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_xor_expr);
    res = setup_right_op (res, r);
    if (!MP ('^', pos)) break;
    res = setup_left_op (pos, IR_NM_xor, res);
  }
  return res;
}

D (non_in_expr) {
  /* Expr = Expr "|" Expr */
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_or_expr);
    res = setup_right_op (res, r);
    if (!MP ('|', pos)) break;
    res = setup_left_op (pos, IR_NM_or, res);
  }
  return res;
}

D (non_land_expr) {
  /* Expr = Expr in Expr */
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_in_expr);
    res = setup_right_op (res, r);
    if (!MP (IN, pos)) break;
    res = setup_left_op (pos, IR_NM_in, res);
  }
  return res;
}

static inline IR_node_t setup_logical_right_op (IR_node_t n, IR_node_t op) {
  if (n == NULL) {
    n = op;
  } else {
    IR_set_cont_operand (n, op);
  }
  return n;
}

static inline IR_node_t setup_logical_left_op (position_t pos, IR_node_mode_t mode, IR_node_t op) {
  IR_node_t res = create_node_with_pos (mode, pos);

  IR_set_operand (res, op);
  return res;
}

D (non_lor_expr) {
  /* Expr = Expr "&&" Expr */
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_land_expr);
    res = setup_logical_right_op (res, r);
    if (!MP (LOGICAL_AND, pos)) break;
    res = setup_logical_left_op (pos, IR_NM_logical_and, res);
  }
  return res;
}

D (non_cond_expr) {
  /* Expr = Expr "||" Expr */
  IR_node_t r, res = NULL;
  position_t pos;

  for (;;) {
    P (non_lor_expr);
    res = setup_logical_right_op (res, r);
    if (!MP (LOGICAL_OR, pos)) break;
    res = setup_logical_left_op (pos, IR_NM_logical_or, res);
  }
  return res;
}

D (expr) {
  /* Expr = Expr '?' Expr ':' Expr */
  IR_node_t r, res;
  position_t pos;

  P (non_cond_expr);
  if (!MP ('?', pos)) return r;
  res = create_node_with_pos (IR_NM_cond, pos);
  IR_set_cond_expr (res, r);
  P (expr);
  IR_set_true_expr (res, r);
  PT (':');
  P (expr);
  IR_set_false_expr (res, r);
  return res;
}

#define END_EXEC_STMT(end)                                       \
  if (((end != PAREN_END && end != COMMA_PAREN_END) || !C (')')) \
      && (end != COMMA_PAREN_END || !C (',')))                   \
  PT (';')

DA (assignment_stmt) {
  /* AssignmentStmt = Designator Assign  Expr ";"
                    | Designator ("++" | "--")  ";" | ("++" | "--") Designator  ";"
     Assign = "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "@=" | "<<=" | ">>=" | ">>>="
            | "&=" | "^=" | "|="
  */
  enum stmt_list_end end = (ptrdiff_t) arg;
  IR_node_mode_t mode;
  IR_node_t r, res;
  position_t pos;
  int decr_p = FALSE;

  if (MP (INCR, pos) || (decr_p = MP (DECR, pos))) {
    P (designator);
    res = create_node_with_pos (IR_NM_plus_assign, pos);
    IR_set_assignment_var (res, r);
    IR_set_assignment_expr (res, get_int_node (decr_p ? -1 : 1, pos));
    END_EXEC_STMT (end);
    return res;
  }
  P (designator);
  if (MP (INCR, pos) || (decr_p = MP (DECR, pos))) {
    res = create_node_with_pos (IR_NM_plus_assign, pos);
    IR_set_assignment_var (res, r);
    IR_set_assignment_expr (res, get_int_node (decr_p ? -1 : 1, pos));
  } else {
    access_token ();
    switch (curr_token.code) {
    case '=':
      MP ('=', pos);
      mode = IR_NM_assign;
      break;
    case MULT_ASSIGN:
      MP (MULT_ASSIGN, pos);
      mode = IR_NM_mult_assign;
      break;
    case DIV_ASSIGN:
      MP (DIV_ASSIGN, pos);
      mode = IR_NM_div_assign;
      break;
    case MOD_ASSIGN:
      MP (MOD_ASSIGN, pos);
      mode = IR_NM_mod_assign;
      break;
    case PLUS_ASSIGN:
      MP (PLUS_ASSIGN, pos);
      mode = IR_NM_plus_assign;
      break;
    case MINUS_ASSIGN:
      MP (MINUS_ASSIGN, pos);
      mode = IR_NM_minus_assign;
      break;
    case CONCAT_ASSIGN:
      MP (CONCAT_ASSIGN, pos);
      mode = IR_NM_concat_assign;
      break;
    case LSHIFT_ASSIGN:
      MP (LSHIFT_ASSIGN, pos);
      mode = IR_NM_lshift_assign;
      break;
    case RSHIFT_ASSIGN:
      MP (RSHIFT_ASSIGN, pos);
      mode = IR_NM_rshift_assign;
      break;
    case ASHIFT_ASSIGN:
      MP (ASHIFT_ASSIGN, pos);
      mode = IR_NM_ashift_assign;
      break;
    case AND_ASSIGN:
      MP (AND_ASSIGN, pos);
      mode = IR_NM_and_assign;
      break;
    case XOR_ASSIGN:
      MP (XOR_ASSIGN, pos);
      mode = IR_NM_xor_assign;
      break;
    case OR_ASSIGN:
      MP (OR_ASSIGN, pos);
      mode = IR_NM_or_assign;
      break;
    default: return err_node;
    }
    res = create_node_with_pos (mode, pos);
    IR_set_assignment_var (res, r);
    P (expr);
    IR_set_assignment_expr (res, r);
  }
  END_EXEC_STMT (end);
  return res;
}

DA (stmt);

D (for_in) { /* for  "(" Designator in Expr ")" Stmt  */
  IR_node_t r, res;
  position_t pos;

  PTP (FOR, pos);
  res = create_node_with_pos (IR_NM_foreach_stmt, pos);
  PT ('(');
  P (designator);
  IR_set_foreach_index_designator (res, r);
  PT (IN);
  P (expr);
  IR_set_foreach_tab (res, r);
  PT (')');
  PA (stmt, (void *) DEFAULT_END);
  IR_set_foreach_stmts (res, uncycle_stmt_list (r));
  return res;
}

D (pattern) { /* Pattern = Expr */
  IR_node_t r;

  P (expr);
  return r;
}

DA (stmt_list);

DA (executive_stmt) {
  /* ExecutiveStmt = ";"
                   | Expr ";"
                   | AssignmentStmt
                   | if  "(" Expr ")" Stmt [ else Stmt ]
                   | for  "(" Stmt ForGuardExpr ";"  Stmt ")" Stmt
                   | for  "(" Designator in Expr ")" Stmt
                   | (pmatch | rmatch) "(" Expr ")" "{" CaseList "}"
                   | break ";"
                   | continue ";"
                   | return  [ Expr ] ";"
                   | throw  Expr ";"
                   | wait  "(" Expr ")" Stmt
                   | BlockStmt
                   | TryBlockStmt
                   | C_CODE
     ForGuardExpr = [Expr]
     CaseList = { case PatternExpr [CaseCond] ":" StmtList }
     CaseCond = if Expr
     BlockStmt = Block
     TryBlockStmt = try Block { Catch }
     Catch = catch  "(" ExceptClassList ")" Block
  */
  enum stmt_list_end end = (ptrdiff_t) arg;
  IR_node_mode_t mode;
  IR_node_t r, res;
  position_t pos;
  int flag;

  if ((res = TRY_FA (assignment_stmt, arg)) != err_node) {
  } else if (MN (CODE, r)) {
    res = r;
  } else if (MP (IF, pos)) {
    PT ('(');
    P (expr);
    res = create_node_with_pos (IR_NM_if_stmt, pos);
    IR_set_if_expr (res, r);
    PT (')');
    PA (stmt, (void *) DEFAULT_END);
    IR_set_if_part (res, uncycle_stmt_list (r));
    IR_set_else_part (res, NULL);
    if (M (ELSE)) {
      PA (stmt, (void *) DEFAULT_END);
      IR_set_else_part (res, uncycle_stmt_list (r));
    }
  } else if ((res = TRY_F (for_in)) != err_node) {
  } else if (MP (FOR, pos)) {
    PT ('(');
    PA (stmt, (void *) DEFAULT_END);
    res = create_node_with_pos (IR_NM_for_stmt, pos);
    IR_set_for_initial_stmt (res, uncycle_stmt_list (r));
    IR_set_for_guard_expr (res, NULL);
    if (M (';')) {
      IR_set_for_guard_expr (res, get_int_node (1, source_position));
    } else {
      P (expr);
      IR_set_for_guard_expr (res, r);
      PT (';');
    }
    PA (stmt, (void *) PAREN_END);
    IR_set_for_iterate_stmt (res, uncycle_stmt_list (r));
    PT (')');
    PA (stmt, (void *) DEFAULT_END);
    IR_set_for_stmts (res, uncycle_stmt_list (r));
  } else if ((flag = MP (PMATCH, pos)) || MP (RMATCH, pos)) {
    IR_node_t list, c, save, pat, cond, break_stmt;

    res = create_node_with_pos (flag ? IR_NM_pmatch_stmt : IR_NM_rmatch_stmt, pos);
    PT ('(');
    P (expr);
    IR_set_match_expr (res, r);
    PT (')');
    PT ('{');
    for (list = NULL;;) {
      if (!M (CASE)) break;
      c = current_scope = create_empty_block (current_scope);
      save = additional_stmts;
      additional_stmts = NULL;
      P (pattern);
      pat = r;
      cond = NULL;
      if (M (IF)) {
        P (expr);
        cond = r;
      }
      PT (':');
      PA (stmt_list, (void *) CASE_END);
      IR_set_match_stmt (c, res);
      IR_set_case_pattern (c, pat);
      IR_set_case_cond (c, cond);
      /* Add implicit break at the end of case-stmts. */
      access_token ();
      break_stmt = create_node_with_pos (IR_NM_break_stmt, curr_token.pos);
      IR_set_next_stmt (break_stmt, break_stmt);
      IR_set_implicit_case_break_stmt (c, break_stmt);
      IR_set_block_stmts (c, uncycle_stmt_list (merge_stmt_lists (r, break_stmt)));
      IR_set_next_stmt (c, c);
      current_scope = IR_block_scope (c);
      additional_stmts = save;
      list = merge_stmt_lists (list, c);
    }
    IR_set_cases (res, uncycle_stmt_list (list));
    PT ('}');
  } else if (MP (BREAK, pos)) {
    res = create_node_with_pos (IR_NM_break_stmt, pos);
    END_EXEC_STMT (end);
  } else if (MP (CONTINUE, pos)) {
    res = create_node_with_pos (IR_NM_continue_stmt, pos);
    END_EXEC_STMT (end);
  } else if (MP (RETURN, pos)) {
    if (C (';') || C (')') || C (',')) {
      res = create_node_with_pos (IR_NM_return_without_result, pos);
    } else {
      P (expr);
      res = create_node_with_pos (IR_NM_return_with_result, pos);
      IR_set_returned_expr (res, r);
    }
    END_EXEC_STMT (end);
  } else if (MP (THROW, pos)) {
    P (expr);
    res = create_node_with_pos (IR_NM_throw, pos);
    IR_set_throw_expr (res, r);
    END_EXEC_STMT (end);
  } else if (MP (WAIT, pos)) {
    PT ('(');
    P (expr);
    res = create_node_with_pos (IR_NM_wait_stmt, pos);
    IR_set_wait_guard_expr (res, r);
    PT (')');
    PA (stmt, (void *) DEFAULT_END);
    IR_set_wait_stmt (res, uncycle_stmt_list (r));
  } else if (C ('{')) {
    res = create_empty_block (current_scope);
    current_scope = res;
    P (block);
    IR_set_block_stmts (res, uncycle_stmt_list (r));
    IR_set_friend_list (res, uncycle_friend_list (IR_friend_list (res)));
    current_scope = IR_block_scope (res);
  } else if (C (TRY)) {
    IR_node_t list, el, b;

    if ((r = TRY_F (expr)) != err_node) {
      res = create_node_with_pos (IR_NM_expr_stmt, IR_pos (r));
      IR_set_stmt_expr (res, r);
      END_EXEC_STMT (end);
    } else {
      PT (TRY);
      res = current_scope = create_empty_block (current_scope);
      P (block);
      IR_set_block_stmts (res, uncycle_stmt_list (r));
      IR_set_friend_list (res, uncycle_friend_list (IR_friend_list (res)));
      current_scope = IR_block_scope (res);
      for (list = NULL;;) {
        if (!M (CATCH)) break;
        PT ('(');
        P (except_class_list);
        el = r;
        PTP (')', pos);
        b = create_catch_block (pos);
        P (block);
        finish_catch_block (b, r, el, uncycle_friend_list (IR_friend_list (current_scope)));
        list = merge_exception_lists (list, el);
      }
      IR_set_exceptions (res, uncycle_exception_list (list));
    }
  } else if (!C (';') && ((end != PAREN_END && end != COMMA_PAREN_END) || !C (')'))
             && (end != COMMA_PAREN_END || !C (','))) {
    P (expr);
    res = create_node_with_pos (IR_NM_expr_stmt, IR_pos (r));
    IR_set_stmt_expr (res, r);
    END_EXEC_STMT (end);
  } else { /* empty stmt */
    res = NULL;
    END_EXEC_STMT (end);
  }
  return res;
}

struct val_access {
  int val_p;
  access_val_t access;
};

DA (pattern_var) { /* PatternVar = PatternExpr "="  Expr */
  const struct val_access *var_access = (struct val_access *) arg;
  IR_node_t r, var;
  position_t pos;

  P (expr);
  var = r;
  PTP ('=', pos);
  P (expr);
  return process_var_pattern (var_access->access, var, var_access->val_p, r, pos,
                              IR_NM_var_pattern_assign);
}

D (qual_ident) { /* QualIdent = IDENT {"." IDENT} */
  IR_node_t r, res, n;
  position_t pos;

  for (res = NULL;;) {
    PTN (IDENT);
    if (res == NULL)
      res = r;
    else {
      n = create_node_with_pos (IR_NM_period, pos);
      IR_set_designator (n, res);
      IR_set_component (n, r);
      res = n;
    }
    if (!MP ('.', pos)) break;
    if (M ('*')) {
      // ??? undo FOLD_MULT
      break;
    }
  }
  return res;
}

DA (inclusion) {
  IR_node_t r, fn = arg;
  const char *fname, *encoding;

  if (!C (INCLUSION)) return NULL;
  current_position = curr_token.pos;
  fname = get_full_file_and_encoding_name (fn, &encoding);
  start_scanner_file (fname, encoding, IR_pos (fn));
  PT (INCLUSION);
  PA (stmt_list, (void *) DEFAULT_END);
  PT (END_OF_INCLUDE_FILE);
  finish_scanner_file ();
  return r;
}

static position_t block_end_pos;

D (declaration) {
  /* Declaration = VarDeclarations | FriendClause | ExternDeclarations | FuncClassDeclaration
                 | SingletonObject | ForwardDeclaration | ExposeClause | UseClause
                 | IncludeDeclaration | RequireClause
     VarDeclarations =  [pub | priv] (val | var)  VarList ";"
     VarList = Var { "," Var }
     Var = IDENT | PatternVar
     FriendClause = friend IDENT { "," IDENT } ";"
     ExternDeclarations = [pub | priv] extern ExternItems ";"
     ExternItems = ExternItem { "," ExternItem }
     ExternItem = IDENT | IDENT  "(" ")"
     SingletonObject = [pub | priv] obj IDENT block
     ForwardDeclaration = Header ";"
     Header = [Qualifiers] FuncFiberClass IDENT
     Qualifiers = pub | priv | final | pub final | priv final | final pub | final priv
     FuncClassDeclaration = Header FormalParameters Hint Block
     Hint = [ "!" IDENT ]
     ExposeClause = expose ExposeItem { "," ExposeItem } ";"
     ExposeItem = QualIdent ["(" IDENT ")"] | QaulIdent ".*"
     UseClause = use QualIdent { UseItemClause } ";"
     UseItemClause = [former | later] UseItem { "," UseItem }
     IncludeDeclaration = include ["+"] STRING ";"
     RequireClause = REQUIRE NUMBER ";"
   */
  access_val_t access;
  IR_node_t r, ident, qident, expose, list, res = NULL;
  struct token start_token, token_after_access;
  int val_flag, final_p;
  position_t pos;
  int dots_p;

  access_token ();
  start_token = token_after_access = curr_token;
  if (M (PRIV) || M (PUB)) {
    access_token ();
    token_after_access = curr_token;
    add_token_back (&token_after_access);
    curr_token = start_token;
    source_position = curr_token.pos;
  }
  if (token_after_access.code == VAR || token_after_access.code == VAL) {
    access = M (PUB) ? PUBLIC_ACCESS : M (PRIV) ? PRIVATE_ACCESS : DEFAULT_ACCESS;
    if ((val_flag = M (VAL)) || M (VAR)) {
    } else {
      assert (FALSE);
    }
    for (;;) {
      IR_node_t first, last;
      struct val_access va = {val_flag, access};

      if ((r = TRY_FA (pattern_var, &va)) == err_node) {
        PTN (IDENT);
        r = process_var_decl (access, r, val_flag, NULL, NULL, IR_pos (r), IR_NM_var_assign);
      }
      last = r;
      if (res != NULL) {
        first = IR_next_stmt (last);
        IR_set_next_stmt (last, IR_next_stmt (res));
        IR_set_next_stmt (res, first);
      }
      res = last;
      if (!M (',')) break;
    }
    if (!C (')')) PT (';');
  } else if (M (FRIEND)) {
    for (list = NULL;;) {
      PTN (IDENT);
      res = create_node (IR_NM_friend_ident);
      IR_set_ident_in_clause (res, r);
      if (list == NULL) {
        IR_set_next_friend_ident (res, res);
      } else {
        IR_set_next_friend_ident (res, IR_next_friend_ident (list));
        IR_set_next_friend_ident (list, res);
      }
      list = res;
      if (!M (',')) break;
    }
    IR_set_friend_list (current_scope, merge_friend_lists (IR_friend_list (current_scope), list));
    res = NULL;
    if (!C (')')) PT (';');
  } else if (token_after_access.code == EXTERN) {
    access = M (PUB) ? PUBLIC_ACCESS : M (PRIV) ? PRIVATE_ACCESS : DEFAULT_ACCESS;
    PT (EXTERN);
    for (list = NULL;;) {
      PTN (IDENT);
      if (M ('(')) {
        res = create_node_with_pos (IR_NM_external_fun, IR_pos (r));
        PT (')');
      } else {
        res = create_node_with_pos (IR_NM_external_var, IR_pos (r));
      }
      IR_set_scope (res, current_scope);
      IR_set_ident (res, r);
      IR_set_next_stmt (res, res);
      IR_set_access (res, access);
      if (list != NULL) {
        IR_node_t first;

        first = IR_next_stmt (res);
        IR_set_next_stmt (res, IR_next_stmt (list));
        IR_set_next_stmt (list, first);
      }
      list = res;
      if (!M (',')) break;
    }
  } else if (token_after_access.code == OBJ) {
    access = M (PUB) ? PUBLIC_ACCESS : M (PRIV) ? PRIVATE_ACCESS : DEFAULT_ACCESS;
    PT (OBJ);
    PTN (IDENT);
    ident = r;
    res = process_obj_header (ident);
    P (block);
    source_position = block_end_pos;
    res = process_obj_block (ident, res, r, access);
  } else if (M (EXPOSE)) {
    int all_p = FALSE;

    for (res = NULL;;) {
      P (qual_ident);
      list = r;
      if (MP (FOLD_MULT, pos)) {
        expose = create_node_with_pos (IR_NM_expose, pos);
        IR_set_expose_alias (expose, NULL);
        IR_set_expose_internals_flag (expose, TRUE);
      } else {
        if (!M ('(')) {
          r = NULL;
        } else {
          PTN (IDENT);
          PT (')');
        }
        expose = create_node_with_pos (IR_NM_expose, IR_pos (list));
        IR_set_expose_alias (expose, r);
        IR_set_expose_internals_flag (expose, FALSE);
      }
      IR_set_expose_designator (expose, list);
      if (res == NULL) {
        res = expose;
        IR_set_next_stmt (res, res);
      } else {
        IR_set_next_stmt (expose, IR_next_stmt (res));
        IR_set_next_stmt (res, expose);
        res = expose;
      }
      if (!M (',')) break;
    }
    if (!C (')')) PT (';');
  } else if (M (USE)) {
    IR_node_t items;
    int former_p;

    P (qual_ident);
    qident = r;
    for (list = NULL;;) {
      if (!((former_p = M (FORMER)) || M (LATER))) break;
      for (items = NULL;;) { /* UseItem = IDENT [ "(" IDENT ")"] */
        PTN (IDENT);
        res = create_node_with_pos (former_p ? IR_NM_former_item : IR_NM_later_item, IR_pos (r));
        IR_set_use_item_ident (res, r);
        IR_set_next_use_item (res, res);
        if (!M ('(')) {
          ident = NULL;
        } else {
          PTN (IDENT);
          ident = r;
          PT (')');
        }
        IR_set_alias (res, ident);
        items = merge_use_item_lists (items, res);
        if (!M (',')) break;
      }
      list = merge_use_item_lists (list, items);
    }
    res = create_node_with_pos (IR_NM_use, IR_pos (qident));
    IR_set_next_stmt (res, res);
    IR_set_use_qual_ident (res, qident);
    IR_set_use_items (res, uncycle_use_item_list (list));
    if (!C (')')) PT (';');
  } else if (M (INCLUDE)) {
    int flag = M ('+');
    PTN (STRING);
    PT (';');
    d_assert (curr_token.code < 0);
    if (add_include_file (r) || flag) {
      struct token l = {INCLUSION, IR_pos (r), NULL};
      curr_token = l;
    }
    PAE (inclusion, r, fail);
    res = r;
  fail:;
  } else if (M (REQUIRE)) {
    PTN (NUMBER);
    res = create_node_with_pos (IR_NM_require, IR_pos (r));
    IR_set_next_stmt (res, res);
    IR_set_required_version (res, r);
    if (!C (')')) PT (';');
  } else {
    final_p = FALSE;
    access = DEFAULT_ACCESS;
    if (M (PUB)) {
      access = PUBLIC_ACCESS;
      if (M (FINAL)) final_p = TRUE;
    } else if (M (PRIV)) {
      access = PRIVATE_ACCESS;
      if (M (FINAL)) final_p = TRUE;
    } else if (M (FINAL)) {
      final_p = TRUE;
      if (M (PUB)) {
        access = PUBLIC_ACCESS;
      } else if (M (PRIV)) {
        access = PRIVATE_ACCESS;
      }
    }
    P (fun_fiber_class);
    res = process_fun_start (r, final_p, access);
    PTN (IDENT);
    IR_set_pos (res, IR_pos (r));
    if (M (';')) {
      process_header (FALSE, res, r);
    } else {
      IR_node_t hint_ident = NULL;

      process_header (TRUE, res, r);
      PA (formal_parameters, &dots_p);
      res = process_formal_parameters (res, r, dots_p);
      if (M ('!')) {
        PTN (IDENT);
        hint_ident = r;
      }
      P (block);
      res = process_header_block (res, r, hint_ident);
    }
  }
  return res;
}

D (fun_fiber_class) { /* FuncFiberClass = fun | fiber | class */
  IR_node_t r;
  position_t pos;

  if (MP (FIBER, pos)) {
    r = create_node (IR_NM_fun);
    IR_set_fiber_flag (r, TRUE);
  } else if (MP (CLASS, pos)) {
    r = create_node (IR_NM_class);
  } else {
    PTP (FUN, pos);
    r = create_node (IR_NM_fun);
    IR_set_fiber_flag (r, FALSE);
  }
  IR_set_pos (r, pos);
  return r;
}

D (par);

DA (formal_parameters) {
  /* FormalParameters = | "(" [ ParList ] ")" | "(" ParList "," "..." ")" | "(" "..." ")" */
  /* ParList = Par { "," Par} */
  int *dots_p = arg;
  IR_node_t r, res, list, first;
  position_t pos;

  *dots_p = FALSE;
  if (!M ('(')) {
    return NULL;
  } else {
    if (MP (DOTS, pos)) {
      *dots_p = TRUE;
      res = create_node_with_pos (IR_NM_var, pos);
      IR_set_scope (res, current_scope);
      IR_set_ident (res, get_ident_node (ARGS_NAME, pos));
      IR_set_next_stmt (res, res);
    } else if (C (')')) {
      res = NULL;
    } else {
      for (list = NULL;;) {
        P (par);
        if (list != NULL) {
          first = IR_next_stmt (r);
          IR_set_next_stmt (r, IR_next_stmt (list));
          IR_set_next_stmt (list, first);
        }
        list = r;
        if (!M (',')) {
          break;
        }
        if (MP (DOTS, pos)) {
          *dots_p = TRUE;
          break;
        }
      }
      res = list;
      if (*dots_p) {
        res = create_node_with_pos (IR_NM_var, pos);
        if (list != NULL) {
          IR_set_next_stmt (res, IR_next_stmt (list));
          IR_set_next_stmt (list, res);
        } else
          IR_set_next_stmt (res, res);
        IR_set_scope (res, current_scope);
        IR_set_ident (res, get_ident_node (ARGS_NAME, pos));
      }
    }
    PT (')');
  }
  return res;
}

D (par) { /* Par = [pub | priv] [val | var] IDENT [ "=" Expr] */
  access_val_t access = DEFAULT_ACCESS;
  IR_node_t r, ident;
  position_t pos;
  int val_p;

  if (M (PUB)) {
    access = PUBLIC_ACCESS;
  } else if (M (PRIV)) {
    access = PRIVATE_ACCESS;
  }
  if ((val_p = M (VAL)) || M (VAR)) {
  }
  PTN (IDENT);
  ident = r;
  r = NULL;
  if (MP ('=', pos)) {
    P (expr);
  }
  return process_var_decl (access, ident, val_p, NULL, r, pos, IR_NM_par_assign);
}

DA (stmt) {
  /* Stmt = ExecutiveStmt | Declaration */
  enum stmt_list_end end = (ptrdiff_t) arg;
  IR_node_t r;

  access_token ();
  switch (curr_token.code) {
  case PUB:
  case PRIV:
  case FINAL:
  case VAR:
  case VAL:
  case FRIEND:
  case EXTERN:
  case OBJ:
  case USE:
  case INCLUDE:
  case REQUIRE:
  case EXPOSE: P (declaration); break;
  case FUN:
  case CLASS:
  case FIBER:
    if ((r = TRY_F (declaration)) != err_node) break;
  default: {
    PA (executive_stmt, arg);
    if (r != NULL) IR_set_next_stmt (r, r);
    break;
  }
  }
  return r;
}

DA (stmt_list) { /* StmtList = { Stmt } */
  enum stmt_list_end end = (ptrdiff_t) arg;
  IR_node_t r, res = NULL;

  while (!C (END_OF_FILE) && !C (END_OF_INCLUDE_FILE) && !(end != DEFAULT_END && C ('}'))
         && !(end == CASE_END && C (CASE))) {
    PAE (stmt, DEFAULT_END, err);
    res = merge_stmt_lists (res, merge_additional_stmts (r));
    repl_process_flag = repl_can_process_p ();
    continue;
  err:
    error_recovery (0, "<statement>");
  }
  return res;
}

D (block) { /* Block = "{"  StmtList "}" */
  IR_node_t r, res, save;
  position_t pos;

  PTP ('{', pos);
  start_block ();
  save = additional_stmts;
  additional_stmts = NULL;
  PA (stmt_list, (void *) PAREN_END);
  IR_set_pos (current_scope, pos);
  res = r;
  finish_block ();
  additional_stmts = save;
  block_end_pos = source_position;
  PT ('}');
  return res;
}

D (program) { /* Program = StmtList  */
  IR_node_t r, scope = current_scope;

  repl_process_flag = FALSE;
  IR_set_friend_list (current_scope, NULL);
  IR_set_next_stmt (current_scope, NULL);
  PA (stmt_list, (void *) DEFAULT_END);
  /* In REPL mode we can achieve this code in error
     recovery mode, don't change first_program_stmt it
     should be always the same for REPL.  */
  if (!repl_flag || first_program_stmt == NULL) first_program_stmt = scope;
  IR_set_block_stmts (first_program_stmt, uncycle_stmt_list (r));
  IR_set_friend_list (scope, uncycle_friend_list (IR_friend_list (scope)));
  PT (END_OF_FILE);
  return NULL;
}

/* Merging two cyclic lists into one cyclic list. */
static IR_node_t merge_friend_lists (IR_node_t list1, IR_node_t list2) {
  IR_node_t result;

  if (list2 != NULL) {
    if (list1 != NULL) {
      IR_node_t first;

      first = IR_next_friend_ident (list2);
      IR_set_next_friend_ident (list2, IR_next_friend_ident (list1));
      IR_set_next_friend_ident (list1, first);
    }
    result = list2;
  } else
    result = list1;
  return result;
}

/* Make normal list from cycle ident list with the pointer to the last
   stmt. */
static IR_node_t uncycle_friend_list (IR_node_t list) {
  IR_node_t first;

  if (list == NULL) return list;
  first = IR_next_friend_ident (list);
  IR_set_next_friend_ident (list, NULL);
  return first;
}

/* Merging two cyclic lists into one cyclic list. */
static IR_node_t merge_use_item_lists (IR_node_t list1, IR_node_t list2) {
  IR_node_t result;

  if (list2 != NULL) {
    if (list1 != NULL) {
      IR_node_t first;

      first = IR_next_use_item (list2);
      IR_set_next_use_item (list2, IR_next_use_item (list1));
      IR_set_next_use_item (list1, first);
    }
    result = list2;
  } else
    result = list1;
  return result;
}

/* Make normal list from cycle item list with the pointer to the
   last item list. */
static IR_node_t uncycle_use_item_list (IR_node_t list) {
  IR_node_t first;

  if (list == NULL) return list;
  first = IR_next_use_item (list);
  IR_set_next_use_item (list, NULL);
  return first;
}

/* Merging two cyclic lists into one cyclic list. */
static IR_node_t merge_exception_lists (IR_node_t list1, IR_node_t list2) {
  IR_node_t result;

  if (list2 != NULL) {
    if (list1 != NULL) {
      IR_node_t first;

      first = IR_next_exception (list2);
      IR_set_next_exception (list2, IR_next_exception (list1));
      IR_set_next_exception (list1, first);
    }
    result = list2;
  } else
    result = list1;
  return result;
}

/* Make normal list from cycle exception list with the pointer to the last
   exception. */
static IR_node_t uncycle_exception_list (IR_node_t list) {
  IR_node_t first;

  if (list == NULL) return list;
  first = IR_next_exception (list);
  IR_set_next_exception (list, NULL);
  return first;
}

/* Create variable IDENT with VAL_FLAG and OBJ_BLOCK.  If EXPR is not
   null, create also assignment with ASSIGN of EXPR with EXPR_POS to
   the variable.  Create cyclic list of the node(s) and return the
   last one.  */
static IR_node_t process_var_decl (access_val_t access, IR_node_t ident, int val_flag,
                                   IR_node_t obj_block, IR_node_t expr, position_t expr_pos,
                                   IR_node_mode_t assign) {
  position_t ident_pos = IR_pos (ident);
  IR_node_t res = create_node_with_pos (IR_NM_var, ident_pos);

  IR_set_access (res, access);
  IR_set_scope (res, current_scope);
  IR_set_ident (res, ident);
  IR_set_const_flag (res, val_flag);
  IR_set_obj_block (res, obj_block);
  if (obj_block != NULL && IR_fun_class (obj_block) != NULL)
    IR_set_obj_var (IR_fun_class (obj_block), res);
  d_assert (obj_block == NULL || val_flag);
  if (expr == NULL)
    IR_set_next_stmt (res, res);
  else {
    IR_node_t init = create_node_with_pos (assign, expr_pos);

    IR_set_assignment_var (init, ident);
    IR_set_assignment_expr (init, expr);
    IR_set_next_stmt (res, init);
    IR_set_next_stmt (init, res);
    res = merge_additional_stmts (init);
  }
  return res;
}

/* Create pattern var node with PATTERN, ACCESS, VAR_FLAG and pattern
   assignment node PATTERN, EXPR and return them.  */
static IR_node_t process_var_pattern (access_val_t access, IR_node_t pattern, int val_flag,
                                      IR_node_t expr, position_t expr_pos, IR_node_mode_t assign) {
  IR_node_t res, init;

  d_assert (expr != NULL);
  res = create_node_with_pos (IR_NM_pattern_var, expr_pos);
  IR_set_pattern_const_flag (res, val_flag);
  IR_set_pattern_var_access (res, access);
  IR_set_pattern (res, pattern);
  init = create_node_with_pos (assign, expr_pos);
  IR_set_assignment_var (init, pattern);
  IR_set_assignment_expr (init, expr);
  IR_set_next_stmt (res, init);
  IR_set_next_stmt (init, res);
  res = merge_additional_stmts (init);
  return res;
}

/* Process function/fiber/class header.  */
static void process_header (int create_block_p, IR_node_t decl, IR_node_t ident) {
  IR_node_t block;

  IR_set_scope (decl, current_scope);
  IR_set_ident (decl, ident);
  if (!create_block_p) {
    IR_set_next_stmt (decl, decl);
    IR_set_forward_decl_flag (decl, TRUE);
    return;
  }
  block = create_empty_block (current_scope);
  IR_set_forward_decl_flag (decl, FALSE);
  IR_set_next_stmt (block, decl);
  IR_set_next_stmt (decl, block);
  IR_set_fun_class (block, decl);
  /* This assignment is here for that formal parameters are to be in
     corresponding block. */
  current_scope = block;
}

/* Process formal parameters PARS of fun/fiber/class DECL.  Return the
   parameters.  */
static IR_node_t process_formal_parameters (IR_node_t decl, IR_node_t pars, int dots_p) {
  IR_node_t next, current_decl = pars;

  if (current_decl != NULL) {
    do {
      current_decl = IR_next_stmt (current_decl);
      if (IR_IS_OF_TYPE (current_decl, IR_NM_var)) IR_set_par_flag (current_decl, TRUE);
    } while (current_decl != pars);
  }
  IR_set_args_flag (decl, dots_p);
  return pars;
}

/* Process BLOCK of fun/fiber/class/obj HEADER decl.  Return the
   block.  */
static IR_node_t process_header_block (IR_node_t header, IR_node_t block, IR_node_t hint_ident) {
  IR_node_t res = current_scope; /*i.e. block.*/

  IR_set_hint_ident (res, hint_ident);
  IR_set_block_stmts (res, uncycle_stmt_list (merge_stmt_lists (header, block)));
  IR_set_friend_list (res, uncycle_friend_list (IR_friend_list (res)));
  current_scope = IR_block_scope (res);
  return res;
}

static IR_node_t merge_additional_stmts (IR_node_t list) {
  IR_node_t res = merge_stmt_lists (additional_stmts, list);

  additional_stmts = NULL;
  return res;
}

static IR_node_t process_obj_header (IR_node_t ident) {
  IR_node_t unique_ident, class_ident, class_def;

  class_def = create_node_with_pos (IR_NM_class, IR_pos (ident));
  IR_set_final_flag (class_def, TRUE);
  VLO_NULLIFY (temp_scanner_vlo);
  VLO_ADD_STRING (temp_scanner_vlo, "$");
  VLO_ADD_STRING (temp_scanner_vlo, IR_ident_string (IR_unique_ident (ident)));
  unique_ident = create_unique_ident_node (VLO_BEGIN (temp_scanner_vlo));
  class_ident = create_node_with_pos (IR_NM_ident, IR_pos (ident));
  IR_set_unique_ident (class_ident, unique_ident);
  process_header (TRUE, class_def, class_ident);
  return class_ident;
}

static IR_node_t process_obj_block (IR_node_t ident, IR_node_t class_ident, IR_node_t block_stmts,
                                    access_val_t access) {
  IR_node_t val, expr;
  IR_node_t block = current_scope;

  IR_set_block_stmts (block, uncycle_stmt_list (block_stmts));
  IR_set_friend_list (block, uncycle_friend_list (IR_friend_list (block)));
  current_scope = IR_block_scope (block);
  expr = create_node_with_pos (IR_NM_class_fun_fiber_call, source_position);
  IR_set_fun_expr (expr, class_ident);
  IR_set_actuals (expr, NULL);
  val = process_var_decl (access, ident, TRUE, block, expr, IR_pos (ident), IR_NM_var_assign);
  return merge_stmt_lists (block, val);
}

static IR_node_t process_fun_start (IR_node_t fun, int final_flag, access_val_t access) {
  IR_set_final_flag (fun, final_flag);
  IR_set_access (fun, access);
  return fun;
}

static IR_node_t create_except_class (IR_node_t before_list, IR_node_t expr, position_t pos) {
  IR_node_t res;
  pos.column_number--;

  res = create_node_with_pos (IR_NM_exception, pos);
  IR_set_exception_class_expr (res, expr);
  IR_set_catch_block (res, NULL);
  if (before_list == NULL)
    IR_set_next_exception (res, res);
  else {
    IR_set_next_exception (res, IR_next_exception (before_list));
    IR_set_next_exception (before_list, res);
  }
  return res;
}

static IR_node_t create_catch_block (position_t pos) {
  IR_node_t res = create_empty_block (current_scope);

  current_scope = res;
  /* Add variable for catched exception. */
  res = create_node_with_pos (IR_NM_var, pos);
  IR_set_next_stmt (res, res);
  IR_set_scope (res, current_scope);
  IR_set_ident (res, get_ident_node (CATCH_EXCEPTION_NAME, pos));
  return res;
}

static void finish_catch_block (IR_node_t catch_block, IR_node_t block, IR_node_t excepts,
                                IR_node_t friend_list) {
  IR_node_t first_exception = IR_next_exception (excepts);

  IR_set_block_stmts (current_scope, uncycle_stmt_list (merge_stmt_lists (catch_block, block)));
  IR_set_friend_list (current_scope, friend_list);
  IR_set_catch_block (first_exception, current_scope);
  current_scope = IR_block_scope (current_scope);
}

/* We have try (<stmt>, <except>, ...).  Create and return
   fun {try {<stmt>;return 1;} catch (<except>) {return 0;}} ().

   TRY_BLOCK is already created block for the try-statement. LPAR_POS
   and RPAR_POS are positions of correspondingly left and right
   parenthesis in try-expr.

   ADDITIONAL_STMTS_BEFORE_TRY are stmts should be before the
   try-expr, while ADDITIONAL_STMTS are stmts should be before the
   stmt in the try-expr.  */
static IR_node_t create_try_expr (IR_node_t try_block, IR_node_t stmt, IR_node_t excepts,
                                  position_t lpar_pos, position_t rpar_pos,
                                  IR_node_t additional_stmts_before_try) {
  IR_node_t fun, stmt_list, catch_block, fun_expr, call;

  fun = IR_fun_class (IR_block_scope (try_block));

  /* Create <executive_stmt>; return 1; */
  if (stmt != NULL) IR_set_next_stmt (stmt, stmt);
  stmt = merge_additional_stmts (stmt);
  stmt_list = create_node_with_pos (IR_NM_return_with_result, rpar_pos);
  IR_set_returned_expr (stmt_list, get_int_node (1, rpar_pos));
  IR_set_next_stmt (stmt_list, stmt_list);
  stmt_list = merge_stmt_lists (stmt, stmt_list);

  finish_block (); /* finish try-block */

  /* create try { <executive_stmt>; return 1; } ... */
  IR_set_block_stmts (try_block, uncycle_stmt_list (stmt_list));
  IR_set_friend_list (try_block, NULL);
  current_scope = IR_block_scope (try_block);

  /* create catch (<except>) {...} */
  catch_block = create_catch_block (rpar_pos);
  IR_set_pos (current_scope, rpar_pos);
  start_block (); /* start catch-block */

  /* create return 0; */
  stmt_list = create_node_with_pos (IR_NM_return_with_result, rpar_pos);
  IR_set_returned_expr (stmt_list, get_int_node (0, rpar_pos));
  IR_set_next_stmt (stmt_list, stmt_list);

  /* finish catch_block: */
  finish_block ();
  finish_catch_block (catch_block, stmt_list, excepts, NULL);
  IR_set_exceptions (try_block, uncycle_exception_list (excepts));

  IR_set_pos (current_scope, lpar_pos);
  finish_block (); /* finish function block */

  /* Move (header, block) before the current statement: */
  IR_set_next_stmt (try_block, try_block);
  additional_stmts
    = merge_stmt_lists (additional_stmts_before_try, process_header_block (NULL, try_block, NULL));
  /* create function call  */
  fun_expr = IR_ident (fun);
  call = create_node_with_pos (IR_NM_class_fun_fiber_call, rpar_pos);
  IR_set_fun_expr (call, fun_expr);
  IR_set_actuals (call, NULL);
  return call;
}

int parse (void) {
  curr_token.code = -1;
  return program (FALSE) == err_node;
}

/*
Local Variables:
mode:c
End:
*/
