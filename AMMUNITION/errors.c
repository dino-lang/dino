/* FILE NAME:   errors.c

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package for output of compiler messages; you can
   redistribute it and/or modify it under the terms of the GNU Library
   General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later
   version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Package for output of compiler messages

   DESCRIPTION: This package serves to output one-pass or
       multi-pass compiler messages of various modes (errors,
       warnings, fatal, system errors and appended messages) in Unix
       style or for traditional listing.  The package also permit
       adequate error reporting for included files.  The package works
       in two regimes: of immediately output of messages or of storing
       all fixed messages.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of the package.
         The default value of macro `MAX_ERROR_MESSAGE_LENGTH' can be
       redefined with corresponding C compiler option `-D' during
       compilation of the package.
         Defining macro `HAVE_STRERROR' (e.g. by option -D
       in C compiler command line) during the file compilation permits
       to use standard C include-file in which function `strerror'
       (file `string.h') is present.  In opposite case include-file
       `errno.h' is used and function `strerror' is implemented in
       this file with the aid of external varaibles `sys_nerr' and
       `sys_errlist'.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_STRERROR
#define HAVE_STRERROR
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

#include "errors.h"

#include <assert.h>

/* The value of macro is suggested to be maximum length of generated
   error message.  The length should be less than this value. */

#ifndef MAX_ERROR_MESSAGE_LENGTH
#define MAX_ERROR_MESSAGE_LENGTH 150
#endif

#ifndef HAVE_STRERROR

/* Here it is suggested that there are no standard C include-file
   which contains definition of function `strerror'. */

/* These two external definitions are needed because they are absent
   in SunOS system include-file `errno.h'. */

extern int sys_nerr;
extern char *sys_errlist[];


/* The following function is an analog of standard C function
   `strerror'.  The function returns message string of system error
   with given number. */

static char *
strerror (int errnum)
{
  static char system_message[MAX_ERROR_MESSAGE_LENGTH + 1];

  if (!errnum)
    return "";

  if (errnum > 0 && errnum < sys_nerr)
    return sys_errlist[errnum];

  sprintf (system_message, "Unknown error %d", errnum);
  assert (strlen (system_message) <= MAX_ERROR_MESSAGE_LENGTH);
  return system_message;
}

#endif

/* Value of this variable is number of errors fixed after the most
   recent package initiation. */

unsigned int number_of_errors;

/* Value of this variable is maximum number of errors which will be
   fixed.  If a error is fixed with number equals to
   `maximum_number_of_errors' than special fatal error `fatal error --
   too many errors' with position of given error is fixed instead of
   the error.  And all following errors are ignored.  Zero value of
   the variable means that the special fatal error will never fixed. */

unsigned int maximum_number_of_errors;

/* Originally value of variable `maximum_number_of_errors' is equal to
   this constant. */

const unsigned int default_maximum_number_of_errors = 50;

/* Value of this variable is number of warnings fixed after the most
   recent package initiation. */

unsigned int number_of_warnings;

/* This variable contains pointer to function without parameters which
   will be called after fixing a fatal error by functions `error' or 
   `system_error'.  The fatal error function is suggested to do not
   return the control back. */

void (*fatal_error_function) (void);

/* This is default fatal error function.  Originally (after the
   package initiation) value of variable `fatal_error_function' is
   equal to this function.  The function only calls `exit (1)'. */

void
default_fatal_error_function (void)
{
  exit (1);
}

/* This variable contains pointer to function which is used to output
   error message.  The function has three parameters -- flag of
   appended message, message position and message itself. */

void (*output_error_function) (int appended_message_flag, position_t position,
                               const char *message);

/* This variable contains position of the most recent output
   non-appended message (after last the package initiation) which was
   bound to concrete file, i.e. whose position has non-null file name. */

static position_t previous_output_position;

/* This is default output error function.  Originally (after the
   package initiation) value of variable `output_error_function' is
   equal to this function.  The function is oriented to output in Unix
   style according to GNU standard.  The function output message in
   following formats
  MESSAGE                              (for position with NULL file name)
  FILE_NAME:1: MESSAGE                 (for position with zero line number)
  FILE_NAME:LINE_NUMBER: MESSAGE       (for position with zero column number)
  FILE_NAME:LINE_NUMBER:COLUMN_NUMBER: MESSAGE   (for all other cases)
     After that the function outputs newline.  The function also outputs
   additional messages `in file processed from...'  if given message
   is not appended message and corresponds to file different from one
   of previous output error or warning.  This message reflects path of
   the message position (see package `positions'), i.e. reflects
   positions of corresponding include-clauses.  Decremented column
   number is output in order to be in concordance with editor Emacs in
   which positions start with 0. */

void
default_output_error_function (int appended_message_flag, position_t
                               position, const char *message)
{
  const position_t *position_ptr;

  if (position.file_name == NULL)
    fputs (message, stderr);
  else
    {
      assert (position.path != NULL);
      if (!appended_message_flag && position.path->path != NULL
          && (
              previous_output_position.path == NULL
              || compare_positions (*position.path,
                                    *previous_output_position.path) != 0
              || strcmp (position.file_name,
                         previous_output_position.file_name) != 0))
        {
          /* This is an new included file -- output path. */
          fprintf (stderr, "In file processed");
          for (position_ptr = position.path;;)
            {
              assert (position_ptr->file_name != NULL);
              if (position_ptr->line_number == 0)
                fprintf (stderr, " from %s:1:", position_ptr->file_name);
              else if (position_ptr->column_number == 0)
                fprintf (stderr, " from %s:%u:", position_ptr->file_name,
                         position_ptr->line_number);
              else
                fprintf (stderr, " from %s:%u:%u:", position_ptr->file_name,
                         position_ptr->line_number,
                         position_ptr->column_number);
              position_ptr = position_ptr->path;
              if (position_ptr->path != NULL)
                fputc (',', stderr);
              else
                {
                  fputc ('\n', stderr);
                  break;
                }
            }
        }
      if (position.line_number == 0)
        fprintf (stderr, "%s:1: %s", position.file_name, message);
      else if (position.column_number == 0)
        fprintf (stderr, "%s:%u: %s", position.file_name,
                 position.line_number, message);
      else
        fprintf (stderr, "%s:%u:%u: %s", position.file_name,
                 position.line_number, position.column_number, message);
      if (!appended_message_flag)
        previous_output_position = position;
    }
  fputc ('\n', stderr);
}

/* The package regime which was set up by the most recent package
   initiation. */

static int immediate_output_regime;

/* This type describes of major structure (descriptor) of internal
   representation  of all fixed messages which which are stored for
   subsequent output. */

typedef struct
{
  /* Order number of stored message.  It is needed for stable sorting. */
  int message_number;
  /* Position of given message. This value for appended message is defined
     from previous message. */
  position_t message_position;
  /* This member value is TRUE if the message is appended message. */
  int appended_message_flag;
  /* This member is defined only for appended messages and contains
     value given from parameter of function `append_message'. */
  position_t true_appended_message_position;
  /* Message itself. */
  const char *message_start;
} message_descriptor_t;

/* The following function compares messages.  This function is used by
   standard function `qsort'.  The function returns 1 if position of
   the first message is greater than one of the second, and -1 if
   position of the first message is less than one of the second.  If
   the positions are equal then the order of fixing their is taken
   into account. */

static int
compare_messages (const void *descriptor_1,
                  const void *descriptor_2)
{
  int result;

  result
    = compare_positions
      (((message_descriptor_t *) descriptor_1)->message_position,
       ((message_descriptor_t *) descriptor_2)->message_position);
  if (result == 0)
    {
      if (((message_descriptor_t *) descriptor_1)->message_number
          < ((message_descriptor_t *) descriptor_2)->message_number)
        result = (-1);
      else
        {
          assert (((message_descriptor_t *) descriptor_1)->message_number
                  != ((message_descriptor_t *) descriptor_2)->message_number);
          result = 1;
        }
    }
  return result;
}

/* This variable length object contains all stored descriptors of
   messages. */

static vlo_t descriptors_of_messages;

/* This object stack contains strings of all stored messages. */

static os_t messages;

/* This function initiates the package in a regime of two possible
   regimes: of immediately output of messages or of storing all fixed
   messages.  The function must be called only once before any work
   with the package. */

void
initiate_errors (int immediate_output_flag)
{
  number_of_errors = 0;
  number_of_warnings = 0;
  maximum_number_of_errors = default_maximum_number_of_errors;
  fatal_error_function = default_fatal_error_function;
  output_error_function = default_output_error_function;
  immediate_output_regime = immediate_output_flag;
  VLO_CREATE (descriptors_of_messages, 0);
  OS_CREATE (messages, 0);
  previous_output_position = no_position;
}

/* This function free all memory allocated during package work.  Only
   call of function `initiate_errors' is possible immediately after
   this function call. */

void
finish_errors (void)
{
  VLO_DELETE (descriptors_of_messages);
  OS_DELETE (messages);
}

/* This function sorts (stable sorting) all stored messages by their
   positions, outputs ones, and deletes ones.  Appended messages will
   be output after corresponding error or warning.  This function
   should be used only in regime of storing messages. */

void
output_errors (void)
{
  message_descriptor_t *current_descriptor_ptr;

  qsort (VLO_BEGIN (descriptors_of_messages),
         VLO_LENGTH (descriptors_of_messages) / sizeof (message_descriptor_t),
         sizeof (message_descriptor_t), compare_messages);
  for (current_descriptor_ptr = VLO_BEGIN (descriptors_of_messages);
       (char *) current_descriptor_ptr
       <= (char *) VLO_END (descriptors_of_messages);
       current_descriptor_ptr++)
    (*output_error_function)
      (current_descriptor_ptr->appended_message_flag,
       (current_descriptor_ptr->appended_message_flag
        ? current_descriptor_ptr->true_appended_message_position
        : current_descriptor_ptr->message_position),
       current_descriptor_ptr->message_start);
  VLO_NULLIFY (descriptors_of_messages);
  OS_DELETE (messages);
  OS_CREATE (messages, 0);
}

/* This function fixes message.  Depending on the package regime the
   message is output or stored.  If the message is fatal than
   functions `output_errors' and `fatal_error_function' are called.
   If the error is fixed with number equals to
   `maximum_number_of_errors' than special fatal error `fatal error --
   too many errors' with position of given error is fixed instead of
   the error.  The previously fixed error or warning must exist for
   appended message in store regime. */

static void
fix_message (int error_flag, int fatal_error_flag, int appended_message_flag,
             position_t position, const char *message)
{
  message_descriptor_t descriptor;

  if (error_flag)
    {
      assert (!appended_message_flag);
      number_of_errors++;
      if (number_of_errors == maximum_number_of_errors)
        {
          fix_message (1 /* TRUE */, 1 /* TRUE*/, 0 /* FALSE */, position,
                       "fatal error -- too many errors");
          return;
        }
    }
  else
    {
      assert (!fatal_error_flag);
      if (!appended_message_flag)
        number_of_warnings++;
    }
  if (immediate_output_regime)
    (*output_error_function) (appended_message_flag, position, message);
  else
    {
      descriptor.message_number = (VLO_LENGTH (descriptors_of_messages)
                                   / sizeof (message_descriptor_t));
      if (appended_message_flag)
        {
          assert (descriptor.message_number != 0);
          descriptor.message_position
            = ((message_descriptor_t *) VLO_BEGIN (descriptors_of_messages))
              [descriptor.message_number - 1].message_position;
          descriptor.true_appended_message_position = position;
        }
      else
        descriptor.message_position = position;
      descriptor.appended_message_flag = appended_message_flag;
      OS_TOP_ADD_STRING (messages, message);
      descriptor.message_start = OS_TOP_BEGIN (messages);
      OS_TOP_FINISH (messages);
      VLO_ADD_MEMORY (descriptors_of_messages, &descriptor,
                      sizeof (descriptor));
    }
  if (fatal_error_flag)
    {
      if (!immediate_output_regime)
        output_errors ();
      (*fatal_error_function) ();
      assert (0 /* FALSE */);
    }
}

/* This function forms message for given error and calls function
   `fix_message'.  The diagnostic message are formed analogous to
   output of function `printf'.  For example,

      error (TRUE, current_position, "fatal error - no memory"); */

void
error (int fatal_error_flag, position_t position, const char *format, ...)
{
  char message[MAX_ERROR_MESSAGE_LENGTH + 1];
  va_list arguments;

  va_start (arguments, format);
  vsprintf (message, format, arguments);
  va_end (arguments);
  assert (strlen (message) <= MAX_ERROR_MESSAGE_LENGTH);
  fix_message (1 /* TRUE */, fatal_error_flag, 0 /* FALSE */,
               position, message);
}

/* This function forms message for given warning and calls function
   `fix_message'.  The diagnostic message are formed analogous to
   output of function `printf'. */

void
warning (position_t position, const char *format, ...)
{
  char message[MAX_ERROR_MESSAGE_LENGTH + 1];
  va_list arguments;

  va_start (arguments, format);
  vsprintf (message, format, arguments);
  va_end (arguments);
  assert (strlen (message) <= MAX_ERROR_MESSAGE_LENGTH);
  fix_message (0 /* FALSE */, 0 /* FALSE */, 0 /* FALSE */, position, message);
}

/* This function forms message for given appended message and calls
   function `fix_message'.  The diagnostic message are formed
   analogous to output of function `printf'.  The appended message
   will be output with the most recent fixed error or warning
   independently from value of the first parameter.  For example, this
   function may be used for generation of messages of type
  
        `<file>:<line>:<position>: repeated declaration' and then
        `<file>:<line>:<position>: previous declaration'.

   The previously fixed error or warning must exist in store regime. */

void
append_message (position_t position, const char *format, ...)
{
  char message[MAX_ERROR_MESSAGE_LENGTH + 1];
  va_list arguments;

  va_start (arguments, format);
  vsprintf (message, format, arguments);
  va_end (arguments);
  assert (strlen (message) <= MAX_ERROR_MESSAGE_LENGTH);
  fix_message (0 /* FALSE */, 0 /* FALSE */, 1 /* TRUE */, position, message);
}

/* This function forms message for given system error and calls
   function `fix_message'.  The diagnostic message are formed
   analogous to output of function `printf'.  After that the current
   system message without head blanks (given by standard function
   `strerror') is placed after the message formed by the function
   parameters.  For example, the following call may be used when a
   file is not opened

      system_error (TRUE, current_position, "fatal error - %s:",
                    new_file_name); */

void
system_error (int fatal_error_flag, position_t position,
              const char *format, ...)
{
  char message[MAX_ERROR_MESSAGE_LENGTH + 1];
  va_list arguments;

  va_start (arguments, format);
  vsprintf (message, format, arguments);
  va_end (arguments);
  assert (strlen (message) <= MAX_ERROR_MESSAGE_LENGTH);
  strcat (message, strerror (errno));
  assert (strlen (message) <= MAX_ERROR_MESSAGE_LENGTH);
  fix_message (1 /* TRUE */, fatal_error_flag, 0 /* FALSE */,
               position, message);
}
