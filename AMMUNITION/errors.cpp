/*
   FILE NAME:   errors.cpp

   TITLE:       Implementation of class for output of compiler messages

   DESCRIPTION: This class serves to output one-pass or multi-pass
       compiler messages of various modes (errors, warnings, fatal,
       system errors and appended messages) in Unix style or for
       traditional listing.  The class also permit adequate error
       reporting for included files.  The class instance works in two
       regimes: of immediately output of messages or of storing all
       fixed messages.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C++ compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of class instance.
         The default value of macro `MAX_ERROR_MESSAGE_LENGTH' can be
       redefined with corresponding C++ compiler option `-D' during
       compilation of the class.
         Defining macro `HAVE_STRERROR' (e.g. by option -D
       in C compiler command line) during the file compilation permits
       to use standard C include-file in which function `strerror'
       (file `string.h') is present.  In opposite case include-file
       `errno.h' is used and function `strerror' is implemented in
       this file with the aid of external varaibles `sys_nerr' and
       `sys_errlist'.  */

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_STRERROR
#define HAVE_STRERROR
#endif
#ifndef HAVE_ERRNO_H
#define HAVE_ERRNO_H
#endif
#ifndef HAVE_VSPRINTF
#define HAVE_VSPRINTF
#endif
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#ifndef HAVE_VSPRINTF
#error COCOM can not work with vsprintf
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "vlobject.h"
#include "objstack.h"
#include "position.h"
#include "errors.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

/* The value of macro is suggested to be maximum length of generated
   error message.  The length should be less than this value. */

#ifndef MAX_ERROR_MESSAGE_LENGTH
#define MAX_ERROR_MESSAGE_LENGTH 150
#endif

#ifndef HAVE_STRERROR

/* Here it is suggested that there are no standard C/C++ include-file
   which contains definition of function `strerror'. */

/* These two external definitions are needed because they are absent
   in SunOS 4.x system include-file `errno.h'. */

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

/* This virtual function will be called after fixing a fatal error by
   functions `error' or `system_error'.  The fatal error function is
   suggested to do not return the control back.  The function only
   calls `exit (1)'. */

void
errors::fatal_error_function (void)
{
  exit (1);
}

/* This virtual function is used to output error message.  The
   function has three parameters -- flag of appended message, message
   position and message itself.  The function is oriented to output in
   Unix style according to GNU standard.  The function output message
   in following formats
  MESSAGE                              (for position with NULL file name)
  FILE_NAME:1: MESSAGE                 (for position with zero line number)
  FILE_NAME:LINE_NUMBER: MESSAGE       (for position with zero column number)
  FILE_NAME:LINE_NUMBER:COLUMN_NUMBER-1: MESSAGE   (for all other cases)
     After that the function outputs newline.  The function also outputs
   additional messages `in file processed from...'  if given message
   is not appended message and corresponds to file different from one
   of previous output error or warning.  This message reflects path of
   the message position (see package `positions'), i.e. reflects
   positions of corresponding include-clauses.  Decremented column
   number is output in order to be in concordance with editor Emacs in
   which positions start with 0. */

void
errors::output_error_function (int appended_message_flag, position_t
                               position, const char *message)
{
  position_t *position_ptr;

  if (position.file_name () == NULL)
    fprintf (stderr, message);
  else
    {
      assert (position.path () != NULL);
      if (!appended_message_flag && position.path ()->path () != NULL
          && (
              previous_output_position.path () == NULL
              || positions::compare (*position.path (),
                                     *previous_output_position.path ()) != 0
              || strcmp (position.file_name (),
                         previous_output_position.file_name ()) != 0))
        {
          /* This is an new included file -- output path. */
          fprintf (stderr, "In file processed");
          for (position_ptr = position.path ();;)
            {
              assert (position_ptr->file_name () != NULL);
              if (position_ptr->line_number == 0)
                fprintf (stderr, " from %s:1:", position_ptr->file_name ());
              else if (position_ptr->column_number == 0)
                fprintf (stderr, " from %s:%u:", position_ptr->file_name (),
                         position_ptr->line_number);
              else
                fprintf (stderr, " from %s:%u:%u:", position_ptr->file_name (),
                         position_ptr->line_number,
                         position_ptr->column_number - 1);
              position_ptr = position_ptr->path ();
              if (position_ptr->path () != NULL)
                fputc (',', stderr);
              else
                {
                  fputc ('\n', stderr);
                  break;
                }
            }
        }
      if (position.line_number == 0)
        fprintf (stderr, "%s:1: %s", position.file_name (), message);
      else if (position.column_number == 0)
        fprintf (stderr, "%s:%u: %s", position.file_name (),
                 position.line_number, message);
      else
        fprintf (stderr, "%s:%u:%u: %s", position.file_name (),
                 position.line_number, position.column_number - 1, message);
      if (!appended_message_flag)
        previous_output_position = position;
    }
  fputc ('\n', stderr);
}

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
compare_messages (const void *descriptor_1, const void *descriptor_2)
{
  int result;

  result
    = positions::compare
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

/* This constructior initiates the class object in a regime of two
   possible regimes: of immediately output of messages or of storing
   all fixed messages. */

errors::errors (int immediate_output_flag)
{
  _number_of_errors = 0;
  _number_of_warnings = 0;
  maximum_number_of_errors = 50;
  immediate_output_regime = immediate_output_flag;
  previous_output_position = positions::no_position;
  descriptors_of_messages = new vlo;
  messages = new os;
}

/* The destructor. */

errors::~errors (void)
{
  delete descriptors_of_messages;
  delete messages;
}

void
errors::output (void)
{
  message_descriptor_t *current_descriptor_ptr;

  qsort (descriptors_of_messages->begin (),
         descriptors_of_messages->length () / sizeof (message_descriptor_t),
         sizeof (message_descriptor_t), compare_messages);
  for (current_descriptor_ptr
       = (message_descriptor_t *) descriptors_of_messages->begin ();
       (char *) current_descriptor_ptr
       <= (char *) descriptors_of_messages->end ();
       current_descriptor_ptr++)
    output_error_function
      (current_descriptor_ptr->appended_message_flag,
       (current_descriptor_ptr->appended_message_flag
        ? current_descriptor_ptr->true_appended_message_position
        : current_descriptor_ptr->message_position),
       current_descriptor_ptr->message_start);
  descriptors_of_messages->nullify ();
  delete messages;
  messages = new os;
}

/* This function fixes message.  Depending on the class instance regime the
   message is output or stored.  If the message is fatal than
   functions `output_errors' and `fatal_error_function' are called.
   If the error is fixed with number equals to
   `maximum_number_of_errors' than special fatal error `fatal error --
   too many errors' with position of given error is fixed instead of
   the error.  The previously fixed error or warning must exist for
   appended message in store regime. */

void
errors::fix_message (int error_flag, int fatal_error_flag,
                     int appended_message_flag, position_t position,
                     const char *message)
{
  message_descriptor_t descriptor;

  if (error_flag)
    {
      assert (!appended_message_flag);
      _number_of_errors++;
      if (_number_of_errors == maximum_number_of_errors)
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
        _number_of_warnings++;
    }
  if (immediate_output_regime)
    output_error_function (appended_message_flag, position, message);
  else
    {
      descriptor.message_number = (descriptors_of_messages->length ()
                                   / sizeof (message_descriptor_t));
      if (appended_message_flag)
        {
          assert (descriptor.message_number != 0);
          descriptor.message_position
            = ((message_descriptor_t *) descriptors_of_messages->begin ())
              [descriptor.message_number - 1].message_position;
          descriptor.true_appended_message_position = position;
        }
      else
        descriptor.message_position = position;
      descriptor.appended_message_flag = appended_message_flag;
      messages->top_add_string (message);
      descriptor.message_start = (const char *) messages->top_begin ();
      messages->top_finish ();
      descriptors_of_messages->add_memory (&descriptor, sizeof (descriptor));
    }
  if (fatal_error_flag)
    {
      if (!immediate_output_regime)
        output ();
      fatal_error_function ();
      assert (0 /* FALSE */);
    }
}

/* This function forms message for given error and calls function
   `fix_message'.  The diagnostic message are formed analogous to
   output of function `printf'.  For example,

      ...error (TRUE, current_position, "fatal error - no memory"); */

void
errors::error (int fatal_error_flag, position_t position,
               const char *format, ...)
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
errors::warning (position_t position, const char *format, ...)
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
errors::append_message (position_t position, const char *format, ...)
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
errors::system_error (int fatal_error_flag, position_t position,
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
