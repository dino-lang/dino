/*
   FILE NAME:   errors.h

   TITLE:       Include file of package for output of compiler messages

   DESCRIPTION: This header file contains ANSI C prototype definitions
                of the package functions and definitions of external
                variables of the package and C++ class for output of
                compiler messages.

*/

#ifndef __ERRORS__
#define __ERRORS__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#endif

#include "vlobject.h"
#include "objstack.h"
#include "position.h"


#ifndef __cplusplus


extern unsigned int number_of_errors;
extern unsigned int maximum_number_of_errors;
extern const unsigned int default_maximum_number_of_errors;

extern unsigned int number_of_warnings;

extern void (*fatal_error_function) (void);
extern void default_fatal_error_function (void);

extern void (*output_error_function) (int appended_message_flag,
                                      position_t position,
                                      const char *message);
extern void default_output_error_function (int appended_message_flag,
                                           position_t position,
                                           const char *message);

extern void initiate_errors (int immediate_output_flag);
extern void finish_errors (void);
extern void output_errors (void);
extern void error (int fatal_error_flag, position_t position,
                   const char *format, ...);
extern void warning (position_t position, const char *format, ...);
extern void append_message (position_t position, const char *format, ...);
extern void system_error (int fatal_error_flag, position_t position,
                          const char *format, ...);


#else /* #ifndef __cplusplus */


class errors
{
  /* Value of this variable is number of errors fixed after the class
     instance creation. */

  unsigned int _number_of_errors;

  /* Value of this variable is number of warnings fixed after the
     class instance creation. */

  unsigned int _number_of_warnings;
  
  /* This variable contains position of the most recent output
     non-appended message (after the class instance construction)
     which was bound to concrete file, i.e. whose position has
     non-null file name. */

  position_t previous_output_position;

  /* The regime which was set up by the class constructor. */

  int immediate_output_regime;

  /* This variable length object contains all stored descriptors of
     messages. */

  vlo_t *descriptors_of_messages;

  /* This object stack contains strings of all stored messages. */

  os_t *messages;

  void fix_message (int error_flag, int fatal_error_flag,
                    int appended_message_flag, position_t position,
                    const char *message);

public:

  /* Value of this variable is maximum number of errors which will be
     fixed.  If a error is fixed with number equals to
     `maximum_number_of_errors' than special fatal error `fatal error
     -- too many errors' with position of given error is fixed instead
     of the error.  And all following errors are ignored.  Zero value
     of the variable means that the special fatal error will never
     fixed.  The original value of it is 50. */

  unsigned int maximum_number_of_errors;

  inline unsigned int number_of_errors (void) {return _number_of_errors;}

  inline unsigned int number_of_warnings (void) {return _number_of_warnings;}

  /* The following two functions allocate memory for the class instance. */

  inline void *operator new (size_t size)
    {
      return allocate::malloc (size);
    }

  inline void *operator new[] (size_t size)
    {
      return allocate::malloc (size);
    }

  /* The following two functions free memory for the class instance. */

  inline void operator delete (void *mem)
    {
      allocate:: free (mem);
    }

  inline void operator delete[] (void *mem)
    {
      allocate:: free (mem);
    }

  virtual void fatal_error_function (void);
  
  virtual void output_error_function (int appended_message_flag,
                                      position_t position,
                                      const char *message);
  void output (void);
  void error (int fatal_error_flag, position_t position,
              const char *format, ...);
  void warning (position_t position, const char *format, ...);
  void append_message (position_t position, const char *format, ...);
  void system_error (int fatal_error_flag, position_t position,
                     const char *format, ...);
  errors (int immediate_output_flag);
  ~errors (void);
};

#endif /* #ifndef __cplusplus */

#endif /* #ifndef __ERRORS__ */
