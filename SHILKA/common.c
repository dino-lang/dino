/*
   FILE NAME:   common.c

   TITLE:       Common variables for all Shilka (keywords description
                translator)

   DESCRIPTION: This file contains declarations of external variables
                common for all Shilka (keywords description translator).

*/

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>
#include "ird.h"
#include "common.h"

/* Output C++ code instead of C code (`-c++'). */

int cpp_flag;

#ifndef NDEBUG

/* Output debugging information during Shilka run (`-debug'). */

int debug_flag;

#endif

/* Generate macro switching on gathering and printing keyword usage
   statistics (`-statistics'). */

int statistics_flag;

/* Generate keyword inline for the function (`-inline'). */

int inline_flag;

/* Strip code for gathering and printing statistics (`-strip'). */

int strip_flag;

/* Generation additional interface file (with suffix .h)
   (`-enum'). */

int interface_flag;

/* Output enumeration instead of macro definitions for instruction
   codes (`-enum'). */

int enum_flag;

/* Usage of strncmp instead of strcmp to compare keywords (`-length').
   In other words, terminated '\0' is not necessary in keywords. */

int length_flag;

/* Keywords case is ignored (`-case'). */

int case_flag;

/* Definitions of identifiers of keywords are not generated
   (`-no-definitions'). */

int no_definitions_flag;

/* Generation of macros defining identifiers of instructions
   (`-export') in the interface file (instead of implementation
   file). */

int export_flag;

/* Value of this variable is prefix of names of generated keywords
   recognizer objects.  This value is defined by argument of option
   `-p...' or by standard prefix. */

char *prefix;

/* Flag of output of time statistics (`-v'). */

int time_flag;

/* The following is number after option `-fast'.  If number of rest
   unchecked characters is less or equal than this number, use these
   characters comparison instead strcmp (default value is 3). */

int fast_number;

/* Flag of disabling output of warnings (`-w'). */

int w_flag;

/* Flag of creation of description file which contains description of
   result automaton and statistics information (`-v'). */

int v_flag;

/* Value of this variable is name of description.  The value is
   defined by file name (without suffix) given as operand of the
   Shilka command line. */

char *description_name;

/* Interface file of keywords recognizer.  The value is NULL if the
   file is not created. */

FILE *output_interface_file;

/* Keywords recognizer interface file name. */

char *output_interface_file_name;

/* Implementation file of keywords recognizer.  The value is NULL if
   the file is not created. */

FILE *output_implementation_file;

/* Keywords recognizer implementation file name. */

char *output_implementation_file_name;

/* Description file of keywords recognizer.  The value is NULL if the
   file is not created. */

FILE *output_description_file;

/* Keywords recognizer description file name. */

char *output_description_file_name;

/* Value of the following variable is node representing description
   being processed. */

IR_node_t description;

/* I remember that tolower on a computer worked only for letters.
   This is for the case. */

int
our_tolower (int c)
{
  unsigned char ch = c;

  if (isalpha (ch))
    return tolower (ch);
  else
    return ch;
}
