/* FILE NAME:   commline.c

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package for work command line; you can redistribute
   it and/or modify it under the terms of the GNU Library General
   Public License as published by the Free Software Foundation; either
   version 2, or (at your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Package for work with command line

   DESCRIPTION: This package implements features analogous to ones of
       public domain function `getopt'.  The goal of the package
       creation is to use more readable language of command line
       description and to use command line description as help output
       of the command.  POSIX terminology concerning command
       line is used here.  Command line is divided into command name
       and arguments.  The arguments are subdivided into options,
       option-arguments and operands.  Option starts with `-'.  All
       arguments after first `--' in command line are treated as
       operands.

         The description of command line is made up from two parts.
       Any parts (or both) may be absent in the description.  First
       part contains option-arguments names of options which are in
       the second part.  option-arguments names are separated by white
       space.  The second part starts with percents `%%' and contains
       any text in which description of options are placed.  Any
       description of option starts with character ``' followed by
       character `-' and finishes by character `''.  White spaces may
       precede option-argument name.  It means that the corresponding
       option has obligatory separate option-argument.  For example,
       the following may be a part of description of options of a
       compiler command line.

       dir xxx file
       %%
       command line:  pc [options] file ... 
                               Options:
       `-0'      Pascal standard level 0    `-1'      Pascal standard Level 1.
       `-29000'  AMD29000 code generation   `-29050'* AMD29050 code generation
       `-c'      only object files creation `-el'     output of listing
       `-g'      information for debuggers  `-Idir'   data task units directory
       `-lxxx'   library                    `-Ldir'   libraries directory
       `-o file' output file                `-O'      all optimizations
       `-S'      only ass. code creation    `-v'      version indication
       `-w'      no warnings generation
                      Star * marks defaults

         In this example options with names `-I', `-l', `-L' and `-o'
       have option-arguments but only option with name `-o' has
       separate option-argument, i.e. option-argument which is
       represented by separate argument after given option in command
       line.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of the package.

*/


#ifdef HAVE_CONFIG_H
#include "config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "vlobject.h"
#include "commline.h"

#include <assert.h>


/* The following structure is used for storing information about
   options which may be present in command line. */

struct option_description
{
  /* Option prefix in the option description on which option in the
     command line is recognized.  For example, `-D' is option name in
     option description `-DMACRO=DEFN'. */
  char *option_name;
  /* The rest in the option description.  For previous example, it is
     `MACRO=DEFN'. */
  char *option_suffix;
  /* TRUE if the option suffix starts with white spaces.  It means that
     the corresponding option has obligatory option-argument. */
  int separate_suffix_option;
};

/* The following variable contains pointers to arguments
   in the command line.  The element with order number 0 points to the
   command name.  I.e. this variable has analogous value as parameter
   `argv' of function `main'. */

char **argument_vector;

/* Number of elements in previous vector.  I.e. this variable has
   analogous value as parameter `argc' of function `main'.*/

int argument_count;

/* The following variable length oblect contains all option
   descriptions (see `struct option_description' above).  The elements
   are ordered by member option_name. */ 

static vlo_t all_options;

/* The following string is a reference to description of options.  The
   description form is described in file header. */

static const char *description_of_options;

/* The following function compares descriptions of two options.  This
   function is used by standard function `qsort'.  The function
   returns 0 if name of the options are equal, >0 if name of the first
   option is greater than name of the second, and <0 if name of the
   first option is less than name of the second. */

static int
compare_options (const void *option1, const void *option2)
{
  return strcmp (((struct option_description *) option1)->option_name,
                 ((struct option_description *) option2)->option_name);
}

/* The following function processes command line description (see file
   header) and command line itself.  The function also initiates
   variables `argument_vector' and `argument_count'.  These variables
   determine original command line.  The function returns FALSE if
   error in command line description is fixed, otherwise TRUE (it
   means success).  The function must be called only once before any
   work with the package. */

int
start_command_line_processing (int argc, char **argv,
                               const char *description)
{
  vlo_t suffixes, name;
  int suffix_length;
  char *end_name, *str;
  struct option_description option_description;

  argument_vector = argv;
  argument_count = argc;
  description_of_options = description;
  VLO_CREATE (all_options, sizeof (struct option_description) * 50);
  /* Processing first part of the description (see file header above). */
  VLO_CREATE (suffixes, 100);
  for (;;)
    {
      while (*description == ' ' || *description == '\t'
             || *description == '\n')
        description++;
      if (*description == '\0'
          || (*description == '%' && description[1] == '%'))
        break;
      do
        {
          VLO_ADD_BYTE (suffixes, *description);
          description++;
        }
      while (*description != '\0' && *description != ' '
             && *description != '\t'
             && *description != '\n'
             && (*description != '%' || description[1] != '%'));
      VLO_ADD_BYTE (suffixes, '\0');
    }
  if (*description != '%')
    return 1 /* TRUE */;
  /* Processing the second part of the description. */
  for (;;)
    {
      while (*description != '\0' && *description != '`')
        description++;
      if (*description == '\0')
        {
          /* End of the description.  Sorting option descriptions. */
          qsort (VLO_BEGIN (all_options),
                 VLO_LENGTH (all_options) / sizeof (struct option_description),
                 sizeof (struct option_description), compare_options);
          return 1 /* TRUE */;
        }
      description++;
      if (*description != '-')
        {
          /* Fixing error of the option description start. */
          VLO_DELETE (all_options);
          VLO_DELETE (suffixes);
          return 0 /* FALSE */;
        }
      VLO_CREATE (name, 50);
      do
        {
          VLO_ADD_BYTE (name, *description);
          description++;
        }
      while (*description != '\0' && *description != '\'');
      VLO_ADD_BYTE (name, '\0');
      if (*description == '\0')
        {
          /* Fixing error of the option description finish. */
          VLO_DELETE (all_options);
          VLO_DELETE (suffixes);
          return 0 /* FALSE */;
        }
      end_name = VLO_END (name);
      /* Testing suffix presence. */
      option_description.option_suffix = NULL;
      option_description.separate_suffix_option = 0 /* FALSE */;
      for (str = VLO_BEGIN (suffixes);
           str <= (char *) VLO_END (suffixes);
           str += suffix_length + 1)
        {
          suffix_length = strlen (str);
          /* Testing that suffix is separated by white spaces.
             `2' accounts for `-' and first option character
             (option name can not be empty). */
          if (suffix_length + 2 <= VLO_LENGTH (name) - 1
              && strcmp (str, end_name - suffix_length) == 0)
            {
              option_description.option_suffix = str;
              end_name -= suffix_length;
              while (*(end_name - 1) == ' '
                     || *(end_name - 1) == '\t' || *(end_name - 1) == '\n')
                {
                  option_description.separate_suffix_option = 1 /* TRUE */;
                  suffix_length++;
                  end_name--;
                }
              VLO_SHORTEN (name, suffix_length + 1);
              VLO_ADD_BYTE (name, '\0');
              break;
            }
        }
      VLO_TAILOR (name);
      option_description.option_name = VLO_BEGIN (name);
      VLO_ADD_MEMORY (all_options, (char *) &option_description,
                      sizeof (struct option_description));
    }
}

/* This function outputs the second part (without `%%' -- see file
   header) of description of options to stderr.  The function must be
   called only after function `start_command_line_processing'. */

void
output_command_line_description (void)
{
  const char *current_char_ptr;

  fprintf (stderr, "%s options:\n", argument_vector[0]);
  for (current_char_ptr = description_of_options;
       *current_char_ptr != '\0'
       && (*current_char_ptr != '%' || current_char_ptr[1] != '%');)
    current_char_ptr++;
  if (*current_char_ptr == '%')
    current_char_ptr += 2;
  fprintf (stderr, "%s\n", current_char_ptr);
}

/* This function searches for option descriptor which describes the
   command line argument with given number.  The function uses binary
   search.  The function returns pointer to the option descriptor or
   NULL if the corresponding option descriptor is not found.  The
   function must be called only after function
   `start_command_line_processing'. */

static struct option_description *
option_description (int argument_number)
{
  struct option_description *option_description_ptr;
  struct option_description *option_description_table;
  int left, right, middle, comparison_code;
  char *option_in_command_line;

  option_in_command_line = argument_vector[argument_number];
  option_description_table
    = (struct option_description *) VLO_BEGIN (all_options);
  for (left = 0,
       right = (VLO_LENGTH (all_options) / sizeof (struct option_description)
                - 1);
       left <= right;)
    {
      middle = (left + right) / 2;
      comparison_code = strcmp (option_description_table[middle].option_name,
                                option_in_command_line);
      if (comparison_code == 0)
        return option_description_table + middle;
      else if (comparison_code > 0)
        right = middle - 1;
      else
        left = middle + 1;
    }
  /* This code is needed because option in command line has
     option-argument which is not separated by white spaces (e.g.
     option in command line `-Ufoo' has  name in option description
     `-U').  In this case (so long the option descriptor is not found)
     RIGHT is the number of last option descriptor whose name is less
     than OPTION_IN_COMMAND_LINE. */
  for (option_description_ptr = option_description_table + right;
       option_description_ptr
       >= (struct option_description *) VLO_BEGIN (all_options)
       && option_in_command_line[1] == option_description_ptr->option_name[1];
       option_description_ptr--)
    if (option_description_ptr->option_suffix != NULL
        && !option_description_ptr->separate_suffix_option
        && strncmp (option_description_ptr->option_name,
                    option_in_command_line,
                    strlen (option_description_ptr->option_name)) == 0)
      return option_description_ptr;
  return NULL;
}

/* The following variables are used by function `next_operand'. */

/* Number of command line argument with which the next operand will be
   searched. */ 

static int next_operand_number;

/* TRUE if all the rest arguments are operands (i.e. `--' is processed in
   command line). */

static int only_operands;

/* This function searches for the first or next operand.  The function
   returns command line argument number of the first or next operand,
   0 if all operands are processed.  Returned number may be used as
   index of array `argument_vector' to access corresponding operand.
   The function must be called only after function
   `start_command_line_processing'.  This function searching the first
   operand is to be called before any the function call for searching
   for next operands. */

int
next_operand (int flag_of_first)
{
  struct option_description *option_description_ptr;

  if (flag_of_first)
    {
      next_operand_number = 1;
      only_operands = 0 /* FALSE */;
    }
  else
    assert (next_operand_number != 0);
  for (; next_operand_number < argument_count; next_operand_number++)
    {
      if (only_operands || *argument_vector[next_operand_number] != '-'
          || strcmp (argument_vector[next_operand_number], "-") == 0)
        {
          next_operand_number++;
          return next_operand_number - 1;
        }
      else if (strcmp (argument_vector[next_operand_number], "--") == 0)
        only_operands = 1 /* TRUE */;
      else
        {
          option_description_ptr = option_description (next_operand_number);
          if (option_description_ptr != NULL
              && option_description_ptr->separate_suffix_option)
            next_operand_number++;
        }
    }
  return 0;
}

/* This function defines number of operands in the command line.  The
   function returns number of operands in the command line.  The
   function must be called only after function
   `start_command_line_processing'.  The function does not affect the
   result of function `next_operand'. */

int
number_of_operands (void)
{
  int current_operand_number, result;
  int saved_next_operand_number, saved_only_operands;

  saved_next_operand_number = next_operand_number;
  saved_only_operands = only_operands;
  result = 0;
  for (current_operand_number = next_operand (1 /* TRUE */);
       current_operand_number != 0;
       current_operand_number = next_operand (0 /* FALSE */))
    result++;
  next_operand_number = saved_next_operand_number;
  only_operands = saved_only_operands;
  return result;
}

/* The following variables are used by function next_option. */

/* Number of command line argument with which the next option will be
   searched for. */ 

static int next_option_number;

/* TRUE if all the rest arguments are operands (i.e. `--' is processed in
   command line. */

static int no_more_options;

/* This function searches for the first or next option.  The function
   returns command line argument number of the first or next option, 0
   if all options are processed.  Returned number may be used as index
   of array `argument_vector' to access corresponding option.  The
   function must be called only after function
   `start_command_line_processing'.  This function searching the first
   option is to be called before any the function call for searching
   for next options.  */

int
next_option (int flag_of_first)
{
  struct option_description *option_description_ptr;
  int result;

  if (flag_of_first)
    {
      next_option_number = 1;
      no_more_options = 0 /* FALSE */;
    }
  else
    assert (next_option_number != 0);
  for (; next_option_number < argument_count; next_option_number++)
    {
      if (no_more_options)
        return 0;
      else if (strcmp (argument_vector[next_option_number], "--") == 0)
        no_more_options = 1 /* TRUE */;
      else if (*argument_vector[next_option_number] == '-'
               && strcmp (argument_vector[next_option_number], "-") != 0)
        {
          result = next_option_number;
          option_description_ptr = option_description (next_option_number);
          if (option_description_ptr != NULL
              && option_description_ptr->separate_suffix_option)
            next_option_number++;       /* Skip the option-argument. */
          next_option_number++;
          return result;
        }
    }
  return 0;
}

/* This function defines option name which describes the command line
   argument with given command line argument number.  Remember that
   option name may be differ from option in the command line
   (e.g. '-U' and `-Ufoo').  The function also defines that given
   option must have option-argument.  The case of returned NULL and
   `*option_has_argument' equals to TRUE means that given option must
   have option-argument but the option has not option-argument in the
   command line.  The function returns NULL if given command line
   argument is not option or is not described option or is a option
   described as with option-argument but which has not
   option-argument, option name otherwise.  The function must be
   called only after function `start_command_line_processing'. */

char *
option_characteristics (int argument_number, int *option_has_argument)
{
  struct option_description *option_description_ptr;

  *option_has_argument = 0 /* FALSE */;
  option_description_ptr = option_description (argument_number);
  if (option_description_ptr != NULL)
    {
      *option_has_argument = option_description_ptr->separate_suffix_option;
      if (!option_description_ptr->separate_suffix_option
          || (argument_number < argument_count - 1
              && (*argument_vector[argument_number + 1] != '-'
                  || strcmp (argument_vector[argument_number + 1], "-") == 0)))
        return option_description_ptr->option_name;
      else
        return NULL;
    }
  else
    return NULL;
}

/* This function searches for place of last option with given option
   name in the command line.  The function returns 0 if the option is
   not in the command line, otherwise command line argument number of
   last such option.  The function must be called only after function
   `start_command_line_processing'.  The function does not affect the
   result of function `next_option'. */

int
last_option_place (const char *option_name)
{
  char *current_option_name;
  int argument_number, last, saved_next_option_number;
  int saved_no_more_options;
  int param;

  saved_next_option_number = next_option_number;
  saved_no_more_options = no_more_options;
  last = 0;
  argument_number = next_option (1 /* TRUE */);
  while (argument_number != 0)
    {
      current_option_name = option_characteristics (argument_number, &param);
      if (current_option_name != NULL
          && strcmp (current_option_name, option_name) == 0)
        last = argument_number;
      argument_number = next_option (0 /* FALSE */);
    }
  next_option_number = saved_next_option_number;
  no_more_options = saved_no_more_options;
  return last;
}

/* This function searches for option-argument of last option in the
   command line with given option name.  The function returns pointer
   to argument of last option in the command line with given option
   name or NULL if the option is absent in the command line.  The
   function must be called only after function
   `start_command_line_processing'.  The function must be called only
   for options which have argument separated by white spaces. */

char *
option_argument (const char *option_name)
{
  int argument_number;

  argument_number = last_option_place (option_name);
  return (argument_number == 0 ? NULL : argument_vector[argument_number + 1]);
}
