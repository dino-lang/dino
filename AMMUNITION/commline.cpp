/*
   FILE NAME:   commline.cpp

   TITLE:       Implementation of class for work with command line

   DESCRIPTION: This package implements features analogous to ones of
       public domain function `getopt'.  The goal of the class
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
         Defining macro `NDEBUG' (e.g. by option `-D' in C++ compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of the class.

*/


#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "objstack.h"
#include "vlobject.h"
#include "commline.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif


/* The following function compares descriptions of two options.  This
   function is used by standard function `qsort'.  The function
   returns 0 if name of the options are equal, >0 if name of the first
   option is greater than name of the second, and <0 if name of the
   first option is less than name of the second. */

static int
compare_options (const void *option1, const void *option2)
{
  return
    strcmp
    (((struct command_line::option_description *) option1)->option_name,
     ((struct command_line::option_description *) option2)->option_name);
}

/* The constructor processes command line description (see file
   header) and command line itself.  The function also initiates
   variables `_argument_vector' and `_argument_count'.  These
   variables determine original command line.  The function returns
   FALSE with the aid of the last parameter if error in command line
   description is fixed, otherwise TRUE (it means success). */

command_line::command_line (int argc, char **argv, const char *description,
                            int &correct_description_flag)
{
  vlo_t suffixes (100);
  int suffix_length;
  char *end_name, *str;
  struct option_description option_description;

  _argument_vector = argv;
  _argument_count = argc;
  description_of_options = description;
  all_options = new vlo (sizeof (struct option_description) * 50);
  option_names = new os ();
  /* Processing first part of the description (see file header above). */
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
          suffixes.add_byte (*description);
          description++;
        }
      while (*description != '\0' && *description != ' '
             && *description != '\t'
             && *description != '\n'
             && (*description != '%' || description[1] != '%'));
      suffixes.add_byte ('\0');
    }
  if (*description != '%')
    {
      correct_description_flag = 1 /* TRUE */;
      return;
    }
  /* Processing the second part of the description. */
  for (;;)
    {
      while (*description != '\0' && *description != '`')
        description++;
      if (*description == '\0')
        {
          /* End of the description.  Sorting option descriptions. */
          qsort (all_options->begin (),
                 all_options->length () / sizeof (struct option_description),
                 sizeof (struct option_description), compare_options);
          correct_description_flag = 1 /* TRUE */;
          return;
        }
      description++;
      if (*description != '-')
        {
          /* Fixing error of the option description start. */
          delete all_options;
          correct_description_flag = 0 /* FALSE */;
          return;
        }
      do
        {
          option_names->top_add_byte (*description);
          description++;
        }
      while (*description != '\0' && *description != '\'');
      option_names->top_add_byte ('\0');
      if (*description == '\0')
        {
          /* Fixing error of the option description finish. */
          delete all_options;
          correct_description_flag = 0 /* FALSE */;
          return;
        }
      end_name = (char *) option_names->top_end ();
      /* Testing suffix presence. */
      option_description.option_suffix = NULL;
      option_description.separate_suffix_option = 0 /* FALSE */;
      for (str = (char *) suffixes.begin ();
           str <= (char *) suffixes.end ();
           str += suffix_length + 1)
        {
          suffix_length = strlen (str);
          /* Testing that suffix is separated by white spaces.
             `2' accounts for `-' and first option character
             (option name can not be empty). */
          if ((unsigned) suffix_length + 2 <= option_names->top_length () - 1
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
              option_names->top_shorten (suffix_length + 1);
              option_names->top_add_byte ('\0');
              break;
            }
        }
      option_description.option_name = (char *) option_names->top_begin ();
      option_names->top_finish ();
      all_options->add_memory ((char *) &option_description,
                               sizeof (struct option_description));
    }
}

/* The destructor. */

command_line::~command_line (void)
{
  delete all_options;
  delete option_names;
}

/* This function outputs the second part (without `%%' -- see file
   header) of description of options to stderr. */

void
command_line::output_description (void)
{
  const char *current_char_ptr;

  fprintf (stderr, "%s options:\n", argument_vector () [0]);
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
   NULL if the corresponding option descriptor is not found. */

struct command_line::option_description *
command_line::option_description (int argument_number)
{
  struct option_description *option_description_ptr;
  struct option_description *option_description_table;
  int left, right, middle, comparison_code;
  char *option_in_command_line;

  option_in_command_line = argument_vector () [argument_number];
  option_description_table
    = (struct option_description *) all_options->begin ();
  for (left = 0,
       right = (all_options->length () / sizeof (struct option_description)
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
       >= (struct option_description *) all_options->begin ()
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

/* This function searches for the first or next operand.  The function
   returns command line argument number of the first or next operand,
   0 if all operands are processed.  Returned number may be used as
   index of array `argument_vector' to access corresponding operand.
   This function searching the first operand is to be called before
   any the function call for searching for next operands. */

int
command_line::next_operand (int flag_of_first)
{
  struct option_description *option_description_ptr;

  if (flag_of_first)
    {
      next_operand_number = 1;
      only_operands = 0 /* FALSE */;
    }
  else
    assert (next_operand_number != 0);
  for (; next_operand_number < argument_count (); next_operand_number++)
    {
      if (only_operands || *argument_vector () [next_operand_number] != '-'
          || strcmp (argument_vector () [next_operand_number], "-") == 0)
        {
          next_operand_number++;
          return next_operand_number - 1;
        }
      else if (strcmp (argument_vector () [next_operand_number], "--") == 0)
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
   function does not affect the result of function `next_operand'. */

int
command_line::number_of_operands (void)
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

/* This function searches for the first or next option.  The function
   returns command line argument number of the first or next option, 0
   if all options are processed.  Returned number may be used as index
   of array `argument_vector' to access corresponding option.  This
   function searching the first option is to be called before any the
   function call for searching for next options.  */

int
command_line::next_option (int flag_of_first)
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
  for (; next_option_number < argument_count (); next_option_number++)
    {
      if (no_more_options)
        return 0;
      else if (strcmp (argument_vector () [next_option_number], "--") == 0)
        no_more_options = 1 /* TRUE */;
      else if (*argument_vector () [next_option_number] == '-'
               && strcmp (argument_vector () [next_option_number], "-") != 0)
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
   option-argument, option name otherwise. */

char *
command_line::option_characteristics (int argument_number,
                                      int *option_has_argument)
{
  struct option_description *option_description_ptr;

  *option_has_argument = 0 /* FALSE */;
  option_description_ptr = option_description (argument_number);
  if (option_description_ptr != NULL)
    {
      *option_has_argument = option_description_ptr->separate_suffix_option;
      if (!option_description_ptr->separate_suffix_option
          || (argument_number < argument_count () - 1
              && (*argument_vector () [argument_number + 1] != '-'
                  || strcmp (argument_vector () [argument_number + 1],
                             "-") == 0)))
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
   last such option.  The function does not affect the result of
   function `next_option'. */

int
command_line::last_option_place (const char *option_name)
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
   function must be called only for options which have argument
   separated by white spaces. */

char *
command_line::option_argument (const char *option_name)
{
  int argument_number;

  argument_number = last_option_place (option_name);
  return (argument_number == 0
          ? (char *) NULL : argument_vector () [argument_number + 1]);
}
