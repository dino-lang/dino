/*
   FILE NAME:   yacc.y

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool SHILKA.

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

   TITLE:       Parser of Shilka (keywords description translator)
                described as YACC file

   DESCRIPTION: This file makes lexical, syntactic analysis
                (with possible syntactic error recovery) of a file and
                builds up internal representation of parsed keywords
                description.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of parser work and errors of its usage.

*/

%{

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "position.h"
#include "errors.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
#include "yacc.h"


#include <assert.h>

/* The following variables and forward definitions of functions are
   used in actions of YACC grammar. */

/* The following variable value is begin position of the last lexema
   returned by the function `yylex'. */

static position_t current_lexema_position;

static void syntax_error (const char *s);

/* The following macro value is frequency value of keyword when the
   frequency is not given in the description. */

#define DEFAULT_FREQUENCY_VALUE 100

/* The following variable is attribute of construction
   `%type identifier'. */

static IR_node_t keyword_type;

static char *make_lower_case_string (char *string);

%}


/* Attributes of all YACC symbols (token and nonterminals) will be
   `node'.  Some construction YACC action will be `position' or `number'. */

%union
{
  IR_node_t node;
  int number;
  position_t position;
}

/* The attributes of the following tokens are not defined and not used. */

%token PERCENTS LOCAL IMPORT EXPORT TYPE EQUAL OTHER

/* The attributes of the following tokens are defined and used. */

%token <node>   IDENTIFIER STRING CODE_INSERTION ADDITIONAL_C_CODE
%token <number> NUMBER

%type <node>    declaration_part  keyword_definition_list declaration
                keyword_definition optional_name optional_action

%type <number>  frequency

%start description

%%

description : declaration_part  PERCENTS
                {
                  description
                    = IR_new_description
                      (($1 == NULL ? (keyword_type == NULL
                                      ? current_lexema_position
                                      : IR_position (keyword_type))
                        : IR_position ($1)),
                       $1, keyword_type);
                }
              keyword_definition_list  ADDITIONAL_C_CODE
                {
                  IR_node_t first_declaration;
                  IR_node_t first_keyword;
                  
                  /* Transform cyclic declaration list to usual list. */
                  if ($1 != NULL)
                    {
                      first_declaration = IR_next_declaration ($1);
                      IR_set_next_declaration ($1, NULL);
                      $1 = first_declaration;
                    }
		  IR_set_declaration_list (description, $1);
                  /* Transform cyclic keyword definition list to usual
                     list. */
                  if ($4 != NULL)
                    {
                      first_keyword = IR_next_keyword ($4);
                      IR_set_next_keyword ($4, NULL);
                      $4 = first_keyword;
                    }
                  IR_set_keyword_list (description, $4);
                  IR_set_additional_code (description, $5);
                }
            ;

/* The attribute of the following nonterminal is the last declaration
   (except for declarations of kind of `%type identifier' of given
   declaration list.  The declaration list is cyclic. */

declaration_part :               {$$ = NULL; keyword_type = NULL;}
                 | declaration_part  declaration
                     {
                       if ($2 == NULL)
                         $$ = $1;
                       else
                         {
                           if ($1 == NULL)
                             /* Make cycle. */
                             IR_set_next_declaration ($2, $2);
                           else
                             {
                               /* Add element to cyclic list */
                               IR_set_next_declaration
                                 ($2, IR_next_declaration ($1));
                               IR_set_next_declaration ($1, $2);
                             }
                           $$ = $2;
                         }
                     }
                 | declaration_part  error
                     {
                       syntax_error ("syntax error in declaration");
                       $$ = $1;  /* Ignore error declaration. */
                     }
                 ;

/* The attribute of the following nonterminal is the corresponding
   declaration node (NULL for keyword type identifier). */

declaration : TYPE IDENTIFIER
                {
                  if (keyword_type != NULL)
                    {
                      if (!w_flag)
                        {
                          warning
                            (IR_position ($2),
                             "warning -- repeated type declaration is ignored");
                          append_message (IR_position (keyword_type),
                                          "here the first type declaration");
                        }
                    }
                  else
                    keyword_type = $2;
                  $$ = NULL;
                }
            | LOCAL  {$<position>$ = current_lexema_position;} CODE_INSERTION
                {
                  $$ = IR_new_local_code ($<position>2, NULL, $3);
                }
            | IMPORT {$<position>$ = current_lexema_position;} CODE_INSERTION
                {
                  $$ = IR_new_import_code ($<position>2, NULL, $3);
                }
            | EXPORT {$<position>$ = current_lexema_position;} CODE_INSERTION
                {
                  $$ = IR_new_export_code ($<position>2, NULL, $3);
                }
            ;

/* The attribute of the following nonterminal is the last
   keyword_definition of given keyword definition list.  The keyword
   definition list is cyclic. */

keyword_definition_list :                                  {$$ = NULL;}
                        | keyword_definition_list  keyword_definition
                           {
                             if ($2 == NULL)
                               /* Other is not included into the list */
                               $$ = $1;
                             else
                               {
                                 if ($1 == NULL)
                                   /* Make cycle. */
                                   IR_set_next_keyword ($2, $2);
                                 else
                                   {
                                     /* Add element to cyclic list */
                                     IR_set_next_keyword
                                       ($2, IR_next_keyword ($1));
                                     IR_set_next_keyword ($1, $2);
                                   }
                                 $$ = $2;
                               }
                           }
                        | keyword_definition_list  error
                           {
                             syntax_error
                               ("syntax error in keyword definition");
                             $$ = $1;  /* Ignore error keyword definition. */
                           }
                        ;

/* The attribute of the following nonterminal is keyword
   declaration. */

keyword_definition : IDENTIFIER {$<position>$ = current_lexema_position;}
                     optional_name frequency optional_action
                       {
			 IR_node_t identifier = $1;

			 if (case_flag)
			   identifier
			     = IR_new_identifier (IR_position ($1),
						  make_lower_case_string
						  (IR_identifier_itself ($1)));
                         $$ = IR_new_identifier_keyword
                              ($<position>2,
                               ($3 == NULL ? $1 : $3), $4, $5, NULL,
			       identifier);
                       }
                   | STRING {$<position>$ = current_lexema_position;}
                     EQUAL IDENTIFIER frequency optional_action
                       {
			 IR_node_t string = $1;

			 if (case_flag)
			   string
			     = IR_new_string (IR_position ($1),
					      IR_string_representation ($1),
					      make_lower_case_string
					      (IR_string_itself ($1)));
                         $$ = IR_new_string_keyword ($<position>2,
                                                     $4, $5, $6, NULL,
						     string);
                       }
                   | OTHER {$<position>$ = current_lexema_position;}
                     optional_action
                       {
                         $$ = NULL;
                         if (IR_other_is_present (description))
                           {
                             if (!w_flag)
                               {
                                 warning
                                   ($<position>2,
                                    "warning -- repeated %other is ignored");
                                 append_message
                                   (IR_other_position (description),
                                    "here the first %other");
                               }
                           }
                         else
                           {
                             IR_set_other_action (description, $3);
                             IR_set_other_position (description, $<position>2);
                             IR_set_other_is_present (description, TRUE);
                           }
                       }
                   ;

/* The attribute of the following nonterminal is keyword name if
   any. */

optional_name :                   {$$ = NULL;}
              | EQUAL IDENTIFIER  {$$ = $2;}
              ;

/* The attribute of the following nonterminal is keyword frequency
   usage. */

frequency :         {$$ = DEFAULT_FREQUENCY_VALUE;}
          | NUMBER  {$$ = $1;}
          ;

/* The attribute of the following nonterminal is the corresponding
   code insertion (or NULL). */

optional_action :                 {$$ = NULL;}
                | CODE_INSERTION  {$$ = $1;}
                ;

%%



/* Return lower case variant of STRING. */

static char *
make_lower_case_string (char *string)
{
  char *str;

  IR_TOP_ADD_STRING (string);
  string = (char *) IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  for (str = string; *str != '\0'; str++)
    *str = our_tolower (*str);
  return string;
}



/* This page contains abstract data `scanner'.  This abstract data
   divides input stream characters on tokens (lexemas). */

/* The following structure describes current input stream and scanner
   state. */

struct input_stream_state
{
  FILE *input_file;
  /* The following member is TRUE if the first `%%' in given input stream
     was already processed or FALSE otherwise. */
  int it_is_first_percents;
  /* The following member is TRUE if the ADDITIONAL_CODE lexema was already
     returned by the scanner or FALSE otherwise.  Remember that lexema
     ADDITIONAL_CODE is always returned by the scanner
     even if second `%%' in the input stream is absent. */
  int additional_c_code_was_returned;
};

/* The following structure contains current input stream and
   scanner state.  If the member `input_file' value is NULL
   then input stream is not defined. */

static struct input_stream_state current_input_stream_state;


/* The function returns lexical code of corresponding keyword or 0 if
   given identifier (parameter `str' is identifier (without start `%')
   string with end marker '\0'; `length' is length of identifier
   string without end marker '\0') is not keyword.  Fast search is
   implemented by minimal pruned O-trie forest.  */

static int
find_keyword (const char *str, int length)
{
  switch (length)
    {
    case 4:
      return (strcmp (str, "type") == 0 ? TYPE : 0);
    case 5:
      switch (*str)
        {
        case 'l':
          return (strcmp (str, "local") == 0 ? LOCAL : 0);
        case 'o':
          return (strcmp (str, "other") == 0 ? OTHER : 0);
        default:
          return 0;
        }
    case 6:
      switch (*str)
        {
        case 'e':
          return (strcmp (str, "export") == 0 ? EXPORT : 0);
        case 'i':
          return (strcmp (str, "import") == 0 ? IMPORT : 0);
        default:
          return 0;
        }
    default:
      return 0;
    }
}

/* The following value is used if previous_char does not contain an
   input char */

#define EMPTY_PREVIOUS_CHAR (-2000)

/* The following variable is used for storing '\r'. */

static int previous_char;

/* The following two functions for replacing "\r\n" onto "\n". */

static int
getch (FILE *f)
{
  int c;

  if (previous_char != '\r')
    c = getc (f);
  else
    {
      c = previous_char;
      previous_char = EMPTY_PREVIOUS_CHAR;
    }
  if (c == '\r')
    {
      c = getc (f);
      if (c != '\n')
        {
          ungetc (c, f);
          c = '\r';
        }
    }
  return c;
}

static void
ungetch (int c, FILE *f)
{
  if (c != '\r')
    ungetc (c, f);
  else
    previous_char = c;
}


/* The function skips C commentary.  Parameter `store_flag' is flag of
   storing commentary characters onto top internal representation
   object.  The function returns TRUE if no error (EOF) was fixed,
   otherwise FALSE
     Current position corresponds to `*' after commentary start ('/')
   before the function call.  Current_position corresponds to first
   character after skipped commentary ('/') or EOF after the function
   call. */

static int
skip_commentary (int store_flag)
{
  int input_char;
  
  current_position.column_number++;
  if (store_flag)
    IR_TOP_ADD_BYTE ('*');
  for (;;)
    {
      input_char = getch (current_input_stream_state.input_file);
      /* Here `current_position' corresponds to `input_char'. */
      if (input_char == '*')
        {
          if (store_flag)
            IR_TOP_ADD_BYTE (input_char);
          input_char = getch (current_input_stream_state.input_file);
          current_position.column_number++;
          /* Here `current_position' corresponds to `input_char'. */
          if (input_char == '/')
            {
              if (store_flag)
                IR_TOP_ADD_BYTE (input_char);
              current_position.column_number++;
              return TRUE;
            }
          else
            {
              ungetch (input_char, current_input_stream_state.input_file);
              continue;
            }
        }
      else if (input_char == EOF)
        {
          ungetch (input_char, current_input_stream_state.input_file);
          return FALSE;
        }
      if (input_char == '\n')
        {
          current_position.column_number = 1;
          current_position.line_number++;
        }
      else if (input_char == '\t')
        current_position.column_number
          = ((current_position.column_number - 1) / TAB_STOP + 1)
            * TAB_STOP + 1;
      else
        current_position.column_number++;
      if (store_flag)
        IR_TOP_ADD_BYTE (input_char);
    }
}


/* The function skips C string code (e.g `a', `\n', or `\077').  The
   function also stores skipped characters onto top internal
   representation object.  Parameter `input_char' is the most recent
   read character.  The function returns (with the aid of parameter
   `correct_new_line') flag of that correct new line was read.  The
   function returns TRUE if no error (EOF or incorrect new line) was
   fixed, otherwise FALSE.
     The value of parameter `correct_new_line' can be used for fixing
   errors when character constant is processed.  It is supposed that
   the current character is not end marker (string or character
   constant).  Current position corresponds to parameter `input_char'
   before the function call.  Current_position corresponds to first
   character after skipped valid character code or EOF (or incorrect
   new line) after the function call. */

static int
skip_string_code (int input_char, int *correct_new_line)
{
  if (input_char == EOF || input_char == '\n')
    {
      ungetch (input_char, current_input_stream_state.input_file);
      return FALSE;
    }
  *correct_new_line = FALSE;
  IR_TOP_ADD_BYTE (input_char);
  if (input_char == '\\')
    {
      current_position.column_number++;
      input_char = getch (current_input_stream_state.input_file);
      IR_TOP_ADD_BYTE (input_char);
      if (input_char == 'n' || input_char == 't' || input_char == 'v'
          || input_char == 'b' || input_char == 'r' || input_char == 'f'
          || input_char == '\\' || input_char == '\'' || input_char == '\"')
        current_position.column_number++;
      else if (input_char == '\n')
        {
          current_position.column_number = 1;
          current_position.line_number++;
          *correct_new_line = TRUE;
        }
      else if (isdigit (input_char) && input_char != '8'
               && input_char != '9')
        {
          current_position.column_number++;
          input_char = getch (current_input_stream_state.input_file);
          if (!isdigit (input_char) || input_char == '8'
              || input_char == '9')
            ungetch (input_char, current_input_stream_state.input_file);
          else
            {
              current_position.column_number++;
              IR_TOP_ADD_BYTE (input_char);
              input_char = getch (current_input_stream_state.input_file);
              if (!isdigit (input_char) || input_char == '8'
                  || input_char == '9')
                ungetch (input_char, current_input_stream_state.input_file);
              else
                {
                  current_position.column_number++;
                  IR_TOP_ADD_BYTE (input_char);
                }
            }
        }
      else
        current_position.column_number++;
    }
  else if (input_char == '\t')
    current_position.column_number
      = ((current_position.column_number - 1) / TAB_STOP + 1) * TAB_STOP + 1;
  else
    current_position.column_number++;
  return TRUE;
}


static const char *
get_string_code (const char *code_representation,
                 int *character_code, int *correct_new_line)
{
  *correct_new_line = FALSE;
  *character_code = *code_representation;
  if (*code_representation == '\\')
    {
      code_representation++;
      if (*code_representation == 'n' || *code_representation == 't'
          || *code_representation == 'v' || *code_representation == 'b'
          || *code_representation == 'r' || *code_representation == 'f'
          || *code_representation == '\\' || *code_representation == '\''
          || *code_representation == '\"')
        {
            if (*code_representation == 'n')
              *character_code = '\n';
            else if (*code_representation == 't')
              *character_code = '\t';
            else if (*code_representation == 'v')
              *character_code = '\v';
            else if (*code_representation == 'b')
              *character_code = '\b';
            else if (*code_representation == 'r')
              *character_code = '\r';
            else if (*code_representation == 'f')
              *character_code = '\f';
            else if (*code_representation == '\\')
              *character_code = '\\';
            else if (*code_representation == '\'')
              *character_code = '\'';
            else if (*code_representation == '\"')
              *character_code = '\"';
            else
              assert (FALSE);
        }
      else if (isdigit (*code_representation) && *code_representation != '8'
               && *code_representation != '9')
        {
          *character_code = *code_representation - '0';
          code_representation++;
          if (!isdigit (*code_representation) || *code_representation == '8'
              || *code_representation == '9')
            code_representation--;
          else
            {
              *character_code
                = *character_code * 8 + *code_representation - '0';
              code_representation++;
              if (!isdigit (*code_representation)
                  || *code_representation == '8'
                  || *code_representation == '9')
                code_representation--;
              else
                {
                  *character_code
                    = *character_code * 8 + *code_representation - '0';
                  current_position.column_number++;
                }
            }
        }
      else if (*code_representation != '\n')
        *character_code = *code_representation;
      else
        *correct_new_line = TRUE;
    }
  return code_representation;
}

/* The function skips C character constant by using function
   `skip_string_code'.  The function may fix error in the C character
   constant.  The function also stores skipped characters onto top
   internal representation object.  Current position corresponds to
   first character after '\'' (which was already read) before the
   function call.  Current_position corresponds to to first character
   after '\'' if error was not fixed after the function call. */

static void
skip_C_character (void)
{
  int input_char;
  int correct_new_line;
  int error_flag;

  error_flag = FALSE;
  input_char = getch (current_input_stream_state.input_file);
  /* Here `current_position' corresponds to `input_char'. */
  if (input_char == '\'')
    {
      IR_TOP_ADD_BYTE (input_char);
      error (FALSE, current_position, "invalid character constant");
      current_position.column_number++;
      error_flag = TRUE;
    }
  else
    {
      if (!skip_string_code (input_char, &correct_new_line)
          || correct_new_line)
        {
          error (FALSE, current_position, "invalid character constant");
          error_flag = TRUE;
        }
    }
  input_char = getch (current_input_stream_state.input_file);
  if (input_char != '\'')
    {
      ungetch (input_char, current_input_stream_state.input_file);
      if (!error_flag)
        error (FALSE, current_position, "invalid character constant");
    }
  else
    {
      IR_TOP_ADD_BYTE (input_char);
      current_position.column_number++;
    }
}

/* The function skips C string constant by using function
   `skip_string_code'.  The function may fix error in the C string
   constant.  The function also stores skipped characters onto top
   internal representation object.  Current position corresponds to
   first character after '\"' (which was already read) before the
   function call.  Current_position corresponds to to first character
   after '\"' if error was not fixed after the function call.  */

static void
skip_C_string (void)
{
  int input_char;
  int correct_new_line;
  
  for (;;)
    {
      /* Here `current_position' corresponds to next character. */
      input_char = getch (current_input_stream_state.input_file);
      if (input_char == '\"')
        {
          current_position.column_number++;
          IR_TOP_ADD_BYTE (input_char); 
          break;
        }
      if (!skip_string_code (input_char, &correct_new_line))
        {
          error (FALSE, current_position, "string end is absent");
          break;
        }
    }
}


static void
add_C_string_value (const char *string_representation,
                    int string_representation_length)
{
  int correct_new_line;
  int character_code;

  for (;;)
    {
      string_representation++;
      string_representation_length--;
      if (string_representation_length < 0
          || *string_representation == '\"')
        break;
      string_representation
        = get_string_code (string_representation,
                           &character_code, &correct_new_line);
      if (!correct_new_line)
        IR_TOP_ADD_BYTE (character_code);
    }
  IR_TOP_ADD_BYTE (0);
}

/* This function is major scanner function.  The function recognizes
   next source lexema from the input file, returns its code, modifies
   variable current_position so that its value is equal to position of
   the current character in the current input file and sets up
   variable `current_lexema_position' so that its value is equal to
   start position of the returned lexema, creates corresponding
   internal representation node (if it is needed) and sets up yylval
   to the node.  The function skips all white spaces and commentaries
   and fixes the most of lexical errors. */

int
yylex (void)
{
  int input_char;
  int number_of_successive_error_characters;
  
  for (number_of_successive_error_characters = 0;;)
    {
      input_char = getch (current_input_stream_state.input_file);
      /* `current_position' corresponds `input_char' here */
      switch (input_char)
        {
        case '\f':
          /* flow-through */
        case ' ':
          current_position.column_number++;
          break;
        case '\t':
          current_position.column_number
            = ((current_position.column_number - 1) / TAB_STOP + 1)
              * TAB_STOP + 1;
          break;
        case '\n':
          current_position.column_number = 1;
          current_position.line_number++;
          break;
        case EOF:
          current_lexema_position = current_position;
          if (!current_input_stream_state.additional_c_code_was_returned)
            {
              char *additional_code_itself;

              ungetch (input_char, current_input_stream_state.input_file);
              current_input_stream_state.additional_c_code_was_returned = TRUE;
              IR_TOP_ADD_BYTE ('\0');
              additional_code_itself = IR_TOP_BEGIN ();
              IR_TOP_FINISH ();
              yylval.node = IR_new_additional_code (current_lexema_position,
                                                    additional_code_itself);
              return ADDITIONAL_C_CODE;
            }
          else
            return 0;
        case '/':
          current_lexema_position = current_position;
          current_position.column_number++;
          input_char = getch (current_input_stream_state.input_file);
          if (input_char != '*')
            {
              number_of_successive_error_characters++;
              if (number_of_successive_error_characters == 1)
                error (FALSE, current_lexema_position,
                       "invalid input character '/'");
              ungetch (input_char, current_input_stream_state.input_file);
            }
          else if (!skip_commentary (FALSE))
            error (FALSE, current_lexema_position, "commentary end is absent");
          break;
        case '=':
          current_lexema_position = current_position;
          current_position.column_number++;
          return EQUAL;
        case '\"':
          {
            char *string_representation;
            char *string_itself;
            int string_representation_length;

            current_lexema_position = current_position;
            current_position.column_number++;
            IR_TOP_ADD_BYTE (input_char); /* Add " */
            skip_C_string ();
            string_representation_length = IR_TOP_LENGTH ();
            IR_TOP_ADD_BYTE (0);
            string_representation = insert_string (IR_TOP_BEGIN ());
            if (string_representation == (char *) IR_TOP_BEGIN ())
              IR_TOP_FINISH ();
            else
              IR_TOP_NULLIFY ();
            add_C_string_value (string_representation,
                                string_representation_length);
            string_itself = insert_string (IR_TOP_BEGIN ());
            if (strlen (string_itself) + 1 != IR_TOP_LENGTH ())
              error (FALSE, current_lexema_position,
                     "null character in %s", string_representation);
            if (string_itself == (char *) IR_TOP_BEGIN ())
              IR_TOP_FINISH ();
            else
              IR_TOP_NULLIFY ();
            yylval.node = IR_new_string (current_lexema_position,
                                         string_representation,
                                         string_itself);
            return STRING;
          }
          break;
        case '{':
          {
            int bracket_level = 1;
            int error_flag;
            char *code_itself;
            
            current_lexema_position = current_position;
            current_position.column_number++;
            for (error_flag = FALSE; !error_flag && bracket_level != 0;)
              {
                input_char = getch (current_input_stream_state.input_file);
                IR_TOP_ADD_BYTE (input_char);
                /* `current_position' corresponds to `input_char' here */
                switch (input_char)
                  {
                  case '/':
                    current_position.column_number++;
                    input_char = getch (current_input_stream_state.input_file);
                    if (input_char == '*')
                      {
                        current_position.column_number++;
                        error_flag = !skip_commentary (TRUE);
                      }
                    else
                      ungetch (input_char,
                               current_input_stream_state.input_file);
                    break;
                  case '{':
                    current_position.column_number++;
                    bracket_level++;
                    break;
                  case '}':
                    current_position.column_number++;
                    bracket_level--;
                    if (bracket_level == 0)
                      IR_TOP_SHORTEN (1);
                    break;
                  case '\'':
                    current_position.column_number++;
                    skip_C_character ();
                    break;
                  case '\"':
                    current_position.column_number++;
                    skip_C_string ();
                    break;
                  case '\t':
                    current_position.column_number
                      = ((current_position.column_number - 1) / TAB_STOP + 1)
                        * TAB_STOP + 1;
                    break;
                  case '\n':
                    current_position.column_number = 1;
                    current_position.line_number++;
                    break;
                  case EOF:
                    IR_TOP_SHORTEN (1);
                    ungetch (input_char,
                             current_input_stream_state.input_file);
                    error_flag = TRUE;
                    break;
                  default:
                    current_position.column_number++;
                    break;
                  }
              }
            if (error_flag)
              error (FALSE, current_lexema_position,
                     "C code insertion end is absent");
            IR_TOP_ADD_BYTE ('\0');
            code_itself = IR_TOP_BEGIN ();
            IR_TOP_FINISH ();
            yylval.node = IR_new_code_insertion (current_lexema_position,
                                                 code_itself);
            return CODE_INSERTION;
          }
        case '%':
          current_lexema_position = current_position;
          current_position.column_number++;
          input_char = getch (current_input_stream_state.input_file);
          if (input_char == '%')
            {
              current_position.column_number++;
              if (current_input_stream_state.it_is_first_percents)
                {
                  current_input_stream_state.it_is_first_percents = FALSE;
                  return PERCENTS;
                }
              else
                {
                  char *additional_code_itself;

                  current_position.column_number++;
                  input_char = getch (current_input_stream_state.input_file);
                  while (input_char != EOF)
                    {
                      current_position.column_number++;
                      IR_TOP_ADD_BYTE (input_char);
                      input_char
                        = getch (current_input_stream_state.input_file);
                    }
                  ungetch (input_char, current_input_stream_state.input_file);
                  IR_TOP_ADD_BYTE ('\0');
                  additional_code_itself = IR_TOP_BEGIN ();
                  IR_TOP_FINISH ();
                  current_input_stream_state.additional_c_code_was_returned
                    = TRUE;
                  yylval.node
                    = IR_new_additional_code (current_lexema_position,
                                              additional_code_itself);
                  return ADDITIONAL_C_CODE;
                }
            }
          else if (isalpha (input_char) || input_char == '_' )
            {
              int keyword;
              
              /* Identifier recognition. */
              do
                {
                  current_position.column_number++;
                  IR_TOP_ADD_BYTE (input_char);
                  input_char = getch (current_input_stream_state.input_file);
                }
              while (isalpha (input_char) || isdigit (input_char)
                     || input_char == '_');
              ungetch (input_char, current_input_stream_state.input_file);
              IR_TOP_ADD_BYTE ('\0');
              keyword = find_keyword (IR_TOP_BEGIN (), IR_TOP_LENGTH () - 1);
              if (keyword != 0)
                {
                  IR_TOP_NULLIFY ();
                  return keyword;
                }
              else
                {
                  error (FALSE, current_lexema_position,
                         "unknown keyword `%%%s'", IR_TOP_BEGIN ());
                  IR_TOP_NULLIFY ();
                }
            }
          else
            {
              number_of_successive_error_characters++;
              if (number_of_successive_error_characters == 1)
                error (FALSE, current_lexema_position,
                       "invalid input character '%%'");
              ungetch (input_char, current_input_stream_state.input_file);
            }
          break;
        default:
          if (isalpha (input_char) || input_char == '_' )
            {
              char *identifier_itself;

              current_lexema_position = current_position;
              /* Identifier recognition. */
              do
                {
                  current_position.column_number++;
                  IR_TOP_ADD_BYTE (input_char);
                  input_char = getch (current_input_stream_state.input_file);
                }
              while (isalpha (input_char) || isdigit (input_char)
                     || input_char == '_');
              ungetch (input_char, current_input_stream_state.input_file);
              IR_TOP_ADD_BYTE ('\0');
              identifier_itself = insert_string (IR_TOP_BEGIN ());
              if (identifier_itself == (char *) IR_TOP_BEGIN ())
                IR_TOP_FINISH ();
              else
                IR_TOP_NULLIFY ();
              yylval.node = IR_new_identifier (current_lexema_position,
                                               identifier_itself);
              return IDENTIFIER;
            }
          else if (isdigit (input_char))
            {
              int value;

              current_lexema_position = current_position;
              value = 0;
              /* Number recognition. */
              do
                {
                  current_position.column_number++;
                  value = value * 10 + (input_char - '0');
                  input_char = getch (current_input_stream_state.input_file);
                }
              while (isdigit (input_char));
              ungetch (input_char, current_input_stream_state.input_file);
              yylval.number = value;
              return NUMBER;
            }
          else
            {
              number_of_successive_error_characters++;
              if (number_of_successive_error_characters == 1)
                {
                  if (isprint (input_char))
                    error (FALSE, current_position,
                           "invalid input character '%c'", input_char);
                  else
                    error (FALSE, current_position, "invalid input character");
                }
              current_position.column_number++;
            }
          break;
        }
    }
}

/* The function initiates current input stream and scanner state as
   undefined.  The function must be called only once before any work
   with the scanner.  */

static void
initiate_scanner (void)
{
  current_input_stream_state.input_file = NULL;
  current_input_stream_state.it_is_first_percents = TRUE;
  current_input_stream_state.additional_c_code_was_returned = FALSE;
}

/* The function opens file corresponding to new input stream, and sets
   up new current input stream and scanner state.  The function
   reports error if the new file is not opened.  */

static void
start_scanner_file (const char *new_file_name)
{
  current_input_stream_state.input_file = fopen (new_file_name, "rb");
  if (current_input_stream_state.input_file == NULL)
    system_error (TRUE, no_position, "fatal error -- `%s': ", new_file_name);
  start_file_position (new_file_name);
  previous_char = EMPTY_PREVIOUS_CHAR;
  current_input_stream_state.it_is_first_percents = TRUE;
  current_input_stream_state.additional_c_code_was_returned = FALSE;
}

/* The function closes current input file if current input stream
   state is defined.  Only call of function `initiate_scanner' or
   `finish_scanner' is possible immediately after this function call.  */

static void
finish_scanner (void)
{
  if (current_input_stream_state.input_file != NULL)
    {
      fclose (current_input_stream_state.input_file);
      current_input_stream_state.input_file = NULL;
      finish_file_position ();
    }
}



/*   This page contains abstract data `syntax_errors'.  This abstract
   data is used to error reporting.
     YACC has unsatisfactory syntactic error reporting.  Essentially
   YACC generates only one syntactic error "syntax error".  Abstract
   data `syntax_errors' for solution of this problem is suggested
   here.  This abstract data saves only one the most recent error
   message and its position.  Function `yyerror' used only by function
   `yyparse' flushes saved early error message and saves message given
   as parameter (usually this message is simply "syntax error" but may
   be "stack overflow" or others) and its position.  More detail
   diagnostic message is generated by the call of function
   `syntax_error' within action after special token `error'.  This
   function only rewrites saved early error message (not error
   position).  As consequence the message of last call `syntax_error'
   or the message of function `yyerror' (if such call of
   `syntax_error' does not exist) is fixed within one recover mode
   interval because function `yyerror' is called by `yyparse' only in
   non recover mode.
     This solution permits to create brief syntactic message which
   contain partially information about recovery location (error line
   and position is always in non-fatal errors and warnings -- see
   package for `errors').  */


/* This variable saves the most recent syntax error message or NULL (if
   such syntax error was not fixed). */

static const char *last_syntax_error_message;

/* This variable saves the first syntax error position. */

static position_t first_syntax_error_position;

/* The function initiates the abstract data as without saved syntax
   error message.  The function must be called only once before any
   work with the abstract data.  */

static void
initiate_syntax_errors (void)
{
  last_syntax_error_message = NULL;
}

/* The function flushes saved syntax error message, if any.  */

static void
flush_syntax_error (void)
{
  if (last_syntax_error_message != NULL)
    {
      error (FALSE, first_syntax_error_position,
             "%s", last_syntax_error_message);
      last_syntax_error_message = NULL;
    }
}

/* The function is called by `yyparse' on syntax error.  This function
   flushes saved syntax error message with the aid of
   `flush_syntax_error' and saves message given as parameter and
   `current_lexema position' as its position (see scanner). */

int
yyerror (const char *message)
{
  flush_syntax_error ();
  last_syntax_error_message = message;
  first_syntax_error_position = current_lexema_position;
  return 0;
}

/* The function rewrites saved syntax error message but does not
   change saved position.  */

static void
syntax_error (const char *s)
{
  last_syntax_error_message = s;
}

/* The function finishes work with the abstract data (flushes saved
   syntax error message with the aid of function
   `flush_syntax_error').  */

static void
finish_syntax_errors (void)
{
  flush_syntax_error ();
}



/* This page contains all external functions of parser. */

/* The function initiates all internal abstract data (the scanner and
   `syntax_errors').  The function must be called only once before any
   work with the abstract data. */

void
initiate_parser (void)
{
  initiate_scanner ();
  initiate_syntax_errors ();
}

/* The function forces to tune parser onto file with given name (calls
   function `start_scanner_file').  The function must be called only
   once after parser initiation.  */

void
start_parser_file (const char *file_name)
{
  start_scanner_file (file_name);
}

/* This function finishes all internal abstract data (the scanner and
   abstract data `syntax_errors').  Only call of function
   `initiate_parser' or `finish_parser' is possible immediately after
   this function call. */

void
finish_parser (void)
{
  finish_scanner ();
  finish_syntax_errors ();
}

/*
Local Variables:
mode:c
End:
*/
