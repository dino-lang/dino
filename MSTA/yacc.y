/*
   Copyright (C) 1997-2005 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of the tool MSTA.

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

/* Grammar for the input to MSTA (syntax description translator). */


%{

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "position.h"
#include "errors.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
#include "yacc.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif
#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif
#ifndef SCHAR_MAX
#define SCHAR_MAX 127
#endif
#ifndef SCHAR_MIN
#define SCHAR_MIN (-128)
#endif
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif
#ifndef SHRT_MAX
#define SHRT_MAX 32767
#endif  
#ifndef SHRT_MIN
#define SHRT_MIN (-32768)
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


/* The following variables and forward definitions of functions are
   used in actions of YACC grammar. */

/* The following variable value is begin position of the last lexema returned
   by the function `yylex'. */

static position_t current_lexema_position;

static IR_node_t form_new_sequence (IR_node_t cyclic_sequence,
                                    IR_node_t last_code_insertion_atom);

static void syntax_error (const char *s);

%}


/* Attributes of all YACC symbols (token and nonterminals) will be
   `node'.  Some construction YACC action will be `position'. */

%union
{
  IR_node_t node;
  position_t position;
  struct
    {
      IR_node_t code_insertion;
      IR_node_t precedence_identifier_or_literal;
      IR_node_t look_ahead_number; 
    } precedence_and_code_insertion_and_la;
}


/* The following are recognized by the lexical analyzer.  The
   attributes of the following tokens are defined and used. */

%token <node> IDENTIFIER_OR_LITERAL /* identifier (including '.')
                                       and literal */
%token <node> C_IDENTIFIER          /* identifier followed by a : */
%token <node> NUMBER                /* [0-9][0-9]* */
%token <node> STRING                /* "..." of C syntax */
%token <node> CODE_INSERTION        /* { ... } */
%token <node> YACC_CODE_INSERTION   /* %{ ... %} */
%token <node> ADDITIONAL_C_CODE     /* code after second %% */

/* Reserved words : %type=>TYPE %left=>LEFT, etc.  The attributes of
   the following tokens are not defined and not used. */

%token   LEFT RIGHT NONASSOC TOKEN PREC TYPE START
         UNION LOCAL IMPORT EXPORT SCANNER EXPECT CP LA
%token   PERCENTS             /* the %% mark */
%token   SEMICOLON            /* ; */
%token   BAR                  /* | */
%token   SLASH                /* / */
%token   STAR                 /* * */
%token   PLUS                 /* + */
%token   LESS                 /* < */
%token   GREATER              /* > */
%token   LEFT_PARENTHESIS     /* ) */
%token   RIGHT_PARENTHESIS    /* ( */
%token   LEFT_SQUARE_BRACKET  /* [ */
%token   RIGHT_SQUARE_BRACKET /* ] */
%token   AT                   /* @ */
%token   RANGE                /* - */
%token   RANGE_NO_LEFT_BOUND  /* <- */
%token   RANGE_NO_RIGHT_BOUND /* -> */
%token   RANGE_NO_LEFT_RIGHT_BOUNDS  /* <-> */


%type <node>  description tail definitions definition
%type <node>  symbol_list_start tag symbol_list symbol
%type <node>  rules rule pattern alternatives alternative
%type <node>  sequence sequence_element nonamed_sequence_element unit

%type <position> fix_position

%type <precedence_and_code_insertion_and_la>  prec_la

%type <position> code_insertion_semicolon_list

/*       8-bit character literals stand for themselves; */
/*       tokens have to be defined for multibyte characters */

%start   description

%%

description  : definitions PERCENTS rules tail
                 {
                   IR_node_t first_definition;
                   IR_node_t first_rule;
                   position_t position;

                   /* Transform cyclic definition list to usual list. */
                   if ($1 != NULL)
                     {
                       first_definition = IR_next_definition ($1);
                       IR_set_next_definition ($1, NULL);
                       $1 = first_definition;
                     }
                   /* Transform cyclic rule list to usual list. */
                   if ($3 != NULL)
                     {
                       first_rule = IR_next_rule ($3);
                       IR_set_next_rule ($3, NULL);
                       $3 = first_rule;
                     }
                   position.column_number = 1;
                   position.line_number = 1;
                   description = IR_new_description (position, $1, $3, $4);
                 }
             ;

tail  : /* empty */          {$$ = NULL;}
      | ADDITIONAL_C_CODE    {$$ = $1;}
      ;

definitions  : /* empty */   {$$ = NULL;}
             | definitions  definition  definition_tail
                 {
                   if ($1 == NULL)
                     /* Make cycle. */
                     IR_set_next_definition ($2, $2);
                   else
                     {
                       /* Add element to cyclic list */
                       IR_set_next_definition ($2, IR_next_definition ($1));
                       IR_set_next_definition ($1, $2);
                     }
                   $$ = $2;
                 }
             | definitions  error
                 {
                   syntax_error ("syntax error in declaration");
                   $$ = $1;  /* Ignore error declaration. */
                 }
             ;

definition_tail :
                | definition_semicolon_list
                ;

definition_semicolon_list
  : SEMICOLON
    {
      if (yacc_input_flag)
        {
          if (strict_flag)
            error (FALSE, current_lexema_position,
                   "POSIX YACC prohibits ';' after a definition");
          else if (!w_flag)
            warning
              (current_lexema_position,
               "warning: POSIX YACC prohibits ';' after a definition");
        }
    }
  | definition_semicolon_list SEMICOLON
  ;

definition   : START
                 {
                   $<position>$ = current_lexema_position;
                 }
               IDENTIFIER_OR_LITERAL
                 {
                   $$ = IR_new_start_definition ($<position>2, NULL, $3);
                 }
             | UNION  {$<position>$ = current_lexema_position;} CODE_INSERTION
                 {
                   $$ = IR_new_union_code ($<position>2, NULL, $3);
                 }
             | YACC_CODE_INSERTION
                 {
                   $$ = IR_new_yacc_code (current_lexema_position, NULL, $1);
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
             | SCANNER
                 {
                   $$ = IR_new_scanner_definition
                          (current_lexema_position, NULL);
                 }
             | EXPECT {$<position>$ = current_lexema_position;} NUMBER
                 {
                   $$ = IR_new_expect_definition ($<position>2, NULL, $3);
                 }
             | symbol_list_start tag symbol_list
                 {
                   IR_node_t first_symbol;

                   /* Transform cyclic symbol list to usual list. */
                   if ($3 != NULL)
                     {
                       first_symbol = IR_next_symbol ($3);
                       IR_set_next_symbol ($3, NULL);
                       $3 = first_symbol;
                     }
                   IR_set_symbol_list ($1, $3);
                   IR_set_tag ($1, $2);
                   $$ = $1;
                 }
             ;

symbol_list_start : TOKEN
                      {
                        $$ = IR_new_token_definition (current_lexema_position,
                                                      NULL, NULL, NULL);
                      }
                  | LEFT
                      {
                        $$ = IR_new_left_definition (current_lexema_position,
                                                     NULL, NULL, NULL);
                      }
                  | RIGHT
                      {
                        $$ = IR_new_right_definition (current_lexema_position,
                                                      NULL, NULL, NULL);
                      }
                  | NONASSOC
                      {
                        $$
                          = IR_new_nonassoc_definition
                            (current_lexema_position, NULL, NULL, NULL);
                      }
                  | TYPE
                      {
                        $$ = IR_new_type_definition (current_lexema_position,
                                                     NULL, NULL, NULL);
                      }
                  ;

tag : /* empty */                           {$$ = NULL;}
    | LESS  IDENTIFIER_OR_LITERAL  GREATER  {$$ = $2;}
    ;

symbol_list : symbol
                {
                  /* Make cycle. */
                  IR_set_next_symbol ($1, $1);
                  $$ = $1;
                }
            | symbol_list  symbol
                {
                  /* Add element to cyclic list */
                  IR_set_next_symbol ($2, IR_next_symbol ($1));
                  IR_set_next_symbol ($1, $2);
                  $$ = $2;
                }
            ;

symbol  : IDENTIFIER_OR_LITERAL
            {
              $$ = IR_new_symbol (IR_position ($1), $1, NULL, NULL);
            }
        | IDENTIFIER_OR_LITERAL  NUMBER
            {
              $$ = IR_new_symbol (IR_position ($1), $1, $2, NULL);
            }
        ;

/* Rule section */

rules : rule  semicolons
          {
            if ($1 != NULL)
              /* NULL is possible because of syntax errors -- Make cycle. */
              IR_set_next_rule ($1, $1);
            $$ = $1;
          }
      | rules  rule  semicolons
          {
            if ($2 == NULL)
              $$ = $1;
            else
              {
                if ($1 == NULL)
                  /* It is possible because of syntax errors -- Make cycle. */
                  IR_set_next_rule ($2, $2);
                else
                  {
                    /* Add element to cyclic list */
                    IR_set_next_rule ($2, IR_next_rule ($1));
                    IR_set_next_rule ($1, $2);
                  }
                $$ = $2;
              }
          }
      ;

rule : C_IDENTIFIER {$<position>$ = current_lexema_position;} pattern
         {
           $$ = IR_new_rule ($<position>2, $1, $3, NULL);
         }
     | error
         {
           syntax_error ("syntax error in rule");
           $$ = NULL;  /* Ignore error rule. */
         }
     ;

pattern : alternatives
            {
              IR_node_t first_alternative;
  
              /* Transform cyclic alternative list to usual list. */
              assert ($1 != NULL);
              first_alternative = IR_next_alternative ($1);
              IR_set_next_alternative ($1, NULL);
              $1 = first_alternative;
              $$ = IR_new_pattern (IR_position ($1), $1);
            }
        ;

alternatives : alternatives  BAR  alternative
                 {
                   /* Add element to cyclic list */
                   $$ = $3;
                   IR_set_next_alternative ($$, IR_next_alternative ($1));
                   IR_set_next_alternative ($1, $$);
                 }
             | alternative
                 {
                   /* Make cyclic list. */
                   $$ = $1;
                   IR_set_next_alternative ($$, $$);
                 }
             ;

alternative : fix_position sequence prec_la
                {
                  $$ = IR_new_sequence ($1, NULL,
                                        form_new_sequence ($2,
                                                           $3.code_insertion),
                                        $3.precedence_identifier_or_literal,
					$3.look_ahead_number);
                }
            | fix_position sequence prec_la SLASH
                {
                  $<position>$ = current_lexema_position;
                }
              sequence prec_la
                {
                  $$
                    = IR_new_separator_iteration
                      ($<position>5, NULL,
                       form_new_sequence ($2, $3.code_insertion),
                       $3.precedence_identifier_or_literal,
		       $3.look_ahead_number,
                       form_new_sequence ($6, $7.code_insertion),
                       $7.precedence_identifier_or_literal,
		       $7.look_ahead_number);
                }
            ;

fix_position : {$$ = current_lexema_position;}
;

sequence : /* empty */                     {$$ = NULL;}
         | sequence  sequence_element
             {
               if ($1 == NULL)
                 /* It is possible because of syntax errors -- Make cycle. */
                 IR_set_next_sequence_element ($2, $2);
               else
                 {
                   /* Add element to cyclic list */
                   IR_set_next_sequence_element
                     ($2, IR_next_sequence_element ($1));
                   IR_set_next_sequence_element ($1, $2);
                 }
               $$ = $2;
             }
         ;

sequence_element : CP
                      {
                        $$ = IR_new_control_point (current_lexema_position,
                                                   NULL, NULL);
                      }
                 | nonamed_sequence_element                          {$$ = $1;}
                 | nonamed_sequence_element AT IDENTIFIER_OR_LITERAL
                      {
                        IR_set_sequence_element_identifier ($1, $3);
                        $$ = $1;
                      }
                 ;

nonamed_sequence_element
   : LEFT_SQUARE_BRACKET
       {
         $<position>$ = current_lexema_position;
       }
     pattern  RIGHT_SQUARE_BRACKET
       {
         $$ = IR_new_default ($<position>2, NULL,
                              NULL, $3);
       }
   | unit  STAR
       {
         $$
           = IR_new_star_iteration
             (current_lexema_position, NULL, NULL, $1);
       }
   | unit  PLUS
       {
         $$
           = IR_new_plus_iteration
             (current_lexema_position, NULL, NULL, $1);
       }
   | CODE_INSERTION  code_insertion_tail
       {
         $$
           = IR_new_code_insertion_atom
             (IR_position ($1), NULL, NULL, $1);
       }
   | unit  {$$ = $1;}
   ;

code_insertion_tail
   :
   | code_insertion_semicolon_list
       {
         if (yychar != C_IDENTIFIER
             && yychar != ADDITIONAL_C_CODE
             && yychar != 0 && yacc_input_flag)
           {
             if (strict_flag)
               error (FALSE, $1,
                      "POSIX YACC prohibits ';' after a code insertion");
             else if (!w_flag)
               warning
                 ($1,
                  "warning: POSIX YACC prohibits ';' after a code insertion");
           }
       }
   ;

code_insertion_semicolon_list
  : SEMICOLON                               {$$ = current_lexema_position;}
  | code_insertion_semicolon_list SEMICOLON {$$ = $1;}
  ;

unit : LEFT_PARENTHESIS
         {
           $<position>$ = current_lexema_position;
         }
       pattern  RIGHT_PARENTHESIS
         {
           $$ = IR_new_group ($<position>2, NULL, NULL, $3);
         }
     | IDENTIFIER_OR_LITERAL
         {
           $$ = IR_new_identifier_or_literal_atom (IR_position ($1),
                                                   NULL, NULL, $1);
         }
     | STRING
         {
           $$ = IR_new_string_atom (IR_position ($1), NULL, NULL, $1);
         }
     | IDENTIFIER_OR_LITERAL  RANGE
         {
           $<position>$ = current_lexema_position;
         }
       IDENTIFIER_OR_LITERAL
         {
           $$ = IR_new_range_atom ($<position>3, NULL, NULL, $1, $4);
         }
     | IDENTIFIER_OR_LITERAL  RANGE_NO_LEFT_BOUND
         {
           $<position>$ = current_lexema_position;
         }
       IDENTIFIER_OR_LITERAL
         {
           $$ = IR_new_range_no_left_bound_atom
                ($<position>3, NULL, NULL, $1, $4);
         }
     | IDENTIFIER_OR_LITERAL  RANGE_NO_RIGHT_BOUND
         {
           $<position>$ = current_lexema_position;
         }
       IDENTIFIER_OR_LITERAL
         {
           $$ = IR_new_range_no_right_bound_atom
                ($<position>3, NULL, NULL, $1, $4);
         }
     | IDENTIFIER_OR_LITERAL  RANGE_NO_LEFT_RIGHT_BOUNDS
         {
           $<position>$ = current_lexema_position;
         }
       IDENTIFIER_OR_LITERAL
         {
           $$ = IR_new_range_no_left_right_bounds_atom
                ($<position>3, NULL, NULL, $1, $4);
         }
     ;

prec_la : /* empty */
            {
              $$.code_insertion = NULL;
              $$.precedence_identifier_or_literal = NULL;
              $$.look_ahead_number = NULL;
            }
        | PREC  IDENTIFIER_OR_LITERAL
            {
              $$.code_insertion = NULL;
              $$.precedence_identifier_or_literal = $2;
              $$.look_ahead_number = NULL;
            }
        | PREC  IDENTIFIER_OR_LITERAL  CODE_INSERTION  code_insertion_tail
            {
              $$.code_insertion = IR_new_code_insertion_atom (IR_position ($3),
                                                              NULL, NULL, $3);
              $$.precedence_identifier_or_literal = $2;
              $$.look_ahead_number = NULL;
            }
        | LA NUMBER
            {
              $$.code_insertion = NULL;
              $$.precedence_identifier_or_literal = NULL;
              $$.look_ahead_number = $2;
	      if (max_look_ahead_number < IR_number_value ($2))
		max_look_ahead_number = IR_number_value ($2);
            }
        | PREC  IDENTIFIER_OR_LITERAL   LA NUMBER
            {
              $$.code_insertion = NULL;
              $$.precedence_identifier_or_literal = $2;
              $$.look_ahead_number = $4;
	      if (max_look_ahead_number < IR_number_value ($4))
		max_look_ahead_number = IR_number_value ($4);
            }
        | LA NUMBER   PREC  IDENTIFIER_OR_LITERAL 
            {
              $$.code_insertion = NULL;
              $$.precedence_identifier_or_literal = $4;
              $$.look_ahead_number = $2;
	      if (max_look_ahead_number < IR_number_value ($2))
		max_look_ahead_number = IR_number_value ($2);
            }
        | LA NUMBER  CODE_INSERTION code_insertion_tail
            {
              $$.code_insertion = IR_new_code_insertion_atom (IR_position ($3),
                                                              NULL, NULL, $3);
              $$.precedence_identifier_or_literal = NULL;
              $$.look_ahead_number = $2;
	      if (max_look_ahead_number < IR_number_value ($2))
		max_look_ahead_number = IR_number_value ($2);
            }
        | PREC IDENTIFIER_OR_LITERAL  LA NUMBER
               CODE_INSERTION code_insertion_tail
            {
              $$.code_insertion = IR_new_code_insertion_atom (IR_position ($5),
                                                              NULL, NULL, $5);
              $$.precedence_identifier_or_literal = $2;
              $$.look_ahead_number = $4;
	      if (max_look_ahead_number < IR_number_value ($4))
		max_look_ahead_number = IR_number_value ($4);
            }
        | LA NUMBER  PREC IDENTIFIER_OR_LITERAL 
               CODE_INSERTION code_insertion_tail
            {
              $$.code_insertion = IR_new_code_insertion_atom (IR_position ($5),
                                                              NULL, NULL, $5);
              $$.precedence_identifier_or_literal = $4;
              $$.look_ahead_number = $2;
	      if (max_look_ahead_number < IR_number_value ($2))
		max_look_ahead_number = IR_number_value ($2);
            }
        ;

semicolons : /* empty */
           | semicolons  SEMICOLON
           ;


%%

static IR_node_t
form_new_sequence (IR_node_t cyclic_sequence,
                   IR_node_t last_code_insertion_atom)
{
  IR_node_t first_sequence_element;

  if (last_code_insertion_atom != NULL)
    {
      if (cyclic_sequence == NULL)
        /* It is possible because of syntax errors -- Make cycle. */
        IR_set_next_sequence_element (last_code_insertion_atom,
                                      last_code_insertion_atom);
      else
        {
          /* Add element to cyclic sequence */
          IR_set_next_sequence_element
            (last_code_insertion_atom,
             IR_next_sequence_element (cyclic_sequence));
          IR_set_next_sequence_element (cyclic_sequence,
                                        last_code_insertion_atom);
        }
      cyclic_sequence = last_code_insertion_atom;
    }
  /* Transform cyclic sequence to usual list. */
  if (cyclic_sequence != NULL)
    {
      first_sequence_element = IR_next_sequence_element (cyclic_sequence);
      IR_set_next_sequence_element (cyclic_sequence, NULL);
      cyclic_sequence = first_sequence_element;
    }
  return cyclic_sequence;
}



/*   This page contains abstract data `scanner'.  This abstract data
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
     returned by the scanner or FALSE otherwise. */
  int additional_c_code_was_returned;
};

/* The following structure contains current input stream and
   scanner state.  If the member `input_file' value is NULL
   then input stream is not defined. */

static struct input_stream_state current_input_stream_state;

static int
find_keyword (const char *str, int length)
{
  switch (length)
    {
    case 2:
      if (*str == 'c')
        return (strcmp (str, "cp") == 0 ? CP : 0);
      else
        return (strcmp (str, "la") == 0 ? LA : 0);
    case 4:
      switch (*str)
        {
        case 'l':
          return (strcmp (str, "left") == 0 ? LEFT : 0);
        case 'p':
          return (strcmp (str, "prec") == 0 ? PREC : 0);
        case 't':
          return (strcmp (str, "type") == 0 ? TYPE : 0);
        default:
          return 0;
        }
    case 5:
      switch (*str)
        {
        case 'r':
          return (strcmp (str, "right") == 0 ? RIGHT : 0);
        case 't':
          return (strcmp (str, "token") == 0 ? TOKEN : 0);
        case 's':
          return (strcmp (str, "start") == 0 ? START : 0);
        case 'u':
          return (strcmp (str, "union") == 0 ? UNION : 0);
        case 'l':
          return (strcmp (str, "local") == 0 ? LOCAL : 0);
        default:
          return 0;
        }
    case 6:
      switch (*str)
        {
        case 'i':
          return (strcmp (str, "import") == 0 ? IMPORT : 0);
        case 'e':
          if (str [3] == 'o')
            return (strcmp (str, "export") == 0 ? EXPORT : 0);
          else
            return (strcmp (str, "expect") == 0 ? EXPECT : 0);
        default:
          return 0;
        }
    case 7:
      return (strcmp (str, "scanner") == 0 ? SCANNER : 0);
    case 8:
      return (strcmp (str, "nonassoc") == 0 ? NONASSOC : 0);
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

static int
C_character_value (const char *character_representation)
{
  int correct_new_line;
  int character_code;

  if (*character_representation != '\''
      || get_string_code (character_representation + 1,
                          &character_code, &correct_new_line) [1] != '\''
      || correct_new_line)
    character_code = -1;
  return character_code;
}

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

static void
process_code_insertion (int yacc_code_insertion_flag)
{
  int input_char;
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
            ungetch (input_char, current_input_stream_state.input_file);
          break;
        case '{':
          current_position.column_number++;
          if (!yacc_code_insertion_flag)
            bracket_level++;
          break;
        case '}':
          current_position.column_number++;
          if (!yacc_code_insertion_flag)
            {
              bracket_level--;
              if (bracket_level == 0)
                IR_TOP_SHORTEN (1);
            }
          break;
        case '%':
          current_position.column_number++;
          if (yacc_code_insertion_flag)
            {
              input_char = getch (current_input_stream_state.input_file);
              if (input_char == '}')
                {
                  current_position.column_number++;
                  IR_TOP_SHORTEN (1);
                  bracket_level = 0; /* To exit from the loop */
                }
              else
                ungetch (input_char, current_input_stream_state.input_file);
            }
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
          ungetch (input_char, current_input_stream_state.input_file);
          error_flag = TRUE;
          break;
        default:
          current_position.column_number++;
          break;
        }
    }
  if (error_flag)
    error (FALSE, current_lexema_position, "C code insertion end is absent");
  IR_TOP_ADD_BYTE ('\0');
  code_itself = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  yylval.node = IR_new_code_insertion (current_lexema_position, code_itself);
}

static int
skip_blanks (int *number_of_successive_error_characters)
{
  int input_char;
  
  for (*number_of_successive_error_characters = 0;;)
    {
      input_char = getch (current_input_stream_state.input_file);
      /* `current_position' corresponds `input_char' here */
      switch (input_char)
        {
        case '\f':
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
        case '/':
          current_lexema_position = current_position;
          current_position.column_number++;
          input_char = getch (current_input_stream_state.input_file);
          if (input_char != '*')
            {
              ungetch (input_char, current_input_stream_state.input_file);
              return '/';
            }
          else if (!skip_commentary (FALSE))
            error (FALSE, current_lexema_position, "commentary end is absent");
          break;
        default:
          return input_char;
        }
    }
}

int
yylex (void)
{
  int input_char;
  int number_of_successive_error_characters;
  
  for (;;)
    {
      input_char = skip_blanks (&number_of_successive_error_characters);
      /* `current_position' corresponds `input_char' here */
      switch (input_char)
        {
        case EOF:
          current_lexema_position = current_position;
          return 0;
        case ';':
          current_lexema_position = current_position;
          current_position.column_number++;
          return SEMICOLON;
        case '|':
          current_lexema_position = current_position;
          current_position.column_number++;
          return BAR;
        case '/':
          current_lexema_position = current_position;
          current_position.column_number++;
          return SLASH;
        case '*':
          current_lexema_position = current_position;
          current_position.column_number++;
          return STAR;
        case '+':
          current_lexema_position = current_position;
          current_position.column_number++;
          return PLUS;
        case '[':
          current_lexema_position = current_position;
          current_position.column_number++;
          return LEFT_SQUARE_BRACKET;
        case ']':
          current_lexema_position = current_position;
          current_position.column_number++;
          return RIGHT_SQUARE_BRACKET;
        case '<':
          current_lexema_position = current_position;
          current_position.column_number++;
          input_char = getch (current_input_stream_state.input_file);
          if (input_char == '-')
            {
              current_position.column_number++;
              input_char = getch (current_input_stream_state.input_file);
              if (input_char == '>')
                {
                  current_position.column_number++;
                  return RANGE_NO_LEFT_RIGHT_BOUNDS;
                }
              else
                {
                  ungetch (input_char, current_input_stream_state.input_file);
                  return RANGE_NO_LEFT_BOUND;
                }
            }
          else
            {
              ungetch (input_char, current_input_stream_state.input_file);
              return LESS;
            }
          break;
        case '>':
          current_lexema_position = current_position;
          current_position.column_number++;
          return GREATER;
        case '(':
          current_lexema_position = current_position;
          current_position.column_number++;
          return LEFT_PARENTHESIS;
        case ')':
          current_lexema_position = current_position;
          current_position.column_number++;
          return RIGHT_PARENTHESIS;
        case '@':
          current_lexema_position = current_position;
          current_position.column_number++;
          return AT;
        case '-':
          current_lexema_position = current_position;
          current_position.column_number++;
          input_char = getch (current_input_stream_state.input_file);
          if (input_char == '>')
            {
              current_position.column_number++;
              return RANGE_NO_RIGHT_BOUND;
            }
          else
            {
              ungetch (input_char, current_input_stream_state.input_file);
              return RANGE;
            }
          break;
        case '\'':
          {
            char *character_representation;

            current_lexema_position = current_position;
            current_position.column_number++;
            IR_TOP_ADD_BYTE (input_char); /* Add ' */
            skip_C_character ();
            IR_TOP_ADD_BYTE (0);
            character_representation = insert_string (IR_TOP_BEGIN ());
            if (character_representation == (char *) IR_TOP_BEGIN ())
              IR_TOP_FINISH ();
            else
              IR_TOP_NULLIFY ();
            yylval.node
              = IR_new_literal (current_lexema_position,
                                character_representation,
                                C_character_value (character_representation));
            if  (IR_literal_code (yylval.node) < 0)
              IR_set_literal_code (yylval.node, 0);
            return IDENTIFIER_OR_LITERAL;
          }
          break;
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
          process_code_insertion (FALSE);
          return CODE_INSERTION;
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
          else if (isalpha (input_char) || input_char == '_')
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
                         "unknown keyword `%%%s'",
                         IR_TOP_BEGIN ());
                  IR_TOP_NULLIFY ();
                }
            }
          else if (input_char == '{')
            {
              process_code_insertion (TRUE);
              return YACC_CODE_INSERTION;
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
          if (isalpha (input_char) || input_char == '_' || input_char == '.')
            {
              char *identifier_itself;
              int dot_presence_flag;

              current_lexema_position = current_position;
              /* Identifier recognition. */
              dot_presence_flag = FALSE;
              do
                {
                  if (input_char == '.')
                    dot_presence_flag = TRUE;
                  current_position.column_number++;
                  IR_TOP_ADD_BYTE (input_char);
                  input_char = getch (current_input_stream_state.input_file);
                }
              while (isalpha (input_char) || isdigit (input_char)
                     || input_char == '_' || input_char == '.');
              ungetch (input_char, current_input_stream_state.input_file);
              IR_TOP_ADD_BYTE ('\0');
              identifier_itself = insert_string (IR_TOP_BEGIN ());
              if (identifier_itself == (char *) IR_TOP_BEGIN ())
                IR_TOP_FINISH ();
              else
                IR_TOP_NULLIFY ();
              yylval.node
                = IR_new_identifier (current_lexema_position,
                                     identifier_itself, dot_presence_flag);
              input_char
                = skip_blanks (&number_of_successive_error_characters);
              if (input_char == ':')
                {
                  current_position.column_number++;
                  return C_IDENTIFIER;
                }
              else
                {
                  ungetch (input_char, current_input_stream_state.input_file);
                  return IDENTIFIER_OR_LITERAL;
                }
            }
          else if (isdigit (input_char))
            {
              int number_value;
              int overflow_flag;
              position_t number_position;

              number_position = current_position;
              overflow_flag = FALSE;
              number_value = 0;
              do
                {
                  /* `current_position' corresponds to `input_char' here. */
                  current_position.column_number++;
                  if (number_value > (INT_MAX - (input_char - '0')) / 10)
                    {
                      if (overflow_flag)
                        error (FALSE, number_position,
                               "number value is too big");
                      overflow_flag = TRUE;
                      number_value = INT_MAX;
                    }
                  else
                    number_value = number_value * 10 + (input_char - '0');
                  input_char = getch (current_input_stream_state.input_file);
                  /* `current_position' corresponds to `input_char' here. */
                }
              while (isdigit (input_char));
              ungetch (input_char, current_input_stream_state.input_file);
              yylval.node = IR_new_number (number_position, number_value);
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

static void
initiate_scanner (void)
{
  current_input_stream_state.input_file = NULL;
  current_input_stream_state.it_is_first_percents = TRUE;
  current_input_stream_state.additional_c_code_was_returned = FALSE;
}

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

/* The function can be called two and more times (after the last call
   of `initiate_scanner'. */
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


static void
initiate_syntax_errors (void)
{
  last_syntax_error_message = NULL;
}


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


yyerror (const char *message)
{
  flush_syntax_error ();
  last_syntax_error_message = message;
  first_syntax_error_position = current_lexema_position;
  return 0;
}

static void
syntax_error (const char *s)
{
  last_syntax_error_message = s;
}


static void
finish_syntax_errors (void)
{
  flush_syntax_error ();
}



/* This page contains all external functions of parser. */

void
initiate_parser (void)
{
  initiate_scanner ();
  initiate_syntax_errors ();
}

void
start_parser_file (const char *file_name)
{
  start_scanner_file (file_name);
}

/* The function can be called more two times after the last call of
   `initiate_parser'. */

void
finish_parser (void)
{
  finish_scanner ();
  finish_syntax_errors ();
}
