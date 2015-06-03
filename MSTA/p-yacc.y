/*
   FILE NAME:   p-yacc.y

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

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

   TITLE:       Parser of Pascal cross-compiler described as YACC file

   DESCRIPTION: This file syntactic analysis (with possible syntactic
                error recovery and processing include files) of one or
                more files and builds up internal representation of
                parsed pascal source file.

   SPECIAL CONSIDERATION:
         Defining macro `DEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation may permits to fix
       some internal errors of parser work and errors of its usage.
   
         This file was obtained from Pascal collected syntax (from
       Annex A of standard ISO/IEC 7185:1990).  Following
       transformation were used
         o change of EBNF-constructions by BNF-constructions
         o deletion of unused rules and chain rules
         o deletion of some other rules, e.g. rules defining
           nonterminals which used only once
         o making unambiguous grammar
         o description of expressions by ambiguous rules with conflict
           resolution with the aid of YACC construction describing
           precedence and associativity of operators

      More detailed information about the transformations may be found
    in commentaries in the files.  One detail is worthy of mention.
    Conflict between actual parameter lists of user defined procedure
    and standard procedures READ, WRITE, READLN, WRITELN are resolved
    by adding new terminals WRITE, READ, WRITELN, READLN which will be
    generated scanner after recognizing identifier (and left
    parenthesis) of standard procedures write, read, readln, writeln.
    It will be solved in action of YACC rule.  In this manner feedback
    from the parser to scanner occurs (see scanner).
      Transformation of original Pascal grammar to description on YACC
    may be made by various ways.  Our way is aimed at saving original
    names of syntax constructions and at reasonable minimization of
    nonterminals especially added nonterminals to speed up generated
    parser.  Also left recursion was used because result grammar will
    be input to YACC (bottom-up parser).
  
      All mentioned above grammar transformations are described in
    design document in more details.
  
      Commentary with Pascal standard section is placed before rules
    which are described in given section.
  
      Additional rules are needed to describe Pascal standard
    extensions, error recovery and common semantic actions.  The new
    rules (with new nonterminals at the left side) are marked
    commentaries with `+'.  The word `new' means that such rules and
    nonterminals are absent in standard Pascal description.
  
      All grammar description is divided on two parts.  The first
    describes standard Pascal.  The second describes Pascal extensions.
  
      To process included files (statement `INCLUDE ...') the parser
    could call itself recursively.  But according to POSIX the utility
    YACC does not give such feature.  Therefore the parser does not
    make switching process of other source files.  The scanner executes
    all this work (see scanner specification).  Such implementation
    requires enrichment of syntax which reflects that task data unit
    (with start lexema `INCLUSION' and finish lexema
    `END_OF_INCLUDED_FILE') may be after INCLUDE statement.
  
      YACC has features sufficient for high quality (local) error
    recovery.  Unfortunately this high quality error recovery is
    achieved by adding numerous rules with special token `error' and
    difficulties in modifications of rules.  This disadvantage is a
    consequence of that YACC take into account look ahead tokens after
    only one symbol `error' during skipping tokens which are not the
    look ahead tokens.  For example, rules
  
       expression : '(' expression ')'
                  | '(' error ')'
                  ;
  
    result in skipping all tokens (including such tokens as `END', `;')
    except for `('.  To avoid this additional error rules are needed to
    increase the look ahead token set, e.g.
  
       expression : '(' expression   { here reporting syntax error }
                  ;
    or
       expression : '(' expression END {yychar = END; reporting syntax error;}
                  | '(' expression ';' {yychar = ';'; reporting syntax error;}
                  ;
  
    Another disadvantage is that YACC is not real LR-analyzer.  It is
    LALR-analyzer whose look ahead tokens for reduce may contain
    additional tokens which do not correspond text being parsed.  This
    acts on quality of error recovery when special symbol `error' is
    the last in rule.  Especial precaution is needed to error rules for
    lists.  For example,
  
      conformant_array_schema
        : ARRAY '[' ind_type_specification_list ']' OF conformant_array_schema
        ;
      ind_type_specification_list
        : ind_type_specification
        | error
        | ind_type_specification_list SEMICOLON ind_type_specification
        | ind_type_specification_list error
        ;
  
    What does happen if syntax error will occur after keyword `OF'?
    The parser will return back to recognizing
    `ind_type_specification_list' and discard all tokens except for
    ']'.  Therefore additional rule is needed
  
      conformant_array_schema
        : ARRAY '[' ind_type_specification_list  {fix syntactic error;}
        ;
    or
      conformant_array_schema
        : ARRAY '[' ind_type_specification_list ')' {yychar = ')'; fix error;}
        ;
  
      It is necessary to aim at very detail error recovery especially
    for declaration in order to do not generate spurious semantic
    errors such as `undeclared identifier'.  But thus results in
    enormous additional rules.  Therefore only statement and
    declaration level is used for the cross-compiler, and there are no
    error rules for identifier list and expression.  The main goal of
    our error recovery is to do not skip very large source program
    portions such as variable declaration parts, type definitions parts
    and so on.
  
      To disable influence syntactic errors on fragments of file
    started with unique lexema (e.g. constant section started with
    keyword `CONST' and so on) the following actions with `yyerrok'
    will be placed
  
      no_empty_type_definition_part : TYPE 
                                        {yyerrok;}
                                      type_definition SEMICOLON
                                    | no_empty_type_definition_part
                                         type_definition SEMICOLON
                                    ;
  
      As mentioned above the implementation of processing INCLUDE
    statements by parser require robust error recovery within a task
    data unit, i.e. syntactic errors fixed in included task data unit
    may not affect fixing syntactic errors in other compilation units
    (program or another task data unit).  Such robust error recovery is
    to be implemented by addition of following actions which are off
    influence of any syntactic errors in the previous file on included
    file and influence of syntactic errors in given file on
    continuation of the previous file
  
       task_data_unit :   { yyerrok;}
                          ...
                        END_OF_INCLUDED_FILE
                          { yyerrok;}
  
    Also when function yyerror fixed too many syntactic errors it may
    skip up to end of file of current compilation unit.  But it should
    be remembered that all `error' constructions in rules must have
    token `END_OF_INCLUDED_FILE' as look ahead token set (this is
    achieved by additional error rules).  Otherwise the parser will
    move in cycles.  For example, it is necessary for rule
  
             field_list : error
    to write rule
             record_type : RECORD field_list END_OF_INCLUDED_FILE
  
    Analogous rule must be for `record_type' and so on.
  
      When a parser makes error repair, its internal representation
    always (even when syntactic errors are fixed) corresponds to
    syntactically correct program.  We does not plan to make an error
    repair. Error language construction are to be represented by
    special node `error'.  This representation is formed in description
    on YACC within action of rules with special token `error'.
  
      See commentaries below for abstract data `syntax_errors'
    about used method of syntactic error reporting.

*/

%{

#include <stdlib.h>
#include <stdio.h>
#include "vl-object.h"
#include "positions.h"
#include "errors.h"
#include "p-common.h"
#include "p-options.h"
#include "p-messages.h"
#include "p_fe.h"
#include "p-scanner.h"
#include "p-analyzer.h"
#include "p-front-end.h"
#include "p-yacc.h"

/* The following variable value is full file name of task data which
   was fixed in the last include-statement. */

static const char *full_file_name_in_last_include_statement;

static FE_node_t
  transform_procedure_and_function_declaration_list (FE_node_t list);
static FE_node_t transform_statement_list (FE_node_t list);
static FE_node_t
  add_element_to_procedure_and_function_declaration_list (FE_node_t list,
                                                          FE_node_t element);
static FE_node_t form_field_list (FE_node_t record_section_list,
                                  FE_node_t variant_part);
static FE_node_t transform_formal_parameter_section_list (FE_node_t list);
static FE_node_t transform_ind_type_specification_list (FE_node_t list);
static FE_node_t transform_case_list_element_list (FE_node_t list);
static const char *full_included_file_name  (const char *task_data_name);

static void flush_syntax_error (void);
static void syntax_error (const char *s);
static void finish_parser_file (void);

%}

%union {
  /* The most nonterminals and lexemas have attributes of given type. */
  FE_node_t node;
  /* Some lexemas have attributes of given type. */
  lexema_attribute_t lexema_attribute;
}

/* Symbols classes */
%token <node> IDENTIFIER DIGIT_SEQUENCE UNSIGNED_REAL CHARACTER_STRING

/* Special symbol */
%token <lexema_attribute> PLUS               MINUS         STAR
%token <lexema_attribute> SLASH              EQUAL         LESS
%token <lexema_attribute> GREATER            LEFT_BRACKET  RIGHT_BRACKET
%token <lexema_attribute> PERIOD             COMMA         COLON
%token <lexema_attribute> SEMICOLON          ARROW         LEFT_PARENTHESIS
%token <lexema_attribute> RIGHT_PARENTHESIS  UNEQUAL       LESS_OR_EQUAL
%token <lexema_attribute> GREATER_OR_EQUAL   ASSIGN        RANGE

/* Word symbol */
%token <lexema_attribute> AND_KW       ARRAY_KW   BEGIN_KW
%token <lexema_attribute> CASE_KW      CONST_KW   DIV_KW
%token <lexema_attribute> DO_KW        DOWNTO_KW  ELSE_KW
%token <lexema_attribute> END_KW       FILE_KW    FOR_KW
%token <lexema_attribute> FUNCTION_KW  GOTO_KW    IF_KW
%token <lexema_attribute> IN_KW        LABEL_KW   MOD_KW
%token <lexema_attribute> NIL_KW       NOT_KW     OF_KW
%token <lexema_attribute> OR_KW        PACKED_KW  PROCEDURE_KW
%token <lexema_attribute> PROGRAM_KW   RECORD_KW  REPEAT_KW
%token <lexema_attribute> SET_KW       THEN_KW    TO_KW
%token <lexema_attribute> TYPE_KW      UNTIL_KW   VAR_KW
%token <lexema_attribute> WHILE_KW     WITH_KW

/* Word symbols needed for standard Pascal extensions. */
%token <lexema_attribute> ENTRY_KW  INCLUDE_KW  SCALED_KW  TASKDATA_KW

/* Auxiliary lexemas for resolution of conflicts */
%token WRITE  READ  WRITELN  READLN

/* Auxiliary lexemas for processing included files. */
%token INCLUSION END_OF_INCLUDED_FILE

/* Auxiliary lexema for disable Pascal standard extensions. */
%token STANDARD

/* Precedence and associativity of operators. */
%nonassoc EQUAL  UNEQUAL  LESS  GREATER  LESS_OR_EQUAL  GREATER_OR_EQUAL  IN_KW
%left STAR  SLASH  DIV_KW  MOD_KW  AND_KW
%nonassoc NOT_KW

%type <node>  unsigned_number  block  statement_part  constant_definition_part
%type <node>  no_empty_constant_definition_part  label_declaration_part
%type <node>  no_empty_label_declaration_part
%type <node>  procedure_and_function_declaration_part  type_definition_part
%type <node>  no_empty_type_definition_part  variable_declaration_part
%type <node>  no_empty_variable_declaration_part  constant  constant_definition
%type <node>  type_definition  type_denoter
%type <node>  enumerated_type_constant_list  enumerated_type_constant_sequence
%type <node>  ordinal_type  identifier_list
%type <node>  identifier_sequence  subrange_type
%type <node>  unpacked_structured_type  index_type_list
%type <node>  case_constant_list  case_constant_sequence
%type <node>  field_list  fixed_part  record_section
%type <node>  variant  variant_part
%type <node>  variant_selector  variable_access  variable_declaration
%type <node>  index_expression_list procedure_heading  function_heading
%type <node>  formal_parameter_section_list  formal_parameter_section
%type <node>  conformant_array_schema  ind_type_specification
%type <node>  ind_type_specification_list  expression  simple_expression
%type <node>  term member_designator  member_designator_list
%type <node>  new_actual_parameter_list  actual_parameter_sequence
%type <node>  statement  no_label_statement
%type <node>  statement_sequence  case_list_element  case_list_element_list
%type <node>  variable_access_list  variable_access_sequence
%type <node>  write_parameter  new_write_parameter_list
%type <node>  write_parameter_sequence
%type <node>  program  program_heading  compilation_file  compilation_unit
%type <node>  include_statement_list  entry_procedure_heading
%type <node>  entry_function_heading  task_data  create_task_data_declaration
%type <node>  task_data_heading  subtask_unit
%type <node>  external_procedure_and_function_declaration_part

%start compilation_file

%%

/* The following rule is necessary to avoid conflicts in several
   grammar places. */

/* (+) */

recover_mode_off : {yyerrok;}
                 ;

/* 6.1.5. Attribute of the following nonterminal is a real or integer
   number. */

unsigned_number : DIGIT_SEQUENCE  {$$ = $1;}
                | UNSIGNED_REAL   {$$ = $1;}
                ;

/* 6.2.1.  Attribute of the following nonterminal is a block. */

block :   {
            $<lexema_attribute>$.position = current_position;
            $<lexema_attribute>$.pseudocomment_state = pseudocomment_state;
            IR_start_block_memory ();
          }
        label_declaration_part constant_definition_part
        initiate_types
        type_definition_part  variable_declaration_part
        finish_types
        start_procedures
        procedure_and_function_declaration_part
        finish_procedures
        statement_part
          {
            $$ = FE_new_block
                 ($<lexema_attribute>1.position,
                  $<lexema_attribute>1.pseudocomment_state, $2, $3, $5, $6,
                  transform_procedure_and_function_declaration_list ($9), $11);
            process_block ($$);
            /* The following statement is necessary for possible
               fixing errors and modification of
               `number_of_errors'. */
            flush_syntax_error ();
            if (number_of_errors == 0)
              after_analyzing_block ($$);
            IR_finish_block_memory ();
            $$ = NULL;  /* We already freed block memory. */
          }
      ;

/* (+) */
initiate_types : {initiate_processing_types ();}
               ;

/* (+) */
finish_types : {finish_processing_types ();}
             ;

/* (+) Before any occurrence of the following nonterminal nonterminal
       `variable_declaration_part' must be. */
start_procedures : {start_procedural_part ($<node>-1);}
                 ;

/* (+) */
finish_procedures : {finish_procedural_part ();}
                  ;

/* 6.2.1.  Attribute of the following nonterminal is compound
   statement corresponding to given statement part. */

statement_part :  BEGIN_KW recover_mode_off statement_sequence END_KW
                   {
                     $$ = FE_new_compound_statement
                          ($1.position, $1.pseudocomment_state, NULL, NULL,
                           $4.position, transform_statement_list ($3));
                   }
               ;

/* Attribute of the following nonterminal is list of constant definitions. */

constant_definition_part
  :   {$$ = NULL;}
  | no_empty_constant_definition_part
      {
        /* Transformation of cyclic list to simple list */
        if ($1 != NULL)
          {
            $$ = FE_next_constant_definition ($1);
            FE_set_next_constant_definition ($1, NULL);
          }
        else
          $$ = NULL;
      }
  ;

/* (+) The attribute of the following nonterminal is the last constant
   definition, if any, corresponding to given nonterminal.  The constant
   definitions derived from given nonterminal form cyclic list. */

no_empty_constant_definition_part
  : CONST_KW recover_mode_off constant_definition SEMICOLON
      {
        $$ = $3;
        FE_set_next_constant_definition ($3, $3); /* Make cycle. */
      }

  /* The following rule is necessary to disable skipping `TYPE_KW', `VAR_KW',
     and so on at the begin of `no_empty_constant_definition_part'. */
  | CONST_KW recover_mode_off error
      {
        syntax_error (SERR_constant_definition);
        $$ = NULL; /* Ignore error constant definition. */
      }
  | no_empty_constant_definition_part constant_definition SEMICOLON
      {
#ifdef DEBUG
        if ($2 == NULL)
          abort ();
#endif
        if ($1 == NULL)
          /* It is the first element of the list. */
            FE_set_next_constant_definition ($2, $2); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_constant_definition ($2,
                                             FE_next_constant_definition ($1));
            FE_set_next_constant_definition ($1, $2);
          }
        $$ = $2;
      }

  /* The following rule is necessary to disable skipping `TYPE_KW', `VAR_KW',
     and so on. */
  | no_empty_constant_definition_part error
      {
        syntax_error (SERR_constant_definition);
        $$ = $1; /* Ignore error constant definition. */
      }
  ;

/* The attribute of the following nonterminal is list of digit
   sequences if any, derived from given nonterminal. */

label_declaration_part
  :   {$$ = NULL;}
  | no_empty_label_declaration_part SEMICOLON
      {
        /* Transformation of cyclic list to simple list */
        if ($1 != NULL)
          {
            $$ = FE_next_digit_sequence ($1);
            FE_set_next_digit_sequence ($1, NULL);
          }
        else
          $$ = NULL;
      }

  /* The following rule is necessary to disable skipping `CONST_KW', `TYPE_KW',
     and so on in `no_empty_label_declaration_part'. */
  | no_empty_label_declaration_part
      {
        if (!YYRECOVERING ())
          yyerror (SERR_label_declaration_part);
        else
          syntax_error (SERR_label_declaration_part);
        /* Transformation of cyclic list to simple list */
        if ($1 != NULL)
          {
            $$ = FE_next_digit_sequence ($1);
            FE_set_next_digit_sequence ($1, NULL);
          }
        else
          $$ = NULL;
      }
  ;

/* (+) The attribute of the following nonterminal is last digit
   sequence if any, derived from given nonterminal.  The digit sequences
   derived from given nonterminal form cyclic list. */

no_empty_label_declaration_part
  : LABEL_KW recover_mode_off DIGIT_SEQUENCE
      {
        $$ = $3;
        FE_set_next_digit_sequence ($3, $3); /* Make cycle. */
        $$ = analyze_declaration ($$);
      }

  /* The following rule is necessary to disable skipping `CONST_KW', `TYPE_KW',
     and so on at the begin of `no_empty_label_declaration_part'. */
  | LABEL_KW recover_mode_off error
      {
        syntax_error (SERR_label_declaration_part);
        $$ = NULL; /* Ignore error list element. */
      }
  | no_empty_label_declaration_part COMMA DIGIT_SEQUENCE
      {
        if ($1 == NULL)
          /* It is the first element of the list. */
          FE_set_next_digit_sequence ($3, $3); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_digit_sequence ($3, FE_next_digit_sequence ($1));
            FE_set_next_digit_sequence ($1, $3);
          }
        $$ = $3;
        $$ = analyze_declaration ($$);
      }

  /* The following rule is necessary to disable skipping `COMMA', `SEMICOLON',
     and tokens mentioned in rule for `label_declaration_part'. */
  | no_empty_label_declaration_part error
      {
        syntax_error (SERR_label_declaration_part);
        $$ = $1; /* Ignore error list element. */
      }
  ;

/* The attribute of the following nonterminal is last procedure
   or function declaration (more concretely specification, definition, or
   declaration_itself), if any, derived from given nonterminal.  The
   procedure and function declarations derived from given nonterminal form
   cyclic list. */

procedure_and_function_declaration_part
  :   {$$ = NULL;}
  | procedure_and_function_declaration_part procedure_heading SEMICOLON
      IDENTIFIER SEMICOLON
      {
        $$ = FE_new_procedure_specification
             (FE_position ($2), FE_pseudocomment_state ($2), NULL, $2, $4);
        $$ = analyze_declaration ($$);
        $$ = add_element_to_procedure_and_function_declaration_list ($1, $$);
      }
  | procedure_and_function_declaration_part procedure_heading SEMICOLON
      {
        $<node>$ = FE_new_procedure_declaration_itself
                   (FE_position ($2), FE_pseudocomment_state ($2), NULL,
                    $2, NULL);
        /* The procedure declaration itself may be changed by procedure
           definition in the following call. */
        $<node>$ = analyze_declaration ($<node>$);
      }
      block SEMICOLON
      {
        FE_set_procedure_block ($<node>4, $5);
        $$ = add_element_to_procedure_and_function_declaration_list ($1,
                                                                     $<node>4);
      }
  | procedure_and_function_declaration_part function_heading SEMICOLON
      IDENTIFIER SEMICOLON
      {
        $$ = FE_new_function_specification
             (FE_position ($2), FE_pseudocomment_state ($2), NULL, $2, $4);
        $$ = analyze_declaration ($$);
        $$ = add_element_to_procedure_and_function_declaration_list ($1, $$);
      }
  | procedure_and_function_declaration_part FUNCTION_KW recover_mode_off
      IDENTIFIER SEMICOLON 
      {
        $<node>$ = FE_new_function_definition
                   ($2.position, $2.pseudocomment_state, NULL,
                    FE_new_function_identifier
                    (FE_position ($4), FE_pseudocomment_state ($4), $4), NULL);
        $<node>$ = analyze_declaration ($<node>$);
      }
      block SEMICOLON
      {
        FE_set_function_block ($<node>6, $7);
        $$ = add_element_to_procedure_and_function_declaration_list ($1,
                                                                     $<node>6);
      }
  | procedure_and_function_declaration_part function_heading SEMICOLON
      {
        $<node>$ = FE_new_function_declaration_itself
                   (FE_position ($2), FE_pseudocomment_state ($2), NULL,
                    $2, NULL);
        $<node>$ = analyze_declaration ($<node>$);
      }
      block SEMICOLON
      {
        FE_set_function_block ($<node>4, $5);
        $$ = add_element_to_procedure_and_function_declaration_list ($1,
                                                                     $<node>4);
      }

  /* These two rules are necessary to disable skipping `LABEL_KW',
     `CONST_KW', `TYPE_KW', `VAR_KW', and so on when error is not processed
     in `procedure_heading' or `function_heading'. */
  | procedure_and_function_declaration_part PROCEDURE_KW error
      {
        syntax_error (SERR_procedure_heading);
        $<node>$
          = FE_new_procedure_declaration_itself
            ($2.position, $2.pseudocomment_state, NULL,
             FE_new_procedure_heading (no_position, 0, error_node, NULL),
             NULL);
      }
      block SEMICOLON
      {
        FE_set_procedure_block ($<node>4, $5);
        $$ = add_element_to_procedure_and_function_declaration_list ($1,
                                                                     $<node>4);
      }
  | procedure_and_function_declaration_part FUNCTION_KW error
      {
        syntax_error (SERR_function_heading);
        $<node>$
          = FE_new_function_declaration_itself
            ($2.position, $2.pseudocomment_state, NULL,
             FE_new_function_heading (no_position, 0, error_node, NULL,
                                       error_node), NULL);
      }
      block SEMICOLON
      {
        FE_set_function_block ($<node>4, $5);
        $$ = add_element_to_procedure_and_function_declaration_list ($1,
                                                                     $<node>4);
      }
  ;

/* Attribute of the following nonterminal is list of type definitions. */

type_definition_part
  :   {$$ = NULL;}
  | no_empty_type_definition_part
      {
        /* Transformation of cyclic list to simple list */
        if ($1 != NULL)
          {
            $$ = FE_next_type_definition ($1);
            FE_set_next_type_definition ($1, NULL);
          }
        else
          $$ = NULL;
      }
  ;

/* (+) The attribute of the following nonterminal is the last type
   definition, if any, corresponding to given nonterminal.  The type
   definitions derived from given nonterminal form cyclic list. */

no_empty_type_definition_part
  : TYPE_KW recover_mode_off type_definition SEMICOLON
      {
        $$ = $3;
        FE_set_next_type_definition ($3, $3); /* Make cycle. */
      }

  /* The following rule is necessary to disable skipping `VAR_KW',
     `PROCEDURE_KW', and so on at the begin of
     `no_empty_type_definition_part'. */
  | TYPE_KW recover_mode_off error
      {
        syntax_error (SERR_type_definition);
        $$ = NULL; /* Ignore error type definition. */
      }
  | no_empty_type_definition_part type_definition SEMICOLON
      {
#ifdef DEBUG
        if ($2 == NULL)
          abort ();
#endif
        if ($1 == NULL)
          /* It is the first element of the list. */
          FE_set_next_type_definition ($2, $2); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_type_definition ($2, FE_next_type_definition ($1));
            FE_set_next_type_definition ($1, $2);
          }
        $$ = $2;
      }

  /* The following rule is necessary to disable skipping `VAR_KW',
     `PROCEDURE_KW', and so on in `type_definition' from which `...error..'
     is derived. */
  | no_empty_type_definition_part error
      {
        syntax_error (SERR_type_definition);
        $$ = $1; /* Ignore error type definition. */
      }
  ;

/* Attribute of the following nonterminal is list of variable declarations. */

variable_declaration_part
  :   {$$ = NULL;}
  | no_empty_variable_declaration_part
      {
        /* Transformation of cyclic list to simple list */
        if ($1 != NULL)
          {
            $$ = FE_next_variable_declaration ($1);
            FE_set_next_variable_declaration ($1, NULL);
          }
        else
          $$ = NULL;
      }
  ;

/* (+) The attribute of the following nonterminal is the last variable
   declaration, if any, corresponding to given nonterminal.  The variable
   declarations derived from given nonterminal form cyclic list. */

no_empty_variable_declaration_part
  : VAR_KW recover_mode_off variable_declaration SEMICOLON
      {
        $$ = $3;
        FE_set_next_variable_declaration ($3, $3); /* Make cycle. */
      }

  /* The following rule is necessary to disable skipping `PROCEDURE_KW',
     `FUNCTION_KW', and `BEGIN_KW' at the begin of
     `no_empty_variable_declaration_part'. */
  | VAR_KW recover_mode_off error
      {
        syntax_error (SERR_variable_declaration);
        $$ = NULL; /* Ignore error variable declaration. */
      }
  | no_empty_variable_declaration_part variable_declaration SEMICOLON
      {
#ifdef DEBUG
        if ($2 == NULL)
          abort ();
#endif
        if ($1 == NULL)
          /* It is the first element of the list. */
          FE_set_next_variable_declaration ($2, $2); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_variable_declaration
              ($2, FE_next_variable_declaration ($1));
            FE_set_next_variable_declaration ($1, $2);
          }
        $$ = $2;
      }

  /* The following rule is necessary to disable skipping `PROCEDURE_KW',
     `FUNCTION_KW', and `BEGIN_KW' in `variable_declaration' from which
     `...error..' is derived. */
  | no_empty_variable_declaration_part error
      {
        syntax_error (SERR_variable_declaration);
        $$ = $1; /* Ignore error variable declaration. */
      }
  ;

/* 6.3. The attribute of the following nonterminal is node
   representing construction `constant'. */

constant
  : PLUS unsigned_number
      {
        $$ = FE_new_constant
             ($1.position, $1.pseudocomment_state,
              FE_new_constant_number_or_identifier
              (FE_position ($2), FE_pseudocomment_state ($2), TRUE, FALSE, $2),
              NULL);
      }
  | MINUS unsigned_number
      {
        $$ = FE_new_constant
             ($1.position, $1.pseudocomment_state,
              FE_new_constant_number_or_identifier
              (FE_position ($2), FE_pseudocomment_state ($2), TRUE, TRUE, $2),
              NULL);
      }
  | PLUS IDENTIFIER
      {
        $$ = FE_new_constant
             ($1.position, $1.pseudocomment_state,
              FE_new_constant_number_or_identifier
              (FE_position ($2), FE_pseudocomment_state ($2), TRUE, FALSE,
               FE_new_constant_identifier
               (FE_position ($2), FE_pseudocomment_state ($2), $2)), NULL);
      }
  | MINUS IDENTIFIER
      {
        $$ = FE_new_constant
             ($1.position, $1.pseudocomment_state,
              FE_new_constant_number_or_identifier
              (FE_position ($2), FE_pseudocomment_state ($2), TRUE, TRUE,
               FE_new_constant_identifier
               (FE_position ($2), FE_pseudocomment_state ($2), $2)), NULL);
      }
  | unsigned_number
      {
        $$ = FE_new_constant
             (FE_position ($1), FE_pseudocomment_state ($1),
              FE_new_constant_number_or_identifier
              (FE_position ($1), FE_pseudocomment_state ($1), FALSE, FALSE,
               $1), NULL);
      }
  | IDENTIFIER
      {
        $$ = FE_new_constant
             (FE_position ($1), FE_pseudocomment_state ($1),
              FE_new_constant_number_or_identifier
              (FE_position ($1), FE_pseudocomment_state ($1), FALSE, FALSE,
               FE_new_constant_identifier
               (FE_position ($1), FE_pseudocomment_state ($1), $1)), NULL);
      }
  | CHARACTER_STRING
      {
        $$ = FE_new_constant (FE_position ($1), FE_pseudocomment_state ($1),
                              $1, NULL);
      }
  ;

/* The attribute of the following nonterminal is node representing
   construction `constant_definition'. */

constant_definition
  : IDENTIFIER EQUAL constant
      {
        $$ = FE_new_constant_definition
             (FE_position ($1), FE_pseudocomment_state ($1), $1, $3, NULL);
        $$ = analyze_declaration ($$);
      }
  ;

/* 6.4.1.  The attribute of the following nonterminal is node
   representing construction `type_definition'. */

type_definition
  : IDENTIFIER EQUAL type_denoter
      {
        $$ = FE_new_type_definition
             (FE_position ($1), FE_pseudocomment_state ($1), $1, $3, NULL);
        $$ = analyze_declaration ($$);
      }
;

/*  The attribute of the following nonterminal is node
    representing construction `type_denoter'. */

type_denoter
  : IDENTIFIER
      {
        $$ = FE_new_type_identifier
             (FE_position ($1), FE_pseudocomment_state ($1), $1);
      }
  | LEFT_PARENTHESIS enumerated_type_constant_sequence RIGHT_PARENTHESIS
      {
        $$ = FE_new_enumerated_type ($1.position, $1.pseudocomment_state, $2);
      }
  | subrange_type {$$ = $1;}
  | PACKED_KW unpacked_structured_type
     {
       FE_set_it_is_packed ($2, TRUE);
       FE_set_position ($2, $1.position);
       $$ = $2;
     }
  | unpacked_structured_type {$$ = $1;}
  | ARROW IDENTIFIER
      {
        $$ = FE_new_new_pointer_type ($1.position, $1.pseudocomment_state,
                                      FE_new_type_identifier
                                      (FE_position ($2),
                                       FE_pseudocomment_state ($2), $2));
      }
  ;

/* (+) The attribute of the following nonterminal is the list of
   enumeration constants. */

enumerated_type_constant_sequence
  : enumerated_type_constant_list
      {
        /* Transformation of cyclic list to simple list */
        if ($1 != NULL)
          {
            $$ = FE_next_enumeration_constant ($1);
            FE_set_next_enumeration_constant ($1, NULL);
          }
        else
          $$ = $1;
      }
  ;

/* (+) The attribute of the following nonterminal is the last
   enumeration constant, if any, corresponding to given nonterminal.  The
   enumeration constants derived from given nonterminal form cyclic list. */

enumerated_type_constant_list
  : IDENTIFIER
      {
        $$ = FE_new_enumeration_constant
             (FE_position ($1), FE_pseudocomment_state ($1), $1, NULL);
        FE_set_next_enumeration_constant ($$, $$); /* Make cycle. */
      }
  | enumerated_type_constant_list COMMA IDENTIFIER
      {
        $$ = FE_new_enumeration_constant
             (FE_position ($3), FE_pseudocomment_state ($3), $3, NULL);
        if ($1 != NULL)
          {
            FE_set_next_enumeration_constant
              ($$, FE_next_enumeration_constant ($1));
            FE_set_next_enumeration_constant ($1, $$);
          }
        else
          /* See also rules for extensions of standard below. */
          FE_set_next_enumeration_constant ($$, $$);
      }
  ;

/* 6.4.2.1.  The attribute of the following nonterminal is node
   representing construction `ordinal_type'. */

ordinal_type
  : LEFT_PARENTHESIS enumerated_type_constant_sequence RIGHT_PARENTHESIS
      {
        $$ = FE_new_ordinal_type
             ($1.position, $1.pseudocomment_state,
              FE_new_enumerated_type ($1.position, $1.pseudocomment_state, $2),
              NULL);
      }
  | subrange_type
      {
        $$ = FE_new_ordinal_type
             (FE_position ($1), FE_pseudocomment_state ($1), $1, NULL);
      }
  | IDENTIFIER
      {
        $$ = FE_new_ordinal_type
             (FE_position ($1), FE_pseudocomment_state ($1),
              FE_new_type_identifier
              (FE_position ($1), FE_pseudocomment_state ($1), $1), NULL);
      }
  ;

/* 6.4.2.3.  Attribute of the following nonterminal is list of
   identifiers. */

identifier_list
  : identifier_sequence
      {
        /* Transformation of cyclic list to simple list */
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        $$ = FE_next_identifier ($1);
        FE_set_next_identifier ($1, NULL);
      }
  ;

/* (+) The attribute of the following nonterminal is the last
   identifier corresponding to given nonterminal.  The identifiers
   derived from given nonterminal form cyclic list. */

identifier_sequence
  : IDENTIFIER
      {
        $$ = $1;
        FE_set_next_identifier ($1, $1); /* Make cycle. */
      }
  | identifier_sequence COMMA IDENTIFIER
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        /* Add new element to the cyclic list. */
        FE_set_next_identifier ($3, FE_next_identifier ($1));
        FE_set_next_identifier ($1, $3);
        $$ = $3;
      }
  ;

/* 6.4.2.4. The attribute of the following nonterminal is node
   representing construction `subrange_type'. */

subrange_type
  : constant RANGE constant
      {
        $$ = FE_new_subrange_type ($2.position, $2.pseudocomment_state,
                                   $1, $3);
      }
  ;

/* 6.4.3.1. The attribute of the following nonterminal is node
   representing construction `unpacked_construction_type'. */

unpacked_structured_type
  : ARRAY_KW LEFT_BRACKET index_type_list RIGHT_BRACKET OF_KW type_denoter
      {
        /* Transformation of cyclic ordinal type list to simple list */
#ifdef DEBUG
        if ($3 == NULL)
          abort ();
#endif
        $$ = FE_next_ordinal_type ($3);
        FE_set_next_ordinal_type ($3, NULL);
        $$ = FE_new_array_type ($1.position, $1.pseudocomment_state, FALSE, $$, $6);
      }
  | RECORD_KW field_list END_KW
      {
        $$ = FE_new_record_type ($1.position, $1.pseudocomment_state, FALSE,
                                 $2);
      }
  | SET_KW OF_KW ordinal_type
      {
        $$ = FE_new_set_type ($1.position, $1.pseudocomment_state, FALSE, $3);
      }
  | FILE_KW OF_KW type_denoter
      {
        $$ = FE_new_file_type ($1.position, $1.pseudocomment_state, FALSE, $3);
      }
  ;

/* 6.4.3.2. (+) The attribute of the following nonterminal is the last
   ordinal type corresponding to given nonterminal.  The ordinal types
   derived from given nonterminal form cyclic list. */

index_type_list
  : ordinal_type
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        $$ = $1;
        FE_set_next_ordinal_type ($1, $1); /* Make cycle. */
      }
  | index_type_list COMMA ordinal_type
      {
#ifdef DEBUG
        if ($1 == NULL || $3 == NULL)
          abort ();
#endif
        /* Add new element to the cyclic list. */
        FE_set_next_ordinal_type ($3, FE_next_ordinal_type ($1));
        FE_set_next_ordinal_type ($1, $3);
        $$ = $3;
      }
  ;

/* 6.4.3.3 Attribute of the following nonterminal is list of
   constants. */

case_constant_list
  : case_constant_sequence
      {
        /* Transformation of cyclic list to simple list */
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        $$ = FE_next_constant ($1);
        FE_set_next_constant ($1, NULL);
      }
  ;

/* (+) The attribute of the following nonterminal is the last
   constant corresponding to given nonterminal.  The constants
   derived from given nonterminal form cyclic list. */

case_constant_sequence
  : constant
      {
        $$ = $1;
        FE_set_next_constant ($1, $1); /* Make cycle. */
      }
  | case_constant_sequence COMMA constant
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        /* Add new element to the cyclic list. */
        FE_set_next_constant ($3, FE_next_constant ($1));
        FE_set_next_constant ($1, $3);
        $$ = $3;
      }
  ;

/* The attribute of the following nonterminal is node
   representing construction `field_list'. */

field_list
  :   {$$ = form_field_list (NULL, NULL);}
  | fixed_part SEMICOLON variant_part  {$$ = form_field_list ($1, $3);}
  | fixed_part SEMICOLON variant_part SEMICOLON
      {
        $$ = form_field_list ($1, $3);
      }
  | fixed_part  {$$ = form_field_list ($1, NULL);}
  | fixed_part SEMICOLON  {$$ = form_field_list ($1, NULL);}
  | variant_part  {$$ = form_field_list (NULL, $1);}
  | variant_part SEMICOLON  {$$ = form_field_list (NULL, $1);}
  | fixed_part CASE_KW
      {
        yychar = CASE_KW;
        if (!YYRECOVERING ())
          yyerror (SERR_record_fixed_part);
        else
          syntax_error (SERR_record_fixed_part);
      }
      variant_part
      {
        $$ = form_field_list ($1, $4);
      }
  ;

/* The attribute of the following nonterminal is the last record
   section corresponding to given nonterminal.  The record sections
   derived from given nonterminal form cyclic list. */

fixed_part
  : record_section
      {
        $$ = $1;
        FE_set_next_record_section ($1, $1); /* Make cycle. */
      }

  /* The following rule is necessary to disable skipping `END_KW', `SEMICOLON',
     `RIGHT_PARENTHESIS', and token mentioned in rule for `field_list'
     at the begin of `fixed_part'. */
  | error
      {
        syntax_error (SERR_record_section);
        $$ = NULL;
      }
  | fixed_part SEMICOLON record_section
      {
        if ($1 == NULL)
          /* It is the first element of the list. */
          FE_set_next_record_section ($3, $3); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_record_section ($3, FE_next_record_section ($1));
            FE_set_next_record_section ($1, $3);
          }
        $$ = $3;
      }

  /* The following rule is necessary to disable skipping `END_KW', `SEMICOLON',
     `RIGHT_PARENTHESIS', and token mentioned in rule for `field_list'. */
  | fixed_part error
      {
        syntax_error (SERR_record_section);
        $$ = $1; /* Ignore error record section */
      }
  ;

/* The attribute of the following nonterminal is node representing
   construction `record_section'. */

record_section
  : identifier_list COLON type_denoter
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        $$ = FE_new_record_section
             (FE_position ($1), FE_pseudocomment_state ($1), $1, $3, NULL);
      }
  ;

/* The attribute of the following nonterminal is node representing
   construction `variant'. */

variant
  : case_constant_list COLON LEFT_PARENTHESIS field_list RIGHT_PARENTHESIS
      {
        $$ = FE_new_variant (FE_position ($1), FE_pseudocomment_state ($1),
                             $1, $4, NULL);
      }

  /* The following rule is necessary to disable skipping `END_KW' in
     `field_list' when `RIGHT_PARENTHESIS' is omitted. */
  | case_constant_list COLON LEFT_PARENTHESIS field_list END_KW
      {
        yychar = END_KW;
        if (!YYRECOVERING ())
          yyerror (SERR_variant_part_field_list);
        else
          syntax_error (SERR_variant_part_field_list);
        $$ = FE_new_variant (FE_position ($1), FE_pseudocomment_state ($1),
                             $1, $4, NULL);
      }
  ;

/* The attribute of the following nonterminal is node representing
   construction `variant_part'.  The field `variant_list' of the node is
   the last variant corresponding to given nonterminal.  The variants
   derived from given nonterminal form cyclic list. */

variant_part
  : CASE_KW recover_mode_off variant_selector OF_KW variant
      {
        $$ = FE_new_variant_part ($1.position, $1.pseudocomment_state, $3, $5);
        FE_set_next_variant ($5, $5); /* Make cycle in variant list. */
      }

  /* The following rule is necessary to disable skipping `SEMICOLON',
     `RIGHT_PARENTHESIS' at the begin of `variant_part'. */
  | CASE_KW recover_mode_off error
      {
        syntax_error (SERR_variant);
        $$ = FE_new_variant_part ($1.position, $1.pseudocomment_state,
                                  error_node, NULL);
      }
  | variant_part SEMICOLON variant
      {
        if (FE_variant_list ($1) == NULL)
          {
            FE_set_variant_list ($1, $3);
            FE_set_next_variant ($3, $3); /* Make cycle. */
          }
        else
          {
            /* Add new element to the cycle list. */
            $$ = FE_next_variant (FE_variant_list ($1));
            FE_set_next_variant (FE_variant_list ($1), $3);
            FE_set_next_variant ($3, $$);
            FE_set_variant_list ($1, $3);
          }
        $$ = $1;
      }

  /* The following rule is necessary to disable skipping `SEMICOLON',
     `RIGHT_PARENTHESIS' mentioned in rule for `variant'. */
  | variant_part error
      {
        syntax_error (SERR_variant);
        $$ = $1; /* Ignore error variant. */
      }
  ;

/* The attribute of the following nonterminal is node
   representing construction `variant_selector'. */

variant_selector
  : IDENTIFIER
       {
         $$ = FE_new_variant_selector
              (FE_position ($1), FE_pseudocomment_state ($1), NULL,
               FE_new_type_identifier (FE_position ($1),
                                       FE_pseudocomment_state ($1), $1));
       }
  | IDENTIFIER COLON IDENTIFIER
       {
         $$ = FE_new_variant_selector
              (FE_position ($1), FE_pseudocomment_state ($1), $1,
               FE_new_type_identifier (FE_position ($3),
                                       FE_pseudocomment_state ($3), $3));
       }
  ;

/* 6.5.1.  The attribute of the following nonterminal is node
   representing construction `variable_access'. */

variable_access
  : IDENTIFIER
      {
        /* The following variable identifier may be changed by field
           designator identifier during processing block. */
        $$ = FE_new_variable_identifier
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, $1);
      }
  | variable_access LEFT_BRACKET index_expression_list RIGHT_BRACKET
      {
        /* Transformation of cyclic list to simple list. */
#ifdef DEBUG
        if ($3 == NULL)
          abort ();
#endif
        $$ = FE_next_expression ($3);
        FE_set_next_expression ($3, NULL);
        $$ = FE_new_indexed_variable ($2.position, $2.pseudocomment_state,
                                      NULL, $1, $$);
      }
  | variable_access PERIOD IDENTIFIER
      {
        $$ = FE_new_field_access
             ($2.position, $2.pseudocomment_state, NULL, $1,
              FE_new_field_identifier (FE_position ($3),
                                       FE_pseudocomment_state ($3), $3));
      }
  | variable_access ARROW
      {
        /* The following identified variable may be changed by buffer
           variable during processing block. */
        $$ = FE_new_identified_variable ($2.position, $2.pseudocomment_state,
                                         NULL, $1);
      }
  ;

/* The attribute of the following nonterminal is node representing
   construction `variable_declaration'. */

variable_declaration : identifier_list COLON type_denoter
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        $$ = FE_new_variable_declaration
               (FE_position ($1), FE_pseudocomment_state ($1), $1, $3, NULL);
        $$ = analyze_declaration ($$);
      }
  ;

/* 6.5.3.2 (+) The attribute of the following nonterminal is the last
   expression corresponding to given nonterminal.  The expressions
   derived from given nonterminal form cyclic list. */

index_expression_list
  : expression
      {
        $$ = $1;
        FE_set_next_expression ($1, $1); /* Make cycle. */
      }
  | index_expression_list COMMA expression
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        /* Add new element to the cyclic list. */
        FE_set_next_expression ($3, FE_next_expression ($1));
        FE_set_next_expression ($1, $3);
        $$ = $3;
      }
  ;

/* 6.6.1.  The attribute of the following nonterminal is node representing
   construction `procedure_heading'. */

procedure_heading
  : PROCEDURE_KW recover_mode_off IDENTIFIER
      {
        $$ = FE_new_procedure_heading (FE_position ($3),
                                       FE_pseudocomment_state ($3), $3, NULL);
      }
  | PROCEDURE_KW recover_mode_off IDENTIFIER LEFT_PARENTHESIS
      formal_parameter_section_list RIGHT_PARENTHESIS
      {
        $$ = FE_new_procedure_heading
             (FE_position ($3), FE_pseudocomment_state ($3), $3,
              transform_formal_parameter_section_list ($5));
      }
  ;

/* 6.6.2.  The attribute of the following nonterminal is node representing
   construction `function_heading'. */

function_heading
  : FUNCTION_KW recover_mode_off IDENTIFIER COLON IDENTIFIER
      {
        $$ = FE_new_function_heading
             (FE_position ($3), FE_pseudocomment_state ($3), $3, NULL,
              FE_new_type_identifier (FE_position ($5),
                                      FE_pseudocomment_state ($5), $5));
      }
  | FUNCTION_KW recover_mode_off IDENTIFIER LEFT_PARENTHESIS
      formal_parameter_section_list RIGHT_PARENTHESIS COLON IDENTIFIER
      {
        $$ = FE_new_function_heading
             (FE_position ($3), FE_pseudocomment_state ($3), $3,
              transform_formal_parameter_section_list ($5),
              FE_new_type_identifier (FE_position ($8),
                                      FE_pseudocomment_state ($8), $8));
      }
  ;

/* 6.6.3.1. (+) The attribute of the following nonterminal is the last
   formal parameter section corresponding to given nonterminal.  The
   formal parameter sections derived from given nonterminal form cyclic
   list. */

formal_parameter_section_list
  : formal_parameter_section
      {
        $$ = $1;
        FE_set_next_formal_parameter_section ($1, $1); /* Make cycle. */
      }

  /* The following rule is necessary to disable skipping `SEMICOLON', and
     `RIGHT_PARENTHESIS' at the begin of `formal_parameter_section_list'. */
  | error
      {
        syntax_error (SERR_formal_parameter_section);
        $$ = NULL;  /* Ignore error list element. */
      }
  | formal_parameter_section_list SEMICOLON formal_parameter_section
      {
        if ($1 == NULL)
          /* It is the first element of the list. */
          FE_set_next_formal_parameter_section ($3, $3); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_formal_parameter_section
              ($3, FE_next_formal_parameter_section ($1));
            FE_set_next_formal_parameter_section ($1, $3);
          }
        $$ = $3;
      }

  /* The following rule is necessary to disable skipping `SEMICOLON', and
     `RIGHT_PARENTHESIS'. */
  | formal_parameter_section_list error
      {
        syntax_error (SERR_formal_parameter_section);
        $$ = $1; /* Ignore error list element. */
      }
  ;

/* The attribute of the following nonterminal is node representing
   a construction `formal_parameter_section'. */

formal_parameter_section
  : identifier_list COLON IDENTIFIER
      {
        $$ = FE_new_value_parameter_specification
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, $1,
              FE_new_type_identifier (FE_position ($3),
                                      FE_pseudocomment_state ($3), $3));
      }
  | VAR_KW identifier_list COLON IDENTIFIER
      {
        $$ = FE_new_variable_parameter_specification
             ($1.position, $1.pseudocomment_state, NULL, $2,
              FE_new_type_identifier (FE_position ($4),
                                      FE_pseudocomment_state ($4), $4));
      }
  | procedure_heading
      {
        $$ = FE_new_procedural_parameter_specification
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, $1);
      }
  | function_heading
      {
        $$ = FE_new_functional_parameter_specification
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, $1);
      }
  ;

/* 6.6.3.7.1.  The attribute of the following nonterminal is node
   representing a construction `conformant_array_schema'. */

conformant_array_schema
  : PACKED_KW ARRAY_KW LEFT_BRACKET ind_type_specification RIGHT_BRACKET
      OF_KW IDENTIFIER
      {
        $$ = FE_new_packed_conformant_array_schema
             ($1.position, $1.pseudocomment_state, $4,
              FE_new_type_identifier (FE_position ($7),
                                      FE_pseudocomment_state ($7), $7));
      }
  | ARRAY_KW LEFT_BRACKET ind_type_specification_list RIGHT_BRACKET
      OF_KW IDENTIFIER
      {
        $$ = FE_new_unpacked_conformant_array_schema
             ($1.position, $1.pseudocomment_state,
              transform_ind_type_specification_list ($3),
              FE_new_type_identifier
              (FE_position ($6), FE_pseudocomment_state ($6), $6));
      }
  | ARRAY_KW LEFT_BRACKET ind_type_specification_list RIGHT_BRACKET OF_KW
      conformant_array_schema
      {
        $$ = FE_new_unpacked_conformant_array_schema
             ($1.position, $1.pseudocomment_state,
              transform_ind_type_specification_list ($3), $6);
      }

  /* The following rule is necessary to disable skipping `RIGHT_PARENTHESIS' in
     `ind_type_specification_list' from which `...error..' is derived. */
  | ARRAY_KW LEFT_BRACKET ind_type_specification_list RIGHT_PARENTHESIS
      {
        yychar = RIGHT_PARENTHESIS;
        if (!YYRECOVERING ())
          yyerror (SERR_index_specification_list);
        else
          syntax_error (SERR_index_specification_list);
        $$ = FE_new_unpacked_conformant_array_schema
             ($1.position, $1.pseudocomment_state,
              transform_ind_type_specification_list ($3), error_node);
      }
  ;

/* The attribute of the following nonterminal is node representing a
   construction `formal_parameter_section'.  See also rule for
   `formal_parameter_section' above. */

formal_parameter_section
  : identifier_list COLON
      {
        if (flag_0)
          if (!YYRECOVERING ())
            yyerror (SERR_conformant_array_parameters);
        else
            syntax_error (SERR_conformant_array_parameters);
      }
      conformant_array_schema
      {
        $$ = FE_new_value_conformant_array_specification
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, $1,
              (flag_0 ? error_node : $4));
      }
  | VAR_KW identifier_list COLON
      {
        if (flag_0)
          if (!YYRECOVERING ())
            yyerror (SERR_conformant_array_parameters);
        else
            syntax_error (SERR_conformant_array_parameters);
      }
      conformant_array_schema
      {
        $$ = FE_new_variable_conformant_array_specification
             ($1.position, $1.pseudocomment_state, NULL, $2,
              (flag_0 ? error_node : $5));
      }
  ;

/* The attribute of the following nonterminal is node representing a
   construction `ind_type_specification'. */

ind_type_specification
  : IDENTIFIER RANGE IDENTIFIER COLON IDENTIFIER
      {
        $$ = FE_new_ind_type_specification
             ($2.position, $2.pseudocomment_state, $1, $3,
              FE_new_type_identifier (FE_position ($5),
                                      FE_pseudocomment_state ($5), $5), NULL);
      }
  ;

/* (+) The attribute of the following nonterminal is last index type
   specification, if any, derived from given nonterminal.  The index type
   specifications derived from given nonterminal form cyclic list. */

ind_type_specification_list
  : ind_type_specification
      {
        $$ = $1;
        FE_set_next_ind_type_specification ($1, $1); /* Make cycle. */
      }

  /* The following rule is necessary to disable skipping `RIGHT_BRACKET',
     `SEMICOLON' and token mentioned in rule for `conformant_array_schema'
     at the begin of `ind_type_specification_list'. */
  | error
      {
        syntax_error (SERR_index_specification);
        $$ = NULL;  /* Ignore error list element. */
      }
  | ind_type_specification_list SEMICOLON ind_type_specification
      {
        if ($1 == NULL)
          /* It is the first element of the list. */
          FE_set_next_ind_type_specification ($3, $3); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_ind_type_specification
              ($3, FE_next_ind_type_specification ($1));
            FE_set_next_ind_type_specification ($1, $3);
          }
        $$ = $3;
      }

  /* The following rule is necessary to disable skipping `RIGHT_BRACKET',
     `SEMICOLON' and token mentioned in rule for `conformant_array_schema'. */
  | ind_type_specification_list error
      {
        syntax_error (SERR_index_specification);
        $$ = $1; /* Ignore error list element. */
      }
  ;

/* 6.7.1. The attribute of the following nonterminal is node representing a
   construction `expression'. */

expression
  : expression EQUAL expression
      {
        $$ = FE_new_eq_operator ($2.position, $2.pseudocomment_state, NULL,
                                 $1, $3);
      }
  | expression UNEQUAL expression
      {
        $$ = FE_new_ne_operator ($2.position, $2.pseudocomment_state, NULL,
                                 $1, $3);
      }
  | expression LESS expression
      {
        $$ = FE_new_lt_operator ($2.position, $2.pseudocomment_state, NULL,
                                 $1, $3);
      }
  | expression GREATER expression
      {
        $$ = FE_new_gt_operator ($2.position, $2.pseudocomment_state, NULL,
                                 $1, $3);
      }
  | expression LESS_OR_EQUAL expression
      {
        $$ = FE_new_le_operator ($2.position, $2.pseudocomment_state, NULL,
                                 $1, $3);
      }
  | expression GREATER_OR_EQUAL expression
      {
        $$ = FE_new_ge_operator ($2.position, $2.pseudocomment_state, NULL,
                                 $1, $3);
      }
  | expression IN_KW expression
      {
        $$ = FE_new_in_operator ($2.position, $2.pseudocomment_state, NULL,
                                 $1, $3);
      }
  | simple_expression {$$ = $1;}
  ;

/* The attribute of the following nonterminal is node representing a
   construction `expression'. */

simple_expression
  : term {$$ = $1;}
  | PLUS term
      {
        $$ = FE_new_positive_operator ($1.position, $1.pseudocomment_state,
                                       NULL, $2);
      }
  | MINUS term
      {
        $$ = FE_new_negative_operator ($1.position, $1.pseudocomment_state,
                                       NULL, $2);
      }
  | simple_expression PLUS term
      {
        $$ = FE_new_plus_operator ($2.position, $2.pseudocomment_state,
                                   NULL, $1, $3);
      }
  | simple_expression MINUS term
      {
        $$ = FE_new_minus_operator ($2.position, $2.pseudocomment_state, NULL,
                                    $1, $3);
      }
  | simple_expression OR_KW term
      {
        $$ = FE_new_or_operator ($2.position, $2.pseudocomment_state, NULL,
                                 $1, $3);
      }
  ;

/* The attribute of the following nonterminal is node representing a
   construction `expression'. */

term
  : term STAR term
      {
        $$ = FE_new_mult_operator ($2.position, $2.pseudocomment_state, NULL,
                                   $1, $3);
      }
  | term SLASH term
      {
        $$ = FE_new_rdiv_operator ($2.position, $2.pseudocomment_state, NULL,
                                   $1, $3);
      }
  | term DIV_KW term
      {
        $$ = FE_new_div_operator ($2.position, $2.pseudocomment_state, NULL,
                                  $1, $3);
      }
  | term MOD_KW term
      {
        $$ = FE_new_mod_operator ($2.position, $2.pseudocomment_state, NULL,
                                  $1, $3);
      }
  | term AND_KW term
      {
        $$ = FE_new_and_operator ($2.position, $2.pseudocomment_state, NULL,
                                  $1, $3);
      }
  | variable_access
      {
        /* The variable access may be changed by unsigned constant,
           function designator or bound identifier during processing  block. */
        $$ = FE_new_expression_variable_access
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, $1);
      }
  | unsigned_number
      {
        $$ = FE_new_unsigned_constant (FE_position ($1),
                                       FE_pseudocomment_state ($1), NULL, $1);
      }
  | CHARACTER_STRING
      {
        $$ = FE_new_unsigned_constant (FE_position ($1),
                                       FE_pseudocomment_state ($1), NULL, $1);
      }
  | NIL_KW
      {
        $$ = FE_new_unsigned_constant
             ($1.position, $1.pseudocomment_state, NULL,
              FE_new_nil ($1.position, $1.pseudocomment_state));
      }
  | IDENTIFIER LEFT_PARENTHESIS new_actual_parameter_list RIGHT_PARENTHESIS
      {
        $$ = FE_new_function_designator
             ($2.position, $2.pseudocomment_state, NULL,
              FE_new_function_identifier
              (FE_position ($1), FE_pseudocomment_state ($1), $1), $3);
      }
  | LEFT_BRACKET RIGHT_BRACKET
      {
        $$ = FE_new_set_constructor ($1.position, $1.pseudocomment_state,
                                     NULL, NULL);
      }
  | LEFT_BRACKET member_designator_list RIGHT_BRACKET
      {
        /* Transformation of cyclic list to simple list */
#ifdef DEBUG
        if ($2 == NULL)
          abort ();
#endif
        $$ = FE_next_member_designator ($2);
        FE_set_next_member_designator ($2, NULL);
        $$ = FE_new_set_constructor ($1.position, $1.pseudocomment_state,
                                     NULL, $$);
      }
  | LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
      {
        $$ = $2;
        FE_set_expression_in_parentheses ($$, TRUE);
      }
  | NOT_KW term
      {
        $$ = FE_new_not_operator ($1.position, $1.pseudocomment_state, NULL,
                                  $2);
      }
  ;

/* The attribute of the following nonterminal is node representing a
   construction `member_designator'. */

member_designator
  : expression
      {
        $$ = FE_new_member_designator
             (FE_position ($1), FE_pseudocomment_state ($1), $1, NULL, NULL);
      }
  | expression RANGE expression
      {
        $$ = FE_new_member_designator ($2.position, $2.pseudocomment_state,
                                       $1, $3, NULL);
      }
  ;

/* (+) The attribute of the following nonterminal is the last member
   designator corresponding to given nonterminal.  The member designators
   derived from given nonterminal form cyclic list. */

member_designator_list
  : member_designator
      {
        $$ = $1;
        FE_set_next_member_designator ($1, $1); /* Make cycle. */
      }
  | member_designator_list COMMA member_designator
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        /* Add new element to the cycle list. */
        FE_set_next_member_designator ($3, FE_next_member_designator ($1));
        FE_set_next_member_designator ($1, $3);
        $$ = $3;
      }
  ;

/* 6.7.3. (+) The attribute of the following nonterminal is the list
   of actual parameter corresponding to given nonterminal. */

new_actual_parameter_list
  : actual_parameter_sequence
      {
        /* Transformation of cyclic list to simple list */
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        $$ = FE_next_actual_parameter ($1);
        FE_set_next_actual_parameter ($1, NULL);
      }
  ;

/* (+)  The attribute of the following nonterminal is the last actual
   parameter corresponding to given nonterminal.  The actual
   parameters derived from given nonterminal form cyclic list. */

actual_parameter_sequence
  : expression
      {
        /* The expression may be changed by variable_access,
           procedure_identifier, or function_identifier during
           processing block. */
        $$ = FE_new_actual_parameter (FE_position ($1),
                                      FE_pseudocomment_state ($1), $1,
                                      FE_expression_in_parentheses ($1), NULL);
        FE_set_next_actual_parameter ($$, $$); /* Make cycle. */
      }
  | actual_parameter_sequence COMMA expression
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        /* The expression may be changed by variable_access,
           procedure_identifier, or function_identifier during
           processing block. */
        $$ = FE_new_actual_parameter (FE_position ($3),
                                      FE_pseudocomment_state ($3), $3,
                                      FE_expression_in_parentheses ($3), NULL);
        /* Add new element to the cycle list. */
        FE_set_next_actual_parameter ($$, FE_next_actual_parameter ($1));
        FE_set_next_actual_parameter ($1, $$);
      }
  ;

/* 6.8.1.  The attribute of the following nonterminal is node representing
   statement. */

statement
  : no_label_statement {$$ = $1;}
  | DIGIT_SEQUENCE COLON no_label_statement
      {
        FE_set_opt_label ($3, $1);
        $$ = $3;
      }
  ;

/* (+) The attribute of the following nonterminal is node representing
   statement. */

no_label_statement
  /* The following for rules are needed to avoid shift/reduce conflict with
     rule `statement_list : error'. */
  : SEMICOLON
      {
        yychar = SEMICOLON;
        $$ = FE_new_empty_statement ($1.position, $1.pseudocomment_state,
                                     NULL, NULL);
      }
  | END_KW
      {
        yychar = END_KW;
        $$ = FE_new_empty_statement ($1.position, $1.pseudocomment_state,
                                     NULL, NULL);
      }
  | ELSE_KW
      {
        yychar = ELSE_KW;
        $$ = FE_new_empty_statement ($1.position, $1.pseudocomment_state,
                                     NULL, NULL);
      }
  | UNTIL_KW
      {
        yychar = UNTIL_KW;
        $$ = FE_new_empty_statement ($1.position, $1.pseudocomment_state,
                                     NULL, NULL);
      }
  | variable_access ASSIGN expression
      {
        /* The variable access may be changed by function identifier
           during processing block. */
        $$ = FE_new_assignment_statement ($2.position, $2.pseudocomment_state,
                                          NULL, NULL, $1, $3);
      }
  | IDENTIFIER
      {
        register FE_node_t actuals;
        register enum procedure_identifier_class identifier_class;

        identifier_class = procedure_identifier_class ($1);
        if (identifier_class == PIC_READLN)
          actuals = FE_new_readln_parameter_list (no_position, 0, NULL, NULL);
        else if (identifier_class == PIC_WRITELN)
          actuals = FE_new_writeln_parameter_list (no_position, 0, NULL, NULL);
        else
          actuals = FE_new_opt_actual_parameter_list (no_position, 0, NULL);
        $$ = FE_new_procedure_statement
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, NULL,
              FE_new_procedure_identifier
              (FE_position ($1), FE_pseudocomment_state ($1), $1), actuals);
      }
  | IDENTIFIER LEFT_PARENTHESIS read_write_determination
      new_actual_parameter_list RIGHT_PARENTHESIS
      {
        $$ = FE_new_procedure_statement
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, NULL,
              FE_new_procedure_identifier (FE_position ($1),
                                           FE_pseudocomment_state ($1), $1),
              FE_new_opt_actual_parameter_list
              ($2.position, $2.pseudocomment_state, $4));
      }
  | IDENTIFIER LEFT_PARENTHESIS read_write_determination READ
      variable_access_list RIGHT_PARENTHESIS
      {
        /* The first parameter may become file variable during processing
           block. */
        $$ = FE_new_procedure_statement
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, NULL,
              FE_new_procedure_identifier (FE_position ($1),
                                           FE_pseudocomment_state ($1), $1),
              FE_new_read_parameter_list ($2.position, $2.pseudocomment_state,
                                          NULL, $5));
      }
  | IDENTIFIER LEFT_PARENTHESIS read_write_determination READLN
      variable_access_list RIGHT_PARENTHESIS
      {
        /* The first parameter may become file variable during processing
           block. */
        $$ = FE_new_procedure_statement
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, NULL,
              FE_new_procedure_identifier (FE_position ($1),
                                           FE_pseudocomment_state ($1), $1),
              FE_new_readln_parameter_list
              ($2.position, $2.pseudocomment_state, NULL, $5));
      }
  | IDENTIFIER LEFT_PARENTHESIS read_write_determination WRITE
      new_write_parameter_list RIGHT_PARENTHESIS
      {
        /* The first parameter may become file variable during processing
           block. */
        $$ = FE_new_procedure_statement
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, NULL,
              FE_new_procedure_identifier (FE_position ($1),
                                           FE_pseudocomment_state ($1), $1),
              FE_new_write_parameter_list ($2.position, $2.pseudocomment_state,
                                           NULL, $5));
      }
  | IDENTIFIER LEFT_PARENTHESIS read_write_determination WRITELN
      new_write_parameter_list RIGHT_PARENTHESIS
      {
        /* The first parameter may become file variable during processing
           block. */
        $$ = FE_new_procedure_statement
             (FE_position ($1), FE_pseudocomment_state ($1), NULL, NULL,
              FE_new_procedure_identifier (FE_position ($1),
                                           FE_pseudocomment_state ($1), $1),
              FE_new_writeln_parameter_list
              ($2.position, $2.pseudocomment_state, NULL, $5));
      }
  | GOTO_KW recover_mode_off DIGIT_SEQUENCE
      {
        $$ = FE_new_goto_statement ($1.position, $1.pseudocomment_state,
                                    NULL, NULL, $3);
      }
  | BEGIN_KW recover_mode_off statement_sequence END_KW
      {
        $$ = FE_new_compound_statement ($1.position, $1.pseudocomment_state,
                                        NULL, NULL, $4.position,
                                        transform_statement_list ($3));
      }

  /* The following rule is necessary to disable skipping `UNTIL_KW' in
     `statement_sequence' when `END_KW' is omitted. */
  | BEGIN_KW recover_mode_off statement_sequence UNTIL_KW
      {
        
        yychar = UNTIL_KW;
        if (!YYRECOVERING ())
          yyerror (SERR_compound_statement);
        else
          syntax_error (SERR_compound_statement);
        $$ = FE_new_compound_statement ($1.position, $1.pseudocomment_state,
                                        NULL, NULL, no_position,
                                        transform_statement_list ($3));
      }

  /* The following rule is necessary to disable skipping `ELSE_KW' in
     `statement_sequence' when `END_KW' is omitted. */
  | BEGIN_KW recover_mode_off statement_sequence ELSE_KW
      {
        yychar = ELSE_KW;
        if (!YYRECOVERING ())
          yyerror (SERR_compound_statement);
        else
          syntax_error (SERR_compound_statement);
        $$ = FE_new_compound_statement ($1.position, $1.pseudocomment_state,
                                        NULL, NULL, no_position,
                                        transform_statement_list ($3));
      }
  | IF_KW recover_mode_off expression THEN_KW statement
      {
        $$ = FE_new_if_statement ($1.position, $1.pseudocomment_state,
                                  NULL, NULL, $3, $5, NULL);
      }
  | IF_KW recover_mode_off expression THEN_KW statement ELSE_KW statement
      {
        $$ = FE_new_if_statement ($1.position, $1.pseudocomment_state,
                                  NULL, NULL, $3, $5, $7);
      }
  | CASE_KW recover_mode_off expression OF_KW case_list_element_list END_KW
      {
        $$ = FE_new_case_statement
             ($1.position, $1.pseudocomment_state, NULL, NULL, $3,
              transform_case_list_element_list ($5));
      }
  | CASE_KW recover_mode_off expression OF_KW case_list_element_list SEMICOLON
      END_KW
      {
        $$ = FE_new_case_statement
             ($1.position, $1.pseudocomment_state, NULL, NULL, $3,
              transform_case_list_element_list ($5));
      }

  /* The following rule is necessary to disable skipping `UNTIL_KW' in
     `case_list_element_list' when `END_KW' is omitted. */
  | CASE_KW recover_mode_off expression OF_KW case_list_element_list UNTIL_KW
      {
        yychar = UNTIL_KW;
        if (!YYRECOVERING ())
          yyerror (SERR_case_statement);
        else
          syntax_error (SERR_case_statement);
        $$ = FE_new_case_statement
             ($1.position, $1.pseudocomment_state, NULL, NULL, $3,
              transform_case_list_element_list ($5));
      }

  /* The following rule is necessary to disable skipping `ELSE_KW' in
     `case_list_element_list' when `END_KW' is omitted. */
  | CASE_KW recover_mode_off expression OF_KW case_list_element_list ELSE_KW
      {
        yychar = ELSE_KW;
        if (!YYRECOVERING ())
          yyerror (SERR_case_statement);
        else
          syntax_error (SERR_case_statement);
        $$ = FE_new_case_statement
             ($1.position, $1.pseudocomment_state, NULL, NULL, $3,
              transform_case_list_element_list ($5));
      }
  | REPEAT_KW recover_mode_off statement_sequence UNTIL_KW expression
      {
        $$ = FE_new_repeat_statement ($1.position, $1.pseudocomment_state,
                                      NULL, NULL, $5,
                                      transform_statement_list ($3));
      }

  /* The following rule is necessary to disable skipping `END_KW' in
     `statement_sequence' when `UNTIL_KW' is omitted. */
  | REPEAT_KW recover_mode_off statement_sequence END_KW
      {
        
        yychar = END_KW;
        if (!YYRECOVERING ())
          yyerror (SERR_repeat_statement);
        else
          syntax_error (SERR_repeat_statement);
        $$ = FE_new_repeat_statement ($1.position, $1.pseudocomment_state,
                                      NULL, NULL, error_node,
                                      transform_statement_list ($3));
      }

  /* The following rule is necessary to disable skipping `ELSE_KW' in
     `statement_sequence' when `UNTIL_KW' is omitted. */
  | REPEAT_KW recover_mode_off statement_sequence ELSE_KW
      {
        yychar = ELSE_KW;
        if (!YYRECOVERING ())
          yyerror (SERR_repeat_statement);
        else
          syntax_error (SERR_repeat_statement);
        $$ = FE_new_repeat_statement ($1.position, $1.pseudocomment_state,
                                      NULL, NULL, error_node,
                                      transform_statement_list ($3));
      }
  | WHILE_KW recover_mode_off expression DO_KW statement
      {
        $$ = FE_new_while_statement ($1.position, $1.pseudocomment_state,
                                     NULL, NULL, $3, $5);
      }
  | FOR_KW recover_mode_off IDENTIFIER ASSIGN expression TO_KW expression
      DO_KW statement
      {
        $$ = FE_new_forward_for_statement
             ($1.position, $1.pseudocomment_state, NULL, NULL,
              FE_new_variable_identifier
              (FE_position ($3), FE_pseudocomment_state ($3), NULL, $3),
              $5, $7, $9);
      }
  | FOR_KW recover_mode_off IDENTIFIER ASSIGN expression DOWNTO_KW expression
      DO_KW statement
      {
        $$ = FE_new_backward_for_statement
             ($1.position, $1.pseudocomment_state, NULL, NULL,
              FE_new_variable_identifier
              (FE_position ($3), FE_pseudocomment_state ($3), NULL, $3),
              $5, $7, $9);
      }
  | WITH_KW recover_mode_off variable_access_list DO_KW statement
      {
        $$ = FE_new_with_statement ($1.position, $1.pseudocomment_state, NULL,
                                    NULL, $3, $5);
      }
  ;

/* (+)  An identifier and something must before this nonterminal! */

read_write_determination
  :
     {
       register enum procedure_identifier_class identifier_class;

       identifier_class = procedure_identifier_class ($<node>-1);
       if (identifier_class == PIC_READ)
         add_lexema_to_input_file (READ);
       else if (identifier_class == PIC_READLN)
         add_lexema_to_input_file (READLN);
       else if (identifier_class == PIC_WRITE)
         add_lexema_to_input_file (WRITE);
       else if (identifier_class == PIC_WRITELN)
         add_lexema_to_input_file (WRITELN);
     }
  ;

/* 6.8.3.1.  The attribute of the following nonterminal is the last
   statement, if any, corresponding to given nonterminal.  The statements
   derived from given nonterminal form cyclic list. */

statement_sequence
  : statement
      {
        $$ = $1;
        FE_set_next_statement ($1, $1); /* Make cycle. */
      }

  /* The following rule is necessary to disable skipping `SEMICOLON', `END_KW',
     `UNTIL_KW', and `ELSE_KW' (see also error rules for `no_label_statement')
     at the begin of `statement_sequence'. */
  | error
      {
        syntax_error (SERR_statement);
        $$ = NULL; /* Ignore error statement. */
      }
  | statement_sequence SEMICOLON statement
      {
#ifdef DEBUG
        if ($3 == NULL)
          abort ();
#endif
        if ($1 == NULL)
          /* It is the first element of the list. */
          FE_set_next_statement ($3, $3); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_statement ($3, FE_next_statement ($1));
            FE_set_next_statement ($1, $3);
          }
        $$ = $3;
      }

  /* The following rule is necessary to disable skipping `SEMICOLON', `END_KW',
     `UNTIL_KW', and `ELSE_KW' (see error rules for `no_label_statement'). */
  | statement_sequence error
      {
        syntax_error (SERR_statement);
        $$ = $1; /* Ignore error statement. */
      }
  ;

/* 6.8.3.5.  The attribute of the following nonterminal is node
   representing construction `case_list_element'. */

case_list_element
  : case_constant_list COLON statement
      {
        $$ = FE_new_case_list_element ($2.position, $2.pseudocomment_state,
                                       $1, $3, NULL);
      }
  ;

/* (+)  The attribute of the following nonterminal is the last case
   list element, if any, corresponding to given nonterminal.  The case
   list elements derived from given nonterminal form cyclic list. */

case_list_element_list
  : case_list_element
      {
        $$ = $1;
        FE_set_next_case_list_element ($1, $1); /* Make cycle. */
      }

  /* The following rule is necessary to disable skipping `SEMICOLON', `END_KW',
     `UNTIL_KW', and `ELSE_KW' (see also error rules for `no_label_statement')
     at the begin of `case_list_element_list'. */
  | error
      {
        syntax_error (SERR_statement);
        $$ = NULL; /* Ignore error case list element. */
      }
  | case_list_element_list SEMICOLON case_list_element
      {
#ifdef DEBUG
        if ($3 == NULL)
          abort ();
#endif
        if ($1 == NULL)
          /* It is the first element of the list. */
          FE_set_next_case_list_element ($3, $3); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_case_list_element ($3, FE_next_case_list_element ($1));
            FE_set_next_case_list_element ($1, $3);
          }
        $$ = $3;
      }

  /* The following rule is necessary to disable skipping `SEMICOLON', `END_KW',
     `UNTIL_KW', and `ELSE_KW' (see error rules for `no_label_statement'). */
  | case_list_element_list error
      {
        syntax_error (SERR_statement);
        $$ = $1; /* Ignore error case list element. */
      }
  ;

/* 6.9.1. (+)  The attribute of the following nonterminal is the list of
   variable access, if any, corresponding to given nonterminal. */

variable_access_list
  : variable_access_sequence
      {
        /* Transformation of cyclic list to simple list. */
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        $$ = FE_next_variable_access ($1);
        FE_set_next_variable_access ($1, NULL);
      }
  ;

/* (+)  The attribute of the following nonterminal is the last
   variable access, if any, corresponding to given nonterminal.  The
   variable accesses derived from given nonterminal form cyclic list. */

variable_access_sequence
  : variable_access
      {
        $$ = $1;
        FE_set_next_variable_access ($1, $1); /* Make cycle. */
      }
  | variable_access_sequence COMMA variable_access
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        /* Add new element to the cyclic list. */
        FE_set_next_variable_access ($3, FE_next_variable_access ($1));
        FE_set_next_variable_access ($1, $3);
        $$ = $3;
      }
  ;

/* 6.9.3.  The attribute of the following nonterminal is node
   representing construction `write_parameter'. */

write_parameter
  : expression
      {
        $$ = FE_new_write_parameter
             (FE_position ($1), FE_pseudocomment_state ($1), $1, NULL, NULL);
      }
  | expression COLON expression
      {
        $$ = FE_new_write_parameter
             (FE_position ($1), FE_pseudocomment_state ($1), $1,
              FE_new_format_specification ($2.position, $2.pseudocomment_state,
                                           $3, NULL), NULL);
      }
  | expression COLON expression COLON expression
      {
        $$ = FE_new_write_parameter
             (FE_position ($1), FE_pseudocomment_state ($1), $1,
              FE_new_format_specification ($2.position, $2.pseudocomment_state,
                                           $3, $5), NULL);
      }
  ;

/* (+) The attribute of the following nonterminal is the list of
   write-parameter, if any, corresponding to given nonterminal. */

new_write_parameter_list
  : write_parameter_sequence
      {
        /* Transformation of cyclic list to simple list. */
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        $$ = FE_next_write_parameter ($1);
        FE_set_next_write_parameter ($1, NULL);
      }
  ;

/* (+) The attribute of the following nonterminal is the last
   write-parameter, if any, corresponding to given nonterminal.  The
   write-parameters derived from given nonterminal form cyclic list. */

write_parameter_sequence
  : write_parameter
      {
        $$ = $1;
        FE_set_next_write_parameter ($1, $1); /* Make cycle. */
      }
  | write_parameter_sequence COMMA write_parameter
      {
#ifdef DEBUG
        if ($1 == NULL)
          abort ();
#endif
        /* Add new element to the cyclic list. */
        FE_set_next_write_parameter ($3, FE_next_write_parameter ($1));
        FE_set_next_write_parameter ($1, $3);
        $$ = $3;
      }
  ;

/* 6.10. The attribute of the following nonterminal is node
   representing construction `program'. */

program
  : program_heading SEMICOLON
      {
        $<node>$ = FE_new_program
                   (FE_position ($1), FE_pseudocomment_state ($1), NULL,
                    $1, NULL);
        $<node>$ = analyze_declaration ($<node>$);
      }
      block PERIOD
      {
        FE_set_program_block ($<node>3, $4);
        $$ = $<node>3;
      }

  /* The following rule is necessary to disable skipping `LABEL_KW',
     `CONST_KW', `TYPE_KW', and so on in `block'.  It is better
     `error block PERIOD' if only Pascal standard syntax is used. */
  | PROGRAM_KW recover_mode_off error
      {
        syntax_error (SERR_program_heading);
        $<node>$ = FE_new_program ($1.position, $1.pseudocomment_state, NULL,
                                   error_node, NULL);
      }
      block PERIOD
      {
        FE_set_program_block ($<node>4, $5);
        $$ = $<node>4;
      }
  ;

/* The attribute of the following nonterminal is node
   representing construction `program_heading'. */

program_heading
  : PROGRAM_KW recover_mode_off IDENTIFIER
      {
        $$ = FE_new_program_heading ($1.position, $1.pseudocomment_state,
                                     $3, NULL);
      }
  | PROGRAM_KW recover_mode_off IDENTIFIER LEFT_PARENTHESIS
      identifier_list RIGHT_PARENTHESIS
      {
        $$ = FE_new_program_heading ($1.position, $1.pseudocomment_state, $3,
                                     $5);
      }
  ;

/* (+)  The attribute of the following nonterminal is node
   representing compilation unit. */

compilation_file
  : standard_or_not STANDARD program  {$$ = $3;}

  /* The following rule is necessary to disable skipping `LABEL_KW',
     `CONST_KW', `TYPE_KW', and so on in `block'. */
  | standard_or_not STANDARD error
      {
        syntax_error (SERR_program_heading);
        $<node>$ = FE_new_program (no_position, 0, NULL, error_node, NULL);
      }
    block PERIOD
      {
        FE_set_program_block ($<node>4, $5);
        $$ = $<node>4;
      }
  | standard_or_not include_statement_list compilation_unit
      {
        /* Transformation of cyclic list to simple list. */
        if ($2 != NULL)
          {
            $$ = FE_next_include_statement ($2);
            FE_set_next_include_statement ($2, NULL);
            $$ = $2;
          }
        FE_set_include_statement_list ($3, $2);
        $$ = $3;
      }
  ;

/* (+) */
standard_or_not
  :
    {
      start_compilation_file_analysis ();
      if (flag_0 || flag_1)
        add_lexema_to_input_file (STANDARD);
    }
  ;

/* (+)  The attribute of the following nonterminal is node
   representing any compilation unit. */

compilation_unit : program {$$ = $1;}
                 ;

/* (+) */

include_statement_list : {$$ = NULL;}
                       ;


/* All Pascal standard extensions are placed below.  To obtain syntax
   of standard pascal all rules below are to be deleted.  The scanner
   in only standard regime does not return extension constructions
   keywords.  Therefore syntax errors occur in such regime during
   processing the most of extensions. */

/* There are also rules for given nonterminal upper. */

enumerated_type_constant_list
  :
    {
      if (!enumeration_extension_flag)
        if (!YYRECOVERING ())
          yyerror (SERR_empty_enumerated_constant);
        else
          syntax_error (SERR_empty_enumerated_constant);
      $$ = FE_new_enumeration_constant
           (current_position, pseudocomment_state, NULL, NULL);
      FE_set_next_enumeration_constant ($$, $$); /* Make cycle. */
    }
  | enumerated_type_constant_list COMMA
    {
      if (!enumeration_extension_flag)
        if (!YYRECOVERING ())
          yyerror (SERR_empty_enumerated_constant);
        else
          syntax_error (SERR_empty_enumerated_constant);
      $$ = FE_new_enumeration_constant
           (current_position, pseudocomment_state, NULL, NULL);
      FE_set_next_enumeration_constant ($$, FE_next_enumeration_constant ($1));
      FE_set_next_enumeration_constant ($1, $$);
    }
  ;

/* There are also rules for given nonterminal upper.  The attribute of
   the following nonterminal is node representing scaled number type. */

type_denoter
  :
      {
        if (!scaled_extension_flag)
          if (!YYRECOVERING ())
            yyerror (SERR_scaled_number_type);
          else
            syntax_error (SERR_scaled_number_type);
      }
    SCALED_KW LEFT_PARENTHESIS  constant  COMMA  constant  RIGHT_PARENTHESIS
      {
        $$ = FE_new_scaled_type ($2.position, $2.pseudocomment_state, $4, $6);
      }
  ;

/* There are also rules for given nonterminal upper. */

procedure_and_function_declaration_part
  : procedure_and_function_declaration_part entry_procedure_heading
      {
        /* Procedure declaration itself may be changed by procedure
           definition. */
        if (flag_0 || flag_1)
          $<node>$ = FE_new_procedure_declaration_itself
                     (FE_position ($2), FE_pseudocomment_state ($2), NULL,
                      $2, NULL);
        else
          $<node>$ = FE_new_entry_procedure_declaration
                     (FE_position ($2), FE_pseudocomment_state ($2), NULL,
                      $2, NULL);
        /* The procedure declaration itself may be changed by procedure
           definition in the following call. */
        $<node>$ = analyze_declaration ($<node>$);
      }
      SEMICOLON block SEMICOLON
      {
        FE_set_procedure_block ($<node>3, $5);
        $$ = add_element_to_procedure_and_function_declaration_list ($1,
                                                                     $<node>3);
      }
  | procedure_and_function_declaration_part entry_function_heading
      {
        if (flag_0 || flag_1)
          $<node>$ = FE_new_function_declaration_itself
                     (FE_position ($2), FE_pseudocomment_state ($2), NULL,
                      $2, NULL);
        else
          $<node>$ = FE_new_entry_function_declaration
                     (FE_position ($2), FE_pseudocomment_state ($2), NULL,
                      $2, NULL);
        $<node>$ = analyze_declaration ($<node>$);
      }
      SEMICOLON block SEMICOLON
      {
        FE_set_function_block ($<node>3, $5);
        $$ = add_element_to_procedure_and_function_declaration_list ($1,
                                                                     $<node>3);
      }
  ;

/* (+) See analogous rules for `procedure_heading' upper. */

entry_procedure_heading
  : PROCEDURE_KW recover_mode_off ENTRY_KW IDENTIFIER
      {
        $$ = FE_new_procedure_heading (FE_position ($4),
                                       FE_pseudocomment_state ($4), $4, NULL);
      }

  /* The following rules is needed for situation when only standard is used
     and identifier `entry' occurs. */
  | PROCEDURE_KW recover_mode_off IDENTIFIER error
      {
#ifdef DEBUG
        if (yychar < 0)
          abort ();
#endif
        if (yychar == IDENTIFIER)
          syntax_error (SERR_identifier_before_procedure_identifier);
        else
          syntax_error (SERR_procedure_heading);
        $$ = FE_new_procedure_heading
             (FE_position (yychar == IDENTIFIER ? yylval.node: $3),
              FE_pseudocomment_state (yychar == IDENTIFIER ? yylval.node: $3),
              (yychar == IDENTIFIER ? yylval.node: $3), NULL);
      }

  /* The following rules is needed for situation when only standard is used
     and identifier `entry' occurs. */
  | PROCEDURE_KW recover_mode_off IDENTIFIER error
      {
#ifdef DEBUG
        if (yychar < 0)
          abort ();
#endif
        if (yychar == IDENTIFIER)
          syntax_error (SERR_identifier_before_procedure_identifier);
        else
          syntax_error (SERR_procedure_heading);
        $<node>$ = (yychar == IDENTIFIER ? yylval.node : $3);
      }
      LEFT_PARENTHESIS formal_parameter_section_list RIGHT_PARENTHESIS
      {
        $$ = FE_new_procedure_heading
             (FE_position ($<node>5), FE_pseudocomment_state ($<node>5),
              $<node>5, transform_formal_parameter_section_list ($7));
      }
  | PROCEDURE_KW recover_mode_off ENTRY_KW IDENTIFIER LEFT_PARENTHESIS
      formal_parameter_section_list RIGHT_PARENTHESIS
      {
        $$ = FE_new_procedure_heading
             (FE_position ($4), FE_pseudocomment_state ($4), $4,
              transform_formal_parameter_section_list ($6));
      }
  ;

/* (+) See analogous rules for `function_heading' upper. */

entry_function_heading
  : FUNCTION_KW recover_mode_off ENTRY_KW IDENTIFIER COLON IDENTIFIER
      {
        $$ = FE_new_function_heading
             (FE_position ($4), FE_pseudocomment_state ($4), $4, NULL,
              FE_new_type_identifier (FE_position ($6),
                                      FE_pseudocomment_state ($6), $6));
      }

  /* The following rules is needed for situation when only standard is used
     and identifier `entry' occurs. */
  | FUNCTION_KW recover_mode_off IDENTIFIER error
      {
#ifdef DEBUG
        if (yychar < 0)
          abort ();
#endif
        if (yychar == IDENTIFIER)
          syntax_error (SERR_identifier_before_function_identifier);
        else
          syntax_error (SERR_function_heading);
        $<node>$ = (yychar == IDENTIFIER ? yylval.node : $3);
      }
      COLON IDENTIFIER
      {
        $$ = FE_new_function_heading
             (FE_position ($<node>5), FE_pseudocomment_state ($<node>5),
              $<node>5, NULL,
              FE_new_type_identifier (FE_position ($7),
                                      FE_pseudocomment_state ($7), $7));
      }

  /* The following rules is needed for situation when only standard is used
     and identifier `entry' occurs. */
  | FUNCTION_KW recover_mode_off IDENTIFIER error
      {
#ifdef DEBUG
        if (yychar < 0)
          abort ();
#endif
        if (yychar == IDENTIFIER)
          syntax_error (SERR_identifier_before_function_identifier);
        else
          syntax_error (SERR_function_heading);
        $<node>$ = (yychar == IDENTIFIER ? yylval.node : $3);
      }
      LEFT_PARENTHESIS formal_parameter_section_list RIGHT_PARENTHESIS
      COLON IDENTIFIER
      {
        $$ = FE_new_function_heading
             (FE_position ($<node>5), FE_pseudocomment_state ($<node>5),
              $<node>5, transform_formal_parameter_section_list ($7),
              FE_new_type_identifier (FE_position ($10),
                                      FE_pseudocomment_state ($10), $10));
      }
  | FUNCTION_KW recover_mode_off ENTRY_KW IDENTIFIER LEFT_PARENTHESIS
      formal_parameter_section_list RIGHT_PARENTHESIS COLON IDENTIFIER
      {
        $$ = FE_new_function_heading
             (FE_position ($4), FE_pseudocomment_state ($4), $4,
              transform_formal_parameter_section_list ($6),
              FE_new_type_identifier (FE_position ($9),
                                      FE_pseudocomment_state ($9), $9));
      }
  ;

/* There are also rules for given nonterminal upper. */

compilation_unit : task_data {$$ = $1;}
                 | subtask_unit {$$ = $1;}
                 ;

/* (+) The attribute of the following nonterminal is node representing
   task data. */

task_data
  : task_data_heading  create_task_data_declaration  constant_definition_part
    initiate_types
    type_definition_part  variable_declaration_part
    finish_types
    external_procedure_and_function_declaration_part
    END_KW PERIOD
      {
        /* Transformation of cyclic list to simple list. */
        if ($8 != NULL)
          {
            $$ = FE_next_external_procedure_and_function_declaration ($8);
            FE_set_next_external_procedure_and_function_declaration ($8, NULL);
          }
        else
          $$ = NULL;
        FE_set_constant_definition_part ($2, $3);
        FE_set_type_definition_part ($2, $5);
        FE_set_variable_declaration_part ($2, $6);
        FE_set_external_procedure_and_function_declaration_part ($2, $$);
      }
  ;

/* (+)  A symbol `task_data_heading' must before this nonterminal! */

create_task_data_declaration
  :
    {
      $$ = FE_new_task_data (FE_position ($<node>0),
                             FE_pseudocomment_state ($<node>0),
                             NULL, $<node>0, NULL, NULL, NULL, NULL);
      $$ = analyze_declaration ($$);
    }
  ;

/* (+) The attribute of the following nonterminal is node representing
   task data heading. */

task_data_heading
  : TASKDATA_KW recover_mode_off IDENTIFIER SEMICOLON
      {
        $$ = FE_new_task_data_heading ($1.position, $1.pseudocomment_state,
                                       $3);
      }

  /* The following rule is necessary to disable skipping `CONST_KW',`TYPE_KW',
     and so on inside `task_data'. */
  | TASKDATA_KW recover_mode_off error
      {
        syntax_error (SERR_task_data_heading); $$ = NULL;
        $$ = FE_new_task_data_heading ($1.position, $1.pseudocomment_state,
                                       error_node);
      }
  ;

/* (+) The attribute of the following nonterminal is node representing
   subtask unit. */

subtask_unit
  :
      {
        $<lexema_attribute>$.position = current_position;
        $<lexema_attribute>$.pseudocomment_state = pseudocomment_state;
      }
    constant_definition_part
    initiate_types
    type_definition_part  variable_declaration_part
    finish_types
    start_procedures
    procedure_and_function_declaration_part
    finish_procedures
      {
        $$ = FE_new_subtask_unit
             ($<lexema_attribute>1.position,
              $<lexema_attribute>1.pseudocomment_state, NULL, $2, $4, $5,
              transform_procedure_and_function_declaration_list ($8));
      }
  ;

/* (+) The attribute of the following nonterminal is last external
   procedure or function declaration, if any, derived from given
   nonterminal.  The external procedure and function declarations derived
   from given nonterminal form cyclic list. */

external_procedure_and_function_declaration_part
  :   {$$ = NULL;}
  | external_procedure_and_function_declaration_part
      procedure_heading SEMICOLON IDENTIFIER SEMICOLON
      {
        $$ = FE_new_external_procedure_declaration
             (FE_position ($2), FE_pseudocomment_state ($2), NULL, $2, $4);
        $$ = analyze_declaration ($$);
        if ($1 == NULL)
          /* It is the first element of the list. Make cycle. */
          FE_set_next_external_procedure_and_function_declaration ($$, $$);
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_external_procedure_and_function_declaration
              ($$, FE_next_external_procedure_and_function_declaration ($1));
            FE_set_next_external_procedure_and_function_declaration ($1, $$);
          }
      }
  | external_procedure_and_function_declaration_part
      function_heading SEMICOLON IDENTIFIER SEMICOLON
      {
        $$ = FE_new_external_function_declaration
             (FE_position ($2), FE_pseudocomment_state ($2), NULL, $2, $4);
        $$ = analyze_declaration ($$);
        if ($1 == NULL)
          /* It is the first element of the list. Make cycle. */
          FE_set_next_external_procedure_and_function_declaration ($$, $$);
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_external_procedure_and_function_declaration
              ($$, FE_next_external_procedure_and_function_declaration ($1));
            FE_set_next_external_procedure_and_function_declaration ($1, $$);
          }
      }

  /* These two rules are necessary to disable skipping `END_KW'
     when error is not processed in `procedure_heading' or
     `function_heading'. */
  | external_procedure_and_function_declaration_part PROCEDURE_KW error
      {
        syntax_error (SERR_procedure_heading);
        $$ = $1; /* Ignore error external declaration. */
      }
  | external_procedure_and_function_declaration_part FUNCTION_KW error
      {
        syntax_error (SERR_function_heading);
        $$ = $1; /* Ignore error external declaration. */
      }
  ;

/* There are also rules for given nonterminal upper. The attribute of
   the following nonterminal is the last include statement, if any,
   derived from given nonterminal.  The include statements derived from
   given nonterminal form cyclic list.  */

include_statement_list
  : include_statement_list INCLUDE_KW recover_mode_off IDENTIFIER SEMICOLON
      {
        full_file_name_in_last_include_statement
          = full_included_file_name (FE_identifier_itself ($4));
        if (!scanning_file_already_started
             (full_file_name_in_last_include_statement))
          add_lexema_to_input_file (INCLUSION);
        else if (!w_flag)
          warning (FE_position ($4), SWARN_repeated_file_inclusion_is_ignored,
                   full_file_name_in_last_include_statement);

      }
      included_taskdata
      {
        $$ = FE_new_include_statement ($2.position, $2.pseudocomment_state,
                                       $4, NULL);
        if ($1 == NULL)
          /* It is the first element of the list. */
            FE_set_next_include_statement ($$, $$); /* Make cycle. */
        else
          {
            /* Add new element to the cyclic list. */
            FE_set_next_include_statement ($$, FE_next_include_statement ($1));
            FE_set_next_include_statement ($1, $$);
          }
      }

  /* The following rule is necessary to disable skipping `INCLUDE_KW'.
     This rule is also necessary `CONST_KW',`TYPE_KW', and so on
     in `subtask_unit' and `PROGRAM_KW' and `TASKDATA_KW in `program'
     and `subtask_unit'. */
  | include_statement_list error
      {
        syntax_error (SERR_compilation_unit_start);
        $$ = $1; /* Ignore error include statement */
      }
  ;

/* (+) A symbol with attribute equal to node representing identifier
   in constructions `INCLUDE ...' must before this nonterminal!
   The attribute of this nonterminal is not defined and not used. */

included_taskdata :
                  | recover_mode_off
                    INCLUSION
                      {
                        start_parser_file
                          (full_file_name_in_last_include_statement,
                           FE_position ($<node>-2));
                      }
                    task_data_or_error END_OF_INCLUDED_FILE recover_mode_off
                      {
                        finish_parser_file ();
                      }
                  ;

/* (+) */

task_data_or_error
  : include_statement_list task_data
      { /* In order to remove warning: type clash on default action. */ }
  | include_statement_list END_OF_INCLUDED_FILE
      {
        yychar = END_OF_INCLUDED_FILE;
        if (!YYRECOVERING ())
          yyerror (SERR_included_task_data);
        else
          syntax_error (SERR_included_task_data);
      }
  ;

/* All following rules are necessary to guarantee that token
   `END_OF_INCLUDED_FILE' is not skipped in error rules above and results
   in their reducing.  Remember that any addition of error rule to the
   grammar may require addition of such rules here!   Action can be not
   written because the attributes of the nonterminals will be ignored as
   new syntactic error will occur on `END_OF_INCLUDED_FILE'. */

/* There are also rules for given nonterminal upper. */

task_data
  : task_data_heading  create_task_data_declaration
    constant_definition_part
    initiate_types
    type_definition_part  variable_declaration_part
    finish_types
    external_procedure_and_function_declaration_part
    END_OF_INCLUDED_FILE
      {
        yychar = END_OF_INCLUDED_FILE;
        if (!YYRECOVERING ())
          yyerror (SERR_task_data);
        else
          syntax_error (SERR_task_data);
      }
  ;

/* There are also rules for given nonterminal upper. */

unpacked_structured_type
  : RECORD_KW field_list END_OF_INCLUDED_FILE
      {
        yychar = END_OF_INCLUDED_FILE;
        if (!YYRECOVERING ())
          yyerror (SERR_task_data);
        else
          syntax_error (SERR_task_data);
      }
  ;

/* There are also rules for given nonterminal upper. */

variant
  : case_constant_list COLON LEFT_PARENTHESIS field_list END_OF_INCLUDED_FILE
      {
        yychar = END_OF_INCLUDED_FILE;
        if (!YYRECOVERING ())
          yyerror (SERR_task_data);
        else
          syntax_error (SERR_task_data);
      }
  ;

/* There are also rules for given nonterminal upper. */

procedure_heading
  : PROCEDURE_KW recover_mode_off IDENTIFIER LEFT_PARENTHESIS
      formal_parameter_section_list END_OF_INCLUDED_FILE
        {
          yychar = END_OF_INCLUDED_FILE;
          if (!YYRECOVERING ())
            yyerror (SERR_task_data);
          else
            syntax_error (SERR_task_data);
        }
  ;

/* There are also rules for given nonterminal upper. */

function_heading
  : FUNCTION_KW recover_mode_off IDENTIFIER
      LEFT_PARENTHESIS formal_parameter_section_list END_OF_INCLUDED_FILE
        {
          yychar = END_OF_INCLUDED_FILE;
          if (!YYRECOVERING ())
            yyerror (SERR_task_data);
          else
            syntax_error (SERR_task_data);
        }
  ;

/* There are also rules for given nonterminal upper. */

ind_type_specification_list
  : ARRAY_KW LEFT_BRACKET ind_type_specification_list END_OF_INCLUDED_FILE
      {
        yychar = END_OF_INCLUDED_FILE;
        if (!YYRECOVERING ())
          yyerror (SERR_task_data);
        else
          syntax_error (SERR_task_data);
      }
  ;

%%



/* This page contains functions used only in YACC actions. */


/*
   FUNCTION NAME:   transform_procedure_and_function_declaration_list

   DESCRIPTION: The function transforms cyclic list of declarations of
       procedures or/and functions to simple list.

   INPUT DATA:
       list               the last element of given cyclic list

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       The first element of the list or NULL if the list is empty

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static FE_node_t
transform_procedure_and_function_declaration_list (FE_node_t list)
{
  register FE_node_t result;

  /* Transformation of cyclic list to simple list */
  if (list != NULL)
    {
      result = FE_next_procedure_and_function_declaration (list);
      FE_set_next_procedure_and_function_declaration (list, NULL);
    }
  else
    result = NULL;
  return result;
}


/*
   FUNCTION NAME:   add_element_to_procedure_and_function_declaration_list

   DESCRIPTION: The function adds declaration of procedure or function
       to end of (possibly empty) cyclic list.

   INPUT DATA:
       list               the last element of given cyclic list
       element            the element being added

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       The element added to the cyclic list

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static FE_node_t
add_element_to_procedure_and_function_declaration_list (FE_node_t list,
                                                        FE_node_t element)
{
#ifdef DEBUG
  if (element == NULL)
    abort ();
#endif
  if (list == NULL)
    /* It is the first element of the list. Make cycle. */
    FE_set_next_procedure_and_function_declaration (element, element);
  else
    {
      /* Add new element to the cyclic list. */
      FE_set_next_procedure_and_function_declaration
        (element, FE_next_procedure_and_function_declaration (list));
      FE_set_next_procedure_and_function_declaration (list, element);
    }
  return element;
}


/*
   FUNCTION NAME:   form_field_list

   DESCRIPTION: The function transforms given cyclic record section
       list and variant list of given variant part to the simple lists
       and creates field list with given record section list and
       variant part.

   INPUT DATA:
       record_section_list the last element of given cyclic record section list
       variant_part        given variant part     

   GLOBALS READ:
       error_node          node representing any error Pascal construction
       no_position         position of field list when given record section
                           list and variant part are empty
   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       The node representing field list with given record section list
       and variant part

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static FE_node_t
form_field_list (FE_node_t record_section_list, FE_node_t variant_part)
{
  register FE_node_t temporary;
  register FE_node_t result;

#ifdef DEBUG
  if (variant_part != NULL && FE_NODE_MODE (variant_part) == FENM__error)
    abort ();
#endif
  if (variant_part != NULL)
    {
      /* Transformation of cyclic variant list to simple list. */
      if (FE_variant_list (variant_part) != NULL)
        {
          temporary = FE_next_variant (FE_variant_list (variant_part));
          FE_set_next_variant (FE_variant_list (variant_part), NULL);
          FE_set_variant_list (variant_part, temporary);
        }
      else
        FE_set_variant_list (variant_part, error_node);
    }
  if (record_section_list == NULL)
    result = FE_new_field_list
             (variant_part == NULL ? no_position : FE_position (variant_part),
              variant_part == NULL ? 0 : FE_pseudocomment_state (variant_part),
              NULL, variant_part);
  else
    {
      /* Transformation of cyclic record section list to simple list. */
      temporary = FE_next_record_section (record_section_list);
      FE_set_next_record_section (record_section_list, NULL);
      result = FE_new_field_list
               (FE_position (temporary), FE_pseudocomment_state (temporary),
                temporary, variant_part);
    }
  return result;
}


/*
   FUNCTION NAME:   transform_formal_parameter_section_list

   DESCRIPTION: The function transforms cyclic list of formal
       parameter sections to simple list.

   INPUT DATA:
       list               the last element of given cyclic list

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       The first element of the list or NULL if the list is empty

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static FE_node_t
transform_formal_parameter_section_list (FE_node_t list)
{
  register FE_node_t result;

  if (list != NULL)
    {
      /* Transformation of cyclic list to simple list */
      result = FE_next_formal_parameter_section (list);
      FE_set_next_formal_parameter_section (list, NULL);
    }
  else
    result = NULL;
  return result;
}


/*
   FUNCTION NAME:   transform_ind_type_specification_list

   DESCRIPTION: The function transforms cyclic list of array index
       type specifications to simple list.

   INPUT DATA:
       list               the last element of given cyclic list

   GLOBALS READ:
       error_node          node representing any error Pascal construction

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       The first element of the list or error node if the list is empty

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static FE_node_t
transform_ind_type_specification_list (FE_node_t list)
{
  register FE_node_t result;

  if (list == NULL)
    result = error_node;
  else
    {
      /* Transformation of cyclic list to simple list */
      result = FE_next_ind_type_specification (list);
      FE_set_next_ind_type_specification (list, NULL);
    }
  return result;
}


/*
   FUNCTION NAME:   transform_statement_list

   DESCRIPTION: The function transforms cyclic list of statements to
       simple list.  If the list empty (it is possible only when
       syntax error was fixed) the function creates simple list of
       one empty statement.

   INPUT DATA:
       list               the last element of given cyclic list

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       The first element of the list

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static FE_node_t
transform_statement_list (FE_node_t list)
{
  register FE_node_t result;
  
  /* Transformation of cyclic list to simple list. */
  if (list != NULL)
    {
      result = FE_next_statement (list);
      FE_set_next_statement (list, NULL);
    }
  else
    /* This situation is possible when statement sequence contains
       only one error statement.  Otherwise there is empty statement
       at least. */
    result = FE_new_empty_statement (current_position, pseudocomment_state,
                                     NULL, NULL);
  return result;
}


/*
   FUNCTION NAME:   transform_case_list_element_list

   DESCRIPTION: The function transforms cyclic list of case list
       elements to simple list.

   INPUT DATA:
       list               the last element of given cyclic list

   GLOBALS READ:
       error_node          node representing any error Pascal construction

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       The first element of the list or error node if the list is empty

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static FE_node_t
transform_case_list_element_list (FE_node_t list)
{
  register FE_node_t result;
  
  /* Transformation of cyclic list to simple list. */
  if (list == NULL)
    result = error_node;
  else
    {
      result = FE_next_case_list_element (list);
      FE_set_next_case_list_element (list, NULL);
    }
  return result;
}


/*
   FUNCTION NAME:   full_included_file_name

   DESCRIPTION: The function returns full file name of task data with
       given name.  To make this functions searches for files in
       current directory (i.e. directory in which source file with the
       corresponding include-statement is placed) and in directories
       given in the command line of the cross-compiler.  If the
       included file is not found the function returns the task data
       file name in the current directory

   INPUT DATA:
       task_data_name      task data name given in the corresponding
                           include-statement

   GLOBALS READ:
       currently_scanned_source_file_name
                           currently scanned source file name
                           or NULL if such file is undefined
       include_directories null ended vector of directories in which
                           are given in options `-I' task data are searched for
       FERR_fatal_system_error
                           message about error during closing found file
       no_position         position of the fixed error mentioned above


   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static const char *
full_included_file_name (const char *task_data_name)
{
  register const char *current_directory_name;
  register const char *real_file_name;
  register const char *file_name;
  register const char **include_directory_ptr;
  register FILE *current_file;

  current_directory_name
         = file_name_directory (currently_scanned_source_file_name);
  real_file_name = file_path_name (current_directory_name,
                                   task_data_name, TASK_DATA_UNIT_FILE_SUFFIX);
  current_file = fopen (real_file_name, "r");
  if (current_file == NULL)
    for (include_directory_ptr = include_directories;
         *include_directory_ptr != NULL; include_directory_ptr++)
      {
        file_name = file_path_name (*include_directory_ptr,
                                    task_data_name,
                                    TASK_DATA_UNIT_FILE_SUFFIX);
        current_file = fopen (file_name, "r");
        if (current_file != NULL)
          {
            real_file_name = file_name;
            break;
          }
      }
  if (current_file != NULL && fclose (current_file) == EOF)
    system_error (TRUE, no_position, FERR_fatal_system_error, real_file_name);
  return real_file_name;
}



/*   This page contains abstract data `syntax_errors'.  This abstract
   data is used for error reporting.
     YACC has unsatisfactory syntactic error reporting.  Essentially
   YACC generates only one syntactic error "syntax error".  Abstract
   data `syntax_errors' for solution of this problem is suggested
   here.  This abstract data saves only one the most recent error
   message and its position.  Function `yyerror' used by function
   `yyparse' flushes saved early error message and saves message given
   as parameter (usually this message is simply "syntax error" but may
   be "stack overflow" or others), its position and the erroneous
   lexema representation.  More detail diagnostic message is generated
   by the call of function `syntax_error' within action after special
   token `error'.  This function only rewrites saved early error
   message (not error position and lexema representation).  As
   consequence the message of last call `syntax_error' or the message
   of function `yyerror' (if such call of `syntax_error' does not
   exist) is fixed within one recover mode interval because function
   `yyerror' is called by `yyparse' only in non recover mode.
     This solution permits to create brief syntactic message which
   contains erroneous lexema representation and partially information
   about recovery location (error line and position is always in
   non-fatal errors and warnings -- see package for output of compiler
   messages).  Now few syntactic errors are planned to create.  But
   this solution permits to enrich syntactic error reporting in the
   future.  */


/* This variable saves the most recent syntax error message or NULL (if
   such syntax error was not fixed). */

static const char *last_syntax_error_message;

/* This variable saves the first (during error recover mode) syntax error
   lexema representation.  The value is undefined when
   `last_syntax_error_message' is NULL. */

static vlo_t first_syntax_error_lexema_representation;

/* This variable saves the position of the first syntax error lexema. */

static position_t first_syntax_error_position;


/*
   FUNCTION NAME:   initiate_syntax_errors

   DESCRIPTION: The function initiates the abstract data as without
                saved syntax error message.

   INPUT DATA:
       <None>

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       last_syntax_error_message                 saved the most recent syntax
                                                 error message
       first_syntax_error_lexema_representation  representation of the first
                                                 syntax error lexema

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
         The function must be called only once before any work with the
       abstract data.

   ANCHOR: <>
   SOURCE: <>
*/

static void
initiate_syntax_errors (void)
{
  last_syntax_error_message = NULL;
  VLO_CREATE (first_syntax_error_lexema_representation, 20);
}


/*
   FUNCTION NAME:   flush_syntax_error

   DESCRIPTION: The function flushes saved syntax error message, if any.

   INPUT DATA:
       <None>

   GLOBALS READ:
       last_syntax_error_message                 saved the most recent syntax
                                                 error message
       first_syntax_error_position               position of the saved message
       first_syntax_error_lexema_representation  representation of the first
                                                 syntax error lexema

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       last_syntax_error_message                 saved the most recent syntax
                                                 error message
       first_syntax_error_lexema_representation  representation of the first
                                                 syntax error lexema

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static void
flush_syntax_error (void)
{
  if (last_syntax_error_message != NULL)
    {
      error (FALSE, first_syntax_error_position,
             "%s before %s", last_syntax_error_message,
             VLO_BEGIN (first_syntax_error_lexema_representation));
      last_syntax_error_message = NULL;
      VLO_NULLIFY (first_syntax_error_lexema_representation);
    }
}


/*
   FUNCTION NAME:   yyerror

   DESCRIPTION: The function is called by `yyparse' on syntax error.
                This function flushes saved syntax error message
                with the aid of `flush_syntax_error' and saves message
                given as parameter and position of current lexema (from
                `yylval') as position of message (see scanner).

   INPUT DATA:
       message                     message being saved

   GLOBALS READ:
       yylval                                    start internal representation
                                                 node or attributes of read
                                                 lexema (token)
       first_syntax_error_lexema_representation  representation of the first
                                                 syntax error lexema

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       last_syntax_error_message                 saved the most recent syntax
                                                 error message
       first_syntax_error_position               position of the saved message
       first_syntax_error_lexema_representation  representation of the first
                                                 syntax error lexema

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

yyerror (const char *message)
{
  flush_syntax_error ();
#ifdef DEBUG
  if (last_syntax_error_message != NULL
      || VLO_LENGTH (first_syntax_error_lexema_representation) != 0)
    abort ();
#endif
  last_syntax_error_message = message;
  switch (current_lexema_code)
    {
    case IDENTIFIER:
    case DIGIT_SEQUENCE:
    case UNSIGNED_REAL:
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`");
      first_syntax_error_position = FE_position (yylval.node);
      if (current_lexema_code == IDENTIFIER)
        VLO_ADD_STRING (first_syntax_error_lexema_representation,
                        FE_identifier_itself (yylval.node));
      else if (current_lexema_code == DIGIT_SEQUENCE)
        VLO_ADD_STRING (first_syntax_error_lexema_representation,
                        FE_integer_representation (yylval.node));
#ifdef DEBUG
      else if (current_lexema_code != UNSIGNED_REAL)
        abort ();
#endif
      else
        VLO_ADD_STRING (first_syntax_error_lexema_representation,
                        FE_real_representation (yylval.node));
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "'");
      break;
    case CHARACTER_STRING:
      first_syntax_error_position = FE_position (yylval.node);
      /* Remember that string can contain null and other unprintable
         characters. */
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "STRING");
      break;
    case PLUS:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`+'");
      break;
    case MINUS:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`-'");
      break;
    case STAR:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`*'");
      break;
    case SLASH:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`/'");
      break;
    case EQUAL:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`='");
      break;
    case LESS:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`<'");
      break;
    case GREATER:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`>'");
      break;
    case LEFT_BRACKET:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`['");
      break;
    case RIGHT_BRACKET:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`]'");
      break;
    case PERIOD:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`.'");
      break;
    case COMMA:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`,'");
      break;
    case COLON:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`:'");
      break;
    case SEMICOLON:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`;'");
      break;
    case ARROW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`^' or `@'");
      break;
    case LEFT_PARENTHESIS:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`('");
      break;
    case RIGHT_PARENTHESIS:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`)'");
      break;
    case UNEQUAL:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`<>'");
      break;
    case LESS_OR_EQUAL:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`<='");
      break;
    case GREATER_OR_EQUAL:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`>='");
      break;
    case ASSIGN:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`:='");
      break;
    case RANGE:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "`..'");
      break;
    case AND_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "AND");
      break;
    case ARRAY_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "ARRAY");
      break;
    case BEGIN_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "BEGIN");
      break;
    case CASE_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "CASE");
      break;
    case CONST_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "CONST");
      break;
    case DIV_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "DIV");
      break;
    case DO_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "DO");
      break;
    case DOWNTO_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "DOWNTO");
      break;
    case ELSE_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "ELSE");
      break;
    case END_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "END");
      break;
    case FILE_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "FILE");
      break;
    case FOR_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "FOR");
      break;
    case FUNCTION_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "FUNCTION");
      break;
    case GOTO_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "GOTO");
      break;
    case IF_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "IF");
      break;
    case IN_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "IN");
      break;
    case LABEL_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "LABEL");
      break;
    case MOD_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "MOD");
      break;
    case NIL_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "NIL");
      break;
    case NOT_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "NOT");
      break;
    case OF_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "OF");
      break;
    case OR_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "OR");
      break;
    case PACKED_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "PACKED");
      break;
    case PROCEDURE_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "PROCEDURE");
      break;
    case PROGRAM_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "PROGRAM");
      break;
    case RECORD_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "RECORD");
      break;
    case REPEAT_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "REPEAT");
      break;
    case SET_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "SET");
      break;
    case THEN_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "THEN");
      break;
    case TO_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "TO");
      break;
    case TYPE_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "TYPE");
      break;
    case UNTIL_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "UNTIL");
      break;
    case VAR_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "VAR");
      break;
    case WHILE_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "WHILE");
      break;
    case WITH_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "WITH");
      break;
    case ENTRY_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "ENTRY");
      break;
    case INCLUDE_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "INCLUDE");
      break;
    case SCALED_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "SCALED");
      break;
    case TASKDATA_KW:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "TASKDATA");
      break;
    case 0:
    case END_OF_INCLUDED_FILE:
      first_syntax_error_position = yylval.lexema_attribute.position;
      VLO_ADD_STRING (first_syntax_error_lexema_representation, "EOF");
      break;
    default:
      abort ();
    }
}


/*
   FUNCTION NAME:   syntax_error

   DESCRIPTION: The function rewrites saved syntax error message but
                does not change saved error lexema representation and saved
                position.

   INPUT DATA:
       *s            syntax error message

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       last_syntax_error_message   saved the most recent syntax error message

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static void
syntax_error (const char *s)
{
  last_syntax_error_message = s;
}


/*
   FUNCTION NAME:   finish_syntax_errors

   DESCRIPTION: The function finishes work with the abstract data (flushes
                saved syntax error message with the aid of function
                `flush_syntax_error').

   INPUT DATA:
       <None>

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       first_syntax_error_lexema_representation  representation of the first
                                                 syntax error lexema

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static void
finish_syntax_errors (void)
{
  flush_syntax_error ();
  VLO_DELETE (first_syntax_error_lexema_representation);
}



/* This page contains all external functions of parser. */

/*
   FUNCTION NAME:   initiate_parser

   DESCRIPTION: The function initiates all internal abstract data (the
                scanner and `syntax_errors').

   INPUT DATA:
       <None>

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
         The function must be called only once before any work with the
       abstract data.

   ANCHOR: <>
   SOURCE: <>
*/

void
initiate_parser (void)
{
  initiate_scanner ();
  initiate_syntax_errors ();
}


/*
   FUNCTION NAME:   start_parser_file

   DESCRIPTION: The function forces to tune parser onto new file
                (calls functions `flush_syntax_error' from abstract data
                `syntax_errors' and `start_scanner_file').

   INPUT DATA:
       new_file_name    name of file corresponding to new input stream
       error_position   position for fixing errors

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

void
start_parser_file (const char *new_file_name, position_t error_position)
{
  flush_syntax_error ();
  start_scanner_file (new_file_name, error_position);
}


/*
   FUNCTION NAME:   finish_parser_file

   DESCRIPTION: This function is called after processing lexema
                END_OF_INCLUDED_FILE (see above).  The function calls
                functions `finish_task_data_unit', `flush_syntax_error',
                and `finish_scanner_file'.

   INPUT DATA:
       <None>

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
       <None>

   ANCHOR: <>
   SOURCE: <>
*/

static void
finish_parser_file (void)
{
  finish_task_data_unit ();
  flush_syntax_error ();
  finish_scanner_file ();
}


/*
   FUNCTION NAME:   finish_parser

   DESCRIPTION: This function finishes all internal abstract data
                (the scanner and abstract data `syntax_errors').

   INPUT DATA:
       <None>

   GLOBALS READ:
       <None>

   OUTPUT DATA:
       <None>

   GLOBALS WRITTEN:
       <None>

   RETURN VALUE:
       <None>

   SPECIAL CONSIDERATION:
         Only call of function `initiate_parser' is possible
       immediately after this function call.

   ANCHOR: <>
   SOURCE: <>
*/

finish_parser (void)
{
  finish_scanner ();
  finish_syntax_errors ();
}
