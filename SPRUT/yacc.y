/*
   FILE NAME:   yacc.y

   TITLE:       Parser of SPRUT (internal representation description
                translator) described as YACC file

   DESCRIPTION: This file makes lexical, syntactic analysis
                (with possible syntactic error recovery) of one or more
                files and builds up internal representation of parsed internal
                representation description.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of parser work and errors of its usage.
   
*/

%{

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>
#include <stdio.h>

#include <string.h>
#include "vlobject.h"
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


/* Attributes of all YACC symbols (token and nonterminals) will be
   of given type. */

#define YYSTYPE IR_node_t

/* This macro value is used for correct calculation of current position in
   file when TAB character is processed. */

#define TAB_STOP 8

/* The following variables and forward definitions of functions are
   used in actions of YACC grammar. */

/* The following variable value is begin position of the last lexema returned
   by the function `yylex'. */

static position_t current_lexema_position;

/* The following variable value is flag of that the last list of
   immediate super types is list of basic immediate super types
   (i.e. this list does not start with comma -- see syntax). */

static int current_basic_super_types_flag;

static int file_has_been_already_processed (const char *new_file_name);

static void start_scanner_file (const char *new_file_name,
                                position_t error_position);

static void finish_scanner_file (void);

static void add_lexema_to_input_file (int lexema_code);

static void syntax_error (const char *s);

static void append_additional_code (IR_node_t additional_code);

static void append_type_list (IR_node_t type_list);

static void append_double_declaration (IR_node_t double_declaration);

static IR_node_t merge_and_set_field_declaration_part_and_class_double_fields
  (IR_node_t class_fields, IR_node_t skeleton_fields, IR_node_t other_fields);

static void process_node_type_list
  (IR_node_t last_node_type, int abstract_flag,
   IR_node_t first_super_type_list_element,
   IR_node_t last_super_type_list_element, int basic_super_types_flag,
   IR_node_t field_list);

static const char *full_extended_file_name  (const char *extend_identifier);

static void finish_parser_file (void);

%}

/*   To process included files (construction `%extend ...') the parser
   could call itself recursively.  But according to POSIX the utility
   YACC does not give such feature.  Therefore the parser does not make
   switching process of other source files.  The scanner executes all
   this work (see commentaries for scanner).  Such implementation
   requires enrichment of syntax which reflects that basic description
   part (with lexema `EXTENSION') may be after construction `%extend
   ....' and robust error recovery within a description part.
     To disable influence syntactic errors on fragments of file started
   with unique lexema (see rules for nonterminals
   `predefined_types_declaration' and `double_nodes_declaration')
   `yyerrok' is used.
     The implementation of processing construction `%extend ...'  by
   parser require robust error recovery within a description part, i.e.
   syntactic errors fixed in basic description part may not affect fixing
   syntactic errors in original description part.  Such robust error
   recovery is to be implemented also by using `yyerrok' (see rule for
   nonterminal basic_description_part).
     Also when function yyerror fixed too many syntactic errors it may
   skip up to end of file of current compilation unit.  But it should
   be remembered that all `error' constructions in rules must have
   token `END_OF_BASIC_FILE' as look ahead token set (this is achieved
   by additional error rules).  Otherwise the parser will move in
   cycles.
     See commentaries below for abstract data `syntax_errors'
   about used method of syntactic error reporting. */


/* The attributes of the following tokens are not defined and not used. */

%token PERCENTS COMMA COLON DOUBLE_COLON SEMICOLON
       EXTENSION END_OF_BASIC_FILE
       DOUBLE EXTEND LOCAL IMPORT EXPORT TYPE ROOT ABSTRACT CLASS SKELETON
       OTHER 

/* The attributes of the following tokens are defined and used. */

%token IDENTIFIER CODE_INSERTION EXPRESSION ADDITIONAL_C_CODE

%start description
%%

/* The attribute of the following nonterminal is not defined and not used. */

description :    {
                   IR_node_t description_part;

                   /* Create new description part. */
                   description_part = IR_create_node (IR_NM_description_part);
                   IR_set_position (description_part, current_position);
                   IR_set_description_part_level (description_part, 0);
                   IR_set_first_basic_description_part (description_part,
                                                        NULL);
                   IR_set_next_basic_description_part (description_part,
                                                       NULL);
                   IR_set_last_code_insertion (description_part, NULL);
                   IR_set_last_double_declaration (description_part, NULL);
                   IR_set_last_type (description_part, NULL);
                   IR_set_last_additional_code (description_part, NULL);
                   if (current_description_part != NULL)
                     {
                       IR_node_t last_basic_description_part;
                       IR_node_t current_part;
                       
                       last_basic_description_part = NULL;
                       for (current_part
                            = IR_first_basic_description_part
                            (current_description_part);
                            current_part != NULL;
                            current_part
                            = IR_next_basic_description_part (current_part))
                         last_basic_description_part = current_part;
                       if (last_basic_description_part == NULL)
                         IR_set_first_basic_description_part
                           (current_description_part, description_part);
                       else
                         IR_set_next_basic_description_part
                           (last_basic_description_part, description_part);
                     }
                   current_description_part = description_part;
                 }
              declaration_part  PERCENTS node_type_definition_list
              ADDITIONAL_C_CODE
                 {append_additional_code ($5);}
            ;

/* A symbol with attribute equal to node representing identifier
   in constructions `%extend ...' must before this nonterminal.
   The attribute of this nonterminal is not defined and not used. */

basic_description_part :
                       |  {
                            yyerrok;
                          }
                        EXTENSION
                          {
                            const char *file_name;
                            
                            file_name
                              = full_extended_file_name (IR_identifier_itself
                                                         ($0));
                            start_parser_file (file_name, IR_position ($0));
                            $$ = current_description_part;
                          }
                        description END_OF_BASIC_FILE
                          {
                            yyerrok;
                            finish_parser_file ();
                            IR_set_description_part_level
                              ($3,
                               IR_description_part_level
                               (current_description_part) + 1);
                            current_description_part = $3;
                          }
                       ;

/* A symbol with attribute equal to identifier in the first
  construction `%extend ...' (or NULL) in source file must before this
  nonterminal.  The attribute of the following nonterminal is not
  defined and not used. */

declaration_part
      :
      | declaration_part double_nodes_declaration
      | declaration_part EXTEND extend_identifier_list
      | declaration_part predefined_types_declaration
      | declaration_part code_insertion
           {
             if (IR_last_code_insertion (current_description_part) != NULL)
               {
                 IR_set_next_code_insertion
                   ($2, IR_next_code_insertion (IR_last_code_insertion
                                                (current_description_part)));
                 IR_set_next_code_insertion
                   (IR_last_code_insertion (current_description_part), $2);
               }
             else
               IR_set_next_code_insertion ($2, $2);
             IR_set_last_code_insertion (current_description_part, $2);
           }
      | declaration_part error
           {syntax_error ("syntax error in declaration");}
      ;

/* The attribute of this nonterminal is not defined and not used. */

extend_identifier_list : extend_identifier
                       | extend_identifier_list extend_identifier
                       ;

/* The attribute of this nonterminal is not defined and not used. */

extend_identifier
    : IDENTIFIER
        {
          /* The attribute of this action (`$$') is used in rule with
             nonterminal `basic_description_part' in the left hand
             side.  The value of the attribute is node representing
             identifier in construction `%extend ...'  in current
             source file. */
          const char *file_name;

          $$ = $1;
          file_name = full_extended_file_name (IR_identifier_itself ($1));
          if (!file_has_been_already_processed (file_name))
            /* It is first construction `%extend ...' with given
               identifier -- start parsing new description part. */
            add_lexema_to_input_file (EXTENSION);
          else if (v_flag)
            warning
              (IR_position ($1),
               "warning -- repeated extension of description `%s' is ignored",
               IR_identifier_itself ($1));
        }
      basic_description_part
;

/* Attribute of this nonterminal is node representing last of
  predefined type of cyclic list of predefined type in current
  description part. */

predefined_types_declaration
      : TYPE  {yyerrok;}
      | predefined_types_declaration IDENTIFIER
          {
            $$ = IR_create_node (IR_NM_predefined_type);
            IR_set_position ($$, IR_position ($2));
            IR_set_type_identifier ($$, $2);
            IR_set_next_type ($$, $$);
            /* Append cyclic list of one node to the list of current
               description part. */
            append_type_list ($$);
          }
      | predefined_types_declaration error
          {syntax_error ("syntax error in predefined type declaration");}
      ;

/* Attribute of this nonterminal is node representing last of
   identifier in double node type declaration of cyclic list of such
   identifiers in current description part. */

double_nodes_declaration
      : DOUBLE  {yyerrok;}
      | double_nodes_declaration any_node_type_name
          {
            $$ = IR_create_node (IR_NM_double_declaration);
            IR_set_position ($$, IR_position ($2));
            IR_set_double_declaration_identifier ($$, $2);
            /* Append node to the list of current description part. */
            append_double_declaration ($$);
          }
      | double_nodes_declaration error
          {
            syntax_error ("syntax error in double node type declaration");
          }
      ;

/* Attribute of this nonterminal is node representing corresponding
   local or export/import code. */

code_insertion : LOCAL CODE_INSERTION
                   {
                     $$ = IR_create_node (IR_NM_local_code);
                     IR_set_position ($$, IR_position ($2));
                     IR_set_code_itself ($$, $2);
                   }
               | IMPORT  CODE_INSERTION
                   {
                     $$ = IR_create_node (IR_NM_import_code);
                     IR_set_position ($$, IR_position ($2));
                     IR_set_code_itself ($$, $2);
                   }
               | EXPORT  CODE_INSERTION
                   {
                     $$ = IR_create_node (IR_NM_export_code);
                     IR_set_position ($$, IR_position ($2));
                     IR_set_code_itself ($$, $2);
                   }
               ;

/* The attribute of the following nonterminal is not defined and not
   used. */

node_type_definition_list
      : node_type_definition
      | error
          {
            syntax_error ("syntax error in node type definition");
          }
      | node_type_definition_list  SEMICOLON  node_type_definition
      ;

/* The attribute of the following nonterminal is not defined and not
   used. */

node_type_definition
  :
  | abstract_node_flag  node_type_identifier_list
    optional_immediate_super_type_list  class_field_definition_part
    skeleton_field_definition_part  other_field_definition_part
       {
         IR_node_t last_field;
         IR_node_t last_super_type_list_element;

         last_field
           = merge_and_set_field_declaration_part_and_class_double_fields
             ($4, $5, $6);
         last_super_type_list_element = $3;
         if ($3 != NULL)
           {
             /* Make uncyclic list. */
             $3 = IR_next_super_type_list_element ($3);
             IR_set_next_super_type_list_element (last_super_type_list_element,
                                                  NULL);
           }
         process_node_type_list
           ($2, $1 != NULL, $3, last_super_type_list_element,
            current_basic_super_types_flag, last_field);
       }
  ;

/* Any non-nil value of attribute of this nonterminal means that
   keyword abstract is present. */

abstract_node_flag :           {$$ = NULL;}
                   | ABSTRACT  {$$ = root_identifier;}
                   ;

/* Attribute of this nonterminal is node representing last of node
   type of cyclic list of node types in given node type identifier
   list. */

node_type_identifier_list : node_type_identifier
                              {  
                                IR_set_next_type ($1, $1);
                                $$ = $1;
                              }
                          | node_type_identifier_list COMMA
                              node_type_identifier
                              {  
                                assert ($1 != NULL);
                                IR_set_next_type ($3, IR_next_type ($1));
                                IR_set_next_type ($1, $3);
                                $$ = $3;
                              }
                          ;

/* Attribute of this nonterminal is node representing corresponding
   node type. */

node_type_identifier : IDENTIFIER
                         {
                           $$ = IR_create_node (IR_NM_node_type);
                           IR_set_position ($$, IR_position ($1));
                           IR_set_type_identifier ($$, $1);
                           IR_set_double_node_flag ($$, FALSE);
                           IR_set_last_field ($$, NULL);
                         }
                     ;

/* Attribute of this nonterminal is node representing last immediate
   super type list element of cyclic list of them.  There is
   additional attribute `current_basic_super_types_flag' of the
   following nonterminal. */

optional_immediate_super_type_list
  :
      {
        current_basic_super_types_flag = FALSE;
        $$ = NULL;
      }
  | immediate_super_type_list
      {
        $$ = $1;
      }
  ;

/* Attribute of this nonterminal is node representing last immediate
   super type list element of cyclic list of them.  There is
   additional attribute `current_basic_super_types_flag' of the
   following nonterminal. */

immediate_super_type_list
     : DOUBLE_COLON any_node_type_name
         {
           current_basic_super_types_flag = TRUE;
           $$ = IR_create_node (IR_NM_super_type_list_element);
           IR_set_position ($$, IR_position ($2));
           IR_set_immediate_super_type_identifier ($$, $2);
           /* Create cyclic list. */
           IR_set_next_super_type_list_element ($$, $$);
         }
     | DOUBLE_COLON COMMA any_node_type_name
         {
           current_basic_super_types_flag = FALSE;
           $$ = IR_create_node (IR_NM_super_type_list_element);
           IR_set_position ($$, IR_position ($3));
           IR_set_immediate_super_type_identifier ($$, $3);
           /* Create cyclic list. */
           IR_set_next_super_type_list_element ($$, $$);
         }
     | immediate_super_type_list COMMA any_node_type_name
         {
           assert ($1 != NULL);
           $$ = IR_create_node (IR_NM_super_type_list_element);
           IR_set_position ($$, IR_position ($3));
           IR_set_immediate_super_type_identifier ($$, $3);
           /* Add new element to the cyclic list. */
           IR_set_next_super_type_list_element
             ($$, IR_next_super_type_list_element ($1));
           IR_set_next_super_type_list_element ($1, $$);
         }
     ;

/* Attribute of this nonterminal is node representing given identifier. */
   
any_node_type_name : ROOT
                       {
                         /* Copy is needed because any identifier node
                            representing identifier occurrence (see
                            commentaries for node `identifier'). */
                         $$ = IR_copy_node (root_identifier);
                         IR_set_position ($$, current_lexema_position);
                       }
                   | IDENTIFIER   {$$ = $1;}
                   ;

/* Attribute of this nonterminal is node representing last field, action
   or constraint of cyclic list of class fields (or NULL) in given field
   definition list. */

class_field_definition_part :                              {$$ = NULL;}
                            | CLASS field_definition_list  {$$ = $2;}
                            ;

/* Attribute of this nonterminal is node representing last field, action
   or constraint of cyclic list of skeleton fields (or NULL) in given field
   definition list. */

skeleton_field_definition_part :                                {$$ = NULL;}
                               | SKELETON field_definition_list {$$ = $2;}
                               ;

/* Attribute of this nonterminal is node representing last field, action
   or constraint of cyclic list of other fields (or NULL) in given field
   definition list. */

other_field_definition_part :                              {$$ = NULL;}
                            | OTHER field_definition_list  {$$ = $2;}
                            ;

/* Attribute of this nonterminal is node representing last field, action
   or constraint of cyclic list of them in given field definition list. */

field_definition_list :                                         {$$ = NULL;}
                      | field_definition_list field_definition
                          {
                            if ($1 != NULL)
                              {
                                $$ = IR_next_field ($2);
                                IR_set_next_field ($2, IR_next_field ($1));
                                IR_set_next_field ($1, $$);
                              }
                            $$ = $2;
                          }
                      | field_definition_list constraint_or_action
                          {  
                            if ($1 != NULL)
                              {
                                IR_set_next_field ($2, IR_next_field ($1));
                                IR_set_next_field ($1, $2);
                              }
                            else
                              IR_set_next_field ($2, $2);
                            $$ = $2;
                          }
                      | field_definition_list error
                           {
                             syntax_error ("syntax error in field definition");
                             $$ = $1;
                           }
                      ;

/* Attribute of this nonterminal is node representing constraint or action. */

constraint_or_action : constraint  {$$ = $1;}
                     | action      {$$ = $1;}
                     ;

/* Attribute of this nonterminal is node representing last field of cyclic
   list of them in given field definition. */

field_definition : field_identifier_list  COLON  optional_double
                     any_node_type_name
                     {
                       $$ = $1;
                       assert ($1 != NULL);
                       /* Set up `double_field_flag' for all fields
                          of the list. */
                       do
                         {
                           IR_set_double_field_flag ($1, $3 != NULL);
                           IR_set_field_type_identifier ($1, $4);
                           $1 = IR_next_field ($1);
                         }
                       while ($1 != $$);
                     }
                 ;

/* Any non-nill value of attribute of this nonterminal means that keyword
   double is present. */

optional_double :          {$$ = NULL;}
                | DOUBLE   {$$ = root_identifier;}
                ;

/* Attribute of this nonterminal is node representing constraint. */

constraint : EXPRESSION
               {
                 $$ = IR_create_node (IR_NM_constraint);
                 IR_set_position ($$, IR_position ($1));
                 IR_set_constraint_code ($$, $1);
               }
           ;

/* Attribute of this nonterminal is node representing action. */

action : CODE_INSERTION
           {
             $$ = IR_create_node (IR_NM_action);
             IR_set_position ($$, IR_position ($1));
             IR_set_action_code ($$, $1);
           }
       ;

/* Attribute of this nonterminal is node representing last field of cyclic
   list of fields in given field identifier list. */

field_identifier_list : field_identifier
                          {
                            IR_set_next_field ($1, $1);
                            $$ = $1;
                          }
                      | field_identifier_list  COMMA  field_identifier
                          {  
                            assert ($1 != NULL);
                            IR_set_next_field ($3, IR_next_field ($1));
                            IR_set_next_field ($1, $3);
                            $$ = $3;
                          }
                      ;

/* Attribute of this nonterminal is node representing field. */

field_identifier : IDENTIFIER
                     {
                       $$ = IR_create_node (IR_NM_field);
                       IR_set_position ($$, IR_position ($1));
                       IR_set_field_identifier ($$, $1);
                     }
                 ;

/* Two following rules are necessary to guarantee that token
   `END_OF_BASIC_FILE' is not skipped in error rules above and results
   in their reducing.  Remember that any addition of error rule to the
   grammar may require addition of such rules here! */

/* There are also rules for given nonterminal upper.  This rule covers
   also errors in `predefined_types_declaration' and
   `double_nodes_declaration'. */

declaration_part : declaration_part  END_OF_BASIC_FILE
                     {
#ifndef YYRECOVERING
#define YYRECOVERING() (!!yyerrflag)
#endif
                       yychar = END_OF_BASIC_FILE;
                       if (!YYRECOVERING ())
                         yyerror ("error in description part");
                       else
                         syntax_error ("error in description part");
                     }
                 ;

/* There are also rules for given nonterminal upper. */

field_definition_list : field_definition_list END_OF_BASIC_FILE
                          {
                            yychar = END_OF_BASIC_FILE;
#ifndef YYRECOVERING
#define YYRECOVERING() (!!yyerrflag)
#endif
                            if (!YYRECOVERING ())
                              yyerror ("error in description part");
                            else
                              syntax_error ("error in description part");
                          }
                      ;

%%



/* This page contains functions used only in actions of YACC grammar. */

/* The following function appends given additional code to the cyclic
   list of all additional codes of current description part (it can be
   the only one in given description part). */

static void
append_additional_code (IR_node_t additional_code)
{
  if (IR_last_additional_code (current_description_part) != NULL)
    {
      IR_set_next_additional_code
        (additional_code,
         IR_next_additional_code (IR_last_additional_code
                                  (current_description_part)));
      IR_set_next_additional_code
        (IR_last_additional_code (current_description_part), additional_code);
    }
  else
    IR_set_next_additional_code (additional_code, additional_code);
  IR_set_last_additional_code (current_description_part, additional_code);
}

/* The following function appends cyclic list of node types or
   predefined types to the cyclic list of node types and predefined
   types of current description part. */

static void
append_type_list (IR_node_t last_type)
{
  IR_node_t first_appended_type;

  if (IR_last_type (current_description_part) != NULL)
    {
      first_appended_type = IR_next_type (last_type);
      IR_set_next_type
        (last_type, IR_next_type (IR_last_type (current_description_part)));
      IR_set_next_type (IR_last_type (current_description_part),
                        first_appended_type);
    }
  IR_set_last_type (current_description_part, last_type);
}

/* The function appends given double declaration to the cyclic list of
   all double declarations of current description part. */

static void
append_double_declaration (IR_node_t double_declaration)
{
  if (IR_last_double_declaration (current_description_part) != NULL)
    {
      IR_set_next_double_declaration
        (double_declaration,
         IR_next_double_declaration (IR_last_double_declaration
                                     (current_description_part)));
      IR_set_next_double_declaration (IR_last_double_declaration
                                      (current_description_part),
                                      double_declaration);
    }
  else
    IR_set_next_double_declaration (double_declaration, double_declaration);
  IR_set_last_double_declaration (current_description_part,
                                  double_declaration);
}

/* The following function sets up field `declaration_part' for all
   nodes representing fields in given cyclic list.  The function also
   checks up that class fields are not double fields (see commentaries
   for `double_field_flag').  If the condition is violated then error
   is fixed and corresponding `double_field_flag' is set up to FALSE. */

static void
set_field_declaration_part_and_class_double_fields
  (IR_node_t last_field, declaration_part_t declaration_part)
{
  IR_node_t current_field;

  current_field = last_field;
  if (current_field != NULL)
    do
      {
        IR_set_declaration_part (current_field, declaration_part);
        if (IR_NODE_MODE (current_field) == IR_NM_field
            && IR_double_field_flag (current_field)
            && declaration_part == DP_CLASS)
          {
            error (FALSE, IR_position (current_field),
                   "class field `%s' is double",
                   IR_identifier_itself (IR_field_identifier (current_field)));
            IR_set_double_field_flag (current_field, FALSE);
          }
        current_field = IR_next_field (current_field);
      }
    while (current_field != last_field);
}

/* The following function calls function
   `set_field_declaration_part_and_class_double_fields' for each list
   of three lists (of class fields, skeleton fields, and other
   fields).  After that the function merges these lists.  The function
   returns node (may be NULL) representing last field of the result
   list. */

static IR_node_t
merge_and_set_field_declaration_part_and_class_double_fields
  (IR_node_t class_fields, IR_node_t skeleton_fields, IR_node_t other_fields)
{
  IR_node_t first_field;
  IR_node_t last_field;

  set_field_declaration_part_and_class_double_fields (other_fields, DP_OTHER);
  set_field_declaration_part_and_class_double_fields (class_fields, DP_CLASS);
  set_field_declaration_part_and_class_double_fields (skeleton_fields,
                                                      DP_SKELETON);
  last_field = class_fields;
  if (last_field == NULL)
    last_field = skeleton_fields;
  else if (skeleton_fields != NULL)
    {
      first_field = IR_next_field (skeleton_fields);
      IR_set_next_field (skeleton_fields, IR_next_field (last_field));
      IR_set_next_field (last_field, first_field);
    }
  if (last_field == NULL)
    last_field = other_fields;
  else if (other_fields != NULL)
    {
      first_field = IR_next_field (other_fields);
      IR_set_next_field (other_fields, IR_next_field (last_field));
      IR_set_next_field (last_field, first_field);
    }
  return last_field;
}

/* The following function makes copy of given cyclic field (and action
   and constraints) list.  The function returns node (may be NULL)
   representing last field (or action or constraints) of the result
   (new) list. */

static IR_node_t
copy_field_list (IR_node_t field_list)
{
  IR_node_t current_field;
  IR_node_t current_new_field;
  IR_node_t new_field;
  IR_node_t result;

  if (field_list == NULL)
    return NULL;
  current_new_field = result = IR_copy_node (field_list);
  IR_set_next_field (result, result);
  for (current_field = field_list;;)
    {
      current_field = IR_next_field (current_field);
      if (current_field == field_list)
        break;
      new_field  = IR_copy_node (current_field);
      IR_set_next_field (new_field, IR_next_field (current_new_field));
      IR_set_next_field (current_new_field, new_field);
      current_new_field = new_field;
    }
  return result;
}

/* The following function sets up fields `abstract_flag',
   `first_super_type_list_element', `last_super_type_list_element',
   and `last_field' for all nodes representing node types in given
   cyclic list.  If the node type list contains more one node type the
   copy of fields (and action and constraints) is made for each non
   last node type of the list.  After that function `append_type_list'
   is called for given node type list. */

static void
process_node_type_list
  (IR_node_t last_node_type, int abstract_flag,
   IR_node_t first_super_type_list_element,
   IR_node_t last_super_type_list_element, int basic_super_types_flag,
   IR_node_t last_field)
{
  IR_node_t current_node_type;

  current_node_type = last_node_type;
  if (current_node_type != NULL)
    do
      {
        assert (IR_NODE_MODE (current_node_type) == IR_NM_node_type);
        IR_set_abstract_flag (current_node_type, abstract_flag);
        IR_set_first_super_type_list_element (current_node_type,
                                              first_super_type_list_element);
        IR_set_last_super_type_list_element (current_node_type,
                                             last_super_type_list_element);
        IR_set_basic_super_types_flag (current_node_type,
                                       basic_super_types_flag);
        IR_set_last_field (current_node_type,
                           (current_node_type == last_node_type
                            ? last_field : copy_field_list (last_field)));
        current_node_type = IR_next_type (current_node_type);
      }
  while (current_node_type != last_node_type);
  append_type_list (last_node_type);
}



/* This page contains abstract data `input_stream_stack'.  This
   abstract data opens and closes included files saves and restores
   scanner states corresponding to processed files. */


/* The following structure describes current input stream and scanner
   state and is used for saving and restoring input stream and scanner
   state to process included files. */

struct input_stream_state
{
  /* The following member is defined only for `current_input_stream_state'. */
  FILE *input_file;
  /* File name of given input stream. */
  const char *input_file_name;
  /* Current position in file corresponding to given input stream.
     The following member is defined only for structures in the input
     stream stack. */
  long input_file_position;
  /* The following member contains parameter value of the function
     `add_lexema_to_input_file' after immediately call of this function or
     -1 otherwise. */
  int uninput_lexema_code;
  /* The following member is TRUE if the first `%%' in given input stream
     was already processed or FALSE otherwise. */
  int it_is_first_percents;
  /* The following member is TRUE if the ADDITIONAL_CODE lexema was already
     returned by the scanner or FALSE otherwise.  Remember that lexema
     ADDITIONAL_CODE is always returned by the scanner
     even if second `%%' in the input stream is absent. */
  int additional_c_code_was_returned;
};

/* The following structure contains all current input stream and
   scanner state.  If the member `input_file' value is NULL
   then input stream is not defined. */

static struct input_stream_state current_input_stream_state;

/* All input stream stack is implemented by variable length object.
   See package `vl-object'. */

static vlo_t input_stream_stack;

/* The following function creates empty input stream stack and
   initiates current input stream and scanner state as undefined.  The
   function must be called only once before any work with the input
   stream stack. */

static void
initiate_input_stream_stack (void)
{
  VLO_CREATE (input_stream_stack, 0);
  current_input_stream_state.input_file = NULL;
  current_input_stream_state.uninput_lexema_code = (-1);
  current_input_stream_state.it_is_first_percents = TRUE;
  current_input_stream_state.additional_c_code_was_returned = FALSE;
}

/* The following function deletes the input stream stack and closes
   current input file if current input stream state is defined.  Only
   call of function `initiate_input_stream_stack' is possible
   immediately after this function call. */

static void
finish_input_stream_stack (void)
{
  VLO_DELETE (input_stream_stack);
  if (current_input_stream_state.input_file != NULL)
    fclose (current_input_stream_state.input_file);
}

/* The following function returns height of input stream stack,
   i.e. current file inclusion level. */

static int
input_stream_stack_height (void)
{
  return VLO_LENGTH (input_stream_stack) / sizeof (struct input_stream_state);
}

/* The following value is used if previous_char does not contain an
   input char */

#define EMPTY_PREVIOUS_CHAR (-2000)

/* The following variable is used for storing '\r'. */

static int previous_char;

/* The following function saves current input stream and scanner state
   (if it is defined) in the stack, closes file corresponding to
   current input stream, opens file corresponding to new input stream,
   and sets up new current input stream and scanner state.  The
   function also checks up absence of loop of file inclusions.  The
   function reports errors if the loop exists or new file is not
   opened. */

static void
push_current_input_stream (const char *new_file_name,
                           position_t error_position)
{
  int i;

  if (current_input_stream_state.input_file != NULL)
    {
      current_input_stream_state.input_file_position
        = ftell (current_input_stream_state.input_file);
      fclose (current_input_stream_state.input_file);
      current_input_stream_state.input_file = NULL;
      VLO_ADD_MEMORY (input_stream_stack, &current_input_stream_state,
                      sizeof (struct input_stream_state));
      for (i = 0; i < input_stream_stack_height (); i++)
        if (strcmp
            (((struct input_stream_state *) VLO_BEGIN (input_stream_stack))
             [i].input_file_name, new_file_name) == 0)
          error (TRUE, error_position,
                 "fatal error -- cycle on inclusion of file `%s'", 
                  new_file_name);
    }
  current_input_stream_state.input_file_name = new_file_name;
  current_input_stream_state.input_file = fopen (new_file_name, "rb");
  previous_char = EMPTY_PREVIOUS_CHAR;
  if (current_input_stream_state.input_file == NULL)
    system_error (TRUE, error_position, "fatal error -- `%s': ", 
                  current_input_stream_state.input_file_name);
  start_file_position (current_input_stream_state.input_file_name);
  current_input_stream_state.uninput_lexema_code = (-1);
  current_input_stream_state.it_is_first_percents = TRUE;
  current_input_stream_state.additional_c_code_was_returned = FALSE;
}

/* The following function closes file corresponding to current input
   stream (it must be defined), reopens file corresponding to previous
   input stream and restores previous input stream and scanner state.
   The function can fix error if the file is not reopened. */

static void
pop_input_stream_stack (void)
{
  assert (current_input_stream_state.input_file != NULL);
  fclose (current_input_stream_state.input_file);
  current_input_stream_state.input_file = NULL;
  if (input_stream_stack_height () != 0)
    {
      current_input_stream_state
	= (((struct input_stream_state *) VLO_BEGIN (input_stream_stack))
	   [input_stream_stack_height () - 1]);
      VLO_SHORTEN (input_stream_stack, sizeof (struct input_stream_state));
      current_input_stream_state.input_file
	= fopen (current_input_stream_state.input_file_name, "rb");
      if (current_input_stream_state.input_file == NULL
	  || fseek (current_input_stream_state.input_file,
		    current_input_stream_state.input_file_position, 0) != 0)
	system_error (TRUE, no_position,
		      "fatal error -- repeated opening file `%s': ", 
		      current_input_stream_state.input_file_name);
      finish_file_position ();
    }
}

/* The following field returns directory name of given file name. */

static const char *
file_name_directory (const char *file_name)
{
  const char *last_slash;
  const char *current_char_ptr;
  const char *result;

  assert (file_name != NULL);
  for (current_char_ptr = file_name, last_slash = NULL;
       *current_char_ptr != '\0'; current_char_ptr++)
#ifdef WIN32
    if (*current_char_ptr == '/' || *current_char_ptr == '\\')
#else
    if (*current_char_ptr == '/')
#endif
      last_slash = current_char_ptr;
  if (last_slash == NULL)
    return ""; /* current directory */
  else
    {
      IR_TOP_ADD_MEMORY (file_name, last_slash - file_name + 1);
      IR_TOP_ADD_BYTE ('\0');
      result = IR_TOP_BEGIN ();
      IR_TOP_FINISH ();
      return result;
    }
}

/* The following field returns full name of file with given directory
   name, file name itself, and suffix. */

static const char *
file_path_name (const char *directory_name, const char *file_name,
                const char *file_suffix)
{
  const char *result;

  assert (directory_name != NULL);
  IR_TOP_ADD_STRING (directory_name);
  if (strlen (directory_name) != 0
#ifdef WIN32
      && directory_name [strlen (directory_name) - 1] != '/'
      && directory_name [strlen (directory_name) - 1] != '\\')
    IR_TOP_ADD_STRING ("\\");
#else
      && directory_name [strlen (directory_name) - 1] != '/')
    IR_TOP_ADD_STRING ("/");
#endif
  IR_TOP_ADD_STRING (file_name);
  IR_TOP_ADD_STRING (file_suffix);
  result = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  return result;
}

/* The following function returns full file name of extended
   specification with given identifier.  To make this functions
   searches for files in current directory (i.e. directory in which
   source file with the corresponding extend-clause is placed) and in
   directories given in the command line of the SPRUT.  If the extended
   specification file is not found the function returns the extended
   specification file name in the current directory. */

static const char *
full_extended_file_name (const char *extend_identifier)
{
  const char *current_directory_name;
  const char *real_file_name;
  const char *file_name;
  const char **extended_specification_directory_ptr;
  FILE *current_file;

  current_directory_name
    = file_name_directory (current_input_stream_state.input_file_name);
  real_file_name = file_path_name (current_directory_name,
                                   extend_identifier,
                                   STANDARD_INPUT_FILE_SUFFIX);
  current_file = fopen (real_file_name, "rb");
  if (current_file == NULL)
    for (extended_specification_directory_ptr
         = extended_specification_directories;
         *extended_specification_directory_ptr != NULL;
         extended_specification_directory_ptr++)
      {
        file_name = file_path_name (*extended_specification_directory_ptr,
                                    extend_identifier,
                                    STANDARD_INPUT_FILE_SUFFIX);
        current_file = fopen (file_name, "rb");
        if (current_file != NULL)
          {
            real_file_name = file_name;
            break;
          }
      }
  if (current_file != NULL && fclose (current_file) == EOF)
    system_error (TRUE, no_position, "fatal error -- `%s': ", real_file_name);
  return real_file_name;
}



/*   This page contains abstract data `scanner'.  This abstract data
   divides input stream characters on tokens (lexemas).

     There is requirement of back control (from parser to scanner).
   This back control is needed to switch to processing new file after
   recognizing identifier in construction `%extend ...'.  After
   recognizing the first identifier the parser is to tell scanner that
   the next lexema must be EXTENSION.  After getting such lexema the
   parser is switched correctly onto processing new file.
    
     When EOF is read the scanner returns 0 only when input stream
   stack is empty, otherwise returns lexema END_OF_BASIC_FILE.  */

/* The following function returns lexical code of corresponding
   keyword or 0 if given identifier is not keyword.  Fast search is
   implemented by minimal pruned O-trie forest. */

static int
find_keyword (const char *str, int length)
{
  switch (length)
    {
    case 4:
      switch (*str)
        {
        case 'r':
          return (strcmp (str, "root") == 0 ? ROOT : 0);
        case 't':
          return (strcmp (str, "type") == 0 ? TYPE : 0);
        default:
          return 0;
        }
    case 5:
      switch (*str)
        {
        case 'c':
          return (strcmp (str, "class") == 0 ? CLASS : 0);
        case 'l':
          return (strcmp (str, "local") == 0 ? LOCAL : 0);
        case 'o':
          return (strcmp (str, "other") == 0 ? OTHER : 0);
        default:
          return 0;
        }
    case 6:
      switch (str[2])
        {
        case 'u':
          return (strcmp (str, "double") == 0 ? DOUBLE : 0);
        case 't':
          return (strcmp (str, "extend") == 0 ? EXTEND : 0);
        case 'p':
          if (*str == 'e')
            return (strcmp (str, "export") == 0 ? EXPORT : 0);
          else
            return (strcmp (str, "import") == 0 ? IMPORT : 0);
        default:
          return 0;
        }
    case 8:
      switch (*str)
        {
        case 'a':
          return (strcmp (str, "abstract") == 0 ? ABSTRACT : 0);
        case 's':
          return (strcmp (str, "skeleton") == 0 ? SKELETON : 0);
        default:
          return 0;
        }
    default:
      return 0;
    }
}

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

/* The following function skips C commentary.  The function returns
   TRUE if no error (EOF) was fixed, otherwise FALSE.  Current
   position corresponds to `*' after commentary start ('/') before the
   function call.  Current_position corresponds to first character
   after skipped commentary ('/') or EOF after the function call. */

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

/* The following function skips C string code (e.g `a', `\n', or
   `\077').  The function also stores skipped characters onto top
   internal representation object.  The function returns TRUE if no
   error (EOF or incorrect new line) was fixed, otherwise FALSE. The
   value of parameter `correct_new_line' can be used for fixing errors
   when character constant is processed.  It is supposed that the
   current character is not end marker (string or character constant).
   Current position corresponds to parameter `input_char' before the
   function call.  Current_position corresponds to first character
   after skipped valid character code or EOF (or incorrect new line)
   after the function call. */

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

/* The following function skips C character constant by using function
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

/* The following function skips C string constant by using function
   `skip_string_code'.  The function may fix error in the C string
   constant.  The function also stores skipped characters onto top
   internal representation object.  Current position corresponds to
   first character after '\"' (which was already read) before the
   function call.  Current_position corresponds to to first character
   after '\"' if error was not fixed after the function call. */

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

/* This function is major scanner function.  The function recognizes
   next source lexema from the input file, returns its code, modifies
   variable current_position so that its value is equal to position of
   the current character in the current input file and sets up
   variable `current_lexema_position' so that its value is equal to
   start position of the returned lexema, creates corresponding
   internal representation node (if it is needed) and sets up yylval
   to the node.  The function skips all white spaces and commentaries
   and fixes the most of lexical errors.  The function returns read
   token code.  If member `uninput_lexema_code' of
   `current_input_stream_state' is not negative the function returns
   the member value instead of code of lexema in input file.  When EOF
   is read the scanner returns ADDITIONAL_CODE if
   `additional_c_code_was_returned' of `current_input_stream_state' is
   FALSE, otherwise 0 only when input stream stack is empty, otherwise
   returns lexema END_OF_BASIC_FILE. */

int
yylex (void)
{
  int input_char;
  int number_of_successive_error_characters;
  
  if (current_input_stream_state.uninput_lexema_code >= 0)
    {
      int result;
      
      current_lexema_position = current_position;
      result = current_input_stream_state.uninput_lexema_code;
      current_input_stream_state.uninput_lexema_code = (-1);
      return result;
    }
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
              ungetch (input_char, current_input_stream_state.input_file);
              current_input_stream_state.additional_c_code_was_returned = TRUE;
              yylval = IR_create_node (IR_NM_additional_code);
              IR_set_position (yylval, current_lexema_position);
              IR_TOP_ADD_BYTE ('\0');
              IR_set_additional_code_itself (yylval, IR_TOP_BEGIN ());
              IR_TOP_FINISH ();
              return ADDITIONAL_C_CODE;
            }
          else
            return (input_stream_stack_height () == 0 ? 0 : END_OF_BASIC_FILE);
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
        case ',':
          current_lexema_position = current_position;
          current_position.column_number++;
          return COMMA;
        case ';':
          current_lexema_position = current_position;
          current_position.column_number++;
          return SEMICOLON;
        case ':':
          current_lexema_position = current_position;
          current_position.column_number++;
          input_char = getch (current_input_stream_state.input_file);
          if (input_char == ':')
            {
              current_position.column_number++;
              return DOUBLE_COLON;
            }
          else
            {
              ungetch (input_char, current_input_stream_state.input_file);
              return COLON;
            }
        case '[':
          /* flow-through */
        case '{':
          {
            int start_char = input_char;
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
                  case '[':
                    /* flow-through */
                  case '{':
                    current_position.column_number++;
                    if (start_char == input_char)
                      bracket_level++;
                    break;
                  case ']':
                    /* flow-through */
                  case '}':
                    current_position.column_number++;
                    if ((start_char == '{' && input_char == '}')
                        || (start_char == '[' && input_char == ']'))
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
                     (start_char == '[' ? "constraint end is absent"
                      : "C code insertion end is absent"));
            IR_TOP_ADD_BYTE ('\0');
            code_itself = IR_TOP_BEGIN ();
            IR_TOP_FINISH ();
            yylval = IR_create_node (start_char == '[' ? IR_NM_expression
                                  : IR_NM_code_insertion);
            IR_set_position (yylval, current_lexema_position);
            if (start_char == '[')
              IR_set_expression_itself (yylval, code_itself);
            else
              IR_set_code_insertion_itself (yylval, code_itself);
            return (start_char == '[' ? EXPRESSION : CODE_INSERTION);
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
                  yylval = IR_create_node (IR_NM_additional_code);
                  IR_set_position (yylval, current_lexema_position);
                  IR_set_additional_code_itself (yylval,
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
                         "unknown keyword `%%%s'",
                         IR_TOP_BEGIN ());
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
              identifier_itself = insert_identifier (IR_TOP_BEGIN ());
              if (identifier_itself == (char *) IR_TOP_BEGIN ())
                IR_TOP_FINISH ();
              else
                IR_TOP_NULLIFY ();
              yylval = IR_create_node (IR_NM_identifier);
              IR_set_position (yylval, current_lexema_position);
              IR_set_identifier_itself (yylval, identifier_itself);
              return IDENTIFIER;
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

/* The following variable contains pointers to names of files whose
   scanning already finished. */

static vlo_t scanned_file_names;

/* The following function initiates internal state of the scanner
   (more accurately initiates internal state of abstract data
   `input_stream_stack').  The function must be called only once
   before any work with the scanner. */

static void
initiate_scanner (void)
{
  VLO_CREATE (scanned_file_names, 30);
  initiate_input_stream_stack ();
}

/* The following function returns TRUE if file with given name is
   already processed.  The function returns TRUE if file with given
   name is already processed, FALSE otherwise. */

static int
file_has_been_already_processed (const char *new_file_name)
{
  int i;

  for (i = 0; i < VLO_LENGTH (scanned_file_names) / sizeof (char *); i++)
    if (strcmp (((char **) VLO_BEGIN (scanned_file_names)) [i],
                new_file_name) == 0)
      return TRUE;
  return FALSE;
}

/* The following function is called to tune the scanner on input
   stream from given file.  The function is needed for nested
   procession of included files. The function saves scanner state
   concerning current input stream with the aid of abstract data
   `input_stream_stack'.  Previous scanner state is restored after
   processing end of file in current input stream (see parser and also
   function `yylex'). */

static void
start_scanner_file (const char *new_file_name, position_t error_position)
{
  push_current_input_stream (new_file_name, error_position);
}

/* The following function restores scanner state concerning previous
   input stream with the aid of abstract data `input_stream_stack'. */

static void
finish_scanner_file (void)
{
  if (!file_has_been_already_processed (current_input_stream_state
                                        .input_file_name))
    VLO_ADD_MEMORY (scanned_file_names,
                    &current_input_stream_state.input_file_name,
                    sizeof (char *));
  pop_input_stream_stack ();
}

/* The following function is called from the parser and points out
   that lexema given as parameter will be returned by following
   function `yylex' call. */

static void
add_lexema_to_input_file (int lexema_code)
{
  assert (lexema_code >= 0);
  current_input_stream_state.uninput_lexema_code = lexema_code;
}

/* The function frees all memory allocated during the scanner work
   (more accurately frees memory allocated by abstract data
   `input_stream_stack').  Only call of function `initiate_scanner' is
   possible immediately after this function call. */

static void
finish_scanner (void)
{
  VLO_DELETE (scanned_file_names);
  finish_input_stream_stack ();
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

/* The following function initiates the abstract data as without saved
   syntax error message.  The function must be called only once before
   any work with the abstract data. */

static void
initiate_syntax_errors (void)
{
  last_syntax_error_message = NULL;
}

/* The following function flushes saved syntax error message, if
   any. */

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

/* The following function is called by `yyparse' on syntax error.
   This function flushes saved syntax error message with the aid of
   `flush_syntax_error' and saves message given as parameter and
   `current_lexema position' as its position (see scanner). */

yyerror (const char *message)
{
  flush_syntax_error ();
  last_syntax_error_message = message;
  first_syntax_error_position = current_lexema_position;
  return 0;
}

/* The following function rewrites saved syntax error message but does
   not change saved position. */

static void
syntax_error (const char *s)
{
  last_syntax_error_message = s;
}

/* The following function finishes work with the abstract data
   (flushes saved syntax error message with the aid of function
   `flush_syntax_error'). */

static void
finish_syntax_errors (void)
{
  flush_syntax_error ();
}



/* This page contains all external functions of parser. */

/* The following function initiates all internal abstract data (the
   scanner and `syntax_errors').  The function must be called only
   once before any work with the abstract data. */

void
initiate_parser (void)
{
  initiate_scanner ();
  initiate_syntax_errors ();
}

/* The following function forces to tune parser onto new file (calls
   functions `flush_syntax_error' from abstract data `syntax_errors'
   and `start_scanner_file'). */

void
start_parser_file (const char *new_file_name, position_t error_position)
{
  flush_syntax_error ();
  start_scanner_file (new_file_name, error_position);
}

/* This function is called after processing lexema END_OF_BASIC_FILE
   (see above).  The function calls functions `flush_syntax_error' and
   `finish_scanner_file'. */

static void
finish_parser_file (void)
{
  flush_syntax_error ();
  finish_scanner_file ();
}

/* This function finishes all internal abstract data (the scanner and
   abstract data `syntax_errors').  Only call of function
   `initiate_parser' is possible immediately after this function
   call. */

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
