/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of interpreter of DINO.

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

%{

#include "d_common.h"
#include "d_ir.h"
#include "d_yacc.h"

/* The value of following var is first program stmt
   (see member stmt_list in nodes).  The value may be NULL:
   it means empty program. */
IR_node_t first_program_stmt;

extern int yylex (void);
static int yyerror (const char *message);

static IR_node_t merge_friend_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_friend_list (IR_node_t list);

static IR_node_t merge_use_item_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_use_item_list (IR_node_t list);

static IR_node_t merge_exception_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_exception_list (IR_node_t list);

static IR_node_t
process_var_decl (access_val_t access, IR_node_t ident,
                  int val_flag, IR_node_t obj_block, IR_node_t expr,
		  position_t expr_pos, IR_node_mode_t assign);
static IR_node_t
process_var_pattern (access_val_t access, IR_node_t pattern, int val_flag,
		     IR_node_t expr, position_t expr_pos,
		     IR_node_mode_t assign);

static void process_header (int, IR_node_t, IR_node_t);
static IR_node_t process_formal_parameters (IR_node_t, IR_node_t);
static IR_node_t process_header_block (IR_node_t, IR_node_t, hint_val_t);

static hint_val_t get_hint (IR_node_t);

static IR_node_t merge_additional_stmts (IR_node_t);

static IR_node_t process_obj_header (IR_node_t);
static IR_node_t process_obj_block (IR_node_t, IR_node_t, IR_node_t, access_val_t);
static IR_node_t process_fun_start (IR_node_t, int, access_val_t);
static IR_node_t create_except_class (IR_node_t before_list, IR_node_t expr);
static IR_node_t create_catch_block (position_t pos);
static void finish_catch_block (IR_node_t catch_block, IR_node_t block,
				IR_node_t excepts, IR_node_t friend_list);
static IR_node_t create_try_expr (IR_node_t try_block, IR_node_t stmt,
				  IR_node_t excepts,
				  position_t lpar_pos, position_t rpar_pos);

/* The following vars are used by yacc analyzer. */

/* True when we can process typed statements in REPL.  */
static int repl_process_flag;

/* Pointer to block node which opens current scope. */
static IR_node_t current_scope;

/* Pointer to cycle list of stmts which should be included before the
   current stmt.  They are generated from anonymous
   functions/class/threads/classes in the current stmt.  */
static IR_node_t additional_stmts;

/* This var is used as second attribute of nonterminal actual_parameters.
   Its value is source pos of corresponding left parenthesis. Its
   value is to be defined only at rule end because of nesting. */
static position_t actual_parameters_construction_pos;

/* This var is used as second attribute of nonterminal formal_parameters.
   Its value is flag of variable number parameters (usage of ...). */
static int formal_parameter_args_flag;

static const char *get_full_file_and_encoding_name (IR_node_t fname,
						    const char **encoding);

static IR_node_t get_new_ident (position_t);

static void add_lexema_to_file (int lexema_code);
static void finish_scanner_file (void);

static void start_block (void);
static void finish_block (void);
static int add_include_file (IR_node_t fname);

static int repl_can_process_p (void);

%}

%union
  {
    IR_node_t pointer;
    position_t pos;
    int flag; /* FALSE - var/tab, TRUE - val/vec */
    hint_val_t hint;
    access_val_t access;
   }

%token <pointer> NUMBER CHARACTER STRING IDENT
%token <pos> BREAK CASE CATCH CHAR CLASS CONTINUE ELSE EXPOSE EXTERN
       FINAL FLOAT FOR FORMER FRIEND FUN HIDE HIDEBLOCK IF IN INT
       LONG LATER NEW NIL OBJ PMATCH PRIV PROCESS PUB RETURN RMATCH
       TAB THIS THREAD THROW TRY TYPE USE VAL VAR VEC WAIT
%token <pos> LOGICAL_OR LOGICAL_AND EQ NE IDENTITY UNIDENTITY LE GE
             LSHIFT RSHIFT ASHIFT
%token <pos> MULT_ASSIGN DIV_ASSIGN MOD_ASSIGN PLUS_ASSIGN MINUS_ASSIGN
%token <pos> CONCAT_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN ASHIFT_ASSIGN AND_ASSIGN
%token <pos> XOR_ASSIGN OR_ASSIGN
%token <pos> INCR DECR
%token <pos> DOTS
%token <pos> FOLD_PLUS FOLD_MULT FOLD_AND FOLD_XOR FOLD_OR FOLD_CONCAT
%token <pos> INCLUDE INCLUSION END_OF_FILE END_OF_INCLUDE_FILE WILDCARD
%token <pos> '?' ':' '|' '^' '&' '<' '>' '@' '+' '-' '*' '/' '%' '!' '~' '#'
%token <pos> '(' ')' '[' ']' '{' '}' ';' '.' ',' '='

%nonassoc ':'
%nonassoc '?'
%left LOGICAL_OR
%left LOGICAL_AND
%left IN
%left '|'
%left '^'
%left '&'
%left EQ NE IDENTITY UNIDENTITY
%left '<' '>' LE GE
%left LSHIFT RSHIFT ASHIFT
%left '@'
%left '+' '-'
%left '*' '/' '%'
%left '!' '#' '~' FOLD_PLUS FOLD_MULT FOLD_AND FOLD_XOR FOLD_OR FOLD_CONCAT FINAL NEW
/* For resolution of conflicts: `TAB .' and `TAB . [' or `TAB . (',
   and `CHAR .' and `CHAR. (' etc.  */
%nonassoc TAB CHAR INT LONG FLOAT VEC TYPE
/* For resolution of conflicts: FUN . and FUN . '(' on '(' etc.  */
%left '(' '[' '.'

%type <pos> all_fields pos
%type <pointer> aheader expr designator type except_class_list_opt
                elist_parts_list elist_parts_list_empty elist_part
                expr_list expr_list_empty actual_parameters friend_list
                val_var_list val_var assign stmt executive_stmt incr_decr
                match_head case_list a_case opt_cond pattern regexp_pattern for_guard_expr
                block_stmt try_block_stmt catch_list catch except_class_list
                header declaration expose_clause qual_ident expose_qual_ident
                use_clause_list use_item_list use_item alias_opt extern_list
                extern_item fun_thread_class fun_thread_class_start else_part
                expr_empty opt_step par_list par_list_empty par
                formal_parameters block stmt_list program inclusion
%type <flag> clear_flag set_flag  set_flag2 par_kind
%type <hint> hint
%type <access> access

%start program

%%

clear_flag : {$$ = 0;}
           ;
set_flag   : {$$ = 1;}
           ;
set_flag2  : {$$ = 2;}
           ;
expr : NUMBER        {$$ = $1;}
     | CHARACTER     {$$ = $1;}
     | NIL           {$$ = create_node_with_pos (IR_NM_nil, $1);}
     | STRING        {$$ = $1;}
     | type          {$$ = $1;}
     | expr '?' expr ':' expr
       	{
          $$ = create_node_with_pos (IR_NM_cond, $2);
          IR_set_cond_expr ($$, $1);
          IR_set_true_expr ($$, $3);
          IR_set_false_expr ($$, $5);
        }
     | expr LOGICAL_OR expr
       	{
          $$ = create_node_with_pos (IR_NM_logical_or, $2);
          IR_set_operand ($$, $1);
          IR_set_cont_operand ($$, $3);
        }
     | expr LOGICAL_AND expr
       	{
          $$ = create_node_with_pos (IR_NM_logical_and, $2);
          IR_set_operand ($$, $1);
          IR_set_cont_operand ($$, $3);
        }
     | expr IN expr
       	{
          $$ = create_node_with_pos (IR_NM_in, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '|' expr
       	{
          $$ = create_node_with_pos (IR_NM_or, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '^' expr
       	{
          $$ = create_node_with_pos (IR_NM_xor, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '&' expr 
       	{
          $$ = create_node_with_pos (IR_NM_and, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr EQ expr  
       	{
          $$ = create_node_with_pos (IR_NM_eq, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr NE expr  
       	{
          $$ = create_node_with_pos (IR_NM_ne, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr IDENTITY expr  
       	{
          $$ = create_node_with_pos (IR_NM_identity, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr UNIDENTITY expr  
       	 {
           $$ = create_node_with_pos (IR_NM_unidentity, $2);
           IR_set_left_operand ($$, $1);
           IR_set_right_operand ($$, $3);
         }
     | expr '<' expr
       	{
          $$ = create_node_with_pos (IR_NM_lt, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '>' expr  
       	{
          $$ = create_node_with_pos (IR_NM_gt, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        } 
     | expr LE expr
       	{
          $$ = create_node_with_pos (IR_NM_le, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr GE expr 
       	{
          $$ = create_node_with_pos (IR_NM_ge, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr LSHIFT expr
       	{
          $$ = create_node_with_pos (IR_NM_lshift, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr RSHIFT expr
       	{
          $$ = create_node_with_pos (IR_NM_rshift, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr ASHIFT expr
       	{
          $$ = create_node_with_pos (IR_NM_ashift, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '@' expr
       	{
          $$ = create_node_with_pos (IR_NM_concat, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '+' expr
       	{
          $$ = create_node_with_pos (IR_NM_plus, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '-' expr
       	{
          $$ = create_node_with_pos (IR_NM_minus, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '*' expr
       	{
          $$ = create_node_with_pos (IR_NM_mult, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '/' expr
       	{
          $$ = create_node_with_pos (IR_NM_div, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | expr '%' expr
       	{
          $$ = create_node_with_pos (IR_NM_mod, $2);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $3);
        }
     | '!' expr
       	{
          $$ = create_node_with_pos (IR_NM_not, $1);
          IR_set_operand ($$, $2);
        }
     | '+' expr %prec '!'
       	{
          $$ = create_node_with_pos (IR_NM_unary_plus, $1);
          IR_set_operand ($$, $2);
        }
     | '-' expr %prec '!'
       	{
          $$ = create_node_with_pos (IR_NM_unary_minus, $1);
          IR_set_operand ($$, $2);
        }
     | '~' expr
       	{
          $$ = create_node_with_pos (IR_NM_bitwise_not, $1);
          IR_set_operand ($$, $2);
        }
     | '#' expr
       	{
          $$ = create_node_with_pos (IR_NM_length, $1);
          IR_set_operand ($$, $2);
        }
     | FOLD_PLUS expr
       	{
          $$ = create_node_with_pos (IR_NM_fold_plus, $1);
          IR_set_operand ($$, $2);
        }
     | FOLD_MULT expr
       	{
          $$ = create_node_with_pos (IR_NM_fold_mult, $1);
          IR_set_operand ($$, $2);
        }
     | FOLD_AND expr
       	{
          $$ = create_node_with_pos (IR_NM_fold_and, $1);
          IR_set_operand ($$, $2);
        }
     | FOLD_XOR expr
       	{
          $$ = create_node_with_pos (IR_NM_fold_xor, $1);
          IR_set_operand ($$, $2);
        }
     | FOLD_OR expr
       	{
          $$ = create_node_with_pos (IR_NM_fold_or, $1);
          IR_set_operand ($$, $2);
        }
     | FOLD_CONCAT expr
       	{
          $$ = create_node_with_pos (IR_NM_fold_concat, $1);
          IR_set_operand ($$, $2);
        }
     | FINAL expr
       	{
          $$ = create_node_with_pos (IR_NM_const, $1);
          IR_set_operand ($$, $2);
        }
     | NEW expr
       	{
          $$ = create_node_with_pos (IR_NM_new, $1);
          IR_set_operand ($$, $2);
        }
     | expr actual_parameters %prec '('
        {
	  $$ = create_node_with_pos (IR_NM_class_fun_thread_call,
				     actual_parameters_construction_pos);
	  IR_set_fun_expr ($$, $1);
	  IR_set_actuals ($$, $2);
	}
     | designator    {$$ = $1;}
     | '(' expr ')'
          {
	    if (IR_IS_OF_TYPE ($2, IR_NM_paren))
	      $$ = $2;
	    else
	      {
		$$ = create_node_with_pos (IR_NM_paren, $1);
		IR_set_operand ($$, $2);
	      }
	  }
     | '(' error
          {
	    if (repl_flag)
	      YYABORT;
          }
        bracket_stop
	  {
	    $$ = NULL;
	  }
     | '[' set_flag elist_parts_list_empty ']'
      	{
          $$ = create_node_with_pos (IR_NM_vec, $1);
          IR_set_elist ($$, $3);
        }
     | '[' error
          {
	    if (repl_flag)
	      YYABORT;
	  }
        sqbracket_stop
	  {
	    $$ = NULL;
	  }
     | TAB '[' clear_flag elist_parts_list_empty ']'
      	 {
	   $$ = create_node_with_pos (IR_NM_tab, $1);
	   IR_set_elist ($$, $4);
	 }
     | TAB '[' error
          {
	    if (repl_flag)
	      YYABORT;
	  }
       stmt_stop  {$$ = NULL;}
     | TYPE '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_typeof, $1);
          IR_set_operand ($$, $3);
        }
     | CHAR '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_charof, $1);
          IR_set_operand ($$, $3);
        }
     | INT '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_intof, $1);
          IR_set_operand ($$, $3);
        }
     | LONG '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_longof, $1);
          IR_set_operand ($$, $3);
        }
     | FLOAT '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_floatof, $1);
          IR_set_operand ($$, $3);
        }
     | VEC '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_vecof, $1);
          IR_set_operand ($$, $3);
        }
     | VEC '(' expr ',' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_format_vecof, $1);
          IR_set_left_operand ($$, $3);
          IR_set_right_operand ($$, $5);
        }
     | TAB '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_tabof, $1);
          IR_set_operand ($$, $3);
        }
     | THIS { $$ = create_node_with_pos (IR_NM_this, $1); }
     | TRY  '('
         {
	   IR_node_t fun;

	   /* Create anonymous fun: */
	   fun = create_node (IR_NM_fun);
	   IR_set_thread_flag (fun, FALSE);
	   IR_set_pos (fun, $2);
	   IR_set_final_flag (fun, TRUE);
	   IR_set_args_flag (fun, FALSE);
	   process_header (TRUE, fun, get_new_ident ($2)); /* creates fun block */
	   start_block (); /* start func block */
	   $<pointer>$ = create_empty_block (current_scope); /* try block */
	   current_scope = $<pointer>$;
	   IR_set_pos (current_scope, $2);
	   start_block (); /* start try block */
	 }
       set_flag2  executive_stmt  except_class_list_opt  ')'
         {
	   $$ = create_try_expr ($<pointer>3, $5, $6, $2, $7);
	 }
     | TRY '(' error
          {
	    if (repl_flag)
	      YYABORT;
          }
       bracket_stop
	  {
	    $$ = NULL;
	  }
     /* The following is allowed only for patterns.  */
     | WILDCARD { $$ = create_node_with_pos (IR_NM_wildcard, $1); }
     | DOTS     { $$ = create_node_with_pos (IR_NM_dots, $1); }
     ;
type : CHAR          {$$ = create_node_with_pos (IR_NM_char_type, $1);}
     | INT           {$$ = create_node_with_pos (IR_NM_int_type, $1);}
     | LONG          {$$ = create_node_with_pos (IR_NM_long_type, $1);}
     | FLOAT         {$$ = create_node_with_pos (IR_NM_float_type, $1);}
     | HIDE          {$$ = create_node_with_pos (IR_NM_hide_type, $1);}
     | HIDEBLOCK     {$$ = create_node_with_pos (IR_NM_hideblock_type, $1);}
     | VEC           {$$ = create_node_with_pos (IR_NM_vec_type, $1);}
     | TAB           {$$ = create_node_with_pos (IR_NM_tab_type, $1);}
     | fun_thread_class %prec ':'
         {
	   $$ = create_node_with_pos (IR_NODE_MODE ($1) == IR_NM_fun
				      ? IR_NM_fun_type
				      : IR_NODE_MODE ($1) == IR_NM_class
				      ? IR_NM_class_type
				      : IR_NM_thread_type,
				      IR_pos ($1));
	 }
     | OBJ           {$$ = create_node_with_pos (IR_NM_stack_type, $1);}
     | PROCESS       {$$ = create_node_with_pos (IR_NM_process_type, $1);}
     | TYPE          {$$ = create_node_with_pos (IR_NM_type_type, $1);}
     ;
except_class_list_opt :  {
                           $$ = create_except_class (NULL,
						     get_ident_node
						     (EXCEPT_NAME,
						      current_position));
                         }
                      | ',' except_class_list  { $$ = $2; }
	              ;
aheader : fun_thread_class
            {
	      IR_set_final_flag ($1, TRUE);
	      process_header (TRUE, $1, get_new_ident (IR_pos ($1)));
	      $$ = process_formal_parameters ($1, NULL);
            }
        | fun_thread_class '('
            {
	      IR_set_final_flag ($1, TRUE);
              process_header (TRUE, $1, get_new_ident (IR_pos ($1)));
            }
          formal_parameters ')'
            {
              $$ = process_formal_parameters ($1, $4);
            }
	;
/* Stop symbols:*/
eof_stop : END_OF_FILE          {yychar = END_OF_FILE;}
         | END_OF_INCLUDE_FILE  {yychar = END_OF_INCLUDE_FILE;}
         ;
bracket_stop : eof_stop
             | ')'
             | '}' {yychar = '}';}
             | ';' {yychar = ';';}
             ;
/* Stop symbols:*/
sqbracket_stop : eof_stop
               | ']'
               | '}' {yychar = '}';}
               | ';' {yychar = ';';}
               ;
/* Stop symbols:*/
stmt_stop : eof_stop
          | '}' {yychar = '}';}
          | ';' {yychar = ';';}
          ;
pos :  {$$ = current_position;}
    ;
hint :           {$$ = NO_HINT;}
     | '!' IDENT {$$ = get_hint ($2);}
     | '!' error
       {
	 if (repl_flag)
	   YYABORT;
         $$ = NO_HINT;
       }
     ;
designator : expr '[' expr ']'
       	       {
                 $$ = create_node_with_pos (IR_NM_index, $2);
                 IR_set_designator ($$, $1);
                 IR_set_component ($$, $3);
               }
           | expr '[' expr_empty ':' expr_empty opt_step ']'
       	       {
                 $$ = create_node_with_pos (IR_NM_slice, $2);
                 IR_set_designator ($$, $1);
		 if ($3 == NULL)
		   $3 = get_int_node (0, $4);
                 IR_set_component ($$, $3);
		 if ($5 == NULL)
		   $5 = get_int_node (-1, $4);
                 IR_set_bound ($$, $5);
		 if ($6 == NULL)
		   $6 = get_int_node (1, $7);
                 IR_set_step ($$, $6);
               }
           | expr '[' error
	       {
		 if (repl_flag)
		   YYABORT;
	       }
	     sqbracket_stop
       	       /* The designator have to be vec. */
       	       {
                 $$ = create_node_with_pos (IR_NM_index, $2);
                 IR_set_designator ($$, $1);
                 IR_set_component ($$, NULL);
               }
           | expr '.' IDENT
       	       {
                 $$ = create_node_with_pos (IR_NM_period, $2);
                 IR_set_designator ($$, $1);
                 IR_set_component ($$, $3);
               }
           | IDENT     {$$ = $1;}
           | aheader hint block
               {
		 additional_stmts
		   = merge_stmt_lists (additional_stmts,
				       process_header_block ($1, $3, $2));
		 $$ = IR_ident (IR_next_stmt (additional_stmts));
	       }
           ;
/* Attribute value is the last element of the cycle list.  The
   nonterminal with attribute of type flag must be before
   elist_parts_list. */
elist_parts_list : elist_part
       	            {
                      $$ = $1;
                      IR_set_next_elist ($$, $$);
                    }
                 | elist_parts_list ','  {$<flag>$ = $<flag>0;} elist_part
       	            {
	              $$ = $4;
                      IR_set_next_elist ($$, IR_next_elist ($1));
                      IR_set_next_elist ($1, $$);
                    }
       	         ;
/* The nonterminal with attribute of type flag must be before
   elist_part. */
elist_part : pos expr
               {
	         $$ = create_node_with_pos (IR_NM_elist_element, $1);
                 if ($<flag>0)
                   {
                     /* vec */
                     IR_set_repetition_key ($$, get_int_node (1, $1));
                     IR_set_expr ($$, $2);
                   }
	         else
                   {
                     IR_set_repetition_key ($$, $2);
                     IR_set_expr ($$, create_node_with_pos (IR_NM_nil, $1));
                   }
               }
           | pos expr ':' expr
               {
                 $$ = create_node_with_pos (IR_NM_elist_element, $1);
		 IR_set_repetition_key_flag ($$, TRUE);
     	         IR_set_repetition_key ($$, $2);
                 IR_set_expr ($$, $4);
               }
           ;
/* The nonterminal with attribute of type flag must be before
   elist_parts_list_empty. */
elist_parts_list_empty :      {$$ = NULL;}
       	               | elist_parts_list
                              {
				$$ = IR_next_elist ($1);
				IR_set_next_elist ($1, NULL);
			      }
       	               ;
/* This nonterminal has second attributes:
   var actual_parameters_construction_pos (see commentaries for
   the var). */
actual_parameters : '(' expr_list_empty ')'
       		       {
			 actual_parameters_construction_pos = $1;
			 $$ = $2;
		       }
       	          | '(' error
		      {
			if (repl_flag)
			  YYABORT;
		      }
		    bracket_stop
       		      {
			actual_parameters_construction_pos = $1;
			$$ = NULL;
		      }
       	          ;
expr_list_empty :           {$$ = NULL;}
       	        | expr_list
                    {
		      $$ = IR_next_elist ($1);
		      IR_set_next_elist ($1, NULL);
		    }
       	        ;
/* Attribute value is the last element of the cycle list. */
expr_list : pos expr
              {
	        $$ = create_node_with_pos (IR_NM_elist_element, $1);
                IR_set_expr ($$, $2);
	        IR_set_repetition_key ($$, NULL);
                IR_set_next_elist ($$, $$);
              }
          | expr_list ',' expr
              {
                $$ = create_node_with_pos (IR_NM_elist_element, $2);
     	        IR_set_repetition_key ($$, NULL);
                IR_set_expr ($$, $3);
                IR_set_next_elist ($$, IR_next_elist ($1));
                IR_set_next_elist ($1, $$);
              }
          ;
access :      { $$ = DEFAULT_ACCESS; }
       | PRIV { $$ = PRIVATE_ACCESS; }
       | PUB  { $$ = PUBLIC_ACCESS; }
       ;
/* Attribute value is cyclic list of stmts corresponding to var decls
   with the pointer to the last element.  The attributes before
   val_var_list must be access flag and flag of that this is in val
   decl (not in var decl). */
val_var_list : val_var   {$$ = $1;}
             | val_var_list ','
                 {$<access>$ = $<access>-1;} {$<flag>$ = $<flag>0;} val_var
       	         {
	           $$ = $5;
	           if ($1 != NULL)
	     	     {
		       IR_node_t first;
	     	   
		       first = IR_next_stmt ($$);
		       IR_set_next_stmt ($$, IR_next_stmt ($1));
		       IR_set_next_stmt ($1, first);
		     }
	         }
       	     | error
	         {
		   if (repl_flag)
		     YYABORT;
		   $$ = NULL;
		 }
       	     ;
/* Attribute value is cyclic list of stmts corresponding to var decl
   (and possibly assignment stmt) with the pointer to the last
   element.  The attributes before val_var_list must be access flag
   and flag of that this is in val decl (not in var decl). */
val_var : IDENT
            {
	      $$ = process_var_decl ($<access>-1, $1, $<flag>0, NULL,
				     NULL, IR_pos ($1), IR_NM_var_assign);
	    }
        | pattern '=' expr
            {
	      $$ = process_var_pattern ($<access>-1, $1, $<flag>0,
					$3, $2, IR_NM_var_pattern_assign);
	    }
        ;
/* Attribute value is cyclic list of stmts corresponding to stmt
   (declaration). */
stmt : executive_stmt
         {
           if ($1 != NULL)
             IR_set_next_stmt ($1, $1);
           $$ = $1; 
         }
     | declaration        {$$ = $1;}
     | error
         {
	   if (repl_flag)
	     YYABORT;
	 }
       stmt_stop
         {
	   $$ = NULL;
	 }
     ;
assign : '='
           {$$ = create_node_with_pos (IR_NM_assign, $1);}
       | MULT_ASSIGN
           {$$ = create_node_with_pos (IR_NM_mult_assign, $1);}
       | DIV_ASSIGN
           {$$ = create_node_with_pos (IR_NM_div_assign, $1);}
       | MOD_ASSIGN
           {$$ = create_node_with_pos (IR_NM_mod_assign, $1);}
       | PLUS_ASSIGN
           {$$ = create_node_with_pos (IR_NM_plus_assign, $1);}
       | MINUS_ASSIGN
           {$$ = create_node_with_pos (IR_NM_minus_assign, $1);}
       | CONCAT_ASSIGN
           {$$ = create_node_with_pos (IR_NM_concat_assign, $1);}
       | LSHIFT_ASSIGN
           {$$ = create_node_with_pos (IR_NM_lshift_assign, $1);}
       | RSHIFT_ASSIGN
           {$$ = create_node_with_pos (IR_NM_rshift_assign, $1);}
       | ASHIFT_ASSIGN
           {$$ = create_node_with_pos (IR_NM_ashift_assign, $1);}
       | AND_ASSIGN
           {$$ = create_node_with_pos (IR_NM_and_assign, $1);}
       | XOR_ASSIGN
           {$$ = create_node_with_pos (IR_NM_xor_assign, $1);}
       | OR_ASSIGN
           {$$ = create_node_with_pos (IR_NM_or_assign, $1);}
       ;
incr_decr : INCR {$$ = get_int_node (1, $1);}
          | DECR {$$ = get_int_node (-1, $1);}
          ;
executive_stmt :
      {$<flag>$ = $<flag>0;} end_exec_stmt      {$$ = NULL;}
    | expr {$<pos>$ = current_position; $<pos>$.column_number--; }
        {$<flag>$ = $<flag>0;} end_exec_stmt
        {
          $$ = create_node_with_pos (IR_NM_expr_stmt,
				     $1 == NULL ? $<pos>2 : IR_pos ($1));
	  IR_set_stmt_expr ($$, $1);
        }
    | designator assign expr {$<flag>$ = $<flag>0;} end_exec_stmt
       	{
          $$ = $2;
          IR_set_assignment_var ($$, $1);
          IR_set_assignment_expr ($$, $3);
        }
    | designator incr_decr {$<flag>$ = $<flag>0;} end_exec_stmt
       	{
          $$ = create_node_with_pos (IR_NM_plus_assign, IR_pos ($2));
          IR_set_assignment_var ($$, $1); 
          IR_set_assignment_expr ($$, $2);
        }
    | incr_decr designator {$<flag>$ = $<flag>0;} end_exec_stmt
       	{
          $$ = create_node_with_pos (IR_NM_plus_assign, IR_pos ($1));
          IR_set_assignment_var ($$, $2); 
          IR_set_assignment_expr ($$, $1);
        }
    | IF '(' expr ')' {$<flag>$ = $<flag>0;} stmt else_part
       	{
          $$ = create_node_with_pos (IR_NM_if_stmt, $1);
          IR_set_if_expr ($$, $3);
          IR_set_if_part ($$, uncycle_stmt_list ($6));
          IR_set_else_part ($$, $7);
        }
    | IF '(' error
        {
	  if (repl_flag)
	    YYABORT;
        }
      bracket_stop {$<flag>$ = $<flag>0;} stmt else_part
       	{
          $$ = create_node_with_pos (IR_NM_if_stmt, $1);
          IR_set_if_expr ($$, NULL);
          IR_set_if_part ($$, uncycle_stmt_list ($7));
          IR_set_else_part ($$, $8);
        }
    | FOR '(' clear_flag stmt for_guard_expr ';' set_flag stmt ')'
        {$<flag>$ = $<flag>0;} stmt
       	{
          $$ = create_node_with_pos (IR_NM_for_stmt, $1);
          IR_set_for_initial_stmt ($$, uncycle_stmt_list ($4));
          IR_set_for_guard_expr ($$, $5);
          IR_set_for_iterate_stmt ($$, uncycle_stmt_list ($8));
          IR_set_for_stmts ($$, uncycle_stmt_list ($11));
        }
    | FOR '(' clear_flag designator IN expr ')'
        {$<flag>$ = $<flag>0;} stmt
       	{
          $$ = create_node_with_pos (IR_NM_foreach_stmt, $1);
          IR_set_foreach_index_designator ($$, $4);
          IR_set_foreach_tab ($$, $6);
          IR_set_foreach_stmts ($$, uncycle_stmt_list ($9));
        }
    | FOR '(' error
        {
	  if (repl_flag)
	    YYABORT;
	}
      bracket_stop {$<flag>$ = $<flag>0;} stmt
       	{
          $$ = create_node_with_pos (IR_NM_for_stmt, $1);
          IR_set_for_initial_stmt ($$, NULL);
          IR_set_for_guard_expr ($$, NULL);
          IR_set_for_iterate_stmt ($$, NULL);
          IR_set_foreach_stmts ($$, uncycle_stmt_list ($7));
        }
    | match_head '(' expr ')' '{'
        {
          IR_set_match_expr ($1, $3);
	  $<pointer>$ = $1;
	}
      case_list '}'
        {
	  $$ = $1;
          IR_set_cases ($$, uncycle_stmt_list ($7)); 
	}
    | BREAK {$<flag>$ = $<flag>0;} end_exec_stmt
        {$$ = create_node_with_pos (IR_NM_break_stmt, $1);}
    | CONTINUE {$<flag>$ = $<flag>0;} end_exec_stmt
        {$$ = create_node_with_pos (IR_NM_continue_stmt, $1);}
    | RETURN expr_empty {$<flag>$ = $<flag>0;} end_exec_stmt
       	{
          if ($2 == NULL)
            $$ = create_node_with_pos (IR_NM_return_without_result, $1);
          else
            {
              $$ = create_node_with_pos (IR_NM_return_with_result, $1);
              IR_set_returned_expr ($$, $2);
            }
       	}
    | THROW expr {$<flag>$ = $<flag>0;} end_exec_stmt
       	{
          $$ = create_node_with_pos (IR_NM_throw, $1);
          IR_set_throw_expr ($$, $2); 
        }
    | WAIT '(' expr ')' {$<flag>$ = $<flag>0;} stmt
       	{
          $$ = create_node_with_pos (IR_NM_wait_stmt, $1);
          IR_set_wait_guard_expr ($$, $3);
          IR_set_wait_stmt ($$, uncycle_stmt_list ($6));
        }
    | WAIT '(' error
        {
	  if (repl_flag)
	    YYABORT;
	}
      bracket_stop {$<flag>$ = $<flag>0;} stmt
       	{
          $$ = create_node_with_pos (IR_NM_wait_stmt, $1);
          IR_set_wait_guard_expr ($$, NULL);
          IR_set_wait_stmt ($$, uncycle_stmt_list ($7));
        }
    | block_stmt     {$$ = $1;}
    | try_block_stmt {$$ = $1;}
    ;
match_head : PMATCH { $$ = create_node_with_pos (IR_NM_pmatch_stmt, $1); }
           | RMATCH { $$ = create_node_with_pos (IR_NM_rmatch_stmt, $1); }
           ;
/* attribute before is the match-stmt  */
case_list :                        { $$ = NULL; }
          | case_list {$<pointer>$ = $<pointer>0; } a_case
	      { $$ = merge_stmt_lists ($1, $3); }
          | error
              {
		if (repl_flag)
		  YYABORT;
		$$ = NULL;
	      }
          ;
/* attribute before is the match-stmt  */
a_case :   {
              $<pointer>$ = create_empty_block (current_scope);
              current_scope = $<pointer>$;
            }
       	 CASE regexp_pattern opt_cond ':' stmt_list
            {
	      IR_node_t break_stmt;
	      
              $$ = $<pointer>1;
	      IR_set_match_stmt ($$, $<pointer>0);
	      IR_set_case_pattern ($$, $3);
	      IR_set_case_cond ($$, $4);
	      /* Add implicit break at the end of case-stmts. */
	      break_stmt = create_node_with_pos (IR_NM_break_stmt,
						 current_position);
	      IR_set_next_stmt (break_stmt, break_stmt);
	      IR_set_implicit_case_break_stmt ($$, break_stmt);
	      IR_set_block_stmts ($$, uncycle_stmt_list (merge_stmt_lists
							 ($6, break_stmt)));
	      IR_set_next_stmt ($$, $$);
	      current_scope = IR_block_scope ($$);
	    }
        ;
opt_cond :           { $$ = NULL; }
         | IF expr   { $$ = $2; }
         ;
pattern : expr  { $$ = $1; }
        ;
regexp_pattern : expr  { $$ = $1; }
               ;
for_guard_expr :      {$$ = get_int_node (1, source_position);}
               | expr {$$ = $1;}
               ;
block_stmt :    {
                  $<pointer>$ = create_empty_block (current_scope);
                  current_scope = $<pointer>$;
                }
       	      block
       		{
                  $$ = $<pointer>1;
       		  IR_set_block_stmts ($$, uncycle_stmt_list ($2));
       		  IR_set_friend_list
		    ($$, uncycle_friend_list (IR_friend_list ($$)));
       		  current_scope = IR_block_scope ($$);
                }
           ;
try_block_stmt : TRY
                    {
                      $<pointer>$ = create_empty_block (current_scope);
		      current_scope = $<pointer>$;
                    }
                 block
       		    {
		      $<pointer>$ = $<pointer>2;
		      IR_set_block_stmts ($<pointer>$, uncycle_stmt_list ($3));
		      IR_set_friend_list
			($<pointer>$, uncycle_friend_list (IR_friend_list
							   ($<pointer>$)));
		      current_scope = IR_block_scope ($<pointer>$);
		    }
                 catch_list
                    {
		      $$ = $<pointer>4;
		      IR_set_exceptions ($$, uncycle_exception_list ($5));
                    }
       	       ;
/* Attribute value is cyclic list of exceptions with the pointer to
   the last one. */
catch_list :                           {$$ = NULL;}
           | catch_list  catch
               {
		 $$ = merge_exception_lists ($1, $2);
	       }
           ;
/* Attribute value is cyclic list of exceptions with the pointer to
   the last one. */
catch : CATCH  '(' except_class_list ')'
          {
            $<pointer>$ = create_catch_block ($4);
          }
       	block
       	  {
	    finish_catch_block
	      ($<pointer>5, $6, $3,
	       uncycle_friend_list (IR_friend_list (current_scope)));
	    $$ = $3;
	  }
      | error
          {
	    if (repl_flag)
	      YYABORT;
	    $$ = NULL;
	  }
      ;
/* Attribute value is cyclic list of exceptions with the pointer to
   the last one. */
except_class_list : expr { $$ = create_except_class (NULL, $1); }
                  | except_class_list ',' expr  { $$ = create_except_class ($1, $3); }
                  ;
/* Attribute value is cyclic list of ident in clause with the pointer
   to the last one. */
friend_list : IDENT
               {
                 $$ = create_node (IR_NM_friend_ident);
                 IR_set_ident_in_clause ($$, $1);
                 IR_set_next_friend_ident ($$, $$);
               }
     	    | friend_list ',' IDENT
     	        {
		  $$ = create_node (IR_NM_friend_ident);
		  if ($1 != NULL)
		    {
		      IR_set_next_friend_ident ($$, IR_next_friend_ident ($1));
		      IR_set_next_friend_ident ($1, $$);
		    }
		  else
		    IR_set_next_friend_ident ($$, $$);
		  IR_set_ident_in_clause ($$, $3);
		}
     	    | error
	        {
		  if (repl_flag)
		    YYABORT;
		  $$ = NULL;
		}
     	    ;
/* Attribute value is cyclic list of stmts corresponding to decls with
   the pointer to last element.  Class (function) declaration is
   represented by two stmt nodes: class (function) node and block
   node. */
declaration : access VAL {$<access>$ = $1;} set_flag
                val_var_list {$<flag>$ = $<flag>0;} end_simple_stmt {$$ = $5;}
            | access VAR {$<access>$ = $1;} clear_flag
                val_var_list {$<flag>$ = $<flag>0;} end_simple_stmt {$$ = $5;}
            | FRIEND friend_list {$<flag>$ = $<flag>0;} end_simple_stmt
                {
		  IR_set_friend_list (current_scope,
				      merge_friend_lists
				      (IR_friend_list (current_scope), $2));
		  $$ = NULL;
		}
            | access EXTERN {$<access>$ = $1;}
                extern_list {$<flag>$ = $<flag>0;} end_simple_stmt {$$ = $4;}
            | header hint block
                {
		  $$ = process_header_block ($1, $3, $2);
		}
            | fun_thread_class_start IDENT
                {
		  IR_set_pos ($1, IR_pos ($2));
		  $<flag>$ = $<flag>0;
                }
              end_simple_stmt
    	        {
		  process_header (FALSE, $1, $2);
		  $$ = $1;
		}
            /* Access is flattened out to resolve conflicts on OBJ.  */
            | OBJ IDENT { $<pointer>$ = process_obj_header ($2); }
                block {$$ = process_obj_block ($2, $<pointer>3, $4, DEFAULT_ACCESS);}
            | PRIV OBJ IDENT { $<pointer>$ = process_obj_header ($3); }
                block {$$ = process_obj_block ($3, $<pointer>4, $5, PRIVATE_ACCESS);}
            | PUB OBJ IDENT { $<pointer>$ = process_obj_header ($3); }
                block {$$ = process_obj_block ($3, $<pointer>4, $5, PUBLIC_ACCESS);}
            | INCLUDE STRING {$<flag>$ = $<flag>0;} end_simple_stmt
                {
		  $<pointer>$ = $2;
                  if (add_include_file ($2))
                    add_lexema_to_file (INCLUSION);
                }
              inclusion
                {
                  $$ = $6;
                }
            | INCLUDE '+' STRING {$<flag>$ = $<flag>0;} end_simple_stmt
                {
		  $<pointer>$ = $3;
                  add_include_file ($3);
		  add_lexema_to_file (INCLUSION);
                }
              inclusion
                {
                  $$ = $7;
                }
            | expose_clause {$<flag>$ = $<flag>0;} end_simple_stmt
                {
		  $$ = $1;
	        }
            | USE qual_ident use_clause_list
	        {$<flag>$ = $<flag>0;} end_simple_stmt
                {
		  $$ = create_node_with_pos (IR_NM_use, IR_pos ($2));
		  IR_set_next_stmt ($$, $$);
                  IR_set_use_qual_ident ($$, $2);
		  IR_set_use_items ($$, uncycle_use_item_list ($3));
	        }
            ;
expose_clause : EXPOSE expose_qual_ident
                  {
		    $$ = $2;
		    IR_set_next_stmt ($$, $$);
                  }
              | expose_clause ',' expose_qual_ident
		  {
		    IR_set_next_stmt ($3, IR_next_stmt ($1));
		    IR_set_next_stmt ($1, $3);
		    $$ = $3;
		  }
              ;
all_fields : '.' '*'    {$$ = $1;}
           | FOLD_MULT  {$$ = $1;}
           ;
expose_qual_ident : qual_ident alias_opt
                      {
			$$ = create_node_with_pos (IR_NM_expose, IR_pos ($1));
			IR_set_expose_designator ($$, $1);
			IR_set_expose_alias ($$, $2);
			IR_set_expose_internals_flag ($$, FALSE);
		      }
                  | qual_ident all_fields
                      {
			$$ = create_node_with_pos (IR_NM_expose, $2);
			IR_set_expose_designator ($$, $1);
			IR_set_expose_alias ($$, NULL);
			IR_set_expose_internals_flag ($$, TRUE);
		      }
                  ;
qual_ident : IDENT { $$ = $1; }
           | qual_ident '.' IDENT
               {
                 $$ = create_node_with_pos (IR_NM_period, $2);
                 IR_set_designator ($$, $1);
                 IR_set_component ($$, $3);
               }
           ;
use_clause_list :   {$$ = NULL;}
                | use_clause_list use_item_list
                    {
		      $$ = merge_use_item_lists ($1, $2);
		    }
                ;
use_item_list : FORMER set_flag   use_item { $$ = $3; }
              | LATER  clear_flag use_item { $$ = $3; }
              | use_item_list ','
                  {$<flag>$ = IR_NODE_MODE ($1) == IR_NM_former_item;} use_item
                  {
                    $$ = merge_use_item_lists ($1, $4);
	          }
              ;
use_item : IDENT alias_opt
             {
	       $$ = create_node_with_pos ($<flag>0
					  ? IR_NM_former_item : IR_NM_later_item,
					  IR_pos ($1));
	       IR_set_use_item_ident ($$, $1);
	       IR_set_next_use_item ($$, $$);
	       IR_set_alias ($$, $2);
	     }
         ;
alias_opt :  { $$ = NULL; }
          | '(' IDENT ')'  { $$ = $2; }
          ;
extern_list : extern_item {$$ = $1;}
            | extern_list ',' {$<access>$ = $<access>0;} extern_item
                {
		   $$ = $4;
                   if ($1 != NULL)
                     {
		       IR_node_t first;

		       first = IR_next_stmt ($$);
                       IR_set_next_stmt ($$, IR_next_stmt ($1));
                       IR_set_next_stmt ($1, first);
                     }
                }
            ;
extern_item : IDENT
                {
		  $$ = create_node_with_pos (IR_NM_external_var, IR_pos ($1));
		  IR_set_scope ($$, current_scope);
		  IR_set_ident ($$, $1);
		  IR_set_next_stmt ($$, $$);
		  IR_set_access ($$, $<access>0);
                }
            | IDENT '(' ')'
                {
		  $$ = create_node_with_pos (IR_NM_external_fun, IR_pos ($1));
		  IR_set_scope ($$, current_scope);
		  IR_set_ident ($$, $1);
		  IR_set_next_stmt ($$, $$);
		  IR_set_access ($$, $<access>0);
                }
            ;
inclusion :   {$$ = NULL;}
          |   {
	        yyerrok;
              }
            INCLUSION
              {
		const char *fname, *encoding;

		fname = get_full_file_and_encoding_name ($<pointer>0,
							 &encoding);
                start_scanner_file (fname, encoding,
				    IR_pos ($<pointer>0));
              }
            stmt_list END_OF_INCLUDE_FILE
              {
                yyerrok;
                finish_scanner_file ();
                $$ = $4;
	      }
          ;
/* Trick for more concise grammar description. */
end_simple_stmt : ';'
                | ')' 
                    {
                      if ($<flag>0)
                        yychar = ')';
		      else if (!YYRECOVERING ())
			{
			  yyerror ("syntax error");
			  if (repl_flag)
			    YYABORT;
			}
                    }
                ;
end_exec_stmt : end_simple_stmt
              | ',' 
                  {
                    if ($<flag>0 == 2)
                      yychar = ',';
	            else if (!YYRECOVERING ())
		      {
			yyerror ("syntax error");
			if (repl_flag)
			  YYABORT;
		      }
                  }
              ;
header : fun_thread_class_start IDENT
           {
	     IR_set_pos ($1, IR_pos ($2));
	     process_header (TRUE, $1, $2);
	     $$ = process_formal_parameters ($1, NULL);
	   }
       | fun_thread_class_start IDENT
           {
	     IR_set_pos ($1, IR_pos ($2));
	     process_header (TRUE, $1, $2);
	   }
         '(' formal_parameters ')'
           { $$ = process_formal_parameters ($1, $5); }
       ;
/* Access is flatten out for resolving conflicts on OBJ and FINAL.  */
fun_thread_class_start : fun_thread_class
                           {$$ = process_fun_start ($1, FALSE, DEFAULT_ACCESS);}
                       | PRIV fun_thread_class
                           {$$ = process_fun_start ($2, FALSE, PRIVATE_ACCESS);}
                       | PUB fun_thread_class
                           {$$ = process_fun_start ($2, FALSE, PUBLIC_ACCESS);}
                       | FINAL fun_thread_class
                           {$$ = process_fun_start ($2, TRUE, DEFAULT_ACCESS);}
                       | FINAL PRIV fun_thread_class
                           {$$ = process_fun_start ($3, TRUE, PRIVATE_ACCESS);}
                       | FINAL PUB fun_thread_class
                           {$$ = process_fun_start ($3, TRUE, PUBLIC_ACCESS);}
                       | PRIV FINAL fun_thread_class
                           {$$ = process_fun_start ($3, TRUE, PRIVATE_ACCESS);}
                       | PUB FINAL fun_thread_class
                           {$$ = process_fun_start ($3, TRUE, PUBLIC_ACCESS);}
                       ;
fun_thread_class : FUN
                     {
		       $$ = create_node (IR_NM_fun);
		       IR_set_thread_flag ($$, FALSE);
		       IR_set_pos ($$, $1);
		     }
       	         | THREAD
                     {
                       $$ = create_node (IR_NM_fun);
		       IR_set_thread_flag ($$, TRUE);
		       IR_set_pos ($$, $1);
		     }
       	         | CLASS
		     {
		       $$ = create_node (IR_NM_class);
		       IR_set_pos ($$, $1);
		     }
      	         ;
else_part :                                    {$$ = NULL;}
          | ELSE {$<flag>$ = $<flag>-1;} stmt  {$$ = uncycle_stmt_list ($3);}
          ;
expr_empty :             {$$ = NULL;}
       	   | expr        {$$ = $1;}
       	   ;
opt_step :           {$$ = NULL;}
         | ':' expr  {$$ = $2;}
         ;
/* Attribute value is cyclic list of stmts corresponding to par decls
   with the pointer to the last element.  */
par_list : par   {$$ = $1;}
         | par_list ',' par
       	     {
	       $$ = $3;
	       if ($1 != NULL)
	 	 {
	 	   IR_node_t first;
	 	   
	 	   first = IR_next_stmt ($$);
	 	   IR_set_next_stmt ($$, IR_next_stmt ($1));
	 	   IR_set_next_stmt ($1, first);
	 	 }
	     }
       	 | error
	     {
	       if (repl_flag)
		 YYABORT;
	       $$ = NULL;
	     }
       	 ;
par_kind :      {$$ = 0;}
         | VAL  {$$ = 1;}
         | VAR  {$$ = 0;}
         ;
par : access par_kind IDENT
        {
	  $$ = process_var_decl ($1, $3, $2, NULL,
				 NULL, IR_pos ($3), IR_NM_par_assign);
        }
    | access par_kind IDENT '=' expr
        {
	  $$ = process_var_decl ($1, $3, $2, NULL, $5, $4, IR_NM_par_assign);
        }
    ;
par_list_empty :          {$$ = NULL;} 
       	       | par_list {$$ = $1;}
       	       ;
formal_parameters : par_list_empty
                       {
                         formal_parameter_args_flag = FALSE;
			 $$ = $1;
		       }
       	          | par_list ',' DOTS
                       {
			 formal_parameter_args_flag = TRUE;
			 $$ = create_node_with_pos (IR_NM_var, $3);
			 if ($1 != NULL)
			   {
			     IR_set_next_stmt ($$, IR_next_stmt ($1));
			     IR_set_next_stmt ($1, $$);
			   }
			 else
			   IR_set_next_stmt ($$, $$);
			 IR_set_scope ($$, current_scope);
			 IR_set_ident ($$, get_ident_node (ARGS_NAME, $3));
		       }
       	          | DOTS
                       {
			 formal_parameter_args_flag = TRUE;
			 $$ = create_node_with_pos (IR_NM_var, $1);
			 IR_set_scope ($$, current_scope);
			 IR_set_ident ($$,  get_ident_node (ARGS_NAME, $1));
			 IR_set_next_stmt ($$, $$);
		       }
       	          ;
/* See comments for stmt_list. */
block : '{'
             {
	       start_block ();
	     }
         stmt_list '}'
             {
               IR_set_pos (current_scope, $1);
               $$ = $3;
	       finish_block ();
             }
      ;
/* Attribute value is the last element of the cycle list. */
stmt_list :                          	      {$$ = NULL;}
          | stmt_list clear_flag stmt
	      {
		$$ = merge_stmt_lists ($1, merge_additional_stmts ($3));
		repl_process_flag = repl_can_process_p ();
	      }
       	  | stmt_list error
	      {
		if (repl_flag)
		  YYABORT;
		$$ = $1;
	      }
       	  ;
program :   {
	      repl_process_flag = FALSE;
	      IR_set_friend_list (current_scope, NULL);
	      IR_set_next_stmt (current_scope, NULL);
            }
          stmt_list
            {
	      /* In REPL mode we can achieve this code in error
		 recovery mode, don't change first_program_stmt it
		 should be always the same for REPL.  */
	      if (! repl_flag || first_program_stmt == NULL)
		first_program_stmt = current_scope;
              IR_set_block_stmts (first_program_stmt,
                                  uncycle_stmt_list ($2));
	      IR_set_friend_list
		(current_scope,
		 uncycle_friend_list (IR_friend_list (current_scope)));
            }
          END_OF_FILE
            {
	      YYACCEPT;
	    }
        ;
%%

/* Containers used temporary by the scanner.  */
static vlo_t temp_scanner_vlo;
static vlo_t temp_scanner_vlo2;

/* True if we did not print syntax error yet.  We can not leave
   yyparse by longjmp as we need to finalize some data.  We leave
   YYABORT in most cases but it is guaranted for all yyerors
   calls. Spurious subsequent syntax errors should be suppressed for
   better experience in REPL.  */
static int first_error_p;

/* This function is called by yacc parser and for fatal error
   reporting. */
static int
yyerror (const char *message)
{
   if (! repl_flag || first_error_p)
    d_error (FALSE, source_position, "%s", message);
  first_error_p = FALSE;
  return 0; /* No warnings */
}



/* Merging two cyclic lists into one cyclic list. */
static IR_node_t
merge_friend_lists (IR_node_t list1, IR_node_t list2)
{
  IR_node_t result;

  if (list2 != NULL) {
    if (list1 != NULL)
      {
        IR_node_t first;
        
        first = IR_next_friend_ident (list2);
        IR_set_next_friend_ident (list2, IR_next_friend_ident (list1));
        IR_set_next_friend_ident (list1, first);
      }
    result = list2;
  }
  else
    result = list1;
  return result;
}

/* Make normal list from cycle ident list with the pointer to the last
   stmt. */
static IR_node_t
uncycle_friend_list (IR_node_t list)
{
  IR_node_t first;

  if (list == NULL)
    return list;
  first = IR_next_friend_ident (list);
  IR_set_next_friend_ident (list, NULL);
  return first;
}



/* Merging two cyclic lists into one cyclic list. */
static IR_node_t
merge_use_item_lists (IR_node_t list1, IR_node_t list2)
{
  IR_node_t result;

  if (list2 != NULL) {
    if (list1 != NULL)
      {
        IR_node_t first;
        
        first = IR_next_use_item (list2);
        IR_set_next_use_item (list2, IR_next_use_item (list1));
        IR_set_next_use_item (list1, first);
      }
    result = list2;
  }
  else
    result = list1;
  return result;
}

/* Make normal list from cycle item list with the pointer to the
   last item list. */
static IR_node_t
uncycle_use_item_list (IR_node_t list)
{
  IR_node_t first;

  if (list == NULL)
    return list;
  first = IR_next_use_item (list);
  IR_set_next_use_item (list, NULL);
  return first;
}



/* Merging two cyclic lists into one cyclic list. */
static IR_node_t
merge_exception_lists (IR_node_t list1, IR_node_t list2)
{
  IR_node_t result;

  if (list2 != NULL) {
    if (list1 != NULL)
      {
        IR_node_t first;
        
        first = IR_next_exception (list2);
        IR_set_next_exception (list2, IR_next_exception (list1));
        IR_set_next_exception (list1, first);
      }
    result = list2;
  }
  else
    result = list1;
  return result;
}

/* Make normal list from cycle exception list with the pointer to the last
   exception. */
static IR_node_t
uncycle_exception_list (IR_node_t list)
{
  IR_node_t first;

  if (list == NULL)
    return list;
  first = IR_next_exception (list);
  IR_set_next_exception (list, NULL);
  return first;
}

/* Create variable IDENT with VAL_FLAG and OBJ_BLOCK.  If EXPR is not
   null, create also assignment with ASSIGN of EXPR with EXPR_POS to
   the variable.  Create cyclic list of the node(s) and return the
   last one.  */
static IR_node_t
process_var_decl (access_val_t access, IR_node_t ident,
		  int val_flag, IR_node_t obj_block, IR_node_t expr,
		  position_t expr_pos, IR_node_mode_t assign)
{
  position_t ident_pos = IR_pos (ident);
  IR_node_t res = create_node_with_pos (IR_NM_var, ident_pos);
  
  IR_set_access (res, access);
  IR_set_scope (res, current_scope);
  IR_set_ident (res, ident);
  IR_set_const_flag (res, val_flag);
  IR_set_obj_block (res, obj_block);
  if (obj_block != NULL && IR_fun_class (obj_block) != NULL)
    IR_set_obj_var (IR_fun_class (obj_block), res);
  d_assert (obj_block == NULL || val_flag);
  if (expr == NULL)
    IR_set_next_stmt (res, res);
  else
    {
      IR_node_t init = create_node_with_pos (assign, expr_pos);

      IR_set_assignment_var (init, ident);
      IR_set_assignment_expr (init, expr);
      IR_set_next_stmt (res, init);
      IR_set_next_stmt (init, res);
      res = merge_additional_stmts (init);
    }
  return res;
}

/* Create pattern var node with PATTERN, ACCESS, VAR_FLAG and pattern
   assignment node PATTERN, EXPR and return them.  */
static IR_node_t
process_var_pattern (access_val_t access, IR_node_t pattern,
		     int val_flag, IR_node_t expr, position_t expr_pos,
		     IR_node_mode_t assign)
{
  IR_node_t res, init;
  
  d_assert (expr != NULL);
  res = create_node_with_pos (IR_NM_pattern_var, expr_pos);
  IR_set_pattern_const_flag (res, val_flag);
  IR_set_pattern_var_access (res, access);
  IR_set_pattern (res, pattern);
  init = create_node_with_pos (assign, expr_pos);
  IR_set_assignment_var (init, pattern);
  IR_set_assignment_expr (init, expr);
  IR_set_next_stmt (res, init);
  IR_set_next_stmt (init, res);
  res = merge_additional_stmts (init);
  return res;
}

/* Process function/thread/class header.  */
static void
process_header (int create_block_p, IR_node_t decl, IR_node_t ident)
{
  IR_node_t block;
  
  IR_set_scope (decl, current_scope);
  IR_set_ident (decl, ident);
  if (! create_block_p)
    {
      IR_set_next_stmt (decl, decl);
      IR_set_forward_decl_flag (decl, TRUE);
      return;
    }
  block = create_empty_block (current_scope);
  IR_set_forward_decl_flag (decl, FALSE);
  IR_set_next_stmt (block, decl);
  IR_set_next_stmt (decl, block);
  IR_set_fun_class (block, decl);
  /* This assignment is here for that formal parameters are to be in
     corresponding block. */
  current_scope = block;
}

/* Process formal parameters PARS of fun/thread/class DECL.  Return
   the parameters.  */
static IR_node_t
process_formal_parameters (IR_node_t decl, IR_node_t pars)
{
  IR_node_t next, current_decl = pars, first_assign = NULL;
  
  if (current_decl != NULL)
    do
      {
	current_decl = IR_next_stmt (current_decl);
	if (IR_IS_OF_TYPE (current_decl, IR_NM_var))
	  {
	    IR_set_par_flag (current_decl, TRUE);
	    if (first_assign != NULL && ! formal_parameter_args_flag
		&& (next = IR_next_stmt (current_decl)) != NULL
		&& IR_IS_OF_TYPE (next, IR_NM_var))
	      error (FALSE, IR_pos (first_assign),
		     ERR_default_value_parameters_should_be_last);
	  }
	if (IR_IS_OF_TYPE (current_decl, IR_NM_par_assign))
	  first_assign = current_decl;
      }
    while (current_decl != pars);
  IR_set_args_flag (decl, formal_parameter_args_flag);
  if (formal_parameter_args_flag && first_assign != NULL)
    error (FALSE, IR_pos (first_assign), ERR_default_value_parameter_and_dots);
  return pars;
}


static hint_val_t
get_hint (IR_node_t ident)
{
  const char *str = IR_ident_string (IR_unique_ident (ident));

  if (strcmp (str, "jit") == 0)
    return JIT_HINT;
  else if (strcmp (str, "inline") == 0)
    return INLINE_HINT;
  else if (strcmp (str, "pure") == 0)
    return PURE_HINT;
  else
    {
      error (FALSE, IR_pos (ident), ERR_unknown_hint, str);
      return NO_HINT;
    }
}

/* Process BLOCK of fun/thread/class/obj HEADER decl.  Return the
   block.  */
static IR_node_t
process_header_block (IR_node_t header, IR_node_t block, hint_val_t hint)
{
  IR_node_t res = current_scope; /*i.e. block.*/
  IR_node_t fun = IR_fun_class (res);

  if (fun != NULL && (hint == PURE_HINT || hint == INLINE)
      && (! IR_IS_OF_TYPE (fun, IR_NM_fun) || IR_thread_flag (fun)))
    {
      error (FALSE, IR_pos (fun), ERR_wrong_hint_for_non_fun);
      hint = NO_HINT;
    }
  else if (fun != NULL && hint == JIT_HINT
	   && IR_IS_OF_TYPE (fun, IR_NM_fun) && IR_thread_flag (fun))
    {
      error (FALSE, IR_pos (fun), ERR_jit_hint_for_thread);
      hint = NO_HINT;
    }
  IR_set_hint (res, hint);
  IR_set_block_stmts
    (res, uncycle_stmt_list (merge_stmt_lists (header, block)));
  IR_set_friend_list (res, uncycle_friend_list (IR_friend_list (res)));
  current_scope = IR_block_scope (res);
  return res;
}

static IR_node_t
merge_additional_stmts (IR_node_t list)
{
  IR_node_t res = merge_stmt_lists (additional_stmts, list);

  additional_stmts = NULL;
  return res;
}

static IR_node_t
process_obj_header (IR_node_t ident)
{
  IR_node_t unique_ident, class_ident, class_def;
  
  class_def = create_node_with_pos (IR_NM_class, IR_pos (ident));
  IR_set_final_flag (class_def, TRUE);
  VLO_NULLIFY (temp_scanner_vlo);
  VLO_ADD_STRING (temp_scanner_vlo, "$");
  VLO_ADD_STRING (temp_scanner_vlo, IR_ident_string (IR_unique_ident (ident)));
  unique_ident = create_unique_ident_node (VLO_BEGIN (temp_scanner_vlo));
  class_ident = create_node_with_pos (IR_NM_ident, IR_pos (ident));
  IR_set_unique_ident (class_ident, unique_ident);
  process_header (TRUE, class_def, class_ident);
  return class_ident;
}

static IR_node_t
process_obj_block (IR_node_t ident, IR_node_t class_ident,
		   IR_node_t block_stmts, access_val_t access)
{
  IR_node_t val, expr;
  IR_node_t block = current_scope;
  
  IR_set_block_stmts (block, uncycle_stmt_list (block_stmts));
  IR_set_friend_list (block, uncycle_friend_list (IR_friend_list (block)));
  current_scope = IR_block_scope (block);
  expr = create_node_with_pos (IR_NM_class_fun_thread_call, source_position);
  IR_set_fun_expr (expr, class_ident);
  IR_set_actuals (expr, NULL);
  val = process_var_decl (access, ident, TRUE, block,
			  expr, IR_pos (ident), IR_NM_var_assign);
  return merge_stmt_lists (block, val);
}

static IR_node_t
process_fun_start (IR_node_t fun, int final_flag, access_val_t access)
{
  IR_set_final_flag (fun, final_flag);
  IR_set_access (fun, access);
  return fun;
}

static IR_node_t
create_except_class (IR_node_t before_list, IR_node_t expr)
{
  IR_node_t res;
  position_t pos = current_position;
  pos.column_number--;
  
  res = create_node_with_pos (IR_NM_exception, pos);
  IR_set_exception_class_expr (res, expr);
  IR_set_catch_block (res, NULL);
  if (before_list == NULL)
    IR_set_next_exception (res, res);
  else
    {
      IR_set_next_exception (res, IR_next_exception (before_list));
      IR_set_next_exception (before_list, res);
    }
  return res;
}

static IR_node_t
create_catch_block (position_t pos)
{
  IR_node_t res = create_empty_block (current_scope);

  current_scope = res;
  /* Add variable for catched exception. */
  res = create_node_with_pos (IR_NM_var, pos);
  IR_set_next_stmt (res, res);
  IR_set_scope (res, current_scope);
  IR_set_ident (res,
		get_ident_node (CATCH_EXCEPTION_NAME, current_position));
  return res;
}

static void
finish_catch_block (IR_node_t catch_block, IR_node_t block,
		    IR_node_t excepts, IR_node_t friend_list)
{
  IR_node_t first_exception = IR_next_exception (excepts);
  
  IR_set_block_stmts (current_scope,
		      uncycle_stmt_list (merge_stmt_lists (catch_block, block)));
  IR_set_friend_list (current_scope, friend_list);
  IR_set_catch_block (first_exception, current_scope);
  current_scope = IR_block_scope (current_scope);
}

/* We have try (<stmt>, <except>, ...).  Create and return
   fun {try {<stmt>;return 1;} catch (<except>) {return 0;}} ().

   TRY_BLOCK is already created block for the try-statement. LPAR_POS
   and RPAR_POS are positions of correspondingly left and right
   parenthesis in try-expr.  */
static IR_node_t
create_try_expr (IR_node_t try_block, IR_node_t stmt, IR_node_t excepts,
		 position_t lpar_pos, position_t rpar_pos)
{
  IR_node_t fun, stmt_list, catch_block, fun_expr, call;
  
  fun = IR_fun_class (IR_block_scope (try_block));
  
  /* Create <executive_stmt>; return 1; */
  if (stmt != NULL)
    IR_set_next_stmt (stmt, stmt);
  stmt_list = create_node_with_pos (IR_NM_return_with_result, rpar_pos);
  IR_set_returned_expr (stmt_list, get_int_node (1, rpar_pos));
  IR_set_next_stmt (stmt_list, stmt_list);
  stmt_list = merge_stmt_lists (stmt, stmt_list);
  
  finish_block (); /* finish try-block */
  
  /* create try { <executive_stmt>; return 1; } ... */
  IR_set_block_stmts (try_block, uncycle_stmt_list (stmt_list));
  IR_set_friend_list (try_block, NULL);
  current_scope = IR_block_scope (try_block);
  
  /* create catch (<except>) {...} */
  catch_block = create_catch_block (rpar_pos);
  IR_set_pos (current_scope, rpar_pos);
  start_block (); /* start catch-block */
  
  /* create return 0; */
  stmt_list = create_node_with_pos (IR_NM_return_with_result, rpar_pos);
  IR_set_returned_expr (stmt_list, get_int_node (0, rpar_pos));
  IR_set_next_stmt (stmt_list, stmt_list);
  
   /* finish catch_block: */
  finish_block ();
  finish_catch_block (catch_block, stmt_list, excepts, NULL);
  IR_set_exceptions (try_block, uncycle_exception_list (excepts));
  
  IR_set_pos (current_scope, lpar_pos);
  finish_block (); /* finish function block */
  
  /* Move (header, block) before the current statement: */
  IR_set_next_stmt (try_block, try_block);
  additional_stmts
    = merge_stmt_lists (additional_stmts,
			process_header_block (NULL, try_block, NO_HINT));
  /* create function call  */
  fun_expr = IR_ident (IR_next_stmt (additional_stmts));
  call = create_node_with_pos (IR_NM_class_fun_thread_call, rpar_pos);
  IR_set_fun_expr (call, fun_expr);
  IR_set_actuals (call, NULL);
  return call;
}




/* This page contains abstracr data for reading, storing and
   retrieving lines.  */

/* Container for pointers to read lines.  */
static vlo_t lines_vec;

/* Container for read lines themself.  */
static os_t lines;

/* Initiate the abstract data.  */
static void
initiate_lines (void)
{
  VLO_CREATE (lines_vec, 0);
  OS_CREATE (lines, 0);
}

/* Read next line from file, store it, and return it.  The non-empty
   read line will always have NL at its end.  The trailing `\r' is
   removed.  The empty line means reaching EOF.  */
static const ucode_t *
read_line (FILE *f)
{
  int c;
  ucode_t uc;
  const ucode_t *ln;

  for (;;)
    {
      c = (curr_reverse_ucode_cd == NO_CONV_DESC ? dino_getc (f)
	   : get_ucode_from_stream (read_byte, curr_reverse_ucode_cd,
				    curr_encoding_type, f));
      if (c == EOF || c == '\n')
	break;
      if (c == UCODE_BOUND)
	{
	  /* Skip to the end of line: */
	  do
	    {
	      c = (curr_reverse_ucode_cd == NO_CONV_DESC ? dino_getc (f)
		   : get_ucode_from_stream (read_byte, curr_reverse_ucode_cd,
					    curr_encoding_type, f));
	    }
	  while (c != EOF && c != '\n');
	  OS_TOP_NULLIFY (lines);
	  d_error (TRUE, no_position, ERR_line_decoding, curr_encoding_name);
	}
      uc = c;
      OS_TOP_ADD_MEMORY (lines, &uc, sizeof (ucode_t));
    }
  if (c != EOF || OS_TOP_LENGTH (lines) > 0)
    {
      if (OS_TOP_LENGTH (lines) > 0 && *((ucode_t *) OS_TOP_END (lines)) =='\r')
	OS_TOP_SHORTEN (lines, sizeof (ucode_t));
      uc = '\n';
      OS_TOP_ADD_MEMORY (lines, &uc, sizeof (ucode_t));
    }
  uc = '\0';
  OS_TOP_ADD_MEMORY (lines, &uc, sizeof (ucode_t));
  ln = OS_TOP_BEGIN (lines);
  OS_TOP_FINISH (lines);
  VLO_ADD_MEMORY (lines_vec, &ln, sizeof (ucode_t *));
  return ln;
}

/* Return N-th read line.  */
ucode_t *
get_read_line (int n)
{
  d_assert (n >= 0 && VLO_LENGTH (lines_vec) > n * sizeof (ucode_t *));
  return ((ucode_t **)VLO_BEGIN (lines_vec)) [n];
}

/* Finish the abstract data.  */
static void
finish_lines (void)
{
  OS_DELETE (lines);
  VLO_DELETE (lines_vec);
}




/* This page contains abstract data `istream_stack'.  This abstract
   data opens and closes included files saves and restores scanner
   states corresponding to processed files. */

/* The following structure describes current input stream and scanner
   state and is used for saving and restoring input stream and scanner
   state to process included files. */
struct istream_state
{
  /* The following member is defined only for
     `curr_istream_state'. And its value NULL when the current input
     stream is undefined, command line, or REPL stdin.  */
  FILE *file;
  /* File name of given input stream. Null if the current input stream
     is undefined.  Empty string if the current input stream is
     command line or stdin in case of REPL. */
  const char *file_name;
  /* Current position in file corresponding to given input stream.
     The following member is defined only for structures in the input
     stream stack and if it is not command line stream or REPL
     stdin. */
  long file_pos;
  /* The following member contains parameter value of the function
     `add_lexema_to_file' after immediately call of this
     function or -1 otherwise. */
  int uninput_lexema_code;
  /* Name of file encoding.  */
  const char *encoding_name;
  /* Type of file encoding.  */
  encoding_type_t encoding_type;
  /* Conversion descriptor used for the file.  */
  conv_desc_t cd;
};

/* The following structure contains all current input stream and
   scanner state.  If the member `file_name' value is NULL then input
   stream is not defined. */
static struct istream_state curr_istream_state;

/* All input stream stack is implemented by variable length object.
   See package `vl-object'. */
static vlo_t istream_stack;

/* The following variable is used for storing ungotten code. */
static int previous_char;

/* The following function creates empty input stream stack and
   initiates current input stream and scanner state (including cd) as
   undefined. */
static void
initiate_istream_stack (void)
{
  VLO_CREATE (istream_stack, 0);
  curr_istream_state.file_name = NULL;
  curr_istream_state.file = NULL;
  curr_istream_state.encoding_name = NULL;
  curr_istream_state.cd = NO_CONV_DESC;
  curr_istream_state.uninput_lexema_code = (-1);
}

/* The following function deletes the input stream stack and closes
   current input file and its cd if they are defined.  */
static void
finish_istream_stack (void)
{
  VLO_DELETE (istream_stack);
  if (curr_istream_state.file != NULL)
    fclose (curr_istream_state.file);
#ifdef HAVE_ICONV_H
  if (curr_istream_state.cd != NO_CONV_DESC)
    iconv_close (curr_istream_state.cd);
#endif
}

/* The following function returns height of input stream stack,
   i.e. current file inclusion level. */
int
istream_stack_height (void)
{
  return VLO_LENGTH (istream_stack) / sizeof (struct istream_state);
}

/* Ucode string containing only 0 element.  */
static ucode_t empty_ucode_string[] = {0};

/* The following function saves current input stream and scanner state
   (if it is defined) in the stack, closes file corresponding to
   current input stream and its cd, opens file corresponding to new
   input stream, and sets up new current input stream and scanner
   state including cd.  The function also checks up absence of loop of
   file inclusions.  The function reports errors if the loop exists or
   new file is not opened.  If ENCODING_NAME is not NULL, use the
   corresponding encoding.  Otherwise use the current encoding.  */
static void
push_curr_istream (const char *new_file_name, const char *encoding_name,
		   position_t error_pos)
{
  int i;

  if (curr_istream_state.file_name != NULL)
    {
      /* The current stream is defined. */
      if (*curr_istream_state.file_name != '\0')
	{
	  /* The current stream is not command line or REPL stdin. */
	  d_assert (curr_istream_state.file != stdin);
	  curr_istream_state.file_pos = ftell (curr_istream_state.file);
	  fclose (curr_istream_state.file);
	  curr_istream_state.file = NULL;
#ifdef HAVE_ICONV_H
	  if (curr_istream_state.cd != NO_CONV_DESC)
	    iconv_close (curr_istream_state.cd);
	  curr_istream_state.cd = NO_CONV_DESC;
#endif
	}
      VLO_ADD_MEMORY (istream_stack, &curr_istream_state,
		      sizeof (struct istream_state));
      for (i = 0; i < istream_stack_height (); i++)
	if (strcmp (((struct istream_state *) VLO_BEGIN (istream_stack))
		    [i].file_name, new_file_name) == 0)
	  error (TRUE, error_pos,
		 "fatal error -- cycle on inclusion of file `%s'",
		 new_file_name);
    }
  curr_istream_state.file_name = new_file_name;
  curr_istream_state.file = NULL;
  curr_istream_state.cd = NO_CONV_DESC;
  if (*new_file_name != '\0')
    {
      /* The current stream is not commad line or REPL stdin. */
      curr_istream_state.file = fopen (new_file_name, "rb");
      if (curr_istream_state.file == NULL)
	system_error (TRUE, error_pos, "fatal error -- `%s': ", 
		      curr_istream_state.file_name);
      curr_istream_state.encoding_name
	= get_unique_string (encoding_name != NULL
			     ? encoding_name : curr_encoding_name);
      if (! set_conv_descs (curr_istream_state.encoding_name,
			    NULL, NULL, &curr_istream_state.cd,
			    &curr_istream_state.encoding_type))
	d_error (TRUE, no_position, ERR_source_file_encoding,
		 curr_istream_state.encoding_name, curr_istream_state.file_name);
      else if (! check_encoding_on_ascii (curr_istream_state.encoding_name))
	{
#ifdef HAVE_ICONV_H
	  if (curr_istream_state.cd != NO_CONV_DESC)
	    iconv_close (curr_istream_state.cd);
#endif
	  d_error (TRUE, no_position, ERR_non_ascii_source_file_encoding,
		   curr_istream_state.encoding_name, curr_istream_state.file_name);
	}
    }
  else if (repl_flag)
    /* To read line when we need it. */
    command_line_program = empty_ucode_string;
  previous_char = NOT_A_CHAR;
  start_file_position (curr_istream_state.file_name);
  curr_istream_state.uninput_lexema_code = (-1);
}

/* The following function closes file corresponding to current input
   stream (it must be defined) and its cdq, reopens file corresponding
   to previous input stream (if it is defined) and restores previous
   input stream and scanner state (including file cd).  The function
   can fix error if the file is not reopened. */
static void
pop_istream_stack (void)
{
  d_assert (curr_istream_state.file_name != NULL);
  if (curr_istream_state.file != NULL)
    {
      d_assert (istream_stack_height () == 0
		|| curr_istream_state.file != stdin);
      fclose (curr_istream_state.file);
      curr_istream_state.file = NULL;
#ifdef HAVE_ICONV_H
      if (curr_istream_state.cd != NO_CONV_DESC)
	iconv_close (curr_istream_state.cd);
      curr_istream_state.cd = NO_CONV_DESC;
#endif
    }
  if (istream_stack_height () != 0)
    {
      curr_istream_state
	= (((struct istream_state *) VLO_BEGIN (istream_stack))
	   [istream_stack_height () - 1]);
      VLO_SHORTEN (istream_stack, sizeof (struct istream_state));
      if (*curr_istream_state.file_name != '\0')
	{
	  /* It is not command line stream or REPL stdin. */
	  curr_istream_state.file = fopen (curr_istream_state.file_name, "rb");
	  curr_istream_state.cd = NO_CONV_DESC;
	  if (curr_istream_state.file == NULL
	      || fseek (curr_istream_state.file,
			curr_istream_state.file_pos, 0) != 0)
	    system_error (TRUE, no_position,
			  "fatal error -- repeated opening file `%s': ", 
			  curr_istream_state.file_name);
	  if (! set_conv_descs (curr_istream_state.encoding_name,
				NULL, NULL, &curr_istream_state.cd,
				&curr_istream_state.encoding_type))
	    /* We already used the file encoding before.  */
	    d_assert (FALSE);
	}
      finish_file_position ();
    }
}

/* The following field returns directory name of given file name. */
static const char *
file_dir_name (const char *file_name)
{
  const char *last_slash;
  const char *curr_char_ptr;
  const char *result;

  d_assert (file_name != NULL);
  for (curr_char_ptr = file_name, last_slash = NULL;
       *curr_char_ptr != '\0';
       curr_char_ptr++)
    if (*curr_char_ptr == '/')
      last_slash = curr_char_ptr;
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

  d_assert (directory_name != NULL);
  IR_TOP_ADD_STRING (directory_name);
  if (strlen (directory_name) != 0
      && directory_name [strlen (directory_name) - 1] != '/')
    IR_TOP_ADD_STRING ("/");
  IR_TOP_ADD_STRING (file_name);
  IR_TOP_ADD_STRING (file_suffix);
  result = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  return result;
}

#include <pwd.h>

static const char *
canonical_path_name (const char *name)
{
  char buf [PATH_MAX + 1];
  char *p, *str, *result;
  int sep, sep1;
  sep = sep1 = '/';
  if (*name != sep && *name != sep1)
    {
      getcwd (buf, PATH_MAX);
      IR_TOP_ADD_STRING (buf);
      IR_TOP_SHORTEN (1);
      IR_TOP_ADD_BYTE (sep);
      IR_TOP_ADD_BYTE ('\0');
    }
  IR_TOP_ADD_STRING (name);
  result = IR_TOP_BEGIN ();
  IR_TOP_FINISH ();
  for (p = result; *p != '\0'; p++)
    if (*p == sep1)
      *p = sep;
  /* Remove // /. /<something>/.. */
  for (p = result; *p != '\0';)
    if (*p == sep && p[1] == sep)
      memmove (p, p + 1, strlen (p));
    else if (*p == sep && p[1] == '.' && p[2] == sep)
      memmove (p, p + 2, strlen (p + 1));
    else if (p == result && *p == sep && p[1] == '.' && p[2] == '.')
      memmove (p, p + 3, strlen (p + 2));
    else if (*p == sep && p[1] == '.' && p[2] == '.' && p[3] == sep)
      {
        if (p == result)
          memmove (p, p + 3, strlen (p + 2));
        else
          {
            for (str = p - 1; *str != sep; str--)
              ;
            memmove (str, p + 3, strlen (p + 2));
            p = str;
	  }
      }
    else
      p++;
  return (const char *) result;
}

static int
skip_spaces (const char *str, size_t from)
{
  for (; isspace_ascii (str[from]);from++)
    ;
  return from;
}

/* The function finds the first string described by a pattern

   -\*-[ \t]*coding:[ \t]*[_-A-Za-z0-9/.:]+[ \t]*-\*- 

  in LN and return the name after "coding:".  Return NULL if we did
  not find encoding.  */
static const char *
find_encoding (char *ln)
{
  size_t i, len, start, bound;
  static const char *prefix = "coding:";
  
  len = strlen (prefix);
  for (i = 0; ln[i] != 0; i++)
    if (ln[i] == '-' && ln[i + 1] == '*' && ln[i + 2] == '-')
      {
         start = i;
         i = skip_spaces (ln, i + 3);
	 if (strncmp (ln + i, prefix, len) != 0)
	  {
	    /* No "coding:" -- start scanning from second "-" in
	       "-*-".  */
            i = start + 1;
            continue;
	  }
         start = i = skip_spaces (ln, i + len);
         for (; ln[i] != 0; i++)
	   if (! isalpha_ascii (ln[i]) && ! isdigit_ascii (ln[i])
	       && ln[i] != '-' && ln[i] != '_'
	       && ln[i] != '/' && ln[i] != '.' && ln[i] != ':')
	     break;
	 if (ln[i] == 0)
	   return NULL;
	 bound = i;
	 if (start == bound)
	   return NULL;
	 i = skip_spaces (ln, i);
	 if (ln[i] == '-' && ln[i + 1] == '*' && ln[i + 2] == '-')
	   {
	     ln[bound] = '\0';
	     return get_unique_string (ln + start);
	   }
	 i--;
      }
    return NULL;
}

static char *
read_str_line (FILE *f, vlo_t *container)
{
  int c;
  
  VLO_NULLIFY (*container);
  while ((c = dino_getc (f)) != EOF && c != '\n')
    VLO_ADD_BYTE (*container, c);
  VLO_ADD_BYTE (*container, '\0');
  return VLO_BEGIN (*container);
}

/* Find encoding on the first two lines of file F and return it.  If
   it is not found, return NULL.  */
static const char *
read_file_encoding (FILE *f)
{
  char *ln;
  const char *name;
  
  if (((ln = read_str_line (f, &temp_scanner_vlo)) != NULL
       && (name = find_encoding (ln)) != NULL)
      || ((ln = read_str_line (f, &temp_scanner_vlo)) != NULL
	  && (name = find_encoding (ln)) != NULL))
    return name;
  return NULL;
}

/* The following function returns full file name.  To make this
   functions searches for files in
     1. current directory (when the current stream is command line stream)
        or in directory in which source file with the corresponding
        include-clause is placed
     2. and in directories
        given in the command line of DINO and DINO environment variable
        DINO_PATH.
     3. Standard library directory.
   If the file is not found the function returns the extended
   specification file name mentioned in 1.  The function also returns
   file encoding through ENCODING if it can read encoing name in the
   file, or NULL otherwise. */
static const char *
get_full_file_and_encoding_name (IR_node_t ir_fname, const char **encoding)
{
  const char *fname;
  const char *curr_directory_name;
  const char *real_file_name;
  const char *file_name;
  const char **path_directory_ptr;
  size_t i, len;
  FILE *curr_file;

  curr_directory_name
    = (*curr_istream_state.file_name == '\0'
       ? "" : file_dir_name (curr_istream_state.file_name));
  VLO_NULLIFY (temp_scanner_vlo);
  VLO_NULLIFY (temp_scanner_vlo2);
  if (IR_IS_OF_TYPE (ir_fname, IR_NM_string))
    {
      VLO_ADD_STRING (temp_scanner_vlo2,
		      IR_string_value (IR_unique_string (ir_fname)));
      fname = encode_byte_str_vlo (VLO_BEGIN (temp_scanner_vlo2),
				   curr_byte_cd, curr_encoding_type,
				   &temp_scanner_vlo, &len);
      if (fname != NULL)
	{
	  /* Check NULL bytes:  */
	  for (i = 0; i < len && fname[i] != 0; i++)
	    ;
	  if (i < len)
	    fname = NULL;
	}
    }
  else if (curr_ucode_cd != NO_CONV_DESC) /* ucodestr */
    {
      ucodestr_t ustr = IR_ucodestr_value (IR_unique_ucodestr (ir_fname));
      
      for (len = 0; ustr[len] != 0; len++)
	;
      VLO_ADD_MEMORY (temp_scanner_vlo2, ustr, sizeof (ucode_t) * (len + 1));
      fname = encode_ucode_str_vlo (VLO_BEGIN (temp_scanner_vlo2),
				    curr_ucode_cd, curr_encoding_type,
				    &temp_scanner_vlo, &len);
      if (fname != NULL)
	{
	  for (i = 0; i < len && fname[i] != 0; i++)
	    ;
	  if (i < len)
	    fname = NULL;
	}
    }
  else
    fname = encode_ucode_str_to_raw_vlo (IR_ucodestr_value (IR_unique_ucodestr (ir_fname)),
					 &temp_scanner_vlo);
  if (fname == NULL)
    error (TRUE, IR_pos (ir_fname),
	   ERR_file_name_cannot_represented_in_current_encoding);
  real_file_name = file_path_name (curr_directory_name, fname,
                                   STANDARD_INPUT_FILE_SUFFIX);
  curr_file = fopen (real_file_name, "rb");
  if (curr_file == NULL)
    for (path_directory_ptr = include_path_directories;
         *path_directory_ptr != NULL;
         path_directory_ptr++)
      {
        file_name = file_path_name (*path_directory_ptr, fname,
	                            STANDARD_INPUT_FILE_SUFFIX);
        curr_file = fopen (file_name, "rb");
        if (curr_file != NULL)
          {
            real_file_name = file_name;
            break;
          }
      }
  if (encoding != NULL)
    {
      *encoding = NULL;
      if (curr_file != NULL)
	*encoding = read_file_encoding (curr_file);
    }
  if (curr_file != NULL && fclose (curr_file) == EOF)
    system_error (TRUE, no_position, "fatal error -- `%s': ", real_file_name);
  return canonical_path_name (real_file_name);
}

/* Return file encoding found in FNAME.  If the file can not be read
   or there is no encoding in the file, return NULL.  */
const char *
source_file_encoding (const char *fname)
{
  FILE *f;
  const char *res;
  
  f = fopen (fname, "rb");
  if (f == NULL)
    return NULL;
  res = read_file_encoding (f);
  fclose (f);
  return res;
}



/* This page contains abstract data `scanner'.  This abstract data
   divides input stream characters on tokens (lexemas).

   There is requirement of back control (from parser to scanner).
   This back control is needed to switch to processing new file after
   recognizing string in construction `include'.  After recognizing
   the first string the parser is to tell scanner that the next lexema
   must be INCLUSSION.  After getting such lexema the parser is
   switched correctly onto processing new file.
    
   When EOF is read the scanner returns END_OF_FILE only when input
   stream stack is empty, otherwise returns lexema
   END_OF_INCLUDE_FILE.  */

#include "d_kw.c"

/* The following variable referes for current char of parsed
   environment.  If it refers for zero character, the environment has
   been parsed already. */
static const char *environment;

/* The following variable is position which will be set up after the
   parsing environment. */
static position_t after_environment_position;

/* The variable is used for implementation of d_getc when reading from
   the command line string. */
static int curr_char_number;

/* True if we did not see non-blank chars yet for the current REPL
   bunch of stmts.  */
static int first_repl_empty_line_p;

/* Getc for dino.  Reading unicode and replacing "\r\n" onto "\n". */
static int
d_getc (void)
{
  int result;

  if (*environment != '\0')
    {
      result = *environment++;
      /* Environment is always Latin-1 string.  */
      d_assert (in_byte_range_p (result));
      if (*environment == 0)
	{
	  /* Restore the position. */
	  current_position = after_environment_position;
	  if (result == '\n')
	    /* It will be incremented back. */
	    current_position.line_number--;
	}
    }
  else if (curr_istream_state.file != NULL)
    {
      if (previous_char == NOT_A_CHAR)
	{
	  result
	    = (curr_reverse_ucode_cd == NO_CONV_DESC
	       ? dino_getc (curr_istream_state.file)
	       : get_ucode_from_stream (read_byte, curr_istream_state.cd,
					curr_istream_state.encoding_type,
					curr_istream_state.file));
	  if (result == UCODE_BOUND)
	    d_error (TRUE, current_position, ERR_file_decoding,
		     curr_istream_state.encoding_name);
	}
      else
	{
	  result = previous_char;
	  previous_char = NOT_A_CHAR;
	}
      if (result == '\r')
	{
	  result
	    = (curr_istream_state.cd == NO_CONV_DESC
	       ? dino_getc (curr_istream_state.file)
	       : get_ucode_from_stream (read_byte, curr_istream_state.cd,
					curr_istream_state.encoding_type,
					curr_istream_state.file));
	  if (result == UCODE_BOUND)
	    d_error (TRUE, current_position, ERR_file_decoding,
		     curr_istream_state.encoding_name);
	  if (result != '\n')
	    {
	      previous_char = result;
	      result = '\r';
	    }
	}
    }
  else
    {
      d_assert (command_line_program != NULL);
      result = command_line_program [curr_char_number];
      if (repl_flag && result == 0)
	{
	  command_line_program = read_line (stdin);
	  if (first_repl_empty_line_p)
	    {
	      int c;

	      for (curr_char_number = 0;
		   (c = command_line_program [curr_char_number]) == ' '
		    || c == '\t' || c == '\n' || c == '\r' || c == 'f';
		   curr_char_number++)
		;
	      first_repl_empty_line_p = c == '\0';
	      /* Don't reuse curr_char_number value, we need to process
		 positions correctly.  */
	    }
	  curr_char_number = 0;
	  result = *command_line_program;
	}
      if (result == 0)
	result = EOF;
      else
	curr_char_number++;
    }
  return result;
}

/* Ungetc for dino. */
static void
d_ungetc (int ch)
{
  if (*environment != '\0')
    environment--;
  else if (curr_istream_state.file != NULL)
     {
       d_assert (previous_char == NOT_A_CHAR);
       previous_char = ch;
     }
   else
     {
       d_assert (command_line_program != NULL);
       if (curr_char_number != 0 && ch != EOF)
	 curr_char_number--;
     }
}

/* Used by REPL to skip the all line.  */
void
skip_line_rest (void)
{
  d_assert (repl_flag && *environment == 0);
  /* Close all include files.  */
  while (istream_stack_height () != 0)
    {
      previous_char = NOT_A_CHAR;
      pop_istream_stack ();
    }
  command_line_program = empty_ucode_string;
  curr_char_number = 0;
  current_position.line_number++;
}

/* Var length string used by function yylval for text presentation of
   the symbol. */
static vlo_t symbol_text;

/* The following function recognizes next source symbol from the input
   file, returns its code, modifies var current_position so that its
   value is equal to position of the current character in the input
   file and sets up var source_position so that its value is equal to
   position of the returned symbol start, creates corresponding code
   node (if it is needed) and sets up yylval to the node address.  The
   function skips all white spaces and commentaries and fixes all
   lexical errors. */
int
yylex (void)
{
  int wrong_escape_code;
  int input_char;
  ucode_t uc;
  int number_of_successive_error_characters;
  int last_repl_process_flag = repl_process_flag;

  repl_process_flag = FALSE;
  if (curr_istream_state.uninput_lexema_code >= 0)
    {
      int result;
      
      yylval.pos = source_position = current_position;
      result = curr_istream_state.uninput_lexema_code;
      curr_istream_state.uninput_lexema_code = (-1);
      return result;
    }
  VLO_NULLIFY (symbol_text);
  for (number_of_successive_error_characters = 0;;)
    {
      input_char = d_getc ();
      /* `current_position' corresponds `input_char' here */
      switch (input_char)
        {
          /* Break results in skipping all white spaces. */
        case ' ':
        case '\f':
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
	  if (repl_flag)
	    {
	      if (last_repl_process_flag)
		return END_OF_FILE;
	      if (*environment == 0)
		{
		  if (first_repl_empty_line_p)
		    print_stmt_prompt ();
		  else
		    print_stmt_cont_prompt ();
		}
	    }
          break;
        case '\r':
          current_position.column_number++;
          break;
        case '~':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return XOR_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '~';
            }
        case '+':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '+')
	    {
	      current_position.column_number++;
	      return INCR;
	    }
          else if (input_char == '=')
	    {
	      current_position.column_number++;
	      return PLUS_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '+';
            }
        case '-':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '-')
	    {
	      current_position.column_number++;
	      return DECR;
	    }
          else if (input_char == '=')
	    {
	      current_position.column_number++;
	      return MINUS_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '-';
            }
        case '=':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
              input_char = d_getc ();
	      if (input_char == '=')
		{
		  current_position.column_number++;
		  return IDENTITY;
		}
              else
               {
                 d_ungetc (input_char);
                 return EQ;
               }
	    }
          else
            {
              d_ungetc (input_char);
	      return '=';
            }
        case '@':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return CONCAT_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '@';
            }
        case '<':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return LE;
	    }
          else if (input_char == '<')
            {
	      current_position.column_number++;
              input_char = d_getc ();
	      if (input_char == '=')
		{
		  current_position.column_number++;
		  return LSHIFT_ASSIGN;
		}
              else
               {
                 d_ungetc (input_char);
                 return LSHIFT;
               }
            }
          else
            {
              d_ungetc (input_char);
              return '<';
            }
        case '>':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return GE;
	    }
          else if (input_char == '>')
            {
	      current_position.column_number++;
              input_char = d_getc ();
	      if (input_char == '=')
		{
		  current_position.column_number++;
		  return ASHIFT_ASSIGN;
		}
	      else if (input_char == '>')
                {
		  current_position.column_number++;
                  input_char = d_getc ();
	          if (input_char == '=')
		    {
		      current_position.column_number++;
		      return RSHIFT_ASSIGN;
		    }
                  else
                   {
                     d_ungetc (input_char);
                     return RSHIFT;
                   }
                }
              else
                {
                  d_ungetc (input_char);
                  return ASHIFT;
                }
            }
          else
            {
              d_ungetc (input_char);
              return '>';
            }
        case '*':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return MULT_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '*';
            }
        case '/':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return DIV_ASSIGN;
	    }
          else if (input_char == '/')
	    {
              /* commentary */
              for (;;)
                {
		  current_position.column_number++;
                  input_char = d_getc ();
                  if (input_char == '\n')
		    {
		      current_position.column_number = 1;
		      current_position.line_number++;
		      break;
		    }
                  else if (input_char == EOF)
                    break;
		}
	      break;
	    }
          else if (input_char == '*')
	    {
	      current_position.column_number++;
              for (;;)
                {
                  input_char = d_getc ();
		  if (input_char == '*')
		    {
		      current_position.column_number++;
		      input_char = d_getc ();
		      if (input_char == '/')
			{
			  current_position.column_number++;
			  break;
			}
		      else
			d_ungetc (input_char);
		    }
		  else if (input_char == '\n')
		    {
		      current_position.column_number = 1;
		      current_position.line_number++;
		    }
                  else if (input_char == EOF)
		    {
		      error (FALSE, current_position, ERR_eof_in_comment);
		      break;
		    }
		}
	      break;
	    }
          else
            {
              d_ungetc (input_char);
              return '/';
            }
        case '&':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '&')
	    {
	      current_position.column_number++;
	      return LOGICAL_AND;
	    }
          else if (input_char == '=')
	    {
	      current_position.column_number++;
	      return AND_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '&';
            }
        case '|':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '|')
	    {
	      current_position.column_number++;
	      return LOGICAL_OR;
	    }
          else if (input_char == '=')
	    {
	      current_position.column_number++;
	      return OR_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '|';
            }
        case '%':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return MOD_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '%';
            }
        case '^':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return XOR_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '^';
            }
        case '!':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
              input_char = d_getc ();
	      if (input_char == '=')
		{
		  current_position.column_number++;
		  return UNIDENTITY;
		}
              else
               {
                 d_ungetc (input_char);
                 return NE;
               }
	    }
          else
            {
              d_ungetc (input_char);
              return '!';
            }
        case '.':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '.')
	    {
	      current_position.column_number++;
	      input_char = d_getc ();
	      if (input_char == '.')
		{
		  current_position.column_number++;
		  return DOTS;
		}
	      else
		{
		  current_position.column_number--;
		  error (FALSE, current_position, ERR_invalid_input_char);
		  current_position.column_number++;
		  d_ungetc (input_char);
		  return '.';
		}
	    }
          else if (input_char == '+')
	    {
	      current_position.column_number++;
	      return FOLD_PLUS;
	    }
          else if (input_char == '*')
	    {
	      current_position.column_number++;
	      return FOLD_MULT;
	    }
          else if (input_char == '&')
	    {
	      current_position.column_number++;
	      return FOLD_AND;
	    }
          else if (input_char == '^')
	    {
	      current_position.column_number++;
	      return FOLD_XOR;
	    }
          else if (input_char == '|')
	    {
	      current_position.column_number++;
	      return FOLD_OR;
	    }
          else if (input_char == '@')
	    {
	      current_position.column_number++;
	      return FOLD_CONCAT;
	    }
	  else
            {
              d_ungetc (input_char);
              return '.';
            }
        case ':':
        case ',':
        case ';':
        case '?':
        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case '#':
	  yylval.pos = source_position = current_position;
          current_position.column_number++;
          return input_char;
        case EOF:
	  yylval.pos = source_position = current_position;
          return (istream_stack_height () == 0
		  ? END_OF_FILE : END_OF_INCLUDE_FILE);
        case '\'':
          {
            IR_node_t unique_char_node_ptr;
            int correct_newln;
	    int character_code;
            
	    source_position = current_position;
	    current_position.column_number++;
            input_char = d_getc ();
	    current_position.column_number++;
            if (input_char == '\'')
	      {
		current_position.column_number--;
		error (FALSE, current_position, ERR_invalid_char_constant);
		current_position.column_number++;
	      }
            else
              {
                input_char = read_dino_string_code (input_char, &correct_newln,
						    &wrong_escape_code,
						    d_getc, d_ungetc);
                if (input_char < 0 || correct_newln)
		  {
		    current_position.column_number--;
		    error (FALSE, current_position, ERR_invalid_char_constant);
		    current_position.column_number++;
		  }
		else if (wrong_escape_code)
		  {
		    current_position.column_number--;
		    error (FALSE, current_position, ERR_invalid_escape_code);
		    current_position.column_number++;
		  }
              }
	    current_position.column_number++;
            character_code = d_getc ();
            if (character_code != '\'')
              {
		current_position.column_number--;
                d_ungetc (character_code);
                error (FALSE, current_position, ERR_invalid_char_constant);
              }
            IR_set_char_value (temp_unique_char, input_char);
            unique_char_node_ptr = *find_table_entry (temp_unique_char, FALSE);
            if (unique_char_node_ptr == NULL)
              {
                unique_char_node_ptr = create_node (IR_NM_unique_char);
                IR_set_char_value (unique_char_node_ptr, input_char);
                include_to_table (unique_char_node_ptr);
              }
            yylval.pointer = create_node_with_pos (IR_NM_char,
						   source_position);
            IR_set_unique_char (yylval.pointer, unique_char_node_ptr);
            return CHARACTER;
          }
        case '\"':
        case '`':
          {
            int correct_newln, unicode_p;
            IR_node_t unique_string_node_ptr;
            char *string_value_in_code_memory;
	    int no_escape_p = input_char == '`';
            
	    source_position = current_position;
	    current_position.column_number++;
	    unicode_p = FALSE;
            for (;;)
              {
                input_char = d_getc ();
		current_position.column_number++;
		if (no_escape_p)
		  {
		    if (input_char == '`')
		      {
			input_char = d_getc ();
			if (input_char == '`')
			  current_position.column_number++;
			else
			  {
			    d_ungetc (input_char);
			    break;
			  }
		      }
		    else if (input_char == '\n')
		      {
			d_ungetc (input_char);
			error (FALSE, current_position, ERR_string_end_absence);
			break;
		      }
		  }
                else
		  {
		    if (input_char == '\"')
		      break;
		    input_char = read_dino_string_code (input_char, &correct_newln,
							&wrong_escape_code,
							d_getc, d_ungetc);
		  }
		if (input_char < 0)
                  {
                    error (FALSE, current_position, ERR_string_end_absence);
                    break;
                  }
                if (! no_escape_p && wrong_escape_code)
                  {
		    error (FALSE, current_position, ERR_invalid_escape_code);
                    continue;
                  }
                if (no_escape_p || ! correct_newln)
		  {
		    if (! unicode_p && ! in_byte_range_p (input_char))
		      {
			/* Transform accumulated string into ucode
			   string: */
			unicode_p = TRUE;
			copy_vlo (&temp_scanner_vlo, &symbol_text);
			str_to_ucode_vlo (&symbol_text,
					  VLO_BEGIN (temp_scanner_vlo),
					  VLO_LENGTH (symbol_text));
		      }
		    if (unicode_p)
		      {
			uc = input_char;
			VLO_ADD_MEMORY (symbol_text, &uc, sizeof (ucode_t));
		      }
		    else
		      VLO_ADD_BYTE (symbol_text, input_char);
		  }
              }
	    if (unicode_p)
	      {
		uc = '\0';
		VLO_ADD_MEMORY (symbol_text, &uc, sizeof (ucode_t));
		IR_set_ucodestr_value (temp_unique_ucodestr,
				       VLO_BEGIN (symbol_text));
		d_assert (VLO_LENGTH (symbol_text) > sizeof (ucode_t));
		IR_set_ucodestr_size
		  (temp_unique_ucodestr,
		   VLO_LENGTH (symbol_text) - sizeof (ucode_t));
		unique_string_node_ptr = *find_table_entry (temp_unique_ucodestr,
							    FALSE);
		if (unique_string_node_ptr == NULL)
		  {
		    unique_string_node_ptr
		      = create_unique_node_with_string
		        (IR_NM_unique_ucodestr, VLO_BEGIN (symbol_text),
			 VLO_LENGTH (symbol_text),
			 &string_value_in_code_memory);
		    IR_set_ucodestr_value (unique_string_node_ptr,
					     (ucodestr_t) string_value_in_code_memory);
		    IR_set_ucodestr_size
		      (unique_string_node_ptr,
		       VLO_LENGTH (symbol_text) - sizeof (ucode_t));
		    include_to_table (unique_string_node_ptr);
		  }
		yylval.pointer = create_node_with_pos (IR_NM_ucodestr,
						       source_position);
		IR_set_unique_ucodestr (yylval.pointer,
					unique_string_node_ptr);
	      }
	    else
	      {
		VLO_ADD_BYTE (symbol_text, '\0');
		IR_set_string_value (temp_unique_string, VLO_BEGIN (symbol_text));
		unique_string_node_ptr = *find_table_entry (temp_unique_string,
							    FALSE);
		if (unique_string_node_ptr == NULL)
		  {
		    unique_string_node_ptr
		      = create_unique_node_with_string
		      (IR_NM_unique_string, VLO_BEGIN (symbol_text),
		       VLO_LENGTH (symbol_text),
		       &string_value_in_code_memory);
		    IR_set_string_value (unique_string_node_ptr,
					 string_value_in_code_memory);
		    include_to_table (unique_string_node_ptr);
		  }
		yylval.pointer = create_node_with_pos (IR_NM_string,
						       source_position);
		IR_set_unique_string (yylval.pointer, unique_string_node_ptr);
	      }
            return STRING;
          }
        default:
          if (isalpha_ascii (input_char) || input_char == '_' )
            {
              int keyword;
              
	      yylval.pos = source_position = current_position;
              /* Ident recognition. */
              do
                {
		  current_position.column_number++;
                  VLO_ADD_BYTE (symbol_text, input_char);
                  input_char = d_getc ();
                }
              while (isalpha_ascii (input_char) || isdigit_ascii (input_char)
                     || input_char == '_');
              d_ungetc (input_char);
              VLO_ADD_BYTE (symbol_text, '\0');
              keyword = KR_find_keyword (VLO_BEGIN (symbol_text),
                                         VLO_LENGTH (symbol_text) - 1);
              if (keyword != 0)
                return keyword;
              else
                {
                  IR_node_t unique_ident;
                  
                  unique_ident
                    = create_unique_ident_node (VLO_BEGIN (symbol_text));
                  yylval.pointer = create_node_with_pos (IR_NM_ident,
							 source_position);
                  IR_set_unique_ident (yylval.pointer, unique_ident);
                  return IDENT;
                }
            }
          else if (isdigit_ascii (input_char))
            {
              /* Recognition numbers. */
	      enum read_number_code err_code;
	      int read_ch_num, float_p, long_p, base;
	      const char *result; /* Number is always ASCII sequence.  */

	      source_position = current_position;
	      current_position.column_number++;
	      err_code = read_dino_number (input_char, d_getc, d_ungetc, &read_ch_num,
					   &result, &base, &float_p, &long_p);
	      if (err_code == ABSENT_EXPONENT)
		{
		  error (FALSE, source_position, ERR_exponent_absence);
		  yylval.pointer = get_float_node (0.0, source_position);
		}
	      else if (err_code == NON_DECIMAL_FLOAT)
		{
		  error (FALSE, source_position,
			 ERR_float_value_not_in_decimal_base);
		  yylval.pointer = get_float_node (0.0, source_position);
		}
	      else if (err_code == WRONG_OCTAL_INT)
		{
		  error (FALSE, source_position, ERR_octal_int_value);
		  yylval.pointer = get_int_node (0, source_position);
		}
	      else
		{
		  d_assert (err_code == NUMBER_OK);
		  if (long_p)
		    yylval.pointer = get_long_node (result,
						    source_position, base);
		  else
		    {
		      if (float_p)
			{
			  yylval.pointer
			    = get_float_node (a2f (result), source_position);
			  if (errno)
			    error (FALSE, source_position, ERR_float_value);
			}
		      else
			{
			  yylval.pointer
			    = get_int_node (a2i (result, base), source_position);
			  if (errno)
			    error (FALSE, source_position, ERR_int_value);
			}
		    }
		}
	      current_position.column_number += read_ch_num;
              return NUMBER;
            }
          else
            {
              number_of_successive_error_characters++;
              if (number_of_successive_error_characters == 1)
                error (FALSE, current_position, ERR_invalid_input_char);
              current_position.column_number++;
            }
        }
    }
}

static IR_node_t
get_new_ident (position_t pos)
{
  IR_node_t ident, unique_ident;
  char str [50]; /* Enough for integer representation.  */

  VLO_NULLIFY (symbol_text);
  VLO_ADD_STRING (symbol_text, "$anon");
  if (pos.file_name != NULL && *pos.file_name != 0)
    {
      VLO_ADD_STRING (symbol_text, ".");
      VLO_ADD_STRING (symbol_text, pos.file_name);
    }
  VLO_ADD_STRING (symbol_text, ".ln");
  sprintf (str, "%d", pos.line_number);
  VLO_ADD_STRING (symbol_text, str);
  VLO_ADD_STRING (symbol_text, ".pos");
  sprintf (str, "%d", pos.column_number);
  VLO_ADD_STRING (symbol_text, str);
  unique_ident = create_unique_ident_node (VLO_BEGIN (symbol_text));
  ident = create_node_with_pos (IR_NM_ident, pos);
  IR_set_unique_ident (ident, unique_ident);
  return ident;
}

/* The following function initiates internal state of the scanner.
   The function must be called only once before any work with the
   scanner. */
void
initiate_scanner (void)
{
  if (repl_flag)
    initiate_lines ();
  initiate_istream_stack ();
  curr_char_number = 0;
  environment = ENVIRONMENT;
  VLO_CREATE (symbol_text, 500);
  VLO_CREATE (temp_scanner_vlo, 500);
  VLO_CREATE (temp_scanner_vlo2, 500);
}

/* The following function is called to tune the scanner on input
   stream from given file.  The function is needed for nested
   procession of included files.  If the FILE_NAME is empty string
   then it is command line stream.  If it is the first scanner file,
   special code for correct diganostic during and after parsing
   environment is executed. */
void
start_scanner_file (const char *new_file_name, const char *encoding_name,
		    position_t error_pos)
{
  push_curr_istream (new_file_name, encoding_name, error_pos);
  if (*environment != 0)
    {
      /* Environment is not processed yet. Save the position. */
      after_environment_position = current_position;
      /* File name for environment. */
      current_position.file_name = ENVIRONMENT_PSEUDO_FILE_NAME;
    }
}

/* The following function restores scanner state concerning previous
   input stream with the aid of abstract data `istream_stack'. */
static void
finish_scanner_file (void)
{
  pop_istream_stack ();
}

/* The following function is called from the parser and points out
   that lexema given as parameter will be returned by following
   function `yylex' call. */
static void
add_lexema_to_file (int lexema_code)
{
  d_assert (lexema_code >= 0);
  curr_istream_state.uninput_lexema_code = lexema_code;
}

/* The function frees all memory allocated during the scanner work. */
void
finish_scanner (void)
{
  VLO_DELETE (symbol_text);
  VLO_DELETE (temp_scanner_vlo);
  VLO_DELETE (temp_scanner_vlo2);
  if (repl_flag)
    finish_lines ();
  finish_istream_stack ();
}



/* This page contains functions needed for processing
   include-clauses. */

/* Current covering block level (1, 2, ...).  0 means no covering
   block. */
static int block_level = 0;

/* VLO for pointers to names of include files and numbers of include
   file names for each covering blocks. */
static vlo_t include_file_names;

/* Number of include files for the current block. */
static int curr_block_include_file_names_number;

/* The function starts new block.  It should be also called for the
   implicit top-level block. */
static void
start_block (void)
{
  if (block_level == 0)
    VLO_CREATE (include_file_names, 0);
  else
    VLO_ADD_MEMORY (include_file_names, &curr_block_include_file_names_number,
		    sizeof (curr_block_include_file_names_number));
  curr_block_include_file_names_number = 0;
  block_level++;
}

/* The function finishes the block. */
static void
finish_block (void)
{
  d_assert (block_level > 0);
  block_level--;
  if (block_level == 0)
    VLO_DELETE (include_file_names);
  else
    {
      VLO_SHORTEN (include_file_names,
		   curr_block_include_file_names_number * sizeof (char *));
      memcpy (&curr_block_include_file_names_number,
	      (char *) VLO_END (include_file_names)
	      - sizeof (curr_block_include_file_names_number) + 1,
	      sizeof (curr_block_include_file_names_number));
      VLO_SHORTEN (include_file_names,
		   sizeof (curr_block_include_file_names_number));
    }
}

/* The function adds new include file name for the list of include
   files.  The function returns TRUE if the file has been not inserted
   into the list yet.  Otherwise it returns FALSE.  NAME is string in
   the include-clase. */
static int
add_include_file (IR_node_t fname)
{
  const char *name;
  char **names;
  int i;

  d_assert (block_level > 0);
  name = get_full_file_and_encoding_name (fname, NULL);
  names = (char **) ((char *) VLO_END (include_file_names) + 1
		     - sizeof (char *) * curr_block_include_file_names_number);
  for (i = 0; i < curr_block_include_file_names_number; i++)
    if (strcmp (names [i], name) == 0)
      break;
  if (i < curr_block_include_file_names_number)
    return FALSE;
  VLO_ADD_MEMORY (include_file_names, &name, sizeof (name));
  curr_block_include_file_names_number++;
  return TRUE;
}

/* Return true if we can evaluate parsed stmnts in REPL.  */
static int
repl_can_process_p (void)
{
  return (repl_flag && block_level == 1
	  /* To exclude include files */
	  && istream_stack_height () == 0
	  /* To process all environment at once.  Environment[0] is \n
	     in case of finishing environment.  */
	  && (*environment == '\0' || environment[1] == '\0'));
}



void
initiate_parser (void)
{
  current_scope = create_empty_block (NULL);
  start_block ();
  additional_stmts = NULL;
  first_error_p = TRUE;
}

void
initiate_new_parser_REPL_stmts (void)
{
  d_assert (repl_flag);
  first_error_p = first_repl_empty_line_p = TRUE;
}

void
finish_parser (void)
{
  finish_block ();
}

/*
Local Variables:
mode:c
End:
*/
