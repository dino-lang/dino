%{

#include "d_common.h"
#include "d_ir.h"
#include "d_yacc.h"

/* The value of following var is first program stmt
   (see member stmt_list in nodes).  The value may be NULL:
   it means empty program. */
IR_node_t first_program_stmt;

static IR_node_t merge_stmt_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_stmt_list (IR_node_t list);

static IR_node_t merge_access_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_access_list (IR_node_t list);

static IR_node_t merge_exception_lists (IR_node_t list1, IR_node_t list2);
static IR_node_t uncycle_exception_list (IR_node_t list);

/* The following vars are used by yacc analyzer. */

/* Pointer to block node which opens current scope. */
static IR_node_t current_scope;

/* This var is used as second attribute of nonterminal actual_parameters.
   Its value is source pos of corresponding left parenthesis. Its
   value is to be defined only at rule end because of nesting. */
static position_t actual_parameters_construction_pos;

/* This var is used as second attribute of nonterminal formal_parameters.
   Its value is flag of variable number parameters (usage of ...). */
static int formal_parameter_args_flag;

/* These vars are used as attributes of nonterminal `access'. */
static int public_flag, friend_flag;

static const char *full_file_name  (const char *extend_identifier);
static void add_lexema_to_file (int lexema_code);
static void finish_scanner_file (void);

static void start_block (void);
static void finish_block (void);
static int add_include_file (const char *name);

%}

%union
  {
    IR_node_t pointer;
    position_t pos;
    int flag;
    IR_node_mode_t node_mode;
   }

%token <pointer> NUMBER CHARACTER STRING IDENT
%token BREAK CATCH CHAR CLASS CONTINUE ELSE EXT EXTERN
       FINAL FLOAT FOR FRIEND FUNC HIDE HIDEBLOCK IF IN INT
       NEW NIL PUBLIC PRIVATE RETURN TABLE THREAD THROW TRY TYPE
       VAR VECTOR WAIT
%token ARROW
%token MULT_ASSIGN DIV_ASSIGN REM_ASSIGN PLUS_ASSIGN MINUS_ASSIGN CONCAT_ASSIGN
%token LSHIFT_ASSIGN RSHIFT_ASSIGN ASHIFT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN
%token INCR DECR
%token DOTS
%token INCLUDE INCLUSION END_OF_FILE END_OF_INCLUDE_FILE

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
%left '!' '#' '~' FINAL NEW

%type <pos> pos
%type <node_mode> assign
%type <pointer> expr designator
                elist_parts_list elist_parts_list_empty elist_part
                expr_list expr_list_empty
            	actual_parameters access_list var_par_list var_par_list_empty
        	var_par stmt executive_stmt incr_decr for_guard_expr
                block_stmt try_block_stmt catch_list catch except_class_list
                header declaration extern_list extern_item
        	func_thread_class else_part expr_empty formal_parameters
        	block stmt_list program inclusion
%type <flag> clear_flag  set_flag  access  opt_final

%start program

%%

clear_flag : {$$ = 0;}
           ;
set_flag   : {$$ = 1;}
           ;
expr : expr '?' pos expr ':' expr
       	{
          $$ = create_node_with_pos (IR_NM_cond, $3);
          IR_set_cond ($$, $1);
          IR_set_left_operand ($$, $4);
          IR_set_right_operand ($$, $6);
        }
     | expr LOGICAL_OR pos expr
       	{
          $$ = create_node_with_pos (IR_NM_logical_or, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr LOGICAL_AND pos expr
       	{
          $$ = create_node_with_pos (IR_NM_logical_and, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr IN pos expr
       	{
          $$ = create_node_with_pos (IR_NM_in, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '|' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_or, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '^' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_xor, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '&' pos expr 
       	{
          $$ = create_node_with_pos (IR_NM_and, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr EQ pos expr  
       	{
          $$ = create_node_with_pos (IR_NM_eq, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr NE pos expr  
       	{
          $$ = create_node_with_pos (IR_NM_ne, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr IDENTITY pos expr  
       	{
          $$ = create_node_with_pos (IR_NM_identity, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr UNIDENTITY pos expr  
       	 {
           $$ = create_node_with_pos (IR_NM_unidentity, $3);
           IR_set_left_operand ($$, $1);
           IR_set_right_operand ($$, $4);
         }
     | expr '<' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_lt, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '>' pos expr  
       	{
          $$ = create_node_with_pos (IR_NM_gt, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        } 
     | expr LE pos expr
       	{
          $$ = create_node_with_pos (IR_NM_le, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr GE pos expr 
       	{
          $$ = create_node_with_pos (IR_NM_ge, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr LSHIFT pos expr
       	{
          $$ = create_node_with_pos (IR_NM_lshift, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr RSHIFT pos expr
       	{
          $$ = create_node_with_pos (IR_NM_rshift, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr ASHIFT pos expr
       	{
          $$ = create_node_with_pos (IR_NM_ashift, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '@' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_concat, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '+' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_plus, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '-' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_minus, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '*' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_mult, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '/' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_div, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | expr '%' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_mod, $3);
          IR_set_left_operand ($$, $1);
          IR_set_right_operand ($$, $4);
        }
     | '!' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_not, $2);
          IR_set_operand ($$, $3);
        }
     | '+' pos expr %prec '!'
       	{
          $$ = create_node_with_pos (IR_NM_unary_plus, $2);
          IR_set_operand ($$, $3);
        }
     | '-' pos expr %prec '!'
       	{
          $$ = create_node_with_pos (IR_NM_unary_minus, $2);
          IR_set_operand ($$, $3);
        }
     | '~' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_bitwise_not, $2);
          IR_set_operand ($$, $3);
        }
     | '#' pos expr
       	{
          $$ = create_node_with_pos (IR_NM_length, $2);
          IR_set_operand ($$, $3);
        }
     | FINAL pos expr
       	{
          $$ = create_node_with_pos (IR_NM_const, $2);
          IR_set_operand ($$, $3);
        }
     | NEW pos expr
       	{
          $$ = create_node_with_pos (IR_NM_new, $2);
          IR_set_operand ($$, $3);
        }
     | designator    {$$ = $1;}
     | NUMBER        {$$ = $1;}
     | CHARACTER     {$$ = $1;}
     | pos NIL       {$$ = create_node_with_pos (IR_NM_nil, $1);}
     | '(' expr ')'  {$$ = $2;}
     | '(' error bracket_stop    {$$ = NULL;}
     | '[' pos set_flag elist_parts_list_empty ']'
      	{
          $$ = create_node_with_pos (IR_NM_vector, $2);
          IR_set_elist ($$, $4);
        }
     | '[' error sqbracket_stop  {$$ = NULL;}
     | '{' pos clear_flag elist_parts_list_empty '}'
      	{
          $$ = create_node_with_pos (IR_NM_table, $2);
          IR_set_elist ($$, $4);
        }
     | '{' error stmt_stop  {$$ = NULL;}
     | STRING            {$$ = $1;}
     | pos CHAR          {$$ = create_node_with_pos (IR_NM_char_type, $1);}
     | pos INT           {$$ = create_node_with_pos (IR_NM_int_type, $1);}
     | pos FLOAT         {$$ = create_node_with_pos (IR_NM_float_type, $1);}
     | pos HIDE          {$$ = create_node_with_pos (IR_NM_hide_type, $1);}
     | pos HIDEBLOCK     {$$ = create_node_with_pos (IR_NM_hideblock_type,
						     $1);}
     | pos VECTOR        {$$ = create_node_with_pos (IR_NM_vector_type, $1);}
     | pos TABLE         {$$ = create_node_with_pos (IR_NM_table_type, $1);}
     | pos FUNC          {$$ = create_node_with_pos (IR_NM_func_type, $1);}
     | pos THREAD        {$$ = create_node_with_pos (IR_NM_thread_type, $1);}
     | pos CLASS         {$$ = create_node_with_pos (IR_NM_class_type, $1);}
     | pos FUNC '(' ')'  {$$ = create_node_with_pos (IR_NM_stack_type, $1);}
     | pos THREAD '(' ')'{$$ = create_node_with_pos (IR_NM_process_type, $1);}
     | pos CLASS '(' ')' {$$ = create_node_with_pos (IR_NM_instance_type, $1);}
     | pos TYPE          {$$ = create_node_with_pos (IR_NM_type_type, $1);}
     | pos TYPE '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_typeof, $1);
          IR_set_operand ($$, $4);
        }
     | pos CHAR '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_charof, $1);
          IR_set_operand ($$, $4);
        }
     | pos INT '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_intof, $1);
          IR_set_operand ($$, $4);
        }
     | pos FLOAT '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_floatof, $1);
          IR_set_operand ($$, $4);
        }
     | pos VECTOR '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_vectorof, $1);
          IR_set_operand ($$, $4);
        }
     | pos VECTOR '(' expr ',' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_format_vectorof, $1);
          IR_set_operand ($$, $4);
          IR_set_format ($$, $6);
        }
     | pos TABLE '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_tableof, $1);
          IR_set_operand ($$, $4);
        }
     | pos FUNC '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_funcof, $1);
          IR_set_operand ($$, $4);
        }
     | pos THREAD '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_threadof, $1);
          IR_set_operand ($$, $4);
        }
     | pos CLASS '(' expr ')'
       	{
          $$ = create_node_with_pos (IR_NM_classof, $1);
          IR_set_operand ($$, $4);
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
designator : designator '[' pos expr ']'
       	       {
                 $$ = create_node_with_pos (IR_NM_index, $3);
                 IR_set_left_operand ($$, $1);
                 IR_set_right_operand ($$, $4);
               }
           | designator '[' pos error sqbracket_stop
       	       /* The designator have to be vector. */
       	       {
                 $$ = create_node_with_pos (IR_NM_index, $3);
                 IR_set_left_operand ($$, $1);
                 IR_set_right_operand ($$, NULL);
               }
           | designator '{' pos expr '}'
       	       {
                 $$ = create_node_with_pos (IR_NM_key_index, $3);
                 IR_set_left_operand ($$, $1);
                 IR_set_right_operand ($$, $4);
               }
           | designator '{' pos error stmt_stop
       	       /* The designator have to be table. */
       	       {
                 $$ = create_node_with_pos (IR_NM_key_index, $3);
                 IR_set_left_operand ($$, $1);
                 IR_set_right_operand ($$, NULL);
               }
           | designator actual_parameters
               {
		 $$
		   = create_node_with_pos (IR_NM_class_func_thread_call,
					   actual_parameters_construction_pos);
		 IR_set_func_expr ($$, $1);
		 IR_set_elist ($$, $2);
	       }
           | designator '.' pos IDENT
       	       {
                 $$ = create_node_with_pos (IR_NM_period, $3);
                 IR_set_left_operand ($$, $1);
                 IR_set_right_operand ($$, $4);
               }
           | '(' expr ')' ARROW pos IDENT
       	       {
                 $$ = create_node_with_pos (IR_NM_arrow, $5);
                 IR_set_left_operand ($$, $2);
                 IR_set_right_operand ($$, $6);
               }
           | pos '*' '(' expr ')'
       	       {
		 $$ = create_node_with_pos (IR_NM_deref, $1);
		 IR_set_operand ($$, $4);
               }
           | IDENT     {$$ = $1;}
           ;
/* Attribute value is the last element of the cycle list.  The
   nonterminal with attribute of type flag must be before
   elist_parts_list. */
elist_parts_list : elist_part
       	            {
                      $$ = $1;
                      IR_set_elist ($$, $$);
                    }
                 | elist_parts_list ','  {$<flag>$ = $<flag>0;} elist_part
       	            {
	              $$ = $4;
                      IR_set_elist ($$, IR_elist ($1));
                      IR_set_elist ($1, $$);
                    }
       	         ;
/* The nonterminal with attribute of type flag must be before
   elist_part. */
elist_part : pos expr
               {
	         $$ = create_node_with_pos (IR_NM_elist_element, $1);
                 if ($<flag>0)
                   {
                     /* vector */
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
     	         IR_set_repetition_key ($$, $2);
                 IR_set_expr ($$, $4);
               }
           ;
/* The nonterminal with attribute of type flag must be before
   elist_parts_list_empty. */
elist_parts_list_empty :      {$$ = NULL;}
       	               | elist_parts_list
                              {$$ = IR_elist ($1); IR_set_elist ($1, NULL);}
       	               ;
/* This nonterminal has second attributes:
   var actual_parameters_construction_pos (see commentaries for
   the var). */
actual_parameters : '(' pos expr_list_empty ')'
       		       {actual_parameters_construction_pos = $2; $$ = $3;}
       	          | '(' pos error bracket_stop
       		       {actual_parameters_construction_pos = $2; $$ = NULL;}
       	          ;
expr_list_empty :           {$$ = NULL;}
       	        | expr_list {$$ = IR_elist ($1); IR_set_elist ($1, NULL);}
       	        ;
/* Attribute value is the last element of the cycle list. */
expr_list : pos expr
              {
	        $$ = create_node_with_pos (IR_NM_elist_element, $1);
                IR_set_expr ($$, $2);
	        IR_set_repetition_key ($$, NULL);
                IR_set_elist ($$, $$);
              }
          | expr_list ',' pos expr
              {
                $$ = create_node_with_pos (IR_NM_elist_element, $3);
     	        IR_set_repetition_key ($$, NULL);
                IR_set_expr ($$, $4);
                IR_set_elist ($$, IR_elist ($1));
                IR_set_elist ($1, $$);
              }
          ;
/* Attribute value is cyclic list of stmts corresponding to var decls
   with the pointer to the last element.  The attribute before
   var_par_list must be flag of that this is in var decl (not
   parameters). */
var_par_list : var_par   {$$ = $1;}
       	     | var_par_list ',' {$<flag>$ = $<flag>0;} var_par
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
       	     | error {$$ = NULL;}
       	     ;
/* Attribute value is cyclic list of stmts corresponding to var decl
   (and possibly assignment stmt) with the pointer to the last
   element.  The attribute before var_par_list must be flag of that
   this is in var decl (not parameters). */
var_par : opt_final IDENT pos
            {
              $$ = create_node_with_pos (IR_NM_var, $3);
              IR_set_scope ($$, current_scope);
              IR_set_ident ($$, $2);
              IR_set_next_stmt ($$, $$);
	      IR_set_const_flag ($$, $1);
	    }
        | opt_final IDENT pos '=' pos expr
            {
	      IR_node_t assign;

              $$ = create_node_with_pos (IR_NM_var, $3);
              IR_set_scope ($$, current_scope);
              IR_set_ident ($$, $2);
	      IR_set_const_flag ($$, $1);
	      assign = create_node_with_pos (($<flag>0
					      ? IR_NM_var_assign
					      : IR_NM_par_assign),
					     $5);
	      IR_set_assignment_var (assign, $2);
	      IR_set_assignment_expr (assign, $6);
	      IR_set_next_stmt ($$, assign);
	      IR_set_next_stmt (assign, $$);
	      $$ = assign;
	    }
        ;
/* see commentaries for var_par_list. */
var_par_list_empty :              {$$ = NULL;} 
       	           | var_par_list {$$ = $1;}
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
     | error stmt_stop    {$$ = NULL;}
     ;
assign : '='            {$$ = IR_NM_assign;}
       | MULT_ASSIGN    {$$ = IR_NM_mult_assign;}
       | DIV_ASSIGN     {$$ = IR_NM_div_assign;}
       | REM_ASSIGN     {$$ = IR_NM_rem_assign;}
       | PLUS_ASSIGN    {$$ = IR_NM_plus_assign;}
       | MINUS_ASSIGN   {$$ = IR_NM_minus_assign;}
       | CONCAT_ASSIGN  {$$ = IR_NM_concat_assign;}
       | LSHIFT_ASSIGN  {$$ = IR_NM_lshift_assign;}
       | RSHIFT_ASSIGN  {$$ = IR_NM_rshift_assign;}
       | ASHIFT_ASSIGN  {$$ = IR_NM_ashift_assign;}
       | AND_ASSIGN     {$$ = IR_NM_and_assign;}
       | XOR_ASSIGN     {$$ = IR_NM_xor_assign;}
       | OR_ASSIGN      {$$ = IR_NM_or_assign;}
       ;
incr_decr : INCR {$$ = get_int_node (1, source_position);}
          | DECR {$$ = get_int_node (-1, source_position);}
          ;
executive_stmt :
      {$<flag>$ = $<flag>0;} end_simple_stmt      {$$ = NULL;}
    | designator assign pos expr {$<flag>$ = $<flag>0;} end_simple_stmt
       	{
          $$ = create_node_with_pos ($2, $3);
          IR_set_assignment_var ($$, $1); 
          IR_set_assignment_expr ($$, $4);
        }
    | designator incr_decr pos {$<flag>$ = $<flag>0;} end_simple_stmt
       	{
          $$ = create_node_with_pos (IR_NM_plus_assign, $3);
          IR_set_assignment_var ($$, $1); 
          IR_set_assignment_expr ($$, $2);
        }
    | incr_decr pos designator {$<flag>$ = $<flag>0;} end_simple_stmt
       	{
          $$ = create_node_with_pos (IR_NM_plus_assign, $2);
          IR_set_assignment_var ($$, $3); 
          IR_set_assignment_expr ($$, $1);
        }
    | designator actual_parameters {$<flag>$ = $<flag>0;} end_simple_stmt
       	{
          $$ = create_node_with_pos (IR_NM_procedure_call,
                                      actual_parameters_construction_pos);
       	 IR_set_proc_expr ($$, $1);
       	 IR_set_elist ($$, $2);
        }
    | IF pos '(' expr ')' {$<flag>$ = $<flag>0;} stmt else_part
       	{
          $$ = create_node_with_pos (IR_NM_if_stmt, $2);
          IR_set_if_expr ($$, $4);
          IR_set_if_part ($$, uncycle_stmt_list ($7));
          IR_set_else_part ($$, $8);
        }
    | IF pos '(' error bracket_stop {$<flag>$ = $<flag>0;} stmt else_part
       	{
          $$ = create_node_with_pos (IR_NM_if_stmt, $2);
          IR_set_if_expr ($$, NULL);
          IR_set_if_part ($$, uncycle_stmt_list ($7));
          IR_set_else_part ($$, $8);
        }
    | FOR pos '(' clear_flag stmt for_guard_expr ';' set_flag stmt ')'
        {$<flag>$ = $<flag>0;} stmt
       	{
          $$ = create_node_with_pos (IR_NM_for_stmt, $2);
          IR_set_for_initial_stmt ($$, uncycle_stmt_list ($5));
          IR_set_for_guard_expr ($$, $6);
          IR_set_for_iterate_stmt ($$, uncycle_stmt_list ($9));
          IR_set_for_stmts ($$, uncycle_stmt_list ($12));
        }
    | FOR pos '(' clear_flag designator IN expr ')'
        {$<flag>$ = $<flag>0;} stmt
       	{
          $$ = create_node_with_pos (IR_NM_foreach_stmt, $2);
          IR_set_foreach_designator ($$, $5);
          IR_set_foreach_table ($$, $7);
          IR_set_foreach_stmts ($$, uncycle_stmt_list ($10));
        }
    | FOR pos '(' error bracket_stop {$<flag>$ = $<flag>0;} stmt
       	{
          $$ = create_node_with_pos (IR_NM_for_stmt, $2);
          IR_set_for_initial_stmt ($$, NULL);
          IR_set_for_guard_expr ($$, NULL);
          IR_set_for_iterate_stmt ($$, NULL);
          IR_set_foreach_stmts ($$, uncycle_stmt_list ($7));
        }
    | BREAK pos {$<flag>$ = $<flag>0;} end_simple_stmt
        {$$ = create_node_with_pos (IR_NM_break_stmt, $2);}
    | CONTINUE pos {$<flag>$ = $<flag>0;} end_simple_stmt
 	{$$ = create_node_with_pos (IR_NM_continue_stmt, $2);}
    | RETURN pos expr_empty {$<flag>$ = $<flag>0;} end_simple_stmt
       	{
          if ($3 == NULL)
            $$ = create_node_with_pos (IR_NM_return_without_result, $2);
          else
            {
              $$ = create_node_with_pos (IR_NM_return_with_result, $2);
              IR_set_returned_expr ($$, $3);
            }
       	}
    | THROW pos expr {$<flag>$ = $<flag>0;} end_simple_stmt
       	{
          $$ = create_node_with_pos (IR_NM_throw, $2);
          IR_set_throw_expr ($$, $3); 
        }
    | WAIT pos expr {$<flag>$ = $<flag>0;} end_simple_stmt
       	{
          $$ = create_node_with_pos (IR_NM_wait_stmt, $2);
          IR_set_wait_guard_expr ($$, $3);
        }
    | block_stmt     {$$ = $1;}
    | try_block_stmt {$$ = $1;}
    ;
for_guard_expr :      {$$ = get_int_node (1, source_position);}
               | expr {$$ = $1;}
               ;
block_stmt :    {
                  $<pointer>$ = create_node_with_pos (IR_NM_block,
	                                              no_position);
                  IR_set_func_class_ext ($<pointer>$, NULL);
                  IR_set_access_list ($<pointer>$, NULL);
                  IR_set_scope ($<pointer>$, current_scope);
                  current_scope = $<pointer>$;
		  IR_set_exceptions ($<pointer>$, NULL);
                }
       	     block
       		{
                  $$ = $<pointer>1;
       		  IR_set_block_stmts ($$, uncycle_stmt_list ($2));
       		  IR_set_access_list
		    ($$, uncycle_access_list (IR_access_list ($$)));
       		  current_scope = IR_scope ($$);
                }
           ;
try_block_stmt :    {
	              $<pointer>$ = create_node_with_pos (IR_NM_block,
							  no_position);
		      IR_set_func_class_ext ($<pointer>$, NULL);
		      IR_set_access_list ($<pointer>$, NULL);
		      IR_set_scope ($<pointer>$, current_scope);
		      current_scope = $<pointer>$;
		      IR_set_exceptions ($<pointer>$, NULL);
                    }
                 TRY block
       		    {
		      $<pointer>$ = $<pointer>1;
		      IR_set_block_stmts ($<pointer>$, uncycle_stmt_list ($3));
		      IR_set_access_list
			($<pointer>$, uncycle_access_list (IR_access_list
							   ($<pointer>$)));
		      current_scope = IR_scope ($<pointer>$);
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
            $<pointer>$ = create_node_with_pos (IR_NM_block,
	                                        no_position);
            IR_set_func_class_ext ($<pointer>$, NULL);
            IR_set_access_list ($<pointer>$, NULL);
            IR_set_scope ($<pointer>$, current_scope);
            current_scope = $<pointer>$;
	    IR_set_exceptions ($<pointer>$, NULL);
	    /* Add variable for catched exception. */
	    $<pointer>$ = create_node_with_pos (IR_NM_var, current_position);
	    IR_set_next_stmt ($<pointer>$, $<pointer>$);
	    IR_set_scope ($<pointer>$, current_scope);
	    IR_set_ident ($<pointer>$, get_ident_node (CATCH_EXCEPTION_NAME,
						       current_position));
          }
       	block
       	  {
	    IR_set_block_stmts (current_scope,
				uncycle_stmt_list (merge_stmt_lists
						   ($<pointer>5, $6)));
	    IR_set_access_list
	      (current_scope,
	       uncycle_access_list (IR_access_list (current_scope)));
	    $$ = $3;
	    IR_set_block (IR_next_exception ($3), current_scope);
	    current_scope = IR_scope (current_scope);
	  }
      | error {$$ = NULL;}
      ;
/* Attribute value is cyclic list of exceptions with the pointer to
   the last one. */
except_class_list : expr
                      {
			$$ = create_node_with_pos (IR_NM_exception,
						   current_position);
			IR_set_exception_class_expr ($$, $1);
			IR_set_block ($$, NULL);
			IR_set_next_exception ($$, $$);
		      }
                  | except_class_list ',' expr
                      {
			$$ = create_node_with_pos (IR_NM_exception,
						   current_position);
			IR_set_exception_class_expr ($$, $3);
			IR_set_block ($$, NULL);
			IR_set_next_exception ($$, IR_next_exception ($1));
			IR_set_next_exception ($1, $$);
		      }
                  ;
/* Attribute value is cyclic list of ident in clause with the pointer
   to the last one.  The rules also uses attributes in variables
   public_flag, friend_flag (it is possible because there are no
   nested access list declarations. */
access_list : IDENT
               {
                 $$ = create_node (IR_NM_access_ident);
                 IR_set_ident ($$, $1);
		 IR_set_public_flag ($$, public_flag);
		 IR_set_friend_flag ($$, friend_flag);
                 IR_set_next_access_ident ($$, $$);
               }
     	    | access_list ',' IDENT
     	       {
                 $$ = create_node (IR_NM_access_ident);
		 IR_set_public_flag ($$, public_flag);
		 IR_set_friend_flag ($$, friend_flag);
                 if ($1 != NULL)
                   {
                     IR_set_next_access_ident ($$, IR_next_access_ident ($1));
                     IR_set_next_access_ident ($1, $$);
                   }
                 else
                   IR_set_next_access_ident ($$, $$);
                 IR_set_ident ($$, $3);
               }
     	    | error {$$ = NULL;}
     	    ;
/* Attribute value is cyclic list of stmts corresponding to decls with
   the pointer to last element.  Class (function) declaration is
   represented by two stmt nodes: class (function) node and block
   node. */
declaration : VAR set_flag var_par_list {$<flag>$ = $<flag>0;} end_simple_stmt
                {$$ = $3;}
            | access access_list {$<flag>$ = $<flag>0;} end_simple_stmt
                {
		  IR_set_access_list (current_scope,
				      merge_access_lists
				      (IR_access_list (current_scope), $2));
		  $$ = NULL;
		}
            | EXTERN set_flag extern_list {$<flag>$ = $<flag>0;}
                end_simple_stmt
                {$$ = $3;}
            | header block
                {
                  $$ = current_scope; /*i.e. block.*/
                  IR_set_block_stmts
                    ($$, uncycle_stmt_list (merge_stmt_lists ($1, $2)));
       		  IR_set_access_list
		    ($$, uncycle_access_list (IR_access_list ($$)));
                  current_scope = IR_scope ($$);
                } 
            | INCLUDE STRING {$<flag>$ = $<flag>0;} end_simple_stmt
                {
		  $<pointer>$ = $2;
                  if (add_include_file (IR_string_value
					(IR_unique_string ($2))))
                    add_lexema_to_file (INCLUSION);
                }
              inclusion
                {
                  $$ = $6;
                }
            | INCLUDE '+' STRING {$<flag>$ = $<flag>0;} end_simple_stmt
                {
		  $<pointer>$ = $3;
                  add_include_file (IR_string_value (IR_unique_string ($3)));
		  add_lexema_to_file (INCLUSION);
                }
              inclusion
                {
                  $$ = $7;
                }
            ;
/* Access has two attributes in variables `public_flag' and `friend_flag'. */
access : PUBLIC  {public_flag = TRUE; friend_flag = FALSE;}
       | PRIVATE {public_flag = FALSE; friend_flag = FALSE;}
       | FRIEND  {public_flag = FALSE; friend_flag = TRUE;}
       ;
extern_list : extern_item {$$ = $1;}
            | extern_list ',' {$<flag>$ = $<flag>0;} extern_item
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
extern_item : IDENT pos
                {
		  $$ = create_node_with_pos (IR_NM_external_var, $2);
		  IR_set_scope ($$, current_scope);
		  IR_set_ident ($$, $1);
		  IR_set_next_stmt ($$, $$);
                }
            | IDENT pos '(' ')'
                {
		  $$ = create_node_with_pos (IR_NM_external_func, $2);
		  IR_set_scope ($$, current_scope);
		  IR_set_ident ($$, $1);
		  IR_set_next_stmt ($$, $$);
                }
            ;
inclusion :   {$$ = NULL;}
          |   {
	        yyerrok;
              }
            INCLUSION
              {
                start_scanner_file (full_file_name (IR_string_value
						    (IR_unique_string
						     ($<pointer>0))),
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
                       yyerror ("syntax error");
                    }
                ;
header : func_thread_class IDENT
       	   {
             IR_node_t block;
             
             IR_set_scope ($1, current_scope);
             IR_set_ident ($1, $2);
             block = create_node_with_pos (IR_NM_block, no_position);
             IR_set_next_stmt (block, $1);
             IR_set_next_stmt ($1, block);
             IR_set_func_class_ext (block, $1);
             IR_set_scope (block, current_scope);
	     IR_set_access_list (block, NULL);
	     IR_set_exceptions (block, NULL);
             /* This assignment is here for that formal parameters are
                to be in corresponding block. */
             current_scope = block;
           }
         formal_parameters
           {
             int var_par_list_length = 0;
             IR_node_t current_decl = $4;
             
             if (current_decl != NULL)
               do
                 {
                   var_par_list_length++;
                   current_decl = IR_next_stmt (current_decl);
                 }
               while (current_decl != $4);
             IR_set_parameters_number ($1, var_par_list_length);
             IR_set_args_flag ($1, formal_parameter_args_flag);
	     $$  = $4;
           }
       | EXT pos IDENT
       	   {
             IR_node_t block;
             
	     $$ = create_node_with_pos (IR_NM_ext, $2);
             IR_set_scope ($$, current_scope);
             IR_set_ident ($$, $3);
             block = create_node_with_pos (IR_NM_block, no_position);
             IR_set_next_stmt (block, $$);
             IR_set_next_stmt ($$, block);
             IR_set_func_class_ext (block, $$);
	     IR_set_access_list (block, NULL);
             IR_set_scope (block, current_scope);
	     IR_set_exceptions (block, NULL);
             current_scope = block;
	     $$ = NULL; /* Formal parameters list. */
           }
       ;
func_thread_class : opt_final FUNC pos
                      {
			$$ = create_node_with_pos (IR_NM_func, $3);
			IR_set_thread_flag ($$, FALSE);
			IR_set_final_flag ($$, $1);
		      }
       	          | opt_final THREAD pos
                      {
                        $$ = create_node_with_pos (IR_NM_func, $3);
			IR_set_thread_flag ($$, TRUE);
			IR_set_final_flag ($$, $1);
		      }
       	          | opt_final CLASS pos
                      {
			$$ = create_node_with_pos (IR_NM_class, $3);
			IR_set_final_flag ($$, $1);
		      }
      	          ;
opt_final :          {$$ = FALSE;}
          |  FINAL   {$$ = TRUE;}
          ;
else_part :                                    {$$ = NULL;}
          | ELSE {$<flag>$ = $<flag>-1;} stmt  {$$ = uncycle_stmt_list ($3);}
          ;
expr_empty :             {$$ = NULL;}
       	   | expr        {$$ = $1;}
       	   ;
/* see commentaries for var_par_list. */
formal_parameters : '(' clear_flag var_par_list_empty ')'
                       {
                         formal_parameter_args_flag = FALSE;
			 $$ = $3;
		       }
       	          | '(' clear_flag var_par_list ',' DOTS pos ')'
                       {
			 formal_parameter_args_flag = TRUE;
			 $$ = create_node_with_pos (IR_NM_var, $6);
			 if ($3 != NULL)
			   {
			     IR_set_next_stmt ($$, IR_next_stmt ($3));
			     IR_set_next_stmt ($3, $$);
			   }
			 else
			   IR_set_next_stmt ($$, $$);
			 IR_set_scope ($$, current_scope);
			 IR_set_ident ($$, get_ident_node (ARGS_NAME, $6));
		       }
       	          | '(' DOTS pos ')'
                       {
			 formal_parameter_args_flag = TRUE;
			 $$ = create_node_with_pos (IR_NM_var, $3);
			 IR_set_scope ($$, current_scope);
			 IR_set_ident ($$,  get_ident_node (ARGS_NAME, $3));
			 IR_set_next_stmt ($$, $$);
		       }
       	          ;
/* See comments for stmt_list. */
block : '{' pos
             {
	       start_block ();
	     }
         stmt_list '}'
             {
               IR_set_pos (current_scope, $2);
               $$ = $4;
	       finish_block ();
             }
      ;
/* Attribute value is the last element of the cycle list. */
stmt_list :                          	      {$$ = NULL;}
          | stmt_list clear_flag stmt         {$$ = merge_stmt_lists ($1, $3);}
       	  | stmt_list error                   {$$ = $1;}
       	  ;
program : pos
            {
              current_scope = $<pointer>$
                = create_node_with_pos (IR_NM_block, $1);
              IR_set_next_stmt ($<pointer>$, NULL);
              IR_set_func_class_ext ($<pointer>$, NULL);
	      IR_set_access_list ($<pointer>$, NULL);
	      IR_set_exceptions ($<pointer>$, NULL);
	      start_block ();
            }
          stmt_list
            {
              first_program_stmt = $<pointer>2;
              IR_set_block_stmts (first_program_stmt,
                                  uncycle_stmt_list ($3));
	      IR_set_access_list
		(first_program_stmt,
		 uncycle_access_list (IR_access_list (first_program_stmt)));
	      finish_block ();
            }
          END_OF_FILE
            {
	      YYACCEPT;
	    }
        ;
%%

/* Merging two cyclic lists into one cyclic list. */
static IR_node_t
merge_stmt_lists (IR_node_t list1, IR_node_t list2)
{
  IR_node_t result;

  if (list2 != NULL) {
    if (list1 != NULL)
      {
        IR_node_t first;
        
        first = IR_next_stmt (list2);
        IR_set_next_stmt (list2, IR_next_stmt (list1));
        IR_set_next_stmt (list1, first);
      }
    result = list2;
  }
  else
    result = list1;
  return result;
}

/* Make normal list from cycle stmt list with the pointer to the last
   stmt. */
static IR_node_t
uncycle_stmt_list (IR_node_t list)
{
  IR_node_t first;

  if (list == NULL)
    return list;
  first = IR_next_stmt (list);
  IR_set_next_stmt (list, NULL);
  return first;
}



/* Merging two cyclic lists into one cyclic list. */
static IR_node_t
merge_access_lists (IR_node_t list1, IR_node_t list2)
{
  IR_node_t result;

  if (list2 != NULL) {
    if (list1 != NULL)
      {
        IR_node_t first;
        
        first = IR_next_access_ident (list2);
        IR_set_next_access_ident (list2, IR_next_access_ident (list1));
        IR_set_next_access_ident (list1, first);
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
uncycle_access_list (IR_node_t list)
{
  IR_node_t first;

  if (list == NULL)
    return list;
  first = IR_next_access_ident (list);
  IR_set_next_access_ident (list, NULL);
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
     stream is undefined or command line. */
  FILE *file;
  /* File name of given input stream. Null if the current input stream
     is undefined.  Empty string if the current input stream is
     command line. */
  const char *file_name;
  /* Current position in file corresponding to given input stream.
     The following member is defined only for structures in the input
     stream stack and if it is not command line stream. */
  long file_pos;
  /* The following member contains parameter value of the function
     `add_lexema_to_file' after immediately call of this
     function or -1 otherwise. */
  int uninput_lexema_code;
};

/* The following structure contains all current input stream and
   scanner state.  If the member `file_name' value is NULL then input
   stream is not defined. */
static struct istream_state curr_istream_state;

/* All input stream stack is implemented by variable length object.
   See package `vl-object'. */
static vlo_t istream_stack;

/* The following value is used if previous_char does not contain an
   input char */
#define EMPTY_PREVIOUS_CHAR (-2000)

/* The following variable is used for storing '\r'. */
static int previous_char;

/* The following function creates empty input stream stack and
   initiates current input stream and scanner state as undefined. */
static void
initiate_istream_stack (void)
{
  VLO_CREATE (istream_stack, 0);
  curr_istream_state.file_name = NULL;
  curr_istream_state.file = NULL;
  curr_istream_state.uninput_lexema_code = (-1);
}

/* The following function deletes the input stream stack and closes
   current input file if current input stream state is defined.  */
static void
finish_istream_stack (void)
{
  VLO_DELETE (istream_stack);
  if (curr_istream_state.file != NULL)
    fclose (curr_istream_state.file);
}

/* The following function returns height of input stream stack,
   i.e. current file inclusion level. */
int
istream_stack_height (void)
{
  return VLO_LENGTH (istream_stack) / sizeof (struct istream_state);
}

/* The following function saves current input stream and scanner state
   (if it is defined) in the stack, closes file corresponding to
   current input stream, opens file corresponding to new input stream,
   and sets up new current input stream and scanner state.  The
   function also checks up absence of loop of file inclusions.  The
   function reports errors if the loop exists or new file is not
   opened. */
static void
push_curr_istream (const char *new_file_name, position_t error_pos)
{
  int i;

  if (curr_istream_state.file_name != NULL)
    {
      /* The current stream is defined. */
      if (*curr_istream_state.file_name != '\0')
	{
	  /* The current stream is not command line. */
	  curr_istream_state.file_pos = ftell (curr_istream_state.file);
	  fclose (curr_istream_state.file);
	  curr_istream_state.file = NULL;
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
  if (*new_file_name != '\0')
    {
      /* The current stream is not commad line. */
      curr_istream_state.file = fopen (new_file_name, "rb");
      if (curr_istream_state.file == NULL)
	system_error (TRUE, error_pos, "fatal error -- `%s': ", 
		      curr_istream_state.file_name);
    }
  previous_char = EMPTY_PREVIOUS_CHAR;
  start_file_position (curr_istream_state.file_name);
  curr_istream_state.uninput_lexema_code = (-1);
}

/* The following function closes file corresponding to current input
   stream (it must be defined), reopens file corresponding to previous
   input stream (if it is defined) and restores previous input stream
   and scanner state.  The function can fix error if the file is not
   reopened. */
static void
pop_istream_stack (void)
{
  assert (curr_istream_state.file_name != NULL);
  if (curr_istream_state.file != NULL)
    {
      fclose (curr_istream_state.file);
      curr_istream_state.file = NULL;
    }
  if (istream_stack_height () != 0)
    {
      curr_istream_state
	= (((struct istream_state *) VLO_BEGIN (istream_stack))
	   [istream_stack_height () - 1]);
      VLO_SHORTEN (istream_stack, sizeof (struct istream_state));
      if (*curr_istream_state.file_name != '\0')
	{
	  /* It is not command line stream. */
	  curr_istream_state.file = fopen (curr_istream_state.file_name, "rb");
	  if (curr_istream_state.file == NULL
	      || fseek (curr_istream_state.file,
			curr_istream_state.file_pos, 0) != 0)
	    system_error (TRUE, no_position,
			  "fatal error -- repeated opening file `%s': ", 
			  curr_istream_state.file_name);
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

  assert (file_name != NULL);
  for (curr_char_ptr = file_name, last_slash = NULL;
       *curr_char_ptr != '\0';
       curr_char_ptr++)
#ifdef WIN32
    if (*curr_char_ptr == '/' || *curr_char_ptr == '\\')
#else
    if (*curr_char_ptr == '/')
#endif
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

#ifndef WIN32
#include <pwd.h>
#else
#include <io.h>
#define getcwd(p, s)			_getcwd (p, s)
#endif

static const char *
canonical_path_name (const char *name)
{
  char buf [FILENAME_MAX + 1];
  char *p, *str, *result;
  int sep, sep1;
#ifdef WIN32
  int drive = 0;

  sep = '\\'; sep1 = '/';
  if (isalpha (*name) && name [1] == ':')
    {
      drive = *name;
      name += 2;
    }
  else
    {
      getcwd (buf, FILENAME_MAX);
      if (isalpha (*buf) && buf [1] == ':')
        drive = *buf;
    }
#else
  sep = sep1 = '/';
#endif
  if (*name != sep && *name != sep1)
    {
      getcwd (buf, FILENAME_MAX);
#ifdef WIN32
      if (isalpha (*buf) && buf [1] == ':')
        {
          drive = *buf;
          IR_TOP_ADD_STRING (buf + 2);
        }
      else
        IR_TOP_ADD_STRING (buf);
#else
      IR_TOP_ADD_STRING (buf);
#endif
      IR_TOP_SHORTEN (1);
      IR_TOP_ADD_BYTE (sep);
      IR_TOP_ADD_BYTE ('\0');
    }
  IR_TOP_ADD_STRING (name);
#ifdef WIN32
  /* It is place for possible inserting `drive:'. */
  IR_TOP_ADD_BYTE ('\0');
  IR_TOP_ADD_BYTE ('\0');
#endif
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
#ifdef WIN32
  if (drive != 0)
    {
      memmove (result+2, result, strlen (result) + 1);
      *result = drive;
      result [1] = ':';
    }
#endif
  return (const char *) result;
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
   specification file name mentioned in 1. */
static const char *
full_file_name (const char *fname)
{
  const char *curr_directory_name;
  const char *real_file_name;
  const char *file_name;
  const char **path_directory_ptr;
  FILE *curr_file;

  curr_directory_name
    = (*curr_istream_state.file_name == '\0'
       ? "" : file_dir_name (curr_istream_state.file_name));
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
  if (curr_file != NULL && fclose (curr_file) == EOF)
    system_error (TRUE, no_position, "fatal error -- `%s': ", real_file_name);
  return canonical_path_name (real_file_name);
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

/* The variable is used for implementation of getc when reading from
   the command line string. */
static int curr_char_number;

/* Getc for dino.  Replacing "\r\n" onto "\n". */
static int
d_getc (void)
{
  int result;

  if (*environment != '\0')
    {
      result = *environment++;
      if (*environment == 0)
	/* Restore the position. */
	current_position = after_environment_position;
    }
  else if (curr_istream_state.file != NULL)
    {
      if (previous_char != '\r')
	result = getc (curr_istream_state.file);
      else
	{
	  result = previous_char;
	  previous_char = EMPTY_PREVIOUS_CHAR;
	}
      if (result == '\r')
	{
	  result = getc (curr_istream_state.file);
	  if (result != '\n')
	    {
	      ungetc (result, curr_istream_state.file);
	      result = '\r';
	    }
	}
    }
  else
    {
      assert (command_line_program != NULL);
      result = command_line_program [curr_char_number];
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
       if (ch != '\r')
	 ungetc (ch, curr_istream_state.file);
       else
	 previous_char = ch;
     }
   else
     {
       assert (command_line_program != NULL);
       if (curr_char_number != 0 && ch != EOF)
	 curr_char_number--;
     }
}

/* Var length string used by func yylval for text presentation of the
   symbol. */
static vlo_t symbol_text;

/* The func reads one code (may be composited from some characters)
   using C language conventions.  It is supposed that the current
   character is not end marker (string or character constant).  The
   func returns the code value or negative number if error is fixed.
   After the call the current char is first char after the code or the
   same as before call if error was fixed.  Position is position of
   the char will be read next.  Parameter INPUT_CHAR is current input
   char (the next chars may be read by d_getc ()).  If the code is
   symbol string breaking TRUE is passed through parameter
   correct_newln.  This case is error and the error must be processed
   after the call if character constant is processed. */
static int get_string_code (int input_char, int *correct_newln)
{
  int character_code;

  /* `current_position' corresponds position right after `input_char'
     here. */
  if (input_char == EOF || input_char == '\n')
    {
      current_position.column_number--;
      d_ungetc (input_char);
      return (-1);
    }
  *correct_newln = FALSE;
  if (input_char == '\\')
    {
      input_char = d_getc ();
      current_position.column_number++;
      if (input_char == 'n')
        input_char = '\n';
      else if (input_char == 't')
        input_char = '\t';
      else if (input_char == 'v')
	input_char = '\v';
      else if (input_char == 'a')
        input_char = '\a';
      else if (input_char == 'b')
        input_char = '\b';
      else if (input_char == 'r')
        input_char = '\r';
      else if (input_char == 'f')
        input_char = '\f';
      else if (input_char == '\\' || input_char == '\'' || input_char == '\"')
        ;
      else if (input_char == '\n')
        {
	  current_position.column_number = 1;
	  current_position.line_number++;
          *correct_newln = TRUE;
        }
      else if (isdigit (input_char) && input_char != '8' && input_char != '9')
	{
	  character_code = VALUE_OF_DIGIT (input_char);
	  input_char = d_getc ();
	  if (!isdigit (input_char) || input_char == '8' || input_char == '9')
	    d_ungetc (input_char);
	  else
	    {
	      current_position.column_number++;
	      character_code
		= (character_code * 8 + VALUE_OF_DIGIT (input_char));
	      input_char = d_getc ();
	      if (!isdigit (input_char)
		  || input_char == '8' || input_char == '9')
		d_ungetc (input_char);
	      else
		{
		  character_code
		    = (character_code * 8 + VALUE_OF_DIGIT (input_char));
		  current_position.column_number++;
		}

	    }
	  input_char = character_code;
      }
    }
  return input_char;
}

/* Determine positions for the tabs. */
#define TAB_STOP 8

/* The following func recognizes next source symbol from the input
   file, returns its code, modifies var current_position so that its
   value is equal to position of the current character in the input
   file and sets up var source_position so that its value is equal to
   position of the returned symbol start, creates corresponding code
   node (if it is needed) and sets up yylval to the node address.  The
   func skips all white spaces and commentaries and fixes all lexical
   errors. */
int yylex (void)
{
  int input_char;
  int number_of_successive_error_characters;
  
  if (curr_istream_state.uninput_lexema_code >= 0)
    {
      int result;
      
      source_position = current_position;
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
          break;
        case '\r':
          current_position.column_number++;
          break;
        case '~':
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
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
          else if (input_char == '>')
	    {
	      current_position.column_number++;
	      return ARROW;
	    }
          else
            {
              d_ungetc (input_char);
              return '-';
            }
        case '=':
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
          current_position.column_number++;
          input_char = d_getc ();
          if (input_char == '=')
	    {
	      current_position.column_number++;
	      return REM_ASSIGN;
	    }
          else
            {
              d_ungetc (input_char);
              return '%';
            }
        case '^':
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
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
	  source_position = current_position;
          current_position.column_number++;
          return input_char;
        case EOF:
	  source_position = current_position;
          return (istream_stack_height () == 0
		  ? END_OF_FILE : END_OF_INCLUDE_FILE);
        case '\'':
          {
            IR_node_t unique_char_node_ptr;
            int correct_newln, character_code;
            
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
                input_char = get_string_code (input_char, &correct_newln);
                if (input_char < 0 || correct_newln)
		  {
		    current_position.column_number--;
		    error (FALSE, current_position, ERR_invalid_char_constant);
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
          {
            int correct_newln;
            IR_node_t unique_string_node_ptr;
            char *string_value_in_code_memory;
            
	    source_position = current_position;
	    current_position.column_number++;
            for (;;)
              {
                input_char = d_getc ();
		current_position.column_number++;
                if (input_char == '\"')
                  break;
                input_char = get_string_code (input_char, &correct_newln);
                if (input_char < 0)
                  {
                    error (FALSE, current_position, ERR_string_end_absence);
                    break;
                  }
                if (!correct_newln)
                  VLO_ADD_BYTE (symbol_text, input_char);
              }
            VLO_ADD_BYTE (symbol_text, '\0');
            IR_set_string_value (temp_unique_string, VLO_BEGIN (symbol_text));
            unique_string_node_ptr = *find_table_entry (temp_unique_string,
							FALSE);
            if (unique_string_node_ptr == NULL)
              {
                unique_string_node_ptr
                  = create_unique_node_with_string
                    (IR_NM_unique_string, VLO_BEGIN (symbol_text),
                     &string_value_in_code_memory);
                IR_set_string_value (unique_string_node_ptr,
                                     string_value_in_code_memory);
                include_to_table (unique_string_node_ptr);
              }
            yylval.pointer = create_node_with_pos (IR_NM_string,
						   source_position);
            IR_set_unique_string (yylval.pointer, unique_string_node_ptr);
            return STRING;
          }
        default:
          if (isalpha (input_char) || input_char == '_' )
            {
              int keyword;
              
	      source_position = current_position;
              /* Ident recognition. */
              do
                {
		  current_position.column_number++;
                  VLO_ADD_BYTE (symbol_text, input_char);
                  input_char = d_getc ();
                }
              while (isalpha (input_char) || isdigit (input_char)
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
          else if (isdigit (input_char))
            {
              /* Recognition numbers. */
	      int float_flag = FALSE;

	      source_position = current_position;
              do
                {
                  /* `current_position' corresponds to `input_char' here. */
                  current_position.column_number++;
                  VLO_ADD_BYTE (symbol_text, input_char);
                  input_char = d_getc ();
                }
              while (isdigit (input_char));
              if (input_char == '.')
                {
                  float_flag = TRUE;
                  do
                    {
		      /* `current_position' corresponds to
                         `input_char' here. */
		      current_position.column_number++;
	              VLO_ADD_BYTE (symbol_text, input_char);
                      input_char = d_getc ();
                    }
	          while (isdigit (input_char));
                }
              if (input_char == 'e' || input_char == 'E')
                {
		  float_flag = TRUE;
		  current_position.column_number++;
                  input_char = d_getc ();
		  if (input_char != '+' && input_char != '-'
		      && !isdigit (input_char))
                    error (FALSE, current_position, ERR_exponent_absence);
		  else
                    {
		      VLO_ADD_BYTE (symbol_text, 'e');
		      do
			{
			  /* `current_position' corresponds to
                             `input_char' here. */
			  current_position.column_number++;
			  VLO_ADD_BYTE (symbol_text, input_char);
			  input_char = d_getc ();
			}
		      while (isdigit (input_char));
                    }
                }
	      VLO_ADD_BYTE (symbol_text, '\0');
              d_ungetc (input_char);
	      if (float_flag)
		yylval.pointer
		  = get_float_node (a2f (VLO_BEGIN (symbol_text)),
				    source_position);
              else
		yylval.pointer
		  = get_int_node (a2i (VLO_BEGIN (symbol_text)),
				  source_position);
	      if (errno)
                error (FALSE, current_position,
		       (float_flag ? ERR_float_value : ERR_int_value));
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

/* The following function initiates internal state of the scanner.
   The function must be called only once before any work with the
   scanner. */
void
initiate_scanner (void)
{
  initiate_istream_stack ();
  curr_char_number = 0;
  environment = ENVIRONMENT;
  VLO_CREATE (symbol_text, 500);
}

/* The following function is called to tune the scanner on input
   stream from given file.  The function is needed for nested
   procession of included files.  If the FILE_NAME is empty string
   then it is command line stream.  If it is the first scanner file,
   special code for correct diganostic during and after parsing
   environment is executed. */
void
start_scanner_file (const char *new_file_name, position_t error_pos)
{
  push_curr_istream (new_file_name, error_pos);
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
  assert (lexema_code >= 0);
  curr_istream_state.uninput_lexema_code = lexema_code;
}

/* The function frees all memory allocated during the scanner work. */
void
finish_scanner (void)
{
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
  assert (block_level > 0);
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
add_include_file (const char *name)
{
  char **names;
  int i;

  assert (block_level > 0);
  name = full_file_name (name);
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

/*
Local Variables:
mode:c
End:
*/
