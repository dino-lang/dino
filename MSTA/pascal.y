%{
#include <stdio.h>
#include "types.h"

extern	int	yylineno;
extern	char	yytext[];
extern	char	numbertext[80];
extern	char	*laststring;
extern	char	lastident[];

extern  char	*create_string();

#define YYSTYPE item


int	lexical_level = 0;

static	int	param_level = 0;
static	char *its_a = "error - uninitialised";

%}

%start program

%token UNSIGNED_INT UNSIGNED_REAL STRING IDENTIFIER
%token NE LE GE BECOMES DIV MOD NIL IN OR AND NOT DOTDOT
%token IF THEN ELSE CASE OF REPEAT UNTIL WHILE DO FOR TO DOWNTO
%token SBEGIN END WITH GOTO CONST VAR TYPE ARRAY RECORD SET SFILE FUNCTION
%token PROCEDURE LABEL PACKED PROGRAM

%%   /*start of rules*/
program		: PROGRAM {its_a="program";} newident external_files ';' 
			block '.'
		;
external_files  : /*empty*/
		| '(' {its_a="external-file";} newident_list ')'
		;

block		: opt_declarations  statement_part
		;
opt_declarations: /*empty*/
		| declarations
		;
declarations	: declarations declaration	/*should be left-recursive*/
		| declaration
		;
declaration	: label_dcl_part
		| const_dcl_part
		| type_dcl_part
		| var_dcl_part
		| proc_dcl_part
		;

label_dcl_part	: LABEL labels ';'
		;
labels		: labels ',' label
		| label
		;
label		: UNSIGNED_INT		/* 0 <= value <= 9999 [6.1.6] */
		;

const_dcl_part	: CONST const_defs ';'
		;
const_defs	: const_defs ';' const_def
		| const_def
		;
const_def	: {its_a="constant";} newident '=' constant
		;

constant	: unsigned_num
		| '+' unsigned_num
		| '-' unsigned_num
		| ident				/*check it is constant*/
		| '+' ident
		| '-' ident
		| STRING			/*type is char if len=1*/
		;

unsigned_num	: UNSIGNED_INT
		| UNSIGNED_REAL
		;

type_dcl_part	: TYPE type_defs ';'
		;
type_defs	: type_defs ';' type_def
		| type_def
		;
type_def	: {its_a="type";} newident '=' type
		;

type		: simple_type
		| PACKED struct_type
		| struct_type
		| '^' IDENTIFIER    /*check forward reference semantics*/
		;

simple_type	: '(' {its_a="enumerated-literal";} newident_list ')'
		| constant DOTDOT constant
		| ident
		;

struct_type	: ARRAY '[' index_t_list ']' OF type
		| RECORD /*consider this a scope*/ field_list END
		| SET OF simple_type
		| SFILE OF type
		;
index_t_list	: index_t_list ',' simple_type
		| simple_type
		;
field_list	: fixed_part
		| fixed_part ';' variant_part
		| variant_part
		;
fixed_part	: fixed_part ';' record_section
		| record_section
		;
record_section	: {its_a="field";} newident_list ':' type
		| /*empty*/
		;
variant_part	: CASE {its_a="field";} tag_field OF variants
		;
tag_field	: newident ':' ident 
		| ident /*type*/
		;
variants	: variants ';' variant
		| variant
		;
variant		: case_label_list ':' '(' field_list ')'
		| /*empty*/
		;

var_dcl_part	: VAR variable_dcls ';'
		;
variable_dcls	: variable_dcls ';' variable_dcl
		| variable_dcl
		;
variable_dcl	: {its_a="variable";} newident_list ':' type
		;
newident_list	: new_id_list {its_a="don't know";}
		;
new_id_list	: new_id_list ',' newident
		| newident
		;

proc_dcl_part	: proc_or_func
		;
proc_or_func	: proc_heading ';' body ';'     /*check if forward or fwd refd*/
			{lexical_level--;
			}
		| func_heading ';' body ';'  /*also func heading may be -type */
			{lexical_level--;
			}
		;
proc_heading	: PROCEDURE
			{if(param_level==0)its_a="procedure";}
		  newident {lexical_level++;}
			formal_params
		;
func_heading	: FUNCTION
			{if(param_level==0)its_a="function";}
		  newident {lexical_level++;}
			function_form
		;
function_form	: /*empty*/			/*if forward referenced*/
		| formal_params ':' ident
		;

body		: block
			/* result determined in block */
		| IDENTIFIER				/*directive-FORWARD*/
		;
formal_params	: /*empty*/
		| '(' {param_level++;} formal_p_sects ')' {param_level--;}
		;
formal_p_sects	: formal_p_sects ';' formal_p_sect
		| formal_p_sect
		;
formal_p_sect	: {its_a="value-parameter";} param_group
		| VAR {its_a="var-parameter";} param_group
		| {its_a="procedure-parameter";} proc_heading
			{lexical_level--;}
		| {its_a="function-parameter";} func_heading  {lexical_level--;}
		;
param_group	: newident_list ':' paramtype
		;
paramtype	: ident
		| ARRAY '[' index_specs ']' OF paramtype
		| PACKED ARRAY '[' index_spec ']' OF ident
		;
index_specs	: index_specs ';' index_spec
		| index_spec
		;
index_spec	: {its_a="conformant-bound";} newident DOTDOT newident ':' ident
		;

statement_part	: compound_stmt
		;
compound_stmt	: SBEGIN statements END
		;
statements	: statements ';' statement
		| statement
		;
statement	: /*empty*/
		| label ':' statement
		| compound_stmt
		| assignment
		| procedure_call
		| GOTO label
		| IF expression THEN statement
		| IF expression THEN statement ELSE statement
		| CASE expression OF case_list END
		| WHILE expression DO statement
		| REPEAT statements UNTIL expression
		| FOR ident BECOMES expression direction expression DO statement
		| WITH rec_var_list DO statement 
		;
direction	: TO
		| DOWNTO
		;

assignment	: variable BECOMES expression	/* must test for fn_ident */
	/*	| ident BECOMES expression	*/
		;

procedure_call	: ident actual_params
		;

actual_params	:  /*empty*/
		| '(' actuals_list ')'
		;
actuals_list	: actuals_list ',' actual_param
		| actual_param
		;
actual_param	: expression    /* which could be a variable or a proc/fn id */
		| expression colon_things  /* only in i/o */
		;
colon_things    : ":" expression	/*integer*/
		| ":" expression ":" expression 
		;

case_list	: case_list ';' case_list_elem
		| case_list_elem
		;
case_list_elem	: case_label_list ':' statement
		| /*empty*/
		;
case_label_list	: case_label_list ',' case_label
		| case_label
		;
case_label	: constant
		;

rec_var_list	: rec_var_list ',' record_var
		| record_var
		;

expression	: simple_expr
		| simple_expr relational_op simple_expr
		;
relational_op	: '='
		| '<'
		| '>'
		| LE
		| GE
		| NE
		| IN
		;

simple_expr	: term
		| '+' term
		| '-' term
		| simple_expr add_op term
		;
add_op		: '+'
		| '-'
		| OR
		;

term		: factor
		| term mult_op factor
		;
mult_op		: '*'
		| '/'
		| DIV
		| MOD
		| AND
		;

factor		: variable		/* could be a const_ident of fn_call*/
		| unsigned_lit
		| '(' expression ')'
	/*	| function_call		*/
		| set
		| NOT factor
		;

unsigned_lit	: unsigned_num
		| STRING			/*type is char if len=1*/
		| NIL
		;
/*
function_call	: ident actual_params
		;
*/

set		: '[' member_list ']'
		;
member_list	: /*empty*/
		| members
		;
members		: members ',' member
		| member
		;
member		: expression
		| expression DOTDOT expression
		;

/* kludge */
variable	: ident actual_params	/* check variable, const_id, fn_call */
		| variable '[' expressions ']'
		| variable '.' ident
		| variable '^'
		;
expressions	: expressions ',' expression
		| expression
		;
record_var	: variable
		;
ident		: IDENTIFIER
		;
newident	: IDENTIFIER
		    { 
			if(param_level<2)
			    printf("%s\t%s\n", its_a, lastident);
		    }
		;
%%   /*start of routines*/

yyerror(msg) char *msg;
{
    if(msg==NULL || *msg=='\0')
	fprintf(stderr, "Error at %s near line %d\n",
				token_name(yychar), yylineno);
    else
	fprintf(stderr, "Error at %s near line %d : %s\n",
				token_name(yychar), yylineno, msg);
    exit(1);
}

parser_info()
{
    printf("\n%d line%s parsed\n", yylineno, plural(yylineno));
}


internal_error(s,a1,a2,a3,a4)
{
    fprintf(stderr, "Internal error: ");
    fprintf(stderr, s, a1, a2, a3, a4);
    exit(2);
}

warning(fmt, a1, a2, a3, a4)
{
    fprintf(stderr, "Warning line %d: ", yylineno);
    fprintf(stderr, fmt, a1, a2, a3, a4);
    fprintf(stderr, "\n");
}

main()
{
    yyparse();
}
