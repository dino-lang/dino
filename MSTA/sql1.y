	/* symbolic tokens */

%union {
	int intval;
	double floatval;
	char *strval;
	int subtok;
}
	
%token NAME
%token STRING
%token INTNUM APPROXNUM

	/* operators */

%left OR
%left AND
%left NOT
%left <subtok> COMPARISON /* = <> < > <= >= */
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

	/* literal keyword tokens */

%token ALL AMMSC ANY AS ASC AUTHORIZATION BETWEEN BY
%token CHARACTER CHECK CLOSE COMMIT CONTINUE CREATE CURRENT
%token CURSOR DECIMAL DECLARE DEFAULT DELETE DESC DISTINCT DOUBLE
%token ESCAPE EXISTS FETCH FLOAT FOR FOREIGN FOUND FROM GOTO
%token GRANT GROUP HAVING IN INDICATOR INSERT INTEGER INTO
%token IS KEY LANGUAGE LIKE MODULE NULLX NUMERIC OF ON
%token OPEN OPTION ORDER PRECISION PRIMARY PRIVILEGES PROCEDURE
%token PUBLIC REAL REFERENCES ROLLBACK SCHEMA SELECT SET
%token SMALLINT SOME SQLCODE SQLERROR TABLE TO UNION
%token UNIQUE UPDATE USER VALUES VIEW WHENEVER WHERE WITH WORK
%token COBOL FORTRAN PASCAL PLI C ADA

%%

sql_list:
		sql ';'
	|	sql_list sql ';'
	;


	/* schema definition language */
	/* Note: other ``sql:'' rules appear later in the grammar */
sql:		schema
	;
	
schema:
		CREATE SCHEMA AUTHORIZATION user opt_schema_element_list
	;

opt_schema_element_list:
		/* empty */
	|	schema_element_list
	;

schema_element_list:
		schema_element
	|	schema_element_list schema_element
	;

schema_element:
		base_table_def
	|	view_def
	|	privilege_def
	;

base_table_def:
		CREATE TABLE table '(' base_table_element_commalist ')'
	;

base_table_element_commalist:
		base_table_element
	|	base_table_element_commalist ',' base_table_element
	;

base_table_element:
		column_def
	|	table_constraint_def
	;

column_def:
		column data_type column_def_opt_list
	;

column_def_opt_list:
		/* empty */
	|	column_def_opt_list column_def_opt
	;

column_def_opt:
		NOT NULLX
	|	NOT NULLX UNIQUE
	|	NOT NULLX PRIMARY KEY
	|	DEFAULT literal
	|	DEFAULT NULLX
	|	DEFAULT USER
	|	CHECK '(' search_condition ')'
	|	REFERENCES table
	|	REFERENCES table '(' column_commalist ')'
	;

table_constraint_def:
		UNIQUE '(' column_commalist ')'
	|	PRIMARY KEY '(' column_commalist ')'
	|	FOREIGN KEY '(' column_commalist ')'
			REFERENCES table 
	|	FOREIGN KEY '(' column_commalist ')'
			REFERENCES table '(' column_commalist ')'
	|	CHECK '(' search_condition ')'
	;

column_commalist:
		column
	|	column_commalist ',' column
	;

view_def:
		CREATE VIEW table opt_column_commalist
		AS query_spec opt_with_check_option
	;
	
opt_with_check_option:
		/* empty */
	|	WITH CHECK OPTION
	;

opt_column_commalist:
		/* empty */
	|	'(' column_commalist ')'
	;

privilege_def:
		GRANT privileges ON table TO grantee_commalist
		opt_with_grant_option
	;

opt_with_grant_option:
		/* empty */
	|	WITH GRANT OPTION
	;

privileges:
		ALL PRIVILEGES
	|	ALL
	|	operation_commalist
	;

operation_commalist:
		operation
	|	operation_commalist ',' operation
	;

operation:
		SELECT
	|	INSERT
	|	DELETE
	|	UPDATE opt_column_commalist
	|	REFERENCES opt_column_commalist
	;


grantee_commalist:
		grantee
	|	grantee_commalist ',' grantee
	;

grantee:
		PUBLIC
	|	user
	;

	/* module language */
sql:		module_def
	;

module_def:
		MODULE opt_module
		LANGUAGE lang
		AUTHORIZATION user
		opt_cursor_def_list
		procedure_def_list
	;

opt_module:
		/* empty */
	|	module
	;

lang:
		COBOL
	|	FORTRAN
	|	PASCAL
	|	PLI
	|	C
	|	ADA
	;

opt_cursor_def_list:
		/* empty */
	|	cursor_def_list
	;

cursor_def_list:
		cursor_def
	|	cursor_def_list cursor_def
	;

cursor_def:
		DECLARE cursor CURSOR FOR query_exp opt_order_by_clause
	;

opt_order_by_clause:
		/* empty */
	|	ORDER BY ordering_spec_commalist
	;

ordering_spec_commalist:
		ordering_spec
	|	ordering_spec_commalist ',' ordering_spec
	;

ordering_spec:
		INTNUM opt_asc_desc
	|	column_ref opt_asc_desc
	;

opt_asc_desc:
		/* empty */
	|	ASC
	|	DESC
	;

procedure_def_list:
		procedure_def
	|	procedure_def_list procedure_def
	;

procedure_def:
		PROCEDURE procedure parameter_def_list ';'
		manipulative_statement_list
	;

manipulative_statement_list:
		manipulative_statement
	|	manipulative_statement_list manipulative_statement
	;

parameter_def_list:
		parameter_def
	|	parameter_def_list parameter_def
	;

parameter_def:
		parameter data_type
	|	SQLCODE
	;

	/* manipulative statements */

sql:		manipulative_statement
	;

manipulative_statement:
		close_statement
	|	commit_statement
	|	delete_statement_positioned
	|	delete_statement_searched
	|	fetch_statement
	|	insert_statement
	|	open_statement
	|	rollback_statement
	|	select_statement
	|	update_statement_positioned
	|	update_statement_searched
	;

close_statement:
		CLOSE cursor
	;

commit_statement:
		COMMIT WORK
	;

delete_statement_positioned:
		DELETE FROM table WHERE CURRENT OF cursor
	;

delete_statement_searched:
		DELETE FROM table opt_where_clause
	;

fetch_statement:
		FETCH cursor INTO target_commalist
	;

insert_statement:
		INSERT INTO table opt_column_commalist values_or_query_spec
	;

values_or_query_spec:
		VALUES '(' insert_atom_commalist ')'
	|	query_spec
	;

insert_atom_commalist:
		insert_atom
	|	insert_atom_commalist ',' insert_atom
	;

insert_atom:
		atom
	|	NULLX
	;

open_statement:
		OPEN cursor
	;

rollback_statement:
		ROLLBACK WORK
	;

select_statement:
		SELECT opt_all_distinct selection
		INTO target_commalist
		table_exp
	;

opt_all_distinct:
		/* empty */
	|	ALL
	|	DISTINCT
	;

update_statement_positioned:
		UPDATE table SET assignment_commalist
		WHERE CURRENT OF cursor
	;

assignment_commalist:
	|	assignment
	|	assignment_commalist ',' assignment
	;

assignment:
		column '=' scalar_exp
	|	column '=' NULLX
	;

update_statement_searched:
		UPDATE table SET assignment_commalist opt_where_clause
	;

target_commalist:
		target
	|	target_commalist ',' target
	;

target:
		parameter_ref
	;

opt_where_clause:
		/* empty */
	|	where_clause
	;

	/* query expressions */

query_exp:
		query_term
	|	query_exp UNION query_term
	|	query_exp UNION ALL query_term
	;

query_term:
		query_spec
	|	'(' query_exp ')'
	;

query_spec:
		SELECT opt_all_distinct selection table_exp
	;

selection:
		scalar_exp_commalist
	|	'*'
	;

table_exp:
		from_clause
		opt_where_clause
		opt_group_by_clause
		opt_having_clause
	;

from_clause:
		FROM table_ref_commalist
	;

table_ref_commalist:
		table_ref
	|	table_ref_commalist ',' table_ref
	;

table_ref:
		table 
	|	table range_variable
	;

where_clause:
		WHERE search_condition
	;

opt_group_by_clause:
		/* empty */
	|	GROUP BY column_ref_commalist
	;

column_ref_commalist:
		column_ref
	|	column_ref_commalist ',' column_ref
	;

opt_having_clause:
		/* empty */
	|	HAVING search_condition
	;

	/* search conditions */

search_condition:
	|	search_condition OR search_condition
	|	search_condition AND search_condition
	|	NOT search_condition
	|	'(' search_condition ')'
	|	predicate
	;

predicate:
		comparison_predicate
	|	between_predicate
	|	like_predicate
	|	test_for_null
	|	in_predicate
	|	all_or_any_predicate
	|	existence_test
	;

comparison_predicate:
		scalar_exp COMPARISON scalar_exp
	|	scalar_exp COMPARISON subquery
	;

between_predicate:
		scalar_exp NOT BETWEEN scalar_exp AND scalar_exp
	|	scalar_exp BETWEEN scalar_exp AND scalar_exp
	;

like_predicate:
		scalar_exp NOT LIKE atom opt_escape
	|	scalar_exp LIKE atom opt_escape
	;

opt_escape:
		/* empty */
	|	ESCAPE atom
	;

test_for_null:
		column_ref IS NOT NULLX
	|	column_ref IS NULLX
	;

in_predicate:
		scalar_exp NOT IN '(' subquery ')'
	|	scalar_exp IN '(' subquery ')'
	|	scalar_exp NOT IN '(' atom_commalist ')'
	|	scalar_exp IN '(' atom_commalist ')'
	;

atom_commalist:
		atom
	|	atom_commalist ',' atom
	;

all_or_any_predicate:
		scalar_exp COMPARISON any_all_some subquery
	;
			
any_all_some:
		ANY
	|	ALL
	|	SOME
	;

existence_test:
		EXISTS subquery
	;

subquery:
		'(' SELECT opt_all_distinct selection table_exp ')'
	;

	/* scalar expressions */

scalar_exp:
		scalar_exp '+' scalar_exp
	|	scalar_exp '-' scalar_exp
	|	scalar_exp '*' scalar_exp
	|	scalar_exp '/' scalar_exp
	|	'+' scalar_exp %prec UMINUS
	|	'-' scalar_exp %prec UMINUS
	|	atom
	|	column_ref
	|	function_ref
	|	'(' scalar_exp ')'
	;

scalar_exp_commalist:
		scalar_exp
	|	scalar_exp_commalist ',' scalar_exp
	;

atom:
		parameter_ref
	|	literal
	|	USER
	;

parameter_ref:
		parameter
	|	parameter parameter
	|	parameter INDICATOR parameter
	;

function_ref:
		AMMSC '(' '*' ')'
	|	AMMSC '(' DISTINCT column_ref ')'
	|	AMMSC '(' ALL scalar_exp ')'
	|	AMMSC '(' scalar_exp ')'
	;

literal:
		STRING
	|	INTNUM
	|	APPROXNUM
	;

	/* miscellaneous */

table:
		NAME
	|	NAME '.' NAME
	;

column_ref:
		NAME
	|	NAME '.' NAME	/* needs semantics */
	|	NAME '.' NAME '.' NAME
	;

		/* data types */

data_type:
		CHARACTER
	|	CHARACTER '(' INTNUM ')'
	|	NUMERIC
	|	NUMERIC '(' INTNUM ')'
	|	NUMERIC '(' INTNUM ',' INTNUM ')'
	|	DECIMAL
	|	DECIMAL '(' INTNUM ')'
	|	DECIMAL '(' INTNUM ',' INTNUM ')'
	|	INTEGER
	|	SMALLINT
	|	FLOAT
	|	FLOAT '(' INTNUM ')'
	|	REAL
	|	DOUBLE PRECISION
	;

	/* the various things you can name */

column:		NAME
	;

cursor:		NAME
	;

module:		NAME
	;

parameter:
		':' NAME
	;

procedure:	NAME
	;

range_variable:	NAME
	;

user:		NAME
	;

	/* embedded condition things */
sql:		WHENEVER NOT FOUND when_action
	|	WHENEVER SQLERROR when_action
	;

when_action:	GOTO NAME
	|	CONTINUE
	;
%%

/*
 * *****************************************************************
 * *                                                               *
 * *    Copyright (c) Digital Equipment Corporation, 1991, 1996    *
 * *                                                               *
 * *   All Rights Reserved.  Unpublished rights  reserved  under   *
 * *   the copyright laws of the United States.                    *
 * *                                                               *
 * *   The software contained on this media  is  proprietary  to   *
 * *   and  embodies  the  confidential  technology  of  Digital   *
 * *   Equipment Corporation.  Possession, use,  duplication  or   *
 * *   dissemination of the software and media is authorized only  *
 * *   pursuant to a valid written license from Digital Equipment  *
 * *   Corporation.                                                *
 * *                                                               *
 * *   RESTRICTED RIGHTS LEGEND   Use, duplication, or disclosure  *
 * *   by the U.S. Government is subject to restrictions  as  set  *
 * *   forth in Subparagraph (c)(1)(ii)  of  DFARS  252.227-7013,  *
 * *   or  in  FAR 52.227-19, as applicable.                       *
 * *                                                               *
 * *****************************************************************
 */
/*
 * HISTORY
 */

/* A lexical scanner generated by flex */

/* scanner skeleton version:
 * @(#)$RCSfile$ $Revision$ (DEC) $Date$
 */

#define FLEX_SCANNER

#include <stdio.h>


/* cfront 1.2 defines "c_plusplus" instead of "__cplusplus" */
#ifdef c_plusplus
#ifndef __cplusplus
#define __cplusplus
#endif
#endif


#ifdef __cplusplus

#include <stdlib.h>
#include <unistd.h>

#define YY_USE_PROTOS

/* the "const" storage-class-modifier is valid */
#define YY_USE_CONST

#else	/* ! __cplusplus */

#ifdef __STDC__

#include <stdlib.h>

#define YY_USE_PROTOS
#define YY_USE_CONST

#endif	/* __STDC__ */
#endif	/* ! __cplusplus */

#if defined (YY_USE_PROTOS)
#define YY_PROTO(proto) proto
#else
#define YY_PROTO(proto) ()
#endif


/* amount of stuff to slurp up with each read */
#ifndef YY_READ_BUF_SIZE
#define YY_READ_BUF_SIZE 8192
#endif

/* returned upon end-of-file */
#define YY_END_TOK 0

/* copy whatever the last rule matched to the standard output */

/* this used to be an fputs(), but since the string might contain NUL's,
 * we now use fwrite()
 */
#define ECHO (void) fwrite( yytext, yyleng, 1, yyout )

/* gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#define YY_INPUT(buf,result,max_size) \
	if ( (result = read( fileno(yyin), (char *) buf, max_size )) < 0 ) \
	    YY_FATAL_ERROR( "read() in flex scanner failed" );
#define YY_NULL 0

/* no semi-colon after return; correct usage is to write "yyterminate();" -
 * we don't want an extra ';' after the "return" because that will cause
 * some compilers to complain about unreachable statements.
 */
#define yyterminate() return ( YY_NULL )

/* report a fatal error */

/* The funky do-while is used to turn this macro definition into
 * a single C statement (which needs a semi-colon terminator).
 * This avoids problems with code like:
 *
 * 	if ( something_happens )
 *		YY_FATAL_ERROR( "oops, the something happened" );
 *	else
 *		everything_okay();
 *
 * Prior to using the do-while the compiler would get upset at the
 * "else" because it interpreted the "if" statement as being all
 * done when it reached the ';' after the YY_FATAL_ERROR() call.
 */

#define YY_FATAL_ERROR(msg) \
	do \
		{ \
		(void) fputs( msg, stderr ); \
		(void) putc( '\n', stderr ); \
		exit( 1 ); \
		} \
	while ( 0 )

/* default yywrap function - always treat EOF as an EOF */
#define yywrap() 1

/* enter a start condition.  This macro really ought to take a parameter,
 * but we do it the disgusting crufty way forced on us by the ()-less
 * definition of BEGIN
 */
#define BEGIN yy_start = 1 + 2 *

/* action number for EOF rule of a given start state */
#define YY_STATE_EOF(state) (YY_END_OF_BUFFER + state + 1)

/* special action meaning "start processing a new file" */
#define YY_NEW_FILE \
	do \
		{ \
		yy_init_buffer( yy_current_buffer, yyin ); \
		yy_load_buffer_state(); \
		} \
	while ( 0 )

/* default declaration of generated scanner - a define so the user can
 * easily add parameters
 */
#define YY_DECL int yylex YY_PROTO(( void )) 

/* code executed at the end of each rule */
#define YY_BREAK break;

#define YY_END_OF_BUFFER_CHAR 0

#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE (YY_READ_BUF_SIZE * 2) /* size of default input buffer */
#endif

typedef struct yy_buffer_state *YY_BUFFER_STATE;

#define YY_CHAR unsigned char
# line 1 "scn1.l"
#define INITIAL 0
# line 2 "scn1.l"
/*
 * AT&T lex can't handle this lexer due to lex bugs.  It works with flex
 * 2.3.7, pclex 2.0.5, and MKS lex 3.1a.
 */

#include <string.h>

int lineno = 1;
/* MKS needs the next line to increase the NFA table */
# line 16 "scn1.l"

/* done after the current pattern has been matched and before the
 * corresponding action - sets up yytext
 */
#define YY_DO_BEFORE_ACTION \
	yytext = (char *) yy_bp; \
	yytext -= yy_more_len; \
	yyleng = yy_cp - (YY_CHAR *)yytext; \
	yy_hold_char = *yy_cp; \
	*yy_cp = '\0'; \
	yy_c_buf_p = yy_cp;

#define EOB_ACT_CONTINUE_SCAN 0
#define EOB_ACT_END_OF_FILE 1
#define EOB_ACT_LAST_MATCH 2

/* return all but the first 'n' matched characters back to the input stream */
#define yyless(n) \
	do \
		{ \
		/* undo effects of setting up yytext */ \
		*yy_cp = yy_hold_char; \
		yy_c_buf_p = yy_cp = yy_bp + n; \
		YY_DO_BEFORE_ACTION; /* set up yytext again */ \
		} \
	while ( 0 )

#define unput(c) yyunput( c, (unsigned char *)yytext )


struct yy_buffer_state
    {
    FILE *yy_input_file;

    YY_CHAR *yy_ch_buf;		/* input buffer */
    YY_CHAR *yy_buf_pos;	/* current position in input buffer */

    /* size of input buffer in bytes, not including room for EOB characters*/
    int yy_buf_size;	

    /* number of characters read into yy_ch_buf, not including EOB characters */
    int yy_n_chars;

    int yy_eof_status;		/* whether we've seen an EOF on this buffer */
#define EOF_NOT_SEEN 0
    /* "pending" happens when the EOF has been seen but there's still
     * some text process
     */
#define EOF_PENDING 1
#define EOF_DONE 2
    };

static YY_BUFFER_STATE yy_current_buffer;

/* we provide macros for accessing buffer states in case in the
 * future we want to put the buffer states in a more general
 * "scanner state"
 */
#define YY_CURRENT_BUFFER yy_current_buffer


/* yy_hold_char holds the character lost when yytext is formed */
static YY_CHAR yy_hold_char;

static int yy_n_chars;		/* number of characters read into yy_ch_buf */



#ifndef YY_USER_ACTION
#define YY_USER_ACTION
#endif

#ifndef YY_USER_INIT
#define YY_USER_INIT
#endif

extern char *yytext;
extern int yyleng;
extern FILE *yyin, *yyout;

char *yytext;
int yyleng;

FILE *yyin = (FILE *) 0, *yyout = (FILE *) 0;

#define YY_END_OF_BUFFER 112
typedef int yy_state_type;
static const short int yy_accept[415] =
    {   0,
        0,    0,  112,  111,  109,  108,  111,   98,   98,   98,
      100,   94,   92,   95,   99,   99,   15,   99,   99,   99,
       99,   99,   99,   99,   99,   99,   99,   99,   99,   99,
       99,   99,   99,   99,   99,   99,  109,    0,  107,  106,
        0,  102,    0,  101,  100,    0,   96,   93,   97,   99,
       99,   99,   99,   10,   99,   99,   99,   14,   99,   99,
       99,   99,   99,   99,   99,   99,   99,   99,   99,   99,
       99,   99,   99,   99,   99,   45,   50,   99,   99,   99,
       99,   99,   99,   99,   99,   58,   59,   99,   62,   99,
       99,   99,   99,   99,   99,   99,   99,   99,   99,   99,

       99,   99,   81,   99,   99,   99,   99,   99,   99,   99,
       99,    0,  110,    0,  105,  101,    0,    0,  103,    1,
        2,    3,    9,   11,   99,    4,   99,   99,   99,   99,
       99,   99,   99,   99,   99,   99,   99,   99,   99,   99,
       99,   99,   99,   99,   99,   99,   36,   99,   99,    0,
       99,   99,   99,   99,   99,   99,   48,   51,   99,   99,
        6,    5,   99,   55,   99,   99,   99,   99,   99,   99,
       65,   99,   99,   99,   99,   99,   99,   99,   99,   99,
       76,   99,   99,   99,    7,   99,   99,   99,   99,   99,
       99,   99,   99,   99,    0,  104,   99,   99,   16,   99,

       99,   99,   99,   99,   99,   99,   99,   99,   99,   99,
       99,   99,   29,   99,   99,   99,   99,   99,   99,   99,
       99,   99,   40,    0,   41,   99,   99,   99,   99,   99,
       99,   49,   99,   53,   99,   56,   99,   60,   99,   99,
       99,   99,   99,   99,   99,   99,   71,   99,   99,   99,
       99,   99,   78,   99,   99,   99,   99,   99,   85,   99,
       87,   99,   99,   90,   91,   99,   99,   99,   17,   18,
       19,   99,   99,    8,   99,   99,   99,   99,   99,   99,
       99,   99,   99,   99,   99,   34,   35,   99,   99,   39,
       41,   42,   43,   99,   99,   99,   99,   99,   99,   99,

       99,   63,   99,   99,   99,   99,   99,   99,   99,   99,
       99,   99,   99,   99,   80,   82,   99,   99,   99,   99,
       89,   99,   99,   99,   20,   99,   22,   99,   24,   99,
       99,   99,   28,   99,   31,   32,   33,   99,   99,   44,
       99,   47,   99,   99,   54,   99,   61,   64,   99,   99,
       99,   99,   70,   99,   99,   74,   75,   99,   99,   83,
       84,   86,   99,   99,   13,   99,   99,   23,   25,   26,
       27,   99,   37,   38,   99,   48,   99,   57,   99,   67,
       99,   99,   99,   99,   99,   79,   99,   99,   99,   21,
       30,   99,   52,   99,   99,   99,   99,   73,   77,   88,

       99,   16,   46,   66,   99,   69,   99,   99,   68,   72,
       99,   99,   12,    0
    } ;

static const YY_CHAR yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    4,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    1,    1,    1,    1,    1,    1,    5,    6,
        6,    6,    7,    6,    8,    9,    6,   10,   10,   10,
       10,   10,   10,   10,   10,   10,   10,    6,    6,   11,
       12,   13,    1,    1,   14,   15,   16,   17,   18,   19,
       20,   21,   22,   23,   24,   25,   26,   27,   28,   29,
       30,   31,   32,   33,   34,   35,   36,   37,   38,   39,
        1,    1,    1,    1,   40,    1,   23,   23,   23,   23,

       41,   23,   23,   23,   23,   23,   23,   23,   23,   23,
       23,   23,   23,   23,   23,   23,   23,   23,   23,   23,
       23,   23,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    } ;

static const YY_CHAR yy_meta[42] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    2,
        1,    1,    1,    2,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        2
    } ;

static const short int yy_base[418] =
    {   0,
        0,    0,  450,  451,   40,  451,   40,  451,  441,   36,
       38,   37,  451,  436,   34,   34,   39,   35,   39,   56,
       27,  433,   48,    0,  428,   64,   68,   55,   66,   74,
       73,   76,   84,   82,   93,   95,  116,  116,  451,  451,
      442,  112,  117,  116,  119,  125,  451,  451,  451,    0,
      430,  418,  114,  426,  408,  420,  406,    0,  122,  410,
      124,  419,  405,  122,  403,  400,  417,  410,  398,  402,
       31,  401,  111,  128,  393,  129,    0,  389,  399,  401,
      387,  396,  405,  388,  123,    0,    0,  137,  403,  387,
      396,  141,  402,  152,  391,  394,  139,  400,  387,  387,

      385,  395,    0,  387,  391,  389,  381,  387,  386,  370,
      371,  398,  451,  390,  389,  155,  167,  388,  387,    0,
        0,    0,    0,    0,  375,    0,  359,  363,  377,  360,
      363,  364,  356,  361,  373,  136,   78,  372,  367,  368,
      350,  367,  367,  348,  363,  364,  158,  350,  350,  176,
      347,  347,  339,  350,  349,  352,  161,    0,  349,  350,
        0,    0,  333,    0,  341,  347,  337,  341,  344,  345,
        0,  344,  155,  343,  333,  332,  338,  330,  336,  335,
        0,  327,  333,  334,    0,  324,  115,  334,  316,  312,
      309,  153,  323,  319,  332,  331,  312,  321,  324,  313,

      318,  310,  312,  311,  299,  298,  312,  301,  302,  313,
      292,  292,    0,  302,  298,  293,  288,  299,  286,  296,
      286,  299,    0,  287,    0,  281,  284,  285,  295,  279,
      289,    0,  274,    0,  282,    0,  275,    0,  277,  273,
      289,  280,  287,  278,  281,  276,    0,  266,  281,  269,
      278,  268,    0,  264,  273,  263,  255,  255,    0,  269,
        0,  268,  267,    0,    0,  253,  265,  266,    0,    0,
        0,  248,  253,    0,  261,  251,  246,  262,  244,  249,
      255,  245,  253,  252,  237,    0,    0,  248,  253,    0,
      451,    0,    0,  246,  251,  231,  245,  248,  243,  238,

      232,    0,  233,  225,  225,  230,  237,  237,  234,  237,
      236,  216,  226,  230,    0,    0,  228,  227,  212,  208,
        0,  220,  214,  207,    0,  205,    0,  205,    0,  212,
      218,  202,    0,  218,    0,    0,    0,  206,  205,    0,
      198,    0,  199,  209,    0,  212,    0,    0,  205,  188,
      207,  190,    0,  196,  206,    0,    0,  194,  202,    0,
        0,    0,  201,  179,    0,  199,  198,    0,    0,    0,
        0,  182,    0,    0,  186,    0,  190,    0,  179,    0,
      186,  174,  188,  179,  169,    0,  170,  186,  168,    0,
        0,  167,    0,  170,  177,  176,  175,    0,    0,    0,

      159,    0,    0,    0,  156,    0,  155,  164,    0,    0,
      157,  156,    0,  451,  209,  180,  211
    } ;

static const short int yy_def[418] =
    {   0,
      414,    1,  414,  414,  414,  414,  415,  414,  414,  414,
      414,  414,  414,  414,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  414,  415,  414,  414,
      417,  414,  414,  414,  414,  414,  414,  414,  414,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,

      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  417,  414,  414,  414,  414,  414,  414,  414,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  414,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  414,  414,  416,  416,  416,  416,

      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  414,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      414,  416,  416,  416,  416,  416,  416,  416,  416,  416,

      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,

      416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      416,  416,  416,    0,  414,  414,  414
    } ;

static const short int yy_nxt[493] =
    {   0,
        4,    5,    6,    5,    7,    8,    8,    9,   10,   11,
       12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
       22,   23,   24,   25,   26,   27,   28,   29,   30,   24,
       31,   32,   33,   34,   35,   36,   24,   24,   24,    4,
       24,   37,   39,   37,   40,   42,   44,   45,   47,   48,
       51,   57,   64,   43,   73,   46,   65,   74,   52,   59,
       53,  147,   66,   60,  148,   54,   61,   55,   56,   62,
       67,   58,   63,   69,   76,   68,   43,   79,   46,   77,
       70,   81,   84,   71,   86,   80,   72,   90,   85,   82,
       94,   96,   87,   97,   88,   83,   89,  102,   91,  209,

       95,   98,  210,   99,   92,  100,  107,   93,  104,  101,
      105,  103,  150,  106,  108,  109,  110,   37,   39,   37,
       40,   42,  111,  114,  114,  116,  115,   44,   45,   43,
      122,  118,  118,  117,  119,  128,   46,  137,  131,  129,
      138,  152,  256,  151,  257,  155,  139,  165,  166,  132,
      133,  123,   43,  140,  167,  153,  117,  134,  172,   46,
      156,  157,  173,  180,  116,  176,  207,  208,  174,  168,
      177,  181,  117,  195,  195,  220,  196,  150,  231,  262,
      243,   50,  413,  263,  412,  411,  410,  409,  232,  244,
      221,  408,  407,  406,  405,  117,  404,  403,  402,  401,

      400,  399,  398,  397,  396,  395,  394,  393,  224,   38,
       38,  112,  112,  392,  391,  390,  389,  388,  387,  386,
      385,  384,  383,  382,  381,  380,  379,  378,  377,  376,
      375,  374,  373,  372,  371,  370,  369,  368,  367,  366,
      365,  364,  363,  362,  361,  360,  359,  358,  357,  356,
      355,  354,  353,  352,  351,  350,  349,  348,  347,  346,
      345,  344,  343,  342,  341,  340,  339,  338,  337,  336,
      335,  334,  333,  332,  331,  330,  329,  328,  327,  326,
      325,  324,  323,  322,  321,  320,  319,  318,  317,  316,
      315,  314,  313,  312,  311,  310,  309,  308,  307,  306,

      305,  304,  303,  302,  301,  300,  299,  298,  297,  296,
      295,  294,  293,  292,  291,  290,  289,  288,  287,  286,
      285,  284,  283,  282,  281,  280,  279,  278,  277,  276,
      275,  274,  273,  272,  271,  270,  269,  268,  267,  266,
      196,  196,  265,  264,  261,  260,  259,  258,  255,  254,
      253,  252,  251,  250,  249,  248,  247,  246,  245,  242,
      241,  240,  239,  238,  237,  236,  235,  234,  233,  230,
      229,  228,  227,  226,  225,  223,  222,  219,  218,  217,
      216,  215,  214,  213,  212,  211,  206,  205,  204,  203,
      202,  201,  200,  199,  198,  197,  119,  119,  115,  115,

      113,  194,  193,  192,  191,  190,  189,  188,  187,  186,
      185,  184,  183,  182,  179,  178,  175,  171,  170,  169,
      164,  163,  162,  161,  160,  159,  158,  154,  149,  146,
      145,  144,  143,  142,  141,  136,  135,  130,  127,  126,
      125,  124,  121,  120,  113,   78,   75,   49,   41,  414,
        3,  414,  414,  414,  414,  414,  414,  414,  414,  414,
      414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
      414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
      414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
      414,  414

    } ;

static const short int yy_chk[493] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    5,    7,    5,    7,   10,   11,   11,   12,   12,
       15,   16,   18,   10,   21,   11,   18,   21,   15,   17,
       15,   71,   18,   17,   71,   15,   17,   15,   15,   17,
       19,   16,   17,   20,   23,   19,   10,   26,   11,   23,
       20,   27,   28,   20,   29,   26,   20,   30,   28,   27,
       31,   32,   29,   32,   29,   27,   29,   33,   30,  137,

       31,   32,  137,   32,   30,   32,   35,   30,   34,   32,
       34,   33,   73,   34,   35,   36,   36,   37,   38,   37,
       38,   42,   36,   43,   43,   44,   43,   45,   45,   42,
       53,   46,   46,   44,   46,   59,   45,   64,   61,   59,
       64,   74,  187,   73,  187,   76,   64,   85,   85,   61,
       61,   53,   42,   64,   88,   74,   44,   61,   92,   45,
       76,   76,   92,   97,  116,   94,  136,  136,   92,   88,
       94,   97,  116,  117,  117,  147,  117,  150,  157,  192,
      173,  416,  412,  192,  411,  408,  407,  405,  157,  173,
      147,  401,  397,  396,  395,  116,  394,  392,  389,  388,

      387,  385,  384,  383,  382,  381,  379,  377,  150,  415,
      415,  417,  417,  375,  372,  367,  366,  364,  363,  359,
      358,  355,  354,  352,  351,  350,  349,  346,  344,  343,
      341,  339,  338,  334,  332,  331,  330,  328,  326,  324,
      323,  322,  320,  319,  318,  317,  314,  313,  312,  311,
      310,  309,  308,  307,  306,  305,  304,  303,  301,  300,
      299,  298,  297,  296,  295,  294,  289,  288,  285,  284,
      283,  282,  281,  280,  279,  278,  277,  276,  275,  273,
      272,  268,  267,  266,  263,  262,  260,  258,  257,  256,
      255,  254,  252,  251,  250,  249,  248,  246,  245,  244,

      243,  242,  241,  240,  239,  237,  235,  233,  231,  230,
      229,  228,  227,  226,  224,  222,  221,  220,  219,  218,
      217,  216,  215,  214,  212,  211,  210,  209,  208,  207,
      206,  205,  204,  203,  202,  201,  200,  199,  198,  197,
      196,  195,  194,  193,  191,  190,  189,  188,  186,  184,
      183,  182,  180,  179,  178,  177,  176,  175,  174,  172,
      170,  169,  168,  167,  166,  165,  163,  160,  159,  156,
      155,  154,  153,  152,  151,  149,  148,  146,  145,  144,
      143,  142,  141,  140,  139,  138,  135,  134,  133,  132,
      131,  130,  129,  128,  127,  125,  119,  118,  115,  114,

      112,  111,  110,  109,  108,  107,  106,  105,  104,  102,
      101,  100,   99,   98,   96,   95,   93,   91,   90,   89,
       84,   83,   82,   81,   80,   79,   78,   75,   72,   70,
       69,   68,   67,   66,   65,   63,   62,   60,   57,   56,
       55,   54,   52,   51,   41,   25,   22,   14,    9,    3,
      414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
      414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
      414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
      414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
      414,  414

    } ;

static yy_state_type yy_last_accepting_state;
static YY_CHAR *yy_last_accepting_cpos;

/* the intent behind this definition is that it'll catch
 * any uses of REJECT which flex missed
 */
#define REJECT reject_used_but_not_detected
static int yy_more_flag = 0;
static int yy_doing_yy_more = 0;
static int yy_more_len = 0;
#define yymore() do { yy_more_flag = 1; }while(0)
#define YY_MORE_ADJ (yy_doing_yy_more ? yy_more_len : 0)

/* these variables are all declared out here so that section 3 code can
 * manipulate them
 */
/* points to current character in buffer */
static YY_CHAR *yy_c_buf_p = (YY_CHAR *) 0;
static int yy_init = 1;		/* whether we need to initialize */
static int yy_start = 0;	/* start state number */

/* flag which is used to allow yywrap()'s to do buffer switches
 * instead of setting up a fresh yyin.  A bit of a hack ...
 */
static int yy_did_buffer_switch_on_eof;

static yy_state_type yy_get_previous_state YY_PROTO(( void ));
static yy_state_type yy_try_NUL_trans YY_PROTO(( yy_state_type current_state ));
static int yy_get_next_buffer YY_PROTO(( void ));
static void yyunput YY_PROTO(( YY_CHAR c, YY_CHAR *buf_ptr ));
void yyrestart YY_PROTO(( FILE *input_file ));
void yy_switch_to_buffer YY_PROTO(( YY_BUFFER_STATE new_buffer ));
void yy_load_buffer_state YY_PROTO(( void ));
YY_BUFFER_STATE yy_create_buffer YY_PROTO(( FILE *file, int size ));
void yy_delete_buffer YY_PROTO(( YY_BUFFER_STATE b ));
void yy_init_buffer YY_PROTO(( YY_BUFFER_STATE b, FILE *file ));

#define yy_new_buffer yy_create_buffer

#ifdef __cplusplus
static int yyinput YY_PROTO(( void ));
#else
static int input YY_PROTO(( void ));
#endif

YY_DECL
    {
    register yy_state_type yy_current_state;
    register YY_CHAR *yy_cp, *yy_bp;
    register int yy_act;



    if ( yy_init )
	{
	YY_USER_INIT;

	if ( ! yy_start )
	    yy_start = 1;	/* first start state */

	if ( ! yyin )
	    yyin = stdin;

	if ( ! yyout )
	    yyout = stdout;

	if ( yy_current_buffer )
	    yy_init_buffer( yy_current_buffer, yyin );
	else
	    yy_current_buffer = yy_create_buffer( yyin, YY_BUF_SIZE );

	yy_load_buffer_state();

	yy_init = 0;
	}

    while ( 1 )		/* loops until end-of-file is reached */
	{
	yy_more_len = 0;
	yy_doing_yy_more = yy_more_flag;
	if ( yy_doing_yy_more )
	    {
	    yy_more_len = yyleng;
	    yy_more_flag = 0;
	    }
	yy_cp = yy_c_buf_p;

	/* support of yytext */
	*yy_cp = yy_hold_char;

	/* yy_bp points to the position in yy_ch_buf of the start of the
	 * current run.
	 */
	yy_bp = yy_cp;

	yy_current_state = yy_start;
yy_match:
	do
	    {
	    register YY_CHAR yy_c = yy_ec[*yy_cp];
	    if ( yy_accept[yy_current_state] )
		{
		yy_last_accepting_state = yy_current_state;
		yy_last_accepting_cpos = yy_cp;
		}
	    while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
		{
		yy_current_state = yy_def[yy_current_state];
		if ( yy_current_state >= 415 )
		    yy_c = yy_meta[yy_c];
		}
	    yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
	    ++yy_cp;
	    }
	while ( yy_current_state != 414 );
	yy_cp = yy_last_accepting_cpos;
	yy_current_state = yy_last_accepting_state;

yy_find_action:
	yy_act = yy_accept[yy_current_state];

	YY_DO_BEFORE_ACTION;
	YY_USER_ACTION;

do_action:	/* this label is used only to access EOF actions */


	switch ( yy_act )
	    {
	    case 0: /* must backtrack */
	    /* undo the effects of YY_DO_BEFORE_ACTION */
	    *yy_cp = yy_hold_char;
	    yy_cp = yy_last_accepting_cpos;
	    yy_current_state = yy_last_accepting_state;
	    goto yy_find_action;

	/* literal keyword tokens */
case 1:
# line 20 "scn1.l"
{ return ADA; }
	YY_BREAK
case 2:
# line 21 "scn1.l"
{ return ALL; }
	YY_BREAK
case 3:
# line 22 "scn1.l"
{ return AND; }
	YY_BREAK
case 4:
# line 23 "scn1.l"
{ return AMMSC; }
	YY_BREAK
case 5:
# line 24 "scn1.l"
{ return AMMSC; }
	YY_BREAK
case 6:
# line 25 "scn1.l"
{ return AMMSC; }
	YY_BREAK
case 7:
# line 26 "scn1.l"
{ return AMMSC; }
	YY_BREAK
case 8:
# line 27 "scn1.l"
{ return AMMSC; }
	YY_BREAK
case 9:
# line 28 "scn1.l"
{ return ANY; }
	YY_BREAK
case 10:
# line 29 "scn1.l"
{ return AS; }
	YY_BREAK
case 11:
# line 30 "scn1.l"
{ return ASC; }
	YY_BREAK
case 12:
# line 31 "scn1.l"
{ return AUTHORIZATION; }
	YY_BREAK
case 13:
# line 32 "scn1.l"
{ return BETWEEN; }
	YY_BREAK
case 14:
# line 33 "scn1.l"
{ return BY; }
	YY_BREAK
case 15:
# line 34 "scn1.l"
{ return C; }
	YY_BREAK
case 16:
# line 35 "scn1.l"
{ return CHARACTER; }
	YY_BREAK
case 17:
# line 36 "scn1.l"
{ return CHECK; }
	YY_BREAK
case 18:
# line 37 "scn1.l"
{ return CLOSE; }
	YY_BREAK
case 19:
# line 38 "scn1.l"
{ return COBOL; }
	YY_BREAK
case 20:
# line 39 "scn1.l"
{ return COMMIT; }
	YY_BREAK
case 21:
# line 40 "scn1.l"
{ return CONTINUE; }
	YY_BREAK
case 22:
# line 41 "scn1.l"
{ return CREATE; }
	YY_BREAK
case 23:
# line 42 "scn1.l"
{ return CURRENT; }
	YY_BREAK
case 24:
# line 43 "scn1.l"
{ return CURSOR; }
	YY_BREAK
case 25:
# line 44 "scn1.l"
{ return DECIMAL; }
	YY_BREAK
case 26:
# line 45 "scn1.l"
{ return DECLARE; }
	YY_BREAK
case 27:
# line 46 "scn1.l"
{ return DEFAULT; }
	YY_BREAK
case 28:
# line 47 "scn1.l"
{ return DELETE; }
	YY_BREAK
case 29:
# line 48 "scn1.l"
{ return DESC; }
	YY_BREAK
case 30:
# line 49 "scn1.l"
{ return DISTINCT; }
	YY_BREAK
case 31:
# line 50 "scn1.l"
{ return DOUBLE; }
	YY_BREAK
case 32:
# line 51 "scn1.l"
{ return ESCAPE; }
	YY_BREAK
case 33:
# line 52 "scn1.l"
{ return EXISTS; }
	YY_BREAK
case 34:
# line 53 "scn1.l"
{ return FETCH; }
	YY_BREAK
case 35:
# line 54 "scn1.l"
{ return FLOAT; }
	YY_BREAK
case 36:
# line 55 "scn1.l"
{ return FOR; }
	YY_BREAK
case 37:
# line 56 "scn1.l"
{ return FOREIGN; }
	YY_BREAK
case 38:
# line 57 "scn1.l"
{ return FORTRAN; }
	YY_BREAK
case 39:
# line 58 "scn1.l"
{ return FOUND; }
	YY_BREAK
case 40:
# line 59 "scn1.l"
{ return FROM; }
	YY_BREAK
case 41:
# line 60 "scn1.l"
{ return GOTO; }
	YY_BREAK
case 42:
# line 61 "scn1.l"
{ return GRANT; }
	YY_BREAK
case 43:
# line 62 "scn1.l"
{ return GROUP; }
	YY_BREAK
case 44:
# line 63 "scn1.l"
{ return HAVING; }
	YY_BREAK
case 45:
# line 64 "scn1.l"
{ return IN; }
	YY_BREAK
case 46:
# line 65 "scn1.l"
{ return INDICATOR; }
	YY_BREAK
case 47:
# line 66 "scn1.l"
{ return INSERT; }
	YY_BREAK
case 48:
# line 67 "scn1.l"
{ return INTEGER; }
	YY_BREAK
case 49:
# line 68 "scn1.l"
{ return INTO; }
	YY_BREAK
case 50:
# line 69 "scn1.l"
{ return IS; }
	YY_BREAK
case 51:
# line 70 "scn1.l"
{ return KEY; }
	YY_BREAK
case 52:
# line 71 "scn1.l"
{ return LANGUAGE; }
	YY_BREAK
case 53:
# line 72 "scn1.l"
{ return LIKE; }
	YY_BREAK
case 54:
# line 73 "scn1.l"
{ return MODULE; }
	YY_BREAK
case 55:
# line 74 "scn1.l"
{ return NOT; }
	YY_BREAK
case 56:
# line 75 "scn1.l"
{ return NULLX; }
	YY_BREAK
case 57:
# line 76 "scn1.l"
{ return NUMERIC; }
	YY_BREAK
case 58:
# line 77 "scn1.l"
{ return OF; }
	YY_BREAK
case 59:
# line 78 "scn1.l"
{ return ON; }
	YY_BREAK
case 60:
# line 79 "scn1.l"
{ return OPEN; }
	YY_BREAK
case 61:
# line 80 "scn1.l"
{ return OPTION; }
	YY_BREAK
case 62:
# line 81 "scn1.l"
{ return OR; }
	YY_BREAK
case 63:
# line 82 "scn1.l"
{ return ORDER; }
	YY_BREAK
case 64:
# line 83 "scn1.l"
{ return PASCAL; }
	YY_BREAK
case 65:
# line 84 "scn1.l"
{ return PLI; }
	YY_BREAK
case 66:
# line 85 "scn1.l"
{ return PRECISION; }
	YY_BREAK
case 67:
# line 86 "scn1.l"
{ return PRIMARY; }
	YY_BREAK
case 68:
# line 87 "scn1.l"
{ return PRIVILEGES; }
	YY_BREAK
case 69:
# line 88 "scn1.l"
{ return PROCEDURE; }
	YY_BREAK
case 70:
# line 89 "scn1.l"
{ return PUBLIC; }
	YY_BREAK
case 71:
# line 90 "scn1.l"
{ return REAL; }
	YY_BREAK
case 72:
# line 91 "scn1.l"
{ return REFERENCES; }
	YY_BREAK
case 73:
# line 92 "scn1.l"
{ return ROLLBACK; }
	YY_BREAK
case 74:
# line 93 "scn1.l"
{ return SCHEMA; }
	YY_BREAK
case 75:
# line 94 "scn1.l"
{ return SELECT; }
	YY_BREAK
case 76:
# line 95 "scn1.l"
{ return SET; }
	YY_BREAK
case 77:
# line 96 "scn1.l"
{ return SMALLINT; }
	YY_BREAK
case 78:
# line 97 "scn1.l"
{ return SOME; }
	YY_BREAK
case 79:
# line 98 "scn1.l"
{ return SQLCODE; }
	YY_BREAK
case 80:
# line 99 "scn1.l"
{ return TABLE; }
	YY_BREAK
case 81:
# line 100 "scn1.l"
{ return TO; }
	YY_BREAK
case 82:
# line 101 "scn1.l"
{ return UNION; }
	YY_BREAK
case 83:
# line 102 "scn1.l"
{ return UNIQUE; }
	YY_BREAK
case 84:
# line 103 "scn1.l"
{ return UPDATE; }
	YY_BREAK
case 85:
# line 104 "scn1.l"
{ return USER; }
	YY_BREAK
case 86:
# line 105 "scn1.l"
{ return VALUES; }
	YY_BREAK
case 87:
# line 106 "scn1.l"
{ return VIEW; }
	YY_BREAK
case 88:
# line 107 "scn1.l"
{ return WHENEVER; }
	YY_BREAK
case 89:
# line 108 "scn1.l"
{ return WHERE; }
	YY_BREAK
case 90:
# line 109 "scn1.l"
{ return WITH; }
	YY_BREAK
case 91:
# line 110 "scn1.l"
{ return WORK; }
	YY_BREAK
	/* punctuation */
case 92:
# line 115 "scn1.l"
case 93:
# line 116 "scn1.l"
case 94:
# line 117 "scn1.l"
case 95:
# line 118 "scn1.l"
case 96:
# line 119 "scn1.l"
case 97:
# line 119 "scn1.l"
{ return COMPARISON; }
	YY_BREAK
case 98:
# line 121 "scn1.l"
{ return yytext[0]; }
	YY_BREAK
	/* names */
case 99:
# line 125 "scn1.l"
{ return NAME; }
	YY_BREAK
	/* numbers */
case 100:
# line 130 "scn1.l"
case 101:
# line 131 "scn1.l"
case 102:
# line 131 "scn1.l"
{ return INTNUM; }
	YY_BREAK
case 103:
# line 134 "scn1.l"
case 104:
# line 135 "scn1.l"
case 105:
# line 135 "scn1.l"
{ return APPROXNUM; }
	YY_BREAK
	/* strings */
case 106:
# line 139 "scn1.l"
{
		int c = input();

		unput(c);	/* just peeking */
		if(c != '\'') {
			return STRING;
		} else
			yymore();
	}
	YY_BREAK
case 107:
*yy_cp = yy_hold_char; /* undo effects of setting up yytext */
yy_c_buf_p = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up yytext again */
# line 149 "scn1.l"
{ yyerror("Unterminated string"); }
	YY_BREAK
case 108:
# line 151 "scn1.l"
lineno++;
	YY_BREAK
case 109:
# line 153 "scn1.l"
;	/* white space */
	YY_BREAK
case 110:
*yy_cp = yy_hold_char; /* undo effects of setting up yytext */
yy_c_buf_p = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up yytext again */
# line 155 "scn1.l"
;	/* comment */
	YY_BREAK
case 111:
# line 157 "scn1.l"
ECHO;
	YY_BREAK
case YY_STATE_EOF(INITIAL):
    yyterminate();

	    case YY_END_OF_BUFFER:
		{
		/* amount of text matched not including the EOB char */
		int yy_amount_of_matched_text = yy_cp - (unsigned char *)yytext - 1;

		/* undo the effects of YY_DO_BEFORE_ACTION */
		*yy_cp = yy_hold_char;

		/* note that here we test for yy_c_buf_p "<=" to the position
		 * of the first EOB in the buffer, since yy_c_buf_p will
		 * already have been incremented past the NUL character
		 * (since all states make transitions on EOB to the end-
		 * of-buffer state).  Contrast this with the test in yyinput().
		 */
		if ( yy_c_buf_p <= &yy_current_buffer->yy_ch_buf[yy_n_chars] )
		    /* this was really a NUL */
		    {
		    yy_state_type yy_next_state;

		    yy_c_buf_p = (unsigned char *)yytext + yy_amount_of_matched_text;

		    yy_current_state = yy_get_previous_state();

		    /* okay, we're now positioned to make the
		     * NUL transition.  We couldn't have
		     * yy_get_previous_state() go ahead and do it
		     * for us because it doesn't know how to deal
		     * with the possibility of jamming (and we
		     * don't want to build jamming into it because
		     * then it will run more slowly)
		     */

		    yy_next_state = yy_try_NUL_trans( yy_current_state );

		    yy_bp = (unsigned char *)yytext + YY_MORE_ADJ;

		    if ( yy_next_state )
			{
			/* consume the NUL */
			yy_cp = ++yy_c_buf_p;
			yy_current_state = yy_next_state;
			goto yy_match;
			}

		    else
			{
			    yy_cp = yy_last_accepting_cpos;
			    yy_current_state = yy_last_accepting_state;
			goto yy_find_action;
			}
		    }

		else switch ( yy_get_next_buffer() )
		    {
		    case EOB_ACT_END_OF_FILE:
			{
			yy_did_buffer_switch_on_eof = 0;

			if ( yywrap() )
			    {
			    /* note: because we've taken care in
			     * yy_get_next_buffer() to have set up yytext,
			     * we can now set up yy_c_buf_p so that if some
			     * total hoser (like flex itself) wants
			     * to call the scanner after we return the
			     * YY_NULL, it'll still work - another YY_NULL
			     * will get returned.
			     */
			    yy_c_buf_p = (unsigned char *)yytext + YY_MORE_ADJ;

			    yy_act = YY_STATE_EOF((yy_start - 1) / 2);
			    goto do_action;
			    }

			else
			    {
			    if ( ! yy_did_buffer_switch_on_eof )
				YY_NEW_FILE;
			    }
			}
			break;

		    case EOB_ACT_CONTINUE_SCAN:
			yy_c_buf_p = (unsigned char *)yytext + yy_amount_of_matched_text;

			yy_current_state = yy_get_previous_state();

			yy_cp = yy_c_buf_p;
			yy_bp = (unsigned char *)yytext + YY_MORE_ADJ;
			goto yy_match;

		    case EOB_ACT_LAST_MATCH:
			yy_c_buf_p =
			    &yy_current_buffer->yy_ch_buf[yy_n_chars];

			yy_current_state = yy_get_previous_state();

			yy_cp = yy_c_buf_p;
			yy_bp = (unsigned char *)yytext + YY_MORE_ADJ;
			goto yy_find_action;
		    }
		break;
		}

	    default:
#ifdef FLEX_DEBUG
		printf( "action # %d\n", yy_act );
#endif
		YY_FATAL_ERROR(
			"fatal flex scanner internal error--no action found" );
	    }
	}
    }


/* yy_get_next_buffer - try to read in a new buffer
 *
 * synopsis
 *     int yy_get_next_buffer();
 *     
 * returns a code representing an action
 *     EOB_ACT_LAST_MATCH - 
 *     EOB_ACT_CONTINUE_SCAN - continue scanning from current position
 *     EOB_ACT_END_OF_FILE - end of file
 */

static int yy_get_next_buffer()

    {
    register YY_CHAR *dest = yy_current_buffer->yy_ch_buf;
    register YY_CHAR *source = (unsigned char *)yytext - 1; /* copy prev. char, too */
    register int number_to_move, i;
    int ret_val;

    if ( yy_c_buf_p > &yy_current_buffer->yy_ch_buf[yy_n_chars + 1] )
	YY_FATAL_ERROR(
		"fatal flex scanner internal error--end of buffer missed" );

    /* try to read more data */

    /* first move last chars to start of buffer */
    number_to_move = yy_c_buf_p - (unsigned char *)yytext;

    for ( i = 0; i < number_to_move; ++i )
	*(dest++) = *(source++);

    if ( yy_current_buffer->yy_eof_status != EOF_NOT_SEEN )
	/* don't do the read, it's not guaranteed to return an EOF,
	 * just force an EOF
	 */
	yy_n_chars = 0;

    else
	{
	int num_to_read = yy_current_buffer->yy_buf_size - number_to_move - 1;

	if ( num_to_read > YY_READ_BUF_SIZE )
	    num_to_read = YY_READ_BUF_SIZE;

	else if ( num_to_read <= 0 )
	    YY_FATAL_ERROR( "fatal error - scanner input buffer overflow" );

	/* read in more data */
	YY_INPUT( (&yy_current_buffer->yy_ch_buf[number_to_move]),
		  yy_n_chars, num_to_read );
	}

    if ( yy_n_chars == 0 )
	{
	if ( number_to_move == 1 )
	    {
	    ret_val = EOB_ACT_END_OF_FILE;
	    yy_current_buffer->yy_eof_status = EOF_DONE;
	    }

	else
	    {
	    ret_val = EOB_ACT_LAST_MATCH;
	    yy_current_buffer->yy_eof_status = EOF_PENDING;
	    }
	}

    else
	ret_val = EOB_ACT_CONTINUE_SCAN;

    yy_n_chars += number_to_move;
    yy_current_buffer->yy_ch_buf[yy_n_chars] = YY_END_OF_BUFFER_CHAR;
    yy_current_buffer->yy_ch_buf[yy_n_chars + 1] = YY_END_OF_BUFFER_CHAR;

    /* yytext begins at the second character in yy_ch_buf; the first
     * character is the one which preceded it before reading in the latest
     * buffer; it needs to be kept around in case it's a newline, so
     * yy_get_previous_state() will have with '^' rules active
     */

    yytext = (char *) &yy_current_buffer->yy_ch_buf[1];

    return ( ret_val );
    }


/* yy_get_previous_state - get the state just before the EOB char was reached
 *
 * synopsis
 *     yy_state_type yy_get_previous_state();
 */

static yy_state_type yy_get_previous_state()

    {
    register yy_state_type yy_current_state;
    register YY_CHAR *yy_cp;

    yy_current_state = yy_start;

    for ( yy_cp = (unsigned char *)yytext + YY_MORE_ADJ; yy_cp < yy_c_buf_p; ++yy_cp )
	{
	register YY_CHAR yy_c = (*yy_cp ? yy_ec[*yy_cp] : 1);
	if ( yy_accept[yy_current_state] )
	    {
	    yy_last_accepting_state = yy_current_state;
	    yy_last_accepting_cpos = yy_cp;
	    }
	while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
	    {
	    yy_current_state = yy_def[yy_current_state];
	    if ( yy_current_state >= 415 )
		yy_c = yy_meta[yy_c];
	    }
	yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
	}

    return ( yy_current_state );
    }


/* yy_try_NUL_trans - try to make a transition on the NUL character
 *
 * synopsis
 *     next_state = yy_try_NUL_trans( current_state );
 */

#ifdef YY_USE_PROTOS
static yy_state_type yy_try_NUL_trans( register yy_state_type yy_current_state )
#else
static yy_state_type yy_try_NUL_trans( yy_current_state )
register yy_state_type yy_current_state;
#endif

    {
    register int yy_is_jam;
    register YY_CHAR *yy_cp = yy_c_buf_p;

    register YY_CHAR yy_c = 1;
    if ( yy_accept[yy_current_state] )
	{
	yy_last_accepting_state = yy_current_state;
	yy_last_accepting_cpos = yy_cp;
	}
    while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
	{
	yy_current_state = yy_def[yy_current_state];
	if ( yy_current_state >= 415 )
	    yy_c = yy_meta[yy_c];
	}
    yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
    yy_is_jam = (yy_current_state == 414);

    return ( yy_is_jam ? 0 : yy_current_state );
    }


#ifdef YY_USE_PROTOS
static void yyunput( YY_CHAR c, register YY_CHAR *yy_bp )
#else
static void yyunput( c, yy_bp )
YY_CHAR c;
register YY_CHAR *yy_bp;
#endif

    {
    register YY_CHAR *yy_cp = yy_c_buf_p;

    /* undo effects of setting up yytext */
    *yy_cp = yy_hold_char;

    if ( yy_cp < yy_current_buffer->yy_ch_buf + 2 )
	{ /* need to shift things up to make room */
	register int number_to_move = yy_n_chars + 2; /* +2 for EOB chars */
	register YY_CHAR *dest =
	    &yy_current_buffer->yy_ch_buf[yy_current_buffer->yy_buf_size + 2];
	register YY_CHAR *source =
	    &yy_current_buffer->yy_ch_buf[number_to_move];

	while ( source > yy_current_buffer->yy_ch_buf )
	    *--dest = *--source;

	yy_cp += dest - source;
	yy_bp += dest - source;
	yy_n_chars = yy_current_buffer->yy_buf_size;

	if ( yy_cp < yy_current_buffer->yy_ch_buf + 2 )
	    YY_FATAL_ERROR( "flex scanner push-back overflow" );
	}

    if ( yy_cp > yy_bp && yy_cp[-1] == '\n' )
	yy_cp[-2] = '\n';

    *--yy_cp = c;

    /* note: the formal parameter *must* be called "yy_bp" for this
     *       macro to now work correctly
     */
    YY_DO_BEFORE_ACTION; /* set up yytext again */
    }


#ifdef __cplusplus
static int yyinput()
#else
static int input()
#endif

    {
    int c;
    YY_CHAR *yy_cp = yy_c_buf_p;

    *yy_cp = yy_hold_char;

    if ( *yy_c_buf_p == YY_END_OF_BUFFER_CHAR )
	{
	/* yy_c_buf_p now points to the character we want to return.
	 * If this occurs *before* the EOB characters, then it's a
	 * valid NUL; if not, then we've hit the end of the buffer.
	 */
	if ( yy_c_buf_p < &yy_current_buffer->yy_ch_buf[yy_n_chars] )
	    /* this was really a NUL */
	    *yy_c_buf_p = '\0';

	else
	    { /* need more input */
	    yytext = (char *) yy_c_buf_p;
	    ++yy_c_buf_p;

	    switch ( yy_get_next_buffer() )
		{
		case EOB_ACT_END_OF_FILE:
		    {
		    if ( yywrap() )
			{
			yy_c_buf_p = (unsigned char *)yytext + YY_MORE_ADJ;
			return ( EOF );
			}

		    YY_NEW_FILE;

#ifdef __cplusplus
		    return ( yyinput() );
#else
		    return ( input() );
#endif
		    }
		    break;

		case EOB_ACT_CONTINUE_SCAN:
		    yy_c_buf_p = (unsigned char *)yytext + YY_MORE_ADJ;
		    break;

		case EOB_ACT_LAST_MATCH:
#ifdef __cplusplus
		    YY_FATAL_ERROR( "unexpected last match in yyinput()" );
#else
		    YY_FATAL_ERROR( "unexpected last match in input()" );
#endif
		}
	    }
	}

    c = *yy_c_buf_p;
    yy_hold_char = *++yy_c_buf_p;

    return ( c );
    }


#ifdef YY_USE_PROTOS
void yyrestart( FILE *input_file )
#else
void yyrestart( input_file )
FILE *input_file;
#endif

    {
    yy_init_buffer( yy_current_buffer, input_file );
    yy_load_buffer_state();
    }


#ifdef YY_USE_PROTOS
void yy_switch_to_buffer( YY_BUFFER_STATE new_buffer )
#else
void yy_switch_to_buffer( new_buffer )
YY_BUFFER_STATE new_buffer;
#endif

    {
    if ( yy_current_buffer == new_buffer )
	return;

    if ( yy_current_buffer )
	{
	/* flush out information for old buffer */
	*yy_c_buf_p = yy_hold_char;
	yy_current_buffer->yy_buf_pos = yy_c_buf_p;
	yy_current_buffer->yy_n_chars = yy_n_chars;
	}

    yy_current_buffer = new_buffer;
    yy_load_buffer_state();

    /* we don't actually know whether we did this switch during
     * EOF (yywrap()) processing, but the only time this flag
     * is looked at is after yywrap() is called, so it's safe
     * to go ahead and always set it.
     */
    yy_did_buffer_switch_on_eof = 1;
    }


#ifdef YY_USE_PROTOS
void yy_load_buffer_state( void )
#else
void yy_load_buffer_state()
#endif

    {
    yy_n_chars = yy_current_buffer->yy_n_chars;
    yytext = (char *) ( yy_c_buf_p = yy_current_buffer->yy_buf_pos);
    yyin = yy_current_buffer->yy_input_file;
    yy_hold_char = *yy_c_buf_p;
    }


#ifdef YY_USE_PROTOS
YY_BUFFER_STATE yy_create_buffer( FILE *file, int size )
#else
YY_BUFFER_STATE yy_create_buffer( file, size )
FILE *file;
int size;
#endif

    {
    YY_BUFFER_STATE b;

    b = (YY_BUFFER_STATE) malloc( sizeof( struct yy_buffer_state ) );

    if ( ! b )
	YY_FATAL_ERROR( "out of dynamic memory in yy_create_buffer()" );

    b->yy_buf_size = size;

    /* yy_ch_buf has to be 2 characters longer than the size given because
     * we need to put in 2 end-of-buffer characters.
     */
    b->yy_ch_buf = (YY_CHAR *) malloc( (unsigned) (b->yy_buf_size + 2) );

    if ( ! b->yy_ch_buf )
	YY_FATAL_ERROR( "out of dynamic memory in yy_create_buffer()" );

    yy_init_buffer( b, file );

    return ( b );
    }


#ifdef YY_USE_PROTOS
void yy_delete_buffer( YY_BUFFER_STATE b )
#else
void yy_delete_buffer( b )
YY_BUFFER_STATE b;
#endif

    {
    if ( b == yy_current_buffer )
	yy_current_buffer = (YY_BUFFER_STATE) 0;

    free( (char *) b->yy_ch_buf );
    free( (char *) b );
    }


#ifdef YY_USE_PROTOS
void yy_init_buffer( YY_BUFFER_STATE b, FILE *file )
#else
void yy_init_buffer( b, file )
YY_BUFFER_STATE b;
FILE *file;
#endif

    {
    b->yy_input_file = file;

    /* we put in the '\n' and start reading from [1] so that an
     * initial match-at-newline will be true.
     */

    b->yy_ch_buf[0] = '\n';
    b->yy_n_chars = 1;

    /* we always need two end-of-buffer characters.  The first causes
     * a transition to the end-of-buffer state.  The second causes
     * a jam in that state.
     */
    b->yy_ch_buf[1] = YY_END_OF_BUFFER_CHAR;
    b->yy_ch_buf[2] = YY_END_OF_BUFFER_CHAR;

    b->yy_buf_pos = &b->yy_ch_buf[1];

    b->yy_eof_status = EOF_NOT_SEEN;
    }
# line 157 "scn1.l"



yyerror(char *s)
{
	printf("%d: %s at %s\n", lineno, s, yytext);
}

main(int ac, char **av)
{
	if(ac > 1 && (yyin = fopen(av[1], "r")) == NULL) {
		perror(av[1]);
		exit(1);
	}

	if(!yyparse())
		printf("SQL parse worked\n");
	else
		printf("SQL parse failed\n");
} /* main */
