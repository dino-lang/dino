%token AND
%token ARRAY
%token _BEGIN
%token BY
%token CASE
%token CONST
%token DEFINITION
%token DIV
%token DO
%token ELSE
%token ELSIF
%token END
%token EXIT
%token EXPORT
%token FOR
%token FROM
%token IF
%token IMPLEMENTATION
%token IMPORT
%token IN
%token LOOP
%token MOD
%token MODULE
%token NOT
%token OF
%token OR
%token POINTER
%token PROCEDURE
%token QUALIFIED
%token RECORD
%token REPEAT
%token RETURN
%token SET
%token THEN
%token TO
%token TYPE
%token UNTIL
%token VAR
%token WHILE
%token WITH
%token ampersand
%token asterisk
%token colon
%token colon_equals
%token comma
%token dot
%token dot
%token dot_dot
%token end_of_file
%token equals_sign
%token greater_than
%token greater_than_or_equals
%token identifier
%token integer
%token left_curley_bracket
%token left_parenthesis
%token left_square_bracket
%token left_square_bracket
%token less_than
%token less_than_or_equals
%token minus
%token not_equals
%token plus
%token pound_sign
%token real
%token right_curley_bracket
%token right_parenthesis
%token right_square_bracket
%token semicolon
%token slash
%token string
%token up_arrow
%token vertical_bar
%%
compilation : compilation_unit_list ;
compilation_unit_list : compilation_unit | compilation_unit_list
                        compilation_unit;
compilation_unit : module;
module : program_module | definition_module | implementation_module;
file_ident : identifier;
program_module : MODULE file_ident priority_opt semicolon
                 import_list_opt block ident dot;
definition_module : DEFINITION MODULE file_ident semicolon
                    import_list_opt export_opt definition_list_opt
                    END ident dot;
implementation_module : IMPLEMENTATION program_module;
priority_opt : | priority;
priority : left_square_bracket const_expression right_square_bracket;
const_expression : expression;
relation : equals_sign | pound_sign | not_equals | less_than |
           less_than_or_equals | greater_than | greater_than_or_equals
           | IN;
import_list_opt : | import_list;
import_list : import | import_list import;
import : from_opt IMPORT ident_list semicolon;
from_opt : | from;
from : FROM ident;
ident_list : ident | ident_list comma ident;
ident : identifier;
block : declaration_list_opt begin_and_stmts_opt END;
declaration_list_opt : | declaration_list;
declaration_list : declaration | declaration_list declaration;
declaration : error semicolon |
              CONST constant_declaration_list_opt |
              TYPE type_declaration_list_opt |
              VAR variable_declaration_list_opt |
              procedure_declaration semicolon|
              module_declaration semicolon;
begin_and_stmts_opt : | begin_and_stmts;
begin_and_stmts : _BEGIN statement_list;
export_opt : | export;
export : EXPORT qualified_opt ident_list semicolon;
qualified_opt : | qualified;
qualified : QUALIFIED;
definition_list_opt : | definition_list;
definition_list : definition | definition_list definition;
definition : CONST constant_declaration_list_opt |
             TYPE type_definition_list_opt |
             VAR variable_declaration_list_opt |
             procedure_heading semicolon;
type_declaration_list_opt : | type_declaration_list;
type_declaration_list : type_declaration | type_declaration_list
                        type_declaration;
type_declaration : ident equals_sign type semicolon;
constant_declaration_list_opt : | constant_declaration_list;
constant_declaration_list : constant_declaration | constant_declaration_list
                        constant_declaration;
constant_declaration : ident equals_sign const_expression semicolon;
type_definition_list_opt : | type_definition_list;
type_definition_list : type_definition | type_definition_list
                       type_definition;
type_definition : ident type_definition_opt semicolon;
type_definition_opt : | type_definition;
type_definition : equals_sign type;
variable_declaration_list_opt : | variable_declaration_list;
variable_declaration_list : variable_declaration | variable_declaration_list
                        variable_declaration;
variable_declaration : field_declaration semicolon;
field_declaration : ident_list colon type;
proc_ident : ident;
procedure_heading : PROCEDURE proc_ident formal_parameters_opt function_opt;
procedure_declaration : procedure_heading semicolon block ident;
module_declaration : MODULE ident priority_opt semicolon import_list_opt
                     export_opt block ident;
type : simple_type | array_type | record_type | set_type | pointer_type
       procedure_type;
formal_parameters_opt : | formal_parameters;
formal_parameters : left_parenthesis fp_section_list_opt right_parenthesis;
fp_section_list_opt : | fp_section_list;
fp_section_list : fp_section | fp_section_list semicolon fp_section;
fp_section : var_opt ident_list colon formal_type;
function_opt : | function;
function : colon qualident;
var_opt : | var;
var : VAR;
formal_type : array_opt qualident;
qualident : designator;
array_opt : | array;
array : ARRAY OF;
simple_type : qualident | enumeration | subrange_type;
enumeration : left_parenthesis ident_list right_parenthesis;
subrange_type : left_square_bracket subrange right_square_bracket;
subrange : const_expression dot_dot const_expression;
array_type : ARRAY simple_type_list OF type;
simple_type_list : simple_type | simple_type_list comma simple_type;
record_type : RECORD field_list END;
field_list : field | field_list semicolon field;
field :  | field_declaration | variant_field;
variant_field : CASE tag OF variant_list variant_else_opt END;
tag : ident colon qualident | ident ref_list_opt;
variant_list : variant | variant_list vertical_bar variant;
variant : case_label_list colon field_list;
case_label_list : case_label | case_label_list comma case_label;
case_label : const_expression | subrange;
variant_else_opt : | variant_else;
variant_else : ELSE field_list;
set_type : SET OF simple_type;
pointer_type : POINTER TO type;
procedure_type : PROCEDURE formal_type_list_opt function_opt;
formal_type_list_opt : | formal_type_list;
formal_type_list : left_parenthesis ft_list_opt right_parenthesis;
ft_list_opt : | ft_list;
ft_list : ft | ft_list comma ft;
ft : var_opt formal_type;
statement_list : statement | statement_list semicolon statement;
statement :  | assignment | procedure_call | if_statement |
            case_statement | while_statement | repeat_statement |
            loop_statement | for_statement | with_statement | EXIT |
            RETURN expression_opt;
expression_opt : | expression;
assignment : out_designator out_colon_equals expression;
out_designator : designator;
out_colon_equals : colon_equals;
designator : ident ref_list_opt;
ref_list_opt : | ref_list;
ref_list : ref | ref_list ref;
ref : dot ident | left_square_bracket exp_list right_square_bracket |
      up_arrow;
exp_list_opt : | exp_list;
exp_list : expression | exp_list comma expression;
procedure_call : designator actual_parameters_opt;
actual_parameters_opt : | actual_parameters;
actual_parameters : left_parenthesis exp_list_opt right_parenthesis;
if_statement : IF expression THEN statement_list elsif_list_opt
               else_opt END;
elsif_list_opt : | elsif_list;
elsif_list : elsif | elsif_list elsif;
elsif : ELSIF expression THEN statement_list;
else_opt : | else;
else : ELSE statement_list;
case_statement : CASE expression OF case_list else_opt END;
case_list : case | case_list vertical_bar case;
case : case_label_list colon statement_list;
while_statement : WHILE expression DO statement_list END;
repeat_statement : REPEAT statement_list UNTIL expression;
loop_statement : LOOP statement_list END;
for_statement : FOR ident colon_equals expression TO expression by_opt
                DO statement_list END;
by_opt : | by;
by : BY const_expression;
with_statement : WITH designator DO statement_list END;
expression : simple_expression | simple_expression relation simple_expression;
simple_expression : sign_opt term | simple_expression add_operator term;
add_operator : plus | minus | OR;
sign_opt : | sign;
sign : plus | minus;
term : factor | term mul_operator factor;
mul_operator : asterisk | slash | DIV | MOD | AND | ampersand;
factor : number | string | set | designator actual_parameters_opt |
         left_parenthesis expression right_parenthesis | NOT factor;
number : integer | real;
set : qualident_opt left_curley_bracket element_list_opt
      right_curley_bracket;
qualident_opt : | qualident;
element_list_opt :  | element_list;
element_list : element | element_list comma element;
element : case_label;
%%
#include <stdio.h>
char *filename="-";

int yynerrs;
int yylineno;

main(argc, argv)
int argc;
char *argv[];
{
  register int rc=0;
  extern int yynerrs;
  extern int yylineno;
  if (argc <= 1)
      yyparse();
  else
     {
       while (argc > 1)
       { if (freopen(argv[1], "r", stdin)==NULL) {
             fprintf(stderr, "m2c: %s: cannot open\n", argv[1]);
             rc++; }
         else {
             filename=argv[1];
             yylineno=1;
             yyparse(); }
         argc--; argv++;
       }
      }
   if (yynerrs > 0) rc++;
   return(rc);
}


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
# line 1 "m2cm.l"
#define INITIAL 0
# line 1 "m2cm.l"

/* done after the current pattern has been matched and before the
 * corresponding action - sets up yytext
 */
#define YY_DO_BEFORE_ACTION \
	yytext = (char *) yy_bp; \
	yyleng = yy_cp - yy_bp; \
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

#define YY_END_OF_BUFFER 78
typedef int yy_state_type;
static const short int yy_accept[209] =
    {   0,
        0,    0,   78,   77,    1,   77,   60,   42,   77,   53,
       62,   43,   59,   46,   58,   48,   65,   76,   76,   45,
       64,   57,   49,   51,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   54,   63,   66,   52,   67,   61,
        0,   68,    0,   69,   47,   72,   76,   76,    0,   73,
       74,   75,   44,   55,   56,   50,   70,   70,   70,   70,
        5,   70,   70,   70,   70,   10,   70,   70,   70,   70,
       70,   18,   70,   21,   70,   70,   70,   26,   27,   70,
       70,   70,   70,   70,   70,   36,   70,   70,   70,   70,

       70,   72,    0,    2,   70,   70,   70,   70,   70,    9,
       70,   13,   70,   70,   16,   70,   70,   70,   24,   25,
       70,   70,   70,   70,   70,   70,   34,   70,   70,   70,
       39,   70,   70,    0,   70,   70,    6,   70,   70,   11,
       70,   14,   70,   17,   70,   70,   22,   70,   70,   70,
       70,   70,   70,   70,   35,   37,   70,   70,   41,   71,
        3,    4,    7,   70,   12,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   38,   40,   70,   15,   70,
       20,   23,   70,   70,   70,   31,   32,   33,   70,   70,
       28,   70,   70,   70,   70,   70,   70,   70,   70,   29,

       30,    8,   70,   70,   70,   70,   19,    0
    } ;

static const YY_CHAR yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    1,    4,    5,    1,    1,    6,    7,    8,
        9,   10,   11,   12,   13,   14,   15,   16,   16,   16,
       16,   16,   16,   16,   16,   17,   17,   18,   19,   20,
       21,   22,    1,    1,   23,   24,   25,   26,   27,   28,
       29,   30,   31,   32,   32,   33,   34,   35,   36,   37,
       38,   39,   40,   41,   42,   43,   44,   45,   46,   32,
       47,    1,   48,   49,    1,    1,   32,   32,   32,   32,

       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
       32,   32,   50,   51,   52,    1,    1,    1,    1,    1,
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

static const YY_CHAR yy_meta[53] =
    {   0,
        1,    1,    2,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    3,    3,    1,    1,    1,
        1,    1,    3,    3,    3,    3,    3,    3,    3,    3,
        3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
        3,    3,    3,    3,    3,    3,    1,    1,    1,    1,
        1,    1
    } ;

static const short int yy_base[212] =
    {   0,
        0,    0,  231,  232,  232,  226,  232,  232,  222,  232,
      232,  232,  232,  232,  232,  214,  232,   39,   52,  206,
      232,   36,  232,  205,   35,   27,   36,   44,   46,   46,
        0,   55,  189,  188,  187,   56,   57,  180,  194,  193,
       56,  184,  195,   30,  232,  232,  232,  232,  232,  232,
      213,  232,  209,  232,  232,   71,    0,    0,   83,    0,
        0,  232,  232,  232,  232,  232,    0,  189,  175,  184,
        0,  172,  176,  182,  166,    0,  168,  181,   70,  167,
      169,    0,  167,    0,  167,  176,  160,    0,    0,  169,
      163,  175,   69,  156,  169,    0,  158,  153,  154,  161,

      150,   87,   98,    0,  167,  158,  161,  147,  155,    0,
       81,    0,  144,  148,    0,  149,   80,  145,  139,    0,
      145,  154,  145,  141,  149,  133,    0,  139,  146,  141,
        0,  138,  140,  101,  123,  133,    0,  126,  131,    0,
      137,    0,  125,    0,  136,  123,    0,  128,  119,  132,
      127,  118,  133,  116,    0,    0,  121,  126,    0,  103,
        0,    0,    0,  121,    0,  110,  116,  108,  121,  120,
      120,  117,  118,  102,  107,    0,    0,  100,    0,  113,
        0,    0,  100,   96,  106,    0,    0,    0,  105,  100,
        0,   95,  106,   96,   90,  103,  103,   93,  104,    0,

        0,    0,   84,   91,   79,   70,    0,  232,  120,  123,
       75
    } ;

static const short int yy_def[212] =
    {   0,
      208,    1,  208,  208,  208,  209,  208,  208,  210,  208,
      208,  208,  208,  208,  208,  208,  208,  208,   18,  208,
      208,  208,  208,  208,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  208,  208,  208,  208,  208,  208,
      209,  208,  210,  208,  208,  208,   18,   19,   19,   59,
       59,  208,  208,  208,  208,  208,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,

      211,  208,  208,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  208,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  208,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,
      211,  211,  211,  211,  211,  211,  211,  211,  211,  211,

      211,  211,  211,  211,  211,  211,  211,    0,  208,  208,
      208
    } ;

static const short int yy_nxt[285] =
    {   0,
        4,    5,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,   19,   20,   21,   22,
       23,   24,   25,   26,   27,   28,   29,   30,   31,   31,
       32,   31,   33,   34,   35,   36,   37,   38,   39,   40,
       41,   42,   43,   44,   31,   31,   45,   46,   47,   48,
       49,   50,   56,   70,   57,   58,   64,   65,   72,  100,
      101,   59,   60,   61,   59,   59,   59,   58,   62,   68,
       74,   73,   71,   69,   75,   59,   59,   67,   77,   76,
       78,   80,   82,   88,   81,   95,  102,  102,   83,   84,
       79,   96,   90,  124,   89,   91,  208,  103,   59,   59,

      113,   97,  102,  102,  207,  125,  114,  140,  134,  126,
      134,  141,  145,  103,  206,  146,  160,  160,  160,  160,
       51,  205,   51,   53,  204,   53,  203,  202,  201,  200,
      199,  198,  197,  196,  195,  194,  193,  192,  191,  190,
      189,  188,  187,  186,  185,  184,  183,  182,  181,  180,
      179,  178,  177,  176,  175,  174,  173,  172,  171,  170,
      169,  168,  167,  166,  165,  164,  163,  162,  161,  159,
      158,  157,  156,  155,  154,  153,  152,  151,  150,  149,
      148,  147,  144,  143,  142,  139,  138,  137,  136,  135,
      133,  132,  131,  130,  129,  128,  127,  123,  122,  121,

      120,  119,  118,  117,  116,  115,  112,  111,  110,  109,
      108,  107,  106,  105,  104,   54,   52,   99,   98,   94,
       93,   92,   87,   86,   85,   66,   63,   55,   54,   52,
      208,    3,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208
    } ;

static const short int yy_chk[285] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,   18,   26,   18,   18,   22,   22,   27,   44,
       44,   18,   18,   18,   18,   18,   18,   19,   18,   25,
       28,   27,   26,   25,   28,   19,   19,  211,   29,   28,
       29,   30,   32,   36,   30,   41,   56,   56,   32,   32,
       29,   41,   37,   93,   36,   37,   59,   56,   59,   59,

       79,   41,  102,  102,  206,   93,   79,  111,  103,   93,
      103,  111,  117,  102,  205,  117,  134,  134,  160,  160,
      209,  204,  209,  210,  203,  210,  199,  198,  197,  196,
      195,  194,  193,  192,  190,  189,  185,  184,  183,  180,
      178,  175,  174,  173,  172,  171,  170,  169,  168,  167,
      166,  164,  158,  157,  154,  153,  152,  151,  150,  149,
      148,  146,  145,  143,  141,  139,  138,  136,  135,  133,
      132,  130,  129,  128,  126,  125,  124,  123,  122,  121,
      119,  118,  116,  114,  113,  109,  108,  107,  106,  105,
      101,  100,   99,   98,   97,   95,   94,   92,   91,   90,

       87,   86,   85,   83,   81,   80,   78,   77,   75,   74,
       73,   72,   70,   69,   68,   53,   51,   43,   42,   40,
       39,   38,   35,   34,   33,   24,   20,   16,    9,    6,
        3,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208
    } ;

static yy_state_type yy_last_accepting_state;
static YY_CHAR *yy_last_accepting_cpos;

/* the intent behind this definition is that it'll catch
 * any uses of REJECT which flex missed
 */
#define REJECT reject_used_but_not_detected
#define yymore() yymore_used_but_not_detected
#define YY_MORE_ADJ 0

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
		if ( yy_current_state >= 209 )
		    yy_c = yy_meta[yy_c];
		}
	    yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
	    ++yy_cp;
	    }
	while ( yy_current_state != 208 );
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

case 1:
# line 2 "m2cm.l"
{ ; } /* ignore white space: blanks, newlines & tabs */
	YY_BREAK
case 2:
# line 3 "m2cm.l"
{ return( AND );}
	YY_BREAK
case 3:
# line 4 "m2cm.l"
{ return( ARRAY );}
	YY_BREAK
case 4:
# line 5 "m2cm.l"
{ return( _BEGIN );}
	YY_BREAK
case 5:
# line 6 "m2cm.l"
{ return( BY );}
	YY_BREAK
case 6:
# line 7 "m2cm.l"
{ return( CASE );}
	YY_BREAK
case 7:
# line 8 "m2cm.l"
{ return( CONST );}
	YY_BREAK
case 8:
# line 9 "m2cm.l"
{ return( DEFINITION );}
	YY_BREAK
case 9:
# line 10 "m2cm.l"
{ return( DIV );}
	YY_BREAK
case 10:
# line 11 "m2cm.l"
{ return( DO );}
	YY_BREAK
case 11:
# line 12 "m2cm.l"
{ return( ELSE );}
	YY_BREAK
case 12:
# line 13 "m2cm.l"
{ return( ELSIF );}
	YY_BREAK
case 13:
# line 14 "m2cm.l"
{ return( END );}
	YY_BREAK
case 14:
# line 15 "m2cm.l"
{ return( EXIT );}
	YY_BREAK
case 15:
# line 16 "m2cm.l"
{ return( EXPORT );}
	YY_BREAK
case 16:
# line 17 "m2cm.l"
{ return( FOR );}
	YY_BREAK
case 17:
# line 18 "m2cm.l"
{ return( FROM );}
	YY_BREAK
case 18:
# line 19 "m2cm.l"
{ return( IF );}
	YY_BREAK
case 19:
# line 20 "m2cm.l"
{ return( IMPLEMENTATION );}
	YY_BREAK
case 20:
# line 21 "m2cm.l"
{ return( IMPORT );}
	YY_BREAK
case 21:
# line 22 "m2cm.l"
{ return( IN );}
	YY_BREAK
case 22:
# line 23 "m2cm.l"
{ return( LOOP );}
	YY_BREAK
case 23:
# line 24 "m2cm.l"
{ return( MODULE );}
	YY_BREAK
case 24:
# line 25 "m2cm.l"
{ return( MOD );}
	YY_BREAK
case 25:
# line 26 "m2cm.l"
{ return( NOT );}
	YY_BREAK
case 26:
# line 27 "m2cm.l"
{ return( OF );}
	YY_BREAK
case 27:
# line 28 "m2cm.l"
{ return( OR );}
	YY_BREAK
case 28:
# line 29 "m2cm.l"
{ return( POINTER );}
	YY_BREAK
case 29:
# line 30 "m2cm.l"
{ return( PROCEDURE );}
	YY_BREAK
case 30:
# line 31 "m2cm.l"
{ return( QUALIFIED );}
	YY_BREAK
case 31:
# line 32 "m2cm.l"
{ return( RECORD );}
	YY_BREAK
case 32:
# line 33 "m2cm.l"
{ return( REPEAT );}
	YY_BREAK
case 33:
# line 34 "m2cm.l"
{ return( RETURN );}
	YY_BREAK
case 34:
# line 35 "m2cm.l"
{ return( SET );}
	YY_BREAK
case 35:
# line 36 "m2cm.l"
{ return( THEN );}
	YY_BREAK
case 36:
# line 37 "m2cm.l"
{ return( TO );}
	YY_BREAK
case 37:
# line 38 "m2cm.l"
{ return( TYPE );}
	YY_BREAK
case 38:
# line 39 "m2cm.l"
{ return( UNTIL );}
	YY_BREAK
case 39:
# line 40 "m2cm.l"
{ return( VAR );}
	YY_BREAK
case 40:
# line 41 "m2cm.l"
{ return( WHILE );}
	YY_BREAK
case 41:
# line 42 "m2cm.l"
{ return( WITH );}
	YY_BREAK
case 42:
# line 43 "m2cm.l"
{ return( ampersand );}
	YY_BREAK
case 43:
# line 44 "m2cm.l"
{ return( asterisk );}
	YY_BREAK
case 44:
# line 45 "m2cm.l"
{ return( colon_equals );}
	YY_BREAK
case 45:
# line 46 "m2cm.l"
{ return( colon );}
	YY_BREAK
case 46:
# line 47 "m2cm.l"
{ return( comma );}
	YY_BREAK
case 47:
# line 48 "m2cm.l"
{ return( dot_dot );}
	YY_BREAK
case 48:
# line 49 "m2cm.l"
{ return( dot );}
	YY_BREAK
case 49:
# line 50 "m2cm.l"
{ return( equals_sign );}
	YY_BREAK
case 50:
# line 51 "m2cm.l"
{ return( greater_than_or_equals );}
	YY_BREAK
case 51:
# line 52 "m2cm.l"
{ return( greater_than );}
	YY_BREAK
case 52:
# line 53 "m2cm.l"
{ return( left_curley_bracket );}
	YY_BREAK
case 53:
# line 54 "m2cm.l"
{ return( left_parenthesis );}
	YY_BREAK
case 54:
# line 55 "m2cm.l"
{ return( left_square_bracket );}
	YY_BREAK
case 55:
# line 56 "m2cm.l"
{ return( less_than_or_equals );}
	YY_BREAK
case 56:
# line 57 "m2cm.l"
{ return( not_equals );}
	YY_BREAK
case 57:
# line 58 "m2cm.l"
{ return( less_than );}
	YY_BREAK
case 58:
# line 59 "m2cm.l"
{ return( minus );}
	YY_BREAK
case 59:
# line 60 "m2cm.l"
{ return( plus );}
	YY_BREAK
case 60:
# line 61 "m2cm.l"
{ return( pound_sign );}
	YY_BREAK
case 61:
# line 62 "m2cm.l"
{ return( right_curley_bracket );}
	YY_BREAK
case 62:
# line 63 "m2cm.l"
{ return( right_parenthesis );}
	YY_BREAK
case 63:
# line 64 "m2cm.l"
{ return( right_square_bracket );}
	YY_BREAK
case 64:
# line 65 "m2cm.l"
{ return( semicolon );}
	YY_BREAK
case 65:
# line 66 "m2cm.l"
{ return( slash );}
	YY_BREAK
case 66:
# line 67 "m2cm.l"
{ return( up_arrow );}
	YY_BREAK
case 67:
# line 68 "m2cm.l"
{ return( vertical_bar );}
	YY_BREAK
case 68:
# line 69 "m2cm.l"
{ return( string );}
	YY_BREAK
case 69:
# line 70 "m2cm.l"
{ return( string );}
	YY_BREAK
case 70:
# line 71 "m2cm.l"
{ return( identifier ); }
	YY_BREAK
case 71:
# line 72 "m2cm.l"
{ return( real );}
	YY_BREAK
case 72:
# line 73 "m2cm.l"
{ return( real );}
	YY_BREAK
case 73:
# line 74 "m2cm.l"
{ return( integer );}
	YY_BREAK
case 74:
# line 75 "m2cm.l"
{ return( integer );}
	YY_BREAK
case 75:
# line 76 "m2cm.l"
{ return( integer );}
	YY_BREAK
case 76:
# line 77 "m2cm.l"
{ return( integer );}
	YY_BREAK
case 77:
# line 78 "m2cm.l"
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
	    if ( yy_current_state >= 209 )
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
	if ( yy_current_state >= 209 )
	    yy_c = yy_meta[yy_c];
	}
    yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
    yy_is_jam = (yy_current_state == 208);

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
# line 78 "m2cm.l"

yyerror(s)
 char *s;
{
 fprintf(stderr, "\"%s\", line %d: %s\n",
         filename, yylineno, s);
}
