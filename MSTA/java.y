%{
/*------------------------------------------------------------------
 * Copyright (C) 1996 Dmitri Bronnikov, All rights reserved.
 *
 * THIS GRAMMAR IS PROVIDED "AS IS" WITHOUT  ANY  EXPRESS  OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR
 * PURPOSE, OR NON-INFRINGMENT.
 *
 * This grammar can be used by anyone without any restrictions.
 *
 * Dmitri Bronnikov
 * 21485 Green Hill Rd
 * Farmington Hills, MI, 48335
 * Bronikov@ic.net
 *
 *------------------------------------------------------------------
 *
 * VERSION 1.04 DATE 11 JULY 1996
 *
 *------------------------------------------------------------------
 *
 * PARSING CONFLICTS RESOLVED
 *
 * Some Shift/Reduce conflicts have been resolved at the expense of
 * the grammar defines a superset of the language. The following
 * actions have to be performed to complete program syntax checking:
 *
 * 1) Check that modifiers applied to a class, interface, field,
 *    or constructor are allowed in respectively a class, inteface,
 *    field or constructor declaration. For example, a class
 *    declaration should not allow other modifiers than abstract,
 *    final and public.
 *
 * 2) For an expression statement, check it is either increment, or
 *    decrement, or assignment expression.
 *
 * 3) Check that type expression in a cast operator indicates a type.
 *
 * 4) '[' optionally followed by any number of white-space characters
 *    immediately followed by ']' should be either defined in lexer,
 *    or intercepted and reaplced by OP_DIM token.
 *
 *    OP_DIM [\[]{white_space}*[\]]
 *
 *------------------------------------------------------------------
 *
 * CONFLICTS REPORTED BY YACC
 *
 * The only conflict reported by YACC is the if-then-else shift/reduce
 * conflict which is traditionally (and intentionally) left unresolved
 *
 *------------------------------------------------------------------
 */
%}

%token ABSTRACT
%token BOOLEAN BREAK BYTE BYVALUE
%token CASE CAST CATCH CHAR CLASS CONST CONTINUE
%token DEFAULT DO DOUBLE
%token ELSE EXTENDS
%token FINAL FINALLY FLOAT FOR FUTURE
%token GENERIC GOTO
%token IF IMPLEMENTS IMPORT INNER INSTANCEOF INT INTERFACE
%token LONG
%token NATIVE NEW NULL
%token OPERATOR OUTER
%token PACKAGE PRIVATE PROTECTED PUBLIC
%token REST RETURN
%token SHORT STATIC SUPER SWITCH SYNCHRONIZED
%token THIS THROW THROWS TRANSIENT TRY
%token VAR VOID VOLATILE
%token WHILE
%token OP_INC OP_DEC
%token OP_SHL OP_SHR OP_SHRR
%token OP_GE OP_LE OP_EQ OP_NE
%token OP_LAND OP_LOR
%token OP_DIM
%token ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB
%token ASS_SHL ASS_SHR ASS_SHRR ASS_AND ASS_XOR ASS_OR
%token IDENTIFIER LITERAL

%start CompilationUnit

%%

TypeSpecifier
	: TypeName
	| TypeSpecifier OP_DIM
	;

TypeName
	: PrimitiveType
	| QualifiedName
	;

TypeNameList
	: TypeName
	| TypeNameList ',' TypeName
	;

PrimitiveType
	: BOOLEAN
	| CHAR
	| BYTE
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| VOID
	;

CompilationUnit
	: PackageStatement ImportStatements TypeDeclarations
	| PackageStatement ImportStatements
	| PackageStatement                  TypeDeclarations
	|                  ImportStatements TypeDeclarations
	| PackageStatement
	|                  ImportStatements
	|                                   TypeDeclarations
	;

PackageStatement
	: PACKAGE QualifiedName ';'
	;

TypeDeclarations
	: TypeDeclaration
	| TypeDeclarations TypeDeclaration
	;

TypeDeclaration
	: ClassDeclaration
	| InterfaceDeclaration
	;

ImportStatements
	: ImportStatement
	| ImportStatements ImportStatement
	;

ImportStatement
	: IMPORT QualifiedName ';'
	| IMPORT QualifiedName '.' '*' ';'
	;

QualifiedName
	: IDENTIFIER
	| QualifiedName '.' IDENTIFIER
	;

ClassDeclaration
	: Modifiers CLASS IDENTIFIER Super Interfaces ClassBody
	| Modifiers CLASS IDENTIFIER Super            ClassBody
	| Modifiers CLASS IDENTIFIER       Interfaces ClassBody
	|           CLASS IDENTIFIER Super Interfaces ClassBody
	| Modifiers CLASS IDENTIFIER                  ClassBody
	|           CLASS IDENTIFIER Super            ClassBody
	|           CLASS IDENTIFIER       Interfaces ClassBody
	|           CLASS IDENTIFIER                  ClassBody
	;

Modifiers
	: Modifier
	| Modifiers Modifier
	;

Modifier
	: ABSTRACT
	| FINAL
	| PUBLIC
	| PROTECTED
	| PRIVATE
	| STATIC
	| TRANSIENT
	| VOLATILE
	| NATIVE
	| SYNCHRONIZED
	;

Super
	: EXTENDS TypeNameList
	;

Interfaces
	: IMPLEMENTS TypeNameList
	;

ClassBody
	: '{' FieldDeclarations '}'
	| '{' '}'
	;

FieldDeclarations
	: FieldDeclaration
        | FieldDeclarations FieldDeclaration
        ;

FieldDeclaration
	: FieldVariableDeclaration ';'
	| MethodDeclaration
	| ConstructorDeclaration
	| StaticInitializer
	;

FieldVariableDeclaration
	: Modifiers TypeSpecifier VariableDeclarators
	|           TypeSpecifier VariableDeclarators
	;

VariableDeclarators
	: VariableDeclarator
	| VariableDeclarators ',' VariableDeclarator
	;

VariableDeclarator
	: DeclaratorName
	| DeclaratorName '=' VariableInitializer
	;

VariableInitializer
	: Expression
	| '{' '}'
        | '{' ArrayInitializers '}'
        ;

ArrayInitializers
	: VariableInitializer
	| ArrayInitializers ',' VariableInitializer
	| ArrayInitializers ','
	;

MethodDeclaration
	: Modifiers TypeSpecifier MethodDeclarator Throws MethodBody
	| Modifiers TypeSpecifier MethodDeclarator        MethodBody
	|           TypeSpecifier MethodDeclarator Throws MethodBody
	|           TypeSpecifier MethodDeclarator        MethodBody
	;

MethodDeclarator
	: DeclaratorName '(' ParameterList ')'
	| DeclaratorName '(' ')'
	| MethodDeclarator OP_DIM
	;

ParameterList
	: Parameter
	| ParameterList ',' Parameter
	;

Parameter
	: TypeSpecifier DeclaratorName
	;

DeclaratorName
	: IDENTIFIER
        | DeclaratorName OP_DIM
        ;

Throws
	: THROWS TypeNameList
	;

MethodBody
	: Block
	| ';'
	;

ConstructorDeclaration
	: Modifiers ConstructorDeclarator Throws Block
	| Modifiers ConstructorDeclarator        Block
	|           ConstructorDeclarator Throws Block
	|           ConstructorDeclarator        Block
	;

ConstructorDeclarator
	: TypeName '(' ParameterList ')'
	| TypeName '(' ')'
	;

StaticInitializer
	: STATIC Block
	;

InterfaceDeclaration
	: Modifiers INTERFACE IDENTIFIER ExtendsInterfaces InterfaceBody
	| Modifiers INTERFACE IDENTIFIER                   InterfaceBody
	|           INTERFACE IDENTIFIER ExtendsInterfaces InterfaceBody
	|           INTERFACE IDENTIFIER                   InterfaceBody
	;

ExtendsInterfaces
	: EXTENDS TypeName
	| ExtendsInterfaces ',' TypeName
        ;

InterfaceBody
	: '{' FieldDeclarations '}'
	;

Block
	: '{' LocalVariableDeclarationsAndStatements '}'
	| '{' '}'
        ;

LocalVariableDeclarationsAndStatements
	: LocalVariableDeclarationOrStatement
	| LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
	;

LocalVariableDeclarationOrStatement
	: LocalVariableDeclarationStatement
	| Statement
	;

LocalVariableDeclarationStatement
	: TypeSpecifier VariableDeclarators ';'
	;

Statement
	: EmptyStatement
	| LabeledStatement
	| ExpressionStatement ';'
        | SelectionStatement
        | IterationStatement
	| JumpStatement
	| GuardingStatement
	| Block
	;

EmptyStatement
	: ';'
        ;

LabeledStatement
	: IDENTIFIER ':' LocalVariableDeclarationOrStatement
        | CASE ConstantExpression ':' LocalVariableDeclarationOrStatement
	| DEFAULT ':' LocalVariableDeclarationOrStatement
        ;

ExpressionStatement
	: Expression
	;

SelectionStatement
	: IF '(' Expression ')' Statement
        | IF '(' Expression ')' Statement ELSE Statement
        | SWITCH '(' Expression ')' Block
        ;

IterationStatement
	: WHILE '(' Expression ')' Statement
	| DO Statement WHILE '(' Expression ')' ';'
	| FOR '(' ForInit ForExpr ForIncr ')' Statement
	| FOR '(' ForInit ForExpr         ')' Statement
	;

ForInit
	: ExpressionStatements ';'
	| LocalVariableDeclarationStatement
	| ';'
	;

ForExpr
	: Expression ';'
	| ';'
	;

ForIncr
	: ExpressionStatements
	;

ExpressionStatements
	: ExpressionStatement
	| ExpressionStatements ',' ExpressionStatement
	;

JumpStatement
	: BREAK IDENTIFIER ';'
	| BREAK            ';'
        | CONTINUE IDENTIFIER ';'
	| CONTINUE            ';'
	| RETURN Expression ';'
	| RETURN            ';'
	| THROW Expression ';'
	;

GuardingStatement
	: SYNCHRONIZED '(' Expression ')' Statement
	| TRY Block Finally
	| TRY Block Catches
	| TRY Block Catches Finally
	;

Catches
	: Catch
	| Catches Catch
	;

Catch
	: CATCH '(' TypeSpecifier IDENTIFIER ')' Block
	| CATCH '(' TypeSpecifier ')' Block
	;

Finally
	: FINALLY Block
	;

PrimaryExpression
	: QualifiedName
	| NotJustName
	;

NotJustName
	: SpecialName
	| AllocationExpression
	| ComplexPrimary
	;

ComplexPrimary
	: '(' Expression ')'
	| ComplexPrimaryNoParenthesis
	;

ComplexPrimaryNoParenthesis
	: LITERAL
	| ArrayAccess
	| FieldAccess
	| MethodCall
	;

ArrayAccess
	: QualifiedName '[' Expression ']'
	| ComplexPrimary '[' Expression ']'
        ;

FieldAccess
	: NotJustName '.' IDENTIFIER
	| ReallyPostfixExpression '.' IDENTIFIER
	;


MethodCall
	: MethodAccess '(' ArgumentList ')'
	| MethodAccess '(' ')'
	;

MethodAccess
	: ComplexPrimaryNoParenthesis
	| QualifiedName
	| SpecialName
	;

SpecialName
	: THIS
	| SUPER
	| NULL
	;

ArgumentList
	: Expression
	| ArgumentList ',' Expression
	;

AllocationExpression
	: NEW TypeName '(' ArgumentList ')'
	| NEW TypeName '('              ')'
	| NEW TypeName DimExprs Dims
	| NEW TypeName DimExprs
	;

DimExprs
	: DimExpr
	| DimExprs DimExpr
	;

DimExpr
	: '[' Expression ']'
	;

Dims
	: OP_DIM
	| Dims OP_DIM
	;

PostfixExpression
	: PrimaryExpression
        | ReallyPostfixExpression
        ;

ReallyPostfixExpression
	: PostfixExpression OP_INC
	| PostfixExpression OP_DEC
	;

UnaryExpression
	: OP_INC UnaryExpression
	| OP_DEC UnaryExpression
	| ArithmeticUnaryOperator CastExpression
	| LogicalUnaryExpression
	;

LogicalUnaryExpression
	: PostfixExpression
	| LogicalUnaryOperator UnaryExpression
	;

LogicalUnaryOperator
	: '~'
	| '!'
	;

ArithmeticUnaryOperator
	: '+'
	| '-'
	;

CastExpression
	: UnaryExpression
	| '(' PrimitiveType ')' CastExpression
	| '(' Expression ')' LogicalUnaryExpression
	;

MultiplicativeExpression
	: CastExpression
	| MultiplicativeExpression '*' CastExpression
	| MultiplicativeExpression '/' CastExpression
	| MultiplicativeExpression '%' CastExpression
	;

AdditiveExpression
	: MultiplicativeExpression
        | AdditiveExpression '+' MultiplicativeExpression
	| AdditiveExpression '-' MultiplicativeExpression
        ;

ShiftExpression
	: AdditiveExpression
        | ShiftExpression OP_SHL AdditiveExpression
        | ShiftExpression OP_SHR AdditiveExpression
        | ShiftExpression OP_SHRR AdditiveExpression
	;

RelationalExpression
	: ShiftExpression
        | RelationalExpression '<' ShiftExpression
	| RelationalExpression '>' ShiftExpression
	| RelationalExpression OP_LE ShiftExpression
	| RelationalExpression OP_GE ShiftExpression
	| RelationalExpression INSTANCEOF TypeSpecifier
	;

EqualityExpression
	: RelationalExpression
        | EqualityExpression OP_EQ RelationalExpression
        | EqualityExpression OP_NE RelationalExpression
        ;

AndExpression
	: EqualityExpression
        | AndExpression '&' EqualityExpression
        ;

ExclusiveOrExpression
	: AndExpression
	| ExclusiveOrExpression '^' AndExpression
	;

InclusiveOrExpression
	: ExclusiveOrExpression
	| InclusiveOrExpression '|' ExclusiveOrExpression
	;

ConditionalAndExpression
	: InclusiveOrExpression
	| ConditionalAndExpression OP_LAND InclusiveOrExpression
	;

ConditionalOrExpression
	: ConditionalAndExpression
	| ConditionalOrExpression OP_LOR ConditionalAndExpression
	;

ConditionalExpression
	: ConditionalOrExpression
	| ConditionalOrExpression '?' Expression ':' ConditionalExpression
	;

AssignmentExpression
	: ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression
	;

AssignmentOperator
	: '='
	| ASS_MUL
	| ASS_DIV
	| ASS_MOD
	| ASS_ADD
	| ASS_SUB
	| ASS_SHL
	| ASS_SHR
	| ASS_SHRR
	| ASS_AND
	| ASS_XOR
	| ASS_OR
	;

Expression
	: AssignmentExpression
        ;

ConstantExpression
	: ConditionalExpression
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
# line 1 "javalex.l"
#define INITIAL 0
# line 2 "javalex.l"
/**************************************************************/
/* This file contains a restricted lexer for java language.   */
/* The lexer will scan java programs in ASCII without unicode */
/* characters and/or unicode escape sequences.                */
/* It is provided only to enable the grammar user to test it  */
/* right away.                                                */
/**************************************************************/
# line 65 "javalex.l"

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

#define YY_END_OF_BUFFER 89
typedef int yy_state_type;
static const short int yy_accept[385] =
    {   0,
        0,    0,   89,   88,   86,    4,   88,    4,    4,   88,
        3,    4,    4,    4,    3,    4,   85,   85,    4,    4,
        4,    4,   84,    3,    4,   84,   84,   84,   84,   84,
       84,   84,   84,   84,   84,   84,   84,   84,   84,   84,
       84,   84,    4,    9,    0,   85,    0,   24,   11,   21,
        0,    0,   19,   12,   17,   13,   18,   85,    0,   87,
       20,   85,   85,    0,   85,   85,   85,    0,    0,   85,
       15,    7,    6,    8,   14,   84,    0,    0,    5,   23,
       84,   84,   84,   84,   84,   84,   84,   84,   84,   29,
       84,   84,   84,   84,   84,   84,   84,   84,   84,   81,

       84,   84,   84,   84,   84,   84,   84,   84,   84,   84,
       84,   84,   84,   84,   84,   84,   84,   84,   84,   84,
       84,   84,   22,   10,   85,   85,    0,    0,    0,   87,
       85,   85,   85,    0,    0,   85,   85,    0,   85,   25,
       26,   16,    0,    0,   84,   84,   84,   84,   84,   84,
       84,   84,   84,   84,   84,   84,   84,   84,   84,   84,
       64,   84,   84,   84,   84,   84,   84,   50,   84,   84,
       70,   84,   84,   84,   84,   84,   84,   84,   84,   84,
       84,   84,   84,   84,   84,   84,   84,   84,   84,   47,
       52,   84,   84,   84,    0,   85,   87,    0,   85,   85,

       85,   27,    0,    0,    0,    0,    0,    5,   84,   84,
       84,   43,   48,   53,   84,   63,   84,   84,   84,   84,
       84,   39,   84,   84,   84,   84,   84,   84,   77,   84,
       84,   84,   84,   84,   60,   84,   74,   84,   84,   84,
       84,   84,   84,   51,   84,   84,   84,   84,   84,   84,
       83,   84,   84,    1,   57,   84,   84,    0,    0,    0,
       84,   84,   38,   58,   68,   72,   84,   84,   84,   84,
        2,   49,   59,   84,   84,   84,   84,   40,   84,   84,
       84,   84,   82,   84,   84,   84,   84,   84,   61,   84,
       71,   84,   84,   32,   84,   84,   67,    0,    0,    0,

        0,    0,    5,   84,   84,   84,   84,   34,   84,   84,
       69,   84,   84,   35,   84,   84,   65,   84,   84,   84,
       84,   46,   56,   66,   75,   84,   37,   84,   84,    0,
        5,    0,    0,    0,   84,   33,   84,   80,   44,   54,
       73,   84,   84,   84,   84,   31,   36,   84,   84,   84,
       84,    0,    0,    0,    0,    0,    0,    0,    5,   28,
       76,   84,   84,   84,   78,   84,   84,   84,   62,    0,
        0,    0,   84,   84,   55,   41,   84,   42,   30,   45,
       84,   84,   79,    0
    } ;

static const YY_CHAR yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    2,    3,    4,
        1,    5,    6,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    7,    8,    9,    1,    1,   10,   11,   12,   13,
       13,   14,   15,   13,   16,   17,   18,   19,   20,   20,
       20,   20,   20,   20,   20,   21,   21,   22,   13,   23,
       24,   25,   22,    1,   26,   26,   26,   27,   28,   29,
       30,   30,   30,   30,   30,   31,   30,   30,   30,   30,
       30,   30,   30,   30,   30,   30,   30,   32,   30,   30,
       33,   34,   35,   36,   30,    1,   37,   38,   39,   40,

       41,   42,   43,   44,   45,   30,   46,   47,   48,   49,
       50,   51,   30,   52,   53,   54,   55,   56,   57,   58,
       59,   60,   13,   61,   13,   22,    1,    1,    1,    1,
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

static const YY_CHAR yy_meta[62] =
    {   0,
        1,    1,    1,    2,    1,    1,    1,    1,    1,    1,
        1,    3,    1,    1,    1,    1,    1,    1,    4,    4,
        4,    1,    1,    1,    1,    4,    4,    4,    4,    5,
        6,    5,    1,    1,    1,    1,    4,    4,    4,    4,
        4,    4,    5,    5,    5,    5,    6,    5,    5,    5,
        5,    5,    5,    5,    5,    5,    5,    5,    5,    5,
        1
    } ;

static const short int yy_base[397] =
    {   0,
        0,    0,  776,  777,  777,  751,   53,  750,   52,  739,
      777,  748,   49,   50,   48,   57,   71,  113,  777,   54,
      747,   55,    0,   91,  746,  731,   55,   73,   65,   25,
       90,   75,  101,  718,   67,   31,   84,  726,  102,  114,
      101,  722,  104,  777,  138,  777,  129,  777,  777,  777,
      753,  135,  777,  777,  777,  777,  777,  171,  750,    0,
      777,  174,  116,  189,  204,  777,  777,    0,  736,    0,
      738,  777,  777,  777,   60,    0,  232,  150,  777,  777,
      708,  710,  718,  704,  122,  720,  719,  706,  712,  698,
      699,  697,  703,  700,  698,  695,  692,  696,  690,    0,

      692,  173,  693,  687,  683,  692,  697,  683,  697,   20,
      697,  126,  684,  665,  645,  637,  623,   79,  203,  619,
      141,  625,  777,  777,  232,  777,  642,  654,  648,    0,
      235,  250,  777,  638,  177,  262,  777,  637,  154,  777,
      777,  639,  648,  282,  607,  613,  622,  617,  187,  618,
      603,  601,  151,  616,  613,  607,  606,  580,  593,  592,
        0,  572,  585,  574,  195,  582,  568,  574,  570,  567,
        0,  564,  557,  565,  559,  535,  534,  540,  531,  529,
      530,  527,  539,  519,  532,  517,  519,  518,  523,    0,
        0,  523,  512,  499,  274,  278,  777,  289,  292,  212,

      777,  777,  531,  525,  320,  338,  344,  362,  490,  499,
      493,    0,    0,    0,  494,    0,  478,  475,  483,  472,
      478,    0,  473,  480,  460,  450,  451,  449,    0,  459,
      446,  445,  459,  437,    0,  431,    0,  449,  433,  446,
      443,  438,  433,    0,  413,  408,  416,  407,  419,  412,
        0,  398,  401,    0,    0,  393,  404,  380,  386,  404,
      407,  406,    0,    0,    0,    0,  392,  391,  396,  383,
        0,  373,    0,  378,  372,  368,  360,    0,  364,  370,
      364,  349,    0,  359,  347,  360,  357,  346,    0,  355,
        0,  337,  326,  324,  331,  330,    0,  422,  428,  446,

      464,  470,  488,  335,  323,  316,  316,    0,  310,  302,
        0,  321,  318,    0,  318,  317,    0,  303,  311,  298,
      283,    0,    0,    0,    0,  286,    0,  294,  284,  287,
      316,  506,  512,  530,  274,    0,  275,    0,    0,    0,
        0,  265,  265,  264,  244,    0,    0,  239,  229,  224,
      227,  301,  548,  554,  572,  590,  596,  614,  632,    0,
        0,  206,  207,  208,    0,  192,  176,  153,    0,  168,
      315,  638,  117,  120,    0,    0,   92,    0,    0,    0,
       78,   61,    0,  777,  673,  679,  682,  688,  694,   85,
      697,  703,  709,  715,  721,  727

    } ;

static const short int yy_def[397] =
    {   0,
      384,    1,  384,  384,  384,  384,  385,  384,  384,  386,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  387,  384,  384,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  384,  384,  385,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  388,  389,
      384,  384,   17,  384,  384,  384,  384,  390,  384,   18,
      384,  384,  384,  384,  384,  387,  384,  384,  384,  384,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,

      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  384,  384,  384,  384,  384,  388,  388,  389,
      384,  384,  384,  384,  384,  384,  384,  384,  391,  384,
      384,  384,  392,  393,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  384,  384,  384,  384,  384,  391,

      384,  384,  392,  392,  393,  393,  393,  393,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  384,  394,  393,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  394,  394,  392,

      394,  394,  394,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  392,
      392,  393,  395,  394,  387,  387,  387,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  396,  394,  394,  394,  392,  395,  394,  394,  387,
      387,  387,  387,  387,  387,  387,  387,  387,  387,  392,
      396,  392,  387,  387,  387,  387,  387,  387,  387,  387,
      387,  387,  387,    0,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384

    } ;

static const short int yy_nxt[839] =
    {   0,
        4,    5,    5,    5,    5,    5,    5,    6,    7,    8,
        9,   10,   11,   12,   13,   14,   15,   16,   17,   18,
       18,   19,   20,   21,   22,   23,   23,   23,   23,   23,
       23,   23,   24,    4,   11,   25,   26,   27,   28,   29,
       30,   31,   32,   23,   33,   23,   34,   23,   35,   36,
       37,   38,   39,   40,   23,   41,   42,   23,   23,   23,
       43,   46,   49,   54,  176,   56,   58,   58,   58,  177,
       59,   91,   55,   57,   60,   50,   71,   72,   74,   75,
       61,  107,   92,  141,  142,  108,   47,   62,  139,   63,
       63,   64,   77,   77,   77,   77,   77,   77,   65,   66,

      383,   67,   68,  104,   82,   89,   83,  105,   78,   85,
       69,   65,   66,   84,   90,   98,   86,   67,  382,   87,
      109,  106,   88,  186,   99,   79,   93,  123,   68,   62,
      187,   70,   70,   70,   94,  110,   95,  120,  111,   96,
       65,   66,  100,   46,   97,  113,   46,  384,  101,  102,
      121,  381,   69,   65,   66,  114,  115,  118,  116,   46,
      117,  380,   45,  143,  124,  119,   45,  144,   51,  379,
       45,   47,   51,  384,  149,  150,   51,   45,  179,  180,
       45,  204,   45,   51,  201,  192,   51,  193,   51,   58,
       58,   58,  131,  131,  131,  136,  136,  136,  125,  126,

      201,  132,  133,  218,  219,   62,  378,   64,   64,   64,
      127,  125,  126,  134,  132,  133,   65,   66,  135,  135,
      377,  166,  136,  136,  136,  167,  168,  213,   69,   65,
       66,  376,  137,   77,   77,   77,   77,   77,   77,  188,
      214,  230,  201,  138,  231,  137,  195,  195,  375,   78,
      196,  196,  196,  131,  131,  131,  374,  189,  201,  373,
      126,  190,  132,  133,  198,  198,   79,  369,  199,  199,
      199,  127,  368,  126,  134,  132,  133,  367,  133,  366,
      136,  136,  136,  206,  206,   77,  206,  206,  206,  134,
      137,  133,  196,  196,  196,  365,  196,  196,  196,  207,

      352,  138,  364,  137,  353,  363,  126,  199,  199,  199,
      199,  199,  199,  362,  371,  361,  208,  127,  372,  126,
      133,  206,  206,   77,  206,  206,  206,  360,  371,  204,
      351,  134,  372,  133,  350,  349,  348,  207,  347,  206,
      206,   77,  206,  206,  206,  206,  206,   77,  206,  206,
      206,  346,  345,  344,  208,  207,  343,  259,  342,  341,
      340,  260,  339,  206,  206,   77,  206,  206,  206,  338,
      337,  336,  208,  335,  329,  328,  327,  326,  208,  207,
      325,   77,   77,   77,   77,   77,   77,  299,  299,  300,
      299,  299,  299,  324,  323,  322,  208,   78,  321,  301,

      320,  319,  318,  302,  317,  206,  206,   77,  206,  206,
      206,  316,  315,  314,   79,  313,  312,  259,  311,  310,
      303,  260,  309,  299,  299,  300,  299,  299,  299,  299,
      299,  300,  299,  299,  299,  301,  308,  307,  208,  302,
      306,  301,  305,  304,  297,  302,  296,  300,  300,  300,
      300,  300,  300,  295,  294,  293,  303,  292,  291,  204,
      290,  289,  303,  330,  288,  299,  299,  300,  299,  299,
      299,  299,  299,  300,  299,  299,  299,  287,  286,  285,
      331,  332,  284,  333,  283,  282,  281,  334,  280,  299,
      299,  300,  299,  299,  299,  279,  278,  277,  303,  276,

      275,  301,  274,  273,  303,  302,  272,  206,  206,   77,
      206,  206,  206,  355,  355,  356,  355,  355,  355,  259,
      271,  270,  303,  260,  269,  357,  268,  267,  266,  358,
      265,  299,  299,  300,  299,  299,  299,  264,  263,  262,
      208,  261,  258,  333,  204,  257,  359,  334,  256,  299,
      299,  300,  299,  299,  299,  299,  299,  300,  299,  299,
      299,  301,  255,  254,  303,  302,  253,  301,  252,  251,
      250,  302,  249,  299,  299,  300,  299,  299,  299,  248,
      247,  246,  303,  245,  244,  301,  243,  242,  303,  302,
      241,  300,  300,  300,  300,  300,  300,  355,  355,  356,

      355,  355,  355,  204,  240,  239,  303,  330,  238,  357,
      237,  236,  235,  358,  234,  299,  299,  300,  299,  299,
      299,  233,  232,  229,  331,  228,  227,  333,  226,  225,
      359,  334,  224,  299,  299,  300,  299,  299,  299,  300,
      300,  300,  300,  300,  300,  301,  223,  222,  303,  302,
      221,  204,  220,  217,  216,  330,  215,  212,  211,  210,
      209,  204,  202,  137,  133,  197,  303,  129,  126,  194,
      191,  185,  331,   45,   45,   45,   45,   45,   45,   51,
       51,  184,   51,   51,   51,   76,   76,   76,  128,  128,
      128,  128,  128,  128,  130,  183,  130,  130,  130,  130,

      200,  182,  200,  203,  203,  203,  203,  203,  203,  205,
      205,  205,  205,  205,  205,  298,  298,  298,  298,  298,
      298,  354,  354,  354,  354,  354,  354,  370,  370,  370,
      370,  370,  370,  181,  178,  175,  174,  173,  172,  171,
      170,  169,  165,  164,  163,  162,  161,  160,  159,  158,
      157,  156,  155,  154,  153,  152,  151,  148,  147,  146,
      145,  140,   66,  129,   46,  122,  112,  103,   81,   80,
       73,   53,   52,   48,   44,  384,    3,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,

      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384
    } ;

static const short int yy_chk[839] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    7,    9,   13,  110,   14,   15,   15,   15,  110,
       16,   30,   13,   14,   16,    9,   20,   20,   22,   22,
       16,   36,   30,   75,   75,   36,    7,   17,  390,   17,
       17,   17,   24,   24,   24,   24,   24,   24,   17,   17,

      382,   17,   17,   35,   27,   29,   27,   35,   24,   28,
       17,   17,   17,   27,   29,   32,   28,   17,  381,   28,
       37,   35,   28,  118,   32,   24,   31,   43,   17,   18,
      118,   18,   18,   18,   31,   37,   31,   41,   37,   31,
       18,   18,   33,   18,   31,   39,   45,   63,   33,   33,
       41,  377,   18,   18,   18,   39,   39,   40,   39,   18,
       39,  374,   47,   78,   43,   40,   47,   78,   52,  373,
       47,   45,   52,   63,   85,   85,   52,   47,  112,  112,
       47,  370,   47,   52,  139,  121,   52,  121,   52,   58,
       58,   58,   62,   62,   62,  135,  135,  135,   58,   58,

      139,   62,   62,  153,  153,   64,  368,   64,   64,   64,
       58,   58,   58,   62,   62,   62,   64,   64,   65,   65,
      367,  102,   65,   65,   65,  102,  102,  149,   64,   64,
       64,  366,   65,   77,   77,   77,   77,   77,   77,  119,
      149,  165,  200,   65,  165,   65,  125,  125,  364,   77,
      125,  125,  125,  131,  131,  131,  363,  119,  200,  362,
      125,  119,  131,  131,  132,  132,   77,  351,  132,  132,
      132,  125,  350,  125,  131,  131,  131,  349,  132,  348,
      136,  136,  136,  144,  144,  144,  144,  144,  144,  132,
      136,  132,  195,  195,  195,  345,  196,  196,  196,  144,

      330,  136,  344,  136,  330,  343,  196,  198,  198,  198,
      199,  199,  199,  342,  352,  337,  144,  196,  352,  196,
      199,  205,  205,  205,  205,  205,  205,  335,  371,  331,
      329,  199,  371,  199,  328,  326,  321,  205,  320,  206,
      206,  206,  206,  206,  206,  207,  207,  207,  207,  207,
      207,  319,  318,  316,  205,  206,  315,  207,  313,  312,
      310,  207,  309,  208,  208,  208,  208,  208,  208,  307,
      306,  305,  206,  304,  296,  295,  294,  293,  207,  208,
      292,  258,  258,  258,  258,  258,  258,  259,  259,  259,
      259,  259,  259,  290,  288,  287,  208,  258,  286,  259,

      285,  284,  282,  259,  281,  260,  260,  260,  260,  260,
      260,  280,  279,  277,  258,  276,  275,  260,  274,  272,
      259,  260,  270,  298,  298,  298,  298,  298,  298,  299,
      299,  299,  299,  299,  299,  298,  269,  268,  260,  298,
      267,  299,  262,  261,  257,  299,  256,  300,  300,  300,
      300,  300,  300,  253,  252,  250,  298,  249,  248,  300,
      247,  246,  299,  300,  245,  301,  301,  301,  301,  301,
      301,  302,  302,  302,  302,  302,  302,  243,  242,  241,
      300,  301,  240,  302,  239,  238,  236,  302,  234,  303,
      303,  303,  303,  303,  303,  233,  232,  231,  301,  230,

      228,  303,  227,  226,  302,  303,  225,  332,  332,  332,
      332,  332,  332,  333,  333,  333,  333,  333,  333,  332,
      224,  223,  303,  332,  221,  333,  220,  219,  218,  333,
      217,  334,  334,  334,  334,  334,  334,  215,  211,  210,
      332,  209,  204,  334,  203,  194,  333,  334,  193,  353,
      353,  353,  353,  353,  353,  354,  354,  354,  354,  354,
      354,  353,  192,  189,  334,  353,  188,  354,  187,  186,
      185,  354,  184,  355,  355,  355,  355,  355,  355,  183,
      182,  181,  353,  180,  179,  355,  178,  177,  354,  355,
      176,  356,  356,  356,  356,  356,  356,  357,  357,  357,

      357,  357,  357,  356,  175,  174,  355,  356,  173,  357,
      172,  170,  169,  357,  168,  358,  358,  358,  358,  358,
      358,  167,  166,  164,  356,  163,  162,  358,  160,  159,
      357,  358,  158,  359,  359,  359,  359,  359,  359,  372,
      372,  372,  372,  372,  372,  359,  157,  156,  358,  359,
      155,  372,  154,  152,  151,  372,  150,  148,  147,  146,
      145,  143,  142,  138,  134,  129,  359,  128,  127,  122,
      120,  117,  372,  385,  385,  385,  385,  385,  385,  386,
      386,  116,  386,  386,  386,  387,  387,  387,  388,  388,
      388,  388,  388,  388,  389,  115,  389,  389,  389,  389,

      391,  114,  391,  392,  392,  392,  392,  392,  392,  393,
      393,  393,  393,  393,  393,  394,  394,  394,  394,  394,
      394,  395,  395,  395,  395,  395,  395,  396,  396,  396,
      396,  396,  396,  113,  111,  109,  108,  107,  106,  105,
      104,  103,  101,   99,   98,   97,   96,   95,   94,   93,
       92,   91,   90,   89,   88,   87,   86,   84,   83,   82,
       81,   71,   69,   59,   51,   42,   38,   34,   26,   25,
       21,   12,   10,    8,    6,    3,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,

      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      384,  384,  384,  384,  384,  384,  384,  384
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
		if ( yy_current_state >= 385 )
		    yy_c = yy_meta[yy_c];
		}
	    yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
	    ++yy_cp;
	    }
	while ( yy_current_state != 384 );
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
# line 67 "javalex.l"
{return LITERAL;}
	YY_BREAK
case 2:
# line 68 "javalex.l"
{return LITERAL;}
	YY_BREAK
case 3:
# line 70 "javalex.l"
{return yytext[0];}
	YY_BREAK
case 4:
# line 71 "javalex.l"
{return yytext[0];}
	YY_BREAK
case 5:
# line 72 "javalex.l"
{return OP_DIM;}
	YY_BREAK
case 6:
# line 74 "javalex.l"
{return OP_EQ;}
	YY_BREAK
case 7:
# line 75 "javalex.l"
{return OP_LE;}
	YY_BREAK
case 8:
# line 76 "javalex.l"
{return OP_GE;}
	YY_BREAK
case 9:
# line 77 "javalex.l"
{return OP_NE;}
	YY_BREAK
case 10:
# line 78 "javalex.l"
{return OP_LOR;}
	YY_BREAK
case 11:
# line 79 "javalex.l"
{return OP_LAND;}
	YY_BREAK
case 12:
# line 80 "javalex.l"
{return OP_INC;}
	YY_BREAK
case 13:
# line 81 "javalex.l"
{return OP_DEC;}
	YY_BREAK
case 14:
# line 82 "javalex.l"
{return OP_SHR;}
	YY_BREAK
case 15:
# line 83 "javalex.l"
{return OP_SHL;}
	YY_BREAK
case 16:
# line 84 "javalex.l"
{return OP_SHRR;}
	YY_BREAK
case 17:
# line 85 "javalex.l"
{return ASS_ADD;}
	YY_BREAK
case 18:
# line 86 "javalex.l"
{return ASS_SUB;}
	YY_BREAK
case 19:
# line 87 "javalex.l"
{return ASS_MUL;}
	YY_BREAK
case 20:
# line 88 "javalex.l"
{return ASS_DIV;}
	YY_BREAK
case 21:
# line 89 "javalex.l"
{return ASS_AND;}
	YY_BREAK
case 22:
# line 90 "javalex.l"
{return ASS_OR;}
	YY_BREAK
case 23:
# line 91 "javalex.l"
{return ASS_XOR;}
	YY_BREAK
case 24:
# line 92 "javalex.l"
{return ASS_MOD;}
	YY_BREAK
case 25:
# line 93 "javalex.l"
{return ASS_SHL;}
	YY_BREAK
case 26:
# line 94 "javalex.l"
{return ASS_SHR;}
	YY_BREAK
case 27:
# line 95 "javalex.l"
{return ASS_SHRR;}
	YY_BREAK
case 28:
# line 97 "javalex.l"
{return ABSTRACT;}
	YY_BREAK
case 29:
# line 98 "javalex.l"
{return DO;}
	YY_BREAK
case 30:
# line 99 "javalex.l"
{return IMPLEMENTS;}
	YY_BREAK
case 31:
# line 100 "javalex.l"
{return PACKAGE;}
	YY_BREAK
case 32:
# line 101 "javalex.l"
{return THROW;}
	YY_BREAK
case 33:
# line 102 "javalex.l"
{return BOOLEAN;}
	YY_BREAK
case 34:
# line 103 "javalex.l"
{return DOUBLE;}
	YY_BREAK
case 35:
# line 104 "javalex.l"
{return IMPORT;}
	YY_BREAK
case 36:
# line 105 "javalex.l"
{return PRIVATE;}
	YY_BREAK
case 37:
# line 106 "javalex.l"
{return THROWS;}
	YY_BREAK
case 38:
# line 107 "javalex.l"
{return BREAK;}
	YY_BREAK
case 39:
# line 108 "javalex.l"
{return ELSE;}
	YY_BREAK
case 40:
# line 109 "javalex.l"
{return INNER;}
	YY_BREAK
case 41:
# line 110 "javalex.l"
{return PROTECTED;}
	YY_BREAK
case 42:
# line 111 "javalex.l"
{return TRANSIENT;}
	YY_BREAK
case 43:
# line 112 "javalex.l"
{return BYTE;}
	YY_BREAK
case 44:
# line 113 "javalex.l"
{return EXTENDS;}
	YY_BREAK
case 45:
# line 114 "javalex.l"
{return INSTANCEOF;}
	YY_BREAK
case 46:
# line 115 "javalex.l"
{return PUBLIC;}
	YY_BREAK
case 47:
# line 116 "javalex.l"
{return TRY;}
	YY_BREAK
case 48:
# line 117 "javalex.l"
{return CASE;}
	YY_BREAK
case 49:
# line 118 "javalex.l"
{return FINAL;}
	YY_BREAK
case 50:
# line 119 "javalex.l"
{return INT;}
	YY_BREAK
case 51:
# line 120 "javalex.l"
{return REST;}
	YY_BREAK
case 52:
# line 121 "javalex.l"
{return VAR;}
	YY_BREAK
case 53:
# line 122 "javalex.l"
{return CAST;}
	YY_BREAK
case 54:
# line 123 "javalex.l"
{return FINALLY;}
	YY_BREAK
case 55:
# line 124 "javalex.l"
{return INTERFACE;}
	YY_BREAK
case 56:
# line 125 "javalex.l"
{return RETURN;}
	YY_BREAK
case 57:
# line 126 "javalex.l"
{return VOID;}
	YY_BREAK
case 58:
# line 127 "javalex.l"
{return CATCH;}
	YY_BREAK
case 59:
# line 128 "javalex.l"
{return FLOAT;}
	YY_BREAK
case 60:
# line 129 "javalex.l"
{return LONG;}
	YY_BREAK
case 61:
# line 130 "javalex.l"
{return SHORT;}
	YY_BREAK
case 62:
# line 131 "javalex.l"
{return VOLATILE;}
	YY_BREAK
case 63:
# line 132 "javalex.l"
{return CHAR;}
	YY_BREAK
case 64:
# line 133 "javalex.l"
{return FOR;}
	YY_BREAK
case 65:
# line 134 "javalex.l"
{return NATIVE;}
	YY_BREAK
case 66:
# line 135 "javalex.l"
{return STATIC;}
	YY_BREAK
case 67:
# line 136 "javalex.l"
{return WHILE;}
	YY_BREAK
case 68:
# line 137 "javalex.l"
{return CLASS;}
	YY_BREAK
case 69:
# line 138 "javalex.l"
{return FUTURE;}
	YY_BREAK
case 70:
# line 139 "javalex.l"
{return NEW;}
	YY_BREAK
case 71:
# line 140 "javalex.l"
{return SUPER;}
	YY_BREAK
case 72:
# line 141 "javalex.l"
{return CONST;}
	YY_BREAK
case 73:
# line 142 "javalex.l"
{return GENERIC;}
	YY_BREAK
case 74:
# line 143 "javalex.l"
{return NULL;}
	YY_BREAK
case 75:
# line 144 "javalex.l"
{return SWITCH;}
	YY_BREAK
case 76:
# line 145 "javalex.l"
{return CONTINUE;}
	YY_BREAK
case 77:
# line 146 "javalex.l"
{return GOTO;}
	YY_BREAK
case 78:
# line 147 "javalex.l"
{return OPERATOR;}
	YY_BREAK
case 79:
# line 148 "javalex.l"
{return SYNCHRONIZED;}
	YY_BREAK
case 80:
# line 149 "javalex.l"
{return DEFAULT;}
	YY_BREAK
case 81:
# line 150 "javalex.l"
{return IF;}
	YY_BREAK
case 82:
# line 151 "javalex.l"
{return OUTER;}
	YY_BREAK
case 83:
# line 152 "javalex.l"
{return THIS;}
	YY_BREAK
case 84:
# line 154 "javalex.l"
{return IDENTIFIER;}
	YY_BREAK
case 85:
# line 156 "javalex.l"
{return LITERAL;}
	YY_BREAK
case 86:
# line 158 "javalex.l"
{}
	YY_BREAK
case 87:
# line 160 "javalex.l"
{}
	YY_BREAK
case 88:
# line 161 "javalex.l"
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
	    if ( yy_current_state >= 385 )
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
	if ( yy_current_state >= 385 )
	    yy_c = yy_meta[yy_c];
	}
    yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
    yy_is_jam = (yy_current_state == 384);

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
# line 161 "javalex.l"

yyerror(string)
char*string;
{
    printf("parser error: %s\n", string);
}


main()
{
    yyparse();
}
