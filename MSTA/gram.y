%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELIPSIS RANGE

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%start file
%%

primary_expr
	: identifier
	| CONSTANT
	| STRING_LITERAL
	| '(' expr ')'
	;

postfix_expr
	: primary_expr
	| postfix_expr '[' expr ']'
	| postfix_expr '(' ')'
	| postfix_expr '(' argument_expr_list ')'
	| postfix_expr '.' identifier
	| postfix_expr PTR_OP identifier
	| postfix_expr INC_OP
	| postfix_expr DEC_OP
	;

argument_expr_list
	: assignment_expr
	| argument_expr_list ',' assignment_expr
	;

unary_expr
	: postfix_expr
	| INC_OP unary_expr
	| DEC_OP unary_expr
	| unary_operator cast_expr
	| SIZEOF unary_expr
	| SIZEOF '(' type_name ')'
	;

unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	;

cast_expr
	: unary_expr
	| '(' type_name ')' cast_expr
	;

multiplicative_expr
	: cast_expr
	| multiplicative_expr '*' cast_expr
	| multiplicative_expr '/' cast_expr
	| multiplicative_expr '%' cast_expr
	;

additive_expr
	: multiplicative_expr
	| additive_expr '+' multiplicative_expr
	| additive_expr '-' multiplicative_expr
	;

shift_expr
	: additive_expr
	| shift_expr LEFT_OP additive_expr
	| shift_expr RIGHT_OP additive_expr
	;

relational_expr
	: shift_expr
	| relational_expr '<' shift_expr
	| relational_expr '>' shift_expr
	| relational_expr LE_OP shift_expr
	| relational_expr GE_OP shift_expr
	;

equality_expr
	: relational_expr
	| equality_expr EQ_OP relational_expr
	| equality_expr NE_OP relational_expr
	;

and_expr
	: equality_expr
	| and_expr '&' equality_expr
	;

exclusive_or_expr
	: and_expr
	| exclusive_or_expr '^' and_expr
	;

inclusive_or_expr
	: exclusive_or_expr
	| inclusive_or_expr '|' exclusive_or_expr
	;

logical_and_expr
	: inclusive_or_expr
	| logical_and_expr AND_OP inclusive_or_expr
	;

logical_or_expr
	: logical_and_expr
	| logical_or_expr OR_OP logical_and_expr
	;

conditional_expr
	: logical_or_expr
	| logical_or_expr '?' logical_or_expr ':' conditional_expr
	;

assignment_expr
	: conditional_expr
	| unary_expr assignment_operator assignment_expr
	;

assignment_operator
	: '='
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;

expr
	: assignment_expr
	| expr ',' assignment_expr
	;

constant_expr
	: conditional_expr
	;

declaration
	: declaration_specifiers ';'
	| declaration_specifiers init_declarator_list ';'
	;

declaration_specifiers
	: storage_class_specifier
	| storage_class_specifier declaration_specifiers
	| type_specifier
	| type_specifier declaration_specifiers
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: declarator
	| declarator '=' initializer
	;

storage_class_specifier
	: TYPEDEF
	| EXTERN
	| STATIC
	| AUTO
	| REGISTER
	;

type_specifier
	: CHAR
	| SHORT
	| INT
	| LONG
	| SIGNED
	| UNSIGNED
	| FLOAT
	| DOUBLE
	| CONST
	| VOLATILE
	| VOID
	| struct_or_union_specifier
	| enum_specifier
	| TYPE_NAME
	;

struct_or_union_specifier
	: struct_or_union identifier '{' struct_declaration_list '}'
	| struct_or_union '{' struct_declaration_list '}'
	| struct_or_union identifier
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: type_specifier_list struct_declarator_list ';'
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: declarator
	| ':' constant_expr
	| declarator ':' constant_expr
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM identifier '{' enumerator_list '}'
	| ENUM identifier
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator
	: identifier
	| identifier '=' constant_expr
	;

declarator
	: declarator2
	| pointer declarator2
	;

declarator2
	: identifier
	| '(' declarator ')'
	| declarator2 '[' ']'
	| declarator2 '[' constant_expr ']'
	| declarator2 '(' ')'
	| declarator2 '(' parameter_type_list ')'
	| declarator2 '(' parameter_identifier_list ')'
	;

pointer
	: '*'
	| '*' type_specifier_list
	| '*' pointer
	| '*' type_specifier_list pointer
	;

type_specifier_list
	: type_specifier
	| type_specifier_list type_specifier
	;

parameter_identifier_list
	: identifier_list
	| identifier_list ',' ELIPSIS
	;

identifier_list
	: identifier
	| identifier_list ',' identifier
	;

parameter_type_list
	: parameter_list
	| parameter_list ',' ELIPSIS
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
	;

parameter_declaration
	: type_specifier_list declarator
	| type_name
	;

type_name
	: type_specifier_list
	| type_specifier_list abstract_declarator
	;

abstract_declarator
	: pointer
	| abstract_declarator2
	| pointer abstract_declarator2
	;

abstract_declarator2
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' constant_expr ']'
	| abstract_declarator2 '[' ']'
	| abstract_declarator2 '[' constant_expr ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| abstract_declarator2 '(' ')'
	| abstract_declarator2 '(' parameter_type_list ')'
	;

initializer
	: assignment_expr
	| '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	;

initializer_list
	: initializer
	| initializer_list ',' initializer
	;

statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

labeled_statement
	: identifier ':' statement
	| CASE constant_expr ':' statement
	| DEFAULT ':' statement
	;

compound_statement
	: '{' '}'
	| '{' statement_list '}'
	| '{' declaration_list '}'
	| '{' declaration_list statement_list '}'
	;

declaration_list
	: declaration
	| declaration_list declaration
	;

statement_list
	: statement
	| statement_list statement
	;

expression_statement
	: ';'
	| expr ';'
	;

selection_statement
	: IF '(' expr ')' statement
	| IF '(' expr ')' statement ELSE statement
	| SWITCH '(' expr ')' statement
	;

iteration_statement
	: WHILE '(' expr ')' statement
	| DO statement WHILE '(' expr ')' ';'
	| FOR '(' ';' ';' ')' statement
	| FOR '(' ';' ';' expr ')' statement
	| FOR '(' ';' expr ';' ')' statement
	| FOR '(' ';' expr ';' expr ')' statement
	| FOR '(' expr ';' ';' ')' statement
	| FOR '(' expr ';' ';' expr ')' statement
	| FOR '(' expr ';' expr ';' ')' statement
	| FOR '(' expr ';' expr ';' expr ')' statement
	;

jump_statement
	: GOTO identifier ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';'
	| RETURN expr ';'
	;

file
	: external_definition
	| file external_definition
	;

external_definition
	: function_definition
	| declaration
	;

function_definition
	: declarator function_body
	| declaration_specifiers declarator function_body
	;

function_body
	: compound_statement
	| declaration_list compound_statement
	;

identifier
	: IDENTIFIER
	;
%%

#include <stdio.h>

# define U(x) ((x)&0377)
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern unsigned char yytext[];
int yymorfg;
extern unsigned char *yysptr, yysbuf[];
int yytchar;
FILE *yyin, *yyout;
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
#include <stdio.h>

void count();
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
		{ comment(); }
break;
case 2:
		{ count(); return(AUTO); }
break;
case 3:
		{ count(); return(BREAK); }
break;
case 4:
		{ count(); return(CASE); }
break;
case 5:
		{ count(); return(CHAR); }
break;
case 6:
		{ count(); return(CONST); }
break;
case 7:
	{ count(); return(CONTINUE); }
break;
case 8:
	{ count(); return(DEFAULT); }
break;
case 9:
		{ count(); return(DO); }
break;
case 10:
	{ count(); return(DOUBLE); }
break;
case 11:
		{ count(); return(ELSE); }
break;
case 12:
		{ count(); return(ENUM); }
break;
case 13:
	{ count(); return(EXTERN); }
break;
case 14:
		{ count(); return(FLOAT); }
break;
case 15:
		{ count(); return(FOR); }
break;
case 16:
		{ count(); return(GOTO); }
break;
case 17:
		{ count(); return(IF); }
break;
case 18:
		{ count(); return(INT); }
break;
case 19:
		{ count(); return(LONG); }
break;
case 20:
	{ count(); return(REGISTER); }
break;
case 21:
	{ count(); return(RETURN); }
break;
case 22:
		{ count(); return(SHORT); }
break;
case 23:
	{ count(); return(SIGNED); }
break;
case 24:
	{ count(); return(SIZEOF); }
break;
case 25:
	{ count(); return(STATIC); }
break;
case 26:
	{ count(); return(STRUCT); }
break;
case 27:
	{ count(); return(SWITCH); }
break;
case 28:
	{ count(); return(TYPEDEF); }
break;
case 29:
		{ count(); return(UNION); }
break;
case 30:
	{ count(); return(UNSIGNED); }
break;
case 31:
		{ count(); return(VOID); }
break;
case 32:
	{ count(); return(VOLATILE); }
break;
case 33:
		{ count(); return(WHILE); }
break;
case 34:
	{ count(); return(check_type()); }
break;
case 35:
	{ count(); return(CONSTANT); }
break;
case 36:
	{ count(); return(CONSTANT); }
break;
case 37:
	{ count(); return(CONSTANT); }
break;
case 38:
	{ count(); return(CONSTANT); }
break;
case 39:
	{ count(); return(CONSTANT); }
break;
case 40:
	{ count(); return(CONSTANT); }
break;
case 41:
	{ count(); return(CONSTANT); }
break;
case 42:
	{ count(); return(CONSTANT); }
break;
case 43:
{ count(); return(CONSTANT); }
break;
case 44:
{ count(); return(CONSTANT); }
break;
case 45:
{ count(); return(STRING_LITERAL); }
break;
case 46:
		{ count(); return(RIGHT_ASSIGN); }
break;
case 47:
		{ count(); return(LEFT_ASSIGN); }
break;
case 48:
		{ count(); return(ADD_ASSIGN); }
break;
case 49:
		{ count(); return(SUB_ASSIGN); }
break;
case 50:
		{ count(); return(MUL_ASSIGN); }
break;
case 51:
		{ count(); return(DIV_ASSIGN); }
break;
case 52:
		{ count(); return(MOD_ASSIGN); }
break;
case 53:
		{ count(); return(AND_ASSIGN); }
break;
case 54:
		{ count(); return(XOR_ASSIGN); }
break;
case 55:
		{ count(); return(OR_ASSIGN); }
break;
case 56:
		{ count(); return(RIGHT_OP); }
break;
case 57:
		{ count(); return(LEFT_OP); }
break;
case 58:
		{ count(); return(INC_OP); }
break;
case 59:
		{ count(); return(DEC_OP); }
break;
case 60:
		{ count(); return(PTR_OP); }
break;
case 61:
		{ count(); return(AND_OP); }
break;
case 62:
		{ count(); return(OR_OP); }
break;
case 63:
		{ count(); return(LE_OP); }
break;
case 64:
		{ count(); return(GE_OP); }
break;
case 65:
		{ count(); return(EQ_OP); }
break;
case 66:
		{ count(); return(NE_OP); }
break;
case 67:
		{ count(); return(';'); }
break;
case 68:
		{ count(); return('{'); }
break;
case 69:
		{ count(); return('}'); }
break;
case 70:
		{ count(); return(','); }
break;
case 71:
		{ count(); return(':'); }
break;
case 72:
		{ count(); return('='); }
break;
case 73:
		{ count(); return('('); }
break;
case 74:
		{ count(); return(')'); }
break;
case 75:
		{ count(); return('['); }
break;
case 76:
		{ count(); return(']'); }
break;
case 77:
		{ count(); return('.'); }
break;
case 78:
		{ count(); return('&'); }
break;
case 79:
		{ count(); return('!'); }
break;
case 80:
		{ count(); return('~'); }
break;
case 81:
		{ count(); return('-'); }
break;
case 82:
		{ count(); return('+'); }
break;
case 83:
		{ count(); return('*'); }
break;
case 84:
		{ count(); return('/'); }
break;
case 85:
		{ count(); return('%'); }
break;
case 86:
		{ count(); return('<'); }
break;
case 87:
		{ count(); return('>'); }
break;
case 88:
		{ count(); return('^'); }
break;
case 89:
		{ count(); return('|'); }
break;
case 90:
		{ count(); return('?'); }
break;
case 91:
	{ count(); }
break;
case 92:
		{ /* ignore bad characters */ }
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

yywrap()
{
	return(1);
}

comment()
{
	char c, c1;

loop:
	while ((c = input()) != '*' && c != 0)
		putchar(c);

	if ((c1 = input()) != '/' && c != 0)
	{
		unput(c1);
		goto loop;
	}

	if (c != 0)
		putchar(c1);
}

int column = 0;

void count()
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
		if (yytext[i] == '\n')
			column = 0;
		else if (yytext[i] == '\t')
			column += 8 - (column % 8);
		else
			column++;

	ECHO;
}

int check_type()
{
/*
* pseudo code --- this is what it should check
*
*	if (yytext == type_name)
*		return(TYPE_NAME);
*
*	return(IDENTIFIER);
*/

/*
*	it actually will only return IDENTIFIER
*/

	return(IDENTIFIER);
}
int yyvstop[] = {
0,

92,
0,

91,
92,
0,

91,
0,

79,
92,
0,

92,
0,

85,
92,
0,

78,
92,
0,

92,
0,

73,
92,
0,

74,
92,
0,

83,
92,
0,

82,
92,
0,

70,
92,
0,

81,
92,
0,

77,
92,
0,

84,
92,
0,

39,
40,
92,
0,

39,
40,
92,
0,

71,
92,
0,

67,
92,
0,

86,
92,
0,

72,
92,
0,

87,
92,
0,

90,
92,
0,

34,
92,
0,

75,
92,
0,

76,
92,
0,

88,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
92,
0,

34,
91,
92,
0,

34,
92,
0,

68,
92,
0,

89,
92,
0,

69,
92,
0,

80,
92,
0,

66,
0,

45,
0,

52,
0,

61,
0,

53,
0,

50,
0,

58,
0,

48,
0,

59,
0,

49,
0,

60,
0,

43,
0,

1,
0,

51,
0,

44,
0,

37,
38,
39,
40,
0,

39,
40,
0,

39,
40,
0,

57,
0,

63,
0,

65,
0,

64,
0,

56,
0,

34,
0,

54,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

9,
34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

17,
34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

55,
0,

62,
0,

41,
0,

43,
0,

43,
44,
0,

44,
0,

37,
38,
39,
40,
0,

42,
0,

35,
36,
0,

47,
0,

46,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

15,
34,
0,

34,
0,

18,
34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

43,
0,

43,
44,
0,

44,
0,

42,
0,

35,
36,
0,

2,
34,
0,

34,
0,

4,
34,
0,

5,
34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

11,
34,
0,

12,
34,
0,

34,
0,

34,
0,

16,
34,
0,

19,
34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

31,
34,
0,

34,
0,

34,
0,

43,
44,
0,

3,
34,
0,

6,
34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

14,
34,
0,

34,
0,

34,
0,

22,
34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

29,
34,
0,

34,
0,

34,
0,

33,
34,
0,

34,
0,

34,
0,

10,
34,
0,

13,
34,
0,

34,
0,

21,
34,
0,

23,
34,
0,

24,
34,
0,

25,
34,
0,

26,
34,
0,

27,
34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

8,
34,
0,

34,
0,

28,
34,
0,

34,
0,

34,
0,

7,
34,
0,

20,
34,
0,

30,
34,
0,

32,
34,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,6,	1,7,	
0,0,	0,0,	1,8,	1,9,	
1,10,	1,11,	1,12,	1,13,	
1,14,	1,15,	1,16,	1,17,	
1,18,	1,19,	1,20,	1,20,	
1,20,	1,20,	1,20,	1,20,	
1,20,	1,20,	1,20,	1,21,	
1,22,	1,23,	1,24,	1,25,	
1,26,	6,50,	1,27,	8,54,	
9,55,	13,59,	1,27,	16,62,	
1,27,	23,74,	23,75,	24,76,	
25,77,	25,78,	14,60,	30,80,	
57,108,	58,0,	74,118,	78,119,	
0,0,	0,0,	18,66,	16,63,	
16,64,	1,27,	0,0,	9,56,	
1,28,	1,3,	1,29,	1,30,	
14,61,	47,106,	1,31,	1,32,	
1,33,	1,34,	1,35,	1,36,	
1,37,	18,67,	1,38,	40,97,	
45,105,	1,39,	58,57,	36,91,	
37,93,	34,86,	36,92,	1,40,	
1,41,	1,42,	1,43,	1,44,	
1,45,	31,81,	32,82,	34,87,	
1,46,	1,47,	1,48,	1,49,	
2,6,	39,96,	38,94,	42,102,	
2,8,	2,9,	43,103,	2,11,	
2,12,	2,13,	38,95,	2,15,	
2,16,	2,17,	2,18,	44,104,	
2,20,	2,20,	2,20,	2,20,	
2,20,	2,20,	2,20,	2,20,	
2,20,	2,21,	2,22,	2,23,	
2,24,	2,25,	2,26,	35,88,	
47,107,	35,89,	81,120,	58,57,	
82,121,	83,122,	7,51,	84,123,	
85,124,	86,125,	87,126,	35,90,	
88,127,	89,128,	7,51,	7,51,	
17,65,	17,65,	17,65,	17,65,	
17,65,	17,65,	17,65,	17,65,	
17,65,	17,65,	2,28,	90,129,	
2,29,	2,30,	41,98,	41,99,	
2,31,	2,32,	2,33,	2,34,	
2,35,	2,36,	2,37,	7,52,	
2,38,	91,130,	41,100,	2,39,	
7,51,	41,101,	92,131,	10,57,	
7,51,	2,40,	2,41,	2,42,	
2,43,	7,51,	2,45,	10,57,	
10,57,	33,83,	2,46,	2,47,	
2,48,	2,49,	53,0,	93,132,	
33,84,	95,133,	96,134,	98,137,	
101,142,	102,143,	7,51,	33,85,	
99,138,	103,144,	7,51,	100,140,	
7,51,	97,135,	71,71,	105,148,	
10,57,	65,109,	65,110,	103,145,	
120,157,	10,0,	53,51,	71,71,	
65,110,	10,57,	97,136,	99,139,	
100,141,	7,51,	10,57,	121,158,	
104,146,	7,53,	19,68,	104,147,	
19,69,	19,69,	19,69,	19,69,	
19,69,	19,69,	19,69,	19,69,	
19,69,	19,69,	71,71,	10,57,	
122,159,	65,109,	65,110,	10,57,	
116,155,	10,57,	114,114,	71,71,	
65,110,	19,70,	116,155,	7,51,	
111,151,	111,152,	117,156,	114,114,	
19,71,	123,160,	125,163,	111,152,	
124,161,	124,162,	10,57,	117,156,	
126,164,	19,71,	10,58,	127,165,	
19,72,	128,166,	129,167,	130,168,	
53,51,	132,169,	134,170,	135,171,	
116,155,	136,172,	114,114,	137,173,	
138,174,	19,70,	116,155,	139,175,	
111,151,	111,152,	117,156,	114,114,	
19,71,	140,176,	141,177,	111,152,	
10,57,	142,178,	143,179,	117,156,	
144,180,	19,71,	145,181,	20,68,	
19,72,	20,73,	20,73,	20,73,	
20,73,	20,73,	20,73,	20,73,	
20,73,	20,73,	20,73,	115,116,	
115,116,	115,116,	115,116,	115,116,	
115,116,	115,116,	115,116,	115,116,	
115,116,	146,182,	20,70,	147,183,	
148,184,	150,110,	154,113,	158,187,	
156,156,	20,71,	161,188,	150,110,	
154,113,	162,189,	70,115,	163,190,	
70,115,	156,156,	20,71,	70,116,	
70,116,	70,116,	70,116,	70,116,	
70,116,	70,116,	70,116,	70,116,	
70,116,	164,191,	167,192,	168,193,	
171,194,	172,195,	20,70,	173,196,	
174,197,	150,110,	154,113,	175,198,	
156,156,	20,71,	176,199,	150,110,	
154,113,	177,200,	178,201,	179,202,	
180,203,	156,156,	20,71,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	181,204,	183,205,	184,206,	
189,207,	190,208,	191,209,	192,210,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	194,211,	195,212,	
197,213,	198,214,	27,79,	199,215,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	27,79,	27,79,	
27,79,	27,79,	68,111,	68,111,	
68,111,	68,111,	68,111,	68,111,	
68,111,	68,111,	68,111,	68,111,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	186,152,	68,112,	
68,113,	200,216,	201,217,	202,218,	
186,152,	204,219,	68,113,	205,220,	
207,221,	208,222,	211,223,	218,224,	
219,225,	220,226,	221,227,	223,228,	
69,114,	225,229,	226,230,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	69,114,	0,0,	0,0,	
0,0,	0,0,	186,152,	68,112,	
68,113,	0,0,	0,0,	0,0,	
186,152,	0,0,	68,113,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
69,114,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	69,114,	72,117,	72,117,	
72,117,	72,117,	72,117,	72,117,	
72,117,	72,117,	72,117,	72,117,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	72,117,	
72,117,	72,117,	72,117,	72,117,	
72,117,	109,149,	0,0,	109,149,	
0,0,	0,0,	109,150,	109,150,	
109,150,	109,150,	109,150,	109,150,	
109,150,	109,150,	109,150,	109,150,	
149,150,	149,150,	149,150,	149,150,	
149,150,	149,150,	149,150,	149,150,	
149,150,	149,150,	0,0,	72,117,	
72,117,	72,117,	72,117,	72,117,	
72,117,	112,153,	0,0,	112,153,	
0,0,	0,0,	112,154,	112,154,	
112,154,	112,154,	112,154,	112,154,	
112,154,	112,154,	112,154,	112,154,	
151,185,	0,0,	151,185,	0,0,	
0,0,	151,186,	151,186,	151,186,	
151,186,	151,186,	151,186,	151,186,	
151,186,	151,186,	151,186,	153,154,	
153,154,	153,154,	153,154,	153,154,	
153,154,	153,154,	153,154,	153,154,	
153,154,	185,186,	185,186,	185,186,	
185,186,	185,186,	185,186,	185,186,	
185,186,	185,186,	185,186,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-95,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+4,	0,		yyvstop+8,
yycrank+-165,	0,		yyvstop+11,
yycrank+6,	0,		yyvstop+13,
yycrank+30,	0,		yyvstop+16,
yycrank+-206,	0,		yyvstop+19,
yycrank+0,	0,		yyvstop+21,
yycrank+0,	0,		yyvstop+24,
yycrank+8,	0,		yyvstop+27,
yycrank+35,	0,		yyvstop+30,
yycrank+0,	0,		yyvstop+33,
yycrank+26,	0,		yyvstop+36,
yycrank+128,	0,		yyvstop+39,
yycrank+44,	0,		yyvstop+42,
yycrank+212,	0,		yyvstop+45,
yycrank+285,	0,		yyvstop+49,
yycrank+0,	0,		yyvstop+53,
yycrank+0,	0,		yyvstop+56,
yycrank+13,	0,		yyvstop+59,
yycrank+14,	0,		yyvstop+62,
yycrank+15,	0,		yyvstop+65,
yycrank+0,	0,		yyvstop+68,
yycrank+355,	0,		yyvstop+71,
yycrank+0,	0,		yyvstop+74,
yycrank+0,	0,		yyvstop+77,
yycrank+18,	0,		yyvstop+80,
yycrank+4,	yysvec+27,	yyvstop+83,
yycrank+8,	yysvec+27,	yyvstop+86,
yycrank+120,	yysvec+27,	yyvstop+89,
yycrank+12,	yysvec+27,	yyvstop+92,
yycrank+51,	yysvec+27,	yyvstop+95,
yycrank+3,	yysvec+27,	yyvstop+98,
yycrank+1,	yysvec+27,	yyvstop+101,
yycrank+28,	yysvec+27,	yyvstop+104,
yycrank+18,	yysvec+27,	yyvstop+107,
yycrank+6,	yysvec+27,	yyvstop+110,
yycrank+86,	yysvec+27,	yyvstop+113,
yycrank+10,	yysvec+27,	yyvstop+116,
yycrank+24,	yysvec+27,	yyvstop+119,
yycrank+32,	yysvec+27,	yyvstop+122,
yycrank+4,	yysvec+27,	yyvstop+126,
yycrank+0,	0,		yyvstop+129,
yycrank+36,	0,		yyvstop+132,
yycrank+0,	0,		yyvstop+135,
yycrank+0,	0,		yyvstop+138,
yycrank+0,	0,		yyvstop+141,
yycrank+0,	yysvec+7,	0,	
yycrank+0,	0,		yyvstop+143,
yycrank+-212,	yysvec+7,	0,	
yycrank+0,	0,		yyvstop+145,
yycrank+0,	0,		yyvstop+147,
yycrank+0,	0,		yyvstop+149,
yycrank+-41,	yysvec+10,	0,	
yycrank+-71,	yysvec+10,	0,	
yycrank+0,	0,		yyvstop+151,
yycrank+0,	0,		yyvstop+153,
yycrank+0,	0,		yyvstop+155,
yycrank+0,	0,		yyvstop+157,
yycrank+0,	0,		yyvstop+159,
yycrank+0,	0,		yyvstop+161,
yycrank+172,	yysvec+17,	yyvstop+163,
yycrank+0,	0,		yyvstop+165,
yycrank+0,	0,		yyvstop+167,
yycrank+430,	0,		yyvstop+169,
yycrank+440,	yysvec+20,	yyvstop+171,
yycrank+323,	0,		0,	
yycrank+162,	0,		yyvstop+176,
yycrank+510,	0,		0,	
yycrank+0,	yysvec+20,	yyvstop+179,
yycrank+21,	0,		yyvstop+182,
yycrank+0,	0,		yyvstop+184,
yycrank+0,	0,		yyvstop+186,
yycrank+0,	0,		yyvstop+188,
yycrank+22,	0,		yyvstop+190,
yycrank+0,	yysvec+27,	yyvstop+192,
yycrank+0,	0,		yyvstop+194,
yycrank+46,	yysvec+27,	yyvstop+196,
yycrank+63,	yysvec+27,	yyvstop+198,
yycrank+50,	yysvec+27,	yyvstop+200,
yycrank+70,	yysvec+27,	yyvstop+202,
yycrank+58,	yysvec+27,	yyvstop+204,
yycrank+67,	yysvec+27,	yyvstop+206,
yycrank+53,	yysvec+27,	yyvstop+208,
yycrank+57,	yysvec+27,	yyvstop+211,
yycrank+56,	yysvec+27,	yyvstop+213,
yycrank+71,	yysvec+27,	yyvstop+215,
yycrank+90,	yysvec+27,	yyvstop+217,
yycrank+92,	yysvec+27,	yyvstop+219,
yycrank+107,	yysvec+27,	yyvstop+221,
yycrank+0,	yysvec+27,	yyvstop+223,
yycrank+109,	yysvec+27,	yyvstop+226,
yycrank+116,	yysvec+27,	yyvstop+228,
yycrank+134,	yysvec+27,	yyvstop+230,
yycrank+116,	yysvec+27,	yyvstop+232,
yycrank+129,	yysvec+27,	yyvstop+234,
yycrank+138,	yysvec+27,	yyvstop+236,
yycrank+123,	yysvec+27,	yyvstop+238,
yycrank+117,	yysvec+27,	yyvstop+240,
yycrank+128,	yysvec+27,	yyvstop+242,
yycrank+151,	yysvec+27,	yyvstop+244,
yycrank+134,	yysvec+27,	yyvstop+246,
yycrank+0,	0,		yyvstop+248,
yycrank+0,	0,		yyvstop+250,
yycrank+0,	0,		yyvstop+252,
yycrank+538,	0,		0,	
yycrank+0,	0,		yyvstop+254,
yycrank+215,	yysvec+68,	yyvstop+256,
yycrank+570,	0,		0,	
yycrank+0,	0,		yyvstop+259,
yycrank+202,	0,		yyvstop+261,
yycrank+295,	0,		0,	
yycrank+206,	yysvec+115,	yyvstop+266,
yycrank+210,	yysvec+72,	yyvstop+268,
yycrank+0,	0,		yyvstop+271,
yycrank+0,	0,		yyvstop+273,
yycrank+133,	yysvec+27,	yyvstop+275,
yycrank+158,	yysvec+27,	yyvstop+277,
yycrank+171,	yysvec+27,	yyvstop+279,
yycrank+175,	yysvec+27,	yyvstop+281,
yycrank+177,	yysvec+27,	yyvstop+283,
yycrank+193,	yysvec+27,	yyvstop+285,
yycrank+198,	yysvec+27,	yyvstop+287,
yycrank+198,	yysvec+27,	yyvstop+289,
yycrank+192,	yysvec+27,	yyvstop+291,
yycrank+201,	yysvec+27,	yyvstop+293,
yycrank+206,	yysvec+27,	yyvstop+295,
yycrank+0,	yysvec+27,	yyvstop+297,
yycrank+194,	yysvec+27,	yyvstop+300,
yycrank+0,	yysvec+27,	yyvstop+302,
yycrank+203,	yysvec+27,	yyvstop+305,
yycrank+202,	yysvec+27,	yyvstop+307,
yycrank+192,	yysvec+27,	yyvstop+309,
yycrank+197,	yysvec+27,	yyvstop+311,
yycrank+202,	yysvec+27,	yyvstop+313,
yycrank+214,	yysvec+27,	yyvstop+315,
yycrank+205,	yysvec+27,	yyvstop+317,
yycrank+205,	yysvec+27,	yyvstop+319,
yycrank+209,	yysvec+27,	yyvstop+321,
yycrank+225,	yysvec+27,	yyvstop+323,
yycrank+217,	yysvec+27,	yyvstop+325,
yycrank+225,	yysvec+27,	yyvstop+327,
yycrank+253,	yysvec+27,	yyvstop+329,
yycrank+258,	yysvec+27,	yyvstop+331,
yycrank+248,	yysvec+27,	yyvstop+333,
yycrank+548,	0,		0,	
yycrank+287,	yysvec+149,	yyvstop+335,
yycrank+585,	0,		0,	
yycrank+0,	0,		yyvstop+337,
yycrank+595,	0,		0,	
yycrank+288,	yysvec+153,	yyvstop+340,
yycrank+0,	0,		yyvstop+342,
yycrank+284,	0,		yyvstop+344,
yycrank+0,	yysvec+27,	yyvstop+347,
yycrank+252,	yysvec+27,	yyvstop+350,
yycrank+0,	yysvec+27,	yyvstop+352,
yycrank+0,	yysvec+27,	yyvstop+355,
yycrank+246,	yysvec+27,	yyvstop+358,
yycrank+260,	yysvec+27,	yyvstop+360,
yycrank+250,	yysvec+27,	yyvstop+362,
yycrank+273,	yysvec+27,	yyvstop+364,
yycrank+0,	yysvec+27,	yyvstop+366,
yycrank+0,	yysvec+27,	yyvstop+369,
yycrank+268,	yysvec+27,	yyvstop+372,
yycrank+267,	yysvec+27,	yyvstop+374,
yycrank+0,	yysvec+27,	yyvstop+376,
yycrank+0,	yysvec+27,	yyvstop+379,
yycrank+269,	yysvec+27,	yyvstop+382,
yycrank+271,	yysvec+27,	yyvstop+384,
yycrank+271,	yysvec+27,	yyvstop+386,
yycrank+287,	yysvec+27,	yyvstop+388,
yycrank+280,	yysvec+27,	yyvstop+390,
yycrank+289,	yysvec+27,	yyvstop+392,
yycrank+298,	yysvec+27,	yyvstop+394,
yycrank+299,	yysvec+27,	yyvstop+396,
yycrank+299,	yysvec+27,	yyvstop+398,
yycrank+290,	yysvec+27,	yyvstop+400,
yycrank+310,	yysvec+27,	yyvstop+402,
yycrank+0,	yysvec+27,	yyvstop+404,
yycrank+298,	yysvec+27,	yyvstop+407,
yycrank+314,	yysvec+27,	yyvstop+409,
yycrank+605,	0,		0,	
yycrank+428,	yysvec+185,	yyvstop+411,
yycrank+0,	yysvec+27,	yyvstop+414,
yycrank+0,	yysvec+27,	yyvstop+417,
yycrank+306,	yysvec+27,	yyvstop+420,
yycrank+309,	yysvec+27,	yyvstop+422,
yycrank+317,	yysvec+27,	yyvstop+424,
yycrank+309,	yysvec+27,	yyvstop+426,
yycrank+0,	yysvec+27,	yyvstop+428,
yycrank+330,	yysvec+27,	yyvstop+431,
yycrank+337,	yysvec+27,	yyvstop+433,
yycrank+0,	yysvec+27,	yyvstop+435,
yycrank+348,	yysvec+27,	yyvstop+438,
yycrank+347,	yysvec+27,	yyvstop+440,
yycrank+352,	yysvec+27,	yyvstop+442,
yycrank+385,	yysvec+27,	yyvstop+444,
yycrank+398,	yysvec+27,	yyvstop+446,
yycrank+402,	yysvec+27,	yyvstop+448,
yycrank+0,	yysvec+27,	yyvstop+450,
yycrank+395,	yysvec+27,	yyvstop+453,
yycrank+402,	yysvec+27,	yyvstop+455,
yycrank+0,	yysvec+27,	yyvstop+457,
yycrank+391,	yysvec+27,	yyvstop+460,
yycrank+393,	yysvec+27,	yyvstop+462,
yycrank+0,	yysvec+27,	yyvstop+464,
yycrank+0,	yysvec+27,	yyvstop+467,
yycrank+409,	yysvec+27,	yyvstop+470,
yycrank+0,	yysvec+27,	yyvstop+472,
yycrank+0,	yysvec+27,	yyvstop+475,
yycrank+0,	yysvec+27,	yyvstop+478,
yycrank+0,	yysvec+27,	yyvstop+481,
yycrank+0,	yysvec+27,	yyvstop+484,
yycrank+0,	yysvec+27,	yyvstop+487,
yycrank+409,	yysvec+27,	yyvstop+490,
yycrank+411,	yysvec+27,	yyvstop+492,
yycrank+405,	yysvec+27,	yyvstop+494,
yycrank+413,	yysvec+27,	yyvstop+496,
yycrank+0,	yysvec+27,	yyvstop+498,
yycrank+401,	yysvec+27,	yyvstop+501,
yycrank+0,	yysvec+27,	yyvstop+503,
yycrank+417,	yysvec+27,	yyvstop+506,
yycrank+417,	yysvec+27,	yyvstop+508,
yycrank+0,	yysvec+27,	yyvstop+510,
yycrank+0,	yysvec+27,	yyvstop+513,
yycrank+0,	yysvec+27,	yyvstop+516,
yycrank+0,	yysvec+27,	yyvstop+519,
0,	0,	0};
struct yywork *yytop = yycrank+662;
struct yysvf *yybgin = yysvec+1;
unsigned char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,011 ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,'"' ,01  ,01  ,01  ,01  ,047 ,
01  ,01  ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'E' ,'A' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,01  ,0134,01  ,01  ,'G' ,
01  ,'A' ,'A' ,'A' ,'A' ,'E' ,'A' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'v' ,'G' ,
'X' ,'G' ,'G' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
unsigned char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*
 * (c) Copyright 1990, OPEN SOFTWARE FOUNDATION, INC.
 * ALL RIGHTS RESERVED
 */
/*
 * OSF/1 Release 1.0
*/
/*
#
# IBM CONFIDENTIAL
# Copyright International Business Machines Corp. 1989
# Unpublished Work
# All Rights Reserved
# Licensed Material - Property of IBM
#
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# 
*/
/* @(#)ncform	1.3  com/lib/l,3.1,8951 9/7/89 18:48:47 */
int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
unsigned char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
unsigned char yysbuf[YYLMAX];
unsigned char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	unsigned char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
            if (yylastch >= yytext + YYLMAX) {
                fprintf(yyout, "Maximum token length exceeded\n");
                yytext[YYLMAX] = 0;
                return 0;
            }
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( yyt > yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if(yyt < yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
                if (yyleng >= YYLMAX) {
                    fprintf(yyout, "Maximum token length exceeded\n");
                    yytext[YYLMAX] = 0;
                    return 0;
                }
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}


extern int column;

yyerror(s)
char *s;
{
	fflush(stdout);
	printf("\n%*s\n%*s\n", column, "^", column, s);
}

main()
{
	int yyparse();

	yyin = stdin;
        yyout = stdout;
	return(yyparse());
}
