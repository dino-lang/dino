%export {

#include "oberon2-lex.c"

}

%token ARRAY BEGIN BY CASE CONST DIV DO ELSE ELSIF END EXIT FOR
%token IF IMPORT IN IS LOOP MOD MODULE NIL OF OR POINTER PROCEDURE
%token RECORD REPEAT RETURN THEN TO TYPE UNTIL VAR WHILE WITH
%token ASSIGN /* := */
%token LE     /* <= */
%token GE     /* >= */
%token RANGE  /* .. */
%token NUMBER
%token STRING
%token CHARACTER
%token IDENT

%token GUARD_IS_NEXT QUALIDENT_DOT_IS_NEXT

%%
Module : MODULE IDENT ';' [ImportList] DeclSeq
         [BEGIN StatementSeq] END IDENT '.'
ImportList : IMPORT (IDENT [ASSIGN IDENT] / ',' ) ';'
DeclSeq : (CONST (ConstDecl ';')* | TYPE (TypeDecl ';')* | VAR (VarDecl ';')*)*
          (ProcDecl ';' | ForwardDecl ';')*
ConstDecl : IdentDef '=' ConstExpr
TypeDecl : IdentDef '=' Type
VarDecl : IdentList ':' Type
ProcDecl : PROCEDURE [Receiver] IdentDef [FormalPars] ';' DeclSeq
           [BEGIN StatementSeq] END IDENT
ForwardDecl : PROCEDURE '@' [Receiver] IdentDef [FormalPars]
FormalPars : '(' [FPSection / ';'] ')' [ ':' Qualident]
FPSection : [VAR] (IDENT / ',') ':' Type
Receiver : '(' [VAR] IDENT ':' IDENT ')'
Type : Qualident
     | ARRAY '[' (ConstExpr/',') ']' OF Type
     | RECORD ['(' Qualident ')'] (FieldList / ';') END
     | POINTER TO Type
     | PROCEDURE [FormalPars]
FieldList : [IdentList ':' Type]
StatementSeq : Statement / ';'
Statement : [   Designator ASSIGN Expr
              | Designator [Choice '(' [ExprList] ')']
              | IF Expr THEN StatementSeq (ELSIF Expr THEN StatementSeq)*
                [ELSE StatementSeq] END
              | CASE Expr OF (Case / '|') [ELSE StatementSeq] END
              | WHILE Expr DO StatementSeq END
              | REPEAT StatementSeq UNTIL Expr
              | FOR IDENT ASSIGN Expr TO Expr [BY ConstExpr]
                DO StatementSeq END
              | LOOP StatementSeq END
              | WITH Guard DO StatementSeq ('|' Guard DO StatementSeq)*
                [ELSE StatementSeq] END
              | EXIT
              | RETURN
            ]
Case : [(CaseLabels / ',') ':' StatementSeq]
CaseLabels : ConstExpr [RANGE ConstExpr]
Guard : Qualident ':' Qualident
ConstExpr : Expr
Expr : SimpleExpr [Relation SimpleExpr]
SimpleExpr : ['+' | '-'] (Term / AddOp)
Term : Factor / MulOp
Factor : Designator [Choice '(' [ExprList] ')'] | NUMBER | CHARACTER | STRING
       | NIL | Set | '(' Expr ')' | '~' Factor
Set : '{' [Element / ','] '}'
Element : Expr [RANGE Expr]
Relation : '=' | '#' | '<' | LE | '>' | GE | IN | IS
AddOp : '+' | '-' | OR
MulOp : '*' | '/' | DIV | MOD | '&'
Designator : Qualident ( '.' IDENT | '['ExprList']'
                       | '@' | Choice GUARD_IS_NEXT '('Qualident')')*
Choice :
ExprList : Expr / ','
IdentList : IdentDef / ','
Qualident : IDENT Choice2 [QUALIDENT_DOT_IS_NEXT '.' IDENT]
IdentDef : IDENT ['*' | '-']
Choice2:

%%

yyerror (const char *s)
{
  fprintf (stderr, "syntax error on line %d\n", lineno);
}

void
main ()
{
  int error_code;

  lineno = 1;
  yylex_start (&error_code);
  if (error_code)
    {
      fprintf (stderr, "cann't create lexer\n");
      exit (1);
    }
  fprintf (stderr, "code = %d\n", yyparse ());
  exit (0);
}
