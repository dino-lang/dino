%local {

#include <stdio.h>

static char id[100];
static int idlen;
static int lineno;

#define yyslex() fgetc (stdin)

#include "oberon2-kw.c"

}

%expect 30
%scanner
%%
program :
        | program lexema
lexema : {idlen = 0;} ident             {return KR_find_keyword (id, idlen);}
       | number_character  {if ($1) return CHARACTER; else return NUMBER;}
       | string            {return STRING;}
       | '+'               {return '+';}
       | ":="              {return ASSIGN;}
       | '-'               {return '-';}
       | '@'               {return '@';}
       | '*'               {return '*';}
       | '='               {return '=';}
       | '/'               {return '/';}
       | '#'               {return '#';}
       | '~'               {return '~';}
       | '<'               {return '<';}
       | '&'               {return '&';}
       | '>'               {return '>';}
       | '.'               {return '.';}
       | "<="              {return LE;}
       | ','               {return ',';}
       | ">="              {return GE;}
       | ';'               {return ';';}
       | ".."              {return RANGE;}
       | '|'               {return '|';}
       | ':'               {return ':';}
       | '('               {return '(';}
       | ')'               {return ')';}
       | '['               {return '[';}
       | ']'               {return ']';}
       | '{'               {return '{';}
       | '}'               {return '}';}
       | comment
       | space
       | error
space : ' ' | '\t' | '\n'  {lineno++;}
ident : letter {id[idlen++]=yysprev_char;}
        (letter {id[idlen++]=yysprev_char;}
         | digit {id[idlen++]=yysprev_char;})*
number_character : {$$ = 0;} digit (hexDigit)*
                   (['H'|'X' {$$ = 1;}] | '.' (digit)* [ScaleFactor])
ScaleFactor : ('E' | 'D') ['+' | '-'] digit (digit)*
hexDigit : digit | 'A'-'F'
digit : '0'-'9'
letter : 'A'-'Z'|'a'-'z'
string : '"' ('\0'->'"' | '"'<-'\377')* '"'
       | '\'' ('\0'->'\'' | '\''<-'\377')* '\''
comment : "(*" ('\0'-'\377' | comment)*  "*)"

%%

int yyserror (char *s)
{
  fprintf (stderr, "illegal code %d on line %d\n", yyschar, lineno);
}

