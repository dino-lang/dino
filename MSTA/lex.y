%local {

#include <stdio.h>
#include <string.h>

#define IDENTIFIER 300
#define NUMBER     301
#define STRING     302


static FILE *input;

static char lexema [100];
static int lexema_index;
static int lineno;

}

%scanner
%%
program :
        | program {lexema_index=0;} lexema
        ;
lexema : identifier {lexema [lexema_index++] = 0;return IDENTIFIER;}
       | number     {lexema [lexema_index++] = 0;return NUMBER;}
       | string     {lexema [lexema_index++] = 0;return STRING;}
       | comment
       | space
       | error
       ;

space : ' ' | '\t' | '\n' {lineno++;}

identifier : identifier (letter | digit)
           | letter
           ;

letter :  ('a'-'z' | 'A' - 'Z') {lexema[lexema_index++] = yysprev_char;}
       ;

digit :  '0' - '9'  {lexema[lexema_index++] = yysprev_char;}
      ;

number : number digit
       | digit
       ;

string : '"' (('\1' -> '"' | '"' <- '\377')
                       {lexema[lexema_index++] = yysprev_char;} ) * '"'
       ;

comment : "/*" '\0' - '\377' * "*/"  /* Conflict shift/reduce on / after * */
        ;

%%

#ifdef __cplusplus

class scanner: public yyscanner
{
public:
  inline int yyslex (void);
  void yyserror (const char *message);
  scanner (int &error_flag) : yyscanner (error_flag) {}
};

int scanner::yyslex (void)
{
  return fgetc (input);
}

void scanner::yyserror (const char *message)
{
  fprintf (stderr, "illegal code %d on line %d\n", yyschar, lineno);
}

static scanner *scan_ptr;

#else

int yyslex (void)
{
  return fgetc (input);
}

yyserror (const char *message)
{
  fprintf (stderr, "illegal code %d on line %d\n", yyschar, lineno);
}
#endif

void
main (int argc, char **argv)
{
  int token;
  int error_flag;

  if (argc != 2)
    {
      fprintf (stderr, "Usage: lex file\n");
      exit (1);
    }
  if (strcmp (argv[1], "-") == 0)
    input = stdin;
  else
    input = fopen (argv[1], "rb");
  if (input == NULL)
    {
      perror (argv[1]);
      exit (1);
    }
#ifdef __cplusplus
  scan_ptr = new scanner (error_flag);
  if (error_flag)
    {
      fprintf (stderr, "no memory for object scanner");
      exit (1);
    }
  lineno = 1;
  while ((token = scan_ptr->yylex ()) > 0)
    fprintf (stderr, "%d - %s\n", token, lexema);
#else
  lineno = 1;
  yylex_start (&error_flag);
  if (error_flag)
    {
      fprintf (stderr, "no memory for scanner arrays");
      exit (1);
    }
  while ((token = yylex ()) > 0)
    fprintf (stderr, "%d - %s\n", token, lexema);
#endif
  exit (0);
}
