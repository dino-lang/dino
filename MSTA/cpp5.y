%{

    /* Copyright (C) 1989-1991 James A. Roskind, All rights reserved. 
    This grammar was developed  and  written  by  James  A.  Roskind. 
    Copying  of  this  grammar  description, as a whole, is permitted 
    providing this notice is intact and applicable  in  all  complete 
    copies.   Translations as a whole to other parser generator input 
    languages  (or  grammar  description  languages)   is   permitted 
    provided  that  this  notice is intact and applicable in all such 
    copies,  along  with  a  disclaimer  that  the  contents  are   a 
    translation.   The reproduction of derived text, such as modified 
    versions of this grammar, or the output of parser generators,  is 
    permitted,  provided  the  resulting  work includes the copyright 
    notice "Portions Copyright (c)  1989,  1990  James  A.  Roskind". 
    Derived products, such as compilers, translators, browsers, etc., 
    that  use  this  grammar,  must also provide the notice "Portions 
    Copyright  (c)  1989,  1990  James  A.  Roskind"  in   a   manner 
    appropriate  to  the  utility,  and in keeping with copyright law 
    (e.g.: EITHER displayed when first invoked/executed; OR displayed 
    continuously on display terminal; OR via placement in the  object 
    code  in  form  readable in a printout, with or near the title of 
    the work, or at the end of the file).  No royalties, licenses  or 
    commissions  of  any  kind are required to copy this grammar, its 
    translations, or derivative products, when the copies are made in 
    compliance with this notice. Persons or corporations that do make 
    copies in compliance with this notice may charge  whatever  price 
    is  agreeable  to  a  buyer, for such copies or derivative works. 
    THIS GRAMMAR IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESS  OR 
    IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED 
    WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR 
    PURPOSE.

    James A. Roskind
    Independent Consultant
    516 Latania Palm Drive
    Indialantic FL, 32903
    (407)729-4348
    jar@hq.ileaf.com


    ---end of copyright notice---

MOTIVATION-

My  goal  is  to  see  software  developers  adopt  this grammar as a 
standard until such time as a better  standard  is  accessible.   The 
only  way  to  get it to become a standard, is to be sure that people 
know that derivations are based on a specific work.   The  intent  of 
releasing  this  grammar is to provide a publicly accessible standard 
grammar for C++.  The intent of the  copyright  notice  is  to  allow 
arbitrary  commercial  and non-commercial use of the grammar, as long 
as reference is given to the original standard.  Without reference to 
a specific standard, many alternative  grammars  would  develop.   By 
referring  to  the  standard,  this grammar is given publicity, which 
should lead to further use in compatible products and  systems.   The 
benefits  of  such  a  standard  to  commercial  products  (browsers, 
beautifiers, translators, compilers, ...) should be  obvious  to  the 
developers,  in  that  other compatible products will emerge, and the 
value of all conforming products  will  rise.   Most  developers  are 
aware  of  the  value  of  acquiring  a fairly complete grammar for a 
language, and the copyright notice  (and  the  resulting  affiliation 
with my work) should not be too high a price to pay.  By copyrighting 
this  grammar,  I have some minor control over what this standard is, 
and I can (hopefully) keep it from degrading without my approval.   I 
will  consistently  attempt  to  provide  upgraded  grammars that are 
compliant  with  the  current  art,  and  the  ANSI   C++   Committee 
recommendation  in  particular.   A developer is never prevented from 
modifying the grammar to improve it in  whatever  way  is  seen  fit.  
There  is  also  no  restriction on the sale of copies, or derivative 
works, providing the requests in the copyright notice are satisfied.

If you are not "copying" my work, but  are  rather  only  abstracting 
some  of  the  standard,  an acknowledgment with references to such a 
standard would be appreciated.  Specifically,  agreements  with  this 
standard  as  to  the  resolution  of otherwise ambiguous constructs, 
should be noted.

Simply put: "make whatever use you would like  of  the  grammar,  but 
include  the  ``portions  Copyright  ...''  as  a  reference  to this 
standard."


*/


/* Last modified 7/4/91, Version 2.0  */

/* File CPP5.Y is translated by YACC to Y.TAB.C */

/*  ACKNOWLEDGMENT: Without Bjarne Stroustrup and his many co-workers 
at Bell Labs, there would be no C++ Language for which to  provide  a 
syntax  description. Bjarne has also been especially helpful and open 
in discussions, and by permitting me to review  his  texts  prior  to 
their publication, allowed me a wonderful vantage point of clarity.

Without  the effort expended by the ANSI C standardizing committee, I 
would have been lost.  Although the ANSI C standard does not  include 
a  fully disambiguated syntax description, the committee has at least 
provided most of the disambiguating rules in  narratives.   This  C++ 
grammar  is intended to be a superset of an ANSI C compatible grammar 
that is provided in an related file.

Several reviewers have also  recently  critiqued  this  grammar,  the 
related  C  grammar,  and  or  assisted  in  discussions  during it's 
preparation.  These reviewers are certainly not responsible  for  the 
errors  I  have committed here, but they are responsible for allowing 
me  to  provide  fewer  errors.   These  colleagues  include:   Bruce 
Blodgett,  Mark Langley, Joe Fialli, Greg Perkins, Ron Guilmette, and 
Eric Krohn. */

/* Required fixes from last release :

done: 0) Allow direct call to destructors

done: 1) Allow placement of declarations in labeled statements.   The 
easiest  fix involves using a larger variance from the C grammar, and 
simply making "statement" include declarations.  Note that it  should 
also  be  legal  for  declarations  to  be  in  the  branches  of  if 
statements, as long as there is no other code in the block (I think).  
Consider:

        ...
        {
           if (0 == a)
                int b=5;
           else
                int c=4;
         }

1) template support: Not  done:  pending  syntax  specification  from 
ANSI.  (This looks like a major effort, as ANSI has decided to extend 
the  "TYPEDEFname"-feedback-to-the-lexer-hack  to  support   template 
names as a new kind of terminal token.)

2)  exception  handling:  Not done: pending syntax specification from 
ANSI (but it doesn't look hard)

done: 3) Support nested types, including identifier::name,  where  we 
realize  that  identifier was a hidden type.  Force the lexer to keep 
pace in this situation.   This  will  require  an  extension  of  the 
yacc-lex feedback loop.

done: 4) Support nested types even when derivations are used in class 
definitions.

5)  Provide  advanced  tutorial  on  YACC  conflicts:  almost done in 
documentation about machine generated documentation.

done: 6) Allow declaration specifiers to be left out of  declarations 
at file and structure scope so that operator conversion functions can 
be  declared and/or defined.  Note that checking to see that it was a 
function type that does not require declaration_specifiers is  now  a 
constraint  check,  and  not  a  syntax  issue.  Within function body 
scopes, declaration specifiers are required, and this is critical  to 
distinguishing expressions.


*/

%}

/*

Interesting ambiguity:
Usually
        typename ( typename2 ) ...
or
        typename ( typename2 [4] ) ...
etc.
is a redeclaration of typename2.

Inside  a structure elaboration, it is sometimes the declaration of a 
constructor!  Note, this only  counts  if  typename  IS  the  current 
containing  class name. (Note this can't conflict with ANSI C because 
ANSI C would call it a redefinition, but  claim  it  is  semantically 
illegal because you can't have a member declared the same type as the 
containing struct!) Since the ambiguity is only reached when a ';' is 
found,   there  is  no  problem  with  the  fact  that  the  semantic 
interpretation  is  providing  the  true  resolution.   As  currently 
implemented, the constructor semantic actions must be able to process 
an  ordinary  declaration.  I may reverse this in the future, to ease 
semantic implementation.

*/



/*

INTRO TO ANSI C GRAMMAR (provided in a separate file):

The refined grammar resolves several typedef ambiguities in the draft 
proposed ANSI C standard syntax down to 1 shift/reduce  conflict,  as 
reported by a YACC process.  Note that the one shift reduce conflicts 
is  the  traditional  if-if-else conflict that is not resolved by the 
grammar.  This ambiguity can be removed using the method described in 
the Dragon Book (2nd edition), but this does  not  appear  worth  the 
effort.

There  was quite a bit of effort made to reduce the conflicts to this 
level, and an additional effort was made to make  the  grammar  quite 
similar  to  the  C++ grammar being developed in parallel.  Note that 
this grammar resolves the following ANSI C ambiguities:

ANSI C section 3.5.6, "If the [typedef  name]  is  redeclared  at  an 
inner  scope,  the  type specifiers shall not be omitted in the inner 
declaration".  Supplying type specifiers prevents consideration of  T 
as a typedef name in this grammar.  Failure to supply type specifiers 
forced the use of the TYPEDEFname as a type specifier.  This is taken 
to an (unnecessary) extreme by this implementation.  The ambiguity is 
only  a  problem  with  the first declarator in a declaration, but we 
restrict  ALL  declarators  whenever  the  users  fails  to   use   a 
type_specifier.

ANSI C section 3.5.4.3, "In a parameter declaration, a single typedef 
name  in  parentheses  is  taken  to  be  an abstract declarator that 
specifies a function  with  a  single  parameter,  not  as  redundant 
parentheses  around  the  identifier".  This is extended to cover the 
following cases:

typedef float T;
int noo(const (T[5]));
int moo(const (T(int)));
...

Where again the '(' immediately to the left of 'T' is interpreted  as 
being  the  start  of  a  parameter type list, and not as a redundant 
paren around a redeclaration of T.  Hence an equivalent code fragment 
is:

typedef float T;
int noo(const int identifier1 (T identifier2 [5]));
int moo(const int identifier1 (T identifier2 (int identifier3)));
...

*/


%{
/*************** Includes and Defines *****************************/
#define YYDEBUG_LEXER_TEXT (yylval) /* our lexer loads this up each time.
				     We are telling the graphical debugger
				     where to find the spelling of the 
				     tokens.*/
#define YYDEBUG 1        /* get the pretty debugging code to compile*/
#define YYSTYPE  char *  /* interface with flex: should be in header file */

/*************** Standard ytab.c continues here *********************/
%}

/*************************************************************************/


/* This group is used by the C/C++ language parser */
%token AUTO            DOUBLE          INT             STRUCT
%token BREAK           ELSE            LONG            SWITCH
%token CASE            ENUM            REGISTER        TYPEDEF
%token CHAR            EXTERN          RETURN          UNION
%token CONST           FLOAT           SHORT           UNSIGNED
%token CONTINUE        FOR             SIGNED          VOID
%token DEFAULT         GOTO            SIZEOF          VOLATILE
%token DO              IF              STATIC          WHILE

/* The following are used in C++ only.  ANSI C would call these IDENTIFIERs */
%token NEW             DELETE
%token THIS
%token OPERATOR
%token CLASS
%token PUBLIC          PROTECTED       PRIVATE
%token VIRTUAL         FRIEND
%token INLINE          OVERLOAD

/* ANSI C Grammar suggestions */
%token IDENTIFIER              STRINGliteral
%token FLOATINGconstant        INTEGERconstant        CHARACTERconstant
%token OCTALconstant           HEXconstant

/* New Lexical element, whereas ANSI C suggested non-terminal */
%token TYPEDEFname

/* Multi-Character operators */
%token  ARROW            /*    ->                              */
%token  ICR DECR         /*    ++      --                      */
%token  LSH RSH          /*    <<      >>                      */
%token  LE GE EQ NE      /*    <=      >=      ==      !=      */
%token  ANDAND OROR      /*    &&      ||                      */
%token  ELLIPSIS         /*    ...                             */
                 /* Following are used in C++, not ANSI C        */
%token  CLCL             /*    ::                              */
%token  DOTstar ARROWstar/*    .*       ->*                    */

/* modifying assignment operators */
%token MULTassign  DIVassign    MODassign   /*   *=      /=      %=      */
%token PLUSassign  MINUSassign              /*   +=      -=              */
%token LSHassign    RSHassign               /*   <<=     >>=             */
%token ANDassign   ERassign     ORassign    /*   &=      ^=      |=      */

/*************************************************************************/

%start translation_unit

/*************************************************************************/

%%

/*********************** CONSTANTS *********************************/
constant:
        INTEGERconstant
        | FLOATINGconstant
        /*  We  are not including ENUMERATIONconstant here because we 
          are treating it like a variable with a type of "enumeration 
          constant".  */
        | OCTALconstant
        | HEXconstant
        | CHARACTERconstant
        ;

string_literal_list:
                STRINGliteral
                | string_literal_list STRINGliteral
                ;


/************************* EXPRESSIONS ********************************/


    /* Note that I provide  a  "scope_opt_identifier"  that  *cannot* 
    begin  with ::.  This guarantees we have a viable declarator, and 
    helps to disambiguate :: based uses in the grammar.  For example:

            ...
            {
            int (* ::b()); // must be an expression
            int (T::b); // officially a declaration, which fails on constraint grounds

    This *syntax* restriction reflects the current syntax in the ANSI 
    C++ Working Papers.   This  means  that  it  is  *incorrect*  for 
    parsers to misparse the example:

            int (* ::b()); // must be an expression

    as a declaration, and then report a constraint error.

    In contrast, declarations such as:

        class T;
        class A;
        class B;
        main(){
              T( F());  // constraint error: cannot declare local function
              T (A::B::a); // constraint error: cannot declare member as a local value

    are  *parsed*  as  declarations,  and *then* given semantic error 
    reports.  It is incorrect for a parser to "change its mind" based 
    on constraints.  If your C++ compiler claims  that  the  above  2 
    lines are expressions, then *I* claim that they are wrong. */

paren_identifier_declarator:
        scope_opt_identifier
        | scope_opt_complex_name
        | '(' paren_identifier_declarator ')'
        ;


    /* Note that CLCL IDENTIFIER is NOT part of scope_opt_identifier, 
    but  it  is  part of global_opt_scope_opt_identifier.  It is ONLY 
    valid for referring to an identifier, and NOT valid for declaring 
    (or importing an external declaration of)  an  identifier.   This 
    disambiguates  the  following  code,  which  would  otherwise  be 
    syntactically and semantically ambiguous:

            class base {
                static int i; // element i;
                float member_function(void);
                };
            base i; // global i
            float base::member_function(void) {
                i; // refers to static int element "i" of base
                ::i; // refers to global "i", with type "base"
                    {
                    base :: i; // import of global "i", like "base (::i);"?
                                // OR reference to global??
                    }
                }
        */

primary_expression:
        global_opt_scope_opt_identifier
        | global_opt_scope_opt_complex_name
        | THIS   /* C++, not ANSI C */
        | constant
        | string_literal_list
        | '(' comma_expression ')'
        ;


    /* I had to disallow struct, union, or enum  elaborations  during 
    operator_function_name.   The  ANSI  C++  Working  paper is vague 
    about whether this should be part of the syntax, or a constraint.  
    The ambiguities that resulted were more than LALR  could  handle, 
    so  the  easiest  fix was to be more specific.  This means that I 
    had to in-line expand type_specifier_or_name far  enough  that  I 
    would  be  able to exclude elaborations.  This need is what drove 
    me to distinguish a whole series of tokens based on whether  they 
    include elaborations:

         struct A { ... }

    or simply a reference to an aggregate or enumeration:

         enum A

    The  latter,  as  well  an  non-aggregate  types are what make up 
    non_elaborating_type_specifier */

    /* Note that the following does not include  type_qualifier_list. 
    Hence,   whenever   non_elaborating_type_specifier  is  used,  an 
    adjacent rule is supplied containing type_qualifier_list.  It  is 
    not  generally  possible  to  know  immediately  (i_e., reduce) a 
    type_qualifier_list, as a TYPEDEFname that follows might  not  be 
    part of a type specifier, but might instead be "TYPEDEFname ::*".  
    */

non_elaborating_type_specifier:
        sue_type_specifier
        | basic_type_specifier
        | typedef_type_specifier

        | basic_type_name
        | TYPEDEFname
        | global_or_scoped_typedefname
        ;


    /*  The  following  introduces  MANY  conflicts.   Requiring  and 
    allowing '(' ')' around the `type' when the type is complex would 
    help a lot. */

operator_function_name:
        OPERATOR any_operator
        | OPERATOR type_qualifier_list            operator_function_ptr_opt
        | OPERATOR non_elaborating_type_specifier operator_function_ptr_opt
        ;


    /* The following causes several ambiguities on *  and  &.   These 
    conflicts  would also be removed if parens around the `type' were 
    required in the derivations for operator_function_name */

    /*  Interesting  aside:  The  use  of  right  recursion  in   the 
    production  for  operator_function_ptr_opt gives both the correct 
    parsing, AND removes a conflict!   Right  recursion  permits  the 
    parser  to  defer  reductions  (a.k.a.:  delay  resolution),  and 
    effectively make a second pass! */

operator_function_ptr_opt:
        /* nothing */
        | unary_modifier        operator_function_ptr_opt
        | asterisk_or_ampersand operator_function_ptr_opt
        ;


    /* List of operators we can overload */
any_operator:
        '+'
        | '-'
        | '*'
        | '/'
        | '%'
        | '^'
        | '&'
        | '|'
        | '~'
        | '!'
        | '<'
        | '>'
        | LSH
        | RSH
        | ANDAND
        | OROR
        | ARROW
        | ARROWstar
        | '.'
        | DOTstar
        | ICR
        | DECR
        | LE
        | GE
        | EQ
        | NE
        | assignment_operator
        | '(' ')'
        | '[' ']'
        | NEW
        | DELETE
        | ','
        ;


    /* The following production for type_qualifier_list was specially 
    placed BEFORE the definition of postfix_expression to  resolve  a 
    reduce-reduce    conflict    set    correctly.    Note   that   a 
    type_qualifier_list is only used  in  a  declaration,  whereas  a 
    postfix_expression is clearly an example of an expression.  Hence 
    we  are helping with the "if it can be a declaration, then it is" 
    rule.  The reduce conflicts are on ')', ',' and '='.  Do not move 
    the following productions */

type_qualifier_list_opt:
        /* Nothing */
        | type_qualifier_list
        ;


    /*  Note  that  the next set of productions in this grammar gives 
    post-increment a higher precedence that pre-increment.   This  is 
    not  clearly  stated  in  the  C++  Reference manual, and is only 
    implied by the grammar in the ANSI C Standard. */

    /* I *DON'T* use  argument_expression_list_opt  to  simplify  the 
    grammar  shown  below.   I am deliberately deferring any decision 
    until    *after*     the     closing     paren,     and     using 
    "argument_expression_list_opt" would commit prematurely.  This is 
    critical to proper conflict resolution. */

    /*  The  {}  in  the following rules allow the parser to tell the 
    lexer to search for the member name  in  the  appropriate  scope, 
    much the way the CLCL operator works.*/

postfix_expression:
        primary_expression
        | postfix_expression '[' comma_expression ']'
        | postfix_expression '(' ')'
        | postfix_expression '(' argument_expression_list ')'
        | postfix_expression {} '.'   member_name
        | postfix_expression {} ARROW member_name
        | postfix_expression ICR
        | postfix_expression DECR

                /* The next 4 rules are the source of cast ambiguity */
        | TYPEDEFname                  '(' ')'
        | global_or_scoped_typedefname '(' ')'
        | TYPEDEFname                  '(' argument_expression_list ')'
        | global_or_scoped_typedefname '(' argument_expression_list ')'
        | basic_type_name '(' assignment_expression ')'
            /* If the following rule is added to the  grammar,  there 
            will  be 3 additional reduce-reduce conflicts.  They will 
            all be resolved in favor of NOT using the following rule, 
            so no harm will be done.   However,  since  the  rule  is 
            semantically  illegal  we  will  omit  it  until  we  are 
            enhancing the grammar for error recovery */
/*      | basic_type_name '(' ')'  /* Illegal: no such constructor*/
        ;


    /* The last two productions in the next set are questionable, but 
    do not induce any conflicts.  I need to ask X3J16 :  Having  them 
    means that we have complex member function deletes like:

          const unsigned int :: ~ const unsigned int
    */

member_name:
        scope_opt_identifier
        | scope_opt_complex_name
        | basic_type_name CLCL '~' basic_type_name  /* C++, not ANSI C */

        | declaration_qualifier_list  CLCL '~'   declaration_qualifier_list
        | type_qualifier_list         CLCL '~'   type_qualifier_list
        ;

argument_expression_list:
        assignment_expression
        | argument_expression_list ',' assignment_expression
        ;

unary_expression:
        postfix_expression
        | ICR  unary_expression
        | DECR unary_expression
        | asterisk_or_ampersand cast_expression
        | '-'                   cast_expression
        | '+'                   cast_expression
        | '~'                   cast_expression
        | '!'                   cast_expression
        | SIZEOF unary_expression
        | SIZEOF '(' type_name ')'
        | allocation_expression
        ;


    /* Note that I could have moved the  newstore  productions  to  a 
    lower  precedence  level  than  multiplication  (binary '*'), and 
    lower than bitwise AND (binary '&').  These moves  are  the  nice 
    way  to  disambiguate a trailing unary '*' or '&' at the end of a 
    freestore expression.  Since the freestore expression (with  such 
    a  grammar  and  hence  precedence  given)  can never be the left 
    operand of a binary '*' or '&', the ambiguity would  be  removed. 
    These  problems  really  surface when the binary operators '*' or 
    '&' are overloaded, but this must be syntactically  disambiguated 
    before the semantic checking is performed...  Unfortunately, I am 
    not  creating  the language, only writing a grammar that reflects 
    its specification, and  hence  I  cannot  change  its  precedence 
    assignments.   If  I  had  my  druthers,  I would probably prefer 
    surrounding the type with parens all the time, and  avoiding  the 
    dangling * and & problem all together.*/

       /* Following are C++, not ANSI C */
allocation_expression:
        global_opt_scope_opt_operator_new                                    '(' type_name ')'
                operator_new_initializer_opt

        | global_opt_scope_opt_operator_new '(' argument_expression_list ')' '(' type_name ')'
                operator_new_initializer_opt

                /* next two rules are the source of * and & ambiguities */
        | global_opt_scope_opt_operator_new                                  operator_new_type
        | global_opt_scope_opt_operator_new '(' argument_expression_list ')' operator_new_type
        ;


       /* Following are C++, not ANSI C */
global_opt_scope_opt_operator_new:
        NEW
        | global_or_scope NEW
        ;

operator_new_type:
        type_qualifier_list              operator_new_declarator_opt
                        operator_new_initializer_opt

        | non_elaborating_type_specifier operator_new_declarator_opt
                        operator_new_initializer_opt
        ;

    
    /*  Right  recursion  is critical in the following productions to 
    avoid a conflict on TYPEDEFname */

operator_new_declarator_opt:
        /* Nothing */
        | operator_new_array_declarator
        | asterisk_or_ampersand operator_new_declarator_opt
        | unary_modifier        operator_new_declarator_opt
        ;

operator_new_array_declarator:
                                        '['                  ']'
        |                               '[' comma_expression ']'
        | operator_new_array_declarator '[' comma_expression ']'
        ;

operator_new_initializer_opt:
        /* Nothing */
        | '('                          ')'
        | '(' argument_expression_list ')'
        ;

cast_expression:
        unary_expression
        | '(' type_name ')' cast_expression
        ;


    /* Following are C++, not ANSI C */
deallocation_expression:
        cast_expression
        | global_opt_scope_opt_delete deallocation_expression
        | global_opt_scope_opt_delete '[' comma_expression ']' deallocation_expression  /* archaic C++, what a concept */
        | global_opt_scope_opt_delete '[' ']' deallocation_expression
        ;


    /* Following are C++, not ANSI C */
global_opt_scope_opt_delete:
        DELETE
        | global_or_scope DELETE
        ;


    /* Following are C++, not ANSI C */
point_member_expression:
        deallocation_expression
        | point_member_expression DOTstar  deallocation_expression
        | point_member_expression ARROWstar  deallocation_expression
        ;

multiplicative_expression:
        point_member_expression
        | multiplicative_expression '*' point_member_expression
        | multiplicative_expression '/' point_member_expression
        | multiplicative_expression '%' point_member_expression
        ;

additive_expression:
        multiplicative_expression
        | additive_expression '+' multiplicative_expression
        | additive_expression '-' multiplicative_expression
        ;

shift_expression:
        additive_expression
        | shift_expression LSH additive_expression
        | shift_expression RSH additive_expression
        ;

relational_expression:
        shift_expression
        | relational_expression '<' shift_expression
        | relational_expression '>' shift_expression
        | relational_expression LE  shift_expression
        | relational_expression GE  shift_expression
        ;

equality_expression:
        relational_expression
        | equality_expression EQ relational_expression
        | equality_expression NE relational_expression
        ;

AND_expression:
        equality_expression
        | AND_expression '&' equality_expression
        ;

exclusive_OR_expression:
        AND_expression
        | exclusive_OR_expression '^' AND_expression
        ;

inclusive_OR_expression:
        exclusive_OR_expression
        | inclusive_OR_expression '|' exclusive_OR_expression
        ;

logical_AND_expression:
        inclusive_OR_expression
        | logical_AND_expression ANDAND inclusive_OR_expression
        ;

logical_OR_expression:
        logical_AND_expression
        | logical_OR_expression OROR logical_AND_expression
        ;

conditional_expression:
        logical_OR_expression

        | logical_OR_expression '?' comma_expression ':'
                conditional_expression
        ;

assignment_expression:
        conditional_expression
        | unary_expression assignment_operator assignment_expression
        ;

assignment_operator:
        '='
        | MULTassign
        | DIVassign
        | MODassign
        | PLUSassign
        | MINUSassign
        | LSHassign
        | RSHassign
        | ANDassign
        | ERassign
        | ORassign
        ;

comma_expression:
        assignment_expression
        | comma_expression ',' assignment_expression
        ;

constant_expression:
        conditional_expression
        ;


    /* The following was used for clarity */
comma_expression_opt:
        /* Nothing */
        | comma_expression
        ;


/******************************* DECLARATIONS *********************************/


    /*  The  following are notably different from the ANSI C Standard 
    specified grammar, but  are  present  in  my  ANSI  C  compatible 
    grammar.  The changes were made to disambiguate typedefs presence 
    in   declaration_specifiers   (vs.    in   the   declarator   for 
    redefinition); to allow struct/union/enum/class tag  declarations 
    without  declarators,  and  to  better  reflect  the  parsing  of 
    declarations    (declarators    must     be     combined     with 
    declaration_specifiers  ASAP, so that they can immediately become 
    visible in the current scope). */

declaration:
        declaring_list ';'
        | default_declaring_list ';'
        | sue_declaration_specifier ';' { /* this is constraint error, as it
                                        includes a storage class!?!*/ }
        | sue_type_specifier ';'
        | sue_type_specifier_elaboration ';'
        ;


    /* Note that if a typedef were  redeclared,  then  a  declaration 
    specifier  must be supplied (re: ANSI C spec).  The following are 
    declarations wherein no declaration_specifier  is  supplied,  and 
    hence the 'default' must be used.  An example of this is

        const a;

    which by default, is the same as:

        const int a;

    `a' must NOT be a typedef in the above example. */


    /*  The  presence of `{}' in the following rules indicates points 
    at which the symbol table MUST be updated so that  the  tokenizer 
    can  IMMEDIATELY  continue  to  maintain  the  proper distinction 
    between a TYPEDEFname and an IDENTIFIER. */

default_declaring_list:  /* Can't  redeclare typedef names */
        declaration_qualifier_list   identifier_declarator {} initializer_opt
        | type_qualifier_list        identifier_declarator {} initializer_opt
        | default_declaring_list ',' identifier_declarator {} initializer_opt

        | declaration_qualifier_list constructed_identifier_declarator
        | type_qualifier_list        constructed_identifier_declarator
        | default_declaring_list ',' constructed_identifier_declarator
        ;


    /* Note how type_qualifier_list is  NOT  used  in  the  following 
    productions.    Qualifiers   are   NOT   sufficient  to  redefine 
    typedef-names (as prescribed by the ANSI C standard).*/

declaring_list:
        declaration_specifier          declarator {} initializer_opt
        | type_specifier               declarator {} initializer_opt
        | basic_type_name              declarator {} initializer_opt
        | TYPEDEFname                  declarator {} initializer_opt
        | global_or_scoped_typedefname declarator {} initializer_opt
        | declaring_list ','           declarator {} initializer_opt

        | declaration_specifier        constructed_declarator
        | type_specifier               constructed_declarator
        | basic_type_name              constructed_declarator
        | TYPEDEFname                  constructed_declarator
        | global_or_scoped_typedefname constructed_declarator
        | declaring_list ','           constructed_declarator
        ;


    /* Declarators with  parenthesized  initializers  present  a  big 
    problem.  Typically  a  declarator  that looks like: "*a(...)" is 
    supposed to bind FIRST to the "(...)", and then to the "*".  This 
    binding  presumes  that  the  "(...)" stuff is a prototype.  With 
    constructed declarators, we must (officially) finish the  binding 
    to the "*" (finishing forming a good declarator) and THEN connect 
    with  the argument list. Unfortunately, by the time we realize it 
    is an argument list (and not a  prototype)  we  have  pushed  the 
    separate  declarator  tokens  "*"  and  "a"  onto  the yacc stack 
    WITHOUT combining them. The solution is to use odd productions to 
    carry  the  incomplete  declarator  along  with   the   "argument 
    expression  list" back up the yacc stack.  We would then actually 
    instantiate the symbol table after we have  fully  decorated  the 
    symbol  with all the leading "*" stuff.  Actually, since we don't 
    have all the type information in one spot till  we  reduce  to  a 
    declaring_list,  this delay is not a problem.  Note that ordinary 
    initializers REQUIRE (ANSI C Standard) that the symbol be  placed 
    into  the symbol table BEFORE its initializer is read, but in the 
    case of parenthesized initializers,  this  is  not  possible  (we 
    don't  even  know  we  have  an  initializer till have passed the 
    opening "(". ) */

constructed_declarator:
        nonunary_constructed_identifier_declarator
        | constructed_paren_typedef_declarator
        | simple_paren_typedef_declarator '(' argument_expression_list ')'

        | simple_paren_typedef_declarator postfixing_abstract_declarator
                                          '(' argument_expression_list ')'  /* constraint error */

        | constructed_parameter_typedef_declarator
        | asterisk_or_ampersand constructed_declarator
        | unary_modifier        constructed_declarator
        ;

constructed_paren_typedef_declarator:
        '(' paren_typedef_declarator ')'
                    '(' argument_expression_list ')'

        | '(' paren_typedef_declarator ')' postfixing_abstract_declarator
                   '(' argument_expression_list ')'

        | '(' simple_paren_typedef_declarator postfixing_abstract_declarator ')'
                   '(' argument_expression_list ')'

        | '(' TYPEDEFname postfixing_abstract_declarator ')'
                   '(' argument_expression_list ')'
        ;


constructed_parameter_typedef_declarator:
        TYPEDEFname    '(' argument_expression_list ')'

        | TYPEDEFname  postfixing_abstract_declarator
                       '(' argument_expression_list ')'  /* constraint error */

        | '(' clean_typedef_declarator ')'
                       '(' argument_expression_list ')'

        | '(' clean_typedef_declarator ')'  postfixing_abstract_declarator
                       '(' argument_expression_list ')'
        ;


constructed_identifier_declarator:
        nonunary_constructed_identifier_declarator
        | asterisk_or_ampersand constructed_identifier_declarator
        | unary_modifier        constructed_identifier_declarator
        ;


    /* The following are restricted to NOT  begin  with  any  pointer 
    operators.   This  includes both "*" and "T::*" modifiers.  Aside 
    from  this  restriction,   the   following   would   have   been: 
    identifier_declarator '(' argument_expression_list ')' */

nonunary_constructed_identifier_declarator:
        paren_identifier_declarator   '(' argument_expression_list ')'

        | paren_identifier_declarator postfixing_abstract_declarator
                       '(' argument_expression_list ')'  /* constraint error*/

        | '(' unary_identifier_declarator ')'
                       '(' argument_expression_list ')'

        | '(' unary_identifier_declarator ')' postfixing_abstract_declarator
                       '(' argument_expression_list ')'
        ;


declaration_specifier:
        basic_declaration_specifier          /* Arithmetic or void */
        | sue_declaration_specifier          /* struct/union/enum/class */
        | typedef_declaration_specifier      /* typedef*/
        ;

type_specifier:
        basic_type_specifier                 /* Arithmetic or void */
        | sue_type_specifier                 /* Struct/Union/Enum/Class */
        | sue_type_specifier_elaboration     /* elaborated Struct/Union/Enum/Class */
        | typedef_type_specifier             /* Typedef */
        ;

declaration_qualifier_list:  /* storage class and optional const/volatile */
        storage_class
        | type_qualifier_list storage_class
        | declaration_qualifier_list declaration_qualifier
        ;

type_qualifier_list:
        type_qualifier
        | type_qualifier_list type_qualifier
        ;

declaration_qualifier:
        storage_class
        | type_qualifier                  /* const or volatile */
        ;

type_qualifier:
        CONST
        | VOLATILE
        ;

basic_declaration_specifier:      /*Storage Class+Arithmetic or void*/
        declaration_qualifier_list    basic_type_name
        | basic_type_specifier        storage_class
        | basic_type_name             storage_class
        | basic_declaration_specifier declaration_qualifier
        | basic_declaration_specifier basic_type_name
        ;

basic_type_specifier:
        type_qualifier_list    basic_type_name /* Arithmetic or void */
        | basic_type_name      basic_type_name
        | basic_type_name      type_qualifier
        | basic_type_specifier type_qualifier
        | basic_type_specifier basic_type_name
        ;

sue_declaration_specifier:          /* Storage Class + struct/union/enum/class */
        declaration_qualifier_list       elaborated_type_name
        | declaration_qualifier_list     elaborated_type_name_elaboration
        | sue_type_specifier             storage_class
        | sue_type_specifier_elaboration storage_class
        | sue_declaration_specifier      declaration_qualifier
        ;

sue_type_specifier_elaboration:
        elaborated_type_name_elaboration     /* elaborated struct/union/enum/class */
        | type_qualifier_list elaborated_type_name_elaboration
        | sue_type_specifier_elaboration type_qualifier
        ;

sue_type_specifier:
        elaborated_type_name              /* struct/union/enum/class */
        | type_qualifier_list elaborated_type_name
        | sue_type_specifier type_qualifier
        ;

typedef_declaration_specifier:       /*Storage Class + typedef types */
        declaration_qualifier_list   TYPEDEFname
        | declaration_qualifier_list global_or_scoped_typedefname

        | typedef_type_specifier       storage_class
        | TYPEDEFname                  storage_class
        | global_or_scoped_typedefname storage_class

        | typedef_declaration_specifier declaration_qualifier
        ;

typedef_type_specifier:              /* typedef types */
        type_qualifier_list      TYPEDEFname
        | type_qualifier_list    global_or_scoped_typedefname

        | TYPEDEFname                  type_qualifier
        | global_or_scoped_typedefname type_qualifier

        | typedef_type_specifier type_qualifier
        ;


/*  There  are  really  several distinct sets of storage_classes. The 
sets vary depending on whether the declaration is at file scope, is a 
declaration within a struct/class, is within a function body, or in a 
function declaration/definition (prototype  parameter  declarations).  
They   are   grouped  here  to  simplify  the  grammar,  and  can  be 
semantically checked.  Note that this  approach  tends  to  ease  the 
syntactic restrictions in the grammar slightly, but allows for future 
language  development,  and tends to provide superior diagnostics and 
error recovery (i_e.: a syntax error does not disrupt the parse).


                File    File    Member  Member  Local   Local  Formal
                Var     Funct   Var     Funct   Var     Funct  Params
TYPEDEF         x       x       x       x       x       x
EXTERN          x       x                       x       x
STATIC          x       x       x       x       x
AUTO                                            x              x
REGISTER                                        x              x
FRIEND                                  x
OVERLOAD                x               x               x
INLINE                  x               x               x
VIRTUAL                                 x               x
*/

storage_class:
        EXTERN
        | TYPEDEF
        | STATIC
        | AUTO
        | REGISTER
        | FRIEND   /* C++, not ANSI C */
        | OVERLOAD /* C++, not ANSI C */
        | INLINE   /* C++, not ANSI C */
        | VIRTUAL  /* C++, not ANSI C */
        ;

basic_type_name:
        INT
        | CHAR
        | SHORT
        | LONG
        | FLOAT
        | DOUBLE
        | SIGNED
        | UNSIGNED
        | VOID
        ;

elaborated_type_name_elaboration:
        aggregate_name_elaboration
        | enum_name_elaboration
        ;

elaborated_type_name:
        aggregate_name
        | enum_name
        ;


    /* Since the expression "new type_name" MIGHT use  an  elaborated 
    type  and a derivation, it MIGHT have a ':'.  This fact conflicts 
    with the requirement that a new expression can be placed  between 
    a '?' and a ':' in a conditional expression (at least it confuses 
    LR(1)   parsers).   Hence   the   aggregate_name_elaboration   is 
    responsible for a series of SR conflicts on ':'.*/

    /* The intermediate actions {}  represent  points  at  which  the 
    database  of  typedef  names  must  be  updated  in C++.  This is 
    critical to the lexer, which must begin to tokenize based on this 
    new information. */

aggregate_name_elaboration:
        aggregate_name derivation_opt  '{' member_declaration_list_opt '}'
        | aggregate_key derivation_opt '{' member_declaration_list_opt '}'
        ;


    /* We distinguish between the above, which  support  elaboration, 
    and  this  set  of  productions  so  that  we can provide special 
    declaration specifiers for operator_new_type, and for  conversion 
    functions.  Note that without this restriction a large variety of 
    conflicts  appear  when  processing  operator_new and conversions 
    operators (which can be  followed  by  a  ':'  in  a  ternary  ?: 
    expression) */

    /*  Note that at the end of each of the following rules we should 
    be sure that the tag name is  in,  or  placed  in  the  indicated 
    scope.   If  no  scope  is  specified, then we must add it to our 
    current scope IFF it cannot  be  found  in  an  external  lexical 
    scope. */

aggregate_name:
                             aggregate_key tag_name
        | global_scope scope aggregate_key tag_name
        | global_scope       aggregate_key tag_name
        | scope              aggregate_key tag_name
        ;

derivation_opt:
        /* nothing */
        | ':' derivation_list
        ;

derivation_list:
        parent_class
        | derivation_list ',' parent_class
        ;

parent_class:
                                       global_opt_scope_opt_typedefname
        | VIRTUAL access_specifier_opt global_opt_scope_opt_typedefname
        | access_specifier virtual_opt global_opt_scope_opt_typedefname
        ;

virtual_opt:
        /* nothing */
        | VIRTUAL
        ;

access_specifier_opt:
        /* nothing */
        | access_specifier
        ;

access_specifier:
        PUBLIC
        | PRIVATE
        | PROTECTED
        ;

aggregate_key:
        STRUCT
        | UNION
        | CLASS /* C++, not ANSI C */
        ;


    /* Note that an empty list is ONLY allowed under C++. The grammar 
    can  be modified so that this stands out.  The trick is to define 
    member_declaration_list, and have that referenced for non-trivial 
    lists. */

member_declaration_list_opt:
        /* nothing */
        | member_declaration_list_opt member_declaration
        ;

member_declaration:
        member_declaring_list ';'
        | member_default_declaring_list ';'

        | access_specifier ':'               /* C++, not ANSI C */

        | new_function_definition            /* C++, not ANSI C */
        | constructor_function_in_class      /* C++, not ANSI C */

        | sue_type_specifier             ';' /* C++, not ANSI C */
        | sue_type_specifier_elaboration ';' /* C++, not ANSI C */
        | identifier_declarator          ';' /* C++, not ANSI C
                                                access modification
                                                conversion functions,
                                                unscoped destructors */

        | typedef_declaration_specifier ';' /* friend T */       /* C++, not ANSI C */
        | sue_declaration_specifier ';'     /* friend class C*/  /* C++, not ANSI C */
        ;

member_default_declaring_list:        /* doesn't redeclare typedef*/
        type_qualifier_list
                identifier_declarator member_pure_opt

        | declaration_qualifier_list
                identifier_declarator member_pure_opt /* C++, not ANSI C */

        | member_default_declaring_list ','
                identifier_declarator member_pure_opt

        | type_qualifier_list                bit_field_identifier_declarator
        | declaration_qualifier_list         bit_field_identifier_declarator /* C++, not ANSI C */
        | member_default_declaring_list ','  bit_field_identifier_declarator
        ;


    /* There is a conflict when "struct A" is used as  a  declaration 
    specifier,  and  there  is a chance that a bit field name will be 
    provided.  To fix this syntactically would require distinguishing 
    non_elaborating_declaration_specifiers   the   way   I    handled 
    non_elaborating_type_specifiers.   I   think  this  should  be  a 
    constraint error anyway :-). */

member_declaring_list:        /* Can possibly redeclare typedefs */
        type_specifier                 declarator member_pure_opt
        | basic_type_name              declarator member_pure_opt

        | global_or_scoped_typedefname declarator member_pure_opt
        | member_conflict_declaring_item
        | member_declaring_list ','    declarator member_pure_opt

        | type_specifier                bit_field_declarator
        | basic_type_name               bit_field_declarator
        | TYPEDEFname                   bit_field_declarator
        | global_or_scoped_typedefname  bit_field_declarator
        | declaration_specifier         bit_field_declarator /* constraint violation: storage class used */
        | member_declaring_list ','     bit_field_declarator
        ;


    /* The following conflict with constructors-
      member_conflict_declaring_item:
        TYPEDEFname             declarator member_pure_opt
        | declaration_specifier declarator member_pure_opt /* C++, not ANSI C * /
        ;
    so we inline expand declarator to get the following productions...
    */
member_conflict_declaring_item:
        TYPEDEFname             identifier_declarator            member_pure_opt
        | TYPEDEFname           parameter_typedef_declarator     member_pure_opt
        | TYPEDEFname           simple_paren_typedef_declarator  member_pure_opt

        | declaration_specifier identifier_declarator            member_pure_opt
        | declaration_specifier parameter_typedef_declarator     member_pure_opt
        | declaration_specifier simple_paren_typedef_declarator  member_pure_opt

        | member_conflict_paren_declaring_item
        ;


    /* The following still conflicts with constructors-
      member_conflict_paren_declaring_item:
        TYPEDEFname             paren_typedef_declarator     member_pure_opt
        | declaration_specifier paren_typedef_declarator     member_pure_opt
        ;
    so paren_typedef_declarator is expanded inline to get...*/

member_conflict_paren_declaring_item:
        TYPEDEFname   asterisk_or_ampersand
                '(' simple_paren_typedef_declarator ')' member_pure_opt
        | TYPEDEFname unary_modifier
                '(' simple_paren_typedef_declarator ')' member_pure_opt
        | TYPEDEFname asterisk_or_ampersand
                '(' TYPEDEFname ')'                     member_pure_opt
        | TYPEDEFname unary_modifier
                '(' TYPEDEFname ')'                     member_pure_opt
        | TYPEDEFname asterisk_or_ampersand
                 paren_typedef_declarator               member_pure_opt
        | TYPEDEFname unary_modifier
                 paren_typedef_declarator               member_pure_opt

        | declaration_specifier asterisk_or_ampersand
                '(' simple_paren_typedef_declarator ')' member_pure_opt
        | declaration_specifier unary_modifier
                '(' simple_paren_typedef_declarator ')' member_pure_opt
        | declaration_specifier asterisk_or_ampersand
                '(' TYPEDEFname ')'                     member_pure_opt
        | declaration_specifier unary_modifier
                '(' TYPEDEFname ')'                     member_pure_opt
        | declaration_specifier asterisk_or_ampersand
                paren_typedef_declarator                member_pure_opt
        | declaration_specifier unary_modifier
                paren_typedef_declarator                member_pure_opt

        | member_conflict_paren_postfix_declaring_item
        ;


    /* but we still have the following conflicts with constructors-
   member_conflict_paren_postfix_declaring_item:
      TYPEDEFname             postfix_paren_typedef_declarator member_pure_opt
      | declaration_specifier postfix_paren_typedef_declarator member_pure_opt
      ;
    so we expand paren_postfix_typedef inline and get...*/

member_conflict_paren_postfix_declaring_item:
        TYPEDEFname     '(' paren_typedef_declarator ')'
                                                           member_pure_opt
        | TYPEDEFname   '(' simple_paren_typedef_declarator
                        postfixing_abstract_declarator ')' member_pure_opt
        | TYPEDEFname   '(' TYPEDEFname
                        postfixing_abstract_declarator ')' member_pure_opt
        | TYPEDEFname   '(' paren_typedef_declarator ')'
                        postfixing_abstract_declarator     member_pure_opt

        | declaration_specifier '(' paren_typedef_declarator ')'
                                                           member_pure_opt
        | declaration_specifier '(' simple_paren_typedef_declarator
                        postfixing_abstract_declarator ')' member_pure_opt
        | declaration_specifier '(' TYPEDEFname
                        postfixing_abstract_declarator ')' member_pure_opt
        | declaration_specifier '(' paren_typedef_declarator ')'
                        postfixing_abstract_declarator     member_pure_opt
        ;
    /* ...and we are done.  Now all  the  conflicts  appear  on  ';', 
    which can be semantically evaluated/disambiguated */


member_pure_opt:
        /* nothing */
        | '=' OCTALconstant /* C++, not ANSI C */ /* Pure function*/
        ;


    /*  Note  that  bit  field  names, where redefining TYPEDEFnames, 
    cannot be parenthesized in C++ (due to  ambiguities),  and  hence 
    this  part of the grammar is simpler than ANSI C. :-) The problem 
    occurs because:

         TYPEDEFname ( TYPEDEFname) : .....

    doesn't look like a bit field, rather it looks like a constructor 
    definition! */

bit_field_declarator:
        bit_field_identifier_declarator
        | TYPEDEFname {} ':' constant_expression
        ;


    /* The actions taken in the "{}" above and below are intended  to 
    allow  the  symbol  table  to  be  updated when the declarator is 
    complete.  It is critical for code like:

            foo : sizeof(foo + 1);
    */

bit_field_identifier_declarator:
                                   ':' constant_expression
        | identifier_declarator {} ':' constant_expression
        ;

enum_name_elaboration:
        global_opt_scope_opt_enum_key '{' enumerator_list '}'
        | enum_name                   '{' enumerator_list '}'
        ;


    /* As with structures, the distinction between "elaborating"  and 
    "non-elaborating"  enum  types  is  maintained.  In actuality, it 
    probably does not cause much in the way of conflicts, since a ':' 
    is not allowed.  For symmetry, we maintain the distinction.   The 
    {}  actions are intended to allow the symbol table to be updated.  
    These updates are significant to code such as:

        enum A { first=sizeof(A)};
    */

enum_name:
        global_opt_scope_opt_enum_key tag_name
        ;

global_opt_scope_opt_enum_key:
        ENUM
        | global_or_scope ENUM
        ;

enumerator_list:
        enumerator_list_no_trailing_comma
        | enumerator_list_no_trailing_comma ',' /* C++, not ANSI C */
        ;


    /* Note that we do not need to rush to add an enumerator  to  the 
    symbol  table  until  *AFTER* the enumerator_value_opt is parsed. 
    The enumerated value is only in scope  AFTER  its  definition  is 
    complete.   Hence the following is legal: "enum {a, b=a+10};" but 
    the following is (assuming no external matching of names) is  not 
    legal:  "enum {c, d=sizeof(d)};" ("d" not defined when sizeof was 
    applied.) This is  notably  contrasted  with  declarators,  which 
    enter scope as soon as the declarator is complete. */

enumerator_list_no_trailing_comma:
        enumerator_name enumerator_value_opt
        | enumerator_list_no_trailing_comma ',' enumerator_name enumerator_value_opt
        ;

enumerator_name:
        IDENTIFIER
        | TYPEDEFname
        ;

enumerator_value_opt:
        /* Nothing */
        | '=' constant_expression
        ;


    /*  We special case the lone type_name which has no storage class 
    (even though it should be an example of  a  parameter_type_list). 
    This helped to disambiguate type-names in parenthetical casts.*/

parameter_type_list:
        '(' ')'                             type_qualifier_list_opt
        | '(' type_name ')'                 type_qualifier_list_opt
        | '(' type_name initializer ')'     type_qualifier_list_opt /* C++, not ANSI C */
        | '(' named_parameter_type_list ')' type_qualifier_list_opt
        ;


    /* The following are used in old style function definitions, when 
    a complex return type includes the "function returning" modifier. 
    Note  the  subtle  distinction  from  parameter_type_list.  These 
    parameters are NOT the parameters for the function being defined, 
    but are simply part of the type definition.  An example would be:

        int(*f(   a  ))(float) long a; {...}

    which is equivalent to the full new style definition:

        int(*f(long a))(float) {...}

    The   type   list    `(float)'    is    an    example    of    an 
    old_parameter_type_list.   The  bizarre point here is that an old 
    function definition declarator can be followed by  a  type  list, 
    which  can  start  with a qualifier `const'.  This conflicts with 
    the new syntactic construct for const member  functions!?!  As  a 
    result,  an  old  style function definition cannot be used in all 
    cases for a member function.  */

old_parameter_type_list:
        '(' ')'
        | '(' type_name ')'
        | '(' type_name initializer ')'  /* C++, not ANSI C */
        | '(' named_parameter_type_list ')'
        ;

named_parameter_type_list:  /* WARNING: excludes lone type_name*/
        parameter_list
        | parameter_list comma_opt_ellipsis
        | type_name comma_opt_ellipsis
        | type_name initializer comma_opt_ellipsis  /* C++, not ANSI C */
        | ELLIPSIS /* C++, not ANSI C */
        ;

comma_opt_ellipsis:
        ELLIPSIS       /* C++, not ANSI C */
        | ',' ELLIPSIS
        ;

parameter_list:
        non_casting_parameter_declaration
        | non_casting_parameter_declaration initializer /* C++, not ANSI C */
        | type_name             ',' parameter_declaration
        | type_name initializer ',' parameter_declaration  /* C++, not ANSI C */
        | parameter_list        ',' parameter_declaration
        ;


    /* There is some very subtle disambiguation going  on  here.   Do 
    not be tempted to make further use of the following production in 
    parameter_list,  or else the conflict count will grow noticeably. 
    Specifically, the next set  of  rules  has  already  been  inline 
    expanded for the first parameter in a parameter_list to support a 
    deferred disambiguation. The subtle disambiguation has to do with 
    contexts where parameter type lists look like old-style-casts. */

parameter_declaration:
        type_name
        | type_name                         initializer  /* C++, not ANSI C */
        | non_casting_parameter_declaration
        | non_casting_parameter_declaration initializer /* C++, not ANSI C */
        ;


    /* There is an LR ambiguity between old-style parenthesized casts 
    and parameter-type-lists.  This tends to happen in contexts where 
    either  an  expression or a parameter-type-list is possible.  For 
    example, assume that T is an  externally  declared  type  in  the 
    code:

           int (T ((int

    it might continue:

           int (T ((int)0));

    which would make it:

           (int) (T) (int)0 ;

    which  is  an  expression,  consisting  of  a  series  of  casts.  
    Alternatively, it could be:

           int (T ((int a)));

    which would make it the redeclaration of T, equivalent to:

           int T (dummy_name (int a));

    if we see a type that either has a named variable (in  the  above 
    case "a"), or a storage class like:

           int (T ((int register

    then  we  know  it  can't  be  a cast, and it is "forced" to be a 
    parameter_list.

    It is not yet clear that the ANSI C++ committee would  decide  to 
    place this disambiguation into the syntax, rather than leaving it 
    as  a  constraint check (i.e., a valid parser would have to parse 
    everything as though it were  a  parameter  list  (in  these  odd 
    contexts),  and  then  give an error if is to a following context 
    (like "0" above) that invalidated this syntax evaluation. */

    /* One big thing implemented here is that a TYPEDEFname CANNOT be 
    redeclared when we don't have declaration_specifiers! Notice that 
    when we do use a TYPEDEFname based declarator, only the "special" 
    (non-ambiguous  in  this  context)  typedef_declarator  is  used. 
    Everything else that is "missing" shows up as a type_name. */

non_casting_parameter_declaration: /*have names or storage classes */
        declaration_specifier
        | declaration_specifier abstract_declarator
        | declaration_specifier identifier_declarator
        | declaration_specifier parameter_typedef_declarator

        | declaration_qualifier_list
        | declaration_qualifier_list abstract_declarator
        | declaration_qualifier_list identifier_declarator

        | type_specifier identifier_declarator
        | type_specifier parameter_typedef_declarator

        | basic_type_name identifier_declarator
        | basic_type_name parameter_typedef_declarator

        | TYPEDEFname                   identifier_declarator
        | TYPEDEFname                   parameter_typedef_declarator

        | global_or_scoped_typedefname  identifier_declarator
        | global_or_scoped_typedefname  parameter_typedef_declarator

        | type_qualifier_list identifier_declarator
        ;

type_name:
        type_specifier
        | basic_type_name
        | TYPEDEFname
        | global_or_scoped_typedefname
        | type_qualifier_list

        | type_specifier               abstract_declarator
        | basic_type_name              abstract_declarator
        | TYPEDEFname                  abstract_declarator
        | global_or_scoped_typedefname abstract_declarator
        | type_qualifier_list          abstract_declarator
        ;

initializer_opt:
        /* nothing */
        | initializer
        ;

initializer:
        '=' initializer_group
        ;

initializer_group:
        '{' initializer_list '}'
        | '{' initializer_list ',' '}'
        | assignment_expression
        ;

initializer_list:
        initializer_group
        | initializer_list ',' initializer_group
        ;


/*************************** STATEMENTS *******************************/

statement:
        labeled_statement
        | compound_statement
        | expression_statement
        | selection_statement
        | iteration_statement
        | jump_statement
        | declaration /* C++, not ANSI C */
        ;

labeled_statement:
        label                      ':' statement
        | CASE constant_expression ':' statement
        | DEFAULT                  ':' statement
        ;


    /*  I sneak declarations into statement_list to support C++.  The 
    grammar is a little clumsy this  way,  but  the  violation  of  C 
    syntax is heavily localized */

compound_statement:
        '{' statement_list_opt '}'
        ;

declaration_list:
        declaration
        | declaration_list declaration
        ;

statement_list_opt:
        /* nothing */
        | statement_list_opt statement
        ;

expression_statement:
        comma_expression_opt ';'
        ;

selection_statement:
          IF '(' comma_expression ')' statement
        | IF '(' comma_expression ')' statement ELSE statement
        | SWITCH '(' comma_expression ')' statement
        ;

iteration_statement:
        WHILE '(' comma_expression_opt ')' statement
        | DO statement WHILE '(' comma_expression ')' ';'

        | FOR '(' comma_expression_opt ';' comma_expression_opt ';'
                comma_expression_opt ')' statement

        | FOR '(' declaration        comma_expression_opt ';'
                comma_expression_opt ')' statement  /* C++, not ANSI C */
        ;

jump_statement:
        GOTO label                     ';'
        | CONTINUE                     ';'
        | BREAK                        ';'
        | RETURN comma_expression_opt  ';'
        ;


    /*  The  following  actions should update the symbol table in the 
    "label" name space */

label:
        IDENTIFIER
        | TYPEDEFname
        ;


/***************************** EXTERNAL DEFINITIONS *****************************/

translation_unit:
        /* nothing */
        | translation_unit external_definition
        ;

external_definition:
        function_declaration                         /* C++, not ANSI C*/
        | function_definition
        | declaration
        | linkage_specifier function_declaration     /* C++, not ANSI C*/
        | linkage_specifier function_definition      /* C++, not ANSI C*/
        | linkage_specifier declaration              /* C++, not ANSI C*/
        | linkage_specifier '{' translation_unit '}' /* C++, not ANSI C*/
        ;

linkage_specifier:
        EXTERN STRINGliteral
        ;


    /* Note that declaration_specifiers are left out of the following 
    function declarations.  Such omission is illegal in ANSI C. It is 
    sometimes necessary in C++, in instances  where  no  return  type 
    should be specified (e_g., a conversion operator).*/

function_declaration:
        identifier_declarator ';'   /*  semantically  verify  it is a 
                                    function, and (if ANSI says  it's 
                                    the  law for C++ also...) that it 
                                    is something that  can't  have  a 
                                    return  type  (like  a conversion 
                                    function, or a destructor */

        | constructor_function_declaration ';'
        ;

function_definition:
        new_function_definition
        | old_function_definition
        | constructor_function_definition
        ;


    /* Note that in ANSI C, function definitions *ONLY* are presented 
    at file scope.  Hence, if there is a typedefname  active,  it  is 
    illegal  to  redeclare  it  (there  is no enclosing scope at file 
    scope).

    In  contrast,  C++  allows   function   definitions   at   struct 
    elaboration scope, and allows tags that are defined at file scope 
    (and  hence  look like typedefnames) to be redeclared to function 
    calls.  Hence several of the rules are "partially C++  only".   I 
    could  actually  build separate rules for typedef_declarators and 
    identifier_declarators, and mention that  the  typedef_declarator 
    rules represent the C++ only features.

    In  some  sense,  this  is  haggling, as I could/should have left 
    these as constraints in the ANSI C grammar, rather than as syntax 
    requirements.  */

new_function_definition:
                                       identifier_declarator compound_statement
        | declaration_specifier                   declarator compound_statement /* partially C++ only */
        | type_specifier                          declarator compound_statement /* partially C++ only */
        | basic_type_name                         declarator compound_statement /* partially C++ only */
        | TYPEDEFname                             declarator compound_statement /* partially C++ only */
        | global_or_scoped_typedefname            declarator compound_statement /* partially C++ only */
        | declaration_qualifier_list   identifier_declarator compound_statement
        | type_qualifier_list          identifier_declarator compound_statement
        ;


    /* Note that I do not support redeclaration of TYPEDEFnames  into 
    function  names  as I did in new_function_definitions (see note). 
    Perhaps I should do it, but for now, ignore the issue. Note  that 
    this  is  a  non-problem  with  ANSI  C,  as  tag  names  are not 
    considered TYPEDEFnames. */

old_function_definition:
                                       old_function_declarator {} old_function_body
        | declaration_specifier        old_function_declarator {} old_function_body
        | type_specifier               old_function_declarator {} old_function_body
        | basic_type_name              old_function_declarator {} old_function_body
        | TYPEDEFname                  old_function_declarator {} old_function_body
        | global_or_scoped_typedefname old_function_declarator {} old_function_body
        | declaration_qualifier_list   old_function_declarator {} old_function_body
        | type_qualifier_list          old_function_declarator {} old_function_body
        ;

old_function_body:
        declaration_list compound_statement
        | compound_statement
        ;


    /*    Verify    via    constraints     that     the     following 
        declaration_specifier           is          really          a 
        typedef_declaration_specifier, consisting of:

        ... TYPEDEFname :: TYPEDEFname

    optionally *preceded* by a "inline" keyword.   Use  care  not  to 
    support "inline" as a postfix!

    Similarly, the global_or_scoped_typedefname must be:

        ... TYPEDEFname :: TYPEDEFname

    with matching names at the end of the list.

    We  use the more general form to prevent a syntax conflict with a 
    typical    function    definition    (which    won't    have    a 
    constructor_init_list) */

constructor_function_definition:
        global_or_scoped_typedefname parameter_type_list
                     constructor_init_list_opt compound_statement

        | declaration_specifier      parameter_type_list
                     constructor_init_list_opt compound_statement
        ;


    /*  Same  comments  as  seen  for constructor_function_definition 
    apply here */

constructor_function_declaration:
        global_or_scoped_typedefname parameter_type_list  /* wasteful redeclaration; used for friend decls.  */

        | declaration_specifier      parameter_type_list  /* request to inline, no definition */
        ;


    /* The following use of declaration_specifiers are made to  allow 
    for  a TYPEDEFname preceded by an INLINE modifier. This fact must 
    be verified semantically.  It should also be  verified  that  the 
    TYPEDEFname  is  ACTUALLY  the  class name being elaborated. Note 
    that we could break out typedef_declaration_specifier from within 
    declaration_specifier, and we  might  narrow  down  the  conflict 
    region a bit. A second alternative (to what is done) for cleaning 
    up  this  stuff  is  to  let the tokenizer specially identify the 
    current class being elaborated as a special token, and not just a 
    typedefname. Unfortunately, things would get very  confusing  for 
    the  lexer,  as  we may pop into enclosed tag elaboration scopes; 
    into function definitions; or into both recursively! */

    /* I should make the following  rules  easier  to  annotate  with 
    scope  entry  and exit actions.  Note how hard it is to establish 
    the scope when you don't even know what the decl_spec is!! It can 
    be done with $-1 hacking, but I should not encourage users to  do 
    this directly. */

constructor_function_in_class:
        declaration_specifier   constructor_parameter_list_and_body
        | TYPEDEFname           constructor_parameter_list_and_body
        ;


    /* The following conflicts with member declarations-
    constructor_parameter_list_and_body:
          parameter_type_list ';'
          | parameter_type_list constructor_init_list_opt compound_statement
          ;
    so parameter_type_list was expanded inline to get */

    /* C++, not ANSI C */
constructor_parameter_list_and_body:
          '('                           ')' type_qualifier_list_opt ';'
        | '(' type_name initializer     ')' type_qualifier_list_opt ';' 
        | '(' named_parameter_type_list ')' type_qualifier_list_opt ';'
        | '('                           ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement
        | '(' type_name initializer     ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement
        | '(' named_parameter_type_list ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | constructor_conflicting_parameter_list_and_body
        ;


    /* The following conflicted with member declaration-
    constructor_conflicting_parameter_list_and_body:
        '('   type_name ')'                 type_qualifier_list_opt ';'
        | '(' type_name ')'                 type_qualifier_list_opt
                constructor_init_list_opt compound_statement
        ;
    so type_name was inline expanded to get the following... */


    /*  Note  that by inline expanding type_qualifier_opt in a few of 
    the following rules I can transform 3  RR  conflicts  into  3  SR 
    conflicts.  Since  all the conflicts have a look ahead of ';', it 
    doesn't  really  matter  (also,  there  are  no   bad   LALR-only 
    components in the conflicts) */

constructor_conflicting_parameter_list_and_body:
        '(' type_specifier                 ')' type_qualifier_list_opt
                ';'
        | '(' basic_type_name              ')' type_qualifier_list_opt
                ';'

        | '(' TYPEDEFname                  ')' type_qualifier_list_opt
                ';'

        | '(' global_or_scoped_typedefname ')' type_qualifier_list_opt
                ';'

        | '(' type_qualifier_list          ')' type_qualifier_list_opt
                ';'


        | '(' type_specifier               abstract_declarator ')' type_qualifier_list_opt
                ';'
        | '(' basic_type_name              abstract_declarator ')' type_qualifier_list_opt
                ';'

        /* missing entry posted below */

        | '(' global_or_scoped_typedefname abstract_declarator ')' type_qualifier_list_opt
                ';'
        | '(' type_qualifier_list          abstract_declarator ')' type_qualifier_list_opt
                ';'


        | '(' type_specifier               ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | '(' basic_type_name              ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | '(' TYPEDEFname                  ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | '(' global_or_scoped_typedefname ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | '(' type_qualifier_list           ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement


        | '(' type_specifier  abstract_declarator ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | '(' basic_type_name abstract_declarator ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        /* missing entry posted below */

        | '(' global_or_scoped_typedefname abstract_declarator ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | '(' type_qualifier_list          abstract_declarator ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | constructor_conflicting_typedef_declarator
        ;


    /* The following have ambiguities with member declarations-
    constructor_conflicting_typedef_declarator:
      '(' TYPEDEFname abstract_declarator ')' type_qualifier_list_opt
                ';'
      |  '(' TYPEDEFname abstract_declarator ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement
      ;
    which can be deferred by expanding abstract_declarator, and in two
    cases parameter_qualifier_list, resulting in ...*/

constructor_conflicting_typedef_declarator:
        '(' TYPEDEFname unary_abstract_declarator          ')' type_qualifier_list_opt
                ';'

        | '(' TYPEDEFname unary_abstract_declarator       ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement

        | '(' TYPEDEFname postfix_abstract_declarator     ')' type_qualifier_list_opt
                ';'

        | '(' TYPEDEFname postfix_abstract_declarator     ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement


        | '(' TYPEDEFname postfixing_abstract_declarator  ')' type_qualifier_list_opt
                ';'

        | '(' TYPEDEFname postfixing_abstract_declarator  ')' type_qualifier_list_opt
                constructor_init_list_opt compound_statement
        ;


constructor_init_list_opt:
        /* nothing */
        | constructor_init_list
        ;

constructor_init_list:
        ':' constructor_init
        | constructor_init_list ',' constructor_init
        ;

constructor_init:
        IDENTIFIER   '(' argument_expression_list ')'
        | IDENTIFIER '('                          ')'

        | TYPEDEFname '(' argument_expression_list ')'
        | TYPEDEFname '('                          ')'
        | global_or_scoped_typedefname '(' argument_expression_list ')'
        | global_or_scoped_typedefname '('                          ')'

        | '(' argument_expression_list ')' /* Single inheritance ONLY*/
        | '(' ')' /* Is this legal? It might be default! */
        ;

declarator:
        identifier_declarator
        | typedef_declarator
        ;

typedef_declarator:
        paren_typedef_declarator          /* would be ambiguous as parameter*/
        | simple_paren_typedef_declarator /* also ambiguous */
        | parameter_typedef_declarator    /* not ambiguous as parameter*/
        ;

parameter_typedef_declarator:
        TYPEDEFname
        | TYPEDEFname postfixing_abstract_declarator
        | clean_typedef_declarator
        ;


    /*  The  following  have  at  least  one  '*'or '&'.  There is no 
    (redundant) '(' between the '*'/'&'  and  the  TYPEDEFname.  This 
    definition  is  critical  in  that  a redundant paren that it too 
    close to the TYPEDEFname (i.e.,  nothing  between  them  at  all) 
    would  make  the TYPEDEFname into a parameter list, rather than a 
    declarator.*/

clean_typedef_declarator:
        clean_postfix_typedef_declarator
        | asterisk_or_ampersand parameter_typedef_declarator
        | unary_modifier        parameter_typedef_declarator
        ;

clean_postfix_typedef_declarator:
        '('   clean_typedef_declarator ')'
        | '(' clean_typedef_declarator ')' postfixing_abstract_declarator
        ;


    /* The following have a redundant '(' placed immediately  to  the 
    left  of the TYPEDEFname.  This opens up the possibility that the 
    TYPEDEFname is really the start of a parameter list, and *not*  a 
    declarator*/

paren_typedef_declarator:
        postfix_paren_typedef_declarator
        | asterisk_or_ampersand '(' simple_paren_typedef_declarator ')'
        | unary_modifier        '(' simple_paren_typedef_declarator ')'
        | asterisk_or_ampersand '(' TYPEDEFname ')' /* redundant paren */
        | unary_modifier        '(' TYPEDEFname ')' /* redundant paren */
        | asterisk_or_ampersand paren_typedef_declarator
        | unary_modifier        paren_typedef_declarator
        ;

postfix_paren_typedef_declarator:
        '(' paren_typedef_declarator ')'
        | '(' simple_paren_typedef_declarator postfixing_abstract_declarator ')'
        | '(' TYPEDEFname postfixing_abstract_declarator ')'              /* redundant paren */
        | '(' paren_typedef_declarator ')' postfixing_abstract_declarator
        ;


    /*  The following excludes lone TYPEDEFname to help in a conflict 
    resolution.  We have special cased lone  TYPEDEFname  along  side 
    all uses of simple_paren_typedef_declarator */

simple_paren_typedef_declarator:
        '(' TYPEDEFname ')'
        | '(' simple_paren_typedef_declarator ')'
        ;

identifier_declarator:
        unary_identifier_declarator
        | paren_identifier_declarator
        ;


    /*  The  following  allows  "function return array of" as well as 
    "array of function returning".  It COULD be cleaned  up  the  way 
    abstract  declarators  have been.  This change might make it hard 
    to recover from user's syntax errors, whereas now they appear  as 
    simple constraint errors. */

unary_identifier_declarator:
        postfix_identifier_declarator
        | asterisk_or_ampersand identifier_declarator
        | unary_modifier        identifier_declarator
        ;

postfix_identifier_declarator:
        paren_identifier_declarator           postfixing_abstract_declarator
        | '(' unary_identifier_declarator ')'
        | '(' unary_identifier_declarator ')' postfixing_abstract_declarator
        ;

old_function_declarator:
        postfix_old_function_declarator
        | asterisk_or_ampersand old_function_declarator
        | unary_modifier      old_function_declarator
        ;


    /*  ANSI  C  section  3.7.1  states  "An identifier declared as a 
    typedef name shall not be redeclared as a parameter".  Hence  the 
    following is based only on IDENTIFIERs.

    Instead  of identifier_lists, an argument_expression_list is used 
    in  old  style  function   definitions.    The   ambiguity   with 
    constructors   required   the  use  of  argument  lists,  with  a 
    constraint verification of the list (e_g.: check to see that  the 
    "expressions" consisted of lone identifiers).

    An interesting ambiguity appeared:
        const constant=5;
        int foo(constant) ...

    Is  this an old function definition or constructor?  The decision 
    is made later by THIS grammar based on trailing context :-). This 
    ambiguity is probably what caused many parsers to give up on  old 
    style function definitions. */

postfix_old_function_declarator:
        paren_identifier_declarator '(' argument_expression_list ')'
        | '(' old_function_declarator ')'
        | '(' old_function_declarator ')' old_postfixing_abstract_declarator
        ;

old_postfixing_abstract_declarator:
        array_abstract_declarator /* array modifiers */
        | old_parameter_type_list  /* function returning modifiers */
        ;

abstract_declarator:
        unary_abstract_declarator
        | postfix_abstract_declarator
        | postfixing_abstract_declarator
        ;

postfixing_abstract_declarator:
        array_abstract_declarator
        | parameter_type_list
        ;

array_abstract_declarator:
        '[' ']'
        | '[' constant_expression ']'
        | array_abstract_declarator '[' constant_expression ']'
        ;

unary_abstract_declarator:
        asterisk_or_ampersand
        | unary_modifier
        | asterisk_or_ampersand abstract_declarator
        | unary_modifier        abstract_declarator
        ;

postfix_abstract_declarator:
        '(' unary_abstract_declarator ')'
        | '(' postfix_abstract_declarator ')'
        | '(' postfixing_abstract_declarator ')'
        | '(' unary_abstract_declarator ')' postfixing_abstract_declarator
        ;

asterisk_or_ampersand:
        '*'
        | '&'
        ;

unary_modifier:
        scope '*' type_qualifier_list_opt
        | asterisk_or_ampersand type_qualifier_list
        ;



/************************* NESTED SCOPE SUPPORT ******************************/


    /*  The  actions taken in the rules that follow involve notifying 
    the lexer that it should use the scope specified to determine  if 
    the  next  IDENTIFIER  token is really a TYPEDEFname token.  Note 
    that the actions must be taken before the parse has a  chance  to 
    "look-ahead" at the token that follows the "::", and hence should 
    be  done  during  a  reduction to "scoping_name" (which is always 
    followed by CLCL).  Since we are defining an  LR(1)  grammar,  we 
    are  assured  that  an action specified *before* the :: will take 
    place before the :: is shifted, and hence before the  token  that 
    follows the CLCL is scanned/lexed. */

    /*  Note that at the end of each of the following rules we should 
    be sure that the tag name is  in,  or  placed  in  the  indicated 
    scope.   If  no  scope  is  specified, then we must add it to our 
    current scope IFF it cannot  be  found  in  an  external  lexical 
    scope. */

scoping_name:
        tag_name
        | aggregate_key tag_name /* also update symbol table here by notifying it about a (possibly) new tag*/
        ;

scope:
        scoping_name CLCL
        | scope scoping_name  CLCL
        ;


    /*  Don't try to simplify the count of non-terminals by using one 
    of the other definitions of  "IDENTIFIER  or  TYPEDEFname"  (like 
    "label").   If you reuse such a non-terminal, 2 RR conflicts will 
    appear. The conflicts are LALR-only. The underlying cause of  the 
    LALR-only   conflict   is  that  labels,  are  followed  by  ':'.  
    Similarly, structure elaborations which provide a derivation have 
    have ':' just  after  tag_name  This  reuse,  with  common  right 
    context, is too much for an LALR parser. */

tag_name:
        IDENTIFIER
        | TYPEDEFname
        ;

global_scope:
        { /*scan for upcoming name in file scope */ } CLCL
        ;

global_or_scope:
        global_scope
        | scope
        | global_scope scope
        ;


    /*  The  following can be used in an identifier based declarator. 
    (Declarators  that  redefine  an  existing  TYPEDEFname   require 
    special  handling,  and are not included here).  In addition, the 
    following are valid "identifiers" in  an  expression,  whereas  a 
    TYPEDEFname is NOT.*/

scope_opt_identifier:
                IDENTIFIER
        | scope IDENTIFIER  /* C++ not ANSI C */
        ;

scope_opt_complex_name:
                complex_name
        | scope complex_name
        ;

complex_name:
        '~' TYPEDEFname
        | operator_function_name
        ;


    /*  Note that the derivations for global_opt_scope_opt_identifier 
    and global_opt_scope_opt_complex_name must be  placed  after  the 
    derivation:

       paren_identifier_declarator : scope_opt_identifier

    There  are several states with RR conflicts on "(", ")", and "[".  
    In these states we give up and assume a declaration, which  means 
    resolving   in  favor  of  paren_identifier_declarator.  This  is 
    basically the "If it can be  a  declaration  rule...",  with  our 
    finite cut off. */

global_opt_scope_opt_identifier:
        global_scope scope_opt_identifier
        |            scope_opt_identifier
        ;

global_opt_scope_opt_complex_name:
        global_scope scope_opt_complex_name
        |            scope_opt_complex_name
        ;


    /*  Note  that we exclude a lone TYPEDEFname.  When all alone, it 
    gets involved in a lot of ambiguities (re: function like cast  vs 
    declaration),   and  hence  must  be  special  cased  in  several 
    contexts. Note that generally every use of scoped_typedefname  is 
    accompanied by a parallel production using lone TYPEDEFname */

scoped_typedefname:
        scope TYPEDEFname
        ;

global_or_scoped_typedefname:
                       scoped_typedefname
        | global_scope scoped_typedefname
        | global_scope TYPEDEFname
        ;

global_opt_scope_opt_typedefname:
        TYPEDEFname
        | global_or_scoped_typedefname
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
# line 1 "cpp5.l"
#define INITIAL 0
# line 2 "cpp5.l"

    /* Copyright (C) 1989-1991 James A. Roskind, All rights reserved.
    This lexer description was written by James A.  Roskind.  Copying
    of  this  file, as a whole, is permitted providing this notice is
    intact  and  applicable   in   all   complete   copies.    Direct
    translations  as a whole to other lexer generator input languages
    (or lexical description languages)  is  permitted  provided  that
    this  notice  is  intact and applicable in all such copies, along
    with a disclaimer that  the  contents  are  a  translation.   The
    reproduction  of derived files or text, such as modified versions
    of this file, or the output of scanner generators, is  permitted,
    provided   the  resulting  work  includes  the  copyright  notice
    "Portions Copyright (c) 1989, 1990 James  A.   Roskind".  Derived
    products  must  also  provide  the notice "Portions Copyright (c)
    1989, 1990 James A.  Roskind" in  a  manner  appropriate  to  the
    utility,   and  in  keeping  with  copyright  law  (e.g.:  EITHER
    displayed when first invoked/executed; OR displayed  continuously
    on  display terminal; OR via placement in the object code in form
    readable in a printout, with or near the title of the work, or at
    the end of the file).  No royalties, licenses or  commissions  of
    any  kind  are  required  to copy this file, its translations, or
    derivative products, when the copies are made in compliance  with
    this  notice.  Persons  or  corporations  that  do make copies in
    compliance  with  this  notice  may  charge  whatever  price   is
    agreeable  to  a buyer, for such copies or derivative works. THIS
    FILE IS PROVIDED ``AS IS'' AND WITHOUT  ANY  EXPRESS  OR  IMPLIED
    WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES
    OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

    James A. Roskind
    Independent Consultant
    516 Latania Palm Drive
    Indialantic FL, 32903
    (407)729-4348
    jar@hq.ileaf.com
    or ...!uunet!leafusa!jar

    ---end of copyright notice---


COMMENTS-

My  goal  is  to  see  software  developers adopt my C++ grammar as a
standard until such time as a better  standard  is  accessible.   The
only  way  to  get it to become a standard, is to be sure that people
know that derivations are based on a specific work.   The  intent  of
releasing  this Flex input file is to facilitate experimentation with
my C++ grammar. The intent  of  the  copyright  notice  is  to  allow
arbitrary  commercial and non-commercial use of this file, as long as
reference is given to my standardization effort.   Without  reference
to  a specific standard, many alternative grammars would develop.  By
referring to the standard, the C++ grammar is given publicity,  which
should  lead  to further use in compatible products and systems.  The
benefits  of  such  a  standard  to  commercial  products  (browsers,
beautifiers,  translators,  compilers,  ...) should be obvious to the
developers, in that other compatible products will  emerge,  and  the
value  of  all  conforming  products  will rise.  Most developers are
aware of the value of acquiring  a  fairly  complete  grammar  for  a
language,  and  the  copyright  notice (and the resulting affiliation
with my work) should not be too high a price to pay.  By copyrighting
my work, I have some minor control over what this standard is, and  I
can  (hopefully)  keep it from degrading without my approval.  I will
consistently attempt to provide upgraded grammars that are  compliant
with  the  current  art, and the ANSI C++ Committee recommendation in
particular.  A  developer  is  never  prevented  from  modifying  the
grammar or this file to improve it in whatever way is seen fit. There
is  also  no  restriction on the sale of copies, or derivative works,
providing the request in the copyright notice are satisfied.

If you are not "copying" my work, but  are  rather  only  abstracting
some of my work, an acknowledgment with references to such a standard
would  be  appreciated.  Specifically, agreements with my grammar and
its resolution of otherwise ambiguous constructs, should be noted.

Simply put: "make whatever use you would like of the grammar and this
file, but include the ``portions Copyright ...'' as  a  reference  to
this standard."


*/

/* Last modified 7/4/91, Version 2.0 */

/* File cpp5.l, becomes lex.yy.c after processing by FLEX  */

/* This file is a dramatically cut down version of the FLEX input file
used in  my  ANSI  C  Preprocessor.   The  executable  version  of  my
preprocessor  is  available on many platforms (shareware), but this is
the only source extract that is currently being distributed.   If  you
need   a   full   ANSI   C  preprocessor,  with  extensive  diagnostic
capabilities and customization facilities, please contact  me  at  the
addresses given above.  Current platforms include IBMPC (DOS/OS2), Sun
(SPARC   and  Motorola),  and  IBM  R/6000.   ...  end  of  commercial
announcement.

This file is being distributed to facilitate experimentation  and  use
of my C and C++ grammar.


Comment  removal  must  be done during the lexing, as context (such as
enclosure in string literals) must be  observed.   For  this  cut-down
lexer,  we  will  assume that comments have been removed (don't assume
this if you are writing a compiler or browser!).


/* For each IDENTIFIER like string that is found,  there  are  several
distinct interpretations that can be applied:

1)  The  preprocessor  may  interpret  the  string as a "keyword" in a
directive (eg: "pragma" or "include", "defined").

2) The parser may interpret the string as a keyword. (eg: "int").

3) Both parser and preprocessor may interpret the string as a  keyword
(eg: "if").

Since  this  file  is based on source that actually lexically analyses
text for both preprocessing and parsing, macro definitions  were  used
throughout.   The macro definitions supplied here have been customized
to a C++ parse only, and  all  preprocessor  keywords  are  passed  as
IDENTIFIER  or  TYPEDEFname.   Also, since there is no symbol table to
interrogate to decide whether a string  is  a  TYPEDEFname,  I  simply
assume  that  any  identifier beginning with an upper case letter is a
TYPEDEFname.  This hack  should  allow  you  to  check  out  how  code
segments  are  parsed  using my grammar.  Unfortunately, if you really
want to parse major league code, you have to write a symbol table, and
maintain appropriate scoping information.



*/


/* Included code before lex code */
/*************** Includes and Defines *****************************/


char  *  yylval;  /*  We  will always point at the text of the lexeme.
          This makes it easy to print out nice trees when  YYDEBUG  is
          enabled.   (see  C++  grammar  file  and  its  definition of
          YYDEBUG_LEXER_TEXT to be "yylval" */



#define WHITE_RETURN(x) /* do nothing */

#define NEW_LINE_RETURN() WHITE_RETURN('\n')

#define PA_KEYWORD_RETURN(x)   RETURN_VAL(x)  /* standard C PArser Keyword */
#define CPP_KEYWORD_RETURN(x)  PA_KEYWORD_RETURN(x)  /* C++ keyword */
#define PPPA_KEYWORD_RETURN(x) RETURN_VAL(x)  /* both PreProcessor and PArser keyword */
#define PP_KEYWORD_RETURN(x)   IDENTIFIER_RETURN()

#define IDENTIFIER_RETURN() RETURN_VAL(isaTYPE(yytext)?TYPEDEFname:IDENTIFIER)

#define PPOP_RETURN(x)       RETURN_VAL((int)*yytext) /* PreProcess and Parser operator */
#define NAMED_PPOP_RETURN(x) /* error: PreProcessor ONLY operator;  Do nothing */
#define ASCIIOP_RETURN(x)    RETURN_VAL((int)*yytext) /* a single character operator */
#define NAMEDOP_RETURN(x)    RETURN_VAL(x)            /* a multichar operator, with a name */

#define NUMERICAL_RETURN(x) RETURN_VAL(x)            /* some sort of constant */
#define LITERAL_RETURN(x)   RETURN_VAL(x)            /* a string literal */

#define RETURN_VAL(x) yylval = yytext; return(x);


# line 204 "cpp5.l"

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

#define YY_END_OF_BUFFER 118
typedef int yy_state_type;
static const short int yy_accept[354] =
    {   0,
        0,    0,  118,  117,    1,    3,    2,   81,  117,   69,
       83,   76,  117,   66,   67,   77,   78,   68,   79,   75,
       82,   61,   60,   89,   90,   84,   91,   85,   88,   59,
       59,   73,   74,   86,   59,   59,   59,   59,   59,   59,
       59,   59,   59,   59,   59,   59,   59,   59,   59,   59,
       59,   59,   71,   87,   72,   80,    1,    3,    0,    2,
      103,    0,   65,    0,   70,  108,  104,  113,    0,    0,
      106,   96,  109,   97,  110,   94,   92,    0,   63,  107,
       63,   61,    0,    0,   61,   61,    0,   60,   60,   60,
       93,   98,  100,  102,  101,   99,   59,    0,    0,  114,

       59,   59,   59,   59,   59,   59,   59,   13,   59,   59,
       59,   59,   59,   59,   59,   59,   24,   59,   59,   59,
       59,   59,   59,   59,   59,   59,   59,   59,   59,   59,
       59,   59,   59,   59,   59,   59,  115,  105,    0,    0,
       64,    0,    0,   95,  116,    0,   63,    0,   63,   61,
       62,   60,  111,  112,   59,   59,   59,   59,   59,   59,
       59,   59,   59,   59,   59,   59,   59,   59,   59,   59,
       22,   59,   59,   59,   59,   59,   59,   28,   59,   59,
       51,   59,   59,   59,   59,   59,   59,   59,   59,   59,
       59,   59,   59,   59,   59,   59,   59,   59,   59,   59,

       59,   59,   59,   59,    0,    0,    0,    0,    0,   63,
       62,   62,    4,   59,    6,    7,   59,   59,   59,   59,
       59,   59,   59,   15,   16,   59,   18,   59,   59,   59,
       59,   23,   59,   59,   59,   59,   29,   30,   59,   59,
       59,   59,   59,   59,   59,   59,   59,   59,   59,   59,
       59,   59,   57,   59,   59,   59,   59,   59,   44,   59,
       59,    0,    0,    0,    0,   62,    5,   47,    8,   59,
       59,   59,   59,   59,   17,   19,   59,   21,   59,   25,
       59,   59,   59,   59,   59,   59,   59,   59,   59,   59,
       59,   34,   59,   59,   59,   59,   59,   59,   41,   42,

       59,   59,   59,   46,   59,   59,   11,   48,   14,   20,
       49,   26,   59,   50,   59,   59,   31,   59,   59,   56,
       59,   33,   35,   36,   37,   38,   39,   59,   59,   59,
       59,   59,   10,   12,   27,   59,   59,   55,   59,   59,
       40,   59,   58,   59,    9,   52,   53,   59,   32,   43,
       45,   54,    0
    } ;

static const YY_CHAR yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        4,    5,    6,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    7,    8,    9,   10,    1,   11,   12,   13,   14,
       15,   16,   17,   18,   19,   20,   21,   22,   23,   23,
       23,   23,   23,   23,   23,   24,   24,   25,   26,   27,
       28,   29,   30,    1,   31,   31,   31,   31,   32,   33,
       34,   34,   34,   34,   34,   35,   34,   34,   34,   34,
       34,   34,   34,   34,   36,   34,   34,   37,   34,   34,
       38,   39,   40,   41,   34,    1,   42,   43,   44,   45,

       46,   47,   48,   49,   50,   34,   51,   52,   53,   54,
       55,   56,   34,   57,   58,   59,   60,   61,   62,   63,
       64,   65,   66,   67,   68,   69,    1,    1,    1,    1,
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

static const YY_CHAR yy_meta[70] =
    {   0,
        1,    1,    2,    1,    1,    1,    1,    1,    3,    1,
        1,    1,    4,    1,    1,    1,    1,    1,    1,    1,
        1,    5,    5,    6,    1,    1,    1,    1,    1,    3,
        6,    6,    6,    7,    7,    7,    7,    1,    3,    1,
        1,    5,    5,    6,    6,    6,    5,    7,    7,    7,
        7,    7,    7,    8,    7,    7,    8,    7,    8,    7,
        8,    7,    8,    7,    7,    1,    1,    1,    1
    } ;

static const short int yy_base[362] =
    {   0,
        0,    0,  560,  561,   68,  561,   74,  531,   73,  548,
      529,   71,  517,  561,  561,  527,   67,  561,   68,   69,
      526,   93,   86,  528,  561,   73,  524,   74,  561,    0,
       77,  561,  561,  523,  490,  492,   82,   52,   95,  102,
      493,   57,   64,  501,   79,   63,  500,  101,   77,  491,
       93,  495,  561,   60,  561,  561,  162,  561,  168,  174,
      561,  146,  561,  139,  561,  561,  561,  561,  169,  161,
      561,  561,  561,  561,  561,  527,  561,  522,  164,  561,
      168,  199,  206,  220,  153,  160,    0,  226,  157,  201,
      561,  513,  561,  561,  561,  512,    0,  216,  500,  561,

      479,  491,  478,  493,  492,  479,  157,  472,   86,  173,
      474,  471,  474,  471,  477,  467,   88,  188,  471,  470,
      461,  476,  475,  214,  477,   46,  464,  206,  218,  468,
      467,  460,  218,  458,  155,  464,  561,  561,  257,    0,
      561,  260,    0,  561,  561,  268,  561,  271,  278,  561,
      262,  561,  561,  561,  458,  470,  465,  453,  451,  230,
      156,  462,  464,  459,  459,  454,  450,  447,  455,  458,
        0,  453,  443,  451,  451,  443,  444,    0,  447,  444,
        0,  434,  433,  441,  427,  428,  434,  435,  424,  426,
      428,  435,  421,  419,  419,  419,  430,  429,  419,  423,

      413,  426,  428,  417,  294,  325,  296,  360,  282,  328,
      205,  222,    0,  417,    0,    0,  409,  407,  415,  404,
      409,  403,  409,    0,    0,  413,    0,  402,  401,  398,
      402,    0,  408,  408,  393,  398,    0,    0,  409,  398,
      396,  406,  401,  396,  380,  380,  373,  385,  364,  367,
      365,  357,    0,  355,  351,  343,  348,  335,    0,  335,
      344,  268,    0,  271,    0,  561,    0,    0,    0,  335,
      336,  341,  340,  339,    0,    0,  327,    0,  334,    0,
      331,  332,  330,  315,  311,  323,  303,  316,  315,  296,
      300,    0,  308,  299,  301,  285,  294,  296,    0,    0,

      287,  298,  289,    0,  278,  278,  291,    0,    0,    0,
        0,    0,  286,    0,  276,  287,    0,  282,  268,    0,
      280,    0,    0,    0,    0,    0,    0,  277,  277,  269,
      268,  269,    0,    0,    0,  255,  263,    0,  235,  213,
        0,  222,    0,  181,    0,    0,    0,  149,    0,    0,
        0,    0,  561,  407,  415,  419,  425,  431,  435,  437,
      439
    } ;

static const short int yy_def[362] =
    {   0,
      353,    1,  353,  353,  353,  353,  353,  353,  354,  353,
      353,  353,  355,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  356,
      356,  353,  353,  353,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  354,  353,  357,  353,  353,  353,  353,  355,  358,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  359,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  356,  354,  355,  353,

      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  353,  353,  354,  360,
      353,  355,  361,  353,  353,  353,  353,  353,  353,  353,
      359,  353,  353,  353,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,

      356,  356,  356,  356,  354,  354,  355,  355,  353,  353,
      353,  353,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  354,  206,  355,  208,  353,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,

      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,    0,  353,  353,  353,  353,  353,  353,  353,
      353
    } ;

static const short int yy_nxt[631] =
    {   0,
        4,    5,    6,    7,    7,    7,    5,    8,    9,   10,
       11,   12,   13,   14,   15,   16,   17,   18,   19,   20,
       21,   22,   23,   23,   24,   25,   26,   27,   28,   29,
       30,   30,   30,   30,   31,   30,   30,   32,    4,   33,
       34,   35,   36,   37,   38,   39,   40,   41,   30,   42,
       30,   43,   30,   44,   45,   46,   47,   48,   49,   50,
       51,   52,   30,   30,   30,   53,   54,   55,   56,   57,
       58,   59,   59,   59,   57,   59,   58,   60,   60,   60,
       59,   63,   67,   72,   77,   98,   74,  137,   78,   99,
       79,   79,   79,  188,   73,   75,   76,  107,   68,   92,

       93,   95,   96,  117,  189,   81,  108,   88,   88,   88,
      118,   64,   81,  119,   82,   82,   83,   84,  120,  124,
       89,   90,  125,  103,   84,  131,  138,   85,   86,   87,
      104,   84,  174,  105,  122,  164,  106,   89,   84,  123,
      132,  175,  134,  165,   85,   90,  109,  135,  110,  127,
      128,  111,   86,  113,   63,   87,  114,  112,  115,  129,
      139,  139,  130,   57,   58,   59,   59,   59,   57,   59,
       58,   59,   59,   59,   59,   59,   58,   60,   60,   60,
       59,  141,  142,  142,   64,   79,   79,   79,  150,   79,
       79,   79,  152,  352,  150,  146,  147,  220,  147,  146,

      147,  140,  147,  161,  202,  221,  203,   70,  162,  146,
      147,  150,  150,  146,  147,  147,  152,  166,   81,  147,
       82,   82,   83,  143,   63,   81,  351,   83,   83,   83,
       84,  176,  167,   85,   86,  152,  148,   84,  148,  177,
      266,  149,  149,  149,   84,   81,  178,   88,   88,   88,
       85,   84,  152,  191,   64,  184,  266,   84,   86,  193,
       89,   90,  198,  185,  266,   63,  350,  199,  186,  349,
      192,   84,  141,  266,  194,  200,   63,   89,  205,  205,
      348,  207,  207,  141,  209,   90,  209,  218,  219,  210,
      210,  210,  149,  149,  149,   64,  211,  212,   70,  149,

      149,  149,   63,  210,  210,  210,   64,  347,  141,   70,
      147,  346,  147,  211,  345,  262,  262,  264,  264,  344,
      343,  212,  342,  341,  147,  340,  339,  338,  337,  147,
      336,  335,   64,   63,   70,  334,  333,  332,  331,  330,
      329,  328,  327,  326,  325,  324,  263,  263,  263,  210,
      210,  210,  323,  322,  321,  263,  263,  263,  320,  319,
      147,  318,  147,   64,  317,  316,  263,  263,  263,  263,
      263,  263,  141,  315,  147,  314,  313,  312,  311,  147,
      310,  265,  265,  265,  309,  308,  307,  306,  305,  304,
      265,  265,  265,  303,  302,  301,  300,  299,   70,  298,

      297,  265,  265,  265,  265,  265,  265,   62,  296,   62,
       62,   62,   62,   62,   62,   69,  295,   69,  294,   69,
       69,   69,   69,   97,   97,   97,   97,   62,   62,   62,
      293,  292,   62,   69,   69,   69,  291,  290,   69,  151,
      151,  206,  206,  208,  208,  289,  288,  287,  286,  285,
      284,  283,  282,  281,  280,  279,  278,  277,  276,  275,
      274,  273,  272,  271,  270,  269,  268,  267,  261,  260,
      259,  258,  257,  256,  255,  254,  253,  252,  251,  250,
      249,  248,  247,  246,  245,  244,  243,  242,  241,  240,
      239,  238,  237,  236,  235,  234,  233,  232,  231,  230,

      229,  228,  227,  226,  225,  224,  223,  222,  217,  216,
      215,  214,  213,  204,  201,  197,  196,  195,  190,  187,
      183,  182,  181,  180,  179,  173,  172,  171,  170,  169,
      168,  163,  160,  159,  158,  157,  156,  155,   70,  154,
      153,  145,  144,  136,  133,  126,  121,  116,  102,  101,
      100,   94,   91,   80,   71,   70,   66,   65,   61,  353,
        3,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,

      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353
    } ;

static const short int yy_chk[631] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    5,
        5,    5,    5,    5,    5,    7,    7,    7,    7,    7,
        7,    9,   12,   17,   20,   31,   19,   54,   20,   31,
       20,   20,   20,  126,   17,   19,   19,   38,   12,   26,

       26,   28,   28,   42,  126,   23,   38,   23,   23,   23,
       42,    9,   22,   43,   22,   22,   22,   23,   43,   46,
       23,   23,   46,   37,   22,   49,   54,   22,   22,   22,
       37,   23,  117,   37,   45,  109,   37,   23,   22,   45,
       49,  117,   51,  109,   22,   23,   39,   51,   39,   48,
       48,   39,   22,   40,   62,   22,   40,   39,   40,   48,
       64,   64,   48,   57,   57,   57,   57,   57,   57,   59,
       59,   59,   59,   59,   59,   60,   60,   60,   60,   60,
       60,   69,   70,   70,   62,   79,   79,   79,   85,   81,
       81,   81,   89,  348,   86,   79,   79,  161,   79,   81,

       81,   64,   81,  107,  135,  161,  135,   69,  107,   79,
       79,   86,   85,   81,   81,   79,   89,  110,   82,   81,
       82,   82,   82,   70,   98,   83,  344,   83,   83,   83,
       82,  118,  110,   82,   82,   90,   84,   83,   84,  118,
      211,   84,   84,   84,   82,   88,  118,   88,   88,   88,
       82,   83,   90,  128,   98,  124,  212,   88,   82,  129,
       88,   88,  133,  124,  211,  139,  342,  133,  124,  340,
      128,   88,  142,  212,  129,  133,  262,   88,  139,  139,
      339,  142,  142,  264,  146,   88,  146,  160,  160,  146,
      146,  146,  148,  148,  148,  139,  151,  151,  142,  149,

      149,  149,  205,  209,  209,  209,  262,  337,  207,  264,
      149,  336,  149,  151,  332,  205,  205,  207,  207,  331,
      330,  151,  329,  328,  149,  321,  319,  318,  316,  149,
      315,  313,  205,  206,  207,  307,  306,  305,  303,  302,
      301,  298,  297,  296,  295,  294,  206,  206,  206,  210,
      210,  210,  293,  291,  290,  206,  206,  206,  289,  288,
      210,  287,  210,  206,  286,  285,  206,  206,  206,  206,
      206,  206,  208,  284,  210,  283,  282,  281,  279,  210,
      277,  208,  208,  208,  274,  273,  272,  271,  270,  261,
      208,  208,  208,  260,  258,  257,  256,  255,  208,  254,

      252,  208,  208,  208,  208,  208,  208,  354,  251,  354,
      354,  354,  354,  354,  354,  355,  250,  355,  249,  355,
      355,  355,  355,  356,  356,  356,  356,  357,  357,  357,
      248,  247,  357,  358,  358,  358,  246,  245,  358,  359,
      359,  360,  360,  361,  361,  244,  243,  242,  241,  240,
      239,  236,  235,  234,  233,  231,  230,  229,  228,  226,
      223,  222,  221,  220,  219,  218,  217,  214,  204,  203,
      202,  201,  200,  199,  198,  197,  196,  195,  194,  193,
      192,  191,  190,  189,  188,  187,  186,  185,  184,  183,
      182,  180,  179,  177,  176,  175,  174,  173,  172,  170,

      169,  168,  167,  166,  165,  164,  163,  162,  159,  158,
      157,  156,  155,  136,  134,  132,  131,  130,  127,  125,
      123,  122,  121,  120,  119,  116,  115,  114,  113,  112,
      111,  108,  106,  105,  104,  103,  102,  101,   99,   96,
       92,   78,   76,   52,   50,   47,   44,   41,   36,   35,
       34,   27,   24,   21,   16,   13,   11,   10,    8,    3,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,

      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353
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
		if ( yy_current_state >= 354 )
		    yy_c = yy_meta[yy_c];
		}
	    yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
	    ++yy_cp;
	    }
	while ( yy_current_state != 353 );
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
# line 207 "cpp5.l"
{
			WHITE_RETURN(' ');
			}
	YY_BREAK
case 2:
# line 211 "cpp5.l"
{
			WHITE_RETURN(' ');
			}
	YY_BREAK
case 3:
# line 216 "cpp5.l"
{
			NEW_LINE_RETURN();
			}
	YY_BREAK
case 4:
# line 220 "cpp5.l"
{PA_KEYWORD_RETURN(AUTO);}
	YY_BREAK
case 5:
# line 221 "cpp5.l"
{PA_KEYWORD_RETURN(BREAK);}
	YY_BREAK
case 6:
# line 222 "cpp5.l"
{PA_KEYWORD_RETURN(CASE);}
	YY_BREAK
case 7:
# line 223 "cpp5.l"
{PA_KEYWORD_RETURN(CHAR);}
	YY_BREAK
case 8:
# line 224 "cpp5.l"
{PA_KEYWORD_RETURN(CONST);}
	YY_BREAK
case 9:
# line 225 "cpp5.l"
{PA_KEYWORD_RETURN(CONTINUE);}
	YY_BREAK
case 10:
# line 226 "cpp5.l"
{PA_KEYWORD_RETURN(DEFAULT);}
	YY_BREAK
case 11:
# line 227 "cpp5.l"
{PP_KEYWORD_RETURN(DEFINE);}
	YY_BREAK
case 12:
# line 228 "cpp5.l"
{PP_KEYWORD_RETURN(OPDEFINED);}
	YY_BREAK
case 13:
# line 229 "cpp5.l"
{PA_KEYWORD_RETURN(DO);}
	YY_BREAK
case 14:
# line 230 "cpp5.l"
{PA_KEYWORD_RETURN(DOUBLE);}
	YY_BREAK
case 15:
# line 231 "cpp5.l"
{PP_KEYWORD_RETURN(ELIF);}
	YY_BREAK
case 16:
# line 232 "cpp5.l"
{PPPA_KEYWORD_RETURN(ELSE);}
	YY_BREAK
case 17:
# line 233 "cpp5.l"
{PP_KEYWORD_RETURN(ENDIF);}
	YY_BREAK
case 18:
# line 234 "cpp5.l"
{PA_KEYWORD_RETURN(ENUM);}
	YY_BREAK
case 19:
# line 235 "cpp5.l"
{PP_KEYWORD_RETURN(ERROR);}
	YY_BREAK
case 20:
# line 236 "cpp5.l"
{PA_KEYWORD_RETURN(EXTERN);}
	YY_BREAK
case 21:
# line 237 "cpp5.l"
{PA_KEYWORD_RETURN(FLOAT);}
	YY_BREAK
case 22:
# line 238 "cpp5.l"
{PA_KEYWORD_RETURN(FOR);}
	YY_BREAK
case 23:
# line 239 "cpp5.l"
{PA_KEYWORD_RETURN(GOTO);}
	YY_BREAK
case 24:
# line 240 "cpp5.l"
{PPPA_KEYWORD_RETURN(IF);}
	YY_BREAK
case 25:
# line 241 "cpp5.l"
{PP_KEYWORD_RETURN(IFDEF);}
	YY_BREAK
case 26:
# line 242 "cpp5.l"
{PP_KEYWORD_RETURN(IFNDEF);}
	YY_BREAK
case 27:
# line 243 "cpp5.l"
{PP_KEYWORD_RETURN(INCLUDE); }
	YY_BREAK
case 28:
# line 244 "cpp5.l"
{PA_KEYWORD_RETURN(INT);}
	YY_BREAK
case 29:
# line 245 "cpp5.l"
{PP_KEYWORD_RETURN(LINE);}
	YY_BREAK
case 30:
# line 246 "cpp5.l"
{PA_KEYWORD_RETURN(LONG);}
	YY_BREAK
case 31:
# line 247 "cpp5.l"
{PP_KEYWORD_RETURN(PRAGMA);}
	YY_BREAK
case 32:
# line 248 "cpp5.l"
{PA_KEYWORD_RETURN(REGISTER);}
	YY_BREAK
case 33:
# line 249 "cpp5.l"
{PA_KEYWORD_RETURN(RETURN);}
	YY_BREAK
case 34:
# line 250 "cpp5.l"
{PA_KEYWORD_RETURN(SHORT);}
	YY_BREAK
case 35:
# line 251 "cpp5.l"
{PA_KEYWORD_RETURN(SIGNED);}
	YY_BREAK
case 36:
# line 252 "cpp5.l"
{PA_KEYWORD_RETURN(SIZEOF);}
	YY_BREAK
case 37:
# line 253 "cpp5.l"
{PA_KEYWORD_RETURN(STATIC);}
	YY_BREAK
case 38:
# line 254 "cpp5.l"
{PA_KEYWORD_RETURN(STRUCT);}
	YY_BREAK
case 39:
# line 255 "cpp5.l"
{PA_KEYWORD_RETURN(SWITCH);}
	YY_BREAK
case 40:
# line 256 "cpp5.l"
{PA_KEYWORD_RETURN(TYPEDEF);}
	YY_BREAK
case 41:
# line 257 "cpp5.l"
{PP_KEYWORD_RETURN(UNDEF);}
	YY_BREAK
case 42:
# line 258 "cpp5.l"
{PA_KEYWORD_RETURN(UNION);}
	YY_BREAK
case 43:
# line 259 "cpp5.l"
{PA_KEYWORD_RETURN(UNSIGNED);}
	YY_BREAK
case 44:
# line 260 "cpp5.l"
{PA_KEYWORD_RETURN(VOID);}
	YY_BREAK
case 45:
# line 261 "cpp5.l"
{PA_KEYWORD_RETURN(VOLATILE);}
	YY_BREAK
case 46:
# line 262 "cpp5.l"
{PA_KEYWORD_RETURN(WHILE);}
	YY_BREAK
case 47:
# line 265 "cpp5.l"
{CPP_KEYWORD_RETURN(CLASS);}
	YY_BREAK
case 48:
# line 266 "cpp5.l"
{CPP_KEYWORD_RETURN(DELETE);}
	YY_BREAK
case 49:
# line 267 "cpp5.l"
{CPP_KEYWORD_RETURN(FRIEND);}
	YY_BREAK
case 50:
# line 268 "cpp5.l"
{CPP_KEYWORD_RETURN(INLINE);}
	YY_BREAK
case 51:
# line 269 "cpp5.l"
{CPP_KEYWORD_RETURN(NEW);}
	YY_BREAK
case 52:
# line 270 "cpp5.l"
{CPP_KEYWORD_RETURN(OPERATOR);}
	YY_BREAK
case 53:
# line 271 "cpp5.l"
{CPP_KEYWORD_RETURN(OVERLOAD);}
	YY_BREAK
case 54:
# line 272 "cpp5.l"
{CPP_KEYWORD_RETURN(PROTECTED);}
	YY_BREAK
case 55:
# line 273 "cpp5.l"
{CPP_KEYWORD_RETURN(PRIVATE);}
	YY_BREAK
case 56:
# line 274 "cpp5.l"
{CPP_KEYWORD_RETURN(PUBLIC);}
	YY_BREAK
case 57:
# line 275 "cpp5.l"
{CPP_KEYWORD_RETURN(THIS);}
	YY_BREAK
case 58:
# line 276 "cpp5.l"
{CPP_KEYWORD_RETURN(VIRTUAL);}
	YY_BREAK
case 59:
# line 278 "cpp5.l"
{IDENTIFIER_RETURN();}
	YY_BREAK
case 60:
# line 280 "cpp5.l"
{NUMERICAL_RETURN(INTEGERconstant);}
	YY_BREAK
case 61:
# line 281 "cpp5.l"
{NUMERICAL_RETURN(OCTALconstant);}
	YY_BREAK
case 62:
# line 282 "cpp5.l"
{NUMERICAL_RETURN(HEXconstant);}
	YY_BREAK
case 63:
# line 283 "cpp5.l"
{NUMERICAL_RETURN(FLOATINGconstant);}
	YY_BREAK
case 64:
# line 286 "cpp5.l"
{
			NUMERICAL_RETURN(CHARACTERconstant);
			}
	YY_BREAK
case 65:
# line 291 "cpp5.l"
{
			LITERAL_RETURN(STRINGliteral);}
	YY_BREAK
case 66:
# line 297 "cpp5.l"
{PPOP_RETURN(LP);}
	YY_BREAK
case 67:
# line 298 "cpp5.l"
{PPOP_RETURN(RP);}
	YY_BREAK
case 68:
# line 299 "cpp5.l"
{PPOP_RETURN(COMMA);}
	YY_BREAK
case 69:
# line 300 "cpp5.l"
{NAMED_PPOP_RETURN('#') ;}
	YY_BREAK
case 70:
# line 301 "cpp5.l"
{NAMED_PPOP_RETURN(POUNDPOUND);}
	YY_BREAK
case 71:
# line 303 "cpp5.l"
{ASCIIOP_RETURN(LC);}
	YY_BREAK
case 72:
# line 304 "cpp5.l"
{ASCIIOP_RETURN(RC);}
	YY_BREAK
case 73:
# line 305 "cpp5.l"
{ASCIIOP_RETURN(LB);}
	YY_BREAK
case 74:
# line 306 "cpp5.l"
{ASCIIOP_RETURN(RB);}
	YY_BREAK
case 75:
# line 307 "cpp5.l"
{ASCIIOP_RETURN(DOT);}
	YY_BREAK
case 76:
# line 308 "cpp5.l"
{ASCIIOP_RETURN(AND);}
	YY_BREAK
case 77:
# line 309 "cpp5.l"
{ASCIIOP_RETURN(STAR);}
	YY_BREAK
case 78:
# line 310 "cpp5.l"
{ASCIIOP_RETURN(PLUS);}
	YY_BREAK
case 79:
# line 311 "cpp5.l"
{ASCIIOP_RETURN(MINUS);}
	YY_BREAK
case 80:
# line 312 "cpp5.l"
{ASCIIOP_RETURN(NEGATE);}
	YY_BREAK
case 81:
# line 313 "cpp5.l"
{ASCIIOP_RETURN(NOT);}
	YY_BREAK
case 82:
# line 314 "cpp5.l"
{ASCIIOP_RETURN(DIV);}
	YY_BREAK
case 83:
# line 315 "cpp5.l"
{ASCIIOP_RETURN(MOD);}
	YY_BREAK
case 84:
# line 316 "cpp5.l"
{ASCIIOP_RETURN(LT);}
	YY_BREAK
case 85:
# line 317 "cpp5.l"
{ASCIIOP_RETURN(GT);}
	YY_BREAK
case 86:
# line 318 "cpp5.l"
{ASCIIOP_RETURN(XOR);}
	YY_BREAK
case 87:
# line 319 "cpp5.l"
{ASCIIOP_RETURN(PIPE);}
	YY_BREAK
case 88:
# line 320 "cpp5.l"
{ASCIIOP_RETURN(QUESTION);}
	YY_BREAK
case 89:
# line 321 "cpp5.l"
{ASCIIOP_RETURN(COLON);}
	YY_BREAK
case 90:
# line 322 "cpp5.l"
{ASCIIOP_RETURN(SEMICOLON);}
	YY_BREAK
case 91:
# line 323 "cpp5.l"
{ASCIIOP_RETURN(ASSIGN);}
	YY_BREAK
case 92:
# line 325 "cpp5.l"
{NAMEDOP_RETURN(DOTstar);}
	YY_BREAK
case 93:
# line 326 "cpp5.l"
{NAMEDOP_RETURN(CLCL);}
	YY_BREAK
case 94:
# line 327 "cpp5.l"
{NAMEDOP_RETURN(ARROW);}
	YY_BREAK
case 95:
# line 328 "cpp5.l"
{NAMEDOP_RETURN(ARROWstar);}
	YY_BREAK
case 96:
# line 329 "cpp5.l"
{NAMEDOP_RETURN(ICR);}
	YY_BREAK
case 97:
# line 330 "cpp5.l"
{NAMEDOP_RETURN(DECR);}
	YY_BREAK
case 98:
# line 331 "cpp5.l"
{NAMEDOP_RETURN(LSH);}
	YY_BREAK
case 99:
# line 332 "cpp5.l"
{NAMEDOP_RETURN(RSH);}
	YY_BREAK
case 100:
# line 333 "cpp5.l"
{NAMEDOP_RETURN(LE);}
	YY_BREAK
case 101:
# line 334 "cpp5.l"
{NAMEDOP_RETURN(GE);}
	YY_BREAK
case 102:
# line 335 "cpp5.l"
{NAMEDOP_RETURN(EQ);}
	YY_BREAK
case 103:
# line 336 "cpp5.l"
{NAMEDOP_RETURN(NE);}
	YY_BREAK
case 104:
# line 337 "cpp5.l"
{NAMEDOP_RETURN(ANDAND);}
	YY_BREAK
case 105:
# line 338 "cpp5.l"
{NAMEDOP_RETURN(OROR);}
	YY_BREAK
case 106:
# line 339 "cpp5.l"
{NAMEDOP_RETURN(MULTassign);}
	YY_BREAK
case 107:
# line 340 "cpp5.l"
{NAMEDOP_RETURN(DIVassign);}
	YY_BREAK
case 108:
# line 341 "cpp5.l"
{NAMEDOP_RETURN(MODassign);}
	YY_BREAK
case 109:
# line 342 "cpp5.l"
{NAMEDOP_RETURN(PLUSassign);}
	YY_BREAK
case 110:
# line 343 "cpp5.l"
{NAMEDOP_RETURN(MINUSassign);}
	YY_BREAK
case 111:
# line 344 "cpp5.l"
{NAMEDOP_RETURN(LSHassign);}
	YY_BREAK
case 112:
# line 345 "cpp5.l"
{NAMEDOP_RETURN(RSHassign);}
	YY_BREAK
case 113:
# line 346 "cpp5.l"
{NAMEDOP_RETURN(ANDassign);}
	YY_BREAK
case 114:
# line 347 "cpp5.l"
{NAMEDOP_RETURN(ERassign);}
	YY_BREAK
case 115:
# line 348 "cpp5.l"
{NAMEDOP_RETURN(ORassign);}
	YY_BREAK
case 116:
# line 349 "cpp5.l"
{NAMEDOP_RETURN(ELLIPSIS);}
	YY_BREAK
case 117:
# line 353 "cpp5.l"
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
	    if ( yy_current_state >= 354 )
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
	if ( yy_current_state >= 354 )
	    yy_c = yy_meta[yy_c];
	}
    yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
    yy_is_jam = (yy_current_state == 353);

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
# line 353 "cpp5.l"


/* I won't bother to provide any error recovery. I won't  even  handle
unknown characters */

/*******************************************************************/
int isaTYPE(string)
char * string;
{
    /*  We  should  really  be  maintaining  a  symbol  table,  and be
    carefully keeping track of what the current scope is  (or  in  the
    case  of  "rescoped"  stuff,  what  scope  to  look in). Since the
    grammar is not annotated with  actions  to  track  transitions  to
    various  scopes,  and  there  is no symbol table, we will supply a
    hack to allow folks to test  the  grammar  out.   THIS  IS  NOT  A
    COMPLETE IMPLEMENTATION!!!! */

    return ('A' <= string[0] && 'Z' >= string[0]);
}

yyerror(string)
char*string;
{
    printf("parser error: %s\n", string);
}


main()
{
    yyparse();
}
