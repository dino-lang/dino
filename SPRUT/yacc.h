/*
   FILE NAME:   yacc.h

   TITLE:       Include file of parser of SPRUT (internal representation
                description translator)

   DESCRIPTION: This header file contains ANSI C prototype definitions of
                external functions of SPRUT parser.

   SPECIAL CONSIDERATION:
         The function `initiate_parser' is to be called only once as the first
       parser function.  The function `finish_parser' is to be called
       only once as the last parser function.  This file can not be
       included repeatedly.


*/

#include "position.h"

extern void initiate_parser (void);
extern void start_parser_file (const char *new_file_name,
                               position_t error_position);
extern yyparse ();
extern void finish_parser (void);
