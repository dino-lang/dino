/*
   FILE NAME:   yacc.h

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool SPRUT.

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

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
extern int yylex (void);
extern int yyerror (const char *message);
extern int yyparse ();
extern void finish_parser (void);
