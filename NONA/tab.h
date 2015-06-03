/*
   FILE NAME:   tab.h

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool NONA.

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

   TITLE:       Include file of NONA (code selector description translator)
                tables

   DESCRIPTION: This header file contains ANSI C prototype definitions of
                external functions which implement the following tables
                      table of identifiers
                      table of single declarations (of terminals
                                                    or noterminals)

   SPECIAL CONSIDERATION:
       This file can not be included repeatedly.

*/

#include "ird.h"

/* Table of identifiers. */

extern char *insert_identifier (const char *identifier);
extern void initiate_identifier_table (void);
extern void finish_identifier_table (void);

/* Table of single declarations. */

extern IR_node_t insert_single_declaration (IR_node_t single_declaration);
extern IR_node_t find_single_declaration (IR_node_t identifier);
extern void initiate_single_declaration_table (void);
extern void finish_single_declaration_table (void);
