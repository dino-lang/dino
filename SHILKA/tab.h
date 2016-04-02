/*
   FILE NAME:   tab.h

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool SHILKA.

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

   TITLE:       Include file of Shilka (keywords description translator)
                tables

   DESCRIPTION: This header file contains ANSI C prototype definitions of
                external functions which implement the following tables
                      table of strings (and identifiers)
                      table of keywords
                      table of keyword names

   SPECIAL CONSIDERATION:
       This file can not be included repeatedly.

*/

#include "ird.h"

/* Table of strings. */

extern char *insert_string (const char *string);
extern void initiate_string_table (void);
extern void finish_string_table (void);

/* Table of keywords. */

extern IR_node_t insert_keyword (IR_node_t keyword);
extern void initiate_keyword_table (void);
extern void finish_keyword_table (void);

/* Table of keyword names. */

extern IR_node_t insert_keyword_name (IR_node_t keyword);
extern void initiate_keyword_name_table (void);
extern void finish_keyword_name_table (void);
