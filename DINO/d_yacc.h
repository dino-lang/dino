/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of interpreter of DINO.

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

*/

extern IR_node_t first_program_stmt;

#include "position.h"

extern const char *source_file_encoding (const char *fname);
extern void initiate_scanner (void);
extern void start_scanner_file (const char *file_name, const char *encoding,
				position_t error_pos);
extern const ucode_t *get_read_line (int n);
extern void skip_line_rest ();
extern int yyparse (void);
extern void finish_scanner (void);
extern void initiate_parser (void);
extern void initiate_new_parser_REPL_stmts (void);
extern void finish_parser (void);
