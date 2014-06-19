/*
   Copyright (C) 1997-2014 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

extern void repl_print (ER_node_t val, int def_p);
extern int internal_inside_call (const char **message_ptr, ER_node_t where,
				 ER_node_t what, int context_flag);
extern int internal_isa_call (const char **message_ptr, ER_node_t where,
			      ER_node_t what);
extern void print_trace_stack (void);
extern void process_imm_ifun_call (BC_node_t code, int actuals_num);
extern void process_fun_class_call (ER_node_t call_start, int_t pars_number,
				    int tail_call_flag);
extern void initiate_funcs (void);
