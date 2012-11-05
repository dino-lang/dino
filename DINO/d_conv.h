/*
   Copyright (C) 1997-2012 Vladimir Makarov.

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

#include "d_run.h"

extern ER_node_t to_vect_string_conversion (ER_node_t var, const char *format,
					    int tvar_num);
extern ER_node_t implicit_arithmetic_conversion (ER_node_t op1, int tvar_num);
extern void implicit_conversion_for_binary_arithmetic_op (ER_node_t op1,
							  ER_node_t op2,
							  ER_node_t *l,
							  ER_node_t *r);
extern ER_node_t implicit_int_conversion (ER_node_t op, int new_p);
extern void implicit_conversion_for_binary_int_op (ER_node_t op1, ER_node_t op2,
						   ER_node_t *l, ER_node_t *r);
extern void implicit_conversion_for_binary_string_op (void);
extern void implicit_conversion_for_eq_op (ER_node_t op1, ER_node_t op2,
					   ER_node_t *l, ER_node_t *r);
