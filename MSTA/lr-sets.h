/*
   Copyright (C) 1997-2005 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of the tool MSTA.

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

void initiate_LR_situations (void);
IR_node_t get_new_LR_situation (IR_node_t element_after_dot, context_t context,
                                IR_node_t LR_set, IR_node_t next_LR_situation,
                                int important_LR_situation_flag);
void free_LR_situation (IR_node_t LR_situation);
void free_LR_situations_list (IR_node_t LR_situations_list);
void output_LR_situation (FILE *f, IR_node_t LR_situation,
                          const char *indent, int new_line_flag);
void finish_LR_situations (void);

IR_node_t insert_LR_core (IR_node_t LR_core);
IR_node_t find_LR_core (IR_node_t LR_situation_list);
void initiate_LR_core_table (void);
void finish_LR_core_table (void);

IR_node_t insert_LR_set (IR_node_t LR_set);
IR_node_t find_LR_set (IR_node_t LR_core, IR_node_t LR_situation_list);
void delete_LR_set_from_table (IR_node_t LR_set);
void initiate_LR_set_table (void);
void finish_LR_set_table (void);
void output_LR_set_situations (FILE *f, IR_node_t LR_set, const char *indent);

