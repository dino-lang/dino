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

void output_string (FILE *f, const char *string);
void output_char (int ch, FILE *f);
void output_decimal_number (FILE *f, int number, int minimum_width);
void initiate_output (void);
int identifier_or_literal_representation (IR_node_t identifier_or_literal,
                                          int in_string_flag,
                                          vlo_t *representation);
int output_identifier_or_literal (FILE *f, IR_node_t identifier_or_literal,
                                  int in_string_flag);
void single_definition_representation (IR_node_t single_definition,
                                       vlo_t *representation);
void output_single_definition (FILE *f, IR_node_t single_definition);
void output_line (FILE *f, int line_number, const char *file_name);
void output_current_line (FILE *f);
