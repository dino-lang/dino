/*
   Copyright (C) 1997-2002 Vladimir Makarov.

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

token_string_t get_new_token_string (IR_node_t *tokens, int tokens_number);
IR_node_t get_n_th_token (token_string_t token_string, int n);
int token_string_comparison (token_string_t token_string_1,
                             token_string_t token_string_2);
int token_string_length (token_string_t token_string);
token_string_t token_string_shortening
                     (token_string_t token_string,
                      int maximum_result_token_string_length);
void output_token_string (token_string_t token_string, FILE *f);

extern context_t null_context_in_table;

context_t get_null_context (void);
void free_context (context_t context);
int it_is_in_context (int order_number, context_t context);
void set_context_element_value (context_t context,
                                token_string_t element_number,
                                int element_value);
void context_copy (context_t to, context_t from);
void zero_context (context_t context);
int context_size (context_t context);
int it_is_zero_context (context_t context);
int context_in (context_t context_1, context_t context_2);
void context_or (context_t context_1, context_t context_2);
void context_and (context_t context_1, context_t context_2);
void context_or_of_and (context_t or_context,
                        context_t and_context_1, context_t and_context_2);
void context_subtraction (context_t context_1, context_t context_2);
void context_concat (context_t context_1, context_t context_2,
                     int maximum_result_token_string_length);
void process_context_token_strings
                    (context_t context,
                     void (*applied_function) (token_string_t token_string));
context_t context_shortening (context_t context,
                              int maximum_result_token_string_length);
unsigned context_hash_value (context_t context);
int context_eq (context_t context_1, context_t context_2);
context_t insert_or_free_context (context_t context_outside_table);
void output_context (FILE *f, context_t context);
void initiate_contexts (void);
void finish_contexts (void);
