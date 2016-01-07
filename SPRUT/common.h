/*
   FILE NAME:   common.h

   Copyright (C) 1997-2015 Vladimir Makarov.

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

   TITLE:       Include file of common things for all SPRUT (internal
                representation description translator)

   DESCRIPTION: This header file contains definitions of macros
                and external definitions of variables common for
                all SPRUT (internal representation translator).

   SPECIAL CONSIDERATION:
         This file can not be included repeatedly.
   
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ird.h"

/* Standard designators for true and false values. */

#define FALSE 0
#define TRUE  1

/* Suffix which any internal representation description file name must
   have.  If this macro value is changed then documentation of sprut must
   be changed. */

#define STANDARD_INPUT_FILE_SUFFIX ".sprut"

extern int v_flag;
extern int cpp_flag;
extern int statistics_flag;
extern int flat_flag;
extern int flat_structure_flag;
extern int fast_flag;
extern int long_node_size_flag;
extern int short_node_size_flag;
extern int macro_flag;
extern int only_macro_flag;
extern int debug_flag;
extern int no_line_flag;
extern int access_flag;
extern int set_flag;
extern int new_flag;
extern int free_flag;
extern int free_graph_flag;
extern int copy_flag;
extern int copy_graph_flag;
extern int equal_flag;
extern int equal_graph_flag;
extern int check_flag;
extern int check_graph_flag;
extern int print_flag;
extern int input_flag;
extern int output_flag;
extern int traverse_flag;
extern int reverse_traverse_flag;
extern int transform_flag;
extern int no_node_name_flag;

extern const char **extended_specification_directories;

extern char *prefix;

extern char *original_description_part_name;

extern FILE *output_interface_file;
extern char *output_interface_file_name;

extern FILE *output_implementation_file;
extern char *output_implementation_file_name;

extern IR_node_t current_description_part;

extern IR_node_t root_identifier;
extern IR_node_t root_node_type;

extern int number_of_node_types;
