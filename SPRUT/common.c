/*
   FILE NAME:   common.c

   Copyright (C) 1997-2005 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

   TITLE:       Common variables for all SPRUT (internal
                representation description translator)

   DESCRIPTION: This file contains declarations of external variables
                common for SPRUT (all internal representation translator).

*/

#include "ird.h"
#include "common.h"

/* Begin of declarations of all option flags (except for `-all') of SPRUT. */

/* Flag of output of verbose warning information (`-v'). */

int v_flag;

/* Flag of generation of C++ code instead of C code (`-c++'). */

int cpp_flag;

/* Flag of output of statistics information (`-statistics'). */

int statistics_flag;

/* Flag of flat implementation of work with node type fields (`-flat'). */

int flat_flag;

/* Flag of flat implementation of node type structures (`-flat-structure'). */

int flat_structure_flag;

/* Flag of generation of some big arrays as unpacked for faster work
   (`-fast'). */

int fast_flag;

/* Flag of generation of node size as unsigned long (`-long-node-size'). */

int long_node_size_flag;

/* Flag of generation of node size as unsigned short (`-short-node-size'). */

int short_node_size_flag;

/* Generation of macros and access functions (`-macro').  Only flag
   `-macro' or `only-macro' can be in SPRUT command line. */

int macro_flag;

/* Generation of macros instead of access functions (`-only-macro').
   Only flag `-macro' or `only-macro' can be in SPRUT command line. */

int only_macro_flag;

/* Generation of check code for modification functions (`-debug'). */

int debug_flag;

/* No generation of files containing numbered line directives (`-no-line'). */

int no_line_flag;

/* Access functions (macros) generation (`-access'). */

int access_flag;

/* Field modification functions generation (`-set'). */

int set_flag;

/* Generation of node type specific creation functions (`-new'). */

int new_flag;

/* Generation of function for freeing node (`-free'). */

int free_flag;

/* Generation of functions for freeing node and graph (`-free-graph'). */

int free_graph_flag;

/* Generation of function for copying node (`-copy'). */

int copy_flag;

/* Generation of functions for copying node and graph (`-copy-graph'). */

int copy_graph_flag;

/* Node comparison function generation (`-equal'). */

int equal_flag;

/* Node and graph comparison functions generation (`-equal-graph'). */

int equal_graph_flag;

/* Node check function generation (`-check').  Also print function is
   generated under this flag because the check functions use print
   function. */

int check_flag;

/* Node and graph check functions generation (`-check-graph').  Also
   print function is generated under this flag because the check
   functions use print function.*/

int check_graph_flag;

/* Node print function generation (`-print'). */

int print_flag;

/* Node input function generation (`-input'). */

int input_flag;

/* Node output function generation (`-output'). */

int output_flag;

/* Generation of function for traversing graphs (`-traverse'). */

int traverse_flag;

/* Generation of function for reverse traversing graphs
   (`-reverse-traverse'). */

int reverse_traverse_flag;

/* Generation of function for transforming graphs (`-transform'). */

int transform_flag;

/* Prohibiting generation of array containing names of nodes
   (`-no-node-name'). */

int no_node_name_flag;

/* Null ended vector of directories in which are given in options `-E'
   and in which extended specifications file are searched for. */

const char **extended_specification_directories;

/* Value of this variable is prefix of names of standard procedural
   interface (SPI) objects.  This value is defined by argument of option
   `-p...' or by standard prefix. */

char *prefix;

/* Value of this variable is name of original description part.  The
   value is defined by file name (without suffix) given as operand of the
   SPRUT command line. */

char *original_description_part_name;

/* Interface file of SPI.  The value is NULL if the file is not created. */

FILE *output_interface_file;

/* SPI interface file name. */

char *output_interface_file_name;

/* Implementation file of SPI.  The value is NULL if the file is not
   created. */

FILE *output_implementation_file;

/* SPI implementation file name. */

char *output_implementation_file_name;

/* Value of the following variable is node representing current
   description part during parsing.  After parsing the value is
   description part of the uppest level, i.e. the part whose file is
   given on SPRUT command line.  After semantic analysis the value is
   equivalent one level description (all description parts are changed by
   this one level description). */

IR_node_t current_description_part;

/* Node representing identifier `%root'.  There are a few such node
   (one for each occurrence identifier `%root' in given description). */

IR_node_t root_identifier;

/* Node representing node type `%root'.  There is the only such node. */

IR_node_t root_node_type;

/* Value of the following variable is number of already declared node
   types.  As consequence the value is number of all node types after
   semantic analysis. */

int number_of_node_types;
