/*
   FILE NAME:   common.c

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of the tool OKA.

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

   TITLE:       Common variables for all OKA (pipeline hazards description
                translator)

   DESCRIPTION: This file contains declarations of external variables
                common for all OKA (pipeline hazards description translator).

*/

#include "ird.h"
#include "common.h"

/* Output C++ code instead of C code (`-c++'). */

int cpp_flag;

/* Output of debugging information during execution of generated code
   (`-debug'). */

int debug_flag;

/* Output enumeration instead of macro definitions for instruction
   codes (`-enum'). */

int enum_flag;

/* Generation of macros defining identifiers of instructions
   (`-export') in the interface file (instead of implementation
   file). */

int export_flag;

/* Do not make minimization of DFA (`-no-minimization'). */

int no_minimization_flag;

/* Value of this variable is number of automatons being generated.
   The actual number of automatons may be less this value if there is
   not sufficient number of units.  This value is defined by argument
   of option `-split ' or by constructions `%automaton' if the value
   is zero (it is default value of the argument). */

int split_argument;

/* Value of this variable is prefix of names of generated pipeline
   hazards description objects.  This value is defined by argument of
   option `-p...' or by standard prefix. */

char *prefix;

/* Flag of output time statistics (`-v'). */

int time_flag;

/* Flag of creation of description file which contains description of
   result automaton and statistics information (`-v'). */

int v_flag;

/* Value of this variable is name of description.  The value is
   defined by file name (without suffix) given as operand of the OKA
   command line. */

char *description_name;

/* Interface file of pipeline hazards (PHR).  The value is NULL if the
   file is not created. */

FILE *output_interface_file;

/* PHR interface file name. */

char *output_interface_file_name;

/* Implementation file of PHR.  The value is NULL if the file is not
   created. */

FILE *output_implementation_file;

/* PHR implementation file name. */

char *output_implementation_file_name;

/* Description file of PHR.  The value is NULL if the file is not created. */

FILE *output_description_file;

/* PHR description file name. */

char *output_description_file_name;

/* Value of the following variable is node representing description
   being processed. */

IR_node_t description;
