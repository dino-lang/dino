/*
   FILE NAME:   common.c

   Copyright (C) 1997-2002 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of the tool NONA.

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

   TITLE:       Common variables for all NONA (code selector 
                description translator)

   DESCRIPTION: This file contains declarations of external variables
                common for all NONA (code selector description translator).  */

#include "ird.h"
#include "common.h"

/* Flag of generation of C++ code (`-c++'). */

int cpp_flag;

/* Flag of output of verbose warning information (`-v'). */

int v_flag;

/* Output of debugging information during execution of generated code
   (`-debug'). */

int debug_flag;

/* Generation of macros defining identifiers of terminals (`-export'). */

int export_flag;

/* Value of this variable is prefix of names of generated machine
   description objects.  This value is defined by argument of option
   `-p...' or by standard prefix. */

char *prefix;

/* Value of this variable is name of description.  The value is
   defined by file name (without suffix) given as operand of the NONA
   command line. */

char *description_name;

/* Interface file of code selector.  The value is NULL if the file is
   not created. */

FILE *output_interface_file;

/* Code selector interface file name. */

char *output_interface_file_name;

/* Implementation file of code selector.  The value is NULL if the
   file is not created. */

FILE *output_implementation_file;

/* Code selector implementation file name. */

char *output_implementation_file_name;

/* Value of the following variable is node representing description
   being processed. */

IR_node_t description;
