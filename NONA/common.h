/*
   FILE NAME:   common.h

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

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

   TITLE:       Include file of common things for all NONA (code selector
                description translator)

   DESCRIPTION: This header file contains definitions of macros
                and external definitions of variables common for
                all NONA (code selector description translator).

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

/* Suffix which any code selector description file name must have.  If
   this macro value is changed then documentation of nona must be
   changed. */

#define STANDARD_INPUT_FILE_SUFFIX ".nona"

/* This macro value is used for correct calculation of current position in
   file when TAB character is processed. */

#define TAB_STOP 8

extern int cpp_flag;
extern int v_flag;
extern int debug_flag;
extern int export_flag;

extern char *prefix;

extern char *description_name;

extern FILE *output_interface_file;
extern char *output_interface_file_name;

extern FILE *output_implementation_file;
extern char *output_implementation_file_name;

extern IR_node_t description;
