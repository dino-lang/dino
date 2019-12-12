/* FILE NAME:   position.h

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package for work with file positions; you can
   redistribute it and/or modify it under the terms of the GNU Library
   General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later
   version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Include file of package for work with positions of files

   DESCRIPTION: This header file contains ANSI C prototype definitions
                of the package functions and definitions of external
                variables of the package and C++ class of positions
                and definitions of external variables of the package.

*/


#ifndef __POSITIONS__
#define __POSITIONS__

/* This type describes a file position.  The value of this type contains
   information about all sequence of positions of include-clauses caused
   given file compilation. */

struct _position_struct
{
  /* Name of file to which given position belongs */
  const char *file_name;
  /* Source line corresponding to given position */
  unsigned int line_number;
  /* Source line column corresponding to given position */
  unsigned int column_number;
  /* Pointer to another position structure representing position
     of include-clause which caused immediately given file compilation */
  const struct _position_struct *path;
};

typedef struct _position_struct position_t;

extern const position_t no_position;

extern position_t current_position;

extern void initiate_positions (void);

extern void finish_positions (void);

extern int position_file_inclusion_level (position_t position);

extern void start_file_position (const char *file_name);

extern void finish_file_position (void);

extern int compare_positions (position_t position_1, position_t position_2);

#endif /* #ifndef __POSITIONS__ */
