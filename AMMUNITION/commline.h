/* FILE NAME:   commline.h

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package for work command line; you can redistribute
   it and/or modify it under the terms of the GNU Library General
   Public License as published by the Free Software Foundation; either
   version 2, or (at your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Include file of package for work with command line

   DESCRIPTION: This header file contains ANSI C prototype definitions
                of the package functions and definitions of external
                variables of the package and C++ class for work with
                command line.

*/


#ifndef __COMMAND_LINE__
#define __COMMAND_LINE__

extern int argument_count;
extern char **argument_vector;

extern int start_command_line_processing (int argc, char **argv,
                                          const char *description);
extern void output_command_line_description (void);
extern int next_operand (int flag_of_first);
extern int number_of_operands (void);
extern int next_option (int flag_of_first);
extern char *option_characteristics (int argument_number,
                                     int *option_has_argument);
extern int last_option_place (const char *option_name);
extern char *option_argument (const char *option_name);

#endif /* #ifndef __COMMAND_LINE__ */
