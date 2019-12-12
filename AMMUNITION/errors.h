/* FILE NAME:   errors.h

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package for output of compiler messages; you can
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

   TITLE:       Include file of package for output of compiler messages

   DESCRIPTION: This header file contains ANSI C prototype definitions
                of the package functions and definitions of external
                variables of the package and C++ class for output of
                compiler messages.

*/

#ifndef __ERRORS__
#define __ERRORS__

#include "vlobject.h"
#include "objstack.h"
#include "position.h"

extern unsigned int number_of_errors;
extern unsigned int maximum_number_of_errors;
extern const unsigned int default_maximum_number_of_errors;

extern unsigned int number_of_warnings;

extern void (*fatal_error_function) (void);
extern void default_fatal_error_function (void);

extern void (*output_error_function) (int appended_message_flag,
                                      position_t position,
                                      const char *message);
extern void default_output_error_function (int appended_message_flag,
                                           position_t position,
                                           const char *message);

extern void initiate_errors (int immediate_output_flag);
extern void finish_errors (void);
extern void output_errors (void);
extern void error (int fatal_error_flag, position_t position,
                   const char *format, ...);
extern void warning (position_t position, const char *format, ...);
extern void append_message (position_t position, const char *format, ...);
extern void system_error (int fatal_error_flag, position_t position,
                          const char *format, ...);

#endif /* #ifndef __ERRORS__ */
