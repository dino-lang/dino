/*
   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of interpreter of DINO.

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

#include "d_dino.h"

#include "allocate.h"
#include "vlobject.h"
#include "position.h"
#include "hashtab.h"
#include "ticker.h"
#include "d_errors.h"

#define FALSE 0
#define TRUE 1

/* Macros return value of digit CH.  Macros is undefined for non digit. */

#define VALUE_OF_DIGIT(ch) ((ch) - '0')
#define STANDARD_INPUT_FILE_SUFFIX ".d"

extern const char **include_path_directories;
extern const char **libraries;
extern char *command_line_program;
extern int program_arguments_number;
extern char **program_arguments;
extern char **program_environment;
extern position_t source_position;
extern unsigned int heap_chunk_size;
extern int statistics_flag;
extern int profile_flag;
extern double start_time;
extern int max_block_level;
extern char *get_ch_repr (int ch);
extern void dino_finish (int code);

#define SET_SOURCE_POSITION(ref)     (source_position = IR_pos (ref))

#define ENVIRONMENT_PSEUDO_FILE_NAME "<environment>"

#ifndef HAVE_MEMCPY
extern void *memcpy (void *to, const void *from, size_t size);
#endif /* #ifndef HAVE_MEMCPY */

#ifndef HAVE_MEMSET
extern void *memset (void *to, int value, size_t size);
#endif /* #ifndef HAVE_MEMSET */

#ifndef HAVE_MEMCMP
extern int memcmp (const void *mem1, const void *mem2, size_t size);
#endif /* #ifndef HAVE_MEMCMP */

#ifndef HAVE_MEMMOVE
extern void *memmove (void *s1, const void *s2, size_t n);
#endif /* #ifndef HAVE_MEMMOVE */

#if !defined (NO_PROFILE) && !defined (HAVE_SETITIMER)
#define HAVE_SETITIMER 0
#endif

#ifndef INLINE
#ifdef __GNUC__
#define INLINE 1
#else
#define INLINE 0
#endif
#endif
