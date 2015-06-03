/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

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

#ifdef HAVE_CONFIG_H
#include "d_config.h"
#else /* In this case we are oriented to ANSI C and dfcn.h */
#ifndef HAVE_MEMSET
#define HAVE_MEMSET
#endif
#ifndef HAVE_MEMCPY
#define HAVE_MEMCPY
#endif
#ifndef HAVE_MEMMOVE
#define HAVE_MEMMOVE
#endif
#ifndef HAVE_MEMCMP
#define HAVE_MEMCMP
#endif
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#ifndef HAVE_FLOAT_H
#define HAVE_FLOAT_H
#endif
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#ifndef HAVE_TIME_H
#define HAVE_TIME_H
#endif
#ifndef HAVE_ERRNO_H
#define HAVE_ERRNO_H
#endif
#ifndef HAVE_DLFCN_H
#define HAVE_DLFCN_H
#endif
#ifdef HAVE_SYS_TIME_H
#undef HAVE_SYS_TIME_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <stdlib.h>
#include <signal.h>

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif
#ifndef SCHAR_MAX
#define SCHAR_MAX 127
#endif
#ifndef SCHAR_MIN
#define SCHAR_MIN (-128)
#endif
#ifndef UINT_MAX
#define UINT_MAX (INT_MAX * 2U + 1)
#endif
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif  
#ifndef INT_MIN
#define INT_MIN (-INT_MAX-1)
#endif
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#else
#define FLT_MAX  3.40282347e+38F         /* IEEE float */
#define DBL_MAX  1.7976931348623157e+308 /* IEEE double */
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

#define d_assert assert

#define d_unreachable() abort ()

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifndef AIX_DLOPEN
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0x0	/* ignore allowing symbols to be global. */
#endif
#else
#ifdef HAVE_DLOPEN
/* Used mode flags for the dlopen routine. */
#define RTLD_LAZY	1	/* lazy function call binding */
#define RTLD_NOW	2	/* immediate function call binding */
#define RTLD_GLOBAL	0x100	/* allow symbols to be global */
void *dlopen (const char *filename, int flag);
const char *dlerror(void);
void *dlsym(void *handle, char *symbol);
int dlclose (void *handle);
#endif
#endif
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#ifndef ITIMER_VIRTUAL
#define ITIMER_VIRTUAL 1
#endif
#if defined (HAVE_SETITIMER) && !defined(SIGVTALRM)
#define SIGVTALRM 26 
#endif
#endif

#include <ctype.h>
#include <string.h>

#ifndef STANDARD_DINO_INCLUDE_DIRECTORY
#define STANDARD_DINO_INCLUDE_DIRECTORY "/usr/local/lib"
#endif

#define STANDARD_DINO_GEN_FILE_NAME "d_gen.i"

#define DINO_INCLUDE_PATH_NAME_VARIABLE "DINO_PATH"

#ifndef STANDARD_DINO_LIB_DIRECTORY
#define STANDARD_DINO_LIB_DIRECTORY "/usr/local/lib"
#endif

#ifndef STANDARD_DINO_LIB_NAME
#define STANDARD_DINO_LIB_NAME "libdino.so"
#endif

/* Libraries are prefixed by $DINO_HOME (if any) or
   STANDARD_DINO_LIB_DIRECTORY.  Separator is :. */
#ifndef STANDARD_DINO_EXTERN_LIBS
#define STANDARD_DINO_EXTERN_LIBS "mpi.so:ieee.so:ipcerr.so:socket.so"
#endif

#define DINO_HOME_NAME_VARIABLE "DINO_HOME"

#define DINO_LIB_NAME_VARIABLE "DINO_LIB"

#define DINO_EXTERN_LIBS_NAME_VARIABLE "DINO_EXTERN_LIBS"

extern int dino_main (int argc, char *argv[], char *envp[]);
