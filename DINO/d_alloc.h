/* FILE NAME:   allocate.h

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package allocate; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License
   as published by the Free Software Foundation; either version 2, or
   (at your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Allocation package include file

   DESCRIPTION:
       This is header file contains macros and the ANSI C prototype
       definitions for the allocation package  (allocation with fixing
       error) and class for the allocation package (allocation with
       fixing error).

   SPECIAL CONSIDERATION:
         The function which processes allocation error should never
       return control back because after calling the function the
       function `abort' is always called in the debug regime.
         Defining macro `NDEBUG' (e.g. by option `-D' in C/C++ compiler
       command line) before the package macros usage disables to
       fix some internal errors and errors of usage of the package.

*/

#ifndef __ALLOCATE__
#define __ALLOCATE__

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#ifdef __GNUC__
#define MIR_VARR_NO_RETURN __attribute__ ((noreturn))
#else
#define MIR_VARR_NO_RETURN
#endif

static inline void MIR_VARR_NO_RETURN d_alloc_error (const char *message) {
#ifdef D_ALLOC_ERROR
  D_ALLOC_ERROR (message);
  assert (0);
#else
  fprintf (stderr, "%s\n", message);
#endif
  exit (1);
}

/* This macro is analogous to ANSI C library function `malloc'.  But
   the macro has another way of passing input and output data and has
   automatic reaction on the situation `no memory'.  The macro has not
   side effects. */
#define MALLOC(ptr, size)                  \
  do {                                     \
    void *_memory = malloc (size);         \
    if (_memory == NULL) {                 \
      d_alloc_error ("*** no memory ***"); \
      assert (0 /* FALSE */);              \
    }                                      \
    (ptr) = _memory;                       \
  } while (0)

/* This macro is analogous to ANSI C library function `calloc'.  But
   the macro has another way of passing input and output data and has
   automatic reaction on the situation `no memory'.  The macro has not
   side effects. */
#define CALLOC(ptr, nel, size)              \
  do {                                      \
    void *_memory = calloc ((nel), (size)); \
    if (_memory == NULL) {                  \
      d_alloc_error ("*** no memory ***");  \
      assert (0 /* FALSE */);               \
    }                                       \
    (ptr) = _memory;                        \
  } while (0)

/* This macro is analogous to ANSI C library function `free'.  But the
   macro can accept NULL pointer value.  In this case the macro does
   nothing.  The macro has not side effects. */
#define FREE(ptr)                        \
  do {                                   \
    void *_memory = (void *) (ptr);      \
    if (_memory != NULL) free (_memory); \
  } while (0)

/* This macro is analogous to ANSI C library function `realloc'.  But
   the macro has another way of passing input and output data and has
   automatic reaction on the situation `no memory'.  The macro has not
   side effects. */
#define REALLOC(new, old, size)              \
  do {                                       \
    void *_memory = realloc ((old), (size)); \
    if (_memory == NULL) {                   \
      d_alloc_error ("*** no memory ***");   \
      assert (0 /* FALSE */);                \
    }                                        \
    (new) = _memory;                         \
  } while (0)

#endif /* #ifndef __ALLOCATE__ */
