/*
   Copyright (C) 1997-2016 Vladimir Makarov.

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

#ifndef STANDARD_DINO_INCLUDE_DIRECTORY
#define STANDARD_DINO_INCLUDE_DIRECTORY "/usr/local/lib"
#endif

#define STANDARD_DINO_GEN_FILE_NAME "d_gen.i"

#define STANDARD_DINO_EXTERN_FILE_NAME "d_api.h"

#define DINO_INCLUDE_PATH_NAME_VARIABLE "DINO_PATH"

#ifndef STANDARD_DINO_LIB_DIRECTORY
#define STANDARD_DINO_LIB_DIRECTORY "/usr/local/lib"
#endif

/* Libraries are prefixed by $DINO_HOME (if any) or
   STANDARD_DINO_LIB_DIRECTORY.  Separator is :. */
#ifndef STANDARD_DINO_EXTERN_LIBS
#define STANDARD_DINO_EXTERN_LIBS "mpi.so:ieee.so:ipcerr.so:socket.so"
#endif

#define DINO_HOME_NAME_VARIABLE "DINO_HOME"

#define DINO_LIB_NAME_VARIABLE "DINO_LIB"

#define DINO_EXTERN_LIBS_NAME_VARIABLE "DINO_EXTERN_LIBS"

#define DINO_ENCODING "DINO_ENCODING"

extern int dino_main (int argc, char *argv[], char *envp[]);
