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

#include "d_run.h"

#if !defined(HAVE_DLOPEN) || defined(NO_EXTERN_SHLIB)

/* See file mpi.c */
extern void *mpi_address (const char *name);
extern void *ieee_address (const char *name);
extern void *ipcerr_address (const char *name);
extern void *socket_address (const char *name);

void *
get_library_search_function (const char *name)
{
  if (strcmp (name, "mpi") == 0)
    return mpi_address;
  if (strcmp (name, "ieee") == 0)
    return ieee_address;
  if (strcmp (name, "ipcerr") == 0)
    return ipcerr_address;
  if (strcmp (name, "socket") == 0)
    return socket_address;
  /* You can add here new library (see mpi_address in mpi.c. */
  return NULL;
}

#endif

#ifdef AIX_DLOPEN
#include "d_aixdl.c"
#endif

int
main (int argc, char *argv[], char *envp[])
{
#if defined(HAVE_DLOPEN) && !defined(NO_DINO_SHLIB)
  char *libraries[3];
  int nlibs;
  char buffer [500];
  char *stdlibs;
  const char *message;
  const char *home;
  int (*dino_func) (int, char **, char **);
  void *handle;
  int i;
  int code;
  FILE *f;

  nlibs = 0;
  libraries [nlibs] = getenv (DINO_LIB_NAME_VARIABLE);
  if (libraries [nlibs] != NULL)
    nlibs++;
  stdlibs = buffer;
  strcpy (stdlibs, "./");
  strcat (stdlibs, STANDARD_DINO_LIB_NAME);
  libraries [nlibs++] = stdlibs;
  stdlibs += strlen (stdlibs) + 1;
  home = getenv (DINO_HOME_NAME_VARIABLE);
  if (home == NULL)
    strcpy (stdlibs, STANDARD_DINO_LIB_DIRECTORY);
  else
    {
      strcpy (stdlibs, home);
      if (strlen (stdlibs) == 0 || stdlibs [strlen (stdlibs) - 1] != '/')
        strcat (stdlibs, "/");
      strcat (stdlibs, "lib");
    }
  if (strlen (stdlibs) == 0 || stdlibs [strlen (stdlibs) - 1] != '/')
    strcat (stdlibs, "/");
  strcat (stdlibs, STANDARD_DINO_LIB_NAME);
  libraries [nlibs++] = stdlibs;
  (void) dlopen (NULL, RTLD_NOW);
  for (i = 0; i < nlibs && libraries[i] != NULL; i++)
    {
      /* Dlopen on some system does not like unexisting libraries. */
      f = fopen (libraries[i], "r");
      if (f == NULL)
	continue;
      fclose (f);
      handle = dlopen (libraries[i], RTLD_NOW | RTLD_GLOBAL);
      if (handle == NULL)
	continue;
      dino_func = dlsym (handle, "dino_main");
      if ((message = dlerror ()) != NULL)
	{
	  fprintf (stderr, "fatal error - %s\n", message);
	  exit (1);
	}
      code = (*dino_func) (argc, argv, envp);
#if 0
      dlclose (handle);
      if ((message = dlerror ()) != NULL)
	{
	  fprintf (stderr, "fatal error - %s\n", message);
	  exit (1);
	}
#endif
      exit (code);
    }
  fprintf (stderr, "fatal error - can't find dino shared library %s\n",
	   STANDARD_DINO_LIB_NAME);
  exit (1);
#else
  exit (dino_main (argc, argv, envp));
#endif
}
