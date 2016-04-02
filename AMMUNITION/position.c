/* FILE NAME:   position.c

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

   TITLE:       Package for work with positions of files

   DESCRIPTION:
         The package implements abstract data which reflects current
       file positions.  The abstract data is represented by structure
       (see file `position.h').
         The strategy of the package usage can be follows.  Function
       `initiate_positions' is called by the first.  After that
       function `start_file_position' is called when a file is opened
       for compilation as source or included file.  Members
       `line_number' and `column_number' of variable
       `current_position' are modified correspondingly during given
       file compilation.  The value of `current_position' can be
       stored into internal representation for usage for output
       diagnostics on following passes.  Function
       `finish_file_position' is called when a processed file is
       closed.  Function `finish_positions' may be called after all
       processing a source file.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of the package.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */

#include <string.h>

#include "vlobject.h"
#include "objstack.h"
#include "position.h"


#include <assert.h>

/* Value of this variable represents file position which does not
   correspond to concrete file position. */

const position_t no_position = {NULL, 0, 0 , NULL};

/* Value of this variable is current file position. */

position_t current_position;

/* This object stack stores copies of processed file names. */

static os_t copied_file_names;

/* This object stack stores copies of some values of
   variable `current_position'. */

static os_t copied_positions;


/* This function initiates the package.  Value of variable
   `current_position' becames equal to `no_position'.  This function
   should be invoked only once before calls of any other functions of
   the package. */

void
initiate_positions (void)
{
  current_position = no_position;
  OS_CREATE (copied_file_names, 0);
  OS_CREATE (copied_positions, 0);
}
                                                                    
/* This function free all memory allocated during package work.  Only
   call of function `initiate_positions' is possible immediately after
   this function call. */

void
finish_positions (void)
{
  OS_DELETE (copied_file_names);
  OS_DELETE (copied_positions);
}                                                                    

/* This function copys position structure (by dynamic memory
   allocation) in variable `current_position' and sets up new values
   of members of `current_position'.  Values of members `file_name',
   `line_number', `column_number', and path became equal to copy of
   given file name, 1, 1, and pointer to the copied structure.  Value
   of `file_name' must be not NULL pointer.  Values of
   `current_position' during different calls of the function must be
   different (e.g. different columns or lines), i.e. positions of
   different include-clause must be different.  */

void
start_file_position (const char *file_name)
{
  assert (file_name != NULL);
  OS_TOP_ADD_MEMORY (copied_positions, &current_position,
                     sizeof (current_position));
  OS_TOP_ADD_STRING (copied_file_names, file_name);
  current_position.file_name = OS_TOP_BEGIN (copied_file_names);
  OS_TOP_FINISH (copied_file_names);
  current_position.line_number = 1;
  current_position.column_number = 1;
  current_position.path = OS_TOP_BEGIN (copied_positions);
  OS_TOP_FINISH (copied_positions);
}
                                                                    
/* This function recovers previous value of variable
   `current_position', more exactly sets up the variable by structure
   to which the variable member `path' refers.  */

void
finish_file_position (void)
{
  if (current_position.path != NULL)
    current_position = *current_position.path;
}
                                                                    
/* The function returns number of level of position file inclusion.
   The level numbers are started with zero for positions corresponding
   non-included files and for positions which does not correspond to
   concrete file.  */

int
position_file_inclusion_level (position_t position)
{
  int level;
  const position_t *current_position_ref;

  level = 0;
  for (current_position_ref = &position;
       current_position_ref->file_name != NULL;
       current_position_ref = current_position_ref->path)
    level++;
  assert (current_position_ref->path == NULL);
  if (level != 0)
    level--;
  return level;
}

/* This recursive function forms vector of pointers to positions to
   which given position refers directly or indirectly.  The last
   element of the vector is pointer to given position structure.  */

static void
form_position_vector (vlo_t *vector, const position_t *position_ptr)
{
  if (position_ptr->path != NULL)
    form_position_vector (vector, position_ptr->path);
  VLO_ADD_MEMORY (*vector, &position_ptr, sizeof (position_t *));
}

/* The following function compares two positions.  The order of
   positions is lexicographic.  The function returns -1 (if the first
   position is less than the second), 0 (if the first position is
   equal to the second) or 1 (if the first position is greater than
   the second). */

int
compare_positions (position_t position_1, position_t position_2)
{
  int i, result;
  vlo_t path_1, path_2;
  position_t **position_ptr_vector_1, **position_ptr_vector_2;

  VLO_CREATE (path_1, 20);
  VLO_CREATE (path_2, 20);
  form_position_vector (&path_1, &position_1);
  form_position_vector (&path_2, &position_2);
  position_ptr_vector_1 = VLO_BEGIN (path_1);
  position_ptr_vector_2 = VLO_BEGIN (path_2);
  assert (*position_ptr_vector_1 != NULL && *position_ptr_vector_2 != NULL);
  for (i = 1;; i++)
    {
      if ((char *) (position_ptr_vector_1 + i) > (char *) VLO_END (path_1))
        result = ((char *) (position_ptr_vector_2 + i)
                  > (char *) VLO_END (path_2)
                  ? 0 : (-1));
      else if ((char *) (position_ptr_vector_2 + i)
               > (char *) VLO_END (path_2))
        result = 1;
      else
        {
          assert (position_ptr_vector_1[i]->file_name != NULL
                  && position_ptr_vector_2[i]->file_name != NULL
                  && strcmp (position_ptr_vector_1[i]->file_name,
                             position_ptr_vector_2[i]->file_name) == 0);
          if (position_ptr_vector_1[i]->line_number
              == position_ptr_vector_2[i]->line_number)
            {
              if (position_ptr_vector_1[i]->column_number
                  == position_ptr_vector_2[i]->column_number)
                continue;
              else if (position_ptr_vector_1[i]->column_number
                       > position_ptr_vector_2[i]->column_number)
                result = 1;
              else
                result = (-1);
            }
          else if (position_ptr_vector_1[i]->line_number
                   > position_ptr_vector_2[i]->line_number)
            result = 1;
          else
            result = (-1);
        }
      break;
    }
  VLO_DELETE (path_1);
  VLO_DELETE (path_2);
  return result;
}
