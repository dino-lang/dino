/*
   FILE NAME:   position.cpp

   TITLE:       Package for work with positions of files

   DESCRIPTION:
         The package implements abstract data which reflects current
       file positions.  The abstract data is represented by classes
       (see file `position.h').
         The strategy of the package usage can be follows.  Function
       `initiate_positions' is called by the first.  After that
       function `create_file_position' is called when a file is opened
       for compilation as source or included file.  Members
       `line_number' and `column_number' of variable
       `current' are modified correspondingly during given
       file compilation.  The value of `current' can be
       stored into internal representation for usage for output
       diagnostics on following passes.  Function
       `finish_file_position' is called when a processed file is
       closed.  Function `finish_positions' may be called after all
       processing a source file.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C++ compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of the package.

*/

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <string.h>
#include "vlobject.h"
#include "objstack.h"
#include "position.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

/* The function returns number of level of the position file
   inclusion. The level numbers are started with zero for positions
   corresponding non-included files and for positions which does not
   correspond to concrete file.  */

int
position::file_inclusion_level (void)
{
  int level;
  position_t *current_position_ref;

  level = 0;
  for (current_position_ref = this;
       current_position_ref->file_name () != NULL;
       current_position_ref = current_position_ref->path ())
    level++;
  assert (current_position_ref->path () == NULL);
  if (level != 0)
    level--;
  return level;
}

/* Value of this variable represents file position which does not
   correspond to concrete file position. */

const position_t positions::no_position;

/* Value of this variable is current file position. */

position_t positions::current;

/* This object stack stores copies of processed file names. */

static os_t *copied_file_names;

/* This object stack stores copies of some values of
   variable `current'. */

static os_t *copied_positions;


/* This function initiates the package.  Value of variable
   `current' becames equal to `no_position'.  This function
   should be invoked only once before calls of any other functions of
   the package. */

void
positions::initiate (void)
{
  current = no_position;
  copied_file_names = new os;
  copied_positions = new os;
}
                                                                    
/* This function frees all memory allocated during package work.  Only
   call of function `initiate_positions' is possible immediately after
   this function call. */

void
positions::finish (void)
{
  delete copied_file_names;
  delete copied_positions;
}                                                                    

/* This function copys position (by dynamic memory allocation) in
   variable `current' and sets up new values of members of `current'.
   Values of members `file_name', `line_number', `column_number', and
   path became equal to copy of given file name, 1, 1, and pointer to
   the copied position.  Value of `file_name' must be not NULL
   pointer.  Values of `current' during different calls of the
   function must be different (e.g. different columns or lines),
   i.e. positions of different include-clause must be different.  */

void
positions::start_file (const char *file_name)
{
  assert (file_name != NULL);
  copied_positions->top_add_memory (&current,
                                    sizeof (current));
  copied_file_names->top_add_string (file_name);
  current._file_name = (char *) copied_file_names->top_begin ();
  copied_file_names->top_finish ();
  current.line_number = 1;
  current.column_number = 1;
  current._path = (class position *) copied_positions->top_begin ();
  copied_positions->top_finish ();
}
                                                                    
/* This function recovers previous value of variable
   `current', more exactly sets up the variable by position
   to which the variable member `path' refers.  */

void
positions::finish_file (void)
{
  if (current.path () != NULL)
    current = *current.path ();
}
                                                                    
/* This recursive function forms vector of pointers to positions to
   which given position refers directly or indirectly.  The last
   element of the vector is pointer to given position position.  */

static void
form_position_vector (vlo_t *vector, position_t *position_ptr)
{
  if (position_ptr->path () != NULL)
    form_position_vector (vector, position_ptr->path ());
  vector->add_memory (&position_ptr, sizeof (position_t *));
}

/* The following function compares two positions.  The order of
   positions is lexicographic.  Tthe function returns -1 (if the first
   position is less than the second), 0 (if the first position is
   equal to the second) or 1 (if the first position is greater than
   the second). */

int
positions::compare (position_t position_1, position_t position_2)
{
  int i, result;
  vlo_t *path_1, *path_2;
  position_t **position_ptr_vector_1, **position_ptr_vector_2;

  path_1 = new vlo (20);
  path_2 = new vlo (20);
  form_position_vector (path_1, &position_1);
  form_position_vector (path_2, &position_2);
  position_ptr_vector_1 = (position_t **) path_1->begin ();
  position_ptr_vector_2 = (position_t **) path_2->begin ();
  assert (*position_ptr_vector_1 != NULL && *position_ptr_vector_2 != NULL);
  for (i = 1;; i++)
    {
      if ((char *) (position_ptr_vector_1 + i) > (char *) path_1->end ())
        result = ((char *) (position_ptr_vector_2 + i)
                  > (char *) path_2->end ()
                  ? 0 : (-1));
      else if ((char *) (position_ptr_vector_2 + i)
               > (char *) path_2->end ())
        result = 1;
      else
        {
          assert (position_ptr_vector_1[i]->file_name () != NULL
                  && position_ptr_vector_2[i]->file_name () != NULL
                  && strcmp (position_ptr_vector_1[i]->file_name (),
                             position_ptr_vector_2[i]->file_name ()) == 0);
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
  delete path_1;
  delete path_2;
  return result;
}
