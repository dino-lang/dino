/*
   FILE NAME:   position.h

   TITLE:       Include file of package for work with positions of files

   DESCRIPTION: This header file contains ANSI C prototype definitions
                of the package functions and definitions of external
                variables of the package and C++ class of positions
                and definitions of external variables of the package.

*/


#ifndef __POSITIONS__
#define __POSITIONS__

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#endif

#ifndef __cplusplus


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
  struct _position_struct *path;
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



#else /* #ifndef __cplusplus */



#include "allocate.h"

/* This class describes a file position.  The value of this type contains
   information about all sequence of positions of include-clauses caused
   given file compilation. */

class position
{
  /* Name of file to which given position belongs */
  const char *_file_name;
  /* Pointer to another position structure representing position
     of include-clause which caused immediately given file compilation */
  class position *_path;

public:

  /* Source line corresponding to given position */
  unsigned int line_number;
  /* Source line column corresponding to given position */
  unsigned int column_number;

  inline const char *file_name (void) {return _file_name;}
         
  inline class position *path (void) {return _path;}

  /* The following two functions allocate memory for the position. */

  inline void *operator new (size_t size)
    {
      return allocate::malloc (size);
    }

  inline void *operator new[] (size_t size)
    {
      return allocate::malloc (size);
    }

  /* The following two functions free memory for the position. */

  inline void operator delete (void *mem)
    {
      allocate:: free (mem);
    }

  inline void operator delete[] (void *mem)
    {
      allocate:: free (mem);
    }

  inline position (void)
    {
      this->_file_name = NULL;
      this->line_number = 0;
      this->column_number = 0;
      this->_path = NULL;
    }

  int file_inclusion_level (void);

  friend class positions;

};


typedef class position position_t;
  
class positions
{

public:

  static const position_t no_position;

  static position_t current;

  static void initiate (void);

  static void finish (void);

  static void start_file (const char *file_name);

  static void finish_file (void);

  static int compare (position_t position_1, position_t position_2);
};

#endif /* #ifndef __cplusplus */

#endif /* #ifndef __POSITIONS__ */
