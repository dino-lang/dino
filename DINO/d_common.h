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
extern position_t *source_position_ptr;
extern unsigned int heap_chunk_size;
extern int statistics_flag;
extern int profile_flag;
extern double start_time;
extern int max_block_level;
extern void dino_finish (int code);

#ifdef WIN32
#define NO_PROFILE
#endif

#define SET_SOURCE_POSITION(ref)     (source_position = IR_pos (ref))

#define SET_SOURCE_POSITION_PTR(ref)					    \
  (source_position_ptr = (position_t *) ((char *) (ref)			    \
					 + _IR_D_pos [IR_NODE_MODE (ref)]))

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

#ifndef INLINE
#ifdef __GNUC__
#define INLINE 1
#else
#define INLINE 0
#endif
#endif
