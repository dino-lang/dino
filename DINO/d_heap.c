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

#include "d_run.h"

/* The following is true if we are executing sync block.  */
int sync_flag;

/* Current program counter of command being executed.  This value is
   used to execution of commands for interpreter (see comments for
   typedef pc_t). */
d_restrict pc_t cpc;

/* Pointer to the stack frame of the block in which cpc is being
   placed. */
ER_node_t cstack;

/* Pointer to the stack frame of the uppest block in which environment
   variables are placed. */
ER_node_t uppest_stack;

/* Start of the vars of the uppest stack and cstack.  */
d_restrict ER_node_t tvars;
d_restrict ER_node_t cvars;

/* Pointers to the var on the top and below the top of stack cstack.
   Don't use the variables during GC. */
ER_node_t ctop;

/* Pointers to a heap objects.  Garbage collector will change it
   correspondingly too.  If the pointer refers for vector, it can not
   be packed during GC or its size can not be changed. */
vlo_t heap_temp_refs;

/* Temporary variable length objects. */
vlo_t temp_vlobj;
vlo_t temp_vlobj2;

vlo_t dl_handle_vlo;

/* Current (active) thread. */
ER_node_t cthread;

/* Variable used to assign unique number to the contexts. */
int context_number;

/* Variable used to assign unique number to the threads. */
int thread_number;

/* The following variable value is the first thread blocked by a wait
   stmt and there is no thread started after this.  This variable is
   used for searching deadlocks. */
ER_node_t first_thread_not_started;



size_t type_size_table [ER_NM__error + 1];
size_t val_displ_table [ER_NM__error + 1];

void
initiate_int_tables (void)
{
  int i;
  ER_node_t v = NULL;

  for (i = 0; i <= ER_NM__error; i++)
    type_size_table [i] = 0;
  type_size_table [ER_NM_undef] = 0;
  type_size_table [ER_NM_nil] = 0;
  type_size_table [ER_NM_hide] = sizeof (hide_t);
  type_size_table [ER_NM_char] = sizeof (ucode_t);
  type_size_table [ER_NM_byte] = sizeof (char);
  type_size_table [ER_NM_int] = sizeof (rint_t);
  type_size_table [ER_NM_long] = sizeof (ER_node_t);
  type_size_table [ER_NM_float] = sizeof (rfloat_t);
  type_size_table [ER_NM_type] = sizeof (ER_node_mode_t);
  type_size_table [ER_NM_tab]
    = type_size_table [ER_NM_thread]
    = type_size_table [ER_NM_stack] = sizeof (ER_node_t);
  type_size_table [ER_NM_vect] = sizeof (ER_node_t);
  type_size_table [ER_NM_code] = sizeof (_ER_code);
  type_size_table [ER_NM_efun] = sizeof (BC_node_t);

  for (i = 0; i <= ER_NM__error; i++)
    val_displ_table [i] = 0;
  val_displ_table [ER_NM_undef] = 0;
  val_displ_table [ER_NM_nil] = 0;
  val_displ_table [ER_NM_hide] = (char *) &((_ER_hide *) v)->hide - (char *) v;
  val_displ_table [ER_NM_hideblock]
    = (char *) &((_ER_hideblock *) v)->hideblock - (char *) v;
  val_displ_table [ER_NM_char] = (char *) &((_ER_char *) v)->ch - (char *) v;
  val_displ_table [ER_NM_int] = (char *) &((_ER_int *) v)->i - (char *) v;
  val_displ_table [ER_NM_long] = (char *) &((_ER_long *) v)->l - (char *) v;
  val_displ_table [ER_NM_float] = (char *) &((_ER_float *) v)->f - (char *) v;
  val_displ_table [ER_NM_type] = (char *) &((_ER_type *) v)->type - (char *) v;
  val_displ_table [ER_NM_vect] = (char *) &((_ER_vect *) v)->vect - (char *) v;
  val_displ_table [ER_NM_tab] = (char *) &((_ER_tab *) v)->tab - (char *) v;
  val_displ_table [ER_NM_thread]
    = (char *) &((_ER_thread *) v)->thread - (char *) v;
  val_displ_table [ER_NM_code] = 0;
  val_displ_table [ER_NM_efun]
    = (char *) &((_ER_efun *) v)->efdecl - (char *) v;
  val_displ_table [ER_NM_stack]
    = (char *) &((_ER_stack *) v)->stack - (char *) v;
}



/* See definitions of the corresponding nodes. */
static size_t do_always_inline
val_displ (ER_node_t var)
{
  d_assert (ER_NODE_MODE (var) != ER_NM_byte);
  return val_displ_table [ER_NODE_MODE (var)];
}



static int do_inline
in_heap_temp_refs (ER_node_t obj)
{
  int i;

  for (i = 0; i < TEMP_REFS_LENGTH (); i++)
    if (GET_TEMP_REF (i) == obj)
      return TRUE;
  return FALSE;
}



#if !defined(NDEBUG)

size_t
_alloc_size (size_t s)
{
  s = (s + MAX_ALIGNMENT - 1) / MAX_ALIGNMENT * MAX_ALIGNMENT;
  return s;
}

char *
_hideblock_start (ER_node_t hideblock)
{
  char *res;

  d_assert (ER_NODE_MODE (hideblock) == ER_NM_heap_hideblock);
  res = (char *) hideblock + ALLOC_SIZE (sizeof (_ER_heap_hideblock));
  return res;
}

char *
_pack_els (ER_node_t vect)
{
  char *res;
  
  d_assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect);
  res = ((char *) vect + ALLOC_SIZE (sizeof (_ER_heap_pack_vect))
	 + ER_disp (vect));
  return res;
}

ER_node_t
_unpack_els (ER_node_t vect)
{
  ER_node_t res;
  
  d_assert (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect);
  res = (ER_node_t) ((char *) vect
		     + ALLOC_SIZE (sizeof (_ER_heap_unpack_vect))
		     + ER_disp (vect));
  return res;
}

ER_node_t
_tab_els (ER_node_t tab)
{
  ER_node_t res;

  d_assert (ER_NODE_MODE (tab) == ER_NM_heap_tab);
  res = (ER_node_t) ((char *) tab + ALLOC_SIZE (sizeof (_ER_heap_tab)));
  return res;
}

entry_ptr_t
_tab_entries (ER_node_t tab)
{
  entry_ptr_t res;

  d_assert (ER_NODE_MODE (tab) == ER_NM_heap_tab);
  res = (entry_ptr_t) ((char *) tab + ALLOC_SIZE (sizeof (_ER_heap_tab))
		       + 2 * ER_els_space_bound (tab) * sizeof (val_t));
  return res;
}

ER_node_t
_stack_vars (ER_node_t stack)
{
  ER_node_t res;
  
  d_assert (ER_NODE_MODE (stack) == ER_NM_heap_stack);
  res = (ER_node_t) ((char *) stack
		     + ALLOC_SIZE (sizeof (_ER_heap_stack)));
  return res;
}

BC_node_t
_stack_block (ER_node_t stack)
{
  d_assert (ER_NODE_MODE (stack) == ER_NM_heap_stack);
  return ER_block_node (stack);
}

stack_ptr_t
_stacks_table (ER_node_t thread)
{
  stack_ptr_t res;
  
  d_assert (ER_NODE_MODE (thread) == ER_NM_heap_thread);
  res = (stack_ptr_t) ((char *) thread
		       + ALLOC_SIZE (sizeof (_ER_heap_thread)));
  return res;
}

ER_node_t
_indexed_val (ER_node_t first_var, int index)
{
  ER_node_t res;
  
  res = (ER_node_t) ((char *) first_var + index * (int) sizeof (val_t));
  return res;
}

ER_node_t
_indexed_el_key (ER_node_t first_el, int index)
{
  ER_node_t res;
  
  res = (ER_node_t) ((char *) first_el + 2 * index * (int) sizeof (val_t));
  return res;
}

ER_node_t
_indexed_el_val (ER_node_t first_el, int index)
{
  ER_node_t res;
  
  res = (ER_node_t) ((char *) first_el
		     + (2 * index + 1) * (int) sizeof (val_t));
  return res;
}

#endif



int
eq_val (val_t *val1_ptr, val_t *val2_ptr, size_t number)
{
  size_t i, displ;

  for (i = 0; i < number; i++)
    if (ER_NODE_MODE ((ER_node_t) &val1_ptr [i])
	!= ER_NODE_MODE ((ER_node_t) &val2_ptr [i]))
      return FALSE;
    else
      {
	displ = val_displ ((ER_node_t) &val1_ptr [i]);
	if (memcmp ((char *) &val1_ptr [i] + displ,
		    (char *) &val2_ptr [i] + displ,
		    type_size_table [ER_NODE_MODE
				     ((ER_node_t) &val1_ptr [i])]) != 0)
	  return FALSE;
      }
  return TRUE;
}

/* Stack size with NVARS variables.  */
static size_t do_always_inline
vars_stack_size (int nvars)
{
  return nvars * sizeof (val_t) + ALLOC_SIZE (sizeof (_ER_heap_stack));
}

/* Minimal stack size of BLOCK_NODE.  */
static size_t do_always_inline
shrink_block_stack_size (BC_node_t block_node)
{
  d_assert (block_node != NULL
	    && BC_IS_OF_TYPE (block_node, BC_NM_block));
  return vars_stack_size (BC_vars_num (block_node));
}

/* The func returns size (in bytes) of the stack of the block node given
   as BLOCK_NODE. */
static size_t do_always_inline
new_block_stack_size (BC_node_t block_node)
{
  d_assert (block_node != NULL
	    && BC_IS_OF_TYPE (block_node, BC_NM_block));
  return (vars_stack_size (BC_vars_num (block_node))
	  + BC_tvars_num (block_node) * sizeof (val_t));
}

/* The func returns size (in bytes) of the STACK. */
size_t do_always_inline
stack_size (ER_node_t stack)
{
  d_assert (ER_NODE_MODE (stack) == ER_NM_heap_stack);
  return (ER_all_block_vars_num (stack) * sizeof (val_t)
	  + ALLOC_SIZE (sizeof (_ER_heap_stack)));
}

ER_node_t
create_gmp (void)
{
  ER_node_t gmp;

  gmp = heap_allocate (ALLOC_SIZE (sizeof (_ER_heap_gmp)), FALSE);
  ER_SET_MODE (gmp, ER_NM_heap_gmp);
  ER_set_immutable (gmp, FALSE);
  mpz_init (*ER_mpz_ptr (gmp));
  return gmp;
}

ER_node_t
copy_gmp (ER_node_t gmp)
{
  ER_node_t res = create_gmp ();

  mpz_set (*ER_mpz_ptr (res), *ER_mpz_ptr (gmp));
  return res;
}



/* This page contains definitions and funcs needed for work with heap. */

/* The heap objects are placed on one side of the heap chunks. New
   chunk is created when there is no memory in GC.

                 heap growth
              ----->     <---- 
         __________________________       __________________________   
	|        |         |       |     |        |          |      |  
	| objects|         |stacks |     | objects|          |stacks|  ...
	 --------------------------       --------------------------   
	          ^	    ^	           
		  |         |
	chunk_free         chunk_stack_top */

struct heap_chunk
{
  /* First and first after the last byte of the heap chunk. */
  char *chunk_start, *chunk_bound;
  char *chunk_free, *chunk_stack_top;
};

/* Array of heap descriptors. */

static vlo_t heap_chunks;

static struct heap_chunk *curr_heap_chunk;

/* The following VLO contains registered external variable
   addresses. */
static vlo_t external_vars;

/* The number of made GC. */
unsigned int gc_number;
/* Average percent of free memory after all GCs. */
int free_gc_memory_percent;
/* Current and maximum size of all heap. */
size_t heap_size, max_heap_size;
/* Free heap memory */
size_t free_heap_memory;
/* Current and maximum number of heap_chunks */
int heap_chunks_number, max_heap_chunks_number;
/* TRUE if we are making GC. */
int in_gc_p;
#ifndef NO_CONTAINER_CACHE
/* All container cache entries are valid when their ticks are equal to
   the following variable value. */
int current_cached_container_tick;
#endif
#ifndef NO_PROFILE
#if HAVE_SETITIMER
/* Number of interrupts in GC. */
int gc_interrupts_number;
#else
/* Time in GC. */
ticker_t gc_ticker;
#endif
#endif

/* EXECUTED_STMTS_COUNT when we recognized that we need GC.  If it is
   possitive we don't need GC.  */
int GC_executed_stmts_count;

/* True if can not do GC now.  */
int no_gc_p;

/* Each heap object has unique number for hashing purpose. */
static size_t unique_number;

static void
new_heap_chunk (size_t size)
{
  char *str;
  size_t old_size;
  static const size_t max_default_chunk_size = 8*1024*1024;

  VLO_EXPAND (heap_chunks, sizeof (struct heap_chunk));
  curr_heap_chunk
    = &((struct heap_chunk *) VLO_BEGIN (heap_chunks))
      [VLO_LENGTH (heap_chunks) / sizeof (struct heap_chunk) - 1];
  old_size = size;
  if (size + free_heap_memory < (heap_size - free_heap_memory) / 3)
    {
      size = (heap_size - 4 * free_heap_memory) / 3;
      if (size > max_default_chunk_size && old_size <= max_default_chunk_size)
	size = max_default_chunk_size;
    }
  size = ALLOC_SIZE (size);
  CALLOC (str, 1, size);
  curr_heap_chunk->chunk_start = curr_heap_chunk->chunk_free = str;
  curr_heap_chunk->chunk_stack_top = curr_heap_chunk->chunk_bound = str + size;
  heap_size += size;
  if (max_heap_size < heap_size)
    max_heap_size = heap_size;
  free_heap_memory += size;
  heap_chunks_number++;
  if (max_heap_chunks_number < heap_chunks_number)
    max_heap_chunks_number = heap_chunks_number;
}

static void
del_heap_chunks (int number_saved_chunks)
{
  int i;
  
  for (i = number_saved_chunks;
       i < VLO_LENGTH (heap_chunks) / sizeof (struct heap_chunk);
       i++)
    {
      curr_heap_chunk = &((struct heap_chunk *) VLO_BEGIN (heap_chunks)) [i];
      FREE (curr_heap_chunk->chunk_start);
      heap_chunks_number--;
      heap_size
	-= curr_heap_chunk->chunk_bound - curr_heap_chunk->chunk_start;
    }
  VLO_SHORTEN (heap_chunks,
	       VLO_LENGTH (heap_chunks)
	       - number_saved_chunks * sizeof (struct heap_chunk));
  curr_heap_chunk = (&((struct heap_chunk *) VLO_BEGIN (heap_chunks))
		     [number_saved_chunks - 1]);
}

/* The func create the heap with SIZE zero bytes and initiate all
   members of the heap descriptor.  The func also initiate cstack.
   The func is to be called only once before any heap usage. */
void
initiate_heap ()
{
  VLO_CREATE (heap_chunks, 0);
  VLO_CREATE (external_vars, 0);
  /* 0 is reserved for NULL. */
  unique_number = 1;
  max_heap_size = heap_size = 0;
  heap_chunks_number = max_heap_chunks_number = 0;
  free_heap_memory = 0;
  new_heap_chunk (heap_chunk_size);
  cstack = NULL;
  uppest_stack = NULL;
  CREATE_TEMP_REF ();
  VLO_CREATE (temp_vlobj, 256);
  VLO_CREATE (temp_vlobj2, 256);
  VLO_CREATE (dl_handle_vlo, 0);
  gc_number = 0;
  GC_executed_stmts_count = 1;
  free_gc_memory_percent = 0;
  context_number = 0;
  no_gc_p = in_gc_p = FALSE;
  d_assert (DESTROY_FLDID_NUM >= 0);
#ifndef NO_CONTAINER_CACHE
  current_cached_container_tick = 0;
#endif
#ifndef NO_PROFILE
#if HAVE_SETITIMER
  gc_interrupts_number = 0;
#else
  if (profile_flag)
    {
      gc_ticker = create_ticker ();
      ticker_off (&gc_ticker);
    }
#endif
#endif
}

/* Some forward declarations. */
static void clean_heap_object_process_flag (void);
static int mark_stacks_need_destroying (int mark_dependent_p);
static void destroy_stacks (void);

/* Just call destroy functions. */
void
final_call_destroy_functions (void)
{
  BC_node_t block_node;

  if (uppest_stack == NULL)
    /* Heap is not initialized.  */
    return;
  d_assert (curr_heap_chunk->chunk_start > (char *) uppest_stack
	    || (char *) uppest_stack >= curr_heap_chunk->chunk_bound
	    || (char *) uppest_stack < curr_heap_chunk->chunk_free
	    || (char *) uppest_stack >= curr_heap_chunk->chunk_stack_top);
  clean_heap_object_process_flag ();
  if (mark_stacks_need_destroying (FALSE))
    {
      d_assert (cstack != NULL);
      if (cstack == uppest_stack)
	{
	  block_node = ER_block_node (cstack);
	  cvars = ER_stack_vars (cstack);
	  ctop = (ER_node_t) ((char *) cvars
			      + BC_vars_num (block_node)
			      * sizeof (val_t) - sizeof (val_t));
	  cpc = BC_next (ER_call_pc (cstack));
	  if (cthread != NULL)
	    ER_set_saved_cstack (cthread, cstack);
	}
      destroy_stacks ();
    }
}

void
finish_heap (void)
{
  for (curr_heap_chunk = VLO_BEGIN (heap_chunks);
       (char *) curr_heap_chunk <= (char *) VLO_END (heap_chunks);
       curr_heap_chunk++)
    FREE (curr_heap_chunk->chunk_start);
  VLO_DELETE (heap_chunks);
  VLO_DELETE (external_vars);
  FINISH_TEMP_REF ();
  VLO_DELETE (dl_handle_vlo);
  VLO_DELETE (temp_vlobj2);
  VLO_DELETE (temp_vlobj);
}

/* The following func is used for allocation of any heap object
   (except for the stack frame) with SIZE (!=0).  The func returns
   pointer to the allocated heap memory.  If there is no heap memory
   than fatal error is fixed.  The allocated memory is zeroed (see
   initiate_heap and compact_heap). */
void *
heap_allocate (size_t size, int stack_p)
{
  void *result;
  static int allocs_after_GC = 0;

  d_assert (size > 0);
  size = ALLOC_SIZE (size);
  allocs_after_GC++;
  if (curr_heap_chunk->chunk_stack_top - curr_heap_chunk->chunk_free < size)
    {
      /* We need GC.  Flag this.  */
      d_assert (executed_stmts_count <= 0);
      if (heap_chunks_number >= 16)
	{
	  GC_executed_stmts_count = executed_stmts_count;
	  executed_stmts_count = 0;
	  allocs_after_GC = 0;
	}
      if (curr_heap_chunk->chunk_stack_top - curr_heap_chunk->chunk_free
	  < size)
	new_heap_chunk (size > heap_chunk_size ? size : heap_chunk_size);
    }
  else if (allocs_after_GC > 1000 && gmp_memory_size > 16 * heap_chunk_size)
    {
      GC_executed_stmts_count = executed_stmts_count;
      executed_stmts_count = 0;
      allocs_after_GC = 0;
    }
  if (stack_p)
    {
      curr_heap_chunk->chunk_stack_top -= size;
      result = curr_heap_chunk->chunk_stack_top;
    }
  else
    {
      result = curr_heap_chunk->chunk_free;
      curr_heap_chunk->chunk_free += size;
    }
  d_assert (curr_heap_chunk->chunk_free <= curr_heap_chunk->chunk_stack_top);
  free_heap_memory -= size;
  ER_SET_MODE ((ER_node_t) result, ER_NM_heap_stack);
  ER_set_unique_number ((ER_node_t) result, unique_number);
  unique_number++;
  return result;
}

static size_t do_inline
heap_object_size (ER_node_t obj)
{
  size_t size = 1;

  switch (ER_NODE_MODE (obj))
    {
    case ER_NM_heap_pack_vect:
    case ER_NM_heap_unpack_vect:
    case ER_NM_heap_tab:
    case ER_NM_heap_redir:
      size = ER_allocated_length (obj);
      break;
    case ER_NM_heap_stack:
      size = stack_size (obj);
      break;
    case ER_NM_heap_thread:
      size = ALLOC_SIZE (sizeof (_ER_heap_thread));
      break;
    case ER_NM_heap_hideblock:
      size = (ALLOC_SIZE (sizeof (_ER_heap_hideblock))
	      + ER_hideblock_length (obj));
      break;
    case ER_NM_heap_gmp:
      size = ALLOC_SIZE (sizeof (_ER_heap_gmp));
      break;
    default:
      d_unreachable ();
    }
  return ALLOC_SIZE (size);
}

static void do_always_inline
try_heap_stack_free (void *from, size_t size)
{
  if (curr_heap_chunk->chunk_stack_top == (char *) from)
    {
      curr_heap_chunk->chunk_stack_top += size;
      free_heap_memory += size;
    }
}

/* Make stack smaller (containing no temps).  */
static void do_always_inline
shrink_stack (ER_node_t stack)
{
  size_t size;
  BC_node_t block = ER_block_node (stack);
  
  d_assert (BC_ext_life_p (block));
  size = heap_object_size (stack);
  if (curr_heap_chunk->chunk_free != (char *) stack + size)
    /* Mark it for shrinkage during GC.  */
    ER_set_prev_stack (stack, NULL);
  else
    {
      size -= ALLOC_SIZE (shrink_block_stack_size (block));
      ER_set_all_block_vars_num (stack, BC_vars_num (block));
      d_assert (stack_size (stack) == shrink_block_stack_size (block));
      curr_heap_chunk->chunk_free -= size;
      free_heap_memory += size;
    }
  /* We need it for correct changing refs during GC.  */
  ER_set_ctop (stack,
	       (char *) ((val_t *) ER_stack_vars (stack)
			 + BC_vars_num (block) - 1));
}

static int do_always_inline
stack_with_destroy (ER_node_t obj)
{
  BC_node_t decl;

  if (ER_NODE_MODE (obj) != ER_NM_heap_stack)
    return FALSE;
  decl = LV_BLOCK_IDENT_DECL (ER_block_node (obj), DESTROY_FLDID_NUM);
  if (decl == NULL)
    return FALSE;
  return BC_IS_OF_TYPE (decl, BC_NM_fdecl);
}

/* Return true if it is a stack needing shrinkage.  */
static int do_always_inline
stack_for_shrink_p (ER_node_t stack)
{
  return (ER_NODE_MODE (stack) == ER_NM_heap_stack
	  && ER_prev_stack (stack) == NULL
	  && BC_ext_life_p (ER_block_node (stack)));
}

static size_t
tailored_heap_object_size (ER_node_t obj)
{
  size_t size, el_size, els_num, head_size, all_els_size, optimal_size;
  ER_node_mode_t node_mode = ER_NODE_MODE (obj);
  
  if (node_mode == ER_NM_heap_pack_vect || node_mode == ER_NM_heap_unpack_vect)
    {
      size = ER_allocated_length (obj);
      el_size = (node_mode == ER_NM_heap_pack_vect
		 ? type_size_table [ER_pack_vect_el_mode (obj)]
		 : sizeof (val_t));
      els_num = ER_els_number (obj);
      all_els_size = els_num * el_size;
      head_size = ALLOC_SIZE (ER_node_size [node_mode]);
      optimal_size = (head_size + OPTIMAL_ELS_NUM (els_num) * el_size);
      if (optimal_size < head_size + all_els_size + ER_disp (obj))
	optimal_size = head_size + all_els_size + ER_disp (obj);
      if (size > optimal_size)
	size = optimal_size;
    }
  else if (stack_for_shrink_p (obj))
    size = shrink_block_stack_size (ER_block_node (obj));
  else
    size = heap_object_size (obj);
  return ALLOC_SIZE (size);
}

static ER_node_t do_always_inline
next_heap_object (ER_node_t obj)
{
  /* ER_it_was_processed (obj) might be not 0 or 1 here when we create
     objects during destroying.  */
  return (ER_node_t) ((char *) obj + heap_object_size (obj));
}

static void
clean_heap_object_process_flag (void)
{
  ER_node_t curr_obj;
  struct heap_chunk *curr_descr;

  for (curr_descr = VLO_BEGIN (heap_chunks);
       (char *) curr_descr <= (char *) VLO_END (heap_chunks);
       curr_descr++)
    {
      for (curr_obj = (ER_node_t) curr_descr->chunk_start;
	   (char *) curr_obj < curr_descr->chunk_free;
	   curr_obj = next_heap_object (curr_obj))
	ER_set_it_was_processed (curr_obj, FALSE);
      for (curr_obj = (ER_node_t) curr_descr->chunk_stack_top;
	   (char *) curr_obj < curr_descr->chunk_bound;
	   curr_obj = next_heap_object (curr_obj))
	ER_set_it_was_processed (curr_obj, FALSE);
    }
}

static void traverse_used_heap_object (ER_node_t start_obj);

static void
traverse_used_var (ER_node_t var)
{
  switch (ER_NODE_MODE (var))
    {
    case ER_NM_undef:
    case ER_NM_nil:
    case ER_NM_hide:
    case ER_NM_char:
    case ER_NM_int:
    case ER_NM_float:
    case ER_NM_type:
      return;
    case ER_NM_long:
      traverse_used_heap_object (ER_l (var));
      return;
    case ER_NM_hideblock:
      traverse_used_heap_object (ER_hideblock (var));
      return;
    case ER_NM_vect:
      traverse_used_heap_object (ER_vect (var));
      return;
    case ER_NM_tab:
      traverse_used_heap_object (ER_tab (var));
      return;
    case ER_NM_code:
      traverse_used_heap_object (ER_code_context (var));
      return;
    case ER_NM_efun:
      return;
    case ER_NM_thread:
      traverse_used_heap_object (ER_thread (var));
      return;
    case ER_NM_stack:
      traverse_used_heap_object (ER_stack (var));
      return;
    case ER_NM_external_var_ref:
      return;
    default:
      d_unreachable ();
    }
}

/* Set up when we find used stack corresponding to C stack.  */
static int leave_c_stacks_p;

/* Traverse and Mark all reachable objects from OBJ.  Set up
   LEAVE_C_STACKS_P if necessary.  */
static void
traverse_used_heap_object (ER_node_t obj)
{
  size_t i;

  if (obj == NULL || ER_it_was_processed (obj))
    return;
  ER_set_it_was_processed (obj, TRUE);
  switch (ER_NODE_MODE (obj))
    {
    case ER_NM_heap_hideblock:
    case ER_NM_heap_gmp:
      return;
    case ER_NM_heap_pack_vect:
      {
	ER_node_mode_t el_type = ER_pack_vect_el_mode (obj);
	size_t el_size;
	
	if (el_type == ER_NM_undef || el_type == ER_NM_nil
	    || el_type == ER_NM_hide
	    || el_type == ER_NM_char || el_type == ER_NM_byte
	    || el_type == ER_NM_int || el_type == ER_NM_float
	    || el_type == ER_NM_type)
	  return;
	el_size = type_size_table [el_type];
	if (el_type == ER_NM_hideblock || el_type == ER_NM_long
	    || el_type == ER_NM_vect || el_type == ER_NM_tab 
	    || el_type == ER_NM_thread || el_type == ER_NM_stack)
	  for (i = 0; i < ER_els_number (obj); i++)
	    traverse_used_heap_object
	      (*(ER_node_t *) (ER_pack_els (obj) + i * el_size));
	else if (el_type == ER_NM_code)
	  for (i = 0; i < ER_els_number (obj); i++)
	    traverse_used_heap_object
	      (((_ER_code *) (ER_pack_els (obj) + i * el_size))
               ->code_context);
	else if (el_type == ER_NM_efun)
	  ;
	else
	  d_unreachable ();
	return;
      }
    case ER_NM_heap_unpack_vect:
      for (i = 0; i < ER_els_number (obj); i++)
	traverse_used_var (IVAL (ER_unpack_els (obj), i));
      return;
    case ER_NM_heap_tab:
      {
	ER_node_mode_t mode;

	ER_set_last_accessed_el_index (obj, EMPTY_ENTRY);
	for (i = 0; i < ER_els_bound (obj); i++)
	  {
	    mode = ER_NODE_MODE (INDEXED_EL_KEY (ER_tab_els (obj), i));
	    if (mode == ER_NM_empty_el)
	      continue;
	    traverse_used_var (INDEXED_EL_KEY (ER_tab_els (obj), i));
	    traverse_used_var (INDEXED_EL_VAL (ER_tab_els (obj), i));
	  }
      }
      return;
    case ER_NM_heap_redir:
      traverse_used_heap_object (ER_redir (obj));
      return;
    case ER_NM_heap_stack:
      {
	ER_node_t var;
	
	if (ER_c_stack_p (obj))
	  {
	    leave_c_stacks_p = TRUE;
	    ER_set_c_stack_p (obj, FALSE);
	  }
	traverse_used_heap_object (ER_prev_stack (obj));
	traverse_used_heap_object (ER_context (obj));
	for (var = ER_stack_vars (obj); /* !!! */
	     (char *) var <= ER_ctop (obj);
	     var = IVAL (var, 1))
	  traverse_used_var (var);
	return;
      }
    case ER_NM_heap_thread:
      traverse_used_heap_object (ER_context (obj));
      traverse_used_heap_object (ER_father (obj));
      traverse_used_heap_object (ER_prev (obj));
      traverse_used_heap_object (ER_next (obj));
      traverse_used_heap_object (ER_saved_cstack (obj));
      return;
    default:
      d_unreachable ();
    }
}

/* Return true if there are objects to be destroyed.  Mark stacks
   achieved from stack to be destroyed if MARK_DEPENDENT_P.  */
static int
mark_stacks_need_destroying (int mark_dependent_p)
{
  ER_node_t curr_obj;
  struct heap_chunk *curr_descr;
  int result = FALSE;

  for (curr_descr = VLO_BEGIN (heap_chunks);
       (char *) curr_descr <= (char *) VLO_END (heap_chunks);
       curr_descr++)
    {
      for (curr_obj = (ER_node_t) curr_descr->chunk_start;
	   (char *) curr_obj < curr_descr->chunk_free;
	   curr_obj = next_heap_object (curr_obj))
	if (!ER_it_was_processed (curr_obj) && stack_with_destroy (curr_obj)
	    && ER_state (curr_obj) != IS_destroyed)
	  {
	    ER_set_state (curr_obj, IS_to_be_destroyed);
	    result = TRUE;
	  }
    }
  if (mark_dependent_p && result)
    for (curr_descr = VLO_BEGIN (heap_chunks);
	 (char *) curr_descr <= (char *) VLO_END (heap_chunks);
	 curr_descr++)
      {
	for (curr_obj = (ER_node_t) curr_descr->chunk_start;
	     (char *) curr_obj < curr_descr->chunk_free;
	     curr_obj = next_heap_object (curr_obj))
	  if (ER_state (curr_obj) == IS_to_be_destroyed)
	    traverse_used_heap_object (curr_obj);
      }
  return result;
}

static int
mark_used_heap_objects (void)
{
  int i;
  BC_node_t *block_ptr;

  clean_heap_object_process_flag ();
  traverse_used_heap_object (cstack);
  traverse_used_heap_object (uppest_stack);
  for (block_ptr = (BC_node_t *) VLO_BEGIN (block_tab);
       block_ptr < (BC_node_t *) VLO_BOUND (block_tab);
       block_ptr++)
    BC_set_free_stacks (*block_ptr, NULL);
  for (i = 0; i < TEMP_REFS_LENGTH (); i++)
    traverse_used_heap_object (GET_TEMP_REF (i));
  for (i = 0; i < VLO_LENGTH (external_vars) / sizeof (void *); i++)
    traverse_used_var ((ER_node_t) ((void **) VLO_BEGIN (external_vars)) [i]);
  /* Current stack table is traversed with cthread. */
  traverse_used_heap_object (cthread);
  traverse_used_heap_object (first_thread_not_started);
  /* Traverse all stacks which needs to be destroyed. */
  return mark_stacks_need_destroying (TRUE);
}

static do_inline char *
define_new_heap_object (ER_node_t obj, struct heap_chunk **descr, char *place)
{
  size_t size;

  if (ER_NODE_MODE (obj) == ER_NM_heap_redir)
    {
      /* Redirections do not survive GC. */
      ER_set_it_was_processed (obj, FALSE);
      ER_set_new_place (obj, NULL);
    }
  else if (ER_it_was_processed (obj))
    {
      int temp_refs_p = in_heap_temp_refs (obj);

      if (!temp_refs_p)
	try_full_pack (obj);
      size = (!temp_refs_p
	      ? tailored_heap_object_size (obj) : heap_object_size (obj));
      if (place + size > (*descr)->chunk_bound)
	{
	  (*descr)++;
	  place = (*descr)->chunk_start;
	}
      ER_set_new_place (obj, place);
      /* Packing will be only in compact_heap */
      place += size;
      d_assert (place <= (*descr)->chunk_bound);
    }
  else
    ER_set_new_place (obj, NULL);
  return place;
}

static int
define_new_heap_object_places (void)
{
  ER_node_t curr_obj;
  struct heap_chunk *curr_descr;
  struct heap_chunk *curr_place_descr;
  char *new_place;
  int result;

  result = mark_used_heap_objects ();
  curr_place_descr = VLO_BEGIN (heap_chunks);
  new_place = curr_place_descr->chunk_start;
  for (curr_descr = VLO_BEGIN (heap_chunks);
       (char *) curr_descr <= (char *) VLO_END (heap_chunks);
       curr_descr++)
    {
      for (curr_obj = (ER_node_t) curr_descr->chunk_start;
	   (char *) curr_obj < curr_descr->chunk_free;
	   curr_obj = next_heap_object (curr_obj))
	new_place
	  = define_new_heap_object (curr_obj, &curr_place_descr, new_place);
      for (curr_obj = (ER_node_t) curr_descr->chunk_stack_top;
	   (char *) curr_obj < curr_descr->chunk_bound;
	   curr_obj = next_heap_object (curr_obj))
	new_place
	  = define_new_heap_object (curr_obj, &curr_place_descr, new_place);
    }
  return result;
}

#define CHANGE_REF(ref)				\
  do						\
    if ((ref) != NULL)				\
      {						\
	(ref) = (ER_node_t) ER_new_place (ref);	\
      }						\
  while (0)

#define CHANGE_VECT_TAB_REF(ref)		\
  do						\
    if ((ref) != NULL)				\
      {						\
	GO_THROUGH_REDIR (ref);			\
	(ref) = (ER_node_t) ER_new_place (ref);	\
      }						\
  while (0)

/* The function changes one value with given VAL_ADDR of packed
   vector. The type of the value is given in MODE.  Don't use SPRUT
   access macros here.  They can check the reference which will refer
   to correct object only after the heap compaction. */
static void do_inline
change_val (ER_node_mode_t mode, ER_node_t *val_addr)
{
  switch (mode)
    {
    case ER_NM_undef:
    case ER_NM_nil:
    case ER_NM_hide:
    case ER_NM_char:
    case ER_NM_byte:
    case ER_NM_int:
    case ER_NM_float:
    case ER_NM_type:
    case ER_NM_efun:
      return;
    case ER_NM_vect:
    case ER_NM_tab:
      CHANGE_VECT_TAB_REF (*val_addr);
      return;
    case ER_NM_long:
    case ER_NM_hideblock:
    case ER_NM_thread:
    case ER_NM_stack:
    case ER_NM_code:
      CHANGE_REF (*val_addr);
      return;
    case ER_NM_external_var_ref:
      return;
    default:
      d_unreachable ();
    }
}

/* The function changes variable VAR value if it refers to a heap
   object.  Don't use SPRUT access macros here.  They can check the
   reference which will refer to correct object only after the heap
   compaction. */
static void
change_var (ER_node_t var)
{
  switch (ER_NODE_MODE (var))
    {
    case ER_NM_undef:
    case ER_NM_nil:
    case ER_NM_hide:
    case ER_NM_char:
    case ER_NM_int:
    case ER_NM_float:
    case ER_NM_type:
    case ER_NM_efun:
      return;
    case ER_NM_vect:
      CHANGE_VECT_TAB_REF (((_ER_vect *) var)->vect);
      return;
    case ER_NM_tab:
      CHANGE_VECT_TAB_REF (((_ER_tab *) var)->tab);
      return;
    case ER_NM_long:
      CHANGE_REF (((_ER_long *) var)->l);
      return;
    case ER_NM_hideblock:
      CHANGE_REF (((_ER_hideblock *) var)->hideblock);
      return;
    case ER_NM_code:
      CHANGE_REF (((_ER_code *) var)->code_context);
      return;
    case ER_NM_thread:
      CHANGE_REF (((_ER_thread *) var)->thread);
      return;
    case ER_NM_stack:
      CHANGE_REF (((_ER_stack *) var)->stack);
      return;
    case ER_NM_external_var_ref:
      return;
    default:
      d_unreachable ();
    }
}

static void do_inline
change_obj_refs (ER_node_t obj)
{
  size_t i;

  if (ER_it_was_processed (obj))
    switch (ER_NODE_MODE (obj))
      {
      case ER_NM_heap_pack_vect:
	{
	  ER_node_mode_t el_type;
	  size_t el_size;
	  
	  el_type = ER_pack_vect_el_mode (obj);
	  if (el_type == ER_NM_undef || el_type == ER_NM_nil || el_type == ER_NM_hide
	      || el_type == ER_NM_char || el_type == ER_NM_byte
	      || el_type == ER_NM_int || el_type == ER_NM_float
	      || el_type == ER_NM_type)
	    break;
	  el_size = type_size_table [el_type];
	  if (el_type == ER_NM_hideblock || el_type == ER_NM_long
	      || el_type == ER_NM_vect || el_type == ER_NM_tab
	      || el_type == ER_NM_thread || el_type == ER_NM_stack)
	    for (i = 0; i < ER_els_number (obj); i++)
	      change_val (el_type,
			  (ER_node_t *) (ER_pack_els (obj) + i * el_size));
	  else if (el_type == ER_NM_code)
	    for (i = 0; i < ER_els_number (obj); i++)
	      change_val (el_type,
			  &((_ER_code *)
			    (ER_pack_els (obj) + i * el_size))->code_context);
	  else if (el_type == ER_NM_efun)
	    ;
	  else
	    d_unreachable ();
	  break;
	}
      case ER_NM_heap_unpack_vect:
	for (i = 0; i < ER_els_number (obj); i++)
	  change_var (IVAL (ER_unpack_els (obj), i));
	break;
      case ER_NM_heap_tab:
	{
	  ER_node_mode_t el_type;
	  
	  for (i = 0; i < ER_els_bound (obj); i++)
	    {
	      el_type = ER_NODE_MODE (INDEXED_EL_KEY (ER_tab_els (obj), i));
	      if (el_type == ER_NM_empty_el)
		continue;
	      change_var (INDEXED_EL_KEY (ER_tab_els (obj), i));
	      change_var (INDEXED_EL_VAL (ER_tab_els (obj), i));
	    }
	  break;
	}
      case ER_NM_heap_stack:
	{
	  ER_node_t var;
	  size_t diff;
	  
	  CHANGE_REF (((_ER_heap_stack *) obj)
		      ->_ER_M_context_heap_obj.context);
	  CHANGE_REF (((_ER_heap_stack *) obj)->prev_stack);
	  for (var = ER_stack_vars (obj);
	       (char *) var <= ER_ctop (obj);
	       var = IVAL (var, 1))
	    change_var (var);
	  diff = ((char *) ER_ctop (obj) - (char *) ER_stack_vars (obj));
	  ((_ER_heap_stack *) obj)->ctop
	    = (ER_new_place (obj)
	       + ALLOC_SIZE (sizeof (_ER_heap_stack)) + diff);
	  break;
	}
      case ER_NM_heap_thread:
	CHANGE_REF (((_ER_heap_thread *) obj)->_ER_M_context_heap_obj.context);
	CHANGE_REF (((_ER_heap_thread *) obj)->father);
	CHANGE_REF (((_ER_heap_thread *) obj)->prev);
	CHANGE_REF (((_ER_heap_thread *) obj)->next);
	CHANGE_REF (((_ER_heap_thread *) obj)->saved_cstack);
	break;
      case ER_NM_heap_gmp:
      case ER_NM_heap_hideblock:
	break;
      default:
	/* Redirections do not survive GC. */
	d_unreachable ();
      }
}

/* The function changes all heap references onto references which must
   be after the heap compaction Don't use SPRUT access macros here.
   They can check the reference which will refer to correct object
   only after the heap compaction .*/
static void
change_refs (void)
{
  ER_node_t curr_obj;
  struct heap_chunk *curr_descr;
  size_t i;
  BC_node_t *block_ptr;

  for (curr_descr = VLO_BEGIN (heap_chunks);
       (char *) curr_descr <= (char *) VLO_END (heap_chunks);
       curr_descr++)
    {
      for (curr_obj = (ER_node_t) curr_descr->chunk_start;
	   (char *) curr_obj < curr_descr->chunk_free;
	   curr_obj = next_heap_object (curr_obj))
	change_obj_refs (curr_obj);
      for (curr_obj = (ER_node_t) curr_descr->chunk_stack_top;
	   (char *) curr_obj < curr_descr->chunk_bound;
	   curr_obj = next_heap_object (curr_obj))
	change_obj_refs (curr_obj);
    }
  CHANGE_REF (cstack);
  CHANGE_REF (uppest_stack);
  for (block_ptr = (BC_node_t *) VLO_BEGIN (block_tab);
       block_ptr < (BC_node_t *) VLO_BOUND (block_tab);
       block_ptr++)
    {
      curr_obj = BC_free_stacks (*block_ptr);
      CHANGE_REF (curr_obj);
      BC_set_free_stacks (*block_ptr, curr_obj);
    }
  /* `heap_temp_refs' may refer for a vector. */
  for (i = 0; i < TEMP_REFS_LENGTH (); i++)
    CHANGE_VECT_TAB_REF (GET_TEMP_REF (i));
  for (i = 0; i < VLO_LENGTH (external_vars) / sizeof (void *); i++)
    change_var ((ER_node_t) ((void **) VLO_BEGIN (external_vars)) [i]);
  CHANGE_REF (cthread);
  CHANGE_REF (first_thread_not_started);
}

static do_inline char *
move_or_destroy_object (ER_node_t obj, struct heap_chunk **descr,
			size_t *curr_heap_size, char *place)
{
  size_t tailored_size;

  if (! ER_it_was_processed (obj))
    {
      if (ER_NODE_MODE (obj) == ER_NM_heap_gmp)
	mpz_clear (*ER_mpz_ptr (obj));
    }
  else
    {
      /* Tailor vector size only here although tailoring has been
	 taken into account in place value.  Remeber that temp
	 refs are already set up to new places. */
      tailored_size = (!in_heap_temp_refs ((ER_node_t) place)
		       ? tailored_heap_object_size (obj)
		       : heap_object_size (obj));
      d_assert (tailored_size <= heap_object_size (obj));
      if (place + tailored_size > (*descr)->chunk_bound)
	{
	  /* Zero e.g. because we compare var as full structs. */
	  if ((*descr)->chunk_free > place)
	    memset (place, 0, (*descr)->chunk_free - place);
	  if ((*descr)->chunk_stack_top > place)
	    memset ((*descr)->chunk_stack_top, 0,
		    (*descr)->chunk_bound - (*descr)->chunk_stack_top);
	  else
	    memset (place, 0, (*descr)->chunk_bound - place);
	  free_heap_memory += (*descr)->chunk_bound - place;
	  (*descr)->chunk_free = place;
	  (*descr)->chunk_stack_top = (*descr)->chunk_bound;
	  (*descr)++;
	  place = (*descr)->chunk_start;
	  *curr_heap_size += ((*descr)->chunk_bound - (*descr)->chunk_start);
	}
      if (ER_NODE_MODE (obj) == ER_NM_heap_pack_vect
	  || ER_NODE_MODE (obj) == ER_NM_heap_unpack_vect)
	ER_set_allocated_length (obj, tailored_size);
      else if (stack_for_shrink_p (obj))
	ER_set_all_block_vars_num (obj, BC_vars_num (ER_block_node (obj)));
      d_assert (ER_new_place (obj) == place);
      if (place != (char *) obj)
	memmove (place, obj, tailored_size);
      place += tailored_size;
      d_assert (place <= (*descr)->chunk_bound);
    }
  return place;
}

static void
compact_heap (void)
{
  ER_node_t curr_obj, next_obj;
  struct heap_chunk *curr_descr;
  struct heap_chunk *curr_place_descr;
  size_t curr_heap_size;
  char *new_place;

  /* Compact the heap */
  curr_place_descr = VLO_BEGIN (heap_chunks);
  new_place = curr_place_descr->chunk_start;
  free_heap_memory = 0;
  curr_heap_size = (curr_place_descr->chunk_bound
		    - curr_place_descr->chunk_start);
  for (curr_descr = VLO_BEGIN (heap_chunks);
       (char *) curr_descr <= (char *) VLO_END (heap_chunks);
       curr_descr++)
    {
      for (curr_obj = (ER_node_t) curr_descr->chunk_start;
	   (char *) curr_obj < curr_descr->chunk_free;
	   curr_obj = next_obj)
	{
	  next_obj = next_heap_object (curr_obj);
	  new_place = move_or_destroy_object (curr_obj, &curr_place_descr,
					      &curr_heap_size, new_place);
	}
      /* Put stacks into another part of the heap.  Such cases should
	 be rare.  */
      for (curr_obj = (ER_node_t) curr_descr->chunk_stack_top;
	   (char *) curr_obj < curr_descr->chunk_bound;
	   curr_obj = next_obj)
	{
	  next_obj = next_heap_object (curr_obj);
	  new_place = move_or_destroy_object (curr_obj, &curr_place_descr,
					      &curr_heap_size, new_place);
	}
    }
  curr_heap_chunk = curr_place_descr;
  d_assert ((char *) (curr_heap_chunk + 1) <= (char *) VLO_END (heap_chunks)
	    || curr_heap_chunk->chunk_free + (curr_heap_chunk->chunk_bound
					      - curr_heap_chunk->chunk_stack_top)
	    >= new_place);
  /* Zero e.g. because we compare var as full structs: */
  /* Possible situation when e.g. two chunks are compacted in one. */
  if (curr_heap_chunk->chunk_free >= new_place)
    memset (new_place, 0, curr_heap_chunk->chunk_free - new_place);
  if (curr_heap_chunk->chunk_stack_top > new_place)
    memset (curr_heap_chunk->chunk_stack_top, 0,
	    curr_heap_chunk->chunk_bound - curr_heap_chunk->chunk_stack_top);
  else
    memset (new_place, 0, curr_heap_chunk->chunk_bound - new_place);
  curr_heap_chunk->chunk_free = new_place;
  curr_heap_chunk->chunk_stack_top = curr_heap_chunk->chunk_bound;
  free_heap_memory += curr_heap_chunk->chunk_bound - new_place;
  for (;;)
    {
      if (free_heap_memory >= curr_heap_size / 4)
	break;
      curr_heap_chunk++;
      if ((char *) curr_heap_chunk > (char *) VLO_END (heap_chunks))
	{
	  curr_heap_chunk--;
	  break;
	}
      free_heap_memory += (curr_heap_chunk->chunk_bound
			   - curr_heap_chunk->chunk_start);
      curr_heap_size += (curr_heap_chunk->chunk_bound
			 - curr_heap_chunk->chunk_start);
      memset (curr_heap_chunk->chunk_start, 0,
	      curr_heap_chunk->chunk_free - curr_heap_chunk->chunk_start);
      memset (curr_heap_chunk->chunk_stack_top, 0,
	      curr_heap_chunk->chunk_bound - curr_heap_chunk->chunk_stack_top);
      curr_heap_chunk->chunk_free = curr_heap_chunk->chunk_start;
      curr_heap_chunk->chunk_stack_top = curr_heap_chunk->chunk_bound;
    }
  del_heap_chunks (curr_heap_chunk
		   - (struct heap_chunk *) VLO_BEGIN (heap_chunks) + 1);
}

/* Sort the chunks in descending size order. */
static int
compare_chunks (const void *chunk1, const void *chunk2)
{
  const struct heap_chunk *c1 = chunk1;
  const struct heap_chunk *c2 = chunk2;

  if (c1->chunk_bound - c1->chunk_start > c2->chunk_bound - c2->chunk_start)
    return -1;
  else if (c1->chunk_bound - c1->chunk_start
	   < c2->chunk_bound - c2->chunk_start)
    return 1;
  else
    return 0;
}

static void
destroy_stacks (void)
{
  BC_node_t decl, fblock;
  ER_node_t curr_obj;
  struct heap_chunk *curr_descr;

  for (curr_descr = VLO_BEGIN (heap_chunks);
       (char *) curr_descr <= (char *) VLO_END (heap_chunks);
       curr_descr++)
    {
      for (curr_obj = (ER_node_t) curr_descr->chunk_start;
	   (char *) curr_obj < curr_descr->chunk_free;
	   curr_obj = next_heap_object (curr_obj))
	if (ER_NODE_MODE (curr_obj) == ER_NM_heap_stack
	    && ER_state (curr_obj) == IS_to_be_destroyed)
	  {
	    decl = LV_BLOCK_IDENT_DECL (ER_block_node (curr_obj),
					DESTROY_FLDID_NUM);
	    /* We mark it before the call to prevent infinite loop if
	       the exception occurs during the call. */
	    ER_set_state (curr_obj, IS_destroyed);
	    d_assert (decl != NULL && BC_IS_OF_TYPE (decl, BC_NM_fdecl));
	    fblock = BC_fblock (decl);
	    d_assert (fblock != NULL);
	    if (BC_next (fblock) != NULL)
	      call_fun_class (BC_fblock (decl), curr_obj, 0, FALSE);
	    else
	      {
		/* It is just a forward decl without definition.  */
		int flag = BC_undef_destroy_p (fblock);

		BC_set_undef_destroy_p (fblock, TRUE);
		if (! flag)
		  eval_error (abstrcall_bc_decl, BC_pos (decl),
			      DERR_unfinished_fun_class_call, BC_ident (decl));
	      }
	  }
    }
}

/* Redirections never survive GC.  The unpacked vectors may be
   transformed to packed (see comments for variable heap_temp_refs).
   The vector size may be tailored (but not increased) if its size is
   more than optimal one (see commentaries for variable heap_temp_refs
   again). */
void
GC (void)
{
  int flag;

  if (no_gc_p)
    return;
  leave_c_stacks_p = FALSE;
  /* Mark that we don't need GC anymore.  */
  d_assert (GC_executed_stmts_count <= 0);
  executed_stmts_count = GC_executed_stmts_count;
  GC_executed_stmts_count = 1;
#if !defined (NO_PROFILE) && !HAVE_SETITIMER
  if (profile_flag)
    ticker_on (&gc_ticker);
#endif
  in_gc_p = TRUE;
  ER_set_ctop (cstack, (char *) ctop);
#ifndef NO_CONTAINER_CACHE
  current_cached_container_tick++;
#endif
  qsort (VLO_BEGIN (heap_chunks),
	 VLO_LENGTH (heap_chunks) / sizeof (struct heap_chunk),
	 sizeof (struct heap_chunk), compare_chunks);
  flag = define_new_heap_object_places ();
  change_refs ();
  compact_heap ();
  tvars = ER_stack_vars (uppest_stack);
  cvars = ER_stack_vars (cstack);
  ctop = (ER_node_t) ER_ctop (cstack);
  gc_number++;
  free_gc_memory_percent
    = (free_gc_memory_percent * (gc_number - 1)
       + (100 * free_heap_memory) / heap_size) / gc_number;
  d_assert (free_gc_memory_percent < 100);
  in_gc_p = FALSE;
#if !defined (NO_PROFILE) && !HAVE_SETITIMER
  if (profile_flag)
    ticker_on (&gc_ticker);
#endif
  if (flag)
    destroy_stacks ();
  /* Switch to byte code execution as stacks might move.  */
  if (leave_c_stacks_p)
    switch_to_bcode ();
}



#if ! defined (NO_PROFILE) && HAVE_SETITIMER
int all_interrupts_number;

/* The following function processes interrupt from virtual alarm. */
void
profile_interrupt (void)
{
  BC_node_t block;
  
  all_interrupts_number++;
  if (in_gc_p)
    {
      gc_interrupts_number++;
      return;
    }
  for (block = ER_block_node (cstack);
       block != NULL && BC_NODE_MODE (block) == BC_NM_block;
       block = BC_scope (block))
    ;
  if (block != NULL)
    BC_set_interrupts_number (block,
			      BC_interrupts_number (block) + 1);
}
#endif

#ifndef NO_PROFILE
/* Update profile info when we use profiling.  */
void do_always_inline
update_profile (BC_node_t block_node)
{
  BC_set_calls_number (block_node, BC_calls_number (block_node) + 1);
  if (BC_NODE_MODE (block_node) == BC_NM_fblock)
    {
#if !HAVE_SETITIMER
      if (BC_calls_number (block_node) == 0)
	BC_set_exec_time (block_node, create_ticker ());
      else
	ticker_on (&BC_exec_time (block_node));
#endif
    }
}
#endif

ER_node_t do_always_inline
alloc_new_stack (BC_node_t block_node, int vars_num)
{
  ER_node_t stack;

  /* Remember about possible GC. */
  stack = ((ER_node_t)
	   heap_allocate (new_block_stack_size (block_node), FALSE));
  ER_SET_MODE (stack, ER_NM_heap_stack);
  ER_set_block_node (stack, block_node);
  ER_set_all_block_vars_num (stack, vars_num + BC_tvars_num (block_node));
  ER_set_context_number (stack, context_number);
  context_number++;
  ER_set_immutable (stack, FALSE);
  ER_set_state (stack, IS_initial);
  return stack;
}

/* As the above function but also update profile info.  */
void
heap_push (BC_node_t block_node, ER_node_t context, int offset)
{
#ifndef NO_PROFILE
  if (profile_flag)
    update_profile (block_node);
#endif
  heap_push_without_profile_update (block_node, context,
				    BC_vars_num (block_node), offset);
}

/* The following func finishes work of the block, modifing cstack and
   cpc (only if the corresponding block is block of func or class).
   No GC during this function. */
void
heap_pop (void)
{
  ER_node_t stack = cstack;
  BC_node_t block_node = ER_block_node (cstack);

  if (BC_NODE_MODE (block_node) == BC_NM_fblock)
    {
      cpc = BC_next (ER_call_pc (stack));
#if ! defined (NO_PROFILE) && !HAVE_SETITIMER
      if (profile_flag)
	ticker_off (&BC_exec_time (block_node));
#endif
    }
  ER_set_ctop (cstack, (char *) ctop);
  cstack = ER_prev_stack (cstack);
#ifndef NO_CONTAINER_CACHE
  current_cached_container_tick++;
#endif
  if (cstack == NULL)
    cvars = ctop = NULL;
  else
    {
      cvars = ER_stack_vars (cstack);
      /* ??? Too conservative until next call: some temporary are
	 saved.  */
      ctop = (ER_node_t) ER_ctop (cstack);
    }
  if (BC_ext_life_p (block_node))
    shrink_stack (stack);
  else
    {
      ER_set_prev_stack (stack, BC_free_stacks (block_node));
      BC_set_free_stacks (block_node, stack);
      if (BC_NODE_MODE (block_node) == BC_NM_fblock
	  && BC_pure_fun_p (block_node))
	{
	  /* Saving the result.  */
	  ER_node_t res = IVAL (ER_stack_vars (stack),
				BC_pars_num (block_node));

	  *(val_t *) res = *(val_t *) IVAL (ctop, 1);
	}
    }
  ER_set_saved_cstack (cthread, cstack);
}

/* Number of vars in previous version of the top block.  */
static int previous_uppest_stack_vars_num;

/* Create the uppest stack from its BLOCK_NODE.  */
void
create_uppest_stack (BC_node_t block_node)
{
  heap_push (block_node, NULL, 0);
  uppest_stack = cstack;
  tvars = ER_stack_vars (uppest_stack);
  previous_uppest_stack_vars_num = BC_vars_num (block_node);
}

/* Expand the uppest stack if necessary.  The current stack should the
   existing uppest stack.  */
void
expand_uppest_stack (void)
{
  BC_node_t block_node;
  int vars_num, tvars_num, new_all_block_vars;
  ER_node_t stack;
  
  d_assert (cstack == uppest_stack);
  block_node = ER_block_node (uppest_stack);
  vars_num = BC_vars_num (block_node);
  d_assert (previous_uppest_stack_vars_num <= vars_num);
  tvars_num = BC_tvars_num (block_node);
  if (ER_all_block_vars_num (uppest_stack) >= vars_num + tvars_num)
    {
      ctop = (ER_node_t) ((char *) cvars + (vars_num - 1) * sizeof (val_t));
      reset_vars (IVAL (cvars, previous_uppest_stack_vars_num), IVAL (ctop, 1));
      previous_uppest_stack_vars_num = vars_num;
      return;
    }
  new_all_block_vars = 2 * (vars_num + tvars_num);
  stack = ((ER_node_t)
	   heap_allocate (vars_stack_size (new_all_block_vars),
			  ! BC_ext_life_p (block_node)));
  memcpy (stack, uppest_stack, shrink_block_stack_size (block_node));
  ER_set_all_block_vars_num (stack, new_all_block_vars);
  setup_new_cstack (stack, vars_num, previous_uppest_stack_vars_num);
  previous_uppest_stack_vars_num = vars_num;
  uppest_stack = cstack;
  tvars = cvars;
}

void
clear_c_stack_flags (void)
{
  ER_node_t thread, stack;
  
  if (generated_c_function_calls_num == 0)
    return;
  for (thread = cthread;;)
    {
      stack = thread == cthread ? cstack : ER_saved_cstack (thread);
      for (; stack != NULL; stack = ER_prev_stack (stack))
	ER_set_c_stack_p (stack, FALSE);
      thread = ER_next (thread);
      if (thread == cthread)
	break;
    }
}



ER_node_t
create_hideblock (size_t length)
{
  ER_node_t hideblock;
  size_t allocated_length;

  allocated_length = (ALLOC_SIZE (sizeof (_ER_heap_hideblock)) + length);
  hideblock = heap_allocate (allocated_length, FALSE);
  ER_SET_MODE (hideblock, ER_NM_heap_hideblock);
  ER_set_immutable (hideblock, FALSE);
  ER_set_hideblock_length (hideblock, length);
  return hideblock;
}



/* The page contains functions for vectors. */

/* Create mutable empty vector (it is always packed vector). */
ER_node_t
create_empty_vector (void)
{
  ER_node_t vect;

  vect = heap_allocate (ALLOC_SIZE (sizeof (_ER_heap_pack_vect)), FALSE);
  ER_SET_MODE (vect, ER_NM_heap_pack_vect);
  ER_set_immutable (vect, FALSE);
  ER_set_allocated_length (vect, ALLOC_SIZE (sizeof (_ER_heap_pack_vect)));
  ER_set_pack_vect_el_mode (vect, ER_NM_nil);
  ER_set_els_number (vect, 0);
  ER_set_disp (vect, 0);
  return vect;
}

/* Create mutable unpacked vector with ELS_NUMBER elements (their
   value is undefined). */
ER_node_t
create_unpack_vector (size_t els_number)
{
  ER_node_t unpack_vect;
  size_t allocated_length;

  allocated_length = (ALLOC_SIZE (sizeof (_ER_heap_unpack_vect))
		      + OPTIMAL_ELS_NUM (els_number) * sizeof (val_t));
  unpack_vect = heap_allocate (allocated_length, FALSE);
  ER_SET_MODE (unpack_vect, ER_NM_heap_unpack_vect);
  ER_set_immutable (unpack_vect, FALSE);
  ER_set_els_number (unpack_vect, els_number);
  ER_set_disp (unpack_vect, 0);
  ER_set_allocated_length (unpack_vect, allocated_length);
  return unpack_vect;
}

/* Create mutable packed vector with ELS_NUMBER elements of ELTYPE
   (their value is undefined). */
ER_node_t
create_pack_vector (size_t els_number, ER_node_mode_t eltype)
{
  ER_node_t pack_vect;
  size_t allocated_length;

  allocated_length = (ALLOC_SIZE (sizeof (_ER_heap_pack_vect))
		      + OPTIMAL_ELS_NUM (els_number)
		      * type_size_table [eltype]);
  pack_vect = heap_allocate (allocated_length, FALSE);
  ER_SET_MODE (pack_vect, ER_NM_heap_pack_vect);
  ER_set_pack_vect_el_mode (pack_vect, eltype);
  ER_set_immutable (pack_vect, FALSE);
  ER_set_els_number (pack_vect, els_number);
  ER_set_disp (pack_vect, 0);
  ER_set_allocated_length (pack_vect, allocated_length);
  return pack_vect;
}

/* Make vector VECT contains room for ELS_NUMBER.  Return the vector
   (may be with new place). */
ER_node_t
expand_vector (ER_node_t vect, size_t els_number)
{
  size_t disp, allocated_length, prev_vect_allocated_length;
  size_t el_length, header_length, vect_els_number;
  const char *els;
  ER_node_t prev_vect;

  disp = ER_disp (vect);
  vect_els_number = ER_els_number (vect);
  allocated_length = ER_allocated_length (vect);
  if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
      && (ER_pack_vect_el_mode (vect) == ER_NM_char
	  || ER_pack_vect_el_mode (vect) == ER_NM_byte))
    {
      els_number++; /* for trailing zero byte */
      vect_els_number++;
    }
  prev_vect_allocated_length = allocated_length;
  if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
    {
      header_length = ALLOC_SIZE (sizeof (_ER_heap_unpack_vect));
      els = (const char *) ER_unpack_els (vect);
      el_length = sizeof (val_t);
    }
  else 
    {
      header_length = ALLOC_SIZE (sizeof (_ER_heap_pack_vect));
      els = ER_pack_els (vect);
      el_length = type_size_table [ER_pack_vect_el_mode (vect)];
    }
  if (allocated_length < header_length + els_number * el_length)
    allocated_length = (header_length
			+ OPTIMAL_ELS_NUM (els_number) * el_length);
  if (allocated_length != prev_vect_allocated_length)
    {
      prev_vect = vect;
      vect = heap_allocate (allocated_length, FALSE);
      /* After this, vect has the same unique_number. */
      if (disp == 0)
	memcpy (vect, prev_vect, prev_vect_allocated_length);
      else
	{	
	  memcpy (vect, prev_vect, header_length);
	  /* Set it before getting els.  */
	  ER_set_disp (vect, 0);
	  memcpy (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect
		  ? (char *) ER_unpack_els (vect) : (char *) ER_pack_els (vect),
		  els, vect_els_number * el_length);
	}
      ER_set_allocated_length (vect, allocated_length);
      ER_SET_MODE (prev_vect, ER_NM_heap_redir);
      ER_set_allocated_length (prev_vect, prev_vect_allocated_length);
      ER_set_redir (prev_vect, vect);
    }
  else if (allocated_length < header_length + els_number * el_length + disp)
    {
      d_assert (allocated_length >= header_length + els_number * el_length);
      /* Set it before getting els.  */
      ER_set_disp (vect, 0);
      memmove (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect
	       ? (char *) ER_unpack_els (vect) : (char *) ER_pack_els (vect),
	       els, ER_els_number (vect) * el_length);
      
    }
  return vect;
}

/* Unpacking vector. */
ER_node_t
unpack_vector (ER_node_t vect)
{
  size_t allocated_length;
  int immutable;
  size_t els_number;
  ER_node_t prev_vect;
  size_t pack_vect_allocated_length;
  size_t el_size;
  ER_node_mode_t el_type;
  size_t displ;
  size_t i, disp;
  val_t temp_var;
  const char *pack_els;
  
  disp = ER_disp (vect);
  pack_vect_allocated_length = allocated_length = ER_allocated_length (vect);
  immutable = ER_immutable (vect);
  els_number = ER_els_number (vect);
  pack_els =  ER_pack_els (vect);
  el_type = ER_pack_vect_el_mode (vect);
  el_size = type_size_table [el_type];
  prev_vect = vect;
  if (allocated_length - disp < (ALLOC_SIZE (sizeof (_ER_heap_unpack_vect))
				 + els_number * sizeof (val_t)))
    {
      allocated_length = (ALLOC_SIZE (sizeof (_ER_heap_unpack_vect))
			  + OPTIMAL_ELS_NUM (els_number) * sizeof (val_t));
      vect = heap_allocate (allocated_length, FALSE);
    }
  ER_SET_MODE (vect, ER_NM_heap_unpack_vect);
  ER_set_disp (vect, 0);
  ER_set_allocated_length (vect, allocated_length);
  ER_set_immutable (vect, immutable);
  ER_set_els_number (vect, els_number);
  ER_SET_MODE ((ER_node_t) &temp_var, el_type == ER_NM_byte ? ER_NM_char : el_type);
  displ = val_displ ((ER_node_t) &temp_var);
  if (els_number != 0)
    {
      ER_node_t els;

      els = ER_unpack_els (vect);
      for (i = els_number - 1;; i--)
	{
	  if (el_type == ER_NM_byte)
	    {
	      byte_t b = ((byte_t *) pack_els)[i];
	      
	      ER_SET_MODE (IVAL (els, i), ER_NM_char);
	      ER_set_ch (IVAL (els, i), b);
	    }
	  else
	    {
	      /* Use this order with essting mode!!!  It is important
		 when we have only one element. */
	      memcpy ((char *) IVAL (els, i) + displ, pack_els + i * el_size,
		      el_size);
	      ER_SET_MODE (IVAL (els, i), el_type);
	      if (el_type == ER_NM_vect)
		ER_set_dim (IVAL (els, i), 0);
	    }
	  if (i == 0)
	    break;
	}
    }
  if (prev_vect != vect)
    {
      ER_SET_MODE (prev_vect, ER_NM_heap_redir);
      ER_set_allocated_length (prev_vect, pack_vect_allocated_length);
      ER_set_redir (prev_vect, vect);
      ER_set_unique_number (vect, ER_unique_number (prev_vect));
    }
  return vect;
}

static void
pack_vector_if_possible (ER_node_t unpack_vect)
{
  ER_node_t pack_vect;
  size_t i;
  size_t allocated_length;
  int immutable;
  size_t els_number;
  char *els;
  ER_node_mode_t el_type;
  size_t el_size, displ;

  immutable = ER_immutable (unpack_vect);
  els_number = ER_els_number (unpack_vect);
  el_type = (els_number != 0
	     ? ER_NODE_MODE (IVAL (ER_unpack_els (unpack_vect), 0))
	     : ER_NM_nil);
  for (i = 0; i < els_number; i++)
    if (ER_NODE_MODE (IVAL (ER_unpack_els (unpack_vect), i)) != el_type)
      break;
  if (i >= els_number)
    {
      /* Pack it */
      el_size = type_size_table [el_type];
      allocated_length = ER_allocated_length (unpack_vect);
      pack_vect = unpack_vect;
      ER_SET_MODE (pack_vect, ER_NM_heap_pack_vect);
      els = ER_pack_els (pack_vect) - ER_disp (pack_vect);
      ER_SET_MODE (unpack_vect, ER_NM_heap_unpack_vect);
      displ = val_displ (IVAL (ER_unpack_els (unpack_vect), 0));
      for (i = 0; i < els_number; i++)
	memcpy (els + i * el_size,
		(char *) IVAL (ER_unpack_els (unpack_vect), i) + displ,
		el_size);
      if (el_type == ER_NM_char)
	((ucode_t *) els) [els_number] = '\0';
      ER_SET_MODE (pack_vect, ER_NM_heap_pack_vect);
      ER_set_immutable (pack_vect, immutable);
      ER_set_pack_vect_el_mode (pack_vect, el_type);
      ER_set_els_number (pack_vect, els_number);
      ER_set_disp (pack_vect, 0);
      ER_set_allocated_length (pack_vect, allocated_length);
    }
}

int
eq_vector (ER_node_t v1, ER_node_t v2)
{
  GO_THROUGH_REDIR (v1);
  GO_THROUGH_REDIR (v2);
  if (v1 == v2)
    return TRUE;
  if (ER_els_number (v1) != ER_els_number (v2))
    return FALSE;
  try_full_pack (v1);
  try_full_pack (v2);
  if (ER_NODE_MODE (v1) != ER_NODE_MODE (v2))
    return FALSE;
  else if (ER_NODE_MODE (v1) == ER_NM_heap_pack_vect)
    return (ER_pack_vect_el_mode (v1) == ER_pack_vect_el_mode (v2)
	    && memcmp (ER_pack_els (v1), ER_pack_els (v2),
		       ER_els_number (v1)
		       * type_size_table [ER_pack_vect_el_mode (v1)]) == 0);
  else
    return eq_val ((val_t *) ER_unpack_els (v1), (val_t *) ER_unpack_els (v2),
		   ER_els_number (v1));
}

/* The folllowing function make copy the vector vect.  The copy is
   always mutable. */
ER_node_t
copy_vector (ER_node_t vect)
{
  size_t vect_size;
  ER_node_t new_vect;
  
  GO_THROUGH_REDIR (vect);
  vect_size = ER_allocated_length (vect);
  new_vect = heap_allocate (vect_size, FALSE);
  memcpy (new_vect, vect, vect_size);
  ER_set_unique_number (new_vect, unique_number);
  unique_number++;
  ER_set_immutable (new_vect, FALSE);
  return new_vect;
}

ER_node_t
create_empty_string (size_t min_length)
{
  ER_node_t vect;
  
  vect = create_pack_vector (min_length, ER_NM_byte);
  ER_set_immutable (vect, TRUE);
  ER_set_els_number (vect, 0);
  ER_pack_els (vect) [0] = '\0';
  return vect;
}

/* Create and return packed byte vector from regular string STR.  */
ER_node_t
create_string (const char *string)
{
  ER_node_t vect;
  size_t chars_number;
  
  chars_number = strlen (string);
  vect = create_pack_vector (chars_number + 1, ER_NM_byte);
  ER_set_immutable (vect, TRUE);
  ER_set_els_number (vect, chars_number);
  strcpy (ER_pack_els (vect), string);
  return vect;
}


/* Create and return packed ucode vector from ucode string STR with
   trailing zero.  */
ER_node_t
create_ucodestr (const ucode_t *str)
{
  ER_node_t vect;
  size_t chars_number;
  
  chars_number = ucodestrlen (str);
  vect = create_pack_vector (chars_number + 1, ER_NM_char);
  ER_set_immutable (vect, TRUE);
  ER_set_els_number (vect, chars_number);
  memcpy (ER_pack_els (vect), str, (chars_number + 1) * sizeof (ucode_t));
  return vect;
}

/* Transform byte packed vector VECT into ucode packed vector.  */
ER_node_t
bytevect_to_ucodevect (ER_node_t vect)
{
  size_t allocated_length;
  int immutable;
  size_t els_number;
  ER_node_t prev_vect;
  size_t prev_vect_allocated_length;
  size_t i, disp;
  ucode_t *els;
  byte_t *prev_els;
  
  d_assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
	    && ER_pack_vect_el_mode (vect) == ER_NM_byte);
  disp = ER_disp (vect);
  prev_vect_allocated_length = allocated_length = ER_allocated_length (vect);
  immutable = ER_immutable (vect);
  els_number = ER_els_number (vect);
  prev_els = (byte_t *) ER_pack_els (vect);
  prev_vect = vect;
  if (allocated_length - disp < (ALLOC_SIZE (sizeof (_ER_heap_pack_vect))
				 + (els_number + 1) * sizeof (ucode_t)))
    {
      allocated_length = (ALLOC_SIZE (sizeof (_ER_heap_pack_vect))
			  + OPTIMAL_ELS_NUM (els_number)
			  * sizeof (ucode_t));
      vect = heap_allocate (allocated_length, FALSE);
      ER_SET_MODE (vect, ER_NM_heap_pack_vect);
    }
  ER_set_disp (vect, 0);
  els = (ucode_t *) ER_pack_els (vect);
  if (els_number != 0)
    for (i = els_number - 1;; i--)
      {
	els[i] = prev_els[i];
	if (i == 0)
	  break;
      }
  els[els_number] = '\0';
  ER_set_pack_vect_el_mode (vect, ER_NM_char);
  if (prev_vect != vect)
    {
      ER_set_allocated_length (vect, allocated_length);
      ER_set_immutable (vect, immutable);
      ER_set_els_number (vect, els_number);
      ER_SET_MODE (prev_vect, ER_NM_heap_redir);
      ER_set_allocated_length (prev_vect, prev_vect_allocated_length);
      ER_set_redir (prev_vect, vect);
      ER_set_unique_number (vect, ER_unique_number (prev_vect));
    }
  return vect;
}

/* Transform ucode packed vector VECT into byte packed vector if
   possible.  */
static void
ucodevect_to_bytevect_if_possible (ER_node_t vect)
{
  size_t i, els_num, el_size;
  const ucode_t *char_els;
  byte_t *els;
  
  d_assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
	    && ER_pack_vect_el_mode (vect) == ER_NM_char);
  els_num = ER_els_number (vect);
  char_els = (ucode_t *) ER_pack_els (vect);
  for (i = 0; i < els_num; i++)
    if (! in_byte_range_p (char_els[i]))
      return;
  el_size = type_size_table [ER_NM_byte];
  els = (byte_t *) ER_pack_els (vect) - ER_disp (vect);
  for (i = 0; i < els_num; i++)
    els [i] = char_els [i];
  els [els_num] = '\0';
  ER_set_pack_vect_el_mode (vect, ER_NM_byte);
  ER_set_disp (vect, 0);
}

/* Transform OBJ to packed byte vector if possible, otherwise to
   packed vector.  */
void
try_full_pack (ER_node_t obj)
{
  if (ER_NODE_MODE (obj) == ER_NM_heap_unpack_vect)
    pack_vector_if_possible (obj);
  if (ER_NODE_MODE (obj) == ER_NM_heap_pack_vect
      && ER_pack_vect_el_mode (obj) == ER_NM_char)
    ucodevect_to_bytevect_if_possible (obj);
}



int do_always_inline
eq_stack (ER_node_t i1, ER_node_t i2)
{
  return (i1 == i2
	  || (ER_block_node (i1) == ER_block_node (i2)
	      && ER_context (i1) == ER_context (i2) /* !!! */
	      && eq_val ((val_t *) ER_stack_vars (i1),
			 (val_t *) ER_stack_vars (i2),
			 (val_t *) ER_ctop (i1) - (val_t *) ER_stack_vars (i1) + 1)));
}



/* The page contains functions for work with tables. */

unsigned int tab_collisions;
unsigned int tab_expansions;

void
initiate_tables (void)
{
  tab_collisions = 0;
  tab_expansions = 0;
}

int
eq_table (ER_node_t t1, ER_node_t t2)
{
  size_t i;
  ER_node_t key;

  GO_THROUGH_REDIR (t1);
  GO_THROUGH_REDIR (t2);
  if (t1 == t2)
    return TRUE;
  if (ER_els_number (t1) != ER_els_number (t2))
    return FALSE;
  for (i = 0; i < ER_els_bound (t1); i++)
    {
      if (ER_NODE_MODE (INDEXED_EL_KEY (ER_tab_els (t1), i)) != ER_NM_empty_el)
	{
	  key = find_tab_el (t2, INDEXED_EL_KEY (ER_tab_els (t1), i), FALSE);
	  if (key == NULL || ER_NODE_MODE (key) == ER_NM_empty_el
	      || ! eq_val ((val_t *) ((char *) key + sizeof (val_t)),
			   (val_t *) INDEXED_EL_VAL (ER_tab_els (t1), i), 1))
	    return FALSE;
	}
    }
  return TRUE;
}



/* Murmur Hash is described on http://murmurhash.googlepages.com.  */
#if SIZEOF_SIZE_T == 8
static const size_t MURMUR_MAGIC = 0xc6a4a7935bd1e995;
static const int MURMUR_SHIFT = 47;
#elif SIZEOF_SIZE_T == 4
static const size_t MURMUR_MAGIC = 0x5bd1e995;
static const int MURMUR_SHIFT = 24;
#else
#error size_t should be 32- or 64-bit
#endif

static const unsigned int MURMUR_SEED = 0x811c9dc5;

static size_t do_always_inline
murmur_init (size_t len)
{
  return MURMUR_SEED ^ (len * MURMUR_MAGIC);
}

static size_t do_always_inline
murmur_step (size_t h, size_t val)
{
  val *= MURMUR_MAGIC; 
  val ^= val >> MURMUR_SHIFT; 
  val *= MURMUR_MAGIC; 
  h ^= val;
  return h * MURMUR_MAGIC;
}

static size_t do_always_inline
murmur_finish (size_t h)
{
  h ^= h >> MURMUR_SHIFT;
  h *= MURMUR_MAGIC;
  h ^= h >> MURMUR_SHIFT;
  return h;
}

static size_t
murmur (const void *key, size_t len)
{
  size_t h = murmur_init (len);
  const unsigned char *data = (const unsigned char *) key;
  
  while (len >= sizeof (size_t))
    {
      size_t k;

      /* It is slower but less hassle with portability.  */
      k  = data[0];
      k |= data[1] << 8;
      k |= data[2] << 16;
      k |= data[3] << 24;
#if SIZEOF_SIZE_T == 8
      k |= ((size_t) data[4]) << 32;
      k |= ((size_t) data[5]) << 40;
      k |= ((size_t) data[6]) << 48;
      k |= ((size_t) data[7]) << 56;
#endif
      h = murmur_step (h, k);
      len -= sizeof (size_t);
      data += sizeof (size_t);
    }
  
  switch (len)
    {
#if SIZEOF_SIZE_T == 8
    case 7:
      h ^= ((size_t) data[6]) << 48;
    case 6:
      h ^= ((size_t) data[5]) << 40;
    case 5:
      h ^= ((size_t) data[4]) << 32;
    case 4:
      h ^= ((size_t) data[3]) << 24;
#endif
    case 3:
      h ^= ((size_t) data[2]) << 16;
    case 2:
      h ^= ((size_t) data[1]) << 8;
    case 1:
      h ^= ((size_t) data[0]);
      h *= MURMUR_MAGIC;
    }
 
  h = murmur_finish (h);
  
  return h;
}



/* This page contains code for DINO hash tables.  */

static size_t do_always_inline
hash_ref (ER_node_t ref)
{
  if (ref == NULL)
    return 0;
  return ER_unique_number (ref);
}

/* Return hash value of integer I.  */
static size_t do_always_inline
int_hash_val (rint_t i)
{
  return (size_t) i;
}

/* Return hash value of VAL.  */
static size_t
hash_val (ER_node_t val)
{
  size_t hash;
  size_t i;
  size_t length;
  unsigned char *string;

  d_assert (val != NULL);
  switch (ER_NODE_MODE (val))
    {
    case ER_NM_nil:
      return 0;
    case ER_NM_char:
      /* See also special case (string) in eq_key.  */
      return (size_t) ER_ch (val);
    case ER_NM_int:
      return int_hash_val (ER_i (val));
    case ER_NM_float:
      {
	rfloat_t f;
	
	length = sizeof (rfloat_t);
	f = ER_f (val);
	string = (unsigned char *) &f;
	hash = murmur (string, length);
	return hash;
      }
    case ER_NM_type:
      return (size_t) ER_type (val);
    case ER_NM_hide:
      return (size_t) ER_hide (val);
    case ER_NM_long:
      return hash_mpz (*ER_mpz_ptr (ER_l (val)));
    case ER_NM_hideblock:
      return (size_t) ER_unique_number (ER_hideblock (val));
    case ER_NM_vect:
      return (size_t) ER_unique_number (ER_vect (val));
    case ER_NM_tab:
      return (size_t) ER_unique_number (ER_tab (val));
    case ER_NM_code:
      return murmur_finish (murmur_step
                            (murmur_step (murmur_init (2), (size_t) ER_code_id (val)),
			     (size_t) ER_unique_number (ER_code_context (val))));
    case ER_NM_efun:
      return (size_t) ER_efdecl (val);
    case ER_NM_thread:
      return (size_t) ER_unique_number (ER_thread (val));
    case ER_NM_stack:
      return (size_t) ER_unique_number (ER_stack (val));
    default:
      d_unreachable ();
    }
}

static size_t do_always_inline
hash_key (ER_node_t key)
{
  size_t hash, el_hash;
  size_t i;
  size_t length;
  unsigned char *string;
  int shift;

  switch (ER_NODE_MODE (key))
    {
    case ER_NM_nil:
    case ER_NM_char:
    case ER_NM_int:
    case ER_NM_long:
    case ER_NM_float:
    case ER_NM_type:
    case ER_NM_hide:
    case ER_NM_code:
    case ER_NM_efun:
    case ER_NM_thread:
      hash = hash_val (key);
      break;
    case ER_NM_vect:
      {
	ER_node_t vect;

	vect = ER_vect (key);
	GO_THROUGH_REDIR (vect);
	ER_set_vect (key, vect);
	try_full_pack (ER_vect (key));
 	if (ER_NODE_MODE (ER_vect (key)) == ER_NM_heap_pack_vect)
	  {
	    ER_node_t pv = ER_vect (key);

	    hash = murmur (ER_pack_els (pv),
			   ER_els_number (pv) 
			   * type_size_table [ER_pack_vect_el_mode (pv)]);
	  }
	else
	  {
	    ER_node_t unpv = ER_vect (key);
	    
	    hash = murmur_init (ER_els_number (unpv));
	    for (i = 0; i < ER_els_number (unpv); i++)
	      {
		el_hash = hash_val (IVAL (ER_unpack_els (unpv), i));
		hash = murmur_step (hash, el_hash);
	      }
	    hash = murmur_finish (hash);
	  }
	break;
      }
    case ER_NM_tab:
      {
	ER_node_t tab;
	ER_node_t el_key;

	tab = ER_tab (key);
	GO_THROUGH_REDIR (tab);
	ER_set_tab (key, tab);
	hash = murmur_init (ER_entries_number (tab));
	for (i = 0; i < ER_els_bound (tab); i++)
	  {
	    el_key = INDEXED_EL_KEY (ER_tab_els (tab), i);
	    if (ER_NODE_MODE (el_key) == ER_NM_empty_el)
	      continue;
	    hash = murmur_step (hash, hash_val (el_key));
	    hash = murmur_step (hash, hash_val (IVAL (el_key, 1)));
	  }
	hash = murmur_finish (hash);
	break;
      }
    case ER_NM_stack:
      {
	ER_node_t var;

	hash = murmur_init ((char *) ER_ctop (ER_stack (key))
			    - (char *) ER_stack_vars (ER_stack (key)));
	for (i = 0, var = ER_stack_vars (ER_stack (key));
	     (char *) var <= ER_ctop (ER_stack (key));
	     var = IVAL (var, 1), i++)
	  {
	    el_hash = hash_val (var);
	    hash = murmur_step (hash, el_hash);
	  }
	hash = murmur_finish (hash);
	break;
      }
    case ER_NM_hideblock:
      string = (unsigned char *) ER_hideblock_start (ER_hideblock (key));
      length = ER_hideblock_length (ER_hideblock (key));
      hash = murmur (string, length);
      break;
    default:
      d_unreachable ();
    }
  return hash;
}

static int do_inline
eq_key (ER_node_t entry_key, ER_node_t key)
{
  if (ER_NODE_MODE (entry_key) != ER_NODE_MODE (key))
    return FALSE;
  switch (ER_NODE_MODE (key))
    {
    case ER_NM_nil:
      return TRUE;
    case ER_NM_char:
      return ER_ch (key) == ER_ch (entry_key);
    case ER_NM_int:
      return ER_i (key) == ER_i (entry_key);
    case ER_NM_long:
      return mpz_cmp (*ER_mpz_ptr (ER_l (key)),
		      *ER_mpz_ptr (ER_l (entry_key))) == 0;
    case ER_NM_float:
      return ER_f (key) == ER_f (entry_key);
    case ER_NM_type:
      return ER_type (key) == ER_type (entry_key);
    case ER_NM_vect:
      return eq_vector (ER_vect (key), ER_vect (entry_key));
    case ER_NM_tab:
      return eq_table (ER_tab (key), ER_tab (entry_key));
    case ER_NM_code:
      return (ER_code_context (key) == ER_code_context (entry_key)
	      && ER_code_id (key) == ER_code_id (entry_key));
    case ER_NM_efun:
      return ER_efdecl (key) == ER_efdecl (entry_key);
    case ER_NM_thread:
      return ER_thread (key) == ER_thread (entry_key);
    case ER_NM_stack:
      return eq_stack (ER_stack (key), ER_stack (entry_key));
    default:
      d_unreachable ();
    }
  return FALSE; /* No warnings */
}

/* Initial table occupancy factor.  It means that at first we have 3
   times more entries than number of elements.  Maximum occupancy will
   be 2/3.  See function create_tab.  */
#define OCCUPANCY_FACTOR 3

/* Initial number of table entries for expected N elements.  */
#define START_ENTRIES_NUMBER(n) \
 (OCCUPANCY_FACTOR * (n) < 300 ? 300 : OCCUPANCY_FACTOR * (n))

/* The following function returns the nearest prime number which is
   greater than given source number. */
unsigned long
higher_prime_number (unsigned long number)
{
  unsigned long i;

  for (number = (number / 2) * 2 + 3;; number += 2)
    {
      for (i = 3; i * i <= number; i += 2)
        if (number % i == 0)
          break;
      if (i * i > number)
        return number;
    }
}

/* This function creates table with length slightly longer than given
   source length.  Created hash table is initiated as empty.  The
   function returns the created hash table.  */
ER_node_t
create_tab (size_t size)
{
  size_t entries_number, max_els_number;
  size_t els_size, allocated_length;
  ER_node_t result, tab_els, el_key;
  entry_ptr_t entries;
  size_t i;

  entries_number = higher_prime_number (START_ENTRIES_NUMBER (size));
  max_els_number = (entries_number / OCCUPANCY_FACTOR) * 2 + 1;
  if (max_els_number > MAX_TAB_ELS_NUMBER)
    max_els_number = MAX_TAB_ELS_NUMBER;
  els_size = 2 * max_els_number * sizeof (val_t);
  allocated_length = (ALLOC_SIZE (sizeof (_ER_heap_tab))
		      + els_size + entries_number * sizeof (entry_t));
  result = (ER_node_t) heap_allocate (allocated_length, FALSE);
  ER_SET_MODE (result, ER_NM_heap_tab);
  ER_set_immutable (result, FALSE);
  ER_set_els_number (result, 0);
  ER_set_allocated_length (result, allocated_length);
  ER_set_entries_number (result, entries_number);
  ER_set_deleted_els_number (result, 0);
  ER_set_els_bound (result, 0);
  ER_set_els_space_bound (result, max_els_number);
  ER_set_last_accessed_el_index (result, EMPTY_ENTRY);
  entries = ER_tab_entries (result);
  for (i = 0; i < ER_entries_number (result); i++)
    entries[i] = EMPTY_ENTRY;
  return result;
}

static ER_node_t tailor_tab (ER_node_t tab);

/* Return index of the first empty elem or EMPTY_ENTRY if there is no
   such element.  */
static entry_t do_always_inline
first_empty_elem (ER_node_t tab)
{
  entry_t els_bound = ER_els_bound (tab);
  
  if (els_bound >= ER_els_space_bound (tab))
    return EMPTY_ENTRY;
  ER_SET_MODE (INDEXED_EL_KEY (ER_tab_els (tab), els_bound), ER_NM_empty_el);
  return els_bound;
}

/* Return index of empty elem or EMPTY_ENTRY if there is no such
   element.  Reserve the element.  */
static entry_t do_always_inline
get_empty_elem_index (ER_node_t tab)
{
  entry_t result = first_empty_elem (tab);

  if (result != EMPTY_ENTRY)
    ER_set_els_bound (tab, ER_els_bound (tab) + 1);
  return result;
}

/* Find entry of TAB for elements with KEY.  Return the entry number.
   If the element is not found, return number of entry when it will
   be placed if RESERVE.  Otherwise just return the first found empty
   entry.  The table can be tailored in reservation mode, if there is
   no elements which can be be reserved.  */
static entry_t
find_tab_entry_num (ER_node_t tab, ER_node_t key, int reserve)
{
  ER_node_t el_key;
  entry_ptr_t entries;
  entry_t entry_num, entry_step, first_deleted_entry_index;
  size_t hash_value;
  entry_t el_index;
  int int_p;

  d_assert (ER_NODE_MODE (tab) == ER_NM_heap_tab);
  int_p = ER_NODE_MODE (key) == ER_NM_int;
  if (reserve && ER_els_bound (tab) >= ER_els_space_bound (tab))
    tab = tailor_tab (tab);
  d_assert (! reserve || first_empty_elem (tab) != EMPTY_ENTRY);
  hash_value = (ER_NODE_MODE (key) == ER_NM_int
		? int_hash_val (ER_i (key)) : hash_key (key));
  entry_step = 1 + hash_value % (ER_entries_number (tab) - 2);
  entry_num = hash_value % ER_entries_number (tab);
  first_deleted_entry_index = EMPTY_ENTRY;
  entries = ER_tab_entries (tab);
  for (;;)
    {
      el_index = entries [entry_num];
      if (el_index == EMPTY_ENTRY)
        {
          if (reserve)
	    {
	      ER_set_els_number (tab, ER_els_number (tab) + 1);
	      el_index = get_empty_elem_index (tab);
	      if (first_deleted_entry_index == EMPTY_ENTRY)
		entries[entry_num] = el_index;
	      else
		{
		  d_assert (ER_deleted_els_number (tab) > 0);
		  ER_set_deleted_els_number
		    (tab, ER_deleted_els_number (tab) - 1);
		  entries[first_deleted_entry_index] = el_index;
		  entry_num = first_deleted_entry_index;
		}
	    }
          break;
        }
      else if (el_index != DELETED_ENTRY)
        {
	  el_key = INDEXED_EL_KEY (ER_tab_els (tab), el_index);
          if (int_p && ER_NODE_MODE (el_key) == ER_NM_int
	      ? ER_i (el_key) == ER_i (key) : eq_key (el_key, key))
	    break;
        }
      else if (first_deleted_entry_index == EMPTY_ENTRY)
	first_deleted_entry_index = entry_num;
      entry_num += entry_step;
      if (entry_num >= ER_entries_number (tab))
        entry_num -= ER_entries_number (tab);
      tab_collisions++;
    }
  return entry_num;
}

/* This function searches for table entry which contains key equal to
   given key or empty entry in which given value can be placed.  The
   function works in two modes.  The first mode is used only for
   search.  In this mode the function returns NULL if the element is
   not found.  The second mode is used for search and reservation
   empty entry for given value (the value with given key should be
   inserted into the table entry before another call of the function).
   In this mode the function never returns NULL.  The table can be
   tailored in reservation mode.  Attention: this means that the
   place of the table may change after any function call with
   reservation. */
ER_node_t
find_tab_el (ER_node_t tab, ER_node_t key, int reserve)
{
  ER_node_t el_key;
  entry_t entry_num, el_index;
  entry_t last_accessed_el_index;
  int int_p;

  d_assert (ER_NODE_MODE (tab) == ER_NM_heap_tab);
  int_p = ER_NODE_MODE (key) == ER_NM_int;
  if ((last_accessed_el_index = ER_last_accessed_el_index (tab)) != EMPTY_ENTRY)
    {
      el_key = INDEXED_EL_KEY (ER_tab_els (tab), last_accessed_el_index);
      if ((int_p && ER_NODE_MODE (el_key) == ER_NM_int
	   && ER_i (el_key) == ER_i (key))
	  || (! int_p && eq_key (el_key, key)))
	return el_key;
    }
  entry_num = find_tab_entry_num (tab, key, reserve);
  /* Tab can be tailored in find_tab_entry_num.  */
  GO_THROUGH_REDIR (tab);
  el_index = ER_tab_entries (tab) [entry_num];
  if (el_index == EMPTY_ENTRY)
    return NULL;
  el_key = INDEXED_EL_KEY (ER_tab_els (tab), el_index);
  if (ER_NODE_MODE (el_key) != ER_NM_empty_el)
    ER_set_last_accessed_el_index (tab, el_index);
  return el_key;
}


/* The following function make copy the table TAB.  The copy is
   always mutable. */
ER_node_t
copy_tab (ER_node_t tab)
{
  ER_node_t new_tab;
  ER_node_t tab_els, new_el;
  size_t i;

  new_tab = create_tab (ER_els_number (tab));
  tab_els = ER_tab_els (tab);
  for (i = 0; i < ER_els_bound (tab); i++)
    if (ER_NODE_MODE (INDEXED_EL_KEY (tab_els, i)) != ER_NM_empty_el)
      {
        new_el = find_tab_el (new_tab, INDEXED_EL_KEY (tab_els, i), TRUE);
        d_assert (ER_NODE_MODE (new_tab) != ER_NM_heap_redir
		  && new_el != NULL && ER_NODE_MODE (new_el) == ER_NM_empty_el);
        *(val_t *) new_el = *(val_t *) INDEXED_EL_KEY (tab_els, i);
        ((val_t *) new_el) [1] = *(val_t *) INDEXED_EL_VAL (tab_els, i);
	d_assert (ER_IS_OF_TYPE (INDEXED_EL_VAL (tab_els, i), ER_NM_val));
	d_assert
	  (ER_IS_OF_TYPE ((ER_node_t) &((val_t *) new_el) [1], ER_NM_val));
      }
  return new_tab;
}

/* The following function changes expand given table by creating new
   table and inserting the table elements.  The occupancy of the table
   after the call will be about 33%.  Remember also that the place in
   the table of the table entries (identified by keys) are changed. */
static ER_node_t
tailor_tab (ER_node_t tab)
{
  ER_node_t new_tab;
  int immutable;
  size_t allocated_length;
  size_t i;

  immutable = ER_immutable (tab);
  tab_expansions++;
  new_tab = copy_tab (tab);
  ER_set_immutable (new_tab, immutable);
  allocated_length = ER_allocated_length (tab);
  ER_SET_MODE (tab, ER_NM_heap_redir);
  ER_set_allocated_length (tab, allocated_length);
  ER_set_redir (tab, new_tab);
  ER_set_unique_number (new_tab, ER_unique_number (tab));
  ER_set_last_accessed_el_index (new_tab, EMPTY_ENTRY);
  return new_tab;
}

/* This function deletes element with given value from table.  The
   table entry value will be `ER_NM_deleted_entry' after the function
   call.  The function returns TRUE if the element has been removed.
   Otherwise, it returns FALSE.  That means that such element does not
   exist. */
int
remove_tab_el (ER_node_t tab, ER_node_t key)
{
  ER_node_t el_key;
  entry_ptr_t entries = ER_tab_entries (tab);
  entry_t entry_num = find_tab_entry_num (tab, key, FALSE);
  entry_t el_index = entries [entry_num];

  if (el_index == EMPTY_ENTRY)
    return FALSE;
  d_assert (el_index != DELETED_ENTRY);
  entries [entry_num] = DELETED_ENTRY;
  el_key = INDEXED_EL_KEY (ER_tab_els (tab), el_index);
  ER_SET_MODE (el_key, ER_NM_empty_el);
  d_assert (ER_els_number (tab) > 0);
  ER_set_els_number (tab, ER_els_number (tab) - 1);
  ER_set_deleted_els_number (tab, ER_deleted_els_number (tab) + 1);
  ER_set_last_accessed_el_index (tab, EMPTY_ENTRY);
  return TRUE;
}

/* The function returns the next key in TAB starting with START.  It
   returns NULL if there is no anymore.  */
ER_node_t
find_next_key (ER_node_t tab, int start)
{
  size_t i;
  ER_node_t key;

  for (i = start; i < ER_els_bound (tab); i++)
    {
      key = INDEXED_EL_KEY (ER_tab_els (tab), i);
      if (ER_NODE_MODE (key) != ER_NM_empty_el)
	{
	  ER_set_last_accessed_el_index (tab, i);
	  return key;
	}
    }
  return NULL;
}

ER_node_t
table_to_vector_conversion (ER_node_t tab)
{
  size_t i, index;
  ER_node_t vect, key;

  GO_THROUGH_REDIR (tab);
  index = 0;
  if (ER_els_number (tab) == 0)
    vect = create_empty_vector ();
  else
    vect = create_unpack_vector (ER_els_number (tab) * 2);
  for (i = 0; i < ER_els_bound (tab); i++)
    {
      key = INDEXED_EL_KEY (ER_tab_els (tab), i);
      if (ER_NODE_MODE (key) == ER_NM_empty_el)
	continue;
      *(val_t *) IVAL (ER_unpack_els (vect), index) = *(val_t *) key;
      *(val_t *) IVAL (ER_unpack_els (vect), index + 1)
	= *(val_t *) INDEXED_EL_VAL (ER_tab_els (tab), i);
      index += 2;
    }
  return vect;
}

ER_node_t
vector_to_table_conversion (ER_node_t vect)
{
  size_t i;
  ER_node_t el;
  val_t val;
  ER_node_t tab, tvar = (ER_node_t) &val;

  GO_THROUGH_REDIR (vect);
  tab = create_tab (ER_els_number (vect));
  for (i = 0; i < ER_els_number (vect); i++)
    {
      ER_SET_MODE ((ER_node_t) tvar, ER_NM_int);
      ER_set_i (tvar, i);
      el = find_tab_el (tab, tvar, TRUE);
      d_assert (el != NULL);
      *(val_t *) el = *(val_t *) tvar;
      /* Integer is always immutable. */
      if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
	*((val_t *) el + 1) = *(val_t *) IVAL (ER_unpack_els (vect), i);
      else
	{
	  ER_node_t v = (ER_node_t) ((val_t *) el + 1);
	  ER_node_mode_t el_type = ER_pack_vect_el_mode (vect);
	  size_t el_type_size = type_size_table [el_type];

	  if (el_type == ER_NM_byte)
	    {
	      ER_SET_MODE (v, ER_NM_char);
	      ER_set_ch (v, ((byte_t *) ER_pack_els (vect)) [i]);
	    }
	  else
	    {
	      ER_SET_MODE (v, el_type);
	      memcpy ((char *) v + val_displ (v),
		      ER_pack_els (vect) + i * el_type_size, el_type_size);
	      if (el_type == ER_NM_vect)
		ER_set_dim (v, 0);
	    }
	}
    }
  return tab;
}



void
make_immutable (ER_node_t obj)
{
  if (ER_NODE_MODE (obj) == ER_NM_vect)
    {
      ER_node_t vect;

      vect = ER_vect (obj);
      GO_THROUGH_REDIR (vect);
      ER_set_immutable (vect, TRUE);
    }
  else if (ER_NODE_MODE (obj) == ER_NM_tab)
    {
      ER_node_t tab;

      tab = ER_tab (obj);
      GO_THROUGH_REDIR (tab);
      ER_set_immutable (tab, TRUE);
    }
  else if (ER_NODE_MODE (obj) == ER_NM_stack)
    ER_set_immutable (ER_stack (obj), TRUE);
}



ER_node_t
create_thread (pc_t start_thread_pc, BC_node_t block, ER_node_t fun_context)
{
  ER_node_t thread;

  thread = (ER_node_t) heap_allocate (ALLOC_SIZE (sizeof (_ER_heap_thread)),
				      FALSE);
  ER_SET_MODE (thread, ER_NM_heap_thread);
  ER_set_context (thread, fun_context);
  ER_set_context_number (thread, context_number);
  context_number++;
  ER_set_thread_block (thread, block);
  ER_set_thread_status (thread, PS_READY);
  ER_set_thread_number (thread, thread_number);
  thread_number++;
  ER_set_saved_pc (thread, start_thread_pc);
  ER_set_saved_cstack (thread, cstack);
  ER_set_father (thread, cthread);
  if (cthread == NULL)
    {
      ER_set_prev (thread, thread);
      ER_set_next (thread, thread);
    }
  else
    {
      ER_set_prev (thread, ER_prev (cthread));
      ER_set_next (ER_prev (thread), thread);
      ER_set_prev (cthread, thread);
      ER_set_next (thread, cthread);
    }
  return thread;
}

static void
activate_given_thread (ER_node_t thread, int first_p)
{
  ER_node_t var;

#if ! defined (NO_PROFILE) && !HAVE_SETITIMER
  if (cthread != thread && profile_flag)
    {
      BC_node_t block;

      for (block = ER_block_node (ER_saved_cstack (cthread));
	   BC_ident (block) == NULL;
	   block = BC_block_scope (block))
	;
      ticker_off (&BC_exec_time (block));
      for (block = ER_block_node (ER_saved_cstack (thread));
	   BC_ident (block) == NULL;
	   block = BC_block_scope (block))
	;
      if (BC_calls_number (block) == 0)
	abort ();
      ticker_on (&BC_exec_time (block));
    }
#endif
  cthread = thread;
  cpc = ER_saved_pc (cthread);
  if (cstack != NULL)
    ER_set_ctop (cstack, (char *) ctop);
  cstack = ER_saved_cstack (cthread);
  cvars = ER_stack_vars (cstack);
  ctop = (ER_node_t) ER_ctop (cstack);
#ifndef NO_CONTAINER_CACHE
  current_cached_container_tick++;
#endif
  ER_set_thread_status (cthread, PS_READY);
  if (! first_p)
    {
      var = IVAL (ER_stack_vars (get_obj_stack (lang_bc_decl)),
		  BC_var_num (curr_thread_bc_decl));
      ER_SET_MODE (var, ER_NM_thread);
      ER_set_thread (var, cthread);
    }
}

/* It is important for block_cthread that the search of thread for
   activation starts with thread after cthread. */
static void
activate_thread (int first_p)
{
  activate_given_thread (ER_next (cthread), first_p);
  if (first_thread_not_started == cthread)
    eval_error (deadlock_bc_decl, get_cpos (), DERR_deadlock);
  executed_stmts_count = -thread_quantum; /* start new quantum */
}

void
block_cthread (pc_t first_resume_pc, int wait_stmt_flag)
{
  d_assert (cthread != NULL);
  if (sync_flag)
    {
      executed_stmts_count = -thread_quantum; /* start new quantum */
      return;
    }
  ER_set_saved_pc (cthread, first_resume_pc);
  ER_set_saved_cstack (cthread, cstack);
  if (executed_stmts_count != -thread_quantum)
    ER_set_thread_status (cthread, PS_STARTED);
  if (!wait_stmt_flag)
    {
      d_assert (ER_thread_status (cthread) == PS_STARTED);
      ER_set_thread_status (cthread, PS_BLOCKED_BY_QUANTUM_SWITCH);
      first_thread_not_started = NULL;
    }
  else
    {
      if (ER_thread_status (cthread) == PS_READY)
	{
	  ER_set_thread_status (cthread, PS_NOT_STARTED);
	  if (first_thread_not_started == NULL)
	    first_thread_not_started = cthread;
	}
      else
	{
	  d_assert (ER_thread_status (cthread) == PS_STARTED);
	  ER_set_thread_status (cthread, PS_BLOCKED_BY_WAIT);
	  first_thread_not_started = NULL;
	}
    }
  activate_thread (FALSE);
}

void
delete_cthread (void)
{
  first_thread_not_started = NULL;
  if (ER_next (cthread) == cthread)
    dino_finish (0);
  else
    {
      ER_set_prev (ER_next (cthread), ER_prev (cthread));
      ER_set_next (ER_prev (cthread), ER_next (cthread));
      cthread = ER_prev (cthread);
      activate_thread (FALSE);
    }
}

/* Delete current thread because of an exception. */
int
delete_cthread_during_exception (void)
{
  if (ER_next (cthread) == cthread || ER_father (cthread) == NULL)
    return FALSE;
  else
    {
      first_thread_not_started = NULL;
      ER_set_prev (ER_next (cthread), ER_prev (cthread));
      ER_set_next (ER_prev (cthread), ER_next (cthread));
      activate_given_thread (ER_father (cthread), FALSE);
      return TRUE;
    }
}

/* The following variables for switching thread to prevent
   starvation.  The first one is approximate number of executed stmts
   after starting the thread minus thread quantum.  The second one is
   maximal number of stmts executed in a thread without switching. */
int executed_stmts_count;
int thread_quantum;

void 
initiate_threads (pc_t start_pc)
{
  cthread = NULL;
  thread_number = 0;
  thread_quantum = 700;
  executed_stmts_count = -thread_quantum;
  cthread = create_thread (start_pc, NULL, NULL);
  first_thread_not_started = NULL;
  activate_thread (TRUE);
}



/* Interrupt because of quantum expiring or GC is needed.  */
void
interrupt (pc_t first_resume_pc)
{
  if (GC_executed_stmts_count > 0)
    block_cthread (first_resume_pc, FALSE);
  else
    {
      /* It is actually interrupt for GC.  */ 
      executed_stmts_count = GC_executed_stmts_count;
      GC ();
    }
}



void
store_handle (void *handle)
{
  VLO_ADD_MEMORY (dl_handle_vlo, &handle, sizeof (handle));
}
  
void *
external_address (BC_node_t decl)
{
  void *address = NULL;
  const char *name;
  const char **curr_libname_ptr;
  int i;

  name = BC_ident (decl);
  if (BC_address (decl) != NULL)
    address = (external_fun_t *) BC_address (decl);
  else
    {
      void **dl_handle_ptr;
      
      for (dl_handle_ptr = (void **) VLO_BEGIN (dl_handle_vlo);
	   dl_handle_ptr < (void **) VLO_BOUND (dl_handle_vlo);
	   dl_handle_ptr++)
	{
	  address = dlsym (*dl_handle_ptr, name);
	  if (dlerror () == NULL)
	    break;
	}
      if (dl_handle_ptr >= (void **) VLO_BOUND (dl_handle_vlo))
	{
	  (void) dlopen (NULL, RTLD_NOW);
	  for (curr_libname_ptr = libraries;
	       *curr_libname_ptr != NULL;
	       curr_libname_ptr++)
	    {
	      void *handle;
	      FILE *f;

	      /* Dlopen on some system does not like unexisting libraries. */
	      f = fopen (*curr_libname_ptr, "r");
	      if (f == NULL)
		continue;
	      fclose (f);
	      handle = dlopen (*curr_libname_ptr, RTLD_LAZY | RTLD_GLOBAL);
	      if (handle == NULL)
		continue;
	      address = dlsym (handle, name);
	      if (dlerror () == NULL)
		break;
	    }
	  if (*curr_libname_ptr == NULL)
	    eval_error (noextern_bc_decl, get_cpos (), DERR_no_such_external, name);
	}
    }
  BC_set_address (decl, address);
  if (BC_IS_OF_TYPE (decl, BC_NM_evdecl))
    {
      for (i = 0; i < VLO_LENGTH (external_vars) / sizeof (void *); i++)
	/* We need something faster (hashtab) ??? */
	if (((void **) VLO_BEGIN (external_vars)) [i] == address)
	  break;
      if (i >= VLO_LENGTH (external_vars) / sizeof (void *))
	VLO_ADD_MEMORY (external_vars, &address, sizeof (address));
    }
  return address;
}
