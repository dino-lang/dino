#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#ifndef HAVE_MEMCPY
#define HAVE_MEMCPY
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#include <stdlib.h>
#include "hashtab.h"
#include "vlobject.h"
#include "common.h"
#include "ird.h"
#include "gen-comm.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif



/* This page contains functions common for all package functions. */

#ifndef HAVE_MEMCPY

static void *
memcpy (void *to, const void *from, size_t size)
{
  char *cto = (char *) to;
  const char *cfrom = (const char *) from;

  while (size > 0)
    {
      *cto++ = *cfrom;
      size--;
    }
  return to;
}

#endif /* #ifndef HAVE_MEMCPY */



/* The following variable value is definition corresponding to special
   token `error'. */

IR_node_t error_single_definition;

/* The following variable value is definition of LR grammar end
   marker. */

IR_node_t end_marker_single_definition;



/* The following variable value is look ahead tokens actually
   needed. */

int real_look_ahead_number;



int
canonical_rule_right_hand_side_prefix_length
  (IR_node_t canonical_rule, IR_node_t bound_right_hand_side_element)
{
  IR_node_t current_right_hand_side_element;
  int length;

  length = 0;
  for (current_right_hand_side_element = IR_right_hand_side (canonical_rule);
       !IR_IS_OF_TYPE (current_right_hand_side_element,
                       IR_NM_canonical_rule_end)
       && current_right_hand_side_element != bound_right_hand_side_element;
       current_right_hand_side_element
       = IR_next_right_hand_side_element (current_right_hand_side_element))
    length++;
  return length;
}

static void
get_all_LR_set_predecessor_paths
   (vlo_t *paths, IR_node_t LR_set, int path_length,
    int start_length_for_function)
{
  IR_node_t LR_situation_of_immediate_LR_set_predecessor;
  IR_double_link_t LR_situation_reference;

  assert (path_length >= 0);
  if (path_length <= start_length_for_function)
    VLO_ADD_MEMORY (*paths, &LR_set, sizeof (LR_set));
  if (path_length != 0)
    {
      /* Process all immediate LR-predecessors of given LR-set. */
      for (LR_situation_reference = IR__first_double_link (LR_set);
           LR_situation_reference != NULL;
           LR_situation_reference
           = IR__next_double_link (LR_situation_reference))
        {
          LR_situation_of_immediate_LR_set_predecessor
            = IR__owner (LR_situation_reference);
          if (IR_IS_OF_TYPE (LR_situation_of_immediate_LR_set_predecessor,
                             IR_NM_LR_situation))
            {
              /* See comments in file `ird.sprut'. */
              assert (IR_first_symbol_LR_situation
                      (LR_situation_of_immediate_LR_set_predecessor));
              get_all_LR_set_predecessor_paths
                (paths,
                 IR_LR_set (LR_situation_of_immediate_LR_set_predecessor),
                 path_length - 1, start_length_for_function);
            }
        }
    }
}

void
traverse_all_LR_set_predecessor_paths
   (IR_node_t LR_set, int path_length,
    int (*applied_function) (IR_node_t LR_set), int start_length_for_function)
{
  int continue_flag;
  IR_node_t *LR_set_ptr;
  vlo_t paths;

  VLO_CREATE (paths, 0);
  get_all_LR_set_predecessor_paths (&paths, LR_set, path_length,
                                    start_length_for_function);
  for (LR_set_ptr = VLO_BEGIN (paths);
       (char *) LR_set_ptr <= (char *) VLO_END (paths);
       LR_set_ptr++)
    {
      continue_flag = (*applied_function) (*LR_set_ptr);
      if (!continue_flag)
        break;
    }
  VLO_DELETE (paths);
}

/* The traverse cache itself is represented by the following variable. */

static hash_table_t traverse_cache;

/* The following structure is refered for an entry of cache of
   function `traverse_all_LR_set_predecessors'. */

struct traverse_cache_entry
{
  /* The start LR-set.  This corresponds the first parameter of
     function `traverse_all_LR_set_predecessors'. */
  IR_node_t start_LR_set;
  /* This corresponds the second parameter of function
     `traverse_all_LR_set_predecessors'. */
  int path_length;
  /* The length of the following array. */
  int number_of_LR_set_predecessors;
  IR_node_t LR_set_predecessors [1];
};

typedef struct traverse_cache_entry *traverse_cache_entry_t;

static struct traverse_cache_entry work_traverse_cache_entry;

static int traverse_cache_off_flag;
static size_t traverse_cache_entries_size;

static unsigned
traverse_entry_hash_function (hash_table_entry_t traverse_entry)
{
  return
    (unsigned long) ((traverse_cache_entry_t) traverse_entry)->start_LR_set
    + ((traverse_cache_entry_t) traverse_entry)->path_length;
}

static int
traverse_entry_eq_function (hash_table_entry_t traverse_entry_1,
                            hash_table_entry_t traverse_entry_2)
{
  return ((((traverse_cache_entry_t) traverse_entry_1)->start_LR_set
           == ((traverse_cache_entry_t) traverse_entry_2)->start_LR_set)
          && (((traverse_cache_entry_t) traverse_entry_1)->path_length
              == ((traverse_cache_entry_t) traverse_entry_2)->path_length));
}

static void
include_traverse_cache_entry_into_empty_slot (traverse_cache_entry_t entry)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (traverse_cache, entry, TRUE);
  assert (*entry_ptr == NULL);
  *entry_ptr = (hash_table_entry_t) entry;
}

static traverse_cache_entry_t
find_traverse_cache_entry (IR_node_t start_LR_set, int path_length)
{
  hash_table_entry_t *entry_ptr;

  work_traverse_cache_entry.start_LR_set = start_LR_set;
  work_traverse_cache_entry.path_length = path_length;
  entry_ptr = find_hash_table_entry (traverse_cache,
                                     &work_traverse_cache_entry, FALSE);
  return (traverse_cache_entry_t) *entry_ptr;
}

void
initiate_traverse_cache (void)
{
  traverse_cache_off_flag = FALSE;
  traverse_cache_entries_size = 0;
  traverse_cache = create_hash_table (5000, traverse_entry_hash_function,
                                      traverse_entry_eq_function);
}

void
traverse_all_LR_set_predecessors (IR_node_t LR_set, int path_length,
                                  int (*applied_function) (IR_node_t LR_set))
{
  int continue_flag;
  int i;
  IR_node_t *LR_set_ptr;
  traverse_cache_entry_t cache_entry;
  size_t size;
  vlo_t paths;

  if (!traverse_cache_off_flag)
    cache_entry = find_traverse_cache_entry (LR_set, path_length);
  if (traverse_cache_off_flag || cache_entry == NULL)
    {
      VLO_CREATE (paths, 0);
      get_all_LR_set_predecessor_paths (&paths, LR_set, path_length, 0);
      for (LR_set_ptr = VLO_BEGIN (paths);
           (char *) LR_set_ptr <= (char *) VLO_END (paths);
           LR_set_ptr++)
        {
          continue_flag = (*applied_function) (*LR_set_ptr);
          if (!continue_flag)
            break;
        }
      assert (VLO_LENGTH (paths) != 0);
      if (!traverse_cache_off_flag)
        {
          size = (sizeof (struct traverse_cache_entry)
                  + VLO_LENGTH (paths) - sizeof (IR_node_t));
          traverse_cache_entries_size += size;
          IR_TOP_EXPAND (size);
          cache_entry = IR_TOP_BEGIN ();
          IR_TOP_FINISH ();
          cache_entry->start_LR_set = LR_set;
          cache_entry->path_length = path_length;
          cache_entry->number_of_LR_set_predecessors
            = VLO_LENGTH (paths) / sizeof (IR_node_t);
          memcpy (cache_entry->LR_set_predecessors, VLO_BEGIN (paths),
                  VLO_LENGTH (paths));
          include_traverse_cache_entry_into_empty_slot (cache_entry);
        }
      VLO_DELETE (paths);
    }
  else
    {
      for (i = 0; i < cache_entry->number_of_LR_set_predecessors; i++)
        {
          continue_flag
            = (*applied_function) (cache_entry->LR_set_predecessors [i]);
          if (!continue_flag)
            break;
        }
    }
}

void
traverse_cache_off (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf (stderr, "Empty cache -- ");
#endif
  finish_traverse_cache ();
  traverse_cache_off_flag = TRUE;
}

void
traverse_cache_on (void)
{
  initiate_traverse_cache ();
}

void
finish_traverse_cache (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Traverse cache: entries - %lu, elements - %lu (%lu bytes), collisions - %d%%\n",
       hash_table_size (traverse_cache),
       hash_table_elements_number (traverse_cache),
       traverse_cache_entries_size,
       hash_table_collisions (traverse_cache));
#endif
  delete_hash_table (traverse_cache);
}

void
reverse_traverse_all_LR_set_predecessor_paths
  (IR_node_t LR_set, int path_length,
   void (*applied_function) (IR_node_t LR_set),
   int start_length_for_function)
{
  IR_node_t LR_situation_of_immediate_LR_set_predecessor;
  IR_double_link_t LR_situation_reference;
  IR_node_t *LR_situation_ptr;
  vlo_t predecessors;

  assert (path_length >= 0);
  if (path_length != 0)
    {
      /* Collect all immediate LR-predecessors of given LR-set. */
      VLO_CREATE (predecessors, 100);
      for (LR_situation_reference = IR__first_double_link (LR_set);
           LR_situation_reference != NULL;
           LR_situation_reference
           = IR__next_double_link (LR_situation_reference))
        {
          LR_situation_of_immediate_LR_set_predecessor
            = IR__owner (LR_situation_reference);
          if (IR_IS_OF_TYPE (LR_situation_of_immediate_LR_set_predecessor,
                             IR_NM_LR_situation))
            VLO_ADD_MEMORY (predecessors,
                            &LR_situation_of_immediate_LR_set_predecessor,
                            sizeof (IR_node_t));
        }
      /* Process all immediate LR-predecessors of given LR-set. */
      for (LR_situation_ptr = VLO_BEGIN (predecessors);
           (char *) LR_situation_ptr <= (char *) VLO_END (predecessors);
           LR_situation_ptr++)
        {
          /* See comments in file `ird.sprut'. */
          assert (IR_first_symbol_LR_situation (*LR_situation_ptr));
          reverse_traverse_all_LR_set_predecessor_paths
            (IR_LR_set (*LR_situation_ptr),
             path_length - 1, applied_function, start_length_for_function);
        }
      VLO_DELETE (predecessors);
    }
  if (path_length <= start_length_for_function)
    (*applied_function) (LR_set);
}

static IR_node_t
previous_right_hand_side_element (IR_node_t canonical_rule,
                                  IR_node_t right_hand_side_element)
{
  IR_node_t next_right_hand_side_element;
  IR_node_t current_right_hand_side_element;

  current_right_hand_side_element = IR_right_hand_side (canonical_rule);
  if (current_right_hand_side_element == right_hand_side_element)
    return NULL;
  for (;;)
    {
      assert (current_right_hand_side_element != NULL);
      next_right_hand_side_element
        = IR_next_right_hand_side_element (current_right_hand_side_element);
      if (next_right_hand_side_element == right_hand_side_element)
        return current_right_hand_side_element;
      current_right_hand_side_element = next_right_hand_side_element;
    }
}

/* Returns single_definition. */
IR_node_t
characteristic_symbol_of_LR_set (IR_node_t LR_set)
{
  IR_node_t element_before_dot;

  assert (LR_set != NULL);
  element_before_dot
    = previous_right_hand_side_element
      (IR_canonical_rule (IR_element_after_dot (IR_LR_situation_list
                                                (LR_set))),
       IR_element_after_dot (IR_LR_situation_list (LR_set)));
  if (element_before_dot == NULL)
    /* This is start state. */
    return NULL;
  else
    return IR_element_itself (element_before_dot); 
}

static hash_table_t goto_set_cache;
static IR_node_t work_LR_situation;
static IR_node_t work_canonical_rule_element;

static unsigned
goto_set_cache_hash_function (hash_table_entry_t LR_situation)
{
  return ((unsigned long) IR_LR_set ((IR_node_t) LR_situation)
          + (unsigned long) IR_element_itself (IR_element_after_dot
                                               ((IR_node_t) LR_situation)));
}

static int
goto_set_cache_eq_function (hash_table_entry_t LR_situation_1,
                            hash_table_entry_t LR_situation_2)
{
  return
    (IR_LR_set ((IR_node_t) LR_situation_1)
     == IR_LR_set ((IR_node_t) LR_situation_2)
     && (IR_element_itself (IR_element_after_dot ((IR_node_t) LR_situation_1))
         == IR_element_itself (IR_element_after_dot
                               ((IR_node_t) LR_situation_2))));
}

static IR_node_t
insert_goto_set_entry (IR_node_t LR_situation)
{
  hash_table_entry_t *entry_ptr;

  entry_ptr = find_hash_table_entry (goto_set_cache, LR_situation, TRUE);
  if (*entry_ptr == NULL)
    *entry_ptr = (hash_table_entry_t) LR_situation;
  return (IR_node_t) *entry_ptr;
}

static IR_node_t
find_goto_set_entry (IR_node_t LR_set, IR_node_t nonterminal)
{
  hash_table_entry_t *entry_ptr;

  IR_set_LR_set (work_LR_situation, LR_set);
  IR_set_element_itself (work_canonical_rule_element, nonterminal);
  entry_ptr = find_hash_table_entry (goto_set_cache, work_LR_situation, FALSE);
  return (IR_node_t) *entry_ptr;
}

void
initiate_goto_set_cache (void)
{
  work_LR_situation = IR_create_node (IR_NM_LR_situation);
  work_canonical_rule_element = IR_create_node (IR_NM_canonical_rule_element);
  IR_set_element_after_dot (work_LR_situation, work_canonical_rule_element);
  goto_set_cache = create_hash_table (40000, goto_set_cache_hash_function,
                                      goto_set_cache_eq_function);
}

IR_node_t
find_goto_LR_situation (IR_node_t LR_set, IR_node_t single_definition)
{
  IR_node_t current_LR_situation;
  IR_node_t result_LR_situation;

  assert (single_definition != NULL);
  result_LR_situation = find_goto_set_entry (LR_set, single_definition);
  if (result_LR_situation == NULL)
    for (current_LR_situation = IR_LR_situation_list (LR_set);
         current_LR_situation != NULL;
         current_LR_situation = IR_next_LR_situation (current_LR_situation))
      if (IR_first_symbol_LR_situation (current_LR_situation)
          && !IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                             IR_NM_canonical_rule_end))
        {
          insert_goto_set_entry (current_LR_situation);
          /* Removed arcs are also processed because it is necessary for
             processing attributes. */
          if (IR_element_itself (IR_element_after_dot (current_LR_situation))
              == single_definition)
            {
              assert (result_LR_situation == NULL);
              result_LR_situation = current_LR_situation;
#ifdef NDEBUG
              break;
#endif
            }
      }
  assert (result_LR_situation != NULL);
  return result_LR_situation;
}

IR_node_t
goto_by_nonterminal (IR_node_t LR_set, IR_node_t single_definition)
{
  assert (single_definition != NULL
          && IR_IS_OF_TYPE (single_definition,
                            IR_NM_single_nonterm_definition));
  return IR_goto_LR_set (find_goto_LR_situation (LR_set, single_definition));
}

void
finish_goto_set_cache (void)
{
#ifndef NDEBUG
  if (debug_level >= 1)
    fprintf
      (stderr,
       "Goto set cache: entries - %lu, elements - %lu, collisions - %d%%\n",
       hash_table_size (goto_set_cache),
       hash_table_elements_number (goto_set_cache),
       hash_table_collisions (goto_set_cache));
#endif
  delete_hash_table (goto_set_cache);
}

void
LR_set_conflicts_number (IR_node_t LR_set, int *shift_reduce_conflicts_number,
                         int *reduce_reduce_conflicts_number)
{
  IR_node_t current_conflict;

  *shift_reduce_conflicts_number = 0;
  *reduce_reduce_conflicts_number = 0;
  for (current_conflict = IR_conflicts_list (LR_set);
       current_conflict != NULL;
       current_conflict = IR_next_conflict (current_conflict))
    if (IR_IS_OF_TYPE (IR_element_after_dot
                       (IR_used_LR_situation (current_conflict)),
                       IR_NM_canonical_rule_end)
        && IR_IS_OF_TYPE (IR_element_after_dot
                          (IR_unused_LR_situation (current_conflict)),
                          IR_NM_canonical_rule_end))
      (*reduce_reduce_conflicts_number)++;
    else
      (*shift_reduce_conflicts_number)++;
}

/* Remember that correctness of attributes has been already
   checked. */

int
attribute_name_to_attribute_number (const char *attribute_name,
                                    IR_node_t canonical_rule,
                                    IR_node_t bound_right_hand_side_element)
{
  IR_node_t current_right_hand_side_element;
  int attribute_number;

  assert (canonical_rule != IR_canonical_rule_list (description));
  attribute_number = 0;
  for (current_right_hand_side_element = IR_right_hand_side (canonical_rule);
       !IR_IS_OF_TYPE (current_right_hand_side_element,
                       IR_NM_canonical_rule_end)
       && current_right_hand_side_element != bound_right_hand_side_element;
       current_right_hand_side_element
       = IR_next_right_hand_side_element (current_right_hand_side_element))
    {
      attribute_number++;
      if (IR_element_identifier (current_right_hand_side_element) != NULL
          && strcmp (attribute_name,
                     IR_identifier_itself
                     (IR_element_identifier
                      (current_right_hand_side_element))) == 0)
        break;
    }
  assert (!IR_IS_OF_TYPE (current_right_hand_side_element,
                          IR_NM_canonical_rule_end)
          && current_right_hand_side_element != bound_right_hand_side_element);
  return attribute_number;
}

/* The following variable is used for searching for the single LR-set
   predecessor (see function `get_the_single_LR_set_predecessor'). */

static IR_node_t the_single_LR_set_predecessor;

/* The following function is used for searching the single LR-set
   predecessor of a LR-set (see function
   `get_the_single_LR_set_predecessor'). */

static int
fix_the_single_LR_set_predecessor (IR_node_t LR_set)
{
  assert (the_single_LR_set_predecessor == NULL);
  the_single_LR_set_predecessor = LR_set;
  return TRUE;
}

/* The following function returns LR-set which is predecessor of given
   LR-set.  The distance between given LR-set and returned LR-set will
   be equal to `path-length'.  Only one such LR-set must be in
   LR-graph. */

IR_node_t
get_the_single_LR_set_predecessor (IR_node_t LR_set, int path_length)
{
  assert (path_length >= 0);
  the_single_LR_set_predecessor = NULL;
  traverse_all_LR_set_predecessors (LR_set, path_length,
                                    fix_the_single_LR_set_predecessor);
  return the_single_LR_set_predecessor;
}

/* Number of pushed states or attribute on path from `LR_set' back to
   the state which are on given distance `length'.  The last LR-set is
   not included into the path. */

int
pushed_LR_sets_or_attributes_number_on_path (IR_node_t LR_set, int length,
                                             int attribute_flag)
{
  IR_node_t LR_situation_of_immediate_LR_set_predecessor;
  IR_double_link_t LR_situation_reference;
  int result;

  if (length == 0)
    result = 0;
  else
    {
      assert (length > 0);
      /* Process all immediate LR-predecessors of given LR-set. */
      result = -1;
      for (LR_situation_reference = IR__first_double_link (LR_set);
           LR_situation_reference != NULL;
           LR_situation_reference
           = IR__next_double_link (LR_situation_reference))
        {
          LR_situation_of_immediate_LR_set_predecessor
            = IR__owner (LR_situation_reference);
          if (IR_IS_OF_TYPE (LR_situation_of_immediate_LR_set_predecessor,
                             IR_NM_LR_situation))
            {
              assert (IR_first_symbol_LR_situation
                      (LR_situation_of_immediate_LR_set_predecessor));
              result
                = pushed_LR_sets_or_attributes_number_on_path
                  (IR_LR_set (LR_situation_of_immediate_LR_set_predecessor),
                   length - 1, attribute_flag);
              break;
            }
        }
      assert (result >= 0);
      if (attribute_flag)
        {
          if (IR_attribute_is_used (LR_set))
            result++;
        }
      else if (IR_it_is_pushed_LR_set (LR_set))
        result++;
    }
  return result;
}
