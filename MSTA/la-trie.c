#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#include <stdio.h>
#include <stdlib.h>
#include "vlobject.h"
#include "common.h"
#include "ird.h"
#include "contexts.h"
#include "lr-sets.h"
#include "la-trie.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif




/* The following variable value is the first trie node of free trie
   nodes list.  The list is formed with the aid of field
   `next_brother'. */

static IR_node_t first_free_LR_set_look_ahead_trie_node;

/* The following variable length object is used for storing vector of
   token strings of context being transformed into trie
   representation. */

static vlo_t LR_set_look_ahead_trie_token_strings_vector;

static void
free_LR_set_look_ahead_trie_node (IR_node_t trie_node)
{
  IR_set_next_brother (trie_node, first_free_LR_set_look_ahead_trie_node);
  first_free_LR_set_look_ahead_trie_node = trie_node;
}

static IR_node_t
get_free_LR_set_look_ahead_trie_node (IR_node_t single_term_definition)
{
  IR_node_t trie_node;

  if (first_free_LR_set_look_ahead_trie_node == NULL)
    trie_node
      = IR_new_LR_set_look_ahead_trie_node
        (single_term_definition, NULL, NULL, NULL);
  else
    {
      trie_node = first_free_LR_set_look_ahead_trie_node;
      first_free_LR_set_look_ahead_trie_node
        = IR_next_brother (first_free_LR_set_look_ahead_trie_node);
      IR_set_corresponding_single_term_definition (trie_node,
                                                   single_term_definition);
      IR_set_corresponding_LR_situation (trie_node, NULL);
      IR_set_first_son (trie_node, NULL);
      IR_set_next_brother (trie_node, NULL);
    }
  return trie_node;
}

struct LR_set_look_ahead_trie_token_strings_vector_element
{
  /* The following member refers to LR-situation (reduce situation or
     the first symbol (terminal) shift situation). */
  IR_node_t corresponding_LR_situation;
  token_string_t token_string;
};

/* The following variable value is LR-situation whose look ahead
   context is processed. */

static IR_node_t LR_situation_corresponding_context;

static void
add_token_string_to_LR_set_lookahead_trie_token_strings_vector
  (token_string_t token_string)
{
  struct LR_set_look_ahead_trie_token_strings_vector_element element;

  element.corresponding_LR_situation = LR_situation_corresponding_context;
  element.token_string = token_string;
  VLO_ADD_MEMORY
    (LR_set_look_ahead_trie_token_strings_vector, &element,
     sizeof (struct LR_set_look_ahead_trie_token_strings_vector_element));
}

static int
compare_LR_set_look_ahead_trie_token_strings
  (const void *token_string_1, const void *token_string_2)
{
  int result;
  struct LR_set_look_ahead_trie_token_strings_vector_element *element_1;
  struct LR_set_look_ahead_trie_token_strings_vector_element *element_2;

  element_1 = ((struct LR_set_look_ahead_trie_token_strings_vector_element *)
               token_string_1);
  element_2 = ((struct LR_set_look_ahead_trie_token_strings_vector_element *)
               token_string_2);
  result = token_string_comparison (element_1->token_string,
                                    element_2->token_string);
  /* This is specific thing of the used contexts. */
  assert (result != 0
          || (IR_under_control_point_flag (element_1
                                           ->corresponding_LR_situation)
              && IR_under_control_point_flag (element_2
                                              ->corresponding_LR_situation)));
  return result;
}

static int
compare_pointers (const void *pointer_1, const void *pointer_2)
{
  if (*(IR_node_t *) pointer_1 < *(IR_node_t *) pointer_2)
    return -1;
  else if (*(IR_node_t *) pointer_1 == *(IR_node_t *) pointer_2)
    return 0;
  else
    return 1;
}

static IR_node_t
set_up_default_LR_situation_of_trie_level
  (IR_node_t default_reduce_LR_situation, IR_node_t first_trie_node,
   int trie_level)
{
  IR_node_t current_trie_node;
  IR_node_t previous_trie_node;
  IR_node_t next_trie_node;
  IR_node_t default_trie_node;
  IR_node_t *current_LR_situation_ptr;
  IR_node_t *next_LR_situation_ptr;
  int reduces_number;
  int shifts_number;
  IR_node_t current_LR_situation;
  vlo_t reduce_LR_situations;

  if (default_reduce_LR_situation != NULL)
    {
      assert (trie_level == 0);
      for (previous_trie_node = NULL, current_trie_node = first_trie_node;
           current_trie_node != NULL;
           previous_trie_node = current_trie_node,
           current_trie_node = IR_next_brother (current_trie_node))
        ;
      assert (previous_trie_node == NULL);
    }
  else
    {
      for (current_trie_node = first_trie_node;
           current_trie_node != NULL;
           current_trie_node = IR_next_brother (current_trie_node))
        if (IR_first_son (current_trie_node) != NULL)
          IR_set_first_son
            (current_trie_node,
             set_up_default_LR_situation_of_trie_level
             (NULL, IR_first_son (current_trie_node), trie_level + 1));
      shifts_number = 0;
      VLO_CREATE (reduce_LR_situations, 0);
      for (current_trie_node = first_trie_node;
           current_trie_node != NULL;
           current_trie_node = IR_next_brother (current_trie_node))
        if (IR_first_son (current_trie_node) == NULL)
          {
            current_LR_situation
              = IR_corresponding_LR_situation (current_trie_node);
            if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                               IR_NM_canonical_rule_end))
              VLO_ADD_MEMORY (reduce_LR_situations,
                              &current_LR_situation, sizeof (IR_node_t));
            else
              shifts_number++;
          }
      qsort (VLO_BEGIN (reduce_LR_situations),
             VLO_LENGTH (reduce_LR_situations) / sizeof (IR_node_t),
             sizeof (IR_node_t), compare_pointers);
      default_reduce_LR_situation = NULL;
      reduces_number = 0;
      for (current_LR_situation_ptr = VLO_BEGIN (reduce_LR_situations);
           current_LR_situation_ptr
           <= (IR_node_t *) VLO_END (reduce_LR_situations);
           current_LR_situation_ptr = next_LR_situation_ptr)
        {
          for (next_LR_situation_ptr = current_LR_situation_ptr;
               next_LR_situation_ptr
               <= (IR_node_t *) VLO_END (reduce_LR_situations);
               next_LR_situation_ptr++)
            if (*current_LR_situation_ptr != *next_LR_situation_ptr)
              break;
          if (default_reduce_LR_situation == NULL
              || (reduces_number
                  < next_LR_situation_ptr - current_LR_situation_ptr))
            {
              default_reduce_LR_situation = *current_LR_situation_ptr;
              reduces_number
                = next_LR_situation_ptr - current_LR_situation_ptr;
            }
        }
      VLO_DELETE (reduce_LR_situations);
      if (default_reduce_LR_situation != NULL
          || (trie_level != 0 && shifts_number > reduces_number))
        {
          if (trie_level != 0 && shifts_number > reduces_number)
            default_reduce_LR_situation = NULL;
          for (previous_trie_node = NULL, current_trie_node = first_trie_node;
               current_trie_node != NULL;
               current_trie_node = next_trie_node)
            {
              next_trie_node = IR_next_brother (current_trie_node);
              if (IR_first_son (current_trie_node) == NULL
                  && ((trie_level != 0 && shifts_number > reduces_number
                       && !IR_IS_OF_TYPE (IR_element_after_dot
                                          (IR_corresponding_LR_situation
                                           (current_trie_node)),
                                          IR_NM_canonical_rule_end))
                      || (IR_corresponding_LR_situation (current_trie_node)
                          == default_reduce_LR_situation)))
                {
                  assert
                    (default_reduce_LR_situation == NULL
                     ||
                     (default_reduce_LR_situation
                      == IR_corresponding_LR_situation (current_trie_node)));
                  /* This default LR-situation is the single shift
                     LR-situatuion for look ahead number 2, 3, ... */
                  default_reduce_LR_situation           
                    = IR_corresponding_LR_situation (current_trie_node);
                  if (previous_trie_node == NULL)
                    first_trie_node = next_trie_node;
                  else
                    IR_set_next_brother (previous_trie_node, next_trie_node);
                  free_LR_set_look_ahead_trie_node (current_trie_node);
                }
              else
                previous_trie_node = current_trie_node;
            }
        }
    }
  if (default_reduce_LR_situation != NULL)
    {
      assert (previous_trie_node != NULL || first_trie_node == NULL);
      default_trie_node = get_free_LR_set_look_ahead_trie_node (NULL);
      IR_set_corresponding_LR_situation (default_trie_node,
                                         default_reduce_LR_situation);
      if (previous_trie_node == NULL)
        first_trie_node = default_trie_node;
      else
        IR_set_next_brother (previous_trie_node, default_trie_node);
    }
  return first_trie_node;
}

static void
set_up_LR_set_look_ahead_trie (IR_node_t LR_set)
{
  IR_node_t *the_rightmost_trie_nodes_array;
  IR_node_t current_LR_set_look_ahead_trie_node;
  IR_node_t previous_brother;
  int current_look_ahead;
  struct LR_set_look_ahead_trie_token_strings_vector_element
         *current_token_string_ptr;
  int current_the_rightmost_trie_nodes_array_size;
  IR_node_t default_reduce_LR_situation;
  IR_node_t first_LR_set_look_ahead_trie_node;
  IR_node_t single_term_definition;
  IR_node_t back_track_alternative;
  vlo_t the_rightmost_trie_nodes;

#ifndef NDEBUG
  if (debug_level >= 2)
    {
      fprintf (stderr, "build look ahead trie of LR-set:\n");
      output_LR_set_situations (stderr, LR_set, "\t");
    }
#endif
  assert (IR_reachable_flag (LR_set));
  VLO_NULLIFY (LR_set_look_ahead_trie_token_strings_vector);
  default_reduce_LR_situation = NULL;
  for (LR_situation_corresponding_context = IR_LR_situation_list (LR_set);
       LR_situation_corresponding_context != NULL;
       LR_situation_corresponding_context
         = IR_next_LR_situation (LR_situation_corresponding_context))
    {
#ifndef NDEBUG
      if (debug_level >= 2)
        {
          fprintf (stderr, "  process LR-situation:");
          output_LR_situation (stderr, LR_situation_corresponding_context,
                               "\t", TRUE);
        }
#endif
      if (IR_IS_OF_TYPE (IR_element_after_dot
                         (LR_situation_corresponding_context),
                         IR_NM_canonical_rule_end))
        {
          if (IR_look_ahead_context (LR_situation_corresponding_context)
              != NULL)
            process_context_token_strings
              (IR_look_ahead_context (LR_situation_corresponding_context),
               add_token_string_to_LR_set_lookahead_trie_token_strings_vector);
          else
            {
              assert (default_reduce_LR_situation == NULL);
              default_reduce_LR_situation = LR_situation_corresponding_context;
            }
        }
      else if (IR_first_symbol_LR_situation
               (LR_situation_corresponding_context)
               && (!IR_goto_arc_has_been_removed
                   (LR_situation_corresponding_context)
                   || (IR_corresponding_regular_arc
                       (LR_situation_corresponding_context) != NULL))
               && IR_IS_OF_TYPE (IR_element_itself
                                 (IR_element_after_dot
                                  (LR_situation_corresponding_context)),
                                 IR_NM_single_term_definition))
        {
          if (IR_look_ahead_context (LR_situation_corresponding_context)
              == NULL)
            {
              single_term_definition
                = IR_element_itself (IR_element_after_dot
                                     (LR_situation_corresponding_context));
              add_token_string_to_LR_set_lookahead_trie_token_strings_vector
                (get_new_token_string (&single_term_definition, 1));
            }
          else
            process_context_token_strings
              (IR_look_ahead_context (LR_situation_corresponding_context),
               add_token_string_to_LR_set_lookahead_trie_token_strings_vector);
        }
    }
  qsort (VLO_BEGIN (LR_set_look_ahead_trie_token_strings_vector),
         VLO_LENGTH (LR_set_look_ahead_trie_token_strings_vector)
         / sizeof (struct LR_set_look_ahead_trie_token_strings_vector_element),
         sizeof (struct LR_set_look_ahead_trie_token_strings_vector_element),
         compare_LR_set_look_ahead_trie_token_strings);
  VLO_CREATE (the_rightmost_trie_nodes,
              max_look_ahead_number * sizeof (IR_node_t)); 
  the_rightmost_trie_nodes_array = VLO_BEGIN (the_rightmost_trie_nodes);
  current_the_rightmost_trie_nodes_array_size = 0;
  first_LR_set_look_ahead_trie_node = NULL;
  for (current_token_string_ptr
       = VLO_BEGIN (LR_set_look_ahead_trie_token_strings_vector);
       (char *) current_token_string_ptr
       <= (char *) VLO_END (LR_set_look_ahead_trie_token_strings_vector);
       current_token_string_ptr++)
    {
      previous_brother = NULL;
      for (current_look_ahead = 0;
           current_look_ahead < current_the_rightmost_trie_nodes_array_size;
           current_look_ahead++)
        {
          single_term_definition
            = get_n_th_token (current_token_string_ptr->token_string,
                              current_look_ahead);
          previous_brother
            = the_rightmost_trie_nodes_array [current_look_ahead];
          assert (previous_brother != NULL
                  /* There is not possible situation when there are token
                     strings 'ab' and 'abc' in the contexts (i.e. one
                     token string is a prefix of the second token string).
                     The following reflects it. */
                  && single_term_definition != NULL);
          if (IR_corresponding_single_term_definition (previous_brother)
              != single_term_definition)
            break;
        }
      if (current_look_ahead < current_the_rightmost_trie_nodes_array_size
          || ((current_token_string_ptr
               == VLO_BEGIN (LR_set_look_ahead_trie_token_strings_vector))
              && current_the_rightmost_trie_nodes_array_size == 0
              && previous_brother == NULL))
        {
          single_term_definition
            = get_n_th_token (current_token_string_ptr->token_string,
                              current_look_ahead);
          current_LR_set_look_ahead_trie_node
            = get_free_LR_set_look_ahead_trie_node (single_term_definition);
          if (previous_brother == NULL)
            first_LR_set_look_ahead_trie_node
              = current_LR_set_look_ahead_trie_node;
          else
            IR_set_next_brother (previous_brother,
                                 current_LR_set_look_ahead_trie_node);
          for (;;)
            {
              the_rightmost_trie_nodes_array [current_look_ahead]
                = current_LR_set_look_ahead_trie_node;
              current_look_ahead++;
              current_the_rightmost_trie_nodes_array_size = current_look_ahead;
              single_term_definition
                = get_n_th_token (current_token_string_ptr->token_string,
                                  current_look_ahead);
              if (single_term_definition == NULL)
                break;
              IR_set_first_son
                (current_LR_set_look_ahead_trie_node,
                 get_free_LR_set_look_ahead_trie_node
                 (single_term_definition));
              current_LR_set_look_ahead_trie_node
                = IR_first_son (current_LR_set_look_ahead_trie_node);
            }
          IR_set_corresponding_LR_situation
            (current_LR_set_look_ahead_trie_node,
             current_token_string_ptr->corresponding_LR_situation);
        }
      else
        {
          /* New back track alternative: */
          assert (current_look_ahead == 1
                  && IR_under_control_point_flag (current_token_string_ptr
                                                  ->corresponding_LR_situation)
                  && IR_under_control_point_flag (IR_corresponding_LR_situation
                                                  (previous_brother)));
          back_track_alternative
            = IR_new_back_track_alternative (previous_brother, NULL);
          if (IR_first_back_track_alternative (previous_brother) == NULL)
            {
              IR_set_first_back_track_alternative (previous_brother,
                                                   back_track_alternative);
              IR_set_last_back_track_alternative (previous_brother,
                                                  back_track_alternative);
            }
          else
            IR_set_last_back_track_alternative
              (IR_last_back_track_alternative (previous_brother),
               back_track_alternative);
        }
    }
  VLO_DELETE (the_rightmost_trie_nodes); 
  IR_set_LR_set_look_ahead_trie (LR_set,
                                 set_up_default_LR_situation_of_trie_level
                                 (default_reduce_LR_situation,
                                  first_LR_set_look_ahead_trie_node, 0));
}

void
create_LR_set_look_ahead_trie (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        set_up_LR_set_look_ahead_trie (current_LR_set);
}

void
initiate_LR_set_look_ahead_tries (void)
{
  first_free_LR_set_look_ahead_trie_node = NULL;
  VLO_CREATE (LR_set_look_ahead_trie_token_strings_vector, 10000);
}

void
finish_LR_set_look_ahead_tries (void)
{
  VLO_DELETE (LR_set_look_ahead_trie_token_strings_vector);
}



static int
get_trie_height (IR_node_t first_trie_node)
{
  int result;
  int trie_height;
  IR_node_t current_trie_node;

  result = 0;
  for (current_trie_node = first_trie_node;
       current_trie_node != NULL;
       current_trie_node = IR_next_brother (current_trie_node))
    {
      trie_height = get_trie_height (IR_first_son (current_trie_node)) + 1;
      if (trie_height > result)
        result = trie_height;
    }
  return result;
}

int
get_max_look_ahead_string_length (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  int trie_height;
  int result;

  result = 0;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          trie_height
            = get_trie_height (IR_LR_set_look_ahead_trie (current_LR_set));
          if (trie_height > result)
            result = trie_height;
        }
  return result;
}

