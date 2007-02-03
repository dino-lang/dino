/*
   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of the tool OKA.

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

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "schedule.h"

class resource_history;
class list;

class scheduler_state
{
  /* Forward declarations: */

  struct dependence_graph_node;

  struct dependence_graph_arc;

  /* The following type serves to represent a node of data dependence
     graph of a basic block. */
  
  typedef struct dependence_graph_node *dependence_graph_node_t;
  
  /* The following type serves to represent an arc between nodes of
     data dependence graph of a basic block. */

  typedef struct dependence_graph_arc *dependence_graph_arc_t;

  /* Forward declarations: */

  friend class resource_occurrence_pool;
  friend class resource_history;
  friend class list;

  /* The following member contains all history usages and definitions
     of resources of instructions. */

  resource_history *resources;

  /* The following member contains all instructions whose resources
     are already ready. */

  list *data_ready_list;

  /* The following member contains all instructions which can be
     started and which are not in data ready list, i.e. starting such
     instructions results in stopping pipeline because the resources
     are not ready. */

  list *instruction_ready_list;

  /* The following member value is current time (0, 1, ...). */

  int current_time;

  static int ge_for_instruction_ready_list (dependence_graph_node_t node_1,
                                            dependence_graph_node_t node_2);
  
  static int ge_for_data_ready_list (dependence_graph_node_t node_1,
                                     dependence_graph_node_t node_2);

  void finish_forming_dependence_graph (void);
  
  void process_node_resources (dependence_graph_node_t node);
  
  void build_data_dependence_graph (void);
  
  void process_instruction_ready_list (void);
  
  void advance_time (void);

  void append_node_to_result_list (dependence_graph_node_t node);

  void add_node_to_result_list (dependence_graph_node_t node);

public:

  /* Forward declaration: */
  struct resource_occurrence;

  /* The following two memebers refer for top and bottom nodes of the
     data dependence graph. */
  dependence_graph_node_t graph_top;
  dependence_graph_node_t graph_bottom;

  /* The following memeber refers for the last instruction in the
     result list, i.e. list of the started instructions. */

  dependence_graph_node_t last_result_list_node;

  /* The following memeber refers for the current instruction in the
     result list.  The following memeber is used for implementation of
     functions `first_result_list_instruction' and
     `next_result_list_instruction'. */

  dependence_graph_node_t current_result_list_node;

  /* The following member value is critical path length in the data
     dependence graph. */

  int critical_path_length;

  void *operator new (size_t size)
    {
      return scheduler::allocate (size);
    }
  
  void operator delete (void *memory)
    {
      scheduler::free (memory);
    }

  void estimate_basic_block (dependence_graph_node_t start_node);
  
  void evaluate_estimated_basic_block (void);

  void schedule_estimated_basic_block (void);

  void print_result_list (void);

  scheduler_state (void);

  ~scheduler_state (void);
};

/* The following structure describes data dependence graph node. */

struct scheduler_state::dependence_graph_node
{
  /* The following member is instruction corresponding to given node.
     There are two data dependence graph nodes (top and bottom nodes)
     whose values of the member is NULL. */
  scheduler::instruction_t corresponding_instruction;

  /* The following member is order number of the node.  The top
     (start) node has zero order number.  The first instruction in
     original basic block has oder number one and so on.  The bottom
     (finish) node has maximal order number. */
  int order_number;

  /* The following two members refer to first output and input arcs of
     given node. */
  dependence_graph_arc_t first_out_dependence_graph_arc;
  dependence_graph_arc_t first_in_dependence_graph_arc;

  /* The following member value is critical path length of given
     node. */
  int node_estimation;

  /* The following member value is summury of critical path lengths of
     successors of given node. */
  int summary_successors_node_estimation;

  /* The following member value is start time of the corresponding
     instruction.  The following member becomes defined when the
     corresponding instruction is added to the result list. */
  int start_time;

  /* The following member is node which contains the next instruction in
     original basic block. */
  struct dependence_graph_node *next_dependence_graph_node;

  /* The following member is node which contains the next instruction in
     the result list. */
  struct dependence_graph_node *next_result_list_node;

  /* The following members are used to form data ready or instruction
     ready lists. */
  struct dependence_graph_node *next_list_node;
  struct dependence_graph_node *previous_list_node;

  /* The following member is used when the node is in the instruction
     ready list in order to solve when moving it into data ready list
     is possible. */
  int data_ready_time;

  /* The following member is used to solve when the instruction must
     be moved in in the instruction ready list. */
  int unprocessed_arcs_number;
};

/* The following structure describes data dependence graph arc. */

struct scheduler_state::dependence_graph_arc
{
  /* The two following members refer for the source and destination
     nodes of the arc. */
  struct dependence_graph_node *from_dependence_graph_node;
  struct dependence_graph_node *to_dependence_graph_node;

  /* The two following members refer for the next arcs with
     correspondingly the same source and destination nodes of the
     arc. */
  dependence_graph_arc_t next_out_dependence_graph_arc;
  dependence_graph_arc_t next_in_dependence_graph_arc;

  /* The following member value is dependence delay between two nodes
     given by the arc. */
  int dependence_delay;
};

/* The following structure describes resource usage or definition. */

struct scheduler_state::resource_occurrence
{
  /* The following member refers to node whose instruction uses or
     defines given resource. */
  dependence_graph_node_t corresponding_node;
  /* The following member refers to next resource usage or
     definition. */
  struct resource_occurrence *next_resource_occurrence;
  /* The following member makes more accurate resource identification.
     The different values of the member are used ussualy in the case
     of memory. */
  unsigned int subresource;
  /* ??? */
  unsigned int marker;
};
  
/* The following class describes pool of resource occurrences. */

class resource_occurrence_pool
{
  struct scheduler_state::resource_occurrence *free_resource_occurrences;

public:
  
  /* The following function returns free resource occurrence. */

  struct scheduler_state::resource_occurrence *
  get_free_resource_occurrence (scheduler_state::dependence_graph_node_t node,
                                unsigned int subresource, unsigned int marker)
    {
      struct scheduler_state::resource_occurrence *result;
      
      if (free_resource_occurrences == NULL)
        result
          = ((struct scheduler_state::resource_occurrence *)
             (scheduler::allocate (sizeof
                                   (struct
                                    scheduler_state::resource_occurrence))));
      else
        {
          result = free_resource_occurrences;
          free_resource_occurrences
            = free_resource_occurrences->next_resource_occurrence;
        }
      result->corresponding_node = node;
      result->subresource = subresource;
      result->marker = marker;
      return result;
    }

  /* The following function places given resource occurrence into the list
     of free resource occurrences. */

  void
  free_resource_occurrence (struct scheduler_state::resource_occurrence
                            *resource_occurrence)
    {
      resource_occurrence->next_resource_occurrence
        = free_resource_occurrences;
      free_resource_occurrences = resource_occurrence;
    }
  
  /* The following function places resource occurrences from given list
     into the list of free resource occurrences. */

  void
  free_resource_occurrence_list (struct scheduler_state::resource_occurrence *
                                 resource_occurrence_list)
    {
      struct scheduler_state::resource_occurrence *current_resource_occurrence;
      struct scheduler_state::resource_occurrence *next_resource_occurrence;
  
      for (current_resource_occurrence = resource_occurrence_list;
           current_resource_occurrence != NULL;
           current_resource_occurrence = next_resource_occurrence)
        {
          next_resource_occurrence
            = current_resource_occurrence->next_resource_occurrence;
          free_resource_occurrence (current_resource_occurrence);
        }
    }

  void *operator new (size_t size)
    {
      return scheduler::allocate (size);
    }
  
  void operator delete (void *memory)
    {
      scheduler::free (memory);
    }

  /* The constructor of resource occurrences pool. */

  resource_occurrence_pool (void)
    {
      free_resource_occurrences = NULL;
    }

  /* The desctructor of resource occurrences pool frees all memory allocated
     for resource occurrences. */

  ~resource_occurrence_pool (void)
    {
      struct scheduler_state::resource_occurrence *current_resource_occurrence;
      struct scheduler_state::resource_occurrence *next_resource_occurrence;
      
      for (current_resource_occurrence = free_resource_occurrences;
           current_resource_occurrence != NULL;
           current_resource_occurrence = next_resource_occurrence)
        {
          next_resource_occurrence
            = current_resource_occurrence->next_resource_occurrence;
          scheduler::free (current_resource_occurrence);
        }
    }
};

/* The following class describes vectors which contain heads of
   lists of usages and definitions of resource occurrences. */
class resource_heads_vector
{
  struct scheduler_state::resource_occurrence **resource_heads;
  int initial_resource_heads_vector_size;
  int current_resource_heads_vector_size;
  class resource_occurrence_pool *resource_occurrences;
  
  /* The following function nullify given vector of pointers to
     resource occurences. */

  void zeroing (struct scheduler_state::resource_occurrence **start,
                int nelements)
    {
      assert (nelements > 0);
      while (nelements > 0)
        {
          *start++ = NULL;
          nelements--;
        }
    }
  
public:
  
  void *operator new (size_t size)
    {
      return scheduler::allocate (size);
    }

  void operator delete (void *memory)
    {
      scheduler::free (memory);
    }


  inline int vector_size (void)
    {
      return current_resource_heads_vector_size;
    }

  /* The following operator makes access to head of resource
     occurrences list of given resource.  The operator expands the
     vector if it is necessary. */
  struct scheduler_state::resource_occurrence *&operator[] (int index)
    {
      if (index >= current_resource_heads_vector_size)
        {
          current_resource_heads_vector_size = index + index / 2;
          resource_heads
            = ((struct scheduler_state::resource_occurrence **)
               scheduler::reallocate
               (resource_heads,
                current_resource_heads_vector_size
                * sizeof (struct scheduler_state::resource_occurrence *)));
          zeroing (&resource_heads [index],
                   initial_resource_heads_vector_size - index);
        }
      return resource_heads [index];
    }

  /* The following constructor creates initial size null vector of
     heads of lists of resource occurrences. */
  resource_heads_vector (class resource_occurrence_pool *pool)
    {
      resource_occurrences = pool;
      initial_resource_heads_vector_size = 100;
      current_resource_heads_vector_size
        = initial_resource_heads_vector_size;
      resource_heads = (((struct scheduler_state::resource_occurrence **)
                         scheduler::allocate
                         (sizeof (struct
                                  scheduler_state::resource_occurrence *)
                          * current_resource_heads_vector_size)));
      zeroing (resource_heads, initial_resource_heads_vector_size);
    }

  /* The destructor frees all lists of given vector of heads of
     lsists of resource occurences. */
  ~resource_heads_vector (void)
    {
      int current_index;
      
      for (current_index = 0;
           current_index < current_resource_heads_vector_size;
           current_index++)
        if (resource_heads [current_index] != NULL)
          resource_occurrences->free_resource_occurrence_list
            (resource_heads [current_index]);
      scheduler::free (resource_heads);
    }
};


 /* The following class describes history of usages and definitions of
   resources. */
class resource_history
{

   friend class resource_heads_vector;
  
  /* The following members are vectors which contain heads of lists
     of usages and definitions of resource occurrences. */
  class resource_heads_vector *resource_usages;
  class resource_heads_vector *resource_definitions;

  /* The following member is resource occurrence pool. */
  class resource_occurrence_pool *resource_occurrences;

  /* The following member refer for scheduler state which create the
     resource history. */
  class scheduler_state *state;

public:

  /* The following function creates new arc (marked by given dependece
     delay) of the data dependence graph between two given nodes. */

  void add_dependence_graph_arc
         (scheduler_state::dependence_graph_node_t from_node,
          scheduler_state::dependence_graph_node_t to_node,
          int dependence_delay)
  {
    scheduler_state::dependence_graph_arc_t new_arc;
    
    assert (from_node != NULL && to_node != NULL);
    if (from_node == to_node || dependence_delay < 0)
      /* To simplify code, we make this situation is possible -- see
         function `process_node_resources'. */
      return;
    new_arc = ((scheduler_state::dependence_graph_arc_t)
               scheduler::allocate (sizeof
                                    (struct
                                     scheduler_state::dependence_graph_arc)));
    new_arc->from_dependence_graph_node = from_node;
    new_arc->to_dependence_graph_node = to_node;
    new_arc->next_in_dependence_graph_arc
      = to_node->first_in_dependence_graph_arc;
    to_node->first_in_dependence_graph_arc = new_arc;
    new_arc->next_out_dependence_graph_arc
      = from_node->first_out_dependence_graph_arc;
    from_node->first_out_dependence_graph_arc = new_arc;
    new_arc->dependence_delay = dependence_delay;
    assert (new_arc->dependence_delay >= 0);
  }

  /* The function processes given resource usage or definition in
     instruction of given node.  The function creates arcs describing
     data dependences: usage-definition, definition-definition,
     definition-usage.  The functions also modifies lists of usage
     or/and definition of given resource. */

  void process_resource (unsigned int resource, unsigned int subresource,
                         unsigned int marker, int usage_flag,
                         scheduler_state::dependence_graph_node_t node)
    {
      struct scheduler_state::resource_occurrence *resource_occurrence;
      struct scheduler_state::resource_occurrence *current_resource_occurrence;
      struct scheduler_state::resource_occurrence
                              *previous_resource_occurrence;
      struct scheduler_state::resource_occurrence *next_resource_occurrence;
        
      resource_occurrence
        = (resource_occurrences
           ->get_free_resource_occurrence (node, subresource, marker));
      if (usage_flag)
        {
          for (current_resource_occurrence
                 = (*resource_definitions) [resource];
               current_resource_occurrence != NULL;
               current_resource_occurrence
                 = current_resource_occurrence->next_resource_occurrence)
            {        
              /* Create dependence arc: definition-usage. */
              assert (current_resource_occurrence->corresponding_node
                      ->corresponding_instruction != NULL);
              add_dependence_graph_arc
                (current_resource_occurrence->corresponding_node,
                 node,
                 scheduler::dependence_delay
                 (current_resource_occurrence->corresponding_node
                  ->corresponding_instruction, 1 /*TRUE*/,
                  node->corresponding_instruction, 0 /*FALSE*/,
                  resource,
                  current_resource_occurrence->subresource, subresource,
                  current_resource_occurrence->marker, marker));
            }
          resource_occurrence->next_resource_occurrence
            = (*resource_usages) [resource];
          (*resource_usages) [resource] = resource_occurrence;
        }
      else
        {
          /* Process usage-definition pairs. */
          for (current_resource_occurrence = (*resource_usages) [resource];
               current_resource_occurrence != NULL;
               current_resource_occurrence
                 = current_resource_occurrence->next_resource_occurrence)
            {        
              /* Create dependence arc: usage-definition. */
              assert (current_resource_occurrence->corresponding_node
                      ->corresponding_instruction != NULL);
              add_dependence_graph_arc
                (current_resource_occurrence->corresponding_node,
                 node,
                 scheduler::dependence_delay
                 (current_resource_occurrence->corresponding_node
                  ->corresponding_instruction, 0 /*FALSE*/,
                  node->corresponding_instruction, 1 /*TRUE*/,
                  resource,
                  current_resource_occurrence->subresource, subresource,
                  current_resource_occurrence->marker, marker));
            }
          /* Process definition-definition pairs. */
          for (current_resource_occurrence
                 = (*resource_definitions) [resource];
               current_resource_occurrence != NULL;
               current_resource_occurrence
                 = current_resource_occurrence->next_resource_occurrence)
            {        
              /* Create dependence arc: definition-definition. */
              assert (current_resource_occurrence->corresponding_node
                      ->corresponding_instruction != NULL);
              add_dependence_graph_arc
                (current_resource_occurrence->corresponding_node,
                 node,
                 scheduler::dependence_delay
                 (current_resource_occurrence->corresponding_node
                  ->corresponding_instruction, 1 /*TRUE*/,
                  node->corresponding_instruction, 1 /*TRUE*/,
                  resource,
                  current_resource_occurrence->subresource, subresource,
                  current_resource_occurrence->marker, marker));
            }
          /* Modify usage list. */
          for (previous_resource_occurrence = NULL,
               current_resource_occurrence = (*resource_usages) [resource];
               current_resource_occurrence != NULL;
               current_resource_occurrence = next_resource_occurrence)
            {
              next_resource_occurrence
                = current_resource_occurrence->next_resource_occurrence;
              if (current_resource_occurrence->subresource == subresource)
                {
                  resource_occurrences->free_resource_occurrence
                    (current_resource_occurrence);
                  if (previous_resource_occurrence == NULL)
                    (*resource_usages) [resource] = next_resource_occurrence;
                  else
                    previous_resource_occurrence->next_resource_occurrence
                      = next_resource_occurrence;
                }
              else
                previous_resource_occurrence = current_resource_occurrence;
            }
          /* Modify definition list. */
          for (previous_resource_occurrence = NULL,
               current_resource_occurrence
                 = (*resource_definitions) [resource];
               current_resource_occurrence != NULL;
               current_resource_occurrence = next_resource_occurrence)
            {
              next_resource_occurrence
                = current_resource_occurrence->next_resource_occurrence;
              if (current_resource_occurrence->subresource == subresource)
                {
                  resource_occurrences->free_resource_occurrence
                    (current_resource_occurrence);
                  if (previous_resource_occurrence == NULL)
                    (*resource_definitions) [resource]
                      = next_resource_occurrence;
                  else
                    previous_resource_occurrence->next_resource_occurrence
                      = next_resource_occurrence;
                }
              else
                previous_resource_occurrence = current_resource_occurrence;
            }
          resource_occurrence->next_resource_occurrence
            = (*resource_definitions) [resource];
          (*resource_definitions) [resource] = resource_occurrence;
        }
    }

  void *operator new (size_t size)
    {
      return scheduler::allocate (size);
    }

  void operator delete (void *memory)
    {
      scheduler::free (memory);
    }

  /* The following constructor creates resource occurrences pool and
     heads of lists of resource definitions and usages. */

  resource_history (class scheduler_state *state)
    { 
      this->state = state;
      resource_occurrences = new resource_occurrence_pool ();
      resource_usages = new resource_heads_vector (resource_occurrences);
      resource_definitions = new resource_heads_vector (resource_occurrences);
    }

  /* Destructor. */
  ~resource_history (void)
    {
      delete resource_usages;
      delete resource_definitions;
      delete resource_occurrences;
    }
};

/* The following function is used to make order in the instruction
   ready list during scheduling. */

int scheduler_state::ge_for_instruction_ready_list
  (dependence_graph_node_t node_1, dependence_graph_node_t node_2)
{
  return node_1->data_ready_time >= node_2->data_ready_time;
}
  
/* The following function is used to make order in the data ready list
   during scheduling. */

int scheduler_state::ge_for_data_ready_list (dependence_graph_node_t node_1,
                                             dependence_graph_node_t node_2)
{
  return (node_1->node_estimation < node_2->node_estimation
          || node_1->node_estimation == node_2->node_estimation
          && ((node_1->summary_successors_node_estimation
               < node_2->summary_successors_node_estimation)
              || (node_1->summary_successors_node_estimation
                  == node_2->summary_successors_node_estimation)
              && (node_1->order_number >= node_2->order_number)));
}

/* The following class describes data ready and instruction ready
   lists (double linked lists). */

class list
{
public:
  
  typedef int (*ge_function_t) (scheduler_state::dependence_graph_node_t
                                node_1,
                                scheduler_state::dependence_graph_node_t
                                node_2);
  
private:
  
  /* The following members are the first and the last element of the
     list. */
  scheduler_state::dependence_graph_node_t list_head;
  scheduler_state::dependence_graph_node_t list_end;
  
  /* Pointer to the function which is used to make order in the
     instruction ready list (more correctly to insert node into the
     list) during scheduling. */
  ge_function_t ge_function;
  
public:

  void *operator new (size_t size)
    {
      return scheduler::allocate (size);
    }

  void operator delete (void *memory)
    {
      scheduler::free (memory);
    }

  /* The function returns the first node in the list. */
  inline scheduler_state::dependence_graph_node_t head (void)
    {
      return list_head;
    }
  
  /* The function deletes given node from the list.  Of course, given
     node must be in the list. */
  void remove (scheduler_state::dependence_graph_node_t node)
    {
      scheduler_state::dependence_graph_node_t current_node;
      
      assert (node != NULL);
      for (current_node = list_head;
           current_node != NULL;
           current_node = current_node->next_list_node)
        {
          assert (current_node == list_head
                  || (current_node->previous_list_node->next_list_node
                      == current_node));
          if (current_node == node)
            break;
        }
      if (current_node != NULL)
        {
          if (current_node == list_head)
            list_head = current_node->next_list_node;
          else
            current_node->previous_list_node->next_list_node
              = current_node->next_list_node;
          if (current_node == list_end)
            list_end = current_node->previous_list_node;
          else
            current_node->next_list_node->previous_list_node
              = current_node->previous_list_node;
        }
    }
  
  /* The function inserts given node into the list.  Of course, given
     node must be no in a list (data ready or instruction ready). */

  void insert (scheduler_state::dependence_graph_node_t node)
    {
      scheduler_state::dependence_graph_node_t current_node;
      
      assert (node != NULL);
      for (current_node = list_end;
           current_node != NULL;
           current_node = current_node->previous_list_node)
        {
          assert (current_node == list_end
                  || (current_node->next_list_node->previous_list_node
                      == current_node));
          if ((*ge_function) (node, current_node))
            break;
        }
      node->previous_list_node = current_node;
      if (current_node != NULL)
        {
          node->next_list_node = current_node->next_list_node;
          current_node->next_list_node = node;
        }
      else
        {
          node->next_list_node = list_head;
          list_head = node;
        }
      if (node->next_list_node != NULL)
        node->next_list_node->previous_list_node = node;
      else
        list_end = node;
    }
  
  /* Constructor. */
  list (ge_function_t function)
    {
      ge_function = function;
      list_head = NULL;
      list_end = NULL;
    }
};

/* The following function processes nodes in the instruction ready
   list and may transfer some start nodes into data ready list. */

void scheduler_state::process_instruction_ready_list (void)
{
  dependence_graph_node_t current_node;
  dependence_graph_node_t next_node;
  
  for (current_node = instruction_ready_list->head ();
       current_node != NULL
         && current_node->data_ready_time <= current_time;
       current_node = next_node)
    {
      next_node = current_node->next_list_node;
      instruction_ready_list->remove (current_node);
      data_ready_list->insert (current_node);
    }
}

/* The following function increments current time, changes the
   processor state reflecting increment time, and processes the
   instruction ready list. */

void scheduler_state::advance_time (void)
{
  current_time++;
  scheduler::advance_cycle ();
  process_instruction_ready_list ();
}

/* The following function appends given node into the result list and
   sets up start time of the node. */
void scheduler_state::append_node_to_result_list (dependence_graph_node_t node)
{
  assert (node != NULL);
  if (last_result_list_node != NULL)
    last_result_list_node->next_result_list_node = node;
  else
    assert (node == graph_top);
  last_result_list_node = node;
  node->next_result_list_node = NULL;
  node->start_time = current_time;
}

/* The following function appends given node into the result list and
   may insert some nodes into the instruction ready list. */
void scheduler_state::add_node_to_result_list (dependence_graph_node_t node)
{
  dependence_graph_node_t current_node;
  dependence_graph_arc_t current_arc;
  
  assert (node != NULL);
  append_node_to_result_list (node);
  for (current_arc = node->first_out_dependence_graph_arc;
       current_arc != NULL;
       current_arc = current_arc->next_out_dependence_graph_arc)
    {
      current_node = current_arc->to_dependence_graph_node;
      assert (current_node->unprocessed_arcs_number > 0);
      current_node->unprocessed_arcs_number--;
      if (current_node->data_ready_time
          < current_arc->dependence_delay + current_time)
        current_node->data_ready_time
          = current_arc->dependence_delay + current_time;
      if (current_node->unprocessed_arcs_number == 0)
        instruction_ready_list->insert (current_node);
    }
  process_instruction_ready_list ();
}


/* The following function finishes creation of data dependence graph
   for instruction scheduling and evaluation.  The function adds new
   arcs from DAG leaves to the finish node of the data dependence
   graph. */
void
scheduler_state::finish_forming_dependence_graph (void)
{
  dependence_graph_node_t current_node;

  assert (graph_top != NULL && graph_bottom != NULL);
  for (current_node = graph_top;
       current_node != NULL;
       current_node = current_node->next_dependence_graph_node)
    {
      if (graph_top != current_node
          && current_node->first_in_dependence_graph_arc == NULL)
        resources->add_dependence_graph_arc (graph_top, current_node, 0);
      if (current_node->first_out_dependence_graph_arc == NULL)
        resources->add_dependence_graph_arc
          (current_node, graph_bottom,
           (current_node->corresponding_instruction == NULL ? 0
            : scheduler::execution_duration (current_node
                                             ->corresponding_instruction)));
    }
}

/* The following function processes all resources of instruction of
   given node in order to create the data dependence graph. */
void scheduler_state::process_node_resources (dependence_graph_node_t node)
{
  scheduler::instruction_t current_instruction;
  unsigned int resource;
  unsigned int subresource;
  unsigned int marker;
  int number_of_resources;
  int current_resource_number;
  int usage_flag;

  current_instruction = node->corresponding_instruction;
  assert (current_instruction != NULL);
  number_of_resources = scheduler::resources_number (current_instruction);
  for (current_resource_number = 0;
       current_resource_number < number_of_resources;
       current_resource_number++)
    {
      scheduler::get_instruction_resource
        (current_instruction, current_resource_number,
         resource, subresource, marker, usage_flag);
      resources->process_resource (resource, subresource, marker,
                                   usage_flag, node);
    }
}

/* The following function builds data dependence graph neccessary to
   instruction scheduling and evaluation. */
void scheduler_state::build_data_dependence_graph (void)
{
  register scheduler::instruction_t current_instruction;
  register int current_order_number;
  dependence_graph_node_t current_node;
  dependence_graph_node_t previous_dependence_graph_node;

  resources = new resource_history (this);
  graph_top = ((dependence_graph_node_t)
               scheduler::allocate (sizeof (struct dependence_graph_node)));
  graph_top->first_out_dependence_graph_arc = NULL;
  graph_top->first_in_dependence_graph_arc = NULL;
  graph_top->node_estimation = 0;
  graph_top->order_number = 0;
  graph_top->next_dependence_graph_node = NULL;
  graph_top->corresponding_instruction = NULL; /* Start node. */
  previous_dependence_graph_node = graph_top;
  for (current_order_number = 1,
       current_instruction = scheduler::first_instruction ();
       current_instruction != NULL;
       current_instruction = scheduler::next_instruction (current_instruction),
       current_order_number++)
    {
      current_node
        = ((dependence_graph_node_t)
           scheduler::allocate (sizeof (struct dependence_graph_node)));
      current_node->corresponding_instruction = current_instruction;
      current_node->order_number = current_order_number;
      current_node->node_estimation = 0;
      current_node->first_out_dependence_graph_arc = NULL;
      current_node->first_in_dependence_graph_arc = NULL;
      process_node_resources (current_node);
      previous_dependence_graph_node->next_dependence_graph_node
        = current_node;
      previous_dependence_graph_node = current_node;
    }
  graph_bottom = ((dependence_graph_node_t)
                  scheduler::allocate (sizeof (struct dependence_graph_node)));
  graph_bottom->order_number = current_order_number;
  graph_bottom->node_estimation = 0;
  graph_bottom->corresponding_instruction = NULL; /* Finish node. */
  graph_bottom->first_out_dependence_graph_arc = NULL;
  graph_bottom->first_in_dependence_graph_arc = NULL;
  previous_dependence_graph_node->next_dependence_graph_node = graph_bottom;
  graph_bottom->next_dependence_graph_node = NULL;
  finish_forming_dependence_graph ();
  delete resources;
}

/* The following constructor of the scheduler state builds data
   dependence graph and creates to data ready and instruction ready
   lists in order to start scheduling and the evaluation of execution
   time of the basic block. */
scheduler_state::scheduler_state (void)
{
  dependence_graph_node_t current_node;
  dependence_graph_arc_t current_arc;

  build_data_dependence_graph ();
  data_ready_list = new list (ge_for_data_ready_list);
  instruction_ready_list = new list (ge_for_instruction_ready_list);
  current_time = 0;
  last_result_list_node = NULL;
  data_ready_list->insert (graph_top);
  for (current_node = graph_top;
       current_node != NULL;
           current_node = current_node->next_dependence_graph_node)
    {
      current_node->unprocessed_arcs_number = 0;
      current_node->data_ready_time = 0;
      for (current_arc = current_node->first_in_dependence_graph_arc;
           current_arc != NULL;
           current_arc = current_arc->next_in_dependence_graph_arc)
        current_node->unprocessed_arcs_number++;
    }
}

/* The following destructor is called after scheduling and the basic
   block evaluation. */
scheduler_state::~scheduler_state (void)
{
  delete data_ready_list;
  delete instruction_ready_list;
}

/* The following recursive function traverses data dependence graph
   with given start node and sets up levels of nodes which determine a
   critical path.  */

void scheduler_state::estimate_basic_block (dependence_graph_node_t start_node)
{
  dependence_graph_node_t current_node;
  dependence_graph_arc_t current_arc;
  int node_estimation;
  int max_node_estimation;
  int summary_estimation;

  if (start_node->node_estimation != 0)
    /* The node was already traversed. */
    return;
  max_node_estimation = 0;
  summary_estimation = 0;
  for (current_arc = start_node->first_out_dependence_graph_arc;
       current_arc != NULL;
       current_arc = current_arc->next_out_dependence_graph_arc)
    {
      current_node = current_arc->to_dependence_graph_node;
      estimate_basic_block (current_node);
      node_estimation
        = (current_node->node_estimation + current_arc->dependence_delay
           + (current_node->corresponding_instruction == NULL ? 0
              : scheduler::fetch_time (current_node
                                       ->corresponding_instruction)));
      if (max_node_estimation < node_estimation)
        max_node_estimation = node_estimation;
      summary_estimation += node_estimation;
    }
  start_node->node_estimation = max_node_estimation;
  start_node->summary_successors_node_estimation = summary_estimation;
}

/* The following function is major function which makes scheduling. */
void scheduler_state::schedule_estimated_basic_block (void)
{
  dependence_graph_node_t current_node;

  while (last_result_list_node != graph_bottom)
    {
      while (data_ready_list->head () == NULL || scheduler::is_dead_lock ())
        advance_time ();
      for (current_node = data_ready_list->head ();
           current_node != NULL;
           current_node = current_node->next_list_node)
        if (current_node->corresponding_instruction == NULL
            || scheduler::transition (current_node->corresponding_instruction))
          {
            data_ready_list->remove (current_node);
            add_node_to_result_list (current_node);
            break;
          }
      if (current_node == NULL)
        advance_time ();
    }
}

/* The following function is major function which evaluates execution
   time of the basic block. */
void scheduler_state::evaluate_estimated_basic_block (void)
{
  dependence_graph_node_t current_node;
  dependence_graph_arc_t current_arc;

  for (current_node = graph_top;
       current_node != NULL;
       current_node = current_node->next_dependence_graph_node)
    {
      for (;;)
        {
          while (current_time < current_node->data_ready_time
                 || scheduler::is_dead_lock ())
            {
              current_time++;
              scheduler::advance_cycle ();
            }
          if (current_node->corresponding_instruction == NULL
              || scheduler::transition (current_node
                                        ->corresponding_instruction))
            break;
          current_time++;
          scheduler::advance_cycle ();
        }
      append_node_to_result_list (current_node);
      for (current_arc = current_node->first_out_dependence_graph_arc;
           current_arc != NULL;
           current_arc = current_arc->next_out_dependence_graph_arc)
        if (current_arc->to_dependence_graph_node->data_ready_time
            < current_arc->dependence_delay + current_time)
          current_arc->to_dependence_graph_node->data_ready_time
            = current_arc->dependence_delay + current_time;
    }
}

/* The following macro value is number of cycles displayed before the
   diagram wraps. */

#define PIPE_LENGTH 20

/* The following function print profile of execution of instructions
   whose nodes are in the result list. */
void scheduler_state::print_result_list (void)
{
  int i;
  dependence_graph_node_t current_node;
  unsigned int issue_slot;
  unsigned int finish_time;
  unsigned int summary_execution_time;
  unsigned int expected_ideal_slot;
  char boundary_box[PIPE_LENGTH + 3];
  char exec_profile[PIPE_LENGTH + 3];
  
  if (graph_top->next_result_list_node == last_result_list_node)
    return;
  /* Form top scale of boundary box. */
  boundary_box [0] = '|';
  for (i = 0; i < PIPE_LENGTH; i++)
    boundary_box [i + 1] = ((char) i % 10) + '0';
  boundary_box [PIPE_LENGTH+1] = '|';
  boundary_box [PIPE_LENGTH+2] = '\0';
  /* Form left and right boundaries of the box. */
  exec_profile [0] = '|';
  exec_profile [PIPE_LENGTH + 1] = '|';
  exec_profile [PIPE_LENGTH + 2] = '\0';
  /* Print block header. */
  printf ("%s Start  Stop Number Instruction\n", boundary_box);
  expected_ideal_slot = 0;
  summary_execution_time = 0;
  for (current_node = graph_top->next_result_list_node;
       current_node != NULL && current_node != last_result_list_node;
       current_node = current_node->next_result_list_node)
    {
      memset (exec_profile + 1, ' ', PIPE_LENGTH);
      issue_slot = current_node->start_time;
      if (issue_slot != expected_ideal_slot)
        /* Mark stall time in the diagram. */
        for (i = expected_ideal_slot; i < (int) issue_slot; i++)
          exec_profile[(i % PIPE_LENGTH) + 1] = '.';
      exec_profile[(issue_slot % PIPE_LENGTH) + 1] = 'I';
      expected_ideal_slot = issue_slot;
      finish_time
        = (issue_slot
           + scheduler::execution_duration (current_node
                                            ->corresponding_instruction));
      if (finish_time != 0)
        finish_time--;
      if (summary_execution_time < finish_time)
        summary_execution_time = finish_time;
      /* Output the information for this instruction. */
      printf ("%s %5d %5d %6d ", exec_profile, issue_slot, finish_time,
              current_node->order_number);
      scheduler::print_instruction (current_node->corresponding_instruction);
      printf ("\n");
    }
  printf ("Summary execution time = %d\n", summary_execution_time);
}

/* The following function returns critical path length of the basic
   block. */
int
scheduler::critical_path_length (void)
{
  return state->critical_path_length;
}

/* The following function returns the first instruction from the
   instruction list.  The parameter contains start time (0, 1, ... )
   of the instruction. */
scheduler::instruction_t
scheduler::first_result_list_instruction (int &start_time)
{
  state->current_result_list_node = state->graph_top->next_result_list_node;
  assert (state->current_result_list_node != NULL
          && state->last_result_list_node != NULL);
  start_time = state->current_result_list_node->start_time;
  if (state->current_result_list_node == state->last_result_list_node)
    /* It is finish node */
    return NULL;
  else
    return (state->current_result_list_node->corresponding_instruction);
}

/* The following function returns the next instruction from the
   instruction list.  The parameter contains start time (0, 1, ...)
   of the instruction. */
scheduler::instruction_t
scheduler::next_result_list_instruction (int &start_time)
{
  assert (state->current_result_list_node != NULL
          && state->last_result_list_node != NULL);
  if (state->current_result_list_node != state->last_result_list_node)
    state->current_result_list_node
      = (state->current_result_list_node->next_result_list_node);
  start_time = state->current_result_list_node->start_time;
  if (state->current_result_list_node == state->last_result_list_node)
    /* It is finish node */
    return NULL;
  else
    return (state->current_result_list_node->corresponding_instruction);
}

/* The following constructor makes scheduling or evaluation of
   instruction executions dependending on the first parameter value.
   The execution profile of the result is printed if the second
   parameter value is TRUE. */
scheduler::scheduler (int only_evaluation_flag, int print_profile_flag)
{
  initiate_environment ();
  state = new scheduler_state ();
  state->estimate_basic_block (state->graph_top);
  state->critical_path_length = state->graph_top->node_estimation;
  if (only_evaluation_flag)
    state->evaluate_estimated_basic_block ();
  else
    state->schedule_estimated_basic_block ();
  if (print_profile_flag)
    state->print_result_list ();
}

/* Destructor of the scheduler. */
scheduler::~scheduler (void)
{
  delete state;
  finish_environment ();
}
