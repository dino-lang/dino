/*
   FILE NAME:   scheduler.h

   Copyright (C) 1997-2005 Vladimir Makarov.

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

   TITLE:       Include file of package for instruction scheduling inside
                basic block.

   DESCRIPTION: This header file contains ANSI C prototype definitions
                of the package functions and definitions of macros and
                types of the package and C++ class for instruction
                scheduler.

*/

#ifndef __SCHEDULER__
#define __SCHEDULER__

#ifndef __cplusplus

typedef void *instruction_t;

int critical_path_length (void);
instruction_t first_result_list_instruction (int *start_time);
instruction_t next_result_list_instruction (int *start_time);
void initiate_scheduler (int only_evaluation_flag, int print_profile_flag);
void finish_scheduler (void);

/* Environment of scheduler.  The following is called only by
   invariant part of the scheduler and must be implemented in a
   separate file. */

void *scheduler_allocate (size_t size);
void *scheduler_reallocate (void *memory, size_t size);
void scheduler_free (void *memory);

void scheduler_print_instruction (instruction_t instruction);
instruction_t scheduler_first_instruction (void);
instruction_t scheduler_next_instruction (instruction_t instruction);
int scheduler_fetch_time (instruction_t instruction);
int scheduler_resources_number (instruction_t instruction);
void scheduler_get_instruction_resource (instruction_t instruction,
                                         int number,
                                         unsigned int *resource,
                                         unsigned int *subresource,
                                         unsigned int *marker,
                                         int *usage_flag);

int scheduler_dependence_delay (instruction_t predecessor, int producer_flag_1,
                                instruction_t successor, int producer_flag_2,
                                unsigned int resource,
                                unsigned int subresource_1,
                                unsigned int subresource_2,
                                unsigned int marker_1, unsigned int marker_2);
int scheduler_execution_duration (instruction_t instruction);
int scheduler_transition (instruction_t instruction);
int scheduler_is_dead_lock (void);
void scheduler_advance_cycle (void);
void scheduler_initiate_environment (void);
void scheduler_finish_environment (void);



#else /* #ifndef __cplusplus */


#include <stddef.h>

class scheduler
{
  friend class scheduler_state;
  friend class resource_occurrence_pool;
  friend class resource_history;
  friend class resource_heads_vector;
  friend class list;

  class scheduler_state *state;

public:

  typedef void *instruction_t;

  inline void *operator new (size_t size)
    {
      return scheduler::allocate (size);
    }
  
  inline void operator delete (void *memory)
    {
      scheduler::free (memory);
    }
 
  int critical_path_length (void);
  instruction_t first_result_list_instruction (int &start_time);
  instruction_t next_result_list_instruction (int &start_time);
  scheduler (int only_evaluation_flag, int print_profile_flag);
  ~scheduler (void);

private:

  /* Environment of scheduler.  The following is called only by
     invariant part of the scheduler and must be implemented in a
     separate file. */


  static void *allocate (size_t size);
  static void *reallocate (void *memory, size_t size);
  static void free (void *memory);

  static void print_instruction (instruction_t instruction);
  static instruction_t first_instruction (void);
  static instruction_t next_instruction (instruction_t instruction);
  static int fetch_time (instruction_t instruction);
  static int resources_number (instruction_t instruction);
  static void get_instruction_resource (instruction_t instruction, int number,
                                        unsigned int &resource,
                                        unsigned int &subresource,
                                        unsigned int &marker,
                                        int &usage_flag);
  static int dependence_delay (instruction_t predecessor, int producer_flag_1,
                               instruction_t successor, int producer_flag_2,
                               unsigned int resource,
                               unsigned int subresource_1,
                               unsigned int subresource_2,
                               unsigned int marker_1, unsigned int marker_2);
  static int execution_duration (instruction_t instruction);
  static int transition (instruction_t instruction);
  static int is_dead_lock (void);
  static void advance_cycle (void);
  static void initiate_environment (void);
  static void finish_environment (void);
};

#endif /* #ifndef __cplusplus */
#endif /* #ifndef __SCHEDULER__ */
