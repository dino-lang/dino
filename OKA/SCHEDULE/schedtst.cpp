/*
   FILE NAME:   schedtst.c

   Copyright (C) 1997-2002 Vladimir Makarov.

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

   TITLE:       Another part of instruction scheduler inside basic blocks.

   DESCRIPTION: This file contains the rest part of the scheduler
       (`schedule.c') and the driver od scheduler needed for testing
       the scheduler.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of the scheduler.
*/

#include <assert.h>
#include <stdio.h>
#include "test.h"
#include "allocate.h"
#include "schedule.h"

static int current_instruction;

static OKA_chip chip;

struct instruction
{
  unsigned int code;
  unsigned int ra, rb, rc;
};

typedef struct instruction *instruction_t;

void *scheduler::allocate (size_t size)
{        
  return allocate::malloc (size);
}

void *scheduler::reallocate (void *memory, size_t size)
{
  return allocate::realloc (memory, size);
}

void scheduler::free (void *memory)
{
  allocate::free (memory);
}

static void print_code (instruction_t instruction)
{
  switch (instruction->code)
    {
    case OKA_ADD:
      printf ("ADD");
      break;
    case OKA_MUL:
      printf ("MUL");
      break;
    case OKA_DIV:
      printf ("DIV");
      break;
    case OKA_FADD:
      printf ("FADD");
      break;
    case OKA_FMUL:
      printf ("FMUL");
      break;
    case OKA_FDIV:
      printf ("FDIV");
      break;
    default:
      assert (0);
    }
}

static void print_operands (instruction_t instruction)
{
  printf ("\t#%d, #%d, #%d", instruction->ra, instruction->rb,
          instruction->rc);
}

void print_instruction (instruction_t instruction)
{
  print_code (instruction);
  print_operands (instruction);
}

void scheduler::print_instruction (instruction_t instruction)
{
  ::print_instruction ((::instruction_t) instruction);
}

static struct instruction instr [] =
{
  {OKA_ADD, 1, 2, 3},
  {OKA_MUL, 5, 3, 4},
  {OKA_DIV, 1, 2, 5},
  {OKA_DIV, 6, 2, 3},
  {OKA_MUL, 5, 5, 5},
  {OKA_FMUL, 11, 12, 13},
  {OKA_FADD, 13, 13, 14},
  {OKA_FDIV, 11, 12, 13}
};

scheduler::instruction_t scheduler::first_instruction (void)
{
  current_instruction = 0;
  return &instr [current_instruction];
}

scheduler::instruction_t scheduler::next_instruction (instruction_t instruction)
{
  assert (((::instruction_t) instruction)->code < OKA__ADVANCE_CYCLE);
  current_instruction++;
  if (current_instruction >= sizeof (instr) / sizeof (instr[0]))
    return NULL;
  else
    return &instr [current_instruction];
}

int scheduler::fetch_time (instruction_t instruction)
{
  assert (((::instruction_t) instruction)->code < OKA__ADVANCE_CYCLE);
  return 0;
}

int scheduler::resources_number (instruction_t instruction)
{
  assert (((::instruction_t) instruction)->code < OKA__ADVANCE_CYCLE);
  return 3;
}

void scheduler::get_instruction_resource (instruction_t instruction,
                                          int number, unsigned int &resource,
                                          unsigned int &subresource,
                                          unsigned int &marker,
                                          int &usage_flag)
{
  assert (((::instruction_t) instruction)->code < OKA__ADVANCE_CYCLE);
  assert (number >= 0 && number < 3);
  usage_flag = (number == 0 ? 0 /*FALSE*/ : 1 /*TRUE*/);
  subresource = 0;
  marker = 0; /* Ignore */
  if (number == 0)
    resource = ((::instruction_t) instruction)->ra;
  else if (number == 1)
    resource = ((::instruction_t) instruction)->rb;
  else
    resource = ((::instruction_t) instruction)->rc;
}

static int execution_time [6] =
{
  1,  /* ADD */
  19,  /* MULL */
  33, /* DIV */
  3,  /* FADD */
  3,  /* FMUL */
  28  /* FDIV */
};

int
scheduler::execution_duration (instruction_t instruction)
{
  assert (((::instruction_t) instruction)->code < OKA__ADVANCE_CYCLE);
  return execution_time [((::instruction_t) instruction)->code];
}

static int def_use_delay [6][6] =
{
  {1,1,1,1,1,1}, /* ADD: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {19,19,19,19,19,19}, /* MULL: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {33,33,33,33,33,33}, /* DIV: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {3,3,3,3,3,3}, /* FADD: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {3,3,3,3,3,3}, /* FMUL: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {28,28,28,28,28,28} /* FDIV: ADD, MULL, DIV, FADD, FMUL, FDIV */
};

static int def_def_delay [6][6] =
{
  {1,1,1,1,1,1}, /* ADD: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {19,1,0,17,17,0}, /* MULL: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {33,15,1,31,31,6}, /* DIV: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {3,0,0,1,1,0}, /* FADD: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {3,0,0,1,1,0}, /* FMUL: ADD, MULL, DIV, FADD, FMUL, FDIV */
  {28,10,0,26,26,1} /* FDIV: ADD, MULL, DIV, FADD, FMUL, FDIV */
};

int scheduler::dependence_delay (instruction_t predecessor,
                                 int producer_flag_1,
                                 instruction_t successor, int producer_flag_2,
                                 unsigned int resource,
                                 unsigned int subresource_1,
                                 unsigned int subresource_2,
                                 unsigned int marker_1, unsigned int marker_2)
{
  assert (producer_flag_1 || producer_flag_2 || predecessor != NULL);
  assert (((::instruction_t) predecessor)->code < OKA__ADVANCE_CYCLE);
  assert (((::instruction_t) successor)->code < OKA__ADVANCE_CYCLE);
  if (!producer_flag_1 && producer_flag_2)
    return 0; /* use-def */
  else if (!producer_flag_2)
    return def_use_delay [((::instruction_t) predecessor)->code]
                         [((::instruction_t) successor)->code];
  else
    return def_def_delay [((::instruction_t) predecessor)->code]
                         [((::instruction_t) successor)->code];
}

int scheduler::transition (instruction_t instruction)
{
  assert (((::instruction_t) instruction)->code < OKA__ADVANCE_CYCLE);
  return chip.OKA_transition (((::instruction_t) instruction)->code);
}

int scheduler::is_dead_lock (void)
{
  return chip.OKA_is_dead_lock ();
}

void scheduler::advance_cycle (void)
{
  chip.OKA_transition (OKA__ADVANCE_CYCLE);
}

void scheduler::initiate_environment (void)
{
  chip.OKA_reset ();
}

void scheduler::finish_environment (void)
{
}

void main ()
{
  scheduler *sched;

  printf ("******ORIGINAL\n");
  sched = new scheduler (1, 1);
  printf ("critical path length = %d\n", sched->critical_path_length ());
  delete sched;
  printf ("\n******NEW\n");
  sched = new scheduler (0, 1);
  printf ("critical path length = %d\n", sched->critical_path_length ());
  delete sched;
  exit (0);
}
