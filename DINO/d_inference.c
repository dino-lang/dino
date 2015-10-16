/*
   Copyright (C) 2015 Vladimir Makarov.

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

/* The file implements inference of some operands of Bcode and
   produces type specialized Bcode.  We process only global
   (top-level) and non-public local variables which are not accessed
   by lvar(v) Bcode.  All other variables escape our analysis.  The
   algorithm consists from following major steps:

   1. Building *CFG*: insns (contains different info about
      corresponding Bcode), BBs, and edges connecting BBs.  CFG for
      our taks is slightly different from the usual CFG.  We exclude
      edges to exception blocks from the middle of the try blocks.
      Instead, we have two destinations from try block end which are
      the usual block destination and the 1st catch block.  And we
      have two destinations of the catch block which are the usual
      try-block destination and the next catch block.  An immediate
      call finishes BB and the BB has one more destination which is
      the BB of the corresponding fblock.  We also add an edge from
      the BB of the fbend to the BB containing the next insn of the
      call.

   2. Calculating insn *result availability* by solving a forward
      data-flow analysis problem on CFG.  An insn result is available
      at given place, iff there is CFG path on which the insn occurs
      and after the occurrence the insn result is not killed by some
      other insn.  Availability is calculated by using
      operand/variable *place*.  The place of global vars starts from
      zero, and after that places of local vars starts according to
      their var numbers (block stack slots).  We have additional edges
      in CFG which produces paths never corresponding the real
      executions paths (see the 1st step above), e.g. we call fblock
      and can return after another call of the block or we can go from
      the finishing try-block to its exceptions.  So the availability
      sets are more than it should be in reality as some available
      insns are not killed as it should be.  It is ok as it makes type
      analysis conservative but safe.  Variables accessible through
      Bcode lvar(v) escape analysis, e.g. variables used by function
      closures or non-local and non-global variables accessble from
      the catch blocks, as it is hard to follows their life (we need
      to follows insns producing inputs of lvar(v) for this).  Input
      availability for fblock and output availability for fbend are
      always constrained by global vars only.  This is to prevent
      propagation of local definitions in fblock outside the fblock
      and local definitions in call block to place after other calls
      of the fblock.

   3. Using the calculated availability, we build def-use chains
      connecting insn outputs and inputs.  We connect immediate call
      operands and the corresponding blocks for inter-procedural
      analysis only when the function is always called through an
      immediate calls.  We connect Bcode ret/leave to the
      corresponding fbend and the fbend to immediate calls to
      propagate type of the call result.

   4. Calculating possible types of insn inputs and outputs (results)
      by solving another forward data-flow problem on graph of def-use
      web.

   5. Changing Bcode on type specialized one using the calculated
      types.

  So we do inter-procedural analysis in whole program scope.
  Top-level variables analysis is practically CFG-insensitive (as we
  have more paths in CFG than possible -- see the 1st step above),
  other variables (including temporary ones) analysis is CFG-sensitive
  -- we can find types even if a variable changes its type.  We could
  use vdecls of the global variables as a part of def-use web and
  don't add the new CFG-paths (that would make top-level variables
  analysis fully CFG-insensitive), but the analysis would be less
  accurate and we would need another pass to recognize usage of
  undefined values of globals (ignoring that would make top-level
  variable analysis completely useless).  A lot of things escape our
  analysis (e.g. public class variables, vector/table element types,
  non-primitive types etc) and some improving can be done here.  */

#include "d_common.h"
#include "spset.h"
#include "cspset.h"
#include "d_bc.h"

/* Types used for type inference.  As we don't need slice for better
   error detection (slice can not be var value and can be checked on
   context level), when slice type is possible we use TP_varying.  */
typedef enum
{
  TP_unknown, /* bottom of the data flow problem  */
  TP_int,
  TP_char,
  TP_float,
  TP_long,
  TP_vec,
  TP_tab,
  TP_undef,
  TP_varying /* top of the data flow problem  */
} type_t;

static const char *type_name[] = {"unknown", "int", "char", "float", "long",
				  "vec", "tab", "undef", "varying"};

typedef struct cfg_bb *cfg_bb_t;
typedef struct cfg_edge *cfg_edge_t;
typedef struct block *block_t;
typedef struct df_insn *df_insn_t;
typedef struct def_use *def_use_t;

/* Basic block of control-flow graph.  */
struct cfg_bb
{
  int temp;
  /* Index and post-order inverted number of BB.  */
  int index, order;
  /* first/last insns of BB: */
  df_insn_t head, tail;
  /* in/out CFG edges of BB */
  cfg_edge_t srcs, dests;
  /* Available definitions at the beginning/end of BB.  */
  cspset_t avail_in, avail_out;
  /* Definitions became availaible at the end of BB.  */
  cspset_t avail_gen;
  /* Places (see comments for function get_insn_op_place) which are
     results of insns in BB.  */
  cspset_t res_places;
};

/* Control flow graph edge.  */
struct cfg_edge
{
  /* source/destination BB.  */
  cfg_bb_t dest, src;
  /* Next edge correspondingly with the same dest and src.  */
  struct cfg_edge *next_src, *next_dest;
};

/* The following represents BC block.  */
struct block
{
  /* Corresponding BC block.  */
  BC_node_t bc_block;
  /* True if the block contains fblock which is accessed through BC
     fun/class.  */
  int fun_p;
  /* Insn corresponding to bend of given fblock.  */
  df_insn_t bend_insn;
  /* Index is local var num, the element is corresponding value of
     flag that variable escapes from the analysis.  */
  char escape_p[1];
};

struct df_insn
{
  int temp;
  /* Def number of the first definition of the insn.  */
  int defs_start;
  /* Index and post-order inverted number of the insn.  */
  int index, order;
  /* Corresponding bcode.  */
  BC_node_t bc;
  /* Corresponding BC block and the map.  */
  block_t block;
  /* Basic block to which insn belongs to.  */
  cfg_bb_t bb;
  /* def/use chains of the node:  */
  def_use_t defs, uses;
  /* First we have types for outputs (at least one even if the insn
     has no output operands), then inputs.  */
  type_t types[1];
};

/* An edge in def-use graph.  */
struct def_use
{
  /* Number of the output operand in insn def and the input operand in
     insn use.  */
  int def_num, use_num;
  /* Destination/source of the def-use edge.  */
  df_insn_t def, use;
  /* Next edge with the same definition and use insns.  */
  def_use_t next_def, next_use;
};

/* The following describes a graph for a data-flow problem.  */
/* Either bb or insn.  */
typedef void *node_t;
/* Either cfg edge or def-use edge.  */
typedef void *edge_t;

/* Problem for data-flow solution functions.  */
struct problem
{
  /* index of node starting with 0.  */
  int (*get_index) (node_t);
  /* id for external representation.  */
  int (*get_id) (node_t);
  /* graph access functions.  */
  edge_t (*src_edges) (node_t), (*dest_edges) (node_t);
  edge_t (*next_src_edge) (edge_t), (*next_dest_edge) (edge_t);
  node_t (*edge_src) (edge_t), (*edge_dest) (edge_t);
  /* Tick access functions.  */
  int (*get_visit_tick) (node_t);
  void (*set_visit_tick) (node_t, int);
  /* Order number access functions.  */
  int (*get_order) (node_t);
  void (*set_order) (node_t, int);
  /* Confluence and transfer functions.  */
  void (*confl0) (node_t);
  int (*confl) (edge_t);
  int (*transf) (node_t);
};

/* Access functions for control flow graph: */
static int get_bb_index (node_t bb) { return ((cfg_bb_t) bb)->index; }
static edge_t bb_srcs (node_t bb) { return ((cfg_bb_t) bb)->srcs; }
static edge_t bb_dests (node_t bb) { return ((cfg_bb_t) bb)->dests; }
static int get_bb_tick (node_t bb) { return ((cfg_bb_t) bb)->temp; }
static void set_bb_tick (node_t bb, int tick) { ((cfg_bb_t) bb)->temp = tick; }
static int get_bb_order (node_t bb) { return ((cfg_bb_t) bb)->order; }
static void set_bb_order (node_t bb, int order) { ((cfg_bb_t) bb)->order = order; }
static node_t cfg_edge_dest (edge_t e) { return ((cfg_edge_t) e)->dest; }
static node_t cfg_edge_src (edge_t e) { return ((cfg_edge_t) e)->src; }
static edge_t next_src_cfg_edge (edge_t e) { return ((cfg_edge_t) e)->next_src; }
static edge_t next_dest_cfg_edge (edge_t e) { return ((cfg_edge_t) e)->next_dest; }

/* Access functions for def-use graph: */
static int get_insn_index (node_t insn) { return ((df_insn_t) insn)->index; }
static int get_insn_id (node_t insn) { return BC_idn (BC_info (((df_insn_t) insn)->bc)); }
static edge_t insn_uses (node_t insn) { return ((df_insn_t) insn)->uses; }
static edge_t insn_defs (node_t insn) { return ((df_insn_t) insn)->defs; }
static int get_insn_tick (node_t insn) { return ((df_insn_t) insn)->temp; }
static void set_insn_tick (node_t insn, int tick) { ((df_insn_t) insn)->temp = tick; }
static int get_insn_order (node_t insn) { return ((df_insn_t) insn)->order; }
static void set_insn_order (node_t insn, int order) { ((df_insn_t) insn)->order = order; }
static node_t edge_def (edge_t e) { return ((def_use_t) e)->def; }
static node_t edge_use (edge_t e) { return ((def_use_t) e)->use; }
static edge_t next_def_edge (edge_t e) { return ((def_use_t) e)->next_def; }
static edge_t next_use_edge (edge_t e) { return ((def_use_t) e)->next_use; }



/* Allocation data and function for the pass.  */

static os_t pass_os;

static void
pass_alloc_init (void)
{
  OS_CREATE (pass_os, 0);
}

static void *
pass_alloc (size_t size)
{
  void *res;

  OS_TOP_EXPAND (pass_os, size);
  res = OS_TOP_BEGIN (pass_os);
  OS_TOP_FINISH (pass_os);
  return res;
}

static void
pass_alloc_finish (void)
{
  OS_DELETE (pass_os);
}



/* Pages for calculation of post-inverted order of nodes  */

/* All nodes placed according their postorder inverted number.  */
static node_t *nodes_in_postorder_inverted;

/* Set saying what nodes were added to the stack used for
   post-inverted order calculation.  */
static spset_t in_postorder_stack;
/* The stack used for post-inverted order calculation (including
   reachable nodes).  */
static node_t *postorder_stack;

/* The current number of stack elements.  */
static int postorder_stack_bound;

/* Set of reachable nodes from start nodes.  */
static spset_t reachable_nodes;

/* Add TOP and all nodes reachable from it to the reachable node
   set.  */
static void
add_reachable_nodes (struct problem *problem, node_t top)
{
  node_t node;
  edge_t e;
  int saved_bound = postorder_stack_bound;
  int index = problem->get_index (top);

  assert (! spset_in_p (&reachable_nodes, index));
  /* Here we use the free slots of the postorder stack for a stack
     processing reachable nodes.  */
  postorder_stack[postorder_stack_bound++] = top;
  while (postorder_stack_bound > saved_bound)
    {
      node = postorder_stack[--postorder_stack_bound];
      index = problem->get_index (node);
      spset_insert (&reachable_nodes, index);
      for (e = problem->src_edges (node);
	   e != NULL;
	   e = problem->next_src_edge (e))
	{
	  node = problem->edge_src (e);
	  index = problem->get_index (node);
	  if (spset_in_p (&reachable_nodes, index)) 
	    continue;
	  postorder_stack[postorder_stack_bound++] = node;
	}
    }
  assert (saved_bound == postorder_stack_bound);
}

/* Add NODE to process if it is not processed yet.  */
static inline void
add_node_to_consider (struct problem *problem, node_t node)
{
  int index = problem->get_index (node);

  if (spset_in_p (&in_postorder_stack, index))
    return;
  postorder_stack[postorder_stack_bound++] = node;
  spset_insert (&in_postorder_stack, index);
}

/* Set up NODES_IN_POSTORDER_INVERTED for PROBLEM with NODES_NUM
   NODES.  Use the node visit tick as visited flag.  */
static void
calculate_postorder_inverted (struct problem *problem,
			      node_t *nodes, int nodes_num)
{
  int i, index, last_place;
  node_t node, top;

  MALLOC (postorder_stack, sizeof (node_t) * nodes_num);
  spset_init (&in_postorder_stack, nodes_num);
  spset_init (&reachable_nodes, nodes_num);
  for (postorder_stack_bound = 0, i = nodes_num - 1; i >= 0; i--)
    {
      node = nodes[i];
      problem->set_visit_tick (node, FALSE); /* Initiate visit flag */
      if (problem->dest_edges (node) == NULL)
	{
	  add_reachable_nodes (problem, node);
	  add_node_to_consider (problem, node);
	}
    }
  /* Add missed loop nodes: */
  for (i = nodes_num - 1; i >= 0; i--)
    {
      node = nodes[i];
      index = problem->get_index (node);
      if (spset_in_p (&reachable_nodes, index))
	continue;
      add_reachable_nodes (problem, node);
      add_node_to_consider (problem, node);
    }
  spset_finish (&reachable_nodes);
  last_place = 0;
  while (postorder_stack_bound > 0)
    {
      top = postorder_stack[postorder_stack_bound - 1];
      if (problem->get_visit_tick (top))
	{
	  postorder_stack_bound--;
	  assert (last_place < nodes_num);
	  problem->set_order (top, last_place);
	  nodes_in_postorder_inverted[last_place++] = top;
	}
      else
	{
	  edge_t e;

	  problem->set_visit_tick (top, TRUE);
	  for (e = problem->src_edges (top);
	       e != NULL;
	       e = problem->next_src_edge (e))
	    {
	      node = problem->edge_src (e);
	      add_node_to_consider (problem, node);
	    }
	  assert (postorder_stack_bound <= nodes_num);
	}
    }
#if 0
  for (i = 0; i < nodes_num; i++)
    assert (problem->get_order (nodes[i]) >= 0);
#endif
  assert (last_place == nodes_num);
  spset_finish (&in_postorder_stack);
  FREE (postorder_stack);
}



/* Page for solution of forward data-flow problems.  */

static int *last_visit_tick;

static int
propagate_forward (struct problem *problem, node_t node, spset_t *pending, int tick)
{
  edge_t e;
  node_t src;
  int changed_p = !tick;

  /*  Calculate .  */
  if ((e = problem->src_edges (node)) == NULL)
    {
      if (problem->confl0 != NULL)
	problem->confl0 (node);
    }
  else
    for (; e != NULL; e = problem->next_src_edge (e))
      {
	src = problem->edge_src (e);
	if (tick <= problem->get_visit_tick (src))
	  changed_p |= problem->confl (e);
      }
  if (changed_p && problem->transf (node))
    {
      for (e = problem->dest_edges (node); e != NULL; e = problem->next_dest_edge (e))
	spset_insert (pending, problem->get_order (problem->edge_dest (e)));
      return TRUE;
    }
  return changed_p;
}

static void
forward_dataflow (struct problem *problem, node_t *nodes, int nodes_num,
		  int print_order_p)
{
  int i, el, tick, changed_p;
  node_t node;
  spset_t pending, worklist;
  spset_iterator_t si;

  MALLOC (nodes_in_postorder_inverted, sizeof (node_t) * nodes_num);
  MALLOC (last_visit_tick, sizeof (int) * nodes_num);
  calculate_postorder_inverted (problem, nodes, nodes_num);
  if (dump_flag && print_order_p)
    {
      for (i = 0; i < nodes_num; i++)
	{
	  if (i % 10 == 0)
	    printf ("\n// ");
	  printf (" %5d", problem->get_id (nodes_in_postorder_inverted[i]));
	}
      printf ("\n");
    }
  spset_init (&pending, nodes_num);
  spset_init (&worklist, nodes_num);
  for (i = 0; i < nodes_num; i++)
    {
      /* Add all nodes to the worklist.  */
      spset_insert (&pending, i);
      last_visit_tick[i] = 0;
      problem->set_visit_tick (nodes[i], 0);
    }
  while (spset_cardinality (&pending) != 0)
    {
      /* Swap pending and worklist. */
      spset_swap (&pending, &worklist);
      EXECUTE_FOR_EACH_SPSET_ELEM (&worklist, el, si)
	{
	  spset_remove (&pending, el);
	  node = nodes_in_postorder_inverted[el];
	  tick = last_visit_tick[el];
	  changed_p = propagate_forward (problem, node,
					 &pending, last_visit_tick[el]);
	  last_visit_tick[el] = ++tick;
	  if (changed_p)
	    problem->set_visit_tick (node, tick);
	}
      spset_clear (&worklist);
    }
  spset_finish (&pending);
  spset_finish (&worklist);
  FREE (last_visit_tick);
  FREE (nodes_in_postorder_inverted);
}



/* Top level block.  */
static block_t tl_block;

/* Start place of vars of BLOCK.  */
static int inline
get_block_place_start (block_t block)
{
  return block == tl_block ? 0 : BC_vars_num (tl_block->bc_block);
}

static const int NO_MORE_REAL_OPERAND = -1;
static const int NO_MORE_OPERAND = -2;

/* Return place (based operand number) of NOP result (if RESULT_P) or
   input of INSN.  Global (top level) places start with 0, then local
   places start after the global ones.  Return negative value
   NO_MORE_OPERAND if there are no more results/inputs starting with
   NOP.  Return negative value NO_MORE_REAL_OPERAND if the rest of
   inputs or outputs should be ignored.  */
static int
get_insn_op_place (df_insn_t insn, int nop, int result_p)
{
  BC_node_t bc = insn->bc;
  int res;

  switch (BC_NODE_MODE (bc))
    {
    case BC_NM_b: case BC_NM_waitend:
    case BC_NM_out: case BC_NM_stpop: case BC_NM_bend:
      return NO_MORE_OPERAND;
      break;
    case BC_NM_fbend:
      return (nop == 0 ? NO_MORE_REAL_OPERAND : NO_MORE_OPERAND);
    case BC_NM_btdef: case BC_NM_bt: case BC_NM_bf: case BC_NM_bfni:
    case BC_NM_ibt: case BC_NM_ibf: case BC_NM_ibfni:
      if (result_p)
	return NO_MORE_OPERAND;
      else if (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_leave:
      return result_p && nop == 0 ? NO_MORE_REAL_OPERAND : NO_MORE_OPERAND;
    case BC_NM_ret:
      if (result_p)
	return nop == 0 ? NO_MORE_REAL_OPERAND : NO_MORE_OPERAND;
      else if (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_stvt: case BC_NM_sts: case BC_NM_ste:
    case BC_NM_stvtu: case BC_NM_stsu: case BC_NM_steu:
      if (result_p)
	return NO_MORE_OPERAND;
      else if (nop == 0)
	res = BC_op1 (bc);
      else if (nop == 1)
	res = BC_op2 (bc);
      else if (nop == 2)
	res = BC_op3 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_slst:
      if (result_p)
	return NO_MORE_OPERAND;
      else if (nop == 0)
	res = BC_op1 (bc);
      else if (nop == 1)
	res = BC_op3 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_mult_st: case BC_NM_div_st: case BC_NM_mod_st:
    case BC_NM_add_st: case BC_NM_sub_st: case BC_NM_concat_st:
    case BC_NM_lsh_st: case BC_NM_rsh_st: case BC_NM_ash_st:
    case BC_NM_and_st: case BC_NM_xor_st: case BC_NM_or_st:
    case BC_NM_mult_slst: case BC_NM_div_slst: case BC_NM_mod_slst:
    case BC_NM_add_slst: case BC_NM_sub_slst: case BC_NM_concat_slst:
    case BC_NM_lsh_slst: case BC_NM_rsh_slst: case BC_NM_ash_slst:
    case BC_NM_and_slst: case BC_NM_xor_slst: case BC_NM_or_slst:
      if (result_p)
	return NO_MORE_OPERAND;
      else if (nop == 0)
	res = BC_op3 (bc);
      else if (nop == 1)
	res = BC_op4 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_throw: case BC_NM_wait:
      if (result_p)
	return NO_MORE_OPERAND;
      else if (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_except:
      if (result_p)
	return NO_MORE_OPERAND;
      else if (nop == 0)
	res = BC_op1 (bc);
      else if (nop == 1)
	res = BC_op2 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_ldnil: case BC_NM_ldthis:
    case BC_NM_ldi: case BC_NM_ldbi: case BC_NM_ldch: case BC_NM_ldtp:
    case BC_NM_ldl: case BC_NM_ldf: case BC_NM_lds:
    case BC_NM_var: case BC_NM_evar:
    case BC_NM_fun: case BC_NM_efun: case BC_NM_class:
      if (! result_p)
	return NO_MORE_OPERAND;
      if  (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_ofun: case BC_NM_oclass:
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_op1 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op2 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_flat:
      if  (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_ind:
    case BC_NM_fmtvecof: case BC_NM_in:
    case BC_NM_or: case BC_NM_xor:
    case BC_NM_and: case BC_NM_eq: case BC_NM_ne:
    case BC_NM_id: case BC_NM_unid:
    case BC_NM_lt: case BC_NM_gt: case BC_NM_le: case BC_NM_ge:
    case BC_NM_lsh: case BC_NM_rsh: case BC_NM_ash:
    case BC_NM_concat: case BC_NM_add: case BC_NM_sub:
    case BC_NM_mult: case BC_NM_div: case BC_NM_mod:
    case BC_NM_ior: case BC_NM_ixor:
    case BC_NM_iand: case BC_NM_ieq: case BC_NM_ine:
    case BC_NM_ilt: case BC_NM_igt: case BC_NM_ile: case BC_NM_ige:
    case BC_NM_ilsh: case BC_NM_irsh: case BC_NM_iash:
    case BC_NM_iadd: case BC_NM_isub:
    case BC_NM_imult: case BC_NM_idiv: case BC_NM_imod:
    case BC_NM_fadd: case BC_NM_fsub:
    case BC_NM_fmult: case BC_NM_fdiv: case BC_NM_fmodop:
    case BC_NM_ifadd: case BC_NM_ifsub: case BC_NM_fisub:
    case BC_NM_ifmult: case BC_NM_ifdiv: case BC_NM_fidiv:
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_op1 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op2 (bc);
      else if (nop == 1)
	res = BC_op3 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_ind2:
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_op1 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op2 (bc);
      else if (nop == 1)
	res = BC_op3 (bc);
      else if (nop == 2)
	res = BC_op4 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_sl:
    case BC_NM_fld:
    case BC_NM_ovfld:
    case BC_NM_const: case BC_NM_new:
    case BC_NM_not: case BC_NM_plus: case BC_NM_minus: case BC_NM_bnot:
    case BC_NM_inot: case BC_NM_iplus: case BC_NM_iminus: case BC_NM_ibnot:
    case BC_NM_fplus: case BC_NM_fminus:
    case BC_NM_length:
    case BC_NM_tpof: case BC_NM_chof: case BC_NM_iof:
    case BC_NM_lof: case BC_NM_fof: case BC_NM_vecof: case BC_NM_tabof:
    case BC_NM_fold_add: case BC_NM_fold_mult: case BC_NM_fold_and:
    case BC_NM_fold_or: case BC_NM_fold_xor:
    case BC_NM_addi: case BC_NM_multi: case BC_NM_eqi: case BC_NM_nei:
    case BC_NM_lti: case BC_NM_gti: case BC_NM_lei: case BC_NM_gei:
    case BC_NM_iaddi: case BC_NM_imulti: case BC_NM_ieqi: case BC_NM_inei:
    case BC_NM_ilti: case BC_NM_igti: case BC_NM_ilei: case BC_NM_igei:
    case BC_NM_faddi: case BC_NM_fmulti:
    case BC_NM_lconv:
    case BC_NM_move: case BC_NM_imove: case BC_NM_fmove:
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_op1 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op2 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_madd:
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_op1 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op2 (bc);
      else if (nop == 1)
	res = BC_op3 (bc);
      else if (nop == 2)
	res = BC_op4 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_vec: case BC_NM_tab:
      /* They can have many inputs, there is a little sense to track
	 them.  */
      if (! result_p)
	return NO_MORE_OPERAND;
      else if  (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_call: case BC_NM_tcall: case BC_NM_mcall:
    case BC_NM_omcall: case BC_NM_omicall: case BC_NM_omitcall:
      if (result_p && nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_ibcall: case BC_NM_icall: case BC_NM_itcall:
    case BC_NM_ticall: case BC_NM_titcall: case BC_NM_cicall: case BC_NM_citcall:
      if (result_p && nop == 0)
	res = BC_op1 (bc);
      else
	return ! result_p && nop == 0 ? NO_MORE_REAL_OPERAND : NO_MORE_OPERAND;
      break;
    case BC_NM_lvar: case BC_NM_lvarv: case BC_NM_levar: case BC_NM_levarv:
    case BC_NM_lfld: case BC_NM_lfldv: case BC_NM_lslv:
    case BC_NM_lovfld: case BC_NM_lovfldv:
      if (! result_p)
	return NO_MORE_OPERAND;
      else if  (nop == 0)
	res = BC_op1 (bc);
      else if  (nop == 1)
	res = BC_op1 (bc) + 1;
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_brts: case BC_NM_brfs: 
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_res (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_bteq: case BC_NM_btne: case BC_NM_btge:
    case BC_NM_btlt: case BC_NM_btle: case BC_NM_btgt:
    case BC_NM_ibteq: case BC_NM_ibtne: case BC_NM_ibtge:
    case BC_NM_ibtlt: case BC_NM_ibtle: case BC_NM_ibtgt:
    case BC_NM_fbteq: case BC_NM_fbtne: case BC_NM_fbtge:
    case BC_NM_fbtlt: case BC_NM_fbtle: case BC_NM_fbtgt:
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_bcmp_res (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op1 (bc);
      else if (nop == 1)
	res = BC_bcmp_op2 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_bteqi: case BC_NM_btnei: case BC_NM_btgei:
    case BC_NM_btlti: case BC_NM_btlei: case BC_NM_btgti:
    case BC_NM_ibteqi: case BC_NM_ibtnei: case BC_NM_ibtgei:
    case BC_NM_ibtlti: case BC_NM_ibtlei: case BC_NM_ibtgti:
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_bcmp_res (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_bteqinc: case BC_NM_btneinc: case BC_NM_btgeinc:
    case BC_NM_btltinc: case BC_NM_btleinc: case BC_NM_btgtinc:
    case BC_NM_ibteqinc: case BC_NM_ibtneinc: case BC_NM_ibtgeinc:
    case BC_NM_ibtltinc: case BC_NM_ibtleinc: case BC_NM_ibtgtinc:
      if (result_p)
	{
	  if (nop == 0)
	    res = BC_op1 (bc);
	  else if  (nop == 1)
	    res = BC_bcmp_res (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if (nop == 0)
	res = BC_op1 (bc);
      else if (nop == 1)
	res = BC_bcmp_op2 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_foreach:
      if (result_p)
	{
	  if  (nop == 0)
	    res = BC_op3 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      else if  (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_foreach2:
      if (result_p)
	return NO_MORE_OPERAND;
      else if  (nop == 0)
	res = BC_op1 (bc);
      else
	return NO_MORE_OPERAND;
      break;
    case BC_NM_stdecm:
      if (nop > 0)
	return NO_MORE_OPERAND;
      res = result_p ? BC_op1 (bc) : BC_op2 (bc);
      break;
    case BC_NM_stdecu:
      if (! result_p || nop > 0)
	return NO_MORE_OPERAND;
      res = BC_op1 (bc);
      break;
    case BC_NM_stinc: 
      if (! result_p)
	return NO_MORE_OPERAND;
      /* We just take non-par vars into account:  */
      if (nop >= BC_op1 (bc) + BC_op3 (bc) - BC_op2 (bc))
	return NO_MORE_OPERAND;
      res = BC_op2 (bc) + nop;
      break;
    case BC_NM_block:
      if (! result_p)
	return NO_MORE_OPERAND;
      if (nop >= BC_vars_num (insn->block->bc_block))
	return NO_MORE_OPERAND;
      return nop + get_block_place_start (insn->block);
    case BC_NM_fblock:
      if (result_p)
	{
	  if (nop >= BC_vars_num (insn->block->bc_block))
	    return NO_MORE_OPERAND;
	  return nop + BC_vars_num (tl_block->bc_block);
	}
      if (nop >= BC_pars_num (bc))
	return NO_MORE_OPERAND;
      return nop + BC_vars_num (tl_block->bc_block);
    case BC_NM_chvec:
    case BC_NM_chvend:
    case BC_NM_chvlen:
    case BC_NM_chtab:
    case BC_NM_chtend:
    case BC_NM_chst:
    case BC_NM_chstend:
      return NO_MORE_OPERAND;
    case BC_NM_chvel:
      if (! result_p)
	return NO_MORE_OPERAND;
      else
	{
	  if  (nop == 0 && BC_ch_op5 (bc))
	    res = BC_ch_op4 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      break;
    case BC_NM_chtel:
      if (! result_p)
	return NO_MORE_OPERAND;
      else
	{
	  if  (nop == 0 && BC_ch_op4 (bc))
	    res = BC_ch_op3 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      break;
    case BC_NM_chstel:
      if (! result_p)
	return NO_MORE_OPERAND;
      else
	{
	  if  (nop == 0 && BC_ch_op5 (bc))
	    res = BC_ch_op3 (bc);
	  else
	    return NO_MORE_OPERAND;
	}
      break;
    default:
      assert (FALSE);
    }
  if (res < 0)
    return -1 - res;
  return res + get_block_place_start (insn->block);
}

static int insn_defs_num_cache[UCHAR_MAX + 1];
static int insn_uses_num_cache[UCHAR_MAX + 1];

static inline int
get_insn_defs_num (df_insn_t insn)
{
  BC_node_t bc = insn->bc;
  int i, res;

  if ((res = insn_defs_num_cache[BC_NODE_MODE (bc)]) >= 0)
    return res;
  for (i = 0;; i++)
    if (get_insn_op_place (insn, i, TRUE) == NO_MORE_OPERAND)
      {
	if (! BC_IS_OF_TYPE (bc, BC_NM_block)
	    && ! BC_IS_OF_TYPE (bc, BC_NM_stinc)
	    && ! BC_IS_OF_TYPE (bc, BC_NM_chvel)
	    && ! BC_IS_OF_TYPE (bc, BC_NM_chtel)
	    && ! BC_IS_OF_TYPE (bc, BC_NM_chstel))
	  insn_defs_num_cache[BC_NODE_MODE (bc)] = i;
	return i;
      }
}

static inline int
get_insn_uses_num (df_insn_t insn)
{
  BC_node_t bc = insn->bc;
  int i, res;

  if ((res = insn_uses_num_cache[BC_NODE_MODE (bc)]) >= 0)
    return res;
  for (i = 0;; i++)
    if (get_insn_op_place (insn, i, FALSE) == NO_MORE_OPERAND)
      {
	if (! BC_IS_OF_TYPE (bc, BC_NM_block))
	  insn_uses_num_cache[BC_NODE_MODE (bc)] = i;
	return i;
      }
}



/* Container for insn ptrs.  */
static vlo_t insns_vlo;
/* Number of all insns and executed insns.  Executed insns have
   indexes [0, insns_num) and non-executive insns have indexes
   [insns_num, all_insns_num).  */
static int all_insns_num, insns_num;
/* All insns placed according their index.  */
static df_insn_t *insns;

/* Container for insn ptrs.  */
static vlo_t def_insns_vlo;
static int def_insns_num;
/* Map: definition num -> insn.  */
static df_insn_t *def_insns;

/* Add new INSN.  */
static void
add_insn (df_insn_t insn)
{
  int i, place, vdecl_p;

  insn->defs = insn->uses = NULL;
  insn->index = all_insns_num++;
  VLO_ADD_MEMORY (insns_vlo, &insn, sizeof (df_insn_t));
  vdecl_p = BC_IS_OF_TYPE (insn->bc, BC_NM_decl);
  insn->defs_start = def_insns_num;
  if (vdecl_p)
    {
      VLO_ADD_MEMORY (def_insns_vlo, &insn, sizeof (df_insn_t));
      def_insns_num++;
    }
  else
    {
      insns_num++;
      for (i = 0;; i++)
	{
	  if ((place = get_insn_op_place (insn, i, TRUE)) == NO_MORE_OPERAND
	      || place == NO_MORE_REAL_OPERAND)
	    break;
	  assert (i < get_insn_defs_num (insn));
	  VLO_ADD_MEMORY (def_insns_vlo, &insn, sizeof (df_insn_t));
	}
      def_insns_num += i;
    }
  insns = VLO_BEGIN (insns_vlo);
  def_insns = VLO_BEGIN (def_insns_vlo);
}

/* Create and return block for the BC_BLOCK.  Mark public variables as
   escaped.  */
static block_t
create_block (BC_node_t bc_block)
{
  int vars_num, all_vars_num;
  BC_node_t bc_decl;
  block_t block;

  assert (BC_IS_OF_TYPE (bc_block, BC_NM_block));
  vars_num = BC_vars_num (bc_block);
  all_vars_num = vars_num + BC_tvars_num (bc_block);
  if (all_vars_num == 0)
    all_vars_num = 1;
  block = pass_alloc (sizeof (struct block) + sizeof (char) * (all_vars_num - 1));
  memset (block->escape_p, 0, sizeof (char) * all_vars_num);
  block->bc_block = bc_block;
  block->fun_p = FALSE;
  for (bc_decl = BC_decls (bc_block);
       bc_decl != NULL;
       bc_decl = BC_next_decl (bc_decl))
    if (BC_IS_OF_TYPE (bc_decl, BC_NM_vdecl)
	&& BC_public_p (bc_decl))
      {
	assert (BC_var_num (bc_decl) < vars_num);
	block->escape_p[BC_var_num (bc_decl)] = TRUE;
      }
  return block;
}

/* Create insns starting with BC block code START and definition
   map.  */
static void
create_insns (BC_node_t start)
{
  BC_node_t info;
  df_insn_t insn;
  int i, ndefs, vars_num;
  BC_node_t bc;
  block_t block;
  vlo_t block_stack_vlo;
  struct df_insn temp_insn;

  VLO_CREATE (block_stack_vlo, 0);
  assert (BC_IS_OF_TYPE (start, BC_NM_block));
  block = NULL;
  for (info = BC_info (start); info != NULL; info = BC_next_info (info))
    {
      bc = BC_bc (info);      
      if (BC_IS_OF_TYPE (bc, BC_NM_block))
	{
	  VLO_ADD_MEMORY (block_stack_vlo, &block, sizeof (block_t));
	  block = BC_scope (bc) == NULL ? tl_block : create_block (bc);
	}
      /* That is a trick.  Size of the insn depends only on insn bc
	 and its block.  */
      temp_insn.bc = bc; temp_insn.block = block;
      vars_num = get_insn_defs_num (&temp_insn);
      vars_num += get_insn_uses_num (&temp_insn);
      if (vars_num == 0)
	vars_num = 1;
      insn = pass_alloc (sizeof (struct df_insn)
			 + sizeof (type_t) * (vars_num - 1));
      for (i = 0; i < vars_num; i++)
	insn->types[i] = TP_unknown;
      BC_set_aux (info, insn);
      insn->bc = bc;
      insn->block = block;
      insn->order = -1; /* Initialize */
      if (BC_IS_OF_TYPE (bc, BC_NM_bend))
	{
	  block->bend_insn = insn;
	  block = ((block_t *) VLO_BOUND (block_stack_vlo)) [-1];
	  VLO_SHORTEN (block_stack_vlo, sizeof (block_t));
	}
      add_insn (insn);
    }
  assert (block == NULL
	  /* It might be forward decl fun without definition.  */
	  || BC_next (start) == NULL);
  insns = VLO_BEGIN (insns_vlo);
  assert (all_insns_num == insns_num
	  && insns_num == VLO_LENGTH (insns_vlo) / sizeof (df_insn_t));
  def_insns = VLO_BEGIN (def_insns_vlo);
  assert (def_insns_num == VLO_LENGTH (def_insns_vlo) / sizeof (df_insn_t));
  assert (VLO_LENGTH (block_stack_vlo) == 0
	  /* It might be forward decl fun without definition.  */
	  || (BC_next (start) == NULL
	      && VLO_LENGTH (block_stack_vlo) == sizeof (df_insn_t)));
  VLO_DELETE (block_stack_vlo);
}

/* Remove variables accessible through lvar from consideration by
   modifying escape_p array corresponding to the var block.  */
static void
mark_lvar_access_vars_as_escaped (void)
{
  int i;
  df_insn_t insn, block_insn;
  block_t block;
  BC_node_t bc, vdecl;

  for (i = 0; i < insns_num; i++)
    {
      insn = insns[i];
      bc = insn->bc;
      if (! BC_IS_OF_TYPE (bc, BC_NM_lvar) && ! BC_IS_OF_TYPE (bc, BC_NM_lvarv))
	continue;
      vdecl = BC_decl (bc);
      block_insn = BC_aux (BC_info (BC_decl_scope (BC_decl (bc))));
      block = block_insn->block;
      block->escape_p[BC_var_num (vdecl)]= TRUE;
    }
}

static df_insn_t
get_dest (df_insn_t insn, int n)
{
  BC_node_t bc = insn->bc;

  /* We don't care about possible implicit jumps to exception blocks
     as non-local variables escape analysis as they are accessibly only
     by Bcode lvar(v) there.  */
  if (n == 0)
    return BC_next (bc) == NULL ? NULL : BC_aux (BC_info (BC_next (bc)));
  if (BC_IS_OF_TYPE (bc, BC_NM_br))
    {
      if (n == 1)
	return BC_aux (BC_info (BC_pc (bc)));
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_foreach))
    {
      if (n == 1)
	return BC_aux (BC_info (BC_body_pc (bc)));
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_block))
    {
      if (n == 1 && BC_excepts (bc) != NULL)
	return BC_aux (BC_info (BC_excepts (bc)));
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_except))
    {
      if (n == 1 && BC_next_except (bc) != NULL)
	return BC_aux (BC_info (BC_next_except (bc)));
    }
  else if (BC_IS_OF_TYPE (bc, BC_NM_check))
    {
      if (n == 1 && BC_fail_pc (bc) != NULL)
	return BC_aux (BC_info (BC_fail_pc (bc)));
    }
  return NULL;
}

/* Container for bb ptrs.  */
static vlo_t bbs_vlo;
/* Number of all bbs.  */
static int bbs_num;
/* All bbs placed according their index.  */
static cfg_bb_t *bbs;

/* Return BB for HEAD.  Create BB if necessary.  */
static cfg_bb_t
get_bb (df_insn_t head)
{
  cfg_bb_t bb;

  if ((bb = head->bb) != NULL)
    return bb;
  bb = pass_alloc (sizeof (struct cfg_bb));
  head->bb = bb;
  bb->head = head;
  bb->index = bbs_num++;
  bb->srcs = bb->dests = NULL;
  bb->order = -1; /* Initialize */
  cspset_init (&bb->avail_gen, 0);
  cspset_init (&bb->res_places, 0);
  cspset_init (&bb->avail_in, 0);
  cspset_init (&bb->avail_out, 0);
  VLO_ADD_MEMORY (bbs_vlo, &bb, sizeof (cfg_bb_t));
  return bb;
}

static void
finish_bbs (void)
{
  int i;
  cfg_bb_t bb;

  for (i = 0; i < bbs_num; i++)
    {
      bb = bbs[i];
      cspset_finish (&bb->avail_gen);
      cspset_finish (&bb->res_places);
      cspset_finish (&bb->avail_in);
      cspset_finish (&bb->avail_out);
    }
}

static void
add_cfg_edge (cfg_bb_t src, cfg_bb_t dest)
{
  cfg_edge_t e = pass_alloc (sizeof (struct cfg_edge));

  e->src = src; e->dest = dest;
  e->next_src = dest->srcs; dest->srcs = e;
  e->next_dest = src->dests; src->dests = e;
}

static int inline
imcall_to_consider_p (BC_node_t bc)
{
  BC_node_t fblock;
  df_insn_t dest_insn;

  if (! BC_IS_OF_TYPE (bc, BC_NM_imcall))
    return FALSE;
  fblock = BC_cfblock (bc);
  /* We ignore impossible calls of forward decl fun without
     definition.  */
  if (BC_next (fblock) == NULL)
    return FALSE;
  dest_insn = BC_aux (BC_info (fblock));
  return ! dest_insn->block->fun_p;
}

static void
build_cfg (void)
{
  int i, j, n;
  BC_node_t bc;
  df_insn_t insn, head, dest_insn, fblock_insn;
  cfg_bb_t bb, dest_bb, *worklist;
   
  /* Initialize and set up fun_p  */
  for (i = 0; i < insns_num; i++)
    {
      insn = insns[i];
      insn->temp = 0; insn->bb = NULL;
      bc = insn->bc;
      if (BC_IS_OF_TYPE (bc, BC_NM_fun)
	  || BC_IS_OF_TYPE (bc, BC_NM_class))
	{
	  dest_insn = BC_aux (BC_info (BC_fblock (BC_decl (bc))));
	  dest_insn->block->fun_p = TRUE;
	}
    }
  /* Count predecessors.  */
  for (i = 0; i < insns_num; i++)
    {
      insn = insns[i];
      for (j = 0; (dest_insn = get_dest (insn, j)) != NULL; j++)
	dest_insn->temp++;
    }
  bbs_num = 0;
  MALLOC (worklist, sizeof (df_insn_t) * insns_num);
  /* Add insns without predecessors as BB heads.  */
  for (n = i = 0; i < insns_num; i++)
    if ((insn = insns[i])->temp == 0)
      worklist[n++] = get_bb (insn);
  for (i = 0; i < n; i++)
    {
      bb = worklist[i];
      for (insn = bb->head; insn != NULL; insn = dest_insn)
	{
	  insn->bb = bb;
	  /* More one successors or imcall -- end of BB.  */
	  if (get_dest (insn, 1) != NULL || imcall_to_consider_p (insn->bc))
	    break;
	  dest_insn = get_dest (insn, 0);
	  /* Successor has more one predecessors.  */
	  if (dest_insn == NULL || dest_insn->temp > 1)
	    break;
	}
      bb->tail = insn;
      assert (insn != NULL);
      /* Add egdes and destinations as heads if it is not added
	 yet.  */
      for (j = 0; (dest_insn = get_dest (insn, j)) != NULL; j++)
	{
	  if (dest_insn->bb == NULL)
	    worklist[n++] = get_bb (dest_insn);
	  dest_bb = dest_insn->bb;
	  add_cfg_edge (bb, dest_bb);
	}
      bc = insn->bc;
      if (imcall_to_consider_p (bc))
	{
	  /* Add edge imcall -> fblock.  */
	  fblock_insn = BC_aux (BC_info (BC_cfblock (bc)));
	  if (fblock_insn->bb == NULL)
	    worklist[n++] = get_bb (fblock_insn);
	  add_cfg_edge (bb, fblock_insn->bb);
	}
    }
  /* After creating all BBs, add fbend -> <next insn BB after
     imcall>.  */
  for (i = 0; i < insns_num; i++)
    {
      insn = insns[i];
      bc = insn->bc;
      if (! imcall_to_consider_p (bc))
	continue;
      dest_insn = get_dest (insn, 0);
      dest_bb = dest_insn->bb;
      fblock_insn = BC_aux (BC_info (BC_cfblock (bc)));
      add_cfg_edge (fblock_insn->block->bend_insn->bb, dest_bb);
    }
  FREE (worklist);
  bbs = VLO_BEGIN (bbs_vlo);
  assert (bbs_num == VLO_LENGTH (bbs_vlo) / sizeof (cfg_bb_t));
}

/* Return definition number of result PLACE which is INSN_NDEF-th
   definition of INSN.  Return negative if the definition escapes.  */
static inline int
get_place_ndef (df_insn_t insn, int place, int insn_ndef)
{
  BC_node_t bc = insn->bc;
  int nvar;

  assert (place >= 0);
  if (place < BC_vars_num (tl_block->bc_block))
    {
      if (tl_block->escape_p[place])
	return -1;
    }
  else if (insn->block->escape_p[place - BC_vars_num (tl_block->bc_block)])
    return -1;
  return insn->defs_start + insn_ndef;
}

/* Return insn and insn definition order number through parameters
   DEF_INSN and INSN_NDEF of definition for NDEF.  */
static inline void
get_def_insn_ndef (int ndef, df_insn_t *def_insn, int *insn_ndef)
{
  df_insn_t insn;

  assert (ndef < def_insns_num);
  *def_insn = insn = def_insns[ndef];
  *insn_ndef = ndef - insn->defs_start;
}

/* Return place (see comments for function get_insn_op_place) for
   NDEF.  */
static inline int
get_def_place (int ndef)
{
  int res;
  df_insn_t insn;
  int insn_ndef;

  get_def_insn_ndef (ndef, &insn, &insn_ndef);
  res = get_insn_op_place (insn, insn_ndef, TRUE);
  assert (res >= 0);
  return res;
}

static void
calculate_local (void)
{
  int i, j, place;
  cfg_bb_t bb;
  df_insn_t insn;
  cspset_elem_t el;
  int insn_def;
  int fbend_p;
  vlo_t bb_place_to_el_vlo;

  /* Map: place -> gen set element incremented by 1.  Zero element
     means empty.  */
  VLO_CREATE (bb_place_to_el_vlo, 0);
  for (i = 0; i < bbs_num; i++)
    {
      bb = bbs[i];
      if (BC_IS_OF_TYPE (bb->tail->bc, BC_NM_fbend))
	{
	  /* There is no sense to calculate gen for this BB.  */
	  assert (get_dest (bb->tail, 0) == NULL);
	  continue;
	}
      fbend_p = BC_IS_OF_TYPE (bb->tail->bc, BC_NM_fbend);
      for (insn = bb->head; insn != NULL; insn = get_dest (insn, 0))
	{
	  for (j = 0;; j++)
	    {
	      place = get_insn_op_place (insn, j, TRUE);
	      if (place == NO_MORE_OPERAND || place == NO_MORE_REAL_OPERAND)
		break;
	      /* Remove non-global definitions for fbend BB.  */
	      if (fbend_p && place >= BC_vars_num (tl_block->bc_block))
		continue;
	      assert (j < get_insn_defs_num (insn));
	      cspset_insert (&bb->res_places, place);
	      /* Expand the map: */
	      el = 0;
	      while (VLO_LENGTH (bb_place_to_el_vlo) <= place * sizeof (cspset_elem_t))
		VLO_ADD_MEMORY (bb_place_to_el_vlo, &el, sizeof (cspset_elem_t));
	      if ((el = ((cspset_elem_t *) VLO_BEGIN (bb_place_to_el_vlo))[place]) != 0)
		/* Remove previous element with the same place from
		   avail_gen: */
		cspset_remove (&bb->avail_gen, el - 1);
	      /* Insert the new element.  */
	      el = get_place_ndef (insn, place, j);
	      ((cspset_elem_t *) VLO_BEGIN (bb_place_to_el_vlo))[place] = el + 1;
	      cspset_insert (&bb->avail_gen, el);
	    }
	  if (insn == bb->tail)
	    break;
	}
    }
  VLO_DELETE (bb_place_to_el_vlo);
}

static void
avail_confl0 (node_t n)
{
  cfg_bb_t bb = n;

  cspset_clear (&bb->avail_in);
}

static int
avail_confl (edge_t pe)
{
  cfg_edge_t e = pe;
  cfg_bb_t dest = e->dest;
  cspset_iterator_t si;
  cspset_elem_t el;
  int place;
  int fblock_p = BC_IS_OF_TYPE (dest->head->bc, BC_NM_fblock);
  int change_p = FALSE;

  EXECUTE_FOR_EACH_CSPSET_ELEM (&e->src->avail_out, el, si)
    {
      place = get_def_place (el);
      /* Remove non-global definitions for fblock BB.  */
      if (fblock_p && place >= BC_vars_num (tl_block->bc_block))
	continue;
      if (cspset_insert (&dest->avail_in, el))
	change_p = TRUE;
    }
  return change_p;
}

static int
avail_transf (node_t n)
{
  cfg_bb_t bb = n;
  cspset_iterator_t si;
  cspset_elem_t el;
  df_insn_t insn;
  int insn_def;
  int place;
  int change_p = FALSE;
  int fbend_p = BC_IS_OF_TYPE (bb->tail->bc, BC_NM_fbend);

  EXECUTE_FOR_EACH_CSPSET_ELEM (&bb->avail_in, el, si)
    {
      place = get_def_place (el);
      /* Remove non-global definitions for fbend BB.  */
      if (fbend_p && place >= BC_vars_num (tl_block->bc_block))
	continue;
      if (! cspset_in_p (&bb->res_places, place)
	  && cspset_insert (&bb->avail_out, el))
	change_p = TRUE;
    }
  change_p = cspset_unity (&bb->avail_out, &bb->avail_gen) || change_p;
  return change_p;
}

/* We process all BBs of all fblocks at once instead of each fblock
   per step.  It does not matter as we use compact sparse sets.  */
static void
calculate_bb_avail (void)
{
  static struct problem avail_problem =
    {
      get_bb_index, get_bb_index, bb_srcs, bb_dests,
      next_src_cfg_edge, next_dest_cfg_edge, cfg_edge_src, cfg_edge_dest,
      get_bb_tick, set_bb_tick, get_bb_order, set_bb_order,
      avail_confl0, avail_confl, avail_transf
    };
  calculate_local ();
  if (dump_flag)
    printf ("// BB postorder inverted:");
  forward_dataflow (&avail_problem, (node_t *) bbs, bbs_num, TRUE);
}

/* Create def-use for (DEF, DE_NUM) and (USE, USE_NUM) and include it
   into chains.  */
static void
create_def_use (df_insn_t def, int def_num, df_insn_t use, int use_num)
{
  def_use_t du = pass_alloc (sizeof (struct def_use));

  du->def_num = def_num; du->use_num = use_num;  du->def = def; du->use = use;
  du->next_def = def->defs; def->defs = du;
  du->next_use = use->uses; use->uses = du;
}

/* Connect ret/leave INSN to the corresponding fbend.  */
static void
build_ret_def_use (df_insn_t insn)
{
  df_insn_t fblock_insn;
  BC_node_t bc_block, bc = insn->bc;

  if (! BC_IS_OF_TYPE (bc, BC_NM_ret) && ! BC_IS_OF_TYPE (bc, BC_NM_leave))
    return;
  for (bc_block = insn->block->bc_block;
       ! BC_IS_OF_TYPE (bc_block, BC_NM_fblock);
       bc_block = BC_scope (bc_block))
    ;
  fblock_insn = BC_aux (BC_info (bc_block));
  create_def_use (insn, 0, fblock_insn->block->bend_insn, 0);
}

/* Connect arguments using AVAIL to the function block of CALL
   INSN.  */
static void
build_call_def_use (df_insn_t insn, cspset_t *avail)
{
  int start, nargs, var_num, place, p, el, def_num;
  df_insn_t fblock_insn, def_insn;
  BC_node_t fblock, bc_decl;
  cspset_iterator_t si;
  BC_node_t bc = insn->bc;

  if (! BC_IS_OF_TYPE (bc, BC_NM_imcall))
    return;
  fblock = BC_cfblock (bc);
  /* We ignore impossible calls of forward decl fun without
     definition.  */
  if (BC_next (fblock) == NULL)
    return;
  fblock_insn = BC_aux (BC_info (fblock));
  /* It might be accessible by generic call.  Don't connect args in
     this case.  */
  if (fblock_insn->block->fun_p)
    return;
  start = BC_op1 (bc);
  nargs = BC_op2 (bc);
  for (bc_decl = BC_decls (fblock);
       bc_decl != NULL;
       bc_decl = BC_next_decl (bc_decl))
    {
      if (! BC_IS_OF_TYPE (bc_decl, BC_NM_vdecl))
	continue;
      var_num = BC_var_num (bc_decl);
      if (var_num >= nargs)
	continue;
      EXECUTE_FOR_EACH_CSPSET_ELEM (avail, el, si)
	{
	  get_def_insn_ndef (el, &def_insn, &def_num);
	  place = get_insn_op_place (def_insn, def_num, TRUE);
	  if (place != start + var_num + get_block_place_start (insn->block))
	    continue;
	  create_def_use (def_insn, def_num, fblock_insn, var_num);
	}
    }
  create_def_use (fblock_insn->block->bend_insn, 0, insn, 0);
}

/* Build def-use chains using calculated availability info.  */
static void
build_def_use_chains (void)
{
  int i, j, place, def_num, el;
  df_insn_t insn, def_insn;
  cfg_bb_t bb;
  cspset_t avail;
  cspset_iterator_t si;

  cspset_init (&avail, 0);
  for (i = 0; i < bbs_num; i++)
    {
      bb = bbs[i];
      cspset_copy (&avail, &bb->avail_in);
      for (insn = bb->head; insn != NULL; insn = get_dest (insn, 0))
	{
	  if (BC_IS_OF_TYPE (insn->bc, BC_NM_fbend))
	    {
	      /* This insn is included in def-use web by processing
		 return.  It has also artificial input and output
		 places.  */
	      assert (insn == bb->tail && get_dest (insn, 0) == NULL);
	      break;
	    }
	  /* Process input operands: */
	  for (j = 0;; j++)
	    {
	      place = get_insn_op_place (insn, j, FALSE);
	      if (place == NO_MORE_OPERAND || place == NO_MORE_REAL_OPERAND)
		break;
	      assert (j < get_insn_uses_num (insn));
	      EXECUTE_FOR_EACH_CSPSET_ELEM (&avail, el, si)
		if (place == get_def_place (el))
		  {
		    get_def_insn_ndef (el, &def_insn, &def_num);
		    create_def_use (def_insn, def_num, insn, j);
		  }
	    }
	  build_call_def_use (insn, &avail);
	  build_ret_def_use (insn);
	  /* Update availability:  */
	  for (j = 0;; j++)
	    {
	      place = get_insn_op_place (insn, j, TRUE);
	      if (place == NO_MORE_OPERAND || place == NO_MORE_REAL_OPERAND)
		break;
	      assert (j < get_insn_defs_num (insn));
	      EXECUTE_FOR_EACH_CSPSET_ELEM (&avail, el, si)
		if (place == get_def_place (el))
		  cspset_remove (&avail, el);
	      cspset_insert (&avail, get_place_ndef (insn, place, j));
	    }
	  if (insn == bb->tail)
	    break;
	}
    }
  cspset_finish (&avail);
}

static int
type_confl (edge_t te)
{
  type_t from_tp, to_tp, res_tp;
  def_use_t e = te;
  df_insn_t use = e->use, def = e->def;
  int def_num = e->def_num;
  int use_num = e->use_num;
  int use_defs_num = get_insn_defs_num (use);

  assert (def_num < get_insn_defs_num (def));
  from_tp = def->types[def_num];
  assert (use_num < get_insn_uses_num (use));
  to_tp = use->types[use_num + use_defs_num];
  if (from_tp == TP_unknown || to_tp == TP_varying || to_tp == from_tp)
    res_tp = to_tp;
  else if (to_tp == TP_unknown || from_tp == TP_varying)
    res_tp = from_tp;
  else
    res_tp = TP_varying;
  if (res_tp == to_tp)
    return FALSE;
  use->types[use_num + use_defs_num] = res_tp;
  return TRUE;
}

static inline type_t
arithm_type (type_t t)
{
  if (t == TP_unknown)
    return TP_unknown;
  if (t == TP_char)
    t = TP_int;
  if (t == TP_int || t == TP_long || t == TP_float)
    return t;
  return TP_varying;
}

static inline type_t
arithm_int_type (type_t t)
{
  if (t == TP_unknown)
    return TP_unknown;
  if (t == TP_char || t == TP_long || t == TP_float || t == TP_vec)
    t = TP_int;
  if (t == TP_int)
    return t;
  return TP_varying;
}

static inline type_t
bin_arithm_type (type_t t1, type_t t2)
{
  if (t1 == TP_unknown || t2 == TP_unknown)
    return TP_unknown;
  if (t1 == TP_char)
    t1 = TP_int;
  if (t2 == TP_char)
    t2 = TP_int;
  if ((t1 == TP_int || t1 == TP_long) && (t2 == TP_long || t2 == TP_float))
    return t2;
  if ((t2 == TP_int || t2 == TP_long) && (t1 == TP_long || t1 == TP_float))
    return t1;
  if (t1 == TP_int && t2 == TP_int)
    return TP_int;
  if ((t1 == TP_float || t1 == TP_int) && (t2 == TP_int || t2 == TP_float))
    return TP_float;
  return TP_varying;
}

static inline type_t
bin_arithm_int_type (type_t t1, type_t t2)
{
  if (t1 == TP_unknown || t2 == TP_unknown)
    return TP_unknown;
  if (t1 == TP_char || t1 == TP_long || t1 == TP_float || t1 == TP_vec)
    t1 = TP_int;
  if (t2 == TP_char || t2 == TP_long || t2 == TP_float || t2 == TP_vec)
    t2 = TP_int;
  if (t1 == TP_int && t2 == TP_int)
    return TP_int;
  return TP_varying;
}

static inline type_t
eq_type (type_t t1, type_t t2)
{
  if (t1 == TP_unknown || t2 == TP_unknown)
    return TP_unknown;
  if (t1 != TP_varying && t2 != TP_varying)
    return TP_int;
  return TP_varying;
}

static inline type_t
cmp_type (type_t t1, type_t t2)
{
  if (t1 == TP_unknown || t2 == TP_unknown)
    return TP_unknown;
  if (t1 == TP_char|| t1 == TP_int || t1 == TP_long || t1 == TP_float || t1 == TP_vec)
    t1 = TP_int; /* just for convinience */
  if (t2 == TP_char || t1 == TP_int || t1 == TP_long || t1 == TP_float || t1 == TP_vec)
    t2 = TP_int; /* just for convinience */
  if (t1 == TP_int && t2 == TP_int)
    return TP_int;
  return TP_varying;
}

/* Everything is initialized by TP_unkown  */
static int
type_transf (node_t node)
{
  int i, n, change_p;
  type_t res_tp, res_tp2, t2, t3;
  df_insn_t insn = (df_insn_t) node;
  BC_node_t bc = insn->bc;

  res_tp = TP_unknown;
  switch (BC_NODE_MODE (bc))
    {
    case BC_NM_ldch: case BC_NM_chof:
      res_tp = TP_char;
      break;
    case BC_NM_ldi: case BC_NM_ldbi: case BC_NM_iof:
      res_tp = TP_int;
      break;
    case BC_NM_ldl: case BC_NM_lof:
      res_tp = TP_long;
      break;
    case BC_NM_ldf: case BC_NM_fof:
      res_tp = TP_float;
      break;
    case BC_NM_lds: case BC_NM_vec: case BC_NM_vecof: case BC_NM_fmtvecof:
      res_tp = TP_vec;
      break;
    case BC_NM_tab: case BC_NM_tabof:
      res_tp = TP_tab;
      break;
    case BC_NM_plus: case BC_NM_minus: case BC_NM_not:
      res_tp = arithm_type (insn->types[1]);
      break;
    case BC_NM_bnot:
      res_tp = arithm_int_type (insn->types[1]);
      break;
    case BC_NM_add: case BC_NM_sub: case BC_NM_mult: case BC_NM_div: case BC_NM_mod:
      res_tp = bin_arithm_type (insn->types[1], insn->types[2]);
      break;
    case BC_NM_madd:
      res_tp = bin_arithm_type (bin_arithm_type (insn->types[1], insn->types[2]),
				insn->types[3]);
      break;
    case BC_NM_addi:
    case BC_NM_multi:
      res_tp = bin_arithm_type (insn->types[1], TP_int);
      break;
    case BC_NM_or: case BC_NM_xor: case BC_NM_and:
    case BC_NM_lsh: case BC_NM_rsh: case BC_NM_ash:
      res_tp = bin_arithm_int_type (insn->types[1], insn->types[2]);
      break;
    case BC_NM_id: case BC_NM_unid: case BC_NM_eq: case BC_NM_ne:
      res_tp = eq_type (insn->types[1], insn->types[2]);
      break;
    case BC_NM_eqi: case BC_NM_nei:
      res_tp = eq_type (insn->types[1], TP_int);
      break;
    case BC_NM_lt: case BC_NM_gt: case BC_NM_le: case BC_NM_ge:
      res_tp = cmp_type (insn->types[1], insn->types[2]);
      break;
    case BC_NM_lti: case BC_NM_gti: case BC_NM_lei: case BC_NM_gei:
      res_tp = cmp_type (insn->types[1], TP_int);
      break;
    case BC_NM_ldnil: case BC_NM_ldthis: case BC_NM_ldtp: case BC_NM_tpof:
      res_tp = TP_varying;
      break;
    case BC_NM_flat:
      res_tp = TP_vec;
      break;
    case BC_NM_var: case BC_NM_lvar: case BC_NM_lvarv:
    case BC_NM_evar: case BC_NM_levar: case BC_NM_levarv:
    case BC_NM_fld: case BC_NM_lfld: case BC_NM_lfldv:
    case BC_NM_ovfld: case BC_NM_lovfld: case BC_NM_lovfldv:
    case BC_NM_ind:
    case BC_NM_ind2:
    case BC_NM_sl: case BC_NM_lslv:
      res_tp = TP_varying;
      break;
    case BC_NM_fun: case BC_NM_efun: case BC_NM_class:
    case BC_NM_ofun: case BC_NM_oclass:
      res_tp = TP_varying;
      break;
    case BC_NM_const:
    case BC_NM_new:
      res_tp = insn->types[1];
      break;
    case BC_NM_length:
      t2 = insn->types[1]; /* slice/vec(converted to vec)/tab */
      res_tp = TP_int;
      break;
    case BC_NM_fold_add: case BC_NM_fold_mult:
    case BC_NM_fold_and: case BC_NM_fold_or: case BC_NM_fold_xor:
      res_tp = TP_varying; /* int/float/long/slice depended on els */
      break;
    case BC_NM_in:
      t2 = insn->types[1]; t3 = insn->types[2];
      res_tp = TP_int;
      break;
    case BC_NM_concat:
      t2 = insn->types[1]; t3 = insn->types[2];
      res_tp = TP_vec;
      break;
    case BC_NM_brts: case BC_NM_brfs: case BC_NM_lconv:
      res_tp = TP_int;
      break;
    case BC_NM_bteqinc: case BC_NM_btneinc:
    case BC_NM_btgeinc: case BC_NM_btltinc: case BC_NM_btleinc: case BC_NM_btgtinc:
      res_tp = bin_arithm_type (insn->types[2], TP_int);
      res_tp2 = cmp_type (res_tp, insn->types[3]);
      if (insn->types[0] == res_tp && insn->types[1] == res_tp2)
	return FALSE;
      insn->types[0] = res_tp;
      insn->types[1] = res_tp2;
      return TRUE;
    case BC_NM_foreach:
      res_tp = TP_varying; /* We can not know index type */
      if (insn->types[0] == res_tp)
	return FALSE;
      insn->types[0] = res_tp;
      return TRUE;
    case BC_NM_foreach2:
      res_tp = TP_varying;
      break;
    case BC_NM_move:
      res_tp = insn->types[1];
      break;
    case BC_NM_stvt: case BC_NM_sts: case BC_NM_ste:
    case BC_NM_stvtu: case BC_NM_stsu: case BC_NM_steu:
    case BC_NM_slst:
    case BC_NM_mult_st: case BC_NM_div_st: case BC_NM_mod_st:
    case BC_NM_add_st: case BC_NM_sub_st:
    case BC_NM_concat_st:
    case BC_NM_lsh_st: case BC_NM_rsh_st: case BC_NM_ash_st:
    case BC_NM_and_st: case BC_NM_xor_st: case BC_NM_or_st:
    case BC_NM_mult_slst: case BC_NM_div_slst: case BC_NM_mod_slst:
    case BC_NM_add_slst: case BC_NM_sub_slst:
    case BC_NM_concat_slst:
    case BC_NM_lsh_slst: case BC_NM_rsh_slst: case BC_NM_ash_slst:
    case BC_NM_and_slst: case BC_NM_xor_slst: case BC_NM_or_slst:
      /* We work only with local vars.  */
      break;
    case BC_NM_call: case BC_NM_tcall: case BC_NM_mcall:
    case BC_NM_omcall: case BC_NM_omicall: case BC_NM_omitcall:
      /* The result unknown.  */
      break;
    case BC_NM_ibcall: case BC_NM_icall: case BC_NM_itcall:
    case BC_NM_ticall: case BC_NM_titcall: case BC_NM_cicall: case BC_NM_citcall:
      res_tp = insn->types[1];
      break;
    case BC_NM_b: case BC_NM_btdef:
    case BC_NM_bt: case BC_NM_bf: case BC_NM_bfni:
    case BC_NM_ibt: case BC_NM_ibf: case BC_NM_ibfni:
      return FALSE; /* No output */
    case BC_NM_bteq: case BC_NM_btne:
    case BC_NM_btge: case BC_NM_btlt: case BC_NM_btle: case BC_NM_btgt:
      res_tp = cmp_type (insn->types[1], insn->types[2]);
      break;
    case BC_NM_bteqi: case BC_NM_btnei:
    case BC_NM_btgei: case BC_NM_btlti: case BC_NM_btlei: case BC_NM_btgti:
      res_tp = cmp_type (insn->types[1], TP_int);
      break;
    case BC_NM_out: case BC_NM_stpop: case BC_NM_bend:
    case BC_NM_throw: case BC_NM_wait: case BC_NM_waitend: case BC_NM_except:
      return FALSE; /* No output */
    case BC_NM_leave:
      res_tp = TP_undef;
      break;
    case BC_NM_fbend:
      res_tp = insn->types[1];
      break;
    case BC_NM_stdecm:
      res_tp = insn->types[1];
      break;
    case BC_NM_stdecu:
      insn->types[0] = TP_undef;
      /* We can return FALSE as fater the first iteration the type
	 is always TP_undef.  */
      return FALSE;
    case BC_NM_stinc:
      /* See comment for stdecu.  */
      for (i = BC_op1 (bc) + BC_op3 (bc) - BC_op2 (bc) - 1; i >= 0; i--)
	insn->types[i] = TP_undef;
      return FALSE;
    case BC_NM_block:
    case BC_NM_fblock:
      {
	int vars_num = BC_vars_num (insn->block->bc_block);

	change_p = FALSE;
	n = BC_NODE_MODE (bc) == BC_NM_fblock ? BC_pars_num (bc) : 0;
	for (i = n - 1; i >= 0; i--)
	  {
	    /* If it is not connected -- type can be any.  */
	    type_t t = (insn->uses == NULL
			? TP_varying : insn->types[vars_num + i]);
	    
	    if (insn->types[i] != t)
	      change_p = TRUE;
	    insn->types[i] = t;
	  }
	/* See comment for stdecu.  */
	for (i = vars_num - 1; i >= n; i--)
	  insn->types[i] = TP_undef;
	return change_p;
	break;
      }
    case BC_NM_ret:
      res_tp = insn->types[1];
      break;
    case BC_NM_chvec:
    case BC_NM_chvend:
    case BC_NM_chvlen:
    case BC_NM_chtab:
    case BC_NM_chtend:
    case BC_NM_chst:
    case BC_NM_chstend:
      return FALSE;
    case BC_NM_chvel:
      if (! BC_ch_op5 (bc))
	return FALSE;
      break;
    case BC_NM_chtel:
      if (! BC_ch_op4 (bc))
	return FALSE;
      break;
    case BC_NM_chstel:
      if (! BC_ch_op5 (bc))
	return FALSE;
      break;
    default:
      d_assert (FALSE);
    }
  if (insn->types[0] == res_tp)
    return FALSE;
  insn->types[0] = res_tp;
  return TRUE;
}

/* We solve the data-flow problem on def-use chain graph.  */
static void
calculate_types (void)
{
  static struct problem type_problem =
    {
      get_insn_index, get_insn_id, insn_uses, insn_defs,
      next_use_edge, next_def_edge, edge_def, edge_use,
      get_insn_tick, set_insn_tick, get_insn_order, set_insn_order,
      NULL, type_confl, type_transf
    };
  calculate_local ();
  if (dump_flag)
    printf ("// INSN postorder inverted:");
  forward_dataflow (&type_problem, (node_t *) insns, all_insns_num, TRUE);
}



static void
specialize_insn (df_insn_t insn)
{
  BC_node_t bc = insn->bc;
  BC_node_mode_t nm = BC_NODE_MODE (bc);
  int exchange_p = FALSE;

  switch (nm)
    {
    case BC_NM_ldch: case BC_NM_chof:
    case BC_NM_ldi: case BC_NM_ldbi: case BC_NM_iof:
    case BC_NM_ldl: case BC_NM_lof:
    case BC_NM_ldf: case BC_NM_fof:
    case BC_NM_lds: case BC_NM_vec: case BC_NM_vecof: case BC_NM_fmtvecof:
    case BC_NM_tab: case BC_NM_tabof:
      break;
    case BC_NM_plus:
      if (insn->types[1] == TP_int)
	nm = BC_NM_iplus;
      else if (insn->types[1] == TP_float)
	nm = BC_NM_fplus;
      break;
    case BC_NM_minus:
      if (insn->types[1] == TP_int)
	nm = BC_NM_iminus;
      else if (insn->types[1] == TP_float)
	nm = BC_NM_fminus;
      break;
    case BC_NM_not:
      if (insn->types[1] == TP_int)
	nm = BC_NM_inot;
      break;
    case BC_NM_bnot:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ibnot;
      break;
    case BC_NM_add:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_iadd;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fadd;
      else if (insn->types[1] == TP_int && insn->types[2] == TP_float)
	nm = BC_NM_ifadd;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_int)
	{
	  exchange_p = TRUE;
	  nm = BC_NM_ifadd;
	}
      break;
    case BC_NM_sub:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_isub;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fsub;
      else if (insn->types[1] == TP_int && insn->types[2] == TP_float)
	nm = BC_NM_ifsub;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_int)
	nm = BC_NM_fisub;
      break;
    case BC_NM_mult:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_imult;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fmult;
      else if (insn->types[1] == TP_int && insn->types[2] == TP_float)
	nm = BC_NM_ifmult;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_int)
	{
	  exchange_p = TRUE;
	  nm = BC_NM_ifmult;
	}
      break;
    case BC_NM_div:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_idiv;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fdiv;
      else if (insn->types[1] == TP_int && insn->types[2] == TP_float)
	nm = BC_NM_ifdiv;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_int)
	nm = BC_NM_fidiv;
      break;
    case BC_NM_mod:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_imod;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fmodop;
      break;
    case BC_NM_madd:
      break;
    case BC_NM_addi:
      if (insn->types[1] == TP_int)
	nm = BC_NM_iaddi;
      else if (insn->types[1] == TP_float)
	nm = BC_NM_faddi;
      break;
    case BC_NM_multi:
      if (insn->types[1] == TP_int)
	nm = BC_NM_imulti;
      else if (insn->types[1] == TP_float)
	nm = BC_NM_fmulti;
      break;
    case BC_NM_or:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ior;
      break;
    case BC_NM_xor:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ixor;
      break;
    case BC_NM_and:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_iand;
      break;
    case BC_NM_lsh:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ilsh;
      break;
    case BC_NM_rsh:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_irsh;
      break;
    case BC_NM_ash:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_iash;
      break;
    case BC_NM_id: case BC_NM_unid:
      break;
    case BC_NM_eq:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ieq;
      break;
    case BC_NM_ne:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ine;
      break;
    case BC_NM_eqi:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ieqi;
      break;
    case BC_NM_nei:
      if (insn->types[1] == TP_int)
	nm = BC_NM_inei;
      break;
    case BC_NM_lt:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ilt;
      break;
    case BC_NM_gt:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_igt;
      break;
    case BC_NM_le:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ile;
      break;
    case BC_NM_ge:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ige;
      break;
    case BC_NM_lti:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ilti;
      break;
    case BC_NM_gti:
      if (insn->types[1] == TP_int)
	nm = BC_NM_igti;
      break;
    case BC_NM_lei:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ilei;
      break;
    case BC_NM_gei:
      if (insn->types[1] == TP_int)
	nm = BC_NM_igei;
      break;
    case BC_NM_stinc: case BC_NM_stdecm: case BC_NM_stdecu:
    case BC_NM_ldnil: case BC_NM_ldthis: case BC_NM_ldtp: case BC_NM_tpof:
    case BC_NM_flat:
    case BC_NM_var: case BC_NM_lvar: case BC_NM_lvarv:
    case BC_NM_evar: case BC_NM_levar: case BC_NM_levarv:
    case BC_NM_fld: case BC_NM_lfld: case BC_NM_lfldv:
    case BC_NM_ovfld: case BC_NM_lovfld: case BC_NM_lovfldv:
    case BC_NM_ind:
    case BC_NM_ind2:
    case BC_NM_sl: case BC_NM_lslv:
    case BC_NM_fun: case BC_NM_efun: case BC_NM_class:
    case BC_NM_ofun: case BC_NM_oclass:
    case BC_NM_const:
    case BC_NM_new:
    case BC_NM_length:
    case BC_NM_fold_add: case BC_NM_fold_mult:
    case BC_NM_fold_and: case BC_NM_fold_or: case BC_NM_fold_xor:
    case BC_NM_in:
    case BC_NM_concat:
    case BC_NM_brts: case BC_NM_brfs: case BC_NM_lconv:
      break;
    case BC_NM_bteqinc:
      if (insn->types[2] == TP_int && insn->types[3] == TP_int)
	nm = BC_NM_ibteqinc;
      break;
    case BC_NM_btneinc:
      if (insn->types[2] == TP_int && insn->types[3] == TP_int)
	nm = BC_NM_ibtneinc;
      break;
    case BC_NM_btgeinc:
      if (insn->types[2] == TP_int && insn->types[3] == TP_int)
	nm = BC_NM_ibtgeinc;
      break;
    case BC_NM_btltinc:
      if (insn->types[2] == TP_int && insn->types[3] == TP_int)
	nm = BC_NM_ibtltinc;
      break;
    case BC_NM_btleinc:
      if (insn->types[2] == TP_int && insn->types[3] == TP_int)
	nm = BC_NM_ibtleinc;
      break;
    case BC_NM_btgtinc:
      if (insn->types[2] == TP_int && insn->types[3] == TP_int)
	nm = BC_NM_ibtgtinc;
      break;
    case BC_NM_foreach:
    case BC_NM_foreach2:
      break;
    case BC_NM_move:
      if (insn->types[1] == TP_int)
	nm = BC_NM_imove;
      else if (insn->types[1] == TP_float)
	nm = BC_NM_fmove;
      break;
    case BC_NM_stvt: case BC_NM_sts: case BC_NM_ste:
    case BC_NM_stvtu: case BC_NM_stsu: case BC_NM_steu:
    case BC_NM_slst:
    case BC_NM_mult_st: case BC_NM_div_st: case BC_NM_mod_st:
    case BC_NM_add_st: case BC_NM_sub_st:
    case BC_NM_concat_st:
    case BC_NM_lsh_st: case BC_NM_rsh_st: case BC_NM_ash_st:
    case BC_NM_and_st: case BC_NM_xor_st: case BC_NM_or_st:
    case BC_NM_mult_slst: case BC_NM_div_slst: case BC_NM_mod_slst:
    case BC_NM_add_slst: case BC_NM_sub_slst:
    case BC_NM_concat_slst:
    case BC_NM_lsh_slst: case BC_NM_rsh_slst: case BC_NM_ash_slst:
    case BC_NM_and_slst: case BC_NM_xor_slst: case BC_NM_or_slst:
    case BC_NM_call: case BC_NM_tcall: case BC_NM_mcall:
    case BC_NM_omcall: case BC_NM_omicall: case BC_NM_omitcall:
    case BC_NM_ibcall: case BC_NM_icall: case BC_NM_itcall:
    case BC_NM_ticall: case BC_NM_titcall: case BC_NM_cicall: case BC_NM_citcall:
    case BC_NM_b: case BC_NM_btdef:
      break;
    case BC_NM_bt:
      if (insn->types[0] == TP_int)
	nm = BC_NM_ibt;
      break;
    case BC_NM_bf:
      if (insn->types[0] == TP_int)
	nm = BC_NM_ibf;
      break;
    case BC_NM_bfni:
      if (insn->types[0] == TP_int)
	nm = BC_NM_ibfni;
      break;
    case BC_NM_bteq:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ibteq;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fbteq;
      break;
    case BC_NM_btne:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ibtne;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fbtne;
      break;
    case BC_NM_btge:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ibtge;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fbtge;
      break;
    case BC_NM_btlt:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ibtlt;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fbtlt;
      break;
    case BC_NM_btle:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ibtle;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fbtle;
      break;
    case BC_NM_btgt:
      if (insn->types[1] == TP_int && insn->types[2] == TP_int)
	nm = BC_NM_ibtgt;
      else if (insn->types[1] == TP_float && insn->types[2] == TP_float)
	nm = BC_NM_fbtgt;
      break;
    case BC_NM_bteqi:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ibteqi;
      break;
    case BC_NM_btnei:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ibtnei;
      break;
    case BC_NM_btgei:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ibtgei;
      break;
    case BC_NM_btlti:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ibtlti;
      break;
    case BC_NM_btlei:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ibtlei;
      break;
    case BC_NM_btgti:
      if (insn->types[1] == TP_int)
	nm = BC_NM_ibtgti;
      break;
    case BC_NM_out: case BC_NM_stpop: case BC_NM_bend:
    case BC_NM_throw: case BC_NM_wait: case BC_NM_waitend: case BC_NM_except:
    case BC_NM_leave:
    case BC_NM_fbend:
    case BC_NM_block:
    case BC_NM_fblock:
    case BC_NM_ret:
    case BC_NM_chvec:
    case BC_NM_chvend:
    case BC_NM_chvlen:
    case BC_NM_chvel:
    case BC_NM_chtab:
    case BC_NM_chtend:
    case BC_NM_chtel:
    case BC_NM_chst:
    case BC_NM_chstend:
    case BC_NM_chstel:
      break;
    default:
      d_assert (FALSE);
    }
  if (BC_NODE_MODE (bc) != nm)
    BC_SET_MODE (bc, nm);
  if (exchange_p)
    {
      int t = BC_op2 (bc);

      BC_set_op2 (bc, BC_op3 (bc));
      BC_set_op3 (bc, t);
    }
}

static void
specialize_program (void)
{
  int i;

  for (i = 0; i < insns_num; i++)
    specialize_insn (insns[i]);
}



static void
print_op (df_insn_t insn, int num)
{
  BC_node_t bc = insn->bc;

  printf ("%dr%d", BC_idn (BC_info (bc)), num);
}

static void
print_bb_set (const char *name, cspset_t *set, int indent, int ndef_p)
{
  int el, n = 0;
  int def_num;
  df_insn_t def_insn;
  cspset_iterator_t si;

  EXECUTE_FOR_EACH_CSPSET_ELEM (set, el, si)
    {
      if (n % 16 == 0)
	{
	  if (n == 0)
	    {
	      print_indent (indent);
	      printf ("  //    %s:", name);
	    }
	  else
	    {
	      printf ("\n");
	      print_indent (indent);
	      printf ("  //    ");
	    }
	}
      n++;
      if (! ndef_p)
	{
	  printf (" %d", el);
	  continue;
	}
      get_def_insn_ndef (el, &def_insn, &def_num);
      printf (" ");
      print_op (def_insn, def_num);
    }
  if (n != 0)
    printf ("\n");
}

static inline const char *
get_type_name (type_t t)
{
  assert (0 <= t && t <= TP_varying
	  && t < sizeof (type_name) / sizeof (char *));
  return type_name[t];
}

static int
print_defs_uses (df_insn_t insn, int indent)
{
  int n, i, res = FALSE;
  def_use_t du;

  for (n = 0, du = insn->uses; du != NULL; du = du->next_use)
    {
      assert (insn == du->use);
      if (n % 10 == 0)
	{
	  if (n != 0)
	    printf ("\n");
	  print_indent (indent);
	  printf (n == 0 ? "  //  use:" : "  //    ");
	}
      n++;
      printf (" [%d]", du->use_num);
      print_op (du->def, du->def_num);
    }
  if (n != 0)
    printf ("\n");
  res = n != 0 || res;
  for (n = 0, du = insn->defs; du != NULL; du = du->next_def)
    {
      assert (insn == du->def);
      if (n % 10 == 0)
	{
	  if (n != 0)
	    printf ("\n");
	  print_indent (indent);
	  printf (n == 0 ? "  //  def:" : "  //    ");
	}
      n++;
      printf (" [%d]", du->def_num);
      print_op (du->use, du->use_num);
    }
  if (n != 0)
    printf ("\n");
  res = n != 0 || res;
  /* Print types of outputs:  */
  for (n = i = 0;; i++)
    {
      if (get_insn_op_place (insn, i, TRUE) == NO_MORE_OPERAND)
	break;
      if (n % 10 == 0)
	{
	  if (n != 0)
	    printf ("\n");
	  print_indent (indent);
	  printf (n == 0 ? "  //  types:" : "  //    ");
	}
      printf (" [%d]%s", n, get_type_name (insn->types[n]));
      n++;
    }
  /* Print types of inputs:  */
  for (i = 0;; i++)
    {
      if (get_insn_op_place (insn, i, FALSE) == NO_MORE_OPERAND)
	break;
      if (i == 0)
	{
	  if (n != 0)
	    printf (" <=");
	  else
	    {
	      print_indent (indent);
	      printf ("  //  types: <=");
	    }
	}
      if (i != 0 && n % 10 == 0)
	{
	  if (n != 0)
	    printf ("\n");
	  print_indent (indent);
	  printf ("//    ");
	}
      printf (" [%d]%s", n, get_type_name (insn->types[n]));
      n++;
    }
  if (n != 0)
    printf ("\n");
  res = n != 0 || res;
  return n != 0 || res;
}

int
print_inference_info_before_insn (BC_node_t info, int indent)
{
  int n, res = FALSE;
  df_insn_t insn;
  cfg_bb_t bb;
  BC_node_t bc;
  block_t block;
  cfg_edge_t e;

  insn = BC_aux (info);
  bb = insn->bb;
  bc = insn->bc;
  block = insn->block;
  if (bb->head == insn)
    {
      print_indent (indent);
      printf ("  // bb%d start (head_insn_index=i%d): src=", bb->index, insn->index);
      for (e = bb->srcs; e!= NULL; e = e->next_src)
	{
	  printf ("%d", e->src->index);
	  if (e->next_src != NULL)
	    printf (",");
	}
      printf ("\n");
      if (block->bc_block == bc)
	{
	  BC_node_t bc_decl;
	  
	  for (n = 0, bc_decl = BC_decls (bc);
	       bc_decl != NULL;
	       bc_decl = BC_next_decl (bc_decl))
	    {
	      if (! BC_IS_OF_TYPE (bc_decl, BC_NM_vdecl)
		  || ! block->escape_p[BC_var_num (bc_decl)])
		continue;
	      if (n % 10 == 0)
		{
		  if (n != 0)
		    printf ("\n");
		  print_indent (indent);
		  printf (n == 0 ? "  // escaped vars:" : "  //    ");
		}
	      n++;
	      printf (" %d:%s", BC_var_num (bc_decl), BC_ident (bc_decl));
	    }
	  if (n != 0)
	    printf ("\n");
	}
      print_bb_set ("avail_in", &bb->avail_in, indent, TRUE);
      res = TRUE;
    }
  res = print_defs_uses (insn, indent) || res;
  return res;
}

int
print_inference_info_after_insn (BC_node_t info, int indent)
{
  df_insn_t insn;
  cfg_bb_t bb;
  cfg_edge_t e;

  insn = BC_aux (info);
  bb = insn->bb;
  if (bb->tail != insn)
    return FALSE;
  print_indent (indent);
  printf ("  // bb%d end (tail_insn_index=i%d): dest=", bb->index, insn->index);
  for (e = bb->dests; e!= NULL; e = e->next_dest)
    {
      printf ("%d", e->dest->index);
      if (e->next_dest != NULL)
	printf (",");
    }
  printf ("\n");
  print_bb_set ("res_places", &bb->res_places, indent, FALSE);
  print_bb_set ("avail_gen", &bb->avail_gen, indent, TRUE);
  print_bb_set ("avail_out", &bb->avail_out, indent, TRUE);
  return TRUE;
}

void
initiate_inference_pass (void)
{
  int i;

  for (i = 0; i <= UCHAR_MAX; i++)
    insn_defs_num_cache[i] = insn_uses_num_cache[i] = -1;
  pass_alloc_init ();
  VLO_CREATE (insns_vlo, 0);
  VLO_CREATE (def_insns_vlo, 0);
  insns_num = all_insns_num = def_insns_num = 0;
  VLO_CREATE (bbs_vlo, 0);
}

void
inference_pass (BC_node_t top_block, vlo_t *all_fblocks)
{
  BC_node_t *bc_ptr;

  /* Create top level block first as we need global vars number as
     soon as possible.  */
  tl_block = create_block (top_block);
  create_insns (top_block);
  for (bc_ptr = (BC_node_t *) VLO_BEGIN (*all_fblocks);
       bc_ptr < (BC_node_t *) VLO_BOUND (*all_fblocks);
       bc_ptr++)
    create_insns (*bc_ptr);
  mark_lvar_access_vars_as_escaped ();
  build_cfg ();
  calculate_bb_avail ();
  build_def_use_chains ();
  calculate_types ();
  specialize_program ();
}

void
finish_inference_pass (void)
{
  finish_bbs ();
  VLO_DELETE (bbs_vlo);
  VLO_DELETE (def_insns_vlo);
  VLO_DELETE (insns_vlo);
  pass_alloc_finish ();
}
