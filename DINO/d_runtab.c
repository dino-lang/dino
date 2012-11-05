/*
   Copyright (C) 1997-2012 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

#include "d_common.h"
#include "d_ir.h"
#include "d_runtab.h"

vlo_t func_class_tab;
static int_t curr_func_class_no;

struct block_decl_tables block_decl_tables;

/* This func is to be called only once before any work with this abstract
   data. */
void
initiate_run_tables (void)
{
  block_decl_tables.idents_number = 0;
  VLO_CREATE (block_decl_tables.block_ident_decls, 2000);
  curr_func_class_no = 0;
  VLO_CREATE (func_class_tab, 800);
}

/* This func is to set up an order number and the corresponding table
   entry for FUNC_CLASS. */
void
set_func_class_no (IR_node_t func_class)
{
  if (IR_no (func_class) >= 0)
    return;
  IR_set_no (func_class, curr_func_class_no);
  assert (curr_func_class_no * sizeof (IR_node_t)
	  == VLO_LENGTH (func_class_tab));
  VLO_ADD_MEMORY (func_class_tab, &func_class, sizeof (IR_node_t));
  curr_func_class_no++;
}

/* The func creates new block decls idents table in this abstract data
   and returns order number (0, ...) of this table (of block in other
   words). */
int
new_block (void)
{
  int new_block_number;
  vlo_t block_decls;
  
  new_block_number = BLOCKS_NUMBER ();
  VLO_CREATE (block_decls, 0);
  VLO_ADD_MEMORY (block_decl_tables.block_ident_decls,
		  (char *) &block_decls, sizeof (vlo_t));
  return new_block_number;
}

/* The func processes ident used for access to block decl.
   If IDENT is not processed early then set up its (more accurately
   corresponding unique_ident_node) member
   block_decl_ident_number and block decls idents
   tables will contain element for IDENT (it is to be not NULL). */
void
process_block_decl_unique_ident (IR_node_t unique_ident)
{
  if (IR_block_decl_ident_number (unique_ident) < 0)
    {
      IR_set_block_decl_ident_number
	(unique_ident, block_decl_tables.idents_number);
      block_decl_tables.idents_number++;
    }
}

/* The func sets up elements values of block decls idents tables for
   given DECL (in block BLOCK_REF).  Both values are to be not NULL.
   The sequence of the abstract data funcs calls may be described
   regular expr:
   initiate_run_tables
      (
       (new_block | process_block_decl_unique_ident)*
       define_block_decl*
      )* */
void
define_block_decl (IR_node_t decl, IR_node_t block_ref)
{
  IR_node_t null = NULL;
  vlo_t *table_ref;
  int block_number, block_decl_ident_number, i;

  block_number = IR_block_number (block_ref);
  block_decl_ident_number
    = (IR_block_decl_ident_number (IR_unique_ident (IR_ident (decl))));
  if (block_decl_ident_number < 0)
    /* There is no access to identifier. */
    return;
  table_ref = (&LV_BLOCK_IDENT_DECLS_TABLE (block_number));
  if (VLO_LENGTH (*table_ref) <= block_decl_ident_number * sizeof (IR_node_t))
    for (i = VLO_LENGTH (*table_ref) / sizeof (IR_node_t);
	 i <= block_decl_ident_number;
	 i++)
      VLO_ADD_MEMORY (*table_ref, (char *)&null, sizeof (IR_node_t));
  ((IR_node_t *) VLO_BEGIN (*table_ref)) [block_decl_ident_number] = decl;
  IR_set_it_is_declared_in_block (IR_unique_ident (IR_ident (decl)), TRUE);
}

void
finish_run_tables (void)
{
  vlo_t *vlo_ref;

  VLO_DELETE (func_class_tab);
  for (vlo_ref = VLO_BEGIN (block_decl_tables.block_ident_decls);
       (char *) vlo_ref
	 <= (char *) VLO_END (block_decl_tables.block_ident_decls);
       vlo_ref++)
    VLO_DELETE (*vlo_ref);
  VLO_DELETE (block_decl_tables.block_ident_decls);
}
