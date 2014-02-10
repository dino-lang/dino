/*
   Copyright (C) 1997-2014 Vladimir Makarov.

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
#include "d_bc.h"
#include "d_runtab.h"

/* Table of all blocks.  */
vlo_t block_tab;

struct block_decl_tables block_decl_tables;

/* This func is to be called only once before any work with this
   abstract data. */
void
initiate_run_tables (void)
{
  VLO_CREATE (block_decl_tables.block_ident_decls, 2000);
#ifdef BLOCK_TAB
  VLO_CREATE (block_tab, 800);
#endif
}

/* The func creates new block decls idents table in this abstract data. */
void
set_block_number (BC_node_t block)
{
  int new_block_number;
  vlo_t block_decls;
  
  new_block_number = BLOCKS_NUMBER ();
  VLO_CREATE (block_decls, 0);
  VLO_ADD_MEMORY (block_decl_tables.block_ident_decls,
		  (char *) &block_decls, sizeof (vlo_t));
  BC_set_block_number (block, new_block_number);
  assert (new_block_number * sizeof (BC_node_t)
	  == VLO_LENGTH (block_tab));
  VLO_ADD_MEMORY (block_tab, &block, sizeof (BC_node_t));
}

/* The func sets up elements values of block decls idents tables for
   given DECL in BLOCK.  Both values are to be not NULL.  The sequence
   of the abstract data funcs calls may be described regular expr:
   initiate_run_tables
      (
       set_block_number
       define_block_decl*
      )* */
void
define_block_decl (BC_node_t decl, BC_node_t block)
{
  BC_node_t null = NULL;
  vlo_t *table_ref;
  int block_number, decl_ident_num, i;

  block_number = BC_block_number (block);
  decl_ident_num = BC_ident_num (decl);
  if (decl_ident_num < 0)
    /* There is no access to identifier. */
    return;
  table_ref = (&LV_BLOCK_IDENT_DECLS_TABLE (block_number));
  if (VLO_LENGTH (*table_ref) <= decl_ident_num * sizeof (BC_node_t))
    for (i = VLO_LENGTH (*table_ref) / sizeof (BC_node_t);
	 i <= decl_ident_num;
	 i++)
      VLO_ADD_MEMORY (*table_ref, (char *)&null, sizeof (BC_node_t));
  ((BC_node_t *) VLO_BEGIN (*table_ref)) [decl_ident_num] = decl;
}

void
finish_run_tables (void)
{
  vlo_t *vlo_ref;

  VLO_DELETE (block_tab);
  for (vlo_ref = VLO_BEGIN (block_decl_tables.block_ident_decls);
       (char *) vlo_ref
	 <= (char *) VLO_END (block_decl_tables.block_ident_decls);
       vlo_ref++)
    VLO_DELETE (*vlo_ref);
  VLO_DELETE (block_decl_tables.block_ident_decls);
}
