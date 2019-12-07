/*
   Copyright (C) 1997-2016 Vladimir Makarov.

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

#include "d_common.h"
#include "d_bc.h"
#include "d_runtab.h"

/* Table of all blocks.  */
VARR (BC_node_t) * block_tab;

typedef VARR (BC_node_t) * block_tab_t;
DEF_VARR (block_tab_t);

struct block_decl_tables {
  /* The following VARR contains VARR's in which the pointers to block
     decls stored by unique ident numbers. */
  VARR (block_tab_t) * block_ident_decls;
};

static struct block_decl_tables block_decl_tables;

/* The macro call value is number of blocks in block_decl_tables. */
#define BLOCKS_NUMBER() (VARR_LENGTH (block_tab_t, block_decl_tables.block_ident_decls))

/* The macro call value is block decls idents table (represented
   by VARR) for the block with number BLOCK_NUMBER.  The macro call value is
   l-value. */
#define LV_BLOCK_IDENT_DECLS_TABLE(block_number) \
  (VARR_ADDR (block_tab_t, block_decl_tables.block_ident_decls)[block_number])

/* This func is to be called only once before any work with this
   abstract data. */
void initiate_run_tables (void) {
  VARR_CREATE (block_tab_t, block_decl_tables.block_ident_decls, 2000);
  VARR_CREATE (BC_node_t, block_tab, 800);
}

/* The func creates new block decls idents table in this abstract data. */
void set_block_number (BC_node_t block) {
  int new_block_number;
  block_tab_t block_decls;

  new_block_number = BLOCKS_NUMBER ();
  VARR_CREATE (BC_node_t, block_decls, 0);
  VARR_PUSH (block_tab_t, block_decl_tables.block_ident_decls, block_decls);
  BC_set_block_number (block, new_block_number);
  assert (new_block_number == VARR_LENGTH (BC_node_t, block_tab));
  VARR_PUSH (BC_node_t, block_tab, block);
}

/* The func sets up elements values of block decls idents tables for
   given DECL in BLOCK.  Both values are to be not NULL.  The sequence
   of the abstract data funcs calls may be described regular expr:
   initiate_run_tables
      (
       set_block_number
       define_block_decl*
      )* */
void define_block_decl (BC_node_t decl, BC_node_t block) {
  BC_node_t null = NULL;
  block_tab_t table;
  int block_number, fldid_num, i;

  block_number = BC_block_number (block);
  fldid_num = BC_fldid_num (decl);
  if (fldid_num < 0) /* There is no access to identifier. */
    return;
  table = LV_BLOCK_IDENT_DECLS_TABLE (block_number);
  if (VARR_LENGTH (BC_node_t, table) <= fldid_num) {
    for (i = VARR_LENGTH (BC_node_t, table); i <= fldid_num; i++)
      VARR_PUSH (BC_node_t, table, null);
    BC_set_fld_table (block, (void *) VARR_ADDR (BC_node_t, table));
    BC_set_fld_table_len (block, fldid_num + 1);
  }
  VARR_ADDR (BC_node_t, table)[fldid_num] = decl;
}

void finish_run_tables (void) {
  VARR_DESTROY (BC_node_t, block_tab);
  for (size_t i = 0; i < VARR_LENGTH (block_tab_t, block_decl_tables.block_ident_decls); i++) {
    block_tab_t tab = VARR_GET (block_tab_t, block_decl_tables.block_ident_decls, i);
    VARR_DESTROY (BC_node_t, tab);
  }
  VARR_DESTROY (block_tab_t, block_decl_tables.block_ident_decls);
}
