/* The file implements block table (Number->func_class and
   func_class -> Number) and abstract data block_decl_tables.  This
   abstract data mainly serves for access to block decl nodes by
   block_number and decl_ident_number (see commentaries for
   block and decl).  The abstract data is designed and implemented for
   implementation dynamic load another file in the future (see
   commentaries for func define_block_decl).  */

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

#if SIZEOF_CHAR_P > 4
#define BLOCK_TAB 1
#endif

extern vlo_t block_tab;

#ifdef BLOCK_TAB

#define CODE_ID(bl)          (BC_block_number (bl))
#define ID_TO_CODE(id)       (((BC_node_t *) VLO_BEGIN (block_tab)) [id])
#else
#define CODE_ID(bl)          ((int_t) (bl))
#define ID_TO_CODE(id)       ((BC_node_t) (id))
#endif

/* The macro call value is block field idents table element for the
   BLOCK and ident BLOCK_DECL_IDENT_NUMBER.  The element value is NULL
   if the field can not be accessed by '.' with given ident in given
   block.  Otherwise this value is pointer to node representing decl
   with given field ident in given block.  */
#define LV_BLOCK_IDENT_DECL(block, block_decl_ident_number)\
  (block_decl_ident_number < BC_fld_table_len (block) \
   ? ((BC_node_t *) BC_fld_table (block))[block_decl_ident_number] \
   : NULL)

extern void initiate_run_tables (void);
extern void set_block_number (BC_node_t block);
extern void define_block_decl (BC_node_t decl, BC_node_t block);
extern void finish_run_tables (void);
