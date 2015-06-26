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

#include "d_common.h"
#include "UnicodeDB.c"

static const struct uc_tree_node *cached_node = NULL;

static const struct uc_tree_node *
find_ucode_tree_node (int c, const struct uc_tree_node *from)
{
  if (from == NULL)
    return NULL;
  if (from->start_code > c)
    return find_ucode_tree_node (c, from->l);
  else if (c <= from->last_code)
    return from;
  else
    return find_ucode_tree_node (c, from->r);
}

static const struct ucode_desc *
find_ucode_desc (int c)
{
  const struct uc_tree_node *node;
  int desc_ind;
  
  if (cached_node != NULL
      && cached_node->start_code <= c && c <= cached_node->last_code)
    node = cached_node;
  else
    {
      node = find_ucode_tree_node (c, &uc_tree_root);
      if (node == NULL)
	return NULL;
    }
  desc_ind = node->uc_ind[c - node->start_code];
  if (desc_ind == 255)
    return NULL;
  cached_node = node;
  return &ucode_descs [desc_ind + node->start_index];
}

int
check_ucode_db (void)
{
#ifndef NDEBUG
  int i;

  for (i = 0; i < sizeof (ucode_descs) / sizeof (struct ucode_desc); i++)
    if (find_ucode_desc (ucode_descs[i].code) != &ucode_descs[i])
      return FALSE;
#endif
  return TRUE;
}
