/*
   Copyright (C) 1997-2002 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of the tool MSTA.

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

extern int original_LR_sets_number;
extern int original_LR_cores_number;
extern int important_original_LR_situations_number;
extern int all_original_LR_situations_number;

void process_conflicts (int DeRemer_flag);
void create_LALR_sets (void);
void create_LALR_sets_with_all_contexts (void);
void create_LR_sets (void);
void make_LALR_optimization (void);
void make_full_LR_sets (void);
