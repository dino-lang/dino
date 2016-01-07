/*
   FILE NAME:   anal.c

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of the tool SHILKA.

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

   TITLE:       Semantic analyzer of keywords description
                translator (HDT)

   DESCRIPTION: This file tests semantically all description built by
                the HDT parser.

   SPECIAL CONSIDERATION:
         The analyzer is to be called only after HDT parser.
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors of the analyzer.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#else /* In this case we are oriented to ANSI C */
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>
#include "position.h"
#include "errors.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
#include "anal.h"

#include <assert.h>

/* The following function processes keyword definitions in order to
   fix errors of repeated declared keywords and actions
   presence/absence. */

static void
process_keyword_definitions (void)
{
  IR_node_t current_keyword;
  IR_node_t declaration_in_table;
  int action_absence;

  if (IR_keyword_type (description) != NULL)
    {
      if (!w_flag)
        {
          action_absence = FALSE;
          for (current_keyword = IR_keyword_list (description);
               current_keyword != NULL;
               current_keyword = IR_next_keyword (current_keyword))
            action_absence
              = action_absence || IR_action (current_keyword) == NULL;
          if (action_absence)
            warning
              (IR_position (IR_keyword_type (description)),
               "warning -- %%type when there are keywords without action\n");
          if (!IR_other_is_present (description))
            warning
              (IR_position (IR_keyword_type (description)),
               "warning -- %%other is absent when %%type is present\n");
        }
    }
  for (current_keyword = IR_keyword_list (description);
       current_keyword != NULL;
       current_keyword = IR_next_keyword (current_keyword))
    {
      if (IR_frequency (current_keyword) == 0)
        error (FALSE, IR_position (current_keyword),
               "zero keyword `%s' frequency",
               keyword_representation (current_keyword));
      declaration_in_table = insert_keyword (current_keyword);
      if (declaration_in_table != current_keyword)
        {
          error (FALSE, IR_position (current_keyword),
                 "repeated definition of keyword `%s'",
                 keyword_representation (current_keyword));
          append_message (IR_position (declaration_in_table),
                          "here the first definition");
        }
    }
}

/* The following function calls functions for semantic analysis of all
   description. */

void
analyze_description (void)
{
  process_keyword_definitions ();
}
