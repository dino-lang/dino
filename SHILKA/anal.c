/*
   FILE NAME:   anal.c

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
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>
#include "position.h"
#include "errors.h"
#include "ird.h"
#include "common.h"
#include "tab.h"
#include "anal.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

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
