/*
   FILE NAME:   anal.c

   Copyright (C) 1997-2002 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of the tool SPRUT.

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

   TITLE:       Semantic analyzer of SPRUT (internal representation description
                translator)

   DESCRIPTION: This file tests semantically all description parts built by
                the SPRUT parser and after that makes equivalent one level
                description (i.e. with one description part).

   SPECIAL CONSIDERATION:
         The analyzer is to be called only after SPRUT parser.
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

#include <string.h>
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

/* This page contains functions for traversing internal representation. */

/* This recursive function traverses description part hierarchy in
   depth first order. */

static void
traverse_description_part_hierarchy
  (IR_node_t top_description_part,
   void (*applied_function) (IR_node_t description_part))
{
  IR_node_t current_part;

  if (top_description_part == NULL)
    return;
  for (current_part = IR_first_basic_description_part (top_description_part);
       current_part != NULL;
       current_part = IR_next_basic_description_part (current_part))
    traverse_description_part_hierarchy (current_part, applied_function);
  (*applied_function) (top_description_part);
}

/* This function traverses all node type declarations (even if it is
   node type continuation, i.e. without super type) in given
   description part in the same order as in source file.  Remember
   that all declarations of a node type are represented by nodes
   `node_type'. */

static void
traverse_description_part_node_types
  (IR_node_t description_part, void (*applied_function) (IR_node_t node_type))
{
  IR_node_t node_type;

  if (IR_last_type (description_part) == NULL)
    return;
  for (node_type = IR_next_type (IR_last_type (description_part));;
       node_type = IR_next_type (node_type))
    {
      if (IR_NODE_MODE (node_type) == IR_NM_node_type)
        (*applied_function) (node_type);
      else
        assert (IR_NODE_MODE (node_type) == IR_NM_predefined_type);
      if (node_type == IR_last_type (description_part))
        break;
    }
}

/* This function traverses all fields which were declared in given
   description part and node type of which is in table of node type.
   Field is processed only once.  This function must be called only
   after inclusion all node types into table of types and fields into
   table of node fields.  After that all fields of a node type are
   collected in a node type declaration which is included into the
   table of type (remember that there may be a few declarations of a
   node type and that all declarations of a node type are represented
   by internal representation nodes `node_type').  No more one
   activations of this function can exist simultaneously. */

static void
traverse_included_description_part_fields
  (IR_node_t description_part, void (*applied_function) (IR_node_t field))
{
  IR_node_t current_field;
  IR_node_t current_type;
  IR_node_t original_field;
  IR_node_t original_type;

  if (IR_last_type (description_part) == NULL)
    return;
  /* This cycle is needed to guarantee the single pass of node type which
     is in the table. */
  for (current_type = IR_next_type (IR_last_type (description_part));;
       current_type = IR_next_type (current_type))
    {
      original_type = find_type (IR_type_identifier (current_type));
      assert (original_type != NULL);
      if (IR_NODE_MODE (original_type) == IR_NM_node_type)
        IR_set_traverse_flag (original_type, FALSE);
      if (current_type == IR_last_type (description_part))
        break;
    }
  for (current_type = IR_next_type (IR_last_type (description_part));;
       current_type = IR_next_type (current_type))
    {
      original_type = find_type (IR_type_identifier (current_type));
      if (IR_NODE_MODE (original_type) == IR_NM_node_type
          && !IR_traverse_flag (original_type)
          && IR_last_field (original_type) != NULL)
        {
          for (current_field = IR_next_field (IR_last_field (original_type));;
               current_field = IR_next_field (current_field))
            {
              if (IR_NODE_MODE (current_field) == IR_NM_field)
                {
                  original_field
                    = find_node_field (IR_field_identifier (current_field),
                                       original_type);
                  if (strcmp (IR_position (original_field).file_name,
                              IR_position (description_part).file_name) == 0)
                    (*applied_function) (original_field);
                }
              if (current_field == IR_last_field (original_type))
                break;
            }
          IR_set_traverse_flag (original_type, TRUE);
        }
      if (current_type == IR_last_type (description_part))
        break;
    }
}



/* This page contains semantic functions.  The most of the functions are
   called from traverse functions. */

/* This function inserts identifiers of double node type declarations
   of given description part into the table of double node type
   declaration identifiers.  In the case of repeated double node type
   declaration the function generates warning. */

static void
process_double_declaration_list (IR_node_t description_part)
{
  IR_node_t current_double_declaration;
  IR_node_t included_double_declaration_identifier;

  if (IR_last_double_declaration (description_part) == NULL)
    return;
  for (current_double_declaration
       = IR_next_double_declaration (IR_last_double_declaration
                                     (description_part));;
       current_double_declaration
       = IR_next_double_declaration (current_double_declaration))
    {
      included_double_declaration_identifier
        = insert_double_declaration_identifier
          (IR_double_declaration_identifier (current_double_declaration));
      if (included_double_declaration_identifier
          != IR_double_declaration_identifier (current_double_declaration))
        {
          assert
            (strcmp
             (IR_identifier_itself (IR_double_declaration_identifier
                                    (current_double_declaration)),
              IR_identifier_itself (included_double_declaration_identifier))
             == 0);
           if (v_flag)
             {
               warning
                 (IR_position (current_double_declaration),
                  "warning -- repeated declaration `%s' as double node type",
                  IR_identifier_itself
                  (included_double_declaration_identifier));
               append_message
                 (IR_position (included_double_declaration_identifier),
                  "here the first declaration of double node type");
             }
        }
      if (current_double_declaration 
          == IR_last_double_declaration (description_part))
        break;
    }
}

/* This function processes all predefined type and node type
   declarations of given description part.  The processing consists of
           insertion of types into the table of types,
           generation of warning for repeated predefined type
              declaration,
           fixing error for predefined type declaration which is early
              declared as node type,
           fixing error for node type declaration which is early
              declared as predefined type,
           fixing error for repeated node type declaration with
              basic super types,
           fixing error for abstract node type declaration without
              basic super types,
           setting up fields `first_declaration_level' and
              `node_type_number' for node type declarations which
              are inserted in the table,
           setting up field `declaration_level' for all fields of
              all processed node type declaration.
           moving super types from given node type declaration
              to corresponding node type declaration which is in
              the table of types.
   Remember that there may be a few declarations of a node type and
   that all declarations of a node type are represented by internal
   representation nodes `node_type'. */

static void
process_type_list (IR_node_t description_part)
{
  IR_node_t current_field;
  IR_node_t current_type;
  IR_node_t original_type;
  IR_node_t first_additional_super_type_list_element;
  IR_node_t last_additional_super_type_list_element;

  if (IR_last_type (description_part) == NULL)
    return;
  for (current_type = IR_next_type (IR_last_type (description_part));;
       current_type = IR_next_type (current_type))
    {
      if (IR_NODE_MODE (current_type) == IR_NM_predefined_type)
        {
          original_type = insert_type (current_type);
          if (original_type != current_type)
            {
              if (IR_NODE_MODE (original_type) != IR_NM_predefined_type)
                {
                  error
                    (FALSE, IR_position (current_type),
                     "identifier `%s' is declared early as node type",
                     IR_identifier_itself (IR_type_identifier (current_type)));
                  append_message (IR_position (original_type),
                                  "here declaration of the node type");
                }
              else if (v_flag)
                {
                  warning
                    (IR_position (current_type),
                     "warning -- repeated declaration of predefined type `%s'",
                     IR_identifier_itself (IR_type_identifier (current_type)));
                  append_message (IR_position (original_type),
                                  "here first declaration of predefined type");
                }
            }
        }
      else
        {
          assert (IR_NODE_MODE (current_type) == IR_NM_node_type);
          /* Set up field level */
          if (IR_last_field (current_type) != NULL)
            for (current_field = IR_next_field (IR_last_field (current_type));;
                 current_field = IR_next_field (current_field))
              {
                if (IR_NODE_MODE (current_field) == IR_NM_field)
                  IR_set_declaration_level
                    (current_field,
                     IR_description_part_level (description_part));
                if (current_field == IR_last_field (current_type))
                  break;
              }
          original_type = insert_type (current_type);
          if (original_type != current_type)
            {
              if (IR_NODE_MODE (original_type) == IR_NM_predefined_type)
                {
                  error
                    (FALSE, IR_position (current_type),
                     "identifier `%s' is declared early as predefined type",
                     IR_identifier_itself (IR_type_identifier (current_type)));
                  append_message (IR_position (original_type),
                                  "here declaration of predefined type");
                }
              else
                {
                  /* Types are node types. */
                  if (IR_abstract_flag (current_type)
                      && (IR_first_super_type_list_element (current_type)
                          == NULL
                          || !IR_basic_super_types_flag (current_type)))
                    error (FALSE, IR_position (current_type),
                           "`%%abstract' can't be without basic super types");
                  if (IR_first_super_type_list_element (current_type) != NULL)
                    {
                      if (IR_first_super_type_list_element (original_type)
                          != NULL
                          && IR_basic_super_types_flag (current_type)
                          && IR_basic_super_types_flag (original_type))
                        {
                          error
                            (FALSE, IR_position (current_type),
                             "`%s' is declared early with basic super types",
                             IR_identifier_itself (IR_type_identifier
                                                   (current_type)));
                          append_message
                            (IR_position (IR_type_identifier (original_type)),
                             "here declaration with basic super types");
                        }
                      else
                        {
                          if (IR_first_super_type_list_element (original_type)
                              == NULL)
                            {
                              /* Move the list. */
                              IR_set_first_super_type_list_element
                                (original_type,
                                 IR_first_super_type_list_element
                                 (current_type));
                              IR_set_last_super_type_list_element
                                (original_type,
                                 IR_last_super_type_list_element
                                 (current_type));
                              IR_set_basic_super_types_flag
                                (original_type,
                                 IR_basic_super_types_flag (current_type));
                            }
                          else
                            {
                              /* Merge two lists. */
                              if (IR_basic_super_types_flag (current_type))
                                {
                                  first_additional_super_type_list_element
                                    = IR_first_super_type_list_element
                                      (original_type);
                                  last_additional_super_type_list_element
                                    = IR_last_super_type_list_element
                                      (original_type);
                                  IR_set_first_super_type_list_element
                                    (original_type,
                                     IR_first_super_type_list_element
                                     (current_type));
                                  IR_set_last_super_type_list_element
                                    (original_type,
                                     IR_last_super_type_list_element
                                     (current_type));
                                }
                              else
                                {
                                  first_additional_super_type_list_element
                                    = IR_first_super_type_list_element
                                      (current_type);
                                  last_additional_super_type_list_element
                                    = IR_last_super_type_list_element
                                      (current_type);
                                }
                              IR_set_next_super_type_list_element
                                (IR_last_super_type_list_element
                                 (original_type),
                                 first_additional_super_type_list_element);
                              IR_set_last_super_type_list_element
                                (original_type,
                                 last_additional_super_type_list_element);
                              IR_set_basic_super_types_flag
                                (original_type,
                                 IR_basic_super_types_flag (current_type)
                                 || IR_basic_super_types_flag (original_type));
                            }
                          /* Delete from source list. */
                          IR_set_first_super_type_list_element (current_type,
                                                                NULL);
                          IR_set_last_super_type_list_element (current_type,
                                                               NULL);
                        }
                    }
                }
            }
          else
            {
              IR_set_first_declaration_level
                (current_type, IR_description_part_level (description_part));
              IR_set_node_type_number (current_type, number_of_node_types);
              number_of_node_types++;
            }
        }
      if (current_type == IR_last_type (description_part))
        break;
    }
}

/* This function sets up field `double_node_flag' of given node type
   to TRUE iff there is corresponding double node type declaration in
   given description.  Given node type must represents node type
   declaration in given description part. */

static void
make_precise_double_node_flag (IR_node_t description_part, IR_node_t node_type)
{
  IR_node_t included_identifier;

  included_identifier = find_double_declaration_identifier (IR_type_identifier
                                                            (node_type));
  if (included_identifier != NULL
      && strcmp (IR_position (included_identifier).file_name,
                 (IR_position (description_part).file_name)) == 0)
    IR_set_double_node_flag (node_type, TRUE);
}

/* This function calls `make_precise_double_node_flag' for all node
   type declaration in given description part and for node type
   `%root'.  Field `double_node_flag' of all node type declarations is
   set up to FALSE before call of the function. Remember that there
   may be a few declarations of a node type and that all declarations
   of a node type are represented by internal representation nodes
   `node_type'. */

static void
set_basic_double_node_flags (IR_node_t description_part)
{
  IR_node_t current_type;

  if (IR_last_type (description_part) == NULL)
    return;
  for (current_type = IR_next_type (IR_last_type (description_part));;
       current_type = IR_next_type (current_type))
    {
      if (IR_NODE_MODE (current_type) == IR_NM_node_type)
        make_precise_double_node_flag (description_part, current_type);
      else
        assert (IR_NODE_MODE (current_type) == IR_NM_predefined_type);
      if (current_type == IR_last_type (description_part))
        break;
    }
  /* For %root */
  make_precise_double_node_flag (description_part, root_node_type);
}

/* The following recursive function marks given node type and all its
   super types by value `flag'. */

static void
mark_node_type_and_its_super_types (IR_node_t node_type, int flag)
{
  IR_node_t current_super_type_list_element;

  assert (node_type != NULL);
  IR_set_mark_flag (node_type, flag);
  for (current_super_type_list_element
       = IR_first_super_type_list_element (node_type);
       current_super_type_list_element != NULL;
       current_super_type_list_element
       = IR_next_super_type_list_element (current_super_type_list_element))
    if (IR_immediate_super_type (current_super_type_list_element) != NULL)
      mark_node_type_and_its_super_types
        (IR_immediate_super_type (current_super_type_list_element), flag);
}

/* The following recursive function returns true if given node type
   has a marked super type (except for root node type) or given node
   type is marked itself.  */

static int
it_has_a_non_root_marked_super_type (IR_node_t node_type)
{
  IR_node_t current_super_type_list_element;

  assert (node_type != NULL);
  if (node_type != root_node_type)
    {
      if (IR_mark_flag (node_type))
        return TRUE;
      for (current_super_type_list_element
           = IR_first_super_type_list_element (node_type);
           current_super_type_list_element != NULL;
           current_super_type_list_element
           = IR_next_super_type_list_element (current_super_type_list_element))
        if (IR_immediate_super_type (current_super_type_list_element) != NULL
            &&
            it_has_a_non_root_marked_super_type
            (IR_immediate_super_type (current_super_type_list_element)))
          return TRUE;
    }
  return FALSE;
}

/* The following function returns TRUE if given node types have a
   common super type (except for root node type). */

static int
there_is_common_super_type (IR_node_t node_type_1, IR_node_t node_type_2)
{
  int result;

  assert (node_type_1 != NULL && node_type_2 != NULL);
  mark_node_type_and_its_super_types (node_type_1, TRUE);
  result = it_has_a_non_root_marked_super_type (node_type_2);
  mark_node_type_and_its_super_types (node_type_1, FALSE);
  return result;
}

/* This function processes given node type declaration which is in the
   table of types.  The processing consists of
          fixing error for given node type declaration
             have undefined super types identifiers,
          fixing error for using undeclared identifiers as super types,
          fixing error for using predefined type identifier as
             super type,
          fixing error for using intersected super types,
          setting up fields `immediate_super_type' for given
             node type declaration.
   This function is called from function
   `traverse_description_part_node_types'.  The node type is processed
   only once by the function.  Remember that there may be a few
   declarations of a node type and that all declarations of a node
   type are represented by internal representation nodes
   `node_type'. */

static void
set_and_test_super_types (IR_node_t node_type)
{
  IR_node_t original_type;
  IR_node_t super_type;
  IR_node_t current_super_type_list_element;
  IR_node_t next_super_type_list_element;
  IR_node_t previous_super_type_list_element;
  IR_node_t check_super_type_list_element;
  int change_previous_flag;

  original_type = find_type (IR_type_identifier (node_type));
  assert (original_type != NULL);
  /* The original node may be processed many times this is necessary
     because additional super types can be in new files. */
  if (/* The following test is needed because of possible previous errors. */
      IR_NODE_MODE (original_type) != IR_NM_node_type)
    return;
  if (original_type == node_type
      && (IR_first_super_type_list_element (original_type) == NULL
          || !IR_basic_super_types_flag (original_type))
      && original_type != root_node_type)
    error
      (FALSE, IR_position (original_type),
       "basic super types aren't defined anywhere for node type `%s'",
       IR_identifier_itself (IR_type_identifier (original_type)));
  for (current_super_type_list_element
       = IR_first_super_type_list_element (original_type),
       previous_super_type_list_element = NULL;
       current_super_type_list_element != NULL;
       current_super_type_list_element = next_super_type_list_element)
    {
      next_super_type_list_element
        = IR_next_super_type_list_element (current_super_type_list_element);
      if (!IR_setting_and_testing_super_type_was_made
          (current_super_type_list_element))
        {
          IR_set_setting_and_testing_super_type_was_made
            (current_super_type_list_element, TRUE);
          super_type = find_type (IR_immediate_super_type_identifier
                                  (current_super_type_list_element));
          if (super_type == NULL)
            error
              (FALSE, IR_position (IR_immediate_super_type_identifier
                                   (current_super_type_list_element)),
               "undefined identifier `%s' is used as super type",
               IR_identifier_itself (IR_immediate_super_type_identifier
                                     (current_super_type_list_element)));
          else if (IR_NODE_MODE (super_type) == IR_NM_predefined_type)
            {
              error
                (FALSE, IR_position (IR_immediate_super_type_identifier
                                     (current_super_type_list_element)),
                 "predefined type `%s' is used as super type",
                 IR_identifier_itself (IR_immediate_super_type_identifier
                                       (current_super_type_list_element)));
              append_message (IR_position (super_type),
                              "here declaration of predefined type");
              super_type = NULL;
            }
          else
            assert (IR_NODE_MODE (super_type) == IR_NM_node_type);
          IR_set_immediate_super_type (current_super_type_list_element,
                                       super_type);
          /* Check common super types. */
          change_previous_flag = TRUE;
          for (check_super_type_list_element
               = IR_first_super_type_list_element (original_type);
               check_super_type_list_element
               != current_super_type_list_element;
               check_super_type_list_element
               = IR_next_super_type_list_element
               (check_super_type_list_element))
            if (super_type != NULL
                && (IR_immediate_super_type (check_super_type_list_element)
                    != NULL))
              {
                if (super_type
                    == IR_immediate_super_type (check_super_type_list_element))
                  {
                    warning
                      (IR_position (IR_immediate_super_type_identifier
                                    (current_super_type_list_element)),
                       "repeated occurrence `%s' as super type of `%s' is ignored",
                       IR_identifier_itself
                       (IR_immediate_super_type_identifier
                        (current_super_type_list_element)),
                       IR_identifier_itself (IR_type_identifier
                                             (original_type)));
                    /* Delete repeated occurrence. */
                    if (previous_super_type_list_element == NULL)
                      IR_set_first_super_type_list_element
                        (original_type, next_super_type_list_element);
                    else
                      IR_set_next_super_type_list_element
                        (previous_super_type_list_element,
                         next_super_type_list_element);
                    if (next_super_type_list_element == NULL)
                      IR_set_last_super_type_list_element
                        (original_type, previous_super_type_list_element);
                    change_previous_flag = FALSE;
                    break;
                  }
                else if (there_is_common_super_type
                         (IR_immediate_super_type
                          (check_super_type_list_element), super_type))
                  error
                    (FALSE, IR_position (IR_immediate_super_type_identifier
                                         (current_super_type_list_element)),
                     "node type `%s' has intersected super types `%s' and `%s'",
                     IR_identifier_itself (IR_type_identifier (original_type)),
                     IR_identifier_itself (IR_immediate_super_type_identifier
                                           (current_super_type_list_element)),
                     IR_identifier_itself (IR_immediate_super_type_identifier
                                           (check_super_type_list_element)));
              }
          if (change_previous_flag)
            previous_super_type_list_element = current_super_type_list_element;
        }
    }
}

/* The following recursive function returns TRUE if given node type
   has a super type equal to the original node type.  The function is
   invoked such way it is known that given node type is a super type
   of the original node type. */

static int
there_is_super_type_cycle (IR_node_t node_type, IR_node_t original_node_type)
{
  IR_node_t current_super_type_list_element;
  IR_node_t immediate_super_type;

  if (node_type == original_node_type)
    return TRUE;
  for (current_super_type_list_element
       = IR_first_super_type_list_element (node_type);
       current_super_type_list_element != NULL;
       current_super_type_list_element
       = IR_next_super_type_list_element (current_super_type_list_element))
    {
      immediate_super_type
        = IR_immediate_super_type (current_super_type_list_element);
      if (immediate_super_type != NULL
          && there_is_super_type_cycle (immediate_super_type,
                                        original_node_type))
        return TRUE;
    }
  return FALSE;
}

/* This function processes given node type declaration which is in the
   table of types.  The processing consists of only fixing error for
   cycle in super types of given node type.  This function is called
   from function `traverse_description_part_node_types'.  The node
   type is processed only once by the function.  Remember that there
   may be a few declarations of a node type and that all declarations
   of a node type are represented by internal representation nodes
   `node_type'. */

static void
test_super_type_cycle (IR_node_t node_type)
{
  IR_node_t original_type;
  IR_node_t current_super_type_list_element;
  IR_node_t immediate_super_type;

  original_type = find_type (IR_type_identifier (node_type));
  assert (original_type != NULL);
  /* The original node may be processed many times this is necessary
     because additional super types can be in new files. */
  if (/* The following test is needed because of possible previous errors. */
      IR_NODE_MODE (original_type) == IR_NM_node_type)
    {
      for (current_super_type_list_element
           = IR_first_super_type_list_element (node_type);
           current_super_type_list_element != NULL;
           current_super_type_list_element
           = IR_next_super_type_list_element (current_super_type_list_element))
        if (!IR_testing_super_type_cycle_was_made
            (current_super_type_list_element))
          {
            IR_set_testing_super_type_cycle_was_made
              (current_super_type_list_element, TRUE);
            immediate_super_type
              = IR_immediate_super_type (current_super_type_list_element);
            if (immediate_super_type != NULL
                && there_is_super_type_cycle (immediate_super_type,
                                              original_type))
              error (FALSE, IR_position (IR_type_identifier
                                         (immediate_super_type)),
                     "cycle in super type `%s' of node type `%s'",
                     IR_identifier_itself (IR_type_identifier
                                           (immediate_super_type)),
                     IR_identifier_itself (IR_type_identifier
                                           (original_type)));
          }
    }
}

/* The following recursive function sets up field
   `secondary_super_type_flag' for given node type and for all its
   super types below given original node type. */

static void
auxiliary_function_set_secondary_super_type_flags
  (IR_node_t node_type, IR_node_t original_node_type)
{
  IR_node_t current_super_type_list_element;

  if (node_type == NULL || node_type == original_node_type)
    return;
  IR_set_secondary_super_type_flag (node_type, TRUE);
  for (current_super_type_list_element
       = IR_first_super_type_list_element (node_type);
       current_super_type_list_element != NULL;
       current_super_type_list_element
       = IR_next_super_type_list_element (current_super_type_list_element))
    auxiliary_function_set_secondary_super_type_flags
      (IR_immediate_super_type (current_super_type_list_element),
       original_node_type);
}

/* The following function sets up fields `secondary_super_type_flag'
   for all super types of given node type with the aid of function
   `auxiliary_function_set_secondary_super_type_flags'. */

static void
set_secondary_super_type_flags (IR_node_t node_type)
{
  IR_node_t current_super_type_list_element;
  IR_node_t original_type;

  original_type = find_type (IR_type_identifier (node_type));
  assert (original_type != NULL);
  /* The following test is needed because of possible previous errors. */
  if (IR_NODE_MODE (original_type) != IR_NM_node_type)
    return;
  /* The original node may be processed many times this is necessary
     because additional super types can be in new files. */
  current_super_type_list_element
    = IR_first_super_type_list_element (original_type);
  if (current_super_type_list_element != NULL)
    for (;;)
      {
        current_super_type_list_element
          = IR_next_super_type_list_element (current_super_type_list_element);
        if (current_super_type_list_element == NULL)
          break;
        auxiliary_function_set_secondary_super_type_flags
          (IR_immediate_super_type (current_super_type_list_element),
           original_type);
      }
}

/* The following recursive function sets up field `double_node_flag'
   for all super types of given node if the field of given original
   node type is already set up. */

static void
auxiliary_function_set_double_node_flags_for_subtypes
  (IR_node_t node_type, IR_node_t original_node_type)
{
  IR_node_t current_super_type_list_element;
  IR_node_t immediate_super_type;

  if (IR_double_node_flag (node_type))
    IR_set_double_node_flag (original_node_type, TRUE);
  else
    for (current_super_type_list_element
         = IR_first_super_type_list_element (node_type);
         current_super_type_list_element != NULL;
         current_super_type_list_element
         = IR_next_super_type_list_element (current_super_type_list_element))
      {
        immediate_super_type
          = IR_immediate_super_type (current_super_type_list_element);
        if (immediate_super_type != NULL
            && immediate_super_type != original_node_type) 
          auxiliary_function_set_double_node_flags_for_subtypes
            (immediate_super_type, original_node_type);
      }
}

/* This function processes node type declaration which is in the table
   of types and corresponds to given node type declaration.  The
   processing consists of setting up field `double_node_flag' to TRUE
   iff any its super type has field `double_node_flag' equals to
   TRUE. This function is called from function
   `traverse_description_part_node_types'.  As a consequence the node
   types may be processed a few time (if there a few node type
   declarations in given description part).  Remember that there may
   be a few declarations of a node type and that all declarations of a
   node type are represented by internal representation nodes
   `node_type'. */

static void
set_double_node_flags_for_subtypes (IR_node_t node_type)
{
  IR_node_t original_type;

  original_type = find_type (IR_type_identifier (node_type));
  assert (original_type != NULL);
  /* The following test is needed because of previous errors. */
  if (IR_NODE_MODE (original_type) == IR_NM_node_type)
    auxiliary_function_set_double_node_flags_for_subtypes (original_type,
                                                           original_type);
}

/* This function processes all fields of given node type declaration.
   The processing consists of
           fixing error for using undefined identifier as field type,
           setting up field `field_type',
           setting up field `node_type' to node type declaration which
              is in the table of types and corresponds to given
              node type declaration,
           insertion of field into the table of node fields.
           fixing error for repeated declaration of field in
              given node type,
           moving field from given node type declaration
              to corresponding node type declaration which is in
              the table of types.
   Remember that there may be a few declarations of a node type and
   that all declarations of a node type are represented by internal
   representation nodes `node_type'.  This function is called from
   function `traverse_description_part_node_types'.  All fields are
   processed only once by this function. */

static void
insert_field_and_set_field_type (IR_node_t node_type)
{
  IR_node_t current_field;
  IR_node_t original_type;
  IR_node_t field_type;
  IR_node_t original_field;
  IR_node_t previous_field;
  IR_node_t next_field;
  int move_flag;

  original_type = find_type (IR_type_identifier (node_type));
  /* The following test is needed because of previous errors. */
  if (IR_NODE_MODE (original_type) == IR_NM_node_type
      && IR_last_field (node_type) != NULL)
    for (previous_field = IR_last_field (node_type),
         current_field = IR_next_field (previous_field);;
         current_field = next_field)
      {
        move_flag = FALSE;
        next_field = IR_next_field (current_field);
        if (IR_NODE_MODE (current_field) == IR_NM_field)
          {
            /* Set up field type. */
            field_type = find_type (IR_field_type_identifier (current_field));
            if (field_type == NULL)
              error
                (FALSE, IR_position (IR_field_type_identifier (current_field)),
                 "undefined identifier `%s' is used as field type",
                 IR_identifier_itself (IR_field_type_identifier
                                       (current_field)));
            else
              assert (IR_NODE_MODE (field_type) == IR_NM_predefined_type
                      || IR_NODE_MODE (field_type) == IR_NM_node_type);
            IR_set_field_type (current_field, field_type);
            /* Test with fields in the same node type. */
            IR_set_node_type (current_field, original_type);
            original_field = insert_node_field (current_field);
            if (original_field != current_field)
              {
                error
                  (FALSE, IR_position (current_field),
                   "field `%s' is declared early in node type `%s'",
                   IR_identifier_itself (IR_field_identifier (current_field)),
                   IR_identifier_itself (IR_type_identifier (original_type)));
                append_message (IR_position (original_field),
                                "here first declaration of the field");
              }
            else if (original_type != node_type)
              move_flag = TRUE;
          }
        else
          {
            assert (IR_NODE_MODE (current_field) == IR_NM_action
                    || IR_NODE_MODE (current_field) == IR_NM_constraint);
              if (original_type != node_type)
                move_flag = TRUE;
          }
        if (move_flag)
          {
            /* Delete from source list */
            IR_set_next_field (previous_field, next_field);
            /* Add field to original node type. */
            if (IR_last_field (original_type) != NULL)
              {
                IR_set_next_field (current_field,
                                   IR_next_field (IR_last_field
                                                  (original_type)));
                IR_set_next_field (IR_last_field (original_type),
                                   current_field);
              }
            else
              IR_set_next_field (current_field, current_field);
            IR_set_last_field (original_type, current_field);
            if (current_field == IR_last_field (node_type))
              {
                if (current_field == next_field)
                  IR_set_last_field (node_type, NULL);
                break;
              }
          }
        else
          {
            previous_field = current_field;
            if (current_field == IR_last_field (node_type))
              break;
          }
      }
}

/* The following recursive function checks repeated declaration of
   given field in super types of given node type. */

static void
test_field_presence_in_super_type_hierarchy (IR_node_t field,
                                             IR_node_t node_type,
                                             IR_node_t original_node_type)
{
  IR_node_t super_type;
  IR_node_t field_declaration_in_super_type;
  IR_node_t current_super_type_list_element;

  for (current_super_type_list_element
       = IR_first_super_type_list_element (node_type);
       current_super_type_list_element != NULL;
       current_super_type_list_element
       = IR_next_super_type_list_element (current_super_type_list_element))
    {
      super_type = IR_immediate_super_type (current_super_type_list_element);
      if (super_type != NULL && super_type != original_node_type)
        {
          field_declaration_in_super_type
            = find_node_field (IR_field_identifier (field), super_type);
          if (field_declaration_in_super_type != NULL)
            {
              error (FALSE, IR_position (field),
                     "field `%s' is declared early in super type `%s'",
                     IR_identifier_itself (IR_field_identifier (field)),
                     IR_identifier_itself (IR_type_identifier (super_type)));
              append_message (IR_position (field_declaration_in_super_type),
                              "here first declaration of the field");
            }
          test_field_presence_in_super_type_hierarchy (field, super_type,
                                                       original_node_type);
        }
    }
}

/* This recursive function processes given field.  The processing
   consists of only fixing error for declaration of the same field in
   any node type which is a super type of node type of given
   field.  This function is called from function
   `traverse_included_description_part_fields'.  As a consequence only
   fields which are in the table of node fields are processed only
   once. */

static void
test_field_presence_in_super_types (IR_node_t field)
{
  /* Test with fields in super types. */
  assert (IR_node_type (field) != NULL);
  test_field_presence_in_super_type_hierarchy
    (field, IR_node_type (field), IR_node_type (field));
}

/* This function sets up field `double_field_flag' of given field to
   TRUE iff given field is not class field and its type is node type
   with field `double_node_flag' equal to TRUE.  This function is
   called from function `traverse_included_description_part_fields'.
   As a consequence only fields which are in the table of node fields
   are processed only once. */

static void
set_double_field_flag_by_double_node_types (IR_node_t field)
{
  if (IR_field_type (field) != NULL && IR_declaration_part (field) != DP_CLASS
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type
      && IR_double_node_flag (IR_field_type (field)))
    IR_set_double_field_flag (field, TRUE);
}

/* This function sets up field `double_node_flag' of node type which
   is type of given field to TRUE iff field `double_node_flag' of
   given field is equal to TRUE.  This function is called from
   function `traverse_included_description_part_fields'.  As a
   consequence only fields which are in the table of node fields are
   processed only once. */

static void
set_double_node_flag_by_double_fields (IR_node_t field)
{
  if (IR_double_field_flag (field) && IR_field_type (field) != NULL
      && IR_NODE_MODE (IR_field_type (field)) == IR_NM_node_type)
    IR_set_double_node_flag (IR_field_type (field), TRUE);
}

/* This recursive function determines that one node type is a super
   type of another node type.  Third parameter is neccessary because
   the cycle in super types may exist.  The function returns TRUE iff
   the first node type is a super type of the second node type.
   Parameters are to be node types. */

static int
auxiliary_function_it_is_super_type (IR_node_t super_type, IR_node_t sub_type,
                                     IR_node_t original_sub_type)
{
  IR_node_t current_super_type_list_element;
  IR_node_t immediate_super_type;

  assert (super_type != NULL && sub_type != NULL
          && IR_NODE_MODE (super_type) == IR_NM_node_type
          && IR_NODE_MODE (sub_type) == IR_NM_node_type);
  if (sub_type == super_type)
    return TRUE;
  for (current_super_type_list_element
       = IR_first_super_type_list_element (sub_type);
       current_super_type_list_element != NULL;
       current_super_type_list_element
       = IR_next_super_type_list_element (current_super_type_list_element))
    {
      immediate_super_type
        = IR_immediate_super_type (current_super_type_list_element);
      if (immediate_super_type != NULL
          && immediate_super_type != original_sub_type
          && auxiliary_function_it_is_super_type (super_type,
                                                  immediate_super_type,
                                                  original_sub_type))
      return TRUE;
    }
  return FALSE;
}

/* This function with the aid of `auxiliary_function_it_is_super_type'
   determines that one node type is a super type of another node type.
   The function returns TRUE iff the first node type is a super type
   of the second node type.  Parameters are to be node types. */

static int
it_is_super_type (IR_node_t super_type, IR_node_t sub_type)
{
  return auxiliary_function_it_is_super_type (super_type, sub_type, sub_type);
}

/* This function processes given field.  The processing consists of
           insertion of given field into the table of fields,
           forming cyclic list of fields with the same name with
              link by field `previous_synonym_field',
           fixing error for that fields with the same name have
              different types,
           fixing error for that fields with the same name have
              different double field attributes,
           fixing error for that fields with the same name are
              declared in different parts (class or non-class),
           generation of warning about the fields with the same name.
   This function is called from function
   `traverse_included_description_part_fields'.  As a consequence only
   fields which are in the table of node fields are processed only
   once. */

static void
test_and_connect_synonym_fields (IR_node_t field)
{
  IR_node_t first_synonym_field;

  /* Test with fields with the same name */
  first_synonym_field = insert_field (field);
  if (first_synonym_field == field)
    IR_set_previous_synonym_field (field, field);
  else
    {
      IR_set_previous_synonym_field
        (field, IR_previous_synonym_field (first_synonym_field));
      IR_set_previous_synonym_field (first_synonym_field, field);
      if (strcmp (IR_identifier_itself (IR_field_type_identifier
                                        (first_synonym_field)),
                  IR_identifier_itself (IR_field_type_identifier (field)))
          != 0)
        {
          error (FALSE, IR_position (field),
                 "field `%s' in node type `%s' has another type",
                 IR_identifier_itself (IR_field_identifier (field)),
                 IR_identifier_itself (IR_type_identifier
                                       (IR_node_type (first_synonym_field))));
          append_message (IR_position (first_synonym_field),
                          "here first declaration of the field");
        }
      else if (IR_double_field_flag (first_synonym_field)
               != IR_double_field_flag (field))
        {
          error (FALSE, IR_position (field),
                 "field `%s' in node type `%s' has another double attribute",
                 IR_identifier_itself (IR_field_identifier (field)),
                 IR_identifier_itself (IR_type_identifier
                                       (IR_node_type (first_synonym_field))));
          append_message (IR_position (first_synonym_field),
                          "here first declaration of the field");
        }
      else if ((IR_declaration_part (first_synonym_field) == DP_CLASS)
               != (IR_declaration_part (field) == DP_CLASS))
        {
          error
            (FALSE, IR_position (field),
             (IR_declaration_part (first_synonym_field) == DP_CLASS
              ? "field `%s' in node type `%s' is in another (class) part"
              : "field `%s' in node type `%s' is in another (non-class) part"),
             IR_identifier_itself (IR_field_identifier (field)),
             IR_identifier_itself (IR_type_identifier
                                   (IR_node_type (first_synonym_field))));
          append_message (IR_position (first_synonym_field),
                          "here first declaration of the field");
        }
      else if (v_flag
               && !it_is_super_type (IR_node_type (field),
                                     IR_node_type (first_synonym_field))
               && !it_is_super_type (IR_node_type (first_synonym_field),
                                     IR_node_type (field)))
        {
          warning (IR_position (field),
                   "warning -- there is also field `%s' in node type `%s'",
                   IR_identifier_itself (IR_field_identifier (field)),
                   IR_identifier_itself
                   (IR_type_identifier (IR_node_type
                                        (IR_previous_synonym_field (field)))));
          append_message (IR_position (IR_previous_synonym_field (field)),
                          "here previous declaration of the field");
        }
    }
}

/* This function processes given description part.  The processing
   consists of
           processing double node type declarations of
              the description part,
           processing type list of the description part,
           setting up basic double node flags of the description part,
           setting and testing super types of node types of the
              description part,
           testing cycle of super types of  node types of the
              description part,
           setting secondary super type flags for node types
           setting up double node flags for subtypes of the description
              part,
           insertion of fields of the description part into the table
              of node fields and setting up its type,
           testing presence of fields of the description part
              in super types,
           setting double field flags of fields of the description
              part by corresponding double node flags,
           setting double node flags of node types of the
              description part by corresponding double field flags,
           repeated setting up double node flags for subtypes of
              the description part,
           testing and connecting synonym fields of the description
              part.
   This function is called from function
   `traverse_description_part_hierarchy'.  It is very important that
   basic descriptions were already processed. */

static void
process_description_part (IR_node_t description_part)
{
  process_double_declaration_list (description_part);
  process_type_list (description_part);
  set_basic_double_node_flags (description_part);
  traverse_description_part_node_types (description_part,
                                        set_and_test_super_types);
  traverse_description_part_node_types (description_part,
                                        test_super_type_cycle);
  traverse_description_part_node_types (description_part,
                                        set_secondary_super_type_flags);
  traverse_description_part_node_types (description_part,
                                        set_double_node_flags_for_subtypes);
  traverse_description_part_node_types (description_part,
                                        insert_field_and_set_field_type);
  traverse_included_description_part_fields
    (description_part, test_field_presence_in_super_types);
  traverse_included_description_part_fields
    (description_part, set_double_field_flag_by_double_node_types);
  traverse_included_description_part_fields
    (description_part, set_double_node_flag_by_double_fields);
  traverse_description_part_node_types (description_part,
                                        set_double_node_flags_for_subtypes);
  traverse_included_description_part_fields (description_part,
                                             test_and_connect_synonym_fields);
}



/* This page contains functions for forming one level description,
   i.e.  description with the only description part.  This one level
   description contains only node type declarations which are in the
   table of types.  Remember that such node type declarations contains
   all fields of the description. */

/* This function appended all local and import/export codes of given
   description part to the end of corresponding list of description
   `current_description_part'.  After that the appended list is
   deleted from given description part.  Remember that lists are
   cyclic and represented by last element. */

static void
form_one_level_code_insertion_list (IR_node_t description_part)
{
  IR_node_t current_code_insertion;
  IR_node_t next_code_insertion;

  if (IR_last_code_insertion (description_part) == NULL)
    return;
  for (current_code_insertion = IR_next_code_insertion (IR_last_code_insertion
                                                        (description_part));;
       current_code_insertion = next_code_insertion)
    {
      next_code_insertion = IR_next_code_insertion (current_code_insertion);
      /* Insert in new list */
      if (IR_last_code_insertion (current_description_part) == NULL)
        IR_set_next_code_insertion (current_code_insertion,
                                    current_code_insertion);
      else
        {
          IR_set_next_code_insertion
            (current_code_insertion,
             IR_next_code_insertion (IR_last_code_insertion
                                     (current_description_part)));
          IR_set_next_code_insertion
            (IR_last_code_insertion (current_description_part),
             current_code_insertion);
        }
      IR_set_last_code_insertion (current_description_part,
                                  current_code_insertion);
      if (current_code_insertion == IR_last_code_insertion (description_part))
        break;
    }
  /* Delete all list of given description part */
  IR_set_last_code_insertion (description_part, NULL);
}

/* This function appends all node type and predefined type
   declarations of given description part which are in the table of
   types to the end of corresponding list of description
   `current_description_part'.  The appended elements are deleted from
   given description part.  Remember that all declarations of a node
   type are represented by nodes `node_type'.  Remember that lists are
   cyclic and represented by last element. */

static void
form_one_level_type_list (IR_node_t description_part)
{
  IR_node_t current_type;
  IR_node_t next_type;
  IR_node_t previous_type;
  IR_node_t last_type;

  if (IR_last_type (description_part) == NULL)
    return;
  last_type = previous_type = IR_last_type (description_part);
  current_type = IR_next_type (last_type);
  for (;;)
    {
      next_type = IR_next_type (current_type);
      if (current_type == find_type (IR_type_identifier (current_type)))
        {
          /* Delete from list of given description part */
          if (current_type == next_type)
            IR_set_last_type (description_part, NULL);
          else
            IR_set_next_type (previous_type, next_type);
          /* Insert in new list */
          if (IR_last_type (current_description_part) == NULL)
            IR_set_next_type (current_type, current_type);
          else
            {
              IR_set_next_type (current_type,
                                IR_next_type (IR_last_type
                                              (current_description_part)));
              IR_set_next_type (IR_last_type (current_description_part),
                                current_type);
            }
          IR_set_last_type (current_description_part, current_type);
        }
      else
        previous_type = current_type;
      if (current_type == last_type)
        break;
      current_type = next_type;
    }
}

/* This function appended all additional codes (there is the only
   additional code in given description part) of given description
   part to the end of corresponding list of description
   `current_description_part'.  After that the appended list is
   deleted from given description part.  Remember that lists are
   cyclic and represented by last element. */

static void
form_one_level_additional_code_list (IR_node_t description_part)
{
  IR_node_t current_additional_code;
  IR_node_t next_additional_code;

  if (IR_last_additional_code (description_part) == NULL)
    return;
  for (current_additional_code
       = IR_next_additional_code (IR_last_additional_code (description_part));;
       current_additional_code = next_additional_code)
    {
      next_additional_code = IR_next_additional_code (current_additional_code);
      /* Insert in new list */
      if (IR_last_additional_code (current_description_part) == NULL)
        IR_set_next_additional_code (current_additional_code,
                                     current_additional_code);
      else
        {
          IR_set_next_additional_code
            (current_additional_code,
             IR_next_additional_code (IR_last_additional_code
                                      (current_description_part)));
          IR_set_next_additional_code
            (IR_last_additional_code (current_description_part),
             current_additional_code);
        }
      IR_set_last_additional_code (current_description_part,
                                   current_additional_code);
      if (current_additional_code
          == IR_last_additional_code (description_part))
        break;
    }
  /* Delete all list of given description part */
  IR_set_last_additional_code (description_part, NULL);
}

/* This function changes all description parts by one level
   description, i.e. description with the only description part.  To
   make this the function calls functions for forming
         list of import/export and local codes of description
             parts,
         list of node types and predefined types of description parts
         which are in the table of types,
         list of additional codes of description parts.
   The elements from the nested description part will be placed in
   front of the resulted lists. */

static void
make_one_level_description (void)
{
  IR_node_t old_current_description_part;

  old_current_description_part = current_description_part;
  current_description_part = IR_copy_node (current_description_part);
  IR_set_first_basic_description_part (current_description_part, NULL);
  IR_set_next_basic_description_part (current_description_part, NULL);
  IR_set_last_code_insertion (current_description_part, NULL);
  IR_set_last_double_declaration (current_description_part, NULL);
  IR_set_last_type (current_description_part, NULL);
  IR_set_last_additional_code (current_description_part, NULL);
  traverse_description_part_hierarchy (old_current_description_part,
                                       form_one_level_code_insertion_list);
  traverse_description_part_hierarchy (old_current_description_part,
                                       form_one_level_type_list);
  traverse_description_part_hierarchy (old_current_description_part,
                                       form_one_level_additional_code_list);
}



/* This page contains code for checking multiple inheritence. */

static void
process_super_types_on_multiple_inheritence (IR_node_t node_type,
                                             IR_node_t original_node_type)
{
  IR_node_t current_super_type_list_element;
  IR_node_t immediate_super_type;

  if (node_type == root_node_type)
    return;
  else if (IR_who_visisted_on_multiple_inheritence_checking (node_type)
      == original_node_type)
    {
      error (FALSE, IR_position (original_node_type),
             "node type `%s' repeatedly inherits node type `%s'",
             IR_identifier_itself (IR_type_identifier (original_node_type)),
             IR_identifier_itself (IR_type_identifier (node_type)));
      return;
    }
  IR_set_who_visisted_on_multiple_inheritence_checking (node_type,
                                                        original_node_type);
  for (current_super_type_list_element
       = IR_first_super_type_list_element (node_type);
       current_super_type_list_element != NULL;
       current_super_type_list_element
       = IR_next_super_type_list_element (current_super_type_list_element))
    {
      immediate_super_type
        = IR_immediate_super_type (current_super_type_list_element);
      if (immediate_super_type != NULL)
        process_super_types_on_multiple_inheritence (immediate_super_type,
                                                     original_node_type);
    }
}

static void
node_type_multiple_inheritence_check (IR_node_t node_type)
{
  process_super_types_on_multiple_inheritence (node_type, node_type);
}



/* This page contains major, external function of the semantic analyzer. */

/* This function initiates variable `number_of_node_types', calls
   functions for semantic analysis of all description parts (in depth
   first order), increase the variable for node type `%root' and error
   nodes, sets up field `node_type_number' for node type `%root' and
   calls function for forming one level description. */

void
analyze_program (void)
{
  number_of_node_types = 0;
  traverse_description_part_hierarchy (current_description_part,
                                       process_description_part);
  IR_set_node_type_number (root_node_type, number_of_node_types);
  number_of_node_types++; /* It is for `%root' node. */
  number_of_node_types++; /* It is for `%error' node. */
  make_one_level_description ();
  if (number_of_errors == 0)
    traverse_description_part_node_types
      (current_description_part, node_type_multiple_inheritence_check);
}
