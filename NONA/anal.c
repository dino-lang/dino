/*
   FILE NAME:   anal.c

   TITLE:       Semantic analyzer of NONA (code slector description translator)

   DESCRIPTION: This file tests semantically all description built by
                the NONA parser.

   SPECIAL CONSIDERATION:
         The analyzer is to be called only after NONA parser.
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
#include "vlobject.h"
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


/* ??? */
static void
check_union_declarations (void)
{
  IR_node_t current_declaration;
  IR_node_t first_union_code;

  first_union_code = NULL;
  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_union_code))
      {
        if (first_union_code == NULL)
          first_union_code = current_declaration;
        else
          {
            warning
              (IR_position (current_declaration),
               "warning: repeated construction `%%union' -- ignore it");
            append_message (IR_position (first_union_code),
                            "here the first occurrence of `%%union'");
          }
      }
}
/* ??? */
static void
add_single_declaration (IR_node_t single_declaration)
{
  if (IR_single_declaration_list (description) != NULL)
    {
      /* Add element to cyclic declaration list. */
      IR_set_next_single_declaration
        (single_declaration,
         IR_next_single_declaration (IR_single_declaration_list
                                     (description)));
      IR_set_next_single_declaration
        (IR_single_declaration_list (description), single_declaration);
    }
  else
    /* Make cycle. */
    IR_set_next_single_declaration (single_declaration, single_declaration);
  IR_set_single_declaration_list (description, single_declaration);
}
/* ??? */
static void
process_terminal_declarations (void)
{
  IR_node_t current_declaration;
  IR_node_t current_identifier;
  IR_node_t single_declaration;
  IR_node_t declaration_in_table;

  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_term_declaration)
        || IR_IS_OF_TYPE (current_declaration, IR_NM_commutative_declaration))
      {
        for (current_identifier = IR_identifier_list (current_declaration);
             current_identifier != NULL;
             current_identifier = IR_next_identifier (current_identifier))
          {
            declaration_in_table
              = find_single_declaration (current_identifier);
            if (declaration_in_table == NULL)
              {
                single_declaration
                  = IR_new_single_term_declaration
                    (IR_position (current_identifier),
                     current_identifier, NULL, NULL,
                     IR_IS_OF_TYPE (current_declaration,
                                    IR_NM_commutative_declaration),
                     no_position, NULL, -1);
                declaration_in_table
                  = insert_single_declaration (single_declaration);
                assert (declaration_in_table == single_declaration);
                add_single_declaration (single_declaration);
              }
            else
              {
                assert (IR_IS_OF_TYPE (declaration_in_table,
                                       IR_NM_single_term_declaration));
                if (IR_IS_OF_TYPE (current_declaration,
                                   IR_NM_commutative_declaration))
                  IR_set_commutative_flag (declaration_in_table, TRUE);
              }
          }
      }
}
/* ??? */
static int
numerate_pattern (IR_node_t pattern, int number)
{
  IR_node_t current_pattern;

  IR_set_commutative_process_flag (pattern, FALSE);
  IR_set_original_number (pattern, number);
  for (current_pattern = IR_pattern_list (pattern);
       current_pattern != NULL;
       current_pattern = IR_next_pattern (current_pattern))
    number = numerate_pattern (current_pattern, number + 1);
  return number;
}
/* ??? */
static void
numerate_rule_patterns (void)
{
  IR_node_t current_rule;

  for (current_rule = IR_rule_list (description);
       current_rule != NULL;
       current_rule = IR_next_rule (current_rule))
    numerate_pattern (IR_pattern (current_rule), 1);
}
/* ??? */
static IR_node_t
find_unprocessed_commutative_operator_pattern (IR_node_t pattern)
{
  IR_node_t current_pattern;
  IR_node_t unprocessed_commutative_operator_pattern;
  IR_node_t declaration_in_table;

  declaration_in_table = find_single_declaration (IR_identifier (pattern));
  if (declaration_in_table != NULL
      && IR_IS_OF_TYPE (declaration_in_table, IR_NM_single_term_declaration)
      && IR_commutative_flag (declaration_in_table)
      && !IR_commutative_process_flag (pattern)
      && IR_pattern_list (pattern) != NULL)
    return pattern;
  for (current_pattern = IR_pattern_list (pattern);
       current_pattern != NULL;
       current_pattern = IR_next_pattern (current_pattern))
    {
      unprocessed_commutative_operator_pattern =
        find_unprocessed_commutative_operator_pattern (current_pattern);
      if (unprocessed_commutative_operator_pattern != NULL)
        return unprocessed_commutative_operator_pattern;
    }
  return NULL;
}
/* ??? */
static IR_node_t
copy_pattern (IR_node_t pattern, IR_node_t operator_pattern,
              const IR_node_t *operands)
{
  IR_node_t current_pattern;
  IR_node_t new_current_pattern;
  IR_node_t previous_new_pattern;
  IR_node_t result_pattern;

  result_pattern = IR_copy_node (pattern);
  for (previous_new_pattern = NULL,
       current_pattern = IR_pattern_list (pattern);
       current_pattern != NULL;
       previous_new_pattern = new_current_pattern,
       current_pattern = IR_next_pattern (current_pattern))
    {
      if (pattern == operator_pattern)
        {
          new_current_pattern = copy_pattern (*operands, NULL, NULL);
          operands++;
        }
      else
        new_current_pattern
          = copy_pattern (current_pattern, operator_pattern, operands);
      if (previous_new_pattern == NULL)
        IR_set_pattern_list (result_pattern, new_current_pattern);
      else
        IR_set_next_pattern (previous_new_pattern, new_current_pattern);
    }
  if (previous_new_pattern != NULL)
    IR_set_next_pattern (previous_new_pattern, NULL);
  return result_pattern;
}
/* ??? */
static int
pattern_equality (IR_node_t pattern_1, IR_node_t pattern_2)
{
  IR_node_t current_pattern_1;
  IR_node_t current_pattern_2;

  if (strcmp (IR_identifier_itself (IR_identifier (pattern_1)),
              IR_identifier_itself (IR_identifier (pattern_2))) != 0)
    return FALSE;
  for (current_pattern_1 = IR_pattern_list (pattern_1),
       current_pattern_2 = IR_pattern_list (pattern_2);
       current_pattern_1 != NULL && current_pattern_2 != NULL;
       current_pattern_1 = IR_next_pattern (current_pattern_1),
       current_pattern_2 = IR_next_pattern (current_pattern_2))
    if (!pattern_equality (current_pattern_1, current_pattern_2))
      return FALSE;
  return current_pattern_1 == current_pattern_2;
}
/* ??? */
static void
make_pattern_permutation (IR_node_t rule, IR_node_t operator_pattern,
                          IR_node_t *operands, int operands_number,
                          int new_permutation_flag)
{
  IR_node_t new_rule;
  int current_operand_number;
  IR_node_t temporary_operand_variable;
  int current_last_operand_number;
  vlo_t last_operands;

  if (operands_number == 1)
    {
      if (new_permutation_flag)
        {
          /* Create new rule after given rule. */
          new_rule = IR_copy_node (rule);
          IR_set_new_commutative_rule_flag (new_rule, TRUE);
          IR_set_next_rule (new_rule, IR_next_rule (rule));
          IR_set_next_rule (rule, new_rule);
          IR_set_pattern (new_rule,
                          copy_pattern (IR_pattern (rule),
                                        operator_pattern, operands));
        }
    }
  else
    {
      make_pattern_permutation (rule, operator_pattern, operands,
                                operands_number - 1, new_permutation_flag);
      VLO_CREATE (last_operands, 10);
      VLO_ADD_MEMORY (last_operands, &operands [operands_number - 1],
                      sizeof (IR_node_t));
      for (current_operand_number = 0;
           current_operand_number < operands_number - 1;
           current_operand_number++)
        {
          for (current_last_operand_number = 0;
               current_last_operand_number
               < VLO_LENGTH (last_operands) / sizeof (IR_node_t);
               current_last_operand_number++)
            if (pattern_equality
                (((IR_node_t *) VLO_BEGIN (last_operands))
                 [current_last_operand_number],
                 operands [current_operand_number]))
              break;
          if (current_last_operand_number
              >= VLO_LENGTH (last_operands) / sizeof (IR_node_t))
            {
              /* Let last operand be new operand. */
              VLO_ADD_MEMORY (last_operands,
                              &operands [current_operand_number],
                              sizeof (IR_node_t));
              temporary_operand_variable = operands [current_operand_number];
              operands [current_operand_number]
                = operands [operands_number - 1];
              operands [operands_number - 1] = temporary_operand_variable;
              make_pattern_permutation
                (rule, operator_pattern, operands, operands_number - 1, TRUE);
              temporary_operand_variable = operands [current_operand_number];
              operands [current_operand_number]
                = operands [operands_number - 1];
              operands [operands_number - 1] = temporary_operand_variable;
            }
        }
      VLO_DELETE (last_operands);
    }
}
/* ??? */
static int
process_comutative_pattern_rule (IR_node_t rule)
{
  IR_node_t commutative_operator_pattern;
  IR_node_t current_pattern;
  vlo_t operands;

  commutative_operator_pattern
    = find_unprocessed_commutative_operator_pattern (IR_pattern (rule));
  if (commutative_operator_pattern == NULL
      || IR_pattern_list (commutative_operator_pattern) == NULL)
    return FALSE;
  IR_set_commutative_process_flag (commutative_operator_pattern, TRUE);
  VLO_CREATE (operands, 30);
  for (current_pattern = IR_pattern_list (commutative_operator_pattern);
       current_pattern != NULL;
       current_pattern = IR_next_pattern (current_pattern))
    VLO_ADD_MEMORY (operands, &current_pattern, sizeof (IR_node_t));
  make_pattern_permutation (rule, commutative_operator_pattern,
                            VLO_BEGIN (operands),
                            VLO_LENGTH (operands) / sizeof (IR_node_t),
                            FALSE);
  VLO_DELETE (operands);
  return TRUE;
}
/* ??? */
static void
make_new_commutative_pattern_rules (void)
{
  IR_node_t current_rule;

  for (current_rule = IR_rule_list (description);
       current_rule != NULL;
       current_rule = IR_next_rule (current_rule))
    IR_set_new_commutative_rule_flag (current_rule, FALSE);
  current_rule = IR_rule_list (description);
  while (current_rule != NULL)
    {
      if (!process_comutative_pattern_rule (current_rule))
        current_rule = IR_next_rule (current_rule);
    }
}
/* ??? */
static void
add_nonterm_rule (IR_node_t single_nonterm_declaration, IR_node_t rule)
{
  if (IR_nonterm_rule_list (single_nonterm_declaration) != NULL)
    {
      /* Add element to cyclic declaration list. */
      IR_set_next_nonterm_rule
        (rule, IR_next_nonterm_rule (IR_nonterm_rule_list
                                     (single_nonterm_declaration)));
      IR_set_next_nonterm_rule
        (IR_nonterm_rule_list (single_nonterm_declaration), rule);
    }
  else
    /* Make cycle. */
    IR_set_next_nonterm_rule (rule, rule);
  IR_set_nonterm_rule_list (single_nonterm_declaration, rule);
  IR_set_number_of_nonterminal_rules
    (single_nonterm_declaration,
     IR_number_of_nonterminal_rules (single_nonterm_declaration) + 1);
  IR_set_nonterm_rule_number
    (rule, IR_number_of_nonterminal_rules (single_nonterm_declaration));
}
/* ??? */
static void
process_nonterminal_declarations (void)
{
  IR_node_t current_rule;
  IR_node_t single_declaration;
  IR_node_t declaration_in_table;

  for (current_rule = IR_rule_list (description);
       current_rule != NULL; current_rule = IR_next_rule (current_rule))
    {
      declaration_in_table
        = find_single_declaration (IR_nonterm_identifier (current_rule));
      if (declaration_in_table == NULL)
        {
          single_declaration
            = IR_new_single_nonterm_declaration
              (IR_position (current_rule),
               IR_nonterm_identifier (current_rule),
               NULL, NULL, 0, NULL, NULL, FALSE, 0);
          IR_set_number_of_nonterminals
            (description, IR_number_of_nonterminals (description) + 1);
          declaration_in_table
            = insert_single_declaration (single_declaration);
          assert (declaration_in_table == single_declaration);
          add_single_declaration (single_declaration);
          add_nonterm_rule (single_declaration, current_rule);
          IR_set_single_nonterm_declaration (current_rule, single_declaration);
        }
      else if (IR_IS_OF_TYPE (declaration_in_table,
                              IR_NM_single_term_declaration))
        {
          if (!IR_new_commutative_rule_flag (current_rule))
            {
              /* No repeated messages for new commutative rules. */
              error (FALSE, IR_position (current_rule),
                     "terminal `%s' in the left hand side of the rule",
                     IR_identifier_itself (IR_nonterm_identifier
                                           (current_rule)));
              append_message (IR_position (declaration_in_table),
                              "here the first declaration");
            }
          IR_set_single_nonterm_declaration (current_rule, NULL);
        }
      else
        {
          assert (IR_IS_OF_TYPE (declaration_in_table,
                                 IR_NM_single_nonterm_declaration));
          add_nonterm_rule (declaration_in_table, current_rule);
          IR_set_single_nonterm_declaration (current_rule,
                                             declaration_in_table);
        }
    }
}
/* ??? */
static void
set_symbol_types (void)
{
  IR_node_t current_declaration;
  IR_node_t current_identifier;
  IR_node_t declaration_in_table;

  for (current_declaration = IR_declaration_list (description);
       current_declaration != NULL;
       current_declaration = IR_next_declaration (current_declaration))
    if (IR_IS_OF_TYPE (current_declaration, IR_NM_term_declaration)
        || IR_IS_OF_TYPE (current_declaration, IR_NM_commutative_declaration)
        || IR_IS_OF_TYPE (current_declaration, IR_NM_type_declaration))
      {
        for (current_identifier = IR_identifier_list (current_declaration);
             current_identifier != NULL;
             current_identifier = IR_next_identifier (current_identifier))
          {
            declaration_in_table
              = find_single_declaration (current_identifier);
            if (declaration_in_table == NULL)
              error (FALSE, IR_position (current_identifier),
                     "nonterminal `%s' is absent in the lhs of a rule",
                     IR_identifier_itself (current_identifier));
            else
              {
                assert (IR_IS_OF_TYPE (declaration_in_table,
                                       IR_NM_single_declaration));
                if (IR_type (declaration_in_table) != NULL
                    && IR_type (current_declaration) != NULL
                    && strcmp (IR_identifier_itself
                               (IR_type (declaration_in_table)),
                               IR_identifier_itself
                               (IR_type (current_declaration))) != 0)
                  {
                    error (FALSE, IR_position (current_identifier),
                           "redefinition of type of `%s'",
                           IR_identifier_itself (current_identifier));
                    append_message (IR_position (declaration_in_table),
                                    "here the first definition of type");
                  }
                else if (IR_type (declaration_in_table) == NULL)
                  IR_set_type (declaration_in_table,
                               IR_type (current_declaration));
              }
          }
      }
}

/* ??? */
static void
add_textual_pattern (IR_node_t rule, IR_node_t pattern)
{
  if (IR_textual_pattern_list (rule) != NULL)
    {
      /* Add element to cyclic pattern list. */
      IR_set_next_textual_pattern
        (pattern, IR_next_textual_pattern (IR_textual_pattern_list (rule)));
      IR_set_next_textual_pattern (IR_textual_pattern_list (rule), pattern);
    }
  else
    /* Make cycle. */
    IR_set_next_textual_pattern (pattern, pattern);
  IR_set_textual_pattern_list (rule, pattern);
}
/* ??? */
static int
pattern_list_length (IR_node_t pattern_list)
{
  IR_node_t current_pattern;
  int length;

  for (length = 0, current_pattern = pattern_list;
       current_pattern != NULL;
       current_pattern = IR_next_pattern (current_pattern))
    length++;
  return length;
}
/* ??? */
static int
process_pattern (IR_node_t pattern, IR_node_t rule, IR_node_t father,
                 int last_nonterminal_rule_pattern)
{
  IR_node_t current_pattern;
  IR_node_t declaration_in_table;

  IR_set_father (pattern, father);
  declaration_in_table = find_single_declaration (IR_identifier (pattern));
  if (declaration_in_table == NULL)
    {
      if (!IR_new_commutative_rule_flag (rule))
        /* No repeated messages for new commutative rules. */
        error (FALSE, IR_position (IR_identifier (pattern)),
               "nonterminal `%s' is absent in the lhs of a rule",
               IR_identifier_itself (IR_identifier (pattern)));
    }
  else if (IR_pattern_list (pattern) != NULL
           && !IR_IS_OF_TYPE (declaration_in_table,
                              IR_NM_single_term_declaration))
    {
      if (!IR_new_commutative_rule_flag (rule))
        /* No repeated messages for new commutative rules. */
        error (FALSE, IR_position (IR_identifier (pattern)),
               "`%s' before `(' is not terminal",
               IR_identifier_itself (IR_identifier (pattern)));
    }
  else if (IR_IS_OF_TYPE (declaration_in_table, IR_NM_single_term_declaration))
    {
      if (compare_positions (no_position,
                             IR_first_position_in_rules (declaration_in_table))
          == 0)
        {
          IR_set_first_position_in_rules (declaration_in_table,
                                          IR_position (pattern));
          assert (IR_arity (declaration_in_table) == -1);
          IR_set_arity (declaration_in_table,
                        pattern_list_length (IR_pattern_list (pattern)));
        }
      else if (IR_arity (declaration_in_table)
               != pattern_list_length (IR_pattern_list (pattern)))
        {
          if (!IR_new_commutative_rule_flag (rule))
            /* No repeated messages for new commutative rules. */
            {
              error (FALSE, IR_position (pattern),
                     "terminal `%s' has different arity",
                     IR_identifier_itself (IR_identifier (pattern)));
              append_message
                (IR_first_position_in_rules (declaration_in_table),
                 "here the first terminal occurrence");
            }
        }
    }
  else if (IR_IS_OF_TYPE (declaration_in_table,
                          IR_NM_single_nonterm_declaration))
    {
      last_nonterminal_rule_pattern++;
      IR_set_textual_nonterminal_pattern_number
        (pattern, last_nonterminal_rule_pattern);
      if (IR_max_number_of_rule_nonterminals (description)
          < last_nonterminal_rule_pattern)
        {
          IR_set_max_number_of_rule_nonterminals
            (description, last_nonterminal_rule_pattern);
        }
    }
  add_textual_pattern (rule, pattern);
  IR_set_single_declaration (pattern, declaration_in_table);
  for (current_pattern = IR_pattern_list (pattern);
       current_pattern != NULL;
       current_pattern = IR_next_pattern (current_pattern))
    last_nonterminal_rule_pattern
      = process_pattern (current_pattern, rule, pattern,
                         last_nonterminal_rule_pattern);
  return last_nonterminal_rule_pattern;
}
/* ??? */
static void
add_chain_rule (IR_node_t single_nonterm_declaration, IR_node_t rule)
{
  if (IR_chain_rule_list (single_nonterm_declaration) != NULL)
    {
      /* Add element to cyclic declaration list. */
      IR_set_next_chain_rule
        (rule, IR_next_chain_rule (IR_chain_rule_list
                                   (single_nonterm_declaration)));
      IR_set_next_chain_rule (IR_chain_rule_list (single_nonterm_declaration),
                              rule);
    }
  else
    /* Make cycle. */
    IR_set_next_chain_rule (rule, rule);
  IR_set_chain_rule_list (single_nonterm_declaration, rule);
}
/* ??? */
static void
add_term_rule (IR_node_t single_term_declaration, IR_node_t rule)
{
  if (IR_term_rule_list (single_term_declaration) != NULL)
    {
      /* Add element to cyclic declaration list. */
      IR_set_next_term_rule
        (rule, IR_next_term_rule (IR_term_rule_list
                                  (single_term_declaration)));
      IR_set_next_term_rule (IR_term_rule_list (single_term_declaration),
                             rule);
    }
  else
    /* Make cycle. */
    IR_set_next_term_rule (rule, rule);
  IR_set_term_rule_list (single_term_declaration, rule);
}

/* position corresponds to *code at the begining, and *result at the end. */
static const char *
next_attribute (const char *code, position_t *position)
{
  assert (code != NULL);
  /* !! This code is bound to scanner */
  for (; *code != '\0'; code++)
    switch (*code)
      {
      case '\t':
        position->column_number
          = ((position->column_number - 1) / TAB_STOP + 1)
            * TAB_STOP + 1;
        break;
      case '\n':
        position->column_number = 1;
        position->line_number++;
        break;
      case '$':
        if (code [1] == '$' || isdigit (code [1]))
          return code;
        else
          position->column_number++;
        break;
      case '/':
        position->column_number++;
        if (code [1] == '*')
          {
            /* C comment. */
            code++;
            position->column_number++;
            for (;;)
              {
                code++;
                if (*code == '*')
                  {
                    code++;
                    position->column_number++;
                    if (*code == '/')
                      {
                        position->column_number++;
                        break;
                      }
                    else
                      {
                        code--;
                        continue;
                      }
                  }
                else if (*code == '\0')
                  {
                    code--;
                    break;
                  }
                if (*code == '\n')
                  {
                    position->column_number = 1;
                    position->line_number++;
                  }
                else if (*code == '\t')
                  position->column_number
                    = ((position->column_number - 1) / TAB_STOP + 1)
                      * TAB_STOP + 1;
                else
                  position->column_number++;
              }
          }
        break;
      case '\'':
        position->column_number++;
        code++;
        if (*code == '\0' || *code == '\n')
          code--;
        else if (*code != '\'')
          {
            if (*code == '\\')
              {
                position->column_number++;
                code++;
                if (*code == 'n' || *code == 't' || *code == 'v'
                    || *code == 'b' || *code == 'r' || *code == 'f'
                    || *code == '\\' || *code == '\'' || *code == '\"')
                  position->column_number++;
                else if (*code == '\n')
                  {
                    position->column_number = 1;
                    position->line_number++;
                  }
                else if (isdigit (*code) && *code != '8' && *code != '9')
                  {
                    position->column_number++;
                    if (isdigit (code [1]) && code [1] != '8'
                        && code [1] != '9')
                      {
                        code++;
                        position->column_number++;
                        if (isdigit (code [1]) && code [1] != '8'
                            && code [1] != '9')
                          {
                            position->column_number++;
                            code++;
                          }
                      }
                  }
                else
                  code--;
              }
            else if (*code == '\t')
              position->column_number
                = ((position->column_number - 1) / TAB_STOP + 1)
                  * TAB_STOP + 1;
            else
              position->column_number++;
          }
        else
          position->column_number++;
        if (code [1] == '\'')
          {
            position->column_number++;
            code++;
          }
        break;
      case '\"':
        position->column_number++;
        for (;;)
          {
            code++;
            if (*code == '\"')
              {
                position->column_number++;
                break;
              }
            else if (*code == '\0' || *code == '\n')
              {
                code--;
                break;
              }
            else
              {
                if (*code == '\\')
                  {
                    position->column_number++;
                    code++;
                    if (*code == 'n' || *code == 't' || *code == 'v'
                        || *code == 'b' || *code == 'r' || *code == 'f'
                        || *code == '\\' || *code == '\'' || *code == '\"')
                      position->column_number++;
                    else if (*code == '\n')
                      {
                        position->column_number = 1;
                        position->line_number++;
                      }
                    else if (isdigit (*code) && *code != '8' && *code != '9')
                      {
                        position->column_number++;
                        if (isdigit (code [1]) && code [1] != '8'
                            && code [1] != '9')
                          {
                            code++;
                            position->column_number++;
                            if (isdigit (code [1]) && code [1] != '8'
                                && code [1] != '9')
                              {
                                position->column_number++;
                                code++;
                              }
                          }
                      }
                    else
                      code--;
                  }
                else if (*code == '\t')
                  position->column_number
                    = ((position->column_number - 1) / TAB_STOP + 1)
                      * TAB_STOP + 1;
                else
                  position->column_number++;
              }
          }
        break;
      default:
        position->column_number++;
        break;
      }
  return NULL;
}
/* ??? */
static IR_node_t
find_pattern (IR_node_t textual_pattern_list, int number)
{
  IR_node_t current_textual_pattern;
  IR_node_t last_textual_pattern;

  last_textual_pattern = textual_pattern_list;
  assert (last_textual_pattern != NULL);
  current_textual_pattern = last_textual_pattern;
  do
    {
      current_textual_pattern
        = IR_next_textual_pattern (current_textual_pattern);
      number--;
      if (number == 0)
        return current_textual_pattern;
    }
  while (current_textual_pattern != last_textual_pattern);
  return NULL;
}
/* ??? */
static void
check_cost_or_constraint (IR_node_t expression, IR_node_t textual_pattern_list)
{
  position_t position;
  const char *code;
  const char *dollar;
  IR_node_t pattern;
  int number;

  position = IR_position (expression);
  position.column_number++; /* Skip `['. */
  code = IR_expression_itself (expression);
  for (;;)
    {
      dollar = next_attribute (code, &position);
      if (dollar == NULL)
        break;
      assert (dollar [1] == '$' || isdigit (dollar [1]));
      if (dollar [1] != '$')
        {
          number = atoi (dollar + 1);
          pattern = find_pattern (textual_pattern_list, number);
          if (pattern == NULL)
            error (FALSE, position, "too very big number in symbol `$%d'",
                   number);
          else if (IR_single_declaration (pattern) != NULL
                   && !IR_IS_OF_TYPE (IR_single_declaration (pattern),
                                      IR_NM_single_term_declaration))
            error (FALSE, position, "`$%d' corresponds to nonterminal",
                   number);
        }
      else
        error (FALSE, position, "`$$' is used in expression");
      code = dollar + 2;
      position.column_number += 2;
    }
}
/* ??? */
static void
check_action (IR_node_t code_insertion, IR_node_t textual_pattern_list)
{
  position_t position;
  const char *code;
  const char *dollar;
  IR_node_t pattern;
  int number;

  position = IR_position (code_insertion);
  position.column_number++; /* Skip `{'. */
  code = IR_code_insertion_itself (code_insertion);
  for (;;)
    {
      dollar = next_attribute (code, &position);
      if (dollar == NULL)
        break;
      assert (dollar [1] == '$' || isdigit (dollar [1]));
      if (dollar [1] != '$')
        {
          number = atoi (dollar + 1);
          pattern = find_pattern (textual_pattern_list, number);
          if (pattern == NULL)
            error (FALSE, position, "too very big number in symbol `$%d'",
                   number);
        }
      code = dollar + 2;
      position.column_number += 2;
    }
}
/* ??? */
static void
process_rules (void)
{
  IR_node_t current_rule;
  IR_node_t pattern;
  int current_rule_number;

  for (current_rule_number = 1, current_rule = IR_rule_list (description);
       current_rule != NULL;
       current_rule_number++, current_rule = IR_next_rule (current_rule))
    {
      IR_set_rule_number (current_rule, current_rule_number);
      pattern = IR_pattern (current_rule);
      IR_set_textual_pattern_list (current_rule, NULL);
      process_pattern (pattern, current_rule, NULL, 0);
      if (IR_pattern_list (pattern) == NULL
          && IR_single_declaration (pattern) != NULL
          && IR_IS_OF_TYPE (IR_single_declaration (pattern),
                            IR_NM_single_nonterm_declaration))
        add_chain_rule (IR_single_declaration (pattern), current_rule);
      else if (IR_single_declaration (pattern) != NULL
               && IR_IS_OF_TYPE (IR_single_declaration (pattern),
                                 IR_NM_single_term_declaration))
        add_term_rule (IR_single_declaration (pattern), current_rule);
      if (!IR_new_commutative_rule_flag (current_rule))
        {
          /* No repeated messages for new commutative rules. */
          if (IR_optional_cost (current_rule) != NULL)
            check_cost_or_constraint (IR_optional_cost (current_rule),
                                      IR_textual_pattern_list (current_rule));
          if (IR_optional_constraint (current_rule) != NULL)
            check_cost_or_constraint (IR_optional_constraint (current_rule),
                                      IR_textual_pattern_list (current_rule));
          if (IR_optional_action (current_rule) != NULL)
            check_action (IR_optional_action (current_rule),
                          IR_textual_pattern_list (current_rule));
        }
    }
}
/* ??? */
static void
check_terminal_usage (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;

  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration == NULL)
    return;
  current_single_declaration = last_single_declaration;
  do
    {
      current_single_declaration
        = IR_next_single_declaration (current_single_declaration);
      if (IR_IS_OF_TYPE (current_single_declaration,
                         IR_NM_single_term_declaration)
          && compare_positions (no_position,
                                IR_first_position_in_rules
                                (current_single_declaration)) == 0)
        warning (IR_position (current_single_declaration),
                 "warning: terminal `%s' is not used",
                 IR_identifier_itself (IR_identifier
                                       (current_single_declaration)));
    }
  while (current_single_declaration != last_single_declaration);
}
/* ??? */
static void
set_nonterm_derivability (IR_node_t single_nonterm_declaration,
                          int traverse_number)
{
  IR_node_t current_nonterm_rule;
  IR_node_t last_nonterm_rule;
  IR_node_t current_textual_pattern;
  IR_node_t last_textual_pattern;
  int derivability_flag;

  if (IR_traverse_number (single_nonterm_declaration) == traverse_number
      || IR_derivability_flag (single_nonterm_declaration))
    return;
  IR_set_traverse_number (single_nonterm_declaration, traverse_number);
  last_nonterm_rule = IR_nonterm_rule_list (single_nonterm_declaration);
  assert (last_nonterm_rule != NULL);
  current_nonterm_rule = last_nonterm_rule;
  do
    {
      current_nonterm_rule = IR_next_nonterm_rule (current_nonterm_rule);
      last_textual_pattern = IR_textual_pattern_list (current_nonterm_rule);
      assert (last_textual_pattern != NULL);
      current_textual_pattern = last_textual_pattern;
      derivability_flag = TRUE;
      do
        {
          current_textual_pattern
            = IR_next_textual_pattern (current_textual_pattern);
          if (IR_single_declaration (current_textual_pattern) != NULL
              && IR_IS_OF_TYPE (IR_single_declaration
                                (current_textual_pattern),
                                IR_NM_single_nonterm_declaration))
            {
              set_nonterm_derivability
                (IR_single_declaration (current_textual_pattern),
                 traverse_number);
              derivability_flag
                = derivability_flag
                  && IR_derivability_flag (IR_single_declaration
                                           (current_textual_pattern));
            }
        }
      while (current_textual_pattern != last_textual_pattern);
      if (derivability_flag)
        IR_set_derivability_flag (single_nonterm_declaration, TRUE);
    }
  while (current_nonterm_rule != last_nonterm_rule);
  return;
}
/* ??? */
static int
accessible (IR_node_t tested_nonterm_declaration,
            IR_node_t from_nonterm_declaration, int traverse_number)
{
  IR_node_t current_nonterm_rule;
  IR_node_t last_nonterm_rule;
  IR_node_t current_textual_pattern;
  IR_node_t last_textual_pattern;

  if (IR_traverse_number (from_nonterm_declaration) == traverse_number)
    return FALSE;
  IR_set_traverse_number (from_nonterm_declaration, traverse_number);
  if (tested_nonterm_declaration == from_nonterm_declaration)
    return TRUE;
  last_nonterm_rule = IR_nonterm_rule_list (from_nonterm_declaration);
  assert (last_nonterm_rule != NULL);
  current_nonterm_rule = last_nonterm_rule;
  do
    {
      current_nonterm_rule = IR_next_nonterm_rule (current_nonterm_rule);
      last_textual_pattern = IR_textual_pattern_list (current_nonterm_rule);
      assert (last_textual_pattern != NULL);
      current_textual_pattern = last_textual_pattern;
      do
        {
          current_textual_pattern
            = IR_next_textual_pattern (current_textual_pattern);
          if (IR_single_declaration (current_textual_pattern) != NULL
              && IR_IS_OF_TYPE (IR_single_declaration
                                (current_textual_pattern),
                                IR_NM_single_nonterm_declaration)
              && accessible (tested_nonterm_declaration,
                             IR_single_declaration (current_textual_pattern),
                             traverse_number))
            return TRUE;
        }
      while (current_textual_pattern != last_textual_pattern);
    }
  while (current_nonterm_rule != last_nonterm_rule);
  return FALSE;
}
/* ??? */
static void
check_rule_derivability (void)
{
  IR_node_t current_single_declaration;
  IR_node_t last_single_declaration;
  IR_node_t current_rule;
  IR_node_t current_textual_pattern;
  IR_node_t last_textual_pattern;
  int traverse_number;

  last_single_declaration = IR_single_declaration_list (description);
  if (last_single_declaration == NULL)
    return;
  current_single_declaration = last_single_declaration;
  traverse_number = 0;
  do
    {
      current_single_declaration
        = IR_next_single_declaration (current_single_declaration);
      if (IR_IS_OF_TYPE (current_single_declaration,
                         IR_NM_single_nonterm_declaration))
        set_nonterm_derivability (current_single_declaration,
                                  ++traverse_number);
    }
  while (current_single_declaration != last_single_declaration);
  for (current_rule = IR_rule_list (description);
       current_rule != NULL; current_rule = IR_next_rule (current_rule))
    if (!IR_new_commutative_rule_flag (current_rule))
      {
        /* No repeated messages for new commutative rules. */
        last_textual_pattern = IR_textual_pattern_list (current_rule);
        assert (last_textual_pattern != NULL);
        current_textual_pattern = last_textual_pattern;
        do
          {
            current_textual_pattern
              = IR_next_textual_pattern (current_textual_pattern);
            if (IR_single_declaration (current_textual_pattern) != NULL
                && IR_IS_OF_TYPE (IR_single_declaration
                                  (current_textual_pattern),
                                  IR_NM_single_nonterm_declaration)
                && !IR_derivability_flag (IR_single_declaration
                                          (current_textual_pattern))
                /* The following guards decrease number of such messages. */
                && IR_single_nonterm_declaration (current_rule) != NULL
                && accessible (IR_single_nonterm_declaration (current_rule),
                               IR_single_declaration (current_textual_pattern),
                               ++traverse_number))
              {
                error (FALSE, IR_position (current_rule),
                       "this rule for `%s' produces only infinite trees",
                       IR_identifier_itself (IR_nonterm_identifier
                                             (current_rule)));
                break;
              }
          }
        while (current_textual_pattern != last_textual_pattern);
      }
}

/*???*/
void
analyze_program (void)
{
  IR_set_single_declaration_list (description, NULL);
  IR_set_number_of_nonterminals (description, 0);
  IR_set_max_number_of_rule_nonterminals (description, 0);
  check_union_declarations ();
  process_terminal_declarations ();
  numerate_rule_patterns ();
  make_new_commutative_pattern_rules ();
  process_nonterminal_declarations ();
  set_symbol_types ();
  process_rules ();
  check_terminal_usage ();
  check_rule_derivability ();
}
