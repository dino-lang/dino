#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "position.h"
#include "vlobject.h"
#include "ticker.h"
#include "common.h"
#include "ird.h"
#include "gen-comm.h"
#include "output.h"
#include "contexts.h"
#include "parser.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif


#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif
#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif
#ifndef SCHAR_MAX
#define SCHAR_MAX 127
#endif
#ifndef SCHAR_MIN
#define SCHAR_MIN (-128)
#endif
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif
#ifndef SHRT_MAX
#define SHRT_MAX 32767
#endif  
#ifndef SHRT_MIN
#define SHRT_MIN (-32768)
#endif
#ifndef UINT_MAX
#define UINT_MAX (INT_MAX * 2U + 1)
#endif
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif  
#ifndef INT_MIN
#define INT_MIN (-INT_MAX-1)
#endif
#endif



static void
output_msta_title (void)
{
  output_string (output_implementation_file,
                 "/* A MSTA parser generated from `");
  output_string (output_implementation_file, source_file_name);
  if (IR_scanner_flag (description))
    output_string
      (output_implementation_file,
       "' */\n\n#define YYSMSTA 1 /* MSTA scanner identification. */\n\n");
  else
    output_string
      (output_implementation_file,
       "' */\n\n#define YYMSTA 1 /* MSTA parser identification. */\n\n");
}



static void
output_code (FILE *f, IR_node_t code)
{
  assert (IR_IS_OF_TYPE (code, IR_NM_code));
  output_line (f, IR_position (code).line_number,
               IR_position (code).file_name);
  output_string (f, IR_code_insertion_itself (IR_code_itself (code)));
  output_char ('\n', f);
  output_current_line (f);
}

static void
output_start_code_insertions (void)
{
  IR_node_t current_definition;

  if (IR_definition_list (description) == NULL)
    return;
  for (current_definition = IR_definition_list (description);
       current_definition != NULL;
       current_definition = IR_next_definition (current_definition))
    if (IR_IS_OF_TYPE (current_definition, IR_NM_import_code))
      {
        if (define_flag)
          output_code (output_interface_file, current_definition);
        output_code (output_implementation_file, current_definition);
      }
    else if (IR_IS_OF_TYPE (current_definition, IR_NM_local_code)
             || IR_IS_OF_TYPE (current_definition, IR_NM_yacc_code))
      output_code (output_implementation_file, current_definition);
}

static void
output_finish_code_insertions (void)
{
  IR_node_t current_definition;

  if (IR_definition_list (description) == NULL)
    return;
  for (current_definition = IR_definition_list (description);
       current_definition != NULL;
       current_definition = IR_next_definition (current_definition))
    if (IR_IS_OF_TYPE (current_definition, IR_NM_export_code))
      {
        if (define_flag)
          output_code (output_interface_file, current_definition);
        output_code (output_implementation_file, current_definition);
      }
}



#define TRANSLATE_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yystranslate" : "yytranslate")
#define PUSHED_STATE_FLAGS_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yyspushed" : "yypushed")
#define REDUCE_LENGTHS_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysrlength" : "yyrlength")
#define REDUCE_NONTERMS_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysrnonterm" : "yyrnonterm")
#define POP_SHIFT_ACTION_NUMBERS_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yyspsanumber" : "yypsanumber")
#define POP_SHIFT_ACTION_POPPED_STATES_NUMBERS_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yyspsanpop" : "yypsanpop")
#define POP_SHIFT_ACTION_STATES_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yyspsastate" : "yypsastate")

#define ACTION_COMB_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysaction" : "yyaction")
#define ACTION_CHECK_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysacheck" : "yyacheck")
#define ACTION_BASE_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysabase" : "yyabase")
#define ACTION_DEFAULT_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysadefault" : "yyadefault")

#define GOTO_COMB_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysgoto" : "yygoto")
#define GOTO_BASE_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysgbase" : "yygbase")

#define NATTR_POP_COMB_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysnattr_pop" : "yynattr_pop")
#define NAPOP_BASE_VECTOR_NAME\
     (IR_scanner_flag (description) ? "yysnapop_base" : "yynapop_base")

#define TOKEN_NAMES_VECTOR_NAME \
     (IR_scanner_flag (description) ? "yystname" : "yytname")

#define LAST_TOKEN_VALUE_MACRO_NAME\
     (IR_scanner_flag (description)\
      ? "YYSLAST_TOKEN_CODE" : "YYLAST_TOKEN_CODE")
#define NO_TOKEN_INTERNAL_VALUE_MACRO_NAME\
     (IR_scanner_flag (description)\
      ? "YYSNO_TOKEN_INTERNAL_CODE" : "YYNO_TOKEN_INTERNAL_CODE")
#define ERRCODE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSERRCODE" : "YYERRCODE")
#define ERRCLASS_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSERRCLASS" : "YYERRCLASS")

#define YYALLOC_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSALLOC" : "YYALLOC")
#define YYREALLOC_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSREALLOC" : "YYREALLOC")
#define YYFREE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSFREE" : "YYFREE")

#define YYSTACK_SIZE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSSTACK_SIZE" : "YYSTACK_SIZE")
#define YYMAX_STACK_SIZE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSMAX_STACK_SIZE" : "YYMAX_STACK_SIZE")
#define YYSTACK_EXPAND_SIZE_MACRO_NAME\
     (IR_scanner_flag (description)\
      ? "YYSSTACK_EXPAND_SIZE" : "YYMAX_STACK_EXPAND_SIZE")
#define YYSTYPE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSSTYPE" : "YYSTYPE")
#define YYDEBUG_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSDEBUG" : "YYDEBUG")
#define YYEMPTY_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSEMPTY" : "YYEMPTY")
#define YYEOF_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSEOF" : "YYEOF")
#define YYCLEARIN_MACRO_NAME\
     (IR_scanner_flag (description) ? "yysclearin" : "yyclearin")
#define YYDEEPER_ERROR_TRY_MACRO_NAME\
     (IR_scanner_flag (description)\
      ? "yysdeeper_error_try" : "yydeeper_error_try")
#define YYERROK_MACRO_NAME\
     (IR_scanner_flag (description) ? "yyserrok" : "yyerrok")
#define YYABORT_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSABORT" : "YYABORT")
#define YYACCEPT_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSACCEPT" : "YYACCEPT")
#define YYABORT_LABEL_NAME\
     (IR_scanner_flag (description) ? "yysabort" : "yyabort")
#define YYACCEPT_LABEL_NAME\
     (IR_scanner_flag (description) ? "yysaccept" : "yyaccept")
#define YYERROR_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSERROR" : "YYERROR")
#define YYRECOVERING_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSRECOVERING" : "YYRECOVERING")
#define YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME\
   (IR_scanner_flag (description) ? \
    "YYSERR_MAX_LOOK_AHEAD_CHARS" : "YYERR_MAX_LOOK_AHEAD_CHARS")
#define YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME\
   (IR_scanner_flag (description) ? \
    "YYSERR_LOOK_AHEAD_INCREMENT" : "YYERR_LOOK_AHEAD_INCREMENT")
#define YYERR_RECOVERY_END_MACRO_NAME\
 (IR_scanner_flag (description) ? "YYSERR_RECOVERY_END" : "YYERR_RECOVERY_END")
#define YYERR_RECOVERY_MATCHES_MACRO_NAME\
   (IR_scanner_flag (description)\
    ? "YYSERR_RECOVERY_MATCHES" : "YYERR_RECOVERY_MATCHES")
#define YYERR_POPPED_ERROR_STATES_MACRO_NAME\
   (IR_scanner_flag (description)\
    ? "YYSERR_POPPED_ERROR_STATES" : "YYERR_POPPED_ERROR_STATES")
#define YYERR_DISCARDED_CHARS_MACRO_NAME\
   (IR_scanner_flag (description)\
    ? "YYSERR_DISCARDED_CHARS" : "YYERR_DISCARDED_CHARS")

#define YYLOOK_AHEAD_SIZE_MACRO_NAME\
   (IR_scanner_flag (description)\
    ? "YYSLOOK_AHEAD_SIZE" : "YYLOOK_AHEAD_SIZE")
#define YYMAX_LOOK_AHEAD_SIZE_MACRO_NAME\
   (IR_scanner_flag (description)\
    ? "YYSMAX_LOOK_AHEAD_SIZE" : "YYMAX_LOOK_AHEAD_SIZE")
#define YYLOOK_AHEAD_EXPAND_SIZE_MACRO_NAME\
   (IR_scanner_flag (description)\
    ? "YYSLOOK_AHEAD_EXPAND_SIZE" : "YYLOOK_AHEAD_EXPAND_SIZE")

#define YYTOKEN_NAME_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSTOKEN_NAME" : "YYTOKEN_NAME")

#define YYEXPAND_STATES_STACK_FUNCTION_NAME\
     (IR_scanner_flag (description)\
      ? "yysexpand_states_stack" : "yyexpand_states_stack")
#define YYEXPAND_ATTRIBUTES_STACK_FUNCTION_NAME\
     (IR_scanner_flag (description)\
      ? "yysexpand_attributes_stack" : "yyexpand_attributes_stack")


static int max_token_value;

/*  Elements of action table are coded by the following integers:
         NO_ACTION                    -> 0 (it means error)
         SHIFT                 1      -> 1 (Remember: start state has number 0
                                            and is not accessible)
         SHIFT                 2      -> 2
         .....                 
         SHIFT                 n      -> n
         POP-(SHIFT)-ACTION    0      -> n + 1
         .....                 
         POP-(SHIFT)-ACTION    m - 1  -> n + m
         REDUCE                0      -> n + m + 1
         REDUCE                k - 1  -> n + m + k
         LOOK_AHEAD_TABLE      ft     -> n + m + k + 1
         LOOK_AHEAD_TABLE      lt     -> n + m + k + lt - ft + 1

   Here n is number of the last shift, m is number of
   pop-shift-actions, k is number of reduces, lt - ft + 1 is number of
   look ahead tables for processing 2nd, 3rd, ... look ahead
   tokens. */

#define NO_ACTION_BASE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSNO_ACTION_BASE" : "YYNO_ACTION_BASE")
#define NO_STATE_VALUE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSNO_STATE" : "YYNO_STATE")
#define FINAL_STATE_VALUE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSFINAL" : "YYFINAL")
#define NO_ACTION_VALUE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSNO_ACTION" : "YYNO_ACTION")
#define FINAL_STATE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSFINAL" : "YYFINAL")
#define FIRST_POP_SHIFT_ACTION_VALUE_MACRO_NAME\
     (IR_scanner_flag (description)\
      ? "YYS1POP_SHIFT_ACTION" : "YY1POP_SHIFT_ACTION")
#define FIRST_REDUCE_VALUE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYS1REDUCE" : "YY1REDUCE")
#define NREDUCES_VALUE_MACRO_NAME\
     (IR_scanner_flag (description) ? "YYSNREDUCES" : "YYNREDUCES")
#define FIRST_LOOK_AHEAD_TABLE_VALUE_MACRO_NAME\
     (IR_scanner_flag (description)\
      ? "YYS1LOOK_AHEAD_TABLE_VALUE" : "YY1LOOK_AHEAD_TABLE_VALUE")
#define LOOK_AHEAD_TABLE_BASE_MACRO_NAME\
     (IR_scanner_flag (description)\
      ? "YYSLOOK_AHEAD_TABLE_BASE" : "YYLOOK_AHEAD_TABLE_BASE")
#define YYSCANNER_CONSTRUCTOR_ERROR_FLAG_NAME "error_flag"

#define NO_ACTION_VALUE                             0

/* The following value is used as empty element value of the comb
   vectors because all error action value will be in default action
   vector and error action value (0 is start state number) is not used
   in goto vector. */

#define EMPTY_ACTION_AND_GOTO_COMB_VECTOR_ELEMENT_VALUE  NO_ACTION_VALUE

typedef int vector_element_t;

static int no_action_base_value;
static int final_state_number;
static vector_element_t first_pop_shift_action_value;
static vector_element_t first_reduce_value;
static vector_element_t first_look_ahead_table_value;
static int look_ahead_table_base_value;
/* Must be greater then all states numbers. */
static vector_element_t no_state_value;

static void
set_up_max_token_value (void)
{
  IR_node_t current_single_definition;
  int value;

  max_token_value = 0;
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_term_definition))
      {
        if (IR_IS_OF_TYPE (current_single_definition,
                           IR_NM_literal_range_definition))
          value = IR_right_range_bound_value (current_single_definition);
        else
          value = IR_value (current_single_definition);
        if (max_token_value < value)
          max_token_value = value;
      }
}

static void
enumerate_reduces (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_node_t current_LR_situation;
  int reduces_number;

  reduces_number = 0;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                             IR_NM_canonical_rule_end)
              /* Except for axiom rule. */
              && (IR_canonical_rule (IR_element_after_dot
                                     (current_LR_situation))
                  != IR_canonical_rule_list (description))
              && IR_corresponding_regular_arc (current_LR_situation) == NULL
              && (IR_look_ahead_context (current_LR_situation) == NULL
                  || !it_is_zero_context (IR_look_ahead_context
                                          (current_LR_situation))))
            {
              if (regular_optimization_flag)
                {
                  IR_set_reduce_number (current_LR_situation, reduces_number);
                  reduces_number++;
                }
              else
                IR_set_reduce_number
                  (current_LR_situation, 
                   IR_canonical_rule_order_number
                   (IR_canonical_rule (IR_element_after_dot
                                       (current_LR_situation))));
            }
  if (regular_optimization_flag)
    IR_set_reduces_number (description, reduces_number);
  else
    IR_set_reduces_number (description,
                           IR_canonical_rules_number (description));
}

static void
output_macro_definition (const char *comments, const char *macro_name,
                         int macro_value)
{
  output_string (output_implementation_file, comments);
  output_string (output_implementation_file, "\n#define ");
  output_string (output_implementation_file, macro_name);
  output_char (' ', output_implementation_file);
  output_decimal_number (output_implementation_file, macro_value, 0);
  output_string (output_implementation_file, "\n\n");
}

static void
set_up_final_state_number (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;

  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (characteristic_symbol_of_LR_set (current_LR_set)
          == end_marker_single_definition
          && IR_IS_OF_TYPE (IR_element_after_dot (IR_LR_situation_list
                                                  (current_LR_set)),
                                                  IR_NM_canonical_rule_end))
        {
          assert (IR_reachable_flag (current_LR_set));
          final_state_number = IR_LR_set_order_number (current_LR_set);
          return;
        }
  assert (FALSE);
}

static int
internal_trie_nodes_number (IR_node_t first_trie_node)
{
  int result;
  IR_node_t current_trie_node;

  result = 0;
  for (current_trie_node = first_trie_node;
       current_trie_node != NULL;
       current_trie_node = IR_next_brother (current_trie_node))
    if (IR_first_son (current_trie_node) != NULL)
      result
        += internal_trie_nodes_number (IR_first_son (current_trie_node)) + 1;
  return result;
}

static void
prepare_tables_output (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;

  set_up_max_token_value ();
  /* Remember about undefined internal code. */
  no_action_base_value
    = -IR_token_equivalence_classes_number (description) - 1;
  /* Remember that there is not shift to start LR-set which has zero
     number. */
  first_pop_shift_action_value = IR_LR_sets_number (description);
  first_reduce_value
    = first_pop_shift_action_value + IR_number_of_regular_arcs (description);
  enumerate_reduces ();
  first_look_ahead_table_value
    = first_reduce_value + IR_reduces_number (description);
  look_ahead_table_base_value
    = IR_LR_sets_number (description) - first_look_ahead_table_value;
  /* Determine all action tables number. */
  no_state_value = IR_LR_sets_number (description);
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        no_state_value += internal_trie_nodes_number (IR_LR_set_look_ahead_trie
                                                      (current_LR_set));
  output_macro_definition ("/* Max code of all tokens. */",
                           LAST_TOKEN_VALUE_MACRO_NAME, max_token_value);
  output_macro_definition ("/* Undefined internal code for tokens. */",
                           NO_TOKEN_INTERNAL_VALUE_MACRO_NAME,
                           IR_token_equivalence_classes_number (description));
  output_macro_definition ("/* Code for token `error'. */",
                           ERRCODE_MACRO_NAME,
                           IR_value (error_single_definition));
  output_macro_definition
    ("/* Token class of token `error'. */", ERRCLASS_MACRO_NAME,
     IR_equivalence_class_number (error_single_definition));
  output_macro_definition ("/* Base of empty action vector. */",
                           NO_ACTION_BASE_MACRO_NAME, no_action_base_value);
  output_macro_definition ("/* An element of action check vector. */",
                           NO_STATE_VALUE_MACRO_NAME, no_state_value);
  set_up_final_state_number ();
  output_macro_definition ("/* Final state of the parser. */",
                           FINAL_STATE_VALUE_MACRO_NAME, final_state_number);
  output_macro_definition
    ("/* An element of action vector and default action vector. */",
     NO_ACTION_VALUE_MACRO_NAME, NO_ACTION_VALUE);
  output_macro_definition
    ("/* An element of action vector (first pop-shift-action). */",
     FIRST_POP_SHIFT_ACTION_VALUE_MACRO_NAME, first_pop_shift_action_value);
  output_macro_definition
    ("/* An element of action vector (first reduce). */",
     FIRST_REDUCE_VALUE_MACRO_NAME, first_reduce_value);
  output_macro_definition
    ("/* Number of different reduce actions. */",
     NREDUCES_VALUE_MACRO_NAME, IR_reduces_number (description));
  output_macro_definition
    ("/* An element of action vector (the first look ahead table number). */",
     FIRST_LOOK_AHEAD_TABLE_VALUE_MACRO_NAME, first_look_ahead_table_value);
  output_macro_definition
    ("/* Base of the look ahead tables value.  The order number of the\n\
   look ahead table is action look ahead table value + this value. */",
     LOOK_AHEAD_TABLE_BASE_MACRO_NAME, look_ahead_table_base_value);
}

static void
output_vector_element_type (vector_element_t min_vector_element_value,
                            vector_element_t max_vector_element_value)
{
  if (min_vector_element_value >= 0 && max_vector_element_value <= UCHAR_MAX)
    output_string (output_implementation_file, "unsigned char");
  else if (min_vector_element_value >= SCHAR_MIN
           && max_vector_element_value <= SCHAR_MAX)
    output_string (output_implementation_file, "signed char");
  else if (min_vector_element_value >= 0
           && max_vector_element_value <= USHRT_MAX)
    output_string (output_implementation_file, "unsigned short");
  else if (min_vector_element_value >= SHRT_MIN
           && max_vector_element_value <= SHRT_MAX)
    output_string (output_implementation_file, "short");
  else
    {
      assert (min_vector_element_value >= INT_MIN
              && max_vector_element_value <= INT_MAX);
      output_string (output_implementation_file, "int");
    }
}

static void
output_vector (vector_element_t *vector, int vector_length)
{
  int elements_on_line;

  elements_on_line = 1;
  if (vector_length == 0)
    {
      output_decimal_number (output_implementation_file, 0, 0);
      output_string
        (output_implementation_file,
         " /* This is dummy element because the vector is empty */");
    }
  else
    {
      do
        {
          output_decimal_number (output_implementation_file, *vector, 5);
          vector++;
          vector_length--;
          if (elements_on_line == 10)
            {
              elements_on_line = 0;
              output_string (output_implementation_file, ",\n");
            }
          else if (vector_length != 0)
            output_string (output_implementation_file, ", ");
          elements_on_line++;
        }
      while (vector_length != 0);
    }
}



static void
output_translate_vector (void)
{
  IR_node_t current_single_definition;
  int current_token_value;
  vlo_t translate_vector;
  int current_range_value;
  int left_range_value;
  int right_range_value;

  VLO_CREATE (translate_vector, 1000);
  VLO_EXPAND (translate_vector,
              (max_token_value + 1) * sizeof (vector_element_t));
  for (current_token_value = 0;
       current_token_value <= max_token_value;
       current_token_value++)
    /* Undefined value */
    ((vector_element_t *) VLO_BEGIN (translate_vector)) [current_token_value]
      = IR_token_equivalence_classes_number (description);
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition, IR_NM_single_term_definition))
      {
        left_range_value = IR_value (current_single_definition);
        if (IR_IS_OF_TYPE (current_single_definition,
                           IR_NM_literal_range_definition))
          right_range_value
            = IR_right_range_bound_value (current_single_definition);
        else
          right_range_value = left_range_value;
        for (current_range_value = left_range_value;
             current_range_value <= right_range_value;
             current_range_value++)
          ((vector_element_t *) VLO_BEGIN (translate_vector))
            [current_range_value]
              = IR_equivalence_class_number (current_single_definition);
      }
  output_string
    (output_implementation_file,
     "/* Vector for translating external token codes to internal codes. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, max_token_value + 1);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, TRANSLATE_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (translate_vector),
                 VLO_LENGTH (translate_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  VLO_DELETE (translate_vector);
}




struct element
{
  int index;
  vector_element_t value;
};

typedef struct element element_t;

static vlo_t *current_comb_vector_ptr;
static vlo_t *current_check_vector_ptr;
static int current_undefined_base_value;
static vector_element_t current_undefined_comb_vector_element_value;
static vector_element_t current_undefined_check_vector_element_value;
static int max_added_vector_length;

/* Defined only if the current vector contains one or more
   elements. */
static int min_current_vector_index;
static int max_current_vector_index;

static int first_possible_zero_element_index;
static int min_comb_vector_displacement;
static vector_element_t max_comb_vector_element_value;
static vector_element_t min_base_vector_element_value;
static vector_element_t max_base_vector_element_value;

static void
add_vector_element (vlo_t *vector, int index, vector_element_t element_value)
{
  element_t element;

  element.index = index;
  element.value = element_value;
  VLO_ADD_MEMORY (*vector, &element, sizeof (element_t));
  if (max_comb_vector_element_value < element_value)
    max_comb_vector_element_value = element_value;
  if (min_current_vector_index > index)
    min_current_vector_index = index;
  if (max_current_vector_index < index)
    max_current_vector_index = index;
}

static int
add_vector (int vector_number, element_t *vector, int vector_length)
{
  vector_element_t *comb_vector_start;
  vector_element_t *check_vector_start;
  int comb_vector_index;
  int comb_vector_elements_number;
  int vector_index;
  int additional_elements_number;
  int i;

  if (vector_length == 0)
    comb_vector_index = current_undefined_base_value;
  else
    {
      assert (current_check_vector_ptr == NULL
              || (VLO_LENGTH (*current_comb_vector_ptr)
                  == VLO_LENGTH (*current_check_vector_ptr)));
      comb_vector_start = VLO_BEGIN (*current_comb_vector_ptr);
      comb_vector_elements_number
        = VLO_LENGTH (*current_comb_vector_ptr) / sizeof (vector_element_t);
      /* Search for the place in comb vector for the inserted vector. */
#if 1
      if (comb_vector_elements_number - 2 * max_added_vector_length <= 0)
        comb_vector_index = 0;
      else 
        comb_vector_index = rand () % (comb_vector_elements_number
                                       - 2 * max_added_vector_length);
#endif
      for (/*comb_vector_index
             = first_possible_zero_element_index
               - (current_check_vector_ptr == NULL
                  ? min_current_vector_index : 0)*/;
           comb_vector_index < comb_vector_elements_number;
           comb_vector_index++)
        {
          for (vector_index = 0;
               vector_index < vector_length;
               vector_index++)
            if (comb_vector_start [vector [vector_index].index
                                  + comb_vector_index]
                != current_undefined_comb_vector_element_value)
              break;
          if (vector_index >= vector_length)
            break;
        }
      if (comb_vector_elements_number != 0)
        while (comb_vector_start [first_possible_zero_element_index]
               != current_undefined_comb_vector_element_value)
          first_possible_zero_element_index++;
      /* Slot was found (remember about undefined internal code).  Here we
         always reserves additional space for the next vector in order to
         simplify the previous loops.  */
      additional_elements_number
        = (comb_vector_index + max_current_vector_index
           + max_added_vector_length + 1 - comb_vector_elements_number);
      if (additional_elements_number < 0)
        additional_elements_number = 0;
      /* Expand comb and check vectors. */
      VLO_EXPAND (*current_comb_vector_ptr,
                  additional_elements_number * sizeof (vector_element_t));
      comb_vector_start = VLO_BEGIN (*current_comb_vector_ptr);
      if (current_check_vector_ptr != NULL)
        {
          VLO_EXPAND (*current_check_vector_ptr,
                      additional_elements_number * sizeof (vector_element_t));
          check_vector_start = VLO_BEGIN (*current_check_vector_ptr);
        }
      for (i = comb_vector_elements_number;
           i < comb_vector_elements_number + additional_elements_number;
           i++)
        comb_vector_start [i]
          = current_undefined_comb_vector_element_value;
      if (current_check_vector_ptr != NULL)
        for (i = comb_vector_elements_number;
             i < comb_vector_elements_number + additional_elements_number;
             i++)
          check_vector_start [i]
            = current_undefined_check_vector_element_value;
      assert (VLO_LENGTH (*current_comb_vector_ptr) / sizeof (vector_element_t)
              >= comb_vector_index + max_added_vector_length + 1);
      /* Fill comb and check vectors. */
      for (vector_index = 0; vector_index < vector_length; vector_index++)
        {
          /* The following is possible because of terminal equvalence.
             comb_vector_start [comb_vector_index
             + vector [vector_index].index]
             != current_undefined_comb_vector_element_value) */
          comb_vector_start
            [comb_vector_index + vector [vector_index].index]
            = vector [vector_index].value;
          if (current_check_vector_ptr != NULL)
            check_vector_start [comb_vector_index
                               + vector [vector_index].index]
              = vector_number;
        }
      if (comb_vector_index < min_comb_vector_displacement)
        min_comb_vector_displacement = comb_vector_index;
    }
  if (max_base_vector_element_value < comb_vector_index)
    max_base_vector_element_value = comb_vector_index;
  if (min_base_vector_element_value > comb_vector_index)
    min_base_vector_element_value = comb_vector_index;
  /* For forming of the next vector. */
  min_current_vector_index = INT_MAX;
  max_current_vector_index = 0;
  return comb_vector_index;
}

static void
start_comb_vector_forming
  (vlo_t *comb_vector, vlo_t *check_vector, int undefined_base_value,
   vector_element_t undefined_comb_vector_element_value,
   vector_element_t undefined_check_vector_element_value,
   int max_vector_length)
{
  current_comb_vector_ptr = comb_vector;
  current_check_vector_ptr = check_vector;
  current_undefined_base_value = undefined_base_value;
  current_undefined_comb_vector_element_value
    = undefined_comb_vector_element_value;
  current_undefined_check_vector_element_value
    = undefined_check_vector_element_value;
  max_added_vector_length = max_vector_length;
  min_current_vector_index = INT_MAX;
  max_current_vector_index = 0;
  first_possible_zero_element_index = 0;
  min_comb_vector_displacement = 0;
  max_comb_vector_element_value = 0;
  min_base_vector_element_value = 0;
  max_base_vector_element_value = 0;
}

static void
finish_comb_vector_forming (void)
{
  if (current_check_vector_ptr == NULL)
    VLO_SHORTEN (*current_comb_vector_ptr,
                 max_added_vector_length * sizeof (vector_element_t));
  else
    {
      VLO_SHORTEN (*current_comb_vector_ptr,
                   VLO_LENGTH (*current_comb_vector_ptr)
                   - (max_base_vector_element_value + max_added_vector_length)
                   * sizeof (vector_element_t));
      VLO_SHORTEN (*current_check_vector_ptr,
                   VLO_LENGTH (*current_check_vector_ptr)
                   - (max_base_vector_element_value + max_added_vector_length)
                   * sizeof (vector_element_t));
    }
}

static void
add_to_LR_sets_and_trie_nodes_vector (vlo_t *vector,
                                      IR_node_t LR_set_or_trie_node)
{
  IR_node_t current_trie_node;
  IR_node_t first_trie_node;

  VLO_ADD_MEMORY (*vector, &LR_set_or_trie_node, sizeof (IR_node_t));
  if (IR_IS_OF_TYPE (LR_set_or_trie_node, IR_NM_LR_set))
    first_trie_node = IR_LR_set_look_ahead_trie (LR_set_or_trie_node);
  else
    {
      assert (IR_IS_OF_TYPE (LR_set_or_trie_node,
                             IR_NM_LR_set_look_ahead_trie_node));
      first_trie_node = LR_set_or_trie_node;
    }
  for (current_trie_node = first_trie_node;
       current_trie_node != NULL;
       current_trie_node = IR_next_brother (current_trie_node))
    if (IR_first_son (current_trie_node) != NULL)
      add_to_LR_sets_and_trie_nodes_vector (vector,
                                            IR_first_son (current_trie_node));
}

static vector_element_t
LR_situation_vector_element (IR_node_t LR_situation)
{
  if (IR_corresponding_regular_arc (LR_situation) != NULL)
    return (first_pop_shift_action_value
            + IR_number_of_regular_arc (IR_corresponding_regular_arc
                                        (LR_situation)));
  else if (IR_IS_OF_TYPE (IR_element_after_dot (LR_situation),
                          IR_NM_canonical_rule_end))
    return first_reduce_value + IR_reduce_number (LR_situation);
  else
    /* Remember that there is not shift to start LR-set
       which has zero number. */
    return IR_LR_set_order_number (IR_goto_LR_set (LR_situation));
}

#define max(a, b) (a > b ? a : b)

static void
output_action_table (void)
{
  vector_element_t max_default_vector_element_value;
  vector_element_t base_value;
  vector_element_t default_value;
  IR_node_t *current_LR_set_or_trie_node_ptr;
  IR_node_t trie_node_list;
  IR_node_t current_trie_node;
  IR_node_t current_LR_set;
  IR_node_t default_LR_situation;
  int action_table_number;
  IR_node_t current_LR_core;
  vlo_t LR_sets_and_trie_nodes_vector;
  vlo_t comb_vector;
  vlo_t check_vector;
  vlo_t base_vector;
  vlo_t default_vector;
  vlo_t action_vector;
  int vector_length;
#ifndef NDEBUG
  int non_empty_action_elements = 0;
  int all_action_vectors_length = 0;
  int all_based_action_vectors_length = 0;
#endif

  /* Create vector of pointers to LR-sets and trie nodes (of 2nd, 3rd
     and so on levels) ordered by number of action elements (LR-set or
     trie node list with the maximum number is the first). */
  VLO_CREATE (LR_sets_and_trie_nodes_vector, 5000);
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        add_to_LR_sets_and_trie_nodes_vector (&LR_sets_and_trie_nodes_vector,
                                              current_LR_set);
  action_table_number = IR_LR_sets_number (description);
  /* Enumerate additional action tables for 2nd, 3rd, ... look ahead
     tokens. */
  for (current_LR_set_or_trie_node_ptr
       = VLO_BEGIN (LR_sets_and_trie_nodes_vector);
       (char *) current_LR_set_or_trie_node_ptr
       <= (char *) VLO_END (LR_sets_and_trie_nodes_vector);
       current_LR_set_or_trie_node_ptr++)
    if (IR_IS_OF_TYPE (*current_LR_set_or_trie_node_ptr,
                       IR_NM_LR_set_look_ahead_trie_node))
      {
        IR_set_additional_action_table_number
          (*current_LR_set_or_trie_node_ptr, action_table_number);
        action_table_number++;
      }
  /* Create base, action, check and default vectors. */
  VLO_CREATE (comb_vector, 50000);
  VLO_CREATE (check_vector, 50000);
  VLO_CREATE (base_vector, 5000);
  VLO_CREATE (default_vector, 5000);
  VLO_EXPAND (base_vector,
              VLO_LENGTH (LR_sets_and_trie_nodes_vector)
              / sizeof (IR_node_t) * sizeof (vector_element_t));
  VLO_EXPAND (default_vector,
              VLO_LENGTH (LR_sets_and_trie_nodes_vector)
              / sizeof (IR_node_t) * sizeof (vector_element_t));
  VLO_CREATE (action_vector, 2000);
  start_comb_vector_forming
    (&comb_vector, &check_vector, no_action_base_value,
     EMPTY_ACTION_AND_GOTO_COMB_VECTOR_ELEMENT_VALUE, no_state_value,
     IR_token_equivalence_classes_number (description));
  max_default_vector_element_value = 0;
  for (current_LR_set_or_trie_node_ptr
       = VLO_BEGIN (LR_sets_and_trie_nodes_vector);
       (char *) current_LR_set_or_trie_node_ptr
       <= (char *) VLO_END (LR_sets_and_trie_nodes_vector);
       current_LR_set_or_trie_node_ptr++)
    {
      VLO_NULLIFY (action_vector);
      if (IR_IS_OF_TYPE (*current_LR_set_or_trie_node_ptr, IR_NM_LR_set))
        {
          action_table_number
            = IR_LR_set_order_number (*current_LR_set_or_trie_node_ptr);
          if (action_table_number == final_state_number)
            trie_node_list = NULL;
          else
            trie_node_list
              = IR_LR_set_look_ahead_trie (*current_LR_set_or_trie_node_ptr);
        }
      else
        {
          assert (IR_IS_OF_TYPE (*current_LR_set_or_trie_node_ptr,
                                 IR_NM_LR_set_look_ahead_trie_node));
          action_table_number
            = IR_additional_action_table_number
              (*current_LR_set_or_trie_node_ptr);
          trie_node_list = *current_LR_set_or_trie_node_ptr;
        }
      for (current_trie_node = trie_node_list;
           current_trie_node != NULL
           && (IR_corresponding_single_term_definition (current_trie_node)
               != NULL);
           current_trie_node = IR_next_brother (current_trie_node))
        if (IR_first_son (current_trie_node) != NULL)
          add_vector_element
            (&action_vector,
             IR_equivalence_class_number
             (IR_corresponding_single_term_definition (current_trie_node)),
             first_look_ahead_table_value
             + (IR_additional_action_table_number
                (IR_first_son (current_trie_node)))
             - IR_LR_sets_number (description));
        else
          add_vector_element
            (&action_vector,
             IR_equivalence_class_number
             (IR_corresponding_single_term_definition (current_trie_node)),
             LR_situation_vector_element (IR_corresponding_LR_situation
                                          (current_trie_node)));
      if (current_trie_node == NULL)
        default_LR_situation = NULL;
      else
        {
          assert (IR_first_son (current_trie_node) == NULL);
          default_LR_situation
            = IR_corresponding_LR_situation (current_trie_node);
        }
      vector_length = VLO_LENGTH (action_vector) / sizeof (element_t);
#ifndef NDEBUG
      if (debug_level >= 1)
        {
          non_empty_action_elements += vector_length;
          if (vector_length != 0)
            {
              all_action_vectors_length += max_current_vector_index + 1;
              all_based_action_vectors_length
                += max_current_vector_index - min_current_vector_index + 1; 
            }
        }
#endif
      base_value
        = add_vector (action_table_number,
                      (element_t *) VLO_BEGIN (action_vector), vector_length);
      ((vector_element_t *) VLO_BEGIN (base_vector)) [action_table_number]
          = base_value;
      if (default_LR_situation == NULL)
        default_value = NO_ACTION_VALUE;
      else
        default_value = LR_situation_vector_element (default_LR_situation);
      if (max_default_vector_element_value < default_value)
        max_default_vector_element_value = default_value;
      ((vector_element_t *) VLO_BEGIN (default_vector)) [action_table_number]
        = default_value;
    }
#ifndef NDEBUG
  if (debug_level >= 1)
    {
      int i;
      int comb_non_empty = 0;

      for (i = 0; i < VLO_LENGTH (comb_vector) / sizeof (vector_element_t);
           i++)
        if (((vector_element_t *) VLO_BEGIN (comb_vector)) [i]
            != NO_ACTION_VALUE)
          comb_non_empty++;
      fprintf (stderr,
               "Size:    action vectors -- %d, action comb vector -- %ld\n",
               all_action_vectors_length,
               VLO_LENGTH (comb_vector) / sizeof (vector_element_t));
      fprintf
        (stderr,
         "Filling: action vectors -- %d%%, action comb vector -- %ld%%\n",
         non_empty_action_elements * 100 / max (1, all_action_vectors_length),
         comb_non_empty * 100 * sizeof (vector_element_t)
         / max (1, VLO_LENGTH (comb_vector)));
      fprintf (stderr, "         based action vectors -- %d%%\n",
               non_empty_action_elements * 100
               / max (1, all_based_action_vectors_length));
    }
#endif
  assert (min_comb_vector_displacement <= 0);
  finish_comb_vector_forming ();
  output_string (output_implementation_file,
                 "/* Comb vector for actions. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, max_comb_vector_element_value);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, ACTION_COMB_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (comb_vector),
                 VLO_LENGTH (comb_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  output_string (output_implementation_file,
                 "/* Check vector for actions. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, no_state_value);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, ACTION_CHECK_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (check_vector),
                 VLO_LENGTH (check_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  output_string (output_implementation_file,
                 "/* Base vector for actions. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (min_base_vector_element_value,
                              max_base_vector_element_value);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, ACTION_BASE_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (base_vector),
                 VLO_LENGTH (base_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  /* Default actions vector. */
  output_string (output_implementation_file,
                 "/* Default vector for actions. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, max_default_vector_element_value);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, ACTION_DEFAULT_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (default_vector),
                 VLO_LENGTH (default_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  VLO_DELETE (LR_sets_and_trie_nodes_vector);
  VLO_DELETE (action_vector);
  VLO_DELETE (base_vector);
  VLO_DELETE (check_vector);
  VLO_DELETE (comb_vector);
}




static void
output_nonterminal_goto_table (void)
{
  vector_element_t base_value;
  IR_node_t current_LR_situation;
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;
  vlo_t comb_vector;
  vlo_t base_vector;
  vlo_t goto_vector;
  int vector_length;
#ifndef NDEBUG
  int non_empty_goto_elements = 0;
  int all_goto_vectors_length = 0;
  int all_based_goto_vectors_length = 0;
#endif

  /* Create base and goto vectors. */
  VLO_CREATE (comb_vector, 50000);
  VLO_CREATE (base_vector, 5000);
  VLO_EXPAND (base_vector,
              IR_LR_sets_number (description) * sizeof (vector_element_t));
  VLO_CREATE (goto_vector, 2000);
  start_comb_vector_forming
    (&comb_vector, NULL, 0, EMPTY_ACTION_AND_GOTO_COMB_VECTOR_ELEMENT_VALUE, 0,
     IR_nonterminals_number (description));
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          VLO_NULLIFY (goto_vector);
          for (current_LR_situation = IR_LR_situation_list (current_LR_set);
               current_LR_situation != NULL;
               current_LR_situation
                 = IR_next_LR_situation (current_LR_situation))
            if (IR_first_symbol_LR_situation (current_LR_situation)
                && !IR_IS_OF_TYPE (IR_element_after_dot
                                   (current_LR_situation),
                                   IR_NM_canonical_rule_end)
                && !IR_goto_arc_has_been_removed (current_LR_situation)
                && IR_IS_OF_TYPE (IR_element_itself (IR_element_after_dot
                                                     (current_LR_situation)),
                                  IR_NM_single_nonterm_definition))
              add_vector_element
                (&goto_vector,
                 IR_nonterm_order_number
                 (IR_element_itself (IR_element_after_dot
                                     (current_LR_situation))),
                 IR_LR_set_order_number (IR_goto_LR_set
                                         (current_LR_situation)));
          vector_length = VLO_LENGTH (goto_vector) / sizeof (element_t);
#ifndef NDEBUG
          if (debug_level >= 1)
            {
              non_empty_goto_elements += vector_length;
              if (vector_length != 0)
                {
                  all_goto_vectors_length += max_current_vector_index + 1;
                  all_based_goto_vectors_length
                    += (max_current_vector_index
                        - min_current_vector_index + 1); 
                }
            }
#endif
          base_value
            = add_vector (0, (element_t *) VLO_BEGIN (goto_vector),
                          VLO_LENGTH (goto_vector) / sizeof (element_t));
          ((vector_element_t *) VLO_BEGIN (base_vector))
            [IR_LR_set_order_number (current_LR_set)] = base_value;
        }
#ifndef NDEBUG
  if (debug_level >= 1)
    {
      int i;
      int comb_non_empty = 0;

      for (i = 0; i < VLO_LENGTH (comb_vector) / sizeof (vector_element_t);
           i++)
        if (((vector_element_t *) VLO_BEGIN (comb_vector)) [i]
            != EMPTY_ACTION_AND_GOTO_COMB_VECTOR_ELEMENT_VALUE)
          comb_non_empty++;
      fprintf (stderr, "Size: goto vectors -- %d, goto comb vector -- %ld\n",
               all_goto_vectors_length,
               VLO_LENGTH (comb_vector) / sizeof (vector_element_t));
      fprintf
        (stderr,
         "Filling: goto vectors -- %d%%, goto comb vector -- %ld%%\n",
         non_empty_goto_elements * 100 / max (1, all_goto_vectors_length),
         comb_non_empty * 100 * sizeof (vector_element_t)
         / max (1, VLO_LENGTH (comb_vector)));
      fprintf (stderr,
               "         based goto vectors -- %d%%\n",
               non_empty_goto_elements * 100
               / max (1, all_based_goto_vectors_length));
    }
#endif
  finish_comb_vector_forming ();
  output_string (output_implementation_file,
                 "/* Comb vector for gotos. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, max_comb_vector_element_value);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, GOTO_COMB_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (comb_vector),
                 VLO_LENGTH (comb_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  output_string (output_implementation_file,
                 "/* Base vector for gotos. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (min_base_vector_element_value,
                              max_base_vector_element_value);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, GOTO_BASE_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (base_vector),
                 VLO_LENGTH (base_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  VLO_DELETE (goto_vector);
  VLO_DELETE (base_vector);
  VLO_DELETE (comb_vector);
}

/* The following value is empty element of the following vector. */

#define EMPTY_NATTR_POP_VECTOR_ELEMENT_VALUE (-1)

/* The following vector is for `start_LR_set_for_forming_nattr_pop_vector'. */

static vlo_t nattr_pop_vector;


/* The following variable contains LR_set for which given nattr_pop_vector
   is being formed. */

static IR_node_t start_LR_set_for_forming_nattr_pop_vector;

static void
form_nattr_pop_vector (IR_node_t LR_set, int nattr_pop_number)
{
  IR_node_t LR_situation_of_immediate_LR_set_predecessor;
  IR_node_t immediate_LR_set_predecessor;
  IR_double_link_t LR_situation_reference;
  int LR_set_order_number;

  if (IR_start_LR_set_pass (LR_set)
      == start_LR_set_for_forming_nattr_pop_vector)
    return;
  IR_set_start_LR_set_pass (LR_set, start_LR_set_for_forming_nattr_pop_vector);
  if (IR_attribute_is_used (LR_set))
    nattr_pop_number++;
  for (LR_situation_reference = IR__first_double_link (LR_set);
       LR_situation_reference != NULL;
       LR_situation_reference
         = IR__next_double_link (LR_situation_reference))
    {
      LR_situation_of_immediate_LR_set_predecessor
        = IR__owner (LR_situation_reference);
      if (IR_IS_OF_TYPE (LR_situation_of_immediate_LR_set_predecessor,
                         IR_NM_LR_situation))
        {
          /* See comments in file `ird.sprut'. */
          assert (IR_first_symbol_LR_situation
                  (LR_situation_of_immediate_LR_set_predecessor));
          immediate_LR_set_predecessor
	    = IR_LR_set (LR_situation_of_immediate_LR_set_predecessor);
          if (IR_reachable_flag (immediate_LR_set_predecessor)
	      && IR_it_is_pushed_LR_set (immediate_LR_set_predecessor))
            {
              LR_set_order_number
		= IR_LR_set_order_number (immediate_LR_set_predecessor);
              add_vector_element
                (&nattr_pop_vector, LR_set_order_number, nattr_pop_number); 
            }
          else
            form_nattr_pop_vector (immediate_LR_set_predecessor,
                                   nattr_pop_number);
        }
    }
}


static void
output_nattr_pop_table (void)
{
  vector_element_t base_value;
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;
  vlo_t comb_vector;
  vlo_t base_vector;
#ifndef NDEBUG
  int vector_length;
  int non_empty_nattr_pop_elements = 0;
  int all_nattr_pop_vectors_length = 0;
  int all_based_nattr_pop_vectors_length = 0;
#endif

  /* Create base and goto vectors. */
  VLO_CREATE (comb_vector, 50000);
  VLO_CREATE (base_vector, 5000);
  VLO_EXPAND (base_vector,
              IR_LR_sets_number (description) * sizeof (vector_element_t));
  VLO_CREATE (nattr_pop_vector, 2000);
  start_comb_vector_forming
    (&comb_vector, NULL, 0, EMPTY_NATTR_POP_VECTOR_ELEMENT_VALUE, 0,
     IR_LR_sets_number (description));
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        {
          VLO_NULLIFY (nattr_pop_vector);
          start_LR_set_for_forming_nattr_pop_vector = current_LR_set;
          form_nattr_pop_vector (current_LR_set, 0);
#ifndef NDEBUG
          if (debug_level >= 1)
            {
              vector_length
                = VLO_LENGTH (nattr_pop_vector) / sizeof (element_t);
              non_empty_nattr_pop_elements += vector_length;
              if (vector_length != 0)
                {
                  all_nattr_pop_vectors_length += max_current_vector_index + 1;
                  all_based_nattr_pop_vectors_length
                    += (max_current_vector_index
                        - min_current_vector_index + 1); 
                }
            }
#endif
          base_value
            = add_vector
              (0, (element_t *) VLO_BEGIN (nattr_pop_vector),
               VLO_LENGTH (nattr_pop_vector) / sizeof (element_t));
          ((vector_element_t *) VLO_BEGIN (base_vector))
            [IR_LR_set_order_number (current_LR_set)] = base_value;
        }
#ifndef NDEBUG
  if (debug_level >= 1)
    {
      int i;
      int comb_non_empty = 0;

      for (i = 0; i < VLO_LENGTH (comb_vector) / sizeof (vector_element_t);
           i++)
        if (((vector_element_t *) VLO_BEGIN (comb_vector)) [i]
            != EMPTY_NATTR_POP_VECTOR_ELEMENT_VALUE)
          comb_non_empty++;
      fprintf
        (stderr,
         "Size:    npop attr vectors -- %d, npop attr comb vector -- %ld\n",
         all_nattr_pop_vectors_length,
         VLO_LENGTH (comb_vector) / sizeof (vector_element_t));
      fprintf
        (stderr,
         "Filling: npop attr vectors -- %d%%, npop attr comb vector -- %ld%%\n",
         non_empty_nattr_pop_elements * 100
         / max (1, all_nattr_pop_vectors_length),
         comb_non_empty * 100 * sizeof (vector_element_t)
         / max (1, VLO_LENGTH (comb_vector)));
      fprintf (stderr, "         based vectors -- %d%%\n",
               non_empty_nattr_pop_elements * 100
               / max (1, all_based_nattr_pop_vectors_length));
    }
#endif
  finish_comb_vector_forming ();
  output_string
    (output_implementation_file,
     "/* Comb vector for popping attributes during error recovery. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (EMPTY_NATTR_POP_VECTOR_ELEMENT_VALUE,
                              max_comb_vector_element_value);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, NATTR_POP_COMB_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (comb_vector),
                 VLO_LENGTH (comb_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  output_string
    (output_implementation_file,
     "/* Base vector for popping attributes during error recovery. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (min_base_vector_element_value,
                              max_base_vector_element_value);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, NAPOP_BASE_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (base_vector),
                 VLO_LENGTH (base_vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  VLO_DELETE (nattr_pop_vector);
  VLO_DELETE (base_vector);
  VLO_DELETE (comb_vector);
}

static void
output_pushed_states_table (void)
{
  IR_node_t current_LR_set;
  IR_node_t current_LR_core;
  vlo_t vector;
  int vector_index;

  VLO_CREATE (vector, 5000);
  VLO_EXPAND (vector,
              IR_LR_sets_number (description) * sizeof (vector_element_t));
  for (vector_index = 0;
       vector_index < IR_LR_sets_number (description);
       vector_index++)
    ((vector_element_t *) VLO_BEGIN (vector)) [vector_index] = 0;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set)
          && IR_it_is_pushed_LR_set (current_LR_set))
        ((vector_element_t *) VLO_BEGIN (vector))
          [IR_LR_set_order_number (current_LR_set)] = 1;
  output_string (output_implementation_file,
                 "/* Flags of pushed LR-sets. */\n");
  output_string (output_implementation_file, "static const ");
  output_vector_element_type (0, 1);
  output_char (' ', output_implementation_file);
  output_string (output_implementation_file, PUSHED_STATE_FLAGS_VECTOR_NAME);
  output_string (output_implementation_file, "[] = {\n");
  output_vector ((vector_element_t *) VLO_BEGIN (vector),
                 VLO_LENGTH (vector) / sizeof (vector_element_t));
  output_string (output_implementation_file, "};\n\n");
  VLO_DELETE (vector);
}

static int
output_token_representation (FILE *f, IR_node_t single_term_definition,
                             int literal_code)
{
  int left_range_value;
  int right_range_value;
  char representation [20];
  char *str;

  assert (IR_IS_OF_TYPE (single_term_definition,
                         IR_NM_single_term_definition));
  left_range_value = IR_value (single_term_definition);
  if (IR_IS_OF_TYPE (single_term_definition,
                     IR_NM_literal_range_definition))
    {
      assert
        (left_range_value <= literal_code
         && (IR_right_range_bound_value (single_term_definition)
             >= literal_code));
      if (literal_code == left_range_value)
        return
          output_identifier_or_literal
            (f, IR_identifier_or_literal (single_term_definition), TRUE);
      else if (literal_code == right_range_value)
        return
          output_identifier_or_literal
            (f, IR_right_range_bound_literal (single_term_definition), TRUE);
      else
        {
          assert (literal_code >= 0 && literal_code <= UCHAR_MAX);
          if (isprint (literal_code) && literal_code != '\\'
              && literal_code != '"')
            {
              representation[0] = '\'';
              representation[1] = literal_code;
              representation[2] = '\'';
              representation[3] = '\0';
              str = representation;
            }
          else
            {
              if (literal_code == '\n')
                str = "'\\\\n'";
              else if (literal_code == '\t')
                str = "'\\\\t'";
              else if (literal_code == '\v')
                str = "'\\\\v'";
              else if (literal_code == '\b')
                str = "'\\\\b'";
              else if (literal_code == '\r')
                str = "'\\\\r'";
              else if (literal_code == '\f')
                str = "'\\\\f'";
              else if (literal_code == '\\')
                str = "'\\\\\\\\'";
              else if (literal_code == '"')
                str = "'\\\"'";
              else
                {
                  sprintf (representation, "'\\\\%o'", literal_code);
                  str = representation;
                }
            }
          output_string (f, str);
          return strlen (str);
        }
    }
  else
    {
      assert (left_range_value == literal_code);
      return
        output_identifier_or_literal
          (f, IR_identifier_or_literal (single_term_definition), TRUE);
    }
}

#define MAX_COLUMN_IN_TOKEN_NAMES_TABLE  60

#define END_MARKER_NAME                  "end-of-file"

static void
output_token_name_table (void)
{
  IR_node_t current_single_definition;
  int current_token_value;
  int current_column;
  int current_range_value;
  int left_range_value;
  int right_range_value;
  vlo_t single_term_definition_ptrs;

  output_string (output_implementation_file, "#if ");
  output_string (output_implementation_file, YYDEBUG_MACRO_NAME);
  output_string (output_implementation_file, " != 0\n");
  output_string (output_implementation_file, "const char *");
  output_string (output_implementation_file, TOKEN_NAMES_VECTOR_NAME);
  output_string (output_implementation_file, " [] =\n{");
  VLO_CREATE (single_term_definition_ptrs, 2000);
  VLO_EXPAND (single_term_definition_ptrs,
              (max_token_value + 1) * sizeof (IR_node_t));
  for (current_token_value = 0;
       current_token_value <= max_token_value;
       current_token_value++)
    ((IR_node_t *) VLO_BEGIN (single_term_definition_ptrs))
      [current_token_value] = NULL;
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition,
                       IR_NM_single_term_definition))
      {
        left_range_value = IR_value (current_single_definition);
        if (IR_IS_OF_TYPE (current_single_definition,
                           IR_NM_literal_range_definition))
          right_range_value
            = IR_right_range_bound_value (current_single_definition);
        else
          right_range_value = left_range_value;
        for (current_range_value = left_range_value;
             current_range_value <= right_range_value;
             current_range_value++)
          ((IR_node_t *) VLO_BEGIN (single_term_definition_ptrs))
            [current_range_value] = current_single_definition;
      }
  output_string (output_implementation_file, "  ");
  current_column = 3;
  for (current_token_value = 0;
       current_token_value <= max_token_value;
       current_token_value++)
    {
      if (((IR_node_t *) VLO_BEGIN (single_term_definition_ptrs))
          [current_token_value] == NULL)
        {
          /* Incorrect code */
          output_string (output_implementation_file, "0");
          current_column++;
        }
      else
        {
          output_char ('\"', output_implementation_file);
          if (((IR_node_t *) VLO_BEGIN (single_term_definition_ptrs))
              [current_token_value] == end_marker_single_definition)
            {
              output_string (output_implementation_file, END_MARKER_NAME);
              current_column += strlen (END_MARKER_NAME);
            }
          else
            current_column
              += output_token_representation
                (output_implementation_file, 
                 ((IR_node_t *) VLO_BEGIN (single_term_definition_ptrs))
                 [current_token_value], current_token_value);
          output_char ('\"', output_implementation_file);
          current_column += 2;
        }
      if (current_token_value != max_token_value)
        {
          output_string (output_implementation_file, ", ");
          current_column += 2;
        }
      if (current_column > MAX_COLUMN_IN_TOKEN_NAMES_TABLE)
        {
          output_string (output_implementation_file, "\n  ");
          current_column = 3;
        }
    }
  VLO_DELETE (single_term_definition_ptrs);
  output_string (output_implementation_file, "\n};\n");
  output_string (output_implementation_file, "#endif\n\n");
}

static void
output_parser_tables (void)
{
#ifndef NDEBUG
  ticker_t temp_ticker;
#endif

#ifndef NDEBUG
  temp_ticker = create_ticker ();
#endif
  output_translate_vector ();
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr, "      translate vector creation & output -- %ssec\n",
             active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  output_action_table ();
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr, "      action table creation & output -- %ssec\n",
             active_time_string (temp_ticker));
  temp_ticker = create_ticker ();
#endif
  output_nonterminal_goto_table ();
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr,
             "      nonterminal goto table creation & output -- %ssec\n",
             active_time_string (temp_ticker));
#endif
  if (regular_optimization_flag)
    {
#ifndef NDEBUG
      temp_ticker = create_ticker ();
#endif
      output_pushed_states_table ();
#ifndef NDEBUG
      if (time_flag)
        fprintf
          (stderr,
           "      pushed states flag table creation & output -- %ssec\n",
           active_time_string (temp_ticker));
      temp_ticker = create_ticker ();
#endif
      output_nattr_pop_table ();
#ifndef NDEBUG
      if (time_flag)
        fprintf
          (stderr,
           "      popped attributes number table creation & output -- %ssec\n",
           active_time_string (temp_ticker));
#endif
    }
#ifndef NDEBUG
  temp_ticker = create_ticker ();
#endif
  output_token_name_table ();
#ifndef NDEBUG
  if (time_flag)
    fprintf (stderr, "      token name table creation & output -- %ssec\n",
             active_time_string (temp_ticker));
#endif
}

static void
output_include_directives (void)
{
  output_string (output_implementation_file, "#include <stdio.h>\n");
  output_string (output_implementation_file, "#include <stdlib.h>\n\n");
}

static void
output_yyparse_function_name (FILE *f)
{
  output_string (f, sym_prefix);
  if (IR_scanner_flag (description))
    output_string (f, "lex");
  else
    output_string (f, "parse");
}

static void
output_yylex_function_name (FILE *f)
{
  output_string (f, sym_prefix);
  output_string (f, (IR_scanner_flag (description) ? "slex" : "lex"));
}

static void
output_yyparser_class_name (FILE *f)
{
  output_string (f, sym_prefix);
  if (IR_scanner_flag (description))
    output_string (f, "scanner");
  else
    output_string (f, "parser");
}

static void
output_yylex_start_function_name (FILE *f)
{
  if (cpp_flag)
    output_yyparser_class_name (f);
  else
    {
      output_string (f, sym_prefix);
      output_string (f, "lex_start");
    }
}

static void
output_yylex_finish_function_name (FILE *f)
{
  if (cpp_flag)
    output_yyparser_class_name (f);
  else
    {
      output_string (f, sym_prefix);
      output_string (f, "lex_finish");
    }
}

static void
output_yyerror_function_name (FILE *f)
{
  output_string (f, sym_prefix);
  if (IR_scanner_flag (description))
    output_string (f, "serror");
  else
    output_string (f, "error");
}

static void
output_yylval_variable_name (FILE *f)
{
  output_string (f, sym_prefix);
  if (IR_scanner_flag (description))
    output_string (f, "slval");
  else
    output_string (f, "lval");
}

static void
output_yychar_variable_name (FILE *f)
{
  output_string (f, sym_prefix);
  if (IR_scanner_flag (description))
    output_string (f, "schar");
  else
    output_string (f, "char");
}

static void
output_yydebug_variable_name (FILE *f)
{
  output_string (f, sym_prefix);
  if (IR_scanner_flag (description))
    output_string (f, "sdebug");
  else
    output_string (f, "debug");
}

static void
output_yystype_definition (FILE *f)
{
  if (IR_union_code (description) == NULL)
    {
      output_string (f, "#ifndef  ");
      output_string (f, YYSTYPE_MACRO_NAME);
      output_string (f, "\n#define  ");
      output_string (f, YYSTYPE_MACRO_NAME);
      output_string (f, "  int\n#endif\n\n");
    }
  else
    {
      output_string (f, "typedef union {");
      output_line (f,
                   IR_position (IR_union_code (description)).line_number,
                   IR_position (IR_union_code (description)).file_name);
      output_string (f,
                     IR_code_insertion_itself (IR_code_itself
                                               (IR_union_code (description))));
      output_string (f, "}  ");
      output_string (f, YYSTYPE_MACRO_NAME);
      output_string (f, ";\n\n");
      output_current_line (f);
    }
}

static void
output_token_definitions (FILE *f)
{
  IR_node_t current_single_definition;
  int first_enumeration_flag;

  first_enumeration_flag = TRUE;
  for (current_single_definition = IR_single_definition_list (description);
       current_single_definition != NULL;
       current_single_definition
       = IR_next_single_definition (current_single_definition))
    if (IR_IS_OF_TYPE (current_single_definition, IR_NM_single_term_definition)
        && IR_IS_OF_TYPE (IR_identifier_or_literal (current_single_definition),
                          IR_NM_identifier)
        && current_single_definition != end_marker_single_definition
        && current_single_definition != error_single_definition)
      {
        if (enum_flag)
          {
            if (first_enumeration_flag)
              {
                output_string (f, "enum\n");
                output_string (f, "{\n");
                first_enumeration_flag = FALSE;
              }
            else
              output_string (f, ",\n");
            output_string (f, "  ");
            output_string (f,
                           IR_identifier_itself (IR_identifier_or_literal
                                                 (current_single_definition)));
            output_string (f, " = ");
            output_decimal_number (f, IR_value (current_single_definition), 0);
          }
        else
          {
            output_string (f, "#define ");
            output_string (f,
                           IR_identifier_itself (IR_identifier_or_literal
                                                 (current_single_definition)));
            output_char (' ', f);
            output_decimal_number (f, IR_value (current_single_definition), 0);
            output_char ('\n', f);
          }
      }
  if (!first_enumeration_flag)
    {
      output_string (f, "\n};\n");
    }
  output_char ('\n', f);
}

static void
output_yylval_definition (FILE *f)
{
  output_string (f, YYSTYPE_MACRO_NAME);
  output_string (f, "  ");
  output_yylval_variable_name (f);
  output_string (f, ";\n\n");
}

static void
output_yychar_definition (FILE *f)
{
  output_string (f, "int ");
  output_yychar_variable_name (f);
  output_string (f, ";\n\n");
}

static void
output_yydebug_definition (FILE *f)
{
  output_string (f, "int ");
  output_yydebug_variable_name (f);
  output_string (f, ";\n\n");
}

static void
output_yyparse_title (FILE *f, int inside_class)
{
  assert (cpp_flag || !inside_class);
  output_string (f, "int ");
  if (!inside_class && cpp_flag)
    {
      output_yyparser_class_name (f);
      output_string (f, "::");
    }
  output_yyparse_function_name (f);
  if (cpp_flag)
    output_string (f, " (void)");
  else
    output_string (f, " ()");
}


#define YYSTATES_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysstates" : "yystates")
#define YYATTRIBUTES_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysattributes" : "yyattributes")
#define YYLOOK_AHEAD_CHAR_VARIABLE_NAME\
  (IR_scanner_flag (description) ? "yyslook_ahead_char" : "yylook_ahead_char")
#define YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME\
  (IR_scanner_flag (description) ? "yyslook_ahead_attribute" : "yylook_ahead_attribute")
#define YYFIRST_CHAR_PTR_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysfirst_char_ptr" : "yyfirst_char_ptr")
#define YYFIRST_CHAR_PTR_1_VARIABLE_NAME\
     (IR_scanner_flag (description)\
      ? "yysfirst_char_ptr_1" : "yyfirst_char_ptr_1")
#define YYNERRS_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysnerrs" : "yynerrs")

static void
output_inside_outside_definitions (FILE *f, int inside_flag)
{
  /* Definition of `yystates'. */
  output_string (f, (inside_flag ? "  int *" : "static int *"));
  output_string (f, YYSTATES_VARIABLE_NAME);
  output_string (f, ";\n");
  /* Definition of `yyattributes'. */
  output_string (f, (inside_flag ? "  " : "static "));
  output_string (f, YYSTYPE_MACRO_NAME);
  output_string (f, " *");
  output_string (f, YYATTRIBUTES_VARIABLE_NAME);
  output_string (f, ";\n");
  if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
    {
      /* Definition of `yylook_ahead_char'. */
      output_string (f, (inside_flag ? "  int " : "static int "));
      output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (f, ";\n");
      /* Definition of `yylook_ahead_attribute'. */
      output_string (f, (inside_flag ? "  " : "static "));
      output_string (f, YYSTYPE_MACRO_NAME);
      output_string (f, " ");
      output_string (f, YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
      output_string (f, ";\n");
    }
  else if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
    {
      /* Definition of `yylook_ahead_char'. */
      output_string (f, (inside_flag ? "  int *" : "static int *"));
      output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (f, ";\n");
      /* Definition of `yyfirst_char_ptr'. */
      output_string (f, (inside_flag ? "  int *" : "static int *"));
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, ";\n");
      /* Definition of `yylook_ahead_attribute'. */
      output_string (f, (inside_flag ? "  " : "static "));
      output_string (f, YYSTYPE_MACRO_NAME);
      output_string (f, " *");
      output_string (f, YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
      output_string (f, ";\n");
    }
  /* Definition of `yynerrs'. */
  output_string (f, (inside_flag ? "  int " : "static int "));
  output_string (f, YYNERRS_VARIABLE_NAME);
  output_string (f, ";      /* fixed syntactic errors number */\n");
}

static void
output_state_or_attribute_stack_expansion_function_title (FILE *f,
							  int state_flag,
							  int in_class_flag)
{
  assert (!in_class_flag || cpp_flag);
  if (!cpp_flag)
    output_string (f, "static ");
  output_string (f, "int ");
  if (cpp_flag && !in_class_flag)
    {
      output_yyparser_class_name (f);
      output_string (f, "::");
    }
  output_string (f, (state_flag
		     ? YYEXPAND_STATES_STACK_FUNCTION_NAME
		     : YYEXPAND_ATTRIBUTES_STACK_FUNCTION_NAME));
  output_string (f, " (");
  output_string (f, (state_flag ? "int" : YYSTYPE_MACRO_NAME));
  output_string (f, " **start, ");
  output_string (f, (state_flag ? "int" : YYSTYPE_MACRO_NAME));
  output_string (f, " **end, ");
  output_string (f, (state_flag ? "int" : YYSTYPE_MACRO_NAME));
  output_string (f, " **top)");
}

static void
output_class_start (FILE *f)
{
  assert (cpp_flag);
  output_string (f, "class ");
  output_yyparser_class_name (f);
  output_string (f, "{\n");
  if (IR_scanner_flag (description))
    output_inside_outside_definitions (f, TRUE);
  if (expand_flag)
    {
      output_string (f, "  ");
      output_state_or_attribute_stack_expansion_function_title (f, TRUE, TRUE);
      output_string (f, ";\n  ");
      output_state_or_attribute_stack_expansion_function_title (f,
								FALSE, TRUE);
      output_string (f, ";\n");
    }
  output_string (f, "\npublic:\n");
}

static void output_yylex_start_title (FILE *f, int inside_class);

static void
output_class_finish (FILE *f)
{
  assert (cpp_flag);
  output_string (f, "  virtual int ");
  output_yylex_function_name (f);
  output_string (f, " (void) = 0;\n");
  output_string (f, "  virtual void ");
  output_yyerror_function_name (f);
  output_string (f, " (const char *message) = 0;\n");
  output_string (f, "  ");
  output_yyparse_title (f, TRUE);
  output_string (f, ";\n");
  /* Constructor */
  output_string (f, "  ");
  if (IR_scanner_flag (description))
    {
      output_yylex_start_title (f, TRUE);
      output_string (f, ";\n");
    }
  else
    {
      output_yyparser_class_name (f);
      output_string (f, " (void) {}\n");
    }
  /* Destructor */
  output_string (f, "  virtual ~");
  output_yyparser_class_name (f);
  output_string (f, " (void)");
  if (IR_scanner_flag (description)
      && (real_look_ahead_number > 2 || !yacc_error_recovery_flag))
    output_string (f, ";\n");
  else
    output_string (f, "  {}\n");
  output_string (f, "};\n\n");
}

static void
output_external_definitions (void)
{
  /* Definition of `YYSTYPE'. */
  output_yystype_definition (output_implementation_file);
  if (define_flag)
    output_yystype_definition (output_interface_file);
  output_token_definitions (output_implementation_file);
  if (define_flag)
    output_token_definitions (output_interface_file);
  if (cpp_flag)
    {
      output_class_start (output_implementation_file);
      if (define_flag)
        output_class_start (output_interface_file);
    }
  /* Definition of `yylval'. */
  if (cpp_flag)
    output_string (output_implementation_file, "  ");
  output_yylval_definition (output_implementation_file);
  if (define_flag)
    {
      /* Definition of `yylval'. */
      if (cpp_flag)
        output_string (output_interface_file, "  ");
      else
        output_string (output_interface_file, "extern ");
      output_yylval_definition (output_interface_file);
    }
  /* Definition of `yychar'. */
  if (cpp_flag)
    output_string (output_implementation_file, "  ");
  output_yychar_definition (output_implementation_file);
  if (define_flag)
    {
      if (cpp_flag)
        output_string (output_interface_file, "  ");
      else
        output_string (output_interface_file, "extern ");
      output_yychar_definition (output_interface_file);
    }
  /* Definition of `yydebug'. */
  if (cpp_flag)
    output_string (output_implementation_file, "  ");
  output_yydebug_definition (output_implementation_file);
  if (define_flag)
    {
      if (cpp_flag)
        output_string (output_interface_file, "  ");
      else
        output_string (output_interface_file, "extern ");
      output_yydebug_definition (output_interface_file);
    }
  if (cpp_flag)
    {
      output_class_finish (output_implementation_file);
      if (define_flag)
        output_class_finish (output_interface_file);
    }
}

static void
output_yylex_start_title (FILE *f, int inside_class)
{
  assert (cpp_flag || !inside_class);
  if (!cpp_flag)
    output_string (f, "void ");
  if (!inside_class && cpp_flag)
    {
      output_yyparser_class_name (f);
      output_string (f, "::");
    }
  output_yylex_start_function_name (f);
  if (cpp_flag)
    output_string (f, " (int &");
  else
    output_string (f, " (int *");
  output_string (f, YYSCANNER_CONSTRUCTOR_ERROR_FLAG_NAME);
  output_string (f, ")");
}

static void
output_yylex_finish_title (FILE *f, int inside_class)
{
  assert (cpp_flag || !inside_class);
  if (!cpp_flag)
    output_string (f, "void ");
  if (!inside_class && cpp_flag)
    {
      output_yyparser_class_name (f);
      output_string (f, "::");
    }
  if (cpp_flag)
    output_string (f, "~");
  output_yylex_finish_function_name (f);
  output_string (f, " (void)");
}


#define YYERRLAB_LABEL_NAME\
     (IR_scanner_flag (description) ? "yyserrlab" : "yyerrlab")
#define YYSTATE_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysstate" : "yystate")
#define YYPREV_CHAR_VARIABLE_NAME  \
     (IR_scanner_flag (description) ? "yysprev_char" : "yyprev_char")
#define YYCHAR1_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yyschar1" : "yychar1")
#define YYTEMP_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yystemp" : "yytemp")
#define YYTEMP1_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yystemp1" : "yytemp1")
#define YYTEMP2_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yystemp2" : "yytemp2")
#define YYSTATES_TOP_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysstates_top" : "yystates_top")
#define YYATTRIBUTES_TOP_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysattributes_top" : "yyattributes_top")
#define YYSAVED_LVAL_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yyssaved_lval" : "yysaved_lval")
#define YYVAL_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysval" : "yyval")
#define YYSTATES_END_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysstates_end" : "yystates_end")
#define YYATTRIBUTES_END_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yysattributes_end" : "yyattributes_end")
#define YYERR_STATUS_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yyserr_status" : "yyerr_status")
#define YYERR_STATES_BOUND_VARIABLE_NAME\
     (IR_scanner_flag (description)\
      ? "yyserr_states_bound" : "yyerr_states_bound")
#define YYERR_NEW_TRY_VARIABLE_NAME\
     (IR_scanner_flag (description) ? "yyserr_new_try" : "yyerr_new_try")
#define YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME\
     (IR_scanner_flag (description) \
      ? "yyserr_look_ahead_chars" : "yyerr_look_ahead_chars")
#define YYERR_POPPED_ERROR_STATES_VARIABLE_NAME\
     (IR_scanner_flag (description)\
      ? "yyserr_popped_error_states" : "yyerr_popped_error_states")


static void
output_look_ahead_arrays_length (FILE *f)
{
  assert (real_look_ahead_number > 2 || !yacc_error_recovery_flag
	  || IR_back_tracking_exists (description));
  output_string (f, "(");
  output_decimal_number (f,
                         real_look_ahead_number
                         - (yacc_error_recovery_flag ? 1 : 0), 0);
  if (!yacc_error_recovery_flag)
    {
      output_string (f, " + ");
      output_string (f, YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (f, " + ");
      output_string (f, YYERR_RECOVERY_MATCHES_MACRO_NAME);
    }
  output_string (f, ")");
}


static void
output_definitions_outside_yyparse (void)
{
  /* Definition of YYALLOC. */
  output_string (output_implementation_file, "#ifndef  ");
  output_string (output_implementation_file, YYALLOC_MACRO_NAME);
  if (expand_flag)
    output_string
      (output_implementation_file,
       "\n/* Initial state & attribute stacks size (in elems). */");
  output_string (output_implementation_file, "\n#define  ");
  output_string (output_implementation_file, YYALLOC_MACRO_NAME);
  if (cpp_flag)
    output_string (output_implementation_file, "(size)  ::malloc (size)\n");
  else
    output_string (output_implementation_file, "(size)  malloc (size)\n");
  output_string (output_implementation_file, "#endif\n\n");
  /* Definition of YYREALLOC. */
  output_string (output_implementation_file, "#ifndef  ");
  output_string (output_implementation_file, YYREALLOC_MACRO_NAME);
  if (expand_flag)
    output_string
      (output_implementation_file,
       "\n/* Initial state & attribute stacks size (in elems). */");
  output_string (output_implementation_file, "\n#define  ");
  output_string (output_implementation_file, YYREALLOC_MACRO_NAME);
  if (cpp_flag)
    output_string (output_implementation_file,
		   "(ptr, size) ::realloc (ptr, size)\n");
  else
    output_string (output_implementation_file,
		   "(ptr, size)  realloc (ptr, size)\n");
  output_string (output_implementation_file, "#endif\n\n");
  /* Definition of YYFREE. */
  output_string (output_implementation_file, "#ifndef  ");
  output_string (output_implementation_file, YYFREE_MACRO_NAME);
  if (expand_flag)
    output_string
      (output_implementation_file,
       "\n/* Initial state & attribute stacks size (in elems). */");
  output_string (output_implementation_file, "\n#define  ");
  output_string (output_implementation_file, YYFREE_MACRO_NAME);
  if (cpp_flag)
    output_string (output_implementation_file, "(ptr)  ::free (ptr)\n");
  else
    output_string (output_implementation_file, "(ptr)  free (ptr)\n");
  output_string (output_implementation_file, "#endif\n\n");
  /* Definition of `YYSTACK_SIZE' */
  output_string (output_implementation_file, "#ifndef  ");
  output_string (output_implementation_file, YYSTACK_SIZE_MACRO_NAME);
  if (expand_flag)
    output_string
      (output_implementation_file,
       "\n/* Initial state & attribute stacks size (in elems). */");
  output_string (output_implementation_file, "\n#define  ");
  output_string (output_implementation_file, YYSTACK_SIZE_MACRO_NAME);
  output_string (output_implementation_file, "  500\n");
  output_string (output_implementation_file, "#endif\n\n");
  /* Check value of `YYSTACK_SIZE' */
  output_string (output_implementation_file, "#if  ");
  output_string (output_implementation_file,
                 YYSTACK_SIZE_MACRO_NAME);
  output_string (output_implementation_file, " <= 0\n#undef  ");
  output_string (output_implementation_file,
                 YYSTACK_SIZE_MACRO_NAME);
  output_string (output_implementation_file, "\n#define  ");
  output_string (output_implementation_file,
                 YYSTACK_SIZE_MACRO_NAME);
  output_string (output_implementation_file, "  50\n");
  output_string (output_implementation_file, "#endif\n\n");
  if (expand_flag)
    {
      /* Definition of `YYMAX_STACK_SIZE' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file, YYMAX_STACK_SIZE_MACRO_NAME);
      output_string
	(output_implementation_file,
	 "\n/* Max. state & attribute stacks size (in elems). */\n");
      output_string (output_implementation_file, "#define  ");
      output_string (output_implementation_file, YYMAX_STACK_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  5000\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Check value of `YYMAX_STACK_SIZE' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
		     YYMAX_STACK_SIZE_MACRO_NAME);
      output_string (output_implementation_file, " <= 0\n#undef  ");
      output_string (output_implementation_file,
		     YYMAX_STACK_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
		     YYMAX_STACK_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  100\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Definition of `YYSTACK_EXPAND_SIZE' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file,
		     YYSTACK_EXPAND_SIZE_MACRO_NAME);
      output_string
	(output_implementation_file,
	 "\n/* Expansion step of state & attr. stacks size (in elems). */\n");
      output_string (output_implementation_file, "#define  ");
      output_string (output_implementation_file,
		     YYSTACK_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  500\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Check value of `YYSTACK_EXPAND_SIZE' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
		     YYSTACK_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, " <= 0\n#undef  ");
      output_string (output_implementation_file,
		     YYSTACK_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
		     YYSTACK_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  10\n");
      output_string (output_implementation_file, "#endif\n\n");
    }
  /* `#define YYEMPTY  -2' */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
  output_string (output_implementation_file, "  (-2)");
  output_string (output_implementation_file, "\n\n");
  /* `#define YYEOF  0' or `#define YYEOF  -1' */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYEOF_MACRO_NAME);
  if (IR_scanner_flag (description))
    output_string (output_implementation_file, "  -1");
  else
    output_string (output_implementation_file, "  0");
  output_string (output_implementation_file, "\n\n");
  /* `#define yyclearin  do {if (yychar != YYEMPTY) yyprev_char = yychar; yychar = YYEMPTY;} while (0)' */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYCLEARIN_MACRO_NAME);
  output_string (output_implementation_file, " do {if (");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, " != ");
  output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
  output_string (output_implementation_file, ") ");
  output_string (output_implementation_file, YYPREV_CHAR_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, "; ");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
  output_string (output_implementation_file, ";} while (0)\n\n");
  if (!yacc_error_recovery_flag)
    {
      /* `#define yydeeper_error_try' */
      output_string (output_implementation_file, "#define ");
      output_string (output_implementation_file,
                     YYDEEPER_ERROR_TRY_MACRO_NAME);
      output_string (output_implementation_file, "  (");
      output_string (output_implementation_file, YYERR_NEW_TRY_VARIABLE_NAME);
      output_string (output_implementation_file, " = 1)\n\n");
    }
  /* `#define YYABORT  goto yyabort'   for parser
     `#define YYSABORT  goto yysabort' for scanner. */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYABORT_MACRO_NAME);
  output_string (output_implementation_file, " goto ");
  output_string (output_implementation_file, YYABORT_LABEL_NAME);
  output_string (output_implementation_file, "\n\n");
  /* `#define YYACCEPT  goto yyaccept' */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYACCEPT_MACRO_NAME);
  output_string (output_implementation_file, " goto ");
  output_string (output_implementation_file, YYACCEPT_LABEL_NAME);
  output_string (output_implementation_file, "\n\n");
  /* Definition of `YYERR_RECOVERY_MATCHES' */
  output_string (output_implementation_file, "#ifndef  ");
  output_string (output_implementation_file,
                 YYERR_RECOVERY_MATCHES_MACRO_NAME);
  output_string (output_implementation_file, "\n#define  ");
  output_string (output_implementation_file,
                 YYERR_RECOVERY_MATCHES_MACRO_NAME);
  output_string (output_implementation_file, "  3\n");
  output_string (output_implementation_file, "#endif\n\n");
  /* Check value of `YYERR_RECOVERY_MATCHES' */
  output_string (output_implementation_file, "#if  ");
  output_string (output_implementation_file,
                 YYERR_RECOVERY_MATCHES_MACRO_NAME);
  output_string (output_implementation_file, " <= 0\n#undef  ");
  output_string (output_implementation_file,
                 YYERR_RECOVERY_MATCHES_MACRO_NAME);
  output_string (output_implementation_file, "\n#define  ");
  output_string (output_implementation_file,
                 YYERR_RECOVERY_MATCHES_MACRO_NAME);
  output_string (output_implementation_file, "  1\n");
  output_string (output_implementation_file, "#endif\n\n");
  if (!yacc_error_recovery_flag)
    {
      /* Definition of `YYERR_MAX_LOOK_AHEAD_CHARS' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file,
                     YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (output_implementation_file, "  7\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Check value of `YYERR_MAX_LOOK_AHEAD_CHARS' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
                     YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (output_implementation_file, " <= 0\n#undef  ");
      output_string (output_implementation_file,
                     YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (output_implementation_file, "  1\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Definition of `YYERR_LOOK_AHEAD_INCREMENT' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file,
                     YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME);
      output_string (output_implementation_file, "  3\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Check value of `YYERR_LOOK_AHEAD_INCREMENT' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
                     YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME);
      output_string (output_implementation_file, " < 0\n#undef  ");
      output_string (output_implementation_file,
                     YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME);
      output_string (output_implementation_file, "  0\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Definition of `YYERR_POPPED_ERROR_STATES' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file,
                     YYERR_POPPED_ERROR_STATES_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYERR_POPPED_ERROR_STATES_MACRO_NAME);
      output_string (output_implementation_file, "  2\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Check value of `YYERR_POPPED_ERROR_STATES' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
                     YYERR_POPPED_ERROR_STATES_MACRO_NAME);
      output_string (output_implementation_file, " < 0\n#undef  ");
      output_string (output_implementation_file,
                     YYERR_POPPED_ERROR_STATES_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYERR_POPPED_ERROR_STATES_MACRO_NAME);
      output_string (output_implementation_file, "  0\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Definition of `YYERR_DISCARDED_CHARS' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file,
                     YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (output_implementation_file, "  3\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Check value of `YYERR_DISCARDED_CHARS' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
                     YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (output_implementation_file, " < 0\n#undef  ");
      output_string (output_implementation_file,
                     YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (output_implementation_file, "  0\n");
      output_string (output_implementation_file, "#endif\n\n");
    }
  if (real_look_ahead_number > 2 || !yacc_error_recovery_flag
      || IR_back_tracking_exists (description))
    {
      /* Definition of `YYLOOK_AHEAD_SIZE' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  ");
      output_look_ahead_arrays_length (output_implementation_file);
      output_string (output_implementation_file, "\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Check value of `YYLOOK_AHEAD_SIZE' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, " < ");
      output_look_ahead_arrays_length (output_implementation_file);
      output_string (output_implementation_file, "\n#undef  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  ");
      output_look_ahead_arrays_length (output_implementation_file);
      output_string (output_implementation_file, "\n#endif\n\n");
    }
  if (IR_back_tracking_exists (description))
    {
      /* Definition of `YYMAX_LOOK_AHEAD_SIZE' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file,
                     YYMAX_LOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYMAX_LOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  (50*");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, ")\n#endif\n\n");
      /* Check value of `YYMAX_LOOK_AHEAD_SIZE' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
                     YYMAX_LOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, " < ");
      output_string (output_implementation_file, YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#undef  ");
      output_string (output_implementation_file,
                     YYMAX_LOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYMAX_LOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n");
      output_string (output_implementation_file, "#endif\n\n");
      /* Definition of `YYLOOK_AHEAD_EXPAND_SIZE' */
      output_string (output_implementation_file, "#ifndef  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  (5*");
      output_string (output_implementation_file, YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, ")\n#endif\n\n");
      /* Check value of `YYLOOK_AHEAD_EXPAND_SIZE' */
      output_string (output_implementation_file, "#if  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, " <= 0\n#undef  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "\n#define  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_EXPAND_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "  1\n#endif\n\n");
    }
  /* `#define YYERROR  goto yyerrlab' */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYERROR_MACRO_NAME);
  output_string (output_implementation_file, "  goto ");
  output_string (output_implementation_file, YYERRLAB_LABEL_NAME);
  output_string (output_implementation_file, "\n\n");
  /* `#define yyerrok  yyerr_status = (-1)' */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYERROK_MACRO_NAME);
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYERR_STATUS_VARIABLE_NAME);
  output_string (output_implementation_file, " = (-1)\n\n");
  /* `#define YYRECOVERING()  (yyerr_status > 0)' */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYRECOVERING_MACRO_NAME);
  output_string (output_implementation_file, "()  (");
  output_string (output_implementation_file, YYERR_STATUS_VARIABLE_NAME);
  output_string (output_implementation_file, " > 0)\n\n");
  /* `#define YYTOKEN_NAME(code)\
     ((code) < 0 || (code) > YYLAST_TOKEN_CODE || yytname [code] == 0\
     ? "illegal-token" : yytname [code])' */
  output_string (output_implementation_file, "#define ");
  output_string (output_implementation_file, YYTOKEN_NAME_MACRO_NAME);
  output_string (output_implementation_file,
                 "(code)\\\n  ((code) < 0 || (code) > ");
  output_string (output_implementation_file, LAST_TOKEN_VALUE_MACRO_NAME);
  output_string (output_implementation_file, " || ");
  output_string (output_implementation_file, TOKEN_NAMES_VECTOR_NAME);
  output_string (output_implementation_file, " [code] == 0");
  output_string (output_implementation_file, "\\\n");
  output_string (output_implementation_file, "   ? \"illegal-code\" : ");
  output_string (output_implementation_file, TOKEN_NAMES_VECTOR_NAME);
  output_string (output_implementation_file, " [code])\n\n");
  if (IR_scanner_flag (description) && !cpp_flag)
    {
      output_inside_outside_definitions (output_implementation_file, FALSE);
      output_string (output_implementation_file, "\n");
    }
}

static void
output_state_or_attribute_stack_expansion_function (int state_flag)
{
  FILE *f = output_implementation_file;

  output_state_or_attribute_stack_expansion_function_title (f,
							    state_flag, FALSE);
  output_string (f, "\n{\n");
  output_string (f, "  int size = *end - *start + 1;\n");
  output_string (f, "  int new_size = size + ");
  output_string (f, YYSTACK_EXPAND_SIZE_MACRO_NAME);
  output_string (f, ";\n  ");
  output_string (f, (state_flag ? "int" : YYSTYPE_MACRO_NAME));
  output_string (f, " *new_start;\n\n");
  output_string (f, "#if ");
  output_string (f, YYDEBUG_MACRO_NAME);
  output_string (f, " != 0\n");
  output_string (f, "  if (");
  output_yydebug_variable_name (f);
  output_string (f, ")\n    fprintf (stderr, \"Expanding ");
  output_string (f, (state_flag ? "states": "attributes"));
  output_string
    (f, " stack (old size - %d, new size - %d)\\n\", size, new_size);\n");
  output_string (f, "#endif\n");
  output_string (f, "  if (new_size > ");
  output_string (f, YYMAX_STACK_SIZE_MACRO_NAME);
  output_string (f, ")\n");
  output_string (f, "    {\n");
  output_string (f, "      ");
  output_yyerror_function_name (f);
  output_string (f, (state_flag ? " (\"states": " (\"attributes"));
  output_string (f, " stack is overfull\");\n");
  output_string (f, "      return 1;\n");
  output_string (f, "    }\n");
  output_string (f, "  new_start = (");
  output_string (f, (state_flag ? "int" : YYSTYPE_MACRO_NAME));
  output_string (f, "*) ");
  output_string (f, YYREALLOC_MACRO_NAME);
  output_string (f, " (*start, new_size * sizeof (");
  output_string (f, (state_flag ? "int" : YYSTYPE_MACRO_NAME));
  output_string (f, "));\n");
  output_string (f, "  if (new_start == NULL)\n");
  output_string (f, "    {\n");
  output_string (f, "      ");
  output_yyerror_function_name (f);
  output_string (f, " (\"no memory for ");
  output_string (f, (state_flag ? "states": "attributes"));
  output_string (f, " stack expansion\");\n");
  output_string (f, "      return 1;\n");
  output_string (f, "    }\n");
  output_string (f, "  *end = new_start + (new_size - 1);\n");
  output_string (f, "  *top = *top + (new_start - *start);\n");
  output_string (f, "  *start = new_start;\n");
  output_string (f, "  return 0;\n");
  output_string (f, "}\n\n");
}


static void
output_action_char (char ch)
{
  output_char (ch, output_implementation_file);
}

/* The following variable value is reduce LR-situation whose action is
   being output now. */

static IR_node_t output_action_reduce_LR_situation;

static void
output_action_attribute (IR_node_t canonical_rule,
                         position_t attribute_position,
                         const char *tag_name, const char *attribute_name)
{
  int attribute_number;
  int current_attribute_number;
  IR_node_t bound_right_hand_side_element;
  IR_node_t current_right_hand_side_element;
  IR_node_t original_canonical_rule;
  IR_node_t single_definition;

  assert (IR_canonical_rule (IR_element_after_dot
                             (output_action_reduce_LR_situation))
          == canonical_rule);
  if (strcmp (attribute_name, "$") == 0)
    {
      output_string (output_implementation_file, YYVAL_VARIABLE_NAME);
      single_definition = IR_left_hand_side (canonical_rule);
    }
  else
    {
      bound_right_hand_side_element
        = IR_original_code_insertion_place (canonical_rule);
      if (bound_right_hand_side_element != NULL)
        original_canonical_rule
          = IR_canonical_rule (bound_right_hand_side_element);
      else
        original_canonical_rule = canonical_rule;
      if (isdigit (*attribute_name) || *attribute_name == '-')
        attribute_number = atoi (attribute_name);
      else
        attribute_number
          = attribute_name_to_attribute_number
            (attribute_name, original_canonical_rule,
             bound_right_hand_side_element);
      if (attribute_number <= 0)
        single_definition = NULL;
      else
        {
          for (current_attribute_number = attribute_number,
               current_right_hand_side_element
               = IR_right_hand_side (original_canonical_rule);
               current_right_hand_side_element != bound_right_hand_side_element
               && current_attribute_number != 1;
               current_right_hand_side_element
               = IR_next_right_hand_side_element
               (current_right_hand_side_element))
            current_attribute_number--;
          assert (current_right_hand_side_element
                  != bound_right_hand_side_element);
          single_definition
            = IR_element_itself (current_right_hand_side_element);
        }
      output_string (output_implementation_file,
                     YYATTRIBUTES_TOP_VARIABLE_NAME);
      output_string (output_implementation_file, " [");
      output_decimal_number
        (output_implementation_file,
         - (pushed_LR_sets_or_attributes_number_on_path
            (IR_LR_set (output_action_reduce_LR_situation),
             canonical_rule_right_hand_side_prefix_length
             (original_canonical_rule, bound_right_hand_side_element)
             - attribute_number, TRUE)), 0);
      output_char (']', output_implementation_file);
    }
  if (tag_name != NULL && *tag_name != '\0')
    {
      output_char ('.', output_implementation_file);
      output_string (output_implementation_file, tag_name);
    }
  else if (single_definition != NULL && IR_type (single_definition) != NULL)
    {
      output_char ('.', output_implementation_file);
      output_identifier_or_literal (output_implementation_file,
                                    IR_type (single_definition), FALSE);
    }
}



static void
output_attributes_stack_check (int number, const char *indent)
{
  if (number <= 0)
    return;
  /* if (yyattributes_top >= yyattributes_end - <number-1>
------------------ expand_flag ---------------------------------
         && yyexpand_attributes_stack (&yyattributes, &yyattributes_end, &yyattributes_top)
----------------------------------------------------------------
        )
     YYABORT; */
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "if (");
  output_string (output_implementation_file,
                 YYATTRIBUTES_TOP_VARIABLE_NAME);
  output_string (output_implementation_file, " >= ");
  output_string (output_implementation_file,
                 YYATTRIBUTES_END_VARIABLE_NAME);
  if (number != 1)
    {
      output_string (output_implementation_file, " - ");
      output_decimal_number (output_implementation_file, number - 1, 0);
    }
  if (expand_flag)
    {
      output_string (output_implementation_file, "\n    ");
      output_string (output_implementation_file, indent);
      output_string (output_implementation_file, "&& ");
      output_string (output_implementation_file,
		     YYEXPAND_ATTRIBUTES_STACK_FUNCTION_NAME);
      output_string (output_implementation_file, "(&");
      output_string (output_implementation_file, YYATTRIBUTES_VARIABLE_NAME);
      output_string (output_implementation_file, ", &");
      output_string (output_implementation_file,
		     YYATTRIBUTES_END_VARIABLE_NAME);
      output_string (output_implementation_file, ", &");
      output_string (output_implementation_file,
		     YYATTRIBUTES_TOP_VARIABLE_NAME);
      output_string (output_implementation_file, ")");
    }
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYABORT_MACRO_NAME);
  output_string (output_implementation_file, ";\n");
}

static void
output_states_stack_check (int number, const char *indent)
{
  if (number <= 0)
    return;
  /* if (yystates_top >= yystates_end - <number - 1>
------------------ expand_flag ---------------------------------
         && yyexpand_states_stack (&yystates, &yystates_end, &yystates_top)
----------------------------------------------------------------
        )
     YYABORT; */
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "if (");
  output_string (output_implementation_file, YYSTATES_TOP_VARIABLE_NAME);
  output_string (output_implementation_file, " >= ");
  output_string (output_implementation_file, YYSTATES_END_VARIABLE_NAME);
  if (number != 1)
    {
      output_string (output_implementation_file, " - ");
      output_decimal_number (output_implementation_file, number - 1, 0);
    }
  if (expand_flag)
    {
      output_string (output_implementation_file, "\n    ");
      output_string (output_implementation_file, indent);
      output_string (output_implementation_file, "&& ");
      output_string (output_implementation_file,
		     YYEXPAND_STATES_STACK_FUNCTION_NAME);
      output_string (output_implementation_file, "(&");
      output_string (output_implementation_file, YYSTATES_VARIABLE_NAME);
      output_string (output_implementation_file, ", &");
      output_string (output_implementation_file, YYSTATES_END_VARIABLE_NAME);
      output_string (output_implementation_file, ", &");
      output_string (output_implementation_file, YYSTATES_TOP_VARIABLE_NAME);
      output_string (output_implementation_file, ")");
    }
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYABORT_MACRO_NAME);
  output_string (output_implementation_file, ";\n");
}

static void
output_state_pushing (int check_states_stack, const char *indent)
{
  if (check_states_stack)
    output_states_stack_check (1, indent);
  /* (*++yystates_top) = yystate; */
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "(*++");
  output_string (output_implementation_file, YYSTATES_TOP_VARIABLE_NAME);
  output_string (output_implementation_file, ") = ");
  output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
}

static void
output_attribute_pushing (int check_attributes_stack,
                          int terminal_flag, const char *indent)
{
  if (check_attributes_stack)
    output_attributes_stack_check (1, indent);
  /* (*++yyattributes_top) = yylval;
     or
     (*++yyattributes_top) = yyval; */
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, "(*++");
  output_string (output_implementation_file, YYATTRIBUTES_TOP_VARIABLE_NAME);
  output_string (output_implementation_file, ") = ");
  if (terminal_flag)
    output_yylval_variable_name (output_implementation_file);
  else
    output_string (output_implementation_file, YYVAL_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
}

static void
output_pushing (IR_node_t LR_set, int check_states_stack,
                int check_attributes_stack, int terminal_flag)
{
  if (IR_it_is_pushed_LR_set (LR_set))
    output_state_pushing (check_states_stack, "          ");
  if (IR_attribute_is_used (LR_set))
    output_attribute_pushing (check_attributes_stack, terminal_flag,
			      "          ");
}

static void
output_attributes_stack_top_decrement (int number)
{
  /* yyattributes_top -= <needed number>;
     or yyattributes_top--;
     or nothing. */
  if (number == 0)
    return;
  output_string (output_implementation_file, "          ");
  output_string (output_implementation_file, YYATTRIBUTES_TOP_VARIABLE_NAME);
  if (number == 1)
    output_string (output_implementation_file, "--");
  else
    {
      output_string (output_implementation_file, " -= ");
      output_decimal_number (output_implementation_file, number, 0);
    }
  output_string (output_implementation_file, ";\n");
}

static void
output_states_stack_top_decrement (int number)
{
  /* yystates_top -= <needed number>;
     or yystates_top--;
     or nothing. */
  if (number == 0)
    return;
  output_string (output_implementation_file, "          ");
  output_string (output_implementation_file, YYSTATES_TOP_VARIABLE_NAME);
  if (number == 1)
    output_string (output_implementation_file, "--;\n");
  else
    {
      assert (number > 0);
      output_string (output_implementation_file, " -= ");
      output_decimal_number (output_implementation_file, number, 0);
      output_string (output_implementation_file, ";\n");
    }
}

/* The following variables are used for transfering parameters into
   function `output_pop_shift_action_attribute' called by
   `process_canonical_rule_action'.  The value of
   current_pop_shift_action_rule_length can be change in function
   `output_pop_shift_action_attribute'. */

static IR_node_t current_pop_shift_action_LR_set;
static int current_pop_shift_action_rule_length;

static void
output_pop_shift_action_attribute (IR_node_t canonical_rule,
                                   position_t attribute_position,
                                   const char *tag_name,
                                   const char *attribute_name)
{
  int attribute_number;
  int current_attribute_number;
  IR_node_t current_right_hand_side_element;
  IR_node_t bound_right_hand_side_element;
  IR_node_t original_canonical_rule;
  IR_node_t single_definition;
  int rule_length;
  int stack_displacement;

  if (strcmp (attribute_name, "$") == 0)
    {
      output_string (output_implementation_file, YYVAL_VARIABLE_NAME);
      single_definition = IR_left_hand_side (canonical_rule);
    }
  else
    {
      bound_right_hand_side_element
        = IR_original_code_insertion_place (canonical_rule);
      if (bound_right_hand_side_element != NULL)
        {
          original_canonical_rule
            = IR_canonical_rule (bound_right_hand_side_element);
          current_pop_shift_action_rule_length
            = canonical_rule_right_hand_side_prefix_length
              (original_canonical_rule, bound_right_hand_side_element);
        }
      else
        original_canonical_rule = canonical_rule;
      rule_length = (canonical_rule_right_hand_side_prefix_length
                     (original_canonical_rule, NULL));
      if (isdigit (*attribute_name) || *attribute_name == '-')
        attribute_number = atoi (attribute_name);
      else
        attribute_number
          = attribute_name_to_attribute_number
            (attribute_name, original_canonical_rule,
             bound_right_hand_side_element);
      if (attribute_number <= 0)
        single_definition = NULL;
      else
        {
          for (current_attribute_number = attribute_number,
               current_right_hand_side_element
               = IR_right_hand_side (original_canonical_rule);
               current_right_hand_side_element != bound_right_hand_side_element
               && current_attribute_number != 1;
               current_right_hand_side_element
               = IR_next_right_hand_side_element
                 (current_right_hand_side_element))
            current_attribute_number--;
          assert (current_right_hand_side_element
                  != bound_right_hand_side_element);
          single_definition
            = IR_element_itself (current_right_hand_side_element);
        }
      output_string (output_implementation_file,
                     YYATTRIBUTES_TOP_VARIABLE_NAME);
      output_string (output_implementation_file, " [");
      stack_displacement
        = 1 - (pushed_LR_sets_or_attributes_number_on_path
               (current_pop_shift_action_LR_set,
                current_pop_shift_action_rule_length - attribute_number + 1,
                TRUE));
      assert (stack_displacement <= 0);
      output_decimal_number
        (output_implementation_file, stack_displacement, 0);
      output_string (output_implementation_file, "]");
    }
  if (tag_name != NULL && *tag_name != '\0')
    {
      output_char ('.', output_implementation_file);
      output_string (output_implementation_file, tag_name);
    }
  else if (single_definition != NULL && IR_type (single_definition) != NULL)
    {
      output_char ('.', output_implementation_file);
      output_identifier_or_literal (output_implementation_file,
                                    IR_type (single_definition), FALSE);
    }
}

static void
output_yyerr_status_decrement (const char *indent)
{
  /* yyerr_status--; */
  output_string (output_implementation_file, indent);
  output_string (output_implementation_file, YYERR_STATUS_VARIABLE_NAME);
  output_string (output_implementation_file, "--;\n");
}

static void
output_shift_pop_actions (IR_node_t regular_arc)
{
  IR_node_t current_LR_set;
  IR_node_t current_rule_list_element;
  IR_node_t canonical_rule;
  IR_node_t current_regular_arc;
  int rule_length;
  int states_stack_decrement;
  int max_states_stack_increment;
  int states_stack_displacement;
  int max_states_stack_displacement;
  int attributes_stack_decrement;
  int max_attributes_stack_increment;
  int attributes_stack_displacement;
  int max_attributes_stack_displacement;

  /* case <regular arc code>: */
  output_string (output_implementation_file, "        case ");
  output_decimal_number
    (output_implementation_file,
     first_pop_shift_action_value + IR_number_of_regular_arc (regular_arc),
     0);
  output_string (output_implementation_file, ":\n");
  current_LR_set = IR_LR_set (IR_from_LR_situation (regular_arc));
  output_string (output_implementation_file, "          /* ");
  if (IR_terminal_marking_arc (regular_arc) != NULL)
    {
      output_single_definition (output_implementation_file,
                                IR_terminal_marking_arc (regular_arc));
      output_string (output_implementation_file, ":\n");
    }
  else
    output_string (output_implementation_file, "\n");
  output_LR_set_situations (output_implementation_file, current_LR_set,
                            "             ");
  for (current_regular_arc
         = IR_next_equivalent_regular_arc (regular_arc);
       current_regular_arc != regular_arc;
       current_regular_arc
         = IR_next_equivalent_regular_arc (current_regular_arc))
    IR_set_LR_set_has_been_output_in_comment
      (IR_LR_set (IR_from_LR_situation (current_regular_arc)), FALSE);
  for (current_regular_arc
         = IR_next_equivalent_regular_arc (regular_arc);
       current_regular_arc != regular_arc;
       current_regular_arc
         = IR_next_equivalent_regular_arc (current_regular_arc))
    if (!IR_LR_set_has_been_output_in_comment
         (IR_LR_set (IR_from_LR_situation (current_regular_arc))))
      {
        output_string (output_implementation_file, "             or\n");
        output_LR_set_situations (output_implementation_file,
                                  IR_LR_set (IR_from_LR_situation
                                             (current_regular_arc)),
                                  "             ");
        IR_set_LR_set_has_been_output_in_comment
          (IR_LR_set (IR_from_LR_situation (current_regular_arc)), TRUE);
      }
  output_string (output_implementation_file, "           */\n");
  max_attributes_stack_increment
    = IR_max_attributes_stack_increment (regular_arc);
  max_states_stack_increment = IR_max_states_stack_increment (regular_arc);
  output_states_stack_check (max_states_stack_increment, "          ");
  output_attributes_stack_check (max_attributes_stack_increment, "          ");
  states_stack_displacement = 0;
  max_states_stack_displacement = 0;
  attributes_stack_displacement = 0;
  max_attributes_stack_displacement = 0;
  if (IR_terminal_marking_arc (regular_arc) != NULL)
    {
      current_LR_set = IR_goto_LR_set (IR_from_LR_situation (regular_arc));
      if (IR_attribute_is_used (current_LR_set))
        {
          /* Terminal shift: push attribute in stack and empty input char.
             *++yyattributes_top = yylval;
           */
	  if (IR_attribute_is_used (current_LR_set))
	    output_attribute_pushing (FALSE, TRUE, "          ");
          attributes_stack_displacement++;
          max_attributes_stack_displacement++;
        }
      /*
-------------- if scanner || ! yacc error recovery ----------------------
              yyprev_char = yychar;
-------------------------------------------------------------------------
              yychar = YYEMPTY;
              yyerr_status--;
       */
      if (IR_scanner_flag (description) || !yacc_error_recovery_flag)
        {
          output_string (output_implementation_file, "          ");
          output_string (output_implementation_file,
                         YYPREV_CHAR_VARIABLE_NAME);
          output_string (output_implementation_file, " = ");
          output_yychar_variable_name (output_implementation_file);
          output_string (output_implementation_file, ";\n");
        }
      output_string (output_implementation_file, "          ");
      output_yychar_variable_name (output_implementation_file);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
      output_string (output_implementation_file, ";\n");
      output_yyerr_status_decrement ("          ");
      if (IR_it_is_pushed_LR_set (current_LR_set)
          && IR_result_LR_set_will_be_on_the_stack (regular_arc))
        {
          /* (*++yystates_top) = <state_number>;
             or
             (*++yystates_top) = <final_state_value>;
             Because the final state never contains transition by error. */
          output_string (output_implementation_file, "          (*++");
          output_string (output_implementation_file,
                         YYSTATES_TOP_VARIABLE_NAME);
          output_string (output_implementation_file, ") = ");
          if (!IR_reachable_flag (current_LR_set))
            output_string (output_implementation_file,
                           FINAL_STATE_VALUE_MACRO_NAME);
          else
            output_decimal_number (output_implementation_file,
                                   IR_LR_set_order_number (current_LR_set), 0);
          output_string (output_implementation_file, ";\n");
          states_stack_displacement++;
          max_states_stack_displacement++;
        }
    }
  assert (IR_first_rule_list_element (regular_arc) != NULL);
  for (current_rule_list_element = IR_first_rule_list_element (regular_arc);
       current_rule_list_element != NULL;
       current_rule_list_element
         = IR_next_rule_list_element (current_rule_list_element))
    {
      canonical_rule = IR_canonical_rule (current_rule_list_element);
      rule_length
        = canonical_rule_right_hand_side_prefix_length (canonical_rule, NULL);
      if (IR_action (canonical_rule) != NULL)
        {
          output_line (output_implementation_file,
                       IR_position (IR_action (canonical_rule)).line_number,
                       IR_position (IR_action (canonical_rule)).file_name);
          output_char ('{', output_implementation_file);
          current_pop_shift_action_LR_set = current_LR_set;
          current_pop_shift_action_rule_length = rule_length;
          process_canonical_rule_action (canonical_rule, output_action_char,
                                         output_pop_shift_action_attribute);
          output_string (output_implementation_file, "}\n");
          output_current_line (output_implementation_file);
        }
      states_stack_decrement
        = pushed_LR_sets_or_attributes_number_on_path (current_LR_set,
                                                       rule_length, FALSE);
      states_stack_displacement -= states_stack_decrement;
      output_states_stack_top_decrement (states_stack_decrement);
      attributes_stack_decrement
        = pushed_LR_sets_or_attributes_number_on_path (current_LR_set,
                                                       rule_length, TRUE);
      attributes_stack_displacement -= attributes_stack_decrement;
      output_attributes_stack_top_decrement (attributes_stack_decrement);
      current_LR_set = get_the_single_LR_set_predecessor (current_LR_set,
                                                          rule_length);
      current_LR_set
        = goto_by_nonterminal (current_LR_set,
                               IR_left_hand_side (canonical_rule));
      if (IR_next_rule_list_element (current_rule_list_element) == NULL)
        {
          /* yystate = <state_number>;
             or
             yystate = <final_state_value>;
             Because the final state never contains transition by error. */
          output_string (output_implementation_file, "          ");
          output_string (output_implementation_file,
                         YYSTATE_VARIABLE_NAME);
          output_string (output_implementation_file, " = ");
          assert (IR_reachable_flag (current_LR_set));
          output_decimal_number (output_implementation_file,
                                 IR_LR_set_order_number (current_LR_set),
                                 0);
          output_string (output_implementation_file, ";\n");
        }
      if (IR_it_is_pushed_LR_set (current_LR_set)
          && IR_result_LR_set_will_be_on_the_stack (current_rule_list_element))
        {
          /* (*++yystates_top) = yystate;
             or
             (*++yystates_top) = <state_number>;
             or
             (*++yystates_top) = <final_state_value>;
             Because the final state never contains transition by error. */
          output_string (output_implementation_file, "          (*++");
          output_string (output_implementation_file,
                         YYSTATES_TOP_VARIABLE_NAME);
          output_string (output_implementation_file, ") = ");
          if (IR_next_rule_list_element (current_rule_list_element) == NULL)
            output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
          else
            {
              if (!IR_reachable_flag (current_LR_set))
                output_string (output_implementation_file,
                               FINAL_STATE_VALUE_MACRO_NAME);
              else
                output_decimal_number (output_implementation_file,
                                       IR_LR_set_order_number (current_LR_set),
                                       0);
            }
          output_string (output_implementation_file, ";\n");
          states_stack_displacement++;
          if (max_states_stack_displacement < states_stack_displacement)
            max_states_stack_displacement = states_stack_displacement;
        }
      if (IR_lhs_nonterm_attribute_is_used (current_rule_list_element))
        {
          assert (IR_attribute_is_used (current_LR_set));
	  if (IR_attribute_is_used (current_LR_set))
	    output_attribute_pushing (FALSE, FALSE, "          ");
          attributes_stack_displacement++;
          if (max_attributes_stack_displacement
              < attributes_stack_displacement)
            max_attributes_stack_displacement = attributes_stack_displacement;
        }
    }
  assert (max_states_stack_increment == max_states_stack_displacement);
  assert (max_attributes_stack_increment == max_attributes_stack_displacement);
  assert (current_LR_set == IR_to_LR_set (regular_arc));
  output_string (output_implementation_file, "          break;\n");
}

/* The following variable is used for searching for a LR-set
   predecessor (see function `get_a_LR_set_predecessor'). */

static IR_node_t a_LR_set_predecessor;

/* The following function is used for searching a LR-set predecessor
   of a LR-set (see function `get_a_LR_set_predecessor'). */

static int
fix_a_LR_set_predecessor (IR_node_t LR_set)
{
  a_LR_set_predecessor = LR_set;
  return TRUE;
}

/* The following function returns LR-set which is predecessor of given
   LR-set.  The distance between given LR-set and returned LR-set will
   be equal to `path-length'.  Only one such LR-set must be in
   LR-graph. */

static IR_node_t
get_a_LR_set_predecessor (IR_node_t LR_set, int path_length)
{
  assert (path_length >= 0);
  a_LR_set_predecessor = NULL;
  traverse_all_LR_set_predecessors (LR_set, path_length,
                                    fix_a_LR_set_predecessor);
  assert (a_LR_set_predecessor != NULL);
  return a_LR_set_predecessor;
}

static IR_node_t
get_a_target (IR_node_t start_LR_set, IR_node_t canonical_rule)
{
  return
    goto_by_nonterminal
    (get_a_LR_set_predecessor
     (start_LR_set,
      canonical_rule_right_hand_side_prefix_length (canonical_rule, NULL)),
     IR_left_hand_side (canonical_rule));
}

/*
#if YYDEBUG != 0
         if (yydebug)
           fprintf (stderr,
                    "Error recovery saving a token %d (%s)\n",
                    yychar, YYTOKEN_NAME (yychar));
#endif
*/

static void
output_debug_print_about_saving_token (FILE *f, const char *indent)
{
  output_string (f, "#if ");
  output_string (f, YYDEBUG_MACRO_NAME);
  output_string (f, " != 0\n");
  output_string (f, indent);
  output_string (f, "if (");
  output_yydebug_variable_name (f);
  output_string (f, ")\n");
  output_string (f, indent);
  output_string (f, "  fprintf (stderr,\n");
  output_string (f, indent);
  output_string (f, "           \"Error recovery saving token %d (%s)\\n\",\n");
  output_string (f, indent);
  output_string (f, "           ");
  output_yychar_variable_name (f);
  output_string (f, ", ");
  output_string (f, YYTOKEN_NAME_MACRO_NAME);
  output_string (f, " (");
  output_yychar_variable_name (f);
  output_string (f, "));\n#endif\n");
}

/*
   if (yyfirst_char_ptr(_1) >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
     yyfirst_char_ptr(_1) = yylook_ahead_char;
*/

static void
output_check_yyfirst_char_ptr (FILE *f, const char *indent, int flag_1)
{
  output_string (f, indent);
  output_string (f, "if (");
  output_string (f, (flag_1
		     ? YYFIRST_CHAR_PTR_1_VARIABLE_NAME
		     : YYFIRST_CHAR_PTR_VARIABLE_NAME));
  output_string (f, " >= ");
  output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
  output_string (f, " + ");
  output_string (f, YYLOOK_AHEAD_SIZE_MACRO_NAME);
  output_string (f, ")\n");
  output_string (f, indent);
  output_string (f, "  ");
  output_string (f, (flag_1
		     ? YYFIRST_CHAR_PTR_1_VARIABLE_NAME
		     : YYFIRST_CHAR_PTR_VARIABLE_NAME));
  output_string (f, " = ");
  output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
  output_string (f, ";\n");
}

/* 
   yylval = yylook_ahead_attribute [yyfirst_char_ptr - yylook_ahead_char];
   yychar = *yyfirst_char_ptr;
   *yyfirst_char_ptr++ = YYEMPTY;
   if (yyfirst_char_ptr >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
     yyfirst_char_ptr = yylook_ahead_char;
*/

static void
output_look_ahead_read_without_saving (FILE *f, const char *indent)
{
  output_string (f, indent);
  output_yylval_variable_name (f);
  output_string (f, " = ");
  output_string (f, YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
  output_string (f, " [");
  output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
  output_string (f, " - ");
  output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
  output_string (f, "];\n");
  output_string (f, indent);
  output_yychar_variable_name (f);
  output_string (f, " = ");
  output_string (f, "*");
  output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
  output_string (f, ";\n");
  output_string (f, indent);
  output_string (f, "*");
  output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
  output_string (f, "++ = ");
  output_string (f, YYEMPTY_MACRO_NAME);
  output_string (f, ";\n");
  output_check_yyfirst_char_ptr (f, indent, FALSE);
}

static void
output_switch (void)
{
  IR_node_t current_LR_core;
  IR_node_t current_LR_set;
  IR_double_link_t LR_set_reference;
  IR_node_t owner;
  IR_node_t current_LR_situation;
  IR_node_t regular_arc;
  IR_node_t canonical_rule;
  IR_node_t LR_set_target;
  IR_node_t last_LR_set;
  int rule_length;
  int popped_states_number;
  int popped_attributes_number;
  int push_state_flag;
  int push_attribute_flag;
  int first_shift_flag;
  int first_reduce_flag;
  int first_regular_arc_flag;
  int i;
  vlo_t reduce_LR_situations_vector;
  FILE *f = output_implementation_file;

  output_string (f, "      switch (");
  output_string (f, YYTEMP_VARIABLE_NAME);
  output_string (f, ")\n");
  output_string (f, "        {\n");
  output_string (f, "        case ");
  output_string (f, NO_ACTION_VALUE_MACRO_NAME);
  output_string (f, ":\n");
  /*    /* Here error processing and error recovery. * /
        if (yyerr_status <= 0)
          {
            yyerror ("syntax error"); or yyerror ("lexical error");
yyerrlab:
            ++yynerrs;
------- when regular optimization ------------------------
            if (!yypushed [yystate])
              {
                 if (yystates_top >= yystates_end
-------------- when expand_flag--------------------------------
                     && yyexpand_states_stack (&yystates, &yystates_end, &yystates_top)
---------------------------------------------------------------
                    )
                   YYABORT;
                 (*++yystates_top) == yystate;
               }
----------------------------------------------------------
-------------- if !yacc_error_recovery_flag --------------
             yyerr_states_bound = yystates_top - yystates + 1;
             yyerr_new_try = 0;
             yyerr_look_ahead_chars = YYERR_LOOK_AHEAD_INCREMENT;
             yyerr_popped_error_states = 0;
----------------------------------------------------------
          } */
  output_string
    (f, "          /* Here error processing and error recovery. */\n");
  output_string (f, "          if (");
  output_string (f, YYERR_STATUS_VARIABLE_NAME);
  output_string (f, " <= 0)\n            {\n");
  output_string (f, "              ");
  output_yyerror_function_name (f);
  if (IR_scanner_flag (description))
    output_string (f, " (\"lexical error\");\n");
  else
    output_string (f, " (\"syntax error\");\n");
  output_string (f, YYERRLAB_LABEL_NAME);
  output_string (f, ":\n              ++");
  output_string (f, YYNERRS_VARIABLE_NAME);
  output_string (f, ";\n");
  if (regular_optimization_flag)
    {
      output_string (f, "              if (!");
      output_string (f, PUSHED_STATE_FLAGS_VECTOR_NAME);
      output_string (f, " [");
      output_string (f, YYSTATE_VARIABLE_NAME);
      output_string (f, "])\n");
      output_string (f, "                {\n");
      output_string (f, "                  if (");
      output_string (f, YYSTATES_TOP_VARIABLE_NAME);
      output_string (f, " >= ");
      output_string (f, YYSTATES_END_VARIABLE_NAME);
      if (expand_flag)
	{
	  output_string (f, "\n                      && ");
	  output_string (f, YYEXPAND_STATES_STACK_FUNCTION_NAME);
	  output_string (f, "(&");
	  output_string (f, YYSTATES_VARIABLE_NAME);
	  output_string (f, ", &");
	  output_string (f, YYSTATES_END_VARIABLE_NAME);
	  output_string (f, ", &");
	  output_string (f, YYSTATES_TOP_VARIABLE_NAME);
	  output_string (f, ")");
	}
      output_string (f, ")\n");
      output_string (f, "                    ");
      output_string (f, YYABORT_MACRO_NAME);
      output_string (f, ";\n");
      output_string (f, "                  (*++");
      output_string (f, YYSTATES_TOP_VARIABLE_NAME);
      output_string (f, ") = ");
      output_string (f, YYSTATE_VARIABLE_NAME);
      output_string (f, ";\n");
      output_string (f, "                }\n");
    }
  if (!yacc_error_recovery_flag)
    {
      output_string (f, "              ");
      output_string (f, YYERR_STATES_BOUND_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYSTATES_TOP_VARIABLE_NAME);
      output_string (f, " - ");
      output_string (f, YYSTATES_VARIABLE_NAME);
      output_string (f, " + 1;\n");
      output_string (f, "              ");
      output_string (f, YYERR_NEW_TRY_VARIABLE_NAME);
      output_string (f, " = 0;\n");
      /* `yyerr_look_ahead_chars = YYERR_LOOK_AHEAD_INCREMENT;' */
      output_string (f, "              ");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME);
      output_string (f, ";\n");
      /*      if (yyerr_look_ahead_chars > YYERR_MAX_LOOK_AHEAD_CHARS)
               yyerr_look_ahead_chars = YYERR_MAX_LOOK_AHEAD_CHARS;
#if YYDEBUG != 0
              if (yydebug)
                fprintf (stderr,
                         "Start error recovery, look ahead %d tokens\n",
                         yyerr_look_ahead_chars);
#endif
       */
      output_string (f, "              if (");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, " > ");
      output_string (f, YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (f, ")\n");
      output_string (f, "                ");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (f, ";\n");
      output_string (f, "#if ");
      output_string (f, YYDEBUG_MACRO_NAME);
      output_string (f, " != 0\n");
      output_string (f, "              if (");
      output_yydebug_variable_name (f);
      output_string (f, ")\n");
      output_string (f, "                fprintf (stderr,\n");
      output_string (f, "                         \"Start error recovery, look ahead %d tokens\\n\",\n");
      output_string (f, "                         ");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, ");\n#endif\n");
      /* `yyerr_popped_error_states = 0;' */
      output_string (f, "              ");
      output_string (f, YYERR_POPPED_ERROR_STATES_VARIABLE_NAME);
      output_string (f, " = 0;\n");
      /*
         ??? check here on the expansion
         if (yyfirst_char_ptr == yylook_ahead_char)
	   {
             yylook_ahead_char [YYLOOK_AHEAD_SIZE - 1] = yychar;
             yylook_ahead_attribute [YYLOOK_AHEAD_SIZE - 1] = yylval;
           }
         else
           {
             yyfirst_char_ptr [-1] = yychar;
             yylook_ahead_attribute [yyfirst_char_ptr - yylook_ahead_char - 1] = yylval;
           }
         yyfirst_char_ptr_1 = yyfirst_char_ptr - 2;
         for (;;)
           {
             if (yyfirst_char_ptr_1 < yylook_ahead_char)
               yyfirst_char_ptr_1 += YYLOOK_AHEAD_SIZE;
             if (*yyfirst_char_ptr_1 == YYEMPTY)
               break;
             *yyfirst_char_ptr_1-- = YYEMPTY;
           }
#if YYDEBUG != 0
         if (yydebug)
           fprintf (stderr,
                    "Error recovery saving a token %d (%s)\n",
                    yychar, YYTOKEN_NAME (yychar));
#endif
      */
      output_string (f, "              if (");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " == ");
      output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (f, ")\n");
      output_string (f, "                {\n");
      output_string (f, "                  ");
      output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (f, " [");
      output_string (f, YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (f, " - 1] = ");
      output_yychar_variable_name (f);
      output_string (f, ";\n");
      output_string (f, "                  ");
      output_string (f, YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
      output_string (f, " [");
      output_string (f, YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (f, " - 1] = ");
      output_yylval_variable_name (f);
      output_string (f, ";\n");
      output_string (f, "                }\n");
      output_string (f, "              else\n");
      output_string (f, "                {\n");
      output_string (f, "                  ");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " [-1] = ");
      output_yychar_variable_name (f);
      output_string (f, ";\n");
      output_string (f, "                  ");
      output_string (f, YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
      output_string (f, " [");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " - ");
      output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (f, " - 1] = ");
      output_yylval_variable_name (f);
      output_string (f, ";\n");
      output_string (f, "                }\n");
      output_string (f, "              ");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " - 2;\n");
      output_string (f, "              for (;;)\n");
      output_string (f, "                {\n");
      output_string (f, "                  if (");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " < ");
      output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (f, ")\n");
      output_string (f, "                    ");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " += ");
      output_string (f, YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (f, ";\n");
      output_string (f, "                  if (*");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " == ");
      output_string (f, YYEMPTY_MACRO_NAME);
      output_string (f, ")\n");
      output_string (f, "                    break;\n");
      output_string (f, "                  *");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, "-- = ");
      output_string (f, YYEMPTY_MACRO_NAME);
      output_string (f, ";\n");
      output_string (f, "                }\n");
      output_debug_print_about_saving_token (f, "              ");
    }
  output_string (f, "            }\n");
  /* if (yyerr_status < YYERR_RECOVERY_MATCHES
-------------- if !yacc_error_recovery_flag --------------
         || yyerr_look_ahead_chars <= 0
--------------------------------------------------------
        )
       {
-------------- if !yacc_error_recovery_flag --------------
             yyerr_popped_error_states++;
             if (yyerr_look_ahead_chars < yyerr_popped_error_states * YYERR_LOOK_AHEAD_INCREMENT)
;
               yyerr_look_ahead_chars = yyerr_popped_error_states * YYERR_LOOK_AHEAD_INCREMENT;
             if (yyerr_look_ahead_chars > YYERR_MAX_LOOK_AHEAD_CHARS)
               yyerr_look_ahead_chars = YYERR_MAX_LOOK_AHEAD_CHARS;
#if YYDEBUG != 0
             if (yydebug)
               fprintf (stderr,
                        "Continue error recovery, look ahead %d tokens\n",
                        yyerr_look_ahead_chars);
#endif
             yytemp1 = 0;
	     for (yyfirst_char_ptr_1 = yyfirst_char;
	          *yyfirst_char_ptr_1 != YYEMPTY;)
               {
	         yytemp1++;
                 yyfirst_char_ptr_1++;
                 if (yyfirst_char_ptr_1 >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
		   yyfirst_char_ptr_1 = yylook_ahead_char;
               }
	     yytemp2 = 0;
             for (;;)
               {
                 if (yyfirst_char_ptr == yylook_ahead_char)
		   yyfirst_char_ptr_1 = yylook_ahead_char + YYLOOK_AHEAD_SIZE -1;
		 else
		   yyfirst_char_ptr_1 = yyfirst_char_ptr - 1;
		 if (*yyfirst_char_ptr_1 == YYEMPTY)
		   break;
		 yyfirst_char_ptr = yyfirst_char_ptr_1;
                 yytemp2++;
               }
#if YYDEBUG != 0
             if (yydebug)
               fprintf (stderr,
                        "Restore %d tokens saved during error recovery\n",
                        yytemp2);
#endif
	     yytemp1 += yytemp2 - 1;
	     if (yytemp1 < 0)
	       YYABORT;
             yylval = yylook_ahead_attribute [yyfirst_char_ptr - yylook_ahead_char];
	     yychar = *yyfirst_char_ptr;
	     *yyfirst_char_ptr++ = YYEMPTY;
	     if (yyfirst_char_ptr >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
	       yyfirst_char_ptr = yylook_ahead_char;
             if (yyerr_popped_error_states >= YYERR_POPPED_ERROR_STATES)
               {
#if YYDEBUG != 0
                 if (yydebug)
                   fprintf (stderr,
                            "%d error states has been popped -- real discarding tokens\n",
                            YYERR_POPPED_ERROR_STATES);
#endif
                 yyerr_popped_error_states = 0;
                 if (yytemp1 < YYERR_DISCARDED_CHARS)
                   {
#if YYDEBUG != 0
                     if (yydebug)
                       fprintf (stderr,
                                "Discard %d already read tokens\n",
                                yytemp1 + 1);
#endif
                     yytemp1 -= YYERR_DISCARDED_CHARS;
                     while (yytemp1 < 0)
                       {
                         yychar = yylex ();
#if YYDEBUG != 0
                         if (yydebug)
                           fprintf (stderr,
                                    "Read token %d (%s)\n",
                                    yychar, YYTOKEN_NAME (yychar));
#endif
------------------------ scanner ------------------------
                         if (yychar < 0)
-------------------------no scanner ---------------------
                         if (yychar <= 0)
---------------------------------------------------------
                           break;
                         yytemp1++;
#if YYDEBUG != 0
                         if (yytemp1 < 0 && yydebug)
                           fprintf (stderr,
                                    "Discard token %d (%s)\n",
                                    yychar, YYTOKEN_NAME (yychar));
#endif
                       }
                     for (yyfirst_char_ptr_1 = yyfirst_char_ptr; *yyfirst_char_ptr_1 != YYEMPTY;)
                       {
                         *yyfirst_char_ptr_1++ = YYEMPTY;
           	         if (yyfirst_char_ptr_1 >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
	                   yyfirst_char_ptr_1 = yylook_ahead_char;
                       }
                   }
                 else if (YYERR_DISCARDED_CHARS > 0)
                   {
#if YYDEBUG != 0
                     if (yydebug)
                       fprintf (stderr,
                                "Discard %d already read tokens\n",
                                YYERR_DISCARDED_CHARS);
#endif
                     yyfirst_char_ptr_1 = yyfirst_char_ptr + YYERR_DISCARDED_CHARS - 1;
                     if (yyfirst_char_ptr_1 >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
	               yyfirst_char_ptr_1 = yylook_ahead_char;
                     while (yyfirst_char_ptr != yyfirst_char_ptr_1)
                       {
                         *yyfirst_char_ptr++ = YYEMPTY;
                         if (yyfirst_char_ptr >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
                           yyfirst_char_ptr = yylook_ahead_char;
                       }
                     yylval = yylook_ahead_attribute [yyfirst_char_ptr - yylook_ahead_char];
         	     yychar = *yyfirst_char_ptr;
	             *yyfirst_char_ptr++ = YYEMPTY;
	             if (yyfirst_char_ptr >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
	               yyfirst_char_ptr = yylook_ahead_char;
                     
                     yychar = yylook_ahead_char [YYERR_DISCARDED_CHARS - 1];
                     yylval = yylook_ahead_attribute [YYERR_DISCARDED_CHARS - 1];
                   }
               }
--------------------------------------------------------
         yyerr_status = YYERR_RECOVERY_MATCHES;
         for (;;)
           {
             if (
----------------------------------------
                 (yystates_top - yystates < yyerr_states_bound || !yyerr_new_try)
                 &&
-----------------------------------------------
                 yytemp != YYNO_ACTION_BASE
                 && yyacheck [yytemp + YYERRCLASS] == yystate
                 && yyaction [yytemp] < <first pop shift action>)
               {
                 /* shift on error * /
#if YYDEBUG != 0
                 if (yydebug)
                   fprintf (stderr, "state %d, error recovery shifting\
to state %d\n", yystate, yyaction [yytemp]);
#endif
                 yystate = yyaction [yytemp];
------- when regular optimization ------------------------
                 if (yypushed [yystate])
                   {
----------------------------------------------------------
                     if (yystates_top >= yystates_end
		         && yyexpand_states_stack (&yystates, &yystates_end, &yystates_top))
                       YYABORT;
                     (*++yystates_top) = yystate;
------- when regular optimization || expansion ------------------------
                     if (yyattributes_top >= yyattributes_end
		         && yyexpand_attributes_stack (&yyattributes, &yyattributes_end, &yyattributes_top))
                       YYABORT;
----------------------------------------------------------
                     (*++yyattributes_top) = yyval;
------- when regular optimization ------------------------
                   }
----------------------------------------------------------
-------------- if !yacc_error_recovery_flag --------------
                 yyerr_states_bound = yystates_top - yystates;
--------------------------------------------------------
                 break;
               }
             if (yystates_top <= yystates)
               YYABORT;
             yystate = *--yystates_top;
             yytemp = yyabase [yystate];
------- when regular optimization ------------------------
             yyattributes_top
               -= yynattr_pop [yynapop_base [yystates_top [1]]
                               + *yystates_top];
----------------------------------------------------------
------- when no regular optimization ---------------------
             yyattributes_top--;
----------------------------------------------------------
           }
       } 
     else
       {
         if (yychar == YYEOF)
           YYABORT;
#if YYDEBUG != 0
         if (yydebug)
           fprintf (stderr,
                    "state %d, error recovery discards token %d (%s)\n",
                    yystate, yychar, YYTOKEN_NAME(yychar));
#endif
-------------- if !yacc_error_recovery_flag --------------
         yyerr_look_ahead_chars--;
--------------------------------------------------------
-------------- if scanner  || !yacc_error_recovery_flag ----------------------
         yyprev_char = yychar;
------------------------------------------------------------------------------
         yychar = YYEMPTY;
       }
     break; */
  output_string (f, "          if (");
  output_string (f, YYERR_STATUS_VARIABLE_NAME);
  output_string (f, " < ");
  output_string (f, YYERR_RECOVERY_MATCHES_MACRO_NAME);
  if (!yacc_error_recovery_flag)
    {
      output_string (f, " || ");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, " <= 0");
    }
  output_string (f, ")\n");
  output_string (f, "            {\n");
  if (!yacc_error_recovery_flag)
    {
      output_string (f, "              ");
      output_string (f, YYERR_POPPED_ERROR_STATES_VARIABLE_NAME);
      output_string (f, "++;\n");
      output_string (f, "              if (");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, " < ");
      output_string (f, YYERR_POPPED_ERROR_STATES_VARIABLE_NAME);
      output_string (f, " * ");
      output_string (f, YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME);
      output_string (f, ")\n");
      output_string (f, "                ");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYERR_POPPED_ERROR_STATES_VARIABLE_NAME);
      output_string (f, " * ");
      output_string (f, YYERR_LOOK_AHEAD_INCREMENT_MACRO_NAME);
      output_string (f, ";\n");
      output_string (f, "              if (");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, " > ");
      output_string (f, YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (f, ")\n");
      output_string (f, "                ");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYERR_MAX_LOOK_AHEAD_CHARS_MACRO_NAME);
      output_string (f, ";\n");
      output_string (f, "#if ");
      output_string (f, YYDEBUG_MACRO_NAME);
      output_string (f, " != 0\n");
      output_string (f, "              if (");
      output_yydebug_variable_name (f);
      output_string (f, ")\n");
      output_string (f, "                fprintf (stderr,\n");
      output_string (f, "                         \"Continue error recovery, look ahead %d tokens\\n\",\n");
      output_string (f, "                         ");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, ");\n#endif\n");
      output_string (f, "              ");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, " = 0;\n");
      output_string (f, "              for (");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, "; *");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " != ");
      output_string (f, YYEMPTY_MACRO_NAME);
      output_string (f, ";)\n");
      output_string (f, "                {\n");
      output_string (f, "                  ");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, "++;\n");
      output_string (f, "                  ");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, "++;\n");
      output_check_yyfirst_char_ptr (f, "                  ", TRUE);
      output_string (f, "                }\n");

      output_string (f, "              ");
      output_string (f, YYTEMP2_VARIABLE_NAME);
      output_string (f, " = 0;\n");
      output_string (f, "              for (;;)\n");
      output_string (f, "                {\n");
      output_string (f, "                  if (");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " == ");
      output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (f, ")\n");
      output_string (f, "                    ");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (f, " + ");
      output_string (f, YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (f, " - 1;\n");
      output_string (f, "                  else\n");
      output_string (f, "                    ");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " - 1;\n");
      output_string (f, "                  if (*");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " == ");
      output_string (f, YYEMPTY_MACRO_NAME);
      output_string (f, ")\n");
      output_string (f, "                    break;\n");
      output_string (f, "                  ");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, ";\n");
      output_string (f, "                  ");
      output_string (f, YYTEMP2_VARIABLE_NAME);
      output_string (f, "++;\n");
      output_string (f, "                }\n");
      output_string (f, "              ");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, " += ");
      output_string (f, YYTEMP2_VARIABLE_NAME);
      output_string (f, " - 1;\n");
      output_string (f, "              if (");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, " < 0)\n");
      output_string (f, "                ");
      output_string (f, YYABORT_MACRO_NAME);
      output_string (f, ";\n");
      output_string (f, "#if ");
      output_string (f, YYDEBUG_MACRO_NAME);
      output_string (f, " != 0\n");
      output_string (f, "              if (");
      output_yydebug_variable_name (f);
      output_string (f, ")\n");
      output_string (f, "                fprintf (stderr,\n");
      output_string (f, "                         \"Restore %d tokens saved during error recovery\\n\",\n");
      output_string (f, "                         ");
      output_string (f, YYTEMP2_VARIABLE_NAME);
      output_string (f, ");\n#endif\n");
      output_look_ahead_read_without_saving (f, "              ");
      output_string (f, "              if (");
      output_string (f, YYERR_POPPED_ERROR_STATES_VARIABLE_NAME);
      output_string (f, " >= ");
      output_string (f, YYERR_POPPED_ERROR_STATES_MACRO_NAME);
      output_string (f, ")\n");
      output_string (f, "                {\n");
      output_string (f, "#if ");
      output_string (f, YYDEBUG_MACRO_NAME);
      output_string (f, " != 0\n");
      output_string (f, "                  if (");
      output_yydebug_variable_name (f);
      output_string (f, ")\n");
      output_string (f, "                    fprintf (stderr, \"%d error states has been popped -- real discarding tokens\\n\",\n");
      output_string (f, "                             ");
      output_string (f, YYERR_POPPED_ERROR_STATES_MACRO_NAME);
      output_string (f, ");\n");
      output_string (f, "#endif\n");
      output_string (f, "                  ");
      output_string (f, YYERR_POPPED_ERROR_STATES_VARIABLE_NAME);
      output_string (f, " = 0;\n");
      output_string (f, "                  if (");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, " < ");
      output_string (f, YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (f, ")\n");
      output_string (f, "                    {\n");
      output_string (f, "#if ");
      output_string (f, YYDEBUG_MACRO_NAME);
      output_string (f, " != 0\n");
      output_string (f, "                      if (");
      output_yydebug_variable_name (f);
      output_string (f, ")\n");
      output_string
        (f, "                        fprintf (stderr, \"Discard %d already read tokens\\n\",\n");
      output_string (f, "                                 ");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, " + 1);\n");
      output_string (f, "#endif\n");
      output_string (f, "                      ");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, " -= ");
      output_string (f, YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (f, ";\n");
      output_string (f, "                      while (");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, " < 0)\n");
      output_string (f, "                        {\n");
      output_string (f, "                          ");
      output_yychar_variable_name (f);
      output_string (f, " = ");
      output_yylex_function_name (f);
      output_string (f, " ();\n");
      output_string (f, "#if ");
      output_string (f, YYDEBUG_MACRO_NAME);
      output_string (f, " != 0\n");
      output_string (f, "                          if (");
      output_yydebug_variable_name (f);
      output_string (f, ")\n");
      output_string
        (f, "                            fprintf (stderr, \"Read token %d (%s)\\n\",\n");
      output_string (f, "                                     ");
      output_yychar_variable_name (f);
      output_string (f, ", ");
      output_string (f, YYTOKEN_NAME_MACRO_NAME);
      output_string (f, " (");
      output_yychar_variable_name (f);
      output_string (f, "));\n");
      output_string (f, "#endif\n");
      output_string (f, "                          ");
      output_string (f, "if (");
      output_yychar_variable_name (f);
      if (IR_scanner_flag (description))
        output_string (f, " < 0)\n");
      else
        output_string (f, " <= 0)\n");
      output_string (f, "                          ");
      output_string (f, "  break;\n");
      output_string (f, "                          ");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, "++;\n");
      output_string (f, "#if ");
      output_string (f, YYDEBUG_MACRO_NAME);
      output_string (f, " != 0\n");
      output_string (f, "                          if (");
      output_string (f, YYTEMP1_VARIABLE_NAME);
      output_string (f, " < 0 && ");
      output_yydebug_variable_name (f);
      output_string (f, ")\n");
      output_string
        (f, "                            fprintf (stderr, \"Discard token %d (%s)\\n\",\n");
      output_string (f, "                                     ");
      output_yychar_variable_name (f);
      output_string (f, ", ");
      output_string (f, YYTOKEN_NAME_MACRO_NAME);
      output_string (f, " (");
      output_yychar_variable_name (f);
      output_string (f, "));\n");
      output_string (f, "#endif\n");
      output_string (f, "                        }\n");
      output_string (f, "                      for (");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, "; *");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " != ");
      output_string (f, YYEMPTY_MACRO_NAME);
      output_string (f, ";)\n");
      output_string (f, "                        {\n");
      output_string (f, "                           *");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, "++ = ");
      output_string (f, YYEMPTY_MACRO_NAME);
      output_string (f, ";\n");
      output_check_yyfirst_char_ptr (f, "                           ", TRUE);
      output_string (f, "                        }\n");
      output_string (f, "                    }\n");
      output_string (f, "                  else if (");
      output_string (f, YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (f, " > 0)\n");
      output_string (f, "                    {\n");
      output_string (f, "#if ");
      output_string (f, YYDEBUG_MACRO_NAME);
      output_string (f, " != 0\n");
      output_string (f, "                      if (");
      output_yydebug_variable_name (f);
      output_string (f, ")\n");
      output_string
        (f, "                        fprintf (stderr, \"Discard %d already read tokens\\n\",\n");
      output_string
        (f, "                                 ");
      output_string (f, YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (f, ");\n");
      output_string (f, "#endif\n");
      output_string (f, "                      ");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " + ");
      output_string (f, YYERR_DISCARDED_CHARS_MACRO_NAME);
      output_string (f, " - 1;\n");
      output_check_yyfirst_char_ptr (f, "                      ", TRUE);
      output_string (f, "                      while (");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, " != ");
      output_string (f, YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (f, ")\n");
      output_string (f, "                        {\n");
      output_string (f, "                           *");
      output_string (f, YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (f, "++ = ");
      output_string (f, YYEMPTY_MACRO_NAME);
      output_string (f, ";\n");
      output_check_yyfirst_char_ptr (f, "                          ", FALSE);
      output_string (f, "                        }\n");
      output_look_ahead_read_without_saving (f, "                      ");
      output_string (f, "                    }\n");
      output_string (f, "                }\n");
    }
  output_string (f, "              ");
  output_string (f, YYERR_STATUS_VARIABLE_NAME);
  output_string (f, " = ");
  output_string (f, YYERR_RECOVERY_MATCHES_MACRO_NAME);
  output_string (f, ";\n");
  output_string (f, "              for (;;)\n");
  output_string (f, "                {\n");
  output_string (f, "                  if (");
  if (!yacc_error_recovery_flag)
    {
      output_string (f, "(");
      output_string (f, YYSTATES_TOP_VARIABLE_NAME);
      output_string (f, " - ");
      output_string (f, YYSTATES_VARIABLE_NAME);
      output_string (f, " < ");
      output_string (f, YYERR_STATES_BOUND_VARIABLE_NAME);
      output_string (f, " || !");
      output_string (f, YYERR_NEW_TRY_VARIABLE_NAME);
      output_string (f, ")\n                      && ");
    }
  output_string (f, YYTEMP_VARIABLE_NAME);
  output_string (f, " != ");
  output_string (f, NO_ACTION_BASE_MACRO_NAME);
  output_string (f, "\n");
  output_string (f, "                      && ");
  output_string (f, ACTION_CHECK_VECTOR_NAME);
  output_string (f, " [");
  output_string (f, YYTEMP_VARIABLE_NAME);
  output_string (f, " + ");
  output_string (f, ERRCLASS_MACRO_NAME);
  output_string (f, "] == ");
  output_string (f, YYSTATE_VARIABLE_NAME);
  output_string (f, "\n");
  output_string (f, "                      && ");
  output_string (f, ACTION_COMB_VECTOR_NAME);
  output_string (f, " [");
  output_string (f, YYTEMP_VARIABLE_NAME);
  output_string (f, " + ");
  output_string (f, ERRCLASS_MACRO_NAME);
  output_string (f, "] < ");
  output_decimal_number (f, first_pop_shift_action_value, 0);
  output_string (f, "/* after the last shift */)\n");
  output_string (f, "                    {\n");
  output_string (f, "                      /* shift on error */\n");
  output_string (f, "#if ");
  output_string (f, YYDEBUG_MACRO_NAME);
  output_string (f, " != 0\n");
  output_string (f, "                      if (");
  output_yydebug_variable_name (f);
  output_string (f, ")\n");
  output_string (f, "                        fprintf (stderr,\n");
  output_string (f, "                                 \"state %d, ");
  output_string (f, "error shifting to state %d\\n\",\n");
  output_string (f, "                                 ");
  output_string (f, YYSTATE_VARIABLE_NAME);
  output_string (f, ", ");
  output_string (f, ACTION_COMB_VECTOR_NAME);
  output_string (f, " [");
  output_string (f, YYTEMP_VARIABLE_NAME);
  output_string (f, " + ");
  output_string (f, ERRCLASS_MACRO_NAME);
  output_string (f, "]);\n#endif\n");
  output_string (f, "                      ");
  output_string (f, YYSTATE_VARIABLE_NAME);
  output_string (f, " = ");
  output_string (f, ACTION_COMB_VECTOR_NAME);
  output_string (f, " [");
  output_string (f, YYTEMP_VARIABLE_NAME);
  output_string (f, " + ");
  output_string (f, ERRCLASS_MACRO_NAME);
  output_string (f, "];\n ");
  if (regular_optimization_flag)
    {
      output_string (f, "                     if (");
      output_string (f, PUSHED_STATE_FLAGS_VECTOR_NAME);
      output_string (f, " [");
      output_string (f, YYSTATE_VARIABLE_NAME);
      output_string (f, "])\n");
      output_string (f, "                        {\n");
    }
  if (regular_optimization_flag)
    output_state_pushing (TRUE, "                         ");
  else
    output_state_pushing (TRUE, "                     ");
  if (regular_optimization_flag)
    output_attribute_pushing (TRUE, FALSE, "                         ");
  else
    output_attribute_pushing (TRUE, FALSE, "                     ");
  if (regular_optimization_flag)
    output_string (f, "                        }\n");
  if (!yacc_error_recovery_flag)
    {
      output_string (f, "                      ");
      output_string (f, YYERR_STATES_BOUND_VARIABLE_NAME);
      output_string (f, " = ");
      output_string (f, YYSTATES_TOP_VARIABLE_NAME);
      output_string (f, " - ");
      output_string (f, YYSTATES_VARIABLE_NAME);
      output_string (f, ";\n");
    }
  output_string (f, "                      break;\n");
  output_string (f, "                    }\n");
  output_string (f, "                  if (");
  output_string (f, YYSTATES_TOP_VARIABLE_NAME);
  output_string (f, " <= ");
  output_string (f, YYSTATES_VARIABLE_NAME);
  output_string (f, ")\n");
  output_string (f, "                    ");
  output_string (f, YYABORT_MACRO_NAME);
  output_string (f, ";\n");
  output_string (f, "                  ");
  output_string (f, YYSTATE_VARIABLE_NAME);
  output_string (f, " = *--");
  output_string (f, YYSTATES_TOP_VARIABLE_NAME);
  output_string (f, ";\n");
  output_string (f, "                  ");
  output_string (f, YYTEMP_VARIABLE_NAME);
  output_string (f, " = ");
  output_string (f, ACTION_BASE_VECTOR_NAME);
  output_string (f, " [");
  output_string (f, YYSTATE_VARIABLE_NAME);
  output_string (f, "];\n");
  output_string (f, "                  ");
  output_string (f, YYATTRIBUTES_TOP_VARIABLE_NAME);
  if (!regular_optimization_flag)
    output_string (f, "--;\n");
  else
    {
      output_string (f, "\n                    -= ");
      output_string (f, NATTR_POP_COMB_VECTOR_NAME);
      output_string (f, " [");
      output_string (f, NAPOP_BASE_VECTOR_NAME);
      output_string (f, " [");
      output_string (f, YYSTATES_TOP_VARIABLE_NAME);
      output_string (f, " [1]]\n");
      output_string (f, "                                    + *");
      output_string (f, YYSTATES_TOP_VARIABLE_NAME);
      output_string (f, "];\n");
    }
  output_string (f, "                }\n");
  output_string (f, "            }\n");
  output_string (f, "          else\n");
  output_string (f, "            {\n");
  output_string (f, "              if (");
  output_yychar_variable_name (f);
  output_string (f, " == ");
  output_string (f, YYEOF_MACRO_NAME);
  output_string (f, ")\n");
  output_string (f, "                ");
  output_string (f, YYABORT_MACRO_NAME);
  output_string (f, ";\n");
  output_string (f, "#if ");
  output_string (f, YYDEBUG_MACRO_NAME);
  output_string (f, " != 0\n");
  output_string (f, "              if (");
  output_yydebug_variable_name (f);
  output_string (f, ")\n");
  output_string (f, "                 fprintf\n");
  output_string (f, "                   (stderr,\n");
  output_string (f, "                    \"state %d, error recovery discards");
  output_string (f, " token %d (%s)\\n\",\n");
  output_string (f, "                    ");
  output_string (f, YYSTATE_VARIABLE_NAME);
  output_string (f, ", ");
  output_yychar_variable_name (f);
  output_string (f, ", ");
  output_string (f, YYTOKEN_NAME_MACRO_NAME);
  output_string (f, " (");
  output_yychar_variable_name (f);
  output_string (f, "));\n#endif\n");
  if (!yacc_error_recovery_flag)
    {
      output_string (f, "              ");
      output_string (f, YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (f, "--;\n");
    }
  if (IR_scanner_flag (description) || !yacc_error_recovery_flag)
    {
      output_string (f, "              ");
      output_string (f, YYPREV_CHAR_VARIABLE_NAME);
      output_string (f, " = ");
      output_yychar_variable_name (f);
      output_string (f, ";\n");
    }
  output_string (f, "              ");
  output_yychar_variable_name (f);
  output_string (f, " = ");
  output_string (f, YYEMPTY_MACRO_NAME);
  output_string (f, ";\n");
  output_string (f, "            }\n");
  output_string (f, "          break;\n");
  /*  case YYFINAL:
        YYACCEPT;
        break; */
  output_string (f, "        case ");
  output_string (f, FINAL_STATE_VALUE_MACRO_NAME);
  output_string (f, ":\n");
  output_string (f, "          ");
  output_string (f, YYACCEPT_MACRO_NAME);
  output_string (f, ";\n");
  output_string (f, "          break;\n");
  first_shift_flag = TRUE;
  for (push_attribute_flag = 0; push_attribute_flag < 2; push_attribute_flag++)
    for (push_state_flag = 0; push_state_flag < 2; push_state_flag++)
      {
        last_LR_set = NULL;
        for (current_LR_core = IR_LR_core_list (description);
             current_LR_core != NULL;
             current_LR_core = IR_next_LR_core (current_LR_core))
          for (current_LR_set = IR_LR_set_list (current_LR_core);
               current_LR_set != NULL;
               current_LR_set = IR_next_LR_set (current_LR_set))
            if (IR_reachable_flag (current_LR_set)
                && ((IR_it_is_pushed_LR_set (current_LR_set)
                     && push_state_flag)
                    || (!IR_it_is_pushed_LR_set (current_LR_set)
                        && !push_state_flag))
                && ((IR_attribute_is_used (current_LR_set)
                     && push_attribute_flag)
                    || (!IR_attribute_is_used (current_LR_set)
                        && !push_attribute_flag))
                && (characteristic_symbol_of_LR_set (current_LR_set)
                    != end_marker_single_definition
                    || !IR_IS_OF_TYPE (IR_element_after_dot
                                       (IR_LR_situation_list (current_LR_set)),
                                        IR_NM_canonical_rule_end)))
              {
                for (LR_set_reference = IR__first_double_link (current_LR_set);
                     LR_set_reference != NULL;
                     LR_set_reference
                       = IR__next_double_link (LR_set_reference))
                  {
                    owner = IR__owner (LR_set_reference);
                    if (IR_IS_OF_TYPE (owner, IR_NM_LR_situation))
                      {             
                        if (!IR_goto_arc_has_been_removed (owner)
                            && IR_IS_OF_TYPE (IR_element_itself
                                              (IR_element_after_dot (owner)),
                                              IR_NM_single_term_definition))
                          break;
                      }
                  }
                if (LR_set_reference != NULL)
                  {
                    if (first_shift_flag)
                      {
                        /* /* shifts * / */
                        output_string (f, "        /* shifts */\n");
                        first_shift_flag = FALSE;
                      }
                    /* The corresponding state is achievable by shift. */
                    /*  case <destination state number>: */
                    output_string (f, "        case ");
                    output_decimal_number
                      (f, IR_LR_set_order_number (current_LR_set), 0);
                    output_string (f, ":\n");
                    last_LR_set = current_LR_set;
                  }
              }
        if (last_LR_set != NULL)
          {
            /*  yystate = yytemp;
                yyerr_status--;
                #if YYDEBUG != 0
                if (yydebug)
                fprintf(stderr, "Shifting token %d (%s), ",
                        yychar, yytname[yychar1]);
                #endif */
            output_string (f, "#if ");
            output_string (f, YYDEBUG_MACRO_NAME);
            output_string (f, " != 0\n");
            output_string (f, "          ");
            output_string (f, "if (");
            output_yydebug_variable_name (f);
            output_string (f, ")\n");
            output_string (f, "          ");
            output_string
              (f, "  fprintf (stderr, \"Shifting token %d (%s)\\n\", ");
            output_yychar_variable_name (f);
            output_string (f, ",\n");
            output_string (f, "                     ");
            output_string (f, TOKEN_NAMES_VECTOR_NAME);
            output_string (f, "[");
            output_yychar_variable_name (f);
            output_string (f, "]);\n");
            output_string (f, "#endif\n\n");
            output_string (f, "          ");
            output_string (f, YYSTATE_VARIABLE_NAME);
            output_string (f, " = ");
            output_string (f, YYTEMP_VARIABLE_NAME);
            output_string (f, ";\n");
            output_yyerr_status_decrement ("          ");
            output_pushing (last_LR_set,
                            TRUE, regular_optimization_flag || expand_flag,
			    TRUE);
            /*-------------- if scanner || !yacc_error_recovery_flag -----
                       yyprev_char = yychar;
              ------------------------------------------------------------
                       yychar=YYEMPTY;
                       break; */
            if (IR_scanner_flag (description) || !yacc_error_recovery_flag)
              {
                output_string (f, "          ");
                output_string (f, YYPREV_CHAR_VARIABLE_NAME);
                output_string (f, " = ");
                output_yychar_variable_name (f);
                output_string (f, ";\n");
              }
            output_string (f, "          ");
            output_yychar_variable_name (f);
            output_string (f, " = ");
            output_string (f, YYEMPTY_MACRO_NAME);
            output_string (f, ";\n");
            output_string (f, "          break;\n");
          }
      }
  /* Output reduces. */
  VLO_CREATE (reduce_LR_situations_vector, 0);
  VLO_EXPAND (reduce_LR_situations_vector,
              IR_reduces_number (description) * sizeof (IR_node_t));
  for (i = 0; i < IR_reduces_number (description); i++)
    ((IR_node_t *) VLO_BEGIN (reduce_LR_situations_vector)) [i] = NULL;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          if (IR_IS_OF_TYPE (IR_element_after_dot (current_LR_situation),
                             IR_NM_canonical_rule_end)
              /* Except for axiom rule. */
              && (IR_canonical_rule (IR_element_after_dot
                                     (current_LR_situation))
                  != IR_canonical_rule_list (description))
              && IR_corresponding_regular_arc (current_LR_situation) == NULL
              && (IR_look_ahead_context (current_LR_situation) == NULL
                  || !it_is_zero_context (IR_look_ahead_context
                                          (current_LR_situation))))
            ((IR_node_t *) VLO_BEGIN (reduce_LR_situations_vector))
              [IR_reduce_number (current_LR_situation)] = current_LR_situation;
  first_reduce_flag = TRUE;
  for (i = 0; i < IR_reduces_number (description); i++)
    {
      current_LR_situation
        = ((IR_node_t *) VLO_BEGIN (reduce_LR_situations_vector)) [i];
      if (current_LR_situation != NULL)
        {
          if (first_reduce_flag)
            {
              /* /* reduces * / */
              output_string (f, "        /* reduces */\n");
              first_reduce_flag = FALSE;
            }
          /* case <reduce code>: */
          canonical_rule = IR_canonical_rule (IR_element_after_dot
                                              (current_LR_situation));
          output_string (f, "        case ");
          output_decimal_number
            (f, first_reduce_value + IR_reduce_number (current_LR_situation),
             0);
          output_string (f, ":\n");

          output_string (f, "          /* ");
          output_LR_situation (f, current_LR_situation,
                               "             ", FALSE);
          output_string (f, " */\n");
          if (IR_action (canonical_rule) != NULL)
            {
              output_line
                (f, IR_position (IR_action (canonical_rule)).line_number,
                 IR_position (IR_action (canonical_rule)).file_name);
              output_char ('{', f);
              output_action_reduce_LR_situation = current_LR_situation;
              process_canonical_rule_action
                (IR_canonical_rule (IR_element_after_dot
                                    (current_LR_situation)),
                 output_action_char, output_action_attribute);
              output_string (f, "}\n");
              output_current_line (f);
            }
          rule_length
            = canonical_rule_right_hand_side_prefix_length
              (canonical_rule, NULL);
          popped_states_number
            = pushed_LR_sets_or_attributes_number_on_path
              (IR_LR_set (current_LR_situation), rule_length, FALSE);
          /* pushing and decrementing */
          output_states_stack_top_decrement (popped_states_number);
          popped_attributes_number
            = pushed_LR_sets_or_attributes_number_on_path
              (IR_LR_set (current_LR_situation), rule_length, TRUE);
          output_attributes_stack_top_decrement (popped_attributes_number);
          /* yystate = yygoto [yygbase[*yystates_top] + <nonterm>]; */
          output_string (f, "          ");
          output_string (f, YYSTATE_VARIABLE_NAME);
          output_string (f, " = ");
          output_string (f, GOTO_COMB_VECTOR_NAME);
          output_string (f, " [");
          output_string (f, GOTO_BASE_VECTOR_NAME);
          output_string (f, " [*");
          output_string (f, YYSTATES_TOP_VARIABLE_NAME);
          output_string (f, "] + ");
          output_decimal_number (f, IR_nonterm_order_number
                                 (IR_left_hand_side (canonical_rule)),
                                 0);
          output_string (f, "];\n");
          LR_set_target = get_a_target (IR_LR_set (current_LR_situation),
                                        canonical_rule);
          output_pushing
            (LR_set_target, popped_states_number < 1,
             popped_attributes_number < 1
	     && (regular_optimization_flag || expand_flag),
	     FALSE);
          output_string (f, "          break;\n");
        }
    }
  /* Output regular arcs. */
  first_regular_arc_flag = TRUE;
  for (current_LR_core = IR_LR_core_list (description);
       current_LR_core != NULL;
       current_LR_core = IR_next_LR_core (current_LR_core))
    for (current_LR_set = IR_LR_set_list (current_LR_core);
         current_LR_set != NULL;
         current_LR_set = IR_next_LR_set (current_LR_set))
      if (IR_reachable_flag (current_LR_set))
        for (current_LR_situation = IR_LR_situation_list (current_LR_set);
             current_LR_situation != NULL;
             current_LR_situation
               = IR_next_LR_situation (current_LR_situation))
          {
            regular_arc = IR_corresponding_regular_arc (current_LR_situation);
            if (regular_arc != NULL
                && IR_first_equivalent_regular_arc_flag (regular_arc))
              {
                if (first_regular_arc_flag)
                  {
                    /* /* regular arcs * / */
                    output_string (f, "        /* regular arcs */\n");
                    first_regular_arc_flag = FALSE;
                  }
                output_shift_pop_actions (regular_arc);
              }
          }
  output_string (f, "        default:\n          abort ();\n");
  output_string (f, "        }\n");
}

static void
output_definition_yytemp_variable (void)
{
  /* Definition of `yytemp'. */
  output_string (output_implementation_file, "  int ");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
}

static void
output_definition_inside_yyparse (void)
{
  /* Definition of `yystate'. */
  output_string (output_implementation_file, "  int ");
  output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  if (!IR_scanner_flag (description))
    output_inside_outside_definitions (output_implementation_file, TRUE);
  if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
    {
      /* Definition of `yyfirst_char_ptr_1'. */
      output_string (output_implementation_file, "  int *");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
    }
  if (IR_scanner_flag (description) || !yacc_error_recovery_flag)
    {
      /* Definition of `yyprev_char'. */
      output_string (output_implementation_file, "  int ");
      output_string (output_implementation_file, YYPREV_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
    }
  /* Definition of `yychar1'. */
  output_string (output_implementation_file, "  int ");
  output_string (output_implementation_file, YYCHAR1_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  output_definition_yytemp_variable ();
  if (!yacc_error_recovery_flag)
    {
      /* Definition of `yytemp1'. */
      output_string (output_implementation_file, "  int ");
      output_string (output_implementation_file, YYTEMP1_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
      /* Definition of `yytemp2'. */
      output_string (output_implementation_file, "  int ");
      output_string (output_implementation_file, YYTEMP2_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
      /* Definition of `yyerr_new_try'. */
      output_string (output_implementation_file, "  int ");
      output_string (output_implementation_file, YYERR_NEW_TRY_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
    }
  if (real_look_ahead_number >= 2)
    {
      /* Definition of `yysaved_lval'. */
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file, YYSTYPE_MACRO_NAME);
      output_string (output_implementation_file, " ");
      output_string (output_implementation_file, YYSAVED_LVAL_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
    }
  /* Definition of `yyerr_status'. */
  output_string (output_implementation_file, "  long int ");
  output_string (output_implementation_file, YYERR_STATUS_VARIABLE_NAME);
  output_string
    (output_implementation_file,
     ";  /* tokens number to shift before error messages enabled */\n");
  /* Definition of `yyval'. */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYSTYPE_MACRO_NAME);
  output_string (output_implementation_file, " ");
  output_string (output_implementation_file, YYVAL_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  /* Definition of `yystates_end'. */
  output_string (output_implementation_file, "  int *");
  output_string (output_implementation_file, YYSTATES_END_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  /* Definition of `yystates_top'. */
  output_string (output_implementation_file, "  int *");
  output_string (output_implementation_file, YYSTATES_TOP_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  if (!yacc_error_recovery_flag)
    {
      /* Definition of `yyerr_states_bound'. */
      output_string (output_implementation_file, "  int ");
      output_string (output_implementation_file,
                     YYERR_STATES_BOUND_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
      /* Definition of `yyerr_look_ahead_chars'. */
      output_string (output_implementation_file, "  int ");
      output_string (output_implementation_file,
                     YYERR_LOOK_AHEAD_CHARS_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
      /* Definition of `yyerr_popped_error_states'. */
      output_string (output_implementation_file, "  int ");
      output_string (output_implementation_file,
                     YYERR_POPPED_ERROR_STATES_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
      
    }
  /* Definition of `yyattributes_end'. */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYSTYPE_MACRO_NAME);
  output_string (output_implementation_file, " *");
  output_string (output_implementation_file, YYATTRIBUTES_END_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  /* Definition of `yyattributes_top'. */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYSTYPE_MACRO_NAME);
  output_string (output_implementation_file, " *");
  output_string (output_implementation_file, YYATTRIBUTES_TOP_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  output_char ('\n', output_implementation_file);
}


static void
output_code_before_switch (void)
{
  int i;

  /*
     #if YYDEBUG != 0
        if (yydebug)
          fprintf(stderr, "Entering state %d\n", yystate);
     #endif */
  output_string (output_implementation_file, "#if ");
  output_string (output_implementation_file, YYDEBUG_MACRO_NAME);
  output_string (output_implementation_file, " != 0\n");
  output_string (output_implementation_file, "      if (");
  output_yydebug_variable_name (output_implementation_file);
  output_string (output_implementation_file,
                 ")\n        fprintf (stderr, \"Entering state %d\\n\", ");
  output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
  output_string (output_implementation_file, ");\n");
  output_string (output_implementation_file, "#endif\n");
  /* `yytemp = yyabase[yystate];' */
  output_string (output_implementation_file, "      ");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, ACTION_BASE_VECTOR_NAME);
  output_string (output_implementation_file, " [");
  output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
  output_string (output_implementation_file, "];\n");
  /*
     if (yytemp == YYNO_ACTION_BASE)
       yytemp = yyadefault [yystate];
   */
  output_string (output_implementation_file, "      if (");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, " == ");
  output_string (output_implementation_file, NO_ACTION_BASE_MACRO_NAME);
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, "        ");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, ACTION_DEFAULT_VECTOR_NAME);
  output_string (output_implementation_file, " [");
  output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
  output_string (output_implementation_file, "];\n");
  /* 
     else
       {
         if (yychar == YYEMPTY)
           {
   */
  output_string (output_implementation_file, "      else\n        {\n");
  output_string (output_implementation_file, "          if (");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, " == ");
  output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
  output_string (output_implementation_file, ")\n            {\n");
  if (!yacc_error_recovery_flag)
    {
      /*
#ifdef YYERR_RECOVERY_END
             if (yyerr_status == 0)
               YYERR_RECOVERY_END ();
#endif
      */
      output_string (output_implementation_file, "#ifdef ");
      output_string (output_implementation_file,
                     YYERR_RECOVERY_END_MACRO_NAME);
      output_string (output_implementation_file, "\n              if (");
      output_string (output_implementation_file, YYERR_STATUS_VARIABLE_NAME);
      output_string (output_implementation_file, " == 0)\n");
      output_string (output_implementation_file, "                ");
      output_string (output_implementation_file,
                     YYERR_RECOVERY_END_MACRO_NAME);
      output_string (output_implementation_file, " ();\n");
      output_string (output_implementation_file, "#endif\n\n");
    }
  if (real_look_ahead_number >= 2 || !yacc_error_recovery_flag)
    {
      /* `if (yylook_ahead_char == YYEMPTY)'
         or `if (*yyfirst_char_ptr == YYEMPTY)' */
      output_string (output_implementation_file, "              if (");
      if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
	{
	  output_string (output_implementation_file, "*");
	  output_string (output_implementation_file,
			 YYFIRST_CHAR_PTR_VARIABLE_NAME);
	}
      else
	output_string (output_implementation_file,
		       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, " == ");
      output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
      output_string (output_implementation_file, ")\n                {\n");
    }
  /* `yychar = yylex ();'
     or  `yyschar = yyslex ();'*/
  if (real_look_ahead_number >= 2 || !yacc_error_recovery_flag)
    output_string (output_implementation_file, "    ");
  output_string (output_implementation_file, "              ");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_yylex_function_name (output_implementation_file);
  output_string (output_implementation_file, " ();\n");
  /* #if YYDEBUG != 0
        if (yydebug)
          fprintf(stderr, "Reading a token %d (%s)\n",
                  yychar, YYTOKEN_NAME (yychar));
     #endif */
  output_string (output_implementation_file, "#if ");
  output_string (output_implementation_file, YYDEBUG_MACRO_NAME);
  output_string (output_implementation_file, " != 0\n");
  if (real_look_ahead_number >= 2 || !yacc_error_recovery_flag)
    output_string (output_implementation_file, "    ");
  output_string (output_implementation_file, "              if (");
  output_yydebug_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n");
  if (real_look_ahead_number >= 2 || !yacc_error_recovery_flag)
    output_string (output_implementation_file, "    ");
  output_string
    (output_implementation_file,
     "                fprintf (stderr, \"Reading a token %d (%s)\\n\",\n");
  if (real_look_ahead_number >= 2 || !yacc_error_recovery_flag)
    output_string (output_implementation_file, "    ");
  output_string (output_implementation_file, "                         ");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, YYTOKEN_NAME_MACRO_NAME);
  output_string (output_implementation_file, " (");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, "));\n");
  output_string (output_implementation_file, "#endif\n");
  if (!yacc_error_recovery_flag)
    {
      /* 
	 if (yyerr_status > 0)
	   {
	     yylook_ahead_attribute [yyfirst_char_ptr - yylook_ahead] == yylval;
	     *yyfirst_char_ptr++ = yychar;
	     if (yyfirst_char_ptr >= yylook_ahead_char_ptr + YYLOOK_AHEAD_SIZE)
	       yyfirst_char_ptr = yylook_ahead_size;
             ???? testing on expansion here.
	     *yyfirst_char_ptr = YYEMPTY;
#if YYDEBUG != 0
             if (yydebug)
               fprintf (stderr,
                        "Error recovery saving a token %d (%s)\n",
                        yychar, YYTOKEN_NAME (yychar));
#endif
	   }
      */
      output_string (output_implementation_file, "                  if (");
      output_string (output_implementation_file, YYERR_STATUS_VARIABLE_NAME);
      output_string (output_implementation_file, " > 0)\n");
      output_string (output_implementation_file, "                    {\n");
      output_string (output_implementation_file, "                      ");
      output_string (output_implementation_file,
		     YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (output_implementation_file, " - ");
      output_string (output_implementation_file,
		     YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, "] = ");
      output_yylval_variable_name (output_implementation_file);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "                      *");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (output_implementation_file, "++ = ");
      output_yychar_variable_name (output_implementation_file);
      output_string (output_implementation_file, ";\n");
      output_check_yyfirst_char_ptr (output_implementation_file,
				     "                      ", FALSE);
      output_string (output_implementation_file, "                      *");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
      output_string (output_implementation_file, ";\n");
      output_debug_print_about_saving_token (output_implementation_file,
					     "                      ");
      output_string (output_implementation_file, "                    }\n");
    }
  if (real_look_ahead_number >= 2 || !yacc_error_recovery_flag)
    {
      /* }...else...{ */
      output_string (output_implementation_file, "                }\n");
      output_string (output_implementation_file, "              else\n");
      output_string (output_implementation_file, "                {\n");
      /* `yychar = yylook_ahead_char;' or `yychar = *yyfirst_char_ptr;' */
      output_string (output_implementation_file, "                  ");
      output_yychar_variable_name (output_implementation_file);
      output_string (output_implementation_file, " = ");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
	output_string (output_implementation_file,
		       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
	{
	  output_string (output_implementation_file, "*");
	  output_string (output_implementation_file,
			 YYFIRST_CHAR_PTR_VARIABLE_NAME);
	}
      output_string (output_implementation_file, ";\n");
      /* `yylval = yylook_ahead_attribute;'
         or `yylval = yylook_ahead_attribute [yyfirst_char_ptr - yylook_ahead_char];' */
      output_string (output_implementation_file, "                  ");
      output_yylval_variable_name (output_implementation_file);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file, ";\n");
      else
	{
	  output_string (output_implementation_file, " [");
	  output_string (output_implementation_file,
			 YYFIRST_CHAR_PTR_VARIABLE_NAME);
	  output_string (output_implementation_file, " - ");
	  output_string (output_implementation_file,
			 YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
	  output_string (output_implementation_file, "];\n");
	}
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        {
          /* `yylook_ahead_char = YYEMPTY;' */
          output_string (output_implementation_file, "                  ");
          output_string (output_implementation_file,
                         YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
          output_string (output_implementation_file, " = ");
          output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
          output_string (output_implementation_file, ";\n");
        }
      else
        {
          /*
	     if (yyerr_status <= 0)
               *yyfirst_char_ptr++ = YYEMPTY;
	     else
               {
	         yyfirst_char_ptr++;
#if YYDEBUG != 0
                 if (yydebug)
                   fprintf (stderr,
                            "Error recovery saving a token %d (%s)\n",
                            yychar, YYTOKEN_NAME (yychar));
#endif
               }
	     if (yyfirst_char_ptr >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
	       yyfirst_char_ptr = yylook_ahead_char;
	   */
	  output_string (output_implementation_file, "                  if (");
	  output_string (output_implementation_file,
			 YYERR_STATUS_VARIABLE_NAME);
	  output_string (output_implementation_file, " <= 0)\n");
	  output_string (output_implementation_file, "                    *");
	  output_string (output_implementation_file,
			 YYFIRST_CHAR_PTR_VARIABLE_NAME);
          output_string (output_implementation_file, "++ = ");
          output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
          output_string (output_implementation_file, ";\n");
          output_string (output_implementation_file,
			 "                  else\n");
          output_string (output_implementation_file,
			 "                    {\n");
	  output_string (output_implementation_file, "                      ");
	  output_string (output_implementation_file,
			 YYFIRST_CHAR_PTR_VARIABLE_NAME);
          output_string (output_implementation_file, "++;\n");
	  output_debug_print_about_saving_token (output_implementation_file,
						 "                      ");
          output_string (output_implementation_file,
			 "                    }\n");
	  output_check_yyfirst_char_ptr (output_implementation_file,
					 "                  ", FALSE);
        }
      output_string (output_implementation_file, "                }\n");
    }
  output_string (output_implementation_file, "            }\n");
  /* #if YYDEBUG != 0
       if (yydebug)
         {
           fprintf (stderr, "Now input is at %d (%s)",
	            yychar, YYTOKEN_NAME (yychar));
---------------------- la > 2 || !yacc_error_recovery_flag--------------
           for (yyfirst_char_ptr_1 = yyfirst_char_ptr; *yyfirst_char_ptr_1 != YYEMPTY;)
	     {
	       fprintf (stderr, " %d (%s)", *yyfirst_char_ptr_1,
	                YYTOKEN_NAME (*yyfirst_char_ptr_1));
	       yyfirst_char_ptr_1++;
	       if (yyfirst_char_ptr_1 >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
	         yyfirst_char_ptr_1 = yylook_ahead_char;
	     }
-------------------- la == 2 -----------------------------------------
           if (yylook_ahead_char != YYEMPTY)
	     fprintf (stderr, " %d (%s)", yylook_ahead_char,
	              YYTOKEN_NAME (yylook_ahead_char));
---------------------------------------------------------------------
	   fprintf (stderr, "\n");
         }
     #endif */
  output_string (output_implementation_file, "#if ");
  output_string (output_implementation_file, YYDEBUG_MACRO_NAME);
  output_string (output_implementation_file, " != 0\n");
  output_string (output_implementation_file, "          if (");
  output_yydebug_variable_name (output_implementation_file);
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, "            {\n");
  output_string (output_implementation_file, "              ");
  output_string (output_implementation_file,
                 "fprintf (stderr, \"Now input is at %d (%s)\",\n");
  output_string (output_implementation_file, "                       ");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, ", ");
  output_string (output_implementation_file, YYTOKEN_NAME_MACRO_NAME);
  output_string (output_implementation_file, " (");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, "));\n");
  if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
    {
      output_string (output_implementation_file,
		     "              for (");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (output_implementation_file, "; *");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (output_implementation_file, " != ");
      output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
      output_string (output_implementation_file, ";)\n");
      output_string (output_implementation_file, "                {\n");
      output_string (output_implementation_file,
		     "                  fprintf (stderr, \" %d (%s)\", *");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (output_implementation_file, ",\n");
      output_string (output_implementation_file,
		     "                           ");
      output_string (output_implementation_file, YYTOKEN_NAME_MACRO_NAME);
      output_string (output_implementation_file, " (*");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (output_implementation_file, "));\n");
      output_string (output_implementation_file, "                  ");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
      output_string (output_implementation_file, "++;\n");
      output_check_yyfirst_char_ptr (output_implementation_file,
				     "                  ", TRUE);
      output_string (output_implementation_file, "                }\n");
    }
  else if (real_look_ahead_number == 2)
    {
      output_string (output_implementation_file, "              if (");
      output_string (output_implementation_file,
		     YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, " != ");
      output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
      output_string (output_implementation_file, ")\n");
      output_string (output_implementation_file,
		     "                fprintf (stderr, \" %d (%s)\", ");
      output_string (output_implementation_file,
		     YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, ",\n");
      output_string (output_implementation_file,
		     "                         ");
      output_string (output_implementation_file, YYTOKEN_NAME_MACRO_NAME);
      output_string (output_implementation_file, " (");
      output_string (output_implementation_file,
		     YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, "));\n");
    }
  output_string (output_implementation_file,
		 "              fprintf (stderr, \"\\n\");\n");
  output_string (output_implementation_file, "            }\n");
  output_string (output_implementation_file, "#endif\n");
  /*
----------------- scanner -------------
     if (yychar < 0)
----------------- no scanner ----------
     if (yychar <= 0)
---------------------------------------
       {
         yychar1 = yytranslate[0];
         yychar = YYEOF; /* To prevent repeated reading EOF * /
       }
   */
  output_string (output_implementation_file, "          if (");
  output_yychar_variable_name (output_implementation_file);
  if (IR_scanner_flag (description))
    output_string (output_implementation_file, " < 0)\n            {\n");
  else
    output_string (output_implementation_file, " <= 0)\n            {\n");
  output_string (output_implementation_file, "              ");
  output_string (output_implementation_file, YYCHAR1_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, TRANSLATE_VECTOR_NAME);
  output_string (output_implementation_file, " [0];\n");
  output_string (output_implementation_file, "              ");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, YYEOF_MACRO_NAME);
  output_string (output_implementation_file,
                 ";  /* To prevent repeated reading EOF */\n            }\n");
  /*
     else if (yychar > YYLAST_TOKEN_CODE)
       YYABORT;
   */
  output_string (output_implementation_file, "          else if (");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, " > ");
  output_string (output_implementation_file, LAST_TOKEN_VALUE_MACRO_NAME);
  output_string (output_implementation_file, ")\n            ");
  output_string (output_implementation_file, YYABORT_MACRO_NAME);
  output_string (output_implementation_file, ";\n");
  /*
     else
       yychar1 = yytranslate [yychar];
   */
  output_string (output_implementation_file, "          else\n");
  output_string (output_implementation_file, "            ");
  output_string (output_implementation_file, YYCHAR1_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, TRANSLATE_VECTOR_NAME);
  output_string (output_implementation_file, " [");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, "];\n");
  /* 
     yytemp += yychar1;
     if (yyacheck[yytemp] != yystate)
       yytemp = yyadefault[yystate];
     else
       yytemp = yyaction[yytemp];
   */
  output_string (output_implementation_file, "          ");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, " += ");
  output_string (output_implementation_file, YYCHAR1_VARIABLE_NAME);
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "          if (");
  output_string (output_implementation_file, ACTION_CHECK_VECTOR_NAME);
  output_string (output_implementation_file, " [");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, "] != ");
  output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
  output_string (output_implementation_file, ")\n");
  output_string (output_implementation_file, "            ");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, ACTION_DEFAULT_VECTOR_NAME);
  output_string (output_implementation_file, " [");
  output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
  output_string (output_implementation_file, "];\n");
  output_string (output_implementation_file, "          else\n");
  output_string (output_implementation_file, "            ");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, ACTION_COMB_VECTOR_NAME);
  output_string (output_implementation_file, " [");
  output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
  output_string (output_implementation_file, "];\n");
  output_string (output_implementation_file, "        }\n");
  if (real_look_ahead_number >= 2)
    {
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        {
          /* `if (yytemp >= YY1LOOK_AHEAD_TABLE_VALUE)' */
          output_string (output_implementation_file, "      if (");
          output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
          output_string (output_implementation_file, " >= ");
          output_string (output_implementation_file,
                         FIRST_LOOK_AHEAD_TABLE_VALUE_MACRO_NAME);
          output_string (output_implementation_file, ")\n");
        }
      else
        {
          /*
             yyfirst_char_ptr_1 = yyfirst_char_ptr;
             while (yytemp >= YY1LOOK_AHEAD_TABLE_VALUE)
           */
          output_string (output_implementation_file, "      ");
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
          output_string (output_implementation_file, " = ");
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_VARIABLE_NAME);
          output_string (output_implementation_file, ";\n");
          output_string (output_implementation_file, "      while (");
          output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
          output_string (output_implementation_file, " >= ");
          output_string (output_implementation_file,
                         FIRST_LOOK_AHEAD_TABLE_VALUE_MACRO_NAME);
          output_string (output_implementation_file, ")\n");
        }
      output_string (output_implementation_file, "        {\n");
      /*
         yystate = yytemp + YYLOOK_AHEAD_TABLE_BASE;
         yytemp = yyabase[yystate];
       */
      output_string (output_implementation_file, "          ");
      output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " + ");
      output_string (output_implementation_file,
                     LOOK_AHEAD_TABLE_BASE_MACRO_NAME);
      output_string (output_implementation_file, ";\n");
            output_string (output_implementation_file, "          ");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, ACTION_BASE_VECTOR_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
      output_string (output_implementation_file, "];\n");
      /*
         if (yytemp == YYNO_ACTION_BASE)
           yytemp = yyadefault [yystate];
       */
      output_string (output_implementation_file, "          if (");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " == ");
      output_string (output_implementation_file, NO_ACTION_BASE_MACRO_NAME);
      output_string (output_implementation_file, ")\n");
      output_string (output_implementation_file, "            ");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, ACTION_DEFAULT_VECTOR_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
      output_string (output_implementation_file, "];\n");
      /* 
         else
           {
       */
      output_string (output_implementation_file,
                     "          else\n            {\n");
      /*
         if (yylook_ahead_char == YYEMPTY)
           {
             yysaved_lval = yylval;
             yylook_ahead_char = yylex ();
             or
             yyslook_ahead_char = yyslex ();
#if YYDEBUG != 0
             if (yydebug)
               fprintf(stderr, "Reading a look ahead token %d (%s)\n",
                       yylook_ahead_char, YYTOKEN_NAME (yylook_ahead_char));
#endif
             yylook_ahead_attribute = yylval;
             yylval = yysaved_lval;
           }
                  or
         if (*yyfirst_char_ptr_1 == YYEMPTY)
           {
             yysaved_lval = yylval;
             *yyfirst_char_ptr_1 = yylex ();
             or
             *yysfirst_char_ptr_1 = yyslex ();
#if YYDEBUG != 0
             if (yydebug)
               fprintf(stderr, "Reading a look ahead token %d (%s)\n",
                       *yyfirst_char_ptr_1, YYTOKEN_NAME (*yyfirst_char_ptr_1));
#endif
             yylook_ahead_attribute [yyfirst_char_ptr_1 - yylook_ahead_char]
               = yylval;
             yylval = yysaved_lval;
           }
       */
      output_string (output_implementation_file, "              if (");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file,
                       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
        {
          output_char ('*', output_implementation_file);
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
        }
      output_string (output_implementation_file, " == ");
      output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
      output_string (output_implementation_file, ")\n                {\n");
      output_string (output_implementation_file, "                  ");
      output_string (output_implementation_file, YYSAVED_LVAL_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_yylval_variable_name (output_implementation_file);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "                  ");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file,
                       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
        {
          output_char ('*', output_implementation_file);
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
        }
      output_string (output_implementation_file, " = ");
      output_yylex_function_name (output_implementation_file);
      output_string (output_implementation_file, " ();\n");
      if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
	{
	  output_string (output_implementation_file, "                  if (");
          output_string (output_implementation_file,
			 YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
	  output_string (output_implementation_file, " >= ");
          output_string (output_implementation_file,
			 YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
	  output_string (output_implementation_file, " + (");
          output_string (output_implementation_file,
			 YYLOOK_AHEAD_SIZE_MACRO_NAME);
	  output_string (output_implementation_file, " - 1))\n");
	  output_string (output_implementation_file, "                    *");
          output_string (output_implementation_file,
			 YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
	  output_string (output_implementation_file, " = ");
          output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
	  output_string (output_implementation_file, ";\n");
	  output_string (output_implementation_file,
			 "                  else\n");
	  output_string (output_implementation_file, "                    *");
          output_string (output_implementation_file,
			 YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
	  output_string (output_implementation_file, " = ");
          output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
	  output_string (output_implementation_file, ";\n");
	}
      output_string (output_implementation_file, "#if ");
      output_string (output_implementation_file, YYDEBUG_MACRO_NAME);
      output_string (output_implementation_file, " != 0\n");
      output_string (output_implementation_file, "                  if (");
      output_yydebug_variable_name (output_implementation_file);
      output_string (output_implementation_file, ")\n");
      output_string
        (output_implementation_file, "                    ");
      output_string
        (output_implementation_file,
         "fprintf (stderr, \"Reading a look ahead token %d (%s)\\n\",\n");
      output_string
        (output_implementation_file, "                             ");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file,
                       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
        {
          output_char ('*', output_implementation_file);
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
        }
      output_string (output_implementation_file, ", ");
      output_string (output_implementation_file, YYTOKEN_NAME_MACRO_NAME);
      output_string (output_implementation_file, " (");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file,
                       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
        {
          output_char ('*', output_implementation_file);
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
        }
      output_string (output_implementation_file, "));\n");
      output_string (output_implementation_file, "#endif\n");
      output_string (output_implementation_file, "                  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
      if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
        {
          output_string (output_implementation_file, " [");
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
          output_string (output_implementation_file, " - ");
          output_string (output_implementation_file,
                         YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
          output_string (output_implementation_file, "]");
        }
      output_string (output_implementation_file, " = ");
      output_yylval_variable_name (output_implementation_file);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "                  ");
      output_yylval_variable_name (output_implementation_file);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, YYSAVED_LVAL_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "                }\n");
      /*
---------------- scanner ------------
         if (yylook_ahead_char < 0)
---------------- no scanner ---------
         if (yylook_ahead_char <= 0)
-------------------------------------
           {
             yychar1 = 0;
             yylook_ahead_char = YYEOF; /* To prevent repeated reading EOF * /
           }
                    or 
---------------- scanner ------------
         if (*yyfirst_char_ptr_1 < 0)
---------------- no scanner ---------
         if (*yyfirst_char_ptr_1 <= 0)
-------------------------------------
           {
             yychar1 = 0;
             *yyfirst_char_ptr_1 = YYEOF; /* To prevent repeated reading EOF * /
            }
       */
      output_string (output_implementation_file, "              if (");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file,
                       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
        {
          output_char ('*', output_implementation_file);
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
        }
      if (IR_scanner_flag (description))
        output_string (output_implementation_file, " < 0)\n");
      else
        output_string (output_implementation_file, " <= 0)\n");
      output_string (output_implementation_file, "                {\n");
      output_string (output_implementation_file, "                  ");
      output_string (output_implementation_file, YYCHAR1_VARIABLE_NAME);
      output_string (output_implementation_file, " = 0;\n");
      output_string (output_implementation_file, "                  ");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file,
                       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
        {
          output_char ('*', output_implementation_file);
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
        }
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, YYEOF_MACRO_NAME);
      output_string (output_implementation_file,
                     "; /* To prevent repeated reading EOF */\n");
      output_string (output_implementation_file, "                }\n");
      /*
         else if (yylook_ahead_char > YYLAST_TOKEN_CODE)
           YYABORT;
               or
         else if (*yyfirst_char_ptr_1 > YYLAST_TOKEN_CODE)
           YYABORT;
       */
      output_string (output_implementation_file, "              else if (");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file,
                       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
        {
          output_char ('*', output_implementation_file);
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
        }
      output_string (output_implementation_file, " > ");
      output_string (output_implementation_file,
                     LAST_TOKEN_VALUE_MACRO_NAME);
      output_string (output_implementation_file, ")\n");
      output_string (output_implementation_file, "                ");
      output_string (output_implementation_file, YYABORT_MACRO_NAME);
      output_string (output_implementation_file, ";\n");
      /*
         else
           yychar1 = yytranslate [yylook_ahead_char];
                         or
         else
           yychar1 = yytranslate [*yyfirst_char_ptr_1];
       */
      output_string (output_implementation_file, "              else\n");
      output_string (output_implementation_file, "                ");
      output_string (output_implementation_file, YYCHAR1_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, TRANSLATE_VECTOR_NAME);
      output_string (output_implementation_file, " [");
      if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
        output_string (output_implementation_file,
                       YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      else
        {
          output_char ('*', output_implementation_file);
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
        }
      output_string (output_implementation_file, "];\n");
      /* 
         yytemp += yychar1;
         if (yyacheck[yytemp] != yystate)
           yytemp = yyadefault[yystate];
         else
           yytemp = yyaction[yytemp];
       */
      output_string (output_implementation_file, "              ");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " += ");
      output_string (output_implementation_file, YYCHAR1_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
      output_string (output_implementation_file, "              if (");
      output_string (output_implementation_file, ACTION_CHECK_VECTOR_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, "] != ");
      output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
      output_string (output_implementation_file, ")\n");
      output_string (output_implementation_file, "                ");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, ACTION_DEFAULT_VECTOR_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
      output_string (output_implementation_file, "];\n");
      output_string (output_implementation_file, "              else\n");
      output_string (output_implementation_file, "                ");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, ACTION_COMB_VECTOR_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, "];\n");
      if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
        {
          /* yyfirst_char_ptr_1++;
	     if (yyfirst_char_ptr_1 >= yylook_ahead_char + YYLOOK_AHEAD_SIZE)
	       yyfirst_char_ptr_1 = yylook_ahead_char;
	  */
          output_string (output_implementation_file, "              ");
          output_string (output_implementation_file,
                         YYFIRST_CHAR_PTR_1_VARIABLE_NAME);
          output_string (output_implementation_file, "++;\n");
	  output_check_yyfirst_char_ptr (output_implementation_file,
					 "              ", TRUE);
        }
      output_string (output_implementation_file, "            }\n");
      output_string (output_implementation_file, "        }\n");
    }
}

static void
output_initiation_code (void)
{
  /* `yynerrs = 0;' */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYNERRS_VARIABLE_NAME);
  output_string (output_implementation_file, " = 0;\n");
  /* `yychar = YYEMPTY;'. */
  output_string (output_implementation_file, "  ");
  output_yychar_variable_name (output_implementation_file);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
  output_string (output_implementation_file, ";\n");
  if (real_look_ahead_number == 2 && yacc_error_recovery_flag)
    {
      /* `yylook_ahead_char = YYEMPTY;' */
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
      output_string (output_implementation_file, ";\n");
    }
  else if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
    {
      /* yyfirst_char_ptr = yylook_ahead_char; */
      output_string (output_implementation_file, "  ");
      output_string (output_implementation_file,
		     YYFIRST_CHAR_PTR_VARIABLE_NAME);
      output_string (output_implementation_file, " = ");
      output_string (output_implementation_file,
		     YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
      /* for (yytemp = 0; yytemp < YYLOOK_AHEAD_SIZE; yytemp++)
           yylook_ahead_char [yytemp] = YYEMPTY; */
      output_string (output_implementation_file, "  for (");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " = 0; ");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, " < ");
      output_string (output_implementation_file, YYLOOK_AHEAD_SIZE_MACRO_NAME);
      output_string (output_implementation_file, "; ");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, "++)\n");
      output_string (output_implementation_file, "    ");
      output_string (output_implementation_file,
                     YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_string (output_implementation_file, " [");
      output_string (output_implementation_file, YYTEMP_VARIABLE_NAME);
      output_string (output_implementation_file, "] = ");
      output_string (output_implementation_file, YYEMPTY_MACRO_NAME);
      output_string (output_implementation_file, ";\n");
    }
}

static void
output_scanner_array_allocation (const char *array_name, const char *size,
				 const char *type, const char *error_flag_name,
				 int error_flag_dereference)
{
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, array_name);
  output_string (output_implementation_file, " = (");
  output_string (output_implementation_file, type);
  output_string (output_implementation_file, " *) ");
  output_string (output_implementation_file, YYALLOC_MACRO_NAME);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, size);
  output_string (output_implementation_file, " * sizeof (");
  output_string (output_implementation_file, type);
  output_string (output_implementation_file, "));\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, array_name);
  output_string (output_implementation_file, " == NULL)\n");
  output_string (output_implementation_file, "    {\n");
  output_string (output_implementation_file, "      ");
  if (error_flag_dereference)
    output_string (output_implementation_file, "*");
  output_string (output_implementation_file, error_flag_name);
  output_string (output_implementation_file, " = 1;\n");
  output_string (output_implementation_file, "      return;\n");
  output_string (output_implementation_file, "    }\n");

}

static void
output_scanner_array_variables_allocation (void)
{
  output_scanner_array_allocation (YYSTATES_VARIABLE_NAME,
				   YYSTACK_SIZE_MACRO_NAME,
				   "int",
				   YYSCANNER_CONSTRUCTOR_ERROR_FLAG_NAME,
				   !cpp_flag);
  output_scanner_array_allocation
    (YYATTRIBUTES_VARIABLE_NAME, YYSTACK_SIZE_MACRO_NAME, YYSTYPE_MACRO_NAME,
     YYSCANNER_CONSTRUCTOR_ERROR_FLAG_NAME, !cpp_flag);
  if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
    {
      output_scanner_array_allocation
	(YYLOOK_AHEAD_CHAR_VARIABLE_NAME, YYLOOK_AHEAD_SIZE_MACRO_NAME,
	 "int", YYSCANNER_CONSTRUCTOR_ERROR_FLAG_NAME, !cpp_flag);
      output_scanner_array_allocation
	(YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME, YYLOOK_AHEAD_SIZE_MACRO_NAME,
	 YYSTYPE_MACRO_NAME, YYSCANNER_CONSTRUCTOR_ERROR_FLAG_NAME, !cpp_flag);
    }
}

static void
output_parser_array_allocation (const char *array_name, const char *size,
				const char *type, const char *message)
{
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, array_name);
  output_string (output_implementation_file, " = (");
  output_string (output_implementation_file, type);
  output_string (output_implementation_file, " *) ");
  output_string (output_implementation_file, YYALLOC_MACRO_NAME);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, size);
  output_string (output_implementation_file, " * sizeof (");
  output_string (output_implementation_file, type);
  output_string (output_implementation_file, "));\n");
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, array_name);
  output_string (output_implementation_file, " == NULL)\n");
  output_string (output_implementation_file, "    {\n");
  output_string (output_implementation_file, "      ");
  output_yyerror_function_name (output_implementation_file);
  output_string (output_implementation_file, " (\"");
  output_string (output_implementation_file, message);
  output_string (output_implementation_file, "\");\n");
  output_string (output_implementation_file, "      ");
  output_string (output_implementation_file,
		 (IR_scanner_flag (description) && !cpp_flag
		  ? "return 1" : YYABORT_MACRO_NAME));
  output_string (output_implementation_file, ";\n");
  output_string (output_implementation_file, "    }\n");

}

static void
output_parser_array_variables_allocation (void)
{
  output_parser_array_allocation (YYSTATES_VARIABLE_NAME,
				  YYSTACK_SIZE_MACRO_NAME,
				  "int", "no memory for states stack");
  output_parser_array_allocation (YYATTRIBUTES_VARIABLE_NAME,
				  YYSTACK_SIZE_MACRO_NAME, YYSTYPE_MACRO_NAME,
				  "no memory for attributes stack");
  if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
    {
      output_parser_array_allocation
	(YYLOOK_AHEAD_CHAR_VARIABLE_NAME, YYLOOK_AHEAD_SIZE_MACRO_NAME,
	 "int", "no memory for look ahead tokens");
      output_parser_array_allocation
	(YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME, YYLOOK_AHEAD_SIZE_MACRO_NAME,
	 YYSTYPE_MACRO_NAME, "no memory for look ahead token attributes");
    }
}

static void
output_free_array (const char *free_array)
{
  /*   if (free_array != NULL)
         YYFREE (free_array);
  */
  output_string (output_implementation_file, "  if (");
  output_string (output_implementation_file, free_array);
  output_string (output_implementation_file, " != NULL)\n");
  output_string (output_implementation_file, "    ");
  output_string (output_implementation_file, YYFREE_MACRO_NAME);
  output_string (output_implementation_file, " (");
  output_string (output_implementation_file, free_array);
  output_string (output_implementation_file, ");\n");
}

static void
output_free_arrays (void)
{
  output_free_array (YYSTATES_VARIABLE_NAME);
  output_free_array (YYATTRIBUTES_VARIABLE_NAME);
  if (real_look_ahead_number > 2 || !yacc_error_recovery_flag)
    {
      output_free_array (YYLOOK_AHEAD_CHAR_VARIABLE_NAME);
      output_free_array (YYLOOK_AHEAD_ATTRIBUTE_VARIABLE_NAME);
    }
}

static void
output_parser_itself (void)
{
  output_definitions_outside_yyparse ();
  if (expand_flag)
    {
      output_state_or_attribute_stack_expansion_function (TRUE);
      output_state_or_attribute_stack_expansion_function (FALSE);
    }
  if (IR_scanner_flag (description))
    {
      output_yylex_start_title (output_implementation_file, FALSE);
      output_string (output_implementation_file, "\n{\n");
      output_definition_yytemp_variable ();
      output_string (output_implementation_file, "\n");
      output_scanner_array_variables_allocation ();
      output_initiation_code ();
      output_string (output_implementation_file, "  ");
      if (!cpp_flag)
	output_string (output_implementation_file, "*");
      output_string (output_implementation_file,
		     YYSCANNER_CONSTRUCTOR_ERROR_FLAG_NAME);
      output_string (output_implementation_file, " = 0;\n");
      output_string (output_implementation_file, "  return;\n");
      output_string (output_implementation_file, "}\n\n");
    }
  if (IR_scanner_flag (description))
    {
      output_yylex_finish_title (output_implementation_file, FALSE);
      output_string (output_implementation_file, "\n{\n");
      output_free_arrays ();
      output_string (output_implementation_file, "}\n\n");
    }
  output_yyparse_title (output_implementation_file, FALSE);
  output_string (output_implementation_file, "\n{\n");
  output_definition_inside_yyparse ();
  /*
     #if YYDEBUG != 0
        if (yydebug)
          fprintf (stderr, "Starting parse\n");
     #endif */
  output_string (output_implementation_file, "#if ");
  output_string (output_implementation_file, YYDEBUG_MACRO_NAME);
  output_string (output_implementation_file, " != 0\n");
  output_string (output_implementation_file, "  if (");
  output_yydebug_variable_name (output_implementation_file);
  output_string (output_implementation_file,
                 ")\n    fprintf (stderr, \"Starting parse\\n\");\n");
  output_string (output_implementation_file, "#endif\n");
  /* allocation of arrays in parser. */
  if (!IR_scanner_flag (description))
    output_parser_array_variables_allocation ();
  /*
     yystates_end = yystates + YYSTACK_SIZE - 1;
     yystates_top = yystates - 1;
  */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYSTATES_END_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, YYSTATES_VARIABLE_NAME);
  output_string (output_implementation_file, " + ");
  output_string (output_implementation_file, YYSTACK_SIZE_MACRO_NAME);
  output_string (output_implementation_file, " - 1;\n");
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYSTATES_TOP_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, YYSTATES_VARIABLE_NAME);
  output_string (output_implementation_file, " - 1;\n");
  /*
     yyattributes_end = yyattributes + YYSTACK_SIZE - 1;
     yyattributes_top = yyattributes - 1;
  */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYATTRIBUTES_END_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, YYATTRIBUTES_VARIABLE_NAME);
  output_string (output_implementation_file, " + ");
  output_string (output_implementation_file, YYSTACK_SIZE_MACRO_NAME);
  output_string (output_implementation_file, " - 1;\n");
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYATTRIBUTES_TOP_VARIABLE_NAME);
  output_string (output_implementation_file, " = ");
  output_string (output_implementation_file, YYATTRIBUTES_VARIABLE_NAME);
  output_string (output_implementation_file, " - 1;\n");
  if (!IR_scanner_flag (description))
    output_initiation_code ();
  /* `yystate = 0; /* Start state * /' */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
  output_string (output_implementation_file, " = 0; /* Start state */\n");
  if (IR_it_is_pushed_LR_set (IR_LR_set_list (IR_LR_core_list (description))))
    {
      /* (*++yystates_top) = yystate; */
      output_string (output_implementation_file, "  (*++");
      output_string (output_implementation_file, YYSTATES_TOP_VARIABLE_NAME);
      output_string (output_implementation_file, ") = ");
      output_string (output_implementation_file, YYSTATE_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
    }
  if (IR_attribute_is_used (IR_LR_set_list (IR_LR_core_list (description))))
    {
      /* ++yyattributes_top; */
      output_string (output_implementation_file, "  ++");
      output_string (output_implementation_file,
                     YYATTRIBUTES_TOP_VARIABLE_NAME);
      output_string (output_implementation_file, ";\n");
    }
  /* `yyerr_status = (-1);' */
  output_string (output_implementation_file, "  ");
  output_string (output_implementation_file, YYERR_STATUS_VARIABLE_NAME);
  output_string (output_implementation_file, " = (-1);\n");
  /* Main cycle start. */
  output_string (output_implementation_file, "  for (;;)\n    {\n");
  output_code_before_switch ();
  /* Switch. */
  output_switch ();
  output_string (output_implementation_file, "    }\n\n");
  /* code for yyaccept, yyabort */
  output_string (output_implementation_file, YYACCEPT_LABEL_NAME);
  output_string (output_implementation_file, ":\n");
  if (!IR_scanner_flag (description))
    output_free_arrays ();
  output_string (output_implementation_file, "  return 0;\n\n");
  output_string (output_implementation_file, YYABORT_LABEL_NAME);
  output_string (output_implementation_file, ":\n");
  if (!IR_scanner_flag (description))
    output_free_arrays ();
  output_string (output_implementation_file, "  return ");
  output_string (output_implementation_file,
		 IR_scanner_flag (description) ? "-1" : "1");
  output_string (output_implementation_file, ";\n\n");
  output_string (output_implementation_file, "}\n\n");
}

static void
output_additional_code (void)
{
  IR_node_t additional_code;

  additional_code = IR_additional_code (description);
  if (additional_code == NULL)
    return;
  assert (IR_IS_OF_TYPE (additional_code, IR_NM_additional_code));
  output_char ('\n', output_implementation_file);
  output_line (output_implementation_file,
               IR_position (additional_code).line_number,
               IR_position (additional_code).file_name);
  output_string (output_implementation_file,
                 IR_additional_code_itself (additional_code));
  output_char ('\n', output_implementation_file);
  output_current_line (output_implementation_file);
}

void
output_parser (void)
{
  ticker_t all_output_parser_ticker;
  ticker_t tables_ticker;

  all_output_parser_ticker = create_ticker ();
  output_msta_title ();
  output_start_code_insertions ();
  output_external_definitions ();
  output_finish_code_insertions ();
  /* Definition of `YYDEBUG'. */
  output_string (output_implementation_file, "#ifndef  ");
  output_string (output_implementation_file, YYDEBUG_MACRO_NAME);
  output_string (output_implementation_file, "\n#define  ");
  output_string (output_implementation_file, YYDEBUG_MACRO_NAME);
  output_string (output_implementation_file, (trace_flag ? " 1\n" : " 0\n"));
  output_string (output_implementation_file, "#endif\n\n");
  prepare_tables_output ();
  tables_ticker = create_ticker ();
  output_parser_tables ();
  if (time_flag)
    fprintf (stderr, "    creation, compacting, output of tables -- %ssec\n",
             active_time_string (tables_ticker));
  output_include_directives ();
  output_parser_itself ();
  output_additional_code ();
  if (time_flag)
    fprintf (stderr, "  all parser output -- %ssec\n",
             active_time_string (all_output_parser_ticker));
}
