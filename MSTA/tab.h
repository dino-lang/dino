#include "ird.h"

/* Table of strings and identifiers. */

extern char *insert_string (const char *string);
extern void initiate_string_table (void);
extern void finish_string_table (void);

/* Table of single definitions. */

extern IR_node_t insert_single_definition (IR_node_t single_definition);
extern IR_node_t find_single_definition (IR_node_t identifier_or_literal);
extern void initiate_single_definition_table (void);
extern void finish_single_definition_table (void);

/* Table of literal definitions (single term definition denoting a
   literal and literal range definition) with key `values'. */

extern IR_node_t insert_literal_definition (IR_node_t token_definition);
extern IR_node_t find_literal_definition (int left_range_value,
                                          int right_range_value);
extern void initiate_literal_definition_table (void);
extern void finish_literal_definition_table (void);

/* Table of values of terminal (including literal range)
   definitions. */

extern IR_node_t insert_token_code_value (IR_node_t token_definition,
                                          int value);
extern void insert_token_code_value_with_overwriting
              (IR_node_t token_definition, int value);
extern IR_node_t find_token_code_value (int value);
extern void initiate_token_code_value_table (void);
extern void finish_token_code_value_table (void);

