void output_string (FILE *f, const char *string);
void output_char (int ch, FILE *f);
void output_decimal_number (FILE *f, int number, int minimum_width);
void initiate_output (void);
int identifier_or_literal_representation (IR_node_t identifier_or_literal,
                                          int in_string_flag,
                                          vlo_t *representation);
int output_identifier_or_literal (FILE *f, IR_node_t identifier_or_literal,
                                  int in_string_flag);
void single_definition_representation (IR_node_t single_definition,
                                       vlo_t *representation);
void output_single_definition (FILE *f, IR_node_t single_definition);
void output_line (FILE *f, int line_number, const char *file_name);
void output_current_line (FILE *f);
