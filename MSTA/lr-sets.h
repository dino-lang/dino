void initiate_LR_situations (void);
IR_node_t get_new_LR_situation (IR_node_t element_after_dot, context_t context,
                                IR_node_t LR_set, IR_node_t next_LR_situation,
                                int important_LR_situation_flag);
void free_LR_situation (IR_node_t LR_situation);
void free_LR_situations_list (IR_node_t LR_situations_list);
void output_LR_situation (FILE *f, IR_node_t LR_situation,
                          const char *indent, int new_line_flag);
void finish_LR_situations (void);

IR_node_t insert_LR_core (IR_node_t LR_core);
IR_node_t find_LR_core (IR_node_t LR_situation_list);
void initiate_LR_core_table (void);
void finish_LR_core_table (void);

IR_node_t insert_LR_set (IR_node_t LR_set);
IR_node_t find_LR_set (IR_node_t LR_core, IR_node_t LR_situation_list);
void delete_LR_set_from_table (IR_node_t LR_set);
void initiate_LR_set_table (void);
void finish_LR_set_table (void);
void output_LR_set_situations (FILE *f, IR_node_t LR_set, const char *indent);

