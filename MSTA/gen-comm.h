extern IR_node_t end_marker_single_definition;
extern int real_look_ahead_number;

int canonical_rule_right_hand_side_prefix_length
           (IR_node_t canonical_rule, IR_node_t bound_right_hand_side_element);
void traverse_all_LR_set_predecessor_paths
                       (IR_node_t LR_set, int path_length,
                        int (*applied_function) (IR_node_t LR_set),
                        int start_length_for_function);
void initiate_traverse_cache (void);
void traverse_all_LR_set_predecessors
                       (IR_node_t LR_set, int path_length,
                        int (*applied_function) (IR_node_t LR_set));
void traverse_cache_off (void);
void traverse_cache_on (void);
void finish_traverse_cache (void);
void reverse_traverse_all_LR_set_predecessor_paths
                       (IR_node_t LR_set, int path_length,
                        void (*applied_function) (IR_node_t LR_set),
                        int start_length_for_function);
IR_node_t characteristic_symbol_of_LR_set (IR_node_t LR_set);
void initiate_goto_set_cache (void);
IR_node_t find_goto_LR_situation (IR_node_t LR_set,
                                  IR_node_t single_definition);
IR_node_t goto_by_nonterminal (IR_node_t LR_set, IR_node_t single_definition);
void finish_goto_set_cache (void);
void LR_set_conflicts_number (IR_node_t LR_set,
                              int *shift_reduce_conflicts_number,
                              int *reduce_reduce_conflicts_number);
int attribute_name_to_attribute_number
                             (const char *attribute_name,
                              IR_node_t canonical_rule,
                              IR_node_t bound_right_hand_side_element);
IR_node_t get_the_single_LR_set_predecessor (IR_node_t LR_set,
                                             int path_length);
int pushed_LR_sets_or_attributes_number_on_path (IR_node_t LR_set, int length,
                                                 int attribute_flag);
