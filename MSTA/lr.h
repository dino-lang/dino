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
