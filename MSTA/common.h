#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#endif

#include "ird.h"

/* Standard designators for true and false values. */

#define FALSE 0
#define TRUE  1

/* Suffix which any syntax description file name must have.  If this
   macro value is changed then documentation of MSTA must be changed. */

#define STANDARD_INPUT_FILE_SUFFIX ".y"

/* This macro value is used for correct calculation of current position in
   file when TAB character is processed. */

#define TAB_STOP 8

extern int define_flag;
extern int line_flag;
extern int trace_flag;
extern int verbose_flag;
extern const char *file_prefix;
extern const char *sym_prefix;
extern int w_flag;
extern int cpp_flag;
extern int enum_flag;
extern int pattern_equiv_flag;
extern int full_lr_set_flag;
extern int lr_situation_context_flag;
extern int removed_lr_sets_flag;
extern int look_ahead_number;
extern int lr_flag;
extern int lalr_optimization_flag;
extern int regular_optimization_flag;
extern int split_lr_sets_flag;
extern int split_lr_sets_flag_is_defined;
extern int yacc_error_recovery_flag;
extern int yacc_input_flag;
extern int strict_flag;
extern int yacc_file_names_flag;
extern int expand_flag;
extern int time_flag;
#ifndef NDEBUG
extern int debug_level;
#endif
extern const char *output_files_name;
extern const char *source_file_name;

extern FILE *output_description_file;
extern char *output_description_file_name;

extern FILE *output_interface_file;
extern char *output_interface_file_name;

extern FILE *output_implementation_file;
extern char *output_implementation_file_name;

extern FILE *output_description_file;
extern char *output_description_file_name;

extern IR_node_t description;

extern int max_look_ahead_number;
