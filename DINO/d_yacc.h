extern IR_node_t first_program_stmt;

#include "position.h"

extern void initiate_scanner (void);
extern void start_scanner_file (const char *file_name, position_t error_pos);
extern yyparse (void);
extern void finish_scanner (void);
