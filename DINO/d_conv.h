#include "d_run.h"

extern void to_vect_string_conversion (ER_node_t var, const char *format);
extern void implicit_arithmetic_conversion (int depth);
extern void implicit_conversion_for_binary_arithmetic_op (void);
extern void implicit_int_conversion (int depth);
extern void implicit_conversion_for_binary_int_op (void);
extern void implicit_conversion_for_binary_string_op (void);
extern void implicit_conversion_for_eq_op (void);
