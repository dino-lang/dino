/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of interpreter of DINO.

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

*/

#include <stdarg.h>

#include "d_common.h"
#include "d_ir.h"

/* ERR is standard prefix for message which may be used by all error
   found before the evaluation.  DERR is one found during the
   evaluation. */

char ERR_no_memory[] = "no memory";
char ERR_source_file_encoding[]
  = "unknown or not implemented encoding `%s' of file `%s'";
char ERR_non_ascii_source_file_encoding[]
  = "encoding `%s' of file `%s' does not contain ascii byte codes";
char ERR_file_decoding[] = "wrong encoding `%s' in a source file";
char ERR_line_decoding[] = "wrong encoding `%s' during reading a line";
char ERR_file_name_cannot_represented_in_current_encoding[]
  = "can not represent string as file name using current encoding";
char ERR_invalid_char_constant[] = "invalid character constant";
char ERR_invalid_escape_code[] = "invalid escape hex or unicode code";
char ERR_invalid_input_char[] = "invalid symbol";
char ERR_octal_int_value[] = "octal int with non-octal digit";
char ERR_float_value_not_in_decimal_base[] = "float is not in decimal base";
char ERR_float_value[] = "too big or too small floating point value";
char ERR_int_value[] = "too big integer value";
char ERR_string_end_absence[] = "string end is absent";
char ERR_exponent_absence[] = "exponent of float is absent";
char ERR_eof_in_comment[] = "comment is not finished (EOF)";
char ERR_unknown_hint[] = "unknown hint %s";
char ERR_wrong_hint_for_non_fun[]
  = "inline or pure hint is permitted only for fun";
char ERR_jit_hint_for_thread[] = "jit hint is not permitted for thread";
char ERR_default_value_parameter_and_dots[]
  = "usage of parameter default value and `...'";
char ERR_default_value_parameters_should_be_last[]
  = "default value formal parameters should be the last ones";
char ERR_interrupt_exception[] = "interrupt";
char ERR_illegal_instruction_exception[] = "illegal instruction";
char ERR_abort_exception[] = "abort";
char ERR_floating_point_exception[] = "floating point exception";
char ERR_termination_exception[] = "termination";
char ERR_segment_access_violation_exception[]
  = "segment access violation";

char ERR_repeated_decl[] = "repeated declaration for identifier `%s'";
char ERR_forward_and_matched_decls_are_different_entities[]
  = "forward and matched declarations of `%s' are different entities";
char ERR_forward_and_matched_decls_have_different_attrs[]
  = "forward and matched decls of `%s' have different access/final attributes";
char ERR_previous_decl_location[]
  = "(previous declaration of identifier `%s')";
char ERR_undeclared_ident[] = "undeclared identifier `%s'";
char ERR_undeclared_ident_friend_list []
  = "there is no declaration for identifier `%s' in friend list";
char ERR_period_ident_applied_to_slice [] = "`.ident' is applied to slice";
char ERR_vec_tab_element_access_applied_to_slice []
  = "`[...]' is applied to slice";
char ERR_call_applied_to_slice [] = "call is applied to slice";
char ERR_slice_as_foreach_index_designator []
  = "vector slice is used as foreach-stmt index_designator";
char ERR_wildcard_in_expression [] = "`_' in expression";
char ERR_dots_in_expression [] = "`...' in expression";
char ERR_dots_in_the_list_middle [] = "`...' in the list middle";
char ERR_dots_with_repetition_key [] = "`...' with repetition/key";
char ERR_table_dots_as_key_with_value []
  = "`...' as key with value in the table pattern";
char ERR_assignment_pattern_without_variables []
  = "Pattern in assignment has no variables";
char ERR_invalid_friend []
  = "friend identifier `%s' is neither function nor class";
char ERR_no_public_declaration_in_object []
  = "no public declaration `%s' in the object";
char ERR_non_object_in_expose_clause []
  = "non object in exposing object internals";
char ERR_expose_clause_inside_referring_object []
  = "expose-clause inside refering object `%s'";
char ERR_non_object_in_use_qual_ident []
  = "%s is not an object in qualified identifier of use-clause";
char ERR_use_before_definition [] = "use `%s' before defined fun or class";
char ERR_use_of_non_fun_class [] = "use of non fun/class `%s'";
char ERR_use_of_forward_declaration []
  = "use of forward declaration of fun/class `%s'";
char ERR_use_of_final [] = "use of final fun/class `%s'";
char ERR_use_of_class_inside_the_class []
  = "use of fun/class `%s' inside the fun/class";
char ERR_repeated_use_item_ident_occurence_in_use []
  = "repeated occurrence of identifier `%s' in use items";
char ERR_previous_use_item_ident_location []
  = "(previous occurrence of identifier `%s')";
char ERR_undefined_use_item_ident []
  = "identifier `%s' is not found in the corresponding class/fun";
char ERR_used_decl_not_mentioned_in_former_redefines_prev_one []
  = "used decl redefines previous `%s' and is not mentioned in former item";
char ERR_alias_redefines_prev_one [] = "alias redefines previous `%s'";
char ERR_alias_redefinition [] = "alias `%s' is redefined";
char ERR_ident_in_former_has_no_prev_decl []
  = "ident `%s' in former item has no previous decl";
char ERR_used_decl_redefinition_not_mentioned_in_later []
  = "`%s' redefines an used decl not mentioned in later item";
char ERR_here_is_corresponding_use_clause []
  = "here is the corresponding use clause for `%s'";
char ERR_ident_in_former_item_is_not_declared_before_use []
  = "`%s' mentioned in former item is not declared before the use-clause";
char ERR_ident_in_later_item_is_not_declared_after_use []
  = "`%s' mentioned in later item is not declared after the use-clause";
char ERR_decl_is_absent_in_a_block []
  = "there is no such declaration in a block";
char ERR_invalid_type_of_arrow_left_operand []
  = "left operand of arrow must be of string type";
char ERR_invalid_type_of_deref_operand []
  = "operand of unary `*' must be of string type";
char ERR_invalid_logical_operation_operand_type []
  = "invalid type of operand in logical operation";
char ERR_invalid_order_comparison_operation_operand_type []
  = "invalid type of operand in order comparison operation (<, >, <=, >=)";
char ERR_invalid_concat_operation_operand_type []
  = "invalid type of operand in concat operation `@'";
char ERR_invalid_arithmetic_operation_operand_type []
  = "invalid operand type in (+, -, *, /, %, |, ^, &, ~, <<, >>, >>>)";
char ERR_invalid_fold_arithmetic_operation_operand_type []
  = "invalid operand type in (.+, .*, .|, .^, .&)";
char ERR_invalid_fold_concat_operation_operand_type []
  = "invalid operand type in .@";
char ERR_invalid_repetition_type []
  = "invalid type of repetition in vector (`[...]')";
char ERR_invalid_length_operand_type []
  = "invalid operand type in length operation `#'";
char ERR_invalid_conversion_to_char_operand_type []
  = "invalid operand type in char (...)";
char ERR_invalid_conversion_to_int_operand_type []
  = "invalid operand type in int (...)";
char ERR_invalid_conversion_to_long_operand_type []
  = "invalid operand type in long (...)";
char ERR_invalid_conversion_to_float_operand_type []
  = "invalid operand type in float (...)";
char ERR_invalid_conversion_to_vector_operand_type []
  = "invalid operand type vec (...)";
char ERR_invalid_conversion_format_type []
  = "invalid format type in vec (...)";
char ERR_invalid_conversion_to_table_operand_type []
  = "invalid operand type tab (...)";
char ERR_invalid_cond_type []
  = "invalid conition type in conditional expression (`...?...:...')";
char ERR_invalid_vec_slice_type []
  = "invalid type of vector in slice access (`[...]')";
char ERR_invalid_vec_tab_type []
  = "invalid type of vec/tab in elements access (`[...]')";
char ERR_invalid_tab_type []
  = "invalid type of table in table elements access `in'";
char ERR_invalid_slice_start_type []
  = "invalid type of start in vector slice access (`[:]')";
char ERR_invalid_slice_bound_type []
  = "invalid type of bound in vector slice access (`[:]')";
char ERR_invalid_slice_step_type []
  = "invalid type of step in vector slice access (`[:]')";
char ERR_invalid_class_fun_thread_designator []
  = "invalid class, function, or thread designator in class or function call";
char ERR_invalid_if_expr_type [] = "invalid type of if-expr";
char ERR_invalid_for_guard_expr_type [] = "invalid type of for-expr";
char ERR_invalid_wait_guard_expr_type [] = "invalid type of wait-expr";
char ERR_invalid_foreach_table_type [] = "invalid type of foreach-table";
char ERR_invalid_throw_expr_type [] = "invalid type of throw-expr";
char ERR_invalid_catch_expr_type [] = "invalid type of catch-expr";
char ERR_non_variable_in_assignment []
  = "non variable in assignment statement";
char ERR_non_variable_in_swap [] = "non variable in swap statement";
char ERR_const_assignment [] = "constant `%s' in assignment statement";
char ERR_const_swap [] = "constant `%s' in swap statement";
char ERR_non_variable_in_foreach_index []
  = "non variable in foreach-stmt index designator";
char ERR_continue_is_not_in_loop_or_case []
  = "statement continue is not in for-statement or match case";
char ERR_break_is_not_in_loop_or_case []
  = "statement break is not in for-statement or in match case";
char ERR_this_outside_fun_class [] = "this is not in function or class";
char ERR_return_outside_fun_class []
  = "statement return is not in function or class";
char ERR_return_with_result_in_class []
  = "statement return with result is in class";
char ERR_return_with_result_in_thread []
  = "statement return with result is in thread";
char ERR_function_call_in_wait_stmt []
  = "function call is present in wait statement";

/* Byte code specific errors:  */
char ERR_byte_code_should_start_with_label []
  = "byte code should start with label";
char ERR_byte_code_should_have_name [] = "byte code should have name";
char ERR_byte_code_field_should_have_name []
  = "byte code field should have name";
char ERR_byte_code_field_should_be_followed_by_eq []
  = "byte code field should be followed by eq";
char ERR_unknown_byte_code_node [] = "unknown byte code node %s";
char ERR_unknown_byte_code_field [] = "unknown byte code field";
char ERR_wrong_byte_code_field_value [] = "wrong byte code field value";
char ERR_undefined_byte_code_field [] = "undefined byte code field %s";
char ERR_node_has_no_such_field [] = "byte code node has no field %s";

/* Execution errors:  */
char DERR_environment_corrupted[]
  = "run time error - environment has been corrupted";
char DERR_identity_operands_types []
  = "run time error - invalid types of operands of operator \"===\" or \"!==\"";
char DERR_eq_operands_types []
  = "run time error - invalid types of operands of operator \"==\" or \"!=\"";
char DERR_logical_operands_types []
  = "run time error - invalid types of operands of logical operator";
char DERR_logical_or_operands_types []
  = "run time error - invalid types of operands of operator \"||\"";
char DERR_logical_and_operands_types []
  = "run time error - invalid types of operands of operator \"&&\"";
char DERR_cond_operand_type []
  = "run time error - invalid type of condition in operator \"? :\"";
char DERR_not_operand_type []
  = "run time error - invalid type of operand of operator \"!\"";
char DERR_bitwise_not_operand_type []
  = "run time error - invalid type of operand of operator \"~\"";
char DERR_lt_operands_types []
  = "run time error - invalid types of operands of operator \"<\"";
char DERR_gt_operands_types []
  = "run time error - invalid types of operands of operator \">\"";
char DERR_le_operands_types []
  = "run time error - invalid types of operands of operator \"<=\"";
char DERR_ge_operands_types []
  = "run time error - invalid types of operands of operator \">=\"";
char DERR_plus_operands_types []
  = "run time error - invalid types of operands of operator \"+\"";
char DERR_minus_operands_types []
  = "run time error - invalid types of operands of binary operator \"-\"";
char DERR_concat_operands_types []
  = "run time error - invalid types of operands of operator \"@\"";
char DERR_mult_operands_types []
  = "run time error - invalid types of operands of operator \"*\"";
char DERR_div_operands_types []
  = "run time error - invalid types of operands of operator \"/\"";
char DERR_mod_operands_types []
  = "run time error - invalid types of operands of operator \"%\"";
char DERR_lshift_operands_types []
  = "run time error - invalid types of operands of operator \"<<\"";
char DERR_rshift_operands_types []
  = "run time error - invalid types of operands of operator \">>\"";
char DERR_ashift_operands_types []
  = "run time error - invalid types of operands of operator \">>>\"";
char DERR_and_operands_types []
  = "run time error - invalid types of operands of operator \"&\"";
char DERR_xor_operands_types []
  = "run time error - invalid types of operands of operator \"^\"";
char DERR_or_operands_types []
  = "run time error - invalid types of operands of operator \"|\"";
char DERR_unary_plus_operand_type []
  = "run time error - invalid type of operand of unary operator \"+\"";
char DERR_unary_minus_operand_type []
  = "run time error - invalid type of operand of unary operator \"-\"";
char DERR_length_operand_type []
  = "run time error - invalid type of operand of operator \"#\"";
char DERR_conversion_to_char_operand_type []
  = "run time error - invalid type of operand of char (...)";
char DERR_conversion_to_int_operand_type []
  = "run time error - invalid type of operand of int (...)";
char DERR_long_is_too_big_for_conversion_to_int []
  = "run time error - long is too big for conversion to int";
char DERR_conversion_to_long_operand_type []
  = "run time error - invalid type of operand of long (...)";
char DERR_conversion_to_float_operand_type []
  = "run time error - invalid type of operand of float (...)";
char DERR_conversion_to_vector_operand_type []
  = "run time error - invalid type of operand of vec (...)";
char DERR_format_conversion_to_vector_operand_type []
  = "run time error - invalid type of operand of format vec (...)";
char DERR_vector_conversion_format_type []
  = "run time error - invalid type of format of vec (...)";
char DERR_conversion_to_table_operand_type []
  = "run time error - invalid type of operand of tab (...)";
char DERR_elist_repetition_type []
  = "run time error - invalid type of repetition";
char DERR_invalid_if_expr_type []
  = "run time error - invalid if-expression type";
char DERR_invalid_for_guard_expr_type []
  = "run time error - invalid for-guard expression type";
char DERR_invalid_wait_guard_expr_type []
  = "run time error - invalid wait-guard expression type";
char DERR_wait_in_sync_stmt [] = "wait in sync-statement";
char DERR_thread_call_in_sync_stmt [] = "thread call in sync-statement";
char DERR_no_exception_after_throw []
  = "run time - an except instance must be after throw";
char DERR_index_is_not_int []
  = "run time error - index is not integer";
char DERR_index_is_negative_number []
  = "run time error - index is negative number";
char DERR_index_is_greater_than_array_bound []
  = "run time error - index is greater than array bound";
char DERR_index_operation_for_non_vec_tab []
  = "run time error - index operation for non vector or table";
char DERR_slice_start_is_not_int []
  = "run time error - slice start is not integer (dimension = %d)";
char DERR_slice_bound_is_not_int []
  = "run time error - slice bound is not integer (dimension = %d)";
char DERR_slice_step_is_not_int []
  = "run time error - slice step is not integer (dimension = %d)";
char DERR_slice_start_is_negative []
  = "run time error - slice start is negative (dimension = %d)";
char DERR_slice_step_is_zero []
  = "run time error - slice step is zero (dimension = %d)";
char DERR_different_slice_operand_lengths []
  = "run time error - different lengths of slices %ld vs %ld (dimension = %d)";
char DERR_slice_operand_form []
  = "run time error - slice to wrongly formed vector (dimension = %d)";
char DERR_different_vec_operand_lengths []
  = "run time - different vector operand lengths: %ld vs. %ld (dimension = %d)";
char DERR_vector_form_type []
  = "run time error - wrong vector form for vector operation";
char DERR_matrix_form_type []
  = "run time error - wrong matrix form for matrix operation `%s'";
char DERR_repeated_key []
  = "run time error - repeated key #%lld in the table  (`tab [...]')";
char DERR_no_such_key []
  = "run time error - no such key in table";
char DERR_in_table_operand_type [] = "run time error - non table right to `in'";
char DERR_unfinished_fun_class_call []
  = "run time error - unfinished fun/class `%s' call";
char DERR_none_class_or_fun_before_left_bracket []
  = "run time error - none class or function is before \"(\"";
char DERR_parameter_type []
  = "run time error - invalid parameter type of `%s'";
char DERR_parameter_value []
  = "run time error - invalid parameter value for `%s'";
char DERR_invalid_result []
  = "run time error - invalid function result used by function `%s'";;
char DERR_invalid_input []
  = "run time error - invalid input read by function `%s'";;
char DERR_invalid_format [] = "run time error - invalid format in `%s'";
char DERR_too_big_ucode_for_byte_representation []
  = "run time error - unicode is too big to be represented by a byte for `%s'";
char DERR_in_ucode_encoding []
  = "run time error - wrong unicode for encoding used by `%s'";
char DERR_unexpected_input_encoding []
  = "run time error - unexpected input for encoding used by `%s'";
char DERR_non_ascii_default_encoding[]
  = "encoding %s given for `%s' does not contain ascii byte codes";
char DERR_eof_occured []
  = "run time error - EOF occured in `%s'";
char DERR_parameters_number []
  = "run time error - unexpected number of parameters for `%s'";
char DERR_too_few_actual_parameters []
  = "run time error - too few actual parameters for `%s'";
char DERR_too_many_actual_parameters []
  = "run time error - too many actual parameters for `%s'";
char DERR_eaccess [] = "system error - permission denied: `%s'";
char DERR_eagain [] = "system error - resource temporarily unavailable: `%s'";
char DERR_ebadf [] = "system error - bad file descriptor: `%s'";
char DERR_ebusy [] = "system error - resource busy: `%s'";
char DERR_echild [] = "system error - no child processes: `%s'";
char DERR_edeadlk [] = "system error - resource deadlock avoided: `%s'";
char DERR_edom [] = "system error - domain error: `%s'";
char DERR_eexist [] = "system error - file exists in `%s'";
char DERR_efault [] = "system error - bad address: `%s'";
char DERR_efbig [] = "system error - file too large: `%s'";
char DERR_eintr [] = "system error - interrupted function call: `%s'";
char DERR_einval [] = "system error - invalid argument: `%s'";
char DERR_eio [] = "system error - input/output: `%s'";
char DERR_eisdir [] = "system error - is a directory: `%s'";
char DERR_emfile [] = "system error - too many open files: `%s'";
char DERR_emlink [] = "system error - too many links: `%s'";
char DERR_enametoolong [] = "system error - filename too long: `%s'";
char DERR_enfile [] = "system error - too many open files in system: `%s'";
char DERR_enodev [] = "system error - no such device: `%s'";
char DERR_enoent [] = "system error - no such file or directory: `%s'";
char DERR_enoexec [] = "system error - exec format error: `%s'";
char DERR_enolck [] = "system error - no locks available: `%s'";
char DERR_enomem [] = "system error - not enough space: `%s'";
char DERR_enospc [] = "system error - no space left on device: `%s'";
char DERR_enosys [] = "system error - function not implemented: `%s'";
char DERR_enotdir [] = "system error - not a directory: `%s'";
char DERR_enotempty [] = "system error - directory not empty: `%s'";
char DERR_enotty []
  = "system error - inappropriate I/O control operation: `%s'";
char DERR_enxio [] = "system error - no such device or address: `%s'";
char DERR_eperm [] = "system error - operation not permitted: `%s'";
char DERR_epipe [] = "system error - broken pipe: `%s'";
char DERR_erange [] = "system error - result too big (small): `%s'";
char DERR_erofs [] = "system error - read-only file system: `%s'";
char DERR_espipe [] = "system error - invalid seek: `%s'";
char DERR_esrch [] = "system error - no such process: `%s'";
char DERR_exdev [] = "system error - improper link: `%s'";
char DERR_no_shell []
  = "run time error - no shell found during call of `system'";
char DERR_other_fail_in_system_call []
  = "run time error - fail during call of `system'";
char DERR_regex [] = "run time Oniguruma error in `%s' - %s";
char DERR_no_such_external []
  = "run time error - can not find external function %s";
char DERR_library_close_error []
  = "run time error - closing library %s";
char DERR_no_support_extern_funcs []
  = "run time error - there is no support of external functions on the host";
char DERR_corrupted_environment_var []
  = "run time error - bad value of environment variable `%s'";
char DERR_internal_error []
  = "run time error - internal error in `%s'";
char DERR_fun_as_variable []
  = "run time error - usage function as variable";
char DERR_class_as_variable []
  = "run time error - usage class as variable";
char DERR_value_is_not_class_instance_or_stack []
  = "run time error - value is not class instance or stack (\".\")";
char DERR_decl_is_absent_in_given_class_or_block []
  = "run time error - there is no such declaration in given class";
char DERR_undefined_value_access []
  = "run time error - undefined `%s' value access";
char DERR_undefined_value_assign []
  = "run time error - undefined value assign";
char DERR_private_decl_access_from_outside_block []
  = "run time error - access to private decl `%s' from outside";
char DERR_undefined_class_or_fun []
  = "run time error - undefined class or function `%s'";
char DERR_immutable_vector_modification []
  = "run time error - attempt to modify immutable vector";
char DERR_immutable_table_modification []
  = "run time error - attempt to modify immutable table";
char DERR_immutable_instance_modification []
  = "run time error - attempt to modify immutable instance";
char DERR_wrong_vector_pattern_match []
  = "run time error - expr does not match vector pattern";
char DERR_wrong_table_pattern_match []
  = "run time error - expr does not match table pattern";
char DERR_wrong_stack_pattern_match []
  = "run time error - expr does not match stack pattern";
char DERR_rmatch_expr_type []
  = "run time error - rmatch expression is not a string";
char DERR_rmatch_case_expr_type []
  = "run time error - rmatch case expression is not a string";;
char DERR_deadlock [] = "run time error - process deadlock";
char DERR_unprocessed_exception []
  = "run time error - exception %s has not been processed";
