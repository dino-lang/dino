/*
   Copyright (C) 1997-2013 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

#include "errors.h"

extern char ERR_no_memory[];
extern char ERR_invalid_char_constant[];
extern char ERR_invalid_input_char[];
extern char ERR_float_value[];
extern char ERR_int_value[];
extern char ERR_string_end_absence[];
extern char ERR_exponent_absence[];
extern char ERR_eof_in_comment[];
extern char ERR_interrupt_exception[];
extern char ERR_illegal_instruction_exception[];
extern char ERR_abort_exception[];
extern char ERR_floating_point_exception[];
extern char ERR_termination_exception[];
extern char ERR_segment_access_violation_exception[];

extern char ERR_repeated_decl[];
extern char ERR_previous_decl_location[];
extern char ERR_undeclared_ident[];
extern char ERR_period_ident_applied_to_slice [];
extern char ERR_vector_element_access_applied_to_slice [];
extern char ERR_table_element_access_applied_to_slice [];
extern char ERR_call_applied_to_slice [];
extern char ERR_slice_as_foreach_designator[];
extern char ERR_extension_without_class_or_func [];
extern char ERR_extension_of_final [];
extern char ERR_udenclared_ident_access_list [];
extern char ERR_invalid_friend [];
extern char ERR_contradicted_ident_access_list [];
extern char ERR_previous_access_location [];
extern char ERR_decl_is_absent_in_a_block [];
extern char ERR_invalid_type_of_arrow_left_operand [];
extern char ERR_invalid_type_of_deref_operand [];
extern char ERR_invalid_logical_operation_operand_type [];
extern char ERR_invalid_order_comparison_operation_operand_type [];
extern char ERR_invalid_concat_operation_operand_type [];
extern char ERR_invalid_arithmetic_operation_operand_type [];
extern char ERR_invalid_fold_arithmetic_operation_operand_type [];
extern char ERR_invalid_repetition_type [];
extern char ERR_invalid_length_operand_type [];
extern char ERR_invalid_conversion_to_char_operand_type [];
extern char ERR_invalid_conversion_to_int_operand_type [];
extern char ERR_invalid_conversion_to_float_operand_type [];
extern char ERR_invalid_conversion_to_vector_operand_type [];
extern char ERR_invalid_conversion_format_type [];
extern char ERR_invalid_conversion_to_table_operand_type [];
extern char ERR_invalid_cond_type [];
extern char ERR_invalid_vector_type [];
extern char ERR_invalid_table_type [];
extern char ERR_invalid_index_type [];
extern char ERR_invalid_slice_start_type [];
extern char ERR_invalid_slice_bound_type [];
extern char ERR_invalid_slice_step_type [];
extern char ERR_invalid_class_func_thread_designator [];
extern char ERR_invalid_if_expr_type [];
extern char ERR_invalid_for_guard_expr_type [];
extern char ERR_invalid_wait_guard_expr_type [];
extern char ERR_invalid_foreach_table_type [];
extern char ERR_invalid_throw_expr_type [];
extern char ERR_invalid_catch_expr_type [];
extern char ERR_non_variable_in_assignment [];
extern char ERR_non_variable_in_swap [];
extern char ERR_const_assignment [];
extern char ERR_const_swap [];
extern char ERR_non_variable_in_foreach [];
extern char ERR_continue_is_not_in_loop [];
extern char ERR_break_is_not_in_loop [];
extern char ERR_return_outside_func_class_ext [];
extern char ERR_return_with_result_in_class [];
extern char ERR_return_with_result_in_thread [];
extern char ERR_function_call_in_wait_stmt [];

/* Invalid environment. */
extern char DERR_environment_corrupted[];
/* Operand types. */
extern char DERR_logical_or_operands_types [];
extern char DERR_logical_and_operands_types [];
extern char DERR_cond_operand_type [];
extern char DERR_not_operand_type [];
extern char DERR_bitwise_not_operand_type [];
extern char DERR_lt_operands_types [];
extern char DERR_gt_operands_types [];
extern char DERR_le_operands_types [];
extern char DERR_ge_operands_types [];
extern char DERR_plus_operands_types [];
extern char DERR_minus_operands_types [];
extern char DERR_concat_operands_types [];
extern char DERR_mult_operands_types [];
extern char DERR_div_operands_types [];
extern char DERR_mod_operands_types [];
extern char DERR_lshift_operands_types [];
extern char DERR_rshift_operands_types [];
extern char DERR_ashift_operands_types [];
extern char DERR_and_operands_types [];
extern char DERR_xor_operands_types [];
extern char DERR_or_operands_types [];
extern char DERR_unary_plus_operand_type [];
extern char DERR_unary_minus_operand_type [];
extern char DERR_length_operand_type [];
extern char DERR_conversion_to_char_operand_type [];
extern char DERR_conversion_to_int_operand_type [];
extern char DERR_conversion_to_float_operand_type [];
extern char DERR_conversion_to_vector_operand_type [];
extern char DERR_format_conversion_to_vector_operand_type [];
extern char DERR_vector_conversion_format_type [];
extern char DERR_conversion_to_table_operand_type [];
extern char DERR_elist_repetition_type [];
extern char DERR_invalid_if_expr_type [];
extern char DERR_invalid_for_guard_expr_type [];
extern char DERR_invalid_wait_guard_expr_type [];
extern char DERR_wait_in_sync_stmt [];
extern char DERR_thread_call_in_sync_stmt [];
extern char DERR_no_exception_after_throw [];
/* Index type. */
extern char DERR_index_is_not_int [];
/* Index value. */
extern char DERR_index_is_negative_number [];
extern char DERR_index_is_greater_than_array_bound [];
/* Index operation. */
extern char DERR_index_operation_for_non_array [];
/* Slice type. */
extern char DERR_slice_start_is_not_int [];
extern char DERR_slice_bound_is_not_int [];
extern char DERR_slice_step_is_not_int [];
/* Slice len. */
extern char DERR_different_slice_operand_lengths [];
/* Slice form. */
extern char DERR_slice_operand_form [];
/* Veclen. */
extern char DERR_different_vec_operand_lengths [];
/* Vecform. */
extern char DERR_vector_form_type [];
/* Key value. */
extern char DERR_repeated_key [];
extern char DERR_no_such_key [];
/* Key operation. */
extern char DERR_key_index_operation_for_non_table [];
extern char DERR_in_table_operand_type [];
/* Call operation. */
extern char DERR_none_class_or_func_before_left_bracket [];
/* Parameter type. */
extern char DERR_parameter_type [];
/* Invalid result. */
extern char DERR_invalid_result [];
/* Invalid input. */
extern char DERR_invalid_input [];
/* Invalid format. */
extern char DERR_invalid_format [];
/* EOF. */
extern char DERR_eof_occured [];
/* Parameter number. */
extern char DERR_parameters_number [];
/* System errors. */
extern char DERR_eaccess [];
extern char DERR_eagain [];
extern char DERR_ebadf [];
extern char DERR_ebusy [];
extern char DERR_echild [];
extern char DERR_edeadlk [];
extern char DERR_edom [];
extern char DERR_eexist [];
extern char DERR_efault [];
extern char DERR_efbig [];
extern char DERR_eintr [];
extern char DERR_einval [];
extern char DERR_eio [];
extern char DERR_eisdir [];
extern char DERR_emfile [];
extern char DERR_emlink [];
extern char DERR_enametoolong [];
extern char DERR_enfile [];
extern char DERR_enodev [];
extern char DERR_enoent [];
extern char DERR_enoexec [];
extern char DERR_enolck [];
extern char DERR_enomem [];
extern char DERR_enospc [];
extern char DERR_enosys [];
extern char DERR_enotdir [];
extern char DERR_enotempty [];
extern char DERR_enotty [];
extern char DERR_enxio [];
extern char DERR_eperm [];
extern char DERR_epipe [];
extern char DERR_erange [];
extern char DERR_erofs [];
extern char DERR_espipe [];
extern char DERR_esrch [];
extern char DERR_exdev [];
/* Errors in call of function `system'. */
/* No shell found during call of function `system' */
extern char DERR_no_shell [];
/* Other fail during call of function `system' */
extern char DERR_other_fail_in_system_call [];
/* Errors in call of regexp functions. */
extern char DERR_reg_ebrack [];
extern char DERR_reg_erange [];
extern char DERR_reg_ectype [];
extern char DERR_reg_eparen [];
extern char DERR_reg_esubreg [];
extern char DERR_reg_eend [];
extern char DERR_reg_eescape [];
extern char DERR_reg_badpat [];
extern char DERR_reg_esize [];
extern char DERR_reg_espace [];
/* External function errors: */
/* No external function is found. */
extern char DERR_no_such_external [];
/* Error when closing librabry. */
extern char DERR_library_close_error [];
/* No external function support. */
extern char DERR_no_support_extern_funcs [];
/* Corrupted environment variable. */
extern char DERR_corrupted_environment_var [];
/* Internal error in system function calls. */
extern char DERR_internal_error [];
/* Accessop. */
extern char DERR_func_as_variable [];
extern char DERR_class_as_variable [];
extern char DERR_value_is_not_class_instance_or_stack [];
extern char DERR_decl_is_absent_in_given_class_or_block [];
extern char DERR_private_decl_access_from_outside_block [];
/* Immutable. */
extern char DERR_immutable_vector_modification [];
extern char DERR_immutable_table_modification [];
extern char DERR_immutable_instance_modification [];
/* Deadlock. */
extern char DERR_deadlock [];
/* Not catched. */
extern char DERR_unprocessed_exception [];

extern int yyerror (const char *message);
