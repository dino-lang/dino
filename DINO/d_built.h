/* These are declarations of the functions which implement Dino
   environment functions. */

/*
   Copyright (C) 1997-2007 Vladimir Makarov.

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

extern void min_call (int_t pars_number);
extern void max_call (int_t pars_number);
extern void tolower_call (int_t pars_number);
extern void toupper_call (int_t pars_number);
extern void trans_call (int_t pars_number);
extern void eltype_call (int_t pars_number);
extern void keys_call (int_t pars_number);
extern void context_call (int_t pars_number);
extern void inside_call (int_t pars_number);
extern void match_call (int_t pars_number);
extern void gmatch_call (int_t pars_number);
extern void sub_call (int_t pars_number);
extern void gsub_call (int_t pars_number);
extern void split_call (int_t pars_number);
extern void subv_call (int_t pars_number);
extern void cmpv_call (int_t pars_number);
extern void del_call (int_t pars_number);
extern void ins_call (int_t pars_number);
extern void insv_call (int_t pars_number);
extern void rev_call (int_t pars_number);
extern void sort_call (int_t pars_number);
extern void rename_call (int_t pars_number);
extern void remove_call (int_t pars_number);
extern void mkdir_call (int_t pars_number);
extern void rmdir_call (int_t pars_number);
extern void getcwd_call (int_t pars_number);
extern void chdir_call (int_t pars_number);
extern void chumod_call (int_t pars_number);
extern void chgmod_call (int_t pars_number);
extern void chomod_call (int_t pars_number);
extern void isatty_call (int_t pars_number);
extern void open_call (int_t pars_number);
extern void close_call (int_t pars_number);
extern void flush_call (int_t pars_number);
extern void popen_call (int_t pars_number);
extern void pclose_call (int_t pars_number);
extern void tell_call (int_t pars_number);
extern void seek_call (int_t pars_number);
extern void put_call (int_t pars_number);
extern void putln_call (int_t pars_number);
extern void fput_call (int_t pars_number);
extern void fputln_call (int_t pars_number);
extern void sput_call (int_t pars_number);
extern void sputln_call (int_t pars_number);
extern void putf_call (int_t pars_number);
extern void fputf_call (int_t pars_number);
extern void sputf_call (int_t pars_number);
extern void print_call (int_t pars_number);
extern void println_call (int_t pars_number);
extern void fprint_call (int_t pars_number);
extern void fprintln_call (int_t pars_number);
extern void sprint_call (int_t pars_number);
extern void sprintln_call (int_t pars_number);
extern void get_call (int_t pars_number);
extern void getln_call (int_t pars_number);
extern void getf_call (int_t pars_number);
extern void fget_call (int_t pars_number);
extern void fgetln_call (int_t pars_number);
extern void fgetf_call (int_t pars_number);
extern void scan_call (int_t pars_number);
extern void scanln_call (int_t pars_number);
extern void fscan_call (int_t pars_number);
extern void fscanln_call (int_t pars_number);
extern void getpid_call (int_t pars_number);
extern void getun_call (int_t pars_number);
extern void geteun_call (int_t pars_number);
extern void getgn_call (int_t pars_number);
extern void getegn_call (int_t pars_number);
extern void getgroups_call (int_t pars_number);
extern void sqrt_call (int_t pars_number);
extern void exp_call (int_t pars_number);
extern void log_call (int_t pars_number);
extern void log10_call (int_t pars_number);
extern void pow_call (int_t pars_number);
extern void sin_call (int_t pars_number);
extern void cos_call (int_t pars_number);
extern void atan2_call (int_t pars_number);
extern void rand_call (int_t pars_number);
extern void srand_call (int_t pars_number);
extern void process_errno_call (int_t pars_number);
extern void readdir_call (int_t pars_number);
extern void ftype_call (int_t pars_number);
extern void fun_call (int_t pars_number);
extern void fgn_call (int_t pars_number);
extern void fsize_call (int_t pars_number);
extern void fatime_call (int_t pars_number);
extern void fmtime_call (int_t pars_number);
extern void fctime_call (int_t pars_number);
extern void fumode_call (int_t pars_number);
extern void fgmode_call (int_t pars_number);
extern void fomode_call (int_t pars_number);
extern void time_call (int_t pars_number);
extern void strtime_call (int_t pars_number);
extern void clock_call (int_t pars_number);
extern void gc_call (int_t pars_number);
extern void system_call (int_t pars_number);
extern void exit_call (int_t pars_number);
extern void fold_call (int_t pars_number);
extern void filter_call (int_t pars_number);
extern void map_call (int_t pars_number);
extern void init_call (int_t pars_number);

extern void int_earley_parse_grammar (int_t pars_number);
extern void int_earley_set_debug_level (int_t pars_number);
extern void int_earley_set_one_parse_flag (int_t pars_number);
extern void int_earley_set_lookahead_level (int_t pars_number);
extern void int_earley_set_cost_flag (int_t pars_number);
extern void int_earley_set_error_recovery_flag (int_t pars_number);
extern void int_earley_set_recovery_match (int_t pars_number);
extern void int_earley_parse (int_t pars_number);
extern void int_earley_create_grammar (int_t pars_number);
