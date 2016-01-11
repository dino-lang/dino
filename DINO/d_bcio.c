/*
   Copyright (C) 2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of interpreter of DINO.  Reading the byte code.

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


#include "d_common.h"
#include "d_bc.h"
#include "d_bcn.c"
#include "d_bcf.c"
#include "d_bcio.h"
#include "d_inference.h"
#include "d_run.h"

/* Bcode internal representation uses only ISO 8859-1 (aka Latin-1).
   Unicode chars in a string are always represented with escape codes
   \u or \U.  */

/* The line number of the last printed node. */
static const char *curr_file_name = NULL;
static unsigned curr_line_number = 0;

/* Print representation of string STR. */
static void
print_str (const char *str)
{
  const char *s;

  printf ("\"");
  for (s = str; *s != '\0'; s++)
    printf ("%s", get_ucode_ascii_repr (*s));
  printf ("\"");
}

/* Print representation of unicode string STR. */
static void
print_ustr (const ucode_t *str)
{
  const ucode_t *s;

  printf ("\"");
  for (s = str; *s != '\0'; s++)
    printf ("%s", get_ucode_ascii_repr (*s));
  printf ("\"");
}

/* Print position of POS (only changed elements) with SUFFIX. */
static inline void
print_pos (position_t pos, const char *suffix)
{
  if (curr_file_name != pos.file_name)
    {
      curr_file_name = pos.file_name;
      printf (" fn%s=", suffix);
      print_str (pos.file_name);
    }
  if (curr_line_number != pos.line_number)
    {
      curr_line_number = pos.line_number;
      printf (" ln%s=%d", suffix, pos.line_number);
    }
  printf (" pos%s=%d", suffix, pos.column_number);
}

/* Print position of node N. */
static void
print_source (BC_node_t n)
{
  if (BC_IS_OF_TYPE (n, BC_NM_bcode))
    {
      print_source (BC_info (n));
      return;
    }
  if (BC_IS_OF_TYPE (n, BC_NM_info))
    {
      print_source (BC_source (n));
      return;
    }
  d_assert (BC_IS_OF_TYPE (n, BC_NM_source));
  print_pos (BC_pos (n), "");
  if (BC_IS_OF_TYPE (n, BC_NM_source2))
    print_pos (BC_pos2 (n), "2");
  if (BC_IS_OF_TYPE (n, BC_NM_source3))
    print_pos (BC_pos3 (n), "3");
}

/* Return node containing position of node N. */
static BC_node_t
give_source_node (BC_node_t n)
{
  if (BC_IS_OF_TYPE (n, BC_NM_bcode))
    n = BC_info (n);
  if (BC_IS_OF_TYPE (n, BC_NM_info))
    n = BC_source (n);
  return n;
}

/* Return true if the current file is different from node N file. */
static int
new_file_p (BC_node_t n)
{
  n = give_source_node (n);
  return curr_file_name != NULL && curr_file_name != BC_pos (n).file_name;
}

/* Print all byte code using functions
   print_inference_info_before_insn and
   print_inference_info_after_insn.  Separate code from different
   files by ----... */
void
dump_code (BC_node_t infos, int indent)
{
  BC_node_t info, bc;
  BC_node_mode_t node_mode;
  BC_node_t cl;

  for (info = infos; info != NULL; info = BC_next_info (info))
    {
      bc = BC_bc (info);
      node_mode = BC_NODE_MODE (bc);
      if (new_file_p (bc))
	printf ("//-----------------------%s----------------------------\n",
		BC_pos (give_source_node (bc)).file_name);
      if (optimize_flag)
	print_inference_info_before_insn (info, indent);
      print_indent (indent);
      printf ("%6d", BC_idn (info));
      printf (" %s", BC_node_name[node_mode]);
      d_assert (indent >= 0);
      print_source (bc);
      if (BC_next (bc) != NULL
	  && (BC_next_info (info) != BC_info (BC_next (bc))
	      || (BC_IS_TYPE (node_mode, BC_NM_block)
		  && BC_decls (bc) != NULL)))
	printf (" next=%d", BC_idn (BC_info (BC_next (bc))));
      switch (node_mode)
	{
	case BC_NM_nop: /* For debugging purposes */
	  break;
	case BC_NM_ldch:
	  printf (" op1=%d op2=%d // %d <- ",
		  BC_op1 (bc), BC_op2 (bc), BC_op1 (bc));
	  if (isgraph_ascii (BC_op2 (bc)))
	    printf ("(%c -- %x)", BC_op2 (bc), BC_op2 (bc));
	  else
	    printf ("d(%x)", BC_op2 (bc));
	  break;
	case BC_NM_ldi:
	  printf (" op1=%d op2=%d // %d <- i%d", BC_op1 (bc), BC_op2 (bc),
		  BC_op1 (bc), BC_op2 (bc));
	  break;
	case BC_NM_ldbi:
	  if (sizeof (long) >= sizeof (rint_t))
	    printf (" op1=%d bi=%ld // %d <- i%ld",
		    BC_op1 (bc), (long) BC_bi (bc),
		    BC_op1 (bc), (long) BC_bi (bc));
	  else
	    {
	      assert (sizeof (long long) == sizeof (rint_t));
	      printf (" op1=%d bi=%lld // %d <- i%lld",
		      BC_op1 (bc), (long long) BC_bi (bc),
		      BC_op1 (bc), (long long) BC_bi (bc));
	    }
	  break;
	case BC_NM_ldl:
	  printf (" op1=%d mpz=", BC_op1 (bc));
	  mpz_out_str (stdout, 10, *BC_mpz_ptr (bc));
	  printf ("l");
	  break;
	case BC_NM_ldf:
	  printf (" op1=%d f=%.*g // %d <- f%g", BC_op1 (bc),
		  FORMAT_DOUBLE_DIGS, BC_f (bc), BC_op1 (bc), BC_f (bc));
	  break;
	case BC_NM_ldtp:
	  printf (" op1=%d op2=%d // %d <- %s", BC_op1 (bc), BC_op2 (bc),
		  BC_op1 (bc), er_type_name (BC_op2 (bc)));
	  break;
	case BC_NM_ldnil:
	case BC_NM_ldthis:
	  printf (" op1=%d // %d <- nil", BC_op1 (bc), BC_op1 (bc));
	  break;
	case BC_NM_lds:
	  printf (" op1=%d str=", BC_op1 (bc));
	  print_str (BC_str (bc));
	  printf (" // %d <- string", BC_op1 (bc));
	  break;
	case BC_NM_ldus:
	  printf (" op1=%d ustr=", BC_op1 (bc));
	  print_ustr (BC_ustr (bc));
	  printf (" // %d <- string", BC_op1 (bc));
	  break;
	case BC_NM_fld:
	case BC_NM_lfld:
	case BC_NM_lfldv:
	  printf (" op1=%d op2=%d op3=%d fldid=%s // %d <- %d . %d (%s)",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc), BC_fldid (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc), BC_fldid (bc));
	  break;
	case BC_NM_brts:
	case BC_NM_brfs:
	  printf (" res=%d op1=%d pc=%d // %d <- %d pc=%d",
		  BC_res (bc), BC_op1 (bc), BC_idn (BC_info (BC_pc (bc))),
		  BC_res (bc), BC_op1 (bc), BC_idn (BC_info (BC_pc (bc))));
	  break;
	case BC_NM_lconv:
	  printf (" op1=%d op2=%d // %d <- %d", BC_op1 (bc), BC_op2 (bc),
		  BC_op1 (bc), BC_op2 (bc));
	  break;
	case BC_NM_not:
	case BC_NM_bnot:
	case BC_NM_plus:
	case BC_NM_minus:
	case BC_NM_inot:
	case BC_NM_ibnot:
	case BC_NM_iplus:
	case BC_NM_iminus:
	case BC_NM_fplus:
	case BC_NM_fminus:
	case BC_NM_length:
	case BC_NM_fold_add:
	case BC_NM_fold_mult:
	case BC_NM_fold_and:
	case BC_NM_fold_xor:
	case BC_NM_fold_or:
	case BC_NM_fold_concat:
	case BC_NM_const:
	case BC_NM_new:
	case BC_NM_tpof:
	case BC_NM_chof:
	case BC_NM_iof:
	case BC_NM_lof:
	case BC_NM_fof:
	case BC_NM_vecof:
	case BC_NM_tabof:
	  printf (" op1=%d op2=%d // %d <- %d", BC_op1 (bc), BC_op2 (bc),
		  BC_op1 (bc), BC_op2 (bc));
	  break;
	case BC_NM_vec:
	  printf (" op1=%d op2=%d op3=%d // %d <- [els=%d, nparts=%d]",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_tab:
	  printf (" op1=%d op2=%d op3=%d // %d <- {els=%d, nparts=%d}",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_flat:
	  printf (" op1=%d // %d <- %d",
		  BC_op1 (bc), BC_op1 (bc), BC_op1 (bc));
	  break;
	case BC_NM_call:
	case BC_NM_tcall:
	  printf (" op1=%d op2=%d // call (start=%d) (npars=%d)",
		  BC_op1 (bc), BC_op2 (bc), BC_op1 (bc), BC_op2 (bc));
	  break;
	case BC_NM_ind:
	  printf (" op1=%d op2=%d op3=%d // %d <- %d[%d]",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_ind2:
	  printf (" op1=%d op2=%d op3=%d op4=%d // %d <- %d[%d][%d]",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc), BC_op4 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc), BC_op4 (bc));
	  break;
	case BC_NM_icall:
	case BC_NM_ibcall:
	case BC_NM_itcall:
	case BC_NM_ticall:
	case BC_NM_titcall:
	case BC_NM_cicall:
	case BC_NM_citcall:
	  printf (" cfblock=%d op1=%d op2=%d",
		  BC_idn (BC_info (BC_cfblock (bc))),
		  BC_op1 (bc), BC_op2 (bc));
	  break;
	case BC_NM_mcall:
	  printf (" op1=%d op2=%d op3=%d op4=%d mid=%s // %d <- %d . %s[idnum=%d] (%d ...)",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc), BC_op4 (bc), BC_mid (bc),
		  BC_op1 (bc), BC_op3 (bc), BC_mid (bc), BC_op4 (bc), BC_op1 (bc));
	  break;
	case BC_NM_sl:
	case BC_NM_lslv:
	  printf (" op1=%d op2=%d op3=%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_in:
	case BC_NM_eq:
	case BC_NM_ieq:
	case BC_NM_ne:
	case BC_NM_ine:
	case BC_NM_id:
	case BC_NM_unid:
	case BC_NM_lt:
	case BC_NM_ilt:
	case BC_NM_ge:
	case BC_NM_ige:
	case BC_NM_gt:
	case BC_NM_igt:
	case BC_NM_le:
	case BC_NM_ile:
	case BC_NM_add:
	case BC_NM_sub:
	case BC_NM_mult:
	case BC_NM_div:
	case BC_NM_mod:
	case BC_NM_iadd:
	case BC_NM_isub:
	case BC_NM_imult:
	case BC_NM_idiv:
	case BC_NM_imod:
	case BC_NM_fadd:
	case BC_NM_fsub:
	case BC_NM_fmult:
	case BC_NM_fdiv:
	case BC_NM_fmodop:
	case BC_NM_ifadd:
	case BC_NM_ifsub:
	case BC_NM_fisub:
	case BC_NM_ifmult:
	case BC_NM_ifdiv:
	case BC_NM_fidiv:
	case BC_NM_concat:
	case BC_NM_lsh:
	case BC_NM_rsh:
	case BC_NM_ash:
	case BC_NM_and:
	case BC_NM_xor:
	case BC_NM_or:
	case BC_NM_ilsh:
	case BC_NM_irsh:
	case BC_NM_iash:
	case BC_NM_iand:
	case BC_NM_ixor:
	case BC_NM_ior:
	case BC_NM_fmtvecof:
	  printf (" op1=%d op2=%d op3=%d // %d <- %d op %d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_eqi:
	case BC_NM_ieqi:
	case BC_NM_nei:
	case BC_NM_inei:
	case BC_NM_lei:
	case BC_NM_ilei:
	case BC_NM_gei:
	case BC_NM_igei:
	case BC_NM_gti:
	case BC_NM_igti:
	case BC_NM_lti:
	case BC_NM_ilti:
	  printf (" op1=%d op2=%d op3=%d // %d <- %d cmp i%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_addi:
	case BC_NM_iaddi:
	case BC_NM_faddi:
	  printf (" op1=%d op2=%d op3=%d // %d <- %d + i%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_multi:
	case BC_NM_imulti:
	case BC_NM_fmulti:
	  printf (" op1=%d op2=%d op3=%d // %d <- %d * i%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_madd:
	  printf (" op1=%d op2=%d op3=%d op4=%d // %d <- %d * %d + %d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc), BC_op4 (bc),
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc), BC_op4 (bc));
	  break;
	case BC_NM_mult_st:
	case BC_NM_div_st:
	case BC_NM_mod_st:
	case BC_NM_sub_st:
	case BC_NM_concat_st:
	case BC_NM_lsh_st:
	case BC_NM_rsh_st:
	case BC_NM_ash_st:
	case BC_NM_and_st:
	case BC_NM_xor_st:
	case BC_NM_or_st:
	case BC_NM_add_st:
	  printf (" op1=%d op2=%d op3=%d op4=%d", BC_op1 (bc), BC_op2 (bc),
		  BC_op3 (bc), BC_op4 (bc));
	  break;
	case BC_NM_stvt:
	case BC_NM_stvtu:
	case BC_NM_sts:
	case BC_NM_stsu:
	  printf (" op1=%d op2=%d op3=%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_ste:
	case BC_NM_steu:
	  printf (" op1=%d op3=%d", BC_op1 (bc), BC_op3 (bc));
	  break;
	case BC_NM_slst:
	  printf (" op1=%d op2=%d op3=%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_mult_slst:
	case BC_NM_div_slst:
	case BC_NM_mod_slst:
	case BC_NM_sub_slst:
	case BC_NM_concat_slst:
	case BC_NM_lsh_slst:
	case BC_NM_rsh_slst:
	case BC_NM_ash_slst:
	case BC_NM_and_slst:
	case BC_NM_xor_slst:
	case BC_NM_or_slst:
	case BC_NM_add_slst:
	  printf (" op1=%d op2=%d op3=%d op4=%d", BC_op1 (bc),
		  BC_op2 (bc), BC_op3 (bc), BC_op4 (bc));
	  break;
	case BC_NM_btdef:
	  printf (" op1=%d pc=%d // goto %d unless %d is undefined",
		  BC_op1 (bc), BC_idn (BC_info (BC_pc (bc))),
		  BC_idn (BC_info (BC_pc (bc))), BC_op1 (bc));
	  break;
	case BC_NM_bf: /* if_stmt */
	case BC_NM_ibf:
	case BC_NM_bfni: /* cond */
	case BC_NM_ibfni:
	  printf (" op1=%d pc=%d // goto %d unless %d",
		  BC_op1 (bc), BC_idn (BC_info (BC_pc (bc))),
		  BC_idn (BC_info (BC_pc (bc))), BC_op1 (bc));
	  break;
	case BC_NM_bteq:
	case BC_NM_btne:
	case BC_NM_btle:
	case BC_NM_btgt:
	case BC_NM_btge:
	case BC_NM_btlt:
	case BC_NM_ibteq:
	case BC_NM_ibtne:
	case BC_NM_ibtle:
	case BC_NM_ibtgt:
	case BC_NM_ibtge:
	case BC_NM_ibtlt:
	case BC_NM_fbteq:
	case BC_NM_fbtne:
	case BC_NM_fbtle:
	case BC_NM_fbtgt:
	case BC_NM_fbtge:
	case BC_NM_fbtlt:
	  printf (" op1=%d bcmp_op2=%d bcmp_res=%d pc=%d // goto %d if %d <- %d cmp %d",
		  BC_op1 (bc), BC_bcmp_op2 (bc), BC_bcmp_res (bc),
		  BC_idn (BC_info (BC_pc (bc))), BC_idn (BC_info (BC_pc (bc))),
		  BC_bcmp_res (bc), BC_op1 (bc), BC_bcmp_op2 (bc));
	  break;
	case BC_NM_bteqi:
	case BC_NM_btnei:
	case BC_NM_btlei:
	case BC_NM_btgti:
	case BC_NM_btgei:
	case BC_NM_btlti:
	case BC_NM_ibteqi:
	case BC_NM_ibtnei:
	case BC_NM_ibtlei:
	case BC_NM_ibtgti:
	case BC_NM_ibtgei:
	case BC_NM_ibtlti:
	  printf (" op1=%d bcmp_op2=%d bcmp_res=%d pc=%d // goto %d if %d <- %d cmp i%d",
		  BC_op1 (bc), BC_bcmp_op2 (bc), BC_bcmp_res (bc),
		  BC_idn (BC_info (BC_pc (bc))), BC_idn (BC_info (BC_pc (bc))),
		  BC_bcmp_res (bc), BC_op1 (bc), BC_bcmp_op2 (bc));
	  break;
	case BC_NM_bteqinc:
	case BC_NM_btneinc:
	case BC_NM_btleinc:
	case BC_NM_btgtinc:
	case BC_NM_btgeinc:
	case BC_NM_btltinc:
	case BC_NM_ibteqinc:
	case BC_NM_ibtneinc:
	case BC_NM_ibtleinc:
	case BC_NM_ibtgtinc:
	case BC_NM_ibtgeinc:
	case BC_NM_ibtltinc:
	  printf (" op1=%d binc_inc=%d bcmp_op2=%d bcmp_res=%d pc=%d"
		  " // goto %d if %d <- (%d += i%d) cmp %d",
		  BC_op1 (bc), BC_binc_inc (bc),
		  BC_bcmp_op2 (bc), BC_bcmp_res (bc),
		  BC_idn (BC_info (BC_pc (bc))),
		  BC_idn (BC_info (BC_pc (bc))), BC_bcmp_res (bc),
		  BC_op1 (bc), BC_binc_inc (bc), BC_bcmp_op2 (bc));
	  break;
	case BC_NM_ibt:
	case BC_NM_bt:
	  printf (" op1=%d pc=%d // goto %d if %d",
		  BC_op1 (bc), BC_idn (BC_info (BC_pc (bc))),
		  BC_idn (BC_info (BC_pc (bc))), BC_op1 (bc));
	  break;
	case BC_NM_foreach:
	  printf (" op1=%d op2=%d op3=%d body_pc=%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc),
		  BC_idn (BC_info (BC_body_pc (bc))));
	  break;
	case BC_NM_foreach2:
	  printf (" op1=%d op2=%d op3=%d element=%d body_pc=%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc), BC_element (bc),
		  BC_idn (BC_info (BC_body_pc (bc))));
	  break;
	case BC_NM_out:
	  printf (" op1=%d", BC_op1 (bc));
	  break;
	case BC_NM_stpop:
	  printf (" op1=%d op2=%d", BC_op1 (bc), BC_op2 (bc));
	  break;
	case BC_NM_leave:
	  if (BC_next_info (info) == NULL)
	    printf (" block=%d", BC_idn (BC_info (BC_block (bc))));
	  indent -= 2;
	  break;
	case BC_NM_bend:
	case BC_NM_fbend:
	  d_assert (BC_next_info (info) == NULL
		    || ! BC_IS_OF_TYPE (BC_block (bc), BC_NM_fblock));
	  printf (" block=%d", BC_idn (BC_info (BC_block (bc))));
	  indent -= 2;
	  break;
	case BC_NM_ret:
	  printf (" op1=%d", BC_op1 (bc));
	  if (BC_ret_decl (bc) != NULL)
	    printf (" ret_decl=%d // ident=%s", BC_decl_num (BC_ret_decl (bc)),
		    BC_ident (BC_ret_decl (bc)));
	  break;
	case BC_NM_waitcond:
	  printf (" op1=%d pc=%d",
		  BC_op1 (bc), BC_idn (BC_info (BC_pc (bc))));
	  break;
	case BC_NM_waitend:
	  break;
	case BC_NM_fblock:
	case BC_NM_block:
	  {
	    BC_node_t friend, use, bc_decl;
	    const char *str = NULL;

	    indent += 2;
	    if (node_mode == BC_NM_fblock)
	      {
		BC_node_t fdecl = BC_fdecl (bc);

		str = BC_ident (fdecl);
		d_assert (BC_IS_OF_TYPE (bc, BC_NM_fblock));
		printf (" fdecl=%d", BC_decl_num (fdecl));
		if (BC_fun_p (bc))
		  printf (" fun_p=1");
		if (BC_class_p (bc))
		  printf (" class_p=1");
		if (BC_thread_p (bc))
		  printf (" thread_p=1");
		if (BC_args_p (bc))
		  printf (" args_p=1");
		if (BC_simple_p (bc))
		  printf (" simple_p=1");
		if (BC_pure_fun_p (bc))
		  printf (" pure_fun_p=1");
		if (BC_forward_p (bc))
		  printf (" forward_p=1");
		/* builtin mode will be set up anyway.  */
		if (BC_fmode (bc) != 0 && BC_fmode (bc) != BC_builtin)
		  printf (" fmode=%d", (int) BC_fmode (bc));
		if (BC_pars_num (bc) != 0)
		  printf (" pars_num=%d", BC_pars_num (bc));
		if (BC_min_pars_num (bc) != 0)
		  printf (" min_pars_num=%d", BC_min_pars_num (bc));
	      }
	    if (BC_scope (bc) != NULL)
	      printf (" scope=%d", BC_idn (BC_info (BC_scope (bc))));
	    printf (" vars_num=%d tvars_num=%d%s",
		    BC_vars_num (bc), BC_tvars_num (bc),
		    BC_ext_life_p (bc) ? " ext_life_p=1" : "");
	    if (BC_excepts (bc) != NULL)
	      printf (" excepts=%d", BC_idn (BC_info (BC_excepts (bc))));
	    for (friend = BC_friends (bc);
		 friend != NULL;
		 friend = BC_next_friend (friend))
	      printf (" friend=%d", BC_idn (BC_info (BC_friend (friend))));
	    for (use = BC_uses (bc); use != NULL; use = BC_next_use (use))
	      printf (" use=%d", BC_idn (BC_info (BC_use (use))));
	    printf (" // ident=%s\n", str);
	    for (bc_decl = BC_decls (bc);
		 bc_decl != NULL;
		 bc_decl = BC_next_decl (bc_decl))
	      {
		if (BC_IS_OF_TYPE (bc_decl, BC_NM_fdecl)
		    && BC_vars_num (BC_fblock (bc_decl)) < 0)
		  continue;
		if (new_file_p (bc_decl))
		  printf ("//-----------------------%s---------------------------\n",
			  BC_pos (give_source_node (bc_decl)).file_name);
		print_indent (indent);
		printf ("%6d %s", BC_decl_num (bc_decl),
			BC_node_name[BC_NODE_MODE (bc_decl)]);
		print_source (bc_decl);
		printf (" ident=%s decl_scope=%d%s", BC_ident (bc_decl),
			BC_idn (BC_info (BC_decl_scope (bc_decl))),
			BC_public_p (bc_decl) ? " public_p=1" : "");
		if (BC_fldid_num (bc_decl) >= 0)
		  printf (" fldid_num=%d", BC_fldid_num (bc_decl));
		if (BC_IS_OF_TYPE (bc_decl, BC_NM_vdecl))
		  printf (" var_num=%d", BC_var_num (bc_decl));
		else if (BC_IS_OF_TYPE (bc_decl, BC_NM_fdecl))
		  printf (" fblock=%d", BC_idn (BC_info (BC_fblock (bc_decl))));
		printf ("\n");
		if (BC_IS_OF_TYPE (bc_decl, BC_NM_fdecl))
		  {
		    d_assert (BC_IS_OF_TYPE (BC_fblock (bc_decl), BC_NM_fblock));
		    dump_code (BC_info (BC_fblock (bc_decl)), indent);
		  }
	      }
	    break;
	  }
	case BC_NM_stinc:
	  printf (" op1=%d op2=%d op3=%d",
		  BC_op1 (bc), BC_op2 (bc), BC_op3 (bc));
	  break;
	case BC_NM_stdecu:
	  printf (" op1=%d op2=%d", BC_op1 (bc), BC_op2 (bc));
	  break;
	case BC_NM_stdecm:
	  printf (" op1=%d op2=%d", BC_op1 (bc), BC_op2 (bc));
	  break;
	case BC_NM_throw:
	  printf (" op1=%d", BC_op1 (bc));
	  break;
	case BC_NM_except:
	  printf (" op1=%d op2=%d", BC_op1 (bc), BC_op2 (bc));
	  if ((cl = BC_next_except (bc)) != NULL)
	    printf (" next_except=%d", BC_idn (BC_info (BC_next_except (bc))));
	  break;
	case BC_NM_var:
	case BC_NM_lvar:
	case BC_NM_lvarv:
	case BC_NM_evar:
	case BC_NM_levar:
	case BC_NM_levarv:
	case BC_NM_efun:
	case BC_NM_fun:
	case BC_NM_class:
	  printf (" op1=%d decl=%d // %s", BC_op1 (bc),
		  BC_decl_num (BC_decl (bc)), BC_ident (BC_decl (bc)));
	  break;
	case BC_NM_move:
	case BC_NM_imove:
	case BC_NM_fmove:
	  printf (" op1=%d op2=%d move_decl=%d // %d (%s) <- %d",
		  BC_op1 (bc), BC_op2 (bc), BC_decl_num (BC_move_decl (bc)),
		  BC_op1 (bc), BC_ident (BC_move_decl (bc)), BC_op2 (bc));
	  break;
	case BC_NM_chvec:
	  printf (" op1=%d", BC_op1 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // vec");
	  break;
	case BC_NM_chvend:
	  printf (" op1=%d ch_op2=%d", BC_op1 (bc), BC_ch_op2 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // vec, ind");
	  break;
	case BC_NM_chvlen:
	  printf (" op1=%d ch_op2=%d ch_op3=%d",
		  BC_op1 (bc), BC_ch_op2 (bc), BC_ch_op3 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // vec, ind, rep");
	  break;
	case BC_NM_chvel:
	  printf (" op1=%d ch_op2=%d ch_op3=%d ch_op4=%d ch_op5=%d",
		  BC_op1 (bc), BC_ch_op2 (bc), BC_ch_op3 (bc), BC_ch_op4 (bc),
		  BC_ch_op5 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d",
		    BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // vec, ind, rep, el %s assign",
		  BC_ch_op5 (bc) ? "with": "w/o");
	  break;
	case BC_NM_chtab:
	  printf (" op1=%d", BC_op1 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // tab");
	  break;
	case BC_NM_chtend:
	  printf (" op1=%d ch_op2=%d", BC_op1 (bc), BC_ch_op2 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // tab, nel");
	  break;
	case BC_NM_chtel:
	  printf (" op1=%d ch_op2=%d ch_op3=%d ch_op4=%d",
		  BC_op1 (bc), BC_ch_op2 (bc), BC_ch_op3 (bc), BC_ch_op4 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf ("// tab, key");
	  if (BC_ch_op4 (bc) != 2)
	    printf (", el with %s", BC_ch_op4 (bc) ? " assign" : "compare");
	  break;
	case BC_NM_chst:
	  printf (" op1=%d ch_op2=%d", BC_op1 (bc), BC_ch_op2 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // stack, fun/class");
	  break;
	case BC_NM_chstend:
	  printf (" op1=%d ch_op2=%d", BC_op1 (bc), BC_ch_op2 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // fun/class, params_num");
	  break;
	case BC_NM_chstel:
	  printf (" op1=%d ch_op2=%d ch_op3=%d ch_op4=%d ch_op5=%d",
		  BC_op1 (bc), BC_ch_op2 (bc), BC_ch_op3 (bc), BC_ch_op4 (bc), BC_ch_op5 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // stack, class, var, param_num");
	  if (BC_ch_op5 (bc) != 2)
	    printf (", val with %s", BC_ch_op5 (bc) ? " assign" : "compare");
	  break;
	case BC_NM_rmatch:
	  printf (" op1=%d ch_op2=%d ch_op3=%d", BC_op1 (bc), BC_ch_op2 (bc), BC_ch_op3 (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // %d <- regexp_match (%d, %d)",
		  BC_op1 (bc), BC_ch_op2 (bc), BC_ch_op3 (bc));
	  break;
	case BC_NM_rmatchs:
	  printf (" op1=%d ch_op2=%d rm_str=", BC_op1 (bc), BC_ch_op2 (bc));
	  print_str (BC_rm_str (bc));
	  if ((cl = BC_fail_pc (bc)) != NULL)
	    printf (" fail_pc=%d", BC_idn (BC_info (BC_fail_pc (bc))));
	  printf (" // %d <- regexp_match (%d, string)",
		  BC_op1 (bc), BC_ch_op2 (bc));
	  break;
	default:
	  /* Other nodes should not occur here. */
	  d_unreachable ();
	}
      printf ("\n");
      if (optimize_flag)
	print_inference_info_after_insn (info, indent);
    }
}



/* Container where the read byte code is stored. */
static os_t read_bc;



/* This page contains abstract data byte code `scanner'. */

/* Tokens of BC file. */
enum token
{
  D_NL,
  D_EOF,
  D_EQ,
  D_IDENT,
  D_INT,
  D_LONG,
  D_FLOAT,
  D_STRING,
  D_UCODESTR
};

/* The token attributes. */
static union
{
  rint_t i;
  rfloat_t f;
  const char *str;
  const ucode_t *ustr;
} token_attr;

/* We keep it here as it needs initialization/finalization only once.  */
static mpz_t mpz_attr;

/* Last read token and its position. */
static enum token curr_token;
static position_t curr_token_position;

/* Input BC file. */
static FILE *input_file;

/* The variable is used for implementation of bc_ungetc.  */
static int previous_char;

/* Getc for byte code reader.  Replacing "\r\n" onto "\n". */
static int
bc_getc (void)
{
  int result;

  d_assert (input_file != NULL);
  if (previous_char == NOT_A_CHAR)
    result = dino_getc (input_file);
  else
    {
      result = previous_char;
      previous_char = NOT_A_CHAR;
    }
  if (result == '\r')
    {
      result = dino_getc (input_file);
      if (result != '\n')
	{
	  ungetc (result, input_file);
	  result = '\r';
	}
    }
  return result;
}

/* Ungetc for byte code reader. */
static void
bc_ungetc (int ch)
{
  d_assert (input_file != NULL);
  d_assert (previous_char == NOT_A_CHAR);
  previous_char = ch;
}

/* The string hash table. */
static hash_table_t string_hash_table;

/* The unicode string hash table. */
static hash_table_t ucodestr_hash_table;

/* Func for evaluation of hash value of unicode string STR. */
static unsigned
ustr_hash_func (hash_table_entry_t str)
{
  const ucode_t *s = str;
  unsigned int i, hash_value;

  for (hash_value = i = 0; *s != 0; i++, s++)
    hash_value += (*s) << (i & 0x7);
  return hash_value;
}

/* Func used for comparison of unicode strings represented by STR1 and
   STR2.  Return TRUE if the elements represent equal string. */
static int
ustr_compare_func (hash_table_entry_t str1, hash_table_entry_t str2)
{
  size_t i;
  const ucode_t *s1 = str1, *s2 = str2;
  
  for (i = 0;; i++)
    if (s1[i] != s2[i])
      return FALSE;
    else if (s1[i] == 0)
      return TRUE;
}

/* Create the string hash table. */
static void
initiate_string_tables (void)
{
  string_hash_table = create_hash_table (1000, str_hash_func, str_compare_func);
  ucodestr_hash_table = create_hash_table (1000,
					   ustr_hash_func, ustr_compare_func);
}

/* Include STR into string table if it is necessary and return the
   string in the table. */
static const char*
string_to_table (const char *str)
{
  const char **table_entry_pointer;
  char *string_in_table;

  table_entry_pointer
    = (const char **) find_hash_table_entry (string_hash_table,
					     (hash_table_entry_t) str, TRUE);
  if (*table_entry_pointer != NULL)
    return *table_entry_pointer;
  OS_TOP_EXPAND (read_bc, strlen (str) + 1);
  string_in_table = OS_TOP_BEGIN (read_bc);
  OS_TOP_FINISH (read_bc);
  strcpy (string_in_table, str);
  *table_entry_pointer = string_in_table;
  return string_in_table;
}

/* Include unicode STR into unicode string table if it is necessary
   and return the string in the table. */
static const ucode_t *
ucodestr_to_table (const ucode_t *str)
{
  size_t len;
  const ucode_t **table_entry_pointer;
  ucode_t *string_in_table;
  
  table_entry_pointer
    = (const ucode_t **) find_hash_table_entry (ucodestr_hash_table,
						(hash_table_entry_t) str, TRUE);
  if (*table_entry_pointer != NULL)
    return *table_entry_pointer;
  len = ucodestrlen (str) + 1;
  OS_TOP_EXPAND (read_bc, len);
  string_in_table = OS_TOP_BEGIN (read_bc);
  OS_TOP_FINISH (read_bc);
  memcpy (string_in_table, str, len);
  *table_entry_pointer = string_in_table;
  return string_in_table;
}

/* Delete the string hash tables. */
static void
delete_string_tables (void)
{
  delete_hash_table (string_hash_table);
  delete_hash_table (ucodestr_hash_table);
}

/* Var length string used by func yylval for text presentation of the
   symbol. */
static vlo_t symbol_text;

/* Temporary container.  */
static vlo_t temp_vlo;

/* The following func recognizes next source symbol from the input
   file, returns its code, modifies var current_position so that its
   value is equal to position of the current character in the input
   file and sets up var curr_token and curr_token_position so that
   their values are equal to the returned token and position of the
   returned token start and sets up token_attr.  The func skips all
   white spaces and commentaries and fixes all lexical errors. */
static enum token
get_token (void)
{
  int input_char;
  
  VLO_NULLIFY (symbol_text);
  for (;;)
    {
      input_char = bc_getc ();
      /* `current_position' corresponds `input_char' here */
      switch (input_char)
        {
          /* Break results in skipping all white spaces. */
        case ' ':
        case '\f':
          current_position.column_number++;
          break;
        case '\t':
          current_position.column_number
            = ((current_position.column_number - 1) / TAB_STOP + 1)
              * TAB_STOP + 1;
          break;
        case '\n':
          current_position.column_number = 1;
          current_position.line_number++;
	  return curr_token = D_NL;
        case '\r':
          current_position.column_number++;
          break;
        case '/':
	  curr_token_position = current_position;
          current_position.column_number++;
          input_char = bc_getc ();
	  if (input_char == '/')
	    {
              /* commentary */
              for (;;)
                {
		  current_position.column_number++;
                  input_char = bc_getc ();
                  if (input_char == '\n')
		    {
		      current_position.column_number = 1;
		      current_position.line_number++;
		      return curr_token = D_NL;
		    }
                  else if (input_char == EOF)
                    break;
		}
	      error (FALSE, current_position, ERR_eof_in_comment);
	      break;
	    }
        case '=':
	  curr_token_position = current_position;
          current_position.column_number++;
          return curr_token = D_EQ;
        case EOF:
	  curr_token_position = current_position;
          return curr_token = D_EOF;
        case '\"':
          {
            int unicode_p, correct_newln, wrong_escape_code;
	    ucode_t ch;
	    
	    curr_token_position = current_position;
	    current_position.column_number++;
	    unicode_p = FALSE;
            for (;;)
              {
                ch = bc_getc ();
		current_position.column_number++;
                if (ch == '\"')
                  break;
                ch = read_dino_string_code (ch, &correct_newln,
					    &wrong_escape_code,
					    bc_getc, bc_ungetc);
                if (ch < 0)
                  {
                    error (FALSE, current_position, ERR_string_end_absence);
                    break;
                  }
                if (wrong_escape_code)
                  {
		    error (FALSE, current_position, ERR_invalid_escape_code);
                    continue;
                  }
                if (!correct_newln)
		  {
		    if (! unicode_p && ! in_byte_range_p (ch))
		      {
			unicode_p = TRUE;
			copy_vlo (&temp_vlo, &symbol_text);
			str_to_ucode_vlo (&symbol_text,
					  VLO_BEGIN (temp_vlo),
					  VLO_LENGTH (symbol_text));
			
		      }
		    if (unicode_p)
		      VLO_ADD_MEMORY (symbol_text, &ch, sizeof (ucode_t));
		    else
		      VLO_ADD_BYTE (symbol_text, ch);
		  }
              }
	    if (unicode_p)
	       {
		ch = '\0';
		VLO_ADD_MEMORY (symbol_text, &ch, sizeof (ucode_t));
		token_attr.ustr = ucodestr_to_table (VLO_BEGIN (symbol_text));
		return curr_token = D_STRING;
	      }
	    else
	      {
		VLO_ADD_BYTE (symbol_text, '\0');
		token_attr.str = string_to_table (VLO_BEGIN (symbol_text));
		return curr_token = D_STRING;
	      }
          }
        default:
          if (isalpha_ascii (input_char) || input_char == '_' )
            {
	      curr_token_position = current_position;
              /* Ident recognition. */
              do
                {
		  current_position.column_number++;
                  VLO_ADD_BYTE (symbol_text, input_char);
                  input_char = bc_getc ();
                }
              while (isalpha_ascii (input_char)
		     || isdigit_ascii (input_char)
                     || input_char == '_');
              bc_ungetc (input_char);
              VLO_ADD_BYTE (symbol_text, '\0');
	      token_attr.str = string_to_table (VLO_BEGIN (symbol_text));
	      return curr_token = D_IDENT;
            }
          else if (input_char == '-' || isdigit_ascii (input_char))
            {
              /* Recognition numbers. */
	      int float_flag = FALSE, long_flag = FALSE;

	      curr_token_position = current_position;
              do
                {
                  /* `current_position' corresponds to `input_char' here. */
                  current_position.column_number++;
                  VLO_ADD_BYTE (symbol_text, input_char);
                  input_char = bc_getc ();
                }
              while (isdigit_ascii (input_char));
              if (input_char == '.')
                {
                  float_flag = TRUE;
                  do
                    {
		      /* `current_position' corresponds to
                         `input_char' here. */
		      current_position.column_number++;
	              VLO_ADD_BYTE (symbol_text, input_char);
                      input_char = bc_getc ();
                    }
	          while (isdigit_ascii (input_char));
                }
              if (input_char == 'e' || input_char == 'E')
                {
		  float_flag = TRUE;
		  current_position.column_number++;
                  input_char = bc_getc ();
		  if (input_char != '+' && input_char != '-'
		      && !isdigit_ascii (input_char))
                    error (FALSE, current_position, ERR_exponent_absence);
		  else
                    {
		      VLO_ADD_BYTE (symbol_text, 'e');
		      do
			{
			  /* `current_position' corresponds to
                             `input_char' here. */
			  current_position.column_number++;
			  VLO_ADD_BYTE (symbol_text, input_char);
			  input_char = bc_getc ();
			}
		      while (isdigit_ascii (input_char));
                    }
                }
	      else if (! float_flag && (input_char == 'l' || input_char == 'L'))
		{
		  long_flag = TRUE;
		  current_position.column_number++;
                  input_char = bc_getc ();
		}
	      VLO_ADD_BYTE (symbol_text, '\0');
              bc_ungetc (input_char);
	      if (float_flag)
		token_attr.f = a2f (VLO_BEGIN (symbol_text));
              else if (! long_flag)
		token_attr.i= a2i (VLO_BEGIN (symbol_text), 10);
	      else
		mpz_init_set_str (mpz_attr, VLO_BEGIN (symbol_text), 10);
	      if (errno)
		{
		  d_assert (! long_flag);
		  error (FALSE, current_position,
			 (float_flag ? ERR_float_value : ERR_int_value));
		}
              return curr_token = (float_flag ? D_FLOAT
				   : long_flag ? D_LONG
				   : D_INT);
            }
          else
	    error (FALSE, current_position, ERR_invalid_input_char);
        }
    }

}

/* Fields: */
static const char *fn, *fn2, *fn3;
static int ln, ln2, ln3, pos, pos2, pos3, decl_num;
static int public_p, ext_life_p;
static int fun_p, class_p, thread_p, args_p, simple_p, pure_fun_p, forward_p;
static int fmode, pars_num, min_pars_num;

/* Init fields which may have a default value. */
static void
init_fields (void)
{
  public_p = ext_life_p = fun_p = class_p
    = thread_p = args_p = simple_p = pure_fun_p = forward_p = fmode = 0;
  pars_num = min_pars_num = 0;
}

/* The current read BC node. */
static BC_node_t curr_node;
/* The current read BC field id. */
static int curr_fld;
/* The current read BC field name. */
static const char *curr_fld_name;
/* Bit string of ids of fields which were present for the current BC
   node. */
static char curr_fld_presence[FIELD_BOUND / CHAR_BIT + 1];

/* Check that the current token is equal to EXPECTED_TOKEN.  Fix error
   if not and return true. */
static int
check_fld_val (enum token expected_token)
{
  if (curr_token == expected_token)
    return FALSE;
  error (FALSE, curr_token_position,
	 ERR_wrong_byte_code_field_value, curr_fld_name);
  return TRUE;
}

/* Check that the current node is subclass of EXPECTED_NODE_MODE.  If
   it is not, fix error and return true.  Otherwise, just call
   check_fld_val with EXPECTED token. */
static int
check_fld (BC_node_mode_t expected_node_mode, enum token expected_token)
{
  if (! BC_IS_OF_TYPE (curr_node, expected_node_mode))
    {
      error (FALSE, curr_token_position,
	     ERR_node_has_no_such_field, curr_fld_name);
      return TRUE;
    }
  return check_fld_val (expected_token);
}

/* Check that field with id FLD and name FLD_NAME was present for the
   current node when it is subclass of EXPECTED_NODE_MODE.  If the
   check failed, fix error. */
static void
check_fld_set (BC_node_mode_t expected_node_mode, int fld, const char *fld_name)
{
  if (! BC_IS_OF_TYPE (curr_node, expected_node_mode))
    return;
  if (BIT (curr_fld_presence, fld))
    return;
  error (FALSE, curr_token_position,
	 ERR_undefined_byte_code_field, fld_name);
}

/* Structure storing a reference field representation. */
struct ptr_fld
{
  /* Field id and its repreresntation value. */
  int fld;
  int fld_val;
  /* Node where the field was read. */
  BC_node_t node;
  /* The field name. */
  const char *fld_name;
};

/* Reference fields are stored here until all BC nodes are read. */
static vlo_t ptr_flds;

/* Store the current reference field. */
static void
store_curr_ptr_fld (void)
{
  struct ptr_fld temp;

  temp.fld = curr_fld;
  temp.fld_val = token_attr.i;
  temp.node = curr_node;
  temp.fld_name = curr_fld_name;
  VLO_ADD_MEMORY (ptr_flds, &temp, sizeof (temp));
}

/* Maps translating decl label and bcode label correspondingly into
   the decl and bcode. */
static vlo_t decl_map, bcode_map;

/* Bound LABEL to the current node (decl or bcode). */
static void
bound_label_to_curr_node (int label)
{
  vlo_t *map;

  d_assert (label >= 0);
  if (BC_IS_OF_TYPE (curr_node, BC_NM_decl))
    map = &decl_map;
  else
    {
      d_assert (BC_IS_OF_TYPE (curr_node, BC_NM_bcode));
      map = &bcode_map;
    }
  while (VLO_LENGTH (*map) < (label + 1) * sizeof (BC_node_t))
    VLO_ADD_BYTE (*map, 0);
  ((BC_node_t *) VLO_BEGIN (*map))[label] = curr_node;
}

/* Return bcode with given LABEL. */
static BC_node_t
get_bcode (int label)
{
  BC_node_t res;

  d_assert (label * sizeof (BC_node_t) < VLO_LENGTH (bcode_map));
  res = ((BC_node_t *) VLO_BEGIN (bcode_map))[label];
  d_assert (res != NULL);
  return res;
}

/* Return decl with given LABEL. */
static BC_node_t
get_decl (int label)
{
  BC_node_t res;

  d_assert (label * sizeof (BC_node_t) < VLO_LENGTH (decl_map));
  res = ((BC_node_t *) VLO_BEGIN (decl_map))[label];
  d_assert (res != NULL);
  return res;
}

/* Read byte code from file INPF with name FILE_NAME according to the following syntax:

     program : ln*
     ln : [label nodename field*] nl
     label : D_INT
     nodename : D_IDENT
     field : D_IDENT '=' value
     value : D_INT | D_LONG | D_FLOAT | D_IDENT | D_STRING | D_UCODESTR

   Some fields are not obligatory as they have a default value.  Some
   fields represent nodes different from node where they present
   (e.g. position fields, friend/use fields).  Position fields have
   default values from previous read node.

   Create info nodes if INFO_P.
*/
void
read_bc_program (const char *file_name, FILE *inpf, int info_p)
{
  BC_node_t source_node, prev_node = NULL;
  BC_node_mode_t curr_node_mode;
  int skip_ln, label;
  struct ptr_fld *fld;
  BC_node_t *decl_ptr;
  vlo_t block_infos, all_decls;
  BC_node_t prev_info = NULL;

  start_file_position (file_name);
  input_file = inpf;
  mpz_init (mpz_attr);
  VLO_CREATE (all_decls, 1024);
  VLO_CREATE (ptr_flds, 4096);
  VLO_CREATE (bcode_map, 2048);
  VLO_CREATE (decl_map, 2048);
  if (info_p)
    VLO_CREATE (block_infos, 2048);
  previous_char = NOT_A_CHAR;
  for (skip_ln = FALSE;;)
    {
      get_token ();
      if (curr_token == D_EOF)
	break;
      if (curr_token == D_NL)
	{
	  skip_ln = FALSE;
	  continue;
	}
      if (skip_ln)
	continue;
      init_fields ();
      if (curr_token != D_INT)
	{
	  error (FALSE, curr_token_position, ERR_byte_code_should_start_with_label);
	  goto fail;
	}
      label = token_attr.i;
      get_token ();
      if (curr_token != D_IDENT)
	{
	  error (FALSE, curr_token_position, ERR_byte_code_should_have_name);
	  goto fail;
	}
      curr_node_mode = NR_find_keyword (token_attr.str, strlen (token_attr.str));
      if (curr_node_mode == 0)
	{
	  error (FALSE, curr_token_position, ERR_unknown_byte_code_node);
	  goto fail;
	}
      curr_node = BC_create_node (curr_node_mode);
      if (first_program_bc == NULL)
	first_program_bc = curr_node;
      bound_label_to_curr_node (label);
      /* Read fields: */
      bit_string_set (curr_fld_presence, 0, 0, FIELD_BOUND);
      for (;;)
	{
	  get_token ();
	  if (curr_token == D_NL || curr_token == D_EOF)
	    break;
	  if (curr_token != D_IDENT)
	    {
	      error (FALSE, curr_token_position,
		     ERR_byte_code_field_should_have_name);
	      goto fail;
	    }
	  curr_fld = FR_find_keyword (token_attr.str, strlen (token_attr.str));
	  if (curr_fld == FR__not_found)
	    {
	      error (FALSE, curr_token_position, ERR_unknown_byte_code_field,
		     token_attr.str);
	      goto fail;
	    }
	  curr_fld_name = token_attr.str;
	  get_token ();
	  if (curr_token != D_EQ)
	    {
	      error (FALSE, curr_token_position,
		     ERR_byte_code_field_should_be_followed_by_eq);
	      goto fail;
	    }
	  get_token ();
	  /* Set up fields and check that they belong to the node. */
	  switch (curr_fld)
	    {
	    case FR_fn:
	      if (check_fld_val (D_STRING)) goto fail;
	      fn = token_attr.str;
	      break;
	    case FR_fn2:
	      if (check_fld_val (D_STRING)) goto fail;
	      fn2 = token_attr.str;
	      break;
	    case FR_fn3:
	      if (check_fld_val (D_STRING)) goto fail;
	      fn3 = token_attr.str;
	      break;
	    case FR_ln:
	      if (check_fld_val (D_INT)) goto fail;
	      ln = token_attr.i;
	      break;
	    case FR_ln2:
	      if (check_fld_val (D_INT)) goto fail;
	      ln2 = token_attr.i;
	      break;
	    case FR_ln3:
	      if (check_fld_val (D_INT)) goto fail;
	      ln3 = token_attr.i;
	      break;
	    case FR_pos:
	      if (check_fld_val (D_INT)) goto fail;
	      pos = token_attr.i;
	      break;
	    case FR_pos2:
	      if (check_fld_val (D_INT)) goto fail;
	      pos2 = token_attr.i;
	      break;
	    case FR_pos3:
	      if (check_fld_val (D_INT)) goto fail;
	      pos3 = token_attr.i;
	      break;
	    case FR_decl_num:
	      if (check_fld (BC_NM_decl, D_INT)) goto fail;
	      decl_num = token_attr.i;
	      break;
	    case FR_ident:
	      if (check_fld (BC_NM_decl, D_IDENT)) goto fail;
	      BC_set_ident (curr_node, token_attr.str);
	      break;
	    case FR_fldid_num:
	      if (check_fld (BC_NM_decl, D_INT)) goto fail;
	      BC_set_fldid_num (curr_node, token_attr.i);
	      break;
	    case FR_decl_scope:
	      if (check_fld (BC_NM_decl, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_public_p:
	      if (check_fld (BC_NM_decl, D_INT)) goto fail;
	      public_p = token_attr.i;
	      break;
	    case FR_var_num:
	      if (check_fld (BC_NM_vdecl, D_INT)) goto fail;
	      BC_set_var_num (curr_node, token_attr.i);
	      break;
	    case FR_fblock:
	      if (check_fld (BC_NM_fdecl, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_friend:
	      if (check_fld (BC_NM_block, D_INT)) goto fail;
	      store_curr_ptr_fld ();
	      break;
	    case FR_use:
	      if (check_fld (BC_NM_block, D_INT)) goto fail;
	      store_curr_ptr_fld ();
	      break;
	    case FR_next:
	      if (check_fld (BC_NM_bcode, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_scope:
	      if (check_fld (BC_NM_block, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_vars_num:
	      if (check_fld (BC_NM_block, D_INT)) goto fail;
	      BC_set_vars_num (curr_node, token_attr.i);
	      break;
	    case FR_tvars_num:
	      if (check_fld (BC_NM_block, D_INT)) goto fail;
	      BC_set_tvars_num (curr_node, token_attr.i);
	      break;
	    case FR_excepts:
	      if (check_fld (BC_NM_block, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_ext_life_p:
	      if (check_fld (BC_NM_block, D_INT)) goto fail;
	      ext_life_p = token_attr.i;
	      break;
	    case FR_fdecl:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_fun_p:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      fun_p = token_attr.i;
	      break;
	    case FR_class_p:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      class_p = token_attr.i;
	      break;
	    case FR_thread_p:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      thread_p = token_attr.i;
	      break;
	    case FR_args_p:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      args_p = token_attr.i;
	      break;
	    case FR_simple_p:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      simple_p = token_attr.i;
	      break;
	    case FR_pure_fun_p:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      pure_fun_p = token_attr.i;
	      break;
	    case FR_forward_p:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      forward_p = token_attr.i;
	      break;
	    case FR_fmode:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      fmode = token_attr.i;
	      break;
	    case FR_pars_num:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      pars_num = token_attr.i;
	      break;
	    case FR_min_pars_num:
	      if (check_fld (BC_NM_fblock, D_INT)) goto fail;
	      min_pars_num = token_attr.i;
	      break;
	    case FR_op1:
	      if (check_fld (BC_NM_op1, D_INT)) goto fail;
	      BC_set_op1 (curr_node, token_attr.i);
	      break;
	    case FR_decl:
	      if (check_fld (BC_NM_op1_decl, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_op2:
	      if (check_fld (BC_NM_op2, D_INT)) goto fail;
	      BC_set_op2 (curr_node, token_attr.i);
	      break;
	    case FR_op3:
	      if (check_fld (BC_NM_op3, D_INT)) goto fail;
	      BC_set_op3 (curr_node, token_attr.i);
	      break;
	    case FR_op4:
	      if (check_fld (BC_NM_op4, D_INT)) goto fail;
	      BC_set_op4 (curr_node, token_attr.i);
	      break;
	    case FR_pc:
	      if (check_fld (BC_NM_br, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_res:
	      if (check_fld (BC_NM_brs, D_INT)) goto fail;
	      BC_set_res (curr_node, token_attr.i);
	      break;
	    case FR_bcmp_op2:
	      if (check_fld (BC_NM_bcmp, D_INT)) goto fail;
	      BC_set_bcmp_op2 (curr_node, token_attr.i);
	      break;
	    case FR_bcmp_res:
	      if (check_fld (BC_NM_bcmp, D_INT)) goto fail;
	      BC_set_bcmp_res (curr_node, token_attr.i);
	      break;
	    case FR_binc_inc:
	      if (check_fld (BC_NM_binc, D_INT)) goto fail;
	      BC_set_binc_inc (curr_node, token_attr.i);
	      break;
	    case FR_fail_pc:
	      if (check_fld (BC_NM_check, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_ch_op2:
	      if (check_fld (BC_NM_check2, D_INT)) goto fail;
	      BC_set_ch_op2 (curr_node, token_attr.i);
	      break;
	    case FR_ch_op3:
	      if (check_fld (BC_NM_check3, D_INT)) goto fail;
	      BC_set_ch_op3 (curr_node, token_attr.i);
	      break;
	    case FR_ch_op4:
	      if (check_fld (BC_NM_check4, D_INT)) goto fail;
	      BC_set_ch_op4 (curr_node, token_attr.i);
	      break;
	    case FR_ch_op5:
	      if (check_fld (BC_NM_check4, D_INT)) goto fail;
	      BC_set_ch_op5 (curr_node, token_attr.i);
	      break;
	    case FR_cfblock:
	      if (check_fld (BC_NM_imcall, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_next_except:
	      if (check_fld (BC_NM_except, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_mpz:
	      if (check_fld (BC_NM_ldl, D_LONG)) goto fail;
	      mpz_set (*BC_mpz_ptr (curr_node), mpz_attr);
	      break;
	    case FR_f:
	      if (curr_token == D_FLOAT)
		BC_set_f (curr_node, token_attr.f);
	      else if (check_fld (BC_NM_ldf, D_INT))
		goto fail;
	      else
		BC_set_f (curr_node, (rfloat_t) token_attr.i);
	      break;
	    case FR_bi:
	      if (check_fld (BC_NM_ldbi, D_INT))
		goto fail;
	      else
		BC_set_bi (curr_node, token_attr.i);
	      break;
	    case FR_str:
	      if (check_fld (BC_NM_lds, D_STRING)) goto fail;
	      BC_set_str (curr_node, token_attr.str);
	      break;
	    case FR_rm_str:
	      if (check_fld (BC_NM_rmatchs, D_STRING)) goto fail;
	      BC_set_rm_str (curr_node, token_attr.str);
	    case FR_ustr:
	      if (check_fld (BC_NM_ldus, D_UCODESTR)) goto fail;
	      BC_set_ustr (curr_node, token_attr.ustr);
	      break;
	    case FR_fldid:
	      if (check_fld (BC_NM_field, D_IDENT)) goto fail;
	      BC_set_fldid (curr_node, token_attr.str);
	      break;
	    case FR_mid:
	      if (check_fld (BC_NM_mcall, D_IDENT)) goto fail;
	      BC_set_mid (curr_node, token_attr.str);
	      break;
	    case FR_element:
	      if (check_fld (BC_NM_foreach2, D_INT)) goto fail;
	      BC_set_element (curr_node, token_attr.i); 
	      break;
	    case FR_body_pc:
	      if (check_fld (BC_NM_foreach, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_ret_decl:
	      if (check_fld (BC_NM_ret, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_move_decl:
	      if (check_fld (BC_NM_move, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    case FR_block:
	      if (check_fld (BC_NM_bend, D_INT)) goto fail;
	      store_curr_ptr_fld (); 
	      break;
	    default:
	      error (TRUE, current_position,
		     "Internal error: unprocessed field %s", curr_fld_name);
	    }
	  SET_BIT (curr_fld_presence, curr_fld, 1);
	}
      /* Set up next. */
      if (prev_node != NULL
	  && BC_IS_OF_TYPE (curr_node, BC_NM_bcode)
	  && ! BC_IS_OF_TYPE (prev_node, BC_NM_fbend)
	  && ! (BC_IS_OF_TYPE (prev_node, BC_NM_fblock)
		&& BC_forward_p (prev_node)))
	BC_set_next (prev_node, curr_node);
      if (BIT (curr_fld_presence, FR_next))
	prev_node = NULL;
      else if (BC_IS_OF_TYPE (curr_node, BC_NM_bcode))
	{
	  prev_node = curr_node;
	  BC_set_next (curr_node, NULL);
	}
      /* Set up node fields and check that can be omitted: */
      if (BC_IS_OF_TYPE (curr_node, BC_NM_source))
	source_node = curr_node;
      /* Create source nodes. */
      else if (BIT (curr_fld_presence, FR_pos3))
	source_node = BC_create_node (BC_NM_source3);
      else if (BIT (curr_fld_presence, FR_pos2))
	source_node = BC_create_node (BC_NM_source2);
      else if (BIT (curr_fld_presence, FR_pos))
	source_node = BC_create_node (BC_NM_source);
      if (BC_IS_OF_TYPE (source_node, BC_NM_source3))
	{
	  position_t p;

	  p.path = &no_position;
	  p.file_name = (BIT (curr_fld_presence, FR_fn3) ? fn3 : fn);
	  p.line_number = (BIT (curr_fld_presence, FR_ln3) ? ln3 : ln);;
	  p.column_number = pos3;
	  BC_set_pos3 (source_node, p);
	}
      if (BC_IS_OF_TYPE (source_node, BC_NM_source2))
	{
	  position_t p;

	  p.path = &no_position;
	  p.file_name = (BIT (curr_fld_presence, FR_fn2) ? fn2 : fn);
	  p.line_number = (BIT (curr_fld_presence, FR_ln2) ? ln2 : ln);
	  p.column_number = pos2;
	  BC_set_pos2 (source_node, p);
	}
      if (BC_IS_OF_TYPE (source_node, BC_NM_source))
	{
	  position_t p;

	  p.path = &no_position;
	  p.file_name = fn; p.line_number = ln; p.column_number = pos;
	  BC_set_pos (source_node, p);
	}
      if (curr_node != source_node)
	BC_set_info (curr_node, source_node);
      /* Check obligatory field presence.  */
      check_fld_set (BC_NM_decl, FR_ident, "ident");
      check_fld_set (BC_NM_vdecl, FR_var_num, "var_num");
      check_fld_set (BC_NM_fdecl, FR_fblock, "fblock");
      check_fld_set (BC_NM_block, FR_vars_num, "vars_num");
      check_fld_set (BC_NM_block, FR_tvars_num, "tvars_num");
      check_fld_set (BC_NM_fblock, FR_fdecl, "fdecl");
      check_fld_set (BC_NM_op1, FR_op1, "op1");
      check_fld_set (BC_NM_op1_decl, FR_decl, "decl");
      check_fld_set (BC_NM_op2, FR_op2, "op2");
      check_fld_set (BC_NM_op3, FR_op3, "op3");
      check_fld_set (BC_NM_op4, FR_op4, "op4");
      check_fld_set (BC_NM_br, FR_pc, "pc");
      check_fld_set (BC_NM_brs, FR_res, "res");
      check_fld_set (BC_NM_bcmp, FR_bcmp_op2, "bcmp_op2");
      check_fld_set (BC_NM_bcmp, FR_bcmp_res, "bcmp_res");
      check_fld_set (BC_NM_binc, FR_binc_inc, "binc_inc");
      check_fld_set (BC_NM_check, FR_ch_op2, "ch_op2");
      check_fld_set (BC_NM_check3, FR_ch_op3, "ch_op3");
      check_fld_set (BC_NM_check4, FR_ch_op4, "ch_op4");
      check_fld_set (BC_NM_check5, FR_ch_op5, "ch_op5");
      check_fld_set (BC_NM_imcall, FR_cfblock, "cfblock");
      check_fld_set (BC_NM_ldf, FR_f, "f");
      check_fld_set (BC_NM_lds, FR_str, "str");
      check_fld_set (BC_NM_rmatchs, FR_rm_str, "rm_str");
      check_fld_set (BC_NM_ldus, FR_ustr, "ustr");
      check_fld_set (BC_NM_field, FR_fldid, "fldid");
      check_fld_set (BC_NM_mcall, FR_mid, "mid");
      check_fld_set (BC_NM_foreach, FR_body_pc, "body_pc");
      check_fld_set (BC_NM_foreach2, FR_element, "element");
      check_fld_set (BC_NM_move, FR_move_decl, "move_decl");
      check_fld_set (BC_NM_bend, FR_block, "block");
      /* Link infos: */
      if (info_p)
	{
	  if (BC_IS_OF_TYPE (curr_node, BC_NM_fblock)
	      /* Top level block: */
	      || (BC_IS_OF_TYPE (curr_node, BC_NM_block)
		  && ! BIT (curr_fld_presence, FR_scope)))
	    {
	      VLO_ADD_MEMORY (block_infos, &prev_info, sizeof (prev_info));
	      prev_info = NULL;
	    }
	  if (BC_IS_OF_TYPE (curr_node, BC_NM_bcode))
	    {
	      BC_node_t curr_info = attach_info (curr_node);

	      BC_set_prev_info (curr_info, prev_info);
	      if (prev_info != NULL)
		BC_set_next_info (prev_info, curr_info);
	      prev_info = curr_info;
	    }
	  if (BC_IS_OF_TYPE (curr_node, BC_NM_fbend))
	    {
	      d_assert (VLO_LENGTH (block_infos) != 0);
	      prev_info = ((BC_node_t *) VLO_BOUND (block_infos))[-1];
	      VLO_SHORTEN (block_infos, sizeof (prev_info));
	    }
	}
      /* Set up fields which may have a default value. */
      if (BC_IS_OF_TYPE (curr_node, BC_NM_decl))
	{
	  VLO_ADD_MEMORY (all_decls, &curr_node, sizeof (curr_node));
	  BC_set_public_p (curr_node, public_p != 0);
	}
      if (BC_IS_OF_TYPE (curr_node, BC_NM_block))
	{
	  BC_set_excepts (curr_node, NULL);
	  BC_set_ext_life_p (curr_node, ext_life_p != 0);
	  if (! BIT (curr_fld_presence, FR_scope))
	    BC_set_scope (curr_node, NULL);
	  set_block_number (curr_node);
	}
      if (BC_IS_OF_TYPE (curr_node, BC_NM_fblock))
	{
	  BC_set_fun_p (curr_node, fun_p != 0);
	  BC_set_class_p (curr_node, class_p != 0);
	  BC_set_thread_p (curr_node, thread_p != 0);
	  BC_set_args_p (curr_node, args_p != 0);
	  BC_set_simple_p (curr_node, simple_p != 0);
	  BC_set_pure_fun_p (curr_node, pure_fun_p != 0);
	  BC_set_forward_p (curr_node, forward_p != 0);
	  BC_set_fmode (curr_node, (fun_mode_t) fmode);
	  BC_set_pars_num (curr_node, pars_num);
	  BC_set_min_pars_num (curr_node, min_pars_num);
	}
      if (BC_IS_OF_TYPE (curr_node, BC_NM_ret)
	  && ! BIT (curr_fld_presence, FR_ret_decl))
	BC_set_ret_decl (curr_node, NULL);
      if (BC_IS_OF_TYPE (curr_node, BC_NM_except)
	  && ! BIT (curr_fld_presence, FR_next_except))
	BC_set_next_except (curr_node, NULL);
      if (BC_IS_OF_TYPE (curr_node, BC_NM_check)
	  && ! BIT (curr_fld_presence, FR_fail_pc))
	BC_set_pc (curr_node, NULL);
      continue;
    fail:
      if (curr_token != D_NL)
	skip_ln = TRUE;
    }
  if (info_p)
    VLO_DELETE (block_infos);
  /* Set up pointer fields.  Process them in reverse order to connect
     friends, uses, and decls in the right order. */
  for (fld = (struct ptr_fld *) VLO_BOUND (ptr_flds) - 1;
       fld >= (struct ptr_fld *) VLO_BEGIN (ptr_flds);
       fld--)
    {
      BC_node_t ptr;

      switch (fld->fld)
	{
	case FR_decl_scope:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_decl_scope (fld->node, ptr);
	  BC_set_next_decl (fld->node, BC_decls (ptr));
	  BC_set_decls (ptr, fld->node);
	  break;
	case FR_fblock:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_fblock (fld->node, ptr);
	  break;
	case FR_next:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_next (fld->node, ptr);
	  break;
	case FR_scope:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_scope (fld->node, ptr);
	  break;
	case FR_excepts:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_excepts (fld->node, ptr);
	  break;
	case FR_fdecl:
	  ptr = get_decl (fld->fld_val);
	  BC_set_fdecl (fld->node, ptr);
	  break;
	case FR_decl:
	  ptr = get_decl (fld->fld_val);
	  BC_set_decl (fld->node, ptr);
	  break;
	case FR_friend:
	  {
	    BC_node_t friend = BC_create_node (BC_NM_friend);

	    ptr = get_bcode (fld->fld_val);
	    BC_set_friend (friend, ptr);
	    BC_set_next_friend (friend, BC_friends (fld->node));
	    BC_set_friends (fld->node, friend);
	    break;
	  }
	case FR_use:
	  {
	    BC_node_t use = BC_create_node (BC_NM_use);

	    ptr = get_bcode (fld->fld_val);
	    BC_set_use (use, ptr);
	    BC_set_next_use (use, BC_uses (fld->node));
	    BC_set_uses (fld->node, use);
	    break;
	  }
	case FR_pc:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_pc (fld->node, ptr);
	  break;
	case FR_fail_pc:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_fail_pc (fld->node, ptr);
	  break;
	case FR_cfblock:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_cfblock (fld->node, ptr);
	  break;
	case FR_next_except:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_next_except (fld->node, ptr);
	  break;
	case FR_body_pc:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_body_pc (fld->node, ptr);
	  break;
	case FR_move_decl:
	  ptr = get_decl (fld->fld_val);
	  BC_set_move_decl (fld->node, ptr);
	  break;
	case FR_ret_decl:
	  ptr = get_decl (fld->fld_val);
	  BC_set_ret_decl (fld->node, ptr);
	  break;
	case FR_block:
	  ptr = get_bcode (fld->fld_val);
	  BC_set_block (fld->node, ptr);
	  break;
	default:
	  error (TRUE, current_position,
		 "Internal error: unprocessed pointer field %s", fld->fld_name);
	}
    }
  VLO_DELETE (decl_map);
  VLO_DELETE (bcode_map);
  VLO_DELETE (ptr_flds);
  /* Set up environment and block field table.  */
  init_env_decl_processing ();
  for (decl_ptr = (BC_node_t *) VLO_BEGIN (all_decls);
       decl_ptr < (BC_node_t *) VLO_BOUND (all_decls);
       decl_ptr++)
    {
      d_assert (BC_ident (*decl_ptr) != NULL);
      define_block_decl (*decl_ptr, BC_decl_scope (*decl_ptr));
      if (strcmp (BC_pos (*decl_ptr).file_name, ENVIRONMENT_PSEUDO_FILE_NAME) == 0)
	process_env_decl (*decl_ptr);
    }
  VLO_DELETE (all_decls);
  mpz_clear (mpz_attr);
}

/* Initiate data structures for reading byte code. */
void
initiate_read_bc (void)
{
  VLO_CREATE (temp_vlo, 0);
  VLO_CREATE (symbol_text, 0);
  OS_CREATE (read_bc, 0);
  initiate_string_tables ();
}

/* Finilize data structures for read byte code.  It should be done
   when the byte code not necessary anymore (e.g. after its
   execution). */
void
finish_read_bc (void)
{
  delete_string_tables ();
  OS_DELETE (read_bc);
  VLO_DELETE (symbol_text);
  VLO_DELETE (temp_vlo);
}
