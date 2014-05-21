/*
   Copyright (C) 1997-2014 Vladimir Makarov.

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

#include "d_run.h"
#include "d_built.h"
#include "d_conv.h"
#include "d_func.h"

#include <sys/types.h>
#include <sys/stat.h>

#include <dirent.h>
#include <pwd.h>
#include <grp.h>
#include <unistd.h>

#define NEW_VECTOR

#define below_ctop (IVAL (ctop, -1))

/* Where to put the standard function result.  */
static ER_node_t fun_result;

static char *
getun (void)
{
  struct passwd *p;

  p = getpwuid (getuid ());
#ifdef __CYGWIN__
  if (p == NULL)
    return "Unknown";
#endif
  return p->pw_name;
}

static char *
geteun (void)
{
  struct passwd *p;

  p = getpwuid (geteuid ());
#ifdef __CYGWIN__
  if (p == NULL)
    return "Unknown";
#endif
  return p->pw_name;
}

static char *
getgn (void)
{
  struct group *p;

  p = getgrgid (getgid ());
  if (p == NULL)
    return "Unknown";
  return p->gr_name;
}

static char *
getegn (void)
{
  struct group *p;

  p = getgrgid (getegid ());
  if (p == NULL)
    return "Unknown";
  return p->gr_name;
}

#ifdef HAVE_TIME_H
#include <time.h>
#else
extern clock_t clock (void);
#endif

#ifdef AIX_DLOPEN
#include "d_aixdl.c"
#endif

#include "regex.h"

static ER_node_t create_class_stack (val_t *call_start, int_t pars_number,
				     int simple_p);

static void
min_max_call (int_t pars_number, int min_flag)
{
  ER_node_t val, r, v;
  int_t i;
  val_t res;

  if (pars_number < 2)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, (min_flag ? MIN_NAME : MAX_NAME));
  for (i = 0; i < pars_number; i++)
    {
      implicit_arithmetic_conversion (IVAL (ctop, -i), NULL);
      val = IVAL (ctop, -i);
      if (ER_NODE_MODE (val) != ER_NM_int && ER_NODE_MODE (val) != ER_NM_long
	  && ER_NODE_MODE (val) != ER_NM_float)
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, (min_flag ? MIN_NAME : MAX_NAME));
      if (i == 0)
	{
	  res = *(val_t *) val;
	  continue;
	}
      if (ER_NODE_MODE (val) == ER_NODE_MODE ((ER_node_t) &res))
	{
	  r = (ER_node_t) &res;
	  v = val;
	}
      else
	{
	  implicit_conversion_for_binary_arithmetic_op
	    ((ER_node_t) &res, val, &r, &v);
	  d_assert (ER_NODE_MODE (r) != ER_NM_int
		    && ER_NODE_MODE (v) != ER_NM_int);
	}
      d_assert (ER_NODE_MODE (r) == ER_NODE_MODE (v));
      if (ER_NODE_MODE (v) == ER_NM_int)
	{
	  if ((ER_i (v) < ER_i (r)) == min_flag)
	    res = *(val_t *) val;
	}
      else if (ER_NODE_MODE (v) == ER_NM_long)
	{
	  if ((mpz_cmp (*ER_mpz_ptr (ER_l (v)),
			*ER_mpz_ptr (ER_l (r))) < 0) == min_flag)
	    res = *(val_t *) val;
	}
      else if ((ER_f (v) < ER_f (r)) == min_flag)
	res = *(val_t *) val;
    }
  *(val_t *) fun_result = res;
}

void
min_call (int_t pars_number)
{
  min_max_call (pars_number, TRUE);
}

void
max_call (int_t pars_number)
{
  min_max_call (pars_number, FALSE);
}

static void
to_lower_upper (int_t pars_number, int lower_flag)
{
  ER_node_t vect;
  const char *name = (lower_flag ? TOLOWER_NAME : TOUPPER_NAME);
  char *str;
  size_t len;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (), DERR_parameters_number, name);
  to_vect_string_conversion (ctop, NULL, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, name);
  vect = ER_vect (ctop);
#ifndef NEW_VECTOR
  if (ER_immutable (ER_vect (ctop)))
    eval_error (immutable_bc_decl, invaccesses_bc_decl, get_cpos (),
		DERR_immutable_vector_modification);
#endif
  len = strlen (ER_pack_els (vect));
#ifdef NEW_VECTOR
  vect = create_empty_string (len + 1);
  strcpy (ER_pack_els (vect), ER_pack_els (ER_vect (ctop)));
  ER_set_els_number (vect, len);
#endif
  for (str = ER_pack_els (vect); *str != 0; str++)
    if (isalpha (*str))
      *str = (lower_flag ? tolower (*str) : toupper (*str));
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

void
toupper_call (int_t pars_number)
{
  to_lower_upper (pars_number, FALSE);
}

void
tolower_call (int_t pars_number)
{
  to_lower_upper (pars_number, TRUE);
}

void
transliterate_call (int_t pars_number)
{
  ER_node_t vect, v;
  char *str, *subst, map [256];
  int i;
  size_t len;

  if (pars_number != 3)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, TRANSLITERATE_NAME);
  to_vect_string_conversion (IVAL (ctop, -2), NULL, NULL);
  if (ER_NODE_MODE (ctop) == ER_NM_vect)
    {
      v = ER_vect (ctop);
      GO_THROUGH_REDIR (v);
      if (ER_NODE_MODE (v) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (v);
      ER_set_vect (ctop, v);
    }
  if (ER_NODE_MODE (below_ctop) == ER_NM_vect)
    {
      v = ER_vect (below_ctop);
      GO_THROUGH_REDIR (v);
      if (ER_NODE_MODE (v) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (v);
      ER_set_vect (below_ctop, v);
    }
  vect = IVAL (ctop, -2);
  if (ER_NODE_MODE (vect) == ER_NM_vect)
    {
      v = ER_vect (vect);
      GO_THROUGH_REDIR (v);
      if (ER_NODE_MODE (v) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (v);
      ER_set_vect (vect, v);
    }
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || (ER_els_number (ER_vect (ctop)) != 0
	  && (ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char))
      || ER_NODE_MODE (below_ctop) != ER_NM_vect
      || (ER_els_number (ER_vect (below_ctop)) != 0
	  && (ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char))
      || ER_els_number (ER_vect (ctop)) != ER_els_number (ER_vect (below_ctop))
      || ER_NODE_MODE (vect) != ER_NM_vect
      || (ER_els_number (ER_vect (vect)) != 0
	  && (ER_NODE_MODE (ER_vect (vect)) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (ER_vect (vect)) != ER_NM_char)))
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, TRANSLITERATE_NAME);
  vect = ER_vect (vect);
#ifndef NEW_VECTOR
  if (ER_immutable (vect))
    eval_error (immutable_bc_decl, invaccesses_bc_decl, get_cpos (),
		DERR_immutable_vector_modification);
#endif
  len = ER_els_number (vect);
#ifdef NEW_VECTOR
  vect = create_empty_string (len);
  ER_set_els_number (vect, len);
#endif
  if (len != 0 && ER_els_number (ER_vect (ctop)) != 0)
    {
#ifdef NEW_VECTOR
      memcpy (ER_pack_els (vect),
	      ER_pack_els (ER_vect (IVAL (ctop, -2))), len);
#endif
      for (i = 0; i < 256; i++)
	map [i] = i;
      subst = ER_pack_els (ER_vect (ctop));
      for (i = 0, str = ER_pack_els (ER_vect (below_ctop));
	   i < ER_els_number (ER_vect (below_ctop));
	   i++)
	map [(unsigned) str [i]] = subst [i];
      for (str = ER_pack_els (vect), i = 0; i < len; i++)
	str [i] = map [(unsigned) str [i]];
    }
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

void
eltype_call (int_t pars_number)
{
  ER_node_t vect;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, ELTYPE_NAME);
  if (ER_NODE_MODE (ctop) != ER_NM_vect)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, ELTYPE_NAME);
  vect = ER_vect (ctop);
  GO_THROUGH_REDIR (vect);
  if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
    pack_vector_if_possible (vect);
  /* Place the result instead of the function. */
  if (ER_NODE_MODE (vect) != ER_NM_heap_pack_vect)
    ER_SET_MODE (fun_result, ER_NM_nil);
  else
    {
      ER_SET_MODE (fun_result, ER_NM_type);
      ER_set_type (fun_result, ER_pack_vect_el_type (vect));
    }
}

void
keys_call (int_t pars_number)
{
  ER_node_t tab;
  ER_node_t vect;
  size_t i, index;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, KEYS_NAME);
  if (ER_NODE_MODE (ctop) != ER_NM_tab)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, KEYS_NAME);
  tab = ER_tab (ctop);
  GO_THROUGH_REDIR (tab);
  /* Place the result instead of the function. */
  index = 0;
  if (ER_els_number (tab) == 0)
    vect = create_empty_vector ();
  else
    vect = create_unpack_vector (ER_els_number (tab));
  for (i = 0; i < ER_entries_number (tab); i++)
    if (ER_NODE_MODE (INDEXED_ENTRY_KEY (ER_tab_els (tab), i))
	!= ER_NM_empty_entry
	&& (ER_NODE_MODE (INDEXED_ENTRY_KEY (ER_tab_els (tab), i))
	    != ER_NM_deleted_entry))
      {
	*(val_t *) IVAL (ER_unpack_els (vect), index)
	  = *(val_t *) INDEXED_ENTRY_KEY (ER_tab_els (tab), i);
	index++;
      }
  if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
    pack_vector_if_possible (vect);
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

void
context_call (int_t pars_number)
{
  ER_node_t val;
  ER_node_t context;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, CONTEXT_NAME);
  val = IVAL (ctop, -pars_number + 1);
  if (ER_NODE_MODE (val) == ER_NM_stack)
    context = ER_context (ER_stack (val));
  else if (ER_NODE_MODE (val) == ER_NM_code)
    context = ER_code_context (val);
  else if (ER_NODE_MODE (val) == ER_NM_process)
    context = ER_context (ER_process (val));
  else
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, CONTEXT_NAME);
  /* Place the result instead of the function. */
  if (context == NULL)
    ER_SET_MODE (fun_result, ER_NM_nil);
  else
    {
      d_assert (ER_NODE_MODE (context) == ER_NM_heap_stack);
      ER_SET_MODE (fun_result, ER_NM_stack);
      ER_set_stack (fun_result, context);
    }
}

int
internal_inside_call (const char **message_ptr, ER_node_t where, ER_node_t what,
		      int context_flag)
{
  ER_node_t code_context;
  ER_node_t code_2_context;
  BC_node_t code;
  BC_node_t code_2;
  int result;

  *message_ptr = NULL;
  code = NULL;
  if (ER_NODE_MODE (what) == ER_NM_stack)
    {
      code_context = ER_stack (what);
      code = ER_block_node (code_context);
    }
  else if (ER_NODE_MODE (what) == ER_NM_code)
    {
      code = ID_TO_CODE (ER_code_id (what));
      code_context = ER_code_context (what);
    }
  else
    {
      *message_ptr = DERR_parameter_type;
      return 0;
    }
  if (ER_IS_OF_TYPE (where, ER_NM_code))
    {
      code_2 = ID_TO_CODE (ER_code_id (where));
      code_2_context = ER_code_context (where);
    }
  else
    {
      *message_ptr = DERR_parameter_type;
      return 0;
    }
  result = (code == code_2
	    && (!context_flag || code_context == code_2_context));
  if (code != NULL && ER_NODE_MODE (what) == ER_NM_code)
    code = BC_scope (code);
  for (; !result && code != NULL;
       code = BC_scope (code), code_context = ER_context (code_context))
    if (code == code_2
	&& (!context_flag
	    || ER_context (code_context) == code_2_context))
      result = 1;
  return result;
}

void
inside_call (int_t pars_number)
{
  const char *message;
  int result;
  int flag;

  if (pars_number != 2 && pars_number != 3)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, INSIDE_NAME);
  flag = 0;
  if (pars_number == 3)
    {
      implicit_int_conversion (ctop, NULL);
      if (!ER_IS_OF_TYPE (ctop, ER_NM_int))
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, INSIDE_NAME);
      flag = ER_i (ctop);
    }
  result = internal_inside_call (&message, IVAL (ctop, 2 - pars_number),
				 IVAL (ctop, 1 - pars_number), flag);
  if (message != NULL)
    eval_error (partype_bc_decl, get_cpos (), message, INSIDE_NAME);
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, result);
}

int
internal_isa_call (const char **message_ptr, ER_node_t where, ER_node_t what)
{
  BC_node_t code, code_2, use;
  int result;

  if (message_ptr != NULL)
    *message_ptr = NULL;
  if (ER_NODE_MODE (what) == ER_NM_stack)
    {
      ER_node_t code_context = ER_stack (what);

      code = ER_block_node (code_context);
    }
  else if (ER_NODE_MODE (what) == ER_NM_code)
    code = ID_TO_CODE (ER_code_id (what));
  else
    {
      if (message_ptr != NULL)
	*message_ptr = DERR_parameter_type;
      return 0;
    }
  if (ER_IS_OF_TYPE (where, ER_NM_code))
    code_2 = ID_TO_CODE (ER_code_id (where));
  else
    {
      if (message_ptr != NULL)
	*message_ptr = DERR_parameter_type;
      return 0;
    }
  for (result = code == code_2, use = BC_uses (code);
       ! result && use != NULL;
       use = BC_next_use (use))
    if (BC_use (use) == code_2)
      result = 1;
  return result;
}

void
isa_call (int_t pars_number)
{
  const char *message;
  int result;

  if (pars_number != 2)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, ISA_NAME);
  result = internal_isa_call (&message, IVAL (ctop, 0), IVAL (ctop, -1));
  if (message != NULL)
    eval_error (partype_bc_decl, get_cpos (), message, ISA_NAME);
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, result);
}

static void
process_regcomp_errors (int code, const char *function_name)
{
  if (code == REG_EBRACK)
    eval_error (ebrack_bc_decl, get_cpos (), DERR_reg_ebrack, function_name);
  else if (code == REG_ERANGE)
    eval_error (reg_erange_bc_decl, get_cpos (),
		DERR_reg_erange, function_name);
  else if (code == REG_ECTYPE)
    eval_error (ectype_bc_decl, get_cpos (), DERR_reg_ectype, function_name);
  else if (code == REG_EPAREN)
    eval_error (eparen_bc_decl, get_cpos (), DERR_reg_eparen, function_name);
  else if (code == REG_ESUBREG)
    eval_error (esubreg_bc_decl, get_cpos (), DERR_reg_esubreg, function_name);
  else if (code == REG_EEND)
    eval_error (eend_bc_decl, get_cpos (), DERR_reg_eend, function_name);
  else if (code == REG_EESCAPE)
    eval_error (eescape_bc_decl, get_cpos (), DERR_reg_eescape, function_name);
  else if (code == REG_BADPAT || code == REG_BADRPT
	   || code == REG_BADBR || code == REG_EBRACE)
    /* We use badpat because I can not find badrpt, badbr, ebrace
       diagnostics for POSIX in GNU Regex. */
    eval_error (badpat_bc_decl, get_cpos (), DERR_reg_badpat, function_name);
  else if (code == REG_ESIZE)
    eval_error (esize_bc_decl, get_cpos (), DERR_reg_esize, function_name);
  else if (code == REG_ESPACE)
    eval_error (espace_bc_decl, get_cpos (), DERR_reg_espace, function_name);
  else
    /* Internal error: may be something else. */
    eval_error (internal_bc_decl, get_cpos (),
		DERR_internal_error, function_name);
}

#define RE_DINO_SYNTAX (REG_EXTENDED)

/* The following structure is element of the cache of compiled
   regex. */
struct regex_node
{
  /* Compiled regex. */
  regex_t regex;
  /* Regex string representation.  It is a key of in the cache. */
  const char *string;
};

/* Temporary structure. */
static struct regex_node regex_node;

/* Hash table which implements the cache. */
static hash_table_t regex_tab;
/* This object stack contains elements of the cache. */
static os_t regex_os;
/* Vector containing pointers to the cache elements. */
static vlo_t regex_vlo;

/* Hash of the node. */
static unsigned
regex_node_hash (hash_table_entry_t n)
{
  unsigned hash_value, i;
  const char *str = ((struct regex_node *) n)->string;

  for (hash_value = i =0; *str != '\0'; str++, i++)
    hash_value += ((unsigned char) *str << (i % CHAR_BIT));
  return hash_value;
}

/* Equality of nodes. */
static int
regex_node_eq (hash_table_entry_t n1, hash_table_entry_t n2)
{
  struct regex_node *node1 = ((struct regex_node *) n1);
  struct regex_node *node2 = ((struct regex_node *) n2);

  return strcmp (node1->string, node2->string) == 0;
}

/* Find compiled version of regex STRING in the cache.  If it is
   absent, compile it and insert it into cache.  Returns nonzero if
   there were errors during the compilation. */
static int
find_regex (const char *string, regex_t **result)
{
  hash_table_entry_t *entry;
  struct regex_node *reg;
  int code;

  *result = NULL;
  regex_node.string = string;
  entry = find_hash_table_entry (regex_tab, &regex_node, FALSE);
  if (*entry != NULL)
    {
      *result = &((struct regex_node *) (*entry))->regex;
      return 0;
    }
  OS_TOP_EXPAND (regex_os, sizeof (struct regex_node));
  reg = OS_TOP_BEGIN (regex_os);
  code = regcomp (&reg->regex, string, RE_DINO_SYNTAX);
  if (code != 0)
    {
      regfree (&reg->regex);
      OS_TOP_NULLIFY (regex_os);
      return code;
    }
#ifndef USE_POSIX_REGEXEC_FUNCTION
  reg->regex.not_bol = 0;
  reg->regex.not_eol = 0;
  reg->regex.regs_allocated = REGS_FIXED;
#endif
  OS_TOP_FINISH (regex_os);
  VLO_ADD_MEMORY (regex_vlo, &reg, sizeof (reg));
  OS_TOP_EXPAND (regex_os, strlen (string) + 1);
  reg->string = OS_TOP_BEGIN (regex_os);
  OS_TOP_FINISH (regex_os);
  strcpy ((char *) reg->string, string);
  entry = find_hash_table_entry (regex_tab, reg, TRUE);
  *entry = reg;
  *result = &reg->regex;
  return 0;
}

/* Create the cache of compiled regexs. */
static void
initiate_regex_tab (void)
{
  OS_CREATE (regex_os, 0);
  VLO_CREATE (regex_vlo, 0);
  regex_tab = create_hash_table (400, regex_node_hash, regex_node_eq);
}

/* Delete the cache of compiled regexs. */
static void
finish_regex_tab (void)
{
  int i;

  delete_hash_table (regex_tab);
  for (i = 0; i < VLO_LENGTH (regex_vlo) / sizeof (struct regex_node *); i++)
    regfree (&((struct regex_node **) VLO_BEGIN (regex_vlo)) [i]->regex);
  VLO_DELETE (regex_vlo);
  OS_DELETE (regex_os);
}

void
match_call (int_t pars_number)
{
  regex_t *reg;
#ifndef USE_POSIX_REGEXEC_FUNCTION
  int len;
  struct re_registers regs;
#else
  regmatch_t *pmatch;
#endif
  ER_node_t result;
  size_t els_number;
  size_t i;
  int code;

  if (pars_number != 2)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, MATCH_NAME);
  to_vect_string_conversion (ctop, NULL, NULL);
  to_vect_string_conversion (below_ctop, NULL, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || ER_NODE_MODE (below_ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char)
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, MATCH_NAME);
  code = find_regex (ER_pack_els (ER_vect (below_ctop)), &reg);
  if (code != 0)
    process_regcomp_errors (code, MATCH_NAME);
  else
    {
      els_number = (reg->re_nsub + 1) * 2;
      /* Make pmatch vector. */
      VLO_NULLIFY (temp_vlobj);
#ifndef USE_POSIX_REGEXEC_FUNCTION
      regs.num_regs = reg->re_nsub + 1;
      VLO_EXPAND (temp_vlobj, 2 * (reg->re_nsub + 1) * sizeof (regoff_t));
      regs.start = VLO_BEGIN (temp_vlobj);
      regs.end = regs.start + reg->re_nsub + 1;
      len = strlen (ER_pack_els (ER_vect (ctop)));
      if (re_search (reg, ER_pack_els (ER_vect (ctop)),
		     len, 0, len, &regs) >= 0)
	{
	  result = create_pack_vector (els_number, ER_NM_int);
	  for (i = 0; i < els_number; i += 2)
	    {
	      ((int_t *) ER_pack_els (result)) [i] = regs.start[i / 2];
	      ((int_t *) ER_pack_els (result)) [i + 1] = regs.end[i / 2];
	    }
	}
      else
	result = NULL;
#else
      VLO_EXPAND (temp_vlobj, (reg->re_nsub + 1) * sizeof (regmatch_t));
      pmatch = VLO_BEGIN (temp_vlobj);
      if (!regexec (reg, ER_pack_els (ER_vect (ctop)), reg->re_nsub + 1,
		    pmatch, 0))
	{
	  result = create_pack_vector (els_number, ER_NM_int);
	  for (i = 0; i < els_number; i += 2)
	    {
	      ((int_t *) ER_pack_els (result)) [i] = pmatch[i / 2].rm_so;
	      ((int_t *) ER_pack_els (result)) [i + 1] = pmatch[i / 2].rm_eo;
	    }
	}
      else
	result = NULL;
#endif
    }
  if (result == NULL)
    ER_SET_MODE (fun_result, ER_NM_nil);
  else
    {
      ER_SET_MODE (fun_result, ER_NM_vect);
      set_vect_dim (fun_result, result, 0);
    }
}

void
gmatch_call (int_t pars_number)
{
  regex_t *reg;
#ifndef USE_POSIX_REGEXEC_FUNCTION
  int len;
  struct re_registers regs;
#else
  regmatch_t *pmatch;
#endif
  ER_node_t par1, par2, result;
  int code, flag, count, disp;
  int_t el;
  const char *start;

  if (pars_number != 2 && pars_number != 3)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, GMATCH_NAME);
  flag = 0;
  if (pars_number == 3)
    {
      implicit_int_conversion (ctop, NULL);
      if (!ER_IS_OF_TYPE (ctop, ER_NM_int))
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, GMATCH_NAME);
      flag = ER_i (ctop);
    }
  par1 = IVAL (ctop, 1 - pars_number);
  par2 = IVAL (ctop, 2 - pars_number);
  to_vect_string_conversion (par2, NULL, NULL);
  to_vect_string_conversion (par1, NULL, NULL);
  if (ER_NODE_MODE (par2) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (par2)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (par2)) != ER_NM_char
      || ER_NODE_MODE (par1) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (par1)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (par1)) != ER_NM_char)
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, GMATCH_NAME);
  code = find_regex (ER_pack_els (ER_vect (par1)), &reg);
  if (code != 0)
    process_regcomp_errors (code, GMATCH_NAME);
  VLO_NULLIFY (temp_vlobj);
#ifndef USE_POSIX_REGEXEC_FUNCTION
  regs.num_regs = reg->re_nsub + 1;
  VLO_EXPAND (temp_vlobj, 2 * (reg->re_nsub + 1) * sizeof (regoff_t));
  regs.start = VLO_BEGIN (temp_vlobj);
  regs.end = regs.start + reg->re_nsub + 1;
#else
  /* Make vector which can store regmatch_t's. */
  VLO_EXPAND (temp_vlobj, (reg->re_nsub + 1) * sizeof (regmatch_t));
  pmatch = (regmatch_t *) VLO_BEGIN (temp_vlobj);
#endif
  VLO_NULLIFY (temp_vlobj2);
  start = ER_pack_els (ER_vect (par2));
  disp = 0;
  count = 0;
#ifndef USE_POSIX_REGEXEC_FUNCTION
  len = strlen (start);
  while (re_search (reg, start + disp, len, 0, len, &regs) >= 0)
    {
      el = regs.start [0] + disp;
      VLO_ADD_MEMORY (temp_vlobj2, &el, sizeof (el));
      el = regs.end [0] + disp;
      VLO_ADD_MEMORY (temp_vlobj2, &el, sizeof (el));
      if (flag)
	{
	  disp++;
	  len--;
	}
      else
	{
	  disp += regs.end [0];
	  len -= regs.end [0];
	}
      count++;
    }
#else
  while (!regexec (reg, start + disp, reg->re_nsub + 1, pmatch, 0))
    {
      el = pmatch[0].rm_so + disp;
      VLO_ADD_MEMORY (temp_vlobj2, &el, sizeof (el));
      el = pmatch[0].rm_eo + disp;
      VLO_ADD_MEMORY (temp_vlobj2, &el, sizeof (el));
      disp += (!flag ? pmatch[0].rm_eo : 1);
      count++;
    }
#endif
  if (count == 0)
    ER_SET_MODE (fun_result, ER_NM_nil);
  else
    {
      result = create_pack_vector (2 * count, ER_NM_int);
      memcpy (ER_pack_els (result), VLO_BEGIN (temp_vlobj2),
	      2 * count * sizeof (el));
      ER_SET_MODE (fun_result, ER_NM_vect);
      set_vect_dim (fun_result, result, 0);
    }
}

static void
generall_sub_call (int_t pars_number, int global_flag)
{
  regex_t *reg;
#ifndef USE_POSIX_REGEXEC_FUNCTION
  int len;
  struct re_registers regs;
  regoff_t starts [10], ends [10];
#else
  regmatch_t pmatch [10];
#endif
  size_t sub_length [10];
  size_t n_subst;
  ER_node_t result;
  ER_node_t vect;
  ER_node_t regexp_val;
  size_t length;
  size_t evaluated_length;
  size_t start;
  size_t i;
  const char *substitution;
  const char *src;
  char *dst;
  int c;
  int code;

  if (pars_number != 3)
    eval_error (parnumber_bc_decl, get_cpos (), DERR_parameters_number,
		global_flag ? GSUB_NAME : SUB_NAME);
  to_vect_string_conversion (ctop, NULL, NULL);
  to_vect_string_conversion (below_ctop, NULL, NULL);
  regexp_val = IVAL (ctop, -2);
  to_vect_string_conversion (regexp_val, NULL, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || ER_NODE_MODE (below_ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char
      || ER_NODE_MODE (regexp_val) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (regexp_val)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (regexp_val)) != ER_NM_char)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, global_flag ? GSUB_NAME : SUB_NAME);
  code = find_regex (ER_pack_els (ER_vect (regexp_val)), &reg);
  if (code != 0)
    process_regcomp_errors (code, global_flag ? GSUB_NAME : SUB_NAME);
  else
    {
      vect = ER_vect (below_ctop);
      /* Count result string length. */
      start = 0;
      n_subst = 0;
      for (i = 0; i < 10; i++)
	sub_length [i] = 0;
#ifndef USE_POSIX_REGEXEC_FUNCTION
      regs.num_regs = 10;
      regs.start = starts;
      regs.end = ends;
      len = strlen (ER_pack_els (vect));
      while ((start < ER_els_number (vect) || start == 0)
	     && re_search (reg, ER_pack_els (vect) + start, len, 0, len,
			   &regs) >= 0)
	{
	  for (i = 0; i < (reg->re_nsub + 1 > 10 ? 10 : reg->re_nsub + 1); i++)
	    sub_length [i] += (regs.end [i] - regs.start [i]);
	  if (regs.end [0] == 0)
	    {
	      start++;
	      len--;
	    }
	  else
	    {
	      start += regs.end [0];
	      len -= regs.end [0];
	    }
	  n_subst++;
	  if (!global_flag)
	    break;
	}
#else
      while ((start < ER_els_number (vect) || start == 0)
	     && !regexec (reg, ER_pack_els (vect) + start, 10, pmatch, 0))
	{
	  for (i = 0; i < (reg->re_nsub + 1 > 10 ? 10 : reg->re_nsub + 1); i++)
	    sub_length [i] += (pmatch[i].rm_eo - pmatch[i].rm_so);
	  start += (pmatch[0].rm_eo == 0 ? 1 : pmatch[0].rm_eo);
	  n_subst++;
	  if (!global_flag)
	    break;
	}
#endif
      substitution = ER_pack_els (ER_vect (ctop));
      evaluated_length = ER_els_number (vect) - sub_length [0];
      while (*substitution != '\0')
	{
	  c = *substitution++;
	  if (c == '&')
	    evaluated_length += sub_length [0];
	  else if (c == '\\' && '0' <= *substitution && *substitution <= '9')
	    evaluated_length += sub_length [*substitution++ - '0'];
	  else
	    {
	      if (c == '\\' && (*substitution == '\\' || *substitution == '&'))
		substitution++;
	      evaluated_length += n_subst;
	    }
	}
      result = create_pack_vector (evaluated_length, ER_NM_char);
      ER_set_els_number (result, 0);
      /* Make actual substitution. */
      start = length = 0;
      dst = ER_pack_els (result);
      substitution = ER_pack_els (ER_vect (ctop));
#ifndef USE_POSIX_REGEXEC_FUNCTION
      len = strlen (ER_pack_els (vect));
      while ((start < ER_els_number (vect) || start == 0)
	     && re_search (reg, ER_pack_els (vect) + start, len, 0, len,
			   &regs) >= 0)
	{
	  if (regs.start[0] != 0)
	    memcpy (dst, ER_pack_els (vect) + start, regs.start[0]);
	  length += regs.start[0];
	  dst += regs.start[0];
	  src = substitution;
	  while (*src != '\0')
	    {
	      c = *src++;
	      if (c == '&')
		i = 0;
	      else if (c == '\\' && '0' <= *src && *src <= '9')
		i = *src++ - '0';
	      else
		i = 10;
	      
	      if (i >= 10)
		{
		  if (c == '\\' && (*src == '\\' || *src == '&'))
		    c = *src ++;
		  *dst++ = c;
		  length++;
		}
	      else if (i < reg->re_nsub + 1
		       && regs.end[i] != regs.start[i])
		{
		  memcpy (dst, ER_pack_els (vect) + start + regs.start[i],
			  regs.end[i] - regs.start[i]);
		  dst += regs.end[i] - regs.start[i];
		  length += regs.end[i] - regs.start[i];
		}
	    }
	  if (regs.end[0] == 0)
	    {
	      /* Matched empty string */
	      if (ER_els_number (vect) != 0)
		{
		  *dst++ = *(ER_pack_els (vect) + start);
		  length++;
		}
	      start++;
	      len--;
	    }
	  else
	    {
	      start += regs.end[0];
	      len -= regs.end[0];
	    }
	  if (!global_flag)
	    break;
	}
#else
      while ((start < ER_els_number (vect) || start == 0)
	     && !regexec (reg, ER_pack_els (vect) + start, 10, pmatch, 0))
	{
	  if (pmatch[0].rm_so != 0)
	    memcpy (dst, ER_pack_els (vect) + start, pmatch[0].rm_so);
	  length += pmatch[0].rm_so;
	  dst += pmatch[0].rm_so;
	  src = substitution;
	  while (*src != '\0')
	    {
	      c = *src++;
	      if (c == '&')
		i = 0;
	      else if (c == '\\' && '0' <= *src && *src <= '9')
		i = *src++ - '0';
	      else
		i = 10;
	      
	      if (i >= 10)
		{
		  if (c == '\\' && (*src == '\\' || *src == '&'))
		    c = *src ++;
		  *dst++ = c;
		  length++;
		}
	      else if (i < reg->re_nsub + 1
		       && pmatch[i].rm_eo != pmatch[i].rm_so)
		{
		  memcpy (dst, ER_pack_els (vect) + start + pmatch[i].rm_so,
			  pmatch[i].rm_eo - pmatch[i].rm_so);
		  dst += pmatch[i].rm_eo - pmatch[i].rm_so;
		  length += pmatch[i].rm_eo - pmatch[i].rm_so;
		}
	    }
	  if (pmatch[0].rm_eo == 0)
	    {
	      /* Matched empty string */
	      if (ER_els_number (vect) != 0)
		{
		  *dst++ = *(ER_pack_els (vect) + start);
		  length++;
		}
	      start++;
	    }
	  else
	    start += pmatch[0].rm_eo;
	  if (!global_flag)
	    break;
	}
#endif
      if (start < ER_els_number (vect))
	{
	  memcpy (dst, ER_pack_els (vect) + start,
		  ER_els_number (vect) - start);
	  length += ER_els_number (vect) - start;
	  dst += ER_els_number (vect) - start;
	}
      *dst = '\0';
      ER_set_els_number (result, length);
      d_assert (length == evaluated_length);
    }
  if (result == NULL)
    ER_SET_MODE (fun_result, ER_NM_nil);
  else
    {
      ER_SET_MODE (fun_result, ER_NM_vect);
      set_vect_dim (fun_result, result, 0);
    }
}

void
sub_call (int_t pars_number)
{
  generall_sub_call (pars_number, FALSE);
}

void
gsub_call (int_t pars_number)
{
  generall_sub_call (pars_number, TRUE);
}

void
split_call (int_t pars_number)
{
  regex_t *reg;
#ifndef USE_POSIX_REGEXEC_FUNCTION
  int len;
  struct re_registers regs;
  regoff_t regs_start, regs_end;
#else
  regmatch_t pmatch [1];
#endif
  ER_node_t result;
  ER_node_t vect;
  ER_node_t sub_vect;
  size_t els_number;
  size_t chars_number;
  size_t start;
  const char *split_regex;
  ER_node_t split_var;
  int ch;
  int ok;
  int code;

  if (pars_number != 1 && pars_number != 2)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, SPLIT_NAME);
  if (pars_number != 1)
    to_vect_string_conversion (below_ctop, NULL, NULL);
  to_vect_string_conversion (ctop, NULL, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || (pars_number == 2
	  && (ER_NODE_MODE (below_ctop) != ER_NM_vect
	      || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char)))
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, SPLIT_NAME);
  if (pars_number == 2)
    split_regex = ER_pack_els (ER_vect (ctop));
  else
    {
      split_var = IVAL (ER_stack_vars (uppest_stack),
			BC_var_num (split_regex_bc_decl));
      to_vect_string_conversion (split_var, NULL, NULL);
      split_var = IVAL (ER_stack_vars (uppest_stack),
			BC_var_num (split_regex_bc_decl));
      if (ER_NODE_MODE (split_var) == ER_NM_vect
	  && ER_NODE_MODE (ER_vect (split_var)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (ER_vect (split_var)) == ER_NM_char)
        split_regex = ER_pack_els (ER_vect (split_var));
      else
	eval_error (invenvar_bc_decl,
		    get_cpos (), DERR_corrupted_environment_var,
		    SPLIT_REGEX_NAME);
    }
  code = find_regex (split_regex, &reg);
  if (code != 0)
    process_regcomp_errors (code, SPLIT_NAME);
  else
    {
      vect = ER_vect (pars_number == 2 ? below_ctop : ctop);
      els_number = start = 0;
#ifndef USE_POSIX_REGEXEC_FUNCTION
      regs.num_regs = 1;
      regs.start = &regs_start;
      regs.end = &regs_end;
      len = strlen (ER_pack_els (vect));
#endif
      /* Count substrings. */
      while (start < ER_els_number (vect) || start == 0)
	{
#ifndef USE_POSIX_REGEXEC_FUNCTION
	  ok = re_search (reg, ER_pack_els (vect) + start, len,
			   0, len, &regs) >= 0;
	  if (ok && regs.start[0] == 0 && regs.end[0] != 0)
	    {
	      /* Pattern by pattern. */
	      start += regs.end[0];
	      len -= regs.end[0];
	      continue;
	    }
	  els_number++;
	  if (!ok)
	    break;
	  if (regs.end[0] == 0)
	    {
	      start++;
	      len--;
	    }
	  else
	    {
	      start += regs.end[0];
	      len -= regs.end[0];
	    }
#else
	  ok = !regexec (reg, ER_pack_els (vect) + start, 1, pmatch, 0);
	  if (ok && pmatch[0].rm_so == 0 && pmatch[0].rm_eo != 0)
	    {
	      /* Pattern by pattern. */
	      start += pmatch[0].rm_eo;
	      continue;
	    }
	  els_number++;
	  if (!ok)
	    break;
	  start += (pmatch[0].rm_eo == 0 ? 1 : pmatch[0].rm_eo);
#endif
	}
      result = create_pack_vector (els_number, ER_NM_vect);
      ER_set_els_number (result, 0);
      start = els_number = 0;
#ifndef USE_POSIX_REGEXEC_FUNCTION
      len = strlen (ER_pack_els (vect));
#endif
      while (start < ER_els_number (vect) || start == 0)
	{
#ifndef USE_POSIX_REGEXEC_FUNCTION
	  ok = re_search (reg, ER_pack_els (vect) + start, len,
			   0, len, &regs) >= 0;
	  if (ok)
	    {
	      if (regs.start[0] != 0 || regs.end[0] == 0)
		{
		  /* Empty pattern case is here too. */
		  if (regs.start[0] == 0)
		    regs.start[0]++;
		  ch = ER_pack_els (vect) [start + regs.start[0]];
		  ER_pack_els (vect) [start + regs.start[0]] = '\0';
		  chars_number = regs.start[0];
		}
	      else
		{
		  /* Pattern by pattern. */
		  start += regs.end[0];
		  len -= regs.end[0];
		  continue;
		}
	    }
	  else
	    chars_number = ER_els_number (vect) - start;
#else
	  ok = !regexec (reg, ER_pack_els (vect) + start, 1, pmatch, 0);
	  if (ok)
	    {
	      if (pmatch[0].rm_so != 0 || pmatch[0].rm_eo == 0)
		{
		  /* Empty pattern case is here too. */
		  if (pmatch[0].rm_so == 0)
		    pmatch[0].rm_so++;
		  ch = ER_pack_els (vect) [start + pmatch[0].rm_so];
		  ER_pack_els (vect) [start + pmatch[0].rm_so] = '\0';
		  chars_number = pmatch[0].rm_so;
		}
	      else
		{
		  /* Pattern by pattern. */
		  start += pmatch[0].rm_eo;
		  continue;
		}
	    }
	  else
	    chars_number = ER_els_number (vect) - start;
#endif
	  /* Create substring. */
	  sub_vect = create_pack_vector (chars_number, ER_NM_char);
	  ER_set_immutable (sub_vect, TRUE);
	  strcpy (ER_pack_els (sub_vect), ER_pack_els (vect) + start);
	  set_packed_vect_el (result, els_number, sub_vect);
	  els_number++;
	  ER_set_els_number (result, els_number);
	  if (!ok)
	    break;
#ifndef USE_POSIX_REGEXEC_FUNCTION
	  ER_pack_els (vect) [start + regs.start[0]] = ch;
	  if (regs.end[0] == 0)
	    {
	      start++;
	      len--;
	    }
	  else
	    {
	      start += regs.end[0];
	      len -= regs.end[0];
	    }
#else
	  ER_pack_els (vect) [start + pmatch[0].rm_so] = ch;
	  start += (pmatch[0].rm_eo == 0 ? 1 : pmatch[0].rm_eo);
#endif
	}
    }
  if (result == NULL)
    ER_SET_MODE (fun_result, ER_NM_nil);
  else
    {
      ER_SET_MODE (fun_result, ER_NM_vect);
      set_vect_dim (fun_result, result, 0);
    }
}

static int do_inline
compare_elements (ER_node_mode_t el_type, const void *el1, const void *el2)
{
  switch (el_type)
    {
    case ER_NM_char:
      if (*(char_t *) el1 < *(char_t *) el2)
	return -1;
      else if (*(char_t *) el1 == *(char_t *) el2)
	return 0;
      else
	return 1;
    case ER_NM_int:
      if (*(int_t *) el1 < *(int_t *) el2)
	return -1;
      else if (*(int_t *) el1 == *(int_t *) el2)
	return 0;
      else
	return 1;
    case ER_NM_long:
      {
	int i = mpz_cmp (*ER_mpz_ptr (*(ER_node_t *) el1),
			 *ER_mpz_ptr (*(ER_node_t *) el2));
	return (i < 0 ? -1 : i > 0 ? 1 : 0);
      }
    case ER_NM_float:
      if (*(floating_t *) el1 < *(floating_t *) el2)
	return -1;
      else if (*(floating_t *) el1 == *(floating_t *) el2)
	return 0;
      else
	return 1;
    default:
      d_unreachable ();
    }
}

void
subv_call (int_t pars_number)
{
  ER_node_t vect;
  ER_node_t res;
  int_t start;
  int_t length;
  size_t vect_length;
  size_t el_size;
  ER_node_mode_t el_type;

  if (pars_number < 2 || pars_number > 3)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, SUBV_NAME);
  if (pars_number == 2)
    {
      to_vect_string_conversion (below_ctop, NULL, NULL);
      implicit_int_conversion (ctop, NULL);
    }
  else
    {
      to_vect_string_conversion (IVAL (ctop, -2), NULL, NULL);
      implicit_int_conversion (below_ctop, NULL);
      implicit_int_conversion (ctop, NULL);
    }
  if (ER_NODE_MODE (IVAL (ctop, -pars_number + 1)) != ER_NM_vect
      || ER_NODE_MODE (ctop) != ER_NM_int
      || (pars_number == 3 && ER_NODE_MODE (below_ctop) != ER_NM_int))
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, SUBV_NAME);
  if (pars_number == 3)
    {
      start = ER_i (below_ctop);
      length = ER_i (ctop);
    }
  else
    {
      start = ER_i (ctop);
      length = -1;
    }
  vect = ER_vect (IVAL (ctop, -pars_number + 1));
  GO_THROUGH_REDIR (vect);
  vect_length = ER_els_number (vect);
  if (start < 0)
    start = 0;
  if (start < vect_length && (length < 0 || start + length >= vect_length))
    /* Get tail. */
    length = vect_length - start;
  else if (start >= vect_length)
    length = 0;
  if (length == 0)
    {
      if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (vect) == ER_NM_char)
	{
	  res = create_string ("");
	  ER_set_immutable (res, FALSE);
	}
      else
	res = create_empty_vector ();
    }
  else if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
    {
      el_type = ER_pack_vect_el_type (vect);
      el_size = type_size_table [el_type];
      res = create_pack_vector (el_type == ER_NM_char ? length + 1 : length,
				el_type);
      ER_set_els_number (res, length);
      memcpy (ER_pack_els (res), ER_pack_els (vect) + start * el_size,
	      el_size * length);
      if (el_type == ER_NM_char)
	ER_pack_els (res) [length] = '\0';
    }
  else
    {
      res = create_unpack_vector (length);
      memcpy (ER_unpack_els (res),
	      (char *) ER_unpack_els (vect) + start * sizeof (val_t),
	      length * sizeof (val_t));
    }
  ER_set_immutable (res, ER_immutable (vect));
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, res, 0);
}

void
cmpv_call (int_t pars_number)
{
  ER_node_t vect1, vect2;
  size_t i;
  int_t res;
  ER_node_mode_t el_type1, el_type2;
  char *addr1, *addr2;
  ER_node_t el;

  if (pars_number != 2)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, CMPV_NAME);
  to_vect_string_conversion (ctop, NULL, 0);
  to_vect_string_conversion (below_ctop, NULL, 0);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (below_ctop) != ER_NM_vect)
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, CMPV_NAME);
  vect1 = ER_vect (below_ctop);
  GO_THROUGH_REDIR (vect1);
  vect2 = ER_vect (ctop);
  GO_THROUGH_REDIR (vect2);
  res = 0;
  for (i = 0; i < ER_els_number (vect1) && i < ER_els_number (vect2); i++)
    {
      if (ER_NODE_MODE (vect1) == ER_NM_heap_pack_vect)
	{
	  el_type1 = ER_pack_vect_el_type (vect1);
	  addr1 = ER_pack_els (vect1) + i * type_size_table [el_type1];
	}
      else
	{
	  el = IVAL (ER_unpack_els (vect1), i);
	  el_type1 = ER_NODE_MODE (el);
	  addr1
	    = (char *) el + val_displ_table [ER_NODE_MODE ((ER_node_t) el)];
	}
      if (ER_NODE_MODE (vect2) == ER_NM_heap_pack_vect)
	{
	  el_type2 = ER_pack_vect_el_type (vect2);
	  addr2 = ER_pack_els (vect2) + i * type_size_table [el_type2];
	}
      else
	{
	  el = IVAL (ER_unpack_els (vect2), i);
	  el_type2 = ER_NODE_MODE (el);
	  addr2
	    = (char *) el + val_displ_table [ER_NODE_MODE ((ER_node_t) el)];
	}
      if (el_type1 != el_type2
	  || (el_type1 != ER_NM_float
	      && el_type1 != ER_NM_int
	      && el_type1 != ER_NM_char
	      && el_type1 != ER_NM_long))
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, CMPV_NAME);
      res = compare_elements (el_type1, addr1, addr2);
      if (res)
	break;
    }
  if (res == 0)
    {
      if (i < ER_els_number (vect1))
	res = 1;
      else if (i < ER_els_number (vect2))
	res = (-1);
    }
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, res);
}

void
del_call (int_t pars_number)
{
  ER_node_t val;
  ER_node_mode_t mode;
  ER_node_t vect;
  ER_node_t tab;
      
  val = IVAL (ctop, -pars_number + 1);
  if (pars_number < 2 || pars_number > 3
      || (ER_NODE_MODE (val) == ER_NM_tab && pars_number != 2))
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, DEL_NAME);
  mode = ER_NODE_MODE (val);
  if (mode == ER_NM_vect)
    {
      ER_node_t start_val;
      ER_node_t length_val;
      int_t start;
      int_t length;
      size_t vect_length;
      size_t el_size;
      ER_node_mode_t el_type;
      
      implicit_int_conversion (IVAL (ctop, -pars_number + 2), NULL);
      start_val = IVAL (ctop, -pars_number + 2);
      if (pars_number == 3)
	{
	  implicit_int_conversion (ctop, NULL);
	  length_val = IVAL (ctop, 0);
	}
      else
	length_val = NULL;
      if (ER_NODE_MODE (start_val) != ER_NM_int
	  || (length_val != NULL && ER_NODE_MODE (length_val) != ER_NM_int))
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, DEL_NAME);
      start = ER_i (start_val);
      if (length_val != NULL)
	length = ER_i (length_val);
      else
	length = 1;
      vect = ER_vect (val);
      GO_THROUGH_REDIR (vect);
      if (ER_immutable (vect))
	eval_error (immutable_bc_decl, get_cpos (),
		    DERR_immutable_vector_modification);
      vect_length = ER_els_number (vect);
      if (start < 0)
	start = 0;
      if (start < vect_length && (length < 0 || start + length >= vect_length))
	{
	  /* Remove tail */
	  ER_set_els_number (vect, start);
	  if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
	      && ER_pack_vect_el_type (vect) == ER_NM_char)
	    ER_pack_els (vect) [start] = '\0';
	}
      else if (start == 0 && vect_length != 0)
	{
	  /* Remove head */
	  size_t el_size;

	  if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
	    el_size = type_size_table [ER_pack_vect_el_type (vect)];
	  else
	    el_size = sizeof (val_t);
	  ER_set_disp (vect, ER_disp (vect) + length * el_size);
	  ER_set_els_number (vect, ER_els_number (vect) - length);
	}
      else if (start >= vect_length || length == 0)
	;
      else if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
	{
	  el_type = ER_pack_vect_el_type (vect);
	  el_size = type_size_table [el_type];
	  memmove (ER_pack_els (vect) + start * el_size,
		   ER_pack_els (vect) + (start + length) * el_size,
		   el_size * (vect_length - start - length));
	  if (el_type == ER_NM_char)
	    ER_pack_els (vect) [vect_length - length] = '\0';
	  ER_set_els_number (vect, vect_length - length);
	}
      else
	{
	  memmove ((char *) ER_unpack_els (vect) + start * sizeof (val_t),
		   (char *) ER_unpack_els (vect)
		   + (start + length) * sizeof (val_t),
		   sizeof (val_t) * (vect_length - start - length));
	  ER_set_els_number (vect, vect_length - length);
	}
    }
  else if (mode == ER_NM_tab)
    {
      tab = ER_tab (val);
      GO_THROUGH_REDIR (tab);
      if (ER_immutable (tab))
	eval_error (immutable_bc_decl, get_cpos (),
		    DERR_immutable_table_modification);
      remove_tab_el (tab, IVAL (ctop, -pars_number + 2));
    }
  else
    eval_error (partype_bc_decl,
		get_cpos (), DERR_parameter_type, DEL_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, mode);
  if (mode == ER_NM_tab)
    ER_set_tab (fun_result, tab);
  else
    set_vect_dim (fun_result, vect, 0);
}

/* ????? Use disp. */
static void
general_ins_call (int_t pars_number, int vector_flag)
{
  ER_node_t vect_val;
  ER_node_t el_val;
  ER_node_t index_val;
  ER_node_t vect;
  ER_node_t el_vect;
  ER_node_mode_t el_type;
  size_t addition;
  size_t vect_length;
  size_t el_size;
  int_t index;

  if (pars_number != 2  && pars_number != 3)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number,
		(!vector_flag ? INS_NAME : INSV_NAME));
  vect_val = IVAL (ctop, -pars_number + 1);
  el_val = IVAL (ctop, -pars_number + 2);
  if (pars_number == 3)
    {
      implicit_int_conversion (ctop, NULL);
      index_val = IVAL (ctop, 0);
    }
  else
    index_val = NULL;
  if (ER_NODE_MODE (vect_val) != ER_NM_vect
      || (vector_flag && ER_NODE_MODE (el_val) != ER_NM_vect)
      || (index_val != NULL && ER_NODE_MODE (index_val) != ER_NM_int))
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, (vector_flag ? INSV_NAME : INS_NAME));
  if (index_val != NULL)
    index = ER_i (index_val);
  else
    index = 0;
  vect = ER_vect (vect_val);
  GO_THROUGH_REDIR (vect);
  if (vector_flag)
    {
      el_vect = ER_vect (el_val);
      GO_THROUGH_REDIR (el_vect);
      addition = ER_els_number (el_vect);
    }
  if (ER_immutable (vect))
    eval_error (immutable_bc_decl, get_cpos (),
		DERR_immutable_vector_modification);
  if (vector_flag && ER_NODE_MODE (el_vect) == ER_NM_heap_pack_vect
      && (ER_NODE_MODE (vect) != ER_NM_heap_pack_vect
	  || (ER_pack_vect_el_type (vect)
	      != ER_pack_vect_el_type (el_vect))))
    {
      el_vect = unpack_vector (el_vect);
      el_val = IVAL (ctop, -pars_number + 2);
    }
  if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
      && ((! vector_flag
	   && ER_pack_vect_el_type (vect) != ER_NODE_MODE (el_val))
	  || (vector_flag
	      && (ER_NODE_MODE (el_vect) != ER_NM_heap_pack_vect
		  || ER_pack_vect_el_type (vect)
		  != ER_pack_vect_el_type (el_vect)))))
    vect = unpack_vector (vect);
  if (!vector_flag)
    addition = 1;
  else
    addition = ER_els_number (el_vect);
  vect_length = ER_els_number (vect);
  /* Remember about GC! */
  vect = expand_vector (vect, vect_length + addition);
  el_val = IVAL (ctop, -pars_number + 2);
  if (index < 0 || index > vect_length)
    index = vect_length;
  if (index < vect_length)
    {
      /* Move */
      if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
	{
	  el_type = ER_pack_vect_el_type (vect);
	  el_size = type_size_table [el_type];
	  memmove (ER_pack_els (vect) + (index + addition) * el_size,
		   ER_pack_els (vect) + index * el_size,
		   el_size * (vect_length - index));
	}
      else
	memmove ((char *) ER_unpack_els (vect)
		 + (index + addition) * sizeof (val_t),
		 (char *) ER_unpack_els (vect) + index * sizeof (val_t),
		 sizeof (val_t) * (vect_length - index));
    }
  if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
    {
      el_type = ER_pack_vect_el_type (vect);
      el_size = type_size_table [el_type];
      if (!vector_flag)
	memcpy (ER_pack_els (vect) + index * el_size,
		(char *) el_val
		+ val_displ_table [ER_NODE_MODE (el_val)], el_size);
      else
	{
	  d_assert (ER_NODE_MODE (el_vect) == ER_NM_heap_pack_vect
		    && el_type == ER_pack_vect_el_type (el_vect));
	  memcpy (ER_pack_els (vect) + index * el_size,
		  ER_pack_els (el_vect), el_size * addition);
	}
      if (el_type == ER_NM_char)
	ER_pack_els (vect) [vect_length + addition] = '\0';
    }
  else
    {
      if (!vector_flag)
	*(val_t *) IVAL (ER_unpack_els (vect), index) = *(val_t *) el_val;
      else
	memcpy (IVAL (ER_unpack_els (vect), index),
		ER_unpack_els (el_vect), addition * sizeof (val_t));
    }
  ER_set_els_number (vect, vect_length + addition);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

void
ins_call (int_t pars_number)
{
  general_ins_call (pars_number, FALSE);
}

void
insv_call (int_t pars_number)
{
  general_ins_call (pars_number, TRUE);
}

void
rev_call (int_t pars_number)
{
  ER_node_t vect;
  ER_node_mode_t el_type;
  size_t vect_length;
  size_t el_size;
  size_t i, j;
  val_t temp_val;
  char temp_el [sizeof (floating_t) * 8];

  if (pars_number != 1)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, REV_NAME);
  if (ER_NODE_MODE (ctop) != ER_NM_vect)
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, REV_NAME);
  vect = ER_vect (ctop);
  GO_THROUGH_REDIR (vect);
#ifdef NEW_VECTOR
  vect = copy_vector (vect);
#else
  if (ER_immutable (vect))
    eval_error (immutable_bc_decl, get_cpos (),
		DERR_immutable_vector_modification);
#endif
  vect_length = ER_els_number (vect);
  if (vect_length != 0)
    {
      if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
	{
	  el_type = ER_pack_vect_el_type (vect);
	  el_size = type_size_table [el_type];
	  for (i = 0, j = vect_length - 1; i < j; i++, j--)
	    {
	      memcpy (temp_el, ER_pack_els (vect) + i * el_size, el_size);
	      memcpy (ER_pack_els (vect) + i * el_size,
		      ER_pack_els (vect) + j * el_size, el_size);
	      memcpy (ER_pack_els (vect) + j * el_size, temp_el, el_size);
	    }
	}
      else
	{
	  for (i = 0, j = vect_length - 1; i < j; i++, j--)
	    {
	      temp_val = *(val_t *) IVAL (ER_unpack_els (vect), i);
	      *(val_t *) IVAL (ER_unpack_els (vect), i)
		= *(val_t *) IVAL (ER_unpack_els (vect), j);
	      *(val_t *) IVAL (ER_unpack_els (vect), j) = temp_val;
	    }
	}
    }
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

/* The following variable contains type of homogeneous array being
   sorted. */
static ER_node_mode_t sorted_vect_el_type;

/* The function is comparison function for sorting homogeneous
   array. */
static int
homogeneous_array_sort_compare_function (const void *el1, const void *el2)
{
  return compare_elements (sorted_vect_el_type, el1, el2);
}

static BC_node_t dino_compare_fun_block;
static ER_node_t dino_compare_fun_block_context;

static int
array_sort_compare_function (const void *el1, const void *el2)
{
  int res;
  ER_node_t context;

  TOP_UP;
  if (sorted_vect_el_type != ER_NM_val)
    {
      ER_SET_MODE (ctop, sorted_vect_el_type);
      memcpy ((char *) ctop + val_displ_table [ER_NODE_MODE (ctop)],
	      (char **) el1, type_size_table [sorted_vect_el_type]);
      if (sorted_vect_el_type == ER_NM_vect)
	ER_set_dim (ctop, 0);
    }
  else
    *(val_t *) ctop = *(val_t *) el1;
  TOP_UP;
  if (sorted_vect_el_type != ER_NM_val)
    {
      ER_SET_MODE (ctop, sorted_vect_el_type);
      memcpy ((char *) ctop + val_displ_table [ER_NODE_MODE (ctop)],
	      (char *) el2, type_size_table [sorted_vect_el_type]);
      if (sorted_vect_el_type == ER_NM_vect)
	ER_set_dim (ctop, 0);
    }
  else
    *(val_t *) ctop = *(val_t *) el2;
  call_fun_class (dino_compare_fun_block, dino_compare_fun_block_context, 2);
  TOP_UP;
  implicit_int_conversion (ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_int)
    eval_error (invresult_bc_decl, get_cpos (), DERR_invalid_result, SORT_NAME);
  res = ER_i (ctop);
  TOP_DOWN;
  return res;
}

void
sort_call (int_t pars_number)
{
  ER_node_t vect;
  ER_node_t var;
  ptrdiff_t offset = (char *) fun_result - (char *) cstack;

  if (pars_number != 1 && pars_number != 2)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, SORT_NAME);
  var = IVAL (ctop, -pars_number + 1);
  if (ER_NODE_MODE (var) == ER_NM_vect)
    {
      vect = ER_vect (var);
      GO_THROUGH_REDIR (vect);
      if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (vect);
      ER_set_vect (var, vect);
    }
  if (pars_number == 1)
    {
      if (ER_NODE_MODE (ctop) != ER_NM_vect
	  || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
	  || (ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
	      && ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_int
	      && ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_long
	      && ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_float))
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, SORT_NAME);
      vect = copy_vector (ER_vect (ctop));
      sorted_vect_el_type = ER_pack_vect_el_type (vect);
      qsort (ER_pack_els (vect), ER_els_number (vect),
	     type_size_table [sorted_vect_el_type],
	     homogeneous_array_sort_compare_function);
    }
  else
    {
      ER_node_t context;

      if (ER_NODE_MODE (below_ctop) != ER_NM_vect || ! fun_p (ctop))
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, SORT_NAME);
      vect = copy_vector (ER_vect (below_ctop));
      dino_compare_fun_block_context = ER_code_context (ctop);
      dino_compare_fun_block = ID_TO_CODE (ER_code_id (ctop));
      if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
	sorted_vect_el_type = ER_NM_val;
      else
	sorted_vect_el_type = ER_pack_vect_el_type (vect);
      /* We can not do GC as we can not move the array.  */
      no_gc_p = TRUE;
      DECR_CTOP (2); /* free place for compare function params.  */
      qsort ((sorted_vect_el_type == ER_NM_val
	      ? (char *) ER_unpack_els (vect) : ER_pack_els (vect)),
	     ER_els_number (vect),
	     (sorted_vect_el_type != ER_NM_val
	      ? type_size_table [sorted_vect_el_type]
	      : sizeof (val_t)),
	     array_sort_compare_function);
      DECR_CTOP (-2);
      no_gc_p = FALSE;
    }
  /* Place the result instead of the function. */
  fun_result = (ER_node_t) ((char *) cstack + offset);
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

static int
print_context (ER_node_t context)
{
  BC_node_t block;
  string_t ident;
  char str [100];

  if (context == NULL || ER_context (context) == NULL)
    /* We ignore the uppest implicit block. */
    return FALSE;
  block = ER_block_node (context);
  ident = (BC_NODE_MODE (block) == BC_NM_block
	   ? NULL : BC_ident (BC_fdecl (block)));
  if (print_context (ER_context (context)))
    VLO_ADD_STRING (temp_vlobj, ".");
  if (ident == NULL)
    VLO_ADD_STRING (temp_vlobj, "{}");
  else
    {
      VLO_ADD_STRING (temp_vlobj, ident);
      sprintf (str, "(%ld)", (long int) ER_context_number (context));
      VLO_ADD_STRING (temp_vlobj, str);
    }
  return TRUE;
}

static FILE *
get_file (int_t pars_number, const char *function_name)
{
  ER_node_t var;
  ER_node_t instance;

  var = IVAL (ctop, -pars_number + 1);
  if (!ER_IS_OF_TYPE (var, ER_NM_stack)
      || ER_stack_block (ER_stack (var)) != file_bc_decl)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, function_name);
  instance = ER_stack ((ER_node_t) IVAL (ctop, -pars_number + 1));
  return ER_hide (IVAL (ER_stack_vars (instance),
			BC_var_num (file_ptr_bc_decl)));
}

static void
place_file_instance (FILE *f, ER_node_t result)
{
  ER_node_t var;
  ER_node_t instance;

  ER_SET_MODE (result, ER_NM_code);
  ER_set_code_id (result, CODE_ID (file_bc_decl));
  ER_set_code_context (result, uppest_stack);
  instance = create_class_stack ((val_t *) result, 0, TRUE);
  ER_SET_MODE (result, ER_NM_stack);
  ER_set_stack (result, instance);
  var = IVAL (ER_stack_vars (ER_stack (result)), BC_var_num (file_ptr_bc_decl));
  ER_SET_MODE (var, ER_NM_hide);
  ER_set_hide (var, f);
}

static void
two_strings_fun_start (int_t pars_number, const char *function_name)
{
  if (pars_number != 2)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, function_name);
  to_vect_string_conversion (ctop, NULL, NULL);
  to_vect_string_conversion (below_ctop, NULL, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || ER_NODE_MODE (below_ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char)
    eval_error (partype_bc_decl,
		get_cpos (), DERR_parameter_type, function_name);
}

void
rename_call (int_t pars_number)
{
  two_strings_fun_start (pars_number, RENAME_NAME);
  errno = 0;
  rename (ER_pack_els (ER_vect (below_ctop)), ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (RENAME_NAME);
  /* Pop all actual parameters. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

static void
string_fun_start (int_t pars_number, const char *function_name)
{
  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, function_name);
  to_vect_string_conversion (ctop, NULL, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, function_name);
}

void
remove_call (int_t pars_number)
{
  string_fun_start (pars_number, REMOVE_NAME);
  errno = 0;
  remove (ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (REMOVE_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

#ifndef S_IRUSR
#ifndef S_IREAD
#define S_IRUSR 00400
#else
#define S_IRUSR S_IREAD
#endif
#endif

#ifndef S_IWUSR
#ifndef S_IWRITE
#define S_IWUSR 00200
#else
#define S_IWUSR S_IWRITE
#endif
#endif

#ifndef S_IXUSR
#ifndef S_IEXEC
#define S_IXUSR 00100
#else
#define S_IXUSR S_IEXEC
#endif
#endif

#ifndef S_ISVTX
#define S_ISVTX 0001000
#endif

#ifndef S_IRGRP
#define S_IRGRP 00040
#endif

#ifndef S_IWGRP
#define S_IWGRP 00020
#endif

#ifndef S_IXGRP
#define S_IXGRP 00010
#endif

#ifndef S_IROTH
#define S_IROTH 00040
#endif

#ifndef S_IWOTH
#define S_IWOTH 00020
#endif

#ifndef S_IXOTH
#define S_IXOTH 00010
#endif

static int
in_str_p (const char *str, int ch)
{
  for (;*str;str++)
    if (*str == ch)
      return TRUE;
  return FALSE;
}

void
mkdir_call (int_t pars_number)
{
  int mask;

  string_fun_start (pars_number, MKDIR_NAME);
  errno = 0;
  mask = (S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IWGRP | S_IXGRP
	  | S_IROTH | S_IWOTH | S_IXOTH);
  mkdir (ER_pack_els (ER_vect (ctop)), mask);
  if (errno)
    process_system_errors (MKDIR_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

void
rmdir_call (int_t pars_number)
{
  string_fun_start (pars_number, RMDIR_NAME);
  errno = 0;
  rmdir (ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (RMDIR_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

void
getcwd_call (int_t pars_number)
{
  ER_node_t vect;
  char buf [PATH_MAX + 1], *str;

  if (pars_number != 0)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, GETCWD_NAME);
  errno = 0;
  str = getcwd (buf, PATH_MAX);
  if (errno)
    process_system_errors (GETCWD_NAME);
  vect = create_string (str);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

void
chdir_call (int_t pars_number)
{
  string_fun_start (pars_number, CHDIR_NAME);
  errno = 0;
  if (chdir (ER_pack_els (ER_vect (ctop))) < 0 && errno)
    process_system_errors (CHDIR_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

static void
get_stat (ER_node_t var, const char *function_name, struct stat *buf)
{
  int result;

  errno = 0;
  if (ER_NODE_MODE (var) == ER_NM_vect
      && ER_NODE_MODE (ER_vect (var)) == ER_NM_heap_pack_vect
      && ER_pack_vect_el_type (ER_vect (var)) == ER_NM_char)
    result = stat (ER_pack_els (ER_vect (var)), buf);
  else if (ER_IS_OF_TYPE (var, ER_NM_stack)
	   && ER_stack_block (ER_stack (var)) == file_bc_decl)
    result
      = fstat (fileno ((FILE *) ER_hide (IVAL (ER_stack_vars (ER_stack (var)),
					       BC_var_num (file_ptr_bc_decl)))),
	       buf);
  else
    eval_error (partype_bc_decl,
		get_cpos (), DERR_parameter_type, function_name);
  if (result < 0)
    process_system_errors (function_name);
}

static void
general_chmod (int_t pars_number, const char *function_name,
	       int clear_mask, int set_mask)
{
  struct stat buf;
  int mask;

  errno = 0;
  get_stat (below_ctop, function_name, &buf);
  mask = (buf.st_mode & ~clear_mask) | set_mask;
  chmod (ER_pack_els (ER_vect (below_ctop)), mask);
  if (errno)
    process_system_errors (function_name);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

void
chumod_call (int_t pars_number)
{
  int mask = 0;
  char *str;

  two_strings_fun_start (pars_number, CHUMOD_NAME);
  str = ER_pack_els (ER_vect (ctop));
  if (in_str_p (str, 'r'))
    mask |= S_IRUSR;
  if (in_str_p (str, 'w'))
    mask |= S_IWUSR;
  if (in_str_p (str, 'x'))
    mask |= S_IXUSR;
  if (in_str_p (str, 's'))
    mask |= S_ISVTX;
  general_chmod (pars_number, CHUMOD_NAME,
		 S_IRUSR | S_IWUSR | S_IXUSR | S_ISVTX, mask);
}

void
chgmod_call (int_t pars_number)
{
  int mask = 0;
  char *str;

  two_strings_fun_start (pars_number, CHGMOD_NAME);
  str = ER_pack_els (ER_vect (ctop));
  if (in_str_p (str, 'r'))
    mask |= S_IRGRP;
  if (in_str_p (str, 'w'))
    mask |= S_IWGRP;
  if (in_str_p (str, 'x'))
    mask |= S_IXGRP;
  general_chmod (pars_number, CHGMOD_NAME, S_IRGRP | S_IWGRP | S_IXGRP, mask);
}

void
chomod_call (int_t pars_number)
{
  int mask = 0;
  char *str;

  two_strings_fun_start (pars_number, CHOMOD_NAME);
  str = ER_pack_els (ER_vect (ctop));
  if (in_str_p (str, 'r'))
    mask |= S_IROTH;
  if (in_str_p (str, 'w'))
    mask |= S_IWOTH;
  if (in_str_p (str, 'x'))
    mask |= S_IXOTH;
  general_chmod (pars_number, CHOMOD_NAME, S_IROTH | S_IWOTH | S_IXOTH, mask);
}

static FILE *
file_start (int_t pars_number, const char *function_name)
{
  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, function_name);
  return get_file (pars_number, function_name);
}

void
isatty_call (int_t pars_number)
{
  int_t result;
  FILE *f;

  f = file_start (pars_number, ISATTY_NAME);
  result = isatty (fileno (f));
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, result);
}

void
open_call (int_t pars_number)
{
  FILE *f;

  two_strings_fun_start (pars_number, OPEN_NAME);
  errno = 0;
  f = fopen (ER_pack_els (ER_vect (below_ctop)), ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (OPEN_NAME);
  else if (f == NULL)
    eval_error (einval_bc_decl, get_cpos (), DERR_einval, OPEN_NAME);
  /* Place the result instead of the function. */
  place_file_instance (f, fun_result);
}

void
close_call (int_t pars_number)
{
  FILE *f;

  f = file_start (pars_number, CLOSE_NAME);
  errno = 0;
  fclose (f);
  if (errno)
    process_system_errors (CLOSE_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

void
flush_call (int_t pars_number)
{
  FILE *f;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, FLUSH_NAME);
  f = get_file (pars_number, FLUSH_NAME);
  errno = 0;
  fflush (f);
  if (errno)
    process_system_errors (FLUSH_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

void
popen_call (int_t pars_number)
{
  FILE *f;

  two_strings_fun_start (pars_number, POPEN_NAME);
  errno = 0;
  if ((*ER_pack_els (ER_vect (ctop)) != 'r'
       && *ER_pack_els (ER_vect (ctop)) != 'w')
      || strlen (ER_pack_els (ER_vect (ctop))) != 1)
    {
      errno = EINVAL;
      process_system_errors (POPEN_NAME);
    }
  f = popen (ER_pack_els (ER_vect (below_ctop)), ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (POPEN_NAME);
  /* Place the result instead of the function. */
  place_file_instance (f, fun_result);
}

void
pclose_call (int_t pars_number)
{
  FILE *f;
  int res;

  f = file_start (pars_number, PCLOSE_NAME);
  errno = 0;
  res = pclose (f);
  if (res != 0 && errno)
    process_system_errors (PCLOSE_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

void
tell_call (int_t pars_number)
{
  FILE *f;
  int_t pos;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, TELL_NAME);
  f = get_file (pars_number, TELL_NAME);
  errno = 0;
  pos = ftell (f);
  if (errno)
    process_system_errors (TELL_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, pos);
}

void
seek_call (int_t pars_number)
{
  FILE *f;
  int_t pos;
  int whence;
  int ch;

  if (pars_number != 3)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, SEEK_NAME);
  f = get_file (pars_number, SEEK_NAME);
  implicit_arithmetic_conversion (below_ctop, NULL);
  to_vect_string_conversion (ctop, NULL, NULL);
  if (ER_NODE_MODE (below_ctop) != ER_NM_int
      || (ER_NODE_MODE (ctop) != ER_NM_char
	  && (ER_NODE_MODE (ctop) != ER_NM_vect
	      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)))
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, SEEK_NAME);
  pos = ER_i (below_ctop);
  if (ER_NODE_MODE (ctop) == ER_NM_char)
    ch = ER_ch (ctop);
  else
    ch = *ER_pack_els (ER_vect (ctop));
  ch = tolower (ch);
  if (ch == 's')
#ifdef SEEK_SET
    whence = SEEK_SET;
#else
    whence = 0;
#endif
  else if (ch == 'c')
#ifdef SEEK_CUR
    whence = SEEK_CUR;
#else
    whence = 1;
#endif
  else if (ch == 'e')
#ifdef SEEK_END
    whence = SEEK_END;
#else
    whence = 2;
#endif
  else
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, SEEK_NAME);
  errno = 0;
  fseek (f, pos, whence);
  if (errno)
    process_system_errors (SEEK_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

static void
print_ch (int ch)
{
  char *str = get_ch_repr (ch);

  VLO_ADD_STRING (temp_vlobj, str);
}

#define MAX_REPL_PRINTED_ELEMENTS 50

static void
print_val (ER_node_t val, int quote_flag, int full_p)
{
  BC_node_t code;
  ER_node_t vect;
  ER_node_t tab;
  ER_node_t key;
  size_t i, num;
  int flag;
  char *string;
  static char str [100];

  switch (ER_NODE_MODE (val))
    {
    case ER_NM_nil:
      VLO_ADD_STRING (temp_vlobj, "nil");
      break;
    case ER_NM_hide:
      sprintf (str, "hide value %lx", (long int) ER_hide (val));
      VLO_ADD_STRING (temp_vlobj, str);
      break;
    case ER_NM_hideblock:
      VLO_ADD_STRING (temp_vlobj, "hideblock value (");
      for (i = 0; i < ER_hideblock_length (ER_hideblock (val)); i++)
	{
	  if (i != 0)
	    VLO_ADD_STRING (temp_vlobj, " ");
	  sprintf (str, "%x",
		   (unsigned char)
		   ER_hideblock_start (ER_hideblock (val)) [i]);
	  VLO_ADD_STRING (temp_vlobj, str);
	}
      VLO_ADD_STRING (temp_vlobj, ")");
      break;
    case ER_NM_char:
      if (!quote_flag)
	{
	  sprintf (str, "%c", ER_ch (val));
	  VLO_ADD_STRING (temp_vlobj, str);
	}
      else
	{
	  VLO_ADD_STRING (temp_vlobj, "\'");
	  print_ch (ER_ch (val));
	  VLO_ADD_STRING (temp_vlobj, "\'");
	}
      break;
    case ER_NM_int:
      sprintf (str, "%d", ER_i (val));
      VLO_ADD_STRING (temp_vlobj, str);
      break;
    case ER_NM_long:
      {
	ER_node_t heap_mpz = ER_l (val);
	
	VLO_ADD_STRING (temp_vlobj, mpz2a (*ER_mpz_ptr (heap_mpz), 10, FALSE));
	VLO_ADD_STRING (temp_vlobj, "l");
      }
      break;
    case ER_NM_float:
      sprintf (str, "%g", ER_f (val));
      VLO_ADD_STRING (temp_vlobj, str);
      break;
    case ER_NM_vect:
      to_vect_string_conversion (val, NULL, NULL);
      vect = ER_vect (val);
      if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (vect) == ER_NM_char)
	{
	  if (!quote_flag)
	    VLO_ADD_STRING (temp_vlobj, ER_pack_els (vect));
	  else
	    {
	      VLO_ADD_STRING (temp_vlobj, "\"");
	      for (string = (char *) ER_pack_els (vect);
		   *string != '\0';
		   string++)
		print_ch (*string);
	      VLO_ADD_STRING (temp_vlobj, "\"");
	    }
	}
      else if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
	{
	  VLO_ADD_STRING (temp_vlobj, "[");
	  for (i = 0; i < ER_els_number (vect); i++)
	    {
	      if (repl_flag && i >= MAX_REPL_PRINTED_ELEMENTS)
		{
		  VLO_ADD_STRING (temp_vlobj, "...");
		  break;
		}
	      print_val (IVAL (ER_unpack_els (vect), i), TRUE, TRUE);
	      if (i < ER_els_number (vect) - 1)
		VLO_ADD_STRING (temp_vlobj, ", ");
	    }
	  VLO_ADD_STRING (temp_vlobj, "]");
	}
      else
	{
	  ER_node_mode_t el_type = ER_pack_vect_el_type (vect);
	  val_t temp_val;
	  size_t displ;
	  size_t el_size;

	  VLO_ADD_STRING (temp_vlobj, "[");
	  ER_SET_MODE ((ER_node_t) &temp_val, el_type);
	  displ = val_displ_table [ER_NODE_MODE ((ER_node_t) &temp_val)];
	  el_size = type_size_table [el_type];
	  for (i = 0; i < ER_els_number (vect); i++)
	    {
	      if (repl_flag && i >= MAX_REPL_PRINTED_ELEMENTS)
		{
		  VLO_ADD_STRING (temp_vlobj, "...");
		  break;
		}
	      /* We don't care about vector dimension here.  */
	      memcpy ((char *) &temp_val + displ,
		      (char *) ER_pack_els (vect) + i * el_size, el_size);
	      print_val ((ER_node_t) &temp_val, TRUE, TRUE);
	      if (i < ER_els_number (vect) - 1)
		VLO_ADD_STRING (temp_vlobj, ", ");
	    }
	  VLO_ADD_STRING (temp_vlobj, "]");
	}
      break;
    case ER_NM_tab:
      VLO_ADD_STRING (temp_vlobj, "tab [");
      tab = ER_tab (val);
      GO_THROUGH_REDIR (tab);
      flag = FALSE;
      for (num = i = 0; i < ER_entries_number (tab); i++)
	{
	  key = INDEXED_ENTRY_KEY (ER_tab_els (tab), i);
	  if (ER_NODE_MODE (key) == ER_NM_empty_entry
	      || ER_NODE_MODE (key) == ER_NM_deleted_entry)
	    continue;
	  num++;
	  if (repl_flag && num > MAX_REPL_PRINTED_ELEMENTS)
	    {
	      VLO_ADD_STRING (temp_vlobj, ", ...");
	      break;
	    }
	  if (flag)
	    VLO_ADD_STRING (temp_vlobj, ", ");
	  print_val (key, TRUE, TRUE);
	  VLO_ADD_STRING (temp_vlobj, ":");
	  print_val (INDEXED_ENTRY_VAL (ER_tab_els (tab), i), TRUE, TRUE);
	  flag = TRUE;
	}
      VLO_ADD_STRING (temp_vlobj, "]");
      break;
    case ER_NM_code:
      code = ID_TO_CODE (ER_code_id (val));
      if (BC_NODE_MODE (code) == BC_NM_block)
	;
      else if (BC_fun_p (code))
	VLO_ADD_STRING (temp_vlobj, "fun ");
      else if (BC_class_p (code))
	VLO_ADD_STRING (temp_vlobj, "class ");
      else if (BC_thread_p (code))
	VLO_ADD_STRING (temp_vlobj, "thread ");
      if (print_context (ER_code_context (val)))
	VLO_ADD_STRING (temp_vlobj, ".");
      if (BC_NODE_MODE (code) == BC_NM_fblock)
	VLO_ADD_STRING (temp_vlobj, BC_ident (BC_fdecl (code)));
      break;
    case ER_NM_stack:
      {
	BC_node_t block = ER_block_node (ER_stack (val));
	
	if (BC_NODE_MODE (block) == BC_NM_fblock && BC_class_p (block))
	  VLO_ADD_STRING (temp_vlobj, "instance ");
	else
	  VLO_ADD_STRING (temp_vlobj, "stack ");
	/* Context may be uppest block stack. */
	print_context (ER_stack (val));
	break;
      }
    case ER_NM_process:
      if (ER_process_block (ER_process (val)) == NULL) /* ??? */
	VLO_ADD_STRING (temp_vlobj, "main thread");
      else
	{
	  ER_node_t stack;

	  for (stack = ER_saved_cstack (ER_process (val));
	       stack != NULL;
	       stack = ER_prev_stack (stack))
	    if (BC_NODE_MODE (ER_stack_block (stack)) == BC_NM_fblock
		&& BC_thread_p (ER_stack_block (stack)))
	      break;
	  sprintf (str, "thread %ld ",
		   (long int) ER_process_number (ER_process (val)));
	  VLO_ADD_STRING (temp_vlobj, str);
	  if (!print_context (stack))
	    d_unreachable ();
	}
      break;
    case ER_NM_type:
      switch (ER_type (val))
	{
	case ER_NM_nil:
	  string = "type (nil)";
	  break;
	case ER_NM_char:
	  string = "char";
	  break;
	case ER_NM_int:
	  string = "int";
	  break;
	case ER_NM_long:
	  string = "long";
	  break;
	case ER_NM_float:
	  string = "float";
	  break;
	case ER_NM_hide:
	  string = "hide";
	  break;
	case ER_NM_hideblock:
	  string = "hideblock";
	  break;
	case ER_NM_vect:
	  string = "vector";
	  break;
	case ER_NM_tab:
	  string = "table";
	  break;
	case ER_NM_code:
	  string = "closure";
	  break;
	case ER_NM_stack:
	  string = "obj";
	  break;
	case ER_NM_process:
	  string = "process";
	  break;
	case ER_NM_type:
	  string = "type";
	  break;
	default:
	  d_unreachable ();
	}
      VLO_ADD_STRING (temp_vlobj, string);
      break;
    case ER_NM_undef:
      d_assert (repl_flag);
      VLO_ADD_STRING (temp_vlobj, "undef");
      break;
    default:
      d_unreachable ();
    }
}

void
repl_print (ER_node_t val, int def_p)
{
  if (def_p && ER_NODE_MODE (val) == ER_NM_undef)
    return;
  VLO_NULLIFY (temp_vlobj);
  print_val (val, TRUE, FALSE);
  puts (VLO_BEGIN (temp_vlobj));
}

static FILE *
file_function_call_start (int_t pars_number, const char *function_name)
{
  if (pars_number == 0)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, function_name);
  return get_file (pars_number, function_name);
}

enum file_param_type
{
  NO_FILE,
  STANDARD_FILE,
  GIVEN_FILE
};

static void
finish_output (FILE *f, int pars_number)
{
  ER_node_t vect;

  if (f != NULL)
    {
      fputs (VLO_BEGIN (temp_vlobj), f);
      /* Place the result instead of the function. */
      ER_SET_MODE (fun_result, ER_NM_undef);
    }
  else
    {
      vect = create_string (VLO_BEGIN (temp_vlobj));
      /* Place the result instead of the function. */
      ER_SET_MODE (fun_result, ER_NM_vect);
      set_vect_dim (fun_result, vect, 0);
    }
}

static void
general_put_call (FILE *f, int_t pars_number, int ln_flag,
		  enum file_param_type param_type)
{
  int i;
  const char *function_name;
  ER_node_t var;

  errno = 0;
  if (param_type == NO_FILE)
    {
      function_name = (ln_flag ? SPUTLN_NAME : SPUT_NAME);
      d_assert (f == NULL);
    }
  else if (param_type == STANDARD_FILE)
    function_name = (ln_flag ? PUTLN_NAME : PUT_NAME);
  else
    function_name = (ln_flag ? FPUTLN_NAME : FPUT_NAME);
  VLO_NULLIFY (temp_vlobj);
  for (i = -pars_number + (param_type == GIVEN_FILE ? 1 : 0) + 1; i <= 0; i++)
    {
      var = IVAL (ctop, i);
      to_vect_string_conversion (var, NULL, NULL);
      if (ER_NODE_MODE (var) != ER_NM_vect
	  || ER_NODE_MODE (ER_vect (var)) != ER_NM_heap_pack_vect
	  || ER_pack_vect_el_type (ER_vect (var)) != ER_NM_char)
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, function_name);
      VLO_ADD_STRING (temp_vlobj, ER_pack_els (ER_vect (var)));
    }
  if (ln_flag)
    VLO_ADD_STRING (temp_vlobj, "\n");
  if (errno != 0)
    process_system_errors (function_name);
  finish_output (f, pars_number);
}

void
put_call (int_t pars_number)
{
  general_put_call (stdout, pars_number, FALSE, STANDARD_FILE);
}

void
putln_call (int_t pars_number)
{
  general_put_call (stdout, pars_number, TRUE, STANDARD_FILE);
}

void
fput_call (int_t pars_number)
{
  general_put_call (file_function_call_start (pars_number, FPUT_NAME),
		    pars_number, FALSE, GIVEN_FILE);
}

void
fputln_call (int_t pars_number)
{
  general_put_call (file_function_call_start (pars_number, FPUTLN_NAME),
		    pars_number, TRUE, GIVEN_FILE);
}

void
sput_call (int_t pars_number)
{
  general_put_call (NULL, pars_number, FALSE, NO_FILE);
}

void
sputln_call (int_t pars_number)
{
  general_put_call (NULL, pars_number, TRUE, NO_FILE);
}

static void
general_print_call (FILE *f, int_t pars_number, int quote_flag, int ln_flag,
		    enum file_param_type param_type)
{
  int i;
  const char *function_name;

  errno = 0;
  if (param_type == NO_FILE)
    {
      function_name = (ln_flag ? SPRINTLN_NAME : SPRINT_NAME);
      d_assert (f == NULL);
    }
  else if (param_type == STANDARD_FILE)
    function_name = (ln_flag ? PRINTLN_NAME : PRINT_NAME);
  else
    function_name = (ln_flag ? FPRINTLN_NAME : FPRINT_NAME);
  VLO_NULLIFY (temp_vlobj);
  for (i = -pars_number + (param_type == GIVEN_FILE ? 1 : 0) + 1; i <= 0; i++)
    print_val (IVAL (ctop, i), quote_flag, TRUE);
  if (errno != 0)
    process_system_errors (function_name);
  if (ln_flag)
    VLO_ADD_STRING (temp_vlobj, "\n");
  if (errno != 0)
    process_system_errors (function_name);
  finish_output (f, pars_number);
}

void
print_call (int_t pars_number)
{
  general_print_call (stdout, pars_number, TRUE, FALSE, STANDARD_FILE);
}

void
println_call (int_t pars_number)
{
  general_print_call (stdout, pars_number, TRUE, TRUE, STANDARD_FILE);
}

void
fprint_call (int_t pars_number)
{
  general_print_call (file_function_call_start (pars_number, FPRINT_NAME),
		      pars_number, TRUE, FALSE, GIVEN_FILE);
}

void
fprintln_call (int_t pars_number)
{
  general_print_call (file_function_call_start (pars_number, FPRINTLN_NAME),
		      pars_number, TRUE, TRUE, GIVEN_FILE);
}

void
sprint_call (int_t pars_number)
{
  general_print_call (NULL, pars_number, TRUE, FALSE, NO_FILE);
}

void
sprintln_call (int_t pars_number)
{
  general_print_call (NULL, pars_number, TRUE, TRUE, NO_FILE);
}

static void
general_get_call (FILE *f, int file_flag)
{
  int ch;

  errno = 0;
  ch = fgetc (f);
  if (errno != 0)
    process_system_errors (file_flag ? FGET_NAME : GET_NAME);
  if (ch == EOF)
    eval_error (eof_bc_decl, get_cpos (), DERR_eof_occured,
		file_flag ? FGET_NAME : GET_NAME);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_char);
  ER_set_ch (fun_result, ch);
}

static void
general_get_ln_file_call (FILE *f, int param_flag, int ln_flag, int as_lns_p,
			  const char *fun_name)
{
  ER_node_t vect;
  int ch;
  size_t ch_n, els_number, i;

  VLO_NULLIFY (temp_vlobj);
  if (!ln_flag && as_lns_p)
    VLO_NULLIFY (temp_vlobj2);
  errno = 0;
  ch_n = 0;
  for (;;)
    {
      ch  = fgetc (f);
      if (ch != EOF)
	ch_n++;
      if ((ch == '\n' && (ln_flag || as_lns_p)) || ch == EOF)
	{
	  if (ln_flag || !as_lns_p
	      || ch == '\n' || VLO_LENGTH (temp_vlobj) != 0)
	    {
	      VLO_ADD_BYTE (temp_vlobj, '\0');
	      vect = create_string (VLO_BEGIN (temp_vlobj));
	      if (!ln_flag && as_lns_p)
		{
		  VLO_NULLIFY (temp_vlobj);
		  VLO_ADD_MEMORY (temp_vlobj2, &vect, sizeof (vect));
		}
	    }
	  if (ln_flag || ch == EOF)
	    break;
	}
      else
	VLO_ADD_BYTE (temp_vlobj, ch);
    }
  if (!ln_flag && as_lns_p)
    {
      els_number = VLO_LENGTH (temp_vlobj2) / sizeof (ER_node_t);
      vect = create_pack_vector (els_number, ER_NM_vect);
      for (i = 0; i < els_number; i++)
	set_packed_vect_el (vect, i,
			    ((ER_node_t *) VLO_BEGIN (temp_vlobj2)) [i]);
    }
  if (errno != 0)
    process_system_errors (fun_name);
  /* ??? */
  if (ch == EOF && ch_n == 0)
    eval_error (eof_bc_decl, get_cpos (), DERR_eof_occured, fun_name);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

void
get_call (int_t pars_number)
{
  if (pars_number != 0)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, GET_NAME);
  general_get_call (stdin, FALSE);
}

void
getln_call (int_t pars_number)
{
  if (pars_number != 0)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, GETLN_NAME);
  general_get_ln_file_call (stdin, FALSE, TRUE, FALSE, GETLN_NAME);
}

void
getf_call (int_t pars_number)
{
  int flag = 0;

  if (pars_number > 1)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, GETF_NAME);
  if (pars_number == 1)
    {
      implicit_int_conversion (ctop, NULL);
      if (!ER_IS_OF_TYPE (ctop, ER_NM_int))
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, GETF_NAME);
      flag = ER_i (ctop);
    }
  general_get_ln_file_call (stdin, FALSE, FALSE, flag != 0, GETF_NAME);
}

static FILE *
fget_function_call_start (int_t pars_number, const char *function_name)
{
  if (pars_number != 1)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, function_name);
  return get_file (pars_number, function_name);
}

void
fget_call (int_t pars_number)
{
  general_get_call (fget_function_call_start (pars_number, FGET_NAME), TRUE);
}

void
fgetln_call (int_t pars_number)
{
  general_get_ln_file_call
    (fget_function_call_start (pars_number, FGETLN_NAME),
     TRUE, TRUE, FALSE, FGETLN_NAME);
}

void
fgetf_call (int_t pars_number)
{
  int flag = 0;

  if (pars_number == 2)
    {
      implicit_int_conversion (ctop, NULL);
      if (!ER_IS_OF_TYPE (ctop, ER_NM_int))
	eval_error (partype_bc_decl,
		    get_cpos (), DERR_parameter_type, FGETF_NAME);
      flag = ER_i (ctop);
    }
  else if (pars_number != 1)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, FGETF_NAME);
  general_get_ln_file_call (get_file (pars_number, FGETF_NAME),
			    TRUE, FALSE, flag != 0, FGETF_NAME);
}

#define F_CHAR   256
#define F_INT    257
#define F_FLOAT  258
#define F_LONG   259
#define F_STRING 260
#define F_TAB    261

struct token
{
  int token_code;
  union
  {
    char_t ch;
    int_t i;
    floating_t f;
    ER_node_t gmp;
    string_t str;
  } val;
};

/* Var length string used by func yylval for text presentation of the
   symbol. */
static vlo_t el_text;

static void
initiate_io (void)
{
  VLO_CREATE (el_text, 0);
}

static void
finish_io (void)
{
  VLO_DELETE (el_text);
}

/* The following function is analogous to `get_string_code' in Dino
   scanner.  If `get_string_code' is changed, please modify this
   function too. */
static int
get_char_code (FILE *f, int curr_char, int *correct_newln)
{
  int char_code;

  if (curr_char == EOF || curr_char == '\n')
    {
      ungetc (curr_char, f);
      return (-1);
    }
  *correct_newln = FALSE;
  if (curr_char == '\\')
    {
      curr_char = fgetc (f);
      if (curr_char == 'n')
        curr_char = '\n';
      else if (curr_char == 't')
        curr_char = '\t';
      else if (curr_char == 'v')
	curr_char = '\v';
      else if (curr_char == 'a')
        curr_char = '\a';
      else if (curr_char == 'b')
        curr_char = '\b';
      else if (curr_char == 'r')
        curr_char = '\r';
      else if (curr_char == 'f')
        curr_char = '\f';
      else if (curr_char == '\\' || curr_char == '\'' || curr_char == '\"')
        ;
      else if (curr_char == '\n')
	*correct_newln = TRUE;
      else if (isdigit (curr_char) && curr_char != '8' && curr_char != '9')
	{
	  char_code = VALUE_OF_DIGIT (curr_char);
	  curr_char = fgetc (f);
	  if (!isdigit (curr_char) || curr_char == '8' || curr_char == '9')
	    ungetc (curr_char, f);
	  else
	    {
	      char_code = (char_code * 8 + VALUE_OF_DIGIT (curr_char));
	      curr_char = fgetc (f);
	      if (!isdigit (curr_char) || curr_char == '8' || curr_char == '9')
		ungetc (curr_char, f);
	      else
		char_code = (char_code * 8 + VALUE_OF_DIGIT (curr_char));
	    }
	  curr_char = char_code;
      }
    }
  return curr_char;
}

static void
invinput_error (FILE *f, const char *function_name, int ln_flag)
{
  int curr_char;

  if (ln_flag)
    do
      {
	curr_char = fgetc (f);
      }
    while (curr_char != EOF && curr_char != '\n');
  eval_error (invinput_bc_decl, get_cpos (), DERR_invalid_input, function_name);
}

/* Used by read_number.  */
static FILE *number_file;
static int n_getc (void) { return fgetc (number_file); }
static void n_ungetc (int c) { ungetc (c, number_file); }

/* The following function is analogous to `yylex' in Dino scanner.  If
   `yylex' is changed, please modify this function too. */
static struct token
get_token (FILE *f, const char *function_name, int ln_flag)
{
  int curr_char;
  struct token result;

  VLO_NULLIFY (el_text);
  for (;;)
    {
      curr_char = fgetc (f);
      /* `current_position' corresponds `curr_char' here. */
      switch (curr_char)
        {
          /* Break results in skipping all white spaces. */
        case ' ':
        case '\f':
        case '\t':
        case '\r':
        case '\n':
          break;
        case ':':
        case ',':
        case '[':
        case ']':
        case EOF:
	  result.token_code = curr_char;
	  return result;
	case 't':
	  curr_char = fgetc (f);
	  if (curr_char != 'a')
	    invinput_error (f, function_name, ln_flag);
	  curr_char = fgetc (f);
	  if (curr_char != 'b')
	    invinput_error (f, function_name, ln_flag);
	  result.token_code = F_TAB;
	  return result;
        case '\'':
          {
            int correct_newln, char_code;
            
            curr_char = fgetc (f);
            if (curr_char == '\'')
	      invinput_error (f, function_name, ln_flag);
            else
              {
                curr_char = get_char_code (f, curr_char, &correct_newln);
                if (curr_char < 0 || correct_newln)
		  {
		    if (ln_flag && curr_char == '\n')
		      ungetc (curr_char, f);
		    invinput_error (f, function_name, ln_flag);
		  }
              }
            char_code = fgetc (f);
            if (char_code != '\'')
              {
                ungetc (char_code, f);
		invinput_error (f, function_name, ln_flag);
              }
	    result.val.ch = curr_char;
	    result.token_code = F_CHAR;
            return result;
          }
        case '\"':
          {
            int correct_newln;
            
            for (;;)
              {
                curr_char = fgetc (f);
                if (curr_char == '\"')
                  break;
                curr_char = get_char_code (f, curr_char, &correct_newln);
                if (curr_char < 0)
                  {
		    invinput_error (f, function_name, ln_flag);
                    break;
                  }
                if (!correct_newln)
                  VLO_ADD_BYTE (el_text, curr_char);
              }
            VLO_ADD_BYTE (el_text, '\0');
	    result.val.str = VLO_BEGIN (el_text);
	    result.token_code = F_STRING;
            return result;
          }
        default:
	  {
	    int next_char = fgetc (f);

	    ungetc (next_char, f);
	    if (isdigit (curr_char)
		|| ((curr_char == '-' || curr_char == '+')
		    && isdigit (next_char)))
	      {
		enum read_number_code err_code;
		int read_ch_num, float_p, long_p, base;
		const char *repr;

		number_file = f;
		err_code = read_number (curr_char, n_getc, n_ungetc,
					&read_ch_num, &repr, &base,
					&float_p, &long_p);
		if (errno)
		  process_system_errors (function_name);
		if (err_code != NUMBER_OK)
		  {
		    curr_char = fgetc (f);
		    if (ln_flag && curr_char == '\n')
		      ungetc (curr_char, f);
		    invinput_error (f, function_name, ln_flag);
		  }
		else if (long_p)
		  {
		    ER_node_t gmp = create_gmp ();

		    result.token_code = F_LONG;
		    mpz_set_str (*ER_mpz_ptr (gmp), repr, base);
		    result.val.gmp = gmp;
		  }
		else if (float_p)
		  {
		    result.token_code = F_FLOAT;
		    result.val.f = a2f (repr);
		    if (errno)
		      process_system_errors ("string-to-float conversion");
		  }
		else
		  {
		    result.token_code = F_INT;
		    result.val.i = a2i (repr, base);
		    if (errno)
		      process_system_errors ("string-to-int conversion");
		  }
		return result;
	      }
	    else
	      invinput_error (f, function_name, ln_flag);
	  }
        }
    }
}

/* This resursive function reads a DINO value according to the
   following syntax:

      element : char
              | integer-value
              | float-value
              | long-value
              | string
              | '[' [list] ']'
              | tab '[' [list] ']'

      list : [element ':'] element
           | list ',' [element ':'] element

   If syntax (or semantics) of values is changed, please modify this
   function too. */
static val_t
scanel (FILE *f, struct token token, const char *function_name, int ln_flag)
{
  val_t result;
  ER_node_t ptr = (ER_node_t) &result;

  switch (token.token_code)
    {
    case F_CHAR:
      ER_SET_MODE (ptr, ER_NM_char);
      ER_set_ch (ptr, token.val.ch);
      return result;
    case F_INT:
      ER_SET_MODE (ptr, ER_NM_int);
      ER_set_i (ptr, token.val.i);
      return result;
    case F_FLOAT:
      ER_SET_MODE (ptr, ER_NM_float);
      ER_set_f (ptr, token.val.f);
      return result;
    case F_LONG:
      ER_SET_MODE (ptr, ER_NM_long);
      ER_set_l (ptr, token.val.gmp);
      return result;
    case F_STRING:
      {
	ER_node_t vect;

	ER_SET_MODE (ptr, ER_NM_vect);
	vect = create_string (token.val.str);
	set_vect_dim (ptr, vect, 0);
	return result;
      }
    case '[':
      {
	int_t repeat;
	int_t i;
	ER_node_t vect;

	vect = create_empty_vector ();
	token = get_token (f, function_name, ln_flag);
	for (;;)
	  {
	    if (token.token_code == ']')
	      {
		ER_SET_MODE (ptr, ER_NM_vect);
		set_vect_dim (ptr, vect, 0);
		return result;
	      }
	    result = scanel (f, token, function_name, ln_flag);
	    token = get_token (f, function_name, ln_flag);
	    if (token.token_code == ':')
	      {
		implicit_int_conversion (ctop, (ER_node_t) &result);
		if (ER_NODE_MODE (ptr) != ER_NM_int)
		  invinput_error (f, function_name, ln_flag);
		repeat = ER_i (ptr);
		if (repeat < 0)
		  repeat = 0;
		token = get_token (f, function_name, ln_flag);
		result = scanel (f, token, function_name, ln_flag);
		token = get_token (f, function_name, ln_flag);
	      }
	    else
	      repeat = 1;
	    if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
	      vect = unpack_vector (vect);
	    vect = expand_vector (vect, ER_els_number (vect) + repeat);
	    for (i = ER_els_number (vect); repeat > 0; i++, repeat--)
	      *(val_t *) IVAL (ER_unpack_els (vect), i) = result;
	    ER_set_els_number (vect, i);
	    if (token.token_code == ',')
	      token = get_token (f, function_name, ln_flag);
	  }
	}
    case F_TAB:
      {
	ER_node_t tab;
	val_t key_val;
	ER_node_t key = (ER_node_t) &key_val;
	ER_node_t entry;
	ER_node_mode_t mode;

	tab = create_tab (40);
	token = get_token (f, function_name, ln_flag);
	if (token.token_code != '[')
	  invinput_error (f, function_name, ln_flag);
	token = get_token (f, function_name, ln_flag);
	for (;;)
	  {
	    if (token.token_code == ']')
	      {
		ER_SET_MODE (ptr, ER_NM_tab);
		ER_set_tab (ptr, tab);
		return result;
	      }
	    result = scanel (f, token, function_name, ln_flag);
	    token = get_token (f, function_name, ln_flag);
	    if (token.token_code == ':')
	      {
		key_val = result;
		mode = ER_NODE_MODE (key);
		token = get_token (f, function_name, ln_flag);
		result = scanel (f, token, function_name, ln_flag);
		token = get_token (f, function_name, ln_flag);
		if (mode == ER_NM_vect)
		  set_vect_dim (key, ER_vect (key), 0);
		else if (mode == ER_NM_tab)
		  ER_set_tab (key, ER_tab (key));
	      }
	    else
	      key_val = result;
	    entry = find_tab_entry (tab, key, TRUE);
	    if (ER_NODE_MODE (entry) != ER_NM_empty_entry
		&& ER_NODE_MODE (entry) != ER_NM_deleted_entry)
	      invinput_error (f, function_name, ln_flag);
	    *(val_t *) entry = key_val;
	    make_immutable (entry);
	    *((val_t *) entry + 1) = result;
	    if (token.token_code == ',')
	      token = get_token (f, function_name, ln_flag);
	  }
      }
    default:
      invinput_error (f, function_name, ln_flag);
    }
}

static void
general_scan_call (FILE *f, int file_flag, int ln_flag)
{
  const char *function_name;
  struct token token;
  val_t val;
  int curr_char;

  function_name = (file_flag
		   ? (ln_flag ? FSCANLN_NAME : FSCAN_NAME)
		   : (ln_flag ? SCANLN_NAME : SCAN_NAME));
  errno = 0;
  token = get_token (f, function_name, ln_flag);
  if (token.token_code == EOF)
    eval_error (eof_bc_decl, get_cpos (), DERR_eof_occured, function_name);
  val = scanel (f, token, function_name, ln_flag);
  /* Skip input to the of line. */
  if (ln_flag)
    do
      {
	curr_char = fgetc (f);
      }
    while (curr_char != EOF && curr_char != '\n');
  if (errno != 0)
    process_system_errors (function_name);
  /* Place the result. */
  *(val_t *) fun_result = val;
}

void
scan_call (int_t pars_number)
{
  if (pars_number != 0)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, SCAN_NAME);
  general_scan_call (stdin, FALSE, FALSE);
}

void
scanln_call (int_t pars_number)
{
  if (pars_number != 0)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, SCANLN_NAME);
  general_scan_call (stdin, FALSE, TRUE);
}

void
fscan_call (int_t pars_number)
{
  general_scan_call (fget_function_call_start (pars_number, FSCAN_NAME), TRUE,
		     FALSE);
}

void
fscanln_call (int_t pars_number)
{
  general_scan_call (fget_function_call_start (pars_number, FSCANLN_NAME),
		     TRUE, TRUE);
}

static void
int_function_end (int_t result, int_t pars_number)
{
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, result);
}

static void
function_without_par (int_t pars_number, const char *function_name)
{
  if (pars_number != 0)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, function_name);
}

void
getpid_call (int_t pars_number)
{
  function_without_par (pars_number, GETPID_NAME);
  int_function_end (getpid (), pars_number);
}

static void
str_function_end (char *result, int_t pars_number)
{
  ER_node_t vect;

  vect = create_string (result);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

static void
general_putf_call (FILE *f, int_t pars_number, enum file_param_type param_type)
{
  const char *function_name;
  ER_node_t val;
  const char *fmt;
  int start;

  start = 0;
  if (param_type == NO_FILE)
    {
      function_name = SPUTF_NAME;
      d_assert (f == NULL);
    }
  else if (param_type == STANDARD_FILE)
    function_name = PUTF_NAME;
  else
    {
      function_name = FPUTF_NAME;
      start = 1;
    }
  if (pars_number - start <= 0)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, function_name);
  val = IVAL (ctop, -pars_number + 1 + start);
  to_vect_string_conversion (val, NULL, NULL);
  if (ER_NODE_MODE (val) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (val)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (val)) != ER_NM_char)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, function_name);
  fmt = ER_pack_els (ER_vect (val));
  form_format_string (fmt, IVAL (ctop, -pars_number + 2 + start),
		      pars_number - 1 - start, function_name);
  finish_output (f, pars_number);
}

void
putf_call (int_t pars_number)
{
  general_putf_call (stdout, pars_number, STANDARD_FILE);
}

void
fputf_call (int_t pars_number)
{
  general_putf_call (file_function_call_start (pars_number, FPUTF_NAME),
		    pars_number, GIVEN_FILE);
}

void
sputf_call (int_t pars_number)
{
  general_putf_call (NULL, pars_number, NO_FILE);
}

void
getun_call (int_t pars_number)
{
  function_without_par (pars_number, GETUN_NAME);
  str_function_end (getun (), pars_number);
}

void
geteun_call (int_t pars_number)
{
  function_without_par (pars_number, GETEUN_NAME);
  str_function_end (geteun (), pars_number);
}

void
getgn_call (int_t pars_number)
{
  function_without_par (pars_number, GETGN_NAME);
  str_function_end (getgn (), pars_number);
}

void
getegn_call (int_t pars_number)
{
  function_without_par (pars_number, GETEGN_NAME);
  str_function_end (getegn (), pars_number);
}

void
getgroups_call (int_t pars_number)
{
  ER_node_t vect;
  size_t els_number, grs_n;
  size_t i;

  if (pars_number != 0)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, GETGROUPS_NAME);
#if defined(HAVE_GETGROUPS)
  els_number = getgroups (0, NULL);
  VLO_NULLIFY (temp_vlobj);
  VLO_EXPAND (temp_vlobj, sizeof (GETGROUPS_T) * els_number);
  if (getgroups (els_number, (GETGROUPS_T *) VLO_BEGIN (temp_vlobj)) < 0
      && errno)
    process_system_errors (GETGROUPS_NAME);
  for (grs_n = i = 0; i < els_number; i++)
    if (getgrgid (((GETGROUPS_T *) VLO_BEGIN (temp_vlobj)) [i]) != NULL)
      grs_n++;
  if (grs_n == 0)
    vect = create_empty_vector ();
  else
    {
      vect = create_pack_vector (grs_n, ER_NM_vect);
      ER_set_els_number (vect, 0);
    }
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
  if (grs_n != 0)
      for (grs_n = i = 0; i < els_number; i++)
	{
	  struct group *gr;
	  
	  gr = getgrgid (((GETGROUPS_T *) VLO_BEGIN (temp_vlobj)) [i]);
	  if (gr != NULL)
	    {
	      vect = create_string (gr->gr_name);
	      set_packed_vect_el (ER_vect (fun_result), grs_n, vect);
	      grs_n++;
	      ER_set_els_number (ER_vect (fun_result), grs_n);
	    }
	}
#else
  vect = create_empty_vector ();
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
#endif
}

static void do_inline
float_function_start (int_t pars_number, const char *function_name)
{
  floating_t result;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, function_name);
  implicit_float_conversion (ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_float)
    eval_error (partype_bc_decl,
		get_cpos (), DERR_parameter_type, function_name);
  errno = 0;
}

static void do_inline
float_function_start2 (int_t pars_number, const char *function_name)
{
  floating_t result;

  if (pars_number != 2)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, function_name);
  implicit_float_conversion (ctop, NULL);
  implicit_float_conversion (below_ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_float
      || ER_NODE_MODE (below_ctop) != ER_NM_float)
    eval_error (partype_bc_decl,
		get_cpos (), DERR_parameter_type, function_name);
  errno = 0;
}

#ifdef FLOATING_NAN
static floating_t floating_nan;
static floating_t minus_floating_nan;
#endif

static void
float_function_finish (int_t pars_number, floating_t result,
		       const char *function_name)
{
  if (!errno)
    {
#ifdef IS_FLOATING_NAN
      /* Remember NaN == NaN equals FALSE */
      if (IS_FLOATING_NAN (result))
	errno = EDOM;
#endif
#ifdef FLOATING_HUGE_VAL
      if (result == FLOATING_HUGE_VAL || result == -FLOATING_HUGE_VAL)
	errno = ERANGE;
#endif
    }
  if (errno)
    process_system_errors (function_name);
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_float);
  ER_set_f (fun_result, result);
}

void
sqrt_call (int_t pars_number)
{
  float_function_start (pars_number, SQRT_NAME);
  float_function_finish (pars_number, sqrt (ER_f (ctop)), SQRT_NAME);
}

void
exp_call (int_t pars_number)
{
  float_function_start (pars_number, EXP_NAME);
  float_function_finish (pars_number, exp (ER_f (ctop)), EXP_NAME);
}

void
log_call (int_t pars_number)
{
  float_function_start (pars_number, LOG_NAME);
  float_function_finish (pars_number, log (ER_f (ctop)), LOG_NAME);
}

void
log10_call (int_t pars_number)
{
  float_function_start (pars_number, LOG10_NAME);
  float_function_finish (pars_number, log10 (ER_f (ctop)), LOG10_NAME);
}

void
pow_call (int_t pars_number)
{
  float_function_start2 (pars_number, POW_NAME);
  float_function_finish (pars_number, pow (ER_f (below_ctop), ER_f (ctop)),
			 POW_NAME);
}

void
sin_call (int_t pars_number)
{
  float_function_start (pars_number, SIN_NAME);
  float_function_finish (pars_number, sin (ER_f (ctop)), SIN_NAME);
}

void
cos_call (int_t pars_number)
{
  float_function_start (pars_number, COS_NAME);
  float_function_finish (pars_number, cos (ER_f (ctop)), COS_NAME);
}

void
atan2_call (int_t pars_number)
{
  float_function_start2 (pars_number, ATAN2_NAME);
  float_function_finish (pars_number, atan2 (ER_f (below_ctop), ER_f (ctop)),
			 ATAN2_NAME);
}

static void
general_rand_call (int_t pars_number, int rand_flag)
{
  int_t seed;

  if ((rand_flag && pars_number != 0) || (! rand_flag && pars_number > 1))
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number,
		(rand_flag ? RAND_NAME : SRAND_NAME));
  if (!rand_flag &&  pars_number == 1)
    {
      implicit_int_conversion (ctop, NULL);
      if (ER_NODE_MODE (ctop) == ER_NM_int)
	seed = ER_i (ctop);
      else
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, SRAND_NAME);
    }
  if (rand_flag)
    {
      /* Place the result instead of the function. */
      ER_SET_MODE (fun_result, ER_NM_float);
      ER_set_f (fun_result, (rand () + 0.0) / RAND_MAX);
    }
  else
    {
      ER_SET_MODE (fun_result, ER_NM_undef);
      if (pars_number == 1)
	srand ((unsigned) seed);
      else
	srand ((unsigned) time (NULL));
    }
}

void
rand_call (int_t pars_number)
{
  general_rand_call (pars_number, TRUE);
}

void
srand_call (int_t pars_number)
{
  general_rand_call (pars_number, FALSE);
}

void
process_system_errors (const char *function_name)
{
  switch (errno)
    {
#ifdef EACCES
    case EACCES:
      /* Permission denied. */
      eval_error (eaccess_bc_decl, get_cpos (), DERR_eaccess, function_name);
      break;
#endif
#ifdef EAGAIN
    case EAGAIN:
      eval_error (eagain_bc_decl, get_cpos (), DERR_eagain, function_name);
      break;
#endif
#ifdef EBADF
    case EBADF:
      eval_error (ebadf_bc_decl, get_cpos (), DERR_ebadf, function_name);
      break;
#endif
#ifdef EBUSY
    case EBUSY:
      eval_error (ebusy_bc_decl, get_cpos (), DERR_ebusy, function_name);
      break;
#endif
#ifdef ECHILD
    case ECHILD:
      eval_error (echild_bc_decl, get_cpos (), DERR_echild, function_name);
      break;
#endif
#ifdef EDEADLK
    case EDEADLK:
      eval_error (edeadlk_bc_decl, get_cpos (), DERR_edeadlk, function_name);
      break;
#endif
#ifdef EDOM
    case EDOM:
      eval_error (edom_bc_decl, get_cpos (), DERR_edom, function_name);
      break;
#endif
#ifdef EEXIST
    case EEXIST:
      eval_error (eexist_bc_decl, get_cpos (), DERR_eexist, function_name);
      break;
#endif
#ifdef EFAULT
    case EFAULT:
      eval_error (efault_bc_decl, get_cpos (), DERR_efault, function_name);
      break;
#endif
#ifdef EFBIG
    case EFBIG:
      eval_error (efbig_bc_decl, get_cpos (), DERR_efbig, function_name);
      break;
#endif
#ifdef EINTR
    case EINTR:
      eval_error (eintr_bc_decl, get_cpos (), DERR_eintr, function_name);
      break;
#endif
#ifdef EINVAL
    case EINVAL:
      eval_error (einval_bc_decl, get_cpos (), DERR_einval, function_name);
      break;
#endif
#ifdef EIO
    case EIO:
      eval_error (eio_bc_decl, get_cpos (), DERR_eio, function_name);
      break;
#endif
#ifdef EISDIR
    case EISDIR:
      eval_error (eisdir_bc_decl, get_cpos (), DERR_eisdir, function_name);
      break;
#endif
#ifdef EMFILE
    case EMFILE:
      eval_error (emfile_bc_decl, get_cpos (), DERR_emfile, function_name);
      break;
#endif
#ifdef EMLINK
    case EMLINK:
      eval_error (emlink_bc_decl, get_cpos (), DERR_emlink, function_name);
      break;
#endif
#ifdef ENAMETOOLONG
    case ENAMETOOLONG:
      eval_error (enametoolong_bc_decl, get_cpos (),
		  DERR_enametoolong, function_name);
      break;
#endif
#ifdef ENFILE
    case ENFILE:
      eval_error (enfile_bc_decl, get_cpos (), DERR_enfile, function_name);
      break;
#endif
#ifdef ENODEV
    case ENODEV:
      eval_error (enodev_bc_decl, get_cpos (), DERR_enodev, function_name);
      break;
#endif
#ifdef ENOENT
    case ENOENT:
      /* File or directory does not exist, or directory name is an empty
	 string. */
      eval_error (enoent_bc_decl, get_cpos (), DERR_enoent, function_name);
      break;
#endif
#ifdef ENOEXEC
    case ENOEXEC:
      eval_error (enoexec_bc_decl, get_cpos (), DERR_enoexec, function_name);
      break;
#endif
#ifdef ENOLCK
    case ENOLCK:
      eval_error (enolck_bc_decl, get_cpos (), DERR_enolck, function_name);
      break;
#endif
#ifdef ENOMEM
    case ENOMEM:
      eval_error (enomem_bc_decl, get_cpos (), DERR_enomem, function_name);
      break;
#endif
#ifdef ENOSPC
    case ENOSPC:
      eval_error (enospc_bc_decl, get_cpos (), DERR_enospc, function_name);
      break;
#endif
#ifdef ENOSYS
    case ENOSYS:
      eval_error (enosys_bc_decl, get_cpos (), DERR_enosys, function_name);
      break;
#endif
#ifdef ENOTDIR
    case ENOTDIR:
      /* This is not a directory. */
      eval_error (enotdir_bc_decl, get_cpos (), DERR_enotdir, function_name);
      break;
#endif
#ifdef ENOTEMPTY
#if defined(EEXIST) && EEXIST!=ENOTEMPTY
    case ENOTEMPTY:
      eval_error (enotempty_bc_decl, get_cpos (),
		  DERR_enotempty, function_name);
      break;
#endif
#endif
#ifdef ENOTTY
    case ENOTTY:
      eval_error (enotty_bc_decl, get_cpos (), DERR_enotty, function_name);
      break;
#endif
#ifdef ENXIO
    case ENXIO:
      eval_error (enxio_bc_decl, get_cpos (), DERR_enxio, function_name);
      break;
#endif
#ifdef EPERM
    case EPERM:
      eval_error (eperm_bc_decl, get_cpos (), DERR_eperm, function_name);
      break;
#endif
#ifdef EPIPE
    case EPIPE:
      eval_error (epipe_bc_decl, get_cpos (), DERR_epipe, function_name);
      break;
#endif
#ifdef ERANGE
    case ERANGE:
      eval_error (erange_bc_decl, get_cpos (), DERR_erange, function_name);
      break;
#endif
#ifdef EROFS
    case EROFS:
      eval_error (erofs_bc_decl, get_cpos (), DERR_erofs, function_name);
      break;
#endif
#ifdef ESPIPE
    case ESPIPE:
      eval_error (espipe_bc_decl, get_cpos (), DERR_espipe, function_name);
      break;
#endif
#ifdef ESRCH
    case ESRCH:
      eval_error (esrch_bc_decl, get_cpos (), DERR_esrch, function_name);
      break;
#endif
#ifdef EXDEV
    case EXDEV:
      eval_error (exdev_bc_decl, get_cpos (), DERR_exdev, function_name);
      break;
#endif
    default:
      /* We don't care does strerror exist or not because it is for
         errors.c. */
      d_assert (errno > 0);
      eval_error (syserror_bc_decl, get_cpos (),
		  strerror (errno), function_name);
      break;
    }
}

/* The function is not supposed to be used by Dino user.  It should be
   used by developer of Dino external libraries. */
void
process_errno_call (int_t pars_number)
{
  const char *name;

  if (pars_number > 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, PROCESS_ERRNO_NAME);
  if (pars_number == 0)
    name = "";
  else
    {
      to_vect_string_conversion (ctop, NULL, NULL);
      name = ER_pack_els (ER_vect (ctop));
    }
  if (errno)
    process_system_errors (name);
  ER_SET_MODE (fun_result, ER_NM_undef);
}

void
readdir_call (int_t pars_number)
{
  ER_node_t result;
  ER_node_t vect;
  DIR *dir;
  struct dirent *dirent;
  size_t i;
  size_t dir_files_number;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, READDIR_NAME);
  to_vect_string_conversion (ctop, NULL, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, READDIR_NAME);
  dir = opendir (ER_pack_els (ER_vect (ctop)));
  if (dir == NULL)
    process_system_errors (READDIR_NAME);
  else
    {
      errno = 0;
      for (i = 0; readdir (dir) != NULL; i++)
	;
      if (errno != 0)
	/* Internall error: EBADF, EFAULT, EINVAL, ENOENT, ENOTDIR and
           may be something else. */
	eval_error (internal_bc_decl, get_cpos (),
		    DERR_internal_error, READDIR_NAME);
      if (closedir (dir) != 0)
	/* Internall error: EBADF and may be something else. */
	eval_error (internal_bc_decl, get_cpos (),
		    DERR_internal_error, READDIR_NAME);
      dir = opendir (ER_pack_els (ER_vect (ctop)));
      if (dir == NULL)
	process_system_errors (READDIR_NAME);
      else
	{
	  dir_files_number = i;
	  result = create_pack_vector (dir_files_number, ER_NM_vect);
	  ER_set_els_number (result, 0);
	  /* We read maximum which may be in the vector.  Remember
             that the directory may be changed during two opendir
             calls. */
	  for (i = 0; i < dir_files_number; i++)
	    {
	      errno = 0;
	      dirent = readdir (dir);
	      if (errno != 0)
		/* Internall error: EBADF, EFAULT, EINVAL, ENOENT,
		   ENOTDIR and may be something else. */
		eval_error (internal_bc_decl, get_cpos (),
			    DERR_internal_error, READDIR_NAME);
	      if (dirent == NULL)
		break;
	      vect = create_string (dirent->d_name);
	      set_packed_vect_el (result, i, vect);
	      ER_set_els_number (result, i + 1);
	    }
	  if (closedir (dir) != 0)
	    /* Internall error: EBADF and may be something else. */
	    eval_error (internal_bc_decl, get_cpos (),
			DERR_internal_error, READDIR_NAME);
	}
    }
  d_assert (result != NULL);
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, result, 0);
}

static void
stat_start (int_t pars_number, const char *function_name, struct stat *buf)
{
  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, function_name);
  to_vect_string_conversion (ctop, NULL, NULL);
  get_stat (ctop, function_name, buf);
}

void
ftype_call (int_t pars_number)
{
  struct stat buf;
  int result;

  stat_start (pars_number, FTYPE_NAME, &buf);
  if (S_ISREG (buf.st_mode))
    result = 'f';
  else if (S_ISDIR (buf.st_mode))
    result = 'd';
#ifdef S_ISLNK
  else if (S_ISLNK (buf.st_mode))
    result = 'L';
#endif
  else if (S_ISCHR (buf.st_mode))
    result = 'c';
#ifdef S_ISBLK
  else if (S_ISBLK (buf.st_mode))
    result = 'b';
#endif
#ifdef S_ISFIFO
  else if (S_ISFIFO (buf.st_mode))
    result = 'p';
#endif
#ifdef S_ISSOCK
  else if (S_ISSOCK (buf.st_mode))
    result = 'S';
#endif
  else
    result = (-1);
  if (result < 0)
    ER_SET_MODE (fun_result, ER_NM_nil);
  else
    {
      ER_SET_MODE (fun_result, ER_NM_char);
      ER_set_ch (fun_result, result);
    }
}

static void
stat_finish (int_t pars_number, int_t result)
{
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, result);
}

void
fuidn_call (int_t pars_number)
{
  struct stat buf;
  ER_node_t result;

  stat_start (pars_number, FUIDN_NAME, &buf);
  result = create_string (getpwuid (buf.st_uid)->pw_name);
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, result, 0);
}

void
fgrpn_call (int_t pars_number)
{
  struct stat buf;
  ER_node_t result;
  
  stat_start (pars_number, FGRPN_NAME, &buf);
  {
    char *str;
    struct group *p;
    
    p = getgrgid (buf.st_gid);
    if (p == NULL)
      str = "Unknown";
    else
      str = p->gr_name;
    result = create_string (str);
  }
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, result, 0);
}

void
fsize_call (int_t pars_number)
{
  struct stat buf;
  int_t result;

  stat_start (pars_number, FSIZE_NAME, &buf);
  result = buf.st_size;
  stat_finish (pars_number, result);
}

void
fatime_call (int_t pars_number)
{
  struct stat buf;
  int_t result;

  stat_start (pars_number, FATIME_NAME, &buf);
  result = buf.st_atime;
  stat_finish (pars_number, result);
}

void
fmtime_call (int_t pars_number)
{
  struct stat buf;
  int_t result;

  stat_start (pars_number, FMTIME_NAME, &buf);
  result = buf.st_mtime;
  stat_finish (pars_number, result);
}

void
fctime_call (int_t pars_number)
{
  struct stat buf;
  int_t result;

  stat_start (pars_number, FCTIME_NAME, &buf);
  result = buf.st_ctime;
  stat_finish (pars_number, result);
}

static void
mode_finish (int_t pars_number, const char *result)
{
  ER_node_t vect;

  vect = create_string (result);
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

void
fumode_call (int_t pars_number)
{
  struct stat buf;
  char result [5];
  char *str = result;

  stat_start (pars_number, FUMODE_NAME, &buf);
  if (buf.st_mode & S_ISVTX)
    *str++ = 's';
  if (buf.st_mode & S_IRUSR)
    *str++ = 'r';
  if (buf.st_mode & S_IWUSR)
    *str++ = 'w';
  if (buf.st_mode & S_IXUSR)
    *str++ = 'x';
  *str = '\0';
  mode_finish (pars_number, result);
}

void
fgmode_call (int_t pars_number)
{
  struct stat buf;
  char result [5];
  char *str = result;

  stat_start (pars_number, FGMODE_NAME, &buf);
  if (buf.st_mode & S_IRGRP)
    *str++ = 'r';
  if (buf.st_mode & S_IWGRP)
    *str++ = 'w';
  if (buf.st_mode & S_IXGRP)
    *str++ = 'x';
  *str = '\0';
  mode_finish (pars_number, result);
}

void
fomode_call (int_t pars_number)
{
  struct stat buf;
  char result [5];
  char *str = result;

  stat_start (pars_number, FOMODE_NAME, &buf);
  if (buf.st_mode & S_IROTH)
    *str++ = 'r';
  if (buf.st_mode & S_IWOTH)
    *str++ = 'w';
  if (buf.st_mode & S_IXOTH)
    *str++ = 'x';
  *str = '\0';
  mode_finish (pars_number, result);
}

void
time_call (int_t pars_number)
{
  time_t t;

  if (pars_number != 0)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, TIME_NAME);
  t = time (NULL);
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, t);
}

void
strtime_call (int_t pars_number)
{
  time_t t;
  struct tm *tm;
  const char *format;
  const char *str;
  ER_node_t vect;
  ER_node_t format_var;
  int percents_number;
  size_t max;

  if (pars_number > 2)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, STRTIME_NAME);
  if (pars_number == 2)
    {
      implicit_int_conversion (ctop, NULL);
      if (ER_NODE_MODE (ctop) != ER_NM_int)
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, STRTIME_NAME);
      t = ER_i (ctop);
    }
  else
    t = time (NULL);
  if (pars_number >= 1)
    {
      format_var = (pars_number == 1 ? ctop : below_ctop);
      to_vect_string_conversion (format_var, NULL, NULL);
      if (ER_NODE_MODE (format_var) == ER_NM_vect
	  && ER_NODE_MODE (ER_vect (format_var)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (ER_vect (format_var)) == ER_NM_char)
	format = ER_pack_els (ER_vect (format_var));
      else
	eval_error (partype_bc_decl,
		    get_cpos (), DERR_parameter_type, STRTIME_NAME);
    }
  else
    {
      format_var = IVAL (ER_stack_vars (uppest_stack),
			 BC_var_num (time_format_bc_decl));
      to_vect_string_conversion (format_var, NULL, NULL);
      if (ER_NODE_MODE (format_var) == ER_NM_vect
	  && ER_NODE_MODE (ER_vect (format_var)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (ER_vect (format_var)) == ER_NM_char)
	format = ER_pack_els (ER_vect (format_var));
      else
	eval_error (invenvar_bc_decl, get_cpos (),
		    DERR_corrupted_environment_var, TIME_FORMAT_NAME);
    }
  tm = localtime (&t);
  for (percents_number = 0, str = format; *str != 0; str++)
    if (*str == '%')
      percents_number++;
  max = strlen (format) + 2 + percents_number * 10;
  vect = create_empty_string (max);
  strftime (ER_pack_els (vect), max, format, tm);
  ER_set_els_number (vect, strlen (ER_pack_els (vect)));
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

/* The following macro is necessary for non standard include files of
   SUNOS 4..., linux */

#ifndef CLOCKS_PER_SECOND
#ifdef CLOCKS_PER_SEC
#define CLOCKS_PER_SECOND CLOCKS_PER_SEC
#elif __linux__
#define CLOCKS_PER_SECOND 100
#elif sun
#define CLOCKS_PER_SECOND 1000000
#elif CLK_TCK
#define CLOCKS_PER_SECOND CLK_TCK
#else
#error define macro CLOCKS_PER_SECOND
#endif
#endif /* CLOCKS_PER_SECOND */

void
clock_call (int_t pars_number)
{
  floating_t secs;

  if (pars_number != 0)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, CLOCK_NAME);
  secs = (floating_t) (clock () - start_time) / CLOCKS_PER_SECOND;
  ER_SET_MODE (fun_result, ER_NM_float);
  ER_set_f (fun_result, secs);
}

void
gc_call (int_t pars_number)
{
  ptrdiff_t offset;

  if (pars_number != 0)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, GC_NAME);
  GC_executed_stmts_count = executed_stmts_count;
  offset = (char *) fun_result - (char *) cstack;
  GC ();
  fun_result = (ER_node_t) ((char *) cstack + offset);
  /* Place the free memory. */
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, free_heap_memory);
}

void
system_call (int_t pars_number)
{
  int code;
  int error_flag;
  ER_node_t val;
  ER_node_t vect;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, SYSTEM_NAME);
  else
    {
      val = IVAL (ctop, -pars_number + 1);
      if (ER_NODE_MODE (val) != ER_NM_vect)
	error_flag = TRUE;
      else
	{
	  to_vect_string_conversion (val, NULL, NULL);
	  vect = ER_vect (val);
	  if (ER_NODE_MODE (vect) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (vect) != ER_NM_char)
	    error_flag = TRUE;
	  else
	    {
	      code = system ((char *) ER_pack_els (vect));
	      if (code == 127)
		eval_error (noshell_bc_decl, get_cpos (),
			    DERR_no_shell, SYSTEM_NAME);
	      else if (code < 0)
		eval_error (systemfail_bc_decl, get_cpos (),
			    DERR_other_fail_in_system_call, SYSTEM_NAME);
	      error_flag = FALSE;
	    }
	}
      if (error_flag)
	eval_error (partype_bc_decl, get_cpos (),
		    DERR_parameter_type, SYSTEM_NAME);
    }
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, code);
}

/* Array of pointers to functions/calls which will be reported in the
   trace of calls.  */
vlo_t trace_stack;

/* Output info about stack trace.  */
void
print_trace_stack (void)
{
  struct trace_stack_elem *elem_ptr;

  for (elem_ptr = VLO_BEGIN (trace_stack);
       (char *) elem_ptr <= (char *) VLO_END (trace_stack);
       elem_ptr++)
    fprintf (stderr, "%s:%u:%u:calling %s\n",
	     get_pos (elem_ptr->pc).file_name,
	     get_pos (elem_ptr->pc).line_number,
	     get_pos (elem_ptr->pc).column_number,
	     BC_ident (BC_fdecl (elem_ptr->block)));
}

void
exit_call (int_t pars_number)
{
  int code;
  BC_node_t block;
  ER_node_t stack;
  struct trace_stack_elem elem;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, EXIT_NAME);
  implicit_int_conversion (ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_int)
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, EXIT_NAME);
  if (trace_flag)
    {
      VLO_NULLIFY (trace_stack);
      
      for (stack = cstack;
	   stack != uppest_stack;
	   stack = ER_prev_stack (stack))
	{
	  block = ER_block_node (stack);
	  if (BC_NODE_MODE (block) == BC_NM_block)
	    continue;
	  elem.block = block;
	  elem.pc = ER_call_pc (stack);
	  VLO_ADD_MEMORY (trace_stack, &elem, sizeof (elem));
	}
    }
  dino_finish (ER_i (ctop));
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, code);
}

/* The following variables contain type of the elements of folded
   array, fold element function, the current result of the fold
   operation, and fold vector dimension.  */
static ER_node_mode_t fold_vect_el_type;
static BC_node_t fold_el_fun_block;
static val_t fold_initval;
static int_t fold_dim;

static void
fold_function (const void *el)
{
  int res;
  ER_node_t context;

  context = GET_TEMP_REF (fold_dim);
  TOP_UP;
  *(val_t *) ctop = fold_initval;
  TOP_UP;
  if (fold_vect_el_type == ER_NM_val)
    *(val_t *) ctop = *(val_t *) el;
  else
    {
      ER_SET_MODE (ctop, fold_vect_el_type);
      memcpy ((char *) ctop + val_displ_table [fold_vect_el_type],
	      (char *) el, type_size_table [fold_vect_el_type]);
      if (fold_vect_el_type == ER_NM_vect)
	ER_set_dim (ctop, 0);
    }
  call_fun_class (fold_el_fun_block, context, 2);
  TOP_UP;
  fold_initval = *(val_t *) ctop;
  TOP_DOWN;
}

static void
process_fold_vect_op (ER_node_t op, int_t dim, int_t depth)
{
  size_t i, len;
  char *pack_els;
  ER_node_t v;
  ER_node_t unpack_els;

  d_assert (dim > 0);
  GO_THROUGH_REDIR (op);
  len = ER_els_number (op);
  if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
    fold_vect_el_type = ER_pack_vect_el_type (op);
  else
    fold_vect_el_type = ER_NM_val;
  if (dim > 1 && ER_NODE_MODE (op) == ER_NM_heap_pack_vect
      && fold_vect_el_type != ER_NM_vect)
    eval_error (vecform_bc_decl, get_cpos (), DERR_vector_form_type, depth);
  PUSH_TEMP_REF (op);
  for (i = 0; i < len; i++)
    {
      if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
	{
	  pack_els = ER_pack_els (op);
	  if (dim > 1)
	    process_fold_vect_op (((ER_node_t *) pack_els) [i],
				  dim - 1, depth + 1);
	  else
	    fold_function (pack_els + i * type_size_table [fold_vect_el_type]);
	}
      else
	{
	  unpack_els = ER_unpack_els (op);
	  v = IVAL (unpack_els, i);
	  if (dim == 1)
	    fold_function (v);
	  else if (ER_NODE_MODE (v) != ER_NM_vect)
	    eval_error (vecform_bc_decl, get_cpos (),
			DERR_vector_form_type, depth);
	  else
	    process_fold_vect_op (ER_vect (v), dim - 1, depth + 1);
	}
      op = GET_TEMP_REF (0);
    }
  POP_TEMP_REF (1);
}

void
fold_call (int_t pars_number)
{
  ER_node_t context, vect;
  ER_node_t par1, par2;
  int fun_result_offset;

  if (pars_number != 3 && pars_number != 4)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, FOLD_NAME);
  par1 = IVAL (ctop, -pars_number + 1);
  par2 = IVAL (ctop, -pars_number + 2);
  if (! fun_p (par1) || ER_NODE_MODE (par2) != ER_NM_vect)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, FOLD_NAME);
  fun_result_offset = (val_t *) fun_result - (val_t *) cvars;
  vect = ER_vect (par2);
  fold_initval = *(val_t *) IVAL (ctop, -pars_number + 3);
  if (pars_number == 3)
    fold_dim = 1;
  else
    {
      ER_node_t dim_par = IVAL (ctop, -pars_number + 4);
      implicit_int_conversion (dim_par, NULL);
      if (!ER_IS_OF_TYPE (dim_par, ER_NM_int))
	eval_error (partype_bc_decl,
		    get_cpos (), DERR_parameter_type, FOLD_NAME);
      fold_dim = ER_i (dim_par);
    }
  if (fold_dim <= 0)
    ;
  else
    {
      GO_THROUGH_REDIR (vect);
      if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (vect);
      context = ER_code_context (par1);
      fold_el_fun_block = ID_TO_CODE (ER_code_id (par1));
      PUSH_TEMP_REF (context);
      process_fold_vect_op (vect, fold_dim, 1);
      POP_TEMP_REF (1);
    }
  /* Place the result. */
  fun_result = IVAL (cvars, fun_result_offset);
  *(val_t *) fun_result = fold_initval;
}

/* The following variables contain type of the elements of
   filtered array, filter element function, and vector dimension  */
static ER_node_mode_t filter_vect_el_type;
static BC_node_t filter_el_fun_block;
static int_t filter_dim;

static int
filter_function (const void *el)
{
  int res;
  ER_node_t context;
  
  context = GET_TEMP_REF (2 * filter_dim);
  TOP_UP;
  if (filter_vect_el_type == ER_NM_val)
    *(val_t *) ctop = *(val_t *) el;
  else
    {
      ER_SET_MODE (ctop, filter_vect_el_type);
      memcpy ((char *) ctop + val_displ_table [filter_vect_el_type],
	      (char *) el, type_size_table [filter_vect_el_type]);
      if (filter_vect_el_type == ER_NM_vect)
	ER_set_dim (ctop, 0);
    }
  call_fun_class (filter_el_fun_block, context, 1);
  TOP_UP;
  implicit_int_conversion (ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_int)
    eval_error (invresult_bc_decl,
		get_cpos (), DERR_invalid_result, FILTER_NAME);
  res = ER_i (ctop) != 0;
  TOP_DOWN;
  return res;
}

static ER_node_t
process_filter_vect_op (ER_node_t op, int_t dim, int_t depth)
{
  size_t i, nel, len, el_size;
  ER_node_t el, result;

  d_assert (dim > 0);
  GO_THROUGH_REDIR (op);
  len = ER_els_number (op);
  if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
    filter_vect_el_type = ER_pack_vect_el_type (op);
  else
    filter_vect_el_type = ER_NM_val;
  if (dim > 1 && ER_NODE_MODE (op) == ER_NM_heap_pack_vect
      && filter_vect_el_type != ER_NM_vect)
    eval_error (vecform_bc_decl, get_cpos (), DERR_vector_form_type, depth);
  if (dim > 1)
    result = create_pack_vector (len, ER_NM_vect);
  else if (filter_vect_el_type != ER_NM_val)
    result = create_pack_vector (len, filter_vect_el_type);
  else
    result = create_unpack_vector (len);
  ER_set_els_number (result, 0);
  PUSH_TEMP_REF (result);
  PUSH_TEMP_REF (op);
  el_size = type_size_table [filter_vect_el_type];
  for (i = nel = 0; i < len; i++)
    {
      if (dim > 1)
	{
	  if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
	    el = process_filter_vect_op (((ER_node_t *) ER_pack_els (op)) [i],
					 dim - 1, depth + 1);
	  else if (ER_NODE_MODE (IVAL (ER_unpack_els (op), i)) != ER_NM_vect)
	    eval_error (vecform_bc_decl, get_cpos (),
			DERR_vector_form_type, depth);
	  else
	    el = process_filter_vect_op (ER_vect (IVAL (ER_unpack_els (op), i)),
					 dim - 1, depth + 1);
	  op = GET_TEMP_REF (0);
	  result = GET_TEMP_REF (1);
	  set_packed_vect_el (result, nel, el);
	}
      else if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
	{
	  if (!filter_function (ER_pack_els (op) + i * el_size))
	    {
	      op = GET_TEMP_REF (0);
	      result = GET_TEMP_REF (1);
	      continue;
	    }
	  else
	    {
	      op = GET_TEMP_REF (0);
	      result = GET_TEMP_REF (1);
	      memcpy (ER_pack_els (result) + nel * el_size,
		      ER_pack_els (op) + i * el_size, el_size);
	    }
	}
      else
	{
	  int flag = filter_function (IVAL (ER_unpack_els (op), i));

	  op = GET_TEMP_REF (0);
	  result = GET_TEMP_REF (1);
	  if (!flag)
	    continue;
	  *(val_t *) IVAL (ER_unpack_els (result), nel)
	    = *(val_t *) IVAL (ER_unpack_els (op), i);
	}
      nel++;
      ER_set_els_number (result, nel);
    }
  POP_TEMP_REF (2);
  if (dim == 1 && ER_NODE_MODE (result) == ER_NM_heap_unpack_vect)
    pack_vector_if_possible (result);
  return result;
}

void
filter_call (int_t pars_number)
{
  ER_node_t context, vect;
  ER_node_t par1, par2;
  int fun_result_offset;

  if (pars_number != 2 && pars_number != 3)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, FILTER_NAME);
  par1 = IVAL (ctop, -pars_number + 1);
  par2 = IVAL (ctop, -pars_number + 2);
  if (! fun_p (par1) || ER_NODE_MODE (par2) != ER_NM_vect)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, FILTER_NAME);
  fun_result_offset = (val_t *) fun_result - (val_t *) cvars;
  vect = ER_vect (par2);
  if (pars_number == 2)
    filter_dim = 1;
  else
    {
      ER_node_t dim_par = IVAL (ctop, -pars_number + 3);
      implicit_int_conversion (dim_par, NULL);
      if (!ER_IS_OF_TYPE (dim_par, ER_NM_int))
	eval_error (partype_bc_decl,
		    get_cpos (), DERR_parameter_type, FILTER_NAME);
      filter_dim = ER_i (dim_par);
    }
  if (filter_dim <= 0)
    ;
  else
    {
      GO_THROUGH_REDIR (vect);
      if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (vect);
      context = ER_code_context (par1);
      filter_el_fun_block = ID_TO_CODE (ER_code_id (par1));
      PUSH_TEMP_REF (context);
      vect = process_filter_vect_op (vect, filter_dim, 1);
      POP_TEMP_REF (1);
    }
  /* Place the result. */
  fun_result = IVAL (cvars, fun_result_offset);
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

/* The following variables contain type of the elements of mapped
   array, map element function, and map vector dimension.  */
static ER_node_mode_t map_vect_el_type;
static BC_node_t map_el_fun_block;
static int_t map_dim;

static void
map_function (const void *el)
{
  int res;
  ER_node_t context;
  
  context = GET_TEMP_REF (2 * map_dim);
  TOP_UP;
  if (map_vect_el_type == ER_NM_val)
    *(val_t *) ctop = *(val_t *) el;
  else
    {
      ER_SET_MODE (ctop, map_vect_el_type);
      memcpy ((char *) ctop + val_displ_table [map_vect_el_type],
	      (char *) el, type_size_table [map_vect_el_type]);
      if (map_vect_el_type == ER_NM_vect)
	ER_set_dim (ctop, 0);
    }
  call_fun_class (map_el_fun_block, context, 1);
  TOP_UP;
}

static ER_node_t
process_map_vect_op (ER_node_t op, int_t dim, int_t depth)
{
  size_t i, len, el_size;
  ER_node_t el, result;

  d_assert (dim > 0);
  GO_THROUGH_REDIR (op);
  len = ER_els_number (op);
  if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
    map_vect_el_type = ER_pack_vect_el_type (op);
  else
    map_vect_el_type = ER_NM_val;
  if (dim > 1 && ER_NODE_MODE (op) == ER_NM_heap_pack_vect
      && map_vect_el_type != ER_NM_vect)
    eval_error (vecform_bc_decl, get_cpos (), DERR_vector_form_type, depth);
  if (dim == 1)
    result = create_unpack_vector (len);
  else
    result = create_pack_vector (len, ER_NM_vect);
  ER_set_els_number (result, 0);
  PUSH_TEMP_REF (result);
  PUSH_TEMP_REF (op);
  el_size = type_size_table [map_vect_el_type];
  for (i = 0; i < len; i++)
    {
      if (dim == 1)
	{
	  if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
	    map_function (ER_pack_els (op) + i * el_size);
	  else
	    map_function (IVAL (ER_unpack_els (op), i));
	  op = GET_TEMP_REF (0);
	  result = GET_TEMP_REF (1);
	  *(val_t *) IVAL (ER_unpack_els (result), i) = *(val_t *) ctop;
	  TOP_DOWN;
	}
      else
	{
	  if (ER_NODE_MODE (op) == ER_NM_heap_pack_vect)
	    el = process_map_vect_op (((ER_node_t *) ER_pack_els (op)) [i],
				      dim - 1, depth + 1);
	  else if (ER_NODE_MODE (IVAL (ER_unpack_els (op), i)) != ER_NM_vect)
	    eval_error (vecform_bc_decl, get_cpos (),
			DERR_vector_form_type, depth);
	  else
	    el = process_map_vect_op (ER_vect (IVAL (ER_unpack_els (op), i)),
				      dim - 1, depth + 1);
	  op = GET_TEMP_REF (0);
	  result = GET_TEMP_REF (1);
	  set_packed_vect_el (result, i, el);
	}
      ER_set_els_number (result, i + 1);
    }
  if (dim == 1 && ER_NODE_MODE (result) == ER_NM_heap_unpack_vect)
    pack_vector_if_possible (result);
  POP_TEMP_REF (2);
  return result;
}

void
map_call (int_t pars_number)
{
  ER_node_t context, vect;
  ER_node_t par1, par2;
  int fun_result_offset;

  if (pars_number != 2 && pars_number != 3)
    eval_error (parnumber_bc_decl,
		get_cpos (), DERR_parameters_number, MAP_NAME);
  par1 = IVAL (ctop, -pars_number + 1);
  par2 = IVAL (ctop, -pars_number + 2);
  if (! fun_p (par1) || ER_NODE_MODE (par2) != ER_NM_vect)
    eval_error (partype_bc_decl, get_cpos (), DERR_parameter_type, MAP_NAME);
  fun_result_offset = (val_t *) fun_result - (val_t *) cvars;
  vect = ER_vect (par2);
  if (pars_number == 2)
    map_dim = 1;
  else
    {
      ER_node_t dim_par = IVAL (ctop, -pars_number + 3);
      implicit_int_conversion (dim_par, NULL);
      if (!ER_IS_OF_TYPE (dim_par, ER_NM_int))
	eval_error (partype_bc_decl,
		    get_cpos (), DERR_parameter_type, MAP_NAME);
      map_dim = ER_i (dim_par);
    }
  if (map_dim <= 0)
    ;
  else
    {
      GO_THROUGH_REDIR (vect);
      if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (vect);
      context = ER_code_context (par1);
      map_el_fun_block = ID_TO_CODE (ER_code_id (par1));
      PUSH_TEMP_REF (context);
      vect = process_map_vect_op (vect, map_dim, 1);
      POP_TEMP_REF (1);
    }
  /* Place the result. */
  fun_result = IVAL (cvars, fun_result_offset);
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, vect, 0);
}

/* Implement matrix transposition.  */
void
transpose_call (int_t pars_number)
{
  ER_node_t vect, row, col, res, res_el;
  size_t i, j, nrows, ncols, el_size, displ;
  const char *pack_els;
  char *res_pack_els;
  ER_node_mode_t res_eltp = ER_NM__error, eltp;

  if (pars_number != 1)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_parameters_number, TRANSPOSE_NAME);
  if (ER_NODE_MODE (ctop) != ER_NM_vect)
    eval_error (partype_bc_decl, get_cpos (),
		DERR_parameter_type, TRANSPOSE_NAME);
  vect = ER_vect (ctop);
  GO_THROUGH_REDIR (vect);
  if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
    pack_vector_if_possible (vect);
  nrows = ER_els_number (vect);
  if (nrows == 0 || ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect
      || ER_pack_vect_el_type (vect) != ER_NM_vect)
    eval_error (matrixform_bc_decl, get_cpos (),
		DERR_matrix_form_type, TRANSPOSE_NAME);
  pack_els = ER_pack_els (vect);
  for (i = 0; i < nrows; i++)
    {
      row = ((ER_node_t *) pack_els) [i];
      GO_THROUGH_REDIR (row);
      eltp = (ER_NODE_MODE (row) == ER_NM_heap_pack_vect
	      ? ER_pack_vect_el_type (row) : ER_NM__error);
      if (i == 0)
	{
	  ncols = ER_els_number (row);
	  res_eltp = eltp;
	}
      else if (ncols != ER_els_number (row))
	eval_error (matrixform_bc_decl, get_cpos (),
		    DERR_matrix_form_type, TRANSPOSE_NAME);
      else if (eltp != res_eltp)
	res_eltp = ER_NM__error;
    }
  if (ncols == 0)
    eval_error (matrixform_bc_decl, get_cpos (),
		DERR_matrix_form_type, TRANSPOSE_NAME);
  res = create_pack_vector (ncols, ER_NM_vect);
  res_pack_els = ER_pack_els (res);
  for (i = 0; i < ncols; i++)
    ((ER_node_t *) res_pack_els) [i]
      = (res_eltp != ER_NM__error
	 ? create_pack_vector (nrows, res_eltp)
	 : create_unpack_vector (nrows));
  for (i = 0; i < nrows; i++)
    {
      row = ((ER_node_t *) pack_els) [i];
      GO_THROUGH_REDIR (row);
      for (j = 0; j < ncols; j++)
	{
	  col = ((ER_node_t *) res_pack_els) [j];
	  if (res_eltp != ER_NM__error)
	    {
	      d_assert (ER_NODE_MODE (row) == ER_NM_heap_pack_vect
			&& ER_NODE_MODE (col) == ER_NM_heap_pack_vect);
	      el_size = type_size_table [res_eltp];
	      memcpy ((char *) ER_pack_els (col) + i * el_size,
		      (char *) ER_pack_els (row) + j * el_size, el_size);
	    }
	  else if (ER_NODE_MODE (row) == ER_NM_heap_pack_vect)
	    {
	      d_assert (ER_NODE_MODE (col) == ER_NM_heap_unpack_vect);
	      eltp = ER_pack_vect_el_type (row);
	      el_size = type_size_table [eltp];
	      res_el = IVAL (ER_unpack_els (col), i);
	      ER_SET_MODE (res_el, eltp);
	      displ = val_displ_table[eltp];
	      memcpy ((char *) res_el + displ,
		      (char *) ER_pack_els (row) + j * el_size, el_size);
	    }
	  else
	    {
	      d_assert (ER_NODE_MODE (col) == ER_NM_heap_unpack_vect);
	      ((val_t *) ER_unpack_els (col))[i]
		= ((val_t *) ER_unpack_els (row))[j];
	    }
	}
    }
  ER_SET_MODE (fun_result, ER_NM_vect);
  set_vect_dim (fun_result, res, 0);
}

/* This function is a trick to fullfil initiations after execution of
   stmts before __init__ call. */
void
init_call (int_t pars_number)
{
  ER_node_t instance;
  ER_node_t var;

  d_assert (pars_number == 0);
  /* ------ Initiations after execution of stmts before __init__ ----- */
  /* Set stdin, stdout, stderr. */
  var = IVAL (ER_stack_vars (cstack), BC_var_num (stdin_bc_decl));
  place_file_instance (stdin, var);
  var = IVAL (ER_stack_vars (cstack), BC_var_num (stdout_bc_decl));
  place_file_instance (stdout, var);
  var = IVAL (ER_stack_vars (cstack), BC_var_num (stderr_bc_decl));
  place_file_instance (stderr, var);
  /* ----- End of the initiations ----- */
  /* Place the result instead of the function. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

static void
call_external_fun (int pars_number, BC_node_t fdecl)
{
  external_fun_t *fun;
  ER_node_t vect;
  ER_node_t val;
  ER_node_t v;
  ER_node_t tab;
  int curr_actual;

  fun = external_address (fdecl);
  vect = (ER_node_t) create_unpack_vector (pars_number);
  for (curr_actual = 0; curr_actual < pars_number; curr_actual++)
    {
      val = IVAL (ctop, curr_actual - pars_number + 1);
      if (ER_IS_OF_TYPE (val, ER_NM_vect))
	{
	  v = ER_vect (val);
	  GO_THROUGH_REDIR (v);
	  if (ER_NODE_MODE (v) == ER_NM_heap_unpack_vect)
	    pack_vector_if_possible (v);
	  ER_set_vect (val, v);
	}
      else if (ER_IS_OF_TYPE (val, ER_NM_tab))
	{
	  tab = ER_tab (val);
	  GO_THROUGH_REDIR (tab);
	  ER_set_tab (val, tab);
	}
      *(val_t *) IVAL (ER_unpack_els (vect), curr_actual) = *(val_t *) val;
    }
  DECR_CTOP (pars_number);
  /* Pop all actual parameters. */
  *(val_t *) ctop
    = (*fun) (pars_number, (val_t *) IVAL (ER_unpack_els (vect), 0));
  INCREMENT_PC();
}

/* Set up variable FROM to BOUND (no including) to nil.  */
static void do_always_inline
reset_vars (ER_node_t from, ER_node_t bound)
{
  for (; from < bound; from = IVAL (from, 1))
    ER_SET_MODE (from, ER_NM_undef);
}

/* Set up formal parameters starting with VARS of BLOCK from
   actual parameters starting with ACTUAL_START.  Check correct number
   of actual parameters.  Initialize rest of VARS_NUMBER vars by
   NIL.  */
static void do_always_inline
setup_pars (BC_node_t block, int actuals_num,
	    ER_node_t vars, val_t *actual_start, int vars_number)
{
  int i, args_p, copies_num, formals_num, min_actuals_num;

  args_p = BC_args_p (block);
  formals_num = BC_pars_num (block) - (args_p ? 1 : 0);
  min_actuals_num = BC_min_pars_num (block);
  if (actuals_num < min_actuals_num)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_too_few_actual_parameters, BC_ident (BC_fdecl (block)));
  else if (actuals_num > formals_num && ! args_p)
    eval_error (parnumber_bc_decl, get_cpos (),
		DERR_too_many_actual_parameters, BC_ident (BC_fdecl (block)));
  copies_num = (formals_num < actuals_num ? formals_num : actuals_num);
  /* Transfer actuals.  */
  for (i = 0; i < copies_num; i++)
    *(val_t *) IVAL (vars, i) = *actual_start++;
  /* Reset rest of variables.  */
  reset_vars ((ER_node_t) ((val_t *) vars + copies_num),
	      (ER_node_t) ((val_t *) vars + vars_number));
  if (args_p)
    {
      ER_node_t vect;
      int nargs = actuals_num - copies_num;
      
      if (nargs == 0)
	vect = create_empty_vector ();
      else
	vect = (ER_node_t) create_unpack_vector (nargs);
      /* Args number */
      ER_SET_MODE (IVAL (vars, formals_num), ER_NM_vect);
      set_vect_dim (IVAL (vars, formals_num), vect, 0);
      for (i = 0; i < nargs; i++)
	*(val_t *) IVAL (ER_unpack_els (vect), i) = *actual_start++;
    }
}

static ER_node_t do_always_inline
create_class_stack (val_t *call_start, int_t actuals_num, int simple_p)
{
  BC_node_t class_block;
  ER_node_t stack;
  pc_t saved_cpc = cpc;

  class_block = ID_TO_CODE (ER_code_id ((ER_node_t) call_start));
  heap_push (class_block, ER_code_context ((ER_node_t) call_start), -1);
  setup_pars (class_block, actuals_num, ER_stack_vars (cstack),
	      call_start + 1, BC_vars_num (class_block));
  stack = cstack;
  if (simple_p)
    {
      heap_pop ();
      cpc = saved_cpc;
    }
  return stack;
}

/* Return pc for the 1st stmt of FBLOCK.  If there is no one (abstract
   function case), generate error.  */
static BC_node_t do_always_inline
get_fblock_pc (BC_node_t fblock)
{
  BC_node_t pc, fdecl;

  d_assert (BC_NODE_MODE (fblock) == BC_NM_fblock);
  pc = BC_next (fblock);
  if (pc != NULL)
    return pc;
  eval_error (abstrcall_bc_decl, get_cpos (),
	      DERR_unfinished_fun_class_call, BC_ident (BC_fdecl (fblock)));
}

/* The following variable is PC of the last call of real DINO function
   (not external or implementation function).  It is used to
   diagnostic of earley parser functions. */
static pc_t real_fun_call_pc;

/* Function processing tail (if TAIL_FLAG) call of function CODE with
   CONTEXT with ACTUALS_NUM params starting with PAR_START.  */
static void do_always_inline
process_fun_call (val_t *par_start, BC_node_t code, ER_node_t context,
		   int actuals_num, int tail_flag)
{
  BC_node_t block = code;
  int vars_number = real_block_vars_number (block);
  
  if (BC_thread_p (code) && sync_flag)
    /* We check it before creating a stack (see
       find_catch_pc).  */
    eval_error (syncthreadcall_bc_decl, get_cpos (),
		DERR_thread_call_in_sync_stmt);
  real_fun_call_pc = cpc;
  d_assert (BC_NODE_MODE (code) == BC_NM_fblock
	    && BC_implementation_fun (code) == NULL);
  if (tail_flag && ! BC_ext_life_p (ER_block_node (cstack))
      && context != cstack && cstack != uppest_stack
      /* We should not worry about extending stack.  Finally in the
	 chain of calls of different functions we have a function
	 block big enough to contain all subsequent tail calls.  */
      && (ER_all_block_vars_num (cstack) >= vars_number + BC_tvars_num (block)))
    {
      ER_set_context (cstack, context);
      ER_set_block_node (cstack, block);
      ctop = IVAL (cvars, vars_number - 1);
    }
  else
    heap_push (block, context, -1);
  setup_pars (block, actuals_num, cvars, par_start, vars_number);
  cpc = get_fblock_pc (block);
  if (BC_thread_p (code))
    {
      ER_node_t process;
      
      process = create_process (cpc, code, context);
      cpc = BC_next (ER_call_pc (cstack));
      ER_set_ctop (cstack, (char *) ctop);
      cstack = ER_prev_stack (cstack);
      ER_set_saved_cstack (cprocess, cstack);
      cvars = ER_stack_vars (cstack);
      ctop = (ER_node_t) ER_ctop (cstack);
      TOP_UP;
      ER_SET_MODE (ctop, ER_NM_process);
      ER_set_process (ctop, process);
      TOP_DOWN;
    }
}

/* The same as previous but also process implementation functions.  */
void do_always_inline
process_imm_fun_call (val_t *call_start, BC_node_t code, ER_node_t context,
		       int actuals_num, int tail_flag)
{
  int vars_number = real_block_vars_number (code);
  int i;
  
  d_assert (BC_NODE_MODE (code) == BC_NM_fblock
	    && BC_implementation_fun (code) == NULL
	    && ! BC_thread_p (code) && ! BC_args_p (code)
	    && actuals_num == BC_pars_num (code));
  real_fun_call_pc = cpc;
  if (tail_flag && ! BC_ext_life_p (ER_block_node (cstack))
      && context != cstack && cstack != uppest_stack
      /* We should not worry about extending stack.  Finally in the
	 chain of calls of different functions we have a function
	 block big enough to contain all subsequent tail calls.  */
      && (ER_all_block_vars_num (cstack) >= vars_number + BC_tvars_num (code)))
    {
      ER_set_context (cstack, context);
      ER_set_block_node (cstack, code);
      ctop = IVAL (cvars, vars_number - 1);
    }
  else if (heap_push_or_set_res (code, context, call_start))
    {
      INCREMENT_PC ();
      return;
    }
  /* Transfer actuals.  */
  for (i = 0; i < actuals_num; i++)
    *(val_t *) IVAL (cvars, i) = *call_start++;
  /* Reset rest of variables.  */
  reset_vars ((ER_node_t) ((val_t *) cvars + actuals_num),
	      (ER_node_t) ((val_t *) cvars + vars_number));
  cpc = get_fblock_pc (code);
}

void do_always_inline
process_imm_ifun_call (BC_node_t code, int actuals_num)
{
  implementation_fun_t ifunc;

  d_assert (BC_NODE_MODE (code) == BC_NM_fblock);
  ifunc = BC_implementation_fun (code);
  d_assert (ifunc != NULL);
  fun_result = IVAL (ctop, 1);
  DECR_CTOP (-actuals_num);
  (*ifunc) (actuals_num);
  DECR_CTOP (actuals_num);
  INCREMENT_PC ();
}


/* A general function processing tail (if TAIL_FLAG) fun/thread/class
   call of fun with ACTUALS_NUM par maters starting with
   CALL_START.  */
void
process_fun_class_call (ER_node_t call_start, int_t actuals_num, int tail_flag)
{
  BC_node_t code;
  implementation_fun_t ifunc;
  ER_node_t fun_class_val, context;
  ER_node_t instance;
  BC_node_mode_t mode;

  ctop = IVAL (call_start, -1);
  if (ER_NODE_MODE (call_start) == ER_NM_efun)
    {
      d_assert (! tail_flag);
      DECR_CTOP (-actuals_num - 1);
      call_external_fun (actuals_num, ER_efdecl (call_start));
      TOP_DOWN;
      return;
    }
  if (ER_NODE_MODE (call_start) != ER_NM_code)
    eval_error (callop_bc_decl, get_cpos (),
		DERR_none_class_or_fun_before_left_bracket);
  code = ID_TO_CODE (ER_code_id (call_start));
  mode = BC_NODE_MODE (code);
  if ((ifunc = BC_implementation_fun (code)) != NULL)
    {
      fun_result = IVAL (ctop, 1);
      DECR_CTOP (-actuals_num - 1);
      (*ifunc) (actuals_num);
      DECR_CTOP (actuals_num + 1);
      INCREMENT_PC ();
    }
  else if (BC_class_p (code))
    {
      instance = create_class_stack ((val_t *) call_start, actuals_num,
				     BC_simple_p (code));
      if (BC_simple_p (code))
	{
	  TOP_UP;
	  ER_SET_MODE (ctop, ER_NM_stack);
	  ER_set_stack (ctop, instance);
	  TOP_DOWN;
	  INCREMENT_PC ();
	}
      else
	cpc = get_fblock_pc (code);
    }
  else
    {
      context = ER_code_context (call_start);
      process_fun_call ((val_t *) call_start + 1, code, context,
			 actuals_num, tail_flag);
    }
}

void
initiate_funcs (void)
{
  if (trace_flag)
    VLO_CREATE (trace_stack, 0);
  initiate_io ();
  initiate_regex_tab ();
#ifdef FLOATING_NAN
  floating_nan = FLOATING_NAN;
  minus_floating_nan = -FLOATING_NAN;
#endif
}

void
finish_funcs (void)
{
  finish_regex_tab ();
  finish_io ();
  if (trace_flag)
    VLO_DELETE (trace_stack);
}



#include "earley.h"

/* This page contains interface to earley parser.  See file
   `d_ir.sprut' for details in interface. */

/* The following function implements function set_grammar in class
   parser. */
void
int_earley_parse_grammar (int npars)
{
  struct grammar *g;
  int code;
  ER_node_t par1, par2, par3, v;
  const char *name = "set_grammar";

  par1 = IVAL (ctop, -2);
  implicit_int_conversion (below_ctop, NULL);
  par2 = below_ctop;
  par3 = IVAL (ctop, 0);
  d_assert (npars == 3 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par3) == ER_NM_vect)
    {
      v = ER_vect (par3);
      GO_THROUGH_REDIR (v);
      if (ER_NODE_MODE (v) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (v);
      ER_set_vect (par3, v);
    }
  if (ER_NODE_MODE (par2) != ER_NM_int || ER_NODE_MODE (par3) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (par3)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (par3)) != ER_NM_char)
    eval_error (partype_bc_decl, get_pos (real_fun_call_pc),
		DERR_parameter_type, name);
  g = (struct grammar *) ER_hide (par1);
  code = earley_parse_grammar (g, ER_i (par2), ER_pack_els (ER_vect (par3)));
  if (code == EARLEY_NO_MEMORY)
    eval_error (pmemory_bc_decl, get_pos (real_fun_call_pc),
		"run time error (%s) -- no parser memory", name);
  else if (code != 0)
    eval_error (invgrammar_bc_decl, get_pos (real_fun_call_pc),
		"run time error (%s) -- %s", name, earley_error_message (g));
  /* Returned value should be ignored. */
  ER_SET_MODE (fun_result, ER_NM_undef);
}

/* The following function implements function set_debug in class
   parser. */
void
int_earley_set_debug_level (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_debug";

  par1 = IVAL (ctop, -1);
  implicit_int_conversion (ctop, NULL);
  par2 = IVAL (ctop, 0);
  d_assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_bc_decl, get_pos (real_fun_call_pc),
		DERR_parameter_type, name);
  i = earley_set_debug_level ((struct grammar *) ER_hide (par1),
			      ER_i (par2));
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, i);
}

/* The following function implements function set_one_parse in class
   parser. */
void
int_earley_set_one_parse_flag (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_one_parse";

  par1 = IVAL (ctop, -1);
  implicit_int_conversion (ctop, NULL);
  par2 = IVAL (ctop, 0);
  d_assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_bc_decl, get_pos (real_fun_call_pc),
		DERR_parameter_type, name);
  i = earley_set_one_parse_flag ((struct grammar *) ER_hide (par1),
				 ER_i (par2));
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, i);
}

/* The following function implements function set_lookahead in class
   parser. */
void
int_earley_set_lookahead_level (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_lookahead";

  par1 = IVAL (ctop, -1);
  implicit_int_conversion (ctop, NULL);
  par2 = IVAL (ctop, 0);
  d_assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_bc_decl, get_pos (real_fun_call_pc),
		DERR_parameter_type, name);
  i = ER_i (par2);
  i = earley_set_lookahead_level ((struct grammar *) ER_hide (par1),
				  i ? 1 : 0);
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, i);
}

/* The following function implements function set_cost in class
   parser. */
void
int_earley_set_cost_flag (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_cost";

  par1 = IVAL (ctop, -1);
  implicit_int_conversion (ctop, NULL);
  par2 = IVAL (ctop, 0);
  d_assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_bc_decl, get_pos (real_fun_call_pc),
		DERR_parameter_type, name);
  i = earley_set_cost_flag ((struct grammar *) ER_hide (par1), ER_i (par2));
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, i);
}

/* The following function implements function set_recovery in class
   parser. */
void
int_earley_set_error_recovery_flag (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_recovery";

  par1 = IVAL (ctop, -1);
  implicit_int_conversion (ctop, NULL);
  par2 = IVAL (ctop, 0);
  d_assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_bc_decl, get_pos (real_fun_call_pc),
		DERR_parameter_type, name);
  i = earley_set_error_recovery_flag ((struct grammar *)
				      ER_hide (par1),
				      ER_i (par2));
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, i);
}

/* The following function implements function set_recovery_match in
   class parser. */
void
int_earley_set_recovery_match (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_recovery_match";

  par1 = IVAL (ctop, -1);
  implicit_int_conversion (ctop, NULL);
  par2 = IVAL (ctop, 0);
  d_assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_bc_decl, get_pos (real_fun_call_pc),
		DERR_parameter_type, name);
  i = earley_set_recovery_match ((struct grammar *) ER_hide (par1),
				 ER_i (par2));
  ER_SET_MODE (fun_result, ER_NM_int);
  ER_set_i (fun_result, i);
}

/* The following contains parse tree nodes before they will be placed
   into the heap. */
static os_t tree_mem_os;

/* The following variables are vector of tokens and number of the
   current token to read. */
static ER_node_t tokens_vect;
static int curr_token;

/* The following function produces token to earley_parse. */
static int
init_read_token (void **attr)
{
  ER_node_t tok, code;
  BC_node_t decl;
  int n;
  const char *message;
  val_t t, t2;

  d_assert (ER_NODE_MODE (tokens_vect) == ER_NM_heap_pack_vect
	    && ER_pack_vect_el_type (tokens_vect) == ER_NM_stack);
  if ((unsigned_int_t) curr_token >= ER_els_number (tokens_vect))
    return -1;
  tok = *attr = ((ER_node_t *) ER_pack_els (tokens_vect)) [curr_token];
  if (! ER_IS_OF_TYPE (tok, ER_NM_heap_stack))
    ER_SET_MODE ((ER_node_t) &t, ER_NM_nil);
  else
    {
      ER_SET_MODE ((ER_node_t) &t, ER_NM_stack);
      ER_set_stack ((ER_node_t) &t, tok);
    }
  ER_SET_MODE ((ER_node_t) &t2, ER_NM_code);
  ER_set_code_id ((ER_node_t) &t2, CODE_ID (token_bc_decl));
  if (! internal_isa_call (&message, (ER_node_t) &t2, (ER_node_t) &t))
    eval_error (invtoken_bc_decl, get_pos (real_fun_call_pc),
		"run time error (parse) -- invalid token #%d", curr_token);
  curr_token++;
  decl = get_another_block_decl (ER_block_node (tok), code_bc_decl);
  d_assert (BC_NODE_MODE (decl) == BC_NM_vdecl);
  n = BC_var_num (decl);
  code = IVAL (ER_stack_vars (tok), n);
  if (ER_NODE_MODE (code) != ER_NM_int)
    eval_error (invtoken_bc_decl, get_pos (real_fun_call_pc),
		"run time error (parse) -- invalid code of token #%d",
		curr_token - 1);
  return ER_i (code);
}

/* The following is DINO error function called by parser and its
   context. */
static BC_node_t error_fun_block;
static ER_node_t error_fun_block_context;

/* The following function is interface to DINO error function.  We
   need to provide at least 6 temporary variables (see trick for this
   in environment) for this function plus 4 vars for
   int_earley_parse. */
static void
init_syntax_token (int err_tok_num, void *err_tok_attr,
		   int start_ignored_tok_num, void *start_ignored_tok_attr,
		   int start_recovered_tok_num, void *start_recovered_tok_attr)
{
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, err_tok_num);
  TOP_UP;
  if (err_tok_attr == NULL)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      ER_SET_MODE (ctop, ER_NM_stack);
      ER_set_stack (ctop, err_tok_attr);
    }
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, start_ignored_tok_num);
  TOP_UP;
  if (start_ignored_tok_attr == NULL)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      ER_SET_MODE (ctop, ER_NM_stack);
      ER_set_stack (ctop, start_ignored_tok_attr);
    }
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, start_recovered_tok_num);
  TOP_UP;
  if (start_recovered_tok_attr == NULL)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      ER_SET_MODE (ctop, ER_NM_stack);
      ER_set_stack (ctop, start_recovered_tok_attr);
    }
  call_fun_class (error_fun_block, error_fun_block_context, 6);
}

struct tree_heap_node
{
  struct earley_tree_node *tree_node;
  ER_node_t heap_node;
};

static struct tree_heap_node temp_tree_heap_node;
static hash_table_t tree_heap_tab;

/* Hash of the node. */
static unsigned
tree_heap_node_hash (hash_table_entry_t n)
{
  struct tree_heap_node *node = ((struct tree_heap_node *) n);

  return (size_t) node->tree_node;
}

/* Equality of nodes. */
static int
tree_heap_node_eq (hash_table_entry_t n1, hash_table_entry_t n2)
{
  struct tree_heap_node *node1 = ((struct tree_heap_node *) n1);
  struct tree_heap_node *node2 = ((struct tree_heap_node *) n2);

  return node1->tree_node == node2->tree_node;
}

/* The following function places abstract tree into heap and returns
   the result. */
static ER_node_t
tree_to_heap (struct earley_tree_node *root)
{
  hash_table_entry_t *entry;
  ER_node_t var, res, vect, name_vect;
  struct earley_tree_node *node, *alt;
  struct tree_heap_node *tree_heap_node;
  int i;

  tree_heap_node = &temp_tree_heap_node;
  tree_heap_node->tree_node = root;
  entry = find_hash_table_entry (tree_heap_tab, tree_heap_node, TRUE);
  if (*entry != NULL)
    return ((struct tree_heap_node *) *entry)->heap_node;
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_code);
  ER_set_code_id (ctop, CODE_ID (anode_bc_decl));
  ER_set_code_context (ctop, uppest_stack);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_vect);
  switch (root->type)
    {
    case EARLEY_NIL:
    case EARLEY_ERROR:
      var = IVAL (ER_stack_vars (uppest_stack),
		  BC_var_num (root->type == EARLEY_NIL
			      ? nil_anode_bc_decl : error_anode_bc_decl));
      d_assert (ER_NODE_MODE (var) == ER_NM_stack);
      res = ER_stack (var);
      DECR_CTOP (2);
      break;
    case EARLEY_TERM:
      name_vect = create_string ("$term");
      set_vect_dim (ctop, name_vect, 0);
      TOP_UP;
      ER_SET_MODE (ctop, ER_NM_stack);
      d_assert (ER_NODE_MODE ((ER_node_t) root->val.term.attr)
		== ER_NM_heap_stack);
      ER_set_stack (ctop, root->val.term.attr);
      DECR_CTOP (3);
      res = create_class_stack ((val_t *) IVAL (ctop, 1), 2, TRUE);
      break;
    case EARLEY_ANODE:
      name_vect = create_string (root->val.anode.name);
      set_vect_dim (ctop, name_vect, 0);
      for (i = 0; root->val.anode.children [i] != NULL; i++)
	;
      vect = create_empty_vector ();
      ER_set_pack_vect_el_type (vect, ER_NM_stack);
      vect = expand_vector (vect, i);
      TOP_UP;
      ER_SET_MODE (ctop, ER_NM_vect);
      set_vect_dim (ctop, vect, 0);
      DECR_CTOP (3);
      res = create_class_stack ((val_t *) IVAL (ctop, 1), 2, TRUE);
      break;
    case EARLEY_ALT:
      name_vect = create_string ("$alt");
      set_vect_dim (ctop, name_vect, 0);
      for (i = 0, alt = root; alt != NULL; alt = alt->val.alt.next, i++)
	;
      vect = create_empty_vector ();
      ER_set_pack_vect_el_type (vect, ER_NM_stack);
      vect = expand_vector (vect, i);
      TOP_UP;
      ER_SET_MODE (ctop, ER_NM_vect);
      set_vect_dim (ctop, vect, 0);
      DECR_CTOP (3);
      res = create_class_stack ((val_t *) IVAL (ctop, 1), 2, TRUE);
      break;
    default:
      d_unreachable ();
    }
  OS_TOP_EXPAND (tree_mem_os, sizeof (struct tree_heap_node));
  tree_heap_node = (struct tree_heap_node *) OS_TOP_BEGIN (tree_mem_os);
  *entry = tree_heap_node;
  OS_TOP_FINISH (tree_mem_os);
  tree_heap_node->tree_node = root;
  tree_heap_node->heap_node = res;
  if (root->type == EARLEY_ANODE)
    {
      for (i = 0; (node = root->val.anode.children [i]) != NULL; i++)
	((ER_node_t *) ER_pack_els (vect)) [i] = tree_to_heap (node);
      ER_set_els_number (vect, i);
    }
  else if (root->type == EARLEY_ALT)
    {
      for (i = 0, alt = root; alt != NULL; alt = alt->val.alt.next, i++)
	((ER_node_t *) ER_pack_els (vect)) [i]
	  = tree_to_heap (alt->val.alt.node);
      ER_set_els_number (vect, i);
    }
  return res;
}

/* The following function allocates memory for the parse tree. */
static void *
int_parse_alloc (int nmemb)
{
  void *res;

  OS_TOP_EXPAND (tree_mem_os, nmemb);
  res = OS_TOP_BEGIN (tree_mem_os);
  OS_TOP_FINISH (tree_mem_os);
  return res;
}

/* The following function implements function parse in class
   parser. */
void
int_earley_parse (int npars)
{
  struct grammar *g;
  int code, ambiguous_p;
  struct earley_tree_node *root;
  ER_node_t par1, par2, par3, v;
  ER_node_t instance, var;
  const char *name = "parse";
  int fun_result_offset;

  par1 = IVAL (ctop, -2);
  par2 = IVAL (ctop, -1);
  par3 = IVAL (ctop, 0);
  d_assert (npars == 3 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) == ER_NM_vect)
    {
      v = ER_vect (par2);
      GO_THROUGH_REDIR (v);
      if (ER_NODE_MODE (v) == ER_NM_heap_unpack_vect)
	pack_vector_if_possible (v);
      ER_set_vect (par2, v);
    }
  if (ER_NODE_MODE (par2) != ER_NM_vect
      || (ER_NODE_MODE (ER_vect (par2))
	  != ER_NM_heap_pack_vect)
      || (ER_pack_vect_el_type (ER_vect (par2)) != ER_NM_stack)
      || ! fun_p (par3))
    eval_error (partype_bc_decl, get_pos (real_fun_call_pc),
		DERR_parameter_type, name);
  fun_result_offset = (val_t *) fun_result - (val_t *) cvars;
  /* We switch off GC because the parser may call error function
     several times and parser has references to tokens in the heap. */
  tokens_vect = ER_vect (par2);
  curr_token = 0;
  error_fun_block = ID_TO_CODE (ER_code_id (par3));
  error_fun_block_context = ER_code_context (par3);
  g = (struct grammar *) ER_hide (par1);
  OS_CREATE (tree_mem_os, 0);
  tree_heap_tab = create_hash_table (2 * ER_els_number (tokens_vect)
				     * sizeof (struct tree_heap_node *),
				     tree_heap_node_hash, tree_heap_node_eq);
  /* We need it because init_syntax_token may change it. */
  code = earley_parse (g, init_read_token, init_syntax_token,
		       int_parse_alloc, NULL, &root, &ambiguous_p);
  if (code == EARLEY_NO_MEMORY)
    {
      delete_hash_table (tree_heap_tab);
      OS_DELETE (tree_mem_os);
      eval_error (pmemory_bc_decl, get_pos (real_fun_call_pc),
		  "run time error (%s) -- no parser memory", name);
    }
  else if (code == EARLEY_UNDEFINED_OR_BAD_GRAMMAR)
    {
      delete_hash_table (tree_heap_tab);
      OS_DELETE (tree_mem_os);
      eval_error (invgrammar_bc_decl, get_pos (real_fun_call_pc),
		  "run time error (%s) -- %s", name, earley_error_message (g));
    }
  else if (code == EARLEY_INVALID_TOKEN_CODE)
    {
      delete_hash_table (tree_heap_tab);
      OS_DELETE (tree_mem_os);
      eval_error (invtoken_bc_decl, get_pos (real_fun_call_pc),
		  "run time error (%s) -- %s", name, earley_error_message (g));
    }
  else
    d_assert (code == 0);
  /* Set up ambiguous_p. */
  instance = ER_context (cstack);
  d_assert (instance != NULL && ER_NODE_MODE (instance) == ER_NM_heap_stack
	    && ER_stack_block (instance) == parser_bc_decl); /* ??? */
  var = IVAL (ER_stack_vars (instance), BC_var_num (ambiguous_p_bc_decl)); /* ??? */
  ER_SET_MODE (var, ER_NM_int);
  ER_set_i (var, ambiguous_p);
  fun_result = IVAL (cvars, fun_result_offset);
  if (root == NULL)
    ER_SET_MODE (fun_result, ER_NM_nil);
  else
    {
      /* Translation into heap: */
      instance = tree_to_heap (root);
      d_assert (ER_NODE_MODE (instance) == ER_NM_heap_stack);
      ER_SET_MODE (fun_result, ER_NM_stack);
      ER_set_stack (fun_result, instance);
    }
  delete_hash_table (tree_heap_tab);
  OS_DELETE (tree_mem_os);
}

/* The following function is used to initiate class parser. */
void
int_earley_create_grammar (int npars)
{
  struct grammar *g;

  d_assert (npars == 0);
  g = earley_create_grammar ();
  if (g == NULL)
    eval_error (pmemory_bc_decl, get_pos (real_fun_call_pc),
		"run time error (parser) -- no parser memory");
  ER_SET_MODE (fun_result, ER_NM_hide);
  ER_set_hide (fun_result, g);
}
