#define MALLOC(result, size) (result) = allocate::malloc (size)
#define FREE(mem) allocate::free (mem)
#define change_allocation_error_function(func) \
        allocate::change_error_function(func)

#define VLO_CREATE(v, len) (v) = new vlo (len)
#define VLO_DELETE(vlo) delete vlo
#define VLO_LENGTH(vlo) (vlo)->length ()
#define VLO_BEGIN(vlo) (vlo)->begin ()
#define VLO_BOUND(vlo) (vlo)->bound ()
#define VLO_ADD_MEMORY(vlo, addr, size) (vlo)->add_memory (addr, size)
#define VLO_EXPAND(vlo, size) (vlo)->expand (size)
#define VLO_SHORTEN(vlo, size) (vlo)->shorten (size)
#define VLO_NULLIFY(vlo) (vlo)->nullify ()

#define OS_CREATE(o, len) o = new os (len)
#define OS_EMPTY(os) (os)->empty ()
#define OS_DELETE(os) delete os
#define OS_TOP_BEGIN(os) (os)->top_begin ()
#define OS_TOP_LENGTH(os) (os)->top_length ()
#define OS_TOP_ADD_MEMORY(os, addr, size) (os)->top_add_memory (addr, size)
#define OS_TOP_ADD_STRING(os, str) (os)->top_add_string (str)
#define OS_TOP_ADD_BYTE(os, b) (os)->top_add_byte (b)
#define OS_TOP_FINISH(os) (os)->top_finish ()
#define OS_TOP_EXPAND(os, size) (os)->top_expand (size)
#define OS_TOP_SHORTEN(os, size) (os)->top_shorten (size)
#define OS_TOP_NULLIFY(os) (os)->top_nullify ()

#define create_hash_table(size, hash, eq) new hash_table (size, hash, eq)
#define empty_hash_table(tab) (tab)->empty ()
#define delete_hash_table(tab) delete tab
#define find_hash_table_entry(tab, el, res_p) (tab)->find_entry(el, res_p)

/* Forward declarations: */
static void use_functions (int argc, char **argv);
static void use_description (int argc, char **argv);

#include "earley.c"

earley::earley (void)
{
  this->grammar = earley_create_grammar ();
}

earley::~earley (void)
{
  earley_free_grammar (this->grammar);
}

int
earley::error_code (void)
{
  return earley_error_code (this->grammar);
}

const char *
earley::error_message (void)
{
  return earley_error_message (this->grammar);
}

int
earley::read_grammar (int strict_p,
		      const char *(*read_terminal) (int *code),
		      const char *(*read_rule) (const char ***rhs,
						const char **abs_node,
						int **transl))
{
  return earley_read_grammar (this->grammar, strict_p,
			      read_terminal, read_rule);
}

int
earley::parse_grammar (int strict_p, const char *description)
{
  return earley_parse_grammar (this->grammar, strict_p, description);
}

int
earley::set_lookahead_level (int level)
{
  return earley_set_lookahead_level (this->grammar, level);
}

int earley::set_debug_level (int level)
{
  return earley_set_debug_level (this->grammar, level);
}

int earley::set_one_parse_flag (int flag)
{
  return earley_set_one_parse_flag (this->grammar, flag);
}

int earley::set_error_recovery_flag (int flag)
{
  return earley_set_error_recovery_flag (this->grammar, flag);
}

int earley::set_recovery_match (int n_toks)
{
  return earley_set_recovery_match (this->grammar, n_toks);
}

int
earley::parse (int (*read_token) (void **attr),
	       void (*syntax_error) (int err_tok_num,
				     void *err_tok_attr,
				     int start_ignored_tok_num,
				     void *start_ignored_tok_attr,
				     int start_recovered_tok_num,
				     void *start_recovered_tok_attr),
		     void *(*parse_alloc) (int nmemb),
	       struct earley_tree_node **root,
	       int *ambiguous_p)
{
  return earley_parse (this->grammar, read_token,
		       syntax_error, parse_alloc, root, ambiguous_p);
}


#ifdef EARLEY_TEST

/* The following two functions calls earley parser with two different
   ways of forming grammars. */
static void
use_functions (int argc, char **argv)
{
  earley *e;
  struct earley_tree_node *root;
  int ambiguous_p;

  nterm = nrule = 0;
  OS_CREATE (mem_os, 0);
  fprintf (stderr, "Use functions\n");
  e = new earley ();
  if (e == NULL)
    {
      fprintf (stderr, "earley::earley: No memory\n");
      OS_DELETE (mem_os);
      exit (1);
    }
  e->set_one_parse_flag (FALSE);
  if (argc > 1)
    e->set_lookahead_level (atoi (argv [1]));
  if (argc > 2)
    e->set_debug_level (atoi (argv [2]));
  else
    e->set_debug_level (3);
  if (argc > 3)
    e->set_error_recovery_flag (atoi (argv [3]));
  if (argc > 4)
    e->set_one_parse_flag (atoi (argv [4]));
  if (e->read_grammar (TRUE, read_terminal, read_rule) != 0)
    {
      fprintf (stderr, "%s\n", e->error_message ());
      OS_DELETE (mem_os);
      exit (1);
    }
  ntok = 0;
  if (e->parse (test_read_token, test_syntax_error, test_parse_alloc,
		    &root, &ambiguous_p))
    fprintf (stderr, "earley::parse: %s\n", e->error_message ());
  delete e;
  OS_DELETE (mem_os);
}

static void
use_description (int argc, char **argv)
{
  earley *e;
  struct earley_tree_node *root;
  int ambiguous_p;

  fprintf (stderr, "Use description\n");
  OS_CREATE (mem_os, 0);
  e = new earley ();
  if (e == NULL)
    {
      fprintf (stderr, "earley::earley: No memory\n");
      OS_DELETE (mem_os);
      exit (1);
    }
  e->set_one_parse_flag (FALSE);
  if (argc > 1)
    e->set_lookahead_level (atoi (argv [1]));
  if (argc > 2)
    e->set_debug_level (atoi (argv [2]));
  else
    e->set_debug_level (3);
  if (argc > 3)
    e->set_error_recovery_flag (atoi (argv [3]));
  if (argc > 4)
    e->set_one_parse_flag (atoi (argv [4]));
  if (e->parse_grammar (TRUE, description) != 0)
    {
      fprintf (stderr, "%s\n", e->error_message ());
      OS_DELETE (mem_os);
      exit (1);
    }
  if (e->parse (test_read_token, test_syntax_error, test_parse_alloc,
		&root, &ambiguous_p))
    fprintf (stderr, "earley::parse: %s\n", e->error_message ());
  delete e;
  OS_DELETE (mem_os);
}

#endif /* #ifdef EARLEY_TEST */
