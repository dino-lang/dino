#include "d_run.h"
#include "d_built.h"
#include "d_conv.h"
#include "d_func.h"

#include <sys/types.h>
#include <sys/stat.h>

#ifndef WIN32

#include <dirent.h>
#include <pwd.h>
#include <grp.h>

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

#else /* #ifndef WIN32 */

#include <io.h>
#ifndef S_ISREG
#define S_ISREG(m) ((m) & _S_IFREG)
#endif

#ifndef S_ISDIR
#define S_ISDIR(m) ((m) & _S_IFDIR)
#endif

#ifndef S_ISCHR
#define S_ISCHR(m) ((m) & _S_IFCHR)
#endif

#ifndef S_ISFIFO
#define S_ISFIFO(m) ((m) & _S_IFIFO)
#endif

#define popen(cmd,mode) _popen (cmd,mode)
#define pclose(f) _pclose (f)
#define getpid() _getpid()

#define geteun() getun ()
#define getegn() getgn ()

/* It needs something else for Win NT. */
#define getun() "Unknown"
#define getgn() "Unknown"

#define mkdir(p)			_mkdir (p)
#define rmdir(dir)			_rmdir (dir)
#define stat(path, buffer)		_stat (path, buffer)
#define fstat(f, buffer)		_fstat (f, buffer)
#define chmod(fn, m)			_chmod (fn, m)
#define getcwd(p, s)			_getcwd (p, s)
#define chdir(p)			_chdir (p)
#define fileno(f)			_fileno (f)
#define isatty(h)			_isatty (h)

#ifndef S_IREAD
#define S_IREAD _S_IREAD
#endif

#ifndef S_IWRITE
#define S_IWRITE _S_IWRITE
#endif

#define S_IXUSR 0
#define S_ISVTX 0
#define S_IRGRP 0
#define S_IWGRP 0
#define S_IXGRP 0
#define S_IROTH 0
#define S_IWOTH 0
#define S_IXOTH 0

struct dirent
{
  long d_ino;	           /* Always zero. */
  unsigned short d_reclen; /* Always zero. */
  unsigned short d_namlen; /* Length of name in d_name. */
  char*	d_name;	           /* File name. */
  /* NOTE: The name in the dirent structure points to the name in the
           finddata_t structure in the DIR. */
};

/* Win32 implemenation */
typedef struct
{
  /* disk transfer area for this dir */
  struct _finddata_t dd_dta;
  /* dirent struct to return from dir (NOTE: this makes this thread
     safe as long as only one thread uses a particular DIR struct at a
     time). */
  struct dirent	dd_dir;
  /* _findnext handle */
  long dd_handle;
  /*
     Status of search:
       0 = not started yet (next entry to read is first entry)
      -1 = off the end
       positive = 0 based index of next entry
   */
  short	dd_stat;
  /* given path for dir with search pattern (struct is extended) */
  char dd_name[1];
} DIR;

/* WIN32 implementation. */
static DIR*
opendir (const char* dirname)
{
  DIR* dir;
  struct stat dirstat;
  
  errno = 0;
  if (*dirname == '\0')
    {
      errno = ENOTDIR;
      return NULL;
    }
  /* Attempt to determine if the given path really is a directory. */
  if (_stat (dirname, &dirstat))
    /* Stat set already an error value. */
    return NULL;
  if (!S_ISDIR (dirstat.st_mode))
    {
      errno = ENOTDIR;
      return NULL;
    }
  dir = (DIR*) malloc (sizeof (DIR) + strlen (dirname) + 2);
  if (!dir)
    {
      errno = ENOMEM;
      return NULL;
    }
  /* Make pattern. */
  strcpy (dir->dd_name, dirname);
  if (dir->dd_name[strlen (dir->dd_name) - 1] != '/' &&
      dir->dd_name[strlen (dir->dd_name) - 1] != '\\')
    strcat (dir->dd_name, "\\");
  strcat (dir->dd_name, "*");
  /* Initialize handle to -1 so that a premature closedir doesn't try
     to call _findclose on it. */
  dir->dd_handle = -1;
  dir->dd_stat = 0;
  /* Initialize the dirent structure. ino and reclen are invalid under
     Win32, and name simply points at the appropriate part of the
     findfirst_t structure. */
  dir->dd_dir.d_ino = 0;
  dir->dd_dir.d_reclen = 0;
  dir->dd_dir.d_namlen = 0;
  dir->dd_dir.d_name = dir->dd_dta.name;
  return dir;
}

/* Win32 implementation. */
static struct dirent *
readdir (DIR *dir)
{
  errno = 0;
  /* Check dir correctness. */
  if (!dir)
    {
      errno = EFAULT;
      return NULL;
    }
  if (dir->dd_dir.d_name != dir->dd_dta.name)
    {
      errno = EINVAL;
      return NULL;
    }
  if (dir->dd_stat < 0)
    /* We have already returned all files in the directory
       (or the structure has an invalid dd_stat). */
    return NULL;
  else if (dir->dd_stat == 0)
    {
      /* We haven't started the search yet.  Start the search. */
      dir->dd_handle = _findfirst (dir->dd_name, &dir->dd_dta);
      
      if (dir->dd_handle == -1)
	/* There are no files in the directory. */
	dir->dd_stat = -1;
      else
	dir->dd_stat = 1;
    }
  else
    {
      /* Get the next search entry. */
      if (_findnext (dir->dd_handle, &dir->dd_dta))
	{
	  /* We are off the end or otherwise error. */
	  errno = 0; /* Clear flag after _findnext. */
	  _findclose (dir->dd_handle);
	  dir->dd_handle = -1;
	  dir->dd_stat = -1;
	}
      else
	/* Update the status to indicate the correct number. */
	dir->dd_stat++;
    }
  if (dir->dd_stat > 0)
    {
      /* Successfully got an entry. Everything about the file is
	 already appropriately filled in except the length of the file
	 name. */
      dir->dd_dir.d_namlen = strlen (dir->dd_dir.d_name);
      return &dir->dd_dir;
    }
  return NULL;
}


/* WIN32 implementation. */
static int
closedir (DIR* dir)
{
  int rc;
  
  errno = 0;
  rc = 0;
  if (!dir)
    {
      errno = EFAULT;
      return -1;
    }
  if (dir->dd_handle != -1)
    rc = _findclose(dir->dd_handle);
  free (dir);
  return rc;
}

#endif /* #ifndef WIN32 */

#ifdef HAVE_TIME_H
#include <time.h>
#else
extern clock_t clock (void);
#endif

#ifdef AIX_DLOPEN
#include "d_aixdl.c"
#endif

#include "regex.h"

static ER_node_t create_instance (int_t pars_number);


static void
min_max_call (int_t pars_number, int min_flag)
{
  ER_node_t res;
  ER_node_t val;
  int_t i;

  if (pars_number < 2)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, (min_flag ? MIN_NAME : MAX_NAME));
  for (i = 0; i < pars_number; i++)
    {
      implicit_arithmetic_conversion (i);
      val = INDEXED_VAL (ER_CTOP (), -i);
      if (ER_NODE_MODE (val) != ER_NM_int && ER_NODE_MODE (val) != ER_NM_float)
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		    DERR_parameter_type, (min_flag ? MIN_NAME : MAX_NAME));
      if (i == 0)
	res = val;
      else if (ER_NODE_MODE (val) == ER_NM_int
	       && ER_NODE_MODE (res) == ER_NM_int
	       && (ER_i (val) < ER_i (res)) == min_flag)
	res = val;
      else if (ER_NODE_MODE (val) == ER_NM_float
	       && ER_NODE_MODE (res) == ER_NM_int
	       && (ER_f (val) < ER_i (res)) == min_flag)
	res = val;
      else if (ER_NODE_MODE (val) == ER_NM_int
	       && ER_NODE_MODE (res) == ER_NM_float
	       && (ER_i (val) < ER_f (res)) == min_flag)
	res = val;
      else if (ER_NODE_MODE (val) == ER_NM_float
	       && ER_NODE_MODE (res) == ER_NM_float
	       && (ER_f (val) < ER_f (res)) == min_flag)
	res = val;
    }
  *(val_t *) INDEXED_VAL (ER_CTOP (), -pars_number) = *(val_t *) res;
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, name);
  to_vect_string_conversion (ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, name);
  len = strlen (ER_pack_els (ER_vect (ctop)));
  vect = create_empty_string (len + 1);
  strcpy (ER_pack_els (vect), ER_pack_els (ER_vect (ctop)));
  ER_set_els_number (vect, len);
  for (str = ER_pack_els (vect); *str != 0; str++)
    if (isalpha (*str))
      *str = (lower_flag ? tolower (*str) : toupper (*str));
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
}

void
tolower_call (int_t pars_number)
{
  to_lower_upper (pars_number, TRUE);
}

void
toupper_call (int_t pars_number)
{
  to_lower_upper (pars_number, FALSE);
}

void
eltype_call (int_t pars_number)
{
  ER_node_t vect;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, ELTYPE_NAME);
  if (ER_NODE_MODE (ctop) != ER_NM_vect)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, ELTYPE_NAME);
  vect = ER_vect (ctop);
  GO_THROUGH_REDIR (vect);
  if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
    pack_vector_if_possible (vect);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  if (ER_NODE_MODE (vect) != ER_NM_heap_pack_vect)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      ER_SET_MODE (ctop, ER_NM_type);
      ER_set_type (ctop, node_mode_2_type (ER_pack_vect_el_type (vect)));
    }
  INCREMENT_PC();
}

void
keys_call (int_t pars_number)
{
  ER_node_t tab;
  ER_node_t vect;
  size_t i, index;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, KEYS_NAME);
  if (ER_NODE_MODE (ctop) != ER_NM_tab)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, KEYS_NAME);
  tab = ER_tab (ctop);
  GO_THROUGH_REDIR (tab);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  index = 0;
  PUSH_TEMP_REF (tab);
  if (ER_els_number (tab) == 0)
    vect = create_empty_vector ();
  else
    vect = create_unpack_vector (ER_els_number (tab));
  tab = GET_TEMP_REF (0);
  POP_TEMP_REF (1);
  for (i = 0; i < ER_entries_number (tab); i++)
    if (ER_NODE_MODE (INDEXED_ENTRY_KEY (ER_tab_els (tab), i))
	!= ER_NM_empty_entry
	&& (ER_NODE_MODE (INDEXED_ENTRY_KEY (ER_tab_els (tab), i))
	    != ER_NM_deleted_entry))
      {
	*(val_t *) INDEXED_VAL (ER_unpack_els (vect), index)
	  = *(val_t *) INDEXED_ENTRY_KEY (ER_tab_els (tab), i);
	index++;
      }
  if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
    pack_vector_if_possible (vect);
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
}

void
context_call (int_t pars_number)
{
  ER_node_t val;
  ER_node_t context;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, CONTEXT_NAME);
  val = INDEXED_VAL (ER_CTOP (), -pars_number + 1);
  if (ER_NODE_MODE (val) == ER_NM_instance)
    context = ER_context (ER_instance (val));
  else if (ER_NODE_MODE (val) == ER_NM_stack)
    context = ER_context (ER_stack (val));
  else if (ER_NODE_MODE (val) == ER_NM_func)
    context = ER_func_context (val);
  else if (ER_NODE_MODE (val) == ER_NM_class)
    context = ER_class_context (val);
  else if (ER_NODE_MODE (val) == ER_NM_process)
    context = ER_context (ER_process (val));
  else
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, CONTEXT_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  if (context == NULL)
    ER_SET_MODE (ctop, ER_NM_nil);
  else if (ER_NODE_MODE (context) == ER_NM_heap_stack)
    {
      ER_SET_MODE (ctop, ER_NM_stack);
      ER_set_stack (ctop, context);
    }
  else
    {
      assert (ER_NODE_MODE (context) == ER_NM_heap_instance);
      ER_SET_MODE (ctop, ER_NM_instance);
      ER_set_instance (ctop, context);
    }
  INCREMENT_PC();
}

int
internal_inside_call (const char **message_ptr, int context_flag)
{
  ER_node_t block_context;
  ER_node_t func_class_2_context;
  IR_node_t func_class;
  IR_node_t func_class_2;
  IR_node_t block;
  int result;

  *message_ptr = NULL;
  func_class = NULL;
  if (ER_NODE_MODE (below_ctop) == ER_NM_instance)
    {
      block_context = ER_instance (below_ctop);
      block = ER_block_node (block_context);
    }
  else if (ER_NODE_MODE (below_ctop) == ER_NM_stack)
    {
      block_context = ER_stack (below_ctop);
      block = ER_block_node (block_context);
    }
  else if (ER_NODE_MODE (below_ctop) == ER_NM_func)
    {
      func_class = ER_func (below_ctop);
      block_context = ER_func_context (below_ctop);
    }
  else if (ER_NODE_MODE (below_ctop) == ER_NM_class)
    {
      func_class = ER_class (below_ctop);
      block_context = ER_class_context (below_ctop);
    }
  else
    {
      *message_ptr = DERR_parameter_type;
      return 0;
    }
  if (ER_IS_OF_TYPE (ctop, ER_NM_class))
    {
      func_class_2 = ER_class (ctop);
      func_class_2_context = ER_class_context (ctop);
    }
  else if (ER_IS_OF_TYPE (ctop, ER_NM_func))
    {
      func_class_2 = ER_func (ctop);
      func_class_2_context = ER_func_context (ctop);
    }
  else
    {
      *message_ptr = DERR_parameter_type;
      return 0;
    }
  result = (func_class == func_class_2
	    && (!context_flag || block_context == func_class_2_context));
  if (func_class != NULL)
    block = IR_scope (func_class);
  for (; !result && block != NULL;
       block = IR_scope (block), block_context = ER_context (block_context))
    if (IR_func_class_ext (block) == func_class_2
	&& (!context_flag
	    || ER_context (block_context) == func_class_2_context))
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, INSIDE_NAME);
  flag = 0;
  if (pars_number == 3)
    {
      implicit_int_conversion (0);
      if (!ER_IS_OF_TYPE (ctop, ER_NM_int))
	eval_error (partype_decl, invcalls_decl,
		    IR_pos (cpc), DERR_parameter_type, INSIDE_NAME);
      flag = ER_i (ctop);
      TOP_DOWN;
      pars_number--;
    }
  result = internal_inside_call (&message, flag);
  if (message != NULL)
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), message, INSIDE_NAME);
  /* Pop all remaining actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, result);
  INCREMENT_PC();
}

static void
process_regcomp_errors (int code, const char *function_name)
{
  if (code == REG_EBRACK)
    eval_error (ebrack_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_ebrack, function_name);
  else if (code == REG_ERANGE)
    eval_error (erange_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_erange, function_name);
  else if (code == REG_ECTYPE)
    eval_error (ectype_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_ectype, function_name);
  else if (code == REG_EPAREN)
    eval_error (eparen_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_eparen, function_name);
  else if (code == REG_ESUBREG)
    eval_error (esubreg_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_esubreg, function_name);
  else if (code == REG_EEND)
    eval_error (eend_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_eend, function_name);
  else if (code == REG_EESCAPE)
    eval_error (eescape_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_eescape, function_name);
  else if (code == REG_BADPAT || code == REG_BADRPT
	   || code == REG_BADBR || code == REG_EBRACE)
    /* We use badpat because I can not find badrpt, badbr, ebrace
       diagnostics for POSIX in GNU Regex. */
    eval_error (badpat_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_badpat, function_name);
  else if (code == REG_ESIZE)
    eval_error (esize_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_esize, function_name);
  else if (code == REG_ESPACE)
    eval_error (espace_decl, invregexps_decl, IR_pos (cpc),
		DERR_reg_espace, function_name);
  else
    /* Internall error: may be something else. */
    eval_error (internal_decl, invcalls_decl, IR_pos (cpc),
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
  regmatch_t *pmatch;
  ER_node_t vect, result;
  size_t els_number;
  size_t i;
  int code;

  if (pars_number != 2)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, MATCH_NAME);
  to_vect_string_conversion (ctop, NULL);
  to_vect_string_conversion (below_ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || ER_NODE_MODE (below_ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char)
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameter_type, MATCH_NAME);
  code = find_regex (ER_pack_els (ER_vect (below_ctop)), &reg);
  if (code != 0)
    process_regcomp_errors (code, MATCH_NAME);
  else
    {
      els_number = (reg->re_nsub + 1) * 2;
      /* Make pmatch vector. */
      VLO_NULLIFY (temp_vlobj);
      VLO_EXPAND (temp_vlobj, (reg->re_nsub + 1) * sizeof (regmatch_t));
      pmatch = VLO_BEGIN (temp_vlobj);
      if (!regexec (reg, ER_pack_els (ER_vect (ctop)), reg->re_nsub + 1,
		    pmatch, 0))
	{
	  /* Do not change size & packing. */
	  PUSH_TEMP_REF (ER_vect (ctop));
	  PUSH_TEMP_REF (ER_vect (below_ctop));
	  result = create_pack_vector (els_number, ER_NM_int);
	  POP_TEMP_REF (2);
	  for (i = 0; i < els_number; i += 2)
	    {
	      ((int_t *) ER_pack_els (result)) [i] = pmatch[i / 2].rm_so;
	      ((int_t *) ER_pack_els (result)) [i + 1] = pmatch[i / 2].rm_eo;
	    }
	}
      else
	result = NULL;
    }
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  if (result == NULL)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      ER_SET_MODE (ctop, ER_NM_vect);
      ER_set_vect (ctop, result);
    }
  INCREMENT_PC();
}

void
gmatch_call (int_t pars_number)
{
  regex_t *reg;
  regmatch_t *pmatch;
  ER_node_t vect, result;
  size_t els_number;
  int code, flag, count, disp;
  int_t el;
  const char *start;

  if (pars_number != 2 && pars_number != 3)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, GMATCH_NAME);
  flag = 0;
  if (pars_number == 3)
    {
      implicit_int_conversion (0);
      if (!ER_IS_OF_TYPE (ctop, ER_NM_int))
	eval_error (partype_decl, invcalls_decl,
		    IR_pos (cpc), DERR_parameter_type, GMATCH_NAME);
      flag = ER_i (ctop);
      TOP_DOWN;
      pars_number--;
    }
  to_vect_string_conversion (ctop, NULL);
  to_vect_string_conversion (below_ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || ER_NODE_MODE (below_ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char)
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameter_type, GMATCH_NAME);
  code = find_regex (ER_pack_els (ER_vect (below_ctop)), &reg);
  if (code != 0)
    process_regcomp_errors (code, GMATCH_NAME);
  /* Make vector which can store regmatch_t's. */
  VLO_NULLIFY (temp_vlobj);
  VLO_EXPAND (temp_vlobj, (reg->re_nsub + 1) * sizeof (regmatch_t));
  pmatch = (regmatch_t *) VLO_BEGIN (temp_vlobj);
  VLO_NULLIFY (temp_vlobj2);
  start = ER_pack_els (ER_vect (ctop));
  disp = 0;
  count = 0;
  while (!regexec (reg, start + disp, reg->re_nsub + 1, pmatch, 0))
    {
      el = pmatch[0].rm_so + disp;
      VLO_ADD_MEMORY (temp_vlobj2, &el, sizeof (el));
      el = pmatch[0].rm_eo + disp;
      VLO_ADD_MEMORY (temp_vlobj2, &el, sizeof (el));
      disp += (!flag ? pmatch[0].rm_eo : 1);
      count++;
    }
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  if (count == 0)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      result = create_pack_vector (2 * count, ER_NM_int);
      memcpy (ER_pack_els (result), VLO_BEGIN (temp_vlobj2),
	      2 * count * sizeof (el));
      ER_SET_MODE (ctop, ER_NM_vect);
      ER_set_vect (ctop, result);
    }
  INCREMENT_PC();
}

static void
generall_sub_call (int_t pars_number, int global_flag)
{
  regex_t *reg;
  regmatch_t pmatch [10];
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
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number,
		global_flag ? GSUB_NAME : SUB_NAME);
  to_vect_string_conversion (ctop, NULL);
  to_vect_string_conversion (below_ctop, NULL);
  regexp_val = INDEXED_VAL (ER_CTOP (), -2);
  to_vect_string_conversion (regexp_val, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || ER_NODE_MODE (below_ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char
      || ER_NODE_MODE (regexp_val) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (regexp_val)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (regexp_val)) != ER_NM_char)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, global_flag ? GSUB_NAME : SUB_NAME);
  code = find_regex (ER_pack_els (ER_vect (regexp_val)), &reg);
  if (code != 0)
    process_regcomp_errors (code, global_flag ? GSUB_NAME : SUB_NAME);
  else
    {
      vect = ER_vect (below_ctop);
      /* Count result string length. */
      start = 0;
      for (i = 0; i < 10; i++)
	sub_length [i] = 0;
      n_subst = 0;
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
      /* Do not change size & packing. */
      PUSH_TEMP_REF (vect);
      PUSH_TEMP_REF (ER_vect (ctop));
      result = create_pack_vector (evaluated_length, ER_NM_char);
      ER_set_els_number (result, 0);
      vect = GET_TEMP_REF (1);
      POP_TEMP_REF (2);
      /* Make actual substitution. */
      start = length = 0;
      dst = ER_pack_els (result);
      substitution = ER_pack_els (ER_vect (ctop));
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
      if (start < ER_els_number (vect))
	{
	  memcpy (dst, ER_pack_els (vect) + start,
		  ER_els_number (vect) - start);
	  length += ER_els_number (vect) - start;
	  dst += ER_els_number (vect) - start;
	}
      *dst = '\0';
      ER_set_els_number (result, length);
      assert (length == evaluated_length);
    }
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  if (result == NULL)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      ER_SET_MODE (ctop, ER_NM_vect);
      ER_set_vect (ctop, result);
    }
  INCREMENT_PC();
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
  regmatch_t pmatch [1];
  ER_node_t result;
  ER_node_t vect;
  ER_node_t sub_vect;
  size_t els_number;
  size_t chars_number;
  size_t el_size, start;
  const char *split_regex;
  ER_node_t split_var;
  int ch;
  int ok;
  int code;

  if (pars_number != 1 && pars_number != 2)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, SPLIT_NAME);
  if (pars_number != 1)
    to_vect_string_conversion (below_ctop, NULL);
  to_vect_string_conversion (ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || pars_number == 2 &&
      (ER_NODE_MODE (below_ctop) != ER_NM_vect
       || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
       || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char))
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, SPLIT_NAME);
  if (pars_number == 2)
    split_regex = ER_pack_els (ER_vect (ctop));
  else
    {
      split_var = INDEXED_VAL (ER_stack_vars (uppest_stack),
			       IR_var_number_in_block (split_regex_decl));
      to_vect_string_conversion (split_var, NULL);
      split_var = INDEXED_VAL (ER_stack_vars (uppest_stack),
			       IR_var_number_in_block (split_regex_decl));
      if (ER_NODE_MODE (split_var) == ER_NM_vect
	  && ER_NODE_MODE (ER_vect (split_var)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (ER_vect (split_var)) == ER_NM_char)
        split_regex = ER_pack_els (ER_vect (split_var));
      else
	eval_error (invenvar_decl, invcalls_decl,
		    IR_pos (cpc), DERR_corrupted_environment_var,
		    SPLIT_REGEX_NAME);
    }
  code = find_regex (split_regex, &reg);
  if (code != 0)
    process_regcomp_errors (code, SPLIT_NAME);
  else
    {
      el_size = type_size_table [ER_NM_vect];
      vect = ER_vect (pars_number == 2 ? below_ctop : ctop);
      els_number = start = 0;
      /* Count substrings. */
      while (start < ER_els_number (vect) || start == 0)
	{
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
	}
      /* Do not change size & packing. */
      PUSH_TEMP_REF (vect);
      result = create_pack_vector (els_number, ER_NM_vect);
      ER_set_els_number (result, 0);
      PUSH_TEMP_REF (result);
      vect = GET_TEMP_REF (1);
      start = els_number = 0;
      while (start < ER_els_number (vect) || start == 0)
	{
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
	  /* Create substring. */
	  sub_vect = create_pack_vector (chars_number, ER_NM_char);
	  ER_set_immutable (sub_vect, TRUE);
	  result = GET_TEMP_REF (0);
	  vect = GET_TEMP_REF (1);
	  strcpy (ER_pack_els (sub_vect), ER_pack_els (vect) + start);
	  assert (el_size == sizeof (ER_node_t));
	  memcpy (ER_pack_els (result) + el_size * els_number, &sub_vect,
		  el_size);
	  els_number++;
	  ER_set_els_number (result, els_number);
	  if (!ok)
	    break;
	  ER_pack_els (vect) [start + pmatch[0].rm_so] = ch;
	  start += (pmatch[0].rm_eo == 0 ? 1 : pmatch[0].rm_eo);
	}
      POP_TEMP_REF (2);
    }
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  if (result == NULL)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      ER_SET_MODE (ctop, ER_NM_vect);
      ER_set_vect (ctop, result);
    }
  INCREMENT_PC();
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static int
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
    case ER_NM_float:
      if (*(floating_t *) el1 < *(floating_t *) el2)
	return -1;
      else if (*(floating_t *) el1 == *(floating_t *) el2)
	return 0;
      else
	return 1;
    default:
      assert (FALSE);
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, SUBV_NAME);
  if (pars_number == 2)
    {
      to_vect_string_conversion (below_ctop, NULL);
      implicit_int_conversion (0);
    }
  else
    {
      to_vect_string_conversion (INDEXED_VAL (ER_CTOP (), -2), NULL);
      implicit_int_conversion (1);
      implicit_int_conversion (0);
    }
  if (ER_NODE_MODE (INDEXED_VAL (ER_CTOP (), -pars_number + 1))
      != ER_NM_vect
      || ER_NODE_MODE (ctop) != ER_NM_int
      || pars_number == 3 && ER_NODE_MODE (below_ctop) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, SUBV_NAME);
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
  vect = ER_vect (INDEXED_VAL (ER_CTOP (), -pars_number + 1));
  GO_THROUGH_REDIR (vect);
  vect_length = ER_els_number (vect);
  if (start < 0)
    start = 0;
  if (start < vect_length && (length < 0 || start + length >= vect_length))
    /* Get tail. */
    length = vect_length - start;
  else if (start >= vect_length)
    length = 0;
  PUSH_TEMP_REF (vect);
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
      vect = GET_TEMP_REF (0);
    }
  else if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
    {
      el_type = ER_pack_vect_el_type (vect);
      el_size = type_size_table [el_type];
      res = create_pack_vector (el_type == ER_NM_char ? length + 1 : length,
				el_type);
      ER_set_els_number (res, length);
      vect = GET_TEMP_REF (0);
      memcpy (ER_pack_els (res), ER_pack_els (vect) + start * el_size,
	      el_size * length);
      if (el_type == ER_NM_char)
	ER_pack_els (res) [length] = '\0';
    }
  else
    {
      res = create_unpack_vector (length);
      vect = GET_TEMP_REF (0);
      memcpy (ER_unpack_els (res),
	      (char *) ER_unpack_els (vect) + start * sizeof (val_t),
	      length * sizeof (val_t));
    }
  ER_set_immutable (res, ER_immutable (vect));
  POP_TEMP_REF (1);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, res);
  INCREMENT_PC();
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, CMPV_NAME);
  to_vect_string_conversion (ctop, NULL);
  to_vect_string_conversion (below_ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (below_ctop) != ER_NM_vect)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, CMPV_NAME);
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
	  el = INDEXED_VAL (ER_unpack_els (vect1), i);
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
	  el = INDEXED_VAL (ER_unpack_els (vect2), i);
	  el_type2 = ER_NODE_MODE (el);
	  addr2
	    = (char *) el + val_displ_table [ER_NODE_MODE ((ER_node_t) el)];
	}
      if (el_type1 != el_type2
	  || el_type1 != ER_NM_float
	  && el_type1 != ER_NM_int && el_type1 != ER_NM_char)
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
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
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, res);
  INCREMENT_PC();
}

void
del_call (int_t pars_number)
{
  ER_node_t val;
  ER_node_mode_t mode;
  ER_node_t vect;
  ER_node_t tab;
      
  val = INDEXED_VAL (ER_CTOP (), -pars_number + 1);
  if (pars_number < 2 || pars_number > 3
      || ER_NODE_MODE (val) == ER_NM_tab && pars_number != 2)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, DEL_NAME);
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
      
      implicit_int_conversion (pars_number - 2);
      start_val = INDEXED_VAL (ER_CTOP (), -pars_number + 2);
      if (pars_number == 3)
	{
	  implicit_int_conversion (0);
	  length_val = INDEXED_VAL (ER_CTOP (), 0);
	}
      else
	length_val = NULL;
      if (ER_NODE_MODE (start_val) != ER_NM_int
	  || length_val != NULL && ER_NODE_MODE (length_val) != ER_NM_int)
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		    DERR_parameter_type, DEL_NAME);
      start = ER_i (start_val);
      if (length_val != NULL)
	length = ER_i (length_val);
      else
	length = 1;
      vect = ER_vect (val);
      GO_THROUGH_REDIR (vect);
      if (ER_immutable (vect))
	eval_error (immutable_decl, invaccesses_decl, IR_pos (cpc),
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
	eval_error (immutable_decl, invaccesses_decl, IR_pos (cpc),
		    DERR_immutable_table_modification);
      remove_tab_el (tab, INDEXED_VAL (ER_CTOP (), -pars_number + 2));
    }
  else
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameter_type, DEL_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, mode);
  if (mode == ER_NM_tab)
    ER_set_tab (ctop, tab);
  else
    ER_set_vect (ctop, vect);
  INCREMENT_PC();
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
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number,
		(!vector_flag ? INS_NAME : INSV_NAME));
  vect_val = INDEXED_VAL (ER_CTOP (), -pars_number + 1);
  el_val = INDEXED_VAL (ER_CTOP (), -pars_number + 2);
  if (pars_number == 3)
    {
      implicit_int_conversion (0);
      index_val = INDEXED_VAL (ER_CTOP (), 0);
    }
  else
    index_val = NULL;
  if (ER_NODE_MODE (vect_val) != ER_NM_vect
      || vector_flag && ER_NODE_MODE (el_val) != ER_NM_vect
      || index_val != NULL && ER_NODE_MODE (index_val) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, (vector_flag ? INSV_NAME : INS_NAME));
  if (index_val != NULL)
    index = ER_i (index_val);
  else
    index = 0;
  vect = ER_vect (vect_val);
  GO_THROUGH_REDIR (vect);
  if (vector_flag)
    {
      PUSH_TEMP_REF (ER_vect (el_val));
      GO_THROUGH_REDIR (GET_TEMP_REF (0));
      addition = ER_els_number (GET_TEMP_REF (0));
    }
  if (ER_immutable (vect))
    eval_error (immutable_decl, invaccesses_decl, IR_pos (cpc),
		DERR_immutable_vector_modification);
  PUSH_TEMP_REF (vect);
  if (vector_flag && ER_NODE_MODE (GET_TEMP_REF (1)) == ER_NM_heap_pack_vect
      && (ER_NODE_MODE (vect) != ER_NM_heap_pack_vect
	  || (ER_pack_vect_el_type (vect)
	      != ER_pack_vect_el_type (GET_TEMP_REF (1)))))
    {
      /* Remember about GC! */
      GET_TEMP_REF (1) = unpack_vector (GET_TEMP_REF (1));
      el_val = INDEXED_VAL (ER_CTOP (), -pars_number + 2);
    }
  vect = GET_TEMP_REF (0);
  POP_TEMP_REF (1);
  if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect
      && (!vector_flag
	  && ER_pack_vect_el_type (vect) != ER_NODE_MODE (el_val)
	  || vector_flag
	  && (ER_NODE_MODE (GET_TEMP_REF (0)) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (vect)
	      != ER_pack_vect_el_type (GET_TEMP_REF (0)))))
    /* Remember about GC! */
    vect = unpack_vector (vect);
  if (!vector_flag)
    addition = 1;
  else
    addition = ER_els_number (GET_TEMP_REF (0));
  vect_length = ER_els_number (vect);
  /* Remember about GC! */
  vect = expand_vector (vect, vect_length + addition);
  el_val = INDEXED_VAL (ER_CTOP (), -pars_number + 2);
  if (vector_flag)
    {
      el_vect = GET_TEMP_REF (0);
      POP_TEMP_REF (1);
    }
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
	  assert (ER_NODE_MODE (el_vect) == ER_NM_heap_pack_vect
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
	*(val_t *) INDEXED_VAL (ER_unpack_els (vect), index)
	  = *(val_t *) el_val;
      else
	memcpy (INDEXED_VAL (ER_unpack_els (vect), index),
		ER_unpack_els (el_vect), addition * sizeof (val_t));
    }
  ER_set_els_number (vect, vect_length + addition);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
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
  ER_node_t el_vect;
  ER_node_mode_t el_type;
  size_t vect_length;
  size_t el_size;
  size_t i, j;
  val_t temp_val;
  char temp_el [sizeof (floating_t) * 8];

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, REV_NAME);
  if (ER_NODE_MODE (ctop) != ER_NM_vect)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, REV_NAME);
  vect = ER_vect (ctop);
  GO_THROUGH_REDIR (vect);
  if (ER_immutable (vect))
    eval_error (immutable_decl, invaccesses_decl, IR_pos (cpc),
		DERR_immutable_vector_modification);
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
	      temp_val = *(val_t *) INDEXED_VAL (ER_unpack_els (vect), i);
	      *(val_t *) INDEXED_VAL (ER_unpack_els (vect), i)
		= *(val_t *) INDEXED_VAL (ER_unpack_els (vect), j);
	      *(val_t *) INDEXED_VAL (ER_unpack_els (vect), j) = temp_val;
	    }
	}
    }
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
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

static IR_node_t dino_compare_func;

static int
array_sort_compare_function (const void *el1, const void *el2)
{
  int res;

  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_func);
  ER_set_func (ctop, dino_compare_func);
  ER_set_func_context (ctop, GET_TEMP_REF (0));
  TOP_UP;
  if (sorted_vect_el_type != ER_NM_val)
    {
      ER_SET_MODE (ctop, sorted_vect_el_type);
      memcpy ((char *) ctop + val_displ_table [ER_NODE_MODE (ctop)],
	      (char *) el1, type_size_table [sorted_vect_el_type]);
    }
  else
    *(val_t *) ctop = *(val_t *) el1;
  TOP_UP;
  if (sorted_vect_el_type != ER_NM_val)
    {
      ER_SET_MODE (ctop, sorted_vect_el_type);
      memcpy ((char *) ctop + val_displ_table [ER_NODE_MODE (ctop)],
	      (char *) el2, type_size_table [sorted_vect_el_type]);
    }
  else
    *(val_t *) ctop = *(val_t *) el2;
  call_func_class (2);
  if (ER_NODE_MODE (ctop) != ER_NM_int)
    eval_error (invresult_decl, invcalls_decl,
		IR_pos (cpc), DERR_invalid_result, SORT_NAME);
  res = ER_i (ctop);
  TOP_DOWN;
  return res;
}

void
sort_call (int_t pars_number)
{
  ER_node_t vect;
  ER_node_t var;

  if (pars_number != 1 && pars_number != 2)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, SORT_NAME);
  var = INDEXED_VAL (ER_CTOP (), -pars_number + 1);
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
	  || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
	  && ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_int
	  && ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_float)
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		    DERR_parameter_type, SORT_NAME);
      vect = copy_vector (ER_vect (ctop));
      sorted_vect_el_type = ER_pack_vect_el_type (vect);
      qsort (ER_pack_els (vect), ER_els_number (vect),
	     type_size_table [sorted_vect_el_type],
	     homogeneous_array_sort_compare_function);
      /* Pop all actual parameters. */
      DECR_CTOP (pars_number);
      SET_TOP;
    }
  else
    {
      if (ER_NODE_MODE (below_ctop) != ER_NM_vect
	  || ER_NODE_MODE (ctop) != ER_NM_func)
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		    DERR_parameter_type, SORT_NAME);
      vect = copy_vector (ER_vect (below_ctop));
      PUSH_TEMP_REF (ER_func_context (ctop));
      dino_compare_func = ER_func (ctop);
      if (ER_NODE_MODE (vect) == ER_NM_heap_unpack_vect)
	sorted_vect_el_type = ER_NM_val;
      else
	sorted_vect_el_type = ER_pack_vect_el_type (vect);
      /* ??? We could modify GC in order to do not change the array
         location.  But now we have the simpler solution. */
      no_gc_flag = TRUE;
      /* Pop all actual parameters. */
      DECR_CTOP (pars_number);
      SET_TOP;
      qsort ((sorted_vect_el_type == ER_NM_val
	      ? (char *) ER_unpack_els (vect) : ER_pack_els (vect)),
	     ER_els_number (vect),
	     (sorted_vect_el_type != ER_NM_val
	      ? type_size_table [sorted_vect_el_type]
	      : sizeof (val_t)),
	     array_sort_compare_function);
      no_gc_flag = FALSE;
      POP_TEMP_REF (1);
    }
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
}

static void
print_ch (int ch)
{
  char str [10];

  if (ch == '\'' || ch == '"' || ch == '\\')
    sprintf (str, "\\%c", ch);
  else if (isprint (ch))
    sprintf (str, "%c", ch);
  else if (ch == '\n')
    sprintf (str, "\\n");
  else if (ch == '\t')
    sprintf (str, "\\t");
  else if (ch == '\v')
    sprintf (str, "\\v");
  else if (ch == '\a')
    sprintf (str, "\\a");
  else if (ch == '\b')
    sprintf (str, "\\b");
  else if (ch == '\r')
    sprintf (str, "\\r");
  else if (ch == '\f')
    sprintf (str, "\\f");
  else
    sprintf (str, "\\%o", ch);
  VLO_ADD_STRING (temp_vlobj, str);
}

static int
print_context (ER_node_t context)
{
  IR_node_t block;
  IR_node_t func_class;
  char str [100];

  if (context == NULL || ER_context (context) == NULL)
    /* We ignore the uppest implicit block. */
    return FALSE;
  block = ER_block_node (context);
  func_class = IR_func_class_ext (block);
  if (print_context (ER_context (context)))
    VLO_ADD_STRING (temp_vlobj, ".");
  if (func_class == NULL)
    VLO_ADD_STRING (temp_vlobj, "{}");
  else if (IR_IS_OF_TYPE (func_class, IR_NM_func))
    {
      VLO_ADD_STRING
	(temp_vlobj,
	 IR_ident_string (IR_unique_ident (IR_ident (func_class))));
      sprintf (str, "(%ld)", (long int) ER_context_number (context));
      VLO_ADD_STRING (temp_vlobj, str);
    }
  else
    {
      assert (IR_IS_OF_TYPE (func_class, IR_NM_class));
      VLO_ADD_STRING
	(temp_vlobj,
	 IR_ident_string (IR_unique_ident (IR_ident (func_class))));
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

  var = INDEXED_VAL (ER_CTOP (), -pars_number + 1);
  if (!ER_IS_OF_TYPE (var, ER_NM_instance)
      || ER_class (ER_instance (var)) != file_decl)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, function_name);
  instance
    = ER_instance ((ER_node_t) INDEXED_VAL (ER_CTOP (), -pars_number + 1));
  return ER_hide (INDEXED_VAL (ER_instance_vars (instance),
			       IR_var_number_in_block (file_ptr_decl)));
}

static void
place_file_instance (FILE *f)
{
  ER_node_t var;
  ER_node_t instance;

  ER_SET_MODE (ctop, ER_NM_class);
  ER_set_class (ctop, file_decl);
  ER_set_class_context (ctop, uppest_stack);
  instance = create_instance (0);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_instance);
  ER_set_instance (ctop, instance);
  var = INDEXED_VAL (ER_instance_vars (ER_instance (ctop)),
		     IR_var_number_in_block (file_ptr_decl));
  ER_SET_MODE (var, ER_NM_hide);
  ER_set_hide (var, f);
}

static void
two_strings_func_start (int_t pars_number, const char *function_name)
{
  if (pars_number != 2)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, function_name);
  to_vect_string_conversion (ctop, NULL);
  to_vect_string_conversion (below_ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char
      || ER_NODE_MODE (below_ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (below_ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (below_ctop)) != ER_NM_char)
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameter_type, function_name);
}

void
rename_call (int_t pars_number)
{
  two_strings_func_start (pars_number, RENAME_NAME);
  errno = 0;
  rename (ER_pack_els (ER_vect (below_ctop)), ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (RENAME_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
}

static void
string_func_start (int_t pars_number, const char *function_name)
{
  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, function_name);
  to_vect_string_conversion (ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameter_type, function_name);
}

void
remove_call (int_t pars_number)
{
  string_func_start (pars_number, REMOVE_NAME);
  errno = 0;
  remove (ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (REMOVE_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
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

  string_func_start (pars_number, MKDIR_NAME);
  errno = 0;
  mask = (S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IWGRP | S_IXGRP
	  | S_IROTH | S_IWOTH | S_IXOTH);
  mkdir (ER_pack_els (ER_vect (ctop)), mask);
  if (errno)
    process_system_errors (MKDIR_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
}

void
rmdir_call (int_t pars_number)
{
  string_func_start (pars_number, RMDIR_NAME);
  errno = 0;
  rmdir (ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (RMDIR_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
}

void
getcwd_call (int_t pars_number)
{
  ER_node_t vect;
  char buf [FILENAME_MAX + 1], *str;

  if (pars_number != 0)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, GETCWD_NAME);
  errno = 0;
  str = getcwd (buf, FILENAME_MAX);
  if (errno)
    process_system_errors (GETCWD_NAME);
  vect = create_string (str);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
}

void
chdir_call (int_t pars_number)
{
  string_func_start (pars_number, CHDIR_NAME);
  errno = 0;
  chdir (ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (CHDIR_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
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
  else if (ER_IS_OF_TYPE (var, ER_NM_instance)
	   && ER_class (ER_instance (var)) == file_decl)
    result
      = fstat (fileno
	       ((FILE *) ER_hide (INDEXED_VAL
				  (ER_instance_vars (ER_instance (var)),
				   IR_var_number_in_block (file_ptr_decl)))),
	       buf);
  else
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameter_type, function_name);
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
  mask = buf.st_mode & ~clear_mask | set_mask;
  chmod (ER_pack_els (ER_vect (below_ctop)), mask);
  if (errno)
    process_system_errors (function_name);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
}

void
chumod_call (int_t pars_number)
{
  int mask = 0;
  char *str;

  two_strings_func_start (pars_number, CHUMOD_NAME);
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

  two_strings_func_start (pars_number, CHGMOD_NAME);
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

  two_strings_func_start (pars_number, CHOMOD_NAME);
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
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
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  INCREMENT_PC();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, result);
}

void
open_call (int_t pars_number)
{
  FILE *f;

  two_strings_func_start (pars_number, OPEN_NAME);
  errno = 0;
  f = fopen (ER_pack_els (ER_vect (below_ctop)), ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (OPEN_NAME);
  else if (f == NULL)
    eval_error (einval_decl, invcalls_decl, IR_pos (cpc), DERR_einval,
		OPEN_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  place_file_instance (f);
  INCREMENT_PC();
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
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
  INCREMENT_PC();
}

void
flush_call (int_t pars_number)
{
  FILE *f;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, FLUSH_NAME);
  f = get_file (pars_number, FLUSH_NAME);
  errno = 0;
  fflush (f);
  if (errno)
    process_system_errors (FLUSH_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
  INCREMENT_PC();
}

void
popen_call (int_t pars_number)
{
  FILE *f;

  two_strings_func_start (pars_number, POPEN_NAME);
  errno = 0;
  if (*ER_pack_els (ER_vect (ctop)) != 'r'
      && *ER_pack_els (ER_vect (ctop)) != 'w'
      || strlen (ER_pack_els (ER_vect (ctop))) != 1)
    {
      errno = EINVAL;
      process_system_errors (POPEN_NAME);
    }
  f = popen (ER_pack_els (ER_vect (below_ctop)), ER_pack_els (ER_vect (ctop)));
  if (errno)
    process_system_errors (POPEN_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  place_file_instance (f);
  INCREMENT_PC();
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
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
  INCREMENT_PC();
}

void
tell_call (int_t pars_number)
{
  FILE *f;
  int_t pos;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, TELL_NAME);
  f = get_file (pars_number, TELL_NAME);
  errno = 0;
  pos = ftell (f);
  if (errno)
    process_system_errors (TELL_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, pos);
  INCREMENT_PC();
}

void
seek_call (int_t pars_number)
{
  FILE *f;
  int_t pos;
  int whence;
  int ch;
  int res;

  if (pars_number != 3)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, SEEK_NAME);
  f = get_file (pars_number, SEEK_NAME);
  implicit_arithmetic_conversion (1);
  to_vect_string_conversion (ctop, NULL);
  if (ER_NODE_MODE (below_ctop) != ER_NM_int
      || (ER_NODE_MODE (ctop) != ER_NM_char
	  && (ER_NODE_MODE (ctop) != ER_NM_vect
	      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)))
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, SEEK_NAME);
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
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, SEEK_NAME);
  errno = 0;
  fseek (f, pos, whence);
  if (errno)
    process_system_errors (SEEK_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
  INCREMENT_PC();
}

static void
print_val (ER_node_t val, int quote_flag)
{
  ER_node_t vect;
  ER_node_t tab;
  ER_node_t key;
  size_t i;
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
    case ER_NM_float:
      sprintf (str, "%g", ER_f (val));
      VLO_ADD_STRING (temp_vlobj, str);
      break;
    case ER_NM_vect:
      to_vect_string_conversion (val, NULL);
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
	      print_val (INDEXED_VAL (ER_unpack_els (vect), i), TRUE);
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
	      memcpy ((char *) &temp_val + displ,
		      (char *) ER_pack_els (vect) + i * el_size, el_size);
	      print_val ((ER_node_t) &temp_val, TRUE);
	      if (i < ER_els_number (vect) - 1)
		VLO_ADD_STRING (temp_vlobj, ", ");
	    }
	  VLO_ADD_STRING (temp_vlobj, "]");
	}
      break;
    case ER_NM_tab:
      VLO_ADD_STRING (temp_vlobj, "{");
      tab = ER_tab (val);
      GO_THROUGH_REDIR (tab);
      flag = FALSE;
      for (i = 0; i < ER_entries_number (tab); i++)
	{
	  key = INDEXED_ENTRY_KEY (ER_tab_els (tab), i);
	  if (ER_NODE_MODE (key) == ER_NM_empty_entry
	      || ER_NODE_MODE (key) == ER_NM_deleted_entry)
	    continue;
	  if (flag)
	    VLO_ADD_STRING (temp_vlobj, ", ");
	  print_val (key, TRUE);
	  VLO_ADD_STRING (temp_vlobj, ":");
	  print_val (INDEXED_ENTRY_VAL (ER_tab_els (tab), i), TRUE);
	  flag = TRUE;
	}
      VLO_ADD_STRING (temp_vlobj, "}");
      break;
    case ER_NM_func:
    case ER_NM_class:
      if (ER_NODE_MODE (val) == ER_NM_func)
	{
	  VLO_ADD_STRING (temp_vlobj, "func ");
	  if (print_context (ER_func_context (val)))
	    VLO_ADD_STRING (temp_vlobj, ".");
	  VLO_ADD_STRING (temp_vlobj,
			  IR_ident_string (IR_unique_ident 
					   (IR_ident (ER_func (val)))));
	}
      else
	{
	  VLO_ADD_STRING (temp_vlobj, "class ");
	  if (print_context (ER_class_context (val)))
	    VLO_ADD_STRING (temp_vlobj, ".");
	  VLO_ADD_STRING (temp_vlobj,
			  IR_ident_string (IR_unique_ident 
					   (IR_ident (ER_class (val)))));
	}
      break;
    case ER_NM_stack:
      VLO_ADD_STRING (temp_vlobj, "stack ");
      /* Context may be uppest block stack. */
      print_context (ER_stack (val));
      break;
    case ER_NM_instance:
      VLO_ADD_STRING (temp_vlobj, "instance ");
      if (!print_context (ER_instance (val)))
	assert (FALSE);
      break;
    case ER_NM_process:
      if (ER_thread_func (ER_process (val)) == NULL)
	VLO_ADD_STRING (temp_vlobj, "main thread");
      else
	{
	  ER_node_t stack;

	  for (stack = ER_saved_cstack (ER_process (val));
	       stack != NULL;
	       stack = ER_prev_stack (stack))
	    if (IR_func_class_ext (ER_block_node (stack)) != NULL
		&& IR_IS_OF_TYPE (IR_func_class_ext (ER_block_node (stack)), 
				  IR_NM_func)
		&& IR_thread_flag (IR_func_class_ext (ER_block_node (stack))))
	      break;
	  sprintf (str, "thread %ld ",
		   (long int) ER_process_number (ER_process (val)));
	  VLO_ADD_STRING (temp_vlobj, str);
	  if (!print_context (stack))
	    assert (FALSE);
	}
      break;
    case ER_NM_type:
      switch (ER_type (val))
	{
	case ER_T_nil:
	  string = "type (nil)";
	  break;
	case ER_T_char:
	  string = "char";
	  break;
	case ER_T_int:
	  string = "int";
	  break;
	case ER_T_float:
	  string = "float";
	  break;
	case ER_T_hide:
	  string = "hide";
	  break;
	case ER_T_hideblock:
	  string = "hideblock";
	  break;
	case ER_T_vector:
	  string = "vector";
	  break;
	case ER_T_table:
	  string = "table";
	  break;
	case ER_T_thread:
	  string = "thread";
	  break;
	case ER_T_func:
	  string = "func";
	  break;
	case ER_T_class:
	  string = "class";
	  break;
	case ER_T_instance:
	  string = "class ()";
	  break;
	case ER_T_stack:
	  string = "func ()";
	  break;
	case ER_T_process:
	  string = "thread ()";
	  break;
	case ER_T_type:
	  string = "type";
	  break;
	default:
	  assert (FALSE);
	}
      VLO_ADD_STRING (temp_vlobj, string);
      break;
    default:
      assert (FALSE);
    }
}

static FILE *
file_function_call_start (int_t pars_number, const char *function_name)
{
  if (pars_number == 0)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, function_name);
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

  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  if (f != NULL)
    {
      fputs (VLO_BEGIN (temp_vlobj), f);
      /* Place the result instead of the function. */
      ER_SET_MODE (ctop, ER_NM_nil);
    }
  else
    {
      vect = create_string (VLO_BEGIN (temp_vlobj));
      /* Place the result instead of the function. */
      ER_SET_MODE (ctop, ER_NM_vect);
      ER_set_vect (ctop, vect);
    }
  INCREMENT_PC();
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
      assert (f == NULL);
    }
  else if (param_type == STANDARD_FILE)
    function_name = (ln_flag ? PUTLN_NAME : PUT_NAME);
  else
    function_name = (ln_flag ? FPUTLN_NAME : FPUT_NAME);
  VLO_NULLIFY (temp_vlobj);
  for (i = -pars_number + (param_type == GIVEN_FILE ? 1 : 0) + 1; i <= 0; i++)
    {
      var = INDEXED_VAL (ER_CTOP (), i);
      to_vect_string_conversion (var, NULL);
      if (ER_NODE_MODE (var) != ER_NM_vect
	  || ER_NODE_MODE (ER_vect (var)) != ER_NM_heap_pack_vect
	  || ER_pack_vect_el_type (ER_vect (var)) != ER_NM_char)
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
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
      assert (f == NULL);
    }
  else if (param_type == STANDARD_FILE)
    function_name = (ln_flag ? PRINTLN_NAME : PRINT_NAME);
  else
    function_name = (ln_flag ? FPRINTLN_NAME : FPRINT_NAME);
  VLO_NULLIFY (temp_vlobj);
  for (i = -pars_number + (param_type == GIVEN_FILE ? 1 : 0) + 1; i <= 0; i++)
    print_val (INDEXED_VAL (ER_CTOP (), i), quote_flag);
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
    eval_error (eof_decl, invcalls_decl, IR_pos (cpc), DERR_eof_occured,
		file_flag ? FGET_NAME : GET_NAME);
  /* Pop all actual parameters. */
  if (file_flag)
    TOP_DOWN;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_char);
  ER_set_ch (ctop, ch);
  INCREMENT_PC();
}

static void
general_get_ln_file_call (FILE *f, int param_flag, int ln_flag, int as_lns_p,
			  const char *func_name)
{
  ER_node_t vect;
  int ch;
  int saved_no_gc_flag;
  size_t ch_n, els_number, i;

  VLO_NULLIFY (temp_vlobj);
  if (!ln_flag && as_lns_p)
    {
      VLO_NULLIFY (temp_vlobj2);
      saved_no_gc_flag = no_gc_flag;
      no_gc_flag = TRUE;
    }
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
	((ER_node_t *) ER_pack_els (vect)) [i]
	  = ((ER_node_t *) VLO_BEGIN (temp_vlobj2)) [i];
      no_gc_flag = saved_no_gc_flag;
    }
  if (errno != 0)
    process_system_errors (func_name);
  /* ??? */
  if (ch == EOF && ch_n == 0)
    eval_error (eof_decl, invcalls_decl, IR_pos (cpc),
		DERR_eof_occured, func_name);
  /* Pop all actual parameters. */
  if (param_flag)
    TOP_DOWN;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
}

void
get_call (int_t pars_number)
{
  if (pars_number != 0)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, GET_NAME);
  general_get_call (stdin, FALSE);
}

void
getln_call (int_t pars_number)
{
  if (pars_number != 0)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, GETLN_NAME);
  general_get_ln_file_call (stdin, FALSE, TRUE, FALSE, GETLN_NAME);
}

void
getf_call (int_t pars_number)
{
  int flag = 0;

  if (pars_number > 1)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, GETF_NAME);
  if (pars_number == 1)
    {
      implicit_int_conversion (0);
      if (!ER_IS_OF_TYPE (ctop, ER_NM_int))
	eval_error (partype_decl, invcalls_decl,
		    IR_pos (cpc), DERR_parameter_type, GETF_NAME);
      flag = ER_i (ctop);
      TOP_DOWN;
      pars_number--;
    }
  general_get_ln_file_call (stdin, FALSE, FALSE, flag != 0, GETF_NAME);
}

static FILE *
fget_function_call_start (int_t pars_number, const char *function_name)
{
  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, function_name);
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
      implicit_int_conversion (0);
      if (!ER_IS_OF_TYPE (ctop, ER_NM_int))
	eval_error (partype_decl, invcalls_decl,
		    IR_pos (cpc), DERR_parameter_type, FGETF_NAME);
      flag = ER_i (ctop);
      TOP_DOWN;
      pars_number--;
    }
  general_get_ln_file_call
    (fget_function_call_start (pars_number, FGETF_NAME),
     TRUE, FALSE, flag != 0, FGETF_NAME);
}

#define F_CHAR   256
#define F_INT    257
#define F_FLOAT  258
#define F_STRING 259

struct token
{
  int token_code;
  union
  {
    char_t ch;
    int_t i;
    floating_t f;
    string_t str;
  } val;
};

/* Var length string used by func yylval for text presentation of the
   symbol. */
static vlo_t el_text;

static int
initiate_io (void)
{
  VLO_CREATE (el_text, 0);
}

static int
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
  eval_error (invinput_decl, invcalls_decl, IR_pos (cpc),
	      DERR_invalid_input, function_name);
}

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
        case '{':
        case '}':
        case EOF:
	  result.token_code = curr_char;
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
		|| (curr_char == '-' || curr_char == '+')
		&& isdigit (next_char))
	      {
		/* Recognition numbers. */
		result.token_code = F_INT;
		do
		  {
		    VLO_ADD_BYTE (el_text, curr_char);
		    curr_char = fgetc (f);
		  }
		while (isdigit (curr_char));
		if (curr_char == '.')
		  {
		    result.token_code = F_FLOAT;
		    do
		      {
			VLO_ADD_BYTE (el_text, curr_char);
			curr_char = fgetc (f);
		      }
		    while (isdigit (curr_char));
		  }
		if (curr_char == 'e' || curr_char == 'E')
		  {
		    result.token_code = F_FLOAT;
		    curr_char = fgetc (f);
		    if (curr_char != '+' && curr_char != '-'
			&& !isdigit (curr_char))
		      {
			if (ln_flag && curr_char == '\n')
			  ungetc (curr_char, f);
			invinput_error (f, function_name, ln_flag);
		      }
		    else
		      {
			VLO_ADD_BYTE (el_text, 'e');
			do
			  {
			    VLO_ADD_BYTE (el_text, curr_char);
			    curr_char = fgetc (f);
			  }
			while (isdigit (curr_char));
		      }
		  }
		VLO_ADD_BYTE (el_text, '\0');
		ungetc (curr_char, f);
		if (errno)
		  process_system_errors (function_name);
		if (result.token_code == F_FLOAT)
		  result.val.f = a2f (VLO_BEGIN (el_text));
		else
		  result.val.i = a2i (VLO_BEGIN (el_text));
		if (errno)
		  process_system_errors (result.token_code == F_FLOAT
					 ? "string-to-float conversion"
					 : "string-to-int conversion");
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
              | string
              | '[' [list] ']'
              | '{' [list] '}'

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
    case F_STRING:
      {
	ER_node_t vect;

	ER_SET_MODE (ptr, ER_NM_vect);
	vect = create_string (token.val.str);
	ER_set_vect (ptr, vect);
	return result;
      }
    case '[':
      {
	int_t repeat;
	int_t i;
	ER_node_t vect;

	vect = create_empty_vector ();
	PUSH_TEMP_REF (vect);
	token = get_token (f, function_name, ln_flag);
	for (;;)
	  {
	    if (token.token_code == ']')
	      {
		vect = GET_TEMP_REF (0);
		POP_TEMP_REF (1);
		ER_SET_MODE (ptr, ER_NM_vect);
		ER_set_vect (ptr, vect);
		return result;
	      }
	    result = scanel (f, token, function_name, ln_flag);
	    token = get_token (f, function_name, ln_flag);
	    if (token.token_code == ':')
	      {
		TOP_UP;
		*(val_t *) ctop = result;
		implicit_int_conversion (0);
		result = *(val_t *) ctop;
		TOP_DOWN;
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
	    vect = GET_TEMP_REF (0);
	    POP_TEMP_REF (1);
	    if (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect)
	      vect = unpack_vector (vect);
	    TOP_UP;
	    *(val_t *) ctop = result;
	    vect = expand_vector (vect, ER_els_number (vect) + repeat);
	    result = *(val_t *) ctop;
	    TOP_DOWN;
	    for (i = ER_els_number (vect); repeat > 0; i++, repeat--)
	      *(val_t *) INDEXED_VAL (ER_unpack_els (vect), i) = result;
	    ER_set_els_number (vect, i);
	    PUSH_TEMP_REF (vect);
	    if (token.token_code == ',')
	      token = get_token (f, function_name, ln_flag);
	  }
	}
    case '{':
      {
	ER_node_t tab;
	val_t key_val;
	ER_node_t key = (ER_node_t) &key_val;
	ER_node_t entry;
	ER_node_mode_t mode;

	tab = create_tab (40);
	PUSH_TEMP_REF (tab);
	token = get_token (f, function_name, ln_flag);
	for (;;)
	  {
	    if (token.token_code == '}')
	      {
		tab = GET_TEMP_REF (0);
		POP_TEMP_REF (1);
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
		if (mode == ER_NM_vect)
		  PUSH_TEMP_REF (ER_vect (key));
		else if (mode == ER_NM_tab)
		  PUSH_TEMP_REF (ER_tab (key));
		token = get_token (f, function_name, ln_flag);
		result = scanel (f, token, function_name, ln_flag);
		token = get_token (f, function_name, ln_flag);
		if (mode == ER_NM_vect)
		  {
		    ER_set_vect (key, GET_TEMP_REF (0));
		    POP_TEMP_REF (1);
		  }
		else if (mode == ER_NM_tab)
		  {
		    ER_set_tab (key, GET_TEMP_REF (0));
		    POP_TEMP_REF (1);
		  }
	      }
	    else
	      key_val = result;
	    tab = GET_TEMP_REF (0);
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
    eval_error (eof_decl, invcalls_decl, IR_pos (cpc),
		DERR_eof_occured, function_name);
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
  /* Pop all actual parameters. */
  if (file_flag)
    TOP_DOWN;
  /* Place the result instead of the function. */
  *(val_t *) ctop = val;
  INCREMENT_PC();
}

void
scan_call (int_t pars_number)
{
  if (pars_number != 0)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, SCAN_NAME);
  general_scan_call (stdin, FALSE, FALSE);
}

void
scanln_call (int_t pars_number)
{
  if (pars_number != 0)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, SCANLN_NAME);
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
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, result);
  INCREMENT_PC();
}

static void
function_without_par (int_t pars_number, const char *function_name)
{
  if (pars_number != 0)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
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

  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  vect = create_string (result);
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
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
  size_t el_type_size;
  size_t i;

  if (pars_number != 0)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, GETGROUPS_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
#if defined(HAVE_GETGROUPS) && !defined(WIN32)
  els_number = getgroups (0, NULL);
  VLO_NULLIFY (temp_vlobj);
  VLO_EXPAND (temp_vlobj, sizeof (GETGROUPS_T) * els_number);
  getgroups (els_number, (GETGROUPS_T *) VLO_BEGIN (temp_vlobj));
  for (grs_n = i = 0; i < els_number; i++)
    if (getgrgid (((GETGROUPS_T *) VLO_BEGIN (temp_vlobj)) [i]) != NULL)
      grs_n++;
  if (grs_n == 0)
    vect = create_empty_vector ();
  else
    {
      el_type_size = type_size_table [ER_NM_vect];
      vect = create_pack_vector (grs_n, ER_NM_vect);
      ER_set_els_number (vect, 0);
    }
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  if (grs_n != 0)
      for (grs_n = i = 0; i < els_number; i++)
	{
	  struct group *gr;
	  
	  gr = getgrgid (((GETGROUPS_T *) VLO_BEGIN (temp_vlobj)) [i]);
	  if (gr != NULL)
	    {
	      vect = create_string (gr->gr_name);
	      ((ER_node_t *) ER_pack_els (ER_vect (ctop))) [grs_n] = vect;
	      grs_n++;
	      ER_set_els_number (ER_vect (ctop), grs_n);
	    }
	}
#else
  vect = create_empty_vector ();
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
#endif
  INCREMENT_PC();
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
float_function_start (int_t pars_number, const char *function_name)
{
  floating_t result;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, function_name);
  implicit_arithmetic_conversion (0);
  if (ER_NODE_MODE (ctop) == ER_NM_int)
    {
      result = ER_i (ctop);
      ER_SET_MODE (ctop, ER_NM_float);
      ER_set_f (ctop, result);
    }
  if (ER_NODE_MODE (ctop) != ER_NM_float)
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameter_type, function_name);
  errno = 0;
}

#if INLINE && !defined (SMALL_CODE)
__inline__
#endif
static void
float_function_start2 (int_t pars_number, const char *function_name)
{
  floating_t result;

  if (pars_number != 2)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, function_name);
  implicit_arithmetic_conversion (0);
  if (ER_NODE_MODE (ctop) == ER_NM_int)
    {
      result = ER_i (ctop);
      ER_SET_MODE (ctop, ER_NM_float);
      ER_set_f (ctop, result);
    }
  implicit_arithmetic_conversion (1);
  if (ER_NODE_MODE (below_ctop) == ER_NM_int)
    {
      result = ER_i (below_ctop);
      ER_SET_MODE (below_ctop, ER_NM_float);
      ER_set_f (below_ctop, result);
    }
  if (ER_NODE_MODE (ctop) != ER_NM_float
      || ER_NODE_MODE (below_ctop) != ER_NM_float)
    eval_error (partype_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameter_type, function_name);
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
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_float);
  ER_set_f (ctop, result);
  INCREMENT_PC();
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
  floating_t result;
  int_t seed;

  if (rand_flag && pars_number != 0 || !rand_flag && pars_number > 1)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number,
		(rand_flag ? RAND_NAME : SRAND_NAME));
  if (!rand_flag &&  pars_number == 1)
    {
      implicit_arithmetic_conversion (0);
      if (ER_NODE_MODE (ctop) == ER_NM_float)
	{
	  seed = ER_f (ctop);
	  ER_SET_MODE (ctop, ER_NM_int);
	  ER_set_i (ctop, seed);
	}
      else if (ER_NODE_MODE (ctop) == ER_NM_int)
	seed = ER_i (ctop);
      else
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		    DERR_parameter_type, SRAND_NAME);
    }
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  if (rand_flag)
    {
      result = rand () / RAND_MAX;
      /* Place the result instead of the function. */
      ER_SET_MODE (ctop, ER_NM_float);
      ER_set_f (ctop, (rand () + 0.0) / RAND_MAX);
    }
  else
    {
      ER_SET_MODE (ctop, ER_NM_nil);
      if (pars_number == 1)
	srand ((unsigned) seed);
      else
	srand ((unsigned) time (NULL));
    }
  INCREMENT_PC();
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
      eval_error (eaccess_decl, syserrors_decl, IR_pos (cpc),
		  DERR_eaccess, function_name);
      break;
#endif
#ifdef EAGAIN
    case EAGAIN:
      eval_error (eagain_decl, syserrors_decl, IR_pos (cpc),
		  DERR_eagain, function_name);
      break;
#endif
#ifdef EBADF
    case EBADF:
      eval_error (ebadf_decl, syserrors_decl, IR_pos (cpc),
		  DERR_ebadf, function_name);
      break;
#endif
#ifdef EBUSY
    case EBUSY:
      eval_error (ebusy_decl, syserrors_decl, IR_pos (cpc), DERR_ebusy,
		  function_name);
      break;
#endif
#ifdef ECHILD
    case ECHILD:
      eval_error (echild_decl, syserrors_decl, IR_pos (cpc),
		  DERR_echild, function_name);
      break;
#endif
#ifdef EDEADLK
    case EDEADLK:
      eval_error (edeadlk_decl, syserrors_decl, IR_pos (cpc),
		  DERR_edeadlk, function_name);
      break;
#endif
#ifdef EDOM
    case EDOM:
      eval_error (edom_decl, syserrors_decl, IR_pos (cpc), DERR_edom,
		  function_name);
      break;
#endif
#ifdef EEXIST
    case EEXIST:
      eval_error (eexist_decl, syserrors_decl, IR_pos (cpc),
		  DERR_eexist, function_name);
      break;
#endif
#ifdef EFAULT
    case EFAULT:
      eval_error (efault_decl, syserrors_decl, IR_pos (cpc),
		  DERR_efault, function_name);
      break;
#endif
#ifdef EFBIG
    case EFBIG:
      eval_error (efbig_decl, syserrors_decl, IR_pos (cpc), DERR_efbig,
		  function_name);
      break;
#endif
#ifdef EINTR
    case EINTR:
      eval_error (eintr_decl, syserrors_decl, IR_pos (cpc), DERR_eintr,
		  function_name);
      break;
#endif
#ifdef EINVAL
    case EINVAL:
      eval_error (einval_decl, syserrors_decl, IR_pos (cpc),
		  DERR_einval, function_name);
      break;
#endif
#ifdef EIO
    case EIO:
      eval_error (eio_decl, syserrors_decl, IR_pos (cpc), DERR_eio,
		  function_name);
      break;
#endif
#ifdef EISDIR
    case EISDIR:
      eval_error (eisdir_decl, syserrors_decl, IR_pos (cpc),
		  DERR_eisdir, function_name);
      break;
#endif
#ifdef EMFILE
    case EMFILE:
      eval_error (emfile_decl, syserrors_decl, IR_pos (cpc),
		  DERR_emfile, function_name);
      break;
#endif
#ifdef EMLINK
    case EMLINK:
      eval_error (emlink_decl, syserrors_decl, IR_pos (cpc),
		  DERR_emlink, function_name);
      break;
#endif
#ifdef ENAMETOOLONG
    case ENAMETOOLONG:
      eval_error (enametoolong_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enametoolong, function_name);
      break;
#endif
#ifdef ENFILE
    case ENFILE:
      eval_error (enfile_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enfile, function_name);
      break;
#endif
#ifdef ENODEV
    case ENODEV:
      eval_error (enodev_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enodev, function_name);
      break;
#endif
#ifdef ENOENT
    case ENOENT:
      /* File or directory does not exist, or directory name is an empty
	 string. */
      eval_error (enoent_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enoent, function_name);
      break;
#endif
#ifdef ENOEXEC
    case ENOEXEC:
      eval_error (enoexec_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enoexec, function_name);
      break;
#endif
#ifdef ENOLCK
    case ENOLCK:
      eval_error (enolck_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enolck, function_name);
      break;
#endif
#ifdef ENOMEM
    case ENOMEM:
      eval_error (enomem_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enomem, function_name);
      break;
#endif
#ifdef ENOSPC
    case ENOSPC:
      eval_error (enospc_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enospc, function_name);
      break;
#endif
#ifdef ENOSYS
    case ENOSYS:
      eval_error (enosys_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enosys, function_name);
      break;
#endif
#ifdef ENOTDIR
    case ENOTDIR:
      /* This is not a directory. */
      eval_error (enotdir_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enotdir, function_name);
      break;
#endif
#ifdef ENOTEMPTY
#if defined(EEXIST) && EEXIST!=ENOTEMPTY
    case ENOTEMPTY:
      eval_error (enotempty_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enotempty, function_name);
      break;
#endif
#endif
#ifdef ENOTTY
    case ENOTTY:
      eval_error (enotty_decl, syserrors_decl, IR_pos (cpc),
		  DERR_enotty, function_name);
      break;
#endif
#ifdef ENXIO
    case ENXIO:
      eval_error (enxio_decl, syserrors_decl, IR_pos (cpc), DERR_enxio,
		  function_name);
      break;
#endif
#ifdef EPERM
    case EPERM:
      eval_error (eperm_decl, syserrors_decl, IR_pos (cpc), DERR_eperm,
		  function_name);
      break;
#endif
#ifdef EPIPE
    case EPIPE:
      eval_error (epipe_decl, syserrors_decl, IR_pos (cpc), DERR_epipe,
		  function_name);
      break;
#endif
#ifdef ERANGE
    case ERANGE:
      eval_error (erange_decl, syserrors_decl, IR_pos (cpc),
		  DERR_erange, function_name);
      break;
#endif
#ifdef EROFS
    case EROFS:
      eval_error (erofs_decl, syserrors_decl, IR_pos (cpc), DERR_erofs,
		  function_name);
      break;
#endif
#ifdef ESPIPE
    case ESPIPE:
      eval_error (espipe_decl, syserrors_decl, IR_pos (cpc),
		  DERR_espipe, function_name);
      break;
#endif
#ifdef ESRCH
    case ESRCH:
      eval_error (esrch_decl, syserrors_decl, IR_pos (cpc), DERR_esrch,
		  function_name);
      break;
#endif
#ifdef EXDEV
    case EXDEV:
      eval_error (exdev_decl, syserrors_decl, IR_pos (cpc), DERR_exdev,
		  function_name);
      break;
#endif
    default:
      /* We don't care does strerror exist or not because it is for
         errors.c. */
      assert (errno > 0);
      eval_error (syserror_decl, invcalls_decl, IR_pos (cpc),
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, PROCESS_ERRNO_NAME);
  if (pars_number == 0)
    name = "";
  else
    {
      to_vect_string_conversion (ctop, NULL);
      name = ER_pack_els (ER_vect (ctop));
    }
  if (errno)
    process_system_errors (name);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  assert (result != NULL);
  ER_SET_MODE (ctop, ER_NM_nil);
  INCREMENT_PC();
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
  size_t el_size;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, READDIR_NAME);
  to_vect_string_conversion (ctop, NULL);
  if (ER_NODE_MODE (ctop) != ER_NM_vect
      || ER_NODE_MODE (ER_vect (ctop)) != ER_NM_heap_pack_vect
      || ER_pack_vect_el_type (ER_vect (ctop)) != ER_NM_char)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
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
	eval_error (internal_decl, invcalls_decl, IR_pos (cpc),
		    DERR_internal_error, READDIR_NAME);
      if (closedir (dir) != 0)
	/* Internall error: EBADF and may be something else. */
	eval_error (internal_decl, invcalls_decl, IR_pos (cpc),
		    DERR_internal_error, READDIR_NAME);
      dir = opendir (ER_pack_els (ER_vect (ctop)));
      if (dir == NULL)
	process_system_errors (READDIR_NAME);
      else
	{
	  el_size = type_size_table [ER_NM_vect];
	  dir_files_number = i;
	  result = create_pack_vector (dir_files_number, ER_NM_vect);
	  ER_set_els_number (result, 0);
	  PUSH_TEMP_REF (result);
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
		eval_error (internal_decl, invcalls_decl, IR_pos (cpc),
			    DERR_internal_error, READDIR_NAME);
	      if (dirent == NULL)
		break;
	      vect = create_string (dirent->d_name);
	      result = GET_TEMP_REF (0);
	      assert (el_size == sizeof (ER_node_t));
	      memcpy (ER_pack_els (result) + el_size * i, &vect, el_size);
	      ER_set_els_number (result, i + 1);
	    }
	  POP_TEMP_REF (1);
	  if (closedir (dir) != 0)
	    /* Internall error: EBADF and may be something else. */
	    eval_error (internal_decl, invcalls_decl, IR_pos (cpc),
			DERR_internal_error, READDIR_NAME);
	}
    }
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  assert (result != NULL);
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, result);
  INCREMENT_PC();
}

static void
stat_start (int_t pars_number, const char *function_name, struct stat *buf)
{
  int result;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, function_name);
  to_vect_string_conversion (ctop, NULL);
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
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  if (result < 0)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      ER_SET_MODE (ctop, ER_NM_char);
      ER_set_ch (ctop, result);
    }
  INCREMENT_PC();
}

static void
stat_finish (int_t pars_number, int_t result)
{
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, result);
  INCREMENT_PC();
}

void
fun_call (int_t pars_number)
{
  struct stat buf;
  ER_node_t result;

  stat_start (pars_number, FUN_NAME, &buf);
#ifndef WIN32
  result = create_string (getpwuid (buf.st_uid)->pw_name);
#else
  result = create_string ("Unknown"); /* need something else for Win NT. */
#endif
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, result);
  INCREMENT_PC();
}

void
fgn_call (int_t pars_number)
{
  struct stat buf;
  ER_node_t result;
  
  stat_start (pars_number, FGN_NAME, &buf);
#ifndef WIN32
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
#else
  result = create_string ("Unknown"); /* need something else for Win NT. */
#endif
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, result);
  INCREMENT_PC();
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

  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  vect = create_string (result);
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, TIME_NAME);
  t = time (NULL);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, t);
  INCREMENT_PC();
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, STRTIME_NAME);
  if (pars_number == 2)
    {
      implicit_int_conversion (0);
      if (ER_NODE_MODE (ctop) != ER_NM_int)
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		    DERR_parameter_type, STRTIME_NAME);
      t = ER_i (ctop);
    }
  else
    t = time (NULL);
  if (pars_number >= 1)
    {
      format_var = (pars_number == 1 ? ctop : below_ctop);
      to_vect_string_conversion (format_var, NULL);
      if (ER_NODE_MODE (format_var) == ER_NM_vect
	  && ER_NODE_MODE (ER_vect (format_var)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (ER_vect (format_var)) == ER_NM_char)
	format = ER_pack_els (ER_vect (format_var));
      else
	eval_error (partype_decl, invcalls_decl,
		    IR_pos (cpc), DERR_parameter_type, STRTIME_NAME);
    }
  else
    {
      format_var = INDEXED_VAL (ER_stack_vars (uppest_stack),
				IR_var_number_in_block (time_format_decl));
      to_vect_string_conversion (format_var, NULL);
      if (ER_NODE_MODE (format_var) == ER_NM_vect
	  && ER_NODE_MODE (ER_vect (format_var)) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (ER_vect (format_var)) == ER_NM_char)
	format = ER_pack_els (ER_vect (format_var));
      else
	eval_error (invenvar_decl, invcalls_decl,
		    IR_pos (cpc), DERR_corrupted_environment_var,
		    TIME_FORMAT_NAME);
    }
  tm = localtime (&t);
  for (percents_number = 0, str = format; *str != 0; str++)
    if (*str == '%')
      percents_number++;
  max = strlen (format) + 2 + percents_number * 10;
  vect = create_empty_string (max);
  strftime (ER_pack_els (vect), max, format, tm);
  ER_set_els_number (vect, strlen (ER_pack_els (vect)));
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_vect);
  ER_set_vect (ctop, vect);
  INCREMENT_PC();
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
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, CLOCK_NAME);
  secs = (floating_t) (clock () - start_time) / CLOCKS_PER_SECOND;
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_float);
  ER_set_f (ctop, secs);
  INCREMENT_PC();
}

void
gc_call (int_t pars_number)
{
  floating_t result;

  if (pars_number != 0)
    eval_error (parnumber_decl, invcalls_decl,
		IR_pos (cpc), DERR_parameters_number, GC_NAME);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  gc ();
  /* Place the free memory instead of the function. */
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, free_heap_memory);
  INCREMENT_PC();
}

void
system_call (int_t pars_number)
{
  int code;
  int error_flag;
  ER_node_t val;
  ER_node_t vect;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, SYSTEM_NAME);
  else
    {
      val = INDEXED_VAL (ER_CTOP (), -pars_number + 1);
      if (ER_NODE_MODE (val) != ER_NM_vect)
	error_flag = TRUE;
      else
	{
	  to_vect_string_conversion (val, NULL);
	  vect = ER_vect (val);
	  if (ER_NODE_MODE (vect) != ER_NM_heap_pack_vect
	      || ER_pack_vect_el_type (vect) != ER_NM_char)
	    error_flag = TRUE;
	  else
	    {
	      code = system ((char *) ER_pack_els (vect));
	      if (code == 127)
		eval_error (noshell_decl, systemcalls_decl,
			    IR_pos (cpc), DERR_no_shell, SYSTEM_NAME);
	      else if (code < 0)
		eval_error (systemfail_decl, systemcalls_decl,
			    IR_pos (cpc), DERR_other_fail_in_system_call,
			    SYSTEM_NAME);
	      error_flag = FALSE;
	    }
	}
      if (error_flag)
	eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		    DERR_parameter_type, SYSTEM_NAME);
    }
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, code);
  INCREMENT_PC();
}

void
exit_call (int_t pars_number)
{
  int code;
  int error_flag;
  ER_node_t val;
  ER_node_t vect;

  if (pars_number != 1)
    eval_error (parnumber_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameters_number, EXIT_NAME);
  implicit_int_conversion (0);
  if (ER_NODE_MODE (ctop) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (cpc),
		DERR_parameter_type, EXIT_NAME);
  dino_finish (ER_i (ctop));
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, code);
  INCREMENT_PC();
}

/* This function is a trick to fullfil initiations after execution of
   stmts before __init__ call. */
void
init_call (int_t pars_number)
{
  ER_node_t instance;
  ER_node_t var;

  assert (pars_number == 0);
  /* Pop all actual parameters. */
  DECR_CTOP (pars_number);
  SET_TOP;
  /* ------ Initiations after execution of stmts before __init__ ----- */
  /* Set stdin, stdout, stderr. */
  instance = ER_instance (INDEXED_VAL (ER_stack_vars (cstack),
				       IR_var_number_in_block (stdin_decl)));
  var = INDEXED_VAL (ER_instance_vars (instance),
		     IR_var_number_in_block (file_ptr_decl));
  ER_SET_MODE (var, ER_NM_hide);
  ER_set_hide (var, stdin);
  instance = ER_instance (INDEXED_VAL (ER_stack_vars (cstack),
				       IR_var_number_in_block (stdout_decl)));
  var = INDEXED_VAL (ER_instance_vars (instance),
		     IR_var_number_in_block (file_ptr_decl));
  ER_SET_MODE (var, ER_NM_hide);
  ER_set_hide (var, stdout);
  instance = ER_instance (INDEXED_VAL (ER_stack_vars (cstack),
				       IR_var_number_in_block (stderr_decl)));
  var = INDEXED_VAL (ER_instance_vars (instance),
		     IR_var_number_in_block (file_ptr_decl));
  ER_SET_MODE (var, ER_NM_hide);
  ER_set_hide (var, stderr);
  /* ----- End of the initiations ----- */
  /* Place the result instead of the function. */
  ER_SET_MODE (ctop, ER_NM_nil);
  INCREMENT_PC();
}

static void
call_external_func (int pars_number, IR_node_t func_node)
{
  external_func_t *func;
  ER_node_t vect;
  ER_node_t val;
  ER_node_t v;
  ER_node_t tab;
  int curr_actual;

  func = external_address (func_node);
  vect = (ER_node_t) create_unpack_vector (pars_number);
  for (curr_actual = 0; curr_actual < pars_number; curr_actual++)
    {
      val = INDEXED_VAL (ER_CTOP (), curr_actual - pars_number + 1);
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
      *(val_t *) INDEXED_VAL (ER_unpack_els (vect), curr_actual)
	= *(val_t *) val;
    }
  DECR_CTOP (pars_number);
  /* Pop all actual parameters. */
  SET_TOP;
  no_gc_flag = TRUE;
  *(val_t *) ctop = (*func) (pars_number,
			     (val_t *) INDEXED_VAL (ER_unpack_els (vect),
						    0));
  no_gc_flag = FALSE;
  INCREMENT_PC();
}

static ER_node_t
create_instance (int_t pars_number)
{
  size_t curr_actual;
  IR_node_t class; 
  ER_node_t instance;
  ER_node_t curr_var;
  ER_node_t vars;
  int curr_par;
  char *free;
  
  class = ER_class ((ER_node_t) INDEXED_VAL (ER_CTOP (), -pars_number));
  instance = (ER_node_t) heap_allocate (instance_size (class), FALSE);
  ER_SET_MODE (instance, ER_NM_heap_instance);
  ER_set_class (instance, class);
  ER_set_block_node (instance, IR_next_stmt (class));
  ER_set_immutable (instance, FALSE);
  /* Set Context chain. */
  ER_set_context (instance, ER_class_context (INDEXED_VAL (ER_CTOP (),
							   -pars_number)));
  ER_set_context_number (instance, context_number);
  ER_set_state (instance, IS_initial);
  context_number++;
  /* Initialize object variables. */
  free = ((char *) ER_instance_vars (instance)
          + IR_vars_number (IR_next_stmt (class)) * sizeof (val_t));
  /* Seting up mode of all permanent stack vars as nil. */
  for (curr_var = ER_instance_vars (instance);
       (char *) curr_var < free;
       curr_var = INDEXED_VAL (curr_var, 1))
    ER_SET_MODE (curr_var, ER_NM_nil);
  /* Push actual parameters except for args (but no more than
     formal parameters). */
  vars = ER_instance_vars (instance);
  for (curr_actual = 0, curr_par = -pars_number + 1;
       curr_actual < (IR_parameters_number (class)
		      - (IR_args_flag (class) ? 1 : 0))
	 && curr_actual < pars_number;
       curr_actual++, curr_par++)
    *(val_t *) INDEXED_VAL (vars, curr_actual)
      = *(val_t *) INDEXED_VAL (ER_CTOP (), curr_par);
  if (IR_args_flag (class))
    {
      ER_node_t vect;
      size_t start;
      
      PUSH_TEMP_REF (instance);
      if (pars_number == curr_actual)
	vect = create_empty_vector ();
      else
	vect = (ER_node_t) create_unpack_vector (pars_number - curr_actual);
      instance = GET_TEMP_REF (0);
      POP_TEMP_REF (1);
      /* Args number */
      ER_SET_MODE (INDEXED_VAL (ER_instance_vars (instance),
				IR_parameters_number (class) - 1), ER_NM_vect);
      ER_set_vect (INDEXED_VAL (ER_instance_vars (instance),
				IR_parameters_number (class) - 1), vect);
      for (start = curr_actual;
	   curr_actual < pars_number;
	   curr_actual++)
	*(val_t *) INDEXED_VAL (ER_unpack_els (vect), curr_actual - start)
	  = *(val_t *) INDEXED_VAL (ER_CTOP (),
				    curr_actual - pars_number + 1);
    }
  /* Pop all actual parameters and class from current stack. */
  DECR_CTOP (pars_number + 1);
  SET_TOP;
  return instance;
}

/* The following varibale is PC of the last call of real DINO function
   (not external or implementation function).  It is used to
   diagnostic of earley parser functions. */
static pc_t real_func_call_pc;

void
process_func_class_call (int_t pars_number)
{
  size_t curr_actual;
  IR_node_t func_class;
  IR_node_t class;
  ER_node_t func_class_val;
  ER_node_t instance;
  ER_node_t vars, pars;
  int curr_par;

  func_class_val = INDEXED_VAL (ER_CTOP (), -pars_number);
  if (ER_NODE_MODE (func_class_val) == ER_NM_class)
    {
      /* See also case with IR_NM_block_finish .*/
      func_class = ER_class (func_class_val);
      instance = create_instance (pars_number);
      PUSH_TEMP_REF (instance);
      heap_push (IR_next_stmt (func_class), GET_TEMP_REF (0));
      /* Zeroth val of class block is always corresponding
	 instance. */
      ER_SET_MODE (INDEXED_VAL (ER_stack_vars (cstack), 0), ER_NM_instance);
      ER_set_instance (INDEXED_VAL (ER_stack_vars (cstack), 0),
                       GET_TEMP_REF (0));
      POP_TEMP_REF (1);
      cpc = IR_next_pc (IR_next_stmt (func_class));
    }
  else if (ER_NODE_MODE (func_class_val) == ER_NM_func)
    {
      func_class = ER_func (func_class_val);
      if (IR_IS_OF_TYPE (func_class, IR_NM_external_func))
	call_external_func (pars_number, func_class);
      else if (IR_implementation_func (func_class) != NULL)
	(*IR_implementation_func (func_class)) (pars_number);
      else
	{
	  real_func_call_pc = cpc;
	  heap_push (IR_next_stmt (func_class),
		     ER_func_context (func_class_val));
	  /* The value may be changed because of GC. */
	  func_class_val
	    = INDEXED_VAL (ER_ctop (ER_prev_stack (cstack)), -pars_number);
	  /* Push actual parameters except for args (but no more than
	     formal parameters). */
          vars = ER_stack_vars (cstack);
          pars = (ER_node_t) ER_ctop (ER_prev_stack (cstack));
	  for (curr_actual = 0, curr_par = -pars_number + 1;
	       curr_actual < (IR_parameters_number (func_class)
			      - (IR_args_flag (func_class) ? 1 : 0))
		 && curr_actual < pars_number;
	       curr_actual++, curr_par++)
	    *(val_t *) INDEXED_VAL (vars, curr_actual)
	      = *(val_t *) INDEXED_VAL (pars, curr_par);
	  if (IR_args_flag (func_class))
	    {
              ER_node_t vect;
	      size_t start;
	      
	      if (pars_number == curr_actual)
		vect = create_empty_vector ();
	      else
		vect = create_unpack_vector (pars_number - curr_actual);
	      /* Args number */
	      ER_SET_MODE (INDEXED_VAL (ER_stack_vars (cstack),
                                        IR_parameters_number (func_class) - 1),
                           ER_NM_vect);
	      ER_set_vect (INDEXED_VAL (ER_stack_vars (cstack),
                                        IR_parameters_number (func_class) - 1),
			   vect);
	      for (start = curr_actual;
                   curr_actual < pars_number;
		   curr_actual++)
		*(val_t *) INDEXED_VAL (ER_unpack_els (vect),
                                        curr_actual - start)
		  = *(val_t *) INDEXED_VAL (ER_ctop (ER_prev_stack (cstack)),
                                            curr_actual - pars_number + 1);
	    }
	  /* Pop all actual parameters and func from previous stack. */
	  DECR_TOP (ER_prev_stack (cstack), pars_number + 1);
	  cpc = IR_next_pc (IR_next_stmt (func_class));
	  if (IR_thread_flag (func_class))
	    {
	      ER_node_t process;
	      
	      process = create_process (cpc, func_class,
					ER_func_context (func_class_val));
	      cpc = ER_return_pc (cstack);
#ifndef NO_OPTIMIZE
	      ER_set_ctop (cstack, (char *) ctop);
#endif
	      cstack = ER_prev_stack (cstack);
	      ER_set_saved_cstack (cprocess, cstack);
#ifndef NO_OPTIMIZE
	      ctop = (ER_node_t) ER_ctop (cstack);
#endif
	      SET_TOP;
	      TOP_UP;
	      ER_SET_MODE (ctop, ER_NM_process);
              ER_set_process (ctop, process);
	    }
	}
    }
  else
    eval_error (callop_decl, invcalls_decl, IR_pos (cpc),
		DERR_none_class_or_func_before_left_bracket);
}

void
initiate_funcs (void)
{
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

  par1 = INDEXED_VAL (ER_CTOP (), -2);
  implicit_arithmetic_conversion (1);
  par2 = INDEXED_VAL (ER_CTOP (), -1);
  par3 = INDEXED_VAL (ER_CTOP (), 0);
  assert (npars == 3 && ER_NODE_MODE (par1) == ER_NM_hide);
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
    eval_error (partype_decl, invcalls_decl, IR_pos (real_func_call_pc),
		DERR_parameter_type, name);
  g = (struct grammar *) ER_hide (par1);
  code = earley_parse_grammar (g, ER_i (par2), ER_pack_els (ER_vect (par3)));
  if (code == EARLEY_NO_MEMORY)
    eval_error (pmemory_decl, invparsers_decl, IR_pos (real_func_call_pc),
		"run time error (%s) -- no parser memory", name);
  else if (code != 0)
    eval_error (invgrammar_decl, invparsers_decl, IR_pos (real_func_call_pc),
		"run time error (%s) -- %s", name, earley_error_message (g));
  /* Returned value will be ignored. */
  DECR_CTOP (npars);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_nil);
  INCREMENT_PC();
}

/* The following function implements function set_debug in class
   parser. */
void
int_earley_set_debug_level (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_debug";

  par1 = INDEXED_VAL (ER_CTOP (), -1);
  implicit_arithmetic_conversion (0);
  par2 = INDEXED_VAL (ER_CTOP (), 0);
  assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (real_func_call_pc),
		DERR_parameter_type, name);
  i = earley_set_debug_level ((struct grammar *) ER_hide (par1),
			      ER_i (par2));
  DECR_CTOP (npars);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, i);
  INCREMENT_PC();
}

/* The following function implements function set_one_parse in class
   parser. */
void
int_earley_set_one_parse_flag (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_one_parse";

  par1 = INDEXED_VAL (ER_CTOP (), -1);
  implicit_arithmetic_conversion (0);
  par2 = INDEXED_VAL (ER_CTOP (), 0);
  assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (real_func_call_pc),
		DERR_parameter_type, name);
  i = earley_set_one_parse_flag ((struct grammar *) ER_hide (par1),
				 ER_i (par2));
  DECR_CTOP (npars);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, i);
  INCREMENT_PC();
}

/* The following function implements function set_lookahead in class
   parser. */
void
int_earley_set_lookahead_level (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_lookahead";

  par1 = INDEXED_VAL (ER_CTOP (), -1);
  implicit_arithmetic_conversion (0);
  par2 = INDEXED_VAL (ER_CTOP (), 0);
  assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (real_func_call_pc),
		DERR_parameter_type, name);
  i = ER_i (par2);
  i = earley_set_lookahead_level ((struct grammar *) ER_hide (par1),
				  i ? 1 : 0);
  DECR_CTOP (npars);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, i);
  INCREMENT_PC();
}

/* The following function implements function set_cost in class
   parser. */
void
int_earley_set_cost_flag (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_cost";

  par1 = INDEXED_VAL (ER_CTOP (), -1);
  implicit_arithmetic_conversion (0);
  par2 = INDEXED_VAL (ER_CTOP (), 0);
  assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (real_func_call_pc),
		DERR_parameter_type, name);
  i = earley_set_cost_flag ((struct grammar *) ER_hide (par1), ER_i (par2));
  DECR_CTOP (npars);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, i);
  INCREMENT_PC();
}

/* The following function implements function set_recovery in class
   parser. */
void
int_earley_set_error_recovery_flag (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_recovery";

  par1 = INDEXED_VAL (ER_CTOP (), -1);
  implicit_arithmetic_conversion (0);
  par2 = INDEXED_VAL (ER_CTOP (), 0);
  assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (real_func_call_pc),
		DERR_parameter_type, name);
  i = earley_set_error_recovery_flag ((struct grammar *)
				      ER_hide (par1),
				      ER_i (par2));
  DECR_CTOP (npars);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, i);
  INCREMENT_PC();
}

/* The following function implements function set_recovery_match in
   class parser. */
void
int_earley_set_recovery_match (int npars)
{
  int i;
  ER_node_t par1, par2;
  const char *name = "set_recovery_match";

  par1 = INDEXED_VAL (ER_CTOP (), -1);
  implicit_arithmetic_conversion (0);
  par2 = INDEXED_VAL (ER_CTOP (), 0);
  assert (npars == 2 && ER_NODE_MODE (par1) == ER_NM_hide);
  if (ER_NODE_MODE (par2) != ER_NM_int)
    eval_error (partype_decl, invcalls_decl, IR_pos (real_func_call_pc),
		DERR_parameter_type, name);
  i = earley_set_recovery_match ((struct grammar *) ER_hide (par1),
				 ER_i (par2));
  DECR_CTOP (npars);
  SET_TOP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, i);
  INCREMENT_PC();
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
  int n;

  assert (ER_NODE_MODE (tokens_vect) == ER_NM_heap_pack_vect
	  && ER_pack_vect_el_type (tokens_vect) == ER_NM_instance);
  if ((unsigned_int_t) curr_token >= ER_els_number (tokens_vect))
    return -1;
  tok = *attr = ((ER_node_t *) ER_pack_els (tokens_vect)) [curr_token];
  if (ER_class (tok) != token_decl)
    eval_error (invtoken_decl, invparsers_decl, IR_pos (real_func_call_pc),
		"run time error (parse) -- invalid token #%d", curr_token);
  curr_token++;
  n = IR_var_number_in_block (code_decl);
  code = INDEXED_VAL (ER_instance_vars (tok), n);
  if (ER_NODE_MODE (code) != ER_NM_int)
    eval_error (invtoken_decl, invparsers_decl, IR_pos (real_func_call_pc),
		"run time error (parse) -- invalid code of token #%d",
		curr_token - 1);
  return ER_i (code);
}

/* The following is DINO error function called by parser and its
   context. */
static IR_node_t error_func;
static ER_node_t error_func_context;

/* The following function is interface to DINO error function.  We
   need to provide at least 6 temporary variables (see trick for this
   in environment). */
static void
init_syntax_token (int err_tok_num, void *err_tok_attr,
		   int start_ignored_tok_num, void *start_ignored_tok_attr,
		   int start_recovered_tok_num, void *start_recovered_tok_attr)
{
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_func);
  ER_set_func (ctop, error_func);
  ER_set_func_context (ctop, error_func_context);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, err_tok_num);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_instance);
  ER_set_instance (ctop, err_tok_attr);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, start_ignored_tok_num);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_instance);
  ER_set_instance (ctop, start_ignored_tok_attr);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_int);
  ER_set_i (ctop, start_recovered_tok_num);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_instance);
  ER_set_instance (ctop, start_recovered_tok_attr);
  call_func_class (6);
  TOP_DOWN; /* reject the result. */
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

  return (unsigned) node->tree_node;
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
  ER_SET_MODE (ctop, ER_NM_class);
  ER_set_class (ctop, anode_decl);
  ER_set_class_context (ctop, uppest_stack);
  TOP_UP;
  ER_SET_MODE (ctop, ER_NM_vect);
  switch (root->type)
    {
    case EARLEY_NIL:
    case EARLEY_ERROR:
      var = INDEXED_VAL (ER_stack_vars (uppest_stack),
			 IR_var_number_in_block (root->type == EARLEY_NIL
						 ? nil_anode_decl
						 : error_anode_decl));
      assert (ER_NODE_MODE (var) == ER_NM_instance);
      res = ER_instance (var);
      break;
    case EARLEY_TERM:
      name_vect = create_string ("$term");
      ER_set_vect (ctop, name_vect);
      TOP_UP;
      ER_SET_MODE (ctop, ER_NM_instance);
      assert (ER_NODE_MODE ((ER_node_t) root->val.term.attr)
	      == ER_NM_heap_instance);
      ER_set_instance (ctop, root->val.term.attr);
      res = create_instance (2);
      break;
    case EARLEY_ANODE:
      name_vect = create_string (root->val.anode.name);
      ER_set_vect (ctop, name_vect);
      for (i = 0; root->val.anode.children [i] != NULL; i++)
	;
      vect = create_empty_vector ();
      ER_set_pack_vect_el_type (vect, ER_NM_instance);
      vect = expand_vector (vect, i);
      TOP_UP;
      ER_SET_MODE (ctop, ER_NM_vect);
      ER_set_vect (ctop, vect);
      res = create_instance (2);
      break;
    case EARLEY_ALT:
      name_vect = create_string ("$alt");
      ER_set_vect (ctop, name_vect);
      for (i = 0, alt = root; alt != NULL; alt = alt->val.alt.next, i++)
	;
      vect = create_empty_vector ();
      ER_set_pack_vect_el_type (vect, ER_NM_instance);
      vect = expand_vector (vect, i);
      TOP_UP;
      ER_SET_MODE (ctop, ER_NM_vect);
      ER_set_vect (ctop, vect);
      res = create_instance (2);
      break;
    default:
      assert (FALSE);
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
  int i, code, ambiguous_p;
  struct earley_tree_node *root;
  ER_node_t par1, par2, par3, v;
  ER_node_t instance, var;
  const char *name = "parse";
  position_t *save_pos_ptr;

  par1 = INDEXED_VAL (ER_CTOP (), -2);
  par2 = INDEXED_VAL (ER_CTOP (), -1);
  par3 = INDEXED_VAL (ER_CTOP (), 0);
  assert (npars == 3 && ER_NODE_MODE (par1) == ER_NM_hide);
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
      || (ER_pack_vect_el_type (ER_vect (par2))
	  != ER_NM_instance)
      || ER_NODE_MODE (par3) != ER_NM_func)
    eval_error (partype_decl, invcalls_decl, IR_pos (real_func_call_pc),
		DERR_parameter_type, name);
  /* We switch off GC because the parser may call error function
     several times and parser has references to tokens in the heap. */
  no_gc_flag = TRUE;
  tokens_vect = ER_vect (par2);
  curr_token = 0;
  error_func = ER_func (par3);
  error_func_context = ER_func_context (par3);
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
      no_gc_flag = FALSE;
      delete_hash_table (tree_heap_tab);
      OS_DELETE (tree_mem_os);
      eval_error (pmemory_decl, invparsers_decl, IR_pos (real_func_call_pc),
		  "run time error (%s) -- no parser memory", name);
    }
  else if (code == EARLEY_UNDEFINED_OR_BAD_GRAMMAR)
    {
      no_gc_flag = FALSE;
      delete_hash_table (tree_heap_tab);
      OS_DELETE (tree_mem_os);
      eval_error (invgrammar_decl, invparsers_decl, IR_pos (real_func_call_pc),
		  "run time error (%s) -- %s", name, earley_error_message (g));
    }
  else if (code == EARLEY_INVALID_TOKEN_CODE)
    {
      no_gc_flag = FALSE;
      delete_hash_table (tree_heap_tab);
      OS_DELETE (tree_mem_os);
      eval_error (invtoken_decl, invparsers_decl, IR_pos (real_func_call_pc),
		  "run time error (%s) -- %s", name, earley_error_message (g));
    }
  else
    assert (code == 0);
  /* Set up ambiguous_p. */
  instance = ER_context (cstack);
  assert (instance != NULL && ER_NODE_MODE (instance) == ER_NM_heap_instance
	  && ER_class (instance) == parser_decl);
  var = INDEXED_VAL (ER_instance_vars (instance),
		     IR_var_number_in_block (ambiguous_p_decl));
  ER_SET_MODE (var, ER_NM_int);
  ER_set_i (var, ambiguous_p);
  DECR_CTOP (npars);
  SET_TOP;
  if (root == NULL)
    ER_SET_MODE (ctop, ER_NM_nil);
  else
    {
      /* Translation into heap: */
      instance = tree_to_heap (root);
      assert (ER_NODE_MODE (instance) == ER_NM_heap_instance);
      ER_SET_MODE (ctop, ER_NM_instance);
      ER_set_instance (ctop, instance);
    }
  no_gc_flag = FALSE;
  delete_hash_table (tree_heap_tab);
  OS_DELETE (tree_mem_os);
  INCREMENT_PC();
}

/* The following function is used to initiate class parser. */
void
int_earley_create_grammar (int npars)
{
  struct grammar *g;
  int code;

  assert (npars == 0);
  g = earley_create_grammar ();
  if (g == NULL)
    eval_error (pmemory_decl, invparsers_decl, IR_pos (real_func_call_pc),
		"run time error (parser) -- no parser memory");
  ER_SET_MODE (ctop, ER_NM_hide);
  ER_set_hide (ctop, g);
  INCREMENT_PC();
}
