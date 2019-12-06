struct str_code {
  const char *name;
  int code;
};

typedef struct str_code *str_code_t;

DEF_HTAB (str_code_t);

static htab_hash_t str_code_hash (str_code_t el) {
  return dino_hash (el->name, strlen (el->name), 42);
}
static int str_code_eq (str_code_t el1, str_code_t el2) {
  return strcmp (el1->name, el2->name) == 0;
}

static HTAB (str_code_t) * create_str_code_tab (str_code_t els, size_t nels) {
  str_code_t tab_el;
  HTAB (str_code_t) * str_code_tab;

  HTAB_CREATE (str_code_t, str_code_tab, nels * 2, str_code_hash, str_code_eq);
  for (size_t i = 0; i < nels; i++)
    if (HTAB_DO (str_code_t, str_code_tab, &els[i], HTAB_INSERT, tab_el)) assert (FALSE);
  return str_code_tab;
}

static void finish_str_code_tab (HTAB (str_code_t) * str_code_tab) {
  HTAB_DESTROY (str_code_t, str_code_tab);
}

static int find_str_code (HTAB (str_code_t) * str_code_tab, const char *name, int not_found_code) {
  struct str_code el;
  str_code_t tab_el;

  el.name = name;
  if (HTAB_DO (str_code_t, str_code_tab, &el, HTAB_FIND, tab_el)) return tab_el->code;
  return not_found_code;
}
