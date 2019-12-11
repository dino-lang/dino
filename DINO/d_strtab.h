struct str_code {
  const char *name;
  int code;
};

typedef const struct str_code *str_code_t;

static unsigned str_code_hash (const void *sc) {
  str_code_t el = sc;

  return mum_hash (el->name, strlen (el->name), 42);
}
static int str_code_eq (const void *sc1, const void *sc2) {
  str_code_t el1 = sc1, el2 = sc2;

  return strcmp (el1->name, el2->name) == 0;
}

static hash_table_t create_str_code_tab (str_code_t els, size_t nels) {
  str_code_t *table_entry_pointer;
  hash_table_t str_code_tab;

  str_code_tab = create_hash_table (nels * 2, str_code_hash, str_code_eq);
  for (size_t i = 0; i < nels; i++) {
    table_entry_pointer
      = (str_code_t *) find_hash_table_entry (str_code_tab, (hash_table_entry_t) &els[i], TRUE);
    assert (*table_entry_pointer == NULL);
    *table_entry_pointer = &els[i];
  }
  return str_code_tab;
}

static void finish_str_code_tab (hash_table_t str_code_tab) { delete_hash_table (str_code_tab); }

static int find_str_code (hash_table_t str_code_tab, const char *name, int not_found_code) {
  str_code_t *table_entry_pointer;
  struct str_code el;
  str_code_t tab_el;

  el.name = name;
  table_entry_pointer
    = (str_code_t *) find_hash_table_entry (str_code_tab, (hash_table_entry_t) &el, FALSE);
  return (tab_el = *table_entry_pointer) == NULL ? not_found_code : tab_el->code;
}
