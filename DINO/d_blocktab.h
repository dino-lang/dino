/* The following implements abstract data block_decl_idents_tables.
   This abstract data mainly serves for access to block decl nodes by
   block_number and block_decl_ident_number (see commentaries for
   block_node and unique_ident_node).  The abstract data is designed and
   implemented for implementation dynamic load another file in
   the future (see commentaries for func define_block_decl).*/

struct block_decl_idents_tables
{
  /* The following VLO contains VLO'es in which the pointers to block
     decls stored. */
  vlo_t blocks_decls;
  /* The value of following var is modified by func
     process_block_decl_unique_ident.  As the result of this the value is
     maximal number of idents used for access to decls of blocks after
     processing all stmts. */
  int idents_number;
};

extern struct block_decl_idents_tables block_decl_idents_tables;

/* The macro call value is number of blocks in
   block_decl_idents_tables. */

#define BLOCKS_NUMBER()\
  (VLO_LENGTH (block_decl_idents_tables.blocks_decls) / sizeof (vlo_t))

/* The macro call value is block decls idents table (represented
   by VLO) for the block with number BLOCK_NUMBER.  The macro call value is
   l-value. */

#define LV_BLOCK_DECLS_TABLE(block_number)\
  (((vlo_t *) VLO_BEGIN (block_decl_idents_tables.blocks_decls)) [block_number])

/* The macro call value is block decls idents table element for the
   block with number BLOCK_NUMBER and ident BLOCK_DECL_IDENT_NUMBER.
   The element value is NULL if there is not decl with given ident in
   given block.  Otherwise this value is pointer to node representing
   decl with given ident in given block.  BLOCK_NUMBER is to be in
   range 0..BLOCKS_NUMBER () - 1.  BLOCK_DECL_IDENT_NUMBER is to be in
   range 0..block_decl_idents_tables.idents_number-1 (see commentaries
   for field block_decl_ident_number in unique_ident_node).  Also
   order of abstract data operations calls is to be correct (see
   commentaries for func define_block_decl). */

#define LV_BLOCK_DECL(block_number, block_decl_ident_number)\
  (block_decl_ident_number * sizeof (IR_node_t)\
   < VLO_LENGTH (LV_BLOCK_DECLS_TABLE (block_number))\
   ? (((IR_node_t *) VLO_BEGIN (LV_BLOCK_DECLS_TABLE (block_number)))\
      [block_decl_ident_number])\
   : NULL)

extern void initiate_blocks_table (void);
extern int new_block (void);
extern void process_block_decl_unique_ident (IR_node_t unique_ident);
extern void define_block_decl (IR_node_t decl, IR_node_t block_ref);
extern void finish_blocks_table (void);
