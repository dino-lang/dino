#include "d_common.h"
#include "d_ir.h"
#include "d_blocktab.h"


struct block_decl_idents_tables block_decl_idents_tables;

/* This func is to called only one before any work with this abstract
   data. */
void
initiate_blocks_table (void)
{
  block_decl_idents_tables.idents_number = 0;
  VLO_CREATE (block_decl_idents_tables.blocks_decls, 2000);
}

/* The func creates new block decls idents table in this abstract data
   and returns order number (0, ...) of this table (of block in other
   words). */
int
new_block (void)
{
  int new_block_number;
  vlo_t block_decls;
  
  new_block_number = BLOCKS_NUMBER ();
  VLO_CREATE (block_decls, 0);
  VLO_ADD_MEMORY (block_decl_idents_tables.blocks_decls,
		  (char *) &block_decls, sizeof (vlo_t));
  return new_block_number;
}

/* The func processes ident used for access to block decl.
   If IDENT is not processed early then set up its (more accurately
   corresponding unique_ident_node) member
   block_decl_ident_number and block decls idents
   tables will contain element for IDENT (it is to be not NULL). */
void
process_block_decl_ident (IR_node_t ident)
{
  if (IR_block_decl_ident_number (IR_unique_ident (ident)) < 0)
    {
      IR_set_block_decl_ident_number
	(IR_unique_ident (ident), block_decl_idents_tables.idents_number);
      block_decl_idents_tables.idents_number++;
    }
}

/* The func sets up elements values of block decls idents tables for
   given DECL (in block BLOCK_REF).  Both values are to be not NULL.
   The sequence of the abstract data funcs calls may be described
   regular expr:
   initiate_blocks_table
      (
       (new_block | process_block_decl_ident)*
       define_block_decl*
      )* */
void
define_block_decl (IR_node_t decl, IR_node_t block_ref)
{
  IR_node_t null = NULL;
  vlo_t *table_ref;
  int block_number, block_decl_ident_number, i;

  block_number = IR_block_number (block_ref);
  block_decl_ident_number
    = (IR_block_decl_ident_number (IR_unique_ident (IR_ident (decl))));
  if (block_decl_ident_number < 0)
    /* There is no access to identifier. */
    return;
  table_ref = (&LV_BLOCK_DECLS_TABLE (block_number));
  if (VLO_LENGTH (*table_ref) <= block_decl_ident_number * sizeof (IR_node_t))
    for (i = VLO_LENGTH (*table_ref) / sizeof (IR_node_t);
	 i <= block_decl_ident_number;
	 i++)
      VLO_ADD_MEMORY (*table_ref, (char *)&null, sizeof (IR_node_t));
  ((IR_node_t *) VLO_BEGIN (*table_ref)) [block_decl_ident_number] = decl;
  IR_set_it_is_declared_in_block (IR_unique_ident (IR_ident (decl)), TRUE);
}

void
finish_blocks_table (void)
{
  vlo_t *vlo_ref;

  for (vlo_ref = VLO_BEGIN (block_decl_idents_tables.blocks_decls);
       (char *) vlo_ref
	 <= (char *) VLO_END (block_decl_idents_tables.blocks_decls);
       vlo_ref++)
    VLO_DELETE (*vlo_ref);
  VLO_DELETE (block_decl_idents_tables.blocks_decls);
}
