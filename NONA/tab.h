/*
   FILE NAME:   tab.h

   TITLE:       Include file of NONA (code selector description translator)
                tables

   DESCRIPTION: This header file contains ANSI C prototype definitions of
                external functions which implement the following tables
                      table of identifiers
                      table of single declarations (of terminals
                                                    or noterminals)

   SPECIAL CONSIDERATION:
       This file can not be included repeatedly.

*/

#include "ird.h"

/* Table of identifiers. */

extern char *insert_identifier (const char *identifier);
extern void initiate_identifier_table (void);
extern void finish_identifier_table (void);

/* Table of single declarations. */

extern IR_node_t insert_single_declaration (IR_node_t single_declaration);
extern IR_node_t find_single_declaration (IR_node_t identifier);
extern void initiate_single_declaration_table (void);
extern void finish_single_declaration_table (void);
