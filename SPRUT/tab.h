/*
   FILE NAME:   tab.h

   TITLE:       Include file of SPRUT (internal representation description
                translator) tables

   DESCRIPTION: This header file contains ANSI C prototype definitions of
                external functions which implement the following tables
                      table of identifiers
                      table of double declaration identifiers
                      table of predefined types and node types
                      table of fields           (key is the field identifier)
                      table of node type fields (key is the field identifier
                                                 and the node type identifiers)

   SPECIAL CONSIDERATION:
         This file can not be included repeatedly.
*/

#include "ird.h"

/* Table of identifiers. */

extern char *insert_identifier (const char *identifier);
extern void initiate_identifier_table (void);
extern void finish_identifier_table (void);

/* Table of double declaration identifiers. */

extern IR_node_t insert_double_declaration_identifier (IR_node_t identifier);
extern IR_node_t find_double_declaration_identifier (IR_node_t identifier);
extern void initiate_double_declaration_identifier_table (void);
extern void finish_double_declaration_identifier_table (void);

/* Table of predefined types and node types. */

extern IR_node_t insert_type (IR_node_t type);
extern IR_node_t find_type (IR_node_t identifier);
extern void initiate_type_table (void);
extern void finish_type_table (void);

/* Table of fields (key is the field identifier). */

extern IR_node_t insert_field (IR_node_t node_field);
extern IR_node_t find_field (IR_node_t field_identifier);
extern void initiate_field_table (void);
extern void finish_field_table (void);

/* Table node type fields (key is the field identifier and
   the node type identifiers). */

extern IR_node_t insert_node_field (IR_node_t node_field);
extern IR_node_t find_node_field (IR_node_t field_identifier,
                                  IR_node_t node_type);
extern void initiate_node_field_table (void);
extern void finish_node_field_table (void);
