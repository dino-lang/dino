/*
   FILE NAME:   tab.h

   TITLE:       Include file of Shilka (keywords description translator)
                tables

   DESCRIPTION: This header file contains ANSI C prototype definitions of
                external functions which implement the following tables
                      table of strings (and identifiers)
                      table of keywords
                      table of keyword names

   SPECIAL CONSIDERATION:
       This file can not be included repeatedly.

*/

#include "ird.h"

/* Table of strings. */

extern char *insert_string (const char *string);
extern void initiate_string_table (void);
extern void finish_string_table (void);

/* Table of keywords. */

extern IR_node_t insert_keyword (IR_node_t keyword);
extern void initiate_keyword_table (void);
extern void finish_keyword_table (void);

/* Table of keyword names. */

extern IR_node_t insert_keyword_name (IR_node_t keyword);
extern void initiate_keyword_name_table (void);
extern void finish_keyword_name_table (void);
