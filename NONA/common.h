/*
   FILE NAME:   common.h

   TITLE:       Include file of common things for all NONA (code selector
                description translator)

   DESCRIPTION: This header file contains definitions of macros
                and external definitions of variables common for
                all NONA (code selector description translator).

   SPECIAL CONSIDERATION:
       This file can not be included repeatedly.
   
*/

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#endif

#include "ird.h"

/* Standard designators for true and false values. */

#define FALSE 0
#define TRUE  1

/* Suffix which any code selector description file name must have.  If
   this macro value is changed then documentation of nona must be
   changed. */

#define STANDARD_INPUT_FILE_SUFFIX ".nona"

/* This macro value is used for correct calculation of current position in
   file when TAB character is processed. */

#define TAB_STOP 8

extern int cpp_flag;
extern int v_flag;
extern int debug_flag;
extern int export_flag;

extern char *prefix;

extern char *description_name;

extern FILE *output_interface_file;
extern char *output_interface_file_name;

extern FILE *output_implementation_file;
extern char *output_implementation_file_name;

extern IR_node_t description;
