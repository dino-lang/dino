/*
   FILE NAME:   common.h

   TITLE:       Include file of common things for all OKA (pipeline hazards
                description translator)

   DESCRIPTION: This header file contains definitions of macros
                and external definitions of variables common for
                all OKA (pipeline hazards description translator).

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

/* Suffix which any pipeline hazards description file name must have.
   If this macro value is changed then documentation of oka must be
   changed. */

#define STANDARD_INPUT_FILE_SUFFIX ".oka"

/* This macro value is used for correct calculation of current position in
   file when TAB character is processed. */

#define TAB_STOP 8

extern int cpp_flag;
extern int debug_flag;
extern int enum_flag;
extern int export_flag;
extern int no_minimization_flag;

extern int split_argument;
extern char *prefix;
extern int time_flag;
extern int v_flag;

extern char *description_name;

extern FILE *output_interface_file;
extern char *output_interface_file_name;

extern FILE *output_implementation_file;
extern char *output_implementation_file_name;

extern FILE *output_description_file;
extern char *output_description_file_name;

extern IR_node_t description;
