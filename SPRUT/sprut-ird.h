/*
   FILE NAME:   sprut-ird.h

   Copyright (C) 1997-2005 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

   This file is part of the tool SPRUT.

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

   TITLE:       Include file of internal representation description
                for SPRUT (internal representation description translator)
                itself

   DESCRIPTION: This header file is created manually and contains
                definitions of types, macros, external definitions of
                variables and functions which are standard procedural
                interface (SPI -- see documentation of SPRUT) for internal
                representation of SPRUT itself.

   SPECIAL CONSIDERATION:
         This file is created manually and defines SPI analogous to
       part of SPI generated automatically by SPRUT itself from file
       `ird.sprut'.  The important difference is absence of abstract node
       types which are used in file `ird.sprut' for convenience.
   
*/

#ifndef __IR_ird__
#define __IR_ird__

#include "objstack.h"
#include "position.h"


/* This type describes all modes of the SPRUT internal representation
   nodes.  There is enumeration constant with node type name and prefix
   `IR_NM_' for each node type declaration.  Also there are enumeration
   constants with names `IR_NM__root' and `IR_NM__error' for and predefined
   type `%root' and for error nodes.  `IR_NM' is abbreviation of internal
   representation node mode.  The order of enumeration constants is
   important.  It is used in `IR_node_size' and `IR_node_name'. */

typedef enum
{
  IR_NM__root,
  IR_NM__error,
  IR_NM_identifier,
  IR_NM_double_declaration,
  IR_NM_code_insertion,
  IR_NM_expression,
  IR_NM_local_code,
  IR_NM_import_code,
  IR_NM_export_code,
  IR_NM_constraint,
  IR_NM_action,
  IR_NM_field,
  IR_NM_node_type,
  IR_NM_super_type_list_element,
  IR_NM_predefined_type,
  IR_NM_additional_code,
  IR_NM_description_part
} IR_node_mode_t;

/* This type describes all parts (sections -- see SPRUT documentation) of
   declarations of fields, constraints, and actions of an internal
   representation description.  There is enumeration constant with field
   declaration section name and prefix `DP_' for each declaration part.
   `DP' is abbreviation of declaration part. */

typedef enum
{
  DP_CLASS,
  DP_SKELETON,
  DP_OTHER
} declaration_part_t;

/* Type representing pointer to any SPRUT internal representation node.
   All node members representing arcs between nodes is of this type. */

typedef struct _root *IR_node_t;

/* Members in different structures but with the same name have common
   sense and must have the same displacement in the following structures.
   According to this the SPRUT code is simpler and more economic.

   All structures contain the following common members:
       mode (node mode) and
       position (start source position of object represented by given node).
*/

/* It describes any identifier occurrence. All members are defined
   by scanner of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  position_t position;
  char *identifier_itself;
} identifier_node;

/* It describes double node type identifier declaration occurrence.
   All members are defined before semantic analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding double declaration
     identifier. */
  position_t position;
  /* Arc to node representing textually next double declaration. */
  IR_node_t next_double_declaration;
  IR_node_t double_declaration_identifier;
} double_declaration_node;

/* It describes code insertion, i.e. C code in `{' and `}'.  All members
   are defined by scanner of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  position_t position;
  /* Surrounding '{' and '}' are absent here.  Remember that constructions `$'
     & `$$' will be changed only during SPI generation.  */
  char *code_insertion_itself;
} code_insertion_node;

/* It describes expression, i.e. C code in `[' and `]'.  All
   are defined by scanner of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  position_t position;
  /* Surrounding '[' and ']' are absent here.  Remember that constructions `$'
     & `$$' will be changed only during SPI generation.  */
  char *expression_itself;
} expression_node;

/* It describes local code, i.e. construction `%local {...}'.  All
   members are defined before semantic analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding code insertion. */
  position_t position;
  /* Arc to node representing next textually import, export or local code.
     This member is also present in structure `export_code_node'
     and `import_code_node'. */
  IR_node_t next_code_insertion;
  /* Arc to node which represents corresponding code insertion.  This member
     is also present in structure `export_code_node' and `import_code_node'. */
  IR_node_t code_itself;
} local_code_node;

/* It describes import code, i.e. construction `%import {...}'.  All
   members are defined before semantic analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding code insertion. */
  position_t position;
  /* Arc to node representing next textually import, export or local code.
     This member is also present in structure `local_code_node' and
     `export_code_node'. */
  IR_node_t next_code_insertion;
  /* Arc to node which represents corresponding code insertion.  This member
     is also present in structure `local_code_node' and
     `export_code_node'. */
  IR_node_t code_itself;
} import_code_node;

/* It describes export code, i.e. construction `%export {...}'.  All
   members are defined before semantic analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding code insertion. */
  position_t position;
  /* Arc to node representing next textually import, export or local code.
     This member is also present in structure `local_code_node' and
     `import_code_node'. */
  IR_node_t next_code_insertion;
  /* Arc to node which represents corresponding code insertion.  This member
     is also present in structure `local_code_node' and
     `import_code_node'. */
  IR_node_t code_itself;
} export_code_node;

/* It describes constraint.  All members are defined before semantic
   analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding expression. */
  position_t position;
  /* Arc to node representing next field, constraint or action.
     This member is also present in structures `field_node', `action_node'. */
  IR_node_t next_field;
  /* See commentaries for `declaration_part_t'.  This member is also
     present in structures `field_node', `action_node'. */
  declaration_part_t declaration_part;
  /* Arc to node which represents corresponding expression. */
  IR_node_t constraint_code;
} constraint_node;

/* It describes action.  All members are defined before semantic
   analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding code insertion. */
  position_t position;
  /* Arc to node representing next field, constraint or action.
     This member is also present in structures `field_node',
     `constraint_node'. */
  IR_node_t next_field;
  /* See commentaries for `declaration_part_t'.  This member is also present
     in structures `field_node', `constraint_node'. */
  declaration_part_t declaration_part;
  /* Arc to node which represents corresponding code insertion. */
  IR_node_t action_code;
} action_node;

/* It describes fields of node types.  All members (except for
   `previous_synonym_field' and `field_type') are defined before semantic
   analysis of SPRUT.  Member `double_field_flag' is made more precise on
   semantic analysis of SPRUT.  There are copy of field node for each node
   type in the following constructions
        node_type_1, node_type_2, ... :: super_type
           ...
           field : ...
           ...
*/

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding field identifier. */
  position_t position;
  /* Arc to node representing next field, constraint or action.  This
     member is also present in structures `constraint_node', `action_node'. */
  IR_node_t next_field;
  /* See commentaries for `declaration_part_t'.  This member is also
     present in structures `constraint_node', `action_node'. */
  declaration_part_t declaration_part;
  /* TRUE for field with construction `%double' and FALSE otherwise before
     semantic analysis.  Aftewards TRUE also for fields which refer to
     nodes described as double (this is not equivalent to that
     `double_node_flag' of corresponding node type is TRUE). */
  int double_field_flag;
  /* Fields of given node type may be declared in different description parts
     , i.e. in different files (see construction `%extend' in SPRUT
     documentation).  This member informs  on what level description
     (0, 1, ...) the field is declared. */
  int declaration_level;
  /* Arc to node representing identifier of field. */
  IR_node_t field_identifier;
  /* Arc to node representing previous field with the same name.
     The previous field shall be declared in another node type which
     is not super type or subtype of node type of given field. */
  IR_node_t previous_synonym_field;
  /* Arc to node representing identifier of field type. */
  IR_node_t field_type_identifier;
  /* Arc to node representing node type or predefined type which is
     field type. */
  IR_node_t field_type;
  /* Arc to node representing node type in which the field is declared. */
  IR_node_t node_type;
} field_node;

/* It describes all nodes types including `%root'.  All members
   (except for `first_declaration_level', `node_type_number') are
   defined before semantic analysis of SPRUT.  Member
   `double_node_flag' is initiated by FALSE before semantic analysis
   of SPRUT and is made more precise on the semantic analysis. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding node type
     identifier. */
  position_t position;
  /* Arc to node representing next textually node type or predefined type.
     This member is also present in structure `predefined_type_node'. */
  IR_node_t next_type;
  /* Arc to node representing corresponding node type identifier.
     This member is also present in structure `predefined_type_node'. */
  IR_node_t type_identifier;
  /* Given node type may be defined in several description parts, i.e.
     in several files (see construction `%extend' in SPRUT documentation).
     This member informs on what minimum level (0, 1, ...) the node type is
     declared. */
  int first_declaration_level;
  /* Enumeration of all node types: 0, 1, 2, and so on. */
  int node_type_number;
  /* Remember that double and non-double fields can refer to given node.
     The member shall be set up during node creation and after only made more
     precise. */
  int double_node_flag;
  /* The following member is the first element of list of immediate
     super types of given node type. */
  IR_node_t first_super_type_list_element;
  /* The following member is the last element of list of immediate
     super types of given node type. */
  IR_node_t last_super_type_list_element;
  /* The following field is defined if field
     `first_super_type_list_element' is NULL.  The field value is TRUE if
     the previous field is not list of additional immediate super
     types (i.e. this list does not start with comma -- see
     syntax). */
  int basic_super_types_flag;
  /* Arc to node representing the last field, constraint or action of
     cyclic list formed by member `next_field'. */
  IR_node_t last_field;
  /* TRUE for node type declared as abstract. */
  int abstract_flag;
  /* The following member is used in a function traversing all node types.
     The member value is TRUE if node type was already processed. */
  int traverse_flag;
  /* The following members are used during generation. */
  /* The following member is TRUE if decalaration of structure containing
     fields of of given node type is already output. */ 
  int fields_structure_has_been_output;
  /* The following member is TRUE if decalaration of structure containing
     class fields of of given nod type is already output.  The member value
     will be always FALSE if the node type does not have class fields. */ 
  int class_fields_structure_has_been_output;
  /* The following member is TRUE if the node type (or its subtype) is
     used as secondary super type (not the first super type)
     somewhere. */
  int secondary_super_type_flag;
  /* The following member is used during analysis to search for common
     super types of node types.. */
  int mark_flag;
  /* The following field is used during generation when non-flat
     structure implementation is used.  The value is TRUE if the
     structure containing node type fields is already output. */
  int node_structure_has_been_output;
  /* The following member value is the last visited node type who is
     checked on multiple inheretence. */
  IR_node_t who_visisted_on_multiple_inheritence_checking;
} node_type_node;


/* All members (except for `immediate_super_type') are defined before
   semantic analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding node type
     identifier. */
  position_t position;
  /* Arc to node representing identifier of super type. */
  IR_node_t immediate_super_type_identifier;
  /* Arc to node representing node type of immediate super type
     (NULL for `%root'). */
  IR_node_t immediate_super_type;
  /* The following member is NULL for the last element of super type
     list. */
  IR_node_t next_super_type_list_element;
  /* The following member is necessary because additional super types
     may be connected to the node type.  To disable repeated setting
     and testing super types. */
  char setting_and_testing_super_type_was_made;
  /* The following member is necessary because additional super types
     may be connected to the node type.  To disable repeated testing
     super type cycle. */
  char testing_super_type_cycle_was_made;
} super_type_list_element_node;


/* It describes predefined type.  All members are defined before
   semantic analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* This position is defined by position of corresponding predefined type
     identifier. */
  position_t position;
  /* Arc to node representing next textually node type or predefined type.
     This member is also present in structure `node_type_node'. */
  IR_node_t next_type;
  /* Arc to node representing corresponding predefined type identifier.
     This member is also present in structure `node_type_node'. */
  IR_node_t type_identifier;
} predefined_type_node;

/* It describes additional code.  All members (except for
   `next_additional_code') are defined by SPRUT scanner. */

typedef struct
{
  IR_node_mode_t mode;
  position_t position;
  /* Arc to node representing next textually additional code.  The
     next additional code can be only in another description part which
     refers to given description part by construction `%extend'. */
  IR_node_t next_additional_code;
  char *additional_code_itself;
} additional_code_node;

/* It describes description part (it is stored in separate file).
   All members are defined before semantic analysis of SPRUT. */

typedef struct
{
  IR_node_mode_t mode;
  /* Start position of corresponding file. */
  position_t position;
  /* Description part level.  The most nested description part has zero
     level. */
  int description_part_level;
  /* Arc to node representing the first extended description part (see
     construction `%extend') of given description. */
  IR_node_t first_basic_description_part;
  /* Arc to node representing the next extended description part of
     some description part.  The list formed with the aid of following
     member does not contain description part declared in the
     corresponding `%extend' construction if the description part is a
     part of description mentioned early in the `%extend'
     construction.  Therefore only one description refers to given
     description by the members `first_basic_description_part' or
     `next_basic_description_part'. */
  IR_node_t next_basic_description_part;
  /* Arc to node representing the last local or import/export code of
     cyclic list formed by member `next_code_insertion'. */
  IR_node_t last_code_insertion;
  /* Arc to node representing the last double node type identifier declaration
     of cyclic list formed by member `next_double_declaration'. */
  IR_node_t last_double_declaration;
  /* Arc to node representing the last node type or predefined type
     of cyclic list formed by member `next_type'. */
  IR_node_t last_type;
  /* Arc to node representing the last additional code
     of cyclic list formed by member `next_additional_code'. */
  IR_node_t last_additional_code;
} description_part_node;

/* It describes common part of all structures of internal representation
   (except for `_error'). */

struct _root_node
{
  IR_node_mode_t mode;
  position_t position;
};

/* It describes any error construction. */

typedef struct
{
  IR_node_mode_t mode;
} _error_node;

/* It is equivalent to struct `_root_node'. */

typedef struct _root_node _root_node;

/* This macro returns node mode represented by value of type
   `IR_node_mode_t' for node given as the macro argument of type
   `IR_node_t'. */

#define IR_NODE_MODE(n) ((_root_node *) (n))->mode

/* These macros implement SPI code for access to and modification of fields. */

#define IR_position(n) ((_root_node *) (n))->position
#define IR_set_position(n, v) (((_root_node *) (n))->position = (v))

#define IR_identifier_itself(n) ((identifier_node *) (n))->identifier_itself
#define IR_set_identifier_itself(n, v)\
  (((identifier_node *) (n))->identifier_itself = (v))

#define IR_next_double_declaration(n)\
  ((double_declaration_node *) (n))->next_double_declaration
#define IR_set_next_double_declaration(n, v)\
  (((double_declaration_node *) (n))->next_double_declaration = (v))

#define IR_double_declaration_identifier(n)\
  ((double_declaration_node *) (n))->double_declaration_identifier
#define IR_set_double_declaration_identifier(n, v)\
  (((double_declaration_node *) (n))->double_declaration_identifier = (v))

#define IR_code_insertion_itself(n)\
  ((code_insertion_node *) (n))->code_insertion_itself
#define IR_set_code_insertion_itself(n, v)\
         (((code_insertion_node *) (n))->code_insertion_itself = (v))

#define IR_expression_itself(n) ((expression_node *) (n))->expression_itself
#define IR_set_expression_itself(n, v)\
         (((expression_node *) (n))->expression_itself = (v))

/* Common field for local_code_node, import_code_node, export_code_node */

#define IR_next_code_insertion(n)\
  ((local_code_node *) (n))->next_code_insertion
#define IR_set_next_code_insertion(n, v)\
  (((local_code_node *) (n))->next_code_insertion = (v))

/* Common field for local_code_node, import_code_node, export_code_node */

#define IR_code_itself(n) ((local_code_node *) (n))->code_itself
#define IR_set_code_itself(n, v)\
  (((local_code_node *) (n))->code_itself = (v))

/* Common field for constraint_node, action_node, field_node. */

#define IR_next_field(n) ((constraint_node *) (n))->next_field
#define IR_set_next_field(n, v) (((constraint_node *) (n))->next_field = (v))

/* Common field for constraint_node, action_node, field_node. */

#define IR_declaration_part(n) ((constraint_node *) (n))->declaration_part
#define IR_set_declaration_part(n, v)\
  (((constraint_node *) (n))->declaration_part = (v))

#define IR_constraint_code(n) ((constraint_node *) (n))->constraint_code
#define IR_set_constraint_code(n, v)\
  (((constraint_node *) (n))->constraint_code = (v))

#define IR_action_code(n) ((action_node *) (n))->action_code
#define IR_set_action_code(n, v) (((action_node *) (n))->action_code = (v))

#define IR_double_field_flag(n) ((field_node *) (n))->double_field_flag
#define IR_set_double_field_flag(n, v)\
         (((field_node *) (n))->double_field_flag = (v))

#define IR_declaration_level(n) ((field_node *) (n))->declaration_level
#define IR_set_declaration_level(n, v)\
  (((field_node *) (n))->declaration_level = (v))

#define IR_field_identifier(n) ((field_node *) (n))->field_identifier
#define IR_set_field_identifier(n, v)\
         (((field_node *) (n))->field_identifier = (v))

#define IR_previous_synonym_field(n)\
         ((field_node *) (n))->previous_synonym_field
#define IR_set_previous_synonym_field(n, v)\
         (((field_node *) (n))->previous_synonym_field = (v))

#define IR_field_type_identifier(n) ((field_node *) (n))->field_type_identifier
#define IR_set_field_type_identifier(n, v)\
         (((field_node *) (n))->field_type_identifier = (v))

#define IR_field_type(n) ((field_node *) (n))->field_type
#define IR_set_field_type(n, v) (((field_node *) (n))->field_type = (v))

#define IR_node_type(n) ((field_node *) (n))->node_type
#define IR_set_node_type(n, v) (((field_node *) (n))->node_type = (v))

/* Common field for node_type_node, predefined_type_node. */

#define IR_next_type(n) ((node_type_node *) (n))->next_type
#define IR_set_next_type(n, v) (((node_type_node *) (n))->next_type = (v))

/* Common field for node_type_node, predefined_type_node. */

#define IR_type_identifier(n) ((node_type_node *) (n))->type_identifier
#define IR_set_type_identifier(n, v)\
         (((node_type_node *) (n))->type_identifier = (v))

#define IR_first_declaration_level(n)\
  ((node_type_node *) (n))->first_declaration_level
#define IR_set_first_declaration_level(n, v)\
  (((node_type_node *) (n))->first_declaration_level = (v))

#define IR_node_type_number(n) ((node_type_node *) (n))->node_type_number
#define IR_set_node_type_number(n, v)\
  (((node_type_node *) (n))->node_type_number = (v))

#define IR_double_node_flag(n) ((node_type_node *) (n))->double_node_flag
#define IR_set_double_node_flag(n, v)\
   (((node_type_node *) (n))->double_node_flag = (v))

#define IR_first_super_type_list_element(n)\
   ((node_type_node *) (n))->first_super_type_list_element
#define IR_set_first_super_type_list_element(n, v)\
   (((node_type_node *) (n))->first_super_type_list_element = (v))

#define IR_last_super_type_list_element(n)\
   ((node_type_node *) (n))->last_super_type_list_element
#define IR_set_last_super_type_list_element(n, v)\
   (((node_type_node *) (n))->last_super_type_list_element = (v))

#define IR_basic_super_types_flag(n)\
   ((node_type_node *) (n))->basic_super_types_flag
#define IR_set_basic_super_types_flag(n, v)\
   (((node_type_node *) (n))->basic_super_types_flag = (v))

#define IR_last_field(n) ((node_type_node *) (n))->last_field
#define IR_set_last_field(n, v) (((node_type_node *) (n))->last_field = (v))

#define IR_abstract_flag(n) ((node_type_node *) (n))->abstract_flag
#define IR_set_abstract_flag(n, v)\
  (((node_type_node *) (n))->abstract_flag = (v))

#define IR_traverse_flag(n) ((node_type_node *) (n))->traverse_flag
#define IR_set_traverse_flag(n, v)\
  (((node_type_node *) (n))->traverse_flag = (v))

#define IR_fields_structure_has_been_output(n)\
  ((node_type_node *) (n))->fields_structure_has_been_output
#define IR_set_fields_structure_has_been_output(n, v)\
  (((node_type_node *) (n))->fields_structure_has_been_output = (v))

#define IR_class_fields_structure_has_been_output(n)\
  ((node_type_node *) (n))->class_fields_structure_has_been_output
#define IR_set_class_fields_structure_has_been_output(n, v)\
  (((node_type_node *) (n))->class_fields_structure_has_been_output = (v))

#define IR_secondary_super_type_flag(n)\
  ((node_type_node *) (n))->secondary_super_type_flag
#define IR_set_secondary_super_type_flag(n, v)\
  (((node_type_node *) (n))->secondary_super_type_flag = (v))

#define IR_mark_flag(n) ((node_type_node *) (n))->mark_flag
#define IR_set_mark_flag(n, v) (((node_type_node *) (n))->mark_flag = (v))

#define IR_node_structure_has_been_output(n)\
  ((node_type_node *) (n))->node_structure_has_been_output
#define IR_set_node_structure_has_been_output(n, v)\
  (((node_type_node *) (n))->node_structure_has_been_output = (v))

#define IR_who_visisted_on_multiple_inheritence_checking(n)\
  ((node_type_node *) (n))->who_visisted_on_multiple_inheritence_checking
#define IR_set_who_visisted_on_multiple_inheritence_checking(n, v) \
(((node_type_node *) (n))->who_visisted_on_multiple_inheritence_checking = (v))

#define IR_immediate_super_type(n)\
         ((super_type_list_element_node *) (n))->immediate_super_type
#define IR_set_immediate_super_type(n, v)\
         (((super_type_list_element_node *) (n))->immediate_super_type = (v))

#define IR_immediate_super_type_identifier(n)\
 ((super_type_list_element_node *) (n))->immediate_super_type_identifier
#define IR_set_immediate_super_type_identifier(n, v)\
 (((super_type_list_element_node *) (n))->immediate_super_type_identifier =(v))

#define IR_next_super_type_list_element(n)\
 ((super_type_list_element_node *) (n))->next_super_type_list_element
#define IR_set_next_super_type_list_element(n, v)\
 (((super_type_list_element_node *) (n))->next_super_type_list_element =(v))

#define IR_setting_and_testing_super_type_was_made(n)\
 ((super_type_list_element_node *) (n))\
 ->setting_and_testing_super_type_was_made
#define IR_set_setting_and_testing_super_type_was_made(n, v)\
 (((super_type_list_element_node *) (n))\
  ->setting_and_testing_super_type_was_made = (v))

#define IR_testing_super_type_cycle_was_made(n)\
 ((super_type_list_element_node *) (n))->testing_super_type_cycle_was_made
#define IR_set_testing_super_type_cycle_was_made(n, v)\
 (((super_type_list_element_node *) (n))\
  ->testing_super_type_cycle_was_made = (v))

#define IR_next_additional_code(n)\
  ((additional_code_node *) (n))->next_additional_code
#define IR_set_next_additional_code(n, v)\
  (((additional_code_node *) (n))->next_additional_code = (v))

#define IR_additional_code_itself(n)\
  ((additional_code_node *) (n))->additional_code_itself
#define IR_set_additional_code_itself(n, v)\
  (((additional_code_node *) (n))->additional_code_itself = (v))

#define IR_description_part_level(n)\
  ((description_part_node *) (n))->description_part_level
#define IR_set_description_part_level(n, v)\
  (((description_part_node *) (n))->description_part_level = (v))

#define IR_first_basic_description_part(n)\
  ((description_part_node *) (n))->first_basic_description_part
#define IR_set_first_basic_description_part(n, v)\
  (((description_part_node *) (n))->first_basic_description_part = (v))

#define IR_next_basic_description_part(n)\
  ((description_part_node *) (n))->next_basic_description_part
#define IR_set_next_basic_description_part(n, v)\
  (((description_part_node *) (n))->next_basic_description_part = (v))

#define IR_last_code_insertion(n)\
  ((description_part_node *) (n))->last_code_insertion
#define IR_set_last_code_insertion(n, v)\
  (((description_part_node *) (n))->last_code_insertion = (v))

#define IR_last_double_declaration(n)\
  ((description_part_node *) (n))->last_double_declaration
#define IR_set_last_double_declaration(n, v)\
  (((description_part_node *) (n))->last_double_declaration = (v))

#define IR_last_type(n) ((description_part_node *) (n))->last_type
#define IR_set_last_type(n, v)\
  (((description_part_node *) (n))->last_type = (v))

#define IR_last_additional_code(n)\
  ((description_part_node *) (n))->last_additional_code
#define IR_set_last_additional_code(n, v)\
  (((description_part_node *) (n))->last_additional_code = (v))


/* These macros for storage management of SPRUT internal representation: */

/* Start work with the storage manager -- see SPRUT documentation. */

#define IR_START_ALLOC()    OS_CREATE (irp, 0)

/* Finish work with the storage manager -- see SPRUT documentation. */

#define IR_STOP_ALLOC()    OS_DELETE (irp)

/* Allocate storage for internal representation of given size
   -- see SPRUT documentation. */

#define IR_ALLOC(ptr, size, ptr_type)\
  do {\
    OS_TOP_EXPAND (irp, size); ptr = (ptr_type) OS_TOP_BEGIN (irp);\
    OS_TOP_FINISH (irp);\
  } while (0);

/* Free storage of internal representation of given size -- see SPRUT
   documentation. */

#define IR_FREE(ptr, size)


/* These macros are analogous to ones of package `object-stack'
   worked with storage of SPRUT internal representation: */

/* Start new internal representation object -- see also package
   `object-stack'. */

#define IR_TOP_FINISH()  OS_TOP_FINISH (irp)

/* Nullify current internal representation object -- see also package
   `object-stack'. */

#define IR_TOP_NULLIFY()  OS_TOP_NULLIFY (irp)

/* Shorten current internal representation object on given number bytes -- see
   also package `object-stack'. */

#define IR_TOP_SHORTEN(length) OS_TOP_SHORTEN (irp, length)

/* Return start address of current internal representation object -- see also
   package `object-stack'. */

#define IR_TOP_BEGIN()  OS_TOP_BEGIN (irp)

/* Return length of current internal representation object in bytes -- see
   also package `object-stack'. */

#define IR_TOP_LENGTH()  OS_TOP_LENGTH (irp)

/* Add byte to the end of current internal representation object -- see also
   package `object-stack'. */

#define IR_TOP_ADD_BYTE(b)  OS_TOP_ADD_BYTE (irp, b)

/* Add string to the end of current internal representation object -- see also
   package `object-stack'. */

#define IR_TOP_ADD_STRING(str)  OS_TOP_ADD_STRING (irp, str)

/* Add memory of given length to the end of current internal representation
   object -- see also package `object-stack'. */

#define IR_TOP_ADD_MEMORY(mem, length)  OS_TOP_ADD_MEMORY (irp, mem, length)

extern os_t irp;

extern int IR_node_size [];

extern void IR_start (void);
extern void IR_stop (void);

extern IR_node_t IR_create_node (IR_node_mode_t node_mode);

extern IR_node_t IR_copy_node (IR_node_t node);

#endif /* __IR_ird__ */
