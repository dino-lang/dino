/*
   FILE NAME:   anal.h

   TITLE:       Include file of semantic analyzer of NONA (code selector
                description)

   DESCRIPTION: This header file contains ANSI C prototype definition of
                only one external function.

   SPECIAL CONSIDERATION:
         The analyzer is to be called only after NONA parser.  This file
       can not be included repeatedly.

*/

extern void analyze_program (void);
