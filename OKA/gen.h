/*
   FILE NAME:   gen.h

   TITLE:       Include file of generator of OKA (pipeline hazards description
                translator)

   DESCRIPTION: This header file contains ANSI C prototype definition of
                only one external function.

   SPECIAL CONSIDERATION:
         The generator is to be called after OKA semantic analyzer only
       if any error was not fixed.  This file can not be included
       repeatedly.


*/

extern void generate (void);
