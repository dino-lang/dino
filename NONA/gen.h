/*
   FILE NAME:   gen.h

   TITLE:       Include file of generator of NONA (code selector description
                translator)

   DESCRIPTION: This header file contains ANSI C prototype definition of
                only one external function.

   SPECIAL CONSIDERATION:
         The generator is to be called after NONA semantic analyzer only
       if any error was not fixed.  This file can not be included
       repeatedly.

*/

extern void generate (void);
