/*
   FILE NAME:   gen.h

   TITLE:       Include file of standard procedural interface (SPI)
                generator of SPRUT (internal representation description
                translator)

   DESCRIPTION: This header file contains ANSI C prototype definition of
                only one external function.

   SPECIAL CONSIDERATION:
         The generator is to be called after SPRUT semantic analyzer only
       if any error was not fixed.  This file can not be included
       repeatedly.

*/

extern void generate_spi (void);
