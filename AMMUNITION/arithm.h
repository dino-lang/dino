/* FILE NAME:   arithm.h

   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package of arbitrary precision integer arithmetic;
   you can redistribute it and/or modify it under the terms of the GNU
   Library General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later
   version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Include file of package for arbitrary precision integer
                arithmetic

   DESCRIPTION: This header file contains ANSI C prototype definitions of
                the package functions and definitions of external
                variable of the package and C++ classes for arbitrary
                precision integer arithmetic.

   SPECIAL CONSIDERATION:
         C++: Defining macro `NO_TEMPLATE' (e.g. by option `-D' in C++
       compiler command line) during the file compilation disables to
       use template classes `sint' and `unsint'.

*/


#ifndef __ARITHMETIC__
#define __ARITHMETIC__

extern int overflow_bit;
extern const unsigned char *zero_constant;

extern void default_arithmetic_overflow_reaction (void);

extern void
  (*set_unsigned_integer_overflow_reaction (void (*function) (void))) (void);
extern void (*set_integer_overflow_reaction (void (*function) (void))) (void);

extern void unsigned_integer_maximum (int size, void *result);
extern void integer_minimum (int size, void *result);
extern void integer_maximum (int size, void *result);

extern void add_unsigned_integer (int size, const void *op1, const void *op2,
                                  void *result);
extern void add_integer (int size, const void *op1, const void *op2,
                         void *result);
extern void subtract_unsigned_integer (int size, const void *op1,
                                       const void *op2, void *result);
extern void subtract_integer (int size, const void *op1, const void *op2,
                              void *result);
extern void multiply_unsigned_integer (int size, const void *op1,
                                       const void *op2, void *result);
extern void multiply_integer (int size, const void *op1, const void *op2,
                              void *result);
extern void divide_unsigned_integer (int size, const void *op1,
                                     const void *op2, void *result);
extern void divide_integer (int size, const void *op1, const void *op2,
                            void *result);
extern void unsigned_integer_remainder (int size, const void *op1,
                                        const void *op2, void *result);
extern void integer_remainder (int size, const void *op1, const void *op2,
                               void *result);

extern void unsigned_integer_shift_right (int size, const void *operand,
                                          int bits, void *result);
extern void integer_shift_right (int size, const void *operand,
                                 int bits, void *result);
extern void integer_shift_left (int size, const void *operand,
                                int bits, void *result);
extern void unsigned_integer_shift_left (int size, const void *operand,
                                         int bits, void *result);

extern void integer_or (int size, const void *op1,
                        const void *op2, void *result);
extern void unsigned_integer_or (int size, const void *op1,
                                 const void *op2, void *result);
extern void integer_and (int size, const void *op1,
                         const void *op2, void *result);
extern void unsigned_integer_and (int size, const void *op1,
                                  const void *op2, void *result);
extern void integer_xor (int size, const void *op1,
                         const void *op2, void *result);
extern void unsigned_integer_xor (int size, const void *op1,
                                  const void *op2, void *result);
extern void integer_not (int size, const void *operand, void *result);
extern void unsigned_integer_not (int size, const void *operand, void *result);

extern int eq_unsigned_integer (int size, const void *op1, const void *op2);
extern int eq_integer (int size, const void *op1, const void *op2);
extern int ne_unsigned_integer (int size, const void *op1, const void *op2);
extern int ne_integer (int size, const void *op1, const void *op2);
extern int gt_unsigned_integer (int size, const void *op1, const void *op2);
extern int gt_integer (int size, const void *op1, const void *op2);
extern int lt_unsigned_integer (int size, const void *op1, const void *op2);
extern int lt_integer (int size, const void *op1, const void *op2);
extern int ge_unsigned_integer (int size, const void *op1, const void *op2);
extern int ge_integer (int size, const void *op1, const void *op2);
extern int le_unsigned_integer (int size, const void *op1, const void *op2);
extern int le_integer (int size, const void *op1, const void *op2);

extern void change_unsigned_integer_size
  (int operand_size, const void *operand, int result_size, void *result);
extern void change_integer_size (int operand_size, const void *operand,
                                 int result_size, void *result);

extern char *unsigned_integer_to_based_string (int size, const void *operand,
					       int base, char *result);
extern char *unsigned_integer_to_string (int size, const void *operand,
                                         char *result);
extern char *integer_to_based_string (int size, const void *operand, int base,
				      char *result);
extern char *integer_to_string (int size, const void *operand, char *result);

extern char *unsigned_integer_from_based_string (int size, const char *operand,
						 int base, void *result);
extern char *unsigned_integer_from_string (int size, const char *operand,
                                           void *result);
extern char *integer_from_based_string (int size, const char *operand,
					int base, void *result);
extern char *integer_from_string (int size, const char *operand,
                                  void *result);

#endif /* #ifndef __ARITHMETIC__ */
