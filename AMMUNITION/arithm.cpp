/* FILE NAME:   arithm.cpp

   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

   TITLE:       Package for arbitrary precision integer arithmetic

   DESCRIPTION: This package implements arbitrary precision integer
       and unsigned integer numbers by machine independent way.  The
       implementation of the package functions are not sufficiently
       efficient in order to use for run-time.  The package functions
       are oriented to implement constant-folding in compilers.  This
       package is necessary because host machine may not support such
       arithmetic for target machine.  For example, VAX does not
       support does not support more 32-bits integer numbers
       arithmetic.  The numbers are represented by bytes in big endian
       mode, negative integer numbers are represented in complementary
       code.  All sizes are given in bytes and must be positive.
       Results of executions of all functions can coincide with a
       operand(s).  All functions of addition, subtraction,
       multiplication, division, evaluation of remainder, shift,
       changing size and transformation of string into number fix
       overflow.  The overflow is fixed when result can not be
       represented by number of given size.

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C++ compiler
       command line) during the file compilation disables to fix some
       internal errors and errors of usage of the package.
         The default value of macro `MAX_INTEGER_OPERAND_SIZE' can be
       redefined with corresponding C++ compiler option `-D' during
       compilation of the package.  But in any case the minimal value
       of the macros will be 16.
         Defining macro `HAVE_MEMMOVE' (e.g. by option -D in
       C++ compiler command line) during the file compilation permits to
       use standard C function `memmove' (file `string.h').  In
       opposite case function `memmove' is implemented in this file.

*/

#ifdef HAVE_CONFIG_H
#include "cocom-config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_MEMCPY
#define HAVE_MEMCPY
#endif
#ifndef HAVE_MEMSET
#define HAVE_MEMSET
#endif
#ifndef HAVE_MEMCMP
#define HAVE_MEMCMP
#endif
#ifndef HAVE_MEMMOVE
#define HAVE_MEMMOVE
#endif
#ifndef HAVE_ASSERT_H
#define HAVE_ASSERT_H
#endif
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */


#include <ctype.h>
#include <string.h>
#include "arithm.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else
#ifndef assert
#define assert(code) do { if (code == 0) abort ();} while (0)
#endif
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif
#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif
#ifndef SCHAR_MAX
#define SCHAR_MAX 127
#endif
#ifndef SCHAR_MIN
#define SCHAR_MIN (-128)
#endif
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif
#ifndef SHRT_MAX
#define SHRT_MAX 32767
#endif  
#ifndef SHRT_MIN
#define SHRT_MIN (-32768)
#endif
#ifndef UINT_MAX
#define UINT_MAX (INT_MAX * 2U + 1)
#endif
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif  
#ifndef INT_MIN
#define INT_MIN (-INT_MAX-1)
#endif
#endif



/* This page contains definitions of variables and macros common for
   all package functions. */

/* The value of macro is suggested to be maximum length of integer operands
   The length of use integers should be not greater than this value. */

#ifndef MAX_INTEGER_OPERAND_SIZE
#define MAX_INTEGER_OPERAND_SIZE 128
#else
#if MAX_INTEGER_OPERAND_SIZE < 16
#define MAX_INTEGER_OPERAND_SIZE 16
#endif
#endif /* #ifndef MAX_INTEGER_OPERAND_SIZE */

/* The following macro value is sign of integer number (0 or 1) given
   as macro parameter. */

#define INTEGER_SIGN(operand) (*(unsigned char *) (operand) >> (CHAR_BIT - 1))

/* This variable can have only two values 0 or 1.  The value `1'
   corresponds to overflow.  The variable value are modified by all
   functions of addition, subtract, multiplication, division,
   evaluation of remainder, shift, changing size and transformation of
   string into number fix overflow. */

int integer::overflow_bit;

/* This variable stores zero (unsigned) integer of maximal size. */

static unsigned char zero_constant_itself [MAX_INTEGER_OPERAND_SIZE];

/* This variable represents zero (unsigned) integer of any size. */

unsigned const char *integer::zero_constant = zero_constant_itself;

/* This variable contains pointer to function without parameters which
   will be called after fixing a integer overflow.  The function is
   called after setting up variable `overflow_bit'.*/

void (*unsigned_integer::overflow_reaction) (void)
  = integer::default_arithmetic_overflow_reaction;

/* This variable contains pointer to function without parameters which
   will be called after fixing a integer overflow.  The function is
   called after setting up variable `overflow_bit'.*/

void (*signed_integer::overflow_reaction) (void)
  = integer::default_arithmetic_overflow_reaction;



/* This page contains functions common for all package functions. */


#ifndef HAVE_MEMCPY

static void *
memcpy (void *to, const void *from, size_t size)
{
  char *cto = (char *) to;
  const char *cfrom = (const char *) from;

  while (size > 0)
    {
      *cto++ = *cfrom;
      size--;
    }
  return to;
}

#endif /* #ifndef HAVE_MEMCPY */

#ifndef HAVE_MEMSET

static void *
memset (void *to, int value, size_t size)
{
  char *cto  = (char *) to;

  while (size > 0)
    {
      *cto++ = value;
      size--;
    }
  return to;
}

#endif /* #ifndef HAVE_MEMSET */

#ifndef HAVE_MEMCMP

static int
memcmp (const void *mem1, const void *mem2, size_t size)
{
  const unsigned char *m1 = (const unsigned char *) mem1;
  const unsigned char *m2 = (const unsigned char *) mem2;

  while (size > 0)
    {
      if (m1 != m2)
        return (m1 < m2 ? -1 : 1);
      m1++;
      m2++;
      size--;
    }
  return 0;
}

#endif /* #ifndef HAVE_MEMCMP */

#ifndef HAVE_MEMMOVE

/* The following function is an analog of standard C function
   `memmove'.  The function returns the first operand. */

static void *
memmove (void *s1, const void *s2, size_t n)
{
  int i;

  assert (n >= 0);
  if ((char *) s1 < (char *) s2 && (char *) s1 + n <= (char *) s2
      || (char *) s2 < (char *) s1 && (char *) s2 + n <= (char *) s1)
    return (void *) memcpy (s1, s2, n);
  if ((char *) s1 < (char *) s2 && (char *) s1 + n > (char *) s2)
    for (i = 0; (size_t) i < n; i++)
      ((char *) s1) [i] = ((char *) s2) [i];
  else
    for (i = n - 1; i >= 0; i--)
      ((char *) s1)[i] = ((char *) s2) [i];
  return s1;
}

#endif /* #ifndef HAVE_MEMMOVE */

/* The following function adds unsigned integers.  The function
   returns 1 if unsigned integer overflow is fixed, 0 otherwise.
   Result can be placed in any operand. */

static int
add_unsigned_integer_without_overflow_reaction
  (int size, const void *op1, const void *op2, void *result)
{
  int digit_number;
  int carry;
  unsigned int sum;

  assert (size > 0);
  for (digit_number = size - 1, carry = 0; digit_number >= 0; digit_number--)
    {
      sum = (((unsigned char *) op1) [digit_number]
             + ((unsigned char *) op2) [digit_number] + carry);
      if (sum > UCHAR_MAX)
        {
          assert (sum < 2 * (UCHAR_MAX + 1));
          sum -= UCHAR_MAX + 1;
          carry = 1;
        }
      else
        carry = 0;
      ((unsigned char *) result) [digit_number] = sum;
    }
  return carry != 0;
}

/* The following function adds unsigned integers.  The function
   returns 1 if unsigned integer overflow (the first operand is less
   than the second) is fixed, 0 otherwise.  Result can be placed in
   any operand. */

static int
subtract_unsigned_integer_without_overflow_reaction
  (int size, const void *op1, const void *op2, void *result)
{
  int digit_number;
  int carry;
  int subtraction;

  assert (size > 0);
  for (digit_number = size - 1, carry = 0; digit_number >= 0; digit_number--)
    {
      subtraction = (((unsigned char *) op1) [digit_number]
                     - ((unsigned char *) op2) [digit_number] - carry);
      if (subtraction < 0)
        {
          assert (subtraction >= -(UCHAR_MAX + 1));
          subtraction += UCHAR_MAX + 1;
          carry = 1;
        }
      else
        carry = 0;
      ((unsigned char *) result) [digit_number] = subtraction;
    }
  return carry != 0;
}

/* The following function makes complementary code of number.  Result
   can be placed in operand. */

static void
make_complementary_code (int size, const void *operand, void *result)
{
  int digit_number;
  int carry;
  int subtraction;

  assert (size > 0);
  for (digit_number = size - 1, carry = 0; digit_number >= 0; digit_number--)
    {
      subtraction = (0 - ((unsigned char *) operand) [digit_number] - carry);
      if (subtraction != 0)
        {
          assert (subtraction >= -(UCHAR_MAX + 1));
          subtraction += UCHAR_MAX + 1;
          carry = 1;
        }
      else
        carry = 0;
      ((unsigned char *) result) [digit_number] = subtraction;
    }
}

/* The following function multiplys unsigned integer by digit (byte
   size).  The function returns 1 if unsigned integer overflow is
   fixed, 0 otherwise. */

static int
multiply_unsigned_integer_by_digit_without_overflow_reaction
 (int size, void *operand, unsigned int digit)
{
  int digit_number;
  unsigned int carry;
  unsigned int sum;

  assert (size > 0 && digit <= UCHAR_MAX);
  for (digit_number = size - 1, carry = 0; digit_number >= 0; digit_number--)
    {
      sum = (((unsigned char *) operand) [digit_number] * digit + carry);
      if (sum > UCHAR_MAX)
        {
          assert (sum < (unsigned long) (UCHAR_MAX + 1) * (UCHAR_MAX + 1));
          carry = sum / (UCHAR_MAX + 1);
          sum %= UCHAR_MAX + 1;
        }
      else
        carry = 0;
      ((unsigned char *) operand) [digit_number] = sum;
    }
  return carry != 0;
}



/* This page contains functions working with overflow reactions and
   initiation function. */

/* Originally reaction on all integer and unsigned integer overflow is
   equal to the following function.  The function does nothing. */

void
integer::default_arithmetic_overflow_reaction (void)
{
}

/* The function changes reaction on unsigned integer overflow.  The
   function returns previous unsigned integer overflow reaction
   function. */

void
(*unsigned_integer::set_overflow_reaction (void (*function) (void))) (void)
{
  void (*result) (void);

  result = unsigned_integer::overflow_reaction;
  unsigned_integer::overflow_reaction = function;
  return result;
}

/* The function changes reaction on integer overflow.  The function
   returns previous integer overflow reaction function. */

void
(*signed_integer::set_overflow_reaction (void (*function) (void))) (void)
{
  void (*result) (void);

  result = signed_integer::overflow_reaction;
  signed_integer::overflow_reaction = function;
  return result;
}



/* This page contains functions creating arbitrary precision minimal
   and maximum constants. */

/* The function creates given size maximal unsigned integer
   constant. */

void
unsigned_integer::maximum (int size, void *result)
{
  assert (size > 0);
  memset (result, UCHAR_MAX, (size_t) size);
}

/* The function creates given size minimal integer constant. */

void
signed_integer::minimum (int size, void *result)
{
  assert (size > 0);
  memset (result, 0, (size_t) size);
  *(unsigned char *) result = 1 << (CHAR_BIT - 1);
}

/* The function creates given size maximal integer constant. */

void
signed_integer::maximum (int size, void *result)
{
  assert (size > 0);
  memset (result, UCHAR_MAX, (size_t) size);
  *(unsigned char *) result = UCHAR_MAX >> 1;
}



/* This page contains functions for arbitrary precision addition. */

/* The function adds unsigned integers and fixes overflow reaction if
   it is needed.  The function makes this with the aid of function
   `add_unsigned_integer_without_overflow_reaction'.  Result can be
   placed in any operand. */

void
unsigned_integer::add (int size, const void *op1, const void *op2,
                       void *result)
{
  assert (size > 0);
  overflow_bit
    = add_unsigned_integer_without_overflow_reaction (size, op1, op2, result);
  if (overflow_bit != 0)
    (*overflow_reaction) ();
}

/* The function adds integers and fixes overflow reaction if it is
   needed.  The function makes this with the aid of function
   `add_unsigned_integer_without_overflow_reaction'.  Result can be
   placed in any operand. */

void
signed_integer::add (int size, const void *op1, const void *op2, void *result)
{
  int op1_sign;
  int sign_equality;

  assert (size > 0);
  op1_sign = INTEGER_SIGN (op1);
  sign_equality = INTEGER_SIGN (op1) == INTEGER_SIGN (op2);
  add_unsigned_integer_without_overflow_reaction (size, op1, op2, result);
  overflow_bit = sign_equality && (op1_sign != INTEGER_SIGN (result));
  if (overflow_bit != 0)
    (*overflow_reaction) ();
}



/* This page contains functions for arbitrary precision subtraction. */

/* The function subtracts unsigned integers and fixes overflow
   reaction if it is needed.  The function makes this with the aid of
   function `subtract_unsigned_integer_without_overflow_reaction'.
   Result can be placed in any operand. */

void
unsigned_integer::subtract (int size, const void *op1, const void *op2,
                            void *result)
{
  assert (size > 0);
  overflow_bit
    = subtract_unsigned_integer_without_overflow_reaction (size, op1, op2,
                                                           result);
  if (overflow_bit != 0)
    (*overflow_reaction) ();
}

/* The function subtracts integers and fixes overflow reaction if it
   is needed.  The function makes this with the aid of function
   `subtract_unsigned_integer_without_overflow_reaction'.  Result can
   be placed in any operand. */

void
signed_integer::subtract (int size, const void *op1, const void *op2,
                          void *result)
{
  int op1_sign;
  int sign_unequality;

  assert (size > 0);
  op1_sign = INTEGER_SIGN (op1);
  sign_unequality = INTEGER_SIGN (op1) != INTEGER_SIGN (op2);
  subtract_unsigned_integer_without_overflow_reaction (size, op1, op2, result);
  overflow_bit = sign_unequality && (op1_sign != INTEGER_SIGN (result));
  if (overflow_bit != 0)
    (*overflow_reaction) ();
}



/* This page contains functions for arbitrary precision multiplication. */

/* The following function multiplys unsigned integers.  The function
   returns 1 if unsigned integer overflow is fixed, 0 otherwise.
   Result can be placed in any operand. */

static int
multiply_unsigned_integer_without_overflow_reaction
  (int size, const void *op1, const void *op2, void *result)
{
  int op1_digit_number;
  int op2_digit_number;
  int carry;
  unsigned long int partial_sum;
  int result_digit_number;
  int overflow_flag;
  unsigned char long_result [2 * MAX_INTEGER_OPERAND_SIZE];
  
  assert (size > 0);
  assert (size <= MAX_INTEGER_OPERAND_SIZE);
  memset (long_result + size, 0, (size_t) size);  
  for (op2_digit_number = size - 1; op2_digit_number >= 0; op2_digit_number--)
    {
      if (((unsigned char *) op2) [op2_digit_number] != 0)
        {
          for (op1_digit_number = size - 1, carry = 0; op1_digit_number >= 0;
               op1_digit_number--)
            {
              partial_sum
                = (((unsigned char *) op1) [op1_digit_number]
                   * ((unsigned char *) op2) [op2_digit_number]
                   + long_result [op1_digit_number + op2_digit_number + 1]
                   + carry);
              assert (partial_sum
		      < (unsigned long) (UCHAR_MAX + 1) * (UCHAR_MAX + 1));
              long_result [op1_digit_number + op2_digit_number + 1]
                = (unsigned char) (partial_sum % (UCHAR_MAX + 1));
              carry = partial_sum / (UCHAR_MAX + 1);
            }
          long_result [op2_digit_number] = carry;
        }
      else
        long_result [op2_digit_number] = 0;
    }
  overflow_flag = 0;
  for (result_digit_number = size - 1; result_digit_number >= 0;
       result_digit_number--)
    if (long_result [result_digit_number] != 0)
      {
        overflow_flag = 1;
        break;
      }      
  memcpy (result, long_result + size, (size_t) size);
  return overflow_flag;
}

/* The following function multiplys unsigned integers and fixes
   overflow reaction if it is needed.  The function makes this with
   the aid of function
   `multiply_unsigned_integer_without_overflow_reaction'.  Result can
   be placed in any operand. */

void
unsigned_integer::multiply (int size, const void *op1, const void *op2,
                            void *result)
{
  assert (size > 0);
  overflow_bit =
    multiply_unsigned_integer_without_overflow_reaction (size, op1, op2,
                                                         result);
  if (overflow_bit)
    (*overflow_reaction) ();
}

/* The function multiplys integers and fixes overflow reaction if it
   is needed.  The function makes this with the aid of function
   `multiply_unsigned_integer_without_overflow_reaction'.  Result can
   be placed in any operand. */

void
signed_integer::multiply (int size, const void *op1, const void *op2,
                          void *result)
{
  int negative_result_flag;
  unsigned char op1_complementary [MAX_INTEGER_OPERAND_SIZE];
  unsigned char op2_complementary [MAX_INTEGER_OPERAND_SIZE];
  unsigned const char *abs_op1;
  unsigned const char *abs_op2;
  int unsigned_result_sign;

  assert (size > 0);
  assert (size <= MAX_INTEGER_OPERAND_SIZE);
  negative_result_flag = INTEGER_SIGN (op1) != INTEGER_SIGN (op2);
  if (INTEGER_SIGN (op1))
    {
      /* May be integer overflow. But result is correct because
         it is unsigned. */
      make_complementary_code (size, op1, op1_complementary);
      abs_op1 = op1_complementary;
    }
  else
    abs_op1 = (unsigned const char *) op1;
  if (INTEGER_SIGN (op2))
    {
      /* May be integer overflow.  But result is correct because
         it is unsigned. */
      make_complementary_code (size, op2, op2_complementary);
      abs_op2 = op2_complementary;
    }
  else
    abs_op2 = (unsigned const char *) op2;
  overflow_bit =
    multiply_unsigned_integer_without_overflow_reaction (size, abs_op1,
                                                         abs_op2, result);
  unsigned_result_sign = INTEGER_SIGN (result);
  if (negative_result_flag)
    make_complementary_code (size, result, result);
  if (unsigned_result_sign
      && (!negative_result_flag
          || INTEGER_SIGN (result) != unsigned_result_sign))
    /* Unsigned result can not be represented as integer. */
    overflow_bit = 1;
  if (overflow_bit)
    (*overflow_reaction) ();
}



/* This page contains functions for arbitrary precision division. */

/* The following function divides unsigned integers.  The function
   returns 1 if unsigned integer overflow (division by zero) is fixed,
   0 otherwise.  Result can be placed in any operand.  See algorithm
   in Knuth's book. */

static int
divide_unsigned_integer_without_overflow_reaction
  (int size, const void *op1, const void *op2, void *result)
{
  int scaled_op1_digit_number;
  unsigned int q_approximation;
  int first_nonzero_digit_number;
  int op2_digit_number;
  unsigned int scale;
  unsigned char scaled_op1 [MAX_INTEGER_OPERAND_SIZE + 1];
  unsigned char normalized_op2 [MAX_INTEGER_OPERAND_SIZE];
  unsigned char extended_normalized_op2 [MAX_INTEGER_OPERAND_SIZE + 1];
#ifndef NDEBUG
  int iterations;
#endif

  assert (size > 0);
  assert (size <= MAX_INTEGER_OPERAND_SIZE);
  for (op2_digit_number = 0; op2_digit_number < size; op2_digit_number++)
    if (((unsigned char *) op2) [op2_digit_number] != 0)
      break;
  first_nonzero_digit_number = op2_digit_number;
  if (first_nonzero_digit_number == size)
    {
      /* Zero divisor */
      memset (result, 0, (size_t) size);
      return 1 /* TRUE */;
    }
  else if (first_nonzero_digit_number == size - 1)
    {
      /* Division by digit. */
      int digit_number;
      int digit;
      unsigned long divisable;
      unsigned long remainder;

      digit = ((unsigned char *) op2) [first_nonzero_digit_number];
      memcpy (result, op1, (size_t) size);
      remainder = 0;
      for (digit_number = 0; digit_number < size; digit_number++)
        {
          divisable = (remainder * (UCHAR_MAX + 1)
                       + ((unsigned char *) result) [digit_number]);
          remainder = divisable % digit;
          ((unsigned char *) result) [digit_number]
            = (unsigned char) (divisable / digit);
        }
      return 0 /* FALSE */;
    }
  /* Normalization of divisor. */
  scale = (UCHAR_MAX + 1) / (((unsigned char *) op2) [op2_digit_number] + 1);
  memcpy (scaled_op1 + 1, op1, (size_t) size);
  *scaled_op1 = 0;
#ifndef NDEBUG
  assert (!multiply_unsigned_integer_by_digit_without_overflow_reaction
          (size + 1, scaled_op1, scale));
#else
  multiply_unsigned_integer_by_digit_without_overflow_reaction
    (size + 1, scaled_op1, scale);
#endif
  memcpy (normalized_op2, op2, (size_t) size);
#ifndef NDEBUG
  assert (!multiply_unsigned_integer_by_digit_without_overflow_reaction
          (size, normalized_op2, scale));
#else
  multiply_unsigned_integer_by_digit_without_overflow_reaction
    (size, normalized_op2, scale);
#endif
  for (scaled_op1_digit_number = 0;
       scaled_op1_digit_number <= first_nonzero_digit_number;
       scaled_op1_digit_number++)
    {
      /* Division of `scaled_op1[scaled_op1_digit_number]..scaled_op1[size]' by
         `normalized_op2[first_nonzero_digit_number]..normalized_op2[size-1]'
         for evaluation of one digit of quotient
         `result[size-1-first_nonzero_digit_number-scaled_op1_digit_number]'.
         */
      if (scaled_op1 [scaled_op1_digit_number]
          == normalized_op2 [first_nonzero_digit_number])
        q_approximation = UCHAR_MAX;
      else
        q_approximation
          = (scaled_op1 [scaled_op1_digit_number] * (UCHAR_MAX + 1)
             + scaled_op1 [scaled_op1_digit_number + 1])
            / normalized_op2 [first_nonzero_digit_number];
#ifndef NDEBUG      
      iterations = 0;
#endif
      while (normalized_op2 [first_nonzero_digit_number + 1] * q_approximation
             > (((unsigned long int) scaled_op1 [scaled_op1_digit_number]
                 * (UCHAR_MAX + 1)
                 + scaled_op1 [scaled_op1_digit_number + 1]
                 - q_approximation
                 * normalized_op2 [first_nonzero_digit_number])
                * (UCHAR_MAX + 1) + scaled_op1 [scaled_op1_digit_number + 2]))
        {
          q_approximation --;
#ifndef NDEBUG      
          iterations++;
          assert (iterations <= 2);
#endif
        }
      /* Multiply and subtract */
      memcpy (extended_normalized_op2 + 1,
              normalized_op2 + first_nonzero_digit_number,
              (size_t) (size - first_nonzero_digit_number));
      *extended_normalized_op2 = 0;
      multiply_unsigned_integer_by_digit_without_overflow_reaction
        (size - first_nonzero_digit_number + 1, extended_normalized_op2,
         q_approximation);
      if (subtract_unsigned_integer_without_overflow_reaction
          (size - first_nonzero_digit_number + 1,
           scaled_op1 + scaled_op1_digit_number, extended_normalized_op2,
           scaled_op1 + scaled_op1_digit_number))
        {
          /* Negative result.  Compensation by addition. */
          q_approximation--;
          memcpy (extended_normalized_op2 + 1,
                  normalized_op2 + first_nonzero_digit_number,
                  (size_t) (size - first_nonzero_digit_number));
          *extended_normalized_op2 = 0;
#ifndef NDEBUG
          assert (add_unsigned_integer_without_overflow_reaction
                  (size - first_nonzero_digit_number + 1,
                   scaled_op1 + scaled_op1_digit_number,
                   extended_normalized_op2,
                   scaled_op1 + scaled_op1_digit_number));
#else
          add_unsigned_integer_without_overflow_reaction
            (size - first_nonzero_digit_number + 1,
             scaled_op1 + scaled_op1_digit_number, extended_normalized_op2,
             scaled_op1 + scaled_op1_digit_number);
#endif
        }
      ((unsigned char *) result) [size - 1 - first_nonzero_digit_number
                                  + scaled_op1_digit_number] = q_approximation;
    }
  memset (result, 0, (size_t) (size - 1 - first_nonzero_digit_number));
  return 0 /* TRUE */;
}

/* The function divides unsigned integers and fixes overflow reaction
   if it is needed.  The function makes this with the aid of function
   `divide_unsigned_integer_without_overflow_reaction'.  Result can be
   placed in any operand. */

void
unsigned_integer::divide (int size, const void *op1, const void *op2,
                          void *result)
{
  assert (size > 0);
  overflow_bit =
    divide_unsigned_integer_without_overflow_reaction (size, op1, op2, result);
  if (overflow_bit)
    (*overflow_reaction) ();
}

/* The function divides integers and fixes overflow reaction if it is
   needed.  The function makes this with the aid of function
   `divide_unsigned_integer_without_overflow_reaction'.  Result can be
   placed in any operand. */

void
signed_integer::divide (int size, const void *op1, const void *op2,
                        void *result)
{
  int negative_result_flag;
  unsigned char op1_complementary [MAX_INTEGER_OPERAND_SIZE];
  unsigned char op2_complementary [MAX_INTEGER_OPERAND_SIZE];
  unsigned const char *abs_op1;
  unsigned const char *abs_op2;
  int unsigned_result_sign;

  assert (size > 0);
  assert (size <= MAX_INTEGER_OPERAND_SIZE);
  negative_result_flag = INTEGER_SIGN (op1) != INTEGER_SIGN (op2);
  if (INTEGER_SIGN (op1))
    {
      /* May be integer overflow for minimal int. But result is correct because
         it is unsigned. */
      make_complementary_code (size, op1, op1_complementary);
      abs_op1 = op1_complementary;
    }
  else
    abs_op1 = (unsigned const char *) op1;
  if (INTEGER_SIGN (op2))
    {
      /* May be integer overflow for minimal int.  But result is correct
         because it is unsigned. */
      make_complementary_code (size, op2, op2_complementary);
      abs_op2 = op2_complementary;
    }
  else
    abs_op2 = (unsigned const char *) op2;
  overflow_bit =
    divide_unsigned_integer_without_overflow_reaction (size, abs_op1,
                                                       abs_op2, result);
  unsigned_result_sign = INTEGER_SIGN (result);
  if (negative_result_flag)
    make_complementary_code (size, result, result);
  if (unsigned_result_sign
      && (!negative_result_flag
          || INTEGER_SIGN (result) != unsigned_result_sign))
    /* Unsigned result can not be represented as integer. */
    overflow_bit = 1;
  if (overflow_bit)
    (*overflow_reaction) ();
}



/* This page contains functions for arbitrary precision evaluation of
   remainder. */

/* The function evaluates remainder of division of unsigned integers
   as `op1 - (op1/op2)*op2' and fixes overflow reaction if it is
   needed.  Result can be placed in any operand. */

void
unsigned_integer::remainder (int size, const void *op1, const void *op2,
                             void *result)
{
  unsigned char temporary [MAX_INTEGER_OPERAND_SIZE];

  assert (size > 0);
  divide (size, op1, op2, temporary);
  if (overflow_bit)
    /* Reaction on zero is called from `divide'. */
    memset (result, 0, (size_t) size);
  else
    {
      multiply (size, temporary, op2, temporary);
      assert (!overflow_bit);
      subtract (size, op1, temporary, result);
      assert (!overflow_bit);
    }
}

/* The function evaluates remainder of division of integers as `op1 -
   (op1/op2)*op2' and fixes overflow reaction if it is needed.  Result
   can be placed in any operand. */

void
signed_integer::remainder (int size, const void *op1, const void *op2,
                           void *result)
{
  unsigned char temporary [MAX_INTEGER_OPERAND_SIZE];

  assert (size > 0);
  divide (size, op1, op2, temporary);
  if (overflow_bit)
    /* Reaction on zero is called from `divide'. */
    memset (result, 0, (size_t) size);
  else
    {
      multiply (size, temporary, op2, temporary);
      assert (!overflow_bit);
      subtract (size, op1, temporary, result);
      assert (!overflow_bit);
    }
}



/* This page contains functions for arbitrary precision number shifts. */

/* This function makes right shift of unsigned integer of given size
   on given number of bits.  If number of bits is negative the
   function makes shift to left actually with the aid of function
   `unsigned_integer::shift_left'.  The function fixes overflow when
   result can not be represented by number of given size, i.e. in
   other words the opposite unsigned shift (to left) results in number
   not equal to source operand.  Result can be placed in operand. */

void
unsigned_integer::shift_right (int size, const void *operand, int bits,
                               void *result)
{
  int byte_number;
  unsigned byte;
  unsigned carry;
  int bit_shift;
  int byte_shift;

  if (bits < 0)
    shift_left (size, operand, -bits, result);
  else
    {
      assert (size > 0);
      overflow_bit = 0;
      byte_shift = bits / CHAR_BIT;
      bit_shift = bits % CHAR_BIT;
      for (byte_number = (byte_shift >= size ? 0 : size - byte_shift);
           byte_number < size; byte_number++)
        if (((unsigned char *) operand) [byte_number] != 0)
          {
            overflow_bit = 1;
            break;
          }
      if (byte_shift >= size)
        memset (result, 0, (size_t) size);
      else
        {
          memmove ((char *) result + byte_shift, operand,
                   (size_t) (size - byte_shift));
          memset (result, 0, (size_t) byte_shift);
          if (bit_shift == 0)
            return;
          for (byte_number = byte_shift, carry = 0; byte_number < size;
               byte_number++)
            {
              byte = ((unsigned char *) result) [byte_number];
              ((unsigned char *) result) [byte_number]
                = carry | (byte >> bit_shift);
              carry = (byte << (CHAR_BIT - bit_shift)) & UCHAR_MAX;
            }
          if (carry != 0)
            overflow_bit = 1;
        }
      if (overflow_bit)
        (*overflow_reaction) ();
    }
}

/* This function makes right arithmetic shift of integer of given size
   on given number of bits.  If number of bits is negative the
   function makes shift to left actually with the aid of function
   `signed_integer::shift_left'.  The function fixes overflow when result can
   not be represented by number of given size, i.e. in other words the
   opposite shift (to left) results in number not equal to source
   operand.  Result can be placed in operand. */

void
signed_integer::shift_right (int size, const void *operand, int bits,
                             void *result)
{
  int byte_number;
  unsigned byte;
  unsigned carry;
  int bit_shift;
  int byte_shift;
  int operand_sign;

  if (bits < 0)
    shift_left (size, operand, -bits, result);
  else
    {
      assert (size > 0);
      operand_sign = INTEGER_SIGN (operand);
      overflow_bit = 0;
      byte_shift = bits / CHAR_BIT;
      bit_shift = bits % CHAR_BIT;
      for (byte_number = (byte_shift >= size ? 0 : size - byte_shift);
           byte_number < size; byte_number++)
        if (((unsigned char *) operand) [byte_number] != 0)
          {
            overflow_bit = 1;
            break;
          }
      if (byte_shift >= size)
        memset (result, (operand_sign ? UCHAR_MAX : 0), (size_t) size);
      else
        {
          memmove ((char *) result + byte_shift, operand,
                   (size_t) (size - byte_shift));
          memset (result, (operand_sign ? UCHAR_MAX : 0), (size_t) byte_shift);
          if (bit_shift == 0)
            return;
          carry = (((operand_sign ? UCHAR_MAX : 0) << (CHAR_BIT - bit_shift))
                   & UCHAR_MAX);
          for (byte_number = byte_shift; byte_number < size; byte_number++)
            {
              byte = ((unsigned char *) result) [byte_number];
              ((unsigned char *) result) [byte_number]
                = carry | (byte >> bit_shift);
              carry = (byte << (CHAR_BIT - bit_shift)) & UCHAR_MAX;
            }
          if (carry != 0)
            overflow_bit = 1;
        }
      if (overflow_bit)
        (*overflow_reaction) ();
    }
}

/* This function makes left shift of unsigned integer of given size on
   given number of bits.  If number of bits is negative the function
   makes shift to left actually with the aid of function
   `unsigned_integer::shift_right'.  The function fixes overflow when
   result can not be represented by number of given size, i.e. i.e. in
   other words the opposite shift (to right) results in number not
   equal to source operand.  Result can be placed in operand. */

void
unsigned_integer::shift_left (int size, const void *operand, int bits,
                              void *result)
{
  int byte_number;
  unsigned byte;
  unsigned carry;
  int bit_shift;
  int byte_shift;

  if (bits < 0)
    shift_right (size, operand, -bits, result);
  else
    {
      assert (size > 0);
      overflow_bit = 0;
      byte_shift = bits / CHAR_BIT;
      bit_shift = bits % CHAR_BIT;
      for (byte_number = 0; byte_number < byte_shift && byte_number < size;
           byte_number++)
        if (((unsigned char *) operand) [byte_number] != 0)
          {
            overflow_bit = 1;
            break;
          }
      if (byte_shift >= size)
        memset (result, 0, (size_t) size);
      else
        {
          memmove (result, (char *) operand + byte_shift,
                   (size_t) (size - byte_shift));
          memset ((char *) result + (size - byte_shift), 0,
                  (size_t) byte_shift);
          if (bit_shift == 0)
            return;
          for (byte_number = size - byte_shift - 1, carry = 0;
               byte_number >= 0; byte_number--)
            {
              byte = ((unsigned char *) result) [byte_number];
              ((unsigned char *) result) [byte_number]
                = carry | (byte << bit_shift);
              carry = byte >> (CHAR_BIT - bit_shift);
            }
          if (carry != 0)
            overflow_bit = 1;
        }
      if (overflow_bit)
        (*overflow_reaction) ();
    }
}

/* This function makes left arithmetic shift of integer of given size
   on given number of bits.  If number of bits is negative the
   function makes shift to left actually with the aid of function
   `signed_integer::shift_right'.  The function fixes overflow when
   result can not be represented by number of given size, i.e. in
   other words the opposite shift (to right) results in number not
   equal to source operand.  Result can be placed in operand. */

void
signed_integer::shift_left (int size, const void *operand, int bits,
                            void *result)
{
  int byte_number;
  unsigned byte;
  unsigned carry;
  int bit_shift;
  int byte_shift;
  int operand_sign;

  if (bits < 0)
    shift_right (size, operand, -bits, result);
  else
    {
      assert (size > 0);
      operand_sign = INTEGER_SIGN (operand);
      overflow_bit = 0;
      byte_shift = bits / CHAR_BIT;
      bit_shift = bits % CHAR_BIT;
      for (byte_number = 0; byte_number < byte_shift && byte_number < size;
           byte_number++)
        if (((unsigned char *) operand) [byte_number]
            != (operand_sign ? UCHAR_MAX : 0))
          {
            overflow_bit = 1;
            break;
          }
      if (byte_shift >= size)
        memset (result, 0, (size_t) size);
      else
        {
          memmove (result, (char *) operand + byte_shift,
                   (size_t) (size - byte_shift));
          memset ((char *) result + (size - byte_shift), 0,
                  (size_t) byte_shift);
          if (bit_shift == 0)
            return;
          for (byte_number = size - byte_shift - 1, carry = 0;
               byte_number >= 0; byte_number--)
            {
              byte = ((unsigned char *) result) [byte_number];
              ((unsigned char *) result) [byte_number]
                = carry | (byte << bit_shift);
              carry = byte >> (CHAR_BIT - bit_shift);
            }
          if (carry != ((unsigned) (operand_sign ? UCHAR_MAX : 0)
                        >> (CHAR_BIT - bit_shift)))
            overflow_bit = 1;
        }
      if (operand_sign != INTEGER_SIGN (result))
        overflow_bit = 1;
      if (overflow_bit)
        (*overflow_reaction) ();
    }
}



/* This page contains functions for bitwise operations of arbitrary
   precision numbers. */

/* This function makes bitwise `or' of two integers of given size. */

void
signed_integer::_or_ (int size, const void *op1, const void *op2, void *result)
{
  int byte_number;

  assert (size > 0);
  for (byte_number = 0; byte_number < size; byte_number++)
    ((unsigned char *) result) [byte_number]
      = ((unsigned char *) op1) [byte_number]
        | ((unsigned char *) op2) [byte_number];
}

/* This function makes bitwise `or' of two unsigned integers of given
   size. */

void
unsigned_integer::_or_ (int size, const void *op1, const void *op2,
                        void *result)
{
  signed_integer::_or_ (size, op1, op2, result);
}


/* This function makes bitwise `and' of two integers of given size. */

void
signed_integer::_and_ (int size, const void *op1, const void *op2,
                       void *result)
{
  int byte_number;

  assert (size > 0);
  for (byte_number = 0; byte_number < size; byte_number++)
    ((unsigned char *) result) [byte_number]
      = ((unsigned char *) op1) [byte_number]
        & ((unsigned char *) op2) [byte_number];
}

/* This function makes bitwise `and' of two unsigned integers of given
   size. */

void
unsigned_integer::_and_ (int size, const void *op1, const void *op2,
                         void *result)
{
  signed_integer::_and_ (size, op1, op2, result);
}


/* This function makes bitwise `xor' of two integers of given size. */

void
signed_integer::_xor_ (int size, const void *op1, const void *op2,
                       void *result)
{
  int byte_number;

  assert (size > 0);
  for (byte_number = 0; byte_number < size; byte_number++)
    ((unsigned char *) result) [byte_number]
      = ((unsigned char *) op1) [byte_number]
        ^ ((unsigned char *) op2) [byte_number];
}

/* This function makes bitwise `xor' of two unsigned integers of given
   size. */

void
unsigned_integer::_xor_ (int size, const void *op1, const void *op2,
                         void *result)
{
  signed_integer::_xor_ (size, op1, op2, result);
}


/* This function makes bitwise `not' of integer of given size. */

void
signed_integer::_not_ (int size, const void *operand, void *result)
{
  int byte_number;

  assert (size > 0);
  for (byte_number = 0; byte_number < size; byte_number++)
    ((unsigned char *) result) [byte_number]
      = ((unsigned char *) operand) [byte_number] ^ UCHAR_MAX;
}

/* This function makes bitwise `not' of unsigned integer of given
   size. */

void
unsigned_integer::_not_ (int size, const void *operand, void *result)
{
  signed_integer::_not_ (size, operand, result);
}



/* This page contains functions for comparison of arbitrary precision
   numbers. */

/* This function compares two unsigned integers of given size on
   equality.  The function returns 1 if unsigned integers are equal, 0
   otherwise. */

int
unsigned_integer::eq (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  return memcmp (op1, op2, (size_t) size) == 0;
}

/* This function compares two integers of given size on equality.  The
   function returns 1 if integers are equal, 0 otherwise. */

int
signed_integer::eq (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  return memcmp (op1, op2, (size_t) size) == 0;
}

/* This function compares two unsigned integers of given size on
   inequality.  The function returns 1 if unsigned integers are not
   equal, 0 otherwise. */

int
unsigned_integer::ne (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  return memcmp (op1, op2, (size_t) size) != 0;
}

/* This function compares two integers of given size on inequality.
   The function returns 1 if integers are not equal, 0 otherwise. */

int
signed_integer::ne (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  return memcmp (op1, op2, (size_t) size) != 0;
}


/* This function compares two memory parts of given size on that the
   first operand is greater than the second.  The bytes are described
   as unsigned.  The function returns 1 if the first operand is
   greater than the second, - 1 if the first operand is less than the
   second, 0 otherwise. */

static int
bytes_comparison (const void *op1, const void *op2, int size)
{
  const unsigned char *str1 = (const unsigned char *) op1;
  const unsigned char *str2 = (const unsigned char *) op2;

  assert (size > 0);
  while (size > 0 && *str1 == *str2)
    {
      str1++;
      str2++;
      size--;
    }
  if (size <= 0)
    return 0;
  else if (*str1 > *str2)
    return 1;
  else
    return -1;
}

/* This function compares two unsigned integers of given size on that
   the first operand is greater than the second.  The function returns
   1 if the first unsigned integer is greater than the second, 0
   otherwise. */

int
unsigned_integer::gt (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  return bytes_comparison (op1, op2, size) > 0;
}

/* This function compares two integers of given size on that the first
   operand is greater than the second.  The function returns 1 if the
   first integer is greater than the second, 0 otherwise. */

int
signed_integer::gt (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  if (INTEGER_SIGN (op1) == 0)
    {
      if (INTEGER_SIGN (op2) == 0)
        return bytes_comparison (op1, op2, size) > 0;
      else
        return 1; /* TRUE */
    }
  else if (INTEGER_SIGN (op2) == 0)
    return 0; /*FALSE*/
  else
    return bytes_comparison (op1, op2, size) > 0;
}

/* This function compares two unsigned integers of given size on that
   the first operand is less than the second.  The function returns 1
   if the first unsigned integer is less than the second, 0
   otherwise. */

int
unsigned_integer::lt (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  return bytes_comparison (op1, op2, size) < 0;
}

/* This function compares two integers of given size on that the first
   operand is less than the second.  The function returns 1 if the
   first integer is less than the second, 0 otherwise. */

int
signed_integer::lt (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  if (INTEGER_SIGN (op1) == 0)
    {
      if (INTEGER_SIGN (op2) == 0)
        return bytes_comparison (op1, op2, size) < 0;
      else
        return 0; /*FALSE*/
    }
  else if (INTEGER_SIGN (op2) == 0)
    return 1; /* TRUE */
  else
    return bytes_comparison (op1, op2, size) < 0;
}

/* This function compares two unsigned integers of given size on that
   the first operand is greater than or equal to the second.  The
   function returns 1 if the first unsigned integer is greater than or
   equal to the second, 0 otherwise. */

int
unsigned_integer::ge (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  return bytes_comparison (op1, op2, size) >= 0;
}

/* This function compares two integers of given size on that the first
   operand is greater than or equal to the second.  The function
   returns 1 if the first integer is greater than or equal to the
   second, 0 otherwise. */

int
signed_integer::ge (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  if (INTEGER_SIGN (op1) == 0)
    {
      if (INTEGER_SIGN (op2) == 0)
        return bytes_comparison (op1, op2, size) >= 0;
      else
        return 1; /* TRUE */
    }
  else if (INTEGER_SIGN (op2) == 0)
    return 0; /*FALSE*/
  else
    return bytes_comparison (op1, op2, size) >= 0;
}

/* This function compares two unsigned integers of given size on that
   the first operand is less than or equal to the second.  The
   function returns 1 if the first unsigned integer is less than or
   equal to the second, 0 otherwise. */

int
unsigned_integer::le (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  return bytes_comparison (op1, op2, size) <= 0;
}

/* This function compares two integers of given size on that the first
   operand is less than or equal to the second.  The function returns
   1 if the first integer is less than or equal to the second, 0
   otherwise. */

int
signed_integer::le (int size, const void *op1, const void *op2)
{
  assert (size > 0);
  if (INTEGER_SIGN (op1) == 0)
    {
      if (INTEGER_SIGN (op2) == 0)
        return bytes_comparison (op1, op2, size) <= 0;
      else
        return 0; /*FALSE*/
    }
  else if (INTEGER_SIGN (op2) == 0)
    return 1; /* TRUE */
  else
    return bytes_comparison (op1, op2, size) <= 0;
}



/* This page contains functions for changing size of arbitrary
   precision numbers. */

/* The function changes size of unsigned integer.  The function fixes
   overflow when result can not be represented by number of given
   size.  Result can be placed in operand. */

void
unsigned_integer::change_size (int operand_size, const void *operand,
                               int result_size, void *result)
{
  int operand_digit_number;

  assert (operand_size > 0 && result_size > 0);
  overflow_bit = 0;
  if (operand_size <= result_size)
    {
      memmove ((char *) result + result_size - operand_size, operand,
              (size_t) operand_size);
      memset (result, 0, (size_t) (result_size - operand_size));
    }
  else
    {
      for (operand_digit_number = 0;
           operand_digit_number < operand_size - result_size;
           operand_digit_number++)
        if (((unsigned char *) operand) [operand_digit_number] != 0)
          {
            overflow_bit = 1;
            break;
          }
      memmove (result, (char *) operand + operand_size - result_size,
              (size_t) result_size);
    }
  if (overflow_bit)
    (*overflow_reaction) ();
}

/* The function changes size of integer.  The function fixes overflow
   when result can not be represented by number of given size.  Result
   can be placed in operand. */

void
signed_integer::change_size (int operand_size, const void *operand,
                             int result_size, void *result)
{
  int operand_digit_number;
  int operand_sign;

  overflow_bit = 0;
  operand_sign = INTEGER_SIGN (operand);
  if (operand_size <= result_size)
    {
      memmove ((char *) result + result_size - operand_size, operand,
              (size_t) operand_size);
      memset (result, (operand_sign ? UCHAR_MAX : 0),
              (size_t) (result_size - operand_size));
    }
  else
    {
      for (operand_digit_number = 0;
           operand_digit_number < operand_size - result_size;
           operand_digit_number++)
        if (((unsigned char *) operand) [operand_digit_number]
            != (operand_sign ? UCHAR_MAX : 0))
          {
            overflow_bit = 1;
            break;
          }
      memmove (result, (char *) operand + operand_size - result_size,
              (size_t) result_size);
      if (operand_sign != INTEGER_SIGN (result))
          overflow_bit = 1;
    }
  if (overflow_bit)
    (*overflow_reaction) ();
}



/* This page contains functions for conversion of arbitrary precision
   numbers to ascii representation. */

/* This function transforms unsigned integer of given size to BASE
   ascii representation.  BASE should be between 2 and 36 including
   them.  Digits more 9 are represented by 'a', 'b' etc.  Sign is
   absent in result string.  The function returns the result
   string. */

char *
unsigned_integer::to_based_string (int size, const void *operand, int base,
				   char *result)
{
  int digit_number;
  int i;
  unsigned long divisable;
  unsigned long remainder;
  int nonzero_flag;
  int length;
  int temporary;
  unsigned char operand_copy [MAX_INTEGER_OPERAND_SIZE];

  assert (base >= 2 && base <= 36);
  assert (size > 0);
  assert (size <= MAX_INTEGER_OPERAND_SIZE);
  memcpy (operand_copy, operand, (size_t) size);
  length = 0;
  do {
    nonzero_flag = 0 /* FALSE */;
    for (digit_number = 0, remainder = 0; digit_number < size; digit_number++)
      {
        divisable = remainder * (UCHAR_MAX + 1) + operand_copy [digit_number];
        remainder = divisable % base;
        operand_copy [digit_number] = (unsigned char) (divisable / base);
        if (operand_copy [digit_number] != 0)
          nonzero_flag = 1 /* TRUE */;
      }
    result [length++] = (unsigned char) (remainder < 10 ? '0' + remainder
					 : 'a' + remainder - 10);
  } while (nonzero_flag);
  result [length] = '\0';
  for (i = 0; i < length/2; i++)
    {
      temporary = result [i];
      result [i] = result [length - i - 1];
      result [length - i - 1] = temporary;
    }
  return result;
}

/* This function transforms unsigned integer of given size to decimal
   ascii representation.  Sign is absent in result string.  The
   function returns the result string. */

char *
unsigned_integer::to_string (int size, const void *operand, char *result)
{
  return to_based_string (size, operand, 10, result);
}

/* This function transforms integer of given size to BASE ascii
   representation.  BASE should be between 2 and 36 including them.
   Digits more 9 are represented by 'a', 'b' etc.  Sign is present in
   result string only for negative numbers.  The function returns the
   result string. */

char *
signed_integer::to_based_string (int size, const void *operand, int base,
				 char *result)
{
  unsigned char operand_copy [MAX_INTEGER_OPERAND_SIZE];

  if (!INTEGER_SIGN (operand))
    return unsigned_integer::to_based_string (size, operand, base, result);
  assert (size > 0);
  memcpy (operand_copy, operand, (size_t) size);
  /* May be integer overflow. But result is correct because it is unsigned. */
  make_complementary_code (size, operand_copy, operand_copy);
  *result = '-';
  unsigned_integer::to_based_string (size, operand_copy, base, result + 1);
  return result;
}

/* This function transforms integer of given size to decimal ascii
   representation.  Sign is present in result string only for negative
   numbers.  The function returns the result string. */

char *
signed_integer::to_string (int size, const void *operand, char *result)
{
  return to_based_string (size, operand, 10, result);
}



/* This page contains functions for conversion of decimal ascii
   representation to arbitrary precision numbers.  */

/* The function adds digit (byte size) to unsigned integer.  The
   function returns 1 if unsigned integer overflow is fixed, 0
   otherwise. */

static int
add_digit_to_unsigned_integer_without_overflow_reaction
 (int size, void *operand, unsigned int digit)
{
  int digit_number;
  unsigned int carry;
  unsigned int sum;

  assert (size > 0 && digit <= UCHAR_MAX);
  for (digit_number = size - 1, carry = digit; digit_number >= 0;
       digit_number--)
    {
      sum = ((unsigned char *) operand) [digit_number] + carry;
      if (sum > UCHAR_MAX)
        {
          assert (sum < (unsigned long) (UCHAR_MAX + 1) * (UCHAR_MAX + 1));
          carry = sum / (UCHAR_MAX + 1);
          sum %= UCHAR_MAX + 1;
        }
      else
        carry = 0;
      ((unsigned char *) operand) [digit_number] = sum;
    }
  return carry != 0;
}

/* This function transforms source string (BASE ascii representation
   without sign) to given size unsigned integer and returns pointer to
   first non digit in the source string through a parameter.  BASE
   should be between 2 and 36 including them.  If the string started
   with invalid integer representation the result will be zero and
   returns the operand through the parameter.  The function returns 1
   if unsigned integer overflow is fixed, 0 otherwise. */

static int
string_to_unsigned_integer_without_overflow_reaction
  (int size, const char *operand, void *result, char **first_nondigit,
   int base)
{
  int overflow_flag;

  assert (base >= 2 && base <= 36);
  memset (result, 0, (size_t) size);
  for (overflow_flag = 0;
       (isdigit (*operand) && *operand - '0' < base)
	 || (base > 10
	     && ((*operand >= 'a' && *operand < 'a' + base - 10)
		 || (*operand >= 'A' && *operand < 'A' + base - 10)));
       operand++)
    {
      overflow_flag
        = overflow_flag
          || multiply_unsigned_integer_by_digit_without_overflow_reaction
             (size, result, base);
      overflow_flag
        = overflow_flag
          || add_digit_to_unsigned_integer_without_overflow_reaction
             (size, result,
	      isdigit (*operand) ? *operand - '0'
	      : *operand >= 'A' && *operand <= 'F' ? *operand - 'A' + 10
	      : *operand - 'a' + 10);

    }
  *first_nondigit = (char *) operand;
  return overflow_flag;
}

/* This function skips all white spaces at the begin of source string
   and transforms tail of the source string (BASE ascii representation
   without sign) to given size unsigned integer with the aid of
   function `string_to_unsigned_integer_without_overflow_reaction'.
   BASE should be between 2 and 36 including them.  Digits more 9 are
   represented by 'a' (or 'A'), 'b' (or 'B') etc.  If the string
   started with invalid unsigned integer representation the result
   will be zero.  The function fixes overflow when result can not be
   represented by number of given size.  The function returns address
   of the first nondigit in the source string. */

char *
unsigned_integer::from_based_string (int size, const char *operand, int base,
				     void *result)
{
  char *first_nondigit;

  assert (size > 0);
  while (isspace (*operand))
    operand++;
  overflow_bit
    = string_to_unsigned_integer_without_overflow_reaction
      (size, operand, result, &first_nondigit, base);
  if (overflow_bit)
    (*overflow_reaction) ();
  return first_nondigit;
}

/* This function skips all white spaces at the begin of source string
   and transforms tail of the source string (decimal ascii
   representation without sign) to given size unsigned integer with
   the aid of function
   `string_to_unsigned_integer_without_overflow_reaction'.  If the
   string started with invalid unsigned integer representation the
   result will be zero.  The function fixes overflow when result can
   not be represented by number of given size.  The function returns
   address of the first nondigit in the source string. */

char *
unsigned_integer::from_string (int size, const char *operand, void *result)
{
  return from_based_string (size, operand, 10, result);
}

/* This function skips all white spaces at the begin of source string
   and transforms tail of the source string (BASE ascii representation
   with possible sign `+' or `-') to given size integer with the aid
   of function `string_to_unsigned_integer_without_overflow_reaction'.
   BASE should be between 2 and 36 including them.  Digits more 9 are
   represented by 'a' (or 'A'), 'b' (or 'B') etc.  If the string
   started with invalid integer representation the result will be
   zero.  The function fixes overflow when result can not be
   represented by number of given size.  the function returns Address
   of the first nondigit in the source string. */

char *
signed_integer::from_based_string (int size, const char *operand, int base,
				   void *result)
{
  int negative_number_flag;
  char *first_nondigit;
  int unsigned_result_sign;

  assert (size > 0);
  while (isspace (*operand))
    operand++;
  negative_number_flag = 0; /* FALSE */
  if (*operand == '+')
    operand++;
  else if (*operand == '-')
    {
      operand++;
      negative_number_flag = 1; /* TRUE */
    }
  overflow_bit
    = string_to_unsigned_integer_without_overflow_reaction
      (size, operand, result, &first_nondigit, base);
  unsigned_result_sign = INTEGER_SIGN (result);
  if (negative_number_flag)
    /* May be integer overflow when `result' is correct.  But result
       is correct because it is unsigned. */
    make_complementary_code (size, result, result);
  overflow_bit
    = overflow_bit || (unsigned_result_sign
                       && (!negative_number_flag
                           || INTEGER_SIGN (result) != unsigned_result_sign));
  if (overflow_bit)
    (*overflow_reaction) ();
  return first_nondigit;
}

/* This function skips all white spaces at the begin of source string
   and transforms tail of the source string (decimal ascii
   representation with possible sign `+' or `-') to given size integer
   with the aid of function
   `string_to_unsigned_integer_without_overflow_reaction'.  If the
   string started with invalid integer representation the result will
   be zero.  The function fixes overflow when result can not be
   represented by number of given size.  the function returns Address
   of the first nondigit in the source string. */

char *
signed_integer::from_string (int size, const char *operand, void *result)
{
  return from_based_string (size, operand, 10, result);
}
