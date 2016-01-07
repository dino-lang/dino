/* FILE NAME:   IEEEcpp.C

   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This is part of package IEEE; you can redistribute it and/or modify
   it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   TITLE:       Package for work with IEEE floating point numbers

   DESCRIPTION: This package implements IEEE floating point arithmetic
       by machine independent way with the aid of package
       `arithmetic'.  This abstract data is necessary because a host
       machine may not support such arithmetic for target machine.
       For example, VAX does not support IEEE floating point
       arithmetic.  The floating point numbers are represented by
       bytes in big endian mode.  The implementation of the package
       functions are not sufficiently efficient in order to use for
       run-time.  The package classes are oriented to implement
       constant-folding in compilers
         Functions of addition, subtraction, multiplication, division,
       conversion floating point numbers of different formats can fix
       input exceptions.  If an operand of such operation is trapping
       (signal) not a number then invalid operation and reserved
       operand exceptions are fixed and the result is (quiet) NaN,
       otherwise if an operand is (quiet) NaN then only reserved
       operand exception is fixed and the result is (quiet) NaN.
       Operation specific processing the rest of special case values
       of operands is placed with description of the operation.  Also
       in general case functions of addition, subtraction,
       multiplication, division, conversion floating point numbers of
       different formats, conversion of integer to floating point
       numbers and vice versa, and transformation of strings to
       floating point numbers can fix output exceptions and produces
       results for exception according to the following table.  The
       result and status for a given exceptional operation are
       determined by the highest priority exception.  If, for example,
       an operation produces both overflow and imprecise result
       exceptions, the overflow exception, having higher priority,
       determines the behavior of the operation.  The behavior of this
       operation is therefore described by the Overflow entry of the
       table.

         Exception|Condition|                     |Result |Status
       -----------|---------|---------------------|-------|-------------
                  |masked   |         IEEE_RN(_RP)| +Inf  |IEEE_OFL and
                  |overflow | sign +  IEEE_RZ(_RM)| +Max  |IEEE_IMP    
                  |exception|---------------------|-------|-------------
         Overflow |         | sign -  IEEE_RN(_RM)| -Inf  |IEEE_OFL and
                  |         |         IEEE_RZ(_RP)| -Max  |IEEE_IMP    
                  |---------|---------------------|-------|-------------
                  |unmasked | Precise result      |See    |IEEE_OFL
                  |overflow |---------------------|above  |-------------
                  |exception| Imprecise result    |       |IEEE_OFL and
                  |         |                     |       |IEEE_IMP    
       -----------|---------|---------------------|-------|-------------
                  |masked   |                     |Rounded|IEEE_UFL and
                  |underflow| Imprecise result    |result |IEEE_IMP    
        Underflow |exception|                     |       |     
                  |---------|---------------------|-------|-------------
                  |unmasked | Precise result      |result |IEEE_UFL
                  |underflow|---------------------|-------|-------------
                  |exception| Imprecise result    |Rounded|IEEE_UFL and
                  |         |                     |result |IEEE_IMP    
       -----------|-------------------------------|-------|-------------
                  |masked imprecise exception     |Rounded|IEEE_IMP
        Imprecise |                               |result |             
                  |-------------------------------|-------|-------------
                  |unmasked imprecise exception   |Rounded|IEEE_IMP
                  |                               |result |             

   SPECIAL CONSIDERATION:
         Defining macro `NDEBUG' (e.g. by option `-D' in C compiler
       command line) during the file compilation disables to fix
       some internal errors and errors of usage of the package.
         This file includes file `IEEEtens.c' see array
       `powers_of_ten'.  File `IEEEtens.c' contains the most precise
       floating point (more precise than IEEE double floating point
       format) values of non negative powers of ten.  The file is
       generated with the aid of POSIX arbitrary precision calculator
       `bc'.  The powers of ten is used for transformation of string
       (decimal representation) to floating point numbers and vice
       versa.  Calculation of the powers of ten by conventional way
       (by multiplication of IEEE floating point numbers results in
       rounding imprecises).

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#else /* In this case we are oriented to ANSI C */
#ifndef HAVE_MEMSET
#define HAVE_MEMSET
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "bits.h"
#include "arithm.h"
#include "IEEE.h"
#include <assert.h>
#include <limits.h>



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



/* This page contains structures and macros describing format of IEEE
   floating point numbers. */

/* The following structure describes characteristics of IEEE floats of
   different types. */
struct float_desc
{
  /* The following member value is size all floating point number. */
  int size;

  /* The following member value is number (0, 1, ...) of sign bit from
     floating point number start. */
  int sign_bit;

  /* The following member value is number (0, 1, ...) of first
     exponent bit from floating point number start. */
  int start_exponent_bit;

  /* The following member value is exponent length (in bits) inside
     floating point number. */
  int exponent_bit_length;

  /* The following member value is number (0, 1, ...) of first
     fraction bit from floating point number start. */
  int start_fraction_bit;

  /* The following member value is fraction length (in bits) inside
     floating point number. */
  int fraction_bit_length;

  /* The following member value is bias of exponent of floating point
     number. */
  int exponent_bias;

  /* The following member value is maximal value
     (2**exponent_bit_length - 1) of biased exponent of floating point
     number. */
  int max_biased_exponent;

  /* The following member value is floating point number fraction
     length taking into account implicit fraction bit (1) for
     normalized numbers. */
  int true_fraction_bit_length;

};

/* The following macro value is maximal floating point number
   fraction length taking into account implicit fraction bit (1) for
   normalized numbers.  Actually now this is true fraction bit length
   for quad. */

#define MAX_TRUE_FRACTION_BIT_LENGTH 113

/* The following macro value is maximal bias of exponent of floating
   point numbers.  Actually now this is exponent bias for double. */

#define MAX_EXPONENT_BIAS 037777

/* The following structures describes characteristics of IEEE single
   and double precision floating point numbers. */

static struct float_desc single_float_desc
  = {IEEE_FLOAT_SIZE, 0, 1,  8,  9, 23, 0177, 0377, 24};
static struct float_desc double_float_desc
  = {IEEE_DOUBLE_SIZE, 0, 1, 11, 12, 52, 01777, 03777, 53};
#ifdef IEEE_QUAD
static struct float_desc quad_float_desc
  = {IEEE_QUAD_SIZE, 0, 1, 15, 16, 112, MAX_EXPONENT_BIAS, 077777,
     MAX_TRUE_FRACTION_BIT_LENGTH};
#endif

/* The following is major type used for description an IEEE floating
   point numbers. */

typedef struct float_desc *float_desc_t;

/* Minimal bits needed for representation of a hundred. */

#define MINIMAL_BITS_FOR_HUNDRED 7

/* The following macro value is size in bytes of internal
   representation of fractions of single and double precision floating
   point numbers.  This value must contain at least two true double
   fraction length and additionally MINIMAL_BITS_FOR_HUNDRED bits.  Such
   size is needed for transformation of double to string (the most
   critical requirement).  See also commentary for `fraction_t'. */

#define INTERNAL_FRACTION_SIZE\
   ((2 * MAX_TRUE_FRACTION_BIT_LENGTH + MINIMAL_BITS_FOR_HUNDRED\
     + CHAR_BIT - 1) / CHAR_BIT)

/* The following macro value is exponent value which guarantees that
   forming floating point value from any nonzero fraction with such
   exponent results in underflow or is non biased exponent of precise
   zero. */

#define NON_BIASED_EXPONENT_FOR_UNDERFLOW\
   (-MAX_EXPONENT_BIAS - INTERNAL_FRACTION_SIZE * CHAR_BIT)

/* The following macro value is exponent value which guarantees that
   forming floating point value from any nonzero fraction with such
   exponent results in underflow. */

#define NON_BIASED_EXPONENT_FOR_OVERFLOW\
   (MAX_EXPONENT_BIAS + INTERNAL_FRACTION_SIZE * CHAR_BIT)


/* The following macros are used in functions transforming string to
   floating point and vice versa. */

/* The following macro value is string representation of trapping
   (signal) not a number value. */

#define TRAPPING_NaN_NAME       "SNaN"

/* The following macro value is string representation of (quiet) not a
   number value. */

#define NaN_NAME                "QNaN"

/* The following macro value is string representation of positive
   infinity. */

#define POSITIVE_INFINITY_NAME  "+Inf"

/* The following macro value is string representation of negative
   infinity. */

#define NEGATIVE_INFINITY_NAME  "-Inf"



/* This page contains forward declarations of some functions. */

static void positive_zero (void *fp, float_desc_t d);
static void negative_zero (void *fp, float_desc_t d);
static void NaN (void *fp, float_desc_t d);
static void trapping_NaN (void *fp, float_desc_t d);
static void positive_infinity (void *fp, float_desc_t d);
static void negative_infinity (void *fp, float_desc_t d);
static void positive_maximum (void *fp, float_desc_t d);
static void negative_maximum (void *fp, float_desc_t d);
static void positive_minimum (void *fp, float_desc_t d);
static void negative_minimum (void *fp, float_desc_t d);



/* This page contains definition of array containing floating point
   representations of positive powers of ten. */

struct power_of_ten
{       

  /* Non biased exponent which represents given power of ten with the
     fraction formed from member `fraction'.  Range of exponents is
     greater than max of IEEE double number exponent +
     MAX_TRUE_FRACTION_BIT_LENGTH + MINIMAL_BITS_FOR_HUNDRED. */
  short int non_biased_exponent;
  /* The following member value is TRUE if the fraction inexactly represents
     given power of ten. */
  short int ten_power_fraction_imprecise_flag;
  /* Normalized fraction of the numbers is slightly more precise than
     fraction of IEEE double point numbers.  Most significant
     fraction bit corresponds to first binary digit before floating
     point.  Other fraction bits represents binary digit after
     floating point.  See also commentaries for `fraction_t'. */
  unsigned char fraction [MAX_TRUE_FRACTION_BIT_LENGTH / CHAR_BIT + 1];
};

/* The following array represents non negative powers of ten.  The
   array element with zero index represents zero power of ten, the
   element with index 1 represents ten and so on.  File `IEEEtens.c'
   contains the most precise floating point (more precise than IEEE
   double floating point format) values of non negative powers of ten.
   The file is generated with the aid of POSIX arbitrary precision
   calculator `bc'.  The powers of ten is used for transformation of
   string (decimal representation) to floating point numbers and vice
   versa.  Calculation of the powers of ten by conventional way (by
   multiplication of IEEE floating point numbers can result in
   rounding imprecises). */

static struct power_of_ten powers_of_ten [] =
{
  /* Generated elements */
#include "IEEEtens.c"
  /* The following elements is placed here for convenience. */
  {-1, 0, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}}
};



/* This page contains definitions of variables, macros, types and functions
   common for all package functions. */

/* Fractions of floating point numbers of any format (double or
   single) are represented by the following type.  Most significant
   fraction bit corresponds to first binary digit before floating
   point.  Other fraction bits represents binary digit after floating
   point.  Remember that special cases IEEE values (infinitys, NaNs
   and sometimes) are not transformed to this representation. */

typedef unsigned char fraction_t [INTERNAL_FRACTION_SIZE];

/* This function returns biased exponent of floating point number FP
   with characteristics D. */

static inline int biased_exponent (void *fp, float_desc_t d)
{
  unsigned char b0 = 0, b1 = 0;

  if (d->exponent_bit_length <= CHAR_BIT)
    {
      bits::bit_string_copy (&b0, CHAR_BIT - d->exponent_bit_length,
			     fp, d->start_exponent_bit,
			     d->exponent_bit_length);
      return (int) b0;
    }
  else
    {
      bits::bit_string_copy (&b1, 2 * CHAR_BIT - d->exponent_bit_length,
			     fp, d->start_exponent_bit,
			     d->exponent_bit_length - CHAR_BIT);
      bits::bit_string_copy (&b0, 0, fp, (d->start_exponent_bit
					  + d->exponent_bit_length - CHAR_BIT),
			     CHAR_BIT);
      return b0 | ((int) b1 << CHAR_BIT);
    }
}

/* This function sets up only EXPONENT of floating point number FP
   with characteristics D. */

static inline void set_biased_exponent (void *fp, float_desc_t d, int exponent)
{
  unsigned char b0, b1;

  b0 = exponent & 0377;
  b1 = exponent >> CHAR_BIT;
  if (d->exponent_bit_length <= CHAR_BIT)
    bits::bit_string_copy (fp, d->start_exponent_bit, &b0,
			   CHAR_BIT - d->exponent_bit_length,
			   d->exponent_bit_length);
  else
    {
      bits::bit_string_copy (fp, (d->start_exponent_bit
				  + d->exponent_bit_length - CHAR_BIT),
			     &b0, 0, CHAR_BIT);
      bits::bit_string_copy (fp, d->start_exponent_bit,
			     &b1, 2 * CHAR_BIT - d->exponent_bit_length,
			     d->exponent_bit_length - CHAR_BIT);
    }
}

/* The following variable value is current trap mask.  If the mask bit
   corresponding given exception is set, a floating point exception
   trap does not occur for given exception.  Such exception is said to
   be masked exception.  Initial exception trap mask is zero.
   Remember that more one exception may be occurred simultaneously. */

static int current_trap_mask = 0;

/* The following variable value is current sticky status bits.  Only
   sticky status bits corresponding to masked exceptions are updated
   regardless whether a floating point exception trap is taken or not.
   Initial values of sticky status bits are zero. */

static int current_sticky_status_bits = 0;

/* The following variable value is current status bits.  Status bits
   are updated regardless of the current exception trap mask only when
   a floating point exception trap is taken.  Initial values of status
   bits are zero. */

static int current_status_bits = 0;

/* The following variable value is current round mode.  See also
   commentaries for macros defining rounding modes in file
   `IEEE.h'. */

static int current_round_mode = IEEE_RN;

/* The following variable value is function which reacts on any
   floating point exception.  Originally reaction is function
   `default_floating_point_exception_trap'. */

static void (*current_trap) (void)
  = IEEE::default_floating_point_exception_trap;

/* The following function processes exceptions given as input mask.
   Depending on current trap mask the function sets up status and
   sticky status bits (see commentaries for
   `current_sticky_status_bits' and `current_status_bits') and calls
   reaction on floating point exceptions trap. */

static void
process_exception (int exception_mask)
{
  /* Set sticky bits for masked exception. */
  current_sticky_status_bits |= exception_mask & current_trap_mask;
  if ((exception_mask & ~current_trap_mask) != 0)
    {
      /* Set status bits. */
      current_status_bits = exception_mask;
      (*current_trap) ();
    }
}

/* The following enumeration represents non intersected classes of
   IEEE floating point numbers. */

enum number_class
{
  POSITIVE_ZERO,
  NEGATIVE_ZERO,
  NOT_A_NUMBER,
  TRAPPING_NOT_A_NUMBER,
  POSITIVE_INFINITY,
  NEGATIVE_INFINITY,
  NORMALIZED_NUMBER,
  DENORMALIZED_NUMBER
};

/* The following function determines and returns class to which given
   floating point number FP (with characteristics D) belongs. */

static enum number_class
float_class (void *fp, float_desc_t d)
{
  int exponent = biased_exponent (fp, d);

  if (exponent == d->max_biased_exponent)
    {
      if (bits::is_zero_bit_string (fp, d->start_fraction_bit,
				    d->fraction_bit_length))
        return (bits::bit (fp, d->sign_bit)
		? NEGATIVE_INFINITY : POSITIVE_INFINITY);
      else
        return (bits::bit (fp, d->start_fraction_bit)
		? NOT_A_NUMBER : TRAPPING_NOT_A_NUMBER);
    }
  else if (exponent == 0)
    {
      if (bits::is_zero_bit_string (fp, d->start_fraction_bit,
				    d->fraction_bit_length))
        return (bits::bit (fp, d->sign_bit) ? NEGATIVE_ZERO : POSITIVE_ZERO);
      else
        return DENORMALIZED_NUMBER;
    }
  else
    return NORMALIZED_NUMBER;
}

/* The following function forms IEEE floating point number *FP with
   characteristics D from exponent and fraction.  Bits in given
   fraction after first MAX_TRUE_FRACTION_BIT_LENGTH bits are ignored.
   Value of exponent must be correct.  The most significant bit of
   given fraction is absent in the normalized floating point number.
   This is so called implicit bit of fraction.  Its value must be
   nonzero. */

static void
pack_float (void *fp, float_desc_t d,
	    int sign, int exponent, const fraction_t fraction)
{
  assert (exponent <= d->max_biased_exponent && exponent >= 0
          && (exponent == 0 || bits::bit (fraction, 0) != 0));
  bits::set_bit (fp, d->sign_bit, sign);
  set_biased_exponent (fp, d, exponent);
  if (exponent != 0)
    /* Normalized number */
    bits::bit_string_copy (fp, d->start_fraction_bit, fraction, 1,
			   d->fraction_bit_length);
  else
    /* Denormalized number or zero */
    bits::bit_string_copy (fp, d->start_fraction_bit, fraction, 0,
			   d->fraction_bit_length);
}

/* The following function separates sign, exponent and fraction from
   given IEEE floating point number *FP with characteristics D.  Bits
   in the formed fraction after first MAX_TRUE_FRACTION_BIT_LENGTH
   bits are set up to zero number. */

static void
unpack_float (void *fp, float_desc_t d,
	      int *sign, int *exponent, fraction_t fraction)
{
  *sign = bits::bit (fp, d->sign_bit);
  *exponent = biased_exponent (fp, d);
  memset (fraction, 0, INTERNAL_FRACTION_SIZE);
  if (*exponent != 0)
    {
      bits::bit_string_copy (fraction, 1, fp, d->start_fraction_bit,
			     d->fraction_bit_length);
      bits::set_bit (fraction, 0, 1); /* implicit bit */
    }
  else
    /* Denormalized number or zero */
    bits::bit_string_copy (fraction, 0, fp, d->start_fraction_bit,
			   d->fraction_bit_length);
}

/* The following function fulfills normalization, rounding, and
   processing occurred output exceptions, forms corresponding IEEE
   floating point number *FP with characteristics D, and sets up new
   reaction on unsigned integer overflow reaction.  All bits in given
   fraction are processed to fix imprecise exception.  See
   commentaries for output exceptions in file header. */

static void
form_float (void *fp, float_desc_t d,
	    int sign, int exponent, fraction_t fraction, int carry_flag,
            int imprecise_flag, void (*new_unsigned_overflow_reaction) (void))
{
  int shift;
  int exception_mask;
  int round_bit;
  int round_to_nearest_special_case;
  fraction_t one;
  
  /* Normalization */
  if (carry_flag)
    {
      exponent++;
      unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction, 1,
				     fraction);
      bits::set_bit (fraction, 0, 1);
      imprecise_flag = imprecise_flag || integer::overflow_bit;
    }
  if (bits::is_zero_bit_string (fraction, 0,
				INTERNAL_FRACTION_SIZE * CHAR_BIT))
    exponent = 0;
  else if (exponent >= 0)
    {
      for (shift = 0;
	   exponent > 0 && bits::bit (fraction, shift) == 0;
	   shift++)
        exponent--;
      unsigned_integer::shift_left (INTERNAL_FRACTION_SIZE, fraction, shift,
                                    fraction);
    }
  else
    {
      unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction,
                                     -exponent, fraction);
      imprecise_flag = imprecise_flag || integer::overflow_bit;
      exponent = 0;
    }
  /* Rounding */
  if (exponent != 0)
    /* Normalized number */
    unsigned_integer::shift_right
      (INTERNAL_FRACTION_SIZE, fraction,
       INTERNAL_FRACTION_SIZE * CHAR_BIT - d->true_fraction_bit_length - 1,
       fraction);
  else
    /* Denormalized number has less significant bits. */
    unsigned_integer::shift_right
      (INTERNAL_FRACTION_SIZE, fraction,
       INTERNAL_FRACTION_SIZE * CHAR_BIT - d->true_fraction_bit_length,
       fraction);
  imprecise_flag = imprecise_flag || integer::overflow_bit;
  round_bit = bits::bit (fraction, INTERNAL_FRACTION_SIZE * CHAR_BIT - 1);
  round_to_nearest_special_case = !imprecise_flag;
  unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction, 1,
                                 fraction);
  imprecise_flag = imprecise_flag || integer::overflow_bit;
  (void) unsigned_integer::from_string (INTERNAL_FRACTION_SIZE, "1", one);
  if (current_round_mode == IEEE_RN)
    {
      if (round_bit
          && (!round_to_nearest_special_case
              || bits::bit (fraction, INTERNAL_FRACTION_SIZE * CHAR_BIT - 1)))
        unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction, one,
                               fraction);
    }
  else if (current_round_mode == IEEE_RM)
    {
      if (imprecise_flag && sign)
        unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction, one,
                               fraction);
    }
  else if (current_round_mode == IEEE_RP)
    {
      if (imprecise_flag && !sign)
        unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction, one,
                               fraction);
    }
  else
    assert (current_round_mode == IEEE_RZ);
  if (exponent != 0)
    /* Normalized number */
    unsigned_integer::shift_left
      (INTERNAL_FRACTION_SIZE, fraction,
       INTERNAL_FRACTION_SIZE * CHAR_BIT - d->true_fraction_bit_length,
       fraction);
  else
    /* Denormalized number or zero */
    unsigned_integer::shift_left
      (INTERNAL_FRACTION_SIZE, fraction,
       INTERNAL_FRACTION_SIZE * CHAR_BIT - d->true_fraction_bit_length + 1,
       fraction);
  assert (exponent >= 0);
  if (integer::overflow_bit)
    {
      exponent++;
      unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction, 1,
                                     fraction);
      bits::set_bit (fraction, 0, 1);
      imprecise_flag = imprecise_flag || integer::overflow_bit;
    }
  /* Set up output exceptions and results for them. */
  if (exponent >= d->max_biased_exponent)
    {
      exception_mask = IEEE_OFL;
      if (imprecise_flag || current_trap_mask & IEEE_OFL)
        exception_mask |= IEEE_IMP;
      if (sign)
        {
          if (current_round_mode == IEEE_RM || current_round_mode == IEEE_RN)
            negative_infinity (fp, d);
          else
            negative_maximum (fp, d);
        }
      else if (current_round_mode == IEEE_RP || current_round_mode == IEEE_RN)
	positive_infinity (fp, d);
      else
        positive_maximum (fp, d);
    }
  else if (exponent == 0)
    {
      /* Precise zero are processed here. */
      exception_mask = (imprecise_flag ? IEEE_IMP : 0);
      if (imprecise_flag
          || ((current_trap_mask & IEEE_UFL) == 0
              && !bits::is_zero_bit_string (fraction, 0,
                                            INTERNAL_FRACTION_SIZE
                                            * CHAR_BIT)))
        exception_mask |= IEEE_UFL;
      pack_float (fp, d, sign, exponent, fraction);
    }
  else
    {
      exception_mask = (imprecise_flag ? IEEE_IMP : 0);
      pack_float (fp, d, sign, exponent, fraction);
    }
  (void) unsigned_integer::set_overflow_reaction
    (new_unsigned_overflow_reaction);
  process_exception (exception_mask);
}

/* The following function normalize nonzero fraction.  The fraction
   must be non zero.  Exponent can be biased or non biased.  The
   exponent of normalized fraction will also be the same (biased or
   nonbiased). */

static void
normalize_fraction (int *exponent, fraction_t fraction)
{
  int shift;

  assert (!bits::is_zero_bit_string (fraction, 0,
                                     INTERNAL_FRACTION_SIZE * CHAR_BIT));
  for (shift = 0; bits::bit (fraction, shift) == 0; shift++)
    ;
  unsigned_integer::shift_left (INTERNAL_FRACTION_SIZE, fraction, shift,
                                fraction);
  *exponent -= shift;
}

/* The following macro value is log10 (2.0). */

#define LOG_OF_2  0.301029995663981198

/* The following function returns the most precise floating point
   (more precise than IEEE double floating point format) values for
   given non negative power of ten.  The function returns 1 if such
   power exists and was returned, 0 if the power is negative or very
   large (in this case the floating point value is not returned
   through the parameters). */

static int
get_ten_power (int ten_power, fraction_t fraction, int *non_biased_exponent,
               int *ten_power_fraction_imprecise_flag)
{
  if (ten_power < 0
      || ((unsigned) ten_power
          >= sizeof (powers_of_ten)/sizeof (struct power_of_ten) - 1))
    return 0 /* FALSE */;
  *non_biased_exponent = powers_of_ten [ten_power].non_biased_exponent;
  *ten_power_fraction_imprecise_flag
    = powers_of_ten [ten_power].ten_power_fraction_imprecise_flag;
  memcpy (fraction, powers_of_ten [ten_power].fraction,
          (size_t) sizeof (powers_of_ten [ten_power].fraction));
  memset (fraction + (size_t) sizeof (powers_of_ten [ten_power].fraction), 0,
          INTERNAL_FRACTION_SIZE
          - (size_t) sizeof (powers_of_ten [ten_power].fraction));
  return 1 /* TRUE */;
}



/* This page contains functions working with exception bits and
   initiation function. */

/* The following function resets the initial state of the package:
   floating point exception trap mask, sticky status bits, status
   bits, reaction on floating point exception traps, and mode of
   rounding. */

void
IEEE::reset (void)
{
  assert (sizeof (IEEE_float_t) == IEEE_FLOAT_SIZE
          && sizeof (IEEE_double_t) == IEEE_DOUBLE_SIZE);
  current_trap_mask = 0;
  current_sticky_status_bits = 0;
  current_status_bits = 0;
  current_round_mode = IEEE_RN;
  current_trap = default_floating_point_exception_trap;
}

/* The following function sets up new exception trap mask and returns
   the previous.  If the mask bit corresponding given exception is
   set, a floating point exception trap does not occur for given
   exception.  Such exception is said to be masked exception.  Initial
   exception trap mask is zero.  Remember that more one exception may
   be occurred simultaneously. */

int
IEEE::set_trap_mask (int mask)
{
  int old_trap_mask;

  old_trap_mask = current_trap_mask;
  current_trap_mask = mask;
  return old_trap_mask;
}

/* The following function returns current floating point exception
   trap mask.  If the mask bit corresponding given exception is set, a
   floating point exception trap does not occur for given exception.
   Such exception is said to be masked exception.  Initial exception
   trap mask is zero.  Remember that more one exception may be
   occurred simultaneously. */

int
IEEE::get_trap_mask (void)
{
  return current_trap_mask;
}

/* The following function sets up new values of sticky status bits
   given as mask and returns the previous sticky status bits as mask.
   Only sticky status bits corresponding to masked exceptions are
   updated regardless whether a floating point exception trap is taken
   or not.  Initial values of sticky status bits are zero. */

int
IEEE::set_sticky_status_bits (int mask)
{
  int old_sticky_status_bits;

  old_sticky_status_bits = current_sticky_status_bits;
  current_sticky_status_bits = mask;
  return old_sticky_status_bits;
}

/* The following function returns current sticky status bits as mask.
   Only sticky status bits corresponding to masked exceptions are
   updated regardless whether a floating point exception trap is taken
   or not.  Initial values of sticky status bits are zero. */

int
IEEE::get_sticky_status_bits (void)
{
  return current_sticky_status_bits;
}

/* The following function returns current status bits as mask.  It is
   supposed that the function will be used in trap on an floating
   point exception.  Status bits are updated regardless of the current
   exception trap mask only when a floating point exception trap is
   taken.  Initial values of status bits are zero.  User can not set
   up IEEE status bits therefore function `IEEE::set_status_bits' is
   absent. */

int
IEEE::get_status_bits (void)
{
  return current_status_bits;
}



/* This page contains functions working with rounding regimes. */

/* The following function sets up current rounding mode and returns
   previous mode.  See also commentaries for macros defining mode of
   rounding in file `IEEE.h'. */

int
IEEE::set_round (int round_mode)
{
  int old_round_mode;

  assert (round_mode == IEEE_RN || round_mode == IEEE_RM
          || round_mode == IEEE_RP || round_mode == IEEE_RZ);
  old_round_mode = current_round_mode;
  current_round_mode = round_mode;
  return old_round_mode;
}

/* The following function returns current mode of rounding.  See also
   commentaries for macros defining mode of rounding in file
   `IEEE.h'. */

int
IEEE::get_round (void)
{
  return current_round_mode;
}



/* This page contains functions working with exception reactions (traps). */

/* Originally reaction on any floating point exception trap is equal
   to this function.  The function does nothing. */

void
IEEE::default_floating_point_exception_trap (void)
{
}

/* The following function sets up current reaction on any floating
   point exception trap (unmasked exception) and returns previous
   reaction.  All occurred exceptions can be found in the trap with
   the aid of status bits. */

void
(*IEEE::set_floating_point_exception_trap (void (*function) (void))) (void)
{
  void (*old_trap) (void);

  old_trap = current_trap;
  current_trap = function;
  return old_trap;
}



/* This page contains functions creating various floating point constant. */

/* The following function returns (through FP) IEEE positive zero
   constant with characteristics D. */

static void
positive_zero (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 0);
  set_biased_exponent (fp, d, 0);
  bits::bit_string_set (fp, d->start_fraction_bit, 0, d->fraction_bit_length);
}

/* The following function returns (through FP) IEEE negative zero
   constant with characteristics D. */

static void
negative_zero (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 1);
  set_biased_exponent (fp, d, 0);
  bits::bit_string_set (fp, d->start_fraction_bit, 0, d->fraction_bit_length);
}

/* The following function returns (through FP) IEEE (quiet) not a
   number constant with characteristics D.  A (quiet) NaN does not
   cause an Invalid Operation exception and can be reported as an
   operation result.  According to the IEEE standard NaN (and trapping
   NaN) can be represented by more one bit string.  But all function
   of the package generates and used only one its representation
   created by function `NaN' (and `trapping_NaN'). */

static void
NaN (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 0);
  set_biased_exponent (fp, d, d->max_biased_exponent);
  bits::bit_string_set (fp, d->start_fraction_bit, 0, d->fraction_bit_length);
  bits::set_bit (fp, d->start_fraction_bit, 1);
}

/* The following function returns (through FP) IEEE trapping not a
   number constant with characteristics D.  A trapping NaN causes an
   Invalid Operation exception if used as in input operand to floating
   point operation.  Trapping NaN can not be reported as an operation
   result.  According to the IEEE standard NaN (and trapping NaN) can
   be represented by more one bit string.  But all function of the
   package generates and used only one its representation created by
   function `NaN' (and `trapping_NaN'). */

static void
trapping_NaN (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 0);
  set_biased_exponent (fp, d, d->max_biased_exponent);
  bits::bit_string_set (fp, d->start_fraction_bit, 0, d->fraction_bit_length);
  bits::set_bit (fp, d->start_fraction_bit + 1, 1);
}

/* The following function returns (through FP) IEEE positive infinity
   with characteristics D. */

static void
positive_infinity (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 0);
  set_biased_exponent (fp, d, d->max_biased_exponent);
  bits::bit_string_set (fp, d->start_fraction_bit, 0, d->fraction_bit_length);
}

/* The following function returns (through FP) IEEE negative infinity
   with characteristics D. */

static void
negative_infinity (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 1);
  set_biased_exponent (fp, d, d->max_biased_exponent);
  bits::bit_string_set (fp, d->start_fraction_bit, 0, d->fraction_bit_length);
}

/* The following function returns (through FP) IEEE positive maximal
   (normalized) number with characteristics D. */

static void
positive_maximum (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 0);
  set_biased_exponent (fp, d, d->max_biased_exponent - 1);
  bits::bit_string_set (fp, d->start_fraction_bit, 1, d->fraction_bit_length);
}

/* The following function returns (through FP) IEEE negative maximal
   (normalized) number with characteristics D. */

static void
negative_maximum (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 1);
  set_biased_exponent (fp, d, d->max_biased_exponent - 1);
  bits::bit_string_set (fp, d->start_fraction_bit, 1, d->fraction_bit_length);
}

/* The following function returns (through FP) IEEE positive minimal
   (denormalized) number with characteristics D. */

static void
positive_minimum (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 0);
  set_biased_exponent (fp, d, 0);
  bits::bit_string_set (fp, d->start_fraction_bit, 0,
                  d->fraction_bit_length);
  bits::set_bit (fp, d->start_fraction_bit + d->fraction_bit_length - 1, 1);
}

/* The following function returns (through FP)IEEE negative minimal
   (denormalized) number with characteristics D. */

static void
negative_minimum (void *fp, float_desc_t d)
{
  bits::set_bit (fp, d->sign_bit, 1);
  set_biased_exponent (fp, d, 0);
  bits::bit_string_set (fp, d->start_fraction_bit, 0,
                  d->fraction_bit_length);
  bits::set_bit (fp, d->start_fraction_bit + d->fraction_bit_length - 1, 1);
}

/* The following function returns (through FP) IEEE positive zero
   constant with characteristics D. */

void
IEEE_float::positive_zero (void)
{
  ::positive_zero (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision
   negative zero constant. */

void
IEEE_float::negative_zero (void)
{
  current_status_bits = 0;
  ::negative_zero (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision (quiet)
   not a number constant.  A (quiet) NaN does not cause an Invalid
   Operation exception and can be reported as an operation result.
   According to the IEEE standard NaN (and trapping NaN) can be
   represented by more one bit string.  But all function of the
   package generates and used only one its representation set up by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE::double_NaN', `IEEE::double_trapping_NaN',
   `IEEE::quad_NaN', `IEEE::quad_trapping_NaN'). */

void
IEEE_float::NaN (void)
{
  current_status_bits = 0;
  ::NaN (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision
   trapping not a number constant.  A trapping NaN causes an Invalid
   Operation exception if used as in input operand to floating point
   operation.  Trapping NaN can not be reported as an operation
   result.  According to the IEEE standard NaN (and trapping NaN) can
   be represented by more one bit string.  But all function of the
   package generates and used only one its representation set up by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE::double_NaN', `IEEE::double_trapping_NaN',
   `IEEE::quad_NaN', `IEEE::quad_trapping_NaN'). */

void
IEEE_float::trapping_NaN (void)
{
  current_status_bits = 0;
  ::trapping_NaN (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision positive
   infinity. */

void
IEEE_float::positive_infinity (void)
{
  current_status_bits = 0;
  ::positive_infinity (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision
   negative infinity. */

void
IEEE_float::negative_infinity (void)
{
  current_status_bits = 0;
  ::negative_infinity (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision
   positive maximal (normalized) number. */

void
IEEE_float::positive_maximum (void)
{
  current_status_bits = 0;
  ::positive_maximum (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision
   negative maximal (normalized) number. */

void
IEEE_float::negative_maximum (void)
{
  current_status_bits = 0;
  ::negative_maximum (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision
   positive minimal (denormalized) number. */

void
IEEE_float::positive_minimum (void)
{
  current_status_bits = 0;
  ::positive_minimum (this, &single_float_desc);
}

/* The following member function sets up IEEE single precision
   negative minimal (denormalized) number. */

void
IEEE_float::negative_minimum (void)
{
  current_status_bits = 0;
  ::negative_minimum (this, &single_float_desc);
}

/* The following member function sets up IEEE double precision
   positive zero constant. */

void
IEEE_double::positive_zero (void)
{
  current_status_bits = 0;
  ::positive_zero (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision
   negative zero constant. */

void
IEEE_double::negative_zero (void)
{
  current_status_bits = 0;
  ::negative_zero (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision (quiet)
   not a number constant.  A (quiet) NaN does not cause an Invalid
   Operation exception and can be reported as an operation result.
   According to the IEEE standard NaN (and trapping NaN) can be
   represented by more one bit string.  But all function of the
   package generates and used only one its representation set by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_double::NaN', `IEEE_double::trapping_NaN',
   `IEEE_quad::NaN', `IEEE_quad::trapping_NaN'). */

void
IEEE_double::NaN (void)
{
  current_status_bits = 0;
  ::NaN (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision
   trapping not a number constant.  A trapping NaN causes an Invalid
   Operation exception if used as in input operand to floating point
   operation.  Trapping NaN can not be reported as an operation
   result.  According to the IEEE standard NaN (and trapping NaN) can
   be represented by more one bit string.  But all function of the
   package generates and used only one its representation set by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_double::NaN', `IEEE_double::trapping_NaN',
   `IEEE_quad::NaN', `IEEE_quad::trapping_NaN'). */

void
IEEE_double::trapping_NaN (void)
{
  current_status_bits = 0;
  ::trapping_NaN (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision
   positive infinity. */

void
IEEE_double::positive_infinity (void)
{
  current_status_bits = 0;
  ::positive_infinity (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision
   negative infinity. */

void
IEEE_double::negative_infinity (void)
{
  current_status_bits = 0;
  ::negative_infinity (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision
   positive maximal (normalized) number. */

void
IEEE_double::positive_maximum (void)
{
  current_status_bits = 0;
  ::positive_maximum (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision
   negative maximal (normalized) number. */

void
IEEE_double::negative_maximum (void)
{
  current_status_bits = 0;
  ::negative_maximum (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision
   positive minimal (denormalized) number. */

void
IEEE_double::positive_minimum (void)
{
  current_status_bits = 0;
  ::positive_minimum (this, &double_float_desc);
}

/* The following member function sets up IEEE double precision
   negative minimal (denormalized) number. */

void
IEEE_double::negative_minimum (void)
{
  current_status_bits = 0;
  ::negative_minimum (this, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following member function sets up IEEE quad precision
   positive zero constant. */

void
IEEE_quad::positive_zero (void)
{
  current_status_bits = 0;
  ::positive_zero (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision
   negative zero constant. */

void
IEEE_quad::negative_zero (void)
{
  current_status_bits = 0;
  ::negative_zero (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision (quiet)
   not a number constant.  A (quiet) NaN does not cause an Invalid
   Operation exception and can be reported as an operation result.
   According to the IEEE standard NaN (and trapping NaN) can be
   represented by more one bit string.  But all function of the
   package generates and used only one its representation set by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_quad::NaN', `IEEE_quad::trapping_NaN'). */

void
IEEE_quad::NaN (void)
{
  current_status_bits = 0;
  ::NaN (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision
   trapping not a number constant.  A trapping NaN causes an Invalid
   Operation exception if used as in input operand to floating point
   operation.  Trapping NaN can not be reported as an operation
   result.  According to the IEEE standard NaN (and trapping NaN) can
   be represented by more one bit string.  But all function of the
   package generates and used only one its representation set by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_quad::NaN', `IEEE_quad::trapping_NaN'). */

void
IEEE_quad::trapping_NaN (void)
{
  current_status_bits = 0;
  ::trapping_NaN (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision
   positive infinity. */

void
IEEE_quad::positive_infinity (void)
{
  current_status_bits = 0;
  ::positive_infinity (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision
   negative infinity. */

void
IEEE_quad::negative_infinity (void)
{
  current_status_bits = 0;
  ::negative_infinity (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision
   positive maximal (normalized) number. */

void
IEEE_quad::positive_maximum (void)
{
  current_status_bits = 0;
  ::positive_maximum (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision
   negative maximal (normalized) number. */

void
IEEE_quad::negative_maximum (void)
{
  current_status_bits = 0;
  ::negative_maximum (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision
   positive minimal (denormalized) number. */

void
IEEE_quad::positive_minimum (void)
{
  current_status_bits = 0;
  ::positive_minimum (this, &quad_float_desc);
}

/* The following member function sets up IEEE quad precision
   negative minimal (denormalized) number. */

void
IEEE_quad::negative_minimum (void)
{
  current_status_bits = 0;
  ::negative_minimum (this, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions to determine class of IEEE floating point
   numbers. */

/* The following member function tests that the IEEE single precision
   number is positive zero.  The function returns 1, if given IEEE
   single precision number is positive zero, 0 otherwise. */

int
IEEE_float::is_positive_zero (void)
{
  current_status_bits = 0;
  return float_class (this, &single_float_desc) == POSITIVE_ZERO;
}

/* The following member function tests that the IEEE single precision
   number is negative zero.  The function returns 1, if given IEEE
   single precision number is negative zero, 0 otherwise. */

int
IEEE_float::is_negative_zero (void)
{
  current_status_bits = 0;
  return float_class (this, &single_float_desc) == NEGATIVE_ZERO;
}

/* The following member function tests that the IEEE single precision
   number is (quiet) not a number constant.  The function returns 1,
   if given IEEE single precision number is NaN, 0 otherwise.
   According to the IEEE standard NaN (and trapping NaN) can be
   represented by more one bit string.  But all functions of the
   package generate and use only one its representation created by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_double::NaN', `IEEE_double::trapping_NaN', `IEEE_quad::NaN',
   `IEEE_quad::trapping_NaN').  In spite of this the function
   determines any representation of NaN. */

int
IEEE_float::is_NaN (void)
{
  current_status_bits = 0;
  return float_class (this, &single_float_desc) == NOT_A_NUMBER;
}

/* The following member function tests that the IEEE single precision
   number is a trapping not a number constant.  The function returns
   1, if given IEEE single precision number is trapping NaN, 0
   otherwise.  According to the IEEE standard NaN (and trapping NaN)
   can be represented by more one bit string.  But all functions of
   the package generate and use only one its representation created by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_double::NaN', `IEEE_double::trapping_NaN', `IEEE_quad::NaN',
   `IEEE_quad::trapping_NaN').  In spite of this the function
   determines any representation of trapping NaN. */

int
IEEE_float::is_trapping_NaN (void)
{
  current_status_bits = 0;
  return float_class (this, &single_float_desc) == TRAPPING_NOT_A_NUMBER;
}

/* The following member function tests that the IEEE single precision
   number is positive infinity.  The function returns 1, if given IEEE
   single precision number is positive infinity, 0 otherwise. */

int
IEEE_float::is_positive_infinity (void)
{
  current_status_bits = 0;
  return float_class (this, &single_float_desc) == POSITIVE_INFINITY;
}

/* The following member function tests that the IEEE single precision
   number is negative infinity.  The function returns 1, if given IEEE
   single precision number is negative infinity, 0 otherwise. */

int
IEEE_float::is_negative_infinity (void)
{
  current_status_bits = 0;
  return float_class (this, &single_float_desc) == NEGATIVE_INFINITY;
}

/* The following member function tests that the IEEE double precision
   number is positive zero.  The function returns 1, if given IEEE
   double precision number is positive zero, 0 otherwise. */

int
IEEE_double::is_positive_zero (void)
{
  current_status_bits = 0;
  return float_class (this, &double_float_desc) == POSITIVE_ZERO;
}

/* The following member function tests that the IEEE double precision
   number is negative zero.  The function returns 1, if given IEEE
   double precision number is negative zero, 0 otherwise. */

int
IEEE_double::is_negative_zero (void)
{
  current_status_bits = 0;
  return float_class (this, &double_float_desc) == NEGATIVE_ZERO;
}

/* The following member function tests that the IEEE double precision
   number is (quiet) not a number constant.  The function returns 1,
   if given IEEE double precision number is NaN, 0 otherwise.
   According to the IEEE standard NaN (and trapping NaN) can be
   represented by more one bit string.  But all functions of the
   package generate and use only one its representation created by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_double::NaN', `IEEE_double::trapping_NaN', `IEEE_quad::NaN',
   `IEEE_quad::trapping_NaN').  In spite of this the function
   determines any representation of NaN. */

int
IEEE_double::is_NaN (void)
{
  current_status_bits = 0;
  return float_class (this, &double_float_desc) == NOT_A_NUMBER;
}

/* The following member function tests that the IEEE double precision
   number is a trapping not a number constant.  The function returns
   1, if given IEEE double precision number is trapping NaN, 0
   otherwise.  According to the IEEE standard NaN (and trapping NaN)
   can be represented by more one bit string.  But all functions of
   the package generate and use only one its representation created by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_double::NaN', `IEEE_double::trapping_NaN',
   `IEEE_quad::NaN', `IEEE_double::trapping_NaN').  In spite of this
   the function determines any representation of trapping NaN. */

int
IEEE_double::is_trapping_NaN (void)
{
  current_status_bits = 0;
  return float_class (this, &double_float_desc) == TRAPPING_NOT_A_NUMBER;
}

/* The following member function tests that the IEEE double precision
   number is positive infinity.  The function returns 1, if given IEEE
   double precision number is positive infinity, 0 otherwise. */

int
IEEE_double::is_positive_infinity (void)
{
  current_status_bits = 0;
  return float_class (this, &double_float_desc) == POSITIVE_INFINITY;
}

/* The following member function tests that the IEEE double precision
   number is negative infinity.  The function returns 1, if given IEEE
   double precision number is negative infinity, 0 otherwise. */

int
IEEE_double::is_negative_infinity (void)
{
  current_status_bits = 0;
  return float_class (this, &double_float_desc) == NEGATIVE_INFINITY;
}

#ifdef IEEE_QUAD
/* The following member function tests that the IEEE quad precision
   number is positive zero.  The function returns 1, if given IEEE
   quad precision number is positive zero, 0 otherwise. */

int
IEEE_quad::is_positive_zero (void)
{
  current_status_bits = 0;
  return float_class (this, &quad_float_desc) == POSITIVE_ZERO;
}

/* The following member function tests that the IEEE quad precision
   number is negative zero.  The function returns 1, if given IEEE
   quad precision number is negative zero, 0 otherwise. */

int
IEEE_quad::is_negative_zero (void)
{
  current_status_bits = 0;
  return float_class (this, &quad_float_desc) == NEGATIVE_ZERO;
}

/* The following member function tests that the IEEE quad precision
   number is (quiet) not a number constant.  The function returns 1,
   if given IEEE quad precision number is NaN, 0 otherwise.
   According to the IEEE standard NaN (and trapping NaN) can be
   represented by more one bit string.  But all functions of the
   package generate and use only one its representation created by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_quad::NaN', `IEEE_quad::trapping_NaN').  In spite of this
   the function determines any representation of NaN. */

int
IEEE_quad::is_NaN (void)
{
  current_status_bits = 0;
  return float_class (this, &quad_float_desc) == NOT_A_NUMBER;
}

/* The following member function tests that the IEEE quad precision
   number is a trapping not a number constant.  The function returns
   1, if given IEEE quad precision number is trapping NaN, 0
   otherwise.  According to the IEEE standard NaN (and trapping NaN)
   can be represented by more one bit string.  But all functions of
   the package generate and use only one its representation created by
   function `IEEE_float::NaN' (and `IEEE_float::trapping_NaN',
   `IEEE_quad::NaN', `IEEE_quad::trapping_NaN').  In spite of this
   the function determines any representation of trapping NaN. */

int
IEEE_quad::is_trapping_NaN (void)
{
  current_status_bits = 0;
  return float_class (this, &quad_float_desc) == TRAPPING_NOT_A_NUMBER;
}

/* The following member function tests that the IEEE quad precision
   number is positive infinity.  The function returns 1, if given IEEE
   quad precision number is positive infinity, 0 otherwise. */

int
IEEE_quad::is_positive_infinity (void)
{
  current_status_bits = 0;
  return float_class (this, &quad_float_desc) == POSITIVE_INFINITY;
}

/* The following member function tests that the IEEE quad precision
   number is negative infinity.  The function returns 1, if given IEEE
   quad precision number is negative infinity, 0 otherwise. */

int
IEEE_quad::is_negative_infinity (void)
{
  current_status_bits = 0;
  return float_class (this, &quad_float_desc) == NEGATIVE_INFINITY;
}
#endif /* #ifdef IEEE_QUAD */

/* The following member function tests that the IEEE single precision
   number is a normalized number.  The function retruns 1, if given
   IEEE single precision number is a normalized number, 0
   otherwise. */

int
IEEE_float::is_normalized (void)
{
  current_status_bits = 0;
  return float_class (this, &single_float_desc) == NORMALIZED_NUMBER;
}

/* The following member function tests that the IEEE single precision
   number is a denormalized number.  The function returns 1, if given
   IEEE single precision number is a denormalized number, 0
   otherwise. */

int
IEEE_float::is_denormalized (void)
{
  current_status_bits = 0;
  return float_class (this, &single_float_desc) == DENORMALIZED_NUMBER;
}

/* The following member function tests that the IEEE double precision
   number is a denormalized number.  The function returns 1, if given
   IEEE double precision number is a normalized number, 0
   otherwise. */

int
IEEE_double::is_normalized (void)
{
  current_status_bits = 0;
  return float_class (this, &double_float_desc) == NORMALIZED_NUMBER;
}

/* The following member function tests that the IEEE double precision
   number is a denormalized number.  The function returns 1, if given
   IEEE double precision number is a denormalized number, 0
   otherwise. */

int
IEEE_double::is_denormalized (void)
{
  current_status_bits = 0;
  return float_class (this, &double_float_desc) == DENORMALIZED_NUMBER;
}

#ifdef IEEE_QUAD
/* The following member function tests that the IEEE quad precision
   number is a denormalized number.  The function returns 1, if given
   IEEE quad precision number is a normalized number, 0
   otherwise. */

int
IEEE_quad::is_normalized (void)
{
  current_status_bits = 0;
  return float_class (this, &quad_float_desc) == NORMALIZED_NUMBER;
}

/* The following member function tests that the IEEE quad precision
   number is a denormalized number.  The function returns 1, if given
   IEEE quad precision number is a denormalized number, 0
   otherwise. */

int
IEEE_quad::is_denormalized (void)
{
  current_status_bits = 0;
  return float_class (this, &quad_float_desc) == DENORMALIZED_NUMBER;
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions for floating point addition and subtraction. */

/* The following function adds floating point numbers given by their
   signs, exponents, and fractions. */

static void
add (int *sign1, int *exponent1, fraction_t fraction1,
     int sign2, int exponent2, fraction_t fraction2,
     int *carry_flag, int *imprecise_flag)
{
  int swap_flag;

  if (*exponent1 < exponent2)
    {
      int temp_exponent;
      int temp_sign;
      unsigned char *temp_fraction;
      
      temp_sign = *sign1;
      *sign1 = sign2;
      sign2 = temp_sign;
      temp_exponent = *exponent1;
      *exponent1 = exponent2;
      exponent2 = temp_exponent;
      temp_fraction = fraction1;
      fraction1 = fraction2;
      fraction2 = temp_fraction;
      swap_flag = 1 /* TRUE */;
    }
  else
    swap_flag = 0 /* FALSE */;
  unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction2,
                                 *exponent1 - exponent2, fraction2);
  *imprecise_flag = integer::overflow_bit != 0;
  *carry_flag = 0;
  if (*imprecise_flag
      && bits::is_zero_bit_string (fraction2, 0,
                                   INTERNAL_FRACTION_SIZE * CHAR_BIT))
    bits::set_bit (fraction2, INTERNAL_FRACTION_SIZE * CHAR_BIT - 1, 1);
  if (*sign1 == sign2)
    {
      unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction1, fraction2,
                             (swap_flag ? fraction2 : fraction1));
      *carry_flag = integer::overflow_bit;
    }
  else if (unsigned_integer::ge (INTERNAL_FRACTION_SIZE, fraction1,
                                 fraction2))
    unsigned_integer::subtract (INTERNAL_FRACTION_SIZE, fraction1, fraction2,
                                (swap_flag ? fraction2 : fraction1));
  else
    {
      assert (!*imprecise_flag && *exponent1 == exponent2);
      unsigned_integer::subtract (INTERNAL_FRACTION_SIZE, fraction2, fraction1,
                                  (swap_flag ? fraction2 : fraction1));
      *sign1 = sign2;
    }
}


/* The following function fulfills addition of two IEEE precision
   floating point numbers with characteristics D.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described by the following table
          
          first  |         second operand                
          operand|---------------------------------------
                 |    +Inf      |    -Inf     |   Others
          -------|--------------|-------------|----------
          +Inf   |    +Inf      |     NaN     |   +Inf
                 |    none      |IEEE_INV(_RO)|   none
          -------|--------------|-------------|----------
          -Inf   |    NaN       |    -Inf     |   -Inf
                 |IEEE_INV(_RO) |    none     |   none
          -------|--------------|-------------|----------
          Others |    +Inf      |    -Inf     |
                 |    none      |    none     |          

   The function returns result (through FP) of the addition.  See
   commentaries about input exceptions for NaNs and output exceptions
   at the file begin. */

static void
IEEE_add (void *fp, void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int exception_mask;
  fraction_t fraction1;
  fraction_t fraction2;
  int carry_flag;
  int imprecise_flag;
  int exponent1;
  int exponent2;
  int sign1;
  int sign2;
  void (*saved_unsigned_overflow_reaction) (void);

  exception_mask = 0;
  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (class1 == TRAPPING_NOT_A_NUMBER || class2 == TRAPPING_NOT_A_NUMBER)
    {
      NaN (fp, d);
      exception_mask = IEEE_INV | IEEE_RO;
    }
  else if (class1 == NOT_A_NUMBER || class2 == NOT_A_NUMBER)
    {
      NaN (fp, d);
      exception_mask = IEEE_RO;
    }
  else if (class1 == POSITIVE_INFINITY || class2 == POSITIVE_INFINITY)
    {
      if (class1 == NEGATIVE_INFINITY || class2 == NEGATIVE_INFINITY)
        {
          NaN (fp, d);
          exception_mask = IEEE_INV | IEEE_RO;
        }
      else
        positive_infinity (fp, d);
    }
  else if (class1 == NEGATIVE_INFINITY || class2 == NEGATIVE_INFINITY)
    negative_infinity (fp, d);
  else
    {
      assert ((class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER
               || class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO)
              && (class2 == NORMALIZED_NUMBER || class2 == DENORMALIZED_NUMBER
                  || class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO));
      /* Not special cases operands */
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      unpack_float (fp1, d, &sign1, &exponent1, fraction1);
      unpack_float (fp2, d, &sign2, &exponent2, fraction2);
      add (&sign1, &exponent1, fraction1, sign2, exponent2, fraction2,
           &carry_flag, &imprecise_flag);
      form_float (fp, d, sign1, exponent1, fraction1, carry_flag,
		  imprecise_flag, saved_unsigned_overflow_reaction);
      return;
    }
  process_exception (exception_mask);
}

/* The following member function fulfills addition of two IEEE single
   precision floating point numbers.  Results and input exceptions for
   operands of special cases values (except for NaNs) are described in
   function `IEEE_add'.  The function returns result of the addition.
   See commentaries about input exceptions for NaNs and output
   exceptions at the file begin. */

IEEE_float_t
IEEE_float::operator + (IEEE_float_t &operand)
{
  IEEE_float_t result;
  
  current_status_bits = 0;
  IEEE_add (&result, this, &operand, &single_float_desc);
  return result;
}

/* The following member function fulfills addition of two IEEE double
   precision floating point numbers.  Results and input exceptions for
   operands of special cases values (except for NaNs) are described in
   function `IEEE_add'.  The function returns result of the addition.
   See commentaries about input exceptions for NaNs and output
   exceptions at the file begin. */

IEEE_double_t
IEEE_double::operator + (IEEE_double_t &operand)
{
  IEEE_double_t result;

  current_status_bits = 0;
  IEEE_add (&result, this, &operand, &double_float_desc);
  return result;
}

#ifdef IEEE_QUAD
/* The following member function fulfills addition of two IEEE quad
   precision floating point numbers.  Results and input exceptions for
   operands of special cases values (except for NaNs) are described in
   function `IEEE_add'.  The function returns result of the addition.
   See commentaries about input exceptions for NaNs and output
   exceptions at the file begin. */

IEEE_quad_t
IEEE_quad::operator + (IEEE_quad_t &operand)
{
  IEEE_quad_t result;

  current_status_bits = 0;
  IEEE_add (&result, this, &operand, &quad_float_desc);
  return result;
}
#endif /* #ifdef IEEE_QUAD */

/* The following function fulfills subtraction of two IEEE floating
   point numbers FP1, FP2 with characteristics D.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described by the following table
          
          first  |         second operand                
          operand|---------------------------------------
                 |    +Inf     |    -Inf      |   Others
          -------|-------------|--------------|----------
          +Inf   |     NaN     |    +Inf      |   +Inf
                 |IEEE_INV(_RO)|    none      |   none
          -------|-------------|--------------|----------
          -Inf   |    -Inf     |    NaN       |   -Inf
                 |    none     |IEEE_INV(_RO) |   none
          -------|-------------|--------------|----------
          Others |    -Inf     |    +Inf      |
                 |    none     |    none      |          

   The function returns result of the subtraction through FP.  See
   commentaries about input exceptions for NaNs and output exceptions
   at the file begin. */

static void
IEEE_subtract (void *fp, void *fp1, void *fp2, float_desc_t d)
{
  int sign;

  sign = bits::bit (fp2, d->sign_bit);
  bits::set_bit (fp2, d->sign_bit, !sign);
  /* Remember that sign of not a number is not important.  Therefore
     all cases are processed correctly in function for addition. */
  IEEE_add (fp, fp1, fp2, d);
}

/* The following member function fulfills subtraction of two IEEE
   single precision floating point numbers.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described in function `IEEE_subtract'.  The function returns
   result of the subtraction.  See commentaries about input exceptions
   for NaNs and output exceptions at the file begin. */

IEEE_float_t
IEEE_float:: operator - (IEEE_float_t &operand)
{
  IEEE_float_t result, op = operand; /* Remeber we will change operand sign */

  current_status_bits = 0;
  IEEE_subtract (&result, this, &op, &single_float_desc);
  return result;
}

/* The following member function fulfills subtraction of two IEEE
   double precision floating point numbers.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described in function `IEEE_subtract'.  The function returns
   result of the subtraction.  See commentaries about input exceptions
   for NaNs and output exceptions at the file begin. */

IEEE_double_t
IEEE_double:: operator - (IEEE_double_t &operand)
{
  IEEE_double_t result, op = operand; /* Remeber we will change operand sign */

  current_status_bits = 0;
  IEEE_subtract (&result, this, &op, &double_float_desc);
  return result;
}

#ifdef IEEE_QUAD
/* The following member function fulfills subtraction of two IEEE
   quad precision floating point numbers.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described in function `IEEE_subtract'.  The function returns
   result of the subtraction.  See commentaries about input exceptions
   for NaNs and output exceptions at the file begin. */

IEEE_quad_t
IEEE_quad:: operator - (IEEE_quad_t &operand)
{
  IEEE_quad_t result, op = operand; /* Remeber we will change operand sign */

  current_status_bits = 0;
  IEEE_subtract (&result, this, &op, &quad_float_desc);
  return result;
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions for floating point multiplication. */

/* The following function multiplys floating point numbers given by
   their signs, exponents, and fractions.  The first exponent can be
   biased or non biased, the result exponent will be the same (biased
   or nonbiased). */

static void
multiply (int *sign1, int *exponent1, fraction_t fraction1,
          int sign2, int non_biased_exponent2, fraction_t fraction2,
          int *carry_flag)
{
  unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction1,
                                 INTERNAL_FRACTION_SIZE * CHAR_BIT / 2,
                                 fraction1);
  assert (!integer::overflow_bit);
  unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction2,
                                 INTERNAL_FRACTION_SIZE * CHAR_BIT / 2,
                                 fraction2);
  assert (!integer::overflow_bit);
  *sign1 = *sign1 != sign2;
  *exponent1 += non_biased_exponent2;
  unsigned_integer::multiply (INTERNAL_FRACTION_SIZE, fraction1, fraction2,
                              fraction1);
  assert (!integer::overflow_bit);
  unsigned_integer::shift_left (INTERNAL_FRACTION_SIZE, fraction1, 1,
                                fraction1);
  /* If overflow_bit is set up then the fraction is not less than 2. */
  *carry_flag = integer::overflow_bit;
}

/* The following function fulfills multiplication of two IEEE floating
   point numbers FP1, FP2 with characteristics D.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described by the following table
        
        first  |         second operand                
        operand|----------------------------------------------------
               |    +Inf     |    -Inf     |    0        |   Others 
        -------|-------------|-------------|-------------|----------
        +Inf   |    +Inf     |    -Inf     |    NaN      |  (+-)Inf
               |    none     |    none     |IEEE_INV(_RO)|   none   
        -------|-------------|-------------|-------------|----------
        -Inf   |    -Inf     |    +Inf     |    NaN      |  (+-)Inf 
               |    none     |    none     |IEEE_INV(_RO)|   none   
        -------|-------------|-------------|-------------|----------
        0      |     NaN     |    NaN      |   (+-)0     |  (+-)0  
               |IEEE_INV(_RO)|IEEE_INV(_RO)|   none      |  none    
        -------|-------------|-------------|-------------|----------
        Others |   (+-)Inf   |   (+-)Inf   |   (+-)0     |          
               |    none     |    none     |   none      |          

   The function returns result of the multiplication through FP.  See
   commentaries about input exceptions for NaNs and output exceptions
   at the file begin. */

static void
IEEE_multiply (void *fp, void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int exception_mask;
  fraction_t fraction1;
  fraction_t fraction2;
  int carry_flag;
  int exponent1;
  int exponent2;
  int sign1;
  int sign2;
  void (*saved_unsigned_overflow_reaction) (void);

  exception_mask = 0;
  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (class1 == TRAPPING_NOT_A_NUMBER
      || class2 == TRAPPING_NOT_A_NUMBER)
    {
      NaN (fp, d);
      exception_mask = IEEE_INV | IEEE_RO;
    }
  else if (class1 == NOT_A_NUMBER || class2 == NOT_A_NUMBER)
    {
      NaN (fp, d);
      exception_mask = IEEE_RO;
    }
  else if (class1 == POSITIVE_INFINITY || class1 == NEGATIVE_INFINITY
           || class2 == POSITIVE_INFINITY || class2 == NEGATIVE_INFINITY)
    {
      if (class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO
          || class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO)
        {
          NaN (fp, d);
          exception_mask = IEEE_INV | IEEE_RO;
        }
      else if (bits::bit (fp1, d->sign_bit) == bits::bit (fp2, d->sign_bit))
	positive_infinity (fp, d);
      else
	negative_infinity (fp, d);
    }
  else if (class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO
           || class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO)
    {
      if (bits::bit (fp1, d->sign_bit) == bits::bit (fp2, d->sign_bit))
	positive_zero (fp, d);
      else
	negative_zero (fp, d);
    }
  else
    {
      assert ((class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
              && (class2 == NORMALIZED_NUMBER
		  || class2 == DENORMALIZED_NUMBER));
      /* Not special cases operands */
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      unpack_float (fp1, d, &sign1, &exponent1, fraction1);
      unpack_float (fp2, d, &sign2, &exponent2, fraction2);
      multiply (&sign1, &exponent1, fraction1,
                sign2, exponent2 - d->exponent_bias, fraction2,
                &carry_flag);
      form_float (fp, d, sign1, exponent1, fraction1, carry_flag,
		  0 /* FALSE */, saved_unsigned_overflow_reaction);
      return;
    }
  process_exception (exception_mask);
}

/* The following member function fulfills multiplication of two IEEE
   single precision floating point numbers.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described in function `IEEE_multiply'.  The function returns
   result of the multiplication.  See commentaries about input
   exceptions for NaNs and output exceptions at the file begin. */

IEEE_float_t
IEEE_float::operator * (IEEE_float_t &operand)
{
  IEEE_float_t result;

  current_status_bits = 0;
  IEEE_multiply (&result, this, &operand, &single_float_desc);
  return result;
}

/* The following member function fulfills multiplication of two IEEE
   double precision floating point numbers.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described in function `IEEE_multiply'.  The function returns
   result of the multiplication.  See commentaries about input
   exceptions for NaNs and output exceptions at the file begin. */

IEEE_double_t
IEEE_double::operator * (IEEE_double_t &operand)
{
  IEEE_double_t result;

  current_status_bits = 0;
  IEEE_multiply (&result, this, &operand, &double_float_desc);
  return result;
}

#ifdef IEEE_QUAD
/* The following member function fulfills multiplication of two IEEE
   quad precision floating point numbers.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described in function `IEEE_multiply'.  The function returns
   result of the multiplication.  See commentaries about input
   exceptions for NaNs and output exceptions at the file begin. */

IEEE_quad_t
IEEE_quad::operator * (IEEE_quad_t &operand)
{
  IEEE_quad_t result;

  current_status_bits = 0;
  IEEE_multiply (&result, this, &operand, &quad_float_desc);
  return result;
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions for floating point multiplication. */

/* The following function divides floating point numbers given by
   their signs, exponents, and fractions.  The first exponent can be
   biased or non biased, the result exponent will be the same (biased
   or nonbiased). */

static void
divide (int *sign1, int *exponent1, fraction_t fraction1,
        int sign2, int non_biased_exponent2, fraction_t fraction2,
        int *imprecise_flag)
{
  fraction_t result_fraction;

  *sign1 = *sign1 != sign2;
  normalize_fraction (exponent1, fraction1);
  normalize_fraction (&non_biased_exponent2, fraction2);
  /* Shift on INTERNAL_FRACTION_SIZE * CHAR_BIT / 2 + 1 for calculation of
     INTERNAL_FRACTION_SIZE * CHAR_BIT / 2 + 1 bits. */
  unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction2,
                                 INTERNAL_FRACTION_SIZE * CHAR_BIT / 2 + 1,
                                 fraction2);
  assert (!integer::overflow_bit);
  unsigned_integer::divide (INTERNAL_FRACTION_SIZE, fraction1, fraction2,
                            result_fraction);
  unsigned_integer::multiply (INTERNAL_FRACTION_SIZE, result_fraction,
                              fraction2, fraction2);
  assert (!integer::overflow_bit);
  *imprecise_flag = unsigned_integer::ne (INTERNAL_FRACTION_SIZE,
                                          fraction2, fraction1);
  unsigned_integer::shift_left (INTERNAL_FRACTION_SIZE, result_fraction,
                                INTERNAL_FRACTION_SIZE * CHAR_BIT / 2 - 2,
                                result_fraction);
  assert (!integer::overflow_bit);
  *exponent1 -= non_biased_exponent2;
  memcpy (fraction1, result_fraction, (size_t) INTERNAL_FRACTION_SIZE);
}

/* The following function fulfills division of two IEEE floating point
   numbers FP1, FP2 with characteristics D.  Results and input
   exceptions for operands of special cases values (except for NaNs)
   are described by the following table
        
        first  |         second operand                
        operand|----------------------------------------------------
               |    +Inf     |    -Inf     |    0        |   Others 
        -------|-------------|-------------|-------------|----------
        +Inf   |     NaN     |     NaN     |   (+-)Inf   |  (+-)Inf
               |IEEE_INV(_RO)|IEEE_INV(_RO)|   none      |   none   
        -------|-------------|-------------|-------------|----------
        -Inf   |     NaN     |     NaN     |   (+-)Inf   |  (+-)Inf 
               |IEEE_INV(_RO)|IEEE_INV(_RO)|   none      |   none   
        -------|-------------|-------------|-------------|----------
        0      |   (+-)0     |   (+-)0     |     NaN     |  (+-)0  
               |   none      |   none      |IEEE_INV(_RO)|  none    
        -------|-------------|-------------|-------------|----------
        Others |   (+-)0     |   (+-)0     |   (+-)Inf   |          
               |   none      |    none     |   IEEE_DZ   |          

   The function returns result of the division through FP.  See
   commentaries about input exceptions for NaNs and output exceptions
   at the file begin. */

static void
IEEE_divide (void *fp, void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int exception_mask;
  fraction_t fraction1;
  fraction_t fraction2;
  int imprecise_flag;
  int exponent1;
  int exponent2;
  int sign1;
  int sign2;
  void (*saved_unsigned_overflow_reaction) (void);

  exception_mask = 0;
  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (class1 == TRAPPING_NOT_A_NUMBER || class2 == TRAPPING_NOT_A_NUMBER)
    {
      NaN (fp, d);
      exception_mask = IEEE_INV | IEEE_RO;
    }
  else if (class1 == NOT_A_NUMBER || class2 == NOT_A_NUMBER)
    {
      NaN (fp, d);
      exception_mask = IEEE_RO;
    }
  else if (class1 == POSITIVE_INFINITY || class1 == NEGATIVE_INFINITY)
    {
      if (class2 == POSITIVE_INFINITY || class2 == NEGATIVE_INFINITY)
        {
          NaN (fp, d);
          exception_mask = IEEE_INV | IEEE_RO;
        }
      /* Also zero divisor here. */
      else if (bits::bit (fp1, d->sign_bit) == bits::bit (fp2, d->sign_bit))
	positive_infinity (fp, d);
      else
	negative_infinity (fp, d);
    }
  else if (class2 == POSITIVE_INFINITY || class2 == NEGATIVE_INFINITY)
    {
     if (bits::bit (fp1, d->sign_bit) == bits::bit (fp2, d->sign_bit))
       positive_zero (fp, d);
     else
       negative_zero (fp, d);
    }
  else if (class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO)
    {
      if (class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO)
        {
          NaN (fp, d);
          exception_mask = IEEE_INV | IEEE_RO;
        }
      else
        {
	  if (bits::bit (fp1, d->sign_bit) == bits::bit (fp2, d->sign_bit))
            positive_infinity (fp, d);
	  else
	    negative_infinity (fp, d);
          exception_mask = IEEE_DZ;
        }
    }
  else if (class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO)
    {
      if (bits::bit (fp1, d->sign_bit) == bits::bit (fp2, d->sign_bit))
	positive_zero (fp, d);
      else
	negative_zero (fp, d);
    }
  else
    {
      assert ((class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
              && (class2 == NORMALIZED_NUMBER
		  || class2 == DENORMALIZED_NUMBER));
      /* Not special cases operands */
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      unpack_float (fp1, d, &sign1, &exponent1, fraction1);
      unpack_float (fp2, d, &sign2, &exponent2, fraction2);
      divide (&sign1, &exponent1, fraction1,
              sign2, exponent2 - d->exponent_bias, fraction2,
              &imprecise_flag);
      form_float (fp, d, sign1, exponent1, fraction1, 0,
		  imprecise_flag, saved_unsigned_overflow_reaction);
      return;
    }
  process_exception (exception_mask);
}

/* The following function fulfills division of two IEEE single
   precision floating point numbers.  Results and input exceptions for
   operands of special cases values (except for NaNs) are described in
   function IEEE_divide.  The function returns result of the division.
   See commentaries about input exceptions for NaNs and output
   exceptions at the file begin. */

IEEE_float_t
IEEE_float::operator / (IEEE_float_t &operand)
{
  IEEE_float_t result;

  current_status_bits = 0;
  IEEE_divide (&result, this, &operand, &single_float_desc);
  return result;
}

/* The following function fulfills division of two IEEE double
   precision floating point numbers.  Results and input exceptions for
   operands of special cases values (except for NaNs) are described in
   function IEEE_divide.  The function returns result of the division.
   See commentaries about input exceptions for NaNs and output
   exceptions at the file begin. */

IEEE_double_t
IEEE_double::operator / (IEEE_double_t &operand)
{
  IEEE_double_t result;

  current_status_bits = 0;
  IEEE_divide (&result, this, &operand, &double_float_desc);
  return result;
}

#ifdef IEEE_QUAD
/* The following function fulfills division of two IEEE quad
   precision floating point numbers.  Results and input exceptions for
   operands of special cases values (except for NaNs) are described in
   function IEEE_divide.  The function returns result of the division.
   See commentaries about input exceptions for NaNs and output
   exceptions at the file begin. */

IEEE_quad_t
IEEE_quad::operator / (IEEE_quad_t &operand)
{
  IEEE_quad_t result;

  current_status_bits = 0;
  IEEE_divide (&result, this, &operand, &quad_float_desc);
  return result;
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions for single floating point comparison
   on equality or inequality. */

/* The following function fulfills processing special cases of
   comparison on equality or inequality when a operand is NaN (quiet
   or trapping).  Results and input exceptions for operands of special
   cases values are described by the following table
          
          first  |         second operand                
          operand|---------------------------------------
                 |    SNaN     |    QNaN      |   Others
          -------|-------------|--------------|----------
          SNaN   |   FALSE     |   FALSE      |  FALSE
                 |  IEEE_INV   |  IEEE_INV    | IEEE_INV
          -------|-------------|--------------|----------
          QNaN   |   FALSE     |   FALSE      |  FALSE
                 |  IEEE_INV   |    none      |   none
          -------|-------------|--------------|----------
          Others |   FALSE     |   FALSE      |
                 |  IEEE_INV   |    none      |          

   The function returns 1, if a operand is a NaN and consequently
   *result is defined.  Input exceptions for comparisons are differed
   from the ones of other operations (see commentaries for input
   exceptions at the file begin).  Floating point comparison produce
   no output exceptions. */

static int
eq_ne_special_cases (int class1, int class2, int *result)
{
  if (class1 == TRAPPING_NOT_A_NUMBER || class2 == TRAPPING_NOT_A_NUMBER)
    {
      *result = 0;
      process_exception (IEEE_INV);
    }
  else if (class1 == NOT_A_NUMBER || class2 == NOT_A_NUMBER)
    *result = 0;
  else
    {
      assert ((class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO
               || class1 == POSITIVE_INFINITY || class1 == NEGATIVE_INFINITY
               || class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
              && (class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO
                  || class2 == POSITIVE_INFINITY || class2 == NEGATIVE_INFINITY
                  || class2 == NORMALIZED_NUMBER
                  || class2 == DENORMALIZED_NUMBER));
      return 0 /* FALSE */;
    }
  return 1 /* TRUE */;
}

/* The following function fulfills comparison on equality of two IEEE
   floating point numbers FP1, FP2 with characteristics D.  The
   function returns 1 if the numbers are equal.  See commentaries
   about input and output exceptions for NaNs in function
   `eq_ne_special_cases'. */

static int
IEEE_eq (void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int result;

  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (eq_ne_special_cases (class1, class2, &result))
    return result;
  else if (class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO)
    return class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO;
  else if (class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO)
    return 0 /* FALSE */;
  else
    return memcmp (fp1, fp2, (size_t) d->size) == 0;
}

/* The following member function fulfills comparison on equality of two IEEE
   single precision floating point numbers.  The function returns 1 if
   the numbers are equal.  See commentaries about input and output
   exceptions for NaNs in function `eq_ne_special_cases'. */

int
IEEE_float:: operator == (IEEE_float_t &operand)
{
  current_status_bits = 0;
  return IEEE_eq (this, &operand, &single_float_desc);
}

/* The following member function fulfills comparison on equality of two IEEE
   double precision floating point numbers.  The function returns 1 if
   the numbers are equal.  See commentaries about input and output
   exceptions for NaNs in function `eq_ne_special_cases'. */

int
IEEE_double:: operator == (IEEE_double_t &operand)
{
  current_status_bits = 0;
  return IEEE_eq (this, &operand, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following member function fulfills comparison on equality of two IEEE
   quad precision floating point numbers.  The function returns 1 if
   the numbers are equal.  See commentaries about input and output
   exceptions for NaNs in function `eq_ne_special_cases'. */

int
IEEE_quad:: operator == (IEEE_quad_t &operand)
{
  current_status_bits = 0;
  return IEEE_eq (this, &operand, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */

/* The following function fulfills comparison on inequality of two
   IEEE floating point numbers FP1, FP2 with characteristics D.  The
   function returns 1 if the numbers are not equal.  See commentaries
   about input and output exceptions for NaNs in function
   `eq_ne_special_cases'. */

static int
IEEE_ne (void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int result;

  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (eq_ne_special_cases (class1, class2, &result))
    return result;
  else if (class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO)
    return class2 != POSITIVE_ZERO && class2 != NEGATIVE_ZERO;
  else if (class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO)
    return 1 /* TRUE */;
  else
    return memcmp (fp1, fp2, (size_t) d->size) != 0;
}

/* The following member function fulfills comparison on inequality of two
   IEEE single precision floating point numbers.  the function returns
   1 if the numbers are not equal.  See commentaries about input and
   output exceptions for NaNs in function `eq_ne_special_cases'. */

int
IEEE_float:: operator != (IEEE_float_t &operand)
{
  current_status_bits = 0;
  return IEEE_ne (this, &operand, &single_float_desc);
}

/* The following member function fulfills comparison on inequality of two
   IEEE double precision floating point numbers.  The function returns
   1 if the numbers are not equal.  See commentaries about input and
   output exceptions for NaNs in function `eq_ne_special_cases'. */

int
IEEE_double:: operator != (IEEE_double_t &operand)
{
  current_status_bits = 0;
  return IEEE_ne (this, &operand, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following member function fulfills comparison on inequality of two
   IEEE quad precision floating point numbers.  The function returns
   1 if the numbers are not equal.  See commentaries about input and
   output exceptions for NaNs in function `eq_ne_special_cases'. */

int
IEEE_quad:: operator != (IEEE_quad_t &operand)
{
  current_status_bits = 0;
  return IEEE_ne (this, &operand, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions for single floating point order comparison. */

/* The following function fulfills processing special cases of order
   comparison when a operand is NaN (quiet or trapping).  Results and
   input exceptions for operands of special cases values are described
   by the following table
          
          first  |         second operand                
          operand|---------------------------------------
                 |    SNaN     |    QNaN      |   Others
          -------|-------------|--------------|----------
          SNaN   |   FALSE     |   FALSE      |  FALSE
                 |  IEEE_INV   |  IEEE_INV    | IEEE_INV
          -------|-------------|--------------|----------
          QNaN   |   FALSE     |   FALSE      |  FALSE
                 |  IEEE_INV   |  IEEE_INV    | IEEE_INV
          -------|-------------|--------------|----------
          Others |   FALSE     |   FALSE      |
                 |  IEEE_INV   |  IEEE_INV    |          

   The function returns 1, if a operand is a NaN and consequently
   *result is defined.  Input exceptions for comparisons are differed
   from the ones of other operations (see commentaries for input
   exceptions at the file begin).  Floating point comparison produce
   no output exceptions. */

static int
order_comparison_special_cases (int class1, int class2, int *result)
{
  if (class1 == TRAPPING_NOT_A_NUMBER || class2 == TRAPPING_NOT_A_NUMBER
      || class1 == NOT_A_NUMBER || class2 == NOT_A_NUMBER)
    {
      *result = 0;
      process_exception (IEEE_INV);
      return 1 /* TRUE */;
    }
  else
    {
      assert ((class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO
               || class1 == POSITIVE_INFINITY || class1 == NEGATIVE_INFINITY
               || class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
              && (class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO
                  || class2 == POSITIVE_INFINITY || class2 == NEGATIVE_INFINITY
                  || class2 == NORMALIZED_NUMBER
                  || class2 == DENORMALIZED_NUMBER));
        return 0 /* FALSE */;
    }
}

/* The following function fulfills comparison of two IEEE floating
   point numbers given by their classes, signs, exponents, and
   fractions.  The function does not process NaNs.  Results for
   operands of special cases values are described by the following
   table

          first  |         second operand                
          operand|---------------------------------------
                 |    +Inf      |    -Inf     |   Others
          -------|--------------|-------------|----------
          +Inf   |   FALSE      |    TRUE     |   TRUE
                 |    none      |    none     |   none
          -------|--------------|-------------|----------
          -Inf   |   FALSE      |   FALSE     |  FALSE
                 |   none       |    none     |   none
          -------|--------------|-------------|----------
          Others |   FALSE      |    TRUE     |
                 |    none      |    none     |          

   The function returns 1 if the first number is greater than the
   second.  See commentaries about input and output exceptions for
   NaNs in function `order_comparison_special_cases'. */

static int
gt (int class1, int sign1, int exponent1, fraction_t fraction1,
    int class2, int sign2, int exponent2, fraction_t fraction2)
{
  if (class1 == POSITIVE_INFINITY || class1 == NEGATIVE_INFINITY
      || class2 == POSITIVE_INFINITY || class2 == NEGATIVE_INFINITY)
    return (class1 != class2
            && (class1 == POSITIVE_INFINITY || class2 == NEGATIVE_INFINITY));
  else if (class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO)
    return (class2 != POSITIVE_ZERO && class2 != NEGATIVE_ZERO && sign2);
  else if (class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO)
    return !sign1;
  else
    {
      assert ((class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
              || (class2 == NORMALIZED_NUMBER
                  || class2 == DENORMALIZED_NUMBER));
      if (sign1 != sign2)
        return !sign1;
      else
        {
          /* Not special cases operands */
          if (exponent1 < exponent2)
            unsigned_integer::shift_right
              (INTERNAL_FRACTION_SIZE, fraction1, exponent2 - exponent1,
               fraction1);
          else
            unsigned_integer::shift_right
              (INTERNAL_FRACTION_SIZE, fraction2, exponent1 - exponent2,
               fraction2);
          if (sign1)
            return unsigned_integer::gt (INTERNAL_FRACTION_SIZE,
                                         fraction2, fraction1);
          else
            return unsigned_integer::gt (INTERNAL_FRACTION_SIZE,
                                         fraction1, fraction2);
        }
    }
}

/* The following function fulfills comparison of two IEEE floating
   point numbers FP1, FP2 with characteristics D with the aid of
   functions `gt' and `order_comparison_special_cases'.  The function
   returns 1 if the first number is greater than the second.  See
   commentaries about input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

static int
IEEE_gt (void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int result;
  fraction_t fraction1;
  fraction_t fraction2;
  int exponent1;
  int exponent2;
  int sign1;
  int sign2;
  void (*saved_unsigned_overflow_reaction) (void);

  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (order_comparison_special_cases (class1, class2, &result))
    return result;
  else
    {
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      if (class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
	unpack_float (fp1, d, &sign1, &exponent1, fraction1);
      if (class2 == NORMALIZED_NUMBER || class2 == DENORMALIZED_NUMBER)
	unpack_float (fp2, d, &sign2, &exponent2, fraction2);
      result = gt (class1, sign1, exponent1, fraction1,
                   class2, sign2, exponent2, fraction2);
      (void) unsigned_integer::set_overflow_reaction
        (saved_unsigned_overflow_reaction);
      return result;
    }
}

/* The following member function fulfills comparison of two IEEE single
   precision floating point numbers with the aid of functions `gt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is greater than the second.  See commentaries about
   input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_float:: operator > (IEEE_float_t &operand)
{
  current_status_bits = 0;
  return IEEE_gt (this, &operand, &single_float_desc);
}

/* The following member function fulfills comparison of two IEEE double
   precision floating point numbers with the aid of functions `gt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is greater than the second.  See commentaries about
   input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_double:: operator > (IEEE_double_t &operand)
{
  current_status_bits = 0;
  return IEEE_gt (this, &operand, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following member function fulfills comparison of two IEEE quad
   precision floating point numbers with the aid of functions `gt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is greater than the second.  See commentaries about
   input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_quad:: operator > (IEEE_quad_t &operand)
{
  current_status_bits = 0;
  return IEEE_gt (this, &operand, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */

/* The following function fulfills comparison of two IEEE floating
   point numbers given by their classes, signs, exponents, and
   fractions.  The function does not process NaNs.  Results for
   operands of special cases values are described by the following
   table

          first  |         second operand                
          operand|---------------------------------------
                 |    +Inf      |    -Inf     |   Others
          -------|--------------|-------------|----------
          +Inf   |   FALSE      |   FALSE     |  FALSE
                 |    none      |    none     |   none
          -------|--------------|-------------|----------
          -Inf   |   TRUE       |   FALSE     |   TRUE
                 |   none       |    none     |   none
          -------|--------------|-------------|----------
          Others |    TRUE      |   FALSE     |
                 |    none      |    none     |          

   The function returns 1 if the first number is less than the second.
   See commentaries about input and output exceptions for NaNs in
   function `order_comparison_special_cases'. */

static int
lt (int class1, int sign1, int exponent1, fraction_t fraction1,
    int class2, int sign2, int exponent2, fraction_t fraction2)
{
  if (class1 == POSITIVE_INFINITY || class1 == NEGATIVE_INFINITY
      || class2 == POSITIVE_INFINITY || class2 == NEGATIVE_INFINITY)
    return (class1 != class2
            && (class2 == POSITIVE_INFINITY || class1 == NEGATIVE_INFINITY));
  else if (class1 == POSITIVE_ZERO || class1 == NEGATIVE_ZERO)
    return (class2 != POSITIVE_ZERO && class2 != NEGATIVE_ZERO && !sign2);
  else if (class2 == POSITIVE_ZERO || class2 == NEGATIVE_ZERO)
    return sign1;
  else
    {
      assert ((class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
              && (class2 == NORMALIZED_NUMBER
                  || class2 == DENORMALIZED_NUMBER));
      if (sign1 != sign2)
        return sign1;
      else
        {
          /* Not special cases operands */
          if (exponent1 < exponent2)
            unsigned_integer::shift_right
              (INTERNAL_FRACTION_SIZE, fraction1, exponent2 - exponent1,
               fraction1);
          else
            unsigned_integer::shift_right
              (INTERNAL_FRACTION_SIZE, fraction2, exponent1 - exponent2,
               fraction2);
          if (sign1)
            return unsigned_integer::lt (INTERNAL_FRACTION_SIZE,
                                         fraction2, fraction1);
          else
            return unsigned_integer::lt (INTERNAL_FRACTION_SIZE,
                                         fraction1, fraction2);
        }
    }
}

/* The following function fulfills comparison of two IEEE single
   precision floating point numbers FP1, FP2 with characteristics D
   with the aid of functions `lt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is less than the second.  See commentaries about input
   and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

static int
IEEE_lt (void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int result;
  fraction_t fraction1;
  fraction_t fraction2;
  int exponent1;
  int exponent2;
  int sign1;
  int sign2;
  void (*saved_unsigned_overflow_reaction) (void);

  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (order_comparison_special_cases (class1, class2, &result))
    return result;
  else
    {
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      if (class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
	unpack_float (fp1, d, &sign1, &exponent1, fraction1);
      if (class2 == NORMALIZED_NUMBER || class2 == DENORMALIZED_NUMBER)
	unpack_float (fp2, d, &sign2, &exponent2, fraction2);
      result = lt (class1, sign1, exponent1, fraction1,
                   class2, sign2, exponent2, fraction2);
      (void) unsigned_integer::set_overflow_reaction
        (saved_unsigned_overflow_reaction);
      return result;
    }
}

/* The following member function fulfills comparison of two IEEE single
   precision floating point numbers with the aid of functions `lt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is less than the second.  See commentaries about input
   and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_float:: operator < (IEEE_float_t &operand)
{
  current_status_bits = 0;
  return IEEE_lt (this, &operand, &single_float_desc);
}

/* The following member function fulfills comparison of two IEEE double
   precision floating point numbers with the aid of functions `lt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is less than the second.  See commentaries about input
   and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_double:: operator < (IEEE_double_t &operand)
{
  current_status_bits = 0;
  return IEEE_lt (this, &operand, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following member function fulfills comparison of two IEEE quad
   precision floating point numbers with the aid of functions `lt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is less than the second.  See commentaries about input
   and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_quad:: operator < (IEEE_quad_t &operand)
{
  current_status_bits = 0;
  return IEEE_lt (this, &operand, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */

/* The following function fulfills comparison of two IEEE single
   precision floating point numbers FP1, FP2 with characteristics D
   with the aid of functions `lt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is greater than or equal to the second.  See
   commentaries about input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

static int
IEEE_ge (void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int result;
  fraction_t fraction1;
  fraction_t fraction2;
  int exponent1;
  int exponent2;
  int sign1;
  int sign2;
  void (*saved_unsigned_overflow_reaction) (void);

  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (order_comparison_special_cases (class1, class2, &result))
    return result;
  else
    {
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      if (class1 == NORMALIZED_NUMBER || class1 == DENORMALIZED_NUMBER)
	unpack_float (fp1, d, &sign1, &exponent1, fraction1);
      if (class2 == NORMALIZED_NUMBER || class2 == DENORMALIZED_NUMBER)
	unpack_float (fp2, d, &sign2, &exponent2, fraction2);
      result = !lt (class1, sign1, exponent1, fraction1,
                    class2, sign2, exponent2, fraction2);
      (void) unsigned_integer::set_overflow_reaction
        (saved_unsigned_overflow_reaction);
      return result;
    }
}

/* The following member function fulfills comparison of two IEEE single
   precision floating point numbers with the aid of functions `lt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is greater than or equal to the second.  See
   commentaries about input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_float:: operator >= (IEEE_float_t &operand)
{
  current_status_bits = 0;
  return IEEE_ge (this, &operand, &single_float_desc);
}

/* The following member function fulfills comparison of two IEEE double
   precision floating point numbers with the aid of functions `lt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is greater than or equal to the second.  See
   commentaries about input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_double:: operator >= (IEEE_double_t &operand)
{
  current_status_bits = 0;
  return IEEE_ge (this, &operand, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following member function fulfills comparison of two IEEE quad
   precision floating point numbers with the aid of functions `lt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is greater than or equal to the second.  See
   commentaries about input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_quad:: operator >= (IEEE_quad_t &operand)
{
  current_status_bits = 0;
  return IEEE_ge (this, &operand, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */

/* The following function fulfills comparison of two IEEE floating
   point numbers FP1, FP2 with characteristics D with the aid of
   functions `gt' and `order_comparison_special_cases'.  The function
   returns 1 if the first number is less than or equal to the second.
   See commentaries about input and output exceptions for NaNs in
   function `order_comparison_special_cases'. */

static int
IEEE_le (void *fp1, void *fp2, float_desc_t d)
{
  enum number_class class1;
  enum number_class class2;
  int result;
  fraction_t fraction1;
  fraction_t fraction2;
  int exponent1;
  int exponent2;
  int sign1;
  int sign2;
  void (*saved_unsigned_overflow_reaction) (void);

  class1 = float_class (fp1, d);
  class2 = float_class (fp2, d);
  if (order_comparison_special_cases (class1, class2, &result))
    return result;
  else
    {
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      if (class1 == NORMALIZED_NUMBER
          || class1 == DENORMALIZED_NUMBER)
	unpack_float (fp1, d, &sign1, &exponent1, fraction1);
      if (class2 == NORMALIZED_NUMBER
          || class2 == DENORMALIZED_NUMBER)
	unpack_float (fp2, d, &sign2, &exponent2, fraction2);
      result = !gt (class1, sign1, exponent1, fraction1,
                    class2, sign2, exponent2, fraction2);
      (void) unsigned_integer::set_overflow_reaction
        (saved_unsigned_overflow_reaction);
      return result;
    }
}

/* The following member function fulfills comparison of two IEEE single
   precision floating point numbers with the aid of functions `gt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is less than or equal to the second.  See commentaries
   about input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_float:: operator <= (IEEE_float_t &operand)
{
  current_status_bits = 0;
  return IEEE_le (this, &operand, &single_float_desc);
}

/* The following member function fulfills comparison of two IEEE double
   precision floating point numbers with the aid of functions `gt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is less than or equal to the second.  See commentaries
   about input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_double:: operator <= (IEEE_double_t &operand)
{
  current_status_bits = 0;
  return IEEE_le (this, &operand, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following member function fulfills comparison of two IEEE quad
   precision floating point numbers with the aid of functions `gt' and
   `order_comparison_special_cases'.  The function returns 1 if the
   first number is less than or equal to the second.  See commentaries
   about input and output exceptions for NaNs in function
   `order_comparison_special_cases'. */

int
IEEE_quad:: operator <= (IEEE_quad_t &operand)
{
  current_status_bits = 0;
  return IEEE_le (this, &operand, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions for transformation of floating point
   number of a format to another format. */

/* The following function fulfills conversion of IEEE floating point
   number FROM with charcteristics FROM_D to IEEE floating point
   number TO with charcteristics TO_D.  See commentaries about input
   exceptions for NaNs and output exceptions at the file begin.
   Actually no one output exceptions occur. */

static void
IEEE_transform (void *to, float_desc_t to_d, void *from, float_desc_t from_d)
{
  enum number_class from_class;
  int exception_mask;
  fraction_t fraction;
  int exponent;
  int sign;
  void (*saved_unsigned_overflow_reaction) (void);

  exception_mask = 0;
  from_class = float_class (from, from_d);
  if (from_class == TRAPPING_NOT_A_NUMBER)
    {
      NaN (to, to_d);
      exception_mask = IEEE_INV | IEEE_RO;
    }
  else if (from_class == NOT_A_NUMBER)
    {
      NaN (to, to_d);
      exception_mask = IEEE_RO;
    }
  else if (from_class == POSITIVE_INFINITY)
    positive_infinity (to, to_d);
  else if (from_class == NEGATIVE_INFINITY)
    negative_infinity (to, to_d);
  else
    {
      assert (from_class == NORMALIZED_NUMBER
	      || from_class == DENORMALIZED_NUMBER
              || from_class == POSITIVE_ZERO || from_class == NEGATIVE_ZERO);
      /* Not special cases operands */
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      unpack_float (from, from_d, &sign, &exponent, fraction);
      form_float (to, to_d, sign,
		  exponent - from_d->exponent_bias + to_d->exponent_bias,
		  fraction, 0, 0, saved_unsigned_overflow_reaction);
    }
  process_exception (exception_mask);
}

/* The following member function fulfills conversion of IEEE single
   precision floating point number to IEEE double precision floating
   point number.  The function returns IEEE double precision floating
   point number.  See commentaries about input exceptions for NaNs and
   output exceptions at the file begin.  Actually no one output
   exceptions occur. */

IEEE_double_t
IEEE_float::to_double (void)
{
  IEEE_double_t result;

  current_status_bits = 0;
  IEEE_transform (&result, &double_float_desc,
		  this, &single_float_desc);
  return result;
}

#ifdef IEEE_QUAD
/* The following member function fulfills conversion of IEEE single
   precision floating point number to IEEE quad precision floating
   point number.  The function returns IEEE quad precision floating
   point number.  See commentaries about input exceptions for NaNs and
   output exceptions at the file begin.  Actually no one output
   exceptions occur. */

IEEE_quad_t
IEEE_float::to_quad (void)
{
  IEEE_quad_t result;

  current_status_bits = 0;
  IEEE_transform (&result, &quad_float_desc,
		  this, &single_float_desc);
  return result;
}
#endif /* #ifdef IEEE_QUAD */

/* The following member function fulfills conversion of IEEE double
   precision floating point number to IEEE single precision floating
   point number.  The function returns IEEE single precision floating
   point number.  See commentaries about input exceptions for NaNs and
   output exceptions at the file begin. */

IEEE_float_t
IEEE_double::to_single (void)
{
  IEEE_float_t result;

  current_status_bits = 0;
  IEEE_transform (&result, &single_float_desc,
		  this, &double_float_desc);
  return result;
}

#ifdef IEEE_QUAD
/* The following member function fulfills conversion of IEEE quad
   precision floating point number to IEEE single precision floating
   point number.  The function returns IEEE single precision floating
   point number.  See commentaries about input exceptions for NaNs and
   output exceptions at the file begin. */

IEEE_float_t
IEEE_quad::to_single (void)
{
  IEEE_float_t result;

  current_status_bits = 0;
  IEEE_transform (&result, &single_float_desc,
		  this, &quad_float_desc);
  return result;
}

/* The following member function fulfills conversion of IEEE double
   precision floating point number to IEEE quad precision floating
   point number.  The function returns IEEE quad precision floating
   point number.  See commentaries about input exceptions for NaNs and
   output exceptions at the file begin.  Actually no one output
   exceptions occur. */

IEEE_quad_t
IEEE_double::to_quad (void)
{
  IEEE_quad_t result;

  current_status_bits = 0;
  IEEE_transform (&result, &quad_float_desc,
		  this, &double_float_desc);
  return result;
}

/* The following member function fulfills conversion of IEEE quad
   precision floating point number to IEEE double precision floating
   point number.  The function returns IEEE double precision floating
   point number.  See commentaries about input exceptions for NaNs and
   output exceptions at the file begin. */

IEEE_double_t
IEEE_quad::to_double (void)
{
  IEEE_double_t result;

  current_status_bits = 0;
  IEEE_transform (&result, &double_float_desc,
		  this, &quad_float_desc);
  return result;
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions for transformation of (unsigned)
   integer number to floating point number of single or double
   format. */

/* The following function fulfills conversion of unsigned integer to
   floating point number given as exponent and fraction.  The function
   returns 1, if the fraction inexactly represents given unsigned
   integer. */

static int
unsigned_integer_to_fraction
  (int unsigned_integer_size, const unsigned char *unsigned_integer,
   int *non_biased_exponent, fraction_t fraction)
{
  int current_byte;
  int first_nonzero_byte;
  int imprecise_flag;

  assert (unsigned_integer_size > 0);
  *non_biased_exponent = unsigned_integer_size * CHAR_BIT - 1;
  for (current_byte = 0; current_byte < unsigned_integer_size; current_byte++)
    if (unsigned_integer [current_byte] != 0)
      break;
  first_nonzero_byte = current_byte;
  if (first_nonzero_byte >= unsigned_integer_size)
    {
      /* Zero integer */
      memset (fraction, 0, (size_t) INTERNAL_FRACTION_SIZE);
      return 0; /* FALSE */
    }
  else if (unsigned_integer_size - first_nonzero_byte > INTERNAL_FRACTION_SIZE)
    {
      memcpy (fraction, unsigned_integer + first_nonzero_byte,
              (size_t) INTERNAL_FRACTION_SIZE);
      *non_biased_exponent -= first_nonzero_byte * CHAR_BIT;
      imprecise_flag
        = !bits::is_zero_bit_string
          (unsigned_integer + first_nonzero_byte + INTERNAL_FRACTION_SIZE, 0,
           (unsigned_integer_size - first_nonzero_byte
            - INTERNAL_FRACTION_SIZE) * CHAR_BIT);
    }
  else
    {
      imprecise_flag = 0 /* FALSE */;
      memcpy (fraction, unsigned_integer + first_nonzero_byte,
              (size_t) unsigned_integer_size - first_nonzero_byte);
      *non_biased_exponent -= first_nonzero_byte * CHAR_BIT;
      memset (fraction + unsigned_integer_size - first_nonzero_byte, 0,
              (size_t) INTERNAL_FRACTION_SIZE
              - (unsigned_integer_size - first_nonzero_byte));
    }
  return imprecise_flag;
}

/* The following function fulfills conversion of integer to IEEE
   floating point number FP with characteristics D with the aid of
   `unsigned_integer_to_fraction'.  See commentaries about output
   exceptions at the file begin.  No input exceptions occur. */

static void
IEEE_from_integer (void *fp, float_desc_t d, int size, const void *integer)
{
  int imprecise_flag;
  fraction_t fraction;
  int sign;
  int non_biased_exponent;
  void (*saved_integer_overflow_reaction) (void);
  void (*saved_unsigned_overflow_reaction) (void);

  assert (size > 0);
  saved_unsigned_overflow_reaction
    = unsigned_integer::set_overflow_reaction
      (integer::default_arithmetic_overflow_reaction);
  if (signed_integer::lt (size, integer, integer::zero_constant))
    {
      sign = 1;
      saved_integer_overflow_reaction
        = signed_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      /* May be overflow (for minimal integer) but the result as unsigned
         is correct. */
      signed_integer::subtract (size, integer::zero_constant, integer,
				(unsigned char *) integer);
    }
  else
    sign = 0;
  imprecise_flag
    = unsigned_integer_to_fraction (size, (unsigned char *) integer,
				    &non_biased_exponent, fraction);
  if (sign != 0)
    {
      signed_integer::subtract (size, integer::zero_constant, integer,
				(unsigned char *) integer);
      (void) signed_integer::set_overflow_reaction
	(saved_integer_overflow_reaction);
    }
  form_float (fp, d, sign, non_biased_exponent + d->exponent_bias, fraction,
	      0, imprecise_flag, saved_unsigned_overflow_reaction);
}

/* The following function fulfills conversion of unsigned integer to
   IEEE floating point number FP with characteristics D with the aid
   of `unsigned_integer_to_fraction'.  See commentaries about output
   exceptions at the file begin.  No input exceptions occur. */

static void
IEEE_from_unsigned_integer (void *fp, float_desc_t d,
			    int size, const void *unsigned_integer)
{
  int imprecise_flag;
  fraction_t fraction;
  int non_biased_exponent;
  void (*saved_unsigned_overflow_reaction) (void);

  assert (size > 0);
  saved_unsigned_overflow_reaction
    = unsigned_integer::set_overflow_reaction
      (integer::default_arithmetic_overflow_reaction);
  imprecise_flag
    = unsigned_integer_to_fraction (size,
				    (const unsigned char *) unsigned_integer,
                                    &non_biased_exponent, fraction);
  form_float (fp, d, 0, non_biased_exponent + d->exponent_bias,
	      fraction, 0, imprecise_flag, saved_unsigned_overflow_reaction);
}

/* The following member function fulfills conversion of integer to
   the IEEE single precision floating point number with the aid of
   `unsigned_integer_to_fraction'.  The function also returns
   reference for result of conversion.  See commentaries about output
   exceptions at the file begin.  No input exceptions occur. */

class IEEE_float &
IEEE_float::from_signed_integer (int size, const void *integer)
{
  current_status_bits = 0;
  IEEE_from_integer (this, &single_float_desc, size, integer);
  return *this;
}

/* The following member function fulfills conversion of unsigned
   integer to the IEEE single precision floating point number with the
   aid of `unsigned_integer_to_fraction'.  The function also returns
   reference for result of conversion.  See commentaries about output
   exceptions at the file begin.  No input exceptions occur. */

class IEEE_float &
IEEE_float::from_unsigned_integer (int size, const void *unsigned_integer)
{
  current_status_bits = 0;
  IEEE_from_unsigned_integer (this, &single_float_desc,
			      size, unsigned_integer);
  return *this;
}

/* The following member function fulfills conversion of integer to the
   IEEE double precision floating point number with the aid of
   `unsigned_integer_to_fraction'.  The function also returns
   reference of result of conversion.  See commentaries about output
   exceptions at the file begin.  No input exceptions occur. */

class IEEE_double &
IEEE_double::from_signed_integer (int size, const void *integer)
{
  current_status_bits = 0;
  IEEE_from_integer (this, &double_float_desc, size, integer);
  return *this;
}

/* The following member function fulfills conversion of unsigned
   integer to the IEEE double precision floating point number with the
   aid of `unsigned_integer_to_fraction'.  The function also returns
   reference of result of conversion.  See commentaries about output
   exceptions at the file begin.  No input exceptions occur. */

class IEEE_double &
IEEE_double::from_unsigned_integer (int size, const void *unsigned_integer)
{
  current_status_bits = 0;
  IEEE_from_unsigned_integer (this, &double_float_desc,
			      size, unsigned_integer);
  return *this;
}

#ifdef IEEE_QUAD
/* The following member function fulfills conversion of integer to the
   IEEE quad precision floating point number with the aid of
   `unsigned_integer_to_fraction'.  The function also returns
   reference of result of conversion.  See commentaries about output
   exceptions at the file begin.  No input exceptions occur. */

class IEEE_quad &
IEEE_quad::from_signed_integer (int size, const void *integer)
{
  current_status_bits = 0;
  IEEE_from_integer (this, &quad_float_desc, size, integer);
  return *this;
}

/* The following member function fulfills conversion of unsigned
   integer to the IEEE quad precision floating point number with the
   aid of `unsigned_integer_to_fraction'.  The function also returns
   reference of result of conversion.  See commentaries about output
   exceptions at the file begin.  No input exceptions occur. */

class IEEE_quad &
IEEE_quad::from_unsigned_integer (int size, const void *unsigned_integer)
{
  current_status_bits = 0;
  IEEE_from_unsigned_integer (this, &quad_float_desc,
			      size, unsigned_integer);
  return *this;
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains functions for transformation of floating point
   number of single or double format to (unsigned) integer number. */

/* The following function fulfills conversion of floating point number
   given by its sign, exponent, and fraction to integer.  The function
   returns mask of exceptions occurred. */

static int
fraction_to_integer (int sign, int non_biased_exponent, fraction_t fraction,
                     unsigned char *integer, int integer_size)
{
  int exception_mask;
  int shift_right;
  int round_bit;
  int round_to_nearest_special_case;
  fraction_t one;
  void (*saved_integer_overflow_reaction) (void);

  assert (integer_size > 0);
  exception_mask = 0;
  shift_right = INTERNAL_FRACTION_SIZE * CHAR_BIT - 1 - non_biased_exponent;
  if (shift_right > 0)
    {
      if (shift_right > INTERNAL_FRACTION_SIZE * CHAR_BIT)
        round_bit = 0;
      else
        {
          round_bit
            = bits::bit (fraction,
                         INTERNAL_FRACTION_SIZE * CHAR_BIT - shift_right);
          round_to_nearest_special_case
            = shift_right == 1
              || (bits::is_zero_bit_string
                  (fraction,
                   INTERNAL_FRACTION_SIZE * CHAR_BIT - shift_right + 1,
                   shift_right - 1));
        }
      unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction,
                                     shift_right, fraction);
      exception_mask = (integer::overflow_bit != 0 ? IEEE_IMP : 0);
      (void) unsigned_integer::from_string (INTERNAL_FRACTION_SIZE, "1", one);
      /* Rounding */
      if (current_round_mode == IEEE_RN)
        {
          if (round_bit
              && (!round_to_nearest_special_case
                  || bits::bit (fraction,
                                INTERNAL_FRACTION_SIZE * CHAR_BIT - 1)))
            unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction, one,
                                   fraction);
          assert (integer::overflow_bit == 0);
        }
      else if (current_round_mode == IEEE_RM)
        {
          if (exception_mask != 0 && sign)
            /* Imprecise */
            unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction, one,
                                   fraction);
          assert (integer::overflow_bit == 0);
        }
      else if (current_round_mode == IEEE_RP)
        {
          if (exception_mask != 0 && !sign)
            /* Imprecise */
            unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction, one,
                                   fraction);
          assert (integer::overflow_bit == 0);
        }
      else
        assert (current_round_mode == IEEE_RZ);
    }
  else
    assert (bits::bit (fraction, 0) != 0);
  unsigned_integer::change_size (INTERNAL_FRACTION_SIZE, fraction,
                                 integer_size, integer);
  if (integer::overflow_bit != 0)
    /* Overflow */
    exception_mask = IEEE_INV;
  if (shift_right < 0)
    {
      unsigned_integer::shift_left (integer_size, integer, -shift_right,
                                    integer);
      if (integer::overflow_bit != 0)
        /* Overflow */
        exception_mask = IEEE_INV;
    }
  if (sign)
    {
      saved_integer_overflow_reaction
        = signed_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      signed_integer::subtract (integer_size, integer::zero_constant,
                                integer, integer);
      if (integer::overflow_bit != 0)
        /* Overflow */
        exception_mask = IEEE_INV;
      (void) signed_integer::set_overflow_reaction
        (saved_integer_overflow_reaction);
    }
  if ((exception_mask & IEEE_INV) != 0)
    {
      if (sign)
        signed_integer::minimum (integer_size, integer);
      else
        signed_integer::maximum (integer_size, integer);
    }
  return exception_mask;
}

/* The following function fulfills conversion of floating point number
   given by its class, exponent and fraction to integer number.
   Results and input exceptions for operand of special cases values
   (and for NaNs) are described by the following table

            Operand     | Result & Exception
          --------------|-------------------
              SNaN      |     0  
                        |IEEE_INV(_RO)
          --------------|-------------------
              QNaN      |     0    
                        |IEEE_INV(_RO)     
          --------------|-------------------
              +Inf      |    IMax     
                        |  IEEE_INV     
          --------------|-------------------
              -Inf      |    IMin     
                        |  IEEE_INV     
          --------------|-------------------
              Others    |             
                        |               

   The function returns mask of exceptions occurred.  Results and
   exceptions for NaNs are differed from the ones of operations
   described at the file begin. */

static void
float_to_integer (int number_class, int sign, int non_biased_exponent,
                  fraction_t fraction, void *integer, int size)
{
  int exception_mask;
  void (*saved_unsigned_overflow_reaction) (void);

  assert (size > 0);
  exception_mask = 0;
  if (number_class == TRAPPING_NOT_A_NUMBER)
    {
      (void) unsigned_integer::from_string (size, "0", integer);
      exception_mask = IEEE_INV | IEEE_RO;
    }
  else if (number_class == NOT_A_NUMBER)
    {
      (void) unsigned_integer::from_string (size, "0", integer);
      exception_mask = IEEE_INV | IEEE_RO;
    }
  else if (number_class == POSITIVE_INFINITY)
    {
      signed_integer::maximum (size, integer);
      exception_mask = IEEE_INV;
    }
  else if (number_class == NEGATIVE_INFINITY)
    {
      signed_integer::minimum (size, integer);
      exception_mask = IEEE_INV;
    }
  else if (number_class == POSITIVE_ZERO
           || number_class == NEGATIVE_ZERO)
    (void) unsigned_integer::from_string (size, "0", integer);
  else
    {
      assert (number_class == NORMALIZED_NUMBER
              || number_class == DENORMALIZED_NUMBER);
      /* Not special cases operands */
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      exception_mask = fraction_to_integer (sign, non_biased_exponent,
                                            fraction,
                                            (unsigned char *) integer, size);
      (void) unsigned_integer::set_overflow_reaction
        (saved_unsigned_overflow_reaction);
    }
  process_exception (exception_mask);
}

/* The following function transforms IEEE floating point number FP
   with characteristics D to integer with the aid of function
   `float_to_integer'.  See commentaries about input exceptions in
   function `float_to_integer' and output exceptions at the file
   begin. */

static void
IEEE_to_integer (int size, void *fp, float_desc_t d, void *integer)
{
  enum number_class fp_class;
  fraction_t fraction;
  int exponent;
  int sign;
  
  assert (size > 0);
  fp_class = float_class (fp, d);
  unpack_float (fp, d, &sign, &exponent, fraction);
  float_to_integer (fp_class, sign, exponent - d->exponent_bias,
                    fraction, integer, size);
}

/* The following member function transforms theIEEE single precision
   floating point number to integer with the aid of function
   `float_to_integer'.  See commentaries about input exceptions in
   function `float_to_integer' and output exceptions at the file
   begin. */

void
IEEE_float::to_signed_integer (int size, void *integer)
{
  current_status_bits = 0;
  IEEE_to_integer (size, this, &single_float_desc, integer);
}

/* The following member function transforms the IEEE double precision
   floating point number to integer with the aid of function
   `float_to_integer'.  See commentaries about input exceptions in
   function `float_to_integer' and output exceptions at the file
   begin. */

void
IEEE_double::to_signed_integer (int size, void *integer)
{
  current_status_bits = 0;
  IEEE_to_integer (size, this, &double_float_desc, integer);
}

#ifdef IEEE_QUAD
/* The following member function transforms the IEEE quad precision
   floating point number to integer with the aid of function
   `float_to_integer'.  See commentaries about input exceptions in
   function `float_to_integer' and output exceptions at the file
   begin. */

void
IEEE_quad::to_signed_integer (int size, void *integer)
{
  current_status_bits = 0;
  IEEE_to_integer (size, this, &quad_float_desc, integer);
}
#endif /* #ifdef IEEE_QUAD */

/* The following function fulfills conversion of floating point number
   given by its exponent and fraction to unsigned integer.  The
   function returns mask of exceptions occurred. */

static int
fraction_to_unsigned_integer (int non_biased_exponent, fraction_t fraction,
                              unsigned char *unsigned_integer,
                              int unsigned_integer_size)
{
  current_status_bits = 0;
  int exception_mask;
  int shift_right;
  int round_bit;
  int round_to_nearest_special_case;
  fraction_t one;

  assert (unsigned_integer_size > 0);
  exception_mask = 0;
  shift_right = INTERNAL_FRACTION_SIZE * CHAR_BIT - 1 - non_biased_exponent;
  if (shift_right > 0)
    {
      if (shift_right > INTERNAL_FRACTION_SIZE * CHAR_BIT)
        round_bit = 0;
      else
        {
          round_bit
            = bits::bit (fraction,
                         INTERNAL_FRACTION_SIZE * CHAR_BIT - shift_right);
          round_to_nearest_special_case
            = shift_right == 1
              || (bits::is_zero_bit_string
                  (fraction,
                   INTERNAL_FRACTION_SIZE * CHAR_BIT - shift_right + 1,
                   shift_right - 1));
        }
      unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction,
                                     shift_right, fraction);
      exception_mask = (integer::overflow_bit != 0 ? IEEE_IMP : 0);
      (void) unsigned_integer::from_string (INTERNAL_FRACTION_SIZE, "1", one);
      /* Rounding */
      if (current_round_mode == IEEE_RN)
        {
          if (round_bit
              && (!round_to_nearest_special_case
                  || bits::bit (fraction,
                                INTERNAL_FRACTION_SIZE * CHAR_BIT - 1)))
            unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction, one,
                                   fraction);
          assert (integer::overflow_bit == 0);
        }
      else if (current_round_mode == IEEE_RP)
        {
          if (exception_mask != 0)
            /* Imprecise */
            unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction, one,
                                   fraction);
          assert (integer::overflow_bit == 0);
        }
      else
        assert (current_round_mode == IEEE_RM
                || current_round_mode == IEEE_RZ);
    }
  else
    assert (bits::bit (fraction, 0) != 0);
  unsigned_integer::change_size (INTERNAL_FRACTION_SIZE, fraction,
                                 unsigned_integer_size, unsigned_integer);
  if (integer::overflow_bit != 0)
    /* Overflow */
    exception_mask = IEEE_INV;
  if (shift_right < 0)
    {
      unsigned_integer::shift_left (unsigned_integer_size, unsigned_integer,
                                    -shift_right, unsigned_integer);
      if (integer::overflow_bit != 0)
        /* Overflow */
        exception_mask = IEEE_INV;
    }
  if ((exception_mask & IEEE_INV) != 0)
    unsigned_integer::maximum (unsigned_integer_size, unsigned_integer);
  return exception_mask;
}

/* The following function fulfills conversion of floating point number
   given by its class, exponent and fraction to unsigned integer
   number.  Results and input exceptions for operand of special cases
   values (and for NaNs) are described by the following table

            Operand     | Result & Exception
          --------------|-------------------
              SNaN      |     0  
                        |IEEE_INV(_RO)
          --------------|-------------------
              QNaN      |     0    
                        |IEEE_INV(_RO)     
          --------------|-------------------
              +Inf      |    IMax     
                        |  IEEE_INV     
          --------------|-------------------
              -Inf  or  |    0     
         negative number|  IEEE_INV     
          --------------|-------------------
              Others    |             
                        |               

   The function returns mask of exceptions occurred.  Results and
   exceptions for NaNs are differed from the ones of operations
   described at the file begin. */

static void
float_to_unsigned_integer (int number_class, int sign, int non_biased_exponent,
                           fraction_t fraction,
                           void *unsigned_integer, int size)
{
  int exception_mask;
  void (*saved_unsigned_overflow_reaction) (void);

  assert (size > 0);
  exception_mask = 0;
  if (number_class == TRAPPING_NOT_A_NUMBER)
    {
      (void) unsigned_integer::from_string (size, "0", unsigned_integer);
      exception_mask = IEEE_INV | IEEE_RO;
    }
  else if (number_class == NOT_A_NUMBER)
    {
      (void) unsigned_integer::from_string (size, "0", unsigned_integer);
      exception_mask = IEEE_INV | IEEE_RO;
    }
  else if (number_class == POSITIVE_INFINITY)
    {
      unsigned_integer::maximum (size, unsigned_integer);
      exception_mask = IEEE_INV;
    }
  else if (number_class == NEGATIVE_INFINITY)
    {
      (void) unsigned_integer::from_string (size, "0", unsigned_integer);
      exception_mask = IEEE_INV;
    }
  else if (number_class == POSITIVE_ZERO
           || number_class == NEGATIVE_ZERO)
    (void) unsigned_integer::from_string (size, "0", unsigned_integer);
  else
    {
      assert (number_class == NORMALIZED_NUMBER
              || number_class == DENORMALIZED_NUMBER);
      /* Not special cases operands */
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      if (sign)
        {
          (void) unsigned_integer::from_string (size, "0", unsigned_integer);
          exception_mask = IEEE_INV;
        }
      else
        exception_mask
          = fraction_to_unsigned_integer
            (non_biased_exponent, fraction,
             (unsigned char *) unsigned_integer, size);
      (void) unsigned_integer::set_overflow_reaction
        (saved_unsigned_overflow_reaction);
    }
  process_exception (exception_mask);
}

/* The following function transforms IEEE floating point number FP
   with characteristics D to unsigned integer with the aid of function
   `float_to_unsigned_integer'.  See commentaries about input
   exceptions in function `float_to_unsigned_integer' and output
   exceptions at the file begin. */

static void
IEEE_to_unsigned_integer (int size, void *fp, float_desc_t d,
			  void *unsigned_integer)
{
  enum number_class fp_class;
  fraction_t fraction;
  int exponent;
  int sign;

  assert (size > 0);
  fp_class = float_class (fp, d);
  unpack_float (fp, d, &sign, &exponent, fraction);
  float_to_unsigned_integer (fp_class, sign,
                             exponent - d->exponent_bias, fraction,
                             unsigned_integer, size);
}

/* The following member function transforms the IEEE single precision
   floating point number to unsigned integer with the aid of function
   `float_to_unsigned_integer'.  See commentaries about input
   exceptions in function `float_to_unsigned_integer' and output
   exceptions at the file begin. */

void
IEEE_float::to_unsigned_integer (int size, void *unsigned_integer)
{
  current_status_bits = 0;
  IEEE_to_unsigned_integer (size, this, &single_float_desc,
			    unsigned_integer);
}

/* The following member function transforms the IEEE double precision
   floating point number to unsigned integer with the aid of function
   `float_to_unsigned_integer'.  See commentaries about input
   exceptions in function `float_to_unsigned_integer' and output
   exceptions at the file begin. */

void
IEEE_double::to_unsigned_integer (int size, void *unsigned_integer)
{
  current_status_bits = 0;
  IEEE_to_unsigned_integer (size, this, &double_float_desc,
			    unsigned_integer);
}

#ifdef IEEE_QUAD
/* The following member function transforms the IEEE quad precision
   floating point number to unsigned integer with the aid of function
   `float_to_unsigned_integer'.  See commentaries about input
   exceptions in function `float_to_unsigned_integer' and output
   exceptions at the file begin. */

void
IEEE_quad::to_unsigned_integer (int size, void *unsigned_integer)
{
  current_status_bits = 0;
  IEEE_to_unsigned_integer (size, this, &quad_float_desc,
			    unsigned_integer);
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains function for transformation of IEEE floating
   point numbers to ascii binary strings. */

/* The following function transforms IEEE floating point number given
   by its sign, exponent, and fraction to binary (in a given base)
   ascii representation with obligatory integer part (1 digit),
   optional fractional part, and optional binary exponent.  Signs
   minus are present if it is needed.  The function returns result.
   Current round mode does not affect the resultant ascii
   representation.  BASE should be 2, 4, 8, or 16. */

static char *
float_to_binary_string (int non_biased_exponent, int sign, fraction_t fraction,
			int true_fraction_bit_length, int base, char *result)
{
  char *result_without_sign;
  int number_of_significant_digits;
  int i, digit_bits;
  void (*saved_unsigned_overflow_reaction) (void);
  char fraction_str [INTERNAL_FRACTION_SIZE * CHAR_BIT + 1];

  assert (base == 2 || base == 4 || base == 8 || base == 16);
  digit_bits = (base == 2 ? 1 : base == 4 ? 2 : base == 8 ? 3 : 4);
  saved_unsigned_overflow_reaction
    = unsigned_integer::set_overflow_reaction
      (integer::default_arithmetic_overflow_reaction);
  normalize_fraction (&non_biased_exponent, fraction);
  if (sign)
    {
      strcpy (result, "-");
      result_without_sign = result + 1;
    }
  else
    result_without_sign = result;
  result_without_sign++; /* For point */
  unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction,
				digit_bits - 1, fraction);
  unsigned_integer::to_based_string (INTERNAL_FRACTION_SIZE, fraction,
				     base, fraction_str);
  number_of_significant_digits = 0;
  for (i = 0; fraction_str [i] != '\0'; i++)
    if (fraction_str [i] != '0')
      number_of_significant_digits = i;
  number_of_significant_digits++;
  strncpy (result_without_sign, fraction_str, number_of_significant_digits);
  result_without_sign [-1] = *result_without_sign;
  *result_without_sign = '.';
  result_without_sign [number_of_significant_digits] = '\0';
  assert (number_of_significant_digits != 1
	  || result_without_sign [-1] != '0');
  if (non_biased_exponent != 0)
    sprintf (result_without_sign + number_of_significant_digits,
             "p%d", non_biased_exponent);
  (void) unsigned_integer::set_overflow_reaction
    (saved_unsigned_overflow_reaction);
  return result;
}

/* The following function transforms IEEE number FP with
   characteristics D to decimal ascii representation with obligatory
   integer part (1 digit), optional fractional part, and optional
   binary exponent with the aid of function `float_to_binary_string'.
   Signs minus are present if it is needed.  The special cases IEEE
   floating point values are represented by strings `SNaN', `QNaN',
   `+Inf', `-Inf', `+0', and `-0'.  The function returns the result.
   Current round mode does not affect the resultant ascii
   representation.  The function produce no input or output
   exceptions.  BASE should be 2, 4, 8, or 16. */

static char *
IEEE_to_binary_string (void *fp, float_desc_t d, int base, char *result)
{
  enum number_class fp_class;
  fraction_t fraction;
  int sign;
  int exponent;

  fp_class = float_class (fp, d);
  if (fp_class == TRAPPING_NOT_A_NUMBER)
    return strcpy (result, TRAPPING_NaN_NAME);
  else if (fp_class == NOT_A_NUMBER)
    return strcpy (result, NaN_NAME);
  else if (fp_class == POSITIVE_INFINITY)
    return strcpy (result, POSITIVE_INFINITY_NAME);
  else if (fp_class == NEGATIVE_INFINITY)
    return strcpy (result, NEGATIVE_INFINITY_NAME);
  else if (fp_class == POSITIVE_ZERO)
    return strcpy (result, "+0");
  else if (fp_class == NEGATIVE_ZERO)
    return strcpy (result, "-0");
  else
    {
      assert (fp_class == NORMALIZED_NUMBER
              || fp_class == DENORMALIZED_NUMBER);
      unpack_float (fp, d, &sign, &exponent, fraction);
      return
        float_to_binary_string (exponent - d->exponent_bias, sign, fraction,
				d->true_fraction_bit_length, base, result);
    }
}

/* The following function transforms IEEE single precision to binary
   ascii representation with obligatory integer part (1 digit),
   optional fractional part, and optional binary exponent with the aid
   of function `float_to_binary_string'.  Signs minus are present if
   it is needed.  The special cases IEEE floating point values are
   represented by strings `SNaN', `QNaN', `+Inf', `-Inf', `+0', and
   `-0'.  The function returns the result.  Current round mode does
   not affect the resultant ascii representation.  The function
   produce no input or output exceptions.  BASE should be 2, 4, 8, or
   16. */

char *
IEEE_float::to_binary_string (int base, char *result)
{
  current_status_bits = 0;
  return IEEE_to_binary_string (this, &single_float_desc, base, result);
}

/* The following function transforms IEEE double precision to binary
   ascii representation with obligatory integer part (1 digit),
   optional fractional part, and optional binary_exponent with the aid
   of function `float_to_binary_string'.  Signs minus are present if
   it is needed.  The special cases IEEE floating point values are
   represented by strings `SNaN', `QNaN', `+Inf', `-Inf', `+0', and
   `-0'.  The function returns the result.  Current round mode does
   not affect the resultant ascii representation. The function produce
   no input or output exceptions.  BASE should be 2, 4, 8, or 16. */

char *
IEEE_double::to_binary_string (int base, char *result)
{
  current_status_bits = 0;
  return IEEE_to_binary_string (this, &double_float_desc, base, result);
}

#ifdef IEEE_QUAD
/* The following function transforms IEEE quad precision to binary
   ascii representation with obligatory integer part (1 digit),
   optional fractional part, and optional exponent with the aid of
   function `float_to_binary_string'.  Signs minus are present if it
   is needed.  The special cases IEEE floating point values are
   represented by strings `SNaN', `QNaN', `+Inf', `-Inf', `+0', and
   `-0'.  The function returns the result.  Current round mode does
   not affect the resultant ascii representation.  The function
   produce no input or output exceptions.  BASE should be 2, 4, 8, or
   16. */

char *
IEEE_quad::to_binary_string (int base, char *result)
{
  current_status_bits = 0;
  return IEEE_to_binary_string (this, &quad_float_desc, base, result);
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains function for transformation of IEEE floating
   point numbers to strings. */

/* The following function transforms IEEE floating point number given
   by its sign, exponent, and fraction to decimal ascii representation
   with obligatory integer part (1 digit), fractional part (of
   constant length), and optional exponent.  Signs minus are present
   if it is needed.  The function returns result.  Current round mode
   does not affect the resultant ascii representation.  The function
   uses the most precise floating point (more precise than IEEE double
   floating point format) values of non negative powers of ten
   generated with the aid of POSIX arbitrary precision calculator `bc'
   because calculation of the powers of ten by conventional way (by
   multiplication of IEEE floating point numbers can result in
   rounding imprecises). */

static char *
float_to_string (int non_biased_exponent, int sign, fraction_t fraction,
                 int true_fraction_bit_length, char *result)
{
  char *result_without_sign;
  int ten_power;
  int ten_power_exponent;
  int ten_power_fraction_imprecise_flag;
  fraction_t ten_fraction;
  int number_of_significant_digits;
  int shift_right;
  char fraction_str [INTERNAL_FRACTION_SIZE * CHAR_BIT + 1];
  void (*saved_unsigned_overflow_reaction) (void);

  saved_unsigned_overflow_reaction
    = unsigned_integer::set_overflow_reaction
      (integer::default_arithmetic_overflow_reaction);
  normalize_fraction (&non_biased_exponent, fraction);
  if (sign)
    {
      strcpy (result, "-");
      result_without_sign = result + 1;
    }
  else
    result_without_sign = result;
  result_without_sign++; /* For point */
  number_of_significant_digits
    = (int) (LOG_OF_2 * true_fraction_bit_length + 2.0);
  /* Evaluate more decimal digits than `number_of_significant_digits' for
     subsequent rounding. */
  ten_power = (int) ((non_biased_exponent - true_fraction_bit_length
                      - MINIMAL_BITS_FOR_HUNDRED) * LOG_OF_2);
  /* In order to the fraction with exponent near zero has nonpositive
     string representation length > number_of_significant_digits. */
  if (non_biased_exponent - true_fraction_bit_length - MINIMAL_BITS_FOR_HUNDRED
      < 0)
    ten_power--;
#ifndef NDEBUG
  assert (get_ten_power
          ((ten_power < 0 ? -ten_power : ten_power), ten_fraction,
           &ten_power_exponent, &ten_power_fraction_imprecise_flag));
#else
  (void) get_ten_power ((ten_power < 0 ? -ten_power : ten_power), ten_fraction,
                        &ten_power_exponent,
                        &ten_power_fraction_imprecise_flag);
#endif
  if (ten_power > 0)
    {
      /* Fraction with exponent near zero is evaluated by division. */
      /* Shift on true_fraction_bit_length + MINIMAL_BITS_FOR_HUNDRED
         for calculation of true_fraction_bit_length + MINIMAL_BITS_FOR_HUNDRED
         bits. */
      unsigned_integer::shift_right
        (INTERNAL_FRACTION_SIZE, ten_fraction,
         true_fraction_bit_length + MINIMAL_BITS_FOR_HUNDRED, ten_fraction);
      unsigned_integer::divide (INTERNAL_FRACTION_SIZE, fraction,
                                ten_fraction, fraction);
      assert (!integer::overflow_bit);
      non_biased_exponent = non_biased_exponent - ten_power_exponent;
      shift_right = (true_fraction_bit_length + MINIMAL_BITS_FOR_HUNDRED
                     - non_biased_exponent);
      assert (shift_right <= 0);
    }
  else
    {
      /* Fraction with exponent near zero is evaluated by multiplication. */
      unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, ten_fraction,
                                     INTERNAL_FRACTION_SIZE * CHAR_BIT / 2,
                                     ten_fraction);
      unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction,
                                     INTERNAL_FRACTION_SIZE * CHAR_BIT / 2,
                                     fraction);
      assert (!integer::overflow_bit);
      unsigned_integer::multiply (INTERNAL_FRACTION_SIZE, fraction,
                                  ten_fraction, fraction);
      assert (!integer::overflow_bit);
      non_biased_exponent = non_biased_exponent + ten_power_exponent;
      shift_right
        = INTERNAL_FRACTION_SIZE * CHAR_BIT - 2 - non_biased_exponent;
    }
  unsigned_integer::shift_right (INTERNAL_FRACTION_SIZE, fraction, shift_right,
                                 fraction);
  unsigned_integer::to_string (INTERNAL_FRACTION_SIZE, fraction,
                               fraction_str);
  ten_power += strlen (fraction_str) - 1;
  assert (strlen (fraction_str) > (unsigned) number_of_significant_digits
          && (strlen (fraction_str)
              <= (unsigned) number_of_significant_digits + 3));
  if (fraction_str [number_of_significant_digits] >= '5')
    {
      int digit_number;
      
      /* Rounding */
      for (digit_number = number_of_significant_digits - 1;
           digit_number >=0 && fraction_str [digit_number] == '9';
           digit_number--)
        fraction_str [digit_number] = '0';
      if (digit_number >= 0)
        fraction_str [digit_number]++;
      else
        {
          *fraction_str = '1';
          ten_power++;
        }
    }
  fraction_str [number_of_significant_digits] = '\0';
  strcpy (result_without_sign, fraction_str);
  /* Place point */
  result_without_sign [-1] = *result_without_sign;
  *result_without_sign = '.';
  if (ten_power != 0)
    sprintf (result_without_sign + number_of_significant_digits,
             "e%d", ten_power);
  (void) unsigned_integer::set_overflow_reaction
    (saved_unsigned_overflow_reaction);
  return result;
}

/* The following function transforms IEEE number FP with
   characteristics D to decimal ascii representation with obligatory
   integer part (1 digit), fractional part (of constant length), and
   optional exponent with the aid of function `float_to_string'.
   Signs minus are present if it is needed.  The special cases IEEE
   floating point values are represented by strings `SNaN', `QNaN',
   `+Inf', `-Inf', `+0', and `-0'.  The function returns the result.
   Current round mode does not affect the resultant ascii
   representation.  The function produce no input or output
   exceptions. */

static char *
IEEE_to_string (void *fp, float_desc_t d, char *result)
{
  enum number_class fp_class;
  fraction_t fraction;
  int sign;
  int exponent;

  fp_class = float_class (fp, d);
  if (fp_class == TRAPPING_NOT_A_NUMBER)
    return strcpy (result, TRAPPING_NaN_NAME);
  else if (fp_class == NOT_A_NUMBER)
    return strcpy (result, NaN_NAME);
  else if (fp_class == POSITIVE_INFINITY)
    return strcpy (result, POSITIVE_INFINITY_NAME);
  else if (fp_class == NEGATIVE_INFINITY)
    return strcpy (result, NEGATIVE_INFINITY_NAME);
  else if (fp_class == POSITIVE_ZERO)
    return strcpy (result, "+0");
  else if (fp_class == NEGATIVE_ZERO)
    return strcpy (result, "-0");
  else
    {
      assert (fp_class == NORMALIZED_NUMBER
              || fp_class == DENORMALIZED_NUMBER);
      unpack_float (fp, d, &sign, &exponent, fraction);
      return
        float_to_string (exponent - d->exponent_bias, sign, fraction,
                         d->true_fraction_bit_length, result);
    }
}

/* The following member function transforms the IEEE single precision
   to decimal ascii representation with obligatory integer part (1
   digit), fractional part (of constant length), and optional exponent
   with the aid of function `float_to_string'.  Signs minus are
   present if it is needed.  The special cases IEEE floating point
   values are represented by strings `SNaN', `QNaN', `+Inf', `-Inf',
   `+0', and `-0'.  The function returns the result.  Current round
   mode does not affect the resultant ascii representation.  The
   function outputs 9 decimal fraction digits.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used.  But the reverse
   transformation `string with 9 digits -> IEEE floating point number
   -> string' may results in different digits of the fractions in
   ascii representation because a floating point number may represent
   several such strings with differences in the least significant
   digit.  But the ascii representation are identical when function
   `IEEE_float::from_string' does not fix imprecise result exception
   or less than 9 digits of the fractions in the ascii representations
   are compared.  The function produce no input or output
   exceptions. */

char *
IEEE_float::to_string (char *result)
{
  current_status_bits = 0;
  return IEEE_to_string (this, &single_float_desc, result);
}

/* The following member function transforms the IEEE double precision
   to decimal ascii representation with obligatory integer part (1
   digit), fractional part (of constant length), and optional exponent
   with the aid of function `float_to_string'.  Signs minus are
   present if it is needed.  The special cases IEEE floating point
   values are represented by strings `SNaN', `QNaN', `+Inf', `-Inf',
   `+0', and `-0'.  The function returns the result.  Current round
   mode does not affect the resultant ascii representation.  The
   function outputs 17 decimal fraction digits.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used.  But the reverse
   transformation `string with 17 digits -> IEEE floating point number
   -> string' may results in different digits of the fractions in
   ascii representation because a floating point number may represent
   several such strings with differences in the least significant
   digit.  But the ascii representation are identical when function
   `IEEE_double::from_string' does not fix imprecise result exception
   or less than 17 digits of the fractions in the ascii
   representations are compared.  The function produce no input or
   output exceptions. */

char *
IEEE_double::to_string (char *result)
{
  current_status_bits = 0;
  return IEEE_to_string (this, &double_float_desc, result);
}

#ifdef IEEE_QUAD
/* The following member function transforms the IEEE quad precision
   to decimal ascii representation with obligatory integer part (1
   digit), fractional part (of constant length), and optional exponent
   with the aid of function `float_to_string'.  Signs minus are
   present if it is needed.  The special cases IEEE floating point
   values are represented by strings `SNaN', `QNaN', `+Inf', `-Inf',
   `+0', and `-0'.  The function returns the result.  Current round
   mode does not affect the resultant ascii representation.  The
   function outputs 36 decimal fraction digits.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used.  But the reverse
   transformation `string with 36 digits -> IEEE floating point number
   -> string' may results in different digits of the fractions in
   ascii representation because a floating point number may represent
   several such strings with differences in the least significant
   digit.  But the ascii representation are identical when function
   `IEEE_quad::from_string' does not fix imprecise result exception
   or less than 36 digits of the fractions in the ascii
   representations are compared.  The function produce no input or
   output exceptions. */

char *
IEEE_quad::to_string (char *result)
{
  current_status_bits = 0;
  return IEEE_to_string (this, &quad_float_desc, result);
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains function for transformation of binary ascii
   strings to IEEE floating point numbers. */

/* The following function transforms binary ascii representation to
   IEEE floating point number given by its sign, binary exponent, and
   fraction.  The floating point number must correspond the following
   syntax

            ['+' | '-'] [<digits>] [ '.' [<digits>] ]
                  [ ('p' | 'P') ['+' | '-'] <decimal digits>]

   The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  Current round mode may affect resultant
   floating point number.  Digit should be less BASE which in its turn
   should be 2, 4, 8, or 16. */

static char *
binary_string_to_float (const char *operand, int base, int *sign,
			int *non_biased_exponent, fraction_t fraction,
			int *imprecise_flag)
{
  int leading_zero_flag;
  int number_of_rest_significant_digits;
  int binary_exponent;
  int additional_binary_exponent;
  const char *exponent_start;
  int exponent_minus_flag;
  int digit_bits;
  fraction_t temporary_fraction, factor;
  char digit_string [2];

  assert (base == 2 || base == 4 || base == 8 || base == 16);
  *imprecise_flag = 0 /* FALSE */;
  (void) unsigned_integer::from_string (INTERNAL_FRACTION_SIZE, "0", fraction);
  (void) unsigned_integer::from_based_string (INTERNAL_FRACTION_SIZE, "10",
					      base, factor);
  digit_bits = (base == 2 ? 1 : base == 4 ? 2 : base == 8 ? 3 : 4);
  number_of_rest_significant_digits
    = (int) ((INTERNAL_FRACTION_SIZE * CHAR_BIT / 2.0 * digit_bits) + 2.0);
  /* The following statement for subsequent rounding */
  number_of_rest_significant_digits++;
  if (*operand == '-' || *operand == '+')
    {
      *sign = *operand == '-';
      operand++;
    }
  else
    *sign = 0;
  leading_zero_flag = 1 /* TRUE */;
  binary_exponent = INTERNAL_FRACTION_SIZE * CHAR_BIT - 1;
  while ((isdigit (*operand) && *operand - '0' < base)
	 || (base == 16 && ((*operand >= 'a' && *operand <= 'f')
			    || (*operand >= 'A' && *operand <= 'F'))))
    {
      if (*operand != '0' || !leading_zero_flag)
        {
          if (number_of_rest_significant_digits > 0)
            {
              number_of_rest_significant_digits--;
              unsigned_integer::multiply
		(INTERNAL_FRACTION_SIZE, fraction, factor, fraction);
              assert (!integer::overflow_bit);
              *digit_string = *operand;
              digit_string [1] = '\0';
              (void) unsigned_integer::from_based_string
		(INTERNAL_FRACTION_SIZE, digit_string,
		 base, temporary_fraction);
              unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction,
				     temporary_fraction, fraction);
              assert (!integer::overflow_bit);
            }
          else
            {
              *imprecise_flag = *imprecise_flag || *operand != '0';
              binary_exponent += digit_bits;
            }
          leading_zero_flag = 0 /* FALSE */;
        }
      operand++;
    }
  if (*operand == '.')
    {
      operand++;
      while ((isdigit (*operand) && *operand - '0' < base)
	     || (base == 16 && ((*operand >= 'a' && *operand <= 'f')
				|| (*operand >= 'A' && *operand <= 'F'))))
        {
          if (*operand != '0' || !leading_zero_flag)
            {
              if (number_of_rest_significant_digits > 0)
                {
                  number_of_rest_significant_digits--;
                  unsigned_integer::multiply (INTERNAL_FRACTION_SIZE,
					      fraction, factor, fraction);
                  assert (!integer::overflow_bit);
                  *digit_string = *operand;
                  digit_string [1] = '\0';
                  (void) unsigned_integer::from_based_string
		    (INTERNAL_FRACTION_SIZE, digit_string,
		     base, temporary_fraction);
                  unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction,
					 temporary_fraction, fraction);
                  assert (!integer::overflow_bit);
		  binary_exponent -= digit_bits;
                }
              else
                *imprecise_flag = *imprecise_flag || *operand != '0';
              leading_zero_flag = 0 /* FALSE */;
            }
          else
	    binary_exponent -= digit_bits;
          operand++;
        }
    }
  if (*operand == 'p' || *operand == 'P')
    {
      exponent_start = operand;
      operand++;
      if (*operand == '-' || *operand == '+')
        {
          exponent_minus_flag = *operand == '-';
          operand++;
        }
      else
        exponent_minus_flag = 0 /* FALSE */;
      if (!isdigit (*operand))
        operand = exponent_start;
      else
        {
          additional_binary_exponent = binary_exponent;
          binary_exponent = 0;
          do
            {
              if (INT_MAX / 10 < binary_exponent)
                binary_exponent = INT_MAX / 2;
              else
                binary_exponent *= 10;
              if (INT_MAX - (*operand - '0') < binary_exponent)
                binary_exponent = INT_MAX / 2;
              else
                binary_exponent += *operand - '0';
              operand++;
            }
          while (isdigit (*operand));
        }
      if (exponent_minus_flag)
        binary_exponent = additional_binary_exponent - binary_exponent;
      else
        binary_exponent += additional_binary_exponent;
    }
  if (leading_zero_flag)
    {
      /* It is zero */
      assert (bits::is_zero_bit_string (fraction, 0,
					INTERNAL_FRACTION_SIZE * CHAR_BIT)
              && !*imprecise_flag);
      *non_biased_exponent = NON_BIASED_EXPONENT_FOR_UNDERFLOW;
    }
  else
    {
      *non_biased_exponent = binary_exponent;
      normalize_fraction (non_biased_exponent, fraction);
    }
  return (char *) operand;
}

/* The following function skips all white spaces at the begin of
   source string and transforms binary ascii representation to IEEE
   floating point number FP with characteristics D with the aid of
   function `string_to_float'.  The floating point number must
   correspond the following syntax
  
          ['+' | '-'] [<digits>] [ '.' [<digits>] ]
                  [ ('p' | 'P') ['+' | '-'] <decimal digits>]

       or must be the following strings

              `SNaN', `QNaN', `+Inf', `-Inf', `+0', or `-0'.

   Digit should be less BASE which in its turn should be 2, 4, 8, or
   16.  The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  The function can fix output exceptions as
   described in the file begin.  Current round mode may affect
   resultant floating point number.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used. */

static char *
IEEE_from_binary_string (const char *operand, int base,
			 void *fp, float_desc_t d)
{
  int imprecise_flag;
  int sign;
  int non_biased_exponent;
  fraction_t fraction;
  void (*saved_unsigned_overflow_reaction) (void);

  while (isspace (*operand))
    operand++;
  if (strncmp (operand, TRAPPING_NaN_NAME, strlen (TRAPPING_NaN_NAME)) == 0)
    {
      trapping_NaN (fp, d);
      operand += strlen (TRAPPING_NaN_NAME);
    }
  else if (strncmp (operand, NaN_NAME, strlen (NaN_NAME)) == 0)
    {
      NaN (fp, d);;
      operand += strlen (NaN_NAME);
    }
  else if (strncmp (operand, POSITIVE_INFINITY_NAME,
                    strlen (POSITIVE_INFINITY_NAME)) == 0)
    {
      positive_infinity (fp, d);
      operand += strlen (POSITIVE_INFINITY_NAME);
    }
  else if (strncmp (operand, NEGATIVE_INFINITY_NAME,
                    strlen (NEGATIVE_INFINITY_NAME)) == 0)
    {
      negative_infinity (fp, d);
      operand += strlen (NEGATIVE_INFINITY_NAME);
    }
  else
    {
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      operand = binary_string_to_float (operand, base,
					&sign, &non_biased_exponent,
					fraction, &imprecise_flag);
      form_float (fp, d, sign, non_biased_exponent + d->exponent_bias,
		  fraction, 0, imprecise_flag,
		  saved_unsigned_overflow_reaction);
    }
  return (char *) operand;
}

/* The following function skips all white spaces at the begin of
   source string and transforms binary ascii representation to IEEE
   single precision floating point number with the aid of function
   `string_to_float'.  The floating point number must correspond the
   following syntax
  
          ['+' | '-'] [<digits>] [ '.' [<digits>] ]
                  [ ('p' | 'P') ['+' | '-'] <decimal digits>]

       or must be the following strings

              `SNaN', `QNaN', `+Inf', `-Inf', `+0', or `-0'.

   Digit should be less BASE which in its turn should be 2, 4, 8, or
   16.  The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  The function can fix output exceptions as
   described in the file begin.  Current round mode may affect
   resultant floating point number.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used. */

char *
IEEE_float::from_binary_string (const char *operand, int base)
{
  current_status_bits = 0;
  return IEEE_from_binary_string (operand, base, this, &single_float_desc);
}

/* The following function skips all white spaces at the begin of
   source string and transforms decimal ascii representation to IEEE
   double precision floating point number with the aid of function
   `string_to_float'.  The floating point number must correspond the
   following syntax

            ['+' | '-'] [<digits>] [ '.' [<digits>] ]
                  [ ('p' | 'P') ['+' | '-'] <decimal digits>]

       or must be the following strings

              `SNaN', `QNaN', `+Inf', `-Inf', `+0', or `-0'.

   Digit should be less BASE which in its turn should be 2, 4, 8, or
   16.  The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  The function can fix output exceptions as
   described in the file begin.  Current round mode may affect
   resultant floating point number.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used. */

char *
IEEE_double::from_binary_string (const char *operand, int base)
{
  current_status_bits = 0;
  return IEEE_from_binary_string (operand, base, this, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following function skips all white spaces at the begin of
   source string and transforms binary ascii representation to IEEE
   quad precision floating point number with the aid of function
   `string_to_float'.  The floating point number must correspond the
   following syntax

            ['+' | '-'] [<digits>] [ '.' [<digits>] ]
                  [ ('p' | 'P') ['+' | '-'] <decimal digits>]

       or must be the following strings

              `SNaN', `QNaN', `+Inf', `-Inf', `+0', or `-0'.

   Digit should be less BASE which in its turn should be 2, 4, 8, or
   16.  The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  The function can fix output exceptions as
   described in the file begin.  Current round mode may affect
   resultant floating point number.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used. */

char *
IEEE_quad::from_binary_string (const char *operand, int base)
{
  current_status_bits = 0;
  return IEEE_from_binary_string (operand, base, this, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */



/* This page contains function for transformation of strings to IEEE
   floating point numbers. */

/* The following function transforms decimal ascii representation to
   IEEE floating point number given by its sign, exponent, and
   fraction.  The floating point number must correspond the following
   syntax

            ['+' | '-'] [<decimal digits>] [ '.' [<decimal digits>] ]
                  [ ('e' | 'E') ['+' | '-'] <decimal digits>]

   The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  Current round mode may affect resultant
   floating point number.  The function uses the most precise floating
   point (more precise than IEEE double floating point format) values
   of non negative powers of ten generated with the aid of POSIX
   arbitrary precision calculator `bc' because calculation of the
   powers of ten by conventional way (by multiplication of IEEE
   floating point numbers can result in rounding imprecises). */

static char *
string_to_float (const char *operand, int *sign, int *non_biased_exponent,
                 fraction_t fraction, int *imprecise_flag)
{
  int leading_zero_flag;
  int number_of_rest_significant_digits;
  int decimal_exponent;
  int additional_decimal_exponent;
  const char *exponent_start;
  int exponent_minus_flag;
  int ten_power_exponent;
  int ten_power_fraction_imprecise_flag;
  fraction_t ten;
  fraction_t ten_fraction;
  fraction_t temporary_fraction;
  fraction_t result_fraction;
  char digit_string [2];

  *imprecise_flag = 0 /* FALSE */;
  (void) unsigned_integer::from_string (INTERNAL_FRACTION_SIZE, "0", fraction);
  (void) unsigned_integer::from_string (INTERNAL_FRACTION_SIZE, "10", ten);
  number_of_rest_significant_digits
    = (int) ((INTERNAL_FRACTION_SIZE * CHAR_BIT / 2 * LOG_OF_2) + 2.0);
  /* The following statement for subsequent rounding */
  number_of_rest_significant_digits++;
  if (*operand == '-' || *operand == '+')
    {
      *sign = *operand == '-';
      operand++;
    }
  else
    *sign = 0;
  leading_zero_flag = 1 /* TRUE */;
  decimal_exponent = 0;
  while (isdigit (*operand))
    {
      if (*operand != '0' || !leading_zero_flag)
        {
          if (number_of_rest_significant_digits > 0)
            {
              number_of_rest_significant_digits--;
              unsigned_integer::multiply (INTERNAL_FRACTION_SIZE, fraction,
                                          ten, fraction);
              assert (!integer::overflow_bit);
              *digit_string = *operand;
              digit_string [1] = '\0';
              (void) unsigned_integer::from_string (INTERNAL_FRACTION_SIZE,
                                                    digit_string,
                                                    temporary_fraction);
              unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction,
                                     temporary_fraction, fraction);
              assert (!integer::overflow_bit);
            }
          else
            {
              *imprecise_flag = *imprecise_flag || *operand != '0';
              decimal_exponent++;
            }
          leading_zero_flag = 0 /* FALSE */;
        }
      operand++;
    }
  if (*operand == '.')
    {
      operand++;
      while (isdigit (*operand))
        {
          if (*operand != '0' || !leading_zero_flag)
            {
              if (number_of_rest_significant_digits > 0)
                {
                  number_of_rest_significant_digits--;
                  unsigned_integer::multiply (INTERNAL_FRACTION_SIZE,
                                              fraction, ten, fraction);
                  assert (!integer::overflow_bit);
                  *digit_string = *operand;
                  digit_string [1] = '\0';
                  (void) unsigned_integer::from_string (INTERNAL_FRACTION_SIZE,
                                                        digit_string,
                                                        temporary_fraction);
                  unsigned_integer::add (INTERNAL_FRACTION_SIZE, fraction,
                                         temporary_fraction, fraction);
                  assert (!integer::overflow_bit);
                  decimal_exponent--;
                }
              else
                *imprecise_flag = *imprecise_flag || *operand != '0';
              leading_zero_flag = 0 /* FALSE */;
            }
          else
            decimal_exponent--;
          operand++;
        }
    }
  if (*operand == 'e' || *operand == 'E')
    {
      exponent_start = operand;
      operand++;
      if (*operand == '-' || *operand == '+')
        {
          exponent_minus_flag = *operand == '-';
          operand++;
        }
      else
        exponent_minus_flag = 0 /* FALSE */;
      if (!isdigit (*operand))
        operand = exponent_start;
      else
        {
          additional_decimal_exponent = decimal_exponent;
          decimal_exponent = 0;
          do
            {
              if (INT_MAX / 10 < decimal_exponent)
                decimal_exponent = INT_MAX / 2;
              else
                decimal_exponent *= 10;
              if (INT_MAX - (*operand - '0') < decimal_exponent)
                decimal_exponent = INT_MAX / 2;
              else
                decimal_exponent += *operand - '0';
              operand++;
            }
          while (isdigit (*operand));
        }
      if (exponent_minus_flag)
        decimal_exponent = additional_decimal_exponent - decimal_exponent;
      else
        decimal_exponent += additional_decimal_exponent;
    }
  if (leading_zero_flag)
    {
      /* It is zero */
      assert (bits::is_zero_bit_string (fraction, 0,
                                        INTERNAL_FRACTION_SIZE * CHAR_BIT)
              && !*imprecise_flag);
      *non_biased_exponent = NON_BIASED_EXPONENT_FOR_UNDERFLOW;
    }
  else
    {
      *non_biased_exponent = INTERNAL_FRACTION_SIZE * CHAR_BIT - 1;
      normalize_fraction (non_biased_exponent, fraction);
      if (!get_ten_power ((decimal_exponent < 0
                           ? -decimal_exponent : decimal_exponent),
                          ten_fraction, &ten_power_exponent,
                          &ten_power_fraction_imprecise_flag))
        {
          if (decimal_exponent < 0)
            *non_biased_exponent = NON_BIASED_EXPONENT_FOR_UNDERFLOW;
          else
            *non_biased_exponent = NON_BIASED_EXPONENT_FOR_OVERFLOW;
        }
      else if (decimal_exponent < 0)
        {
          *imprecise_flag
            = *imprecise_flag || ten_power_fraction_imprecise_flag;
          unsigned_integer::shift_right
            (INTERNAL_FRACTION_SIZE, ten_fraction,
             INTERNAL_FRACTION_SIZE * CHAR_BIT / 2, temporary_fraction);
          assert (!integer::overflow_bit);
          unsigned_integer::divide (INTERNAL_FRACTION_SIZE, fraction,
                                    temporary_fraction, result_fraction);
          unsigned_integer::multiply (INTERNAL_FRACTION_SIZE, result_fraction,
                                      temporary_fraction, temporary_fraction);
          assert (!integer::overflow_bit);
          *imprecise_flag
            = *imprecise_flag
              || unsigned_integer::ne (INTERNAL_FRACTION_SIZE,
                                       temporary_fraction, fraction);
          unsigned_integer::shift_left
            (INTERNAL_FRACTION_SIZE, result_fraction,
             INTERNAL_FRACTION_SIZE * CHAR_BIT / 2 - 1, result_fraction);
          assert (!integer::overflow_bit);
          *non_biased_exponent -= ten_power_exponent;
          memcpy (fraction, result_fraction, (size_t) INTERNAL_FRACTION_SIZE);
        }
      else
        {
          *imprecise_flag
            = *imprecise_flag || ten_power_fraction_imprecise_flag;
          unsigned_integer::shift_right
            (INTERNAL_FRACTION_SIZE, fraction,
             INTERNAL_FRACTION_SIZE * CHAR_BIT / 2, fraction);
          *imprecise_flag = *imprecise_flag || integer::overflow_bit != 0;
          unsigned_integer::shift_right
            (INTERNAL_FRACTION_SIZE, ten_fraction,
             INTERNAL_FRACTION_SIZE * CHAR_BIT / 2, temporary_fraction);
          *imprecise_flag = *imprecise_flag || integer::overflow_bit != 0;
          *non_biased_exponent += ten_power_exponent;
          unsigned_integer::multiply (INTERNAL_FRACTION_SIZE,
                                      fraction, temporary_fraction, fraction);
          assert (!integer::overflow_bit);
          if (bits::bit (fraction, 0) == 0)
            unsigned_integer::shift_left (INTERNAL_FRACTION_SIZE, fraction, 1,
                                          fraction);
          else
            /* The fraction is not less than 2 here. */
            (*non_biased_exponent)++;
        }
    }
  return (char *) operand;
}

/* The following function skips all white spaces at the begin of
   source string and transforms decimal ascii representation to IEEE
   floating point number FP with characteristics D with the aid of
   function `string_to_float'.  The floating point number must
   correspond the following syntax
  
          ['+' | '-'] [<decimal digits>] [ '.' [<decimal digits>] ]
                  [ ('e' | 'E') ['+' | '-'] <decimal digits>]

       or must be the following strings

              `SNaN', `QNaN', `+Inf', `-Inf', `+0', or `-0'.

   The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  The function can fix output exceptions as
   described in the file begin.  Current round mode may affect
   resultant floating point number.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used. */

static char *
IEEE_from_string (const char *operand, void *fp, float_desc_t d)
{
  int imprecise_flag;
  int sign;
  int non_biased_exponent;
  fraction_t fraction;
  void (*saved_unsigned_overflow_reaction) (void);

  while (isspace (*operand))
    operand++;
  if (strncmp (operand, TRAPPING_NaN_NAME, strlen (TRAPPING_NaN_NAME)) == 0)
    {
      trapping_NaN (fp, d);
      operand += strlen (TRAPPING_NaN_NAME);
    }
  else if (strncmp (operand, NaN_NAME, strlen (NaN_NAME)) == 0)
    {
      NaN (fp, d);;
      operand += strlen (NaN_NAME);
    }
  else if (strncmp (operand, POSITIVE_INFINITY_NAME,
                    strlen (POSITIVE_INFINITY_NAME)) == 0)
    {
      positive_infinity (fp, d);
      operand += strlen (POSITIVE_INFINITY_NAME);
    }
  else if (strncmp (operand, NEGATIVE_INFINITY_NAME,
                    strlen (NEGATIVE_INFINITY_NAME)) == 0)
    {
      negative_infinity (fp, d);
      operand += strlen (NEGATIVE_INFINITY_NAME);
    }
  else
    {
      saved_unsigned_overflow_reaction
        = unsigned_integer::set_overflow_reaction
          (integer::default_arithmetic_overflow_reaction);
      operand = string_to_float (operand, &sign, &non_biased_exponent,
                                 fraction, &imprecise_flag);
      form_float (fp, d, sign, non_biased_exponent + d->exponent_bias,
		  fraction, 0, imprecise_flag,
		  saved_unsigned_overflow_reaction);
    }
  return (char *) operand;
}

/* The following member function skips all white spaces at the begin
   of source string and transforms decimal ascii representation to the
   IEEE single precision floating point number with the aid of
   function `string_to_float'.  The floating point number must
   correspond the following syntax
  
          ['+' | '-'] [<decimal digits>] [ '.' [<decimal digits>] ]
                  [ ('e' | 'E') ['+' | '-'] <decimal digits>]

       or must be the following strings

              `SNaN', `QNaN', `+Inf', `-Inf', `+0', or `-0'.

   The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  The function can fix output exceptions as
   described in the file begin.  Current round mode may affect
   resultant floating point number.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used.  But the reverse
   transformation `string with 9 digits -> IEEE floating point number
   -> string' may results in different digits of the fractions in
   ascii representation because a floating point number may represent
   several such strings with differences in the least significant
   digit.  But the ascii representation are identical when function
   `IEEE_float::from_string' does not fix imprecise result exception
   or less than 9 digits of the fractions in the ascii representations
   are compared. */

char *
IEEE_float::from_string (const char *operand)
{
  current_status_bits = 0;
  return IEEE_from_string (operand, this, &single_float_desc);
}

/* The following member function skips all white spaces at the begin
   of source string and transforms decimal ascii representation to the
   IEEE double precision floating point number with the aid of
   function `string_to_float'.  The floating point number must
   correspond the following syntax

            ['+' | '-'] [<decimal digits>] [ '.' [<decimal digits>] ]
                  [ ('e' | 'E') ['+' | '-'] <decimal digits>]

       or must be the following strings

              `SNaN', `QNaN', `+Inf', `-Inf', `+0', or `-0'.

   The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  The function can fix output exceptions as
   described in the file begin.  Current round mode may affect
   resultant floating point number.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used.  But the reverse
   transformation `string with 17 digits -> IEEE floating point number
   -> string' may results in different digits of the fractions in
   ascii representation because a floating point number may represent
   several such strings with differences in the least significant
   digit.  But the ascii representation are identical when function
   `IEEE_double::from_string' does not fix imprecise result exception
   or less than 17 digits of the fractions in the ascii
   representations are compared. */

char *
IEEE_double::from_string (const char *operand)
{
  current_status_bits = 0;
  return IEEE_from_string (operand, this, &double_float_desc);
}

#ifdef IEEE_QUAD
/* The following member function skips all white spaces at the begin
   of source string and transforms decimal ascii representation to the
   IEEE quad precision floating point number with the aid of
   function `string_to_float'.  The floating point number must
   correspond the following syntax

            ['+' | '-'] [<decimal digits>] [ '.' [<decimal digits>] ]
                  [ ('e' | 'E') ['+' | '-'] <decimal digits>]

       or must be the following strings

              `SNaN', `QNaN', `+Inf', `-Inf', `+0', or `-0'.

   The function returns pointer to first character in the source
   string after read floating point number.  The function reads string
   as long as possible.  The function can fix output exceptions as
   described in the file begin.  Current round mode may affect
   resultant floating point number.  It is guaranteed that
   transformation `IEEE floating point number -> string -> IEEE
   floating point number' results in the same IEEE floating point
   number if round to nearest mode is used.  But the reverse
   transformation `string with 36 digits -> IEEE floating point number
   -> string' may results in different digits of the fractions in
   ascii representation because a floating point number may represent
   several such strings with differences in the least significant
   digit.  But the ascii representation are identical when function
   `IEEE_quad::from_string' does not fix imprecise result exception
   or less than 36 digits of the fractions in the ascii
   representations are compared. */

char *
IEEE_quad::from_string (const char *operand)
{
  current_status_bits = 0;
  return IEEE_from_string (operand, this, &quad_float_desc);
}
#endif /* #ifdef IEEE_QUAD */
