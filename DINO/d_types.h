/*
   Copyright (C) 1997-2015 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of interpreter of DINO.

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

*/

#ifndef __D_CONFIG_H__
#define __D_CONFIG_H__

#ifdef HAVE_CONFIG_H
#include "d_config.h"
#else /* In this case we are oriented to ANSI C and dfcn.h */
#ifndef HAVE_FLOAT_H
#define HAVE_FLOAT_H
#endif
#ifndef HAVE_LIMITS_H
#define HAVE_LIMITS_H
#endif
#endif /* #ifdef HAVE_CONFIG_H */

#include <gmp.h>

#if __GNU_MP__ < 2
#error Too old GMP package
#endif

/* Type mpz_t is an one element array and array can not field type in
   SPRUT.  Therefore we use a wrap up type.  */
typedef struct { mpz_t mpz;} gmp_t;

#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#error We need limits.h
#endif

#include <math.h>

#ifdef HAVE_FLOAT_H
#include <float.h>
#else
#define FLT_MAX  3.40282347e+38F         /* IEEE float */
#define DBL_MAX  1.7976931348623157e+308 /* IEEE double */
#define FLT_MANT_DIG  24
#define FLT_MAX_EXP  128
#define DBL_MANT_DIG  53
#define DBL_MAX_EXP 1024
#define DBL_DIG 16
#endif

#ifdef DBL_DECIMAL_DIG
#define FORMAT_DOUBLE_DIGS (DBL_DECIMAL_DIG)
#else  
#ifdef DECIMAL_DIG
#define FORMAT_DOUBLE_DIGS (DECIMAL_DIG)
#else  
#define FORMAT_DOUBLE_DIGS (DBL_DIG + 1)
#endif
#endif

#if INT_MAX >= 9223372036854775807
typedef int rint_t;
typedef unsigned int unsigned_rint_t;
#define MAX_RINT   INT_MAX	/* int */
#define MIN_RINT   INT_MIN	/* int */
#elif LONG_MAX >= 9223372036854775807
typedef long int rint_t;
typedef unsigned long int unsigned_rint_t;
#define MAX_RINT   LONG_MAX	/* long */
#define MIN_RINT   LONG_MIN	/* long */
#elif LLONG_MAX >= 9223372036854775807
typedef long long int rint_t;
typedef unsigned long long int unsigned_rint_t;
#define MAX_RINT   LLONG_MAX	/* long long */
#define MIN_RINT   LLONG_MIN	/* long long */
#else
#error there is no 64 bits int
#endif

#if DBL_MANT_DIG == 53 && DBL_MAX_EXP == 1024
typedef double rfloat_t;

#ifdef HUGE_VAL
#define RFLOAT_HUGE_VAL HUGE_VAL
#endif

#define MAX_FLOAT DBL_MAX	/* double */
#elif FLT_MANT_DIG == 53 && FLT_MAX_EXP == 1024
typedef float rfloat_t;

#ifdef HUGE_VALF
#define RFLOAT_HUGE_VAL HUGE_VALF
#endif

#define MAX_FLOAT FLT_MAX	/* float */
#else
#error there is no IEEE double
#endif

/* Alignment??? 32-bit int ? */
#ifdef WORDS_BIGENDIAN
static const char __nan__[8] = { 0x7f, 0xf8, 0, 0, 0, 0, 0, 0 };
#define IS_RFLOAT_NAN(var)\
  ((((int *) &var) [0] & ((int *) __nan__) [0]) == ((int *) __nan__) [0])
#else
static const char __nan__[8] = { 0, 0, 0, 0, 0, 0, 0xf8, 0x7f };
#define IS_RFLOAT_NAN(var)\
  ((((int *) &var) [1] & ((int *) __nan__) [1]) == ((int *) __nan__) [1])
#endif

#ifndef RFLOAT_HUGE_VAL
#ifdef WORDS_BIGENDIAN
static const char __infinity[8] = { 0x7f, 0xf0, 0, 0, 0, 0, 0, 0 };
#else
static const char __infinity[8] = { 0, 0, 0, 0, 0, 0, 0xf0, 0x7f };
#endif
#define RFLOAT_NAN (*(rfloat_t *) __infinity)
#endif

typedef unsigned char char_t;

#define MAX_CHAR  UCHAR_MAX /* unsigned char */

typedef char bool_t;

#endif /*#ifndef __D_CONFIG_H__ */
