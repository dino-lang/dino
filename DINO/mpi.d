/*
   Copyright (C) 1997-2014 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

obj mpis {
  // if you change the value, please change it in mpi.c and check
  // maximal value in arithm.c.
  val max_mpi_size = 128;
  var mpi_ignore_overflow = 0;
  
  class mpi_except () {use except;}
  class mpi_type_except () {use mpi_except;}
  class mpi_size_except () {use mpi_except;}
  class mpi_base_except () {use mpi_except;}
  class mpi_unequal_size_except () {use mpi_except;}
  class mpi_overflow_except () {use mpi_except;}

  final class mpi (val size) { // The order of vars is important for mpi.c
    priv var value;
    if (type (size) != int)
      throw mpi_type_except ();
    if (size < 1 || size > max_mpi_size)
      throw mpi_size_except ();
  }
  priv fun check (op) {
    if (type (op) != obj || !inside (op, mpis))
      throw mpi_type_except();
  }
  priv fun check2 (op1, op2) {
    check (op1); check (op2);
    if (op1.size != op2.size)
      throw mpi_unequal_size_except();
  }
  extern mpi_overflow;
  priv fun check_overflow (op) {
    if (mpi_overflow && !mpi_ignore_overflow)
      throw mpi_overflow_except();
    return op;
  }
  priv extern mpi_add(), mpi_unsigned_add(),
    mpi_subtract(), mpi_unsigned_subtract(),
    mpi_multiply(), mpi_unsigned_multiply(),
    mpi_divide(), mpi_unsigned_divide(),
    mpi_remainder(), mpi_unsigned_remainder(),
    mpi_shift_right(), mpi_unsigned_shift_right(),
    mpi_shift_left(), mpi_unsigned_shift_left(), mpi_or(), mpi_unsigned_or(),
    mpi_and(), mpi_unsigned_and(), mpi_xor(), mpi_unsigned_xor(),
    mpi_not(), mpi_unsigned_not(), mpi_eq(), mpi_unsigned_eq(),
    mpi_ne(), mpi_unsigned_ne(), mpi_gt(), mpi_unsigned_gt(),
    mpi_lt(), mpi_unsigned_lt(), mpi_ge(), mpi_unsigned_ge(),
    mpi_le(), mpi_unsigned_le(), mpi_change_size(), mpi_unsigned_change_size(),
    mpi_to_based_string(), mpi_unsigned_to_based_string(),
    mpi_from_based_string(), mpi_unsigned_from_based_string();
  fun add (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_add (op1, op2, new op1));
  }
  fun unsigned_add (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_add (op1, op2, new op1));
  }
  fun subtract (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_subtract (op1, op2, new op1));
  }
  fun unsigned_subtract (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_subtract (op1, op2, new op1));
  }
  fun multiply (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_multiply (op1, op2, new op1));
  }
  fun unsigned_multiply (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_multiply (op1, op2, new op1));
  }
  fun divide (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_divide (op1, op2, new op1));
  }
  fun unsigned_divide (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_divide (op1, op2, new op1));
  }
  fun remainder (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_remainder (op1, op2, new op1));
  }
  fun unsigned_remainder (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_unsigned_remainder (op1, op2, new op1));
  }
  fun shift_right (op, shift) {
    check (op);
    if (type (shift) != int)
      throw mpi_type_except();
    return mpi_shift_right (op, shift, new op);
  }
  fun unsigned_shift_right (op, shift) {
    check (op);
    if (type (shift) != int)
      throw mpi_type_except();
    return mpi_unsigned_shift_right (op, shift, new op);
  }
  fun shift_left (op, shift) { // Overflow is possible
    check (op);
    if (type (shift) != int)
      throw mpi_type_except();
    return check_overflow (mpi_shift_left (op, shift, new op));
  }
  fun unsigned_shift_left (op, shift) { // Overflow is possible
    check (op);
    if (type (shift) != int)
      throw mpi_type_except();
    return check_overflow (mpi_unsigned_shift_left (op, shift, new op));
  }
  fun or (op1, op2) {
    check2 (op1, op2);
    return mpi_or (op1, op2, new op1);
  }
  fun unsigned_or (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_or (op1, op2, new op1);
  }
  fun and (op1, op2) {
    check2 (op1, op2);
    return mpi_and (op1, op2, new op1);
  }
  fun unsigned_and (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_and (op1, op2, new op1);
  }
  fun xor (op1, op2) {
    check2 (op1, op2);
    return mpi_xor (op1, op2, new op1);
  }
  fun unsigned_xor (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_xor (op1, op2, new op1);
  }
  fun not (op) {
    check (op);
    return mpi_not (op, new op);
  }
  fun unsigned_not (op) {
    check (op);
    return mpi_unsigned_not (op, new op);
  }
  fun eq (op1, op2) {
    check2 (op1, op2);
    return mpi_eq (op1, op2);
  }
  fun unsigned_eq (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_eq (op1, op2);
  }
  fun ne (op1, op2) {
    check2 (op1, op2);
    return mpi_ne (op1, op2);
  }
  fun unsigned_ne (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_ne (op1, op2);
  }
  fun gt (op1, op2) {
    check2 (op1, op2);
    return mpi_gt (op1, op2);
  }
  fun unsigned_gt (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_gt (op1, op2);
  }
  fun lt (op1, op2) {
    check2 (op1, op2);
    return mpi_lt (op1, op2);
  }
  fun unsigned_lt (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_lt (op1, op2);
  }
  fun ge (op1, op2) {
    check2 (op1, op2);
    return mpi_ge (op1, op2);
  }
  fun unsigned_ge (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_ge (op1, op2);
  }
  fun le (op1, op2) {
    check2 (op1, op2);
    return mpi_le (op1, op2);
  }
  fun unsigned_le (op1, op2) {
    check2 (op1, op2);
    return mpi_unsigned_le (op1, op2);
  }
  fun change_size (op, new_size) { // Overflow is possible
    check (op);
    if (type (new_size) != int)
      throw mpi_type_except();
    if (new_size < 1 || new_size > max_mpi_size)
      throw mpi_size_except();
    return check_overflow (mpi_change_size (op, new_size, new op));
  }
  fun unsigned_change_size (op, new_size) { // Overflow is possible
    check (op);
    if (type (new_size) != int)
      throw mpi_type_except();
    if (new_size < 1 || new_size > max_mpi_size)
      throw mpi_size_except();
    return check_overflow (mpi_unsigned_change_size (op, new_size, new op));
  }
  fun to_based_string (op, base) {
    if (type (base) != int)
      throw mpi_type_except();
    if (base < 2 || base > 16)
      throw mpi_base_except();
    check (op);
    return mpi_to_based_string (op, base);
  }
  fun unsigned_to_based_string (op, base) {
    if (type (base) != int)
      throw mpi_type_except();
    if (base < 2 || base > 16)
      throw mpi_base_except();
    check (op);
    return mpi_unsigned_to_based_string (op, base);
  }
  fun to_string (op) {
    return to_based_string (op, 10);
  }
  fun unsigned_to_string (op) {
    return unsigned_to_based_string (op, 10);
  }
  fun from_based_string (size, string, base) { // Overflow is possible
    if (type (size) != int || type (base) != int
        || type (string) != vec ||  eltype (string) != char)
      throw mpi_type_except();
    if (size < 1 || size > max_mpi_size)
      throw mpi_size_except();
    if (base < 2 || base > 16)
      throw mpi_base_except();
    return check_overflow (mpi_from_based_string (string, mpi (size), base));
  }
  fun unsigned_from_based_string (size, string, base) { // Overflow is poss.
    if (type (size) != int || type (base) != int
        || type (string) != vec ||  eltype (string) != char)
      throw mpi_type_except();
    if (size < 1 || size > max_mpi_size)
      throw mpi_size_except();
    if (base < 2 || base > 16)
      throw mpi_base_except();
    return check_overflow (mpi_unsigned_from_based_string (string, mpi (size),
							   base));
  }
  fun from_string (size, string) {
    return from_based_string (size, string, 10);
  }
  fun unsigned_from_string (size, string) {
    return unsigned_from_based_string (size, string, 10);
  }
}
