/*
   Copyright (C) 2005, 2006 Vladimir Makarov.

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


ext except {
  class gmp_except () {
    class gmp_param_val (msg) {}
    class gmp_type (msg) {}
  }
}

final class mpz_package () {
  var gmp_excepts = excepts.gmp_except ();

  extern _z_create(), _z_clear(), _z_get_si(), _z_add(), _z_mul(), _z_tdiv_q();
  private _z_create, _z_clear, _z_get_si, _z_add, _z_mul, _z_tdiv_q;
  private check, check2;

  final class mpz (i) {
    private mpz_val; var mpz_val;
    if (type (i) != int) gmp_excepts.gmp_param_val ();
    mpz_val = _z_create (i);
    func destroy () { _z_clear (mpz_val); }
  }
  func check (op) {
    if (type (op) != class () || ! inside (op, mpz_package))
      throw gmp_excepts.gmp_type ();
  }
  func check2 (op1, op2) {
    if (type (op1) != class () || ! inside (op1, mpz_package)
	|| type (op2) != class () || ! inside (op2, mpz_package))
      throw gmp_excepts.gmp_type ();
  }
  func get_i (op)  { check (op); return _z_get_si (op); }
  func add (op1, op2) { check2 (op1, op2); return _z_add (mpz (0), op1, op2); }
  func mul (op1, op2) { check2 (op1, op2); return _z_mul (mpz (0), op1, op2); }
  func tdiv_q (n, d) { check2 (n, d); return _z_tdiv_q (mpz (0), n, d); }
}

var mpzs = mpz_package ();
