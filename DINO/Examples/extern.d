/*
   Copyright (C) 1997-2002 Vladimir Makarov.

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

// Illustrate usage of include and externals.
include "mpi";

var mpi1 = mpis.from_string(1, "10");
var mpi2 = mpis.from_string(1, "5");
putln (mpis.to_string (mpis.add (mpi1, mpi2)));
putln (mpis.to_string (mpis.subtract (mpi1, mpi2)));
putln (mpis.to_string (mpis.subtract (mpi2, mpi1)));
putln (mpis.to_string (mpis.multiply (mpi1, mpi2)));
putln (mpis.to_string (mpis.divide (mpi1, mpi2)));
putln (mpis.to_string (mpis.remainder (mpi1, mpi2)));
putln (mpis.eq (mpi1, mpi2));
putln (mpis.ne (mpi1, mpi2));
putln (mpis.lt (mpi1, mpi2));
putln (mpis.le (mpi1, mpi2));
putln (mpis.gt (mpi1, mpi2));
putln (mpis.ge (mpi1, mpi2));

mpi1 = mpis.from_string(50, "10000000000000000000000000000000000000000000000");
mpi2 = mpis.from_string(50, "5000000000000000000000000000000000000000000000");
putln (mpis.to_string (mpis.add (mpi1, mpi2)));
putln (mpis.to_string (mpis.subtract (mpi1, mpi2)));
putln (mpis.to_string (mpis.subtract (mpi2, mpi1)));
putln (mpis.to_string (mpis.multiply (mpi1, mpi2)));
putln (mpis.to_string (mpis.divide (mpi1, mpi2)));
putln (mpis.to_string (mpis.remainder (mpi1, mpi2)));
putln (mpis.eq (mpi1, mpi2));
putln (mpis.ne (mpi1, mpi2));
putln (mpis.lt (mpi1, mpi2));
putln (mpis.le (mpi1, mpi2));
putln (mpis.gt (mpi1, mpi2));
putln (mpis.ge (mpi1, mpi2));
try {
  mpis.multiply (mpis.multiply (mpi1, mpi2), mpis.multiply (mpi1, mpi2));
} catch (mpis.mpi_excepts.mpi_overflow) {
  putln ("got overflow 1");
}
try {
  mpis.change_size (mpi2, 2);
} catch (mpis.mpi_excepts.mpi_overflow) {
  putln ("got overflow 2");
}
