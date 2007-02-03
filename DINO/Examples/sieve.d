/*
   Copyright (C) 1997-2007 Vladimir Makarov.

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

// Output number of primes number < 8190.
var SieveSize, i, prime, k, count, iter, flags;
SieveSize = 8190;

for (iter = 0; iter < 10; iter++;)
  {
    flags = [SieveSize + 1 : 0];
    count = 0;
    for (i = 0; i <= SieveSize; i++)
      flags[i] = 1;
    for (i = 0; i <= SieveSize; i++;)
      if (flags[i])
        {
          prime = i + i + 3;
          k = i + prime;
          for (;1;;)
            {
	      if (k > SieveSize)
                break;
              flags[k] = 0;
              k += prime;
            }
          count++;
        }
  }
println (count);
