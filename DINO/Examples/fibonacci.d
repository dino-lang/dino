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

// Recursive function to compute & output Fibonacci numbers
func fibonacci (n)
  {
     if (n <= 1) return 1;
     return (fibonacci(n-1) + fibonacci(n-2));
  }

var i, fibnum;

fibnum = 0;
for (i = 0; i <= 24; i++) 
  {
    fibnum = fibonacci(i);
    putln (i @ " " @ fibnum); 
  }
