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

// Illustration of threads.  There is output.
class buffer (length = 3) {
  var b = [length:nil], first = 0, free = 0, empty = 1;
  private b, first, free, length;
  func consume () {
    var res;

    wait !empty;
    res = b [first];
    first = (first + 1) % length;
    empty = first == free;
    return res;
  }
  func produce (val) {
    wait empty || free != first;
    b [free] = val;
    free = (free + 1) % length;
    empty = 0;
  }
}

thread consumer (buffer) {
    func produce (val) {
      buffer.produce (val);
      put ("produce: ");
      println (val);
    }
    produce (10);
    produce (10.5);
    produce ("string");
    produce ('c');
    produce (nil);
}

thread producer (buffer) {
  var val;

  for (;;) {
    val = buffer.consume ();
    if (val == nil)
      break;
    put ("consume: ");
    println (val);
  }
}

var queue = buffer ();
consumer (queue);
producer (queue);
