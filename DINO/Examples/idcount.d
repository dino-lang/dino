/*
   Copyright (C) 1997-2005 Vladimir Makarov.

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

// Output number of occurences of identifiers and numbers.
// It inputs from stdin.
var i, key, voc = {};
for (;;)
  try {
    var ln, a;

    ln = getln ();
    if (ln == "")
      continue;
    a = split (ln, "[^[:alnum:]]");
    for (i = 0; i < #a; i++)
      voc {a[i]} = (a[i] in voc ? voc {a[i]} + 1 : 1);
  } catch (invcalls.eof) {
    break;
  }
func comp (el1, el2) {
  return cmpv (tolower (el1), tolower (el2));
}
key = sort (keys (voc), comp);
for (i = 0; i < #key; i++)
  putln (key[i], " : ", voc{key[i]});
