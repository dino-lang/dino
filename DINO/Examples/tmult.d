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

//  Multiplication of 100x100 Matrixes represented by tables.  No output.
var m1, m2;

func mmult (m1, m2)
{
  var i, j, k, m1rows, m1cols, m2rows, m2cols, result;

  m1rows = #m1; m2rows = #m2;
  m1cols = #m1{0}; m2cols = #m2{0};
  if (m2cols != m2rows)
    {
       println ("matrices don't match");
       return;
    }
  result = {};
  for (i=0; i < m1rows; i++) {
    result{i} = {};
    for (j=0; j < m2cols; j++) {
      result{i}{j} = 0;
      for (k=0; k < m1cols; k++)
        result{i}{j} += m1{i}{k}*m2{k}{j};
    }
  }
  return result;
}

var i, j;

m1 = {};
for (i = 0;i < 100; i++) {
  m1{i} = {};
  for (j = 0;j < 100; j++)
    m1{i}{j} = 2;
}
m2 = m1;
mmult (m1, m2);
