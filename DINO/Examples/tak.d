// very good recursive test.  It should output 7.
func tak (x, y, z)
  {
    if (y >= x)
        return z;
    else
        return tak (tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y));
  }
putln (tak(18, 12, 6));
