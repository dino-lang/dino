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
