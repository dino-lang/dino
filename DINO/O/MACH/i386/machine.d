// Machine dependent parameters.
include "mpi";
include "ieee";

class machine () {
  var final host_byte_bits = 8;

  // sizes in host bytes (boolean is represented by char):
  var final ch_size = 1;
  var final si_size = 1;
  var final i_size = 2;
  var final li_size = 4;
  var final set_size = 4; // one of integer sizes
  var final max_set = set_size * host_byte_bits - 1;
  var final ptr_size = 4; // one of integer sizes
  var final r_class = ieees.single;
  var final lr_class = ieees.double;
  var final max_ch = 255;
  var final si_zero = mpis.from_string (si_size, "0");
  var final i_zero = mpis.from_string (i_size, "0");
  var final li_zero = mpis.from_string (li_size, "0");
  var final max_si = mpis.from_based_string (1, "7f", 16);
  var final max_i = mpis.from_based_string (2, "7fff", 16);
  var final max_li = mpis.from_based_string (4, "7fffffff", 16);
  var final min_si = mpis.from_based_string (1, "-100", 16);
  var final min_i = mpis.from_based_string (2, "-10000", 16);
  var final min_li = mpis.from_based_string (4, "-100000000", 16);
  var final nil_ptr = mpis.from_string (ptr_size , "0");
  var final zero_size = mpis.from_string (ptr_size , "0");
}

var mach = machine ();
