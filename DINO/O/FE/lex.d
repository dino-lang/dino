include "mpi";
include "ieee";

var unique_strings = {};
func uniq_str (str) {
  if (str in unique_strings)
    str = unique_strings {str};
  else
    unique_strings {str} = str;
  return str;
}

class lex (lno, pos, fname, repr, code) {

  class od () { }                                   // Operator/delimiter
  class ident () { }				    // identifier
  class ch (ch_code) { }                            // ch_code is an integer
  class str (str) { str = uniq_str (str); }         // str is an string, eg "A"

  class integer () {                                // v = mpi
    class si (v) {}  class i (v) {}  class li (v) {}
  }

  class floating () {                              // v is a ieee instance
    class r (v) {}   class lr (v) {}
  }

  fname = uniq_str (fname);
  repr = uniq_str (repr);
}

var lexs = lex (), integers = lexs.integer (), reals = lexs.floating ();