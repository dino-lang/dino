include "comm";
include "scanner";
include "parser";
include "et2at";
include "check";

func check_test (fname) { // read preprocessed C file and print lexs:
  var lexs, at, st, start = clock (), scan = scanner (fname);

  lexs = scan.get_lexs ();
  at = o2parse (grammar_fname (), lexs, scan.term_map);
  st = et2at (at); check_st (st);
  fput (stderr, "scanning = ", scan_time, " parsing = ", parse_time);
  fputln (stderr, " transl = ", trans_time, " checking = ", check_time,
	  " overall = ", clock () - start);
}

check_test (#argv ? argv [0] : "-");
