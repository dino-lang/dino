include "comm";
include "scanner";
include "parser";
include "et2at";
include "check";
include "outsym";

func check_test (fname) { // read preprocessed C file and print lexs:
  var lexs, at, st, out_time, all = clock ();
  var fsname, scan = scanner (fname);

  lexs = scan.get_lexs ();
  at = o2parse (grammar_fname (), lexs, scan.term_map); st = et2at (at);
  check_st (st);
  out_time = clock ();
  fsname = "-";
  //  fsname = sub ("(\\.mod|\\.ob)$", fname, ".sym");
  if (fname == fsname && fname != "-")
    internal_error ("bad input file name suffix");
  output_syms (fsname, st);
  out_time = clock () - out_time;
  fput (stderr, "scanning=", scan_time, " parsing=", parse_time);
  fput (stderr, " transl=", trans_time, " checking=", check_time);
  fputln (stderr, " output=", out_time, " overall=", clock () - all);
}

check_test (#argv ? argv [0] : "-");
