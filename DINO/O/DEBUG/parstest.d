include "comm";
include "scanner";
include "parser";

func pr (r, ind) {
  var i, n = r.name;

  for (i = 0; i < ind; i++)
    put (" ");
  put (n);
  if (n == "$term")
    putln ("(", r.transl.lex.code, ", ", r.transl.lex.repr, ")");
  else if (n == "$error" || n == "$nil")
    putln ();
  else {
    putln ("(");
    for (i = 0; i < #r.transl; i++)
       pr (r.transl [i], ind + 2);
    for (i = 0; i < ind; i++)
      put (" ");
    putln (")");
  }
}

func parser_test (fname) { // read preprocessed C file and print lexs:
  var lexs, f1, f2, at, all = clock (), scan = scanner (fname);

  lexs = scan.get_lexs ();
  at = o2parse (grammar_fname (), lexs, scan.term_map);
  pr (at, 0);  
  fputln (stderr, "scanning = ", scan_time, " parsing = ", parse_time,
	  " overall = ", clock () - all);
}

parser_test (#argv ? argv [0] : "-");
