include "scandeb";

func scanner_test (fname) { // read preprocessed C file and print lexs:
  var lexs, f1, f2, start;
  var scan = scanner (fname), fn1 = "__a.l", fn2 = "__b.l";

  fput (stderr, "scanning..."); start = clock ();
  lexs = scan.get_lexs ();
  fputln (stderr, "done (", clock () - start, ")"); 
  fput (stderr, "writing..."); start = clock ();
  scan.write (fn1, lexs);
  fputln (stderr, "done (", clock () - start, ")");
  scan.write ("-", lexs);
  fput (stderr, "reading..."); start = clock ();
  lexs = scan.read (fn1);
  fputln (stderr, "done (", clock () - start, ")");
  fput (stderr, "writing..."); start = clock ();
  scan.write (fn2, lexs);
  fputln (stderr, "done (", clock () - start, ")");
  fput (stderr, "comparing..."); start = clock ();
  f1 = open (fn1, "r"); f2 = open (fn2, "r");
  if (fgetf (f1) != fgetf (f2))
    internal_error ("different files " @ fn1 @ " and " @ fn2);
  fputln (stderr, "done (", clock () - start, ")");
  close (f1); close (f2);
  remove (fn1); remove (fn2);
}

scanner_test (#argv ? argv [0] : "-");
