// Compiler diagnostic support.
class diag () {
  var nerr = 0;
  var nwarn = 0;

  private ppos, output;
  func ppos (fname, lno, pos) {
    fput (stderr, fname, ":", lno, ':');
    if (pos >= 0)
      fput (stderr, pos, ':');
  }
  func output (pars) {
    var i;
    for (i = 0; i < #pars; i++)
      fput (stderr, pars [i]);
    fputln (stderr);
  }
  func error (fname, lno, pos, ...) {
    ppos (fname, lno, pos); output (args); nerr++;
  }
  func fatal (fname, lno, pos, ...) {
    ppos (fname, lno, pos); output (args); exit (1);
  }
  func warn (fname, lno, pos, ...) {
    ppos (fname, lno, pos); output (args); nwarn++;
  }
}

var diags = diag ();

func internal_error (str) {
  if (str != nil)
    str = "internal error:" @ str;
  else
    str = "internal error";
  throw excepts.error (str);
}
