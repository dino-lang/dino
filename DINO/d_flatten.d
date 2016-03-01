// An utility reading a file and recursively substituting `#include
// "fname"` by the corresponding file content.  Additional directory
// besides the current one is used to find the file.

if (#argv != 2) {
  fputln (stderr, "Usage: <file> <additional dir>");
  exit (1);
}

var fname = argv[0], dir = argv[1], f = open (fname, "r"), streams = [], n = 1, ln;
for (;;) {
  for (; try (ln = fgetln (f), eof); n++) {
    rmatch (ln) {
    case `\s*#\s*include\s+"(.*)"`:
      var newf, newfname = ln [m[2]:m[3]];
      if (! try (newf = open (dir @ "/" @ newfname, "r"))
	  && ! try (newf = open (newfname, "r"))) {
	fputln (stderr, "error in open ", newfname); exit (1);
      }
      ins (streams, [f, n + 1, fname], -1);
      f = newf; fname = newfname; n = 0;
      putln (`#line 1 "`, fname, `"`);
    case _:
      putln (ln);
    }
  }
  close (f);
  if (#streams == 0)
    break;
  var [prevf, prevn, prevfname] = streams[#streams - 1];
  f = prevf; n = prevn; fname = prevfname;
  putln ("#line ", n, ` "`, fname, `"`);
  del (streams, #streams - 1);
}

exit (0);
