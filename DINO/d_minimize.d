// Copyright (C) 2015-2016 Vladimir Makarov.
// This is a script to minimize a header file necessary for C code
// generation of Dino functions.  It is done by repetitive removing
// declarations and checking that it is still a correct version by
// GCC.

// The script reads the file from stdin and put the result into stdout.

var code; // Current version of the header file.

// Return [begin, end] of last (N>=1) decls in the code ending BEGIN
// and starting with definition of DEF (if not nil).
fun get_decl (end, n, def = nil) {
  var level = 0, curr = 0, begin;

  for (var i = end; i >= 0; i--)
    if (level == 0 && (code[i] == ';' || code[i] == '}')) {
      begin = i;
      if (end != begin)
        curr++;
      if (def == nil && curr == n
          || def != nil && re.match ("\\A[^{]*" @ def, code[begin + 1:end+1]) != nil)
        return [begin + 1, end];
      if (code[i] == '}')
        level++;
    } else if (code[i] == '}') level++;
    else if (code[i] == '{') level--;
  return [-1, -1];
}

if (#argv != 1) {fputln (stderr, "Usage: <c-compiler> < in > out"); exit (0);}
val CC = argv[0];
code = new getf ();
val start_time = sys.time ();

// Find start of the code we should not change:
var start_invariant = get_decl (#code - 1, 1, "find_context_by_scope");
// fputln (stderr, "Invariant start: ", code[start_invariant[0]: start_invariant[1] + 1]);
// fputln (stderr, "--------------------------");
var pos, removed = 0, all = 0;
var tfnbase = "_dino_minimization_file.";
var tfname = tfnbase @ "c"; // file we use for the compilation
val factor = 2; // Factor to increase/decrease number of simultaneously processed decls
val dot_factor = 50; // How many processed decls for a dot in a progress line
var ncomps = 0;
for (var end = start_invariant[0] - 1;;) {
  for (var n = factor;;) { // We try to remove N decls at once to speed up the process
    pos = get_decl (end, n);
    if (pos[0] < 0) {
      if (n == 1)
        break;
      n /= factor; // Fail: decrease searched decls number
      continue;
    }
    var test = open (tfname, "w");
    fput (test, code[0:pos[0]], code[pos[1]+1:]); // Put all but decls found above
    close (test);
    var before = all / dot_factor;
    if (sys.system (CC @ " -S -Werror=implicit-function-declaration -Werror=implicit-int -Wfatal-errors "
                    @ tfname @ " 2>/dev/null")) {
      if (n != 1) n /= factor; // Fail: decrease searched decls number
      else {
        all++;
	//        fputln (stderr, "Keeping: ", code[pos[0]:pos[1]+1]);
        end = pos[0] - 1; // Don't remove the necessary last decl
        // Always try two decls first to exclude considering one
        // decl 'ident' in something like 'typedef struct ... {}
        // ident;'
        if (end >= 0 && code[end]==';') n = factor;
      }
    } else {
      //      fputln (stderr, "Removing: ", code[pos[0]:pos[1]+1]);
      // Remove unnecessary code and continue from the place right
      // before the removed code.
      del (code, pos[0], pos[1] - pos[0] + 1); end = pos[0] - 1;
      all += n; removed += n; n *= factor;  // Success: increase searched decls number
    }
    ncomps++;
    if (all / dot_factor > before)
      fput (stderr, ".");
  }
  if (pos[0] < 0)
    break; // We processed all
}
fputln (stderr);
remove (tfname);
try (remove (tfnbase @ "s"), except);
fputln (stderr, ncomps, " compilations for ", sys.time () - start_time,
	" sec: Removed ", removed, " decls out of ", all);
putln (code); // Output the result
