// This file generates C code providing info necessary to work with
// unicode in DINO from Unicode database file.
if (#argv != 1) {fputln (stderr, "Usage: <command> <Unicode Data Base File>"); exit (1);}
var udbf;
try {
  udbf = open (argv[0], "r");
} catch (except) {
  fputln (stderr, "error: Can not open file ", argv[0]); exit (1);
}

// Read Unicode datab base file:
var lns = fgetf (udbf, 1);
var all = [];
for (var i = 0; i < #lns; i++) {
  var ln = lns[i];
  var fs = re.split (ln, ";");
  if (#fs != 15) {
    fputln (stderr, "Wrong number of fields on line ", i + 1);
    close (udbf); exit (1);
  }
  var c = int ("0x" @ fs[0]);
  var name = fs[1];
  var cat_str = fs[2];
  var up = #fs[12] == 0 ? c : int ("0x" @ fs[12]);
  var lo = #fs[13] == 0 ? c : int ("0x" @ fs[13]);
  var mid = #fs[14] == 0 ? c : int ("0x" @ fs[14]);
  ins (all, [c, name, cat_str, lo, up, mid], -1);
}

// Print header
putln ("/* This file has been generated from Unicode Database file. */");

// Print gategory definitions:
var cats = ["Cc", "Cf", "Co", "Cs", "Ll", "Lm", "Lo", "Lt", "Lu", "Mc", "Me",
            "Mn", "Nd", "Nl", "No", "Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps",
            "Sc", "Sk", "Sm", "So", "Zl", "Zp", "Zs"];
var cat_tab = tab [];
for (i = 0; i < #cats; i++) {
  cat_tab[cats[i]] = i + 1;
  putf ("#define UC_%s %d\n", cats[i], i + 1);
}

// Print descriptions:
put ("\nstruct ucode_desc {\n\
  unsigned int code:22;\n\
  unsigned int category: 5;\n\
  unsigned int type : 3;\n\
  union u {\n\
    int l : 22; /* lo_diff */\n\
    int u : 22; /* up_diff */\n\
    int mu : 22; /* mid_diff == up_diff */\n\
    struct {\n\
      int mid_diff : 11;\n\
      int up_diff : 11;\n\
    } mu2;\n\
    struct {\n\
      int lo_diff : 8;\n\
      int mid_diff : 8;\n\
      int up_diff : 8;\n\
    } lmu;\n\
  } u;\n\
};\n\
\n\
static const struct ucode_desc ucode_descs[] = {\n");

for (i = 0; i < #all; i++) {
  var [c, name, cat_str, lo, up, mid] = all[i];
  var cat = cat_str in cat_tab ? "UC_" @ cat_str : "0";
  putf ("{0x%X, %s, ", c, cat);
  if (mid == c && up == c) {
    putf ("%d, { .l = %d", 0, lo - c);
  } else if (mid == c && lo == c) {
    putf ("%d, { .u = %d", 1, up - c);
  } else if (lo == c) {
    if (mid != up)
      putf ("%d, { .mu2 = {%d, %d}", 3, mid - c, up - c);
    else
      putf ("%d, { .mu = %d", 2, mid - c);
  } else {
    putf ("%d, { .lmu = {%d, %d, %d}", 4, lo - c, mid - c, up - c);
  }
  putf (" } }, /* %s */\n", name); 
}
putln ("};");

// Create ranges: [start_index, finish_index] where
// finish_index - start_index + 1 <= 255.
if (all[0][0]) {fputf (stderr, "error: Database does not start from 0\n"); exit (1);}
var ranges = [], startc = 0, starti = 0, lastc = 0;
for (i = 0; i < #all; i++) {
  var c = all[i][0];
  if (c - startc + 1 > 255) { // new range
    ins (ranges, [#ranges, starti, startc, lastc], -1); startc = c; starti = i;
  }
  lastc = c;
}
ins (ranges, [#ranges, starti, startc, lastc], -1);

fun print_range (n, starti, startc, lastc) {
  var prevc = startc, name = "uc_ind" @ n;
  putf ("static const unsigned char %s [] = {\n", name);
  for (var i = starti;; i++) {
    var c = all[i][0];
    for (var j = 1; j < c - prevc; j++)
      putf ("  255,\n");
    putf ("  %d,\n", i - starti);
    if (c == lastc) break;
    prevc = c;
  }
  putf ("};\n\n");
  return name;
}

// Print tree:

putf ("struct uc_tree_node {\n\
  int start_index, start_code, last_code;\n\
  const unsigned char *uc_ind;\n\
  const struct uc_tree_node *l, *r;\n\
};\n\n");

fun print_tree (start, bound) {
  if (start >= bound) return nil;
  var m = (start + bound) / 2;
  var [n, starti, startc, lastc] = ranges[m];
  var l = print_tree (start, m);
  var r = print_tree (m + 1, bound);
  var node_name = (start == 0 && bound == #ranges
		   ? "uc_tree_root" : "uc_tree_node" @ m);
  var name = print_range (n, starti, startc, lastc);
  putf ("static const struct uc_tree_node %s = {%d, %d, %d, %s, %s, %s};\n",
	node_name, starti, startc, lastc, name,
        l == nil ? "0" : "&" @ l, r == nil ? "0" : "&" @ r);
  return node_name;
}

print_tree (0, #ranges);
// fprintln (stderr, ranges);

fputf (stderr, "codes=%d, ranges = %d, span=%d\n", #all, #ranges,
       fold (fun (o, r) {o + r[1] - r[0] + 1;}, ranges, 0));
close (udbf);
