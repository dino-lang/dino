// Compiler tables.
// module system ???

include "atree";

class decl_table () {
  var decl_table = {};  // key is [ident.repr, scope]
  
  func get (ident, scope) {
    var key = [ident.repr, scope]; // remember that repr exists in one exemplar
    
    return (key in decl_table ? decl_table {key} : nil);
  }
  
  func find (ident, scope) {
    var decl;

    for (;;) {
      decl = get (ident, scope);
      if (decl != nil) return decl;
      else if (scope == nil) return nil;
      scope = scope.scope;
    }
  }

  func insert (decl, change) {
    var key = [decl.ident.repr, decl.scope];
    
    if (key in decl_table) {
      var d = decl_table {key};

      if (change) decl_table {key} = decl;
      return d;
    } else { decl_table {key} = decl; return decl; }
  }
}

var decltab = decl_table ();

var final universe_name = "<universe>";
func universe_id (name) { return lex (0, 0, universe_name, name).ident (); }

var final universe_scope = nodes.decl (universe_id (universe_name), nil);
var final modules_scope = universe_scope, curr_scope = modules_scope;

var final abs_id = universe_id ("ABS");
var final ash_id = universe_id ("ASH");
var final boolean_id = universe_id ("BOOLEAN");
var final cap_id = universe_id ("CAP");
var final char_id = universe_id ("CHAR");
var final chr_id = universe_id ("CHR");
var final copy_id = universe_id ("COPY");
var final dec_id = universe_id ("DEC");
var final entier_id = universe_id ("ENTIER");
var final excl_id = universe_id ("EXCL");
var final false_id = universe_id ("FALSE");
var final halt_id = universe_id ("HALT");
var final inc_id = universe_id ("INC");
var final incl_id = universe_id ("INCL");
var final integer_id = universe_id ("INTEGER");
var final len_id = universe_id ("LEN");
var final long_id = universe_id ("LONG");
var final longint_id = universe_id ("LONGINT");
var final longreal_id = universe_id ("LONGREAL");
var final max_id = universe_id ("MAX");
var final min_id = universe_id ("MIN");
var final new_id = universe_id ("NEW");
var final nil_id = universe_id ("NIL");
var final odd_id = universe_id ("ODD");
var final ord_id = universe_id ("ORD");
var final real_id = universe_id ("REAL");
var final set_id = universe_id ("SET");
var final short_id = universe_id ("SHORT");
var final shortint_id = universe_id ("SHORTINT");
var final size_id = universe_id ("SIZE");
var final true_id = universe_id ("TRUE");

var final system_id = universe_id ("SYSTEM");

var _uniq_num = 0;
func _unum () { _uniq_num++; return _uniq_num; }
func _nst () { _uniq_num++; return tdefs.stdtype (_unum ()); }

var final t_boolean = _nst (), final t_char = _nst ();
var final t_integer = _nst (), final t_longint = _nst ();
var final t_longreal = _nst (), final t_nil = _nst ();
var final t_real = _nst (), final t_set = _nst ();
var final t_shortint = _nst ();

var final t_abs = _nst (), final t_ash = _nst (), final t_cap = _nst (),
    final t_chr = _nst (), final t_copy = _nst (), final t_dec = _nst (),
    final t_entier = _nst (), final t_excl = _nst (), final t_halt = _nst (),
    final t_inc = _nst (), final t_incl = _nst (), final t_len = _nst (),
    final t_long = _nst (), final t_max = _nst (), final t_min = _nst (),
    final t_new = _nst (), final t_odd = _nst (), final t_ord = _nst (),
    final t_short = _nst (), final t_size = _nst ();

var final v_false = exprs.stdexpr (_unum ());
var final v_true = exprs.stdexpr (_unum ());

var _ins = decltab.insert, _decl = nodes.decl;

var final d_abs = _ins (_decl (abs_id).proc (0, nil, t_abs), 0);
var final d_ash = _ins (_decl (ash_id).proc (0, nil, t_ash), 0);
var final d_boolean = _ins (_decl (boolean_id).tdecl (0, t_boolean), 0);
var final d_cap = _ins (_decl (cap_id).proc (0, nil, t_cap), 0);
var final d_char = _ins (_decl (char_id).tdecl (0, t_char), 0);
var final d_chr = _ins (_decl (chr_id).proc (0, nil, t_chr), 0);
var final d_copy = _ins (_decl (copy_id).proc (0, nil, t_copy), 0);
var final d_dec = _ins (_decl (dec_id).proc (0, nil, t_dec), 0);
var final d_entier = _ins (_decl (entier_id).proc (0, nil, t_entier), 0);
var final d_excl = _ins (_decl (excl_id).proc (0, nil, t_excl), 0);
var final d_false = _ins (_decl (false_id).const (0, v_false), 0);
var final d_halt = _ins (_decl (halt_id).proc (0, nil, t_halt), 0);
var final d_inc = _ins (_decl (inc_id).proc (0, nil, t_inc), 0);
var final d_incl = _ins (_decl (incl_id).proc (0, nil, t_incl), 0);
var final d_integer = _ins (_decl (integer_id).tdecl (0, t_integer), 0);
var final d_len = _ins (_decl (len_id).proc (0, nil, t_len), 0);
var final d_long = _ins (_decl (long_id).proc (0, nil, t_long), 0);
var final d_longint = _ins (_decl (longint_id).tdecl (0, t_longint), 0);
var final d_longreal = _ins (_decl (longreal_id).tdecl (0, t_longreal), 0);
var final d_max = _ins (_decl (max_id).proc (0, nil, t_max), 0);
var final d_min = _ins (_decl (min_id).proc (0, nil, t_min), 0);
var final d_new = _ins (_decl (new_id).proc (0, nil, t_new), 0);
var final d_nil = _ins (_decl (nil_id).tdecl (0, t_nil), 0);
var final d_odd = _ins (_decl (odd_id).proc (0, nil, t_odd), 0);
var final d_ord = _ins (_decl (ord_id).proc (0, nil, t_ord), 0);
var final d_real = _ins (_decl (real_id).tdecl (0, t_real), 0);
var final d_set = _ins (_decl (set_id).tdecl (0, t_set), 0);
var final d_short = _ins (_decl (short_id).proc (0, nil, t_short), 0);
var final d_shortint = _ins (_decl (shortint_id).tdecl (0, t_shortint), 0);
var final d_size = _ins (_decl (size_id).proc (0, nil, t_size), 0);
var final d_true = _ins (_decl (true_id).const (0, v_true), 0);
