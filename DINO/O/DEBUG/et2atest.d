include "comm";
include "scanner";
include "parser";
include "et2at";

var tnodes = {};

func pr_indent (indent) {
  putln ();
  for (; indent != 0; indent--) put (' ');
}

func pr_tree_arr (arr, indent) {
  var i;

  put ("[");
  for (i = 0; i < #arr; i++) {
    if (i != 0) put (",");
    pr_tree (arr [i], indent + 5);
  }
  pr_indent (indent); put ("]");
}

func pr_tree (t, indent) {
  var i, c = class (t);

  if (c == nil) internal_error ("invalid syntax tree");
  if (t in tnodes) {
    put ("#", tnodes {t}); return;
  }
  pr_indent (indent);
  put (#tnodes, ": ");
  tnodes {t} = #tnodes;
  indent += 5; put (class_name (c));
  if (inside (t, class (decls))) {
    put (" ("); pr_indent (indent); put ("scope = ");
    if (t.scope != nil) pr_tree (t.scope, indent); else put ("nil");
    pr_indent (indent); put ("ident = "); put (t.ident.repr);
    if (inside (t, decls.module)) {
      if (#t.imports != 0) {
	pr_indent (indent); put ("imports = ");
	pr_tree_arr (t.imports, indent);
      }
      pr_indent (indent); put ("decls = "); pr_tree_arr (t.decls, indent);
      pr_indent (indent); put ("stmts = "); pr_tree_arr (t.stmts, indent);
    } else if (inside (t, decls.import)) {
      if (t.external != nil) {
	pr_indent (indent); put ("imports = "); pr_tree (t.external, indent);
      }
    } else if (inside (t, decls.par)) {
      pr_indent (indent); put (t.var_p ? "var " : ""); put ("tdef = ");
      pr_tree (t.tdef, indent);
    } else if (t == universe_scope) {
    } else {
      put (t.export ? "*" : "");
      if (inside (t, decls.const)) {
	pr_indent (indent); put ("expr = "); pr_tree (t.expr, indent);
      } else if (inside (t, decls.tdecl) || inside (t, decls.vdecl)
		 || inside (t, decls.field)) {
	pr_indent (indent); put ("tdef = "); pr_tree (t.tdef, indent);
      } else if (inside (t, decls.proc)) {
	put (t.hint != nil ? "@" : "");
	pr_indent (indent); put ("tdef = ");
	pr_tree (t.tdef, indent);
	if (t.decls != nil) {
	  pr_indent (indent); put ("decls = "); pr_tree_arr (t.decls, indent);
	}
	if (t.stmts != nil) {
	  pr_indent (indent); put ("stmts = "); pr_tree_arr (t.stmts, indent);
	}
      } else internal_error ("invalid declaration");
    }
    put (")");
  } else if (inside (t, class (tdefs))) {
    put (" (");
    if (inside (t, tdefs.qualid)) {
      put ("module_ident = ",
	   (t.module_ident == nil ? "nil" : t.module_ident.repr),
	   ", ident = ", t.ident.repr);
    } else if (inside (t, tdefs.arr)) {
      if (t.expr != nil) {
	pr_indent (indent); put ("expr = "); pr_tree (t.expr, indent);
      }
      pr_indent (indent); put ("tdef = "); pr_tree (t.tdef, indent);
    } else if (inside (t, tdefs.rec)) {
      if (t.base != nil) {
	pr_indent (indent); put ("base = "); pr_tree (t.base, indent);
      }
      pr_indent (indent); put ("fields = "); pr_tree_arr (t.fields, indent);
    } else if (inside (t, tdefs.ptr)) {
      pr_indent (indent); put ("tdef = "); pr_tree (t.tdef, indent);
    } else if (inside (t, tdefs.proc)) {
      pr_indent (indent); put ("pars = "); pr_tree_arr (t.pars, indent);
      if (t.tdef != nil) {
	pr_indent (indent); put ("tdef = "); pr_tree (t.tdef, indent);
      }
    } else internal_error ("invalid type");
    put (")");
  } else if (inside (t, class (stmts))) {
    if (inside (t, stmts.assign)) {
      put (" ("); pr_indent (indent); put ("des = "); pr_tree (t.des, indent);
      pr_indent (indent); put ("expr = "); pr_tree (t.expr, indent);
      put (")");
    } else if (inside (t, stmts.pcall)) {
      put (" ("); pr_indent (indent); put ("des = "); pr_tree (t.des, indent);
      pr_indent (indent); put ("actuals = "); pr_tree_arr (t.actuals, indent);
      put (")");
    } else if (inside (t, stmts.if_then)) {
      put (" ("); pr_indent (indent); put ("conds = [");
      for (i = 0; i < #t.conds; i += 2) {
	put (i == 0 ? "" : ", "); pr_tree (t.conds [i], indent); put (",");
	pr_indent (indent); pr_tree_arr (t.conds [i + 1], indent);
      }
      put ("]");
      if (#t.else_stmts != 0) {
	pr_indent (indent); put ("else_stmts = ");
	pr_tree_arr (t.else_stmts, indent);
      }
      put (")");
    } else if (inside (t, stmts.case)) {
      put (" ("); pr_indent (indent); put ("expr = ");
      pr_tree (t.expr, indent); pr_indent (indent); put ("cases = [");
      for (i = 0; i < #t.cases; i += 2) {
	if (i != 0) put (", ");
	pr_tree_arr (t.cases [i], indent); put (",");
	pr_indent (indent); pr_tree_arr (t.cases [i + 1], indent);
      }
      put ("]");
      if (#t.else_stmts != 0) {
	pr_indent (indent); put ("else_stmts = ");
	pr_tree_arr (t.else_stmts, indent);
      }
      put (")");
    } else if (inside (t, stmts.while) || inside (t, stmts.repeat)) {
      put (" ("); pr_indent (indent); put ("expr = ");
      pr_tree (t.expr, indent); pr_indent (indent); put ("stmts = ");
      pr_tree_arr (t.stmts, indent); put (")");
    } else if (inside (t, stmts.loop)) {
      put (" ("); pr_indent (indent); put ("stmts = ");
      pr_tree_arr (t.stmts, indent); put (")");
    } else if (inside (t, stmts.with)) {
      put (" ("); pr_indent (indent); put ("des = "); pr_tree (t.des, indent);
      pr_indent (indent); put ("tdef = "); pr_tree (t.tdef, indent);
      pr_indent (indent); put ("stmts = "); pr_tree_arr (t.stmts, indent);
      put (")");
    } else if (inside (t, stmts.ret)) {
      if (t.expr != nil) {
	put (" ("); pr_indent (indent); put ("expr = ");
	pr_tree (t.expr, indent); put (")");
      }
    } else if (!inside (t, stmts.exit)) internal_error ("invalid statement");
  } else if (inside (t, class (exprs))) {
    put (" (");
    if (inside (t, exprs.op2)) {
      pr_indent (indent); put ("left = "); pr_tree (t.left, indent);
      pr_indent (indent); put ("right = "); pr_tree (t.right, indent);
    } else if (inside (t, exprs.op1)) {
      pr_indent (indent); put ("op = "); pr_tree (t.op, indent);
    } else if (inside (t, exprs.number) || inside (t, exprs.chr)
	     || inside (t, exprs.str)) {
      put ("v = ", t.v.repr);
    } else if (inside (t, exprs.null)) {
    } else if (inside (t, exprs.set)) {
      pr_indent (indent); put ("ranges = [");
      for (i = 0; i < #t.ranges; i += 2) {
	pr_indent (indent); put ("["); pr_tree (t.ranges [i], indent + 5);
	put (", "); pr_tree (t.ranges [i + 1], indent + 5);
	put (i + 2 < #t.ranges ? "], " : "]");
      }
      put ("])");
    } else if (inside (t, exprs.fcall)) {
      pr_indent (indent); put ("des = "); pr_tree (t.des, indent);
      pr_indent (indent); put ("actuals = "); pr_tree_arr (t.actuals, indent);
    } else if (inside (t, class (dess))) {
      if (inside (t, dess.field)) {
	pr_indent (indent); put ("des = ");
	pr_tree (t.des, indent); pr_indent (indent);
	put ("ident = ", t.ident.repr);
      } else if (inside (t, dess.elem)) {
	pr_indent (indent); put ("des = "); pr_tree (t.des, indent);
	pr_indent (indent); put ("index = "); pr_tree (t.index, indent);
      } else if (inside (t, dess.ref)) {
	pr_indent (indent); put ("des = "); pr_tree (t.des, indent);
      } else if (inside (t, dess.guard)) {
	pr_indent (indent); put ("des = "); pr_tree (t.des, indent);
	pr_indent (indent); put ("tdef = "); pr_tree (t.tdef, indent);
      } else if (inside (t, dess.qualid)) {
	put ("module_ident = ",
	     (t.module_ident == nil ? "nil" : t.module_ident.repr),
	     ", ident = ", t.ident.repr);
      } else internal_error ("invalid designator");
    } else internal_error ("invalid expression");
    put (")");
  } else internal_error ("invalid syntax tree");
}

func parser_test (fname) { // read preprocessed C file and print lexs:
  var lexs, et, at, all = clock (), scan = scanner (fname);

  lexs = scan.get_lexs ();
  et = o2parse (grammar_fname (), lexs, scan.term_map);
  at = et2at (et); pr_tree (at, 0); putln ();
  fput (stderr, "scanning = ", scan_time, " parsing = ", parse_time);
  fputln (stderr, " transl = ", trans_time, " overall = ", clock () - all);
}

parser_test (#argv ? argv [0] : "-");
