// Output of checked trees.
include "ctree";
include "table";

var outf;

func mark_expr (e) {
  var i;

  if (inside (e, exprs.op1)) mark_expr (e.op);
  else if (inside (e, exprs.op2)) {
    mark_expr (e.left); mark_expr (e.right);
  } else if (inside (e, exprs.set)) {
    for (i = 0; i < #e.ranges; i+= 2) {
      if (e.ranges [i] !== e.ranges [i + 1]) mark_expr (e.ranges [i]);
      mark_expr (e.ranges [i + 1]);
    }
  } else if (inside (e, exprs.des)) {
    if (inside (e, dess.qualid))
      mark_decl (e.module_ident == nil
		 ? e.decl : decls.get (e.ident, curr_scope) /* import */);
    else {
      mark_expr (e.des);
      if (inside (e, dess.elem)) mark_expr (e.index);
      else  if (inside (e, dess.guard)) mark_tdef (e.tdef);
      else if (!inside (e, dess.field) && !inside (e, dess.ref))
	internal_error ("unknown designator");
    }
  } else if (!inside (e, exprs.null) && !inside (e, exprs.stdexpr)
	     && !inside (e, exprs.number) && !inside (e, exprs.chr)
	     && !inside (e, exprs.str) && !inside (e, exprs.fcall))
    internal_error ("unknown expr");
}

func mark_tdef (t) {
  if (inside (t, tdefs.qualid)) {
    mark_decl (t.module_ident == nil
	       ? t.decl : decls.get (t.ident, curr_scope) /* import */);
  } else if (inside (t, tdefs.arr)) {
    mark_tdef (t.tdef); if (t.expr != nil) mark_expr (t.expr);
  } else if (inside (t, tdefs.rec)) {
    mark_decls (t.fields); if (t.base != nil) mark_tdef (t.base);
  } else if (inside (t, tdefs.ptr)) mark_tdef (t.tdef);
  else if (inside (t, tdefs.proc)) {
    mark_decls (t.pars); if (t.tdef != nil) mark_tdef (t.tdef);
  } else if (!inside (t, tdefs.stdtype)) internal_error ("unknown type");
}

func mark_decls (da) { var i; for (i = 0; i < #da; i++) mark_decl (da [i]); }

func mark_decl (d) {
  if (d.out_sym_p) return;
  d.out_sym_p = 1;
  if (inside (d, decls.const)) mark_expr (d.expr);
  else if (inside (d, decls.tdecl) || inside (d, decls.vdecl)
	   || inside (d, decls.field) || inside (d, decls.par)
	   || inside (d, decls.proc)) mark_tdef (d.tdef);
  else if (!inside (d, decls.module) && !inside (d, decls.import))
    internal_error ("unknown decl");
}

func output_qualid (t) {
  if (t.module_ident != nil) fput (outf, t.module_ident.repr, ".");
  fput (outf, t.ident.repr);
}

func output_expr (e) {
  var i;

  if (inside (e, exprs.op1)) {
    if (inside (e, op1s.pos)) fput (outf, "+");
    else if (inside (e, op1s.neg)) fput (outf, "-");
    else if (inside (e, op1s.not)) fput (outf, "~");
    fput (outf, " "); output_expr (e.op); 
  } else if (inside (e, exprs.op2)) {
    output_expr (e.left); fput (outf, " ");
    if (inside (e, op2s.eq)) fput (outf, "=");
    else if (inside (e, op2s.ne)) fput (outf, "#");
    else if (inside (e, op2s.lt)) fput (outf, "<");
    else if (inside (e, op2s.le)) fput (outf, "<=");
    else if (inside (e, op2s.gt)) fput (outf, ">");
    else if (inside (e, op2s.ge)) fput (outf, ">=");
    else if (inside (e, op2s.set_in)) fput (outf, "IN");
    else if (inside (e, op2s.is)) fput (outf, "IS");
    else if (inside (e, op2s.plus)) fput (outf, "+");
    else if (inside (e, op2s.minus)) fput (outf, "-");
    else if (inside (e, op2s.or)) fput (outf, "OR");
    else if (inside (e, op2s.and)) fput (outf, "AND");
    else if (inside (e, op2s.mult)) fput (outf, "*");
    else if (inside (e, op2s.rdiv)) fput (outf, "/");
    else if (inside (e, op2s.div)) fput (outf, "DIV");
    else if (inside (e, op2s.mod)) fput (outf, "MOD");
    fput (outf, " "); output_expr (e.right);
  } else if (inside (e, exprs.null)) fput (outf, "NIL");
  else if (inside (e, exprs.number) || inside (e, exprs.chr)
	   || inside (e, exprs.str)) fput (outf, e.v.repr);
  else if (inside (e, exprs.set)) {
    fput (outf, "{ ");
    for (i = 0; i < #e.ranges; i+= 2) {
      if (i != 0) fput (outf, ", ");
      if (e.ranges [i] !== e.ranges [i + 1]) {
	output_expr (e.ranges [i]); fput (outf, "..");
      }
      output_expr (e.ranges [i + 1]);
    }
    fput (outf, " }");
  } else if (inside (e, exprs.des)) {
    if (inside (e, dess.qualid)) output_qualid (e);
    else {
      output_expr (e.des);
      if (inside (e, dess.field)) fput (outf, ".", e.ident.repr);
      else if (inside (e, dess.ref)) fput (outf, "^");
      else if (inside (e, dess.elem)) {
        fput (outf, " ["); output_expr (e.index); fput (outf, "]"); 
      } else  if (inside (e, dess.guard)) {
        fput (outf, " ("); output_tdef (e.tdef); fput (outf, ")"); 
      } else internal_error ("unknown designator");
    }
  } else if (!inside (e, exprs.stdexpr) && !inside (e, exprs.fcall))
    internal_error ("unknown expr");
}

func output_proc (t, name = "") {
  fput (outf, "  PROCEDURE ", name, " (");
  output_decls (t.pars); fput (outf, ")");
  if (t.tdef != nil) { fput (outf, " : "); output_tdef (t.tdef); }
}

func output_tdef (t) {
  if (inside (t, tdefs.qualid)) output_qualid (t);
  else if (inside (t, tdefs.arr)) {
    fput (outf, "ARRAY ");
    if (t.expr != nil) {
      output_expr (t.expr); fput (outf, " ");
    }
    fput (outf, "OF "); output_tdef (t.tdef);
  } else if (inside (t, tdefs.rec)) {
    fput (outf, "RECORD ");
    if (t.base != nil) {
      fput (outf, "("); output_tdef (t.base); fput (outf, ") "); 
    }
    output_decls (t.fields); fput (outf, " END");
  } else if (inside (t, tdefs.ptr)) {
    fput (outf, "POINTER TO "); output_tdef (t.tdef);
  } else if (inside (t, tdefs.proc)) output_proc (t);
  else if (!inside (t, tdefs.stdtype)) internal_error ("unknown type");
}

func output_decls (da) {
  var i, d, nd, pd, dea = [];
  
  for (i = 0; i < #da; i++) if (da [i].out_sym_p) ins (dea, da [i], -1);
  da = dea; pd = nil;
  for (i = 0; i < #da; i++) {
    d = da [i]; nd = i + 1 >= #da ? nil : da [i + 1];
    if (inside (d, decls.import)) {
      fput (outf, pd != nil && inside (d, class (pd)) ? ", " : "  IMPORT ");
      fput (outf, d.ident.repr,
	    d.external != nil ? " := " @ d.external.repr : "");
      if (nd == nil || !inside (nd, decls.import))
	fputln (outf, ";");
    } else if (inside (d, decls.const)) {
      fput (outf, "  CONST ", d.ident.repr, d.export ? "* = " : " = ");
      output_expr (d.expr); fputln (outf, ";");
    } else if (inside (d, decls.tdecl)) {
      fput (outf, "  TYPE ", d.ident.repr, d.export ? "* = " : " = ");
      output_tdef (d.tdef); fputln (outf, ";");
    } else if (inside (d, decls.vdecl) || inside (d, decls.field)
	       || inside (d, decls.par)) {
      if (pd != nil && inside (d, class (pd)) && pd.tdef === d.tdef)
	fput (outf, ", ");
      else if (inside (d, decls.vdecl)) fput (outf, "  VAR ");
      else if (inside (d, decls.par) && d.var_p) fput (outf, " VAR ");
      fput (outf, d.ident.repr,
	    !inside (d, decls.par) && d.export ? "*" : "");
      if (nd == nil || !inside (nd, class (d)) || nd.tdef !== d.tdef) {
	fput (outf, " : "); output_tdef (d.tdef);
	if (inside (d, decls.field) || inside (d, decls.par)) {
	  if (nd != nil) fput (outf, "; ");
	} else fputln (outf, ";");
      }
    } else if (inside (d, decls.proc)) {
      output_proc (d.tdef, d.ident.repr @ (d.export ? "*" : ""));
      fputln (outf, ";");
    } else if (!inside (d, decls.module)) internal_error ("unknown decl");
    pd = d;
  }
}

func output_syms (fname, m) {
  var i;

  outf = fname == "-" ? stdout : open (fname, "w");
  if (diags.nerr != 0) return;
  curr_scope = m;
  for (i = 0; i < #m.decls; i++)
    if (m.decls [i].export) mark_decl (m.decls [i]);
  fputln (outf, "MODULE ", m.ident.repr, ";");
  output_decls (m.imports); output_decls (m.decls);
  fputln (outf, "END ", m.ident.repr, ".");
}
