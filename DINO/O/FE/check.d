// check public, private fields
// Decrease number of lines e -> expr, v ->

// Checking context of the program.

include "comm";
include "ctree";
include "table";
include "error";

func is_int (t) {
  return t === t_integer || t === t_shortint || t === t_longint;
}
func is_real (t) {return t === t_real || t === t_longreal;}
func is_numeric (t) {return is_int (t) || is_real (t);}
func is_string (t) {
  return (t != nil && t.tp != nil && inside (t.tp, tdefs.arr)
	  && t.tdef.tp === t_char);
}
func str_char (l) {return l != nil && inside (l, lexs.str) && #l.str == 1;}
func is_of_char (e) {return e != nil && e.tp === t_char || str_char (e.lex);}

var final rank = {t_shortint : 0, t_integer : 1, t_longint : 2,
		  t_real : 3, t_longreal : 4};

func type_included (t0, t1) {
  return (is_numeric (t0) && is_numeric (t1) && rank {t0} <= rank {t1});
}

func type_extended (t0, t1, ptr_p) {
  if (ptr_p && inside (t1, tdefs.ptr))
    return type_extended (t0, t1.tdef.tp, 0);
  if (!inside (t0, tdefs.rec) || !inside (t1, tdefs.rec))
    return 0;
  for (; t0 != nil; t0 = t0.base == nil ? nil : t0.base.tp)
    if (t0 === t1) return 1;
  return 0;
}

func common_numeric_type (t1, t2) {return type_included (t1, t2) ? t2 : t1;}

func not_bool (t) {return t != nil && t !== t_boolean;} 
func not_int (t) {return t != nil && !is_int (t);} 
func not_real (t) {return t != nil && !is_real (t);} 
func not_numeric (t) {return t != nil && !is_numeric (t);} 
func not_numeric_set (t) {return t != nil && !is_numeric (t) && t !== t_set;}

func eq_type (t1, t2) {
  var i, p1, p2, pt1, pt2;

  if (t1 === t2 || t1 === t_nil && t2 != nil
      && (inside (t2, tdefs.ptr) || inside (t2, tdefs.proc))
      || t2 === t_nil && t1 != nil
      && (inside (t1, tdefs.ptr) || inside (t1, tdefs.proc))) return 1;
  else if (t1 != nil && inside (t1, tdefs.proc)
	   && t2 != nil && inside (t2, tdefs.proc)) {
    if (t1.tdef.tp !== t2.tdef.tp || #t1.pars != #t2.pars) return 0;
    for (i = 0; i < #t1.pars; i++) {
      p1 = t1.pars [i]; p2 = t2.pars [i]; pt1 = p1.tdef; pt2 = p2.tdef;
      if (pt1 != nil && inside (pt1, tdefs.arr) && pt1.expr == nil)
	pt1 = pt1.tdef;
      if (pt2 != nil && inside (pt2, tdefs.arr) && pt2.expr == nil)
	pt2 = pt2.tdef;
      if (p1.var_p != p2.var_p || !eq_type (pt1.tp, pt2.tp)) return 0;
    }
    return 1;
  } else return 0;
}

func cerr (t, msg, attr) {
  diags.error (t.fname, t.lno, t.pos, msg, attr == nil ? "" : attr);
}

func mpi2long (v) {return mpis.change_size (v, mach.li_size);}

func mpi2int (i, v) {
  return (v.size == mach.si_size ? i.si (v)
	  : v.size == mach.i_size ? i.i (v) : i.li (v));
}

func check_decls (da) { var i; for (i = 0; i < #da; i++) check_decl (da [i]); }

var ret_p;

func scope_in (s, up) {
  for (; s != nil; s = s.scope)
    if (s == up) return 1;
  return 0;
}

var prev_tdef = nil;

func check_decl (t) {
  var save, b, d;

  if (inside (t, decls.module)) {
    check_decls (t.imports); curr_scope = t;
    check_decls (t.decls); check_stmts (t.stmts); curr_scope = t.scope;
  } else if (inside (t, decls.import)) {
    d = find_module (t.external != nil ? t.external : t.ident);
    if (d == nil) internal_error (class_name (class (t)) @ " in check_decl");
    check_decl (d); // check the imported module
  } else if (inside (t, decls.const)) {
    check_expr (t.expr);
    if (t.expr != nil && t.expr.lex == nil)
      cerr (t, "non-constant value: ", t.ident.repr);
  } else if (inside (t, decls.tdecl) || inside (t, decls.vdecl)
	     || inside (t, decls.field) || inside (t, decls.par)) {
    check_tdef (t.tdef);
    if (t.tdef !== prev_tdef) { // not to report many errors for 'v, u : type'
      prev_tdef = t.tdef;
      if (t.tdef.tp != nil && !inside (t.tdef.tp, nodes.tdef))
	cerr (t.tdef, "non-type for ", t.ident.repr);
      else if (t.tdef.tp != nil && inside (t, decls.field)) {
	if (inside (t.tdef.tp, tdefs.rec) && scope_in (t.scope, t.tdef.tp)) {
	  t.tdef.tp = nil; cerr (t.tdef, "cycle in type of ", t.ident.repr);
	}
      }
    }
    if (t.tdef.tp != nil && inside (t, decls.field)) {
      for (b = t.scope;;) {
	b = (b.base == nil ? nil : b.base.tp);
	if (b == nil) break;
	else if (decltab.get (t.ident, b) != nil) {
	  cerr (t.ident, t.ident.repr, " already is defined in a base type");
	  t.tdef.tp = nil; break;
	}
      }
    }
  } else if (inside (t, decls.proc)) {
    curr_scope = t; save = ret_p; check_tdef (t.tdef);
    if (t.stmts != nil) { // non forward declaration
      if (t.forward != nil && !eq_type (t.tdef, t.forward.tdef))
	cerr (t, "different forward and proc decl", t.ident.repr);
      check_decls (t.decls); ret_p = 0; check_stmts (t.stmts);
      curr_scope = t.scope;
      if (!ret_p && t.tdef != nil && t.tdef.tdef != nil)
	cerr (t, "func has no return");
      ret_p = save;
    }
  } else internal_error (class_name (class (t)) @ " in check_decl");
}

func find_qualid (q, check_p = 1) {
  var d;

  if (q.module_ident == nil) d = decltab.find (q.ident, curr_scope);
  else {
    d = find_module (q.module_ident);
    if (d != nil) d = decltab.get (q.ident, d);
  }
  if (d == nil) cerr (q, "undefined identfifier ", q.ident.repr);
  else if (check_p && usage_before_decl (q, d)) {
    d = nil; cerr (q, "usage before declaration: ", q.ident.repr);
  }
  return d;
}

func check_tdef_qualid (t, ptr_p = 1) {
  var d = find_qualid (t, !ptr_p);

  if (d != nil && !inside (d, decls.tdecl))
    cerr (t, "non-type in type def: ", t.ident.repr);
  else if (d != nil)
    t.tp = (inside (d.tdef, tdefs.stdtype)
	    || ptr_p && !inside (d.tdef, tdefs.qualid) ? d.tdef : d.tdef.tp);
  t.decl = d;
}

func check_tdef (t) {
  var tdef, i, d, l;

  if (inside (t, tdefs.qualid)) check_tdef_qualid (t);
  else if (inside (t, tdefs.arr)) {
    if (t.expr != nil) {
      check_expr (t.expr); tdef = t.expr.tp; l = t.expr.lex;
    }
    if (not_int (tdef) || tdef != nil && l == nil)
      cerr (t.expr, "non-integer constant length");
    else if (t.expr != nil)
      t.expr.lex = lex (l.lno, l.pos, l.fname, l.repr,
			l.code).integer ().li (mpi2long (l.v));
    check_tdef (t.tdef); t.tp = t;
  } else if (inside (t, tdefs.rec)) {
    t.tp = t;
    if (t.base != nil) {
      check_tdef (t.base);
      if (t.base.tp != nil && !inside (t.base.tp, tdefs.rec)) {
	t.base.tp = nil; cerr (t.base, "base type is not record");
      } else if (t === t.base.tp) {
	t.base.tp = nil; cerr (t.base, "cycle in record base type");
      }
    }
    for (i = 0; i < #t.fields; i++) check_decl (t.fields [i]);
  } else if (inside (t, tdefs.ptr)) {
    if (inside (t.tdef, tdefs.qualid)) check_tdef_qualid (t.tdef, 1);
    else check_tdef (t.tdef);
    tdef = t.tdef.tp;
    if (tdef != nil && !inside (tdef, tdefs.arr) && !inside (tdef, tdefs.rec))
      cerr (t, "invalid pointer base type");
    t.tp = t;
  } else if (inside (t, tdefs.proc)) {
    check_decls (t.pars); if (t.tdef != nil) check_tdef (t.tdef); t.tp = t;
  } else internal_error (class_name (class (t)) @ " in check_tdef");
}

func check_stmts (t) {var i; for (i = 0; i < #t; i++) check_stmt (t[i]);}

func not_assign_compatible (e, vt) {
  return (e.tp != nil && vt != nil && e.tp != vt && !type_included (e.tp, vt)
	  && !type_extended (e.tp, vt, 1)
	  && (e.tp !== t_nil
	      || !inside (vt, tdefs.ptr) && !inside (vt, tdefs.proc))
	  && (vt != t_char || !str_char (e.lex))
	  && (!inside (e, dess.qualid) || e.decl == nil
	      || !inside (e.decl, decls.proc)
	      || !inside (vt, tdefs.proc) || !eq_type (vt, e.decl.tdef.tp)
	      || e.decl.scope == nil || !inside (e.decl.scope, decls.module))
	  && (!inside (e.tp, tdefs.arr) || e.tp.expr.lex == nil
	      || !inside (e.tp.expr.lex, lexs.integer)
	      || !inside (vt, tdefs.arr) || vt.expr.lex == nil
	      || !inside (vt.expr.lex, lexs.integer)
	      || !mpis.lt (e.tp.expr.lex.v, vt.expr.lex.v)));
}

func check_std_call (c, func_p) { // ???
  var tp = c.des.tp;

  if (tp == t_inc || tp == t_dec) {
    if (#c.actuals != 1 && #c.actuals != 2)
      cerr (c, "invalid num of args of ", tp == t_inc ? "INC" : "DEC");
    else {
      check_expr (c.actuals [0]);
      if (!inside (c.actuals [0], exprs.des))
	cerr (c, "1st arg is not designator - ", tp == t_inc ? "INC" : "DEC");
      else if (not_int (c.actuals [0].tp))
	cerr (c, "non-integer var - ", tp == t_inc ? "INC" : "DEC");
      else if (#c.actuals == 2) {
	check_expr (c.actuals [1]);
	if (not_int (c.actuals [1].tp))
	  cerr (c, "non-integer 2nd arg - ", tp == t_inc ? "INC" : "DEC");
	else if (common_numeric_type (c.actuals [0].tp, c.actuals [1].tp)
		 != c.actuals [0].tp) cerr (c, "incr/dec too big for var");
      }
    }
  } else cerr (c, "calling non-", func_p ? "func" : "proc");
}

func check_call (c, func_p) {
  var i, tp, a, f, at, ft;

  check_des (c.des);
  if (c.des.tp != nil && inside (c.des.tp, tdefs.stdtype)) {
    check_std_call (c, func_p);
    return;
  }
  if (c.des.tp != nil) {
    if (!inside (c.des.tp, tdefs.proc))
      cerr (c, "calling non-", func_p ? "func" : "proc");
    else if (c.des.tp.tdef == nil && func_p) cerr (c, "proc call in expr");
    else if (c.des.tp.tdef != nil && !func_p) cerr (c, "func call as stmt");
    else tp = c.des.tp;
  }
  if (tp != nil && #c.actuals != #tp.pars) {
    cerr (c, "incorrect parameters number"); tp = nil;
  }
  for (i = 0; i < #c.actuals; i++) {
    a = c.actuals [i]; check_expr (a);
    if (tp != nil) {
      f = tp.pars [i]; at = a.tp; ft = f.tdef.tp;
      if (at != nil && ft != nil
	  && (f.var_p && !type_extended (at, ft, 0)
	      || !f.var_p && not_assign_compatible (at, ft)
	      && (!inside (at, tdefs.arr) || !inside (ft, tdefs.arr)
		  || ft.expr != nil || at.tdef.tp !== ft.tdef.tp)))
	cerr (a, "incorrect actual param value (type)");
    }
  }
}

func check_ext_des (d, et) {
  var vt = d.tp, err = 0;

  if (vt != nil && (!inside (vt, tdefs.ptr) || vt.tdef.tp != nil
		    && !inside (vt.tdef.tp, tdefs.rec))
      && (!inside (d, dess.qualid) || d.decl != nil
	  && (!inside (d.decl, decls.par) || !d.decl.var_p
	      || !inside (vt, tdefs.rec)))) {
    cerr (d, "invalid designator in IS or type guard"); err = 1;
  }
  if (vt != nil && et != nil && !type_extended (et, vt, 1)) {
    cerr (d, "it is not extension of the type"); err = 1;
  }
  return err;
}

var case_tp; // current type of case labels.

func case_lab_cmp (c1, c2) {
  if (case_tp == t_char) return c1.min - c2.min;
  else if (mpis.lt (c1.min, c2.min)) return -1;
  else if (mpis.gt (c1.min, c2.min)) return 1;
  else return 0;
}

var loop_level = 0;

func check_stmt (t) {
  var i, j, k, min, max, r, tp, tp1, tp2, v, v1, v2, cv, err;

  if (inside (t, stmts.assign)) {
    check_expr (t.des); check_expr (t.expr);
    if (t.des.lex != nil) cerr (t, "assignment to non-variable");
    else if (not_assign_compatible (t.expr, t.des.tp))
      cerr (t, "bad assignment value (type)");
  } else if (inside (t, stmts.pcall)) {
    check_call (t, 0);
  } else if (inside (t, stmts.if_then)) {
    for (i = 0; i < #t.conds; i += 2) {
      check_expr (t.conds [i]);
      if (not_bool (t.conds [i].tp)) cerr (t, "invalid type of if-expr");
      check_stmts (t.conds [i + 1]);
    }
    check_stmts (t.else_stmts);
  } else if (inside (t, stmts.case)) {
    check_expr (t.expr); tp = t.expr.tp; cv = {};
    if (not_int (tp) && !is_of_char (t.expr)) {
      cerr (t, "invalid type of case-expr"); tp = nil;
    }
    t.case_labs = [];
    for (i = 0; i < #t.cases; i += 2) {
      r = t.cases [i];
      for (j = 0; j < #r; j += 2) {
        min = r [j]; max = r [j + 1]; check_expr (min); check_expr (max);
	tp1 = min.tp; tp2 = max.tp;
	if (tp == nil && not_int (tp1) && !is_of_char (min)
	    || is_of_char (t.expr) && !is_of_char (min)
            || is_int (tp) && !is_int (tp1))
	  cerr (min, "invalid type of case-value");
	else if (tp == nil && not_int (tp2) && !is_of_char (max)
		 || is_of_char (t.expr) && !is_of_char (max)
		 || is_int (tp) && !is_int (tp2))
	  cerr (max, "invalid type of case-value");
	else if (tp == nil) {
	  if (min.lex == nil) cerr (min, "non const case-value");
	  else if (max.lex == nil)
	    cerr (max, "non const case-value");
	} else {
	  if (is_of_char (t.expr)) {
	    v1 = (tp1 === t_char ? min.lex.ch_code : min.lex.str [0]);
	    v2 = (tp2 === t_char ? max.lex.ch_code : max.lex.str [0]);
	    if (v1 > v2) cerr (min, "lower bound > upper bound");
	    else ins (t.case_labs, node (max.fname, max.lno, max.pos)
		      .case_lab (v1, v2, t.cases [i + 1]), -1);
	  } else {
	    v1 = mpi2long (min.lex.v); v2 = mpi2long (max.lex.v);
	    if (mpis.gt (v1, v2)) cerr (min, "lower bound > upper bound");
	    else ins (t.case_labs, node (max.fname, max.lno, max.pos)
		      .case_lab (v1, v2, t.cases [i + 1]), -1);
	  }
	  case_tp = tp; t.case_labs = sort (t.case_labs, case_lab_cmp);
          for (k = 0; k < #t.case_labs; k++)
            if (k != 0) {
              if (is_of_char (t.expr)
	          && t.case_labs [k - 1].max >= t.case_labs [k].min)
		cerr (t.case_labs [k - 1], "repeated case-value \"",
		      t.case_labs [k - 1].max @ "\"");
              else if (is_int (tp) && mpis.ge (t.case_labs [k - 1].max,
					       t.case_labs [k].min))
		cerr (t.case_labs [k - 1], "repeated case-value ",
		      mpis.to_string (t.case_labs [k - 1].max));
            }
	}
	check_stmts (t.cases [i + 1]);
      }
    }
    check_stmts (t.else_stmts);
  } else if (inside (t, stmts.while) || inside (t, stmts.repeat)) {
    check_expr (t.expr);
    if (not_bool (t.expr.tp))
      cerr (t, (inside (t, stmts.while) ? "invalid type of while-expr"
		: "invalid type of repeat-expr"));
    loop_level++; check_stmts (t.stmts); loop_level--;
  } else if (inside (t, stmts.loop)) {
    loop_level++; check_stmts (t.stmts); loop_level--;
  } else if (inside (t, stmts.with)) {
    check_des (t.des); check_tdef (t.tdef);
    err = check_ext_des (t.des, t.tdef.tp);
    if (!err && t.des.decl != nil) {
      tp = t.des.decl.with_tp; t.des.decl.with_tp = t.tdef.tp;
    }
    check_stmts (t.stmts);
    if (!err && t.des.decl != nil) t.des.decl.with_tp = tp;
  } else if (inside (t, stmts.exit)) {
    if (loop_level == 0) cerr (t, "exit not in a loop");
  } else if (inside (t, stmts.ret)) {
    ret_p = 1;
    if (!inside (curr_scope, decls.proc)) cerr (t, "return not in proc");
    else if (t.expr == nil) {
      if (curr_scope.tdef != nil && curr_scope.tdef.tdef != nil)
	cerr (t, "return without expr in func");
    } else {
      check_expr (t.expr);
      if (curr_scope.tdef != nil) {
	if (curr_scope.tdef.tdef == nil) cerr (t, "return with expr in proc");
	else if (curr_scope.tdef.tdef.tp != nil && t.expr.tp != nil
		 && !eq_type (curr_scope.tdef.tdef.tp, t.expr.tp))
	  cerr (t, "return-expr type is different to func one");
      }
    }
  } else internal_error (class_name (class (t)) @ " in check_stmt");
}

func check_floating (l, msg) {
  var status;

  ieees.ignore_excepts = 0;
  status = ieees.get_status_bits ();
  if (status & ieees.ofl)
    diags.warn (l.fname, l.lno, l.start, "too big floating point value",
		"as result of ", msg);
  else if (status & ieees.ufl)
    diags.warn (l.fname, l.lno, l.start, "too small floating point value",
		"as result of ", msg);
  else if (status & ieees.imp)
    diags.warn (l.fname, l.lno, l.start, "imprecise floating point value",
		"as result of ", msg);
}

func check_op1 (t) { // !!!!
  check_expr (t.op);
  if (inside (t, op1s.pos)) {
    if (not_numeric (t.op.tp)) cerr (t, "invalid type of monadic +");
    else t.tp = t.op.tp == nil ? t_shortint : t.op.tp;
    if (t.tp != nil) t.lex = t.op.lex;
  } else if (inside (t, op1s.neg)) {
    if (not_numeric_set (t.op.tp)) cerr (t, "invalid type of monadic -");
    else t.tp = t.op.tp;
    if (t.tp != nil && t.op.lex != nil) {
      var opl = t.op.lex, l = lex (t.lno, t.pos, t.fname);
      
      if (inside (opl, lexs.floating)) {
	ieees.ignore_excepts = 1;
	if (inside (opl, reals.r))
	  t.lex = l.floating ().r (mach.r_class ("0.0").subtract (opl.v));
	else
	  t.lex = l.floating ().lr (mach.lr_class ("0.0").subtract (opl.v));
	check_floating (l, "monadic -");
      } else if (t.tp !== t_set) {
	mpis.mpi_ignore_overflow = 1;
	if (inside (opl, integers.si))
	  t.lex = l.integer ().si (mpis.subtract (mach.si_zero, opl.v));
	else if (inside (opl, integers.i))
	  t.lex = l.integer ().i (mpis.subtract (mach.i_zero, opl.v));
	else t.lex = l.integer ().li (mpis.subtract (mach.li_zero, opl.v));
	mpis.mpi_ignore_overflow = 0;
	if (mpis.mpi_overflow)
	  diags.error (t.fname, t.lno, t.start, "overflow in monadic -");
      } else t.lex = l.integer ().i (mpis.not (opl.v));
    }
  } else if (inside (t, op1s.not)) {
    if (not_bool (t.op.tp)) cerr (t, "invalid type of bool operator");
    else if (t.op.lex != nil)
      t.lex = lex (t.lno, t.pos, t.fname).ch (!t.op.lex.ch_code);
    t.tp = t_boolean;
  } else internal_error (class_name (class (t)) @ " in check_op1");
}

func to_int_type (op1, op2) {
  if (op1.size >= op2.size) op2 = mpis.change_size (op2, op1.size);
  else op1 = mpis.change_size (op1, op2.size);
  return [op1, op2];
}

func to_real_type (t, t1, op1, t2, op2) {
  if (t1 !== t_real && t1 !== t_longreal) {
    op1 = (t === t_real ? ieees.single ().from_mpi (op1)
	   : ieees.double ().from_mpi (op1));
    t1 = t;
  }
  if (t2 !== t_real && t2 !== t_longreal) {
    op2 = (t === t_real ? ieees.single ().from_mpi (op1)
	   : ieees.double ().from_mpi (op1));
    t2 = t;
  } 
  if (t !== t1) op2 = ieees.double ().from_float (op2);
  if (t !== t2) op1 = ieees.double ().from_float (op1);
  return [op1, op2];
}

func check_op2 (t) {
  var ct, lt, rt, v, rv, lv, l = nil;

  check_expr (t.left);
  if (inside (t, op2s.is)) check_tdef (t.right); else check_expr (t.right);
  lt = t.left.tp; rt = t.right.tp;
  if (t.left.lex != nil && t.right.lex != nil)
    l = lex (t.lno, t.pos, t.fname);
  if (inside (t, op2s.eq) || inside (t, op2s.ne) || inside (t, op2s.lt)
      || inside (t, op2s.le) || inside (t, op2s.gt) || inside (t, op2s.ge)) {
    var res, opn = (inside (t, op2s.eq) ? "=" : inside (t, op2s.ne) ? "#"
		    : inside (t, op2s.lt) ? "<" : inside (t, op2s.le) ? "<="
		    : inside (t, op2s.gt) ? ">" : ">=");
    if (is_numeric (lt) && is_numeric (rt)) {
      if (l != nil) {
	lv = t.left.lex.v; rv = t.right.lex.v; 
	ct = common_numeric_type (lt, rt);
	if (ct === t_real || ct === t_longreal) {
	  v = to_real_type (t.tp, lt, lv, rt, rv); lv = v [0]; rv = v [1];
	  ieees.ignore_excepts = 1;
	  res = (inside (t, op2s.eq) ? lv.eq (rv)
		 : inside (t, op2s.ne) ? lv.ne (rv)
		 : inside (t, op2s.lt) ? lv.lt (rv)
		 : inside (t, op2s.le) ? lv.le (rv)
		 : inside (t, op2s.gt) ? lv.gt (rv) : lv.ge (rv));
	  check_floating (l, opn);
	} else {
	  v = to_int_type (lv, rv); lv = v [0]; rv = v [1];
	  res = (inside (t, op2s.eq) ? mpis.eq (lv, rv)
		 : inside (t, op2s.ne) ? mpis.ne (lv, rv)
		 : inside (t, op2s.lt) ? mpis.lt (lv, rv)
		 : inside (t, op2s.le) ? mpis.le (lv, rv)
		 : inside (t, op2s.gt) ? mpis.gt (lv, rv) : mpis.ge (lv, rv));
	}
	t.lex = l.chr (res);
      }
    } else if (is_string (lt) && is_string (rt)) {
      if (l != nil) {
	var lv = t.left.lex.str, rv = t.right.lex.str;
	
	res = (inside (t, op2s.eq) ? cmpv (lv, rv) == 0
	       : inside (t, op2s.ne) ? cmpv (lv, rv) != 0
	       : inside (t, op2s.lt) ? cmpv (lv, rv) < 0
	       : inside (t, op2s.le) ? cmpv (lv, rv) <= 0
	       : inside (t, op2s.gt) ? cmpv (lv, rv) > 0 : cmpv (lv, rv) >= 0);
	t.lex = l.chr (res);
      }
    } else if ((lt === t_char || str_char (t.left.lex))
	       && (rt === t_char || str_char (t.right.lex))) {
      if (l != nil) {
	var lv = lt === t_char ? t.left.lex.ch_code : t.left.lex.str [0];
	var rv = rt === t_char ? t.right.lex.ch_code : t.right.lex.str [0];
	
	res = (inside (t, op2s.eq) ? lv == rv  : inside (t, op2s.ne) ? lv != rv
	       : inside (t, op2s.lt) ? lv < rv : inside (t, op2s.le) ? lv <= rv
	       : inside (t, op2s.gt) ? lv > rv : lv >= rv);
	t.lex = l.chr (res);
      }
    } else if (inside (t, op2s.eq) || inside (t, op2s.ne)) {
      if (lt === t_boolean && rt === t_boolean) {
	if (l != nil) {
	  var lv = t.left.lex.ch_code, rv = t.right.lex.ch_code;
	  
	  t.lex = l.chr (inside (t, op2s.eq) ? lv == rv  : lv != rv);
	}
      } else if (lt === t_set && rt === t_set) {
	if (l != nil) {
	  var lv = t.left.lex.v, rv = t.right.lex.v;
	  
	  t.lex = l.chr (inside (t, op2s.eq)
			 ? mpis.eq (lv, rv)  : mpis.ne (lv, rv));
	}
      } else if (eq_type (lt, rt)) { // ptr or proc
	if (l != nil) t.lex = lexs.chr (1); // may be only nil
      } else if (lt != nil && rt != nil)
	cerr (t, "invalid types of op. ", opn);
    } else cerr (t, "invalid types of op. ", opn);
    t.tp = t_boolean;
  } else if (inside (t, op2s.set_in)) {
    if (not_int (lt) || rt != nil && rt !== t_set)
      cerr (t, "invalid types of op. IN");
    else if (l != nil) {
      var res, li = mpi2long (t.left.lex.v);
      var max_set = mpis.from_string (mach.li_size, mach.max_set @ "");

      if (mpis.lt (li, mach.li_zero) || mpis.gt (li, max_set))
	res = 0;
      else
	res = mpis.ne (mach.li_zero,
		       mpis.and (mpis.unsigned_shift_right (li, mach.max_set),
				 mpis.from_string (mach.li_size, "1")));
      t.lex = l.chr (res);
    }
    t.tp = t_boolean;
  } else if (inside (t, op2s.is)) {
    check_ext_des (t.left, rt); t.tp = t_boolean;
  } else if (inside (t, op2s.plus) || inside (t, op2s.minus)
	     || inside (t, op2s.mult) || inside (t, op2s.rdiv)) {
    var opn = (inside (t, op2s.plus) ? "+" : inside (t, op2s.minus) ? "-"
	       : inside (t, op2s.mult) ? "*" : "/");

    if (not_numeric_set (lt) || not_numeric_set (rt))
      cerr (t, "invalid types of op. ", opn);
    else if (lt != nil && rt != nil) {
      t.tp = lt === t_set ? t_set : common_numeric_type (lt, rt);
      if (inside (t, op2s.rdiv) && t.tp !== t_set
	  && t.tp != t_real && t.tp !== t_real)
	t.tp = t_real;
    }
    if (t.tp != nil && l != nil) {
      var v, lv = t.left.lex.v, rv = t.right.lex.v;

      if (t.tp === t_set) {
	if (inside (t, op2s.plus)) v = mpis.or (lv, rv);
	else if (inside (t, op2s.minus)) v = mpis.and (lv, mpis.not (rv));
	else if (inside (t, op2s.mult)) v = mpis.and (lv, rv);
	else  v = mpis.or (mpis.and (lv, mpis.not (rv)),
			   mpis.and (rv, mpis.not (lv)));
	t.lex = mpi2int (l.integer (), v);
      } else if (t.tp === t_real || t.tp === t_longreal) {
	ieees.ignore_excepts = 1;
	v = to_real_type (t.tp, lt, lv, rt, rv); lv = v [0]; rv = v [1];
	if (inside (t, op2s.plus)) v = lv.add (rv);
	else if (inside (t, op2s.minus)) v = lv.subtract (rv);
	else if (inside (t, op2s.mult)) v = lv.multiply (rv);
	else v = lv.divide (rv);
	check_floating (l, opn);
	t.lex = (t.tp === t_real
		 ? l.floating ().r (v) : l.floating ().lr (v));
      } else {
	v = to_int_type (lv, rv); lv = v [0]; rv = v [1];
	mpis.mpi_ignore_overflow = 1;
	if (inside (t, op2s.plus)) v = mpis.add (lv, rv);
	else if (inside (t, op2s.minus)) v = mpis.subtract (lv, rv);
	else if (inside (t, op2s.mult)) v = mpis.multiply (lv, rv);
	else  v = mpis.divide (lv, rv);
	mpis.mpi_ignore_overflow = 0;
	if (mpis.mpi_overflow)
	  diags.error (l.fname, l.lno, l.pos, "overflow in ", opn);
	t.lex = mpi2int (l.integer (), v);
      }
    }
  } else if (inside (t, op2s.or) || inside (t, op2s.and)) {
    if (not_bool (lt) || not_bool (rt))
      cerr (t, "invalid types of op. ", inside (t, op2s.or) ? "OR" : "&");
    else if (l != nil)
      t.lex = l.chr (inside (t, op2s.or)
		     ? t.left.lex.ch_code | t.right.lex.ch_code
		     : t.left.lex.ch_code & t.right.lex.ch_code);
    t.tp = t_boolean;
  } else if (inside (t, op2s.div) || inside (t, op2s.mod)) {
    t.tp = t_shortint;
    if (not_int (lt) || not_int (rt))
      cerr (t, "invalid types of op. ",
	    (inside (t, op2s.div) ? "DIV" : "MOD"));
    else if (lt != nil && rt != nil) t.tp = common_numeric_type (lt, rt);
    if (t.tp != nil && l != nil) {
      if (t.tp === lt) rv = mpis.change_size (rv, lv.size);
      else lv = mpis.change_size (lv, rv.size);
      mpis.mpi_ignore_overflow = 1;
      v = (inside (t, op2s.div)
	   ? mpis.divide (lv, rv) : mpis.remainder (lv, rv));
      mpis.mpi_ignore_overflow = 0;
      if (mpis.mpi_overflow)
	diags.error (l.fname, l.lno, l.start, "overflow in ",
		     inside (t, op2s.div) ? "DIV" : "MOD");
      t.lex = mpi2int (l.integer (), v);
    }
  } else internal_error (class_name (class (t)) @ " in check_op2");
}

func usage_before_decl (u, d) {
  return (u.fname == d.fname
	  && (u.lno < d.lno || u.lno == d.lno && u.pos < d.pos));
}

func through_ptr (t, rec_arr) {
  if (t == nil) return 0;
  else if (inside (t, rec_arr)) return t;
  else if (inside (t, tdefs.ptr) && t.tdef.tp != nil
	   && inside (t.tdef.tp, rec_arr)) return t.tdef.tp;
  else return 1;
}

func check_des (t) {
  var d, dt, rec, f, arr, i;

  if (inside (t, dess.field)) {
    check_des (t.des); rec = through_ptr (t.des.tp, tdefs.rec);
    if (type (rec) == int && rec) cerr (t, "reference to not a pointer");
    else if (type (rec) != int) {
      for (;;) {
	f = decltab.get (t.ident, rec);	if (f != nil || rec.base == nil) break;
	rec = rec.base.tp;
      }
      if (f == nil) cerr (t, "no such field in the record: ", t.ident.repr);
      else t.tp = f.tdef.tp;
    }
  } else if (inside (t, dess.elem)) {
    check_des (t.des); check_expr (t.index);
    arr = through_ptr (t.des.tp, tdefs.arr);
    if (type (arr) == int && arr || t.des.lex != nil)
      cerr (t, "indexing not an array");
    else if (type (arr) != int) {
      if (not_int (t.index.tp)) cerr (t, "indexing is not an integer");
      else {
	t.tp = arr.tdef.tp;
	if (t.index.lex != nil) {
	  i = mpi2long (t.index.lex.v);
	  if (arr.expr.lex != nil && is_int (arr.expr.tp)
	      && mpis.ge (i, arr.expr.lex.v) || mpis.lt (i, mach.li_zero))
	    cerr (t, "index out of range");
	}
      }
    }
  } else if (inside (t, dess.ref)) {
    check_des (t.des);
    if (t.des.tp != nil && !inside (t.des.tp, tdefs.ptr))
      cerr (t, "reference to not a pointer");
    else t.tp = t.des.tp.tdef.tp;
  } else if (inside (t, dess.guard)) {
    check_des (t.des); check_tdef (t.tdef); check_ext_des (t.des, t.tdef.tp);
    t.tp = t.tdef.tp;
  } else if (inside (t, dess.qualid)) {
    d = find_qualid (t);
    if (d != nil) {
      if (inside (d, decls.const)) {
	t.decl = d;
	if (d.expr === v_true || d.expr === v_false) {
	  t.tp = t_boolean;
	  t.lex = lex (t.lno, t.pos, t.fname).ch (d.expr === v_true);
	} else { t.tp = d.expr.tp; t.lex = d.expr.lex; }
      } else if (inside (d, decls.vdecl) || inside (d, decls.par)
		 || inside (d, decls.proc)) {
	t.tp = (d.tdef == nil || inside (d.tdef, tdefs.stdtype)
		? d.tdef : d.tdef.tp);
	t.decl = d;
	if ((inside (d, decls.vdecl) || inside (d, decls.par))
	    && type (d.with_tp) != int) t.tp = d.with_tp;
      } else cerr (t, "non var/const/proc ", t.ident.repr);
    }
  } else internal_error (class_name (class (t)) @ " in check_des");
}

func check_expr (t) {
  if (inside (t, exprs.op1)) check_op1 (t);
  else if (inside (t, exprs.op2)) check_op2 (t);
  else if (inside (t, exprs.null)) {
    t.lex = mpi2int (lex (t.lno, t.pos, t.fname).integer (), mach.nil_ptr);
    t.tp = t_nil;
  } else if (inside (t, exprs.number)) {
    if (inside (t.v, integers.i)) t.tp = t_integer;
    else if (inside (t.v, integers.si)) t.tp = t_shortint;
    else if (inside (t.v, integers.li)) t.tp = t_longint;
    else if (inside (t.v, reals.r)) t.tp = t_real;
    else if (inside (t.v, reals.lr)) t.tp = t_longreal;
    else internal_error ("wrong number in check_expr");
    t.lex = t.v;
  } else if (inside (t, exprs.chr)) {
    t.tp = t_char; t.lex = t.v;
  } else if (inside (t, exprs.str)) {
    var l, sl, m, len, elem;

    sl = #t.v.str @ ""; m = mpis.from_string (mach.li_size, sl);
    l = lex (t.lno, t.pos, t.fname, sl).integer ().li (m);
    len = node (t.fname, t.lno, t.pos).expr ().number (l);
    len.tp = t_longint; len.lex = l;
    elem = node (t.fname, t.lno, t.pos).tdef ().qualid (nil, boolean_id);
    elem.tp = t_char;
    t.tp = node (t.fname, t.lno, t.pos).tdef ().arr (len, elem);
    t.lex = t.v;
  } else if (inside (t, exprs.set)) {
    var i, lb, hb, v, mask, const = 1;
    var max_set = mpis.from_string (mach.li_size, mach.max_set @ "");
    
    for (i = 0; i < #t.ranges; i++)
      if (i == 0 || t.ranges [i - 1] !== t.ranges [i])
	{
	  check_expr (t.ranges [i]); v = 0;
	  if (not_int (t.ranges [i].tp))
	    cerr (t, "invalid types of expr in SET");
	  else if (t.ranges [i].lex != nil
		   && (mpis.lt (mpi2long (t.ranges [i].lex.v), mach.li_zero)
		       || mpis.gt (mpi2long (t.ranges [i].lex.v), max_set)))
	    cerr (t, "value out of range for SET");
	  else v = 1;
	  const = const && v && t.ranges [i].lex != nil;
	}
    if (const) {
      mpis.mpi_ignore_overflow = 1;
      v = mpis.from_string (mach.set_size, "0"); mask = mpis.not (v);
      for (i = 0; i < #t.ranges; i += 2) {
	lb = mpis.to_string (t.ranges [i].lex.v) + 0;
	hb = mpis.to_string (t.ranges [i + 1].lex.v) + 0;
	if (hb < lb) {
	  cerr (t.ranges [i].lex, "low range value > high range value");
	  break;
	}
	v = mpis.or (v,
		     mpis.unsigned_shift_right
		     (mpis.shift_left (mpis.unsigned_shift_right (mask, lb),
				       lb + mach.max_set - hb),
		      mach.max_set - hb));
      }
      mpis.mpi_ignore_overflow = 0;
      if (i >= #t.ranges)
	t.lex = mpi2int (lex (t.lno, t.pos, t.fname).integer (), v);
    }
    t.tp = t_set;
  } else if (inside (t, exprs.fcall)) {
    check_call (t, 1);
  } else if (inside (t, exprs.des)) check_des (t);
  else internal_error (class_name (class (t)) @ " in check_expr");
}

var check_time = 0.0; // overal check time

func check_st (t) {
  var start_time = clock ();
  if (inside (t, nodes.decl)) check_decl (t);
  else if (inside (t, nodes.tdef)) check_tdef (t);
  else if (inside (t, nodes.stmt)) check_stmt (t);
  else if (inside (t, nodes.expr)) check_expr (t);
  else internal_error (class_name (class (t)) @ " in check_st");
  check_time += clock () - start_time;
}
