// Definition of abstract tree nodes.
include "lex";

class node (fname, lno, pos) {
  class decl (ident, scope) {
    // ident may be nil. imports is array of imports
    class module (imports, decls, stmts) {}
    class import (external) {}         class const (export, expr) {}
    class tdecl (export, tdef) {}      class vdecl (export, tdef) {}
    class field (export, tdef) {}      class par (var_p, tdef) {}
    // stmts may be empty.  It means forward declaration,
    // hint and decls are always nil in this case.  Forward refers to the
    // corresponding forward declaration.
    class proc (export, hint, tdef, decls, stmts, forward) {}
  }
  
  class tdef () {
    class stdtype (uniq_num) {} // it is used for pre-defined type like INTEGER
    class qualid (module_ident, ident) {} // module_ident may be nil
    class arr (expr, tdef)          {} // nil expr means open array
    // base may be nil, fields is array of fields
    class rec (base, fields)        {var scope;}
    class ptr (tdef)                {}
    // tdef may be nil for a procedure.  pars is array of pars
    class proc (pars, tdef)         {}
  }
  
  class stmt () {
    class assign (des, expr) {}   class pcall (des, actuals) {}
    // conds -- array of pairs (expr, stmts)
    class if_then (conds, else_stmts) {}
    // cases -- array of pairs (ranges, stmts)
    // where ranges is array of pairs (min, max).  max may be identical
    class case (expr, cases, else_stmts) {}
    class while (expr, stmts) {}  class repeat (expr, stmts) {}
    class loop (stmts) {}         class with (des, tdef, stmts) {}
    class exit () {}              class ret (expr) {}  // expr may be nil
  }

  class expr () {

    class op2 (left, right) {
      class eq () {}    class ne () {}    class lt () {}     class le () {}
      class gt () {}    class ge () {}    class set_in () {} class is () {}
      class plus () {}  class minus () {} class or () {}     class and () {}
      class mult () {}  class rdiv () {}  class div () {}    class mod () {}
    }
    
    class op1 (op) { class pos () {}   class neg () {}   class not () {} }
    
    class stdexpr (uniq_num) {} // it is used for pre-defined value like FALSE
    class null () {}  class number (v) {}  class chr (v) {}  class str (v) {}
    class set (ranges) {} // array of pairs (min, max). min, max may be ident.
    class fcall (des, actuals) {}  // array of expressions
    
    class des () {
      class field (des, ident) {}  class elem (des, index) {}
      class ref (des) {}           class guard (des, tdef) {}
      class qualid (module_ident, ident) {}  // module_ident may be nil
    }
  }
}

var nodes = node ("", 0, 0), decls = nodes.decl (), tdefs = nodes.tdef ();
var stmts = nodes.stmt (), exprs = nodes.expr ();
var op1s = exprs.op1 (), op2s = exprs.op2 (), dess = exprs.des ();
