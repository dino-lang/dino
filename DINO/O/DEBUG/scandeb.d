include "scanner";

ext scanner {

  var final File = "File", final Ln = "Ln", final OD = "OD",
    final Ident = "Ident", final Char = "Char", final Str = "Str",
    final SInt = "SInt", final Int = "Int", final LInt = "LInt",
    final Real = "Real", final LReal = "LReal";
  private File, Ln, OD, Ident, Char, Str,  SInt, Int, LInt, Real, LReal;

  func write (fname, lexs) {
    var f, i, l, cfname, clno;

    func ppos (l) {
      if (cfname != l.fname) {
	fprint (f, File); fput (f, ' '); fprintln (f, l.fname);
	cfname = l.fname;
      }
      if (clno != l.lno) {
	fprint (f, Ln); fputln (f, " ", l.lno);
	clno = l.lno;
      }
      fput (f, l.pos);
    }
    func pc (...) {
      var i;
      for (i = 0; i < #args; i++) {
	fput (f, " ");
	fprint (f, args [i]);
      }
    }

    f = fname == "-" ? stdout : open (fname, "w");
    for (i = 0; i < #lexs; i++) {
      l = lexs [i];
      ppos (l);
      fput (f, " "); fprint (f, l.repr);
      if (inside (l, l.od)) pc (OD, l.code);
      else if (inside (l, l.ident)) pc (Ident);
      else if (inside (l, l.str)) pc (Str, l.str);
      else if (inside (l, l.ch)) pc (Char, l.ch_code);
      else if (inside (l, l.integer)) {
	if (inside (l, l.si))
	  pc (SInt, mpis.to_based_string (l.v, 16), mpis.to_string (l.v));
	else if (inside (l, l.i))
	  pc (Int, mpis.to_based_string (l.v, 16), mpis.to_string (l.v));
	else if (inside (l, l.li))
	  pc (LInt, mpis.to_based_string (l.v, 16), mpis.to_string (l.v));
	else
	  internal_error ();
      } else if (inside (l, l.floating)) {
	if (inside (l, l.r))
	  pc (Real, l.v.to_binary_string (16), l.v.to_string ());
	else if (inside (l, l.lr))
	  pc (LReal, l.v.to_binary_string (16), l.v.to_string ());
	else
	  internal_error ();
      }
      fputln (f);
    }
    if (fname != "-") close (f);
  }

  func read (fname) {
    var lfname, llno, lpos, lname, v, start_p, l;
    var f, i, lno = 0, lexs = [];

    f = fname == "-" ? stdout : open (fname, "r");
    for (;;) {
      lno++; start_p = 1;

      try {
	lname = fscan (f); start_p = 0; v = fscan (f);
      } catch (invcalls.eof) {
	if (!start_p)
	  diags.fatal (fname, lno, -1, "unfinished line");
	if (fname != "-") close (f);
	return lexs;
      } catch (invcalls.invinput) {
	diags.fatal (fname, lno, -1, "incorrect line");
      }

      if (lname == File) {
	lfname = v;
	if (type (lfname) != vector || eltype (lfname) != char)
	  diags.fatal (fname, lno, -1, "incorrect line");
      } else if (lname == Ln) {
	llno = v;
	if (type (llno) != int)
	  diags.fatal (fname, lno, -1, "incorrect line");
      } else {
        var repr = v;

	lpos = lname; 
	try {
	  lname = fscan (f);
	} catch (invcalls.eof) {
	  diags.fatal (fname, lno, -1, "unfinished line");
	} catch (invcalls.invinput) {
	  diags.fatal (fname, lno, -1, "incorrect line");
	}
	if (type (lpos) != int
	    || type (repr) != vector || eltype (repr) != char
	    || type (lname) != vector || eltype (lname) != char)
	  diags.fatal (fname, lno, -1, "incorrect line");
      
	if (lname == Ident)
	  l = lex (llno, lpos, lfname, repr, ident).ident ();
	else {
  	  try {
	    v = fscan (f);
	  } catch (invcalls.eof) {
	    diags.fatal (fname, lno, -1, "unfinished line");
	  } catch (invcalls.invinput) {
	    diags.fatal (fname, lno, -1, "incorrect line");
	  }
      	  if (lname == OD) {
      	    if (type (v) != int)
      	      diags.fatal (fname, lno, -1, "incorrect line");
      	    l = lex (llno, lpos, lfname, repr, v).od ();
      	  } else if (lname == Str) {
      	    if (type (v) != vector || eltype (v) != char)
      	      diags.fatal (fname, lno, -1, "incorrect line");
      	    l = lex (llno, lpos, lfname, repr, string).string (v);
      	  } else if (lname == SInt || lname == Int || lname == LInt) {
      	    if (type (v) != vector || eltype (v) != char)
      	      ;
      	    l = lex (llno, lpos, lfname, repr, number).integer ();
      	    if (lname == SInt) {
      	      v = mpis.from_based_string (mach.si_size, v, 16); l = l.si (v);
      	    } else if (lname == Int) {
      	      v = mpis.unsigned_from_based_string (mach.i_size, v, 16);
      	      l = l.i (v);
      	    } else if (lname == LInt) {
      	      v = mpis.from_based_string (mach.li_size, v, 16); l = l.li (v);
      	    } else 
      	      internal_error ();
      	  } else if (lname == Char) {
      	    if (type (v) != int)
      	      diags.fatal (fname, lno, -1, "incorrect line");
      	    l = lex (llno, lpos, lfname, repr, character).ch (v);
      	  } else if (lname == Real || lname == LReal) {
      	    if (type (v) != vector || eltype (v) != char)
      	      ;
      	    l = lex (llno, lpos, lfname, repr, number).floating ();
      	    if (lname == Real) {
      	      l = l.single (mach.r_class ()); l.v.from_binary_string (v, 16);
      	    } else if (lname == LReal) {
      	      l = l.double (mach.lr_class ()); l.v.from_binary_string (v, 16);
      	    } else 
      	      internal_error ();
      	  }
        }
	ins (lexs, l, -1);
      }
      try {
	fgetln (f);
      } catch (invcalls.eof) {
	if (fname != "-") close (f);
	return lexs;
      }
    }
  }
}
