// Scanner.
include "mpi";
include "ieee";
include "machine";
include "error";
include "lex";

var scan_time = 0.0; // overal scan time

class scanner (final fname) {
  var f, ln, lno, pos, start_time = clock ();
  private f, ln, lno, pos, start_time;

  var final ident = 500, final string = 501, final number = 502,
    final  character = 503;
  // keywords codes:
  var final ARRAY=300,  final BEGIN=301,     final BY=302,
    final CASE=303,     final CONST=304,     final DIV=305,
    final DO = 306,     final ELSE = 307,    final ELSIF = 308,
    final END = 309,    final EXIT = 310,    final FOR = 311,
    final IF = 312,     final IMPORT = 313,  final IN = 314,
    final IS = 315,     final LOOP = 316,    final MOD = 317,
    final MODULE = 318, final NIL = 319,     final OF = 320,
    final OR = 321,     final POINTER = 322, final PROCEDURE = 323,
    final RECORD = 324, final REPEAT = 325,  final RETURN = 326,
    final THEN = 327,   final TO = 328,      final TYPE = 329,
    final UNTIL = 330,  final VAR = 331,     final WHILE = 332,
    final WITH = 333;
  // operator and delimeter codes:
  var final ASSIGN = 400,  final LE = 401,  final GE = 402,  final RANGE = 403;
  var kw_map = {"ARRAY" : ARRAY,   "BEGIN" : BEGIN,    "BY" : BY,
		"CASE" : CASE,     "CONST" : CONST,    "DIV" : DIV,
                "DO" : DO,         "ELSE" : ELSE,      "ELSIF" : ELSIF,
		"END" : END,       "EXIT" : EXIT,      "FOR" : FOR,
		"IF" : IF,         "IMPORT" : IMPORT,  "IN" : IN,
                "IS" : IS,         "LOOP" : LOOP,      "MOD" : MOD,
                "MODULE" : MODULE, "NIL" : NIL,        "OF" : OF,
		"OR" : OR,         "POINTER" : POINTER,"PROCEDURE" : PROCEDURE,
		"RECORD" : RECORD, "REPEAT" : REPEAT,  "RETURN" : RETURN,
		"THEN" : THEN,     "TO" : TO,          "TYPE" : TYPE,
                "UNTIL" : UNTIL,   "VAR" : VAR,        "WHILE" : WHILE,
                "WITH" : WITH};
  var od_map = {"+" : 0 + '+',  "-" : 0 + '-',  "*" : 0 + '*',  "/" : 0 + '/',
                "~" : 0 + '~',  "&" : 0 + '&',  "." : 0 + '.',  "," : 0 + ',',
                ";" : 0 + ';',  "|" : 0 + '|',  "(" : 0 + '(',  "[" : 0 + '[',
	        "{" : 0 + '{',  ":=" : ASSIGN,  "^" : 0 + '^',  "=" : 0 + '=',
                "#" : 0 + '#',  "<" : 0 + '<',  ">" : 0 + '>',  "<=" : LE,
                ">=" : GE,      ".." : RANGE,   ":" : 0 + ':',   ")" : 0 + ')',
	        "]" : 0 + ']',  "}" : 0 + '}'};
  private kw_map, od_map;

  //
  // Patterns:
  //
  var p_skip = "[[:space:]]*";                            // blanks
  var p_hd = "[0-9A-F]";                                  // hexDigit
  var p_id = "^([[:alpha:]][[:alnum:]]*)" @ p_skip;       // identifier
  var p_char = "([[:digit:]][A-F]*X)";                    // character
  var p_int = "([[:digit:]]+|[[:digit:]]" @ p_hd @ "*H)"; // integer number
  var p_scale = "[ED][+-]?[[:digit:]]+";                  // ScaleFactor
  var p_real = "([[:digit:]]+\\.[[:digit:]]*(" @ p_scale @ ")?)";// real number
  // number or character
  var p_nc = "^(" @ p_int @ "|" @ p_real @ "|" @ p_char @ ")" @ p_skip;
  // start parenthesis int, real, and char in p_nc:
  var p_intps = 4, p_realps = p_intps + 2, p_charps = p_realps + 4;
  var p_str = "^(\\\"[^\"]*\\\"|'[^']*')" @ p_skip;        // string:
  var p_od = "^([-+*/~&.,;|([{^=#<>:)}]|]|:=|<=|>=|\\.\\.)" @ p_skip; // Op/del

  var patterns = [256:nil];

  func set_pat (str, pat) {
    var i;

    for (i = 0; i < #str; i++)
      patterns [str [i]] = pat;
  }

  set_pat (" \t\f\r\v", p_skip);
  set_pat ("0123456789", p_nc);
  set_pat ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\\", p_id);
  set_pat ("\"'", p_str);
  set_pat ("-+*/~&.,;|([{^=#<>:)]}", p_od);

  var estr = "";
  
  private p_skip, p_hd, p_id, p_char, p_int, p_scale, p_real, p_nc,
    p_intps, p_str, p_od, patterns, set_pat, estr;

  func get_lexs () { // return all lexs.
    var v, tok, i, start, l, pat, level = 0, comm_lno, start_time = clock ();
    var ints = [mach.si_size, mach.i_size, mach.li_size], lexs = [];

    for (;;) {
      if (#ln == 0) {
	try {
	  ln = fgetln (f);
	  lno++;
	} catch (invcalls.eof) {
	  if (level != 0)
            diags.error (fname, lno, pos, "unfinished comment started on ln ",
	                 comm_lno);
	  scan_time += clock () - start_time;
	  return lexs;
	}
	if (#ln == 0)
	  continue;
	pos = 1;
	ln = new ln; // make it mutable
      }
      if (level != 0 || ln [0] == '(' && #ln > 1 && ln [1] == '*') { // comment
        if (level == 0)
          comm_lno = lno;
        for (i = 0; i < #ln - 1; i++)
           if (ln [i] == '(' && ln [i + 1] == '*')
             level++;
           else if (ln [i] == '*' && ln [i + 1] == ')') {
             level--;
             if (level == 0)
               break;
           }
        del (ln, 0, i + 2);
        continue;
      }
      pat = patterns [ln [0]];
      if (pat === p_skip) {
	v = match (p_skip, ln);
        del (ln, 0, v[1]);
	pos += v[1];
	continue;
      }
      v = (pat == nil ? nil : match (pat, ln));
      if (v == nil) { // token is not found
        diags.error (fname, lno, pos, "bad lexema", ln);
	del (ln, 0, 1);
	pos++;
	for (i = 1; i < #ln && patterns [ln [i]] == nil; i++)
	  pos++;
	continue;
      }
      tok = subv (ln, v [2], v [3] - v [2]);
      del (ln, 0, v [1]);
      start = pos;
      pos += v [1];

      if (pat === p_od) // operator/delimeter
	l = lex (lno, start, fname, tok, od_map {tok}).od ();
      else if (pat === p_id) // Identifier:
	l = (tok in kw_map
	     ? lex (lno, start, fname, tok, kw_map {tok}).od ()
	     : lex (lno, start, fname, tok, ident).ident ());
      else if (pat === p_nc) { // number or character:
	if (v [p_intps] >= 0) { // integer number
	  var base = 10;
	  
	  l = lex (lno, start, fname, tok, number).integer ();
	  if (tok [#tok - 1] == 'H') { // there is a suffix.
	    base = 16;
	    del (tok, #tok - 1);
	  }
	  mpis.mpi_ignore_overflow = 1;
	  for (i = 0; i < #ints; i++) {
	    v = mpis.from_based_string (ints [i], tok, base);
	    if (!mpis.mpi_overflow) {
	      l = (i == 2 ? l.li (v) : i == 1 ? l.i (v) : l.si (v));
	      break;
	    }
	  }
	  mpis.mpi_ignore_overflow = 0;
	  if (i >= #ints) {
	    diags.error (fname, lno, start, "too big integer constant ", tok);
	    l = l.li (mach.max_li);
	  }
	  
	} else if (v [p_realps] >= 0) { // real number:
	  var suf, status;
	  
	  l = lex (lno, start, fname, tok, number).floating ();
	  suf = (v [p_realps + 2] >= 0 ? tok [v [p_realps + 2]] : nil);
	  ieees.ignore_excepts = 1;
	  if (suf == 'D') {
	    tok [v [p_realps + 2]] = 'E'; // for function from_string
	    l = l.lr (mach.lr_class ());
          } else l = l.r (mach.r_class ());
          l.v.from_string (tok);
	  if (suf == 'D')
	    tok [v [p_realps + 2]] = 'D';
	  ieees.ignore_excepts = 0;
	  status = ieees.get_status_bits ();
	  if (status & ieees.ofl)
	    diags.warn (fname, lno, start, "too big floating point value");
	  else if (status & ieees.ufl)
	    diags.warn (fname, lno, start, "too small floating point value");
	  else if (status & ieees.imp)
	    diags.warn (fname, lno, start, "imprecise floating point value");
	} else if (v [p_charps] >= 0) { // character
          del (tok, #tok - 1); // remove suffix
	  mpis.mpi_ignore_overflow = 1;
	  v = mpis.unsigned_from_based_string (mach.ch_size, tok, 16);
	  mpis.mpi_ignore_overflow = 0;
	  if (!mpis.mpi_overflow)
	    l = lex (lno, start, fname, tok, character).ch (int (tok));
	  else {
	    diags.error (fname, lno, start, "too big character ", tok);
	    l = lex (lno, start, fname, tok, character).ch (mach.max_ch);
	  }
	} else
	  internal_error (fname @ ":" @ lno @ ":" @ start @ ":" @ tok);
      } else if (pat === p_str) { // string
	tok = subv (tok, 1, #tok - 2);
  	l = lex (lno, start, fname, tok, string).str (tok);
      } else
	internal_error (ln @ ":" @ lno @ ":" @ tok);
      ins (lexs, l, -1);
    }
  }

  f = fname == "-" ? stdin : open (fname, "r");
  lno = 0; ln = estr;

  // The folllowing is table terminal name -> its code:
  var i; private i;
  var term_map = {"ident" : ident, "string" : string, "number" : number,
	          "character" : character,
	          "ASSIGN" : ASSIGN, "LE" : LE, "GE" : GE, "RANGE" : RANGE};
  for (i in kw_map)
    term_map {i} = kw_map {i};
  scan_time += clock () - start_time;
}
