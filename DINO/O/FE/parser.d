// Parser.
include "error";
include "lex";

ext token { var lex; }

var parse_time = 0.0; // overall parse time

func o2parse (final gfname, final lexs, final term_map) {
  var start_time, res, p = parser ();
  var grammar, i, inp = [#lexs : nil];

  start_time = clock ();
  grammar = fgetf (open (gfname, "r"), 0);
  grammar = grammar @ "\n";
  for (i in term_map)
    grammar = grammar @ "TERM " @ i @ "=" @ term_map {i} @ ";\n";
  p.set_debug (0);
  p.set_one_parse (1);
  p.set_grammar (grammar, 1);
  p.set_recovery (1);
  p.set_recovery_match (3);
  
  func syntax_error (err_tok_num, err_tok,
	             start_ignored_tok_num, start_ignored_tok_attr,
	             start_recovered_tok_num, start_recovered_tok) {
    diags.error (inp [err_tok_num].lex.fname, inp [err_tok_num].lex.lno,
		 inp [err_tok_num].lex.pos, "syntax error on token `",
		 inp [err_tok_num].lex.repr, "'");
  }

  for (i = 0; i < #lexs; i++) {
    inp [i] = token (lexs [i].code);
    inp [i].lex = lexs [i];
  }
  res = p.parse (inp, syntax_error);
  parse_time += clock () - start_time;
  return res;
}
