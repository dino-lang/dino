// Common compiler code.
func grammar_fname () {
  var home = "O_HOME", final name = "FE/o.y";
  if (home in env)
    return env{home} @ "/" @ name;
  return name;
}

func imported_module_file_name (mn) {
  var home = "O_HOME";
  if (home in env)
    return env{home} @ "/DEBUG/" @ mn @ ".ob";
  return mn @ ".ob";
}

func class_name (c) {
  var res, pr = sprint (c);

  res = sub (".*\\.", pr, "");
  return (res == nil ? pr : res);
}
