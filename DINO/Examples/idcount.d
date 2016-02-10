// Output number of occurences of identifiers and numbers in stdin
var i, key, voc = tab [];
for (;;)
  try {
    var ln, a;

    ln = getln ();
    if (ln == "")
      continue;
    a = re.split (ln, "[^[:alnum:]]");
    for (i = 0; i < #a; i++)
      voc [a[i]] = (a[i] in voc ? voc [a[i]] + 1 : 1);
  } catch (eof) {
    break;
  }
fun comp (el1, el2) {
  return cmpv (tolower (el1), tolower (el2));
}
key = sort (keys (voc), comp);
for (i = 0; i < #key; i++)
  putln (key[i], " : ", voc[key[i]]);
