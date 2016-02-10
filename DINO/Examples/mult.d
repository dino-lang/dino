// 100x100 Matrixes multiplication.  No output.
var m1, m2;

fun mmult (m1, m2) {
  var i, j, k, m1rows, m1cols, m2rows, m2cols, result;

  m1rows = #m1; m2rows = #m2;
  m1cols = #m1[0]; m2cols = #m2[0];
  if (m2cols != m2rows) {
    println ("matrices don't match");
    return;
  }
  result = [m1rows:[m2cols:0]];
  for (i=0; i < m1rows; i++)
    for (j=0; j < m2cols; j++)
      for (k=0; k < m1cols; k++)
        result[i][j] += m1[i][k]*m2[k][j];
  return result;
}

m1 = [100:[100:1]];
m2 = [100:[100:1]];
mmult (m1, m2);
