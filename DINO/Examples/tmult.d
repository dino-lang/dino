//  Multiplication of 100x100 Matrixes represented by tables.  No output.
var m1, m2;

fun mmult (m1, m2) {
  var i, j, k, m1rows, m1cols, m2rows, m2cols, result;

  m1rows = #m1; m2rows = #m2;
  m1cols = #m1[0]; m2cols = #m2[0];
  if (m2cols != m2rows) {
     println ("matrices don't match");
     return;
  }
  result = tab [];
  for (i=0; i < m1rows; i++) {
    result[i] = tab [];
    for (j=0; j < m2cols; j++) {
      result[i][j] = 0;
      for (k=0; k < m1cols; k++)
        result[i][j] += m1[i][k]*m2[k][j];
    }
  }
  return result;
}

var i, j;

m1 = tab [];
for (i = 0;i < 100; i++) {
  m1[i] = tab [];
  for (j = 0;j < 100; j++)
    m1[i][j] = 2;
}
m2 = m1;
mmult (m1, m2);
