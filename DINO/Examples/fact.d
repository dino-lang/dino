// Output factorial of 12.
func fact (x)
{
  if (x <= 1)
    return 1;
  return x * fact (x-1);
}

var i, x;

for (i = 0; i < 1000; i++)
  x = fact (12);

putln (x);
