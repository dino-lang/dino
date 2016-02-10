// Recursive function to compute & output Fibonacci numbers
fun fibonacci (n) {
  if (n <= 1) return 1;
  return (fibonacci(n-1) + fibonacci(n-2));
}

var i, fibnum;

fibnum = 0;
for (i = 0; i <= 24; i++) 
  {
    fibnum = fibonacci(i);
    putln (i @ " " @ fibnum); 
  }
