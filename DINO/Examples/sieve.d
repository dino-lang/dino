// Output number of primes number < 8190.
var SieveSize, i, prime, k, count, iter, flags;
SieveSize = 8190;

for (iter = 0; iter < 10; iter++;) {
  flags = [SieveSize + 1 : 0];
  count = 0;
  for (i = 0; i <= SieveSize; i++)
    flags[i] = 1;
  for (i = 0; i <= SieveSize; i++;)
    if (flags[i]) {
      prime = i + i + 3;
      k = i + prime;
      for (;1;;) {
        if (k > SieveSize)
          break;
        flags[k] = 0;
        k += prime;
      }
      count++;
    }
}
println (count);
