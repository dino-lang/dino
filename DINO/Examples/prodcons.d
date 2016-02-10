// Illustration of threads.  There is output.
class buffer (length = 3) {
  var b = [length:nil], first = 0, free = 0, empty = 1;
  fun consume {
    var res;

    wait (!empty);
    res = b [first];
    first = (first + 1) % length;
    empty = first == free;
    return res;
  }
  fun produce (value) {
    wait (empty || free != first);
    b [free] = value;
    free = (free + 1) % length;
    empty = 0;
  }
}

thread consumer (buffer) {
    fun produce (value) {
      buffer.produce (value);
      put ("produce: ");
      println (value);
    }
    produce (10);
    produce (10.5);
    produce ("string");
    produce ('c');
    produce (nil);
}

thread producer (buffer) {
  var value;

  for (;;) {
    value = buffer.consume ();
    if (value == nil)
      break;
    put ("consume: ");
    println (value);
  }
}

var queue = buffer ();
consumer (queue);
producer (queue);
