// Illustration of threads.  There is output.
class buffer (length = 3) {
  var b = [length:nil], first = 0, free = 0, empty = 1;
  private b, first, free, length;
  func consume () {
    var res;

    wait !empty;
    res = b [first];
    first = (first + 1) % length;
    empty = first == free;
    return res;
  }
  func produce (val) {
    wait empty || free != first;
    b [free] = val;
    free = (free + 1) % length;
    empty = 0;
  }
}

thread consumer (buffer) {
    func produce (val) {
      buffer.produce (val);
      put ("produce: ");
      println (val);
    }
    produce (10);
    produce (10.5);
    produce ("string");
    produce ('c');
    produce (nil);
}

thread producer (buffer) {
  var val;

  for (;;) {
    val = buffer.consume ();
    if (val == nil)
      break;
    put ("consume: ");
    println (val);
  }
}

var queue = buffer ();
consumer (queue);
producer (queue);
