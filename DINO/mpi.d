ext except {
  class mpi_except () {
    class mpi_type (msg) {}
    class mpi_size () {}
    class mpi_unequal_size () {}
    class mpi_overflow () {}
  }
}
final class mpi_package () {
  var mpi_excepts = excepts.mpi_except ();
  // if you change the value, please change it in mpi.c and check
  // maximal value in arithm.c.
  var final max_mpi_size = 128;

  final class mpi (final size) { // The order of vars is important for mpi.c
    private value; var value;
    if (type (size) != int)
      throw mpi_excepts.mpi_type ();
    if (size < 1 || size > max_mpi_size)
      throw mpi_excepts.mpi_size ();
  }
  private check, check2;
  func check (op) {
    if (type (op) != class () || !inside (op, mpi_package))
      throw mpi_excepts.mpi_type();
  }
  func check2 (op1, op2) {
    check (op1); check (op2);
    if (op1.size != op2.size)
      throw mpi_excepts.mpi_unequal_size();
  }
  extern mpi_overflow;
  func check_overflow (op) {
    if (mpi_overflow)
      throw mpi_excepts.mpi_overflow();
    return op;
  }
  private mpi_add, mpi_subtract, mpi_multiply, mpi_divide, mpi_remainder,
          mpi_shift_right, mpi_shift_left, mpi_or, mpi_and, mpi_not,
          mpi_eq, mpi_ne, mpi_gt, mpi_lt, mpi_ge, mpi_le,
          mpi_change_size, mpi_to_string, mpi_from_string;
  extern mpi_add(), mpi_subtract(), mpi_multiply(),
         mpi_divide(), mpi_remainder(), mpi_shift_right(), mpi_shift_left(),
         mpi_or(), mpi_and(), mpi_not(),
         mpi_eq(), mpi_ne(), mpi_gt(), mpi_lt(), mpi_ge(), mpi_le(),
         mpi_change_size(), mpi_to_string(), mpi_from_string();
  func add (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_add (op1, op2, new op1));
  }
  func subtract (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_subtract (op1, op2, new op1));
  }
  func multiply (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_multiply (op1, op2, new op1));
  }
  func divide (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_divide (op1, op2, new op1));
  }
  func remainder (op1, op2) { // Overflow is possible
    check2 (op1, op2);
    return check_overflow (mpi_remainder (op1, op2, new op1));
  }
  func shift_right (op, shift) {
    check (op);
    if (type (shift) != int)
      throw mpi_excepts.mpi_type();
    return mpi_shift_right (op, shift, new op);
  }
  func shift_left (op, shift) { // Overflow is possible
    check (op);
    if (type (shift) != int)
      throw mpi_excepts.mpi_type();
    return check_overflow (mpi_shift_left (op, shift, new op));
  }
  func or (op1, op2) {
    check2 (op1, op2);
    return mpi_or (op1, op2, new op1);
  }
  func and (op1, op2) {
    check2 (op1, op2);
    return mpi_and (op1, op2, new op1);
  }
  func not (op) {
    check (op);
    return mpi_not (op, new op);
  }
  func eq (op1, op2) {
    check2 (op1, op2);
    return mpi_eq (op1, op2);
  }
  func ne (op1, op2) {
    check2 (op1, op2);
    return mpi_ne (op1, op2);
  }
  func gt (op1, op2) {
    check2 (op1, op2);
    return mpi_gt (op1, op2);
  }
  func lt (op1, op2) {
    check2 (op1, op2);
    return mpi_lt (op1, op2);
  }
  func ge (op1, op2) {
    check2 (op1, op2);
    return mpi_ge (op1, op2);
  }
  func le (op1, op2) {
    check2 (op1, op2);
    return mpi_le (op1, op2);
  }
  func change_size (op, new_size) { // Overflow is possible
    check (op);
    if (type (new_size) != int)
      throw mpi_excepts.mpi_type();
    if (new_size < 1 || new_size > max_mpi_size)
      throw mpi_excepts.mpi_size();
    return check_overflow (mpi_change_size (op, new_size, new op));
  }
  func to_string (op) {
    check (op);
    return mpi_to_string (op);
  }
  func from_string (size, string) { // Overflow is possible
    if (type (size) != int
        || type (string) != vector ||  eltype (string) != char)
      throw mpi_excepts.mpi_type();
    if (size < 1 || size > max_mpi_size)
      throw mpi_excepts.mpi_size();
    return check_overflow (mpi_from_string (string, mpi (size)));
  }
}

var mpis = mpi_package ();
