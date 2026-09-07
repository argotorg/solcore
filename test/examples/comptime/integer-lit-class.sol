// Bare integer literals with integer class instances from std.
// The type checker infers the literal type from context: the integer:Ord/Add/Sub
// instances constrain unresolved literals to `integer`.

import {Eq,Ord,lt,Add,Sub} from std;

function fib(comptime n : integer) returns (comptime integer) {
  if (n < 2) {
    return n;
  } else {
    return
      fib(n - 1) + fib(n - 2);
  }
}

contract IntegerLit {
  function main() returns (word) {
    let comptime x :  integer = 20;
    let comptime res :  word = wordFromInteger(fib(x));
    return res;
  }
}
