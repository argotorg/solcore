// Bare integer literals with integer class instances from std.
// The type checker infers the literal type from context: the integer:Ord/Add/Sub
// instances constrain unresolved literals to `integer`.

import std.{Eq,Ord,lt,Add,Sub};

function fib(comptime n : integer) -> comptime integer {
  if (n < 2) {
    return n;
  } else {
    return
      fib(n - 1) + fib(n - 2);
  }
}

contract IntegerLit {
  function main() -> word {
    let x : comptime integer = 20;
    let res : comptime word = wordFromInteger(fib(x));
    return res;
  }
}
