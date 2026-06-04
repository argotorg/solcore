// Bare integer literals with integer class instances from std.

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
    let x = 20;
    let res : comptime word = Int.fromInteger(fib(x));
    return res;
  }
}
