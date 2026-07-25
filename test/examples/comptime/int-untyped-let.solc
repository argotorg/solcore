// Bare integer literals with integer class instances from std.

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
    let x = 20;
    let comptime res :  word = Int.fromInteger(fib(x));
    return res;
  }
}
