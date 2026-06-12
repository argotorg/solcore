// Fibonacci using the comptime-only integer type.
// No import std needed: uses only compiler builtins.
// Expected: main() folds to word literal 55 (fib(10)).

function fib(comptime n : integer) -> comptime integer {
  if (integerLt(n, 2)) {
    return n;
  } else {
    return integerAdd(
      fib(integerSub(n, 1)),
      fib(integerSub(n, 2))
    );
  }
}

contract FibInteger {
  function main() -> word {
    return wordFromInteger(fib(10));
  }
}
