// Bare integer literals at `integer` sites, without explicit wordToInteger.
// The type checker infers the literal type from the expected type at each site:
//   let x : comptime integer = 10    -- expected type is integer
//   integerLt(n, 2)                  -- param type is integer
//   integerSub(n, 1)                 -- param type is integer
//
// Expected: main() folds to word literal 55 (fib(10)).

function fib(comptime n : integer) returns (comptime integer) {
  if (integerLt(n, 2)) {
    return n;
  } else {
    return integerAdd(
      fib(integerSub(n, 1)),
      fib(integerSub(n, 2))
    );
  }
}

contract IntegerLit {
  function main() returns (word) {
    let comptime x :  integer = 10;
    let comptime res :  word = wordFromInteger(fib(x));
    return res;
  }
}
