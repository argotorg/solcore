import {*} from std;

// Tests Num.fromInteger for word (Typedef.abs = identity) and uint256 (wraps in uint256(...)).
// Also tests the full design-doc pattern: comptime integer fib result converted via Num.fromInteger.

function fib(comptime n : integer) returns (comptime integer) {
  if (integerLt(n, wordToInteger(2))) {
    return n;
  } else {
    return integerAdd(
      fib(integerSub(n, wordToInteger(1))),
      fib(integerSub(n, wordToInteger(2)))
    );
  }
}

// Exercises both instances.
// word  path: Typedef.abs for word is identity => fromInteger(wordToInteger(42)) = 42
// uint256 path: Typedef.abs wraps in uint256   => fromInteger(fib(10)) = uint256(55)
// Returns Typedef.rep(u) = 55, demonstrating the uint256 round-trip.
// Expected: main() folds to word literal 55.
contract IntegerFromInteger {
  function main() returns (word) {
    let comptime w :  word    = Num.fromInteger(wordToInteger(42));
    let comptime u :  uint256 = Num.fromInteger(fib(wordToInteger(10)));
    return Typedef.rep(u);
  }
}
