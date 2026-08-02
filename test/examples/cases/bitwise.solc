import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

// Exercises the `^` / `&` / `|` binary operators, the unary `~`, the
// `^=` / `&=` / `|=` compound assignments and the unary `~=` in-place
// complement, plus the bxorWord / bandWord / borWord / bnotWord constant
// folding (mirrors gtWord).
function fxor(x: word, y: word) -> word {
  let acc : word = x ^ y;
  acc ^= x;          // acc = (x ^ y) ^ x == y
  return acc ^ 0;    // identity: a ^ 0 == a
}

function fbitwise(x: word, y: word) -> word {
  let acc : word = x & y;
  acc |= x;          // acc = (x & y) | x == x
  acc &= y;          // acc = x & y
  return acc | 0;    // identity: a | 0 == a
}

// `~x` complements every bit, so `~(~x) == x` and `x & ~0 == x` (`~0` is
// all ones, the AND identity).
function fnot(x: word) -> word {
  let acc : word = ~x;   // acc = ~x
  acc ~=;                // acc = ~(~x) == x   (in-place `~=`)
  return acc & ~0;       // identity: a & ~0 == a
}

contract Bitwise {
  // fxor(5, 3) == 3, fbitwise(6, 3) == 2, fnot(4) == 4;
  // 3 ^ 2 ^ 4 == 5 — folded at compile time.
  public function main() -> word { return fxor(5, 3) ^ fbitwise(6, 3) ^ fnot(4); }
}
