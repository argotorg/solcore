import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

// Exercises the `%` operator and the `%=` compound assignment
// (the Mod class), plus the mod constant folding.
function f(x: word, y: word) -> word {
  let acc : word = x % y;
  acc %= y;          // (x % y) % y == x % y once reduced
  return acc;
}

contract Modulo {
  // 17 % 5 == 2, 2 % 5 == 2 — folded at compile time.
  public function main() -> word { return f(17, 5); }
}
