import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

// Exercises every compound assignment operator statement, keeping the AST
// statements in sync with the binary operators:
//   +=  -=  *=  /=   (arithmetic: Add / Sub / Mul / Div)
//   %=              (Mod)
//   ^=  &=  |=       (bitwise: BitXor / BitAnd / BitOr)
//   ~=              (unary bitwise NOT: BitNot, `acc ~=` -> `acc := ~acc`)
// each binary `lhs op= rhs` desugars to `lhs := lhs op rhs`.
function f(x: word) -> word {
  let acc : word = x;   // 6
  acc += 4;             // 10
  acc -= 3;             // 7
  acc *= 6;             // 42
  acc /= 2;             // 21
  acc %= 8;             // 5   (21 % 8)
  acc ^= 3;             // 6   (5 ^ 3)
  acc |= 9;             // 15  (6 | 9)
  acc &= 12;            // 12  (15 & 12)
  acc ~=;               // ~12 (in-place bitwise NOT)
  acc &= 15;            // 3   (~12 & 0xf == 0b0011)
  return acc;
}

contract CompoundOperators {
  // f(6) == 3 — folded at compile time.
  public function main() -> word { return f(6); }
}
