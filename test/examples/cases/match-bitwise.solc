import std.{*};
import std.opcodes.{mstore};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

// Regression for the `|` ambiguity between the bitwise-or operator and the
// match-arm separator. Each arm below ends in a *bare* expression statement
// (no trailing `;`), which is exactly the shape that previously made the
// parser read `mstore(...) | <next-pattern> => ...` as a single bitwise-or
// expression and break the `match`. The `|` *inside* the parentheses is a
// genuine bitwise-or; the `|` that starts each arm is a separator.
function emit(x: word) -> () {
  match x {
    | 0 => mstore(0, x | 1)
    | 1 => mstore(0, x & 1)
    | _ => mstore(0, x)
  }
}

contract MatchBitwise {
  // `0 | 1` still folds to 1 at the top level.
  public function main() -> word {
    emit(0);
    return 0 | 1;
  }
}
