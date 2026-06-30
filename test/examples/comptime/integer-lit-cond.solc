// Integer literals in conditional expression branches.
// The expected type is propagated to both branches of a Cond, so literals
// in branches infer the correct type.

contract CondLit {
  function main() -> word {
    // Both literal branches should infer type word from the return annotation.
    let x : word = if (true) then 1 else 2;
    return x;
  }
}
