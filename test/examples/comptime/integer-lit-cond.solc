// Integer literals in conditional expression branches.
// The expected type is propagated to both branches of a Cond, so literals
// in branches infer the correct type.

contract CondLit {
  function main() returns (word) {
    // Both literal branches should infer type word from the return annotation.
    let x : word = ( (true) ? 1 : 2);
    return x;
  }
}
