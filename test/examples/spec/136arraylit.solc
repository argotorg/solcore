// Array literal in memory: `[1,2,3]` builds a memory(DynArray(t)), whose
// elements are then readable through `m[i]`.
import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract ArrayLit {
  function main() -> uint256 {
    let m : memory(DynArray(uint256)) = [1, 2, 3];
    return m[uint256(0)] + m[uint256(2)];
  }
}
