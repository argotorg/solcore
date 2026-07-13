// An array literal may only be assigned to a storage *array* field: storeArrayLit
// does not unify with a plain word field.
import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract ArrayLitBadTarget {
  n : uint256;

  function main() -> uint256 {
    n = [1, 2, 3];
    return n;
  }
}
