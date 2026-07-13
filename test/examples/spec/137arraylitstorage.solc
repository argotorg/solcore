// Assigning an array literal to a storage array field is Solidity's
// memory -> storage copy: it resizes the field and clears any abandoned tail.
import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract ArrayLitStorage {
  reserved : word; // forge uses at least 1 storage slot
  xs : array(uint256);

  function main() -> uint256 {
    xs = [10, 20, 30];
    return xs[uint256(0)] + xs[uint256(2)];
  }
}
