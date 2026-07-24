// Binding a storage array field to a local is an *alias*, not a copy: the local
// holds the same slot, so growing it grows the field. (Solidity's `T[] storage p`.)
import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract AliasPush {
  reserved : word; // forge uses at least 1 storage slot
  xs : array(uint256);

  function main() -> uint256 {
    let p : storage(array(uint256)) = xs;
    ArrayPush.push(p, uint256(1));
    // The push went through the alias, so the field sees it.
    return Length.length(xs);
  }
}
