// `push` stores through the element's storage reference, so the value must be
// something `storage(t)` can store. A type with no `CanStore` instance is
// rejected -- this is the constraint `storage(t):CanStore(v)` on ArrayPush,
// distinct from the `t:StorageCopy` one that whole-array assignment needs.
import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

data Odd = Odd(word);

contract PushNoStore {
  reserved : word;

  function main() -> uint256 {
    let arr : storage(array(Odd)) = storage(0x100);
    ArrayPush.push(arr, Odd(1));
    return uint256(0);
  }
}
