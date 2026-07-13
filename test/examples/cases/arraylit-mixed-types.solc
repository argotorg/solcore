// All elements of an array literal must share one type: unifying uint256 with
// address must fail.
import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract ArrayLitMixed {
  function main() -> uint256 {
    let a : address = Typedef.abs(0x1234);
    let m : memory(DynArray(uint256)) = [uint256(1), a];
    return m[uint256(0)];
  }
}
