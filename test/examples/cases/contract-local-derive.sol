import * from std;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

contract ContractLocalDerive {
  #[derive(Eq)]
  enum Color { Red, Green }

  function same() public returns (bool) {
    return Eq.eq(Color.Red, Color.Red);
  }
}
