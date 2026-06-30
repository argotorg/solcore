import std.{*};
import std.dispatch.{*};

function my_revert() -> word {
  revertLit("regression");
  return 0;
}

contract Foo {
  constructor() {}

  public function noAnswer() -> uint256 {
    return uint256(my_revert());
  }

  public function answer() -> uint256 {
    return uint256(42);
  }
}
