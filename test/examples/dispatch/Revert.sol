import {*} from std;
import {*} from std.dispatch;

function my_revert() returns (word) {
  revertLit("regression");
  return 0;
}

contract Foo {
  constructor() {}

  function noAnswer() public returns (uint256) {
    return uint256(my_revert());
  }

  function answer() public returns (uint256) {
    return uint256(42);
  }
}
