import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

// Exercises the `%` operator and the `%=` compound assignment
// (the Mod class), plus the mod constant folding.
function f(x: word, y: word) returns (word) {
  let acc : word = x % y;
  acc %= y;          // (x % y) % y == x % y once reduced
  return acc;
}

contract Modulo {
  // 17 % 5 == 2, 2 % 5 == 2 — folded at compile time.
  function main() public returns (word) { return f(17, 5); }
}
