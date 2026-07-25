import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;
  function zero () returns (word) {
    return 0;
  }

function one() returns (word) {
    return 1 + zero() ;
  }

function two () returns (word) {
  let x = zero();
  x = x + one();
  x =  x + x ;
  return x;
}

contract Plus {
  function main() public returns (word) { return two() + two(); }
}
