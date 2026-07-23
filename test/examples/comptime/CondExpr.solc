import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

function notAnswer(n : word) returns (word) { return ((n == 42) ? 0 : 42 );}

function answer(n:word) returns (word) { return notAnswer(notAnswer(42)); }

contract Fib {
  function main() public returns (word) { return answer(42); }
}
