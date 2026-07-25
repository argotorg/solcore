import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

function notAnswer(n : word) returns (word) {
   if(n == 42) { return 0; } else {return 42; }
}

function answer(n:word) returns (word) {
  return notAnswer(notAnswer(42));
}
contract Fib {
function main() public returns (word) {
   return answer(42);
}
}
