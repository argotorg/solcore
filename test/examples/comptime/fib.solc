import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

function fib(n : word) returns (word) {
   if(n < 2) { return n; } else {return fib(n-1) + fib(n-2); }
}

contract Fib {
function main() public returns (word) {
  return fib(10);
}
}
