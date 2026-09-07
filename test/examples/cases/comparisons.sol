import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;
function f(x: word, y:word) returns (bool) {
  return (!((x == y)
          && (x != y)
          && (x >= y)
          && (x <= y)
          || (x >  y)
          && (x <  y)
         ));
}

contract Comparisons {
  function main() public returns (bool) { return f(0,1); }
}
