import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;


function pow(b : word, e : word) returns (word) {
  let r : word;
  assembly { r := exp(b, e) }
  return r;
}

contract UserOpLambda {
  function main() returns (word) {
    // helper call used inside a lambda body
    let f = lam(x : word) returns (word) { return pow(x, 3); };
    return f(2);
  }
}
