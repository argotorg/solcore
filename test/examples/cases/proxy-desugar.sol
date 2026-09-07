import {*} from std;
pragma solcore noPattersonCondition;
pragma solcore noCoverageCondition;
pragma solcore noBoundVariableCondition;

function foo(x : Proxy<word>) returns (word) {
  return 0;
}

function fuz(y : word) returns (word) {
  return y + foo(Proxy as Proxy<word>);
}
