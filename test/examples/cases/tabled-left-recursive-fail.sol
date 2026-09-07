pragma solcore noPattersonCondition Loop;

trait Loop<a> {}

impl<a> Loop<a> where a: Loop {}

function needsLoop<a>(x:a) returns (())  where a: Loop {
  return;
}

function main() returns (()) {
  return needsLoop(0);
}
