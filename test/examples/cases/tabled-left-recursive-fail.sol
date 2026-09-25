pragma no-patterson-condition Loop;

trait Loop<a> {}

impl<a> Loop<a> where a: Loop {}

function needsLoop<a>(x:a) returns (unit)  where a: Loop {
  return;
}

function main() returns (unit) {
  return needsLoop(0);
}
