pragma solcore noPattersonCondition Wanted;

trait Known<a> {}
trait Wanted<a> {}

impl<a> Wanted<a> where a: Known {}

function needsWanted<a>(x:a) returns (())  where a: Wanted {
  return;
}

function passKnown<a>(x:a) returns (())  where a: Known {
  return needsWanted(x);
}

function main() returns (()) {
  return;
}
