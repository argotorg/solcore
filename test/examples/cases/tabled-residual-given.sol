pragma no-patterson-condition Wanted;

trait Known<a> {}
trait Wanted<a> {}

impl<a> Wanted<a> where a: Known {}

function needsWanted<a>(x:a) returns (unit)  where a: Wanted {
  return;
}

function passKnown<a>(x:a) returns (unit)  where a: Known {
  return needsWanted(x);
}

function main() returns (unit) {
  return;
}
