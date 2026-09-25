pragma no-patterson-condition Derived;

trait Seed<a> {}
trait Derived<a> {}

impl Seed<word> {}

impl<a> Derived<a> where a: Seed {}

function needsDerivedTwice<a>(x:a) returns (unit)  where a: Derived, a: Derived {
  return;
}

function main() returns (unit) {
  return needsDerivedTwice(0);
}
