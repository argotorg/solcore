pragma solcore noPattersonCondition Derived;

trait Seed<a> {}
trait Derived<a> {}

impl Seed<word> {}

impl<a> Derived<a> where a: Seed {}

function needsDerivedTwice<a>(x:a) returns (())  where a: Derived, a: Derived {
  return;
}

function main() returns (()) {
  return needsDerivedTwice(0);
}
