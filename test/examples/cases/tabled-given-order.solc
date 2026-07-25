pragma solcore noPattersonCondition C;

trait A<a> {}
trait B<a> {}
trait C<a> {}

impl<a> C<a> where a: A, a: B {}

function needsC<a>(x:a) returns (())  where a: C {
  return;
}

function fromAB<a>(x:a) returns (())  where a: A, a: B {
  return needsC(x);
}

function fromBA<a>(x:a) returns (())  where a: B, a: A {
  return needsC(x);
}

function main() returns (()) {
  return;
}
