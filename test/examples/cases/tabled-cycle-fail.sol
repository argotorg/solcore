pragma solcore noPattersonCondition A;
pragma solcore noPattersonCondition B;

trait A<a> {}
trait B<a> {}

impl<a> A<a> where a: B {}
impl<a> B<a> where a: A {}

function needsA<a>(x:a) returns (())  where a: A {
  return;
}

function main() returns (()) {
  return needsA(0);
}
