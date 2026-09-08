pragma no-patterson-condition A;
pragma no-patterson-condition B;

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
