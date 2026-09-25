pragma no-patterson-condition A;
pragma no-patterson-condition B;

trait A<a> {}
trait B<a> {}

impl<a> A<a> where a: B {}
impl<a> B<a> where a: A {}

function needsA<a>(x:a) returns (unit)  where a: A {
  return;
}

function main() returns (unit) {
  return needsA(0);
}
