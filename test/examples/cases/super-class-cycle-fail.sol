trait A<a> where a: B {}
trait B<a> where a: A {}
trait C<a> {}

function needsC<a>(x:a) returns (unit)  where a: C {
  return;
}

function cannotGetC<a>(x:a) returns (unit)  where a: A {
  return needsC(x);
}

function main() returns (unit) {
  return;
}
