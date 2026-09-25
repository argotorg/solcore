trait A<a> where a: B {}
trait B<a> where a: A {}

function needsB<a>(x:a) returns (unit)  where a: B {
  return;
}

function usesSuperCycle<a>(x:a) returns (unit)  where a: A {
  return needsB(x);
}

function main() returns (unit) {
  return;
}
