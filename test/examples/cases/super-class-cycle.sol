trait A<a> where a: B {}
trait B<a> where a: A {}

function needsB<a>(x:a) returns (())  where a: B {
  return;
}

function usesSuperCycle<a>(x:a) returns (())  where a: A {
  return needsB(x);
}

function main() returns (()) {
  return;
}
