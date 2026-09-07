pragma solcore noPattersonCondition A;

enum Wrap<a> { Wrap(a) }

trait A<a> where Wrap<a>: A {}

function needsWrappedA<a>(x:a) returns (())  where Wrap<a>: A {
  return;
}

function shouldUseSuperclass<a>(x:a) returns (())  where a: A {
  return needsWrappedA(x);
}

function main() returns (()) {
  return;
}
