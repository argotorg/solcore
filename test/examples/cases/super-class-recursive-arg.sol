pragma no-patterson-condition A;

enum Wrap<a> { Wrap(a) }

trait A<a> where Wrap<a>: A {}

function needsWrappedA<a>(x:a) returns (unit)  where Wrap<a>: A {
  return;
}

function shouldUseSuperclass<a>(x:a) returns (unit)  where a: A {
  return needsWrappedA(x);
}

function main() returns (unit) {
  return;
}
