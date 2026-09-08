trait Foo<a, b> {
  function foo (x : a, y : word) returns (b);
}

impl Foo<(), ()> {
  function foo (x : (), y : word) returns (()) {
    return;
  }
}

impl<a> Foo<a, ()> {
  function foo (x : a, y : word) returns (()) {
    return;
  }
}
