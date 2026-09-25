trait Foo<a, b> {
  function foo (x : a, y : word) returns (b);
}

impl Foo<unit, unit> {
  function foo (x : unit, y : word) returns (unit) {
    return;
  }
}

impl<a> Foo<a, unit> {
  function foo (x : a, y : word) returns (unit) {
    return;
  }
}
