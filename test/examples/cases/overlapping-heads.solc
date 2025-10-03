forall a b . class a : Foo(b) {
  function foo (x : a, y : word) -> b;
}

instance () : Foo (()) {
  function foo (x : (), y : word) -> () {
    return ();
  }
}

forall a . instance a : Foo (()) {
  function foo (x : a, y : word) -> () {
    return ();
  }
}
