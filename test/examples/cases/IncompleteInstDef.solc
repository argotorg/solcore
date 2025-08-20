forall a b . class a : Foo(b) {
  function foo (x : a, y : b) -> b ;
  function faa (y : a) -> a ;
}

data Bool = False | True;

data Maybe(a) = Nothing | Just(a);
// missing the definition of Foo.foo
instance Bool : Foo(Bool) {
  function faa(y : Bool) -> Bool {
    return y ;
  }
}
