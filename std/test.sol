data bool = true | false;

class ty:C {
  function foo(x : ty) -> bool;
}

forall x . x:C => function bar(x) {
  return C.foo(x);
}

data Thing = Thing;

instance Thing:C {
  function foo(x) {
    return bar(x);
  }
}
