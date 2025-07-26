
forall self . class self:A {
  function foo(p : self) -> word;
}

forall self . class self:B {
  function foo(p : self) -> word;
}

instance word:B {
  function foo(x : word) -> word {
    return x;
  }
}

// error: Constraint for A not found in type of foo
instance word:A {
  function foo(x : word) -> word {
    return x;
  }
}
