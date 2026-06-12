contract Unit {
public function one (x : ()) -> word {
  return 1;
}

public function unitVal() -> () {
  return ();
}

public function unitMatch (x : ()) -> word {
  match x {
  | () => return 1;
  }
}

public function foo (x : word) -> () {
  return ();
}

public function main() -> word {
  return unitMatch(foo(one(unitVal())));
}
}

forall a . class a : Def {
  function def () -> a ;
}

instance () : Def {
  function def() -> () {
    return ();
  }
}
