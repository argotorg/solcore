// Two contracts each declare a type named `T`, with DIFFERENT constructors.
// Contract-local types are qualified by their contract (A.T vs B.T), so the two
// declarations are distinct and neither the type names nor the constructor
// names collide. If they aliased to a single `T`, one contract's `match` would
// fail to find its constructors.
import std.{*};

contract A {
  data T = Foo | Bar;

  public function pickA() -> word {
    match T.Foo {
    | T.Foo => return 1;
    | T.Bar => return 2;
    }
  }
}

contract B {
  data T = Baz | Qux;

  public function pickB() -> word {
    match T.Qux {
    | T.Baz => return 3;
    | T.Qux => return 4;
    }
  }
}
