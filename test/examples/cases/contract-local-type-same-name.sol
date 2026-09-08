// Two contracts each declare a type named `T`, with DIFFERENT constructors.
// Contract-local types are qualified by their contract (A.T vs B.T), so the two
// declarations are distinct and neither the type names nor the constructor
// names collide. If they aliased to a single `T`, one contract's `match` would
// fail to find its constructors.
import * from std;

contract A {
  enum T { Foo, Bar }

  function pickA() public returns (word) {
    match (T.Foo ) {
    case T.Foo { return 1;
    } case T.Bar { return 2;
    } }
  }
}

contract B {
  enum T { Baz, Qux }

  function pickB() public returns (word) {
    match (T.Qux ) {
    case T.Baz { return 3;
    } case T.Qux { return 4;
    } }
  }
}
