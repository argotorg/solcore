// Error case: manual Generic instance without pragma no-generic-instance-for.
// The compiler must reject this with a conflict error.

import std.Generic.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

data Foo = MkFoo(word);

instance Foo : Generic(word) {
    function from(x : Foo) -> word {
        match x { | Foo.MkFoo(v) => return v; }
    }
    function to(v : word) -> Foo {
        return Foo.MkFoo(v);
    }
}
