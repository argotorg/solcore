pragma no-coverage-condition Bar;

data Wrap(a) = Wrap(a);

forall self rep . class self : Foo(rep) {}

forall self rep . class self : Bar(rep) {}

forall a b . a : Foo(b) => instance Wrap(a) : Bar(b) {}

forall a rep . Wrap(a) : Bar(rep) =>
function need_bar(x : Wrap(a)) -> () {
    return ();
}

forall a . a : Foo(word) =>
function use_bar(x : Wrap(a)) -> () {
    need_bar(x);
    return ();
}
