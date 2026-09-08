trait Foo<a, b> {
  function foo (x : a, y : b) returns (b) ;
  function faa (y : a) returns (a) ;
}

enum Bool { False, True }

enum Maybe<a> { Nothing, Just(a) }
// missing the definition of Foo.foo
impl Foo<Bool, Bool> {
  function faa(y : Bool) returns (Bool) {
    return y ;
  }
}
