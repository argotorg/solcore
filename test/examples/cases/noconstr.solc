trait Foo<a> {
  function foo (x : a) returns (word);
}

// here the constraint a : Foo is
// defered to outer scope where the
// error should be detected.

function bla (x : a) returns (word) {
  return Foo.foo(x);
}

contract Test {
  function main() public {
    return bla(1);
  }
}
