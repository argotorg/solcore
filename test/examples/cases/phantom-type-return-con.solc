enum Foo<a> { Foo(word) }
  function wrap<a>(x : word) returns (Foo<a>) {
    return Foo(x);
  }

  function unwrap() returns (word) {
    match(wrap(42)) {
    case Foo(w) { return w;
    } }
  }

  contract C {
    function main() public returns (word) {
      return unwrap();
    }
  }
