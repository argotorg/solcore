data Foo(a) = Foo(word);
  forall a . function wrap(x : word) -> Foo(a) {
    return Foo(x);
  }

  function unwrap() -> word {
    match(wrap(42)) {
    | Foo(w) => return w;
    }
  }

  contract C {
    public function main() -> word {
      return unwrap();
    }
  }
