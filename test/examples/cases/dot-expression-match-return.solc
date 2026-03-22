data Bar = Foo(word);

function x(x: Bar) -> Bar {
  match x {
  | .Foo(w) => return .Foo(w);
  }
}

function main() -> word {
  match x(Bar.Foo(7)) {
  | Bar.Foo(w) => return w;
  }
}
