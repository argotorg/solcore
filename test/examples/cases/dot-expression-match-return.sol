enum Bar { Foo(word) }

function x(x: Bar) returns (Bar) {
  match (x ) {
  case .Foo(w) { return .Foo(w);
  } }
}

function main() returns (word) {
  match (x(Bar.Foo(7)) ) {
  case Bar.Foo(w) { return w;
  } }
}
