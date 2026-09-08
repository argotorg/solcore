contract Option {
  enum Option<a> { None, Some(a) }

  function just(x : word) public returns (Option<word>) { return Option.Some(x); }

  function maybe(n : word, o : Option<word>) public returns (word) {
    match (o ) {
      case Option.None { return n;
      } case Option.Some(x) { return x;
    } }
  }

  function join(mmx : Option<Option<word>>) public returns (Option<word>) {
    match (mmx ) {
      case Option.None { return Option.None;
      } case Option.Some(Option.None) { return Option.None;
      } case Option.Some(Option.Some(x)) { return Option.Some(x);
    } }
  }

  function join2(mmx : Option<Option<word>>) public returns (Option<word>) {
    match (mmx ) {
      case Option.Some(m) { match (m ) {
          case Option.None { return Option.None;
          } case Option.Some(x) { return Option.Some(x);
      } }
      } default { return Option.None;
    } }
  }

  function main() public returns (word) {
   //  return maybe(0, join(Option.Some(Option.Some(42))));
   return 42;
  }
}
