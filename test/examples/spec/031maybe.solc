contract Option {
  enum Option<a> { None, Some(a) }

  function just(x : word) public returns (Option<word>) { return Option.Some(x); }

  function maybe(n : word, o : Option<word>) public returns (word) {
    match (o ) {
      case Option.None { return n;
      } case Option.Some(x) { return x;
    } }
  }

  function main() public returns (word) {
    return maybe(0, Option.Some(42));
  }
}
