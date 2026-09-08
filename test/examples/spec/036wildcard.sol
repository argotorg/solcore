contract Option {
  enum Option<a> { None, Some(a) }

  function maybe(n : word, o : Option<word>) public returns (word) {
    match (o ) {
      case Option.Some(x) { return x;
      } default { return n;
    } }
  }

  function main() public returns (word) {
    return maybe(7, Option.None);
  }
}
