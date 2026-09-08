contract Option {
  enum Option<a> { None, Some(a) }
  enum Bool { False, True }

  function maybe(n : word, o : Option<word>) public returns (word) {
    match (o ) {
      case Option.None { return n;
      } case Option.Some(x) { return x;
    } }
  }

  function join(mmx : Option<Option<word>>) public returns (Option<word>) {
    let result = Option.None;
    match (mmx ) {
      case Option.Some(Option.Some(x)) { result = Option.Some(x);
      } case Option.None { result = Option.None;
    } }
    return result;
  }


  function main() public returns (word) {
    return maybe(0, join(Option.Some(Option.Some(Bool.False))));
  }
}
