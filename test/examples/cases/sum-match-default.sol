contract SumMatchDefault {
  enum Option<a> { None, Some(a) }

  function g(s : Option<word>) public returns (Option<word>) {
    match (s ) {
      case Option.None { return Option.None;
      } case x { return x;
    } }
  }

  function main() public returns (word) {
    g(Option.None);
    return 42;
  }
}
