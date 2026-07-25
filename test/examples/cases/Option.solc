contract Option {
  enum Option<a> { None, Some(a) }

  function join(mmx : Option<Option<word>>) public returns (Option<word>) {
    match (mmx ) {
    case Option.None { return Option.None;
    } case Option.Some(Option.Some(x)) { return Option.Some(x);
    } case Option.Some(Option.None) { return Option.None;
    } }
  }

  function main() public returns (word) { return 0; }
 }
