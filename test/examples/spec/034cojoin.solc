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
    let result = Option.None;
    match (mmx ) {
      case Option.Some(Option.Some(x)) { result = Option.Some(x);
      } case Option.None { result = Option.None;
      } case Option.Some(Option.None) { result = Option.None;
      } default { result = Option.None;
    } }
    return result;
  }

 function extract(mx : Option<word>) public returns (word) {
   match (mx ) {
     case Option.Some(x) { return x;
     } case Option.None { return 0;
   } }
 }

  function cojoin(x : Option<word>) public returns (Option<Option<word>>) { // Test that sum types can grow
    let result = Option.None;
    result = Option.Some(x);
    return result;
   }


  function main() public returns (word) {
    return maybe(0, join(cojoin(Option.Some(42))));
  }
}
