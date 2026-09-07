enum Option { None, Some(word) }

function mkSome(x: word) returns (Option) {
  return .Some(x);
}

function main() returns (word) {
  match (mkSome(7) ) {
  case Option.Some(v) { return v;
  } case Option.None { return 0;
  } }
}
