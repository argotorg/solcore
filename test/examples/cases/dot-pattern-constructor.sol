enum Option { None, Some(word) }

function fromOption(x: Option) returns (word) {
  match (x ) {
  case .Some(v) { return v;
  } case .None { return 0;
  } }
}

function main() returns (word) {
  return fromOption(Option.Some(3));
}
