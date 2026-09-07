enum Option { None, Some(word) }

function mkSome(x: word) returns (Option) {
  return .Some(x);
}

function fromOption(x: Option) returns (word) {
  match (x) {
case .Some(v) {
return v;
}
case .None {
return 0;
}
}
}
