enum Option<a> { None, Some(a) }

function join(mmx: Option<Option<word>>) returns (Option<word>) {
  match (mmx ) {
  case .Some(.Some(x)) { return .Some(x);
  } default { return .None;
  } }
}

function main() returns (word) {
  match (join(.Some(.Some(9))) ) {
  case .Some(v) { return v;
  } case .None { return 0;
  } }
}
