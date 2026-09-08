import dot_left;
import dot_right;

function mkLeft() returns (dot_left.LeftOpt) {
  let x: dot_left.LeftOpt = .Some(1);
  return x;
}

function main() returns (word) {
  match (mkLeft() ) {
  case .Some(v) { return v;
  } case .None { return 0;
  } }
}
