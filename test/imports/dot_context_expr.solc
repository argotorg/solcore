import dot_left;
import dot_right;

function mkLeft() -> dot_left.LeftOpt {
  let x: dot_left.LeftOpt = .Some(1);
  return x;
}

function main() -> word {
  match mkLeft() {
  | .Some(v) => return v;
  | .None => return 0;
  }
}
