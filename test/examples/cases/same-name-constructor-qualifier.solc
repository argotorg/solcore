// Qualifier access (T.C) must work even when T has a same-name constructor.
// Regression test for: `Error.Empty` reporting "Unqualified constructor: Empty".
enum Err { Err(word), Empty, Msg(word) }

function pickEmpty() returns (Err) {
  return Err.Empty;
}

function pickMsg(x: word) returns (Err) {
  return Err.Msg(x);
}

function pickErr(x: word) returns (Err) {
  return Err.Err(x);
}

function main() returns (word) {
  match (pickEmpty() ) {
  case Err.Empty { return 1;
  } case Err.Err(_) { return 2;
  } case Err.Msg(_) { return 3;
  } }
}
