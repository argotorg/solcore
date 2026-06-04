// Qualifier access (T.C) must work even when T has a same-name constructor.
// Regression test for: `Error.Empty` reporting "Unqualified constructor: Empty".
data Err = Err(word) | Empty | Msg(word);

function pickEmpty() -> Err {
  return Err.Empty;
}

function pickMsg(x: word) -> Err {
  return Err.Msg(x);
}

function pickErr(x: word) -> Err {
  return Err.Err(x);
}

function main() -> word {
  match pickEmpty() {
  | Err.Empty => return 1;
  | Err.Err(_) => return 2;
  | Err.Msg(_) => return 3;
  }
}
