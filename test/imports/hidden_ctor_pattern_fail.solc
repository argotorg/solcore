import hidden_ctor_lib.{Token, mkErr};

function main() -> word {
  match mkErr(1) {
  | Token.Err(v) => return v;
  | _ => return 0;
  }
}
