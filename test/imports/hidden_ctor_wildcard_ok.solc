import hidden_ctor_lib.{Token, mkErr};

function main() -> word {
  match mkErr(1) {
  | Token.Ok(v) => return v;
  | _ => return 0;
  }
}
