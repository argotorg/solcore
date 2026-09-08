import {Token, mkErr} from hidden_ctor_lib;

function main() returns (word) {
  match (mkErr(1) ) {
  case Token.Ok(v) { return v;
  } default { return 0;
  } }
}
