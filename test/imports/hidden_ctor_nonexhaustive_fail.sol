import {Token, mkOk} from hidden_ctor_lib;

function main() returns (word) {
  match (mkOk(1) ) {
  case Token.Ok(v) { return v;
  } }
}
