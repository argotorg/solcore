import reexport_ctor_mid;

function main() returns (word) {
  match (reexport_ctor_mid.mkErr(1) ) {
  case reexport_ctor_mid.Token.Ok(v) { return v;
  } default { return 0;
  } }
}
