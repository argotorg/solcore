import booldef;

function main(x: booldef.Bool) returns (word) {
  match (x ) {
  case booldef.Bool.True { return 1;
  } default { return 0;
  } }
}
