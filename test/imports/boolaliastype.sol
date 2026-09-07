import * as B from booldef;

function fromAliasType(b: B.Bool) returns (B.Bool) {
  return B.not(b);
}
