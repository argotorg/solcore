import * as B from booldef;

function fromAlias(b: B.Bool) returns (B.Bool) {
  return B.not(b);
}
