import booldef;

function fromQualified(b: booldef.Bool) returns (booldef.Bool) {
  return booldef.not(b);
}
