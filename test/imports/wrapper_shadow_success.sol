import booldef;

function not(x: word) returns (word) {
  return x;
}

function main(b: booldef.Bool) returns (booldef.Bool) {
  return booldef.not(b);
}
