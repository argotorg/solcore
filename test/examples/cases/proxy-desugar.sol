import * from std;
pragma no-patterson-condition;
pragma no-coverage-condition;
pragma no-bounded-variable-condition;

function foo(x : Proxy<word>) returns (word) {
  return 0;
}

function fuz(y : word) returns (word) {
  return y + foo(@word);
}
