import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;


function pow(b : word, e : word) returns (word) {
  let r : word;
  assembly { r := exp(b, e) }
  return r;
}

contract UserOpLambda {
  function main() returns (word) {
    // helper call used inside a lambda body
    let f = lam(x : word) -> word { return pow(x, 3); };
    return f(2);
  }
}
