import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

infixl 70 (^^) => pow;

function pow(b : word, e : word) -> word {
  let r : word;
  assembly { r := exp(b, e) }
  return r;
}

contract UserOpLambda {
  function main() -> word {
    // operator (^^) used inside a lambda body
    let f = lam(x : word) -> word { return x ^^ 3; };
    return f(2);
  }
}
