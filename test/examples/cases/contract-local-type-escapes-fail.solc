// A data type declared inside a contract is private to that contract: it may
// not be referenced from outside. Qualification (A.Secret) keeps the bare name
// `Secret` out of the top-level scope, so this must fail name resolution.
import std.{*};

contract A {
  data Secret = S;

  public function useIt() -> word {
    match Secret.S {
    | Secret.S => return 1;
    }
  }
}

// `Secret` is not in scope here — it belongs to contract A.
function leak(x : Secret) -> word {
  return 0;
}
