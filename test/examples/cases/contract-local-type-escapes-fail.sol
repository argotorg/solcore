// A data type declared inside a contract is private to that contract: it may
// not be referenced from outside. Qualification (A.Secret) keeps the bare name
// `Secret` out of the top-level scope, so this must fail name resolution.
import * from std;

contract A {
  enum Secret { S }

  function useIt() public returns (word) {
    match (Secret.S ) {
    case Secret.S { return 1;
    } }
  }
}

// `Secret` is not in scope here — it belongs to contract A.
function leak(x : Secret) returns (word) {
  return 0;
}
