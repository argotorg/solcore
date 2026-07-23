enum Bool { False, True }

function test(v0 : Bool, p : Bool) returns (Bool) {
  match (p ) {
  case Bool.True { return Bool.False;
  } case z    { return v0;
  } }
}

contract FreshVariableShadowing {
  function main() public returns (Bool) {
    return test(Bool.True, Bool.False);
  }
}
