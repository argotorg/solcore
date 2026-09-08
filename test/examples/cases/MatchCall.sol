enum Bool { False, True }

contract MatchCall {
  function f() public returns (Bool) {
    return Bool.True;
  }

  function main() public returns (word) {
    match (f() ) {
     case Bool.True { return 42;
     } case Bool.False { return 0;
    } }
  }
}
