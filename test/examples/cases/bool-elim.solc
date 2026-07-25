enum Bool { False, True }

  function second(x : Bool, y : word) returns (word) {
    match (x, y ) {
    case (Bool.True, z ) { return z;
    } case (Bool.False, z ) { return z;
    } }
  }

contract Second {
  function main() public returns (word) {
    return second(Bool.True, 42);
  }
}
