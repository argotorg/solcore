enum Bool { False, True }

contract CatchAll {
    function catchAll(x : Bool, y : Bool) public returns (Bool){
      match (x, y ) {
      case (Bool.True, Bool.True ) { return Bool.True;
      } case (z, w      ) { return z;
      } }
    }

  function main() public returns (Bool) {
    return catchAll(Bool.True, Bool.False);
  }
}
