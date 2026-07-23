contract Not {
  enum Bool { False, True }

  function main() public returns (word) {
    return fromBool(bnot(Bool.False));
  }

  function fromBool(b : Bool) public returns (word) {
    match(b) {
      case Bool.False { return 0;
      } case Bool.True  { return 1;
    } }
  }

  function bnot(b : Bool) public returns (Bool) {
    match (b ) {
      case Bool.False { return Bool.True;
      } case Bool.True { return Bool.False;
    } }
  }
}
