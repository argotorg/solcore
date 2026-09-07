enum Bool { False, True }

  function f(x : Bool) returns (Bool) {
    match (x ) {
    case z     { return z;
    } case Bool.True  { return Bool.True;
    } case Bool.False { return Bool.False;
    } }
  }

  contract Test {
    function main() public returns (Bool) { return f(Bool.True); }
  }
