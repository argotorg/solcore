enum Bool { False, True }

function test(x : Bool, y : Bool) returns (Bool) {
  match (x, y ) {
  case (Bool.True, z  ) { return z;
  } case (w, Bool.True  ) { return w;
  } case (a, b     ) { return b;
  } }
}

contract FalseRedundantWarning {
  function main() public returns (Bool) {
    return test(Bool.False, Bool.True);
  }
}
