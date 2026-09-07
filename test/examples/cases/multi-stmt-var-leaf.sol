enum Bool { False, True }

contract MultiStmtVarLeaf {
  function main(x:Bool) public returns (Bool) {
    match (x ) {
      case y {
        let z = y;
        return z;
    } }
  }
}
