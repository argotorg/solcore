contract WordMatchDefault {
  function f(n : word) public returns (word) {
    let result : word;
    match (n ) {
      case 0 { assembly { result := 100 }
      } case x { assembly { result := x }
    } }
    return result;
  }

  function main() public returns (word) {
    return f(42);
  }
}
