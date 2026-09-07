enum RGB { Red(word), Green(word), Blue(word) }

contract RGB3 {

  function choose(c:RGB) public returns (word) {
    let res : word;
    match (c ) {
      case .Red(x) { assembly { res := add(x,1) }
      } case .Green(x) { assembly { res := add(x,2) }
      } case .Blue(x) { assembly { res := add(x,3) }
      } }
      return res;
  }
  function main() public returns (word) {
    return choose(RGB.Green(42));
  }
}