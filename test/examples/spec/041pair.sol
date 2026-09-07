contract Pair {

  function fst(p : (word, word)) public returns (word) {
    match (p ) {
      case (a,b) { return a;
    } }
  }

  function main() public returns (word) {
    return fst((1,0));
  }
}
