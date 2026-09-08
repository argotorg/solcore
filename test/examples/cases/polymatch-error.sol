function fst<a, b>(p: (a, b)) returns (a) {
    match (p ) {
    case (a, _) { return a;
    } }
}
contract TestUnitMatch {
  function main() public returns (()) {
    match (((), ())) {
     case x { return fst(x);
    } }
  }  
}
