forall a b . function fst(p: (a, b)) -> a {
    match p {
    | (a, _) => return a;
    }
}
contract TestUnitMatch {
  public function main() -> () {
    match ((), ()) {
     | x => return fst(x);
    }
  }  
}
