function addWord(x : word, y : word) -> word {
  let res: word;
  assembly {
     res := add(x, y)
  }
  return res;
}

contract SimpleLambda{
  public function f (z : word) -> word {
    let n = lam (x : word, y : word) {
      return addWord(x,addWord(y,1));
    } ;
    let m = lam (x : word) {
      return addWord (z,x) ;
    } ;
    return m(n(1,0));
  }
  public function main() -> word {
    return f(40);
  }
}
