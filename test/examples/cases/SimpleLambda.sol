function addWord(x : word, y : word) returns (word) {
  let res: word;
  assembly {
     res := add(x, y)
  }
  return res;
}

contract SimpleLambda{
  function f (z : word) public returns (word) {
    let n = lam (x : word, y : word) {
      return addWord(x,addWord(y,1));
    } ;
    let m = lam (x : word) {
      return addWord (z,x) ;
    } ;
    return m(n(1,0));
  }
  function main() public returns (word) {
    return f(40);
  }
}
