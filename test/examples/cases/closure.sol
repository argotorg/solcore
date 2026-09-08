 function foo (z : word, k : (), a : word) returns (word) {
  let f = lam (x : word, y : word) {
      k;
      return primAddWord(a,primAddWord(y,z));
    };
  return f(0,1);
}
