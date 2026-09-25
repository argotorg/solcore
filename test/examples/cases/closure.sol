 function foo (z : word, k : unit, a : word) returns (word) {
  let f = lambda (x : word, y : word) {
      k;
      return primAddWord(a,primAddWord(y,z));
    };
  return f(0,1);
}
