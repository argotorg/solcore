function foo (z : word) -> word {
  let f = lam (x : word, y : word) {
      return primAddWord(x,primAddWord(y,1));
    };
  return primAddWord(f(0,1),z);
}
