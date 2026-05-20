function rec (n : word, b : word, f : word) -> word {
  match n {
  | 0 => return b;
  | m => return f(primAddWord(m,1), rec(m, b, f));
  }
}
