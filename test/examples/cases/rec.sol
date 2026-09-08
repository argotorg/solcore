function rec (n : word, b : word, f : word) returns (word) {
  match (n ) {
  case 0 { return b;
  } case m { return f(primAddWord(m,1), rec(m, b, f));
  } }
}
