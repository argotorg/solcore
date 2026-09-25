function foo (b : bool) returns (unit) {
  let y:word;
  let f = lambda(x : word) {
    if (b) { let z : word = 7; y = z; } else {x = 1;}
  };
  f(44);
}
