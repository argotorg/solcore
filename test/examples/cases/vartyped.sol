function foo () {
  let f : function(word) returns (word) = lam (x) { return x ; } ;
  return f(1);
}
