function compose<a, b, c> (f : function(b) internal returns (c),g : function(a) internal returns (b)) returns (function(a) internal returns (c)) {
  return lam (x) {
      return f(g(x));
    };
}
