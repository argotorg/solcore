function compose<a, b, c> (f : function(b) returns (c),g : function(a) returns (b)) returns (function(a) returns (c)) {
  return lambda (x) {
      return f(g(x));
    };
}
