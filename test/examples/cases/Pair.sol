  function fst<a, b> (x : (a, b)) returns (a) {
    match (x ) {
    case (a,_) { return a;
    } }
  }

  function snd<a, b>(x : (a, b)) returns (b) {
    match (x ) {
    case (_,b) { return b;
    } }
  }

  function uncurry(f : function((word, word)) internal returns (word), x : (word, word)) returns (word) {
    match (x ) {
    case (a,b) { return f(a,b);
    } }
  }

  function snds (p1 : (word, word), p2 : (word, word)) returns ((word, word)) {
    match (p1, p2 ) {
    case ((a,b) , (c,d) ) { return (b,d);
    } }
  }

  function curry(f : function((word, word)) internal returns (word), x : word, y : word) returns (word) {
    return f((x,y)) ;
  }
