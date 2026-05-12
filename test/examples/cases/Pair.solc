  forall a b . function fst (x : (a,b)) -> a {
    match x {
    | (a,_) => return a;
    }
  }

  forall a b . function snd(x : (a,b)) -> b {
    match x {
    | (_,b) => return b;
    }
  }

  function uncurry(f : (word, word) -> word, x : (word,word)) -> word {
    match x {
    | (a,b) => return f(a,b);
    }
  }

  function snds (p1 : (word,word), p2 : (word,word)) -> (word,word) {
    match p1, p2 {
    | (a,b) , (c,d) => return (b,d);
    }
  }

  function curry(f : ((word,word)) -> word, x : word, y : word) -> word {
    return f((x,y)) ;
  }
