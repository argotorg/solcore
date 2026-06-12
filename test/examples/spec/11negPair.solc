
forall a . class  a : Neg {
   function neg(x:a) -> a;
}

data B = F | T;

instance B : Neg {
  function neg (x : B) -> B {
    match x {
    | B.F => return B.T;
    | B.T => return B.F;
    }
  }
}

forall a b . function fst (p : (a, b)) -> a {
  match p {
  | (x,y) => return x;
  }
}

forall a b . function snd(p : (a, b)) -> b {
  match p {
    | (x,y) => return y;
  }
}


forall a b . a : Neg, b : Neg => instance (a,b):Neg {
  function neg(p : (a,b)) -> (a,b) {
    return (Neg.neg (fst(p)), Neg.neg(snd (p)));
  }
}

contract NegPair {

 public function bnot(x : B) -> B {
   match x {
     | B.T => return B.F;
     | B.F => return B.T;
   }
}

 public function fromB(b : B) -> word {
  match b  {
    | B.F => return 0;
    | B.T => return 1;
  }
}

 public function main() -> word { return  fromB(fst(Neg.neg((B.F,B.T)))); }
}
