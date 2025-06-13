class self:Typedef(underlyingType) {
    function rep(x:self) -> underlyingType;
    function abs(x:underlyingType) -> self;
}

forall a.class a : Xor {
  function add(x:a, y:a) -> a;
}

forall a. class a : Reverse {
  function reverse(x:a) -> a;
}

data B = F | T;

instance B : IsBit {}
instance B : IsBits {
  function extract(x) {
    match x {
      | F => return 0;
      | T => return 1;
    }
  }
}

class self:IsBits {
  function extract(x:self) -> word;
}

forall self . self:IsBits => class self:IsBit {}

pragma no-patterson-condition Reverse, IsBits;
forall a . a:IsBit => default instance a:IsBits {
  function extract(x) {
    return IsBits.extract(x);
  }
}
forall lhs rhs . lhs:IsBit, rhs:IsBits => instance (lhs, rhs):IsBits {
  function extract(x) {
    match x {
      | (l,r) =>
          let ret : word;
          let lbit : word = IsBits.extract(l);
          let rbits : word = IsBits.extract(r);
          assembly {
            ret := add(lbit, mul(10, rbits));
          }
          return ret;
    }
  }
}

instance B : Typedef(word) {
  function rep(x) {
    match x {
      | F => return 0;
      | T => return 1;
    }
  }

  function abs(x) {
    match x {
      | 0 => return F;
      | 1 => return T;
    }
  }
}

instance B : Xor {
  function add(x, y) {
    match x {
      | F =>
        match y {
          | F => return F;
          | T => return T;
        }

      | T =>
        match y {
          | F => return T;
          | T => return F;
        }
    }
  }
}

forall lhs rhs . lhs:Xor, rhs:Xor => instance (lhs, rhs):Xor {

  function add(a, b) {
    match a, b {
      | (a1, a2), (b1, b2) => return (Xor.add(a1, b1), Xor.add(a2, b2));
    }

  }

}

contract Compose {

  function main() {
    let cypher = (T, F, T, F, T, F, T);
    let res = Xor.add( Xor.add ((T, T, F, F, T, T, T), cypher), cypher);
    return IsBits.extract(res);
  }
}
