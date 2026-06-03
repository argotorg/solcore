type W = word;

forall self . class self:IdTy {
  function id(x:self) -> self;
}

instance W:IdTy {
  function id(x:W) -> W {
    return x;
  }
}

contract C {
  public function main() -> word {
    return IdTy.id(42);
  }
}
