type W = word;

forall self . class self:IdTy {
  function id(x:self) -> self;
}

instance W:IdTy {
  function id(x:W) -> W { return x; }
}

instance word:IdTy {
  function id(x:word) -> word { return 0; }
}
