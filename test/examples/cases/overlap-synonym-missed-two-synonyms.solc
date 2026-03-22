type W = word;
type V = word;

forall self . class self:IdTy {
  function id(x:self) -> self;
}

instance W:IdTy {
  function id(x:W) -> W { return x; }
}

instance V:IdTy {
  function id(x:V) -> V { return 0; }
}
