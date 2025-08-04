forall self underlyingType . class self:Typedef(underlyingType) {
    function rep(x:self) -> underlyingType;
    function abs(x:underlyingType) -> self;
}

forall t . t : Typedef((word,(word,word))) =>
  function tripleFun(x:t) -> (word, (word, word)) {
    return Typedef.rep(x);
  }
