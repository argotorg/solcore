alias W = word;
alias V = word;

trait IdTy<self> {
  function id(x:self) returns (self);
}

impl IdTy<W> {
  function id(x:W) returns (W) { return x; }
}

impl IdTy<V> {
  function id(x:V) returns (V) { return 0; }
}
