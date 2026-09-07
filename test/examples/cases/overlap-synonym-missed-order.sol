alias W = word;

trait IdTy<self> {
  function id(x:self) returns (self);
}

impl IdTy<word> {
  function id(x:word) returns (word) { return 0; }
}

impl IdTy<W> {
  function id(x:W) returns (W) { return x; }
}
