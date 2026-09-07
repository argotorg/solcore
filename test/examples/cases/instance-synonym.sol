alias W = word;

trait IdTy<self> {
  function id(x:self) returns (self);
}

impl IdTy<W> {
  function id(x:W) returns (W) {
    return x;
  }
}

contract C {
  function main() public returns (word) {
    return IdTy.id(42);
  }
}
