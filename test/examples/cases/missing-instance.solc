// Note: this class has no instances!
trait Typedef<abs, rep> {
    function rep(x:abs) returns (rep);
    function abs(x:rep) returns (abs);
}

trait MemoryType<self> {
    function load(ptr:word) returns (self);
}

impl MemoryType<word> {
    function load(ptr:word) returns (word) {
        return Typedef.abs(MemoryType.load(ptr) as word);
	// `abs` does not make sense here, but it triggers the bug:
	// the typechecker should complain about  missing instance here
    }
}

contract C {
  function main() public returns (word) {
      let ptr : word = 0;
      // if we inline the let below into return then another bug occurs: main is typed as forall a. () -> a
      // let w:word = MemoryType.load(0);
      return MemoryType.load(0);
  }
}
