trait Mem<a> {
  function size(x : a) returns (word);
}

impl Mem<word> {
  function size(x : word) returns (word) {
    return 32;
  }
}

function foo () returns (()) {
  let ptr : word;
  let arg : word = 0;
  let size = Mem.size(arg);
  assembly {
    ptr := add(32, size)
  }
}
