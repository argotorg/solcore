forall a . class a : Mem {
  function size(x : a) -> word;
}

instance word : Mem {
  function size(x : word) -> word {
    return 32;
  }
}

function foo () -> () {
  let ptr : word;
  let arg : word = 0;
  let size = Mem.size(arg);
  assembly {
    ptr := add(32, size)
  }
}
