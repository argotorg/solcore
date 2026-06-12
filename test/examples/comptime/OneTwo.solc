// This function should be in stdlib
function addWord(l: word, r: word) -> word {
  let rw : word;
  assembly {
      rw := add(l,r)
  }
  return rw;
}

  function zero () -> word {
    return 0;
  }

function one() -> word {
    return addWord(1, zero()) ;
  }

function two () -> word {
  let x = zero();
  x = addWord(x, one());
  x = addWord(x,x);
  return x;
}

contract OneTwo {
  public function main() -> word { return two(); }
}

