// This function should be in stdlib
function addWord(l: word, r: word) returns (word) {
  let rw : word;
  assembly {
      rw := add(l,r)
  }
  return rw;
}

  function zero () returns (word) {
    return 0;
  }

function one() returns (word) {
    return addWord(1, zero()) ;
  }

function two () returns (word) {
  let x = zero();
  x = addWord(x, one());
  x = addWord(x,x);
  return x;
}

contract OneTwo {
  function main() public returns (word) { return two(); }
}

