function addWord(l: word, r: word) returns (word) {
  let rw : word;
  assembly {
      rw := add(l,r)
  }
  return rw;
}

function zero () { return 0; }
function one() { return addWord(1, zero()); }

contract OneOne {
    function main() returns (word) { return addWord(one(), one()); }
}