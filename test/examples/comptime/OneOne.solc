function addWord(l: word, r: word) -> word {
  let rw : word;
  assembly {
      rw := add(l,r);
  }
  return rw;
}

function zero () { 0 }
function one() { addWord(1, zero()) }

contract OneOne {
    function main() -> word { addWord(one(), one()) }
}