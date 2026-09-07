

enum B { F, T }
enum Pair<a, b> { Pair(a, b) }

function fst<a, b> (p : Pair<a, b>) returns (a) {
  match (p ) {
    case Pair(x,y) { return x;
  } }
}

function snd<a, b>(p : Pair<a, b>) returns (b) {
  match (p ) {
    case Pair(x,y) { return y;
  } }
}

function add(x : word, y : word) returns (word) {
  let res: word;
  assembly {
     res := add(x, y)
  }
  return res;
}


function addPair(p : Pair<word, word>) returns (word) {
  return add(fst(p), snd(p));
}

contract FstSnd {
 function main() public returns (word) { return  addPair(Pair(41,1)); }
}
