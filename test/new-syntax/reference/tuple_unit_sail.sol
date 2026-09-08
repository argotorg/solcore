enum Pair<a, b> { Pair(a, b) }

function fst<a, b>(p: (a, b)) returns (a) {
  match (p) {
case (x, y) {
return x;
}
}
}

function tupleValue() returns (word, word) {
  return (1, 0);
}

function unitValue() {
  return ();
}

function nestedTupleUnitPattern(p: ((), (word, word))) returns (word) {
  match (p) {
case ((), (x, y)) {
return x;
}
}
}

function groupedSinglePattern(p: word) returns (word) {
  match (p) {
case (y) {
return y;
}
}
}

function pairData(x: word, y: word) returns (Pair<word, word>) {
  return Pair(x, y);
}
