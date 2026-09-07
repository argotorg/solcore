export {*, T(*)};

enum T { T(word) }

function idWord(x: word) returns (word) {
  return x;
}

function mkT(x: word) returns (T) {
  return T.T(x);
}
