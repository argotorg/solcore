export { T(*), mkT };

enum T { T(word) }

function mkT(x: word) returns (T) {
  return T.T(x);
}
