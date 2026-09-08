export { T(A), mk };

enum T { A }

function mk() returns (T) {
  return T.A;
}
