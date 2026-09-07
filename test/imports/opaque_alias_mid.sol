import * as Base from opaque_dep_base;

export { make };

function make(x: word) returns (Base.T) {
  return Base.mkT(x);
}
