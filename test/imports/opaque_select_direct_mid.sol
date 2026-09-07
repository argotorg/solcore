import {T, mkT} from opaque_dep_base;

export { make };

function make(x: word) returns (T) {
  return mkT(x);
}
