import {T as U, mkT} from opaque_dep_base;

export { make };

function make(x: word) returns (U) {
  return mkT(x);
}
