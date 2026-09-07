import * as M from opaque_alias_mid;

function bad(x: word) returns (Base.T) {
  return M.make(x);
}
