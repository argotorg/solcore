import * as M from opaque_select_direct_mid;

function bad(x: word) returns (T) {
  return M.make(x);
}
