// Error: top-level free function with an unannotated parameter
function add(x, y : word) returns (word) {
  let res : word;
  assembly { res := add(x, y) }
  return res;
}
