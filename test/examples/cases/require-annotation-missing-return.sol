// Error: top-level free function with no explicit return type
function double(x : word) {
  let res : word;
  assembly { res := add(x, x) }
  return res;
}
