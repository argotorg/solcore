export { main };

pragma solcore noPattersonCondition;

function main(b : unordered_imports_lib.Bool) returns (unordered_imports_lib.Bool) {
  return unordered_imports_lib.not(b);
}

import unordered_imports_lib;
