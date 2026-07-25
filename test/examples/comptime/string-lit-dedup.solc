// Distinct string literals get distinct allocators; identical literals share
// one (dedup by content).  "alpha" is used twice and "beta" once, so the
// generated hull must contain exactly two __strlit_* allocators.

import std;
import {*} from std;

contract StringDedup {
  function main() public returns (word) {
    let x : string memory = "alpha";
    let y : string memory = "beta";
    let z : string memory = "alpha";
    return strlen(x) + strlen(y) + strlen(z);
  }
}
