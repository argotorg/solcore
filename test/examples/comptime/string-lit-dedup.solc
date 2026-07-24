// Distinct string literals get distinct allocators; identical literals share
// one (dedup by content).  "alpha" is used twice and "beta" once, so the
// generated hull must contain exactly two __strlit_* allocators.

import std;
import std.{*};

contract StringDedup {
  public function main() -> word {
    let x : memory(string) = "alpha";
    let y : memory(string) = "beta";
    let z : memory(string) = "alpha";
    return strlen(x) + strlen(y) + strlen(z);
  }
}
