// Materialize a string literal into a runtime memory(string).
// The literal is comptime; Str.fromString at memory(string) lowers to a
// per-literal allocator that writes the length and characters into memory.
//
// Expected: compiles; main() returns a memory(string) for "abcd".

import std;
import std.{*};

contract StringLitMem {
  public function main() -> memory(string) {
    return "abcd";
  }
}
