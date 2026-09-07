// Comptime string concatenation, materialized into runtime memory(string).
// Concatenation runs at comptime (in the `string` domain); the result is
// materialized at the memory(string) site.  All four forms below build the same
// "Hello, world!" (length 13), so they fold to one StrLit and share ONE
// generated allocator (dedup).  main returns 4 * 13 = 52.
//
// Forms exercised:
//   a — `+` wrapped in Str.fromString (overloaded Add at the `string` site)
//   b — terse concatLit (Str.fromString inserted by the desugarer)
//   c — nested concatLit (intermediate wraps fold to identity)
//   d — A2: a `string`-typed let, then convert (dead-let substitution)

import std;
import {*} from std;

contract StringConcat {
  function viaLet() returns (string memory) {
    let s : string = "Hello, " + "world!";
    return Str.fromString(s);
  }

  function main() public returns (word) {
    let a : string memory = Str.fromString("Hello, " + "world!");
    let b : string memory = concatLit("Hello, ", "world!");
    let c : string memory = concatLit(concatLit("Hello", ", "), "world!");
    let d : string memory = viaLet();
    return strlen(a) + strlen(b) + strlen(c) + strlen(d);
  }
}
