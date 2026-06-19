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
import std.{*};

contract StringConcat {
  function viaLet() -> memory(string) {
    let s : string = "Hello, " + "world!";
    return Str.fromString(s);
  }

  public function main() -> word {
    let a : memory(string) = Str.fromString("Hello, " + "world!");
    let b : memory(string) = concatLit("Hello, ", "world!");
    let c : memory(string) = concatLit(concatLit("Hello", ", "), "world!");
    let d : memory(string) = viaLet();
    return strlen(a) + strlen(b) + strlen(c) + strlen(d);
  }
}
