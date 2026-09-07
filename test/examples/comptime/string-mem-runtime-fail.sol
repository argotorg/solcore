// A `string` is comptime-only: it has no runtime representation.  Returning a
// string-typed value where memory(string) is expected, without an explicit
// Str.fromString conversion, must be rejected by the type checker.

import std;
import {*} from std;

contract StringMemRuntimeFail {
  function f() public returns (string memory) {
    let s : string = "x";
    return s;
  }
}
