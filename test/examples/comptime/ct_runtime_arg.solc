/* Negative: runtime value passed to a comptime parameter — must fail.
   sloadWord uses sload; storage is mutable state, so its result is
   a runtime value; passing it to double's comptime param is an error.
*/
import std;

function sloadWord() -> word {
  let v : word;
  assembly {
    v := sload(0)
  }
  return v;
}

contract ComptimeRuntimeArg {
  function double(comptime x : word) -> comptime word {
    return x + x;
  }
  function main() -> word {
    return double(sloadWord());
  }
}
