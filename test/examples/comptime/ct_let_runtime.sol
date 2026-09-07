/* Negative: comptime let bound to a runtime expression — must fail.
   sloadWord reads from storage (sload); storage is mutable state,
   so its result is runtime.  Binding it with 'let y : comptime word'
   must be rejected by the verifier.
*/
import std;

function sloadWord() returns (word) {
  let v : word;
  assembly {
    v := sload(0)
  }
  return v;
}

contract ComptimeLetRuntime {
  function main() returns (word) {
    let comptime y :  word = sloadWord();
    return y;
  }
}
