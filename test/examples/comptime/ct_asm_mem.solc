/* Positive: function using mstore+mload in assembly is comptime-evaluable
   when its argument is known at compile time.
   The evaluator runs in comptime mode for the RHS of `let x : comptime`.
*/
function storeLoad(x : word) -> word {
  let r : word;
  assembly {
    mstore(0, x)
    r := mload(0)
  }
  return r;
}

contract ComptimeAsmMem {
  function main() -> word {
    let res : comptime word = storeLoad(42);
    return res;
  }
}
