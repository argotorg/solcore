/* Negative: function annotated '-> comptime word' but body reads from
   storage via sload — storage is mutable state, never comptime.
   The verifier must reject this.
*/

contract ComptimeAsmRet {
  function loadFromStorage() -> comptime word {
    let v : word;
    assembly {
      v := sload(0)
    }
    return v;
  }
  function main() -> word {
    return loadFromStorage();
  }
}
