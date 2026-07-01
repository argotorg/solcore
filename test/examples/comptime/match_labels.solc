/* Test comptime expression match labels: the intended use case is
   matching function selectors against keccak hashes of signatures.
   Covers: keccakLit of a literal, keccakLit of a concatenation, wildcard.
*/

import std.{*};

contract MatchLabels {

  function dispatch(selector : word) -> word {
    match selector {
      | comptime keccakLit("transfer(address,uint256)") => return 1;
      | comptime keccakLit("balanceOf" + "(" + "address" + ")")   => return 2;
      | _                                                => return 0;
    }
  }

  function main() -> word {
    let t : comptime word = keccakLit("transfer(address,uint256)");
    let b : comptime word = keccakLit("balanceOf(address)");
    return dispatch(t) + dispatch(b) + dispatch(0);
  }
}
