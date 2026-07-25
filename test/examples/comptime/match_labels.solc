/* Test comptime expression match labels: the intended use case is
   matching function selectors against keccak hashes of signatures.
   Covers: keccakLit of a literal, keccakLit of a concatenation, wildcard.
*/

import {*} from std;

contract MatchLabels {

  function dispatch(selector : word) returns (word) {
    match (selector ) {
      case comptime keccakLit("transfer(address,uint256)") { return 1;
      } case comptime keccakLit("balanceOf" + "(" + "address" + ")")   { return 2;
      } default { return 0;
    } }
  }

  function main() returns (word) {
    let comptime t :  word = keccakLit("transfer(address,uint256)");
    let comptime b :  word = keccakLit("balanceOf(address)");
    return dispatch(t) + dispatch(b) + dispatch(0);
  }
}
