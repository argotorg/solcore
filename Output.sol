// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.23;
import {console,Script} from "lib/stdlib.sol";
contract StringLitOps is Script {
  function run() public {
    console.log("RESULT --> ", wrapper());
  }
  
  function wrapper ()  public returns (uint256 _wrapresult) {
    assembly {function usr$main () -> _v0 {
                mstore(0, "abcd")
                revert(0, 4)
              }
              let _mainresult := usr$main()
              _wrapresult := _mainresult}
  }
}