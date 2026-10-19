import * from std;
import * from std.dispatch;

function rttrue() returns (bool) {
  assembly { pop(sload(0)) } //prevent comptime eval
  return true;
}

function rtfalse() returns (bool) {
  assembly { pop(sload(0)) } //prevent comptime eval
  return false;
}

contract Answer {
    // #[() -> 42]
    function testor() public returns (uint256) {
        if(rttrue() || absurd()) {
            return 42;
        } else {
            return 0;
        }
    }

    // #[() -> 42]
    function testand() public returns (uint256) {
        if(rtfalse() &&  absurd()) {
            return 0;
        } else {
            return 42;
        }
    }

}