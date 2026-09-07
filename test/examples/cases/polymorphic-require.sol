// This should trigger a warning and an error in the specialiser
// due to unability to resolve result type of require
import {uint256,lt,not,Eq,ne,Proxy,bytes4,string} from std;
import {*} from std.dispatch;

function require<a>(cond: bool) returns (a) {
    if (!cond) {
        assembly {
            revert(0, 0)
        }
    }
}

function callvalue() returns (uint256) {
    let res : word;
    assembly {
        res := callvalue()
    }
    return uint256(res);
}

contract Deposit {
function deposit() public returns (()) {
    require(callvalue() != uint256(0));
    return;
  }

function main() public returns (()) {
  deposit();
}
}