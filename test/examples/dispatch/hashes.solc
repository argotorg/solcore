import {*} from std;
import {*} from std.dispatch;
import {mstore} from std.opcodes;

// Build a memory(bytes) holding the three-byte string "abc".
function abcBytes() returns (bytes memory) {
    let p = allocate_memory(64);
    mstore(p, 3);
    mstore(p + 32, 0x6162630000000000000000000000000000000000000000000000000000000000);
    return memory(p);
}

contract C {
    constructor() {}

    function keccak() public returns (bytes32) {
        return keccak256_(abcBytes());
    }

    function sha() public returns (bytes32) {
        return sha256(abcBytes());
    }

    function ripemd() public returns (bytes32) {
        return ripemd160(abcBytes());
    }

    function erc7201_(id: bytes memory) public returns (bytes32) {
        return erc7201(id);
    }
}
