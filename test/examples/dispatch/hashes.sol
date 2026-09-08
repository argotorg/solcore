import * from std;
import * from std.dispatch;
import {mstore} from std.opcodes;

// Build a memory(bytes) holding the three-byte string "abc".
function abcBytes() returns (memory<bytes>) {
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

    // keccakWordLit folds keccak256 of a word's 32-byte big-endian form at
    // compile time; keccakWordLit(0) == keccak256(bytes32(0)).
    function keccakWord() public returns (bytes32) {
        return bytes32(keccakWordLit(0));
    }

    // ERC-7201 namespaced storage slots, folded to constants at compile time
    // from the string-literal namespace (no runtime keccak of the id).
    function erc7201Example() public returns (bytes32) {
        return erc7201("example.main");
    }

    function erc7201Ownable() public returns (bytes32) {
        return erc7201("openzeppelin.storage.Ownable");
    }

    function erc7201Empty() public returns (bytes32) {
        return erc7201("");
    }
}
