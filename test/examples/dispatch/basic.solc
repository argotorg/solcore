import {*} from std;
import {*} from std.dispatch;
import {address as address_} from std.opcodes;

function self() returns (address) {
    return address(address_());
}

contract C {
    constructor() {}
    function nothing() public returns (()) {}

    // Re-enters this very contract via raw_call(address(this), ...). The payload
    // is the 4-byte selector of an existing entry point (something(), 0xa7a0d537),
    // built by left-aligning it in a bytes32 and truncating to 4 bytes. The inner
    // call succeeds, so raw_call reports ok == true and returns its returndata
    // (the abi-encoded uint256(1)).
    function callSelf() public returns ((bool, bytes memory)) {
        let sel: bytes32 = bytes32(0xa7a0d53700000000000000000000000000000000000000000000000000000000);
        let payload = truncate(to_bytes(sel), 4);
        match (raw_call(self(), uint256(0), payload) ) {
            case (ok, ret) { return (ok, ret);
        } }
    }

    // Same shape, but the selector (0xdeadc0de) matches no entry point, so dispatch
    // reverts (there is no fallback). raw_call swallows the inner revert and reports
    // ok == false; this outer call itself still succeeds and returns the revert
    // returndata (the 4-byte NoFallback error selector).
    function callSelfInvalid() public returns ((bool, bytes memory)) {
        let sel: bytes32 = bytes32(0xdeadc0de00000000000000000000000000000000000000000000000000000000);
        let payload = truncate(to_bytes(sel), 4);
        match (raw_call(self(), uint256(0), payload) ) {
            case (ok, ret) { return (ok, ret);
        } }
    }

    function something() public returns (uint256) {
        return uint256(1);
    }

    function add2(x : uint256, y : uint256) public returns (uint256) {
        return Add.add(x,y);
    }

    function add3(x : uint256, y : uint256, z : uint256) public returns (uint256) {
        return Add.add(z, Add.add(x,y));
    }

    function addmod3(x : uint256, y : uint256, k : uint256) public returns (uint256) {
        return addmod(x, y, k);
    }

    function mulmod3(x : uint256, y : uint256, k : uint256) public returns (uint256) {
        return mulmod(x, y, k);
    }

    // Bitwise / modulo via the syntactic sugar only (no explicit class calls):
    //   `^` -> BitXor.bxor, `|` -> BitOr.bor, `&` -> BitAnd.band, `%` -> Mod.mod.
    function bxor2(x : uint256, y : uint256) public returns (uint256) {
        return x ^ y;
    }

    function bor2(x : uint256, y : uint256) public returns (uint256) {
        return x | y;
    }

    function band2(x : uint256, y : uint256) public returns (uint256) {
        return x & y;
    }

    function mod2(x : uint256, y : uint256) public returns (uint256) {
        return x % y;
    }

    function id_bytes(b: bytes memory) public returns (bytes memory) {
        return b;
    }

    function id_string(b: string memory) public returns (string memory) {
        return b;
    }

    function id_bytes32(b: bytes32) public returns (bytes32) {
        return b;
    }

    function id_address(a: address) public returns (address) {
        return a;
    }

    function id_pair() public returns ((uint256, uint256)) {
        return (uint256(7), uint256(11));
    }

    function hidden() returns (uint256) {
        return uint256(42);
    }
}
