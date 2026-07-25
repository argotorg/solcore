import {*} from std;
import {*} from std.dispatch;

contract C {
    constructor() {}

    function concat_b32_b32(a: bytes32, b: bytes32) public returns (bytes memory) {
        return concat(a, b);
    }

    function concat_b32_bytes(a: bytes32, b: bytes memory) public returns (bytes memory) {
        return concat(a, b);
    }

    function concat_bytes_bytes(a: bytes memory, b: bytes memory) public returns (bytes memory) {
        return concat(a, b);
    }

    function to_bytes_b32(a: bytes32) public returns (bytes memory) {
        return to_bytes(a);
    }

    function to_bytes_bytes(a: bytes memory) public returns (bytes memory) {
        return to_bytes(a);
    }

    function empty_area(n: uint256) public returns (bytes memory) {
        return to_bytes(empty(Typedef.rep(n)));
    }

    function concat_b32_empty(a: bytes32, n: uint256) public returns (bytes memory) {
        return concat(a, empty(Typedef.rep(n)));
    }

    function concat_nested_b32(a: bytes32, b: bytes32, c: bytes32) public returns (bytes memory) {
        return concat(a, concat(b, c));
    }

    function concat_nested_empty(a: bytes32, n: uint256, c: bytes32) public returns (bytes memory) {
        return concat(a, concat(empty(Typedef.rep(n)), c));
    }
}
