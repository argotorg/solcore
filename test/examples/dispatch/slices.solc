import {*} from std;
import {*} from std.dispatch;

// Exercises slice_/truncate (memory_slice) composed with concat, to_bytes,
// and the hashing precompiles (keccak256_, sha256). memory_slice implements
// MemorySize + MemoryPointer + MemoryEncode, so it is both sliceable again and
// a valid operand for concat/to_bytes/keccak256_/sha256 with zero copies.
contract C {
    // --- slice_/truncate on a memory(bytes), materialized with to_bytes ---

    function slice_bytes(a: bytes memory, start: uint256) public returns (bytes memory) {
        return to_bytes(slice_(a, Typedef.rep(start)));
    }

    function truncate_bytes(a: bytes memory, end: uint256) public returns (bytes memory) {
        return to_bytes(truncate(a, Typedef.rep(end)));
    }

    // --- slice_/truncate over the result of a concat ---

    function slice_of_concat(a: bytes32, b: bytes32, start: uint256) public returns (bytes memory) {
        return to_bytes(slice_(concat(a, b), Typedef.rep(start)));
    }

    function truncate_of_concat(a: bytes32, b: bytes32, end: uint256) public returns (bytes memory) {
        return to_bytes(truncate(concat(a, b), Typedef.rep(end)));
    }

    // to_bytes(truncate(slice_(concat(a, b), start), end)) -- the headline nesting:
    // drop `start` bytes, then keep `end` of what remains (re-slicing a memory_slice).
    function window_of_concat(a: bytes32, b: bytes32, start: uint256, end: uint256) public returns (bytes memory) {
        return to_bytes(truncate(slice_(concat(a, b), Typedef.rep(start)), Typedef.rep(end)));
    }

    // --- a slice used as a concat operand ---

    function concat_slice_b32(a: bytes memory, start: uint256, c: bytes32) public returns (bytes memory) {
        return concat(slice_(a, Typedef.rep(start)), c);
    }

    function concat_two_slices(a: bytes memory, sa: uint256, b: bytes memory, eb: uint256) public returns (bytes memory) {
        return concat(slice_(a, Typedef.rep(sa)), truncate(b, Typedef.rep(eb)));
    }

    // --- re-slicing a memory_slice ---

    function slice_of_slice(a: bytes memory, s1: uint256, s2: uint256) public returns (bytes memory) {
        return to_bytes(slice_(slice_(a, Typedef.rep(s1)), Typedef.rep(s2)));
    }

    // --- hashing a slice directly (no intermediate copy) ---

    function keccak_slice(a: bytes memory, start: uint256) public returns (bytes32) {
        return keccak256_(slice_(a, Typedef.rep(start)));
    }

    function sha_truncate(a: bytes memory, end: uint256) public returns (bytes32) {
        return sha256(truncate(a, Typedef.rep(end)));
    }

    // keccak256_(truncate(slice_(concat(a, b), start), end)) -- nested chain, hash endpoint.
    function keccak_window_concat(a: bytes32, b: bytes32, start: uint256, end: uint256) public returns (bytes32) {
        return keccak256_(truncate(slice_(concat(a, b), Typedef.rep(start)), Typedef.rep(end)));
    }
}
