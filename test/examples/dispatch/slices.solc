import std.{*};
import std.dispatch.{*};

// Exercises slice_/truncate (memory_slice) composed with concat, to_bytes,
// and the hashing precompiles (keccak256_, sha256). memory_slice implements
// MemorySize + MemoryPointer + MemoryEncode, so it is both sliceable again and
// a valid operand for concat/to_bytes/keccak256_/sha256 with zero copies.
contract C {
    // --- slice_/truncate on a memory(bytes), materialized with to_bytes ---

    public function slice_bytes(a: memory(bytes), start: uint256) -> memory(bytes) {
        return to_bytes(slice_(a, Typedef.rep(start)));
    }

    public function truncate_bytes(a: memory(bytes), end: uint256) -> memory(bytes) {
        return to_bytes(truncate(a, Typedef.rep(end)));
    }

    // --- slice_/truncate over the result of a concat ---

    public function slice_of_concat(a: bytes32, b: bytes32, start: uint256) -> memory(bytes) {
        return to_bytes(slice_(concat(a, b), Typedef.rep(start)));
    }

    public function truncate_of_concat(a: bytes32, b: bytes32, end: uint256) -> memory(bytes) {
        return to_bytes(truncate(concat(a, b), Typedef.rep(end)));
    }

    // to_bytes(truncate(slice_(concat(a, b), start), end)) -- the headline nesting:
    // drop `start` bytes, then keep `end` of what remains (re-slicing a memory_slice).
    public function window_of_concat(a: bytes32, b: bytes32, start: uint256, end: uint256) -> memory(bytes) {
        return to_bytes(truncate(slice_(concat(a, b), Typedef.rep(start)), Typedef.rep(end)));
    }

    // --- a slice used as a concat operand ---

    public function concat_slice_b32(a: memory(bytes), start: uint256, c: bytes32) -> memory(bytes) {
        return concat(slice_(a, Typedef.rep(start)), c);
    }

    public function concat_two_slices(a: memory(bytes), sa: uint256, b: memory(bytes), eb: uint256) -> memory(bytes) {
        return concat(slice_(a, Typedef.rep(sa)), truncate(b, Typedef.rep(eb)));
    }

    // --- re-slicing a memory_slice ---

    public function slice_of_slice(a: memory(bytes), s1: uint256, s2: uint256) -> memory(bytes) {
        return to_bytes(slice_(slice_(a, Typedef.rep(s1)), Typedef.rep(s2)));
    }

    // --- hashing a slice directly (no intermediate copy) ---

    public function keccak_slice(a: memory(bytes), start: uint256) -> bytes32 {
        return keccak256_(slice_(a, Typedef.rep(start)));
    }

    public function sha_truncate(a: memory(bytes), end: uint256) -> bytes32 {
        return sha256(truncate(a, Typedef.rep(end)));
    }

    // keccak256_(truncate(slice_(concat(a, b), start), end)) -- nested chain, hash endpoint.
    public function keccak_window_concat(a: bytes32, b: bytes32, start: uint256, end: uint256) -> bytes32 {
        return keccak256_(truncate(slice_(concat(a, b), Typedef.rep(start)), Typedef.rep(end)));
    }
}
