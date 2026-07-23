import {*} from std;
import {*} from std.dispatch;
import {mload, mstore} from std.opcodes;
import {*} from std.Generic;
import {*} from std.ABIGeneric;

pragma solcore noGenericInstanceFor Option;

enum Option<a> { None, Some(a) }

// Only requirement: Generic instance using the primitive sum type.
// rep = sum((), uint256): inl(()) = None, inr(v) = Some(v)
impl Generic<Option<uint256>, sum<(), uint256>> {
    function from(x : Option<uint256>) returns (sum<(), uint256>) {
        match (x ) {
        case Option.None    { return inl(());
        } case Option.Some(v) { return inr(v);
        } }
    }
    function to(r : sum<(), uint256>) returns (Option<uint256>) {
        match (r ) {
        case inl(_) { return Option.None;
        } case inr(v) { return Option.Some(v);
        } }
    }
}

contract GenericSum {
    constructor() {}

    // Calls encode; returns the tag word (first 32 bytes).
    // None → 0
    function encodeNone() public returns (uint256) {
        let x : Option<uint256> = Option.None;
        let buf = allocate_zeroed_memory(64);
        encode(x, buf, 0, 64);
        return Typedef.abs(mload(buf));
    }

    // Calls encode; returns the tag word (first 32 bytes).
    // Some(n) → 1
    function encodeSomeTag(n : uint256) public returns (uint256) {
        let x : Option<uint256> = Option.Some(n);
        let buf = allocate_zeroed_memory(64);
        encode(x, buf, 0, 64);
        return Typedef.abs(mload(buf));
    }

    // Calls encode; returns the payload word (bytes 32-63).
    function encodePayload(n : uint256) public returns (uint256) {
        let x : Option<uint256> = Option.Some(n);
        let buf = allocate_zeroed_memory(64);
        encode(x, buf, 0, 64);
        return Typedef.abs(mload(buf + 32));
    }

    // Writes [tag][value] into memory, calls decode, returns the value or 0.
    function decodeAndGet(tag : uint256, value : uint256) public returns (uint256) {
        let buf = allocate_zeroed_memory(64);
        mstore(buf, Typedef.rep(tag));
        mstore(buf + 32, Typedef.rep(value));
        let rdr : MemoryWordReader = MemoryWordReader(buf);
        let dec : ABIDecoder<Option<uint256>, MemoryWordReader> = ABIDecoder(rdr);
        let opt : Option<uint256> = decode(dec, 0);
        match (opt ) {
        case Option.None    { return uint256(0);
        } case Option.Some(v) { return v;
        } }
    }
}
