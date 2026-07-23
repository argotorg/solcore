import {*} from std;
import {*} from std.dispatch;
import {mload, mstore} from std.opcodes;
import {*} from std.Generic;
import {*} from std.ABIGeneric;

pragma solcore noGenericInstanceFor Point;

enum Point { Point(uint256, uint256) }

// Only requirement: Generic instance using the primitive pair type.
// rep = (uint256, uint256) — primitive Solcore pair
impl Generic<Point, (uint256, uint256)> {
    function from(p : Point) returns ((uint256, uint256)) {
        match (p ) { case Point(x, y) { return (x, y); } }
    }
    function to(t : (uint256, uint256)) returns (Point) {
        match (t ) { case (x, y) { return Point(x, y); } }
    }
}

contract GenericProduct {
    constructor() {}

    // Calls encode; returns word at offset 0 (the x field).
    function encodeX(a : uint256, b : uint256) public returns (uint256) {
        let p : Point = Point(a, b);
        let buf = allocate_zeroed_memory(64);
        encode(p, buf, 0, 64);
        return Typedef.abs(mload(buf));
    }

    // Calls encode; returns word at offset 32 (the y field).
    function encodeY(a : uint256, b : uint256) public returns (uint256) {
        let p : Point = Point(a, b);
        let buf = allocate_zeroed_memory(64);
        encode(p, buf, 0, 64);
        return Typedef.abs(mload(buf + 32));
    }

    // Writes [a][b] into memory, calls decode, returns the x field.
    function decodeX(a : uint256, b : uint256) public returns (uint256) {
        let buf = allocate_zeroed_memory(64);
        mstore(buf, Typedef.rep(a));
        mstore(buf + 32, Typedef.rep(b));
        let rdr : MemoryWordReader = MemoryWordReader(buf);
        let dec : ABIDecoder<Point, MemoryWordReader> = ABIDecoder(rdr);
        let p : Point = decode(dec, 0);
        match (p ) { case Point(x, _) { return x; } }
    }
}
