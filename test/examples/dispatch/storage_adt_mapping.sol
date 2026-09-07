import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.StorageGeneric;

// An ADT used as the VALUE of a storage mapping.
//
// This is the path opened by routing mapping reads through CanStore instead of
// StorageType (std.solc: readStorage / ridx / RValueIdxAccess). The write side
// already went through Assign -> CanStore.store.
//
// A multi-slot value in a mapping occupies hash2(slot, key) .. + size(v) - 1,
// exactly as Solidity lays out a struct behind a mapping.

enum Option<a> { None, Some(a) }
enum Pair { Pair(uint256, uint256) }

contract C {
    // 2 slots per entry: tag + payload
    opts : mapping(uint256 => Option<uint256>);
    // 2 slots per entry: no tag, two words
    pairs : mapping(uint256 => Pair);
    // 3 slots per entry: tag + max(0, 2)
    optPairs : mapping(uint256 => Option<Pair>);

    constructor() {
        assert(StorageSize.size(Proxy as Proxy<Option<uint256>>) == 2);
        assert(StorageSize.size(Proxy as Proxy<Pair>) == 2);
        assert(StorageSize.size(Proxy as Proxy<Option<Pair>>) == 3);
    }

    function putOpt(k : uint256, v : uint256) public returns (()) {
        opts[k] = Option.Some(v);
    }

    function clearOpt(k : uint256) public returns (()) {
        opts[k] = Option.None;
    }

    // Unset keys read back as the zero slot pattern, i.e. tag 0 = None.
    function hasOpt(k : uint256) public returns (bool) {
        match (opts[k] ) {
        case Option.None    { return false;
        } case Option.Some(_) { return true;
        } }
    }

    function getOpt(k : uint256) public returns (uint256) {
        match (opts[k] ) {
        case Option.None    { revertEmpty(); return uint256(0);
        } case Option.Some(v) { return v;
        } }
    }

    function putPair(k : uint256, a : uint256, b : uint256) public returns (()) {
        pairs[k] = Pair(a, b);
    }

    function pairSum(k : uint256) public returns (uint256) {
        match (pairs[k] ) {
        case Pair(a, b) { return a + b;
        } }
    }

    function putOptPair(k : uint256, a : uint256, b : uint256) public returns (()) {
        optPairs[k] = Option.Some(Pair(a, b));
    }

    function clearOptPair(k : uint256) public returns (()) {
        optPairs[k] = Option.None;
    }

    function optPairSum(k : uint256) public returns (uint256) {
        match (optPairs[k] ) {
        case Option.None    { revertEmpty(); return uint256(0);
        } case Option.Some(p) {
            match (p ) {
            case Pair(a, b) { return a + b;
            } }
        } }
    }
}
