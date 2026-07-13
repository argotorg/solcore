import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.StorageGeneric.{*};

// An ADT used as the VALUE of a storage mapping.
//
// This is the path opened by routing mapping reads through CanStore instead of
// StorageType (std.solc: readStorage / ridx / RValueIdxAccess). The write side
// already went through Assign -> CanStore.store.
//
// A multi-slot value in a mapping occupies hash2(slot, key) .. + size(v) - 1,
// exactly as Solidity lays out a struct behind a mapping.

data Option(a) = None | Some(a);
data Pair = Pair(uint256, uint256);

contract C {
    // 2 slots per entry: tag + payload
    opts : mapping(uint256, Option(uint256));
    // 2 slots per entry: no tag, two words
    pairs : mapping(uint256, Pair);
    // 3 slots per entry: tag + max(0, 2)
    optPairs : mapping(uint256, Option(Pair));

    constructor() {
        assert(StorageSize.size(Proxy : Proxy(Option(uint256))) == 2);
        assert(StorageSize.size(Proxy : Proxy(Pair)) == 2);
        assert(StorageSize.size(Proxy : Proxy(Option(Pair))) == 3);
    }

    public function putOpt(k : uint256, v : uint256) -> () {
        opts[k] = Option.Some(v);
    }

    public function clearOpt(k : uint256) -> () {
        opts[k] = Option.None;
    }

    // Unset keys read back as the zero slot pattern, i.e. tag 0 = None.
    public function hasOpt(k : uint256) -> bool {
        match opts[k] {
        | Option.None    => return false;
        | Option.Some(_) => return true;
        }
    }

    public function getOpt(k : uint256) -> uint256 {
        match opts[k] {
        | Option.None    => revertEmpty(); return uint256(0);
        | Option.Some(v) => return v;
        }
    }

    public function putPair(k : uint256, a : uint256, b : uint256) -> () {
        pairs[k] = Pair(a, b);
    }

    public function pairSum(k : uint256) -> uint256 {
        match pairs[k] {
        | Pair(a, b) => return a + b;
        }
    }

    public function putOptPair(k : uint256, a : uint256, b : uint256) -> () {
        optPairs[k] = Option.Some(Pair(a, b));
    }

    public function clearOptPair(k : uint256) -> () {
        optPairs[k] = Option.None;
    }

    public function optPairSum(k : uint256) -> uint256 {
        match optPairs[k] {
        | Option.None    => revertEmpty(); return uint256(0);
        | Option.Some(p) =>
            match p {
            | Pair(a, b) => return a + b;
            }
        }
    }
}
