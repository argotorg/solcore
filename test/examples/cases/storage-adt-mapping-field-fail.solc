import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.StorageGeneric.{*};

// A mapping cannot be a field of a data type. std only provides
// `storage(mapping(k,v)) : CanStore(storage(mapping(k,v)))` — the slot handle
// loads back as a handle, never as a mapping value — so the structural CanStore
// decomposition of `Wrapper` asks for `storage(mapping(uint256,uint256)) :
// CanStore(mapping(uint256,uint256))`, which does not exist.
//
// (Even if it did, that instance's store/load are `unimplemented()`: copying a
// mapping is not a meaningful storage operation.)

data Wrapper = Wrapper(mapping(uint256, uint256));

contract C {
    w : Wrapper;

    constructor() {}
}
