import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.StorageGeneric;

// `bool` in storage, bare and inside an ADT.
//
// bool is a builtin rather than a Typedef(word), so it has no StorageType
// instance. It is storable only through the dedicated
// `storage(bool):CanStore(bool)` instance, which round-trips it via
// frombool / tobool. This test pins that instance down, both as a plain
// contract field and as a leaf reached through the structural CanStore
// decomposition of an ADT.
//
// Note the entry points take uint256 rather than bool: bool has no ABIDecode
// instance, so it cannot appear in a public parameter position. It can appear
// in a return position, which is what the getters below exercise.

enum Flags { Flags(bool, bool) }
enum Toggle { Off, On(bool) }

function toBool(v : uint256) returns (bool) {
    return v != uint256(0);
}

contract C {
    bare : bool;
    flags : Flags;
    toggle : Toggle;

    constructor() {
        bare = false;
        flags = Flags(false, false);
        toggle = Toggle.Off;
        assert(StorageSize.size(Proxy as Proxy<bool>) == 1);
        // product of two bools
        assert(StorageSize.size(Proxy as Proxy<Flags>) == 2);
        // 1 tag + max(size (), size bool)
        assert(StorageSize.size(Proxy as Proxy<Toggle>) == 2);
    }

    function setBare(v : uint256) public returns (()) {
        bare = toBool(v);
    }

    function getBare() public returns (bool) {
        return bare;
    }

    function setFlags(a : uint256, b : uint256) public returns (()) {
        flags = Flags(toBool(a), toBool(b));
    }

    function firstFlag() public returns (bool) {
        match (flags ) {
        case Flags(a, _) { return a;
        } }
    }

    function secondFlag() public returns (bool) {
        match (flags ) {
        case Flags(_, b) { return b;
        } }
    }

    function turnOn(v : uint256) public returns (()) {
        toggle = Toggle.On(toBool(v));
    }

    function turnOff() public returns (()) {
        toggle = Toggle.Off;
    }

    // Distinguishes Off from On(false): both leave a zero payload slot, so only
    // the tag can tell them apart.
    function isOn() public returns (bool) {
        match (toggle ) {
        case Toggle.Off   { return false;
        } case Toggle.On(_) { return true;
        } }
    }

    function toggleValue() public returns (bool) {
        match (toggle ) {
        case Toggle.Off   { revertEmpty(); return false;
        } case Toggle.On(b) { return b;
        } }
    }
}
