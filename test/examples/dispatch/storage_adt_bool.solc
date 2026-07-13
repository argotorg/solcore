import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.StorageGeneric.{*};

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

data Flags = Flags(bool, bool);
data Toggle = Off | On(bool);

function toBool(v : uint256) -> bool {
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
        assert(StorageSize.size(Proxy : Proxy(bool)) == 1);
        // product of two bools
        assert(StorageSize.size(Proxy : Proxy(Flags)) == 2);
        // 1 tag + max(size (), size bool)
        assert(StorageSize.size(Proxy : Proxy(Toggle)) == 2);
    }

    public function setBare(v : uint256) -> () {
        bare = toBool(v);
    }

    public function getBare() -> bool {
        return bare;
    }

    public function setFlags(a : uint256, b : uint256) -> () {
        flags = Flags(toBool(a), toBool(b));
    }

    public function firstFlag() -> bool {
        match flags {
        | Flags(a, _) => return a;
        }
    }

    public function secondFlag() -> bool {
        match flags {
        | Flags(_, b) => return b;
        }
    }

    public function turnOn(v : uint256) -> () {
        toggle = Toggle.On(toBool(v));
    }

    public function turnOff() -> () {
        toggle = Toggle.Off;
    }

    // Distinguishes Off from On(false): both leave a zero payload slot, so only
    // the tag can tell them apart.
    public function isOn() -> bool {
        match toggle {
        | Toggle.Off   => return false;
        | Toggle.On(_) => return true;
        }
    }

    public function toggleValue() -> bool {
        match toggle {
        | Toggle.Off   => revertEmpty(); return false;
        | Toggle.On(b) => return b;
        }
    }
}
