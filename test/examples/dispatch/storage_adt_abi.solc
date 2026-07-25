import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.ABIGeneric;
import {*} from std.StorageGeneric;

// An ADT crossing the ABI boundary AND living in storage at the same time.
//
// The public entry points take and return `Option(uint256)` directly, so the
// generated dispatch has to
//   * derive an ABIDecode instance for the type (DeriveGeneric.buildABIDecode),
//   * reach ABIEncode / ABIAttribs through the default Generic bridges, and
//   * build a selector from the derived SigString, which for a sum is the
//     structural string "sum(<l>,<r>)" — here rep = sum((), uint256) and
//     sigStr(()) = "", so the signature is `setOpt(sum(,uint256))`.
//
// Wire layout of a sum (std.ABIGeneric): one tag word, then the branch payload
// at +32. So `Some(42)` is 0x...01 followed by 0x...2a, and `None` is 0x...00
// followed by a don't-care word.

enum Option<a> { None, Some(a) }

contract C {
    stored : Option<uint256>;

    constructor() {
        stored = Option.None;
        assert(StorageSize.size(Proxy as Proxy<Option<uint256>>) == 2);
    }

    // ADT as a parameter: decoded from calldata, then written to storage.
    function setOpt(o : Option<uint256>) public returns (()) {
        stored = o;
    }

    // ADT as a return value: loaded from storage, then encoded into returndata.
    function getOpt() public returns (Option<uint256>) {
        return stored;
    }

    // Round-trip in one call, without touching storage.
    function echo(o : Option<uint256>) public returns (Option<uint256>) {
        return o;
    }

    function isSome() public returns (bool) {
        match (stored ) {
        case Option.None    { return false;
        } case Option.Some(_) { return true;
        } }
    }

    function unwrapOr(d : uint256) public returns (uint256) {
        match (stored ) {
        case Option.None    { return d;
        } case Option.Some(v) { return v;
        } }
    }
}
