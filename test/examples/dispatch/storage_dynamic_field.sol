import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.StorageGeneric;

enum Blob { NoBlob, SomeBytes(bytes memory) }

contract C {
    blob : Blob;

    constructor() {
        blob = Blob.NoBlob;
        // A dynamic field occupies one slot, so the sum is 1 (tag) + max(0, 1).
        assert(StorageSize.size(Proxy as Proxy<Blob>) == 2);
    }

    function clear() public returns (()) {
        blob = Blob.NoBlob;
    }

    // Stores the memory(bytes) payload into the ADT field (round-trips the
    // dynamic leaf through storage(bytes)).
    function setBytes(b: bytes memory) public returns (()) {
        blob = Blob.SomeBytes(b);
    }

    function getBytes() public returns (bytes memory) {
        match (blob ) {
        case Blob.NoBlob { revertEmpty(); return memory(0);
        } case Blob.SomeBytes(b) { return b;
        } }
    }

    // Loads the whole ADT back from storage and inspects its tag.
    function isEmpty() public returns (bool) {
        match (blob ) {
        case Blob.NoBlob       { return true;
        } case Blob.SomeBytes(_) { return false;
        } }
    }
}
