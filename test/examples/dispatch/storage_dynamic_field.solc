import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.StorageGeneric.{*};

data Blob =
      NoBlob
    | SomeBytes(memory(bytes));

contract C {
    blob : Blob;

    constructor() {
        blob = Blob.NoBlob;
        // A dynamic field occupies one slot, so the sum is 1 (tag) + max(0, 1).
        assert(StorageSize.size(Proxy : Proxy(Blob)) == 2);
    }

    public function clear() -> () {
        blob = Blob.NoBlob;
    }

    // Stores the memory(bytes) payload into the ADT field (round-trips the
    // dynamic leaf through storage(bytes)).
    public function setBytes(b: memory(bytes)) -> () {
        blob = Blob.SomeBytes(b);
    }

    public function getBytes() -> memory(bytes) {
        match blob {
        | Blob.NoBlob => revertEmpty(); return memory(0);
        | Blob.SomeBytes(b) => return b;
        }
    }

    // Loads the whole ADT back from storage and inspects its tag.
    public function isEmpty() -> bool {
        match blob {
        | Blob.NoBlob       => return true;
        | Blob.SomeBytes(_) => return false;
        }
    }
}
