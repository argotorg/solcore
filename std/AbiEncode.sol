import "ValueTypes.sol"

///// NOTE: incomplete and not correct for reference types / dynamically encoded types, etc.

class self:ABIEncode {
    function encodeInto(headPtr:word, tailPtr:word, v:self) -> (word, word);
    function headSize(x:Proxy(self)) -> word;
}

instance word:ABIEncode {
    function encodeInto(headPtr:word, tailPtr:word, v:word) -> (word, word) {
        assembly {
            mstore(headPtr, v)
            headPtr := add(headPtr, 32)
        }
        return (headPtr, tailPtr);
    }
    function headSize(x:Proxy(word)) -> word {
        return 32;
    }
}

forall a b . a:ABIEncode, b:ABIEncode
instance (a,b):ABIEncode {
    function encodeInto(headPtr:word, tailPtr:word, v:(a,b)) -> (word, word) {
        match v {
            | (x,y) => {
                (headPtr, tailPtr) = ABIEncode.encodeInto(headPtr, tailPtr, x);
                return ABIEncode.encodeInto(headPtr, tailPtr, y);
            }
        }
    }
    function headSize(x:Proxy((a,b))) -> word {
        let sz = AbiEncode.headSize(Proxy:Proxy(a));
        let b_sz = AbiEncode.headSize(Proxy:Proxy(b));
        assembly { sz := add(sz, b_sz) }
        return sz;
    }
}

// TODO: careful about tuples and their ABI representation; need ABITypedef or equivalent to do this properly
forall rep . abs:Typedef(rep), rep:ABIEncode =>
default instance abs:ABIEncode {
    function encodeInto(headPtr:word, tailPtr:word, v:abs) -> (word, word) {
        return ABIEncode.encodeInto(headPtr, tailPtr, Typedef.rep(v));
    }
    function headSize(x:Proxy(abs)) -> word {
        return ABIEncode.headSize(Proxy:Proxy(rep));
    }
}

instance bytes:ABIEncode {
    function encodeInto(headPtr:word, tailPtr:word, v:bytes) -> (word, word) {
        // unreachable, since bytes is void
        return (headPtr, tailPtr);
    }
    function headSize(p:Proxy(bytes)) -> word {
        return 32;
    }
}

class self:ABIEncodeFromMemory {
    function encodeInto(headPtr:word, tailPtr:word, v:memory(self)) -> (word, word);
}

forall a . a:MemoryType, a:ABIEncode =>
default instance a:ABIEncodeFromMemory {
    function encodeInto(headPtr:word, tailPtr:word, v:memory(a)) -> (word, word) {
        return ABIEncode.encodeInto(headPtr, tailPtr, MemoryType.load(Typedef.rep(v)):a);
    }
}

instance bytes:ABIEncodeFromMemory {
    function encodeInto(headPtr:word, tailPtr:word, v:memory(bytes)) -> (word, word) {
        let ptr = Typedef.rep(v);
        let sz;
        assembly {
            sz := mload(ptr)
            ptr := add(ptr, 32)
            mstore(headPtr, tailPtr)
            mcopy(tailPtr, ptr, sz)
            headPtr := add(headPtr, 32)
            tailPtr := add(tailPtr, sz) // TODO: round up to multiple of 32
        }
        return (headPtr, tailPtr);
    }
}


// TODO:
// instance dynamicArray(a):ABIEncodeFromMemory {}

forall a . a:ABIEncodeFromMemory =>
instance memory(a):ABIEncode {
    function encodeInto(headPtr:word, tailPtr:word, v:memory(a)) -> (word, word) {
        return ABIEncodeFromMemory.encodeInto(headPtr, tailPtr, v);
    }
    function headSize(x:Proxy(memory(a))) -> word {
        return ABIEncode.headSize(Proxy:Proxy(a));
    }
}

forall a . a:ABIEncode
function abiEncode(x:a) -> memory(bytes) {
    let headPtr = allocate_unbounded();
    let headSize = AbiEncode.headSize(Proxy:Proxy(a));
    let tailPtr;
    assembly { tailPtr := add(headPtr, headSize) }
    (_, tailPtr) = ABIEncode.encodeInto(headPtr, tailPtr, x);
    let size;
    assembly { size := sub(tailPtr, headPtr) }
    finalize_allocation(headPtr, size);
    return memory(headPtr);
}
