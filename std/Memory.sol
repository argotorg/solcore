import "Prelude.sol";

data memory(a) = memory(word);

instance memory(a):Typedef(word) {
    function rep(x:memory(a)) -> word {
        match x {
            | memory(y) => return y;
        }
    }
    function abs(x:word) -> memory(a) {
        return memory(x);
    }
}

class self:MemorySize {
    /* compeval */ function size(x:Proxy(self)) -> word;
}

forall abs rep . abs:Typedef(rep) =>
default instance abs:MemorySize {
    /* compeval */ function size(x:Proxy(abs)) -> word {
        return MemorySize.size(x:Proxy(rep));
    }
}

instance word:MemorySize {
    /* compeval */ function size(x:Proxy(word)) -> word {
        return 32;
    }
}

forall a b . a:MemorySize, b:MemorySize =>
instance (a,b):MemorySize {
    /* comperval */ function size(x:Proxy((a,b))) -> word {
        let sz = MemorySize.size(Proxy:Proxy(a));
        let b_sz = MemorySize.size(Proxy:Proxy(b));
        assembly { sz := add(sz, b_sz) }
        return sz;
    }
}

class self:MemoryType {
    function store(ptr:word, x:self);
    function load(ptr:word) -> self;
}

instance word:MemoryType {
    function store(ptr:word, x:word) {
        assembly {
            mstore(ptr, x)
        }
    }
    function load(ptr:word) -> word {
        let r:word;
        assembly {
            r := mload(ptr)
        }
        return r;
    }
}

forall abs rep . abs:Typedef(rep), rep:MemoryType =>
default instance abs:MemoryType {
    function store(ptr:word, x:abs) {
        MemoryType.store(ptr, Typedef.rep(x));
    }
    function load(ptr:word) -> abs {
        return Typedef.abs(MemoryType.load(ptr));
    }
}

forall a b . a:MemoryType, b:MemoryType, a:MemorySize =>
instance (a,b):MemoryType {
    function store(ptr:word, p:(a,b)) {
        match p {
            | (x,y) => {
                MemoryType.store(ptr, x);
                // compeval {
                let offset = MemorySize.size(Proxy:Proxy(a));
                assembly { ptr := add(ptr, offset) }
                // }
                MemoryType.store(ptr, y);
            }
        }
    }
}


// Note that this will only e.g. trigger for *(x:memory(a)) = ... and not for
// (x:memory(a)) = ..., since the latter will result in ref(memory(a)):Assign(memory(a))
forall a . a:MemoryType
instance memory(a):Assign(a) {
    function assign(l:memory(a), r:a) -> () {
        MemoryType.store(Typedef.rep(l), r);
    }
}

function allocate_unbounded() -> word {
    let mptr;
    assembly {
        mptr := mload(0x40)
    }
    return mptr;
}

function finalize_allocation(ptr:word, size:word) -> () {
    assembly {
        mstore(0x40, add(ptr, size))
    }
    return ();
}

function allocate_sized(size:word) -> word {
    let mptr;
    assembly {
        mptr := mload(0x40)
        mstore(mptr, add(mptr, size))
    }
    return mptr;
}
