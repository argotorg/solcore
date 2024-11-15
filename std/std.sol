// booleans

data bool = true | false;

// TODO: this should short circuit. probably needs some compiler magic to do so.
function and(x: bool, y: bool) -> bool {
    match x, y {
    | true, y => return y;
    | false, _ => return false;
    };
}

// TODO: this should short circuit. probably needs some compiler magic to do so.
function or(x: bool, y: bool) -> bool {
    match x, y {
    | true, _ => return true;
    | false, y => return y;
    };
}

// Pairs

data Pair(a, b) = Pair(a, b);

function fst(p: Pair(a, b)) -> a {
    match p {
    | Pair(a, _) => return a;
    };
}

function snd(p: Pair(a, b)) -> b {
    match p {
    | Pair(_, b) => return b;
    };
}

// Proxy Type for Passing Types as Paramaters

data Proxy(t) = Proxy;

// Type Abstraction

class abs:Typedef(rep) {
    function abs(x:rep) -> abs;
    function rep(x:abs) -> rep;
}

// Arithmetic

class t:Add {
    function add(l: t, r: t) -> t;
}

class t:Sub {
    function sub(l: t, r: t) -> t;
}

class t:Mul {
    function mul(l: t, r: t) -> t;
}

class t:Div {
    function div(l: t, r: t) -> t;
}

// Word Arithmetic (unchecked for now...)

instance word:Add {
    function add(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := add(l,r);
        };
        return rw;
    }
}

instance word:Sub {
    function sub(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := sub(l,r);
        };
        return rw;
    }
}

instance word:Mul {
    function mul(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := mul(l,r);
        };
        return rw;
    }
}

instance word:Div {
    function div(l: word, r: word) -> word {
        let rw : word;
        assembly {
            rw := div(l,r);
        };
        return rw;
    }
}

// Type Classification

// Marker class for types that can be stored in a single word
class ty:ValueTy {}

// byte

data byte = byte(word);

instance byte:Typedef(word) {
    function abs(w: word) -> byte {
        return byte(w);
    }

    function rep(x: byte) -> word {
        match x {
        | byte(w) => return w;
        };
    }
}

// Generalized References

class ref:Loadable (deref) {
    function load (r : ref) -> deref;
}

class ref:Storable (deref) {
    function store (r : ref, d : deref) -> Unit;
}

class (ref : Loadable(deref), ref : Storable(deref)) => ref:Ref (deref) {}

// Memory References

data memory(t) = memory(word);

instance memory(t) : Typedef(word) {
    function abs(x: word) -> memory(t) {
        return memory(x);
    }

    function rep(x: memory(t)) -> word {
        match x {
        | memory(w) => return w;
        };
    }
}

instance (t:ValueTy) => memory(t) : Loadable(t) {
    function load(loc: memory(t)) -> t {
        let rw : word = Typedef.rep(loc);
        let res = 0;
        assembly {
            res := mload(rw)
        };
        return Typedef.abs(res);
    }
}

instance (t:ValueTy) => memory(t) : Storable(t) {
    function store(loc: memory(t), value: t) -> Unit {
        let rw : word = Typedef.rep(loc);
        let vw : word = Typedef.rep(value);
        assembly {
          mstore(rw, vw)
        };
    }
}

instance memory(t) : Ref(t) {}

// Storage References

data storage(t) = storage(word);

instance storage(t) : Typedef(word) {
    function abs(x: word) -> storage(t) {
        return storage(x);
    }

    function rep(x: storage(t)) -> word {
        match x {
        | storage(w) => return w;
       };
    }
}

instance (t:ValueTy) => storage(t) : Loadable(t) {
    function load(loc: storage(t)) -> t {
        let rw : word = Typedef.rep(loc);
        let res = 0;
        assembly {
            res := sload(rw)
        };
        return Typedef.abs(res);
    }
}

instance (t:ValueTy) => storage(t) : Storable(t) {
    function store(loc: storage(t), value: t) -> Unit {
        let rw : word = Typedef.rep(loc);
        let vw : word = Typedef.rep(value);
        assembly {
          sstore(rw, vw)
        };
    }
}

instance (t:ValueTy) => storage(t) : Ref(t) {}

// Calldata / Returndata (TODO)

data calldata(t) = calldata(word);
data returndata(t) = returndata(word);

// Free Memory Pointer

function get_free_memory() -> word {
    let res : word;
    assembly { res := mload(0x40) };
    return res;
}

function set_free_memory(loc : word) {
    assembly { mstore(0x40, loc) };
}

// Indexable Types

class t:IndexAccess(idx,val) {
    function get(c: t, i: idx) -> val;
    function set(c: t, i: idx, v: val);
}

// bytes (tightly packed byte arrays)

// TODO: bytes should be indexable (but not ref since it can't live on stack...)
// TODO: we would really want this to be a typedef to void (or a datatype with no constructors)
data bytes = bytes;

// ABI Encoding

class t:EncodeInto {
    function encodeInto(val : t, hd : word, tl : word) -> Pair(word,word);
}

class t:Encode {
    // TODO: is the following ever needed??
    // is the abi representation of `t` dynamically sized?
    // function isDynamicallySized(x:Proxy(t)) -> bool;

    // does `t` (or any of it's children) contain a dynamic type?
    function shouldEncodeDynamic(x:Proxy(t)) -> bool;
    // how big is the head portion of the ABI representation of `t`?
    function headSize(x:Proxy(t)) -> word;
}

forall t:Encode, t:EncodeInto . function encode(val:t) -> memory(bytes) {
    let p : Proxy(t);
    let hdSz = Encode.headSize(p);
    let ptr : word = get_free_memory();
    let head : word;
    let tail : word;
    assembly {
        head := add(ptr, 32);
        tail := add(head, hdSz);
    };
    let tl = snd(EncodeInto.encodeInto(val,head,tail));
    set_free_memory(tl);
    assembly {
        mstore(ptr, sub(tl, head))
    };
    let mem : memory(bytes) = memory(ptr);
    return mem;
}

instance (l:Encode, r:Encode) => Pair(l,r):Encode {
    function shouldEncodeDynamic(x : Proxy(Pair(l,r))) -> bool {
        let l: Proxy(l) = Proxy;
        let r: Proxy(r) = Proxy;
        return and(Encode.shouldEncodeDynamic(l), Encode.shouldEncodeDynamic(l));
    }

    function headSize(x : Proxy(Pair(l,r))) -> word {
        match Encode.shouldEncodeDynamic(x) {
        | true =>
            let res;
            assembly { res := 32; };
            return res;
        | false =>
            let l: Proxy(l) = Proxy;
            let r: Proxy(r) = Proxy;
            let lsize = Encode.headSize(l);
            let rsize = Encode.headSize(r);
            let res : word;
            assembly { res := add(lsize,rsize) };
            return res;
        };
    }
}

instance (l:EncodeInto, r:EncodeInto) => Pair(l,r):EncodeInto {
    function encodeInto(val, hd, tl) -> Pair(word,word) {
        let first = fst(val);
        let second = snd(val);
        let range = EncodeInto.encodeInto(first, hd, tl);
        return EncodeInto.encodeInto(second, fst(range), snd(range));
    }
}

// Uint256
data Uint256 = Uint256(word);

instance Uint256 : Typedef(word) {
    function abs(x:word) -> Uint256 {
        return Uint256(x);
    }

    function rep(x: Uint256) -> word {
        match x {
        | Uint256(val) => return val;
        };
    }
}

instance Uint256:Encode {
    function shouldEncodeDynamic(x) -> bool {
        return false;
    }

    function headSize(x) -> word {
        return 32;
    }
}

instance Uint256:EncodeInto {
    function encodeInto(v: Uint256, hd: word, tl: word) -> Pair(word, word) {
        let vw : word = Typedef.rep(v);
        let hd_ : word;
        assembly {
            hd_ := add(hd, 32)
            mstore(hd, vw)
        };
        return Pair(hd_, tl);
    }
}


// Contract Entrypoint

// TODO: need to have a way to write predicate in class function or class itself, or have a function type.
// class self:GenerateDispatch {
//     function  dispatch_if_selector_match(x: self) -> (Unit -> Unit);
// }
//
// data Dispatch(name,args,retvals) = DispatchFunction name (args->retvals)
//
// instance args:ABI => retvals:ABI => name:Selector => dispatchFuncion[name,args,retvals]:GenerateDispatch {
//     function dispatch_if_selector_match(self:dispatchFuncion[name,args,retvals]) -> (() -> ()) {
//         return lambda () {
//             let DispatchFunction(name,f) := self; // or whatever destructuring syntax we want
//             if matches_selector(name) // checks selector in calldata
//             {
//                 let (result_start, result_end) = abi.encode(f(abi.decode(TYPE(retvals)))); // conceptually at least
//                 assembly {
//                     return(result_start, result_end); // as in EVM return, terminating the external call.
//                 }
//             }
//         };
//     }
// }
//
// instance a:GenerateDispatch => b:GenerateDispatch => Pair[a,b]:GenerateDispatch {
//     function dispatch_if_selector_match(self:Pair[a,b]) -> (() -> ()) {
//         return lambda () . {
//             dispatch_if_selector_match(self.first);
//             dispatch_if_selector_match(self.second);
//         };
//     }
// }
//
// /// Translation of the above contract
// struct StorageContext {
//     x:uint;
//     y:bool;
// }
//
// function C_f(ctxt:StorageContext) public {
//     ctxt.x = 42;
// }
//
//
// function entry_C() {
//     GenerateDispatch.dispatch_if_selector_match(DispatchFunction("f()", C_f)); // could also be (nested) pairs of dispatch functions, if the contract had more functions
//     revert("unknown selector");
// }
//
// // init code for contract creation
// function init_C() {
//     // constructor code
//     let code_start := allocate_unbounded() // fetch some free memory
//     let code_length := __builtin_fetch_code(entry_C, code_start) // sounds weirder than it is - this will just add the code for entry_C to a Yul subobject and use some Yul builtins for fetching the code to be deployed
//     assembly {
//         return(code_start, code_length)
//     }
// }
//
//
